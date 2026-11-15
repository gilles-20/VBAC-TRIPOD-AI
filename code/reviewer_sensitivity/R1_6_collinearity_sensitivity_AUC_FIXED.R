#!/usr/bin/env Rscript
# Post-review sensitivity analysis R1.6. Original models and outputs are not modified.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
input <- file.path(root,'us_birth_files/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz')
out <- Sys.getenv('R1_6_COL_OUT', file.path(root,'REVIEWER_REVISION_2026','R1_6_COLLINEARITY'))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(file.exists(input))
cat('Loading:',input,'\n')
d <- read.csv(gzfile(input),check.names=FALSE)
year <- if ('source_year' %in% names(d)) 'source_year' else 'birth_year'
stopifnot(all(c('vbac',year) %in% names(d)))
base <- c('maternal_age','total_prior_live_births','previous_preterm_birth_binary',
          'prepregnancy_diabetes_binary','prepregnancy_hypertension_binary',
          'gestational_diabetes_binary','gestational_hypertension_binary')
anthro <- c('maternal_height_cm','prepregnancy_weight_kg','prepregnancy_bmi')
vars <- c(base,anthro)
stopifnot(all(vars %in% names(d)))
for (v in c(vars,'vbac')) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
if (!all(na.omit(unique(d$vbac)) %in% c(0,1))) stop('vbac must be coded 0/1')
dev <- d[d[[year]] >= 2016 & d[[year]] <= 2021,,drop=FALSE]
val <- d[d[[year]] >= 2022 & d[[year]] <= 2023,,drop=FALSE]
test <- d[d[[year]] == 2024,,drop=FALSE]
stopifnot(nrow(dev)>0,nrow(val)>0,nrow(test)>0)
# Reproduce development-only median/mode imputation. No values learned from validation/test.
for (v in vars) {
  obs <- dev[[v]][!is.na(dev[[v]])]
  if (!length(obs)) stop('No observed development values: ',v)
  if (v %in% c('maternal_age','total_prior_live_births',anthro)) {
    imp <- median(obs)
  } else {
    tb <- table(obs); imp <- as.numeric(names(tb)[which.max(tb)])
  }
  for (nm in c('dev','val','test')) {
    tmp <- get(nm); tmp[[v]][is.na(tmp[[v]])] <- imp; assign(nm,tmp)
  }
}
# All four models use identical cohort, outcome and non-anthropometric covariates.
specs <- list(height_weight=c('maternal_height_cm','prepregnancy_weight_kg'),
              height_bmi=c('maternal_height_cm','prepregnancy_bmi'),
              weight_bmi=c('prepregnancy_weight_kg','prepregnancy_bmi'),
              all_three=anthro)
# Exact linear VIF, using the design matrix for numeric predictors.
get_vif <- function(X) {
  if (ncol(X)<2) return(setNames(rep(NA_real_,ncol(X)),colnames(X)))
  sapply(seq_len(ncol(X)),function(j) {
    y <- X[,j]; others <- X[,-j,drop=FALSE]
    if (sd(y)==0) return(NA_real_)
    rss <- sum(lm.fit(cbind(1,others),y)$residuals^2)
    r2 <- 1-rss/sum((y-mean(y))^2)
    if (r2>=1) Inf else 1/(1-r2)
  })
}
# Avoid dependency on contributed packages: rank-based AUC (ties handled).
auc <- function(y,p) {
  ok <- is.finite(y)&is.finite(p); y<-y[ok];p<-p[ok]
  n1<-as.numeric(sum(y==1));n0<-as.numeric(sum(y==0))
  if (n1==0||n0==0) return(NA_real_)
  (sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0)
}
metrics <- function(y,p,model,period) {
  p<-pmin(pmax(p,1e-10),1-1e-10)
  lp<-qlogis(p)
  intercept <- suppressWarnings(coef(glm(y~1,offset=lp,family=binomial()))[1])
  slope <- suppressWarnings(coef(glm(y~lp,family=binomial()))[2])
  data.frame(model=model,period=period,n=length(y),observed=mean(y),predicted=mean(p),
             auc=auc(y,p),brier=mean((y-p)^2),oe=sum(y)/sum(p),
             calibration_intercept=intercept,calibration_slope=slope)
}
perf<-list(); coefs<-list(); vifs<-list(); preds<-list()
for (name in names(specs)) {
  predictors <- c(base,specs[[name]])
  # Exclude predictors with zero variance using development data only.
  predictors <- predictors[vapply(predictors,function(v) length(unique(dev[[v]]))>1,logical(1))]
  f <- reformulate(predictors,response='vbac')
  cat('Fitting:',name,' predictors=',paste(predictors,collapse=', '),'\n')
  fit <- glm(f,data=dev,family=binomial(),control=glm.control(maxit=50))
  if (!fit$converged) warning('Model did not converge: ',name)
  sm <- summary(fit)$coefficients
  coefs[[name]] <- data.frame(model=name,term=rownames(sm),estimate=sm[,1],se=sm[,2],
                              z=sm[,3],p=sm[,4],row.names=NULL)
  X <- as.matrix(dev[,predictors,drop=FALSE])
  vifs[[name]] <- data.frame(model=name,variable=predictors,vif=as.numeric(get_vif(X)))
  for (period in c('development','intermediate_2022_2023','test_2024')) {
    dat <- switch(period,development=dev,intermediate_2022_2023=val,test_2024=test)
    p <- predict(fit,newdata=dat,type='response')
    perf[[paste(name,period)]] <- metrics(dat$vbac,p,name,period)
    if (period=='test_2024') preds[[name]] <- p
  }
  saveRDS(fit,file.path(out,paste0('model_',name,'.rds')))
}
write.csv(do.call(rbind,perf),file.path(out,'performance_by_specification.csv'),row.names=FALSE)
write.csv(do.call(rbind,coefs),file.path(out,'coefficients.csv'),row.names=FALSE)
write.csv(do.call(rbind,vifs),file.path(out,'vif_diagnostics.csv'),row.names=FALSE)
ref<-preds[['all_three']]
diffs <- do.call(rbind,lapply(names(preds),function(nm) {
  dd<-abs(preds[[nm]]-ref)
  data.frame(model=nm,n=length(dd),mean_abs_difference=mean(dd),
             median_abs_difference=median(dd),p95_abs_difference=unname(quantile(dd,.95)),
             p99_abs_difference=unname(quantile(dd,.99)),max_abs_difference=max(dd))
}))
write.csv(diffs,file.path(out,'prediction_differences_vs_all_three_2024.csv'),row.names=FALSE)
writeLines(c(paste('R version:',R.version.string),paste('Input:',input),
             'All four models were newly fitted for post-review sensitivity analysis.',
             'Imputation values were derived exclusively from 2016-2021.',
             '2024 comparisons are post-review sensitivity analyses, not frozen-protocol validation.',
             capture.output(sessionInfo())),file.path(out,'README_run.txt'))
cat('DONE. Results:',out,'\n')
