#!/usr/bin/env Rscript
# R1.3 post-review nonlinearity sensitivity analysis (base/recommended R only).
# Does not modify original files; 2024 is a post-review sensitivity assessment.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
input <- file.path(root,'us_birth_files/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz')
out <- Sys.getenv(
  'R1_3_OUT',
  file.path(root,'REVIEWER_REVISION_2026','R1_3_NONLINEARITY')
)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(file.exists(input))
set.seed(20261114)
d <- read.csv(gzfile(input),check.names=FALSE)
year <- 'birth_year'
base <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg','prepregnancy_bmi',
          'total_prior_live_births','previous_preterm_birth_binary',
          'prepregnancy_diabetes_binary','prepregnancy_hypertension_binary',
          'gestational_diabetes_binary','gestational_hypertension_binary')
stopifnot(all(c(base,'vbac',year) %in% names(d)))
for(v in c(base,'vbac',year)) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
stopifnot(all(na.omit(unique(d$vbac)) %in% c(0,1)))
dev <- d[d[[year]] %in% 2016:2021,,drop=FALSE]
val <- d[d[[year]] %in% 2022:2023,,drop=FALSE]
test <- d[d[[year]]==2024,,drop=FALSE]
stopifnot(nrow(dev)==499360L,nrow(test)==94071L)
# Match original primary script: development-only medians for continuous variables,
# modes for binary variables; do not learn imputation from validation/test.
continuous <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg',
                'prepregnancy_bmi','total_prior_live_births')
binary <- setdiff(base,continuous)
imp_log <- list()
for(v in base) {
  observed <- dev[[v]][!is.na(dev[[v]])]
  if(!length(observed)) stop('No observed development values for ',v)
  if(v %in% continuous) { value <- median(observed); method <- 'development_median' }
  else { tab <- table(observed); value <- as.numeric(names(tab)[which.max(tab)]); method <- 'development_mode' }
  imp_log[[v]] <- data.frame(variable=v,method=method,value=value)
  dev[[v]][is.na(dev[[v]])] <- value
  val[[v]][is.na(val[[v]])] <- value
  test[[v]][is.na(test[[v]])] <- value
}
write.csv(do.call(rbind,imp_log),file.path(out,'imputation_values.csv'),row.names=FALSE)
# Fix spline boundary knots at DEVELOPMENT range, with knots at development quantiles.
# Explicit knots avoid test-set-informed spline definitions.
make_spline <- function(x, variable, df=3L) {
  inner <- as.numeric(quantile(x,probs=c(1/3,2/3),na.rm=TRUE,names=FALSE))
  boundaries <- range(x,na.rm=TRUE)
  if(length(unique(c(inner,boundaries)))!=4L) stop('Nonunique spline knots for ',variable)
  list(knots=inner,Boundary.knots=boundaries)
}
age_knots <- make_spline(dev$maternal_age,'maternal_age')
bmi_knots <- make_spline(dev$prepregnancy_bmi,'prepregnancy_bmi')
# Use natural cubic splines, with two interior knots (3 degrees of freedom).
# R's splines::ns() handles out-of-range evaluation using boundary linear extrapolation.
ns_term <- function(variable,k) sprintf('splines::ns(%s, knots=c(%s), Boundary.knots=c(%s))',
  variable,paste(format(k$knots,digits=16,scientific=FALSE),collapse=','),
  paste(format(k$Boundary.knots,digits=16,scientific=FALSE),collapse=','))
age_ns <- ns_term('maternal_age',age_knots)
bmi_ns <- ns_term('prepregnancy_bmi',bmi_knots)
forms <- list(
  original_linear=reformulate(base,'vbac'),
  age_spline=reformulate(c(setdiff(base,'maternal_age'),age_ns),'vbac'),
  bmi_spline=reformulate(c(setdiff(base,'prepregnancy_bmi'),bmi_ns),'vbac'),
  age_bmi_splines=reformulate(c(setdiff(base,c('maternal_age','prepregnancy_bmi')),age_ns,bmi_ns),'vbac'),
  height_weight_age_spline=reformulate(c(setdiff(base,c('maternal_age','prepregnancy_bmi')),age_ns),'vbac')
)
auc <- function(y,p) {
  n1 <- as.numeric(sum(y==1)); n0 <- as.numeric(sum(y==0))
  if(!n1||!n0) return(NA_real_)
  (sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0)
}
metrics <- function(y,p,model,period) {
  p <- pmin(pmax(p,1e-10),1-1e-10); lp <- qlogis(p)
  ci <- tryCatch(unname(coef(glm(y~1,offset=lp,family=binomial()))[1]),error=function(e) NA_real_)
  cs <- tryCatch(unname(coef(glm(y~lp,family=binomial()))[2]),error=function(e) NA_real_)
  data.frame(model=model,period=period,n=length(y),observed=mean(y),predicted=mean(p),
    auc=auc(y,p),brier=mean((y-p)^2),oe=sum(y)/sum(p),
    calibration_intercept=ci,calibration_slope=cs)
}
results <- list(); coefs <- list(); fits <- list(); preds <- list()
for(name in names(forms)) {
  cat('Fitting',name,'at',format(Sys.time()),'\n'); flush.console()
  fit <- glm(forms[[name]],data=dev,family=binomial(),control=glm.control(maxit=50))
  if(!fit$converged) warning('Model did not converge: ',name)
  fits[[name]] <- fit
  sm <- summary(fit)$coefficients
  coefs[[name]] <- data.frame(model=name,term=rownames(sm),estimate=sm[,1],se=sm[,2],
                             z=sm[,3],p=sm[,4],row.names=NULL)
  for(period in c('development','intermediate_2022_2023','test_2024')) {
    dat <- switch(period,development=dev,intermediate_2022_2023=val,test_2024=test)
    p <- predict(fit,newdata=dat,type='response')
    if(any(!is.finite(p))) stop('Non-finite predictions for ',name,' ',period)
    results[[paste(name,period)]] <- metrics(dat$vbac,p,name,period)
    if(period=='test_2024') preds[[name]] <- p
  }
  saveRDS(fit,file.path(out,paste0('model_',name,'.rds')))
}
write.csv(do.call(rbind,results),file.path(out,'performance_by_specification.csv'),row.names=FALSE)
write.csv(do.call(rbind,coefs),file.path(out,'coefficients.csv'),row.names=FALSE)
ref <- preds[['original_linear']]
differences <- do.call(rbind,lapply(names(preds),function(name){
  delta <- preds[[name]]-ref
  data.frame(model=name,n=length(delta),mean_signed_difference=mean(delta),
    mean_absolute_difference=mean(abs(delta)),p95_absolute_difference=unname(quantile(abs(delta),.95)),
    p99_absolute_difference=unname(quantile(abs(delta),.99)),max_absolute_difference=max(abs(delta)))
}))
write.csv(differences,file.path(out,'prediction_differences_vs_original_2024.csv'),row.names=FALSE)
# Nested likelihood-ratio tests on DEVELOPMENT data only; exploratory (not model selection on 2024).
lrt <- function(reduced,full,label) {
  a <- anova(fits[[reduced]],fits[[full]],test='Chisq')
  data.frame(comparison=label,reduced=reduced,full=full,
    deviance_reduction=a$Deviance[2],df=a$Df[2],p_value=a$`Pr(>Chi)`[2])
}
tests <- rbind(lrt('original_linear','age_spline','Nonlinear maternal age'),
               lrt('original_linear','bmi_spline','Nonlinear BMI'),
               lrt('original_linear','age_bmi_splines','Both nonlinear'))
write.csv(tests,file.path(out,'development_nonlinearity_tests.csv'),row.names=FALSE)
write.csv(data.frame(variable=c(rep('maternal_age',4),rep('prepregnancy_bmi',4)),
  knot_type=rep(c('boundary_lower','interior_1','interior_2','boundary_upper'),2),
  value=c(age_knots$Boundary.knots[1],age_knots$knots,age_knots$Boundary.knots[2],
          bmi_knots$Boundary.knots[1],bmi_knots$knots,bmi_knots$Boundary.knots[2])),
  file.path(out,'spline_knots.csv'),row.names=FALSE)
# Visualize adjusted partial-effect curves centered at development medians/modes.
pdf(file.path(out,'adjusted_age_bmi_effects.pdf'),width=9,height=5)
par(mfrow=c(1,2))
for(v in c('maternal_age','prepregnancy_bmi')) {
  x <- seq(quantile(dev[[v]],.01),quantile(dev[[v]],.99),length.out=120)
  reference <- as.data.frame(lapply(dev[,base,drop=FALSE],function(z) median(z)))
  newdat <- reference[rep(1,length(x)),,drop=FALSE]; newdat[[v]] <- x
  plot(x,predict(fits[['original_linear']],newdata=newdat,type='response'),type='l',lwd=2,
       xlab=v,ylab='Predicted VBAC probability',main=paste('Adjusted',v,'effect'))
  spline_model <- if(v=='maternal_age') 'age_spline' else 'bmi_spline'
  lines(x,predict(fits[[spline_model]],newdata=newdat,type='response'),lty=2,lwd=2)
  legend('topright',legend=c('Linear','Natural spline (3 df)'),lty=c(1,2),bty='n')
}
dev.off()
writeLines(c('R1.3 post-review exploratory sensitivity analysis',
  paste('Input:',input),paste('R:',R.version.string),
  'Development 2016-2021; intermediate 2022-2023; post-review test assessment 2024.',
  'All model specifications set before evaluating this script; do not select based on 2024 performance.',
  'Development-only median/mode imputation, as in original protocol; not a fold-based CV.',
  'Spline: natural cubic with 3 df, two internal knots at development 1/3 and 2/3 quantiles.',
  'No 2024-based knot selection. No re-estimation or updating using test data.',
  'All-three anthropometric original model remains the reference; height+weight spline is sensitivity only.',
  'Age/BMI spline curves are adjusted illustrations at fixed other predictors, not causal effects.',
  capture.output(sessionInfo())),file.path(out,'README_run.txt'))
cat('DONE. Results:',out,'\n')
