#!/usr/bin/env Rscript
# FIXED AUC arithmetic: cast counts to double to avoid 32-bit integer overflow.
# Reviewer R1.7: fold-specific imputation and missing race/ethnicity sensitivity.
# Post-review analysis; no original results are overwritten. Base R only.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
input <- file.path(root,'us_birth_files/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz')
out <- Sys.getenv(
  'R1_7_OUT',
  file.path(root,'REVIEWER_REVISION_2026','R1_7_CV_IMPUTATION_MISSING_RACE')
)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(file.exists(input))
d <- read.csv(gzfile(input),check.names=FALSE)
year <- if ('source_year' %in% names(d)) 'source_year' else 'birth_year'
core <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg','prepregnancy_bmi',
          'total_prior_live_births','n_previous_cesareans','previous_preterm_birth_binary',
          'prepregnancy_diabetes_binary','prepregnancy_hypertension_binary',
          'gestational_diabetes_binary','gestational_hypertension_binary')
needed <- c(core,'vbac',year,'race_ethnicity_group')
if (!all(needed %in% names(d))) stop('Missing columns: ',paste(setdiff(needed,names(d)),collapse=', '))
for (v in c(core,'vbac',year)) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
if (anyNA(d$vbac) || !all(d$vbac %in% c(0,1))) stop('vbac must be complete and binary')
if (anyNA(d[[year]])) stop('Missing year values')
race <- trimws(as.character(d$race_ethnicity_group))
race[is.na(race)|race==''] <- 'Missing'
d$race_ethnicity_group <- race
miss_race <- tolower(race) %in% c('missing','unknown','not stated','not reported')
dev <- d[d[[year]] %in% 2016:2021,,drop=FALSE]
val <- d[d[[year]] %in% 2022:2023,,drop=FALSE]
test <- d[d[[year]] == 2024,,drop=FALSE]
stopifnot(nrow(dev)>0,nrow(val)>0,nrow(test)>0)
cat('N development:',nrow(dev),' validation:',nrow(val),' test:',nrow(test),'\n')
# The constant number of prior cesareans is removed using DEVELOPMENT data only.
active <- core[vapply(core,function(v) length(unique(na.omit(dev[[v]])))>1L,logical(1))]
removed <- setdiff(core,active)
write.csv(data.frame(variable=core,used=core %in% active),file.path(out,'predictor_audit.csv'),row.names=FALSE)
continuous <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg','prepregnancy_bmi',
                'total_prior_live_births','n_previous_cesareans')
learn_imputation <- function(x, vars) {
  values <- vapply(vars,function(v) {
    z <- x[[v]]; z <- z[!is.na(z)]
    if (!length(z)) stop('No observed values in training set for ',v)
    if (v %in% continuous) median(z) else {
      tb <- table(z); as.numeric(names(tb)[which.max(tb)])
    }
  },numeric(1))
  values
}
apply_imputation <- function(x, values) {
  for (v in names(values)) x[[v]][is.na(x[[v]])] <- values[[v]]
  x
}
auc <- function(y,p) {
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p, ties.method = 'average')
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
metrics <- function(y,p,label,period) {
  ok <- is.finite(p)&is.finite(y); y<-y[ok];p<-p[ok]
  p <- pmin(pmax(p,1e-10),1-1e-10)
  lp <- qlogis(p)
  ci <- tryCatch(unname(coef(glm(y~1,offset=lp,family=binomial()))[1]),error=function(e) NA_real_)
  cs <- tryCatch(unname(coef(glm(y~lp,family=binomial()))[2]),error=function(e) NA_real_)
  data.frame(analysis=label,period=period,n=length(y),events=sum(y),observed=mean(y),
             predicted=mean(p),auc=auc(y,p),brier=mean((y-p)^2),oe=sum(y)/sum(p),
             calibration_intercept=ci,calibration_slope=cs)
}
# Fixed, year-and-outcome-stratified folds; imputation refit inside each fold.
k <- 5L; seed <- 20260808L
set.seed(seed)
strata <- interaction(dev[[year]],dev$vbac,drop=TRUE)
fold <- integer(nrow(dev))
for (s in levels(strata)) {
  idx <- sample(which(strata==s))
  fold[idx] <- rep(seq_len(k),length.out=length(idx))
}
stopifnot(all(fold %in% seq_len(k)))
form <- reformulate(active,response='vbac')
oof <- rep(NA_real_,nrow(dev)); imp_audit <- list(); fold_audit <- list()
for (f in seq_len(k)) {
  cat('Fold',f,'/',k,'\n'); flush.console()
  tr <- dev[fold!=f,,drop=FALSE]; va <- dev[fold==f,,drop=FALSE]
  imp <- learn_imputation(tr,active)
  imp_audit[[f]] <- data.frame(fold=f,variable=names(imp),imputed_value=as.numeric(imp),
                               method=ifelse(names(imp) %in% continuous,'training_fold_median','training_fold_mode'))
  tr <- apply_imputation(tr,imp); va <- apply_imputation(va,imp)
  fit <- glm(form,data=tr,family=binomial(),control=glm.control(maxit=50))
  if (!fit$converged) warning('Nonconvergence fold ',f)
  oof[fold==f] <- predict(fit,newdata=va,type='response')
  fold_audit[[f]] <- data.frame(fold=f,n_train=nrow(tr),n_holdout=nrow(va),
                                 events_holdout=sum(va$vbac),converged=fit$converged)
}
stopifnot(all(is.finite(oof)))
write.csv(do.call(rbind,imp_audit),file.path(out,'fold_imputation_values.csv'),row.names=FALSE)
write.csv(do.call(rbind,fold_audit),file.path(out,'fold_audit.csv'),row.names=FALSE)
# Fit final model using development-only imputation and frozen formula; no temporal leakage.
imp <- learn_imputation(dev,active)
write.csv(data.frame(variable=names(imp),value=as.numeric(imp)),
          file.path(out,'development_imputation_values.csv'),row.names=FALSE)
fit <- glm(form,data=apply_imputation(dev,imp),family=binomial(),control=glm.control(maxit=50))
if (!fit$converged) warning('Final development model did not converge')
pval <- predict(fit,newdata=apply_imputation(val,imp),type='response')
ptest <- predict(fit,newdata=apply_imputation(test,imp),type='response')
# Race is an AUDIT variable, not a predictor. Excluding missing race means evaluation
# on a subset of the SAME model predictions; no race-specific model refitting.
perf <- rbind(
  metrics(dev$vbac,oof,'all_records','development_5fold_oof'),
  metrics(dev$vbac[!tolower(dev$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          oof[!tolower(dev$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          'exclude_missing_race','development_5fold_oof'),
  metrics(val$vbac,pval,'all_records','intermediate_2022_2023'),
  metrics(val$vbac[!tolower(val$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          pval[!tolower(val$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          'exclude_missing_race','intermediate_2022_2023'),
  metrics(test$vbac,ptest,'all_records','test_2024'),
  metrics(test$vbac[!tolower(test$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          ptest[!tolower(test$race_ethnicity_group) %in% c('missing','unknown','not stated','not reported')],
          'exclude_missing_race','test_2024')
)
write.csv(perf,file.path(out,'performance_missing_race_sensitivity.csv'),row.names=FALSE)
write.csv(data.frame(period=c('development_2016_2021','intermediate_2022_2023','test_2024'),
                     n=c(nrow(dev),nrow(val),nrow(test)),
                     missing_race_n=c(sum(tolower(dev$race_ethnicity_group)=='missing'),
                                      sum(tolower(val$race_ethnicity_group)=='missing'),
                                      sum(tolower(test$race_ethnicity_group)=='missing'))),
          file.path(out,'race_missingness_by_period.csv'),row.names=FALSE)
writeLines(c('R1.7 post-review audit. Original models and files unchanged.',
             paste('Data:',input),paste('R:',R.version.string),
             paste('CV seed:',seed,'folds:',k),paste('Zero-variance variables removed:',paste(removed,collapse=', ')),
             'Five-fold out-of-fold predictions: imputation recalculated in each training fold.',
             'Race/ethnicity Missing is a data-quality category, not a demographic group.',
             'Missing-race sensitivity excludes those records only at evaluation; model is not refitted.',
             '2024 is reused in post-review sensitivity analysis and is not an untouched test.',
             'Check against submitted model outputs; this script independently refits the model.',
             capture.output(sessionInfo())),file.path(out,'README_run.txt'))
cat('Completed. Outputs:',out,'\n')
