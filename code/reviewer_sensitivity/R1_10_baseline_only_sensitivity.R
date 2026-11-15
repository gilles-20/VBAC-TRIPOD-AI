#!/usr/bin/env Rscript
# Reviewer R1.10: post-review predictor-timing sensitivity (base R only).
# Compare original primary model specification with a model omitting current-pregnancy diagnoses.
# NOT a validated early-pregnancy tool; does NOT overwrite frozen models.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
input <- file.path(root,'us_birth_files/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz')
out <- Sys.getenv('R1_10_OUT', file.path(root,'REVIEWER_REVISION_2026','R1_10_BASELINE_ONLY_SENSITIVITY'))
dir.create(out, recursive=TRUE, showWarnings=FALSE)
if (!file.exists(input)) stop('Input not found: ',input)

# Mirrors original R1.3 reference model: number of prior cesareans excluded because
# cohort is restricted to exactly one previous cesarean (constant predictor).
continuous <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg',
                'prepregnancy_bmi','total_prior_live_births')
preexisting_binary <- c('previous_preterm_birth_binary','prepregnancy_diabetes_binary',
                        'prepregnancy_hypertension_binary')
gestational_binary <- c('gestational_diabetes_binary','gestational_hypertension_binary')
original_predictors <- c(continuous,preexisting_binary,gestational_binary)
baseline_predictors <- c(continuous,preexisting_binary)
required <- c('source_year','vbac',original_predictors)
cat('Reading model-ready cohort...\n')
d <- read.csv(gzfile(input),check.names=FALSE)
missing_cols <- setdiff(required,names(d))
if (length(missing_cols)) stop('Missing required columns: ',paste(missing_cols,collapse=', '))
for (v in required) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
if (anyNA(d$source_year) || anyNA(d$vbac) || !all(d$vbac %in% c(0,1)))
  stop('Year or outcome is invalid/missing; inspect data encoding.')
dev <- d[d$source_year %in% 2016:2021,required,drop=FALSE]
ival <- d[d$source_year %in% 2022:2023,required,drop=FALSE]
test <- d[d$source_year == 2024,required,drop=FALSE]
if (nrow(dev)!=499360L || nrow(ival)!=179310L || nrow(test)!=94071L)
  stop('Unexpected cohort sizes; verify temporal splits.')
rm(d); gc(verbose=FALSE)

# Impute from development only, identically for both specifications.
imp <- list()
for (v in original_predictors) {
  obs <- dev[[v]][is.finite(dev[[v]])]
  if (!length(obs)) stop('No observed values for ',v)
  if (v %in% continuous) {
    value <- median(obs); method <- 'development_median'
  } else {
    tab <- table(obs); value <- as.numeric(names(tab)[which.max(tab)])
    method <- 'development_mode'
  }
  imp[[v]] <- data.frame(variable=v,method=method,value=value,
                         missing_dev=sum(is.na(dev[[v]])),
                         missing_intermediate=sum(is.na(ival[[v]])),
                         missing_test=sum(is.na(test[[v]])))
  for (nm in c('dev','ival','test')) {
    z <- get(nm)
    z[[v]][!is.finite(z[[v]])] <- value
    assign(nm,z)
  }
}
write.csv(do.call(rbind,imp),file.path(out,'development_imputation_audit.csv'),row.names=FALSE)

auc <- function(y,p) {
  n1 <- as.double(sum(y==1)); n0 <- as.double(sum(y==0))
  if (n1==0 || n0==0) return(NA_real_)
  (sum(rank(p,ties.method='average')[y==1]) - n1*(n1+1)/2)/(n1*n0)
}
metrics <- function(y,p,model,period) {
  p <- pmin(pmax(as.numeric(p),1e-10),1-1e-10)
  lp <- qlogis(p)
  intercept <- tryCatch(unname(coef(glm(y~1,offset=lp,family=binomial()))[1]),
                        error=function(e) NA_real_)
  slope <- tryCatch(unname(coef(glm(y~lp,family=binomial()))[2]),
                    error=function(e) NA_real_)
  data.frame(model=model,period=period,n=length(y),events=sum(y),
    observed=mean(y),mean_predicted=mean(p),auc=auc(y,p),
    brier=mean((y-p)^2),oe=sum(y)/sum(p),
    calibration_intercept=intercept,calibration_slope=slope)
}
models <- list(original_refit=original_predictors,
               baseline_only=baseline_predictors)
results <- list(); test_preds <- list(); coefficients <- list()
for (name in names(models)) {
  cat('Fitting ',name,' at ',format(Sys.time()),'\n',sep='')
  fit <- glm(reformulate(models[[name]],response='vbac'),data=dev,
             family=binomial(),control=glm.control(maxit=50))
  if (!isTRUE(fit$converged)) stop('Fit did not converge: ',name)
  if (anyNA(coef(fit))) stop('Aliased coefficients in ',name,'; inspect predictors')
  s <- summary(fit)$coefficients
  coefficients[[name]] <- data.frame(model=name,term=rownames(s),
                                     estimate=s[,1],se=s[,2],row.names=NULL)
  for (period in c('development_2016_2021_apparent','intermediate_2022_2023','test_2024')) {
    z <- switch(period,development_2016_2021_apparent=dev,
                intermediate_2022_2023=ival,test_2024=test)
    p <- predict(fit,newdata=z,type='response')
    if (any(!is.finite(p))) stop('Non-finite predictions: ',name,' ',period)
    results[[paste(name,period)]] <- metrics(z$vbac,p,name,period)
    if (period=='test_2024') test_preds[[name]] <- p
  }
  saveRDS(fit,file.path(out,paste0(name,'_POST_REVIEW.rds')))
  rm(fit); gc(verbose=FALSE)
}
perf <- do.call(rbind,results)
write.csv(perf,file.path(out,'performance_by_model_period.csv'),row.names=FALSE)
write.csv(do.call(rbind,coefficients),file.path(out,'model_coefficients.csv'),row.names=FALSE)
delta <- test_preds$baseline_only-test_preds$original_refit
write.csv(data.frame(n=length(delta),mean_signed_difference=mean(delta),
  mean_absolute_difference=mean(abs(delta)),
  p95_absolute_difference=unname(quantile(abs(delta),.95)),
  p99_absolute_difference=unname(quantile(abs(delta),.99)),
  max_absolute_difference=max(abs(delta))),
  file.path(out,'test_prediction_differences.csv'),row.names=FALSE)
write.csv(data.frame(model=names(models),
  predictors=vapply(models,function(x) paste(x,collapse='; '),character(1))),
  file.path(out,'predictor_specifications.csv'),row.names=FALSE)
writeLines(c('R1.10 post-review baseline-only sensitivity analysis',
  paste('Input:',input),paste('Output:',out),
  'Reference model refitted on development 2016-2021; baseline-only removes gestational diabetes and gestational hypertension.',
  'Both specifications use identical development-only imputation.',
  'Development performance is APPARENT, not out-of-fold. Intermediate and 2024 are post-review comparisons.',
  '2024 outcomes must NOT be used to select a replacement primary model.',
  'Original frozen model files are not modified.',
  'Baseline-only does NOT establish early-pregnancy predictor availability or a validated clinical landmark.',
  'Other variables may have source-code missingness concealed by harmonisation; review original coding.',
  'Prepregnancy height/weight/BMI collinearity retained for comparability with original specification.',
  'No claims of clinical utility or treatment comparisons can be made from this analysis.',
  capture.output(sessionInfo())),file.path(out,'README_interpretation.txt'))
cat('DONE. Results: ',out,'\n',sep='')
