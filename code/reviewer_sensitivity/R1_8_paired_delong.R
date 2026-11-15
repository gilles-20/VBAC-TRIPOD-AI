#!/usr/bin/env Rscript
# Reviewer R1.8: paired DeLong comparison using ORIGINAL SAVED 2024 predictions.
# Does not refit models or modify original results.
options(stringsAsFactors = FALSE)
ROOT <- Sys.getenv('TRIPOD_ROOT', unset = '')

if (!nzchar(Sys.getenv('TRIPOD_ROOT', unset = ''))) stop('Set TRIPOD_ROOT before running this script.')
INPUT <- Sys.getenv('R1_8_INPUT', file.path(ROOT, 'us_birth_files/harmonised/model_results/race_inclusive_comparison_2026_08/PREDICTIONS_2024_RACE_NEUTRAL_VS_RACE_INCLUSIVE.csv.gz'))
OUT <- Sys.getenv('R1_8_OUT', file.path(ROOT, 'REVIEWER_REVISION_2026/R1_8_PAIRED_DELONG'))
if (!requireNamespace('pROC', quietly=TRUE)) stop('Package pROC is required. Install it in your R environment using install.packages("pROC") or your HPC module/package manager.')
if (!file.exists(INPUT)) stop('Saved paired prediction file not found: ', INPUT, '\nRun original 02_vbac_race_inclusive_posthoc.R or locate its existing saved prediction file; do NOT refit automatically.')
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
d <- read.csv(gzfile(INPUT), stringsAsFactors=FALSE)
needed <- c('vbac','pred_race_neutral','pred_race_inclusive','race_ethnicity_group')
if (!all(needed %in% names(d))) stop('Missing columns: ', paste(setdiff(needed,names(d)), collapse=', '))
if (nrow(d)!=94071L) warning('2024 n differs from the previously documented 94,071: ', nrow(d))
if (anyNA(d[,needed]) || any(!d$vbac %in% c(0,1)) || any(!is.finite(d$pred_race_neutral)) || any(!is.finite(d$pred_race_inclusive)) || any(d$pred_race_neutral < 0 | d$pred_race_neutral > 1) || any(d$pred_race_inclusive < 0 | d$pred_race_inclusive > 1)) stop('Invalid/missing outcomes or predictions; audit input before proceeding.')
if (length(unique(d$vbac))!=2) stop('Both outcome classes required.')
# pROC explicitly fixes the direction: larger prediction -> greater chance of VBAC (case=1).
roc_n <- pROC::roc(d$vbac, d$pred_race_neutral, levels=c(0,1), direction='<', quiet=TRUE, auc=TRUE)
roc_i <- pROC::roc(d$vbac, d$pred_race_inclusive, levels=c(0,1), direction='<', quiet=TRUE, auc=TRUE)
test <- pROC::roc.test(roc_i, roc_n, method='delong', paired=TRUE, alternative='two.sided')
auc_n <- as.numeric(pROC::auc(roc_n)); auc_i <- as.numeric(pROC::auc(roc_i))
ci_n <- as.numeric(pROC::ci.auc(roc_n, method='delong'))
ci_i <- as.numeric(pROC::ci.auc(roc_i, method='delong'))
delta <- auc_i - auc_n
# pROC's paired DeLong test returns a standard error for the AUC difference via its statistic:
# z = delta / SE; using this SE yields the standard normal 95% CI.
z <- as.numeric(test$statistic)
se <- if (is.finite(z) && abs(z)>1e-10) abs(delta/z) else NA_real_
if (!is.finite(se)) stop('Could not recover paired DeLong SE; inspect roc.test output.')
ci_delta <- delta + c(-1,1)*qnorm(0.975)*se
result <- data.frame(n=nrow(d), events=sum(d$vbac==1), auc_neutral=auc_n, auc_neutral_ci_low=ci_n[1], auc_neutral_ci_high=ci_n[3], auc_inclusive=auc_i, auc_inclusive_ci_low=ci_i[1], auc_inclusive_ci_high=ci_i[3], delta_inclusive_minus_neutral=delta, delta_ci_low=ci_delta[1], delta_ci_high=ci_delta[2], delta_se=se, delong_z=z, delong_p=as.numeric(test$p.value), method='Paired DeLong; fixed direction; post-hoc exploratory comparison')
write.csv(result, file.path(OUT,'paired_delong_2024.csv'),row.names=FALSE)
# Subgroup discrimination descriptive; group labels are not harmonized/merged here.
subgroups <- lapply(split(seq_len(nrow(d)), as.character(d$race_ethnicity_group)), function(idx) {
  x <- d[idx,]
  if (length(unique(x$vbac))<2L || nrow(x)<100L) return(data.frame(group=as.character(x$race_ethnicity_group[1]),n=nrow(x),events=sum(x$vbac==1),auc_neutral=NA_real_,auc_inclusive=NA_real_,delta=NA_real_))
  a <- as.numeric(pROC::auc(pROC::roc(x$vbac,x$pred_race_neutral,levels=c(0,1),direction='<',quiet=TRUE)))
  b <- as.numeric(pROC::auc(pROC::roc(x$vbac,x$pred_race_inclusive,levels=c(0,1),direction='<',quiet=TRUE)))
  data.frame(group=as.character(x$race_ethnicity_group[1]),n=nrow(x),events=sum(x$vbac==1),auc_neutral=a,auc_inclusive=b,delta=b-a)
})
write.csv(do.call(rbind,subgroups),file.path(OUT,'subgroup_auc_descriptive_2024.csv'),row.names=FALSE)
# Average calibration and within-group calibration slopes; descriptive, no uncertainty here.
clip <- function(p) pmax(1e-8,pmin(1-1e-8,p))
cal <- lapply(split(seq_len(nrow(d)),as.character(d$race_ethnicity_group)),function(idx){
 x <- d[idx,]; y <- x$vbac
 slope <- function(p) {lp <- qlogis(clip(p)); tryCatch(unname(coef(glm(y~lp,family=binomial()))[2]),error=function(e) NA_real_)}
 data.frame(group=as.character(x$race_ethnicity_group[1]),n=nrow(x),observed=mean(y),pred_neutral=mean(x$pred_race_neutral),pred_inclusive=mean(x$pred_race_inclusive),oe_neutral=mean(y)/mean(x$pred_race_neutral),oe_inclusive=mean(y)/mean(x$pred_race_inclusive),slope_neutral=slope(x$pred_race_neutral),slope_inclusive=slope(x$pred_race_inclusive))
})
write.csv(do.call(rbind,cal),file.path(OUT,'subgroup_calibration_descriptive_2024.csv'),row.names=FALSE)
writeLines(c(paste('Input:',INPUT),paste('Rows:',nrow(d)),'Predictions are original saved 2024 paired predictions.','The inclusive model is post-hoc/exploratory; this is NOT independent validation of that model.','Paired DeLong tests AUC difference, not clinical utility or fairness.','Subgroup summaries are descriptive and not multiplicity-adjusted.',capture.output(sessionInfo())),file.path(OUT,'README_run.txt'))
cat('Completed R1.8. Output:',OUT,'\n');print(result,row.names=FALSE)
