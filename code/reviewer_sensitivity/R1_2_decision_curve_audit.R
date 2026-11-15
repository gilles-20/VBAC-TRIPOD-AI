#!/usr/bin/env Rscript
# R1.2: transparent, descriptive decision-curve audit for VBAC success among observed TOLAC.
# This is NOT a causal comparison of TOLAC vs planned repeat cesarean.
options(stringsAsFactors=FALSE)
ROOT <- Sys.getenv('TRIPOD_ROOT', unset = '')

if (!nzchar(Sys.getenv('TRIPOD_ROOT', unset = ''))) stop('Set TRIPOD_ROOT before running this script.')
PRED <- Sys.getenv('R1_2_INPUT', file.path(ROOT,'us_birth_files/harmonised/model_results/race_inclusive_comparison_2026_08/PREDICTIONS_2024_RACE_NEUTRAL_VS_RACE_INCLUSIVE.csv.gz'))
ORIGINAL_DCA <- Sys.getenv('R1_2_ORIGINAL_DCA', file.path(ROOT,'VBAC-TRIPOD-AI/outputs/model_results/FINAL_TEST_2024_DECISION_CURVE.csv'))
OUT <- Sys.getenv('R1_2_OUT', file.path(ROOT,'REVIEWER_REVISION_2026/R1_2_DECISION_CURVE_AUDIT'))
B <- as.integer(Sys.getenv('R1_2_BOOTSTRAP_B','400'))
SEED <- as.integer(Sys.getenv('R1_2_SEED','20261114'))
if (!is.finite(B) || B<0L || B>5000L) stop('R1_2_BOOTSTRAP_B must be between 0 and 5000')
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
if (!file.exists(PRED)) stop('Prediction file not found: ',PRED)
d <- read.csv(gzfile(PRED), stringsAsFactors=FALSE)
req <- c('vbac','pred_race_neutral','pred_race_inclusive')
if (!all(req %in% names(d))) stop('Missing required columns: ',paste(setdiff(req,names(d)),collapse=', '))
if (anyNA(d[,req]) || any(!d$vbac %in% c(0,1)) || any(!is.finite(d$pred_race_neutral)) || any(!is.finite(d$pred_race_inclusive)) || any(d$pred_race_neutral<0|d$pred_race_neutral>1) || any(d$pred_race_inclusive<0|d$pred_race_inclusive>1)) stop('Invalid input values')
# Thresholds are analytic scenarios, not elicited patient preferences or validated decision thresholds.
ths <- seq(.10,.90,by=.05)
y <- as.integer(d$vbac)
p_neutral <- d$pred_race_neutral
p_inclusive <- d$pred_race_inclusive
n <- length(y)
# Conventional net benefit when the event is VBAC success and action is selecting a predicted success.
# NB = TP/n - FP/n * pt/(1-pt). Treat-all and treat-none are internal classification
# reference strategies among observed TOLAC cases, NOT actual clinical treatment policies.
calc <- function(yy,p,threshold) {
  pos <- p>=threshold
  tp <- sum(pos & yy==1)
  fp <- sum(pos & yy==0)
  (tp - fp*threshold/(1-threshold))/length(yy)
}
models <- c('race_neutral','race_inclusive','classify_all','classify_none')
get_nb <- function(yy,pn,pi,t) c(race_neutral=calc(yy,pn,t),race_inclusive=calc(yy,pi,t),classify_all=mean(yy)-(1-mean(yy))*t/(1-t),classify_none=0)
mat <- t(vapply(ths,function(t) get_nb(y,p_neutral,p_inclusive,t),numeric(4)))
res <- data.frame(threshold=rep(ths,each=4),strategy=rep(models,times=length(ths)),net_benefit=as.vector(t(mat)),n=n,events=sum(y),analysis='Descriptive hypothetical classification among observed TOLAC; not TOLAC-vs-repeat-CS utility')
write.csv(res,file.path(OUT,'descriptive_net_benefit_2024.csv'),row.names=FALSE)
# Paired nonparametric patient-level bootstrap: same resampled indices for all strategies.
# Simultaneous inference across thresholds is NOT claimed.
if (B>0L) {
 set.seed(SEED)
 draws <- vector('list',B)
 for (b in seq_len(B)) {
   idx <- sample.int(n,n,replace=TRUE)
   yy <- y[idx]; pn <- p_neutral[idx]; pi <- p_inclusive[idx]
   mm <- t(vapply(ths,function(t) get_nb(yy,pn,pi,t),numeric(4)))
   draws[[b]] <- data.frame(bootstrap=b,threshold=rep(ths,each=4),strategy=rep(models,times=length(ths)),net_benefit=as.vector(t(mm)))
   if (b%%50==0L) cat('Bootstrap',b,'/',B,'\n')
 }
 boot <- do.call(rbind,draws)
 write.csv(boot,file.path(OUT,'bootstrap_net_benefit_draws_2024.csv'),row.names=FALSE)
 key <- paste(boot$threshold,boot$strategy)
 spl <- split(boot$net_benefit,key)
 ci <- t(vapply(spl,function(x) unname(quantile(x,c(.025,.975),na.rm=TRUE)),numeric(2)))
 k <- paste(res$threshold,res$strategy)
 res$ci_low <- ci[k,1];res$ci_high <- ci[k,2]
 write.csv(res,file.path(OUT,'descriptive_net_benefit_2024_with_ci.csv'),row.names=FALSE)
 # Paired difference inclusive minus neutral (exploratory only).
 diffs <- lapply(ths,function(t) {
   a <- boot[boot$threshold==t,]
   z <- reshape(a[a$strategy %in% c('race_neutral','race_inclusive'),],idvar='bootstrap',timevar='strategy',direction='wide')
   dd <- z$net_benefit.race_inclusive-z$net_benefit.race_neutral
   data.frame(threshold=t,delta=mat[which(ths==t),'race_inclusive']-mat[which(ths==t),'race_neutral'],ci_low=unname(quantile(dd,.025)),ci_high=unname(quantile(dd,.975)))
 })
 write.csv(do.call(rbind,diffs),file.path(OUT,'exploratory_paired_nb_difference_2024.csv'),row.names=FALSE)
}
# Save the original DCA table as an untouched copy for audit, if available.
if (file.exists(ORIGINAL_DCA)) {
 old <- tryCatch(read.csv(ORIGINAL_DCA,stringsAsFactors=FALSE),error=function(e)e)
 if (inherits(old,'error')) warning('Could not parse original DCA: ',conditionMessage(old)) else {
  write.csv(data.frame(column=names(old),class=vapply(old,function(z)paste(class(z),collapse='/'),character(1))),file.path(OUT,'original_dca_columns.csv'),row.names=FALSE)
  file.copy(ORIGINAL_DCA,file.path(OUT,'original_dca_unmodified.csv'),overwrite=TRUE)
 }
} else warning('Original DCA file not found; primary prediction audit still completed')
# PDF only, safe for headless cluster
pdf(file.path(OUT,'descriptive_decision_curves_2024.pdf'),width=9,height=6,useDingbats=FALSE)
cols <- c(race_neutral='#155E75',race_inclusive='#C2410C',classify_all='#6B7280',classify_none='#111827')
lims <- range(res$net_benefit[is.finite(res$net_benefit)]);plot(NA,xlim=range(ths),ylim=lims,xlab='Hypothetical probability threshold',ylab='Conventional net benefit',main='Descriptive classification net benefit within observed TOLAC')
for (s in models) {z <- res[res$strategy==s,];lines(z$threshold,z$net_benefit,col=cols[s],lwd=if(s %in% c('race_neutral','race_inclusive')) 2.5 else 1.5,lty=if(s=='classify_none') 3 else if(s=='classify_all') 2 else 1)}
legend('topright',legend=gsub('_',' ',models),col=cols,lwd=2,lty=c(1,1,2,3),bty='n',cex=.85)
mtext('Not a comparison of TOLAC versus planned repeat cesarean; thresholds not clinically validated',side=1,line=4,cex=.75)
dev.off()
writeLines(c('R1.2 descriptive decision curve audit','Input saved 2024 predictions: ',PRED,paste('n=',n,' events=',sum(y)),paste('Bootstrap replicates:',B),'Definition: event=successful VBAC; hypothetical action=classify as likely success if predicted probability >= threshold.','Net benefit uses TP/n - FP/n * threshold/(1-threshold).','Classify-all and classify-none are hypothetical labels applied to the selected TOLAC cohort, not observed delivery-management alternatives.','No repeat-cesarean counterfactual outcomes, harms, treatment effects, or patient preference weights are available.','Thus these curves do NOT estimate clinical net benefit of recommending TOLAC versus planned repeat cesarean.','Thresholds 0.10-0.90 are illustrative; not clinically validated.','The race-inclusive model and all post-review analysis are exploratory.','Bootstrap intervals are pointwise, not simultaneous, and do not account for model-development uncertainty.','Consider removing DCA from main manuscript if its intended interpretation is a treatment-policy comparison.','Original DCA (if found) copied unchanged; inspect it before deciding if reported definitions match.','',capture.output(sessionInfo())),file.path(OUT,'README_interpretation.txt'))
cat('Completed R1.2 audit in:',OUT,'\n')
