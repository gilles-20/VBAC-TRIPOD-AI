#!/usr/bin/env Rscript
# Reviewer R1.9: subgroup calibration with bootstrap uncertainty.
# Uses ORIGINAL SAVED 2024 predictions. No model refitting or selection.
# Base R + recommended 'splines'; headless-safe PDF plots.
options(stringsAsFactors=FALSE, warn=1)
ROOT <- Sys.getenv('TRIPOD_ROOT', unset = '')

if (!nzchar(Sys.getenv('TRIPOD_ROOT', unset = ''))) stop('Set TRIPOD_ROOT before running this script.')
INPUT <- Sys.getenv('R1_9_INPUT', file.path(ROOT,'us_birth_files/harmonised/model_results/race_inclusive_comparison_2026_08/PREDICTIONS_2024_RACE_NEUTRAL_VS_RACE_INCLUSIVE.csv.gz'))
OUT <- Sys.getenv('R1_9_OUT',file.path(ROOT,'REVIEWER_REVISION_2026/R1_9_SUBGROUP_CALIBRATION'))
B <- as.integer(Sys.getenv('R1_9_BOOTSTRAPS','400'))
SEED <- as.integer(Sys.getenv('R1_9_SEED','20261114'))
if (!is.finite(B) || B<100L) stop('R1_9_BOOTSTRAPS must be >= 100.')
if (!file.exists(INPUT)) stop('Prediction file not found: ', INPUT)
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
d <- read.csv(gzfile(INPUT),stringsAsFactors=FALSE)
needed <- c('vbac','pred_race_neutral','pred_race_inclusive','race_ethnicity_group')
if (!all(needed %in% names(d))) stop('Missing columns: ',paste(setdiff(needed,names(d)),collapse=', '))
if (anyNA(d[,needed]) || any(!d$vbac %in% c(0,1)) || any(!is.finite(d$pred_race_neutral)) || any(!is.finite(d$pred_race_inclusive)) || any(d$pred_race_neutral<=0 | d$pred_race_neutral>=1) || any(d$pred_race_inclusive<=0 | d$pred_race_inclusive>=1)) stop('Invalid input: inspect missingness, outcome and predictions.')
if (nrow(d)!=94071L) warning('Expected 94,071 rows; found ',nrow(d))
main_groups <- c('NH_Black','NH_White','Hispanic','NH_Asian')
if (!all(main_groups %in% unique(d$race_ethnicity_group))) stop('Expected groups not found: ',paste(setdiff(main_groups,unique(d$race_ethnicity_group)),collapse=', '))
models <- c(race_neutral='pred_race_neutral',race_inclusive='pred_race_inclusive')
clip <- function(p) pmin(1-1e-7,pmax(1e-7,p))
# Calibration intercept: logit(y) ~ offset(logit(p)); slope: logit(y) ~ logit(p)
calc_metrics <- function(y,p) {
 lp <- qlogis(clip(p))
 fit_i <- suppressWarnings(tryCatch(glm(y~1+offset(lp),family=binomial()),error=function(e) NULL))
 fit_s <- suppressWarnings(tryCatch(glm(y~lp,family=binomial()),error=function(e) NULL))
 c(observed=mean(y),mean_predicted=mean(p),difference_pp=100*(mean(p)-mean(y)),
   oe=mean(y)/mean(p),calibration_intercept=if(is.null(fit_i)) NA_real_ else unname(coef(fit_i)[1]),
   calibration_slope=if(is.null(fit_s)) NA_real_ else unname(coef(fit_s)[2]))
}
# Nonparametric bootstrap resamples patients WITHIN each subgroup. Both model predictions
# remain paired per patient. CIs are percentile intervals; not multiplicity adjusted.
set.seed(SEED)
rows <- list(); draws <- list(); k <- 0L
for (group in unique(c(main_groups,sort(setdiff(unique(d$race_ethnicity_group),main_groups))))) {
 x <- d[d$race_ethnicity_group==group,,drop=FALSE]; n <- nrow(x); y <- x$vbac
 for (model in names(models)) {
  p <- x[[models[[model]]]]
  m <- calc_metrics(y,p)
  boot <- matrix(NA_real_,nrow=B,ncol=length(m),dimnames=list(NULL,names(m)))
  for (b in seq_len(B)) {
   ix <- sample.int(n,n,replace=TRUE)
   if(length(unique(y[ix]))<2L) next
   boot[b,] <- calc_metrics(y[ix],p[ix])
  }
  low <- apply(boot,2,function(z) if(sum(is.finite(z))>=B*.9) unname(quantile(z[is.finite(z)],.025)) else NA_real_)
  high <- apply(boot,2,function(z) if(sum(is.finite(z))>=B*.9) unname(quantile(z[is.finite(z)],.975)) else NA_real_)
  k <- k+1L
  rows[[k]] <- data.frame(group=group,model=model,n=n,events=sum(y),
    observed=m['observed'],mean_predicted=m['mean_predicted'],difference_pp=m['difference_pp'],
    oe=m['oe'],calibration_intercept=m['calibration_intercept'],calibration_slope=m['calibration_slope'],
    difference_pp_ci_low=low['difference_pp'],difference_pp_ci_high=high['difference_pp'],
    oe_ci_low=low['oe'],oe_ci_high=high['oe'],
    intercept_ci_low=low['calibration_intercept'],intercept_ci_high=high['calibration_intercept'],
    slope_ci_low=low['calibration_slope'],slope_ci_high=high['calibration_slope'],
    bootstrap_replicates=B,stringsAsFactors=FALSE)
  draws[[paste(group,model,sep='__')]] <- data.frame(group=group,model=model,replicate=seq_len(B),
      difference_pp=boot[,'difference_pp'],oe=boot[,'oe'],intercept=boot[,'calibration_intercept'],slope=boot[,'calibration_slope'])
 }
 cat('Finished group:',group,'n=',n,'\n')
}
summary <- do.call(rbind,rows);rownames(summary)<-NULL
write.csv(summary,file.path(OUT,'subgroup_calibration_bootstrap_2024.csv'),row.names=FALSE)
write.csv(do.call(rbind,draws),file.path(OUT,'bootstrap_draws_2024.csv'),row.names=FALSE)
# Equal-frequency bins within each subgroup and each model, with Wilson binomial CI.
wilson <- function(success,n,z=qnorm(.975)) {
 phat<-success/n;den<-1+z^2/n;ctr<-(phat+z^2/(2*n))/den
 rad<-z*sqrt(phat*(1-phat)/n+z^2/(4*n^2))/den
 c(max(0,ctr-rad),min(1,ctr+rad))
}
bins <- list(); curves <- list(); q <- 0L
for (group in main_groups) {
 x <- d[d$race_ethnicity_group==group,,drop=FALSE]
 for (model in names(models)) {
  p <- x[[models[[model]]]];y <- x$vbac;n <- length(y)
  # rank-based deciles ensure no empty bins despite ties
  ix <- order(p); bin_id <- integer(n);bin_id[ix] <- pmin(10L,ceiling(seq_len(n)*10/n))
  for (j in seq_len(10L)) {
   keep <- which(bin_id==j);nn<-length(keep); ci<-wilson(sum(y[keep]),nn)
   q<-q+1L
   bins[[q]]<-data.frame(group=group,model=model,bin=j,n=nn,events=sum(y[keep]),
     mean_predicted=mean(p[keep]),observed=mean(y[keep]),observed_ci_low=ci[1],observed_ci_high=ci[2])
  }
  # Flexible smooth diagnostic curve: natural spline of logit(prediction).
  # Pointwise model-based 95% CIs, not simultaneous bands or external validation.
  lp <- qlogis(clip(p)); lo<-unname(quantile(lp,.01));hi<-unname(quantile(lp,.99))
  grid_lp <- seq(lo,hi,length.out=120L)
  smooth <- tryCatch({
    fit <- suppressWarnings(glm(y~splines::ns(lp,df=4),family=binomial()))
    newdata <- data.frame(lp=grid_lp)
    pred <- predict(fit,newdata=newdata,type='link',se.fit=TRUE)
    data.frame(group=group,model=model,predicted=plogis(grid_lp),
      fitted=plogis(pred$fit),ci_low=plogis(pred$fit-1.96*pred$se.fit),
      ci_high=plogis(pred$fit+1.96*pred$se.fit))
  },error=function(e){warning('Smooth failed for ',group,' ',model,': ',conditionMessage(e));NULL})
  curves[[paste(group,model,sep='__')]]<-smooth
 }
}
bin_df<-do.call(rbind,bins);write.csv(bin_df,file.path(OUT,'subgroup_decile_calibration_2024.csv'),row.names=FALSE)
curve_df<-do.call(rbind,curves)
if(!is.null(curve_df)) write.csv(curve_df,file.path(OUT,'subgroup_smooth_calibration_2024.csv'),row.names=FALSE)
# Headless-safe PDF: 4 pages (one per major subgroup), 2 panels (one per model).
pdf(file.path(OUT,'subgroup_calibration_curves_2024.pdf'),width=11,height=5.8,onefile=TRUE)
for(group in main_groups) {
 par(mfrow=c(1,2),mar=c(4.5,4.5,3.8,1.2),oma=c(0,0,2,0))
 for(model in names(models)) {
  z<-bin_df[bin_df$group==group & bin_df$model==model,]
  s<-if(is.null(curve_df)) NULL else curve_df[curve_df$group==group & curve_df$model==model,]
  plot(NA,xlim=c(0,1),ylim=c(0,1),xlab='Mean predicted VBAC probability',ylab='Observed VBAC proportion',
       main=if(model=='race_neutral') 'Race-neutral model' else 'Exploratory race-inclusive model')
  abline(0,1,lty=2,col='gray50')
  if(!is.null(s) && nrow(s)>0L) {
   polygon(c(s$predicted,rev(s$predicted)),c(s$ci_low,rev(s$ci_high)),border=NA,col=adjustcolor('steelblue',alpha.f=.18))
   lines(s$predicted,s$fitted,col='steelblue4',lwd=2)
  }
  segments(z$mean_predicted,z$observed_ci_low,z$mean_predicted,z$observed_ci_high,col='gray45')
  points(z$mean_predicted,z$observed,pch=19,col='firebrick',cex=.85)
  legend('topleft',c('Ideal','Spline smooth (pointwise 95% CI)','Deciles (Wilson 95% CI)'),
         lty=c(2,1,NA),pch=c(NA,NA,19),col=c('gray50','steelblue4','firebrick'),bty='n',cex=.75)
 }
 mtext(paste0(group,' (n=',sum(d$race_ethnicity_group==group),')'),outer=TRUE,cex=1.1,font=2)
}
dev.off()
writeLines(c('R1.9 subgroup calibration sensitivity (post-review exploratory analysis)',
 paste('Input:',INPUT),paste('N:',nrow(d)),paste('Bootstrap resamples per group/model:',B),paste('Seed:',SEED),
 'Primary subgroups: NH_Black, NH_White, Hispanic, NH_Asian.',
 'All other groups included in bootstrap summary; small groups exploratory.',
 'Nonparametric patient-level bootstrap within groups; percentile 95% CIs for calibration metrics.',
 'Confidence intervals are descriptive, not multiplicity-adjusted.',
 'Deciles: equal-frequency by rank within subgroup/model; Wilson 95% binomial intervals.',
 'Spline curves: logistic regression on natural spline of logit(prediction), df=4; model-based pointwise 95% CIs.',
 'Spline curves plotted over subgroup/model 1st–99th percentile of predicted log-odds.',
 'Caution: spline uncertainty does not account for uncertainty in the original fitted prediction models.',
 'All analyses reuse the same saved 2024 predictions; no original model refitting.',
 'Race-inclusive model is post hoc and exploratory; no causal/fairness inference.',
 capture.output(sessionInfo())),file.path(OUT,'README_run.txt'))
cat('Completed R1.9; outputs in:',OUT,'\n')
