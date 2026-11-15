#!/usr/bin/env Rscript
# Post-review predictor availability / prediction-landmark audit.
# Does not refit models or claim availability timing from birth certificates.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('TRIPOD_ROOT', unset = '')

if (!nzchar(Sys.getenv('TRIPOD_ROOT', unset = ''))) stop('Set TRIPOD_ROOT before running this script.')
infile <- Sys.getenv('VBAC_MODEL_READY', file.path(root,'us_birth_files/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz'))
out <- Sys.getenv(
  'R1_1_OUT',
  file.path(root,'REVIEWER_REVISION_2026','R1_1_R1_10_LANDMARK')
)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if (!file.exists(infile)) stop('Missing model-ready file: ', infile)
preds <- c('maternal_age','maternal_height_cm','prepregnancy_weight_kg','prepregnancy_bmi','total_prior_live_births','n_previous_cesareans','previous_preterm_birth_binary','prepregnancy_diabetes_binary','prepregnancy_hypertension_binary','gestational_diabetes_binary','gestational_hypertension_binary')
landmark <- c('Baseline','Baseline','Baseline','Baseline','Baseline','Baseline','Baseline','Baseline','Baseline','Current pregnancy','Current pregnancy')
known <- c(rep('Expected before delivery; timing not verified in natality file',9),rep('Diagnosis may arise during pregnancy; date of diagnosis unavailable',2))
write.csv(data.frame(predictor=preds,classification=landmark,availability_note=known),file.path(out,'predictor_landmark_classification.csv'),row.names=FALSE)
# Scan the compressed CSV in chunks without loading the full file.
con <- gzfile(infile,open='rt'); on.exit(close(con),add=TRUE)
header_line <- readLines(con,n=1L,warn=FALSE)
header <- names(read.csv(text=header_line, nrows=0L, check.names=FALSE))
message('Parsed ', length(header), ' column names; first columns: ', paste(head(header, 5), collapse=', '))
required <- c('source_year',preds)
if (!all(required %in% header)) stop('Required columns absent: ',paste(setdiff(required,header),collapse=', '))
idx <- match(required,header)
stat <- list(); count <- 0L
repeat {
  lines <- readLines(con,n=20000L,warn=FALSE)
  if (!length(lines)) break
  # Parse original quoted header so names and fields remain aligned.
  tmp <- read.csv(text=paste(c(header_line,lines),collapse='\n'),check.names=FALSE,na.strings=c('','NA'),colClasses='character')
  yr <- tmp[['source_year']]
  for (year in unique(yr)) {
    if (is.na(year) || year=='') next
    subset <- tmp[yr==year,,drop=FALSE]
    if (is.null(stat[[year]])) stat[[year]] <- list(n=0,missing=setNames(rep(0,length(preds)),preds), positive=setNames(rep(0,length(preds)),preds), observed=setNames(rep(0,length(preds)),preds))
    s <- stat[[year]]; s$n <- s$n+nrow(subset)
    for (v in preds) {
      x <- trimws(subset[[v]])
      miss <- is.na(x)|x==''|x %in% c('NA','NaN')
      s$missing[[v]] <- s$missing[[v]]+sum(miss)
      s$observed[[v]] <- s$observed[[v]]+sum(!miss)
      if (v %in% c('gestational_diabetes_binary','gestational_hypertension_binary')) s$positive[[v]] <- s$positive[[v]]+sum(x=='1',na.rm=TRUE)
    }
    stat[[year]] <- s
  }
  count <- count+length(lines)
  if (count %% 200000L < 20000L) message('Scanned ',format(count,big.mark=','),' rows')
}
rows <- do.call(rbind,lapply(names(stat),function(year) {
  s <- stat[[year]]
  data.frame(year=year,predictor=preds,n=s$n,missing=as.numeric(s$missing[preds]),missing_pct=100*as.numeric(s$missing[preds])/s$n,positive=as.numeric(s$positive[preds]),observed=as.numeric(s$observed[preds]),stringsAsFactors=FALSE)
}))
rows <- rows[order(rows$year,match(rows$predictor,preds)),]
write.csv(rows,file.path(out,'predictor_missingness_by_year.csv'),row.names=FALSE)
summary <- aggregate(cbind(n,missing,positive,observed)~predictor,data=rows,FUN=sum)
summary$missing_pct <- 100*summary$missing/summary$n
write.csv(summary,file.path(out,'predictor_summary_all_years.csv'),row.names=FALSE)
writeLines(c('R1.1 / R1.10: predictor availability audit','',paste('Input:',infile),paste('Rows scanned:',count),'The study estimates VBAC success conditional on TOLAC undertaken.','This audit documents which predictors are present and their missingness; it cannot establish diagnosis date or the time when a predictor became clinically available.','Gestational diabetes and hypertension are current-pregnancy diagnoses; avoid describing the existing model as an early-pregnancy counseling tool.','Do not assert that all inputs were available before a specific decision landmark without independent clinical timestamp evidence.','Consider an explicitly labeled post-hoc baseline-only sensitivity model if the intended landmark is early pregnancy.','The number of prior cesareans may be constant due to the exactly-one-prior-CS inclusion criterion; check the original fitting script for dropped terms.'),file.path(out,'README_interpretation.txt'))
cat('Completed. Outputs: ',out,'\n',sep='')
