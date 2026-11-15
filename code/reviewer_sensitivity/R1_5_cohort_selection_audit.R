#!/usr/bin/env Rscript
# R1.5: audit the existing frozen cohort flow and clarify its final exclusion.
# Does not overwrite original extraction, model files, or flow tables.
options(stringsAsFactors=FALSE, scipen=999)
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
harm <- file.path(root,'us_birth_files','harmonised')
out <- Sys.getenv(
  'R1_5_OUT',
  file.path(root,'REVIEWER_REVISION_2026','R1_5_COHORT_FLOW')
)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
original <- file.path(harm,'natality_tripod_flow_2016_2024_TOTAL.csv')
if (!file.exists(original)) original <- file.path(root,'VBAC-TRIPOD-AI','outputs','flow','natality_tripod_flow_2016_2024_TOTAL.csv')
if (!file.exists(original)) stop('Cannot locate original flow. Supply annual flow files in harmonised/ or check repo path.')
flow <- read.csv(original,check.names=FALSE)
stopifnot(all(c('stage_order','stage','n') %in% names(flow)))
flow <- flow[order(flow$stage_order),]
flow$excluded_from_previous_stage <- c(NA,head(flow$n,-1)-tail(flow$n,-1))
write.csv(flow,file.path(out,'original_cohort_flow_verified.csv'),row.names=FALSE)
if (tail(flow$n,1)!=772741) stop('Final cohort differs from 772741; investigate before reporting.')
cat('Existing flow checked; final N = ',tail(flow$n,1),'\n',sep='')
# Inspect year-specific prior-CS extracts; these contain the necessary source variables.
# Read 100,000 rows at a time to avoid loading millions of rows in memory.
needed <- c('previous_cesarean_binary','singleton','cephalic','term',
            'n_previous_cesareans','delivery_method_recode',
            'trial_of_labor_if_cesarean','successful_vbac','failed_tolac','tolac')
parse_bool <- function(x) {x <- toupper(trimws(as.character(x))); x %in% c('TRUE','T','1')}
records <- list(); ix <- 0L
for (year in 2016:2024) {
  f <- file.path(harm,sprintf('natality_harmonised_priorcs_%d.csv.gz',year))
  if (!file.exists(f)) stop('Missing prior-CS extract: ',f)
  con <- gzfile(f,open='rt'); header <- strsplit(readLines(con,n=1),',',fixed=TRUE)[[1]]
  header <- gsub('^"|"$','',header)
  absent <- setdiff(needed,header)
  if (length(absent)) {close(con);stop('Missing columns in ',f,': ',paste(absent,collapse=', '))}
  cat('Processing ',year,'\n',sep='')
  chunk <- 0L
  repeat {
    d <- tryCatch(read.csv(con,header=FALSE,col.names=header,nrows=100000,
                  colClasses='character',check.names=FALSE,na.strings=c('NA','')),
                  error=function(e) {if (grepl('no lines available',conditionMessage(e))) return(data.frame()); stop(e)})
    if (!nrow(d)) break
    chunk <- chunk+1L
    elig <- parse_bool(d$previous_cesarean_binary) & parse_bool(d$singleton) &
      parse_bool(d$cephalic) & parse_bool(d$term) &
      !is.na(suppressWarnings(as.numeric(d$n_previous_cesareans))) &
      suppressWarnings(as.numeric(d$n_previous_cesareans))==1
    x <- d[elig,,drop=FALSE]
    if (!nrow(x)) next
    dm <- suppressWarnings(as.integer(x$delivery_method_recode))
    trial <- toupper(trimws(x$trial_of_labor_if_cesarean))
    vb <- parse_bool(x$successful_vbac)
    failed <- parse_bool(x$failed_tolac)
    inferred <- vb | failed
    # Mutually exclusive *recorded-code* categories. Unknown is not labelled no TOLAC.
    category <- rep('Other delivery-method code / ambiguous TOLAC',nrow(x))
    category[is.na(dm)] <- 'Delivery method missing'
    category[!is.na(dm) & dm==4 & (is.na(trial) | !(trial %in% c('Y','N')))] <- 'Repeat CS; trial missing/other'
    category[!is.na(dm) & dm==4 & !is.na(trial) & trial=='N'] <- 'Repeat CS; trial explicitly N'
    category[!is.na(dm) & dm==4 & !is.na(trial) & trial=='Y'] <- 'Repeat CS trial=Y; inconsistent derived flag'
    category[failed] <- 'Failed TOLAC (repeat CS and trial=Y)'
    category[vb] <- 'Successful VBAC (delivery recode 2)'
    tab <- table(factor(category,levels=c(
      'Successful VBAC (delivery recode 2)',
      'Failed TOLAC (repeat CS and trial=Y)',
      'Repeat CS; trial explicitly N',
      'Repeat CS; trial missing/other',
      'Repeat CS trial=Y; inconsistent derived flag',
      'Delivery method missing',
      'Other delivery-method code / ambiguous TOLAC')))
    ix <- ix+1L
    records[[ix]] <- data.frame(year=year,category=names(tab),n=as.numeric(tab))
  }
  close(con)
}
by_year <- aggregate(n~year+category,do.call(rbind,records),sum)
by_year <- by_year[order(by_year$year,by_year$category),]
write.csv(by_year,file.path(out,'final_stage_categories_by_year.csv'),row.names=FALSE)
total <- aggregate(n~category,by_year,sum)
write.csv(total,file.path(out,'final_stage_categories_total.csv'),row.names=FALSE)
# Crucial reconciliation: categories must total exactly the fourth stage.
stopifnot(sum(total$n)==flow$n[flow$stage_order==4])
retained <- sum(total$n[total$category %in% c('Successful VBAC (delivery recode 2)','Failed TOLAC (repeat CS and trial=Y)')])
stopifnot(retained==tail(flow$n,1))
excluded <- sum(total$n)-retained
stopifnot(excluded==flow$excluded_from_previous_stage[nrow(flow)])
writeLines(c(
  'R1.5 COHORT SELECTION AUDIT',
  'Existing 5-stage cohort flow was checked against the published repository output.',
  'Final-stage categories are mutually exclusive and reflect the recorded CDC-derived variables.',
  'Do not interpret all excluded records as elective repeat cesareans.',
  'The trial-of-labor flag is specifically defined for repeat cesarean deliveries;',
  'a missing flag is not proof that TOLAC did not occur.',
  'The primary model estimates P(VBAC success | identifiable TOLAC undertaken, predictors).',
  'This audit is post-review and does not modify the original models.',
  sprintf('Eligible before final stage: %s',sum(total$n)),
  sprintf('Identifiable TOLAC retained: %s',retained),
  sprintf('Not retained in final stage: %s',excluded)
),file.path(out,'README_interpretation.txt'))
cat('\nRECONCILED FINAL-STAGE CATEGORIES:\n');print(total,row.names=FALSE)
cat('\nOutputs: ',out,'\n',sep='')
