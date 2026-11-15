#!/usr/bin/env Rscript
# R1.4: evidence audit only. Does not refit models or modify existing files.
root <- Sys.getenv('VBAC_ROOT', unset = '')

if (!nzchar(Sys.getenv('VBAC_ROOT', unset = ''))) stop('Set VBAC_ROOT before running this script.')
repo <- file.path(root,'VBAC-TRIPOD-AI')
if (!dir.exists(repo)) repo <- root
out <- Sys.getenv(
  'R1_4_OUT',
  file.path(root,'REVIEWER_REVISION_2026','R1_4_TEMPORAL_DESIGN')
)
dir.create(out, recursive=TRUE, showWarnings=FALSE)
script <- file.path(repo,'code','03_models','01_vbac_logistic_protocol_frozen.R')
if (!file.exists(script)) script <- file.path(root,'VBAC_LOGISTIC_PROTOCOL_FROZEN_2026_08.R')
if (!file.exists(script)) stop('Primary model script not found; set VBAC_ROOT correctly.')
lines <- readLines(script, warn=FALSE)
patterns <- c('source_year >= 2016','source_year >= 2022','source_year == 2024','RUN_FINAL_TEST <-','FINAL_TEST_UNLOCK_TEXT <-','REQUIRED_FINAL_TEST_UNLOCK_TEXT','Final 2024 model test opened','final_test_year=2024')
audit <- do.call(rbind, lapply(patterns,function(p){ ix<-grep(p,lines,fixed=TRUE); if (!length(ix)) return(data.frame(pattern=p,line=NA_integer_,text=NA_character_)); data.frame(pattern=p,line=ix,text=trimws(lines[ix])) }))
write.csv(audit,file.path(out,'implemented_split_and_guard_audit.csv'),row.names=FALSE)
# Record file evidence without assuming timestamps establish scientific chronology.
paths <- unique(c(script,file.path(repo,'docs','VBAC_Natality_2016_2024_Harmonisation.xlsx'),file.path(root,'us_birth_files','harmonised','model_results','race_inclusive_comparison_2026_08','PREDICTIONS_2024_RACE_NEUTRAL_VS_RACE_INCLUSIVE.csv.gz')))
info <- file.info(paths); files <- data.frame(path=paths,exists=file.exists(paths),size_bytes=info$size,modified_time=as.character(info$mtime))
write.csv(files,file.path(out,'file_metadata.csv'),row.names=FALSE)
# Search repo for protocol logs, scripts and result manifests; avoid huge raw data.
all <- list.files(repo,recursive=TRUE,full.names=TRUE,all.files=FALSE)
sel <- grepl('protocol|freeze|unlock|log$|manifest|run_info|final_test|readme',basename(all),ignore.case=TRUE) & !dir.exists(all)
if(length(all)) write.csv(data.frame(path=all[sel]),file.path(out,'candidate_evidence_files.csv'),row.names=FALSE)
# git evidence: commit timestamps and content history, not conclusive proof of test-set access.
git <- Sys.which('git')
if(nzchar(git) && dir.exists(file.path(repo,'.git'))){
 cmd <- c('-C',repo,'log','--date=iso-strict','--pretty=format:%H_%aI_%s','--','code/03_models/01_vbac_logistic_protocol_frozen.R')
 hist <- tryCatch(system2(git,cmd,stdout=TRUE,stderr=TRUE),error=function(e)paste('ERROR:',conditionMessage(e)))
 writeLines(hist,file.path(out,'model_script_git_history.txt'))
 status <- tryCatch(system2(git,c('-C',repo,'status','--short'),stdout=TRUE,stderr=TRUE),error=function(e)paste('ERROR:',conditionMessage(e)))
 writeLines(status,file.path(out,'git_worktree_status.txt'))
}
writeLines(c(
'R1.4 temporal design discrepancy - evidence audit',
'Workbook Cohort_Algorithm step 10 says suggested: 2016-2021 development; 2022 tuning/calibration; 2023-2024 locked validation.',
'Implemented model script: 2016-2021 development; 2022-2023 intermediate evaluation; 2024 final temporal test.',
'Workbook wording is historical planning guidance, not the executed split.',
'Guard flags in a script alone do not prove when the 2024 outcomes were first inspected.',
'Inspect dated logs, git history, execution manifests, and file provenance before claiming analysis-frozen before 2024 opening.',
'Do not overwrite historical source workbook. Add a dated clarification note or corrected copy with original step retained.',
'If chronological evidence is insufficient, state explicitly that a prospective freeze cannot be independently verified.',
'The post-hoc race-inclusive comparison and post-review sensitivity analyses are not untouched 2024 validations.'
),file.path(out,'README_interpretation.txt'))
cat('Audit completed:',out,'\n')
cat('Primary script:',script,'\n')
print(audit)
