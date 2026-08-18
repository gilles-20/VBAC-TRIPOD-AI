#!/usr/bin/env Rscript

# =====================================================================
# AGGREGATE TRIPOD PARTICIPANT FLOW, CDC NATALITY 2016-2024
# =====================================================================
#
# Run AFTER extract_natality_vbac_TRIPOD_FLOW_UPDATED.R has been run
# separately for each year 2016, ..., 2024.
#
# Outputs:
#   natality_tripod_flow_2016_2024_by_year.csv
#   natality_tripod_flow_2016_2024_TOTAL.csv
#
# The final total should equal the manuscript analytical cohort:
#   Identifiable TOLAC outcome = 772,741
# =====================================================================

options(stringsAsFactors = FALSE)
options(scipen = 999)

BASE_DIR <- "/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
OUT_DIR <- file.path(BASE_DIR, "harmonised")

YEARS <- 2016:2024
EXPECTED_FINAL_N <- 772741L

flow_files <- file.path(
  OUT_DIR,
  paste0("natality_tripod_flow_", YEARS, ".csv")
)

missing_files <- flow_files[!file.exists(flow_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing annual flow files:\n",
    paste(missing_files, collapse = "\n"),
    "\nRun the updated annual extraction script for every year 2016-2024 first."
  )
}

annual_list <- lapply(seq_along(flow_files), function(i) {
  x <- read.csv(flow_files[i], stringsAsFactors = FALSE)
  required <- c("year", "stage_order", "stage", "n")
  if (!all(required %in% names(x))) {
    stop("Unexpected columns in: ", flow_files[i])
  }
  x
})

annual <- do.call(rbind, annual_list)
annual <- annual[order(annual$year, annual$stage_order), ]

write.csv(
  annual,
  file.path(OUT_DIR, "natality_tripod_flow_2016_2024_by_year.csv"),
  row.names = FALSE
)

stages <- unique(annual[, c("stage_order", "stage")])
stages <- stages[order(stages$stage_order), ]

totals <- merge(
  stages,
  aggregate(n ~ stage_order, data = annual, FUN = sum),
  by = "stage_order",
  all.x = TRUE,
  sort = TRUE
)

# merge() duplicates stage labels only if included on both sides; retain clean label
if (!"stage" %in% names(totals)) {
  totals <- merge(stages, totals, by = "stage_order", all.x = TRUE)
}

totals <- totals[order(totals$stage_order), c("stage_order", "stage", "n")]

totals$excluded_from_previous_stage <- c(
  NA_integer_,
  totals$n[-nrow(totals)] - totals$n[-1]
)

write.csv(
  totals,
  file.path(OUT_DIR, "natality_tripod_flow_2016_2024_TOTAL.csv"),
  row.names = FALSE
)

final_n <- totals$n[totals$stage == "Identifiable TOLAC outcome"]

if (length(final_n) != 1L) {
  stop("Could not identify the final TOLAC flow stage.")
}

cat("\n============================================================\n")
cat("TRIPOD PARTICIPANT FLOW — 2016-2024\n")
cat("============================================================\n")
for (i in seq_len(nrow(totals))) {
  cat(sprintf("%-58s %12s\n",
              totals$stage[i],
              format(totals$n[i], big.mark = ",", scientific = FALSE)))
}
cat("============================================================\n")

if (final_n != EXPECTED_FINAL_N) {
  stop(
    paste0(
      "\nFINAL COHORT CHECK FAILED.\n",
      "Expected manuscript N = ", format(EXPECTED_FINAL_N, big.mark = ","), "\n",
      "Reconstructed N       = ", format(final_n, big.mark = ","), "\n",
      "Do not populate the manuscript flow diagram until the discrepancy is resolved."
    )
  )
}

cat(
  "\nSUCCESS: reconstructed final TOLAC cohort matches manuscript N = ",
  format(EXPECTED_FINAL_N, big.mark = ","),
  ".\n",
  sep = ""
)
