#!/usr/bin/env Rscript



# ================================================================

# COMBINE CDC NATALITY PRIMARY TOLAC DATA

# 2016-2024

# ================================================================



options(stringsAsFactors = FALSE)

options(scipen = 999)



BASE_DIR <- paste0(

  "/data/brussel/vo/000/bvo00010/vsc11778/",

  "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"

)



DATA_DIR <- file.path(BASE_DIR, "harmonised")



OUT_FILE <- file.path(

  DATA_DIR,

  "NATALITY_TOLAC_2016_2024.csv.gz"

)



cat("============================================================\n")

cat("Combining CDC Natality TOLAC files\n")

cat("Started:", format(Sys.time()), "\n")

cat("============================================================\n\n")



years <- 2016:2024



all_data <- vector(

  "list",

  length(years)

)



names(all_data) <- years



for (i in seq_along(years)) {



  year <- years[i]



  f <- file.path(

    DATA_DIR,

    paste0(

      "natality_tolac_primary_",

      year,

      ".csv.gz"

    )

  )



  if (!file.exists(f)) {

    stop("Missing file: ", f)

  }



  cat(

    "Reading ",

    year,

    "... ",

    sep = ""

  )



  d <- read.csv(

    gzfile(f),

    stringsAsFactors = FALSE

  )



  if (!"source_year" %in% names(d)) {

    d$source_year <- year

  }



  if (any(d$source_year != year, na.rm = TRUE)) {

    stop(

      "Source-year mismatch detected for ",

      year

    )

  }



  cat(

    format(nrow(d), big.mark = ","),

    " rows\n"

  )



  all_data[[i]] <- d

}





# ================================================================

# Check column consistency

# ================================================================



reference_names <- names(all_data[[1]])



for (year in years) {



  if (!identical(

    names(all_data[[as.character(year)]]),

    reference_names

  )) {



    stop(

      "Column structure differs in year ",

      year

    )

  }

}





# ================================================================

# Combine

# ================================================================



cat("\nCombining years...\n")



combined <- do.call(

  rbind,

  all_data

)



rownames(combined) <- NULL



cat(

  "Combined N = ",

  format(nrow(combined), big.mark = ","),

  "\n",

  sep = ""

)





# ================================================================

# Fundamental checks

# ================================================================



EXPECTED_N <- 772741



if (nrow(combined) != EXPECTED_N) {



  warning(

    "Expected ",

    EXPECTED_N,

    " observations but obtained ",

    nrow(combined)

  )

}





if (any(is.na(combined$vbac))) {

  stop(

    "ERROR: Missing VBAC outcome detected."

  )

}





if (!all(combined$vbac %in% c(0, 1))) {

  stop(

    "ERROR: VBAC contains values other than 0/1."

  )

}





# ================================================================

# Add explicit calendar period variables

# ================================================================



combined$calendar_year <-

  combined$source_year



combined$calendar_period <- cut(

  combined$calendar_year,

  breaks = c(

    2015,

    2018,

    2021,

    2024

  ),

  labels = c(

    "2016-2018",

    "2019-2021",

    "2022-2024"

  ),

  include.lowest = TRUE

)





# ================================================================

# PROVISIONAL modelling split

#

# We keep this explicit rather than performing a random split.

#

# 2016-2021 = development

# 2022-2024 = temporal validation

#

# This can be changed later without touching the raw data.

# ================================================================



combined$analysis_period <- ifelse(

  combined$source_year <= 2021,

  "development_2016_2021",

  "temporal_validation_2022_2024"

)





# ================================================================

# Verify counts

# ================================================================



year_table <- as.data.frame(

  table(combined$source_year)

)



names(year_table) <- c(

  "year",

  "n"

)



year_table$year <-

  as.integer(

    as.character(year_table$year)

  )



outcome_table <- aggregate(

  vbac ~ source_year,

  data = combined,

  FUN = function(x) {

    c(

      n = length(x),

      successes = sum(x == 1),

      failures = sum(x == 0),

      success_rate = mean(x == 1)

    )

  }

)



outcome_summary <- data.frame(

  year = outcome_table$source_year,

  n = outcome_table$vbac[, "n"],

  vbac_success =

    outcome_table$vbac[, "successes"],

  failed_tolac =

    outcome_table$vbac[, "failures"],

  vbac_rate =

    outcome_table$vbac[, "success_rate"]

)





# ================================================================

# Split summary

# ================================================================



split_summary <- aggregate(

  vbac ~ analysis_period,

  data = combined,

  FUN = function(x) {

    c(

      n = length(x),

      successes = sum(x == 1),

      failures = sum(x == 0),

      success_rate = mean(x == 1)

    )

  }

)



split_summary_clean <- data.frame(

  analysis_period =

    split_summary$analysis_period,



  n =

    split_summary$vbac[, "n"],



  vbac_success =

    split_summary$vbac[, "successes"],



  failed_tolac =

    split_summary$vbac[, "failures"],



  vbac_rate =

    round(

      100 *

        split_summary$vbac[, "success_rate"],

      3

    )

)





# ================================================================

# Save combined dataset

# ================================================================



cat("\nWriting combined compressed dataset...\n")



con <- gzfile(

  OUT_FILE,

  open = "wt"

)



write.csv(

  combined,

  con,

  row.names = FALSE,

  na = ""

)



close(con)





# ================================================================

# Save QC summaries

# ================================================================



write.csv(

  outcome_summary,

  file.path(

    DATA_DIR,

    "NATALITY_TOLAC_2016_2024_OUTCOME_SUMMARY.csv"

  ),

  row.names = FALSE

)



write.csv(

  split_summary_clean,

  file.path(

    DATA_DIR,

    "NATALITY_TOLAC_ANALYSIS_PERIOD_SUMMARY.csv"

  ),

  row.names = FALSE

)





# ================================================================

# Final output

# ================================================================



cat("\n")

cat("============================================================\n")

cat("YEAR COUNTS\n")

cat("============================================================\n")



print(

  outcome_summary,

  row.names = FALSE

)



cat("\n")

cat("============================================================\n")

cat("PROVISIONAL DEVELOPMENT / VALIDATION SPLIT\n")

cat("============================================================\n")



print(

  split_summary_clean,

  row.names = FALSE

)



cat("\n")

cat("Combined dataset:\n")

cat(OUT_FILE, "\n")



cat("\nFinished:", format(Sys.time()), "\n")

cat("============================================================\n")
