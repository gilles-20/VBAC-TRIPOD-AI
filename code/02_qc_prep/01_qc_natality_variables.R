#!/usr/bin/env Rscript



# ================================================================

# CDC NATALITY TOLAC/VBAC

# VARIABLE-LEVEL QC: 2016-2024

# ================================================================



options(stringsAsFactors = FALSE)

options(scipen = 999)



BASE_DIR <- paste0(

  "/data/brussel/vo/000/bvo00010/vsc11778/",

  "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"

)



DATA_DIR <- file.path(BASE_DIR, "harmonised")

QC_DIR   <- file.path(DATA_DIR, "QC")



dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)



cat("============================================================\n")

cat("CDC Natality TOLAC variable QC\n")

cat("Started:", format(Sys.time()), "\n")

cat("============================================================\n\n")





# ================================================================

# Variables to examine

# ================================================================



continuous_vars <- c(

  "maternal_age",

  "prior_live_births_living",

  "prior_live_births_dead",

  "total_prior_live_births",

  "maternal_height_in",

  "maternal_height_cm",

  "prepregnancy_weight_lb",

  "prepregnancy_weight_kg",

  "prepregnancy_bmi",

  "cigarettes_pre_pregnancy",

  "prenatal_care_start_month",

  "prenatal_visits",

  "obstetric_gest_age_wk"

)



categorical_vars <- c(

  "race_ethnicity",

  "maternal_race6",

  "maternal_hispanic_origin",

  "maternal_education",

  "marital_status",

  "wic_binary",

  "prepregnancy_diabetes_binary",

  "gestational_diabetes_binary",

  "prepregnancy_hypertension_binary",

  "gestational_hypertension_binary",

  "previous_preterm_birth_binary",

  "induction_labor_binary",

  "fetal_presentation",

  "plurality",

  "payment_recode",

  "vbac"

)





# ================================================================

# Storage

# ================================================================



summary_list <- list()

category_list <- list()

outlier_list <- list()

year_summary_list <- list()





# ================================================================

# Helper

# ================================================================



safe_quantile <- function(x, prob) {



  x <- x[is.finite(x)]



  if (length(x) == 0) {

    return(NA_real_)

  }



  as.numeric(

    quantile(

      x,

      probs = prob,

      na.rm = TRUE,

      names = FALSE

    )

  )

}





# ================================================================

# Process each year

# ================================================================



for (YEAR in 2016:2024) {



  cat("\n============================================================\n")

  cat("Processing", YEAR, "\n")

  cat("============================================================\n")



  FILE <- file.path(

    DATA_DIR,

    paste0(

      "natality_tolac_primary_",

      YEAR,

      ".csv.gz"

    )

  )



  if (!file.exists(FILE)) {

    stop("Missing file: ", FILE)

  }



  d <- read.csv(

    gzfile(FILE),

    stringsAsFactors = FALSE

  )



  cat("Rows:", format(nrow(d), big.mark = ","), "\n")

  cat("Columns:", ncol(d), "\n")



  # --------------------------------------------------------------

  # Check year

  # --------------------------------------------------------------



  if ("source_year" %in% names(d)) {



    wrong_year <- sum(

      !is.na(d$source_year) &

      d$source_year != YEAR

    )



  } else {



    wrong_year <- NA_integer_



  }





  # --------------------------------------------------------------

  # Overall yearly summary

  # --------------------------------------------------------------



  N <- nrow(d)



  n_vbac <- sum(d$vbac == 1, na.rm = TRUE)

  n_failed <- sum(d$vbac == 0, na.rm = TRUE)



  year_summary_list[[as.character(YEAR)]] <-

    data.frame(

      year = YEAR,

      n = N,

      vbac_success = n_vbac,

      failed_tolac = n_failed,

      vbac_rate = round(

        100 * n_vbac / N,

        3

      ),

      wrong_source_year = wrong_year

    )





  # --------------------------------------------------------------

  # Continuous variable QC

  # --------------------------------------------------------------



  for (v in continuous_vars) {



    if (!(v %in% names(d))) {

      warning("Missing variable: ", v)

      next

    }



    x <- suppressWarnings(

      as.numeric(d[[v]])

    )



    n_missing <- sum(is.na(x))

    n_nonmissing <- sum(!is.na(x))



    summary_list[[length(summary_list) + 1]] <-

      data.frame(

        year = YEAR,

        variable = v,



        n = length(x),



        nonmissing = n_nonmissing,



        missing = n_missing,



        missing_pct = round(

          100 * n_missing / length(x),

          3

        ),



        mean = ifelse(

          n_nonmissing > 0,

          mean(x, na.rm = TRUE),

          NA

        ),



        sd = ifelse(

          n_nonmissing > 1,

          sd(x, na.rm = TRUE),

          NA

        ),



        min = ifelse(

          n_nonmissing > 0,

          min(x, na.rm = TRUE),

          NA

        ),



        p01 = safe_quantile(x, 0.01),

        p05 = safe_quantile(x, 0.05),

        median = safe_quantile(x, 0.50),

        p95 = safe_quantile(x, 0.95),

        p99 = safe_quantile(x, 0.99),



        max = ifelse(

          n_nonmissing > 0,

          max(x, na.rm = TRUE),

          NA

        )

      )

  }





  # --------------------------------------------------------------

  # Categorical variable QC

  # --------------------------------------------------------------



  for (v in categorical_vars) {



    if (!(v %in% names(d))) {

      warning("Missing variable: ", v)

      next

    }



    x <- d[[v]]



    missing_n <- sum(is.na(x))



    category_list[[length(category_list) + 1]] <-

      data.frame(

        year = YEAR,

        variable = v,

        level = "MISSING",

        n = missing_n,

        pct = round(

          100 * missing_n / length(x),

          3

        )

      )



    tab <- table(

      x,

      useNA = "no"

    )



    if (length(tab) > 0) {



      tmp <- data.frame(

        year = YEAR,

        variable = v,

        level = names(tab),

        n = as.numeric(tab),

        pct = round(

          100 * as.numeric(tab) / length(x),

          3

        )

      )



      category_list[[length(category_list) + 1]] <- tmp

    }

  }





  # --------------------------------------------------------------

  # Clinically useful plausibility checks

  #

  # These are QC flags only.

  # We are NOT excluding these observations here.

  # --------------------------------------------------------------



  get_num <- function(v) {



    if (!(v %in% names(d))) {

      return(rep(NA_real_, nrow(d)))

    }



    suppressWarnings(

      as.numeric(d[[v]])

    )

  }



  age <- get_num("maternal_age")

  bmi <- get_num("prepregnancy_bmi")

  height <- get_num("maternal_height_in")

  weight <- get_num("prepregnancy_weight_lb")

  gest <- get_num("obstetric_gest_age_wk")

  cigarettes <- get_num("cigarettes_pre_pregnancy")



  flags <- data.frame(



    year = YEAR,



    check = c(

      "maternal_age_lt_12",

      "maternal_age_gt_50",



      "bmi_lt_13",

      "bmi_gt_69_9",



      "height_lt_30in",

      "height_gt_78in",



      "weight_lt_75lb",

      "weight_gt_375lb",



      "gest_age_lt_37",

      "gest_age_gt_47",



      "cigarettes_lt_0",

      "cigarettes_gt_98"

    ),



    n_flagged = c(

      sum(age < 12, na.rm = TRUE),

      sum(age > 50, na.rm = TRUE),



      sum(bmi < 13, na.rm = TRUE),

      sum(bmi > 69.9, na.rm = TRUE),



      sum(height < 30, na.rm = TRUE),

      sum(height > 78, na.rm = TRUE),



      sum(weight < 75, na.rm = TRUE),

      sum(weight > 375, na.rm = TRUE),



      sum(gest < 37, na.rm = TRUE),

      sum(gest > 47, na.rm = TRUE),



      sum(cigarettes < 0, na.rm = TRUE),

      sum(cigarettes > 98, na.rm = TRUE)

    )

  )



  flags$pct <- round(

    100 * flags$n_flagged / nrow(d),

    4

  )



  outlier_list[[as.character(YEAR)]] <- flags





  # --------------------------------------------------------------

  # Internal cohort checks

  # --------------------------------------------------------------



  cat(

    "VBAC:",

    format(n_vbac, big.mark = ","),

    "\n"

  )



  cat(

    "Failed TOLAC:",

    format(n_failed, big.mark = ","),

    "\n"

  )



  cat(

    "VBAC rate:",

    round(100 * n_vbac / N, 2),

    "%\n"

  )



  rm(d)

  gc(verbose = FALSE)

}





# ================================================================

# Combine results

# ================================================================



continuous_summary <- do.call(

  rbind,

  summary_list

)



categorical_summary <- do.call(

  rbind,

  category_list

)



plausibility_summary <- do.call(

  rbind,

  outlier_list

)



year_summary <- do.call(

  rbind,

  year_summary_list

)



rownames(continuous_summary) <- NULL

rownames(categorical_summary) <- NULL

rownames(plausibility_summary) <- NULL

rownames(year_summary) <- NULL





# ================================================================

# Save

# ================================================================



write.csv(

  year_summary,

  file.path(

    QC_DIR,

    "QC_YEAR_SUMMARY.csv"

  ),

  row.names = FALSE

)



write.csv(

  continuous_summary,

  file.path(

    QC_DIR,

    "QC_CONTINUOUS_VARIABLES.csv"

  ),

  row.names = FALSE

)



write.csv(

  categorical_summary,

  file.path(

    QC_DIR,

    "QC_CATEGORICAL_VARIABLES.csv"

  ),

  row.names = FALSE

)



write.csv(

  plausibility_summary,

  file.path(

    QC_DIR,

    "QC_PLAUSIBILITY_FLAGS.csv"

  ),

  row.names = FALSE

)





# ================================================================

# Missingness matrix

# ================================================================



missing_matrix <- continuous_summary[

  ,

  c(

    "year",

    "variable",

    "missing_pct"

  )

]



missing_wide <- reshape(

  missing_matrix,

  idvar = "variable",

  timevar = "year",

  direction = "wide"

)



names(missing_wide) <- sub(

  "missing_pct\\.",

  "missing_pct_",

  names(missing_wide)

)



write.csv(

  missing_wide,

  file.path(

    QC_DIR,

    "QC_MISSINGNESS_BY_YEAR.csv"

  ),

  row.names = FALSE

)





# ================================================================

# Print important results

# ================================================================



cat("\n\n")

cat("============================================================\n")

cat("YEAR SUMMARY\n")

cat("============================================================\n")



print(

  year_summary,

  row.names = FALSE

)





cat("\n")

cat("============================================================\n")

cat("MISSINGNESS (%)\n")

cat("============================================================\n")



print(

  missing_wide,

  row.names = FALSE

)





cat("\n")

cat("============================================================\n")

cat("PLAUSIBILITY FLAGS > 0\n")

cat("============================================================\n")



problem_flags <- plausibility_summary[

  plausibility_summary$n_flagged > 0,

  ,

  drop = FALSE

]



if (nrow(problem_flags) == 0) {



  cat("No plausibility violations detected.\n")



} else {



  print(

    problem_flags,

    row.names = FALSE

  )

}





cat("\n")

cat("============================================================\n")

cat("QC COMPLETED\n")

cat("============================================================\n")



cat(

  "Results saved in:\n",

  QC_DIR,

  "\n"

)



cat("Finished:", format(Sys.time()), "\n")
