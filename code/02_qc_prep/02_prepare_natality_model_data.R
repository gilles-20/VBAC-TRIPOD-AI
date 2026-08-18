#!/usr/bin/env Rscript



# =====================================================================

# CDC NATALITY TOLAC/VBAC

# MODEL-READY DATA PREPARATION

# 2016-2024

#

# Development:         2016-2021

# Temporal validation: 2022-2024

#

# IMPORTANT:

# - No predictor imputation is performed here.

# - Race/ethnicity is retained primarily for subgroup/equity evaluation.

# - Outcome-defining/post-baseline variables are explicitly marked.

# =====================================================================



options(stringsAsFactors = FALSE)

options(scipen = 999)



BASE_DIR <- paste0(

  "/data/brussel/vo/000/bvo00010/vsc11778/",

  "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"

)



DATA_DIR <- file.path(BASE_DIR, "harmonised")

MODEL_DIR <- file.path(DATA_DIR, "model_ready")



dir.create(

  MODEL_DIR,

  recursive = TRUE,

  showWarnings = FALSE

)



INPUT_FILE <- file.path(

  DATA_DIR,

  "NATALITY_TOLAC_2016_2024.csv.gz"

)



OUTPUT_FILE <- file.path(

  MODEL_DIR,

  "NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz"

)



DICTIONARY_FILE <- file.path(

  MODEL_DIR,

  "NATALITY_MODEL_DATA_DICTIONARY.csv"

)



QC_FILE <- file.path(

  MODEL_DIR,

  "NATALITY_MODEL_READY_QC.csv"

)



SUBGROUP_FILE <- file.path(

  MODEL_DIR,

  "NATALITY_SUBGROUP_COUNTS.csv"

)



cat("============================================================\n")

cat("CDC Natality model-data preparation\n")

cat("Started:", format(Sys.time()), "\n")

cat("============================================================\n")



if (!file.exists(INPUT_FILE)) {

  stop("Input file not found: ", INPUT_FILE)

}



# =====================================================================

# READ COMBINED DATA

# =====================================================================



cat("\nReading combined data...\n")



d <- read.csv(

  gzfile(INPUT_FILE),

  stringsAsFactors = FALSE

)



cat(

  "Rows:",

  format(nrow(d), big.mark = ","),

  "\n"

)



cat(

  "Columns:",

  ncol(d),

  "\n"

)



EXPECTED_N <- 772741L



if (nrow(d) != EXPECTED_N) {

  warning(

    "Expected ",

    EXPECTED_N,

    " records, found ",

    nrow(d)

  )

}



# =====================================================================

# BASIC CHECKS

# =====================================================================



required <- c(

  "source_year",

  "vbac",

  "maternal_age",

  "maternal_height_cm",

  "prepregnancy_weight_kg",

  "prepregnancy_bmi",

  "prepregnancy_hypertension_binary",

  "prepregnancy_diabetes_binary",

  "previous_preterm_birth_binary",

  "total_prior_live_births",

  "cigarettes_pre_pregnancy",

  "maternal_education",

  "race_ethnicity"

)



missing_required <- setdiff(

  required,

  names(d)

)



if (length(missing_required) > 0) {

  stop(

    "Required variables missing: ",

    paste(missing_required, collapse = ", ")

  )

}



if (!all(d$vbac %in% c(0, 1))) {

  stop("VBAC outcome contains values other than 0/1.")

}



# =====================================================================

# ANALYSIS PERIOD

# =====================================================================



d$analysis_period <- ifelse(

  d$source_year <= 2021,

  "development",

  "temporal_validation"

)



d$development <- ifelse(

  d$source_year <= 2021,

  1L,

  0L

)



d$temporal_validation <- ifelse(

  d$source_year >= 2022,

  1L,

  0L

)



# =====================================================================

# RACE / ETHNICITY LABELS

# CDC MRACEHISP

# =====================================================================



race_labels <- c(

  "1" = "NH_White",

  "2" = "NH_Black",

  "3" = "NH_AIAN",

  "4" = "NH_Asian",

  "5" = "NH_NHOPI",

  "6" = "NH_Multiracial",

  "7" = "Hispanic"

)



d$race_ethnicity_group <-

  unname(

    race_labels[

      as.character(d$race_ethnicity)

    ]

  )



d$race_ethnicity_group[

  is.na(d$race_ethnicity_group)

] <- "Missing"



d$race_ethnicity_group <- factor(

  d$race_ethnicity_group,

  levels = c(

    "NH_White",

    "NH_Black",

    "Hispanic",

    "NH_Asian",

    "NH_AIAN",

    "NH_NHOPI",

    "NH_Multiracial",

    "Missing"

  )

)



# =====================================================================

# MATERNAL EDUCATION LABELS

# =====================================================================



education_labels <- c(

  "1" = "8th_grade_or_less",

  "2" = "9th_12th_no_diploma",

  "3" = "High_school_or_GED",

  "4" = "Some_college_no_degree",

  "5" = "Associate_degree",

  "6" = "Bachelors_degree",

  "7" = "Masters_degree",

  "8" = "Doctorate_or_professional"

)



d$education_group <-

  unname(

    education_labels[

      as.character(d$maternal_education)

    ]

  )



d$education_group[

  is.na(d$education_group)

] <- "Missing"



d$education_group <- factor(

  d$education_group,

  levels = c(

    "8th_grade_or_less",

    "9th_12th_no_diploma",

    "High_school_or_GED",

    "Some_college_no_degree",

    "Associate_degree",

    "Bachelors_degree",

    "Masters_degree",

    "Doctorate_or_professional",

    "Missing"

  )

)



# =====================================================================

# AGE SUBGROUPS FOR CALIBRATION

# =====================================================================



d$age_group <- cut(

  d$maternal_age,

  breaks = c(

    -Inf,

    24,

    29,

    34,

    39,

    Inf

  ),

  labels = c(

    "<25",

    "25-29",

    "30-34",

    "35-39",

    "40+"

  ),

  right = TRUE

)



# =====================================================================

# BMI SUBGROUPS

# =====================================================================



d$bmi_group <- cut(

  d$prepregnancy_bmi,

  breaks = c(

    -Inf,

    18.5,

    25,

    30,

    35,

    40,

    Inf

  ),

  labels = c(

    "Underweight_<18.5",

    "Normal_18.5-24.9",

    "Overweight_25-29.9",

    "Obesity_I_30-34.9",

    "Obesity_II_35-39.9",

    "Obesity_III_40+"

  ),

  right = FALSE

)



d$bmi_group <- as.character(d$bmi_group)



d$bmi_group[

  is.na(d$bmi_group)

] <- "Missing"



d$bmi_group <- factor(

  d$bmi_group,

  levels = c(

    "Underweight_<18.5",

    "Normal_18.5-24.9",

    "Overweight_25-29.9",

    "Obesity_I_30-34.9",

    "Obesity_II_35-39.9",

    "Obesity_III_40+",

    "Missing"

  )

)



# =====================================================================

# HYPERTENSION SUBGROUP

# =====================================================================



d$chronic_htn_group <- ifelse(

  is.na(d$prepregnancy_hypertension_binary),

  "Missing",

  ifelse(

    d$prepregnancy_hypertension_binary == 1,

    "Yes",

    "No"

  )

)



d$chronic_htn_group <- factor(

  d$chronic_htn_group,

  levels = c(

    "No",

    "Yes",

    "Missing"

  )

)



# =====================================================================

# DIABETES SUBGROUP

# =====================================================================



d$prepregnancy_diabetes_group <- ifelse(

  is.na(d$prepregnancy_diabetes_binary),

  "Missing",

  ifelse(

    d$prepregnancy_diabetes_binary == 1,

    "Yes",

    "No"

  )

)



d$prepregnancy_diabetes_group <- factor(

  d$prepregnancy_diabetes_group,

  levels = c(

    "No",

    "Yes",

    "Missing"

  )

)



# =====================================================================

# PARITY / PRIOR LIVE-BIRTH GROUP

#

# IMPORTANT:

# This is NOT equivalent to prior vaginal delivery.

# =====================================================================



d$prior_live_birth_group <- cut(

  d$total_prior_live_births,

  breaks = c(

    -Inf,

    0,

    1,

    2,

    Inf

  ),

  labels = c(

    "0",

    "1",

    "2",

    "3+"

  ),

  right = TRUE

)



d$prior_live_birth_group <-

  as.character(

    d$prior_live_birth_group

  )



d$prior_live_birth_group[

  is.na(d$prior_live_birth_group)

] <- "Missing"



# =====================================================================

# SMOKING GROUP

# =====================================================================



d$smoking_pre_pregnancy <- ifelse(

  is.na(d$cigarettes_pre_pregnancy),

  NA_integer_,

  ifelse(

    d$cigarettes_pre_pregnancy > 0,

    1L,

    0L

  )

)



# =====================================================================

# MISSINGNESS FLAGS

#

# Audit flags only.

# Actual imputation will later be trained ONLY in development data.

# =====================================================================



missingness_variables <- c(

  "maternal_height_cm",

  "prepregnancy_weight_kg",

  "prepregnancy_bmi",

  "total_prior_live_births",

  "cigarettes_pre_pregnancy",

  "maternal_education"

)



for (v in missingness_variables) {



  new_name <- paste0(

    v,

    "_missing"

  )



  d[[new_name]] <- as.integer(

    is.na(d[[v]])

  )

}



# =====================================================================

# DEFINE PREDICTOR SET MEMBERSHIP

# =====================================================================

#

# These columns make the intended role explicit.

# They do not alter the original variables.

# =====================================================================



MFMU_COMPATIBLE <- c(

  "maternal_age",

  "prepregnancy_weight_kg",

  "maternal_height_cm",

  "prepregnancy_hypertension_binary"

)



PRIMARY_RACE_NEUTRAL <- c(

  "maternal_age",

  "maternal_height_cm",

  "prepregnancy_weight_kg",

  "prepregnancy_hypertension_binary",

  "prepregnancy_diabetes_binary",

  "previous_preterm_birth_binary",

  "total_prior_live_births",

  "cigarettes_pre_pregnancy",

  "maternal_education",

  "marital_status",

  "wic_binary",

  "payment_recode"

)



EQUITY_EVALUATION <- c(

  "race_ethnicity_group",

  "maternal_race6",

  "age_group",

  "bmi_group",

  "chronic_htn_group",

  "prepregnancy_diabetes_group",

  "education_group"

)



LEAKAGE_OR_POST_BASELINE <- c(

  "delivery_route",

  "trial_of_labor_if_cesarean",

  "delivery_method_recode",

  "delivery_binary_recode",

  "successful_vbac",

  "failed_tolac",

  "tolac",

  "primary_tolac",

  "induction_labor",

  "induction_labor_binary"

)



# =====================================================================

# VERIFY NO OUTCOME LEAKAGE IN PRIMARY PREDICTOR SET

# =====================================================================



overlap <- intersect(

  PRIMARY_RACE_NEUTRAL,

  LEAKAGE_OR_POST_BASELINE

)



if (length(overlap) > 0) {

  stop(

    "Outcome leakage detected in predictor set: ",

    paste(overlap, collapse = ", ")

  )

}



# =====================================================================

# BUILD DATA DICTIONARY

# =====================================================================



all_variables <- names(d)



dictionary <- data.frame(

  variable = all_variables,

  role = "retained_other",

  description = "",

  stringsAsFactors = FALSE

)



dictionary$role[

  dictionary$variable == "vbac"

] <- "outcome"



dictionary$role[

  dictionary$variable %in% MFMU_COMPATIBLE

] <- "mfmu_compatible_predictor"



dictionary$role[

  dictionary$variable %in%

    setdiff(

      PRIMARY_RACE_NEUTRAL,

      MFMU_COMPATIBLE

    )

] <- "additional_primary_predictor"



dictionary$role[

  dictionary$variable %in% EQUITY_EVALUATION

] <- "subgroup_evaluation"



dictionary$role[

  dictionary$variable %in% LEAKAGE_OR_POST_BASELINE

] <- "excluded_from_primary_prediction"



dictionary$role[

  grepl(

    "_missing$",

    dictionary$variable

  )

] <- "missingness_audit"



dictionary$role[

  dictionary$variable %in%

    c(

      "source_year",

      "calendar_year",

      "calendar_period",

      "analysis_period",

      "development",

      "temporal_validation"

    )

] <- "analysis_design"



# Descriptions for important variables



set_description <- function(variable, text) {



  dictionary$description[

    dictionary$variable == variable

  ] <<- text

}



set_description(

  "vbac",

  "Outcome: 1 successful VBAC; 0 failed TOLAC."

)



set_description(

  "maternal_age",

  "Maternal age in years; exact MFMU-compatible predictor."

)



set_description(

  "prepregnancy_weight_kg",

  "Prepregnancy weight converted from CDC pounds to kilograms; exact MFMU-compatible predictor."

)



set_description(

  "maternal_height_cm",

  "Maternal height converted from CDC inches to centimeters; exact MFMU-compatible predictor."

)



set_description(

  "prepregnancy_hypertension_binary",

  paste(

    "CDC prepregnancy hypertension.",

    "Partial proxy for MFMU medication-treated chronic hypertension;",

    "medication treatment is unavailable."

  )

)



set_description(

  "total_prior_live_births",

  paste(

    "Number of prior live births.",

    "Not equivalent to prior vaginal delivery or prior VBAC."

  )

)



set_description(

  "race_ethnicity_group",

  paste(

    "CDC MRACEHISP-derived race/ethnicity group.",

    "Primary use: subgroup calibration/equity evaluation;",

    "not included in primary race-neutral model."

  )

)



set_description(

  "analysis_period",

  "Development 2016-2021 versus temporal validation 2022-2024."

)



set_description(

  "delivery_method_recode",

  "Outcome-defining CDC delivery method variable; excluded from prediction."

)



set_description(

  "trial_of_labor_if_cesarean",

  "Outcome/cohort-defining variable; excluded from prediction."

)



set_description(

  "induction_labor_binary",

  "Labor-management variable occurring after baseline; excluded from primary early-pregnancy model."

)



# =====================================================================

# QC SUMMARY

# =====================================================================



qc <- data.frame(

  metric = c(

    "total_n",

    "development_n",

    "temporal_validation_n",

    "vbac_success_n",

    "failed_tolac_n",

    "overall_vbac_rate_pct",

    "development_vbac_rate_pct",

    "validation_vbac_rate_pct",

    "race_missing_n",

    "race_missing_pct",

    "bmi_missing_n",

    "bmi_missing_pct"

  ),



  value = c(

    nrow(d),



    sum(

      d$analysis_period == "development"

    ),



    sum(

      d$analysis_period ==

        "temporal_validation"

    ),



    sum(

      d$vbac == 1

    ),



    sum(

      d$vbac == 0

    ),



    round(

      100 * mean(d$vbac == 1),

      3

    ),



    round(

      100 *

        mean(

          d$vbac[

            d$analysis_period ==

              "development"

          ] == 1

        ),

      3

    ),



    round(

      100 *

        mean(

          d$vbac[

            d$analysis_period ==

              "temporal_validation"

          ] == 1

        ),

      3

    ),



    sum(

      d$race_ethnicity_group ==

        "Missing"

    ),



    round(

      100 *

        mean(

          d$race_ethnicity_group ==

            "Missing"

        ),

      3

    ),



    sum(

      is.na(

        d$prepregnancy_bmi

      )

    ),



    round(

      100 *

        mean(

          is.na(

            d$prepregnancy_bmi

          )

        ),

      3

    )

  )

)



# =====================================================================

# SUBGROUP COUNTS / OUTCOME RATES

# =====================================================================



subgroup_variables <- c(

  "race_ethnicity_group",

  "age_group",

  "bmi_group",

  "chronic_htn_group",

  "prepregnancy_diabetes_group",

  "education_group"

)



subgroup_results <- list()



counter <- 0L



for (period in c(

  "development",

  "temporal_validation"

)) {



  dp <- d[

    d$analysis_period == period,

    ,

    drop = FALSE

  ]



  for (v in subgroup_variables) {



    x <- as.character(

      dp[[v]]

    )



    x[

      is.na(x)

    ] <- "Missing"



    levels_present <- unique(x)



    for (lev in levels_present) {



      idx <- x == lev



      n_group <- sum(idx)



      n_success <- sum(

        dp$vbac[idx] == 1

      )



      counter <- counter + 1L



      subgroup_results[[counter]] <-

        data.frame(

          analysis_period = period,

          subgroup_variable = v,

          subgroup_level = lev,

          n = n_group,

          vbac_success = n_success,

          failed_tolac =

            n_group - n_success,

          vbac_rate_pct = round(

            100 *

              n_success /

              n_group,

            3

          ),

          stringsAsFactors = FALSE

        )

    }

  }

}



subgroup_summary <- do.call(

  rbind,

  subgroup_results

)



rownames(subgroup_summary) <- NULL



# =====================================================================

# SAVE

# =====================================================================



cat("\nWriting model-ready dataset...\n")



con <- gzfile(

  OUTPUT_FILE,

  open = "wt"

)



write.csv(

  d,

  con,

  row.names = FALSE,

  na = ""

)



close(con)



write.csv(

  dictionary,

  DICTIONARY_FILE,

  row.names = FALSE

)



write.csv(

  qc,

  QC_FILE,

  row.names = FALSE

)



write.csv(

  subgroup_summary,

  SUBGROUP_FILE,

  row.names = FALSE

)



# =====================================================================

# SAVE MODEL VARIABLE LISTS

# =====================================================================



writeLines(

  MFMU_COMPATIBLE,

  file.path(

    MODEL_DIR,

    "VARIABLES_MFMU_COMPATIBLE.txt"

  )

)



writeLines(

  PRIMARY_RACE_NEUTRAL,

  file.path(

    MODEL_DIR,

    "VARIABLES_PRIMARY_RACE_NEUTRAL.txt"

  )

)



writeLines(

  EQUITY_EVALUATION,

  file.path(

    MODEL_DIR,

    "VARIABLES_EQUITY_EVALUATION.txt"

  )

)



writeLines(

  LEAKAGE_OR_POST_BASELINE,

  file.path(

    MODEL_DIR,

    "VARIABLES_EXCLUDED_LEAKAGE_POSTBASELINE.txt"

  )

)



# =====================================================================

# FINAL REPORT

# =====================================================================



cat("\n============================================================\n")

cat("MODEL DATA QC\n")

cat("============================================================\n")



print(

  qc,

  row.names = FALSE

)



cat("\nPrimary race-neutral predictors:\n")



for (v in PRIMARY_RACE_NEUTRAL) {

  cat("  - ", v, "\n", sep = "")

}



cat("\nMFMU-compatible predictors:\n")



for (v in MFMU_COMPATIBLE) {

  cat("  - ", v, "\n", sep = "")

}



cat("\nEquity/subgroup evaluation variables:\n")



for (v in EQUITY_EVALUATION) {

  cat("  - ", v, "\n", sep = "")

}



cat("\nExcluded leakage/post-baseline variables:\n")



for (v in LEAKAGE_OR_POST_BASELINE) {

  cat("  - ", v, "\n", sep = "")

}



cat("\nOutputs written to:\n")

cat(MODEL_DIR, "\n")



cat("\nFinished:", format(Sys.time()), "\n")

cat("============================================================\n")
