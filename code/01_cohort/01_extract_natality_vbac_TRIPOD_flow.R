#!/usr/bin/env Rscript



# =====================================================================

# CDC NATALITY 2016-2024

# CHUNKED FIXED-WIDTH EXTRACTION FOR TOLAC / VBAC STUDY

# =====================================================================

#

# Usage:

#   Rscript extract_natality_vbac.R 2016

#

# Outputs:

#   natality_harmonised_priorcs_2016.csv.gz

#   natality_tolac_primary_2016.csv.gz

#   natality_qc_2016.csv

#

# The raw Natality file is NEVER loaded completely into memory.

# =====================================================================



options(stringsAsFactors = FALSE)

options(scipen = 999)



# ---------------------------------------------------------------------

# 1. COMMAND LINE ARGUMENT

# ---------------------------------------------------------------------



args <- commandArgs(trailingOnly = TRUE)



if (length(args) < 1) {

  stop("Usage: Rscript extract_natality_vbac.R YEAR")

}



YEAR <- as.integer(args[1])



if (is.na(YEAR) || !(YEAR %in% 2016:2024)) {

  stop("YEAR must be between 2016 and 2024.")

}



# ---------------------------------------------------------------------

# 2. DIRECTORIES

# ---------------------------------------------------------------------



BASE_DIR <- "/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"



OUT_DIR <- file.path(BASE_DIR, "harmonised")



if (!dir.exists(OUT_DIR)) {

  dir.create(OUT_DIR, recursive = TRUE)

}



YEAR_DIR <- file.path(BASE_DIR, paste0("Nat", YEAR, "us"))



if (!dir.exists(YEAR_DIR)) {

  stop("Directory not found: ", YEAR_DIR)

}



# Find the extracted Natality txt file automatically

txt_files <- list.files(

  YEAR_DIR,

  pattern = "\\.txt$",

  full.names = TRUE

)



if (length(txt_files) == 0) {

  stop("No .txt Natality file found in: ", YEAR_DIR)

}



if (length(txt_files) > 1) {

  message("Multiple TXT files found. Using first one:")

  print(txt_files)

}



INPUT_FILE <- txt_files[1]



message("============================================================")

message("CDC Natality VBAC extraction")

message("YEAR       : ", YEAR)

message("INPUT FILE : ", INPUT_FILE)

message("OUTPUT DIR : ", OUT_DIR)

message("STARTED    : ", Sys.time())

message("============================================================")



# ---------------------------------------------------------------------

# 3. FIXED-WIDTH HARMONISATION SPECIFICATION

# ---------------------------------------------------------------------

#

# Positions are 1-based and inclusive.

#

# These variables are stable across the harmonisation specification

# for 2016-2024.

# ---------------------------------------------------------------------



spec <- data.frame(

  original = c(

    "DOB_YY",

    "DOB_MM",

    "MAGE_IMPFLG",

    "MAGER",

    "MRACE6",

    "MHISP_R",

    "MRACEHISP",

    "DMAR",

    "MEDUC",



    "PRIORLIVE",

    "PRIORDEAD",

    "LBO_REC",



    "PRECARE",

    "PREVIS",



    "WIC",

    "CIG_0",



    "M_Ht_In",

    "BMI",

    "BMI_R",

    "PWgt_R",



    "RF_PDIAB",

    "RF_GDIAB",

    "RF_PHYPE",

    "RF_GHYPE",

    "RF_PPTERM",



    "RF_CESAR",

    "RF_CESARN",



    "LD_INDL",



    "ME_PRES",

    "ME_ROUT",

    "ME_TRIAL",

    "RDMETH_REC",

    "DMETH_REC",



    "PAY_REC",



    "DPLURAL",



    "OEGest_Comb",

    "OEGest_R3"

  ),



  standardized = c(

    "birth_year",

    "birth_month",

    "maternal_age_imputed",

    "maternal_age",

    "maternal_race6",

    "maternal_hispanic_origin",

    "race_ethnicity",

    "marital_status",

    "maternal_education",



    "prior_live_births_living",

    "prior_live_births_dead",

    "live_birth_order",



    "prenatal_care_start_month",

    "prenatal_visits",



    "wic",

    "cigarettes_pre_pregnancy",



    "maternal_height_in",

    "prepregnancy_bmi",

    "prepregnancy_bmi_cat",

    "prepregnancy_weight_lb",



    "prepregnancy_diabetes",

    "gestational_diabetes",

    "prepregnancy_hypertension",

    "gestational_hypertension",

    "previous_preterm_birth",



    "previous_cesarean",

    "n_previous_cesareans",



    "induction_labor",



    "fetal_presentation",

    "delivery_route",

    "trial_of_labor_if_cesarean",

    "delivery_method_recode",

    "delivery_binary_recode",



    "payment_recode",



    "plurality",



    "obstetric_gest_age_wk",

    "term_recode"

  ),



  start = c(

    9,

    13,

    73,

    75,

    107,

    115,

    117,

    120,

    124,



    171,

    173,

    179,



    224,

    238,



    251,

    253,



    280,

    283,

    287,

    292,



    313,

    314,

    315,

    316,

    318,



    331,

    332,



    383,



    401,

    402,

    403,

    407,

    408,



    436,



    454,



    499,

    503

  ),



  end = c(

    12,

    14,

    73,

    76,

    107,

    115,

    117,

    120,

    124,



    172,

    174,

    179,



    225,

    239,



    251,

    254,



    281,

    286,

    287,

    294,



    313,

    314,

    315,

    316,

    318,



    331,

    333,



    383,



    401,

    402,

    403,

    407,

    408,



    436,



    454,



    500,

    503

  ),

  stringsAsFactors = FALSE

)



# ---------------------------------------------------------------------

# 4. HELPER FUNCTIONS

# ---------------------------------------------------------------------



clean_char <- function(x) {

  x <- trimws(x)

  x[x == ""] <- NA_character_

  x

}



safe_num <- function(x) {

  x <- clean_char(x)

  suppressWarnings(as.numeric(x))

}



yes_no <- function(x) {

  x <- clean_char(x)



  out <- rep(NA_integer_, length(x))

  out[x == "Y"] <- 1L

  out[x == "N"] <- 0L



  out

}





extract_fields <- function(lines, specification) {



  output <- vector(

    mode = "list",

    length = nrow(specification)

  )



  names(output) <- specification$standardized



  for (j in seq_len(nrow(specification))) {



    output[[j]] <- clean_char(

      substr(

        lines,

        specification$start[j],

        specification$end[j]

      )

    )

  }



  as.data.frame(

    output,

    stringsAsFactors = FALSE

  )

}





convert_variables <- function(d) {



  # -------------------------------------------------------------

  # Numeric variables

  # -------------------------------------------------------------



  numeric_vars <- c(

    "birth_year",

    "birth_month",

    "maternal_age",

    "maternal_race6",

    "maternal_hispanic_origin",

    "race_ethnicity",

    "marital_status",

    "maternal_education",



    "prior_live_births_living",

    "prior_live_births_dead",

    "live_birth_order",



    "prenatal_care_start_month",

    "prenatal_visits",



    "cigarettes_pre_pregnancy",



    "maternal_height_in",

    "prepregnancy_bmi",

    "prepregnancy_bmi_cat",

    "prepregnancy_weight_lb",



    "n_previous_cesareans",



    "fetal_presentation",

    "delivery_route",

    "delivery_method_recode",

    "delivery_binary_recode",



    "payment_recode",

    "plurality",



    "obstetric_gest_age_wk",

    "term_recode"

  )



  for (v in numeric_vars) {

    d[[v]] <- safe_num(d[[v]])

  }



  # -------------------------------------------------------------

  # Explicit CDC missing codes

  # -------------------------------------------------------------



  d$prior_live_births_living[

    d$prior_live_births_living == 99

  ] <- NA



  d$prior_live_births_dead[

    d$prior_live_births_dead == 99

  ] <- NA



  d$live_birth_order[

    d$live_birth_order == 9

  ] <- NA



  d$prenatal_care_start_month[

    d$prenatal_care_start_month == 99

  ] <- NA



  d$prenatal_visits[

    d$prenatal_visits == 99

  ] <- NA



  d$cigarettes_pre_pregnancy[

    d$cigarettes_pre_pregnancy == 99

  ] <- NA



  d$maternal_height_in[

    d$maternal_height_in == 99

  ] <- NA



  d$prepregnancy_bmi[

    d$prepregnancy_bmi == 99.9

  ] <- NA



  d$prepregnancy_bmi_cat[

    d$prepregnancy_bmi_cat == 9

  ] <- NA



  d$prepregnancy_weight_lb[

    d$prepregnancy_weight_lb == 999

  ] <- NA



  d$n_previous_cesareans[

    d$n_previous_cesareans == 99

  ] <- NA



  d$fetal_presentation[

    d$fetal_presentation == 9

  ] <- NA



  d$delivery_route[

    d$delivery_route == 9

  ] <- NA



  d$delivery_method_recode[

    d$delivery_method_recode == 9

  ] <- NA



  d$delivery_binary_recode[

    d$delivery_binary_recode == 9

  ] <- NA



  d$payment_recode[

    d$payment_recode == 9

  ] <- NA



  d$obstetric_gest_age_wk[

    d$obstetric_gest_age_wk == 99

  ] <- NA



  d$term_recode[

    d$term_recode == 3

  ] <- NA



  d$race_ethnicity[

    d$race_ethnicity == 8

  ] <- NA



  d$maternal_hispanic_origin[

    d$maternal_hispanic_origin == 9

  ] <- NA



  d$maternal_education[

    d$maternal_education == 9

  ] <- NA



  # -------------------------------------------------------------

  # Y/N variables

  # -------------------------------------------------------------



  d$previous_cesarean_binary <-

    yes_no(d$previous_cesarean)



  d$prepregnancy_diabetes_binary <-

    yes_no(d$prepregnancy_diabetes)



  d$gestational_diabetes_binary <-

    yes_no(d$gestational_diabetes)



  d$prepregnancy_hypertension_binary <-

    yes_no(d$prepregnancy_hypertension)



  d$gestational_hypertension_binary <-

    yes_no(d$gestational_hypertension)



  d$previous_preterm_birth_binary <-

    yes_no(d$previous_preterm_birth)



  d$induction_labor_binary <-

    yes_no(d$induction_labor)



  d$wic_binary <-

    yes_no(d$wic)



  # -------------------------------------------------------------

  # Derived MFMU-style variables

  # -------------------------------------------------------------



  d$maternal_height_cm <-

    d$maternal_height_in * 2.54



  d$prepregnancy_weight_kg <-

    d$prepregnancy_weight_lb * 0.45359237



  d$total_prior_live_births <-

    ifelse(

      !is.na(d$prior_live_births_living) &

      !is.na(d$prior_live_births_dead),

      d$prior_live_births_living +

        d$prior_live_births_dead,

      NA_real_

    )



  # -------------------------------------------------------------

  # QC: consistency of previous C-section variables

  # -------------------------------------------------------------



  d$previous_cesarean_consistent <-

    ifelse(

      !is.na(d$previous_cesarean_binary) &

      !is.na(d$n_previous_cesareans),

      (

        d$previous_cesarean_binary == 1 &

        d$n_previous_cesareans >= 1

      ) |

      (

        d$previous_cesarean_binary == 0 &

        d$n_previous_cesareans == 0

      ),

      NA

    )



  # -------------------------------------------------------------

  # Successful VBAC

  #

  # RDMETH_REC = 2

  # Vaginal birth after previous C-section

  # -------------------------------------------------------------



  d$successful_vbac <-

    !is.na(d$delivery_method_recode) &

    d$delivery_method_recode == 2



  # -------------------------------------------------------------

  # Failed TOLAC

  #

  # Repeat cesarean AND trial of labor attempted

  # -------------------------------------------------------------



  d$failed_tolac <-

    !is.na(d$delivery_method_recode) &

    d$delivery_method_recode == 4 &

    d$trial_of_labor_if_cesarean == "Y"



  # -------------------------------------------------------------

  # TOLAC indicator

  # -------------------------------------------------------------



  d$tolac <-

    d$successful_vbac |

    d$failed_tolac



  # -------------------------------------------------------------

  # Outcome

  #

  # 1 = successful VBAC

  # 0 = failed TOLAC

  # NA = not in identifiable TOLAC cohort

  # -------------------------------------------------------------



  d$vbac <- NA_integer_



  d$vbac[d$successful_vbac] <- 1L

  d$vbac[d$failed_tolac] <- 0L



  # -------------------------------------------------------------

  # Eligibility components

  # -------------------------------------------------------------



  d$one_prior_cesarean <-

    !is.na(d$n_previous_cesareans) &

    d$n_previous_cesareans == 1 &

    d$previous_cesarean == "Y"



  d$singleton <-

    !is.na(d$plurality) &

    d$plurality == 1



  d$cephalic <-

    !is.na(d$fetal_presentation) &

    d$fetal_presentation == 1



  # Primary term definition:

  # obstetric estimate >=37 weeks.

  #

  # We preserve exact gestational age so that an upper bound

  # can be changed later without returning to the raw data.

  d$term <-

    !is.na(d$obstetric_gest_age_wk) &

    d$obstetric_gest_age_wk >= 37



  # -------------------------------------------------------------

  # Sequential TRIPOD participant-flow stages
  #
  # IMPORTANT:
  # These are deliberately defined in the exact order requested
  # for the manuscript flow diagram:
  #
  #   1. All births
  #   2. Previous cesarean
  #   3. Singleton + cephalic + term
  #   4. Exactly one previous cesarean
  #   5. Identifiable TOLAC outcome
  #
  # The final stage is algebraically identical to the original
  # primary_tolac definition below, so this does NOT change the
  # analysis cohort. It only makes the exclusion flow reproducible.
  # -------------------------------------------------------------

  d$flow_previous_cesarean <-

    !is.na(d$previous_cesarean_binary) &

    d$previous_cesarean_binary == 1L



  d$flow_singleton_cephalic_term <-

    d$flow_previous_cesarean &

    d$singleton &

    d$cephalic &

    d$term



  d$flow_exactly_one_previous_cesarean <-

    d$flow_singleton_cephalic_term &

    !is.na(d$n_previous_cesareans) &

    d$n_previous_cesareans == 1



  d$flow_identifiable_tolac <-

    d$flow_exactly_one_previous_cesarean &

    d$tolac



  # -------------------------------------------------------------

  # Primary analysis cohort

  # -------------------------------------------------------------



  d$primary_eligible <-

    d$one_prior_cesarean &

    d$singleton &

    d$cephalic &

    d$term



  d$primary_tolac <-

    d$primary_eligible &

    d$tolac



  d

}





# ---------------------------------------------------------------------

# 5. OUTPUT FILES

# ---------------------------------------------------------------------



OUT_PRIORCS <- file.path(

  OUT_DIR,

  paste0(

    "natality_harmonised_priorcs_",

    YEAR,

    ".csv.gz"

  )

)



OUT_TOLAC <- file.path(

  OUT_DIR,

  paste0(

    "natality_tolac_primary_",

    YEAR,

    ".csv.gz"

  )

)



OUT_QC <- file.path(

  OUT_DIR,

  paste0(

    "natality_qc_",

    YEAR,

    ".csv"

  )

)



OUT_FLOW <- file.path(

  OUT_DIR,

  paste0(

    "natality_tripod_flow_",

    YEAR,

    ".csv"

  )

)



OUT_GEO_STATUS <- file.path(

  OUT_DIR,

  paste0(

    "natality_geography_status_",

    YEAR,

    ".txt"

  )

)



# Remove old outputs to avoid accidental duplicate appending

for (f in c(OUT_PRIORCS, OUT_TOLAC, OUT_QC, OUT_FLOW, OUT_GEO_STATUS)) {

  if (file.exists(f)) {

    file.remove(f)

  }

}



# ---------------------------------------------------------------------

# 6. OPEN CONNECTIONS

# ---------------------------------------------------------------------



input_con <- file(

  INPUT_FILE,

  open = "r",

  encoding = "ASCII"

)



priorcs_con <- gzfile(

  OUT_PRIORCS,

  open = "wt"

)



tolac_con <- gzfile(

  OUT_TOLAC,

  open = "wt"

)



on.exit({

  try(close(input_con), silent = TRUE)

  try(close(priorcs_con), silent = TRUE)

  try(close(tolac_con), silent = TRUE)

}, add = TRUE)



# ---------------------------------------------------------------------

# 7. QC COUNTERS

# ---------------------------------------------------------------------



qc <- list(

  year = YEAR,



  total_records = 0L,



  # Exact sequential TRIPOD flow counters
  flow_all_births = 0L,

  flow_previous_cesarean = 0L,

  flow_singleton_cephalic_term = 0L,

  flow_exactly_one_previous_cesarean = 0L,

  flow_identifiable_tolac = 0L,



  birth_year_mismatch = 0L,



  previous_cesarean_y = 0L,



  previous_cesarean_count_positive = 0L,



  previous_cesarean_inconsistent = 0L,



  exactly_one_previous_cesarean = 0L,



  singleton_after_one_cs = 0L,



  cephalic_after_one_cs_singleton = 0L,



  term_primary_eligible = 0L,



  successful_vbac = 0L,



  failed_tolac = 0L,



  identifiable_tolac_all = 0L,



  primary_tolac = 0L,



  primary_vbac_success = 0L,



  primary_vbac_failure = 0L

)



# ---------------------------------------------------------------------

# 8. CHUNK SETTINGS

# ---------------------------------------------------------------------

#

# 100,000 records corresponds to roughly 133 MB of raw text.

# This is intentionally conservative for HPC memory.

# ---------------------------------------------------------------------



CHUNK_SIZE <- 100000L



chunk_number <- 0L



first_priorcs_write <- TRUE

first_tolac_write <- TRUE



# ---------------------------------------------------------------------

# 9. MAIN EXTRACTION LOOP

# ---------------------------------------------------------------------



repeat {



  lines <- readLines(

    input_con,

    n = CHUNK_SIZE,

    warn = FALSE

  )



  if (length(lines) == 0) {

    break

  }



  chunk_number <- chunk_number + 1L



  message(

    "[",

    Sys.time(),

    "] Year ",

    YEAR,

    " | chunk ",

    chunk_number,

    " | records ",

    length(lines)

  )



  # -------------------------------------------------------------

  # Basic record length QC

  # -------------------------------------------------------------



  line_lengths <- nchar(lines, type = "bytes")



  if (any(line_lengths < 503)) {

    warning(

      "Some records are shorter than position 503 in chunk ",

      chunk_number

    )

  }



  # -------------------------------------------------------------

  # Fixed-width extraction

  # -------------------------------------------------------------



  d <- extract_fields(

    lines,

    spec

  )



  d <- convert_variables(d)



  # Explicit source year

  d$source_year <- YEAR



  # -------------------------------------------------------------

  # QC counts

  # -------------------------------------------------------------



  qc$total_records <-

    qc$total_records + nrow(d)



  qc$flow_all_births <-

    qc$flow_all_births + nrow(d)



  qc$flow_previous_cesarean <-

    qc$flow_previous_cesarean +

    sum(d$flow_previous_cesarean, na.rm = TRUE)



  qc$flow_singleton_cephalic_term <-

    qc$flow_singleton_cephalic_term +

    sum(d$flow_singleton_cephalic_term, na.rm = TRUE)



  qc$flow_exactly_one_previous_cesarean <-

    qc$flow_exactly_one_previous_cesarean +

    sum(d$flow_exactly_one_previous_cesarean, na.rm = TRUE)



  qc$flow_identifiable_tolac <-

    qc$flow_identifiable_tolac +

    sum(d$flow_identifiable_tolac, na.rm = TRUE)



  qc$birth_year_mismatch <-

    qc$birth_year_mismatch +

    sum(

      !is.na(d$birth_year) &

      d$birth_year != YEAR

    )



  qc$previous_cesarean_y <-

    qc$previous_cesarean_y +

    sum(

      d$previous_cesarean == "Y",

      na.rm = TRUE

    )



  qc$previous_cesarean_count_positive <-

    qc$previous_cesarean_count_positive +

    sum(

      !is.na(d$n_previous_cesareans) &

      d$n_previous_cesareans >= 1

    )



  qc$previous_cesarean_inconsistent <-

    qc$previous_cesarean_inconsistent +

    sum(

      d$previous_cesarean_consistent == FALSE,

      na.rm = TRUE

    )



  qc$exactly_one_previous_cesarean <-

    qc$exactly_one_previous_cesarean +

    sum(

      d$one_prior_cesarean,

      na.rm = TRUE

    )



  qc$singleton_after_one_cs <-

    qc$singleton_after_one_cs +

    sum(

      d$one_prior_cesarean &

      d$singleton,

      na.rm = TRUE

    )



  qc$cephalic_after_one_cs_singleton <-

    qc$cephalic_after_one_cs_singleton +

    sum(

      d$one_prior_cesarean &

      d$singleton &

      d$cephalic,

      na.rm = TRUE

    )



  qc$term_primary_eligible <-

    qc$term_primary_eligible +

    sum(

      d$primary_eligible,

      na.rm = TRUE

    )



  qc$successful_vbac <-

    qc$successful_vbac +

    sum(

      d$successful_vbac,

      na.rm = TRUE

    )



  qc$failed_tolac <-

    qc$failed_tolac +

    sum(

      d$failed_tolac,

      na.rm = TRUE

    )



  qc$identifiable_tolac_all <-

    qc$identifiable_tolac_all +

    sum(

      d$tolac,

      na.rm = TRUE

    )



  qc$primary_tolac <-

    qc$primary_tolac +

    sum(

      d$primary_tolac,

      na.rm = TRUE

    )



  qc$primary_vbac_success <-

    qc$primary_vbac_success +

    sum(

      d$primary_tolac &

      d$vbac == 1,

      na.rm = TRUE

    )



  qc$primary_vbac_failure <-

    qc$primary_vbac_failure +

    sum(

      d$primary_tolac &

      d$vbac == 0,

      na.rm = TRUE

    )



  # -------------------------------------------------------------

  # Dataset 1:

  # all records indicating previous C-section

  #

  # Keeps a broader dataset for later sensitivity analyses.

  # -------------------------------------------------------------



  prior_cs_candidate <-

    (

      d$previous_cesarean == "Y"

    ) |

    (

      !is.na(d$n_previous_cesareans) &

      d$n_previous_cesareans >= 1

    )



  prior_cs_candidate[

    is.na(prior_cs_candidate)

  ] <- FALSE



  prior_data <- d[

    prior_cs_candidate,

    ,

    drop = FALSE

  ]



  if (nrow(prior_data) > 0) {



    write.table(

      prior_data,

      file = priorcs_con,

      sep = ",",

      row.names = FALSE,

      col.names = first_priorcs_write,

      quote = TRUE,

      na = ""

    )



    first_priorcs_write <- FALSE

  }



  # -------------------------------------------------------------

  # Dataset 2:

  # PRIMARY MFMU-compatible identifiable TOLAC cohort

  # -------------------------------------------------------------



  primary_data <- d[

    d$primary_tolac %in% TRUE,

    ,

    drop = FALSE

  ]



  if (nrow(primary_data) > 0) {



    write.table(

      primary_data,

      file = tolac_con,

      sep = ",",

      row.names = FALSE,

      col.names = first_tolac_write,

      quote = TRUE,

      na = ""

    )



    first_tolac_write <- FALSE

  }



  # Free memory before next chunk

  rm(

    lines,

    d,

    prior_data,

    primary_data

  )



  gc(verbose = FALSE)

}



# ---------------------------------------------------------------------

# 10. CLOSE CONNECTIONS BEFORE FINAL REPORT

# ---------------------------------------------------------------------



close(input_con)

close(priorcs_con)

close(tolac_con)



# ---------------------------------------------------------------------

# 11. FINAL QC TABLE

# ---------------------------------------------------------------------



qc_df <- data.frame(

  metric = names(qc),

  value = unlist(qc),

  stringsAsFactors = FALSE

)



write.csv(

  qc_df,

  OUT_QC,

  row.names = FALSE

)



# ---------------------------------------------------------------------
# 11B. TRIPOD PARTICIPANT-FLOW TABLE FOR THIS YEAR
# ---------------------------------------------------------------------

flow_df <- data.frame(
  year = YEAR,
  stage_order = 1:5,
  stage = c(
    "All births",
    "Previous cesarean",
    "Singleton, cephalic, and term among previous cesarean births",
    "Exactly one previous cesarean",
    "Identifiable TOLAC outcome"
  ),
  n = c(
    qc$flow_all_births,
    qc$flow_previous_cesarean,
    qc$flow_singleton_cephalic_term,
    qc$flow_exactly_one_previous_cesarean,
    qc$flow_identifiable_tolac
  ),
  stringsAsFactors = FALSE
)

flow_df$excluded_from_previous_stage <- c(
  NA_integer_,
  flow_df$n[-nrow(flow_df)] - flow_df$n[-1]
)

write.csv(
  flow_df,
  OUT_FLOW,
  row.names = FALSE
)



# ---------------------------------------------------------------------
# 11C. CONSISTENCY CHECK: FINAL FLOW MUST MATCH PRIMARY ANALYSIS COHORT
# ---------------------------------------------------------------------

if (qc$flow_identifiable_tolac != qc$primary_tolac) {
  stop(
    paste0(
      "Internal cohort-flow inconsistency in ", YEAR, ": ",
      "flow_identifiable_tolac = ", qc$flow_identifiable_tolac,
      " but primary_tolac = ", qc$primary_tolac,
      ". Do not use outputs until this is resolved."
    )
  )
}



# ---------------------------------------------------------------------
# 11D. GEOGRAPHY / CALIFORNIA SENSITIVITY STATUS
# ---------------------------------------------------------------------
#
# The standard U.S. Natality public-use microdata do not provide
# record-level state/county geography. Therefore this extractor does
# NOT invent or infer a California field from another variable.
#
# If a separate restricted/geocoded Natality source is later supplied,
# state can be merged upstream and the downstream California sensitivity
# code can use that field.
# ---------------------------------------------------------------------

writeLines(
  c(
    paste0("Year: ", YEAR),
    "Record-level state identifier extracted: NO",
    "",
    "Reason:",
    paste0(
      "The standard CDC/NCHS Natality public-use microdata used by this ",
      "pipeline do not include geographic detail such as state or county ",
      "of birth. No state field is therefore fabricated in this script."
    ),
    "",
    "Implication:",
    paste0(
      "A California-exclusion sensitivity analysis cannot be performed ",
      "from these public-use microdata alone. It would require a separate ",
      "data source containing record-level state information."
    )
  ),
  OUT_GEO_STATUS
)



# ---------------------------------------------------------------------

# 12. FINAL SUMMARY

# ---------------------------------------------------------------------



message("")

message("============================================================")

message("YEAR ", YEAR, " COMPLETED")

message("============================================================")

message("Total records              : ", qc$total_records)

message("")
message("TRIPOD FLOW (sequential)")
message("  All births                         : ", qc$flow_all_births)
message("  Previous cesarean                  : ", qc$flow_previous_cesarean)
message("  Singleton + cephalic + term        : ", qc$flow_singleton_cephalic_term)
message("  Exactly one previous cesarean      : ", qc$flow_exactly_one_previous_cesarean)
message("  Identifiable TOLAC outcome         : ", qc$flow_identifiable_tolac)
message("")

message("Previous C-section = Y      : ", qc$previous_cesarean_y)

message("Exactly one previous CS     : ", qc$exactly_one_previous_cesarean)

message("Primary eligible            : ", qc$term_primary_eligible)

message("Successful VBAC             : ", qc$successful_vbac)

message("Failed TOLAC                : ", qc$failed_tolac)

message("Primary TOLAC cohort        : ", qc$primary_tolac)

message("Primary VBAC successes      : ", qc$primary_vbac_success)

message("Primary VBAC failures       : ", qc$primary_vbac_failure)



if (qc$primary_tolac > 0) {



  vbac_rate <-

    qc$primary_vbac_success /

    qc$primary_tolac



  message(

    "Primary VBAC success rate   : ",

    round(100 * vbac_rate, 2),

    "%"

  )

}



message("")

message("Outputs:")

message("  ", OUT_PRIORCS)

message("  ", OUT_TOLAC)

message("  ", OUT_QC)
message("  ", OUT_FLOW)
message("  ", OUT_GEO_STATUS)

message("")

message("Finished: ", Sys.time())

message("============================================================")