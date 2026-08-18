#!/usr/bin/env Rscript

# =============================================================================
# CDC NATALITY TOLAC/VBAC
# RACE-NEUTRAL VS RACE/ETHNICITY-INCLUSIVE COMPARATIVE ANALYSIS
# =============================================================================
#
# Purpose
# -------
# This is a NEW post-hoc comparative analysis requested after review of the
# frozen race-neutral 2024 results. It DOES NOT replace or alter the original
# protocol-frozen race-neutral analysis.
#
# Models
# ------
# A. Frozen race-neutral counselling model:
#    exactly the same counselling-time predictors used previously.
#
# B. Race/ethnicity-inclusive counselling model:
#    exactly the same predictors + race_ethnicity_group.
#
# Scientific question
# -------------------
# What is the measured effect of omitting race/ethnicity on:
#   - overall discrimination and calibration,
#   - subgroup probability accuracy,
#   - subgroup calibration slope/intercept,
#   - and decision-curve net benefit?
#
# Important design note
# ---------------------
# The original race-neutral 2024 evaluation remains the final temporal test.
# The new race-inclusive model is a post-hoc comparative/sensitivity analysis
# motivated by subgroup calibration findings and supervisor review. It must not
# be described as another untouched prospective validation.
#
# Decision-curve action
# ---------------------
# Action = support/encourage planned TOLAC when predicted probability of
# successful VBAC is >= threshold.
#
# True positive  = TOLAC is supported and VBAC occurs.
# False positive = TOLAC is supported but TOLAC fails.
#
# Net benefit = TP/N - FP/N * pt/(1-pt)
#
# The quantity pt/(1-pt) is the relative weight placed on a false-positive
# action versus a missed true-positive opportunity. For example:
#   pt = 0.70 -> weight = 0.70/0.30 = 2.33.
#
# =============================================================================

options(stringsAsFactors = FALSE)
options(scipen = 999)

# =============================================================================
# 1. PATHS AND SETTINGS
# =============================================================================

BASE_DIR <- paste0(
  "/data/brussel/vo/000/bvo00010/vsc11778/",
  "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
)

DATA_FILE <- file.path(
  BASE_DIR,
  "harmonised/model_ready",
  "NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz"
)

FROZEN_RESULT_DIR <- file.path(
  BASE_DIR,
  "harmonised/model_results/logistic_protocol_frozen_2026_08"
)

FROZEN_RDS <- file.path(
  FROZEN_RESULT_DIR,
  "FROZEN_MODELS_2016_2021.rds"
)

FROZEN_IMPUTATION_FILE <- file.path(
  FROZEN_RESULT_DIR,
  "IMPUTATION_VALUES_DEVELOPMENT_ONLY.csv"
)

OUT_DIR <- file.path(
  BASE_DIR,
  "harmonised/model_results/race_inclusive_comparison_2026_08"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Deterministic internal validation.
CV_FOLDS <- 5L
CV_SEED <- 20260815L

# DCA range and key thresholds.
DCA_THRESHOLDS <- seq(0.20, 0.90, by = 0.01)
DCA_KEY_THRESHOLDS <- c(0.50, 0.60, 0.70, 0.75, 0.80)

# Main subgroup list.
SUBGROUPS <- c(
  "race_ethnicity_group",
  "age_group",
  "bmi_group",
  "chronic_htn_group",
  "prepregnancy_diabetes_group",
  "education_group"
)

cat("====================================================================\n")
cat("VBAC race-neutral vs race-inclusive comparative analysis\n")
cat("Started:", format(Sys.time()), "\n")
cat("Data:", DATA_FILE, "\n")
cat("Output:", OUT_DIR, "\n")
cat("====================================================================\n\n")

if (!file.exists(DATA_FILE)) {
  stop("DATA_FILE not found: ", DATA_FILE)
}

# =============================================================================
# 2. LOAD DATA AND RESOLVE ALIASES
# =============================================================================

d <- read.csv(gzfile(DATA_FILE), stringsAsFactors = FALSE)
cat("Total model-ready N:", nrow(d), "\n")

aliases <- list(
  source_year = c("source_year", "birth_year", "year"),
  vbac = c("vbac", "vbac_success"),

  maternal_age = c("maternal_age"),
  maternal_height_cm = c("maternal_height_cm"),
  prepregnancy_weight_kg = c("prepregnancy_weight_kg"),
  prepregnancy_bmi = c("prepregnancy_bmi", "bmi"),

  total_prior_live_births = c(
    "total_prior_live_births",
    "prior_live_births"
  ),
  n_previous_cesareans = c(
    "n_previous_cesareans",
    "number_previous_cesareans",
    "previous_cesarean_count"
  ),
  previous_preterm_birth_binary = c(
    "previous_preterm_birth_binary",
    "previous_preterm_birth"
  ),

  prepregnancy_diabetes_binary = c(
    "prepregnancy_diabetes_binary",
    "prepregnancy_diabetes"
  ),
  gestational_diabetes_binary = c(
    "gestational_diabetes_binary",
    "gestational_diabetes"
  ),
  prepregnancy_hypertension_binary = c(
    "prepregnancy_hypertension_binary",
    "prepregnancy_hypertension"
  ),
  gestational_hypertension_binary = c(
    "gestational_hypertension_binary",
    "gestational_hypertension"
  ),

  maternal_education = c("maternal_education"),
  race_ethnicity_group = c("race_ethnicity_group", "race_ethnicity"),
  age_group = c("age_group"),
  bmi_group = c("bmi_group", "prepregnancy_bmi_cat"),
  chronic_htn_group = c("chronic_htn_group"),
  prepregnancy_diabetes_group = c("prepregnancy_diabetes_group"),
  education_group = c("education_group")
)

resolve_alias <- function(data, target, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0L) {
    if (required) {
      stop(
        "Required variable '", target, "' not found. Tried: ",
        paste(candidates, collapse = ", ")
      )
    }
    return(data)
  }

  if (!(target %in% names(data))) {
    data[[target]] <- data[[hit[1L]]]
    cat("Alias:", hit[1L], "->", target, "\n")
  }
  data
}

if (!("total_prior_live_births" %in% names(d)) &&
    all(c("prior_live_births_living", "prior_live_births_dead") %in% names(d))) {
  d$total_prior_live_births <-
    suppressWarnings(as.numeric(d$prior_live_births_living)) +
    suppressWarnings(as.numeric(d$prior_live_births_dead))
  cat(
    "Derived prior_live_births_living + prior_live_births_dead ",
    "-> total_prior_live_births\n",
    sep = ""
  )
}

required_targets <- c(
  "source_year", "vbac",
  "maternal_age", "maternal_height_cm", "prepregnancy_weight_kg",
  "prepregnancy_bmi", "total_prior_live_births", "n_previous_cesareans",
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary", "gestational_diabetes_binary",
  "prepregnancy_hypertension_binary", "gestational_hypertension_binary",
  "maternal_education", "race_ethnicity_group"
)

for (nm in names(aliases)) {
  d <- resolve_alias(
    d,
    nm,
    aliases[[nm]],
    required = nm %in% required_targets
  )
}

# =============================================================================
# 3. STANDARDIZE BINARY VARIABLES AND SUBGROUP LABELS
# =============================================================================

normalize_binary <- function(x, variable_name) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) || is.integer(x)) return(suppressWarnings(as.numeric(x)))

  z <- toupper(trimws(as.character(x)))
  out <- rep(NA_real_, length(z))

  out[z %in% c("1", "Y", "YES", "TRUE", "T")] <- 1
  out[z %in% c("0", "N", "NO", "FALSE", "F")] <- 0
  out[z %in% c("", "NA", "N/A", "UNKNOWN", "UNK", "U", "9")] <- NA_real_

  allowed <- c(
    "1", "Y", "YES", "TRUE", "T",
    "0", "N", "NO", "FALSE", "F",
    "", "NA", "N/A", "UNKNOWN", "UNK", "U", "9"
  )

  bad <- unique(z[!is.na(z) & !(z %in% allowed)])
  if (length(bad) > 0L) {
    stop(
      "Unrecognized binary coding in ", variable_name, ": ",
      paste(head(bad, 10), collapse = ", ")
    )
  }

  out
}

binary_vars <- c(
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary",
  "prepregnancy_hypertension_binary",
  "gestational_diabetes_binary",
  "gestational_hypertension_binary"
)

for (v in binary_vars) {
  d[[v]] <- normalize_binary(d[[v]], v)
}

d$source_year <- suppressWarnings(as.integer(d$source_year))
d$vbac <- suppressWarnings(as.integer(d$vbac))

if (any(is.na(d$source_year))) stop("source_year contains missing/non-numeric values.")
if (any(!is.na(d$vbac) & !(d$vbac %in% c(0L, 1L)))) {
  stop("vbac must contain only 0/1.")
}

# Standardize race/ethnicity labels.
if (
  is.numeric(d$race_ethnicity_group) ||
  all(grepl("^[0-9.]+$", na.omit(as.character(d$race_ethnicity_group))))
) {
  x <- suppressWarnings(as.integer(as.character(d$race_ethnicity_group)))
  lab <- c(
    `1` = "NH White",
    `2` = "NH Black",
    `3` = "NH AIAN",
    `4` = "NH Asian",
    `5` = "NH NHOPI",
    `6` = "NH Multiracial",
    `7` = "Hispanic",
    `8` = "Missing"
  )
  d$race_ethnicity_group <- unname(lab[as.character(x)])
}

race_chr <- trimws(as.character(d$race_ethnicity_group))
race_chr[race_chr == "" | is.na(race_chr)] <- "Missing"

# Harmonize common label variants.
race_chr[race_chr %in% c("NH >1 race", "NH >1 Race", "NH multiracial")] <-
  "NH Multiracial"
race_chr[race_chr %in% c("Unknown", "UNKNOWN", "Missing/Unknown")] <- "Missing"

# Pre-specify NH White as the reference group.
race_levels_preferred <- c(
  "NH White",
  "NH Black",
  "Hispanic",
  "NH Asian",
  "NH AIAN",
  "NH NHOPI",
  "NH Multiracial",
  "Missing"
)

race_levels_found <- unique(race_chr)
race_levels <- c(
  race_levels_preferred[race_levels_preferred %in% race_levels_found],
  sort(setdiff(race_levels_found, race_levels_preferred))
)

d$race_ethnicity_group <- factor(race_chr, levels = race_levels)

# Derived subgroup variables if absent.
if (!("age_group" %in% names(d)) || all(is.na(d$age_group))) {
  d$age_group <- cut(
    d$maternal_age,
    breaks = c(-Inf, 24, 29, 34, 39, Inf),
    labels = c("<25", "25-29", "30-34", "35-39", "40+"),
    right = TRUE
  )
}

if (!("bmi_group" %in% names(d)) || all(is.na(d$bmi_group))) {
  d$bmi_group <- cut(
    d$prepregnancy_bmi,
    breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
    labels = c(
      "Underweight <18.5",
      "Normal 18.5-24.9",
      "Overweight 25-29.9",
      "Obesity I 30-34.9",
      "Obesity II 35-39.9",
      "Obesity III 40+"
    ),
    right = FALSE
  )
}

if (!("chronic_htn_group" %in% names(d)) || all(is.na(d$chronic_htn_group))) {
  d$chronic_htn_group <- ifelse(
    is.na(d$prepregnancy_hypertension_binary),
    "Missing",
    ifelse(
      d$prepregnancy_hypertension_binary == 1,
      "Chronic hypertension",
      "No chronic hypertension"
    )
  )
}

if (
  !("prepregnancy_diabetes_group" %in% names(d)) ||
  all(is.na(d$prepregnancy_diabetes_group))
) {
  d$prepregnancy_diabetes_group <- ifelse(
    is.na(d$prepregnancy_diabetes_binary),
    "Missing",
    ifelse(
      d$prepregnancy_diabetes_binary == 1,
      "Prepregnancy diabetes",
      "No prepregnancy diabetes"
    )
  )
}

if (!("education_group" %in% names(d)) || all(is.na(d$education_group))) {
  ed <- suppressWarnings(as.integer(d$maternal_education))
  ed_lab <- c(
    `1` = "8th grade or less",
    `2` = "9th-12th, no diploma",
    `3` = "High school/GED",
    `4` = "Some college, no degree",
    `5` = "Associate degree",
    `6` = "Bachelor's degree",
    `7` = "Master's degree",
    `8` = "Doctorate/professional",
    `9` = "Unknown"
  )
  d$education_group <- unname(ed_lab[as.character(ed)])
}

# =============================================================================
# 4. TEMPORAL SPLIT
# =============================================================================

dev_raw <- d[d$source_year >= 2016 & d$source_year <= 2021, , drop = FALSE]
ival_raw <- d[d$source_year >= 2022 & d$source_year <= 2023, , drop = FALSE]
test_raw <- d[d$source_year == 2024, , drop = FALSE]

cat("Development 2016-2021 N:", nrow(dev_raw), "\n")
cat("Intermediate validation 2022-2023 N:", nrow(ival_raw), "\n")
cat("Post-hoc comparative 2024 N:", nrow(test_raw), "\n\n")

if (nrow(dev_raw) == 0 || nrow(ival_raw) == 0 || nrow(test_raw) == 0) {
  stop("One or more temporal partitions are empty.")
}

# =============================================================================
# 5. MODEL DEFINITIONS
# =============================================================================

race_neutral_protocol_vars <- c(
  "maternal_age",
  "maternal_height_cm",
  "prepregnancy_weight_kg",
  "prepregnancy_bmi",
  "total_prior_live_births",
  "n_previous_cesareans",
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary",
  "prepregnancy_hypertension_binary",
  "gestational_diabetes_binary",
  "gestational_hypertension_binary"
)

zero_variance_vars <- function(data, vars) {
  vars[vapply(vars, function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    length(unique(x)) <= 1L
  }, logical(1))]
}

zero_var <- zero_variance_vars(dev_raw, race_neutral_protocol_vars)

race_neutral_vars <- setdiff(race_neutral_protocol_vars, zero_var)

if (length(zero_var) > 0L) {
  cat(
    "Protocol variable(s) not estimable because of zero variance: ",
    paste(zero_var, collapse = ", "), "\n", sep = ""
  )
}

# Race-inclusive model is IDENTICAL except for race_ethnicity_group.
race_inclusive_vars <- c(
  race_neutral_vars,
  "race_ethnicity_group"
)

# =============================================================================
# 6. MISSINGNESS REPORT BEFORE IMPUTATION
# =============================================================================

missingness_vars <- unique(c(
  race_neutral_protocol_vars,
  "race_ethnicity_group"
))

missingness_period <- function(data, period_name, vars) {
  do.call(rbind, lapply(vars, function(v) {
    x <- data[[v]]

    # Race "Missing" is a valid explicit model category but is reported as
    # missingness for transparency.
    if (v == "race_ethnicity_group") {
      miss <- is.na(x) | as.character(x) == "Missing" | as.character(x) == ""
    } else {
      miss <- is.na(x)
    }

    data.frame(
      period = period_name,
      variable = v,
      n_total = length(x),
      n_missing = sum(miss),
      percent_missing = 100 * mean(miss),
      stringsAsFactors = FALSE
    )
  }))
}

missingness_table <- rbind(
  missingness_period(dev_raw, "development_2016_2021", missingness_vars),
  missingness_period(ival_raw, "intermediate_validation_2022_2023", missingness_vars),
  missingness_period(test_raw, "2024_posthoc_comparative_evaluation", missingness_vars)
)

write.csv(
  missingness_table,
  file.path(OUT_DIR, "TABLE_MISSINGNESS_BY_PERIOD.csv"),
  row.names = FALSE
)

# Also summarize rows with >=1 missing clinical predictor before imputation.
clinical_missing_summary <- function(data, period_name, vars) {
  miss_matrix <- sapply(vars, function(v) is.na(data[[v]]))
  if (is.vector(miss_matrix)) miss_matrix <- matrix(miss_matrix, ncol = 1)
  any_missing <- rowSums(miss_matrix) > 0

  data.frame(
    period = period_name,
    n = nrow(data),
    n_with_any_missing_clinical_predictor = sum(any_missing),
    percent_with_any_missing_clinical_predictor = 100 * mean(any_missing),
    stringsAsFactors = FALSE
  )
}

write.csv(
  rbind(
    clinical_missing_summary(dev_raw, "development_2016_2021", race_neutral_vars),
    clinical_missing_summary(ival_raw, "intermediate_validation_2022_2023", race_neutral_vars),
    clinical_missing_summary(test_raw, "2024_posthoc_comparative_evaluation", race_neutral_vars)
  ),
  file.path(OUT_DIR, "TABLE_ANY_MISSING_PREDICTOR_BY_PERIOD.csv"),
  row.names = FALSE
)

# =============================================================================
# 7. APPLY THE ORIGINAL DEVELOPMENT-ONLY IMPUTATION
# =============================================================================

dev <- dev_raw
ival <- ival_raw
test2024 <- test_raw

# Prefer the exact imputation table used by the frozen analysis.
if (file.exists(FROZEN_IMPUTATION_FILE)) {
  imputation_table <- read.csv(
    FROZEN_IMPUTATION_FILE,
    stringsAsFactors = FALSE
  )
  cat("Loaded frozen imputation values from:\n", FROZEN_IMPUTATION_FILE, "\n")
} else {
  warning(
    "Frozen imputation file not found. Recreating development-only imputation."
  )

  get_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_real_)
    tab <- table(x)
    suppressWarnings(as.numeric(names(tab)[which.max(tab)]))
  }

  imputation_table <- data.frame(
    variable = character(),
    method = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )

  for (v in intersect(
    c(
      "maternal_age",
      "maternal_height_cm",
      "prepregnancy_weight_kg",
      "prepregnancy_bmi",
      "total_prior_live_births",
      "n_previous_cesareans"
    ),
    names(dev)
  )) {
    value <- median(dev[[v]], na.rm = TRUE)
    imputation_table <- rbind(
      imputation_table,
      data.frame(
        variable = v,
        method = "development_median",
        value = value
      )
    )
  }

  for (v in binary_vars) {
    value <- get_mode(dev[[v]])
    imputation_table <- rbind(
      imputation_table,
      data.frame(
        variable = v,
        method = "development_mode",
        value = value
      )
    )
  }
}

for (i in seq_len(nrow(imputation_table))) {
  v <- imputation_table$variable[i]
  value <- suppressWarnings(as.numeric(imputation_table$value[i]))

  if (!(v %in% names(dev))) next
  if (!is.finite(value)) stop("Invalid imputation value for ", v)

  dev[[v]][is.na(dev[[v]])] <- value
  ival[[v]][is.na(ival[[v]])] <- value
  test2024[[v]][is.na(test2024[[v]])] <- value
}

# Keep race missingness as an explicit category; do not mode-impute race.
for (obj_name in c("dev", "ival", "test2024")) {
  obj <- get(obj_name)
  race <- as.character(obj$race_ethnicity_group)
  race[is.na(race) | race == ""] <- "Missing"
  obj$race_ethnicity_group <- factor(race, levels = race_levels)
  assign(obj_name, obj)
}

# =============================================================================
# 8. LOAD THE ORIGINAL FROZEN RACE-NEUTRAL MODEL IF AVAILABLE
# =============================================================================

if (file.exists(FROZEN_RDS)) {
  frozen_obj <- readRDS(FROZEN_RDS)

  if (!("race_neutral_counselling" %in% names(frozen_obj))) {
    stop(
      "Frozen RDS exists but does not contain race_neutral_counselling."
    )
  }

  fit_race_neutral <- frozen_obj$race_neutral_counselling
  cat("Loaded original frozen race-neutral model from:\n", FROZEN_RDS, "\n")
} else {
  warning(
    "Frozen RDS not found. Refitting race-neutral model with the frozen formula."
  )

  formula_race_neutral <- as.formula(
    paste("vbac ~", paste(race_neutral_vars, collapse = " + "))
  )

  fit_race_neutral <- glm(
    formula_race_neutral,
    family = binomial(link = "logit"),
    data = dev
  )
}

# Fit race-inclusive model on exactly the same development period.
formula_race_inclusive <- as.formula(
  paste(
    "vbac ~",
    paste(race_inclusive_vars, collapse = " + ")
  )
)

cat("Fitting post-hoc race/ethnicity-inclusive model on 2016-2021...\n")

fit_race_inclusive <- glm(
  formula_race_inclusive,
  family = binomial(link = "logit"),
  data = dev
)

saveRDS(
  list(
    frozen_race_neutral = fit_race_neutral,
    posthoc_race_inclusive = fit_race_inclusive,
    race_neutral_vars = race_neutral_vars,
    race_inclusive_vars = race_inclusive_vars,
    imputation_table = imputation_table,
    analysis_role = paste(
      "Race-inclusive model is a post-hoc comparative/sensitivity analysis;",
      "the original race-neutral 2024 evaluation remains the locked final test."
    )
  ),
  file.path(OUT_DIR, "RACE_NEUTRAL_AND_RACE_INCLUSIVE_MODELS.rds")
)

# =============================================================================
# 9. PREDICTIONS
# =============================================================================

for (obj_name in c("dev", "ival", "test2024")) {
  obj <- get(obj_name)

  obj$pred_race_neutral <- predict(
    fit_race_neutral,
    newdata = obj,
    type = "response"
  )

  obj$pred_race_inclusive <- predict(
    fit_race_inclusive,
    newdata = obj,
    type = "response"
  )

  assign(obj_name, obj)
}

# =============================================================================
# 10. STATISTICAL UTILITIES
# =============================================================================

clip_prob <- function(p, eps = 1e-8) {
  pmin(pmax(p, eps), 1 - eps)
}

wilson_ci <- function(events, n, level = 0.95) {
  if (n <= 0) return(c(lower = NA_real_, upper = NA_real_))

  z <- qnorm(1 - (1 - level) / 2)
  phat <- events / n
  den <- 1 + z^2 / n
  ctr <- (phat + z^2 / (2 * n)) / den
  half <- z * sqrt(
    phat * (1 - phat) / n +
      z^2 / (4 * n^2)
  ) / den

  c(
    lower = max(0, ctr - half),
    upper = min(1, ctr + half)
  )
}

auc_with_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])

  pos <- p[y == 1L]
  neg <- p[y == 0L]

  n1 <- length(pos)
  n0 <- length(neg)

  if (n1 == 0L || n0 == 0L) {
    return(c(auc = NA, lower = NA, upper = NA, se = NA))
  }

  neg_sorted <- sort(neg)
  pos_sorted <- sort(pos)

  left0 <- findInterval(pos, neg_sorted, left.open = TRUE)
  right0 <- findInterval(pos, neg_sorted)
  v10 <- (left0 + 0.5 * (right0 - left0)) / n0

  less1 <- findInterval(neg, pos_sorted, left.open = TRUE)
  leq1 <- findInterval(neg, pos_sorted)
  greater1 <- n1 - leq1
  ties1 <- leq1 - less1
  v01 <- (greater1 + 0.5 * ties1) / n1

  auc <- mean(v10)
  var_auc <- stats::var(v10) / n1 + stats::var(v01) / n0
  se <- sqrt(max(var_auc, 0))
  z <- qnorm(1 - (1 - level) / 2)

  c(
    auc = auc,
    lower = max(0, auc - z * se),
    upper = min(1, auc + z * se),
    se = se
  )
}

brier_with_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  e <- (as.numeric(y[ok]) - as.numeric(p[ok]))^2

  if (length(e) == 0L) {
    return(c(brier = NA, lower = NA, upper = NA))
  }

  est <- mean(e)
  se <- stats::sd(e) / sqrt(length(e))
  z <- qnorm(1 - (1 - level) / 2)

  c(
    brier = est,
    lower = max(0, est - z * se),
    upper = est + z * se
  )
}

calibration_stats_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  y <- as.numeric(y[ok])
  p <- clip_prob(as.numeric(p[ok]))
  lp <- qlogis(p)

  z <- qnorm(1 - (1 - level) / 2)

  fit_i <- glm(
    y ~ 1,
    family = binomial(link = "logit"),
    offset = lp
  )

  est_i <- unname(coef(fit_i)[1])
  se_i <- unname(summary(fit_i)$coefficients[1, 2])

  fit_s <- glm(
    y ~ lp,
    family = binomial(link = "logit")
  )

  est_s <- unname(coef(fit_s)["lp"])
  se_s <- unname(summary(fit_s)$coefficients["lp", 2])

  c(
    intercept = est_i,
    intercept_lower = est_i - z * se_i,
    intercept_upper = est_i + z * se_i,
    slope = est_s,
    slope_lower = est_s - z * se_s,
    slope_upper = est_s + z * se_s
  )
}

oe_stats_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  y <- as.numeric(y[ok])
  p <- as.numeric(p[ok])

  observed <- sum(y)
  expected <- sum(p)

  if (expected <= 0) {
    return(
      c(
        observed = observed,
        expected = expected,
        OE = NA,
        lower = NA,
        upper = NA
      )
    )
  }

  oe <- observed / expected
  alpha <- 1 - level

  if (observed == 0) {
    lo_count <- 0
    hi_count <- -log(alpha)
  } else {
    lo_count <- 0.5 * qchisq(alpha / 2, 2 * observed)
    hi_count <- 0.5 * qchisq(
      1 - alpha / 2,
      2 * (observed + 1)
    )
  }

  c(
    observed = observed,
    expected = expected,
    OE = oe,
    lower = lo_count / expected,
    upper = hi_count / expected
  )
}

evaluate_model <- function(y, p, model_name, dataset_name) {
  ok <- complete.cases(y, p)
  y <- as.numeric(y[ok])
  p <- as.numeric(p[ok])

  n <- length(y)
  events <- sum(y == 1)
  failures <- sum(y == 0)

  rate_ci <- wilson_ci(events, n)
  auc <- auc_with_ci(y, p)
  br <- brier_with_ci(y, p)
  cal <- calibration_stats_ci(y, p)
  oe <- oe_stats_ci(y, p)

  data.frame(
    model = model_name,
    dataset = dataset_name,
    n = n,
    vbac_events = events,
    failed_tolac = failures,
    observed_rate = mean(y),
    observed_rate_lower = rate_ci["lower"],
    observed_rate_upper = rate_ci["upper"],
    mean_predicted = mean(p),
    calibration_gap = mean(p) - mean(y),
    calibration_gap_percentage_points = 100 * (mean(p) - mean(y)),
    absolute_calibration_gap_percentage_points =
      abs(100 * (mean(p) - mean(y))),
    auc = auc["auc"],
    auc_lower = auc["lower"],
    auc_upper = auc["upper"],
    brier = br["brier"],
    brier_lower = br["lower"],
    brier_upper = br["upper"],
    calibration_intercept = cal["intercept"],
    calibration_intercept_lower = cal["intercept_lower"],
    calibration_intercept_upper = cal["intercept_upper"],
    calibration_slope = cal["slope"],
    calibration_slope_lower = cal["slope_lower"],
    calibration_slope_upper = cal["slope_upper"],
    observed_expected = oe["OE"],
    observed_expected_lower = oe["lower"],
    observed_expected_upper = oe["upper"],
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 11. INTERNAL 5-FOLD VALIDATION ON DEVELOPMENT DATA
# =============================================================================
#
# Proper internal validation:
# - deterministic folds stratified by year and outcome,
# - imputation learned inside each training fold,
# - race-neutral and race-inclusive models evaluated on identical held-out folds.
#
# This replaces the misleading "perfect" apparent development calibration row.
# =============================================================================

cat("\nRunning ", CV_FOLDS, "-fold internal validation on 2016-2021...\n", sep = "")

set.seed(CV_SEED)

make_stratified_folds <- function(data, k) {
  strata <- interaction(
    data$source_year,
    data$vbac,
    drop = TRUE
  )

  fold <- integer(nrow(data))

  for (s in levels(strata)) {
    idx <- which(strata == s)
    idx <- sample(idx)
    fold[idx] <- rep(seq_len(k), length.out = length(idx))
  }

  fold
}

fold_id <- make_stratified_folds(dev_raw, CV_FOLDS)

oof_neutral <- rep(NA_real_, nrow(dev_raw))
oof_inclusive <- rep(NA_real_, nrow(dev_raw))

learn_fold_imputation <- function(train, clinical_vars) {
  out <- list()

  for (v in clinical_vars) {
    x <- train[[v]]

    if (v %in% binary_vars) {
      x2 <- x[!is.na(x)]
      tab <- table(x2)
      value <- suppressWarnings(
        as.numeric(names(tab)[which.max(tab)])
      )
    } else {
      value <- median(x, na.rm = TRUE)
    }

    out[[v]] <- value
  }

  out
}

apply_fold_imputation <- function(data, imp) {
  for (v in names(imp)) {
    data[[v]][is.na(data[[v]])] <- imp[[v]]
  }
  data
}

for (fold in seq_len(CV_FOLDS)) {
  cat("  Fold", fold, "of", CV_FOLDS, "\n")

  tr <- dev_raw[fold_id != fold, , drop = FALSE]
  va <- dev_raw[fold_id == fold, , drop = FALSE]

  imp <- learn_fold_imputation(tr, race_neutral_vars)

  tr <- apply_fold_imputation(tr, imp)
  va <- apply_fold_imputation(va, imp)

  # Race missing remains explicit category.
  for (obj_name in c("tr", "va")) {
    obj <- get(obj_name)
    rr <- as.character(obj$race_ethnicity_group)
    rr[is.na(rr) | rr == ""] <- "Missing"
    obj$race_ethnicity_group <- factor(rr, levels = race_levels)
    assign(obj_name, obj)
  }

  fit_n <- glm(
    as.formula(
      paste("vbac ~", paste(race_neutral_vars, collapse = " + "))
    ),
    family = binomial(link = "logit"),
    data = tr
  )

  fit_r <- glm(
    as.formula(
      paste("vbac ~", paste(race_inclusive_vars, collapse = " + "))
    ),
    family = binomial(link = "logit"),
    data = tr
  )

  oof_neutral[fold_id == fold] <- predict(
    fit_n,
    newdata = va,
    type = "response"
  )

  oof_inclusive[fold_id == fold] <- predict(
    fit_r,
    newdata = va,
    type = "response"
  )
}

internal_validation <- rbind(
  evaluate_model(
    dev_raw$vbac,
    oof_neutral,
    "race_neutral_counselling_logistic",
    paste0("development_", CV_FOLDS, "fold_internal_validation")
  ),
  evaluate_model(
    dev_raw$vbac,
    oof_inclusive,
    "race_inclusive_counselling_logistic",
    paste0("development_", CV_FOLDS, "fold_internal_validation")
  )
)

write.csv(
  internal_validation,
  file.path(OUT_DIR, "TABLE_INTERNAL_VALIDATION_5FOLD.csv"),
  row.names = FALSE
)

# =============================================================================
# 12. OVERALL PERFORMANCE COMPARISON
# =============================================================================

overall_performance <- rbind(
  internal_validation,

  evaluate_model(
    ival$vbac,
    ival$pred_race_neutral,
    "race_neutral_counselling_logistic",
    "intermediate_validation_2022_2023"
  ),
  evaluate_model(
    ival$vbac,
    ival$pred_race_inclusive,
    "race_inclusive_counselling_logistic",
    "intermediate_validation_2022_2023"
  ),

  evaluate_model(
    test2024$vbac,
    test2024$pred_race_neutral,
    "race_neutral_counselling_logistic",
    "2024_posthoc_comparative_evaluation"
  ),
  evaluate_model(
    test2024$vbac,
    test2024$pred_race_inclusive,
    "race_inclusive_counselling_logistic",
    "2024_posthoc_comparative_evaluation"
  )
)

write.csv(
  overall_performance,
  file.path(
    OUT_DIR,
    "TABLE_RACE_NEUTRAL_VS_RACE_INCLUSIVE_OVERALL.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 13. SUBGROUP CALIBRATION FOR BOTH MODELS
# =============================================================================

subgroup_calibration <- function(
  data,
  predvar,
  model_name,
  subgroup_variable,
  period_name
) {
  group <- as.character(data[[subgroup_variable]])
  group[is.na(group) | group == ""] <- "Missing"

  levels_found <- sort(unique(group))

  out <- lapply(levels_found, function(g) {
    idx <- group == g

    y <- data$vbac[idx]
    p <- data[[predvar]][idx]

    ok <- complete.cases(y, p)
    y <- y[ok]
    p <- p[ok]

    if (length(y) < 100L || length(unique(y)) < 2L) {
      return(NULL)
    }

    ev <- evaluate_model(
      y,
      p,
      model_name,
      period_name
    )

    ev$subgroup_variable <- subgroup_variable
    ev$subgroup_level <- g

    ev
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) return(NULL)

  ans <- do.call(rbind, out)

  ans[, c(
    "dataset",
    "model",
    "subgroup_variable",
    "subgroup_level",
    setdiff(
      names(ans),
      c("dataset", "model", "subgroup_variable", "subgroup_level")
    )
  )]
}

all_subgroup_results <- list()
k <- 0L

for (sg in SUBGROUPS) {
  for (period_info in list(
    list(
      data = ival,
      period = "intermediate_validation_2022_2023"
    ),
    list(
      data = test2024,
      period = "2024_posthoc_comparative_evaluation"
    )
  )) {
    dat <- period_info$data
    period_name <- period_info$period

    k <- k + 1L
    all_subgroup_results[[k]] <- subgroup_calibration(
      dat,
      "pred_race_neutral",
      "race_neutral_counselling_logistic",
      sg,
      period_name
    )

    k <- k + 1L
    all_subgroup_results[[k]] <- subgroup_calibration(
      dat,
      "pred_race_inclusive",
      "race_inclusive_counselling_logistic",
      sg,
      period_name
    )
  }
}

all_subgroup_results <- do.call(
  rbind,
  Filter(Negate(is.null), all_subgroup_results)
)

rownames(all_subgroup_results) <- NULL

write.csv(
  all_subgroup_results,
  file.path(
    OUT_DIR,
    "TABLE_FULL_SUBGROUP_RESULTS_BOTH_MODELS.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 14. RACE/ETHNICITY TRADE-OFF TABLE
# =============================================================================

race_2024 <- all_subgroup_results[
  all_subgroup_results$dataset ==
    "2024_posthoc_comparative_evaluation" &
    all_subgroup_results$subgroup_variable ==
      "race_ethnicity_group",
  ,
  drop = FALSE
]

neutral_race <- race_2024[
  race_2024$model == "race_neutral_counselling_logistic",
  ,
  drop = FALSE
]

inclusive_race <- race_2024[
  race_2024$model == "race_inclusive_counselling_logistic",
  ,
  drop = FALSE
]

keep_cols <- c(
  "subgroup_level",
  "n",
  "vbac_events",
  "failed_tolac",
  "observed_rate",
  "mean_predicted",
  "calibration_gap_percentage_points",
  "absolute_calibration_gap_percentage_points",
  "observed_expected",
  "observed_expected_lower",
  "observed_expected_upper",
  "calibration_intercept",
  "calibration_intercept_lower",
  "calibration_intercept_upper",
  "calibration_slope",
  "calibration_slope_lower",
  "calibration_slope_upper",
  "auc",
  "auc_lower",
  "auc_upper"
)

neutral_race <- neutral_race[, keep_cols, drop = FALSE]
inclusive_race <- inclusive_race[, keep_cols, drop = FALSE]

names(neutral_race)[-1] <- paste0(
  "race_neutral_",
  names(neutral_race)[-1]
)

names(inclusive_race)[-1] <- paste0(
  "race_inclusive_",
  names(inclusive_race)[-1]
)

race_tradeoff <- merge(
  neutral_race,
  inclusive_race,
  by = "subgroup_level",
  all = TRUE
)

# Positive value = adding race reduced absolute calibration error.
race_tradeoff$improvement_in_absolute_calibration_gap_pp <-
  race_tradeoff$race_neutral_absolute_calibration_gap_percentage_points -
  race_tradeoff$race_inclusive_absolute_calibration_gap_percentage_points

# Change in O/E distance from 1.
race_tradeoff$improvement_in_absolute_oe_error <-
  abs(race_tradeoff$race_neutral_observed_expected - 1) -
  abs(race_tradeoff$race_inclusive_observed_expected - 1)

write.csv(
  race_tradeoff,
  file.path(
    OUT_DIR,
    "TABLE_2024_RACE_ETHNICITY_CALIBRATION_TRADEOFF.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 15. DECISION CURVE WITH EXPLICIT ACTION
# =============================================================================

decision_curve <- function(
  y,
  p,
  model_name,
  period_name,
  thresholds
) {
  ok <- complete.cases(y, p)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])

  N <- length(y)
  prevalence <- mean(y)

  do.call(
    rbind,
    lapply(thresholds, function(pt) {
      action_positive <- p >= pt

      TP <- sum(action_positive & y == 1L)
      FP <- sum(action_positive & y == 0L)
      FN <- sum(!action_positive & y == 1L)
      TN <- sum(!action_positive & y == 0L)

      fp_weight <- pt / (1 - pt)

      nb_model <- TP / N - FP / N * fp_weight

      TP_all <- sum(y == 1L)
      FP_all <- sum(y == 0L)

      nb_all <- TP_all / N - FP_all / N * fp_weight

      data.frame(
        period = period_name,
        model = model_name,
        threshold = pt,
        false_positive_weight = fp_weight,
        outcome_prevalence = prevalence,

        action =
          "support_or_encourage_planned_TOLAC",

        true_positive_definition =
          "TOLAC_supported_and_VBAC_occurs",

        false_positive_definition =
          "TOLAC_supported_but_TOLAC_fails",

        TP = TP,
        FP = FP,
        FN = FN,
        TN = TN,

        net_benefit = nb_model,
        net_benefit_all = nb_all,
        net_benefit_none = 0,

        standardized_net_benefit =
          nb_model / prevalence,

        stringsAsFactors = FALSE
      )
    })
  )
}

dca_2024 <- rbind(
  decision_curve(
    test2024$vbac,
    test2024$pred_race_neutral,
    "race_neutral_counselling_logistic",
    "2024_posthoc_comparative_evaluation",
    DCA_THRESHOLDS
  ),
  decision_curve(
    test2024$vbac,
    test2024$pred_race_inclusive,
    "race_inclusive_counselling_logistic",
    "2024_posthoc_comparative_evaluation",
    DCA_THRESHOLDS
  )
)

write.csv(
  dca_2024,
  file.path(
    OUT_DIR,
    "TABLE_DCA_BOTH_MODELS_2024.csv"
  ),
  row.names = FALSE
)

write.csv(
  dca_2024[
    dca_2024$threshold %in% DCA_KEY_THRESHOLDS,
    ,
    drop = FALSE
  ],
  file.path(
    OUT_DIR,
    "TABLE_DCA_BOTH_MODELS_2024_KEY_THRESHOLDS.csv"
  ),
  row.names = FALSE
)

# Write explicit DCA interpretation note.
prevalence_2024 <- mean(test2024$vbac)

dca_note <- c(
  "Decision-curve action definition",
  "================================",
  "",
  "Action: support/encourage planned TOLAC when predicted probability",
  "of successful VBAC is greater than or equal to the threshold.",
  "",
  "True positive:",
  "  TOLAC is supported/encouraged and VBAC occurs.",
  "",
  "False positive:",
  "  TOLAC is supported/encouraged but TOLAC fails.",
  "",
  "Net benefit:",
  "  TP/N - FP/N * pt/(1-pt)",
  "",
  paste0(
    "Observed 2024 VBAC prevalence = ",
    sprintf("%.4f", prevalence_2024),
    " (",
    sprintf("%.2f", 100 * prevalence_2024),
    "%)."
  ),
  "",
  "Because VBAC prevalence is high, treat-all is difficult to outperform",
  "at low thresholds. Model separation is therefore expected mainly when",
  "thresholds approach or exceed the population VBAC prevalence.",
  "",
  paste0(
    "At pt=0.70, the false-positive weight is ",
    sprintf("%.2f", 0.70 / 0.30),
    "."
  ),
  paste0(
    "At pt=0.75, the false-positive weight is ",
    sprintf("%.2f", 0.75 / 0.25),
    "."
  ),
  paste0(
    "At pt=0.80, the false-positive weight is ",
    sprintf("%.2f", 0.80 / 0.20),
    "."
  ),
  "",
  "These are decision-analytic preference weights, not claims that the",
  "medical consequences of failed TOLAC and planned repeat cesarean can",
  "be reduced to a single universally applicable clinical ratio."
)

writeLines(
  dca_note,
  file.path(
    OUT_DIR,
    "DCA_ACTION_AND_THRESHOLD_INTERPRETATION.txt"
  )
)

# =============================================================================
# 16. SUBGROUP DCA BY RACE/ETHNICITY
# =============================================================================

race_groups <- sort(
  unique(as.character(test2024$race_ethnicity_group))
)

race_groups <- race_groups[
  !is.na(race_groups) &
  race_groups != "" &
  race_groups != "Missing"
]

race_dca <- list()
k <- 0L

for (g in race_groups) {
  idx <- as.character(test2024$race_ethnicity_group) == g

  if (sum(idx) < 100L || length(unique(test2024$vbac[idx])) < 2L) {
    next
  }

  k <- k + 1L
  tmp_n <- decision_curve(
    test2024$vbac[idx],
    test2024$pred_race_neutral[idx],
    "race_neutral_counselling_logistic",
    "2024_posthoc_comparative_evaluation",
    DCA_THRESHOLDS
  )
  tmp_n$race_ethnicity_group <- g
  race_dca[[k]] <- tmp_n

  k <- k + 1L
  tmp_r <- decision_curve(
    test2024$vbac[idx],
    test2024$pred_race_inclusive[idx],
    "race_inclusive_counselling_logistic",
    "2024_posthoc_comparative_evaluation",
    DCA_THRESHOLDS
  )
  tmp_r$race_ethnicity_group <- g
  race_dca[[k]] <- tmp_r
}

race_dca <- do.call(rbind, race_dca)

write.csv(
  race_dca,
  file.path(
    OUT_DIR,
    "TABLE_2024_RACE_ETHNICITY_SUBGROUP_DCA.csv"
  ),
  row.names = FALSE
)

write.csv(
  race_dca[
    race_dca$threshold %in% DCA_KEY_THRESHOLDS,
    ,
    drop = FALSE
  ],
  file.path(
    OUT_DIR,
    "TABLE_2024_RACE_ETHNICITY_SUBGROUP_DCA_KEY_THRESHOLDS.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 17. MODEL COEFFICIENTS
# =============================================================================

extract_coefs <- function(fit, model_name) {
  s <- summary(fit)$coefficients
  z <- qnorm(0.975)

  data.frame(
    model = model_name,
    term = rownames(s),
    estimate = s[, 1],
    std_error = s[, 2],
    lower_95 = s[, 1] - z * s[, 2],
    upper_95 = s[, 1] + z * s[, 2],
    odds_ratio = exp(s[, 1]),
    odds_ratio_lower_95 = exp(s[, 1] - z * s[, 2]),
    odds_ratio_upper_95 = exp(s[, 1] + z * s[, 2]),
    z = s[, 3],
    p_value = s[, 4],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

coefficients <- rbind(
  extract_coefs(
    fit_race_neutral,
    "race_neutral_counselling_logistic"
  ),
  extract_coefs(
    fit_race_inclusive,
    "race_inclusive_counselling_logistic"
  )
)

write.csv(
  coefficients,
  file.path(
    OUT_DIR,
    "MODEL_COEFFICIENTS_BOTH_MODELS_WITH_95CI.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 18. OPTIONAL CALIFORNIA/HISPANIC SENSITIVITY
# =============================================================================
#
# This is attempted only if a usable state variable exists in the model-ready
# file. If no state field exists, the script records that the sensitivity
# cannot be performed from this file.
# =============================================================================

state_candidates <- c(
  "state",
  "state_code",
  "residence_state",
  "state_residence",
  "birth_state",
  "occurrence_state",
  "state_occurrence",
  "stresfip",
  "stoccfip"
)

state_hit <- state_candidates[state_candidates %in% names(d)]

if (length(state_hit) > 0L) {
  state_var <- state_hit[1L]

  identify_california <- function(x) {
    z <- toupper(trimws(as.character(x)))

    z %in% c(
      "CA",
      "CALIFORNIA",
      "06",
      "6",
      "006"
    )
  }

  is_ca_test <- identify_california(test2024[[state_var]])

  hispanic_test <- as.character(
    test2024$race_ethnicity_group
  ) == "Hispanic"

  sensitivity_rows <- list()
  k <- 0L

  for (definition in c(
    "all_Hispanic_2024",
    "Hispanic_2024_excluding_California"
  )) {
    if (definition == "all_Hispanic_2024") {
      idx <- hispanic_test
    } else {
      idx <- hispanic_test & !is_ca_test
    }

    if (sum(idx) >= 100L && length(unique(test2024$vbac[idx])) == 2L) {
      k <- k + 1L
      a <- evaluate_model(
        test2024$vbac[idx],
        test2024$pred_race_neutral[idx],
        "race_neutral_counselling_logistic",
        definition
      )
      a$state_variable_used <- state_var
      sensitivity_rows[[k]] <- a

      k <- k + 1L
      b <- evaluate_model(
        test2024$vbac[idx],
        test2024$pred_race_inclusive[idx],
        "race_inclusive_counselling_logistic",
        definition
      )
      b$state_variable_used <- state_var
      sensitivity_rows[[k]] <- b
    }
  }

  if (length(sensitivity_rows) > 0L) {
    california_sensitivity <- do.call(
      rbind,
      sensitivity_rows
    )

    write.csv(
      california_sensitivity,
      file.path(
        OUT_DIR,
        "TABLE_HISPANIC_CALIBRATION_CALIFORNIA_SENSITIVITY.csv"
      ),
      row.names = FALSE
    )
  }

} else {
  writeLines(
    c(
      "California/Hispanic sensitivity was not run.",
      "",
      "Reason:",
      "No state variable was found in the model-ready analytical file.",
      "",
      "The 2019 California Hispanic-origin reporting change should therefore",
      "be acknowledged as a potential harmonisation contributor unless an",
      "upstream file containing state information is used for sensitivity analysis."
    ),
    file.path(
      OUT_DIR,
      "CALIFORNIA_SENSITIVITY_NOT_POSSIBLE_FROM_MODEL_READY_DATA.txt"
    )
  )
}

# =============================================================================
# 19. AUC SPREAD / TEMPORAL DISPERSION DIAGNOSTIC
# =============================================================================
#
# Supervisor hypothesis: AUC may have risen because predictor/case-mix spread
# increased as cardiometabolic prevalence rose.
#
# We do not state this as fact. We quantify the spread of the frozen model's
# linear predictor over time to assess whether case-mix dispersion increased.
# =============================================================================

dev$lp_race_neutral <- predict(
  fit_race_neutral,
  newdata = dev,
  type = "link"
)

ival$lp_race_neutral <- predict(
  fit_race_neutral,
  newdata = ival,
  type = "link"
)

test2024$lp_race_neutral <- predict(
  fit_race_neutral,
  newdata = test2024,
  type = "link"
)

lp_spread <- rbind(
  data.frame(
    period = "development_2016_2021",
    n = nrow(dev),
    lp_mean = mean(dev$lp_race_neutral),
    lp_sd = sd(dev$lp_race_neutral),
    lp_iqr = IQR(dev$lp_race_neutral),
    predicted_probability_sd = sd(dev$pred_race_neutral),
    stringsAsFactors = FALSE
  ),
  data.frame(
    period = "intermediate_validation_2022_2023",
    n = nrow(ival),
    lp_mean = mean(ival$lp_race_neutral),
    lp_sd = sd(ival$lp_race_neutral),
    lp_iqr = IQR(ival$lp_race_neutral),
    predicted_probability_sd = sd(ival$pred_race_neutral),
    stringsAsFactors = FALSE
  ),
  data.frame(
    period = "2024_posthoc_comparative_evaluation",
    n = nrow(test2024),
    lp_mean = mean(test2024$lp_race_neutral),
    lp_sd = sd(test2024$lp_race_neutral),
    lp_iqr = IQR(test2024$lp_race_neutral),
    predicted_probability_sd = sd(test2024$pred_race_neutral),
    stringsAsFactors = FALSE
  )
)

write.csv(
  lp_spread,
  file.path(
    OUT_DIR,
    "TABLE_LINEAR_PREDICTOR_SPREAD_BY_PERIOD.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 20. PARTICIPANT FLOW LIMITATION
# =============================================================================
#
# The model-ready file already contains the selected TOLAC cohort. It cannot
# reconstruct the path from all U.S. births through every exclusion. We output
# the final-period counts and an explicit note rather than inventing upstream
# exclusion counts.
# =============================================================================

final_cohort_counts <- aggregate(
  cbind(
    n = rep(1L, nrow(d)),
    vbac_events = d$vbac,
    failed_tolac = 1L - d$vbac
  ),
  by = list(source_year = d$source_year),
  FUN = sum
)

write.csv(
  final_cohort_counts,
  file.path(
    OUT_DIR,
    "PARTICIPANT_FLOW_FINAL_ANALYTIC_COHORT_BY_YEAR.csv"
  ),
  row.names = FALSE
)

writeLines(
  c(
    "Participant-flow limitation",
    "============================",
    "",
    "The model-ready file begins after the TOLAC cohort has already been",
    "constructed. Therefore it cannot recover counts for:",
    "  all births -> prior cesarean -> singleton -> cephalic -> term ->",
    "  exactly one previous cesarean -> identifiable TOLAC outcome -> final cohort.",
    "",
    "Use the upstream cohort-construction/QC script or year-specific harmonised",
    "files to produce the full TRIPOD participant-flow diagram.",
    "",
    "This analysis intentionally does NOT invent those exclusion counts."
  ),
  file.path(
    OUT_DIR,
    "PARTICIPANT_FLOW_REQUIRES_UPSTREAM_COHORT_SCRIPT.txt"
  )
)

# =============================================================================
# 21. SAVE COMPARATIVE PREDICTIONS
# =============================================================================

prediction_2024 <- data.frame(
  source_year = test2024$source_year,
  vbac = test2024$vbac,
  race_ethnicity_group = test2024$race_ethnicity_group,
  age_group = test2024$age_group,
  bmi_group = test2024$bmi_group,
  chronic_htn_group = test2024$chronic_htn_group,
  prepregnancy_diabetes_group = test2024$prepregnancy_diabetes_group,
  education_group = test2024$education_group,
  pred_race_neutral = test2024$pred_race_neutral,
  pred_race_inclusive = test2024$pred_race_inclusive
)

con <- gzfile(
  file.path(
    OUT_DIR,
    "PREDICTIONS_2024_RACE_NEUTRAL_VS_RACE_INCLUSIVE.csv.gz"
  ),
  open = "wt"
)

write.csv(
  prediction_2024,
  con,
  row.names = FALSE
)

close(con)

# =============================================================================
# 22. RUN MANIFEST
# =============================================================================

manifest <- c(
  paste("run_time=", format(Sys.time()), sep = ""),
  paste("data_file=", DATA_FILE, sep = ""),
  "analysis_type=posthoc_race_inclusive_comparison",
  paste(
    "important_note=",
    "original_race_neutral_2024_evaluation_remains_the_locked_final_test",
    sep = ""
  ),
  "development_years=2016-2021",
  "intermediate_validation_years=2022-2023",
  "posthoc_comparative_evaluation_year=2024",
  paste("cv_folds=", CV_FOLDS, sep = ""),
  paste("cv_seed=", CV_SEED, sep = ""),
  paste(
    "race_neutral_vars=",
    paste(race_neutral_vars, collapse = ";"),
    sep = ""
  ),
  paste(
    "race_inclusive_addition=race_ethnicity_group"
  ),
  paste(
    "race_reference_level=",
    levels(dev$race_ethnicity_group)[1],
    sep = ""
  ),
  paste(
    "zero_variance_protocol_vars=",
    paste(zero_var, collapse = ";"),
    sep = ""
  ),
  "dca_action=support_or_encourage_planned_TOLAC",
  "dca_true_positive=TOLAC_supported_and_VBAC_occurs",
  "dca_false_positive=TOLAC_supported_but_TOLAC_fails",
  paste("dca_threshold_min=", min(DCA_THRESHOLDS), sep = ""),
  paste("dca_threshold_max=", max(DCA_THRESHOLDS), sep = ""),
  paste(
    "dca_key_thresholds=",
    paste(DCA_KEY_THRESHOLDS, collapse = ";"),
    sep = ""
  )
)

writeLines(
  manifest,
  file.path(
    OUT_DIR,
    "RUN_MANIFEST_RACE_INCLUSIVE_COMPARISON.txt"
  )
)

# =============================================================================
# 23. FINAL SUMMARY
# =============================================================================

cat("\n====================================================================\n")
cat("COMPARATIVE ANALYSIS COMPLETED\n")
cat("====================================================================\n")
cat("\nOverall performance:\n")
print(overall_performance, row.names = FALSE)

cat("\nKey race/ethnicity trade-off table written to:\n")
cat(
  file.path(
    OUT_DIR,
    "TABLE_2024_RACE_ETHNICITY_CALIBRATION_TRADEOFF.csv"
  ),
  "\n"
)

cat("\nDCA action definition written to:\n")
cat(
  file.path(
    OUT_DIR,
    "DCA_ACTION_AND_THRESHOLD_INTERPRETATION.txt"
  ),
  "\n"
)

cat("\nMissingness table written to:\n")
cat(
  file.path(
    OUT_DIR,
    "TABLE_MISSINGNESS_BY_PERIOD.csv"
  ),
  "\n"
)

cat("\nIMPORTANT:\n")
cat(
  "The race-inclusive model is a post-hoc comparative/sensitivity analysis.\n"
)
cat(
  "Do not relabel it as an untouched final temporal validation.\n"
)

cat("\nFinished:", format(Sys.time()), "\n")
cat("Outputs:", OUT_DIR, "\n")
cat("====================================================================\n")
