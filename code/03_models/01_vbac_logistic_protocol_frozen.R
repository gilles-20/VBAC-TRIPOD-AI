#!/usr/bin/env Rscript

# ============================================================================
# CDC NATALITY TOLAC/VBAC - PROTOCOL-FROZEN LOGISTIC ANALYSIS
# ============================================================================
# Scientific framing (supervisor-approved, August 2026):
#   Primary contribution: subgroup calibration + decision-curve net benefit.
#   Temporal validation is the design, not the headline finding.
#
# Time split:
#   Development:              2016-2021
#   Intermediate validation:  2022-2023
#   Final temporal test:       2024
#
# Models:
#   1. Information-restricted comparator
#      = only MFMU concepts recoverable/approximable from public-use Natality.
#      This is NOT the published 2021 MFMU calculator and must never be called
#      an external validation of that calculator.
#   2. Prespecified race-neutral counselling model
#      = predictors available in late third-trimester prenatal counselling,
#        before onset of labour.
#
# Important protection:
#   RUN_FINAL_TEST is FALSE by default. 2024 model performance is not produced
#   unless the protocol/model are frozen and the explicit unlock text is set.
#   Descriptive 2024 subgroup/event counts may still be written because these
#   are not used for model specification/tuning.
# ============================================================================

options(stringsAsFactors = FALSE)
options(scipen = 999)

# ============================================================================
# USER SETTINGS
# ============================================================================

BASE_DIR <- paste0(
  "/data/brussel/vo/000/bvo00010/vsc11778/",
  "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
)

DATA_FILE <- file.path(
  BASE_DIR,
  "harmonised/model_ready/",
  "NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz"
)

OUT_DIR <- file.path(
  BASE_DIR,
  "harmonised/model_results/logistic_protocol_frozen_2026_08"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Final 2024 protection. Leave FALSE until EVERYTHING is frozen.
RUN_FINAL_TEST <- TRUE
FINAL_TEST_UNLOCK_TEXT <- "I_CONFIRM_MODEL_AND_PROTOCOL_ARE_FROZEN"
REQUIRED_FINAL_TEST_UNLOCK_TEXT <- "I_CONFIRM_MODEL_AND_PROTOCOL_ARE_FROZEN"

# Prespecified DCA range for the intermediate validation analysis.
# If clinical literature leads to a different range, change it BEFORE opening
# the final 2024 test set and record the change in the protocol log.
DCA_THRESHOLDS <- seq(0.20, 0.80, by = 0.01)
DCA_KEY_THRESHOLDS <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80)

cat("============================================================\n")
cat("VBAC protocol-frozen logistic analysis\n")
cat("Started:", format(Sys.time()), "\n")
cat("Data:", DATA_FILE, "\n")
cat("Output:", OUT_DIR, "\n")
cat("RUN_FINAL_TEST:", RUN_FINAL_TEST, "\n")
cat("============================================================\n\n")

if (!file.exists(DATA_FILE)) {
  stop("DATA_FILE does not exist: ", DATA_FILE)
}

# ============================================================================
# LOAD DATA
# ============================================================================

d <- read.csv(gzfile(DATA_FILE), stringsAsFactors = FALSE)
cat("Total N:", nrow(d), "\n")

# ============================================================================
# COLUMN ALIAS RESOLUTION
# Makes the script compatible with names already used in the model-ready file
# and with the standardized names in the harmonisation workbook.
# ============================================================================

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

# Variables required to fit the prespecified models.
required_targets <- c(
  "source_year", "vbac",
  "maternal_age", "maternal_height_cm", "prepregnancy_weight_kg",
  "prepregnancy_bmi", "total_prior_live_births", "n_previous_cesareans",
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary", "gestational_diabetes_binary",
  "prepregnancy_hypertension_binary", "gestational_hypertension_binary",
  "maternal_education", "race_ethnicity_group"
)

# If an explicit total-prior-live-births field is absent, reconstruct the
# total only when BOTH living and dead prior-live-birth fields are available.
# Do not silently substitute living births alone for the total.
if (!("total_prior_live_births" %in% names(d)) &&
    all(c("prior_live_births_living", "prior_live_births_dead") %in% names(d))) {
  d$total_prior_live_births <-
    suppressWarnings(as.numeric(d$prior_live_births_living)) +
    suppressWarnings(as.numeric(d$prior_live_births_dead))
  cat("Derived: prior_live_births_living + prior_live_births_dead -> total_prior_live_births\n")
}

for (nm in names(aliases)) {
  d <- resolve_alias(
    d,
    nm,
    aliases[[nm]],
    required = nm %in% required_targets
  )
}

# Standardize common binary encodings if any harmonised field reached the
# model-ready file as Y/N, TRUE/FALSE or Yes/No instead of 0/1.
normalize_binary <- function(x, variable_name) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) || is.integer(x)) return(suppressWarnings(as.numeric(x)))

  z <- toupper(trimws(as.character(x)))
  out <- rep(NA_real_, length(z))
  out[z %in% c("1", "Y", "YES", "TRUE", "T")] <- 1
  out[z %in% c("0", "N", "NO", "FALSE", "F")] <- 0
  out[z %in% c("", "NA", "N/A", "UNKNOWN", "UNK", "U", "9")] <- NA_real_

  bad <- unique(z[!is.na(z) & !(z %in% c(
    "1", "Y", "YES", "TRUE", "T",
    "0", "N", "NO", "FALSE", "F",
    "", "NA", "N/A", "UNKNOWN", "UNK", "U", "9"
  ))])
  if (length(bad) > 0L) {
    stop(
      "Unrecognized binary coding in ", variable_name, ": ",
      paste(head(bad, 10), collapse = ", ")
    )
  }
  out
}

for (v in c(
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary",
  "gestational_diabetes_binary",
  "prepregnancy_hypertension_binary",
  "gestational_hypertension_binary"
)) {
  d[[v]] <- normalize_binary(d[[v]], v)
}

# Basic coding checks.
d$source_year <- suppressWarnings(as.integer(d$source_year))
d$vbac <- suppressWarnings(as.integer(d$vbac))
if (any(!is.na(d$vbac) & !(d$vbac %in% c(0L, 1L)))) {
  stop("Outcome 'vbac' contains values other than 0/1.")
}
if (any(is.na(d$source_year))) {
  stop("source_year contains missing/non-numeric values.")
}

# ============================================================================
# DERIVE STANDARDIZED SUBGROUP VARIABLES WHEN NECESSARY
# ============================================================================

# Race/ethnicity: if the available field is numeric MRACEHISP-style coding,
# convert to human-readable categories. If already labelled, retain labels.
if (is.numeric(d$race_ethnicity_group) || all(grepl("^[0-9.]+$", na.omit(as.character(d$race_ethnicity_group))))) {
  x <- suppressWarnings(as.integer(as.character(d$race_ethnicity_group)))
  lab <- c(
    `1` = "NH White",
    `2` = "NH Black",
    `3` = "NH AIAN",
    `4` = "NH Asian",
    `5` = "NH NHOPI",
    `6` = "NH >1 race",
    `7` = "Hispanic",
    `8` = "Unknown"
  )
  d$race_ethnicity_group <- unname(lab[as.character(x)])
}

if (!("age_group" %in% names(d)) || all(is.na(d$age_group))) {
  d$age_group <- cut(
    d$maternal_age,
    breaks = c(-Inf, 24, 29, 34, 39, Inf),
    labels = c("<25", "25-29", "30-34", "35-39", ">=40"),
    right = TRUE
  )
}

if (!("bmi_group" %in% names(d)) || all(is.na(d$bmi_group))) {
  d$bmi_group <- cut(
    d$prepregnancy_bmi,
    breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
    labels = c("Underweight", "Normal", "Overweight", "Obesity I", "Obesity II", "Obesity III"),
    right = FALSE
  )
}

if (!("chronic_htn_group" %in% names(d)) || all(is.na(d$chronic_htn_group))) {
  d$chronic_htn_group <- ifelse(
    is.na(d$prepregnancy_hypertension_binary), "Missing",
    ifelse(d$prepregnancy_hypertension_binary == 1, "Chronic hypertension", "No chronic hypertension")
  )
}

if (!("prepregnancy_diabetes_group" %in% names(d)) || all(is.na(d$prepregnancy_diabetes_group))) {
  d$prepregnancy_diabetes_group <- ifelse(
    is.na(d$prepregnancy_diabetes_binary), "Missing",
    ifelse(d$prepregnancy_diabetes_binary == 1, "Prepregnancy diabetes", "No prepregnancy diabetes")
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

# ============================================================================
# FIXED TEMPORAL SPLIT - IGNORE ANY OLD analysis_period COLUMN
# ============================================================================

dev <- d[d$source_year >= 2016 & d$source_year <= 2021, , drop = FALSE]
ival <- d[d$source_year >= 2022 & d$source_year <= 2023, , drop = FALSE]
test2024 <- d[d$source_year == 2024, , drop = FALSE]

cat("Development 2016-2021 N:", nrow(dev), "\n")
cat("Intermediate validation 2022-2023 N:", nrow(ival), "\n")
cat("Final temporal test 2024 N:", nrow(test2024), "\n\n")

if (nrow(dev) == 0L || nrow(ival) == 0L || nrow(test2024) == 0L) {
  stop("One or more temporal partitions are empty. Check source_year.")
}

# ============================================================================
# PROTOCOL-FROZEN PREDICTOR SET
# ============================================================================
# Prediction timepoint: late third-trimester prenatal counselling before labour.
#
# Deliberately EXCLUDED from prediction:
#   - total pregnancy weight gain
#   - total prenatal visits
#   - induction
#   - augmentation
#   - delivery method / route
#   - trial-of-labour item
#   - fetal presentation AT DELIVERY as a predictor
#   - race/ethnicity (audit subgroup only)
#   - marital status, WIC and payment source are not in the frozen clinical core
#     model; they may be studied later only as explicitly prespecified secondary
#     analyses, not selected based on validation performance.
#
# The information-restricted comparator is SECONDARY and is NOT the 2021 MFMU
# calculator.

information_restricted_vars <- c(
  "maternal_age",
  "prepregnancy_weight_kg",
  "maternal_height_cm",
  "prepregnancy_hypertension_binary"
)

race_neutral_counselling_vars_protocol <- c(
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

# The primary cohort is expected to contain exactly one prior cesarean.
# Therefore n_previous_cesareans may have zero variance and cannot be estimated.
# Detect this explicitly and document it rather than allowing a silent singular
# coefficient. No variable is selected/dropped based on validation performance.
zero_variance_in_development <- function(data, vars) {
  vars[vapply(vars, function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    length(unique(x)) <= 1L
  }, logical(1))]
}

zero_var_protocol <- zero_variance_in_development(dev, race_neutral_counselling_vars_protocol)
race_neutral_counselling_vars <- setdiff(
  race_neutral_counselling_vars_protocol,
  zero_var_protocol
)

if (length(zero_var_protocol) > 0L) {
  cat(
    "Protocol predictor(s) not estimable because of zero variance in development: ",
    paste(zero_var_protocol, collapse = ", "), "\n", sep = ""
  )
}

# Save the frozen predictor specification as an audit artifact.
protocol_predictors <- data.frame(
  variable = c(information_restricted_vars, race_neutral_counselling_vars_protocol),
  model = c(
    rep("information_restricted_comparator", length(information_restricted_vars)),
    rep("race_neutral_counselling_model", length(race_neutral_counselling_vars_protocol))
  ),
  prediction_timepoint = "late_third_trimester_prenatal_counselling_before_labour",
  estimable_in_primary_development_cohort = c(
    rep(TRUE, length(information_restricted_vars)),
    !(race_neutral_counselling_vars_protocol %in% zero_var_protocol)
  ),
  exclusion_reason_if_not_estimable = c(
    rep("", length(information_restricted_vars)),
    ifelse(
      race_neutral_counselling_vars_protocol %in% zero_var_protocol,
      "zero variance in 2016-2021 primary cohort (e.g., exactly one previous cesarean by eligibility)",
      ""
    )
  ),
  stringsAsFactors = FALSE
)
write.csv(
  protocol_predictors,
  file.path(OUT_DIR, "PROTOCOL_FROZEN_PREDICTOR_SET.csv"),
  row.names = FALSE
)

# ============================================================================
# DEVELOPMENT-ONLY IMPUTATION
# ============================================================================

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  tab <- table(x)
  suppressWarnings(as.numeric(names(tab)[which.max(tab)]))
}

continuous_vars <- c(
  "maternal_age",
  "maternal_height_cm",
  "prepregnancy_weight_kg",
  "prepregnancy_bmi",
  "total_prior_live_births",
  "n_previous_cesareans"
)

binary_vars <- c(
  "previous_preterm_birth_binary",
  "prepregnancy_diabetes_binary",
  "prepregnancy_hypertension_binary",
  "gestational_diabetes_binary",
  "gestational_hypertension_binary"
)

imputation_table <- data.frame(
  variable = character(), method = character(), value = numeric(),
  stringsAsFactors = FALSE
)

for (v in continuous_vars) {
  value <- median(dev[[v]], na.rm = TRUE)
  if (!is.finite(value)) stop("No usable development values for ", v)
  imputation_table <- rbind(
    imputation_table,
    data.frame(variable = v, method = "development_median", value = value)
  )
  dev[[v]][is.na(dev[[v]])] <- value
  ival[[v]][is.na(ival[[v]])] <- value
  test2024[[v]][is.na(test2024[[v]])] <- value
}

for (v in binary_vars) {
  value <- get_mode(dev[[v]])
  if (!is.finite(value)) stop("No usable development values for ", v)
  imputation_table <- rbind(
    imputation_table,
    data.frame(variable = v, method = "development_mode", value = value)
  )
  dev[[v]][is.na(dev[[v]])] <- value
  ival[[v]][is.na(ival[[v]])] <- value
  test2024[[v]][is.na(test2024[[v]])] <- value
}

write.csv(
  imputation_table,
  file.path(OUT_DIR, "IMPUTATION_VALUES_DEVELOPMENT_ONLY.csv"),
  row.names = FALSE
)

# ============================================================================
# MODEL FITTING - DEVELOPMENT ONLY
# ============================================================================

formula_information_restricted <- as.formula(
  paste("vbac ~", paste(information_restricted_vars, collapse = " + "))
)
formula_race_neutral <- as.formula(
  paste("vbac ~", paste(race_neutral_counselling_vars, collapse = " + "))
)

cat("Fitting information-restricted comparator on 2016-2021...\n")
fit_information_restricted <- glm(
  formula_information_restricted,
  family = binomial(link = "logit"),
  data = dev
)

cat("Fitting race-neutral counselling model on 2016-2021...\n")
fit_race_neutral <- glm(
  formula_race_neutral,
  family = binomial(link = "logit"),
  data = dev
)

saveRDS(
  list(
    information_restricted = fit_information_restricted,
    race_neutral_counselling = fit_race_neutral,
    predictor_specification = protocol_predictors,
    imputation = imputation_table
  ),
  file.path(OUT_DIR, "FROZEN_MODELS_2016_2021.rds")
)

# Predictions for development and intermediate validation.
dev$pred_information_restricted <- predict(
  fit_information_restricted, newdata = dev, type = "response"
)
ival$pred_information_restricted <- predict(
  fit_information_restricted, newdata = ival, type = "response"
)
dev$pred_race_neutral <- predict(
  fit_race_neutral, newdata = dev, type = "response"
)
ival$pred_race_neutral <- predict(
  fit_race_neutral, newdata = ival, type = "response"
)

# Do NOT calculate 2024 model predictions unless explicitly unlocked.
if (RUN_FINAL_TEST) {
  if (!identical(FINAL_TEST_UNLOCK_TEXT, REQUIRED_FINAL_TEST_UNLOCK_TEXT)) {
    stop(
      "RUN_FINAL_TEST=TRUE but final-test unlock text is incorrect. ",
      "Set FINAL_TEST_UNLOCK_TEXT exactly to: ", REQUIRED_FINAL_TEST_UNLOCK_TEXT
    )
  }
  marker <- file.path(
    OUT_DIR,
    paste0("FINAL_TEST_OPENED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  )
  writeLines(
    c(
      paste("Final 2024 model test opened:", format(Sys.time())),
      "Model/predictor set must not be changed after this point."
    ),
    marker
  )
  test2024$pred_information_restricted <- predict(
    fit_information_restricted, newdata = test2024, type = "response"
  )
  test2024$pred_race_neutral <- predict(
    fit_race_neutral, newdata = test2024, type = "response"
  )
}

# ============================================================================
# STATISTICAL UTILITIES WITH UNCERTAINTY
# ============================================================================

clip_prob <- function(p, eps = 1e-8) pmin(pmax(p, eps), 1 - eps)

wilson_ci <- function(events, n, level = 0.95) {
  if (n <= 0) return(c(lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - level) / 2)
  phat <- events / n
  den <- 1 + z^2 / n
  ctr <- (phat + z^2 / (2 * n)) / den
  half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / den
  c(lower = max(0, ctr - half), upper = min(1, ctr + half))
}

# AUC and asymptotic CI using placement values (DeLong-style variance).
auc_with_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  y <- as.integer(y[ok]); p <- as.numeric(p[ok])
  pos <- p[y == 1L]; neg <- p[y == 0L]
  n1 <- length(pos); n0 <- length(neg)
  if (n1 == 0L || n0 == 0L) {
    return(c(auc = NA, lower = NA, upper = NA, se = NA))
  }

  # Efficient empirical placements, including half credit for ties.
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
  n <- length(e)
  if (n == 0L) return(c(brier = NA, lower = NA, upper = NA))
  est <- mean(e)
  se <- stats::sd(e) / sqrt(n)
  z <- qnorm(1 - (1 - level) / 2)
  c(brier = est, lower = max(0, est - z * se), upper = est + z * se)
}

calibration_stats_ci <- function(y, p, level = 0.95) {
  ok <- complete.cases(y, p)
  y <- as.numeric(y[ok]); p <- clip_prob(as.numeric(p[ok]))
  lp <- qlogis(p)
  z <- qnorm(1 - (1 - level) / 2)

  fit_i <- glm(y ~ 1, family = binomial(link = "logit"), offset = lp)
  est_i <- unname(coef(fit_i)[1])
  se_i <- unname(summary(fit_i)$coefficients[1, 2])

  fit_s <- glm(y ~ lp, family = binomial(link = "logit"))
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
  y <- as.numeric(y[ok]); p <- as.numeric(p[ok])
  observed <- sum(y)
  expected <- sum(p)
  if (expected <= 0) {
    return(c(observed = observed, expected = expected, OE = NA, lower = NA, upper = NA))
  }
  oe <- observed / expected
  alpha <- 1 - level
  if (observed == 0) {
    lo_count <- 0
    hi_count <- -log(alpha)  # exact one-sided equivalent for zero count
  } else {
    lo_count <- 0.5 * qchisq(alpha / 2, 2 * observed)
    hi_count <- 0.5 * qchisq(1 - alpha / 2, 2 * (observed + 1))
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
  y <- as.numeric(y[ok]); p <- as.numeric(p[ok])
  n <- length(y); events <- sum(y == 1); nonevents <- sum(y == 0)
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
    failed_tolac = nonevents,
    observed_rate = mean(y),
    observed_rate_lower = rate_ci["lower"],
    observed_rate_upper = rate_ci["upper"],
    mean_predicted = mean(p),
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

# ============================================================================
# OVERALL PERFORMANCE - DEVELOPMENT + 2022-2023 VALIDATION
# ============================================================================

performance <- rbind(
  evaluate_model(
    dev$vbac, dev$pred_information_restricted,
    "information_restricted_comparator", "development_2016_2021"
  ),
  evaluate_model(
    ival$vbac, ival$pred_information_restricted,
    "information_restricted_comparator", "intermediate_validation_2022_2023"
  ),
  evaluate_model(
    dev$vbac, dev$pred_race_neutral,
    "race_neutral_counselling_logistic", "development_2016_2021"
  ),
  evaluate_model(
    ival$vbac, ival$pred_race_neutral,
    "race_neutral_counselling_logistic", "intermediate_validation_2022_2023"
  )
)

if (RUN_FINAL_TEST) {
  performance <- rbind(
    performance,
    evaluate_model(
      test2024$vbac, test2024$pred_information_restricted,
      "information_restricted_comparator", "final_temporal_test_2024"
    ),
    evaluate_model(
      test2024$vbac, test2024$pred_race_neutral,
      "race_neutral_counselling_logistic", "final_temporal_test_2024"
    )
  )
}

write.csv(
  performance,
  file.path(OUT_DIR, "OVERALL_MODEL_PERFORMANCE_WITH_95CI.csv"),
  row.names = FALSE
)

# ============================================================================
# DESCRIPTIVE SUBGROUP / EVENT COUNTS
# These directly answer the supervisor's request and can be produced without
# opening the 2024 model performance.
# ============================================================================

subgroups <- c(
  "race_ethnicity_group",
  "age_group",
  "bmi_group",
  "chronic_htn_group",
  "prepregnancy_diabetes_group",
  "education_group"
)

subgroup_count_table <- function(data, period_name, subgroup_variable) {
  grp <- as.character(data[[subgroup_variable]])
  grp[is.na(grp) | grp == ""] <- "Missing"
  y <- data$vbac
  levels_found <- sort(unique(grp))

  do.call(rbind, lapply(levels_found, function(g) {
    idx <- grp == g & !is.na(y)
    n <- sum(idx)
    events <- sum(y[idx] == 1L)
    failures <- sum(y[idx] == 0L)
    ci <- wilson_ci(events, n)
    data.frame(
      period = period_name,
      subgroup_variable = subgroup_variable,
      subgroup_level = g,
      n = n,
      vbac_events = events,
      failed_tolac = failures,
      vbac_rate = ifelse(n > 0, events / n, NA_real_),
      vbac_rate_lower = ci["lower"],
      vbac_rate_upper = ci["upper"],
      stringsAsFactors = FALSE
    )
  }))
}

count_results <- list(); k <- 0L
for (sg in subgroups) {
  k <- k + 1L; count_results[[k]] <- subgroup_count_table(ival, "2022_2023", sg)
  k <- k + 1L; count_results[[k]] <- subgroup_count_table(test2024, "2024", sg)
}
subgroup_counts <- do.call(rbind, count_results)
rownames(subgroup_counts) <- NULL

write.csv(
  subgroup_counts,
  file.path(OUT_DIR, "SUBGROUP_EVENT_COUNTS_2022_2023_AND_2024.csv"),
  row.names = FALSE
)

# Year-specific table for the exact supervisor question.
year_count_results <- list(); k <- 0L
for (yr in 2022:2024) {
  dy <- d[d$source_year == yr, , drop = FALSE]
  for (sg in c("race_ethnicity_group", "prepregnancy_diabetes_group")) {
    k <- k + 1L
    year_count_results[[k]] <- subgroup_count_table(dy, as.character(yr), sg)
  }
}
supervisor_counts <- do.call(rbind, year_count_results)

# Keep all race/ethnicity groups and only the positive diabetes group (+ Missing
# if present, which is useful for data-quality assessment).
supervisor_counts <- supervisor_counts[
  supervisor_counts$subgroup_variable == "race_ethnicity_group" |
    (supervisor_counts$subgroup_variable == "prepregnancy_diabetes_group" &
       supervisor_counts$subgroup_level %in% c("Prepregnancy diabetes", "Missing")),
  , drop = FALSE
]

write.csv(
  supervisor_counts,
  file.path(OUT_DIR, "SUPERVISOR_REQUESTED_RACE_DIABETES_COUNTS_BY_YEAR.csv"),
  row.names = FALSE
)

# ============================================================================
# SUBGROUP CALIBRATION WITH 95% CIs - 2022-2023 INTERMEDIATE VALIDATION
# ============================================================================

subgroup_calibration <- function(data, predvar, model_name, subgroup_variable, period_name) {
  group <- as.character(data[[subgroup_variable]])
  group[is.na(group) | group == ""] <- "Missing"
  levels_found <- sort(unique(group))

  out <- lapply(levels_found, function(g) {
    idx <- group == g
    y <- data$vbac[idx]
    p <- data[[predvar]][idx]
    ok <- complete.cases(y, p)
    y <- y[ok]; p <- p[ok]

    if (length(y) < 100L || length(unique(y)) < 2L) return(NULL)

    n <- length(y); events <- sum(y == 1L); failures <- sum(y == 0L)
    rate_ci <- wilson_ci(events, n)
    auc <- auc_with_ci(y, p)
    br <- brier_with_ci(y, p)
    cal <- calibration_stats_ci(y, p)
    oe <- oe_stats_ci(y, p)

    data.frame(
      period = period_name,
      model = model_name,
      subgroup_variable = subgroup_variable,
      subgroup_level = g,
      n = n,
      vbac_events = events,
      failed_tolac = failures,
      observed_rate = mean(y),
      observed_rate_lower = rate_ci["lower"],
      observed_rate_upper = rate_ci["upper"],
      mean_predicted = mean(p),
      calibration_intercept = cal["intercept"],
      calibration_intercept_lower = cal["intercept_lower"],
      calibration_intercept_upper = cal["intercept_upper"],
      calibration_slope = cal["slope"],
      calibration_slope_lower = cal["slope_lower"],
      calibration_slope_upper = cal["slope_upper"],
      observed_expected = oe["OE"],
      observed_expected_lower = oe["lower"],
      observed_expected_upper = oe["upper"],
      brier = br["brier"],
      brier_lower = br["lower"],
      brier_upper = br["upper"],
      auc = auc["auc"],
      auc_lower = auc["lower"],
      auc_upper = auc["upper"],
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) return(NULL)
  do.call(rbind, out)
}

subgroup_results <- list(); k <- 0L
for (sg in subgroups) {
  k <- k + 1L
  subgroup_results[[k]] <- subgroup_calibration(
    ival, "pred_information_restricted", "information_restricted_comparator",
    sg, "intermediate_validation_2022_2023"
  )
  k <- k + 1L
  subgroup_results[[k]] <- subgroup_calibration(
    ival, "pred_race_neutral", "race_neutral_counselling_logistic",
    sg, "intermediate_validation_2022_2023"
  )
}
subgroup_results <- do.call(rbind, Filter(Negate(is.null), subgroup_results))
rownames(subgroup_results) <- NULL
write.csv(
  subgroup_results,
  file.path(OUT_DIR, "INTERMEDIATE_VALIDATION_SUBGROUP_CALIBRATION_WITH_95CI.csv"),
  row.names = FALSE
)

# Final 2024 subgroup calibration only if explicitly unlocked.
if (RUN_FINAL_TEST) {
  final_subgroup_results <- list(); k <- 0L
  for (sg in subgroups) {
    k <- k + 1L
    final_subgroup_results[[k]] <- subgroup_calibration(
      test2024, "pred_information_restricted", "information_restricted_comparator",
      sg, "final_temporal_test_2024"
    )
    k <- k + 1L
    final_subgroup_results[[k]] <- subgroup_calibration(
      test2024, "pred_race_neutral", "race_neutral_counselling_logistic",
      sg, "final_temporal_test_2024"
    )
  }
  final_subgroup_results <- do.call(rbind, Filter(Negate(is.null), final_subgroup_results))
  rownames(final_subgroup_results) <- NULL
  write.csv(
    final_subgroup_results,
    file.path(OUT_DIR, "FINAL_TEST_2024_SUBGROUP_CALIBRATION_WITH_95CI.csv"),
    row.names = FALSE
  )
}

# ============================================================================
# CALIBRATION CURVES (20 quantile groups) + PDF FIGURES
# ============================================================================

calibration_bins <- function(y, p, model_name, period_name, n_bins = 20L) {
  ok <- complete.cases(y, p)
  y <- y[ok]; p <- p[ok]
  # Rank-based bins avoid duplicate-quantile break failures.
  grp <- ceiling(rank(p, ties.method = "average") / length(p) * n_bins)
  grp[grp < 1] <- 1; grp[grp > n_bins] <- n_bins

  lev <- sort(unique(grp))
  do.call(rbind, lapply(lev, function(g) {
    idx <- grp == g
    n <- sum(idx); events <- sum(y[idx] == 1L)
    ci <- wilson_ci(events, n)
    data.frame(
      period = period_name,
      model = model_name,
      bin = g,
      n = n,
      mean_predicted = mean(p[idx]),
      observed = mean(y[idx]),
      observed_lower = ci["lower"],
      observed_upper = ci["upper"],
      stringsAsFactors = FALSE
    )
  }))
}

plot_calibration_pdf <- function(tab, file, title_text) {
  grDevices::pdf(file, width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(
    tab$mean_predicted, tab$observed,
    xlim = c(0, 1), ylim = c(0, 1),
    xlab = "Mean predicted probability",
    ylab = "Observed VBAC probability",
    main = title_text,
    pch = 19
  )
  abline(0, 1, lty = 2)
  segments(
    tab$mean_predicted, tab$observed_lower,
    tab$mean_predicted, tab$observed_upper
  )
  lines(tab$mean_predicted, tab$observed)
}

calibration_curves <- rbind(
  calibration_bins(
    ival$vbac, ival$pred_information_restricted,
    "information_restricted_comparator", "intermediate_validation_2022_2023"
  ),
  calibration_bins(
    ival$vbac, ival$pred_race_neutral,
    "race_neutral_counselling_logistic", "intermediate_validation_2022_2023"
  )
)
write.csv(
  calibration_curves,
  file.path(OUT_DIR, "INTERMEDIATE_VALIDATION_CALIBRATION_CURVES.csv"),
  row.names = FALSE
)

for (m in unique(calibration_curves$model)) {
  tt <- calibration_curves[calibration_curves$model == m, , drop = FALSE]
  plot_calibration_pdf(
    tt,
    file.path(OUT_DIR, paste0("CALIBRATION_2022_2023_", m, ".pdf")),
    paste("Calibration:", m, "(2022-2023)")
  )
}

if (RUN_FINAL_TEST) {
  final_curves <- rbind(
    calibration_bins(
      test2024$vbac, test2024$pred_information_restricted,
      "information_restricted_comparator", "final_temporal_test_2024"
    ),
    calibration_bins(
      test2024$vbac, test2024$pred_race_neutral,
      "race_neutral_counselling_logistic", "final_temporal_test_2024"
    )
  )
  write.csv(
    final_curves,
    file.path(OUT_DIR, "FINAL_TEST_2024_CALIBRATION_CURVES.csv"),
    row.names = FALSE
  )
  for (m in unique(final_curves$model)) {
    tt <- final_curves[final_curves$model == m, , drop = FALSE]
    plot_calibration_pdf(
      tt,
      file.path(OUT_DIR, paste0("CALIBRATION_2024_", m, ".pdf")),
      paste("Calibration:", m, "(2024 final test)")
    )
  }
}

# ============================================================================
# DECISION-CURVE ANALYSIS
# Positive action is defined as classifying a woman as a candidate for TOLAC
# based on predicted probability of successful VBAC >= threshold.
# ============================================================================

decision_curve <- function(y, p, model_name, period_name, thresholds) {
  ok <- complete.cases(y, p)
  y <- as.integer(y[ok]); p <- as.numeric(p[ok])
  N <- length(y)

  do.call(rbind, lapply(thresholds, function(pt) {
    positive <- p >= pt
    TP <- sum(positive & y == 1L)
    FP <- sum(positive & y == 0L)

    nb_model <- TP / N - FP / N * pt / (1 - pt)
    TP_all <- sum(y == 1L)
    FP_all <- sum(y == 0L)
    nb_all <- TP_all / N - FP_all / N * pt / (1 - pt)

    data.frame(
      period = period_name,
      model = model_name,
      threshold = pt,
      net_benefit = nb_model,
      net_benefit_all = nb_all,
      net_benefit_none = 0,
      standardized_net_benefit = nb_model / (TP_all / N),
      stringsAsFactors = FALSE
    )
  }))
}

ival_dca <- rbind(
  decision_curve(
    ival$vbac, ival$pred_information_restricted,
    "information_restricted_comparator", "intermediate_validation_2022_2023",
    DCA_THRESHOLDS
  ),
  decision_curve(
    ival$vbac, ival$pred_race_neutral,
    "race_neutral_counselling_logistic", "intermediate_validation_2022_2023",
    DCA_THRESHOLDS
  )
)
write.csv(
  ival_dca,
  file.path(OUT_DIR, "INTERMEDIATE_VALIDATION_DECISION_CURVE.csv"),
  row.names = FALSE
)
write.csv(
  ival_dca[ival_dca$threshold %in% DCA_KEY_THRESHOLDS, , drop = FALSE],
  file.path(OUT_DIR, "INTERMEDIATE_VALIDATION_DCA_KEY_THRESHOLDS.csv"),
  row.names = FALSE
)

if (RUN_FINAL_TEST) {
  final_dca <- rbind(
    decision_curve(
      test2024$vbac, test2024$pred_information_restricted,
      "information_restricted_comparator", "final_temporal_test_2024",
      DCA_THRESHOLDS
    ),
    decision_curve(
      test2024$vbac, test2024$pred_race_neutral,
      "race_neutral_counselling_logistic", "final_temporal_test_2024",
      DCA_THRESHOLDS
    )
  )
  write.csv(
    final_dca,
    file.path(OUT_DIR, "FINAL_TEST_2024_DECISION_CURVE.csv"),
    row.names = FALSE
  )
  write.csv(
    final_dca[final_dca$threshold %in% DCA_KEY_THRESHOLDS, , drop = FALSE],
    file.path(OUT_DIR, "FINAL_TEST_2024_DCA_KEY_THRESHOLDS.csv"),
    row.names = FALSE
  )
}

# ============================================================================
# COEFFICIENTS
# ============================================================================

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

coefficients_all <- rbind(
  extract_coefs(fit_information_restricted, "information_restricted_comparator"),
  extract_coefs(fit_race_neutral, "race_neutral_counselling_logistic")
)
write.csv(
  coefficients_all,
  file.path(OUT_DIR, "MODEL_COEFFICIENTS_WITH_95CI.csv"),
  row.names = FALSE
)

# ============================================================================
# SAVE INTERMEDIATE VALIDATION PREDICTIONS
# ============================================================================

prediction_output <- data.frame(
  source_year = ival$source_year,
  vbac = ival$vbac,
  race_ethnicity_group = ival$race_ethnicity_group,
  age_group = ival$age_group,
  bmi_group = ival$bmi_group,
  chronic_htn_group = ival$chronic_htn_group,
  prepregnancy_diabetes_group = ival$prepregnancy_diabetes_group,
  education_group = ival$education_group,
  pred_information_restricted = ival$pred_information_restricted,
  pred_race_neutral = ival$pred_race_neutral
)
con <- gzfile(
  file.path(OUT_DIR, "INTERMEDIATE_VALIDATION_2022_2023_PREDICTIONS.csv.gz"),
  open = "wt"
)
write.csv(prediction_output, con, row.names = FALSE)
close(con)

if (RUN_FINAL_TEST) {
  final_prediction_output <- data.frame(
    source_year = test2024$source_year,
    vbac = test2024$vbac,
    race_ethnicity_group = test2024$race_ethnicity_group,
    age_group = test2024$age_group,
    bmi_group = test2024$bmi_group,
    chronic_htn_group = test2024$chronic_htn_group,
    prepregnancy_diabetes_group = test2024$prepregnancy_diabetes_group,
    education_group = test2024$education_group,
    pred_information_restricted = test2024$pred_information_restricted,
    pred_race_neutral = test2024$pred_race_neutral
  )
  con <- gzfile(
    file.path(OUT_DIR, "FINAL_TEST_2024_PREDICTIONS.csv.gz"),
    open = "wt"
  )
  write.csv(final_prediction_output, con, row.names = FALSE)
  close(con)
}

# ============================================================================
# RUN MANIFEST / PROTOCOL AUDIT LOG
# ============================================================================

manifest <- c(
  paste("run_time=", format(Sys.time()), sep = ""),
  paste("data_file=", DATA_FILE, sep = ""),
  "prediction_timepoint=late_third_trimester_prenatal_counselling_before_labour",
  "development_years=2016-2021",
  "intermediate_validation_years=2022-2023",
  "final_test_year=2024",
  paste("run_final_test=", RUN_FINAL_TEST, sep = ""),
  paste("information_restricted_vars=", paste(information_restricted_vars, collapse = ";"), sep = ""),
  paste("race_neutral_counselling_vars_protocol=", paste(race_neutral_counselling_vars_protocol, collapse = ";"), sep = ""),
  paste("race_neutral_counselling_vars_estimable=", paste(race_neutral_counselling_vars, collapse = ";"), sep = ""),
  paste("zero_variance_protocol_vars=", paste(zero_var_protocol, collapse = ";"), sep = ""),
  paste("dca_threshold_min=", min(DCA_THRESHOLDS), sep = ""),
  paste("dca_threshold_max=", max(DCA_THRESHOLDS), sep = ""),
  paste("dca_threshold_step=", diff(DCA_THRESHOLDS)[1], sep = ""),
  "warning=2024_performance_must_not_be_used_to_modify_or_refit_the_model"
)
writeLines(manifest, file.path(OUT_DIR, "RUN_MANIFEST.txt"))

# ============================================================================
# FINAL CONSOLE SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("OVERALL PERFORMANCE AVAILABLE IN THIS RUN\n")
cat("============================================================\n")
print(performance, row.names = FALSE)

cat("\nSupervisor-requested descriptive counts written to:\n")
cat(file.path(OUT_DIR, "SUPERVISOR_REQUESTED_RACE_DIABETES_COUNTS_BY_YEAR.csv"), "\n")

if (!RUN_FINAL_TEST) {
  cat("\nIMPORTANT: 2024 MODEL PERFORMANCE WAS NOT OPENED.\n")
  cat("Only descriptive subgroup/event counts for 2024 were produced.\n")
  cat("When the model, subgroup list and DCA thresholds are fully frozen,\n")
  cat("set RUN_FINAL_TEST <- TRUE and set FINAL_TEST_UNLOCK_TEXT to:\n")
  cat(REQUIRED_FINAL_TEST_UNLOCK_TEXT, "\n")
} else {
  cat("\nFINAL 2024 TEST WAS OPENED. DO NOT MODIFY/REFIT THE MODEL AFTER THIS RUN.\n")
}

cat("\nOutputs written to:\n", OUT_DIR, "\n", sep = "")
cat("Finished:", format(Sys.time()), "\n")
cat("============================================================\n")