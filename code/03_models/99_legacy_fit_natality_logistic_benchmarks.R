#!/usr/bin/env Rscript



# =====================================================================

# CDC NATALITY TOLAC/VBAC

# LOGISTIC BENCHMARK MODELS

#

# Development: 2016-2021

# Temporal validation: 2022-2024

#

# Models:

#   1. Natality-compatible MFMU predictor model

#   2. Full prespecified race-neutral logistic model

#

# No information from validation data is used for preprocessing.

# =====================================================================



options(stringsAsFactors = FALSE)

options(scipen = 999)



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

  "harmonised/model_results/logistic"

)



dir.create(

  OUT_DIR,

  recursive = TRUE,

  showWarnings = FALSE

)



cat("============================================================\n")

cat("VBAC Logistic Benchmark Analysis\n")

cat("Started:", format(Sys.time()), "\n")

cat("============================================================\n\n")



# =====================================================================

# LOAD DATA

# =====================================================================



d <- read.csv(

  gzfile(DATA_FILE),

  stringsAsFactors = FALSE

)



cat("Total N:", nrow(d), "\n")



dev <- d[

  d$analysis_period == "development",

  ,

  drop = FALSE

]



val <- d[

  d$analysis_period == "temporal_validation",

  ,

  drop = FALSE

]



cat("Development N:", nrow(dev), "\n")

cat("Validation N:", nrow(val), "\n")





# =====================================================================

# PREDICTOR DEFINITIONS

# =====================================================================



mfmu_vars <- c(

  "maternal_age",

  "prepregnancy_weight_kg",

  "maternal_height_cm",

  "prepregnancy_hypertension_binary"

)



full_vars <- c(

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





# =====================================================================

# IMPUTATION

#

# Learn values ONLY from development data.

# Continuous: development median

# Binary/categorical numeric: development mode

# =====================================================================



get_mode <- function(x) {



  x <- x[!is.na(x)]



  if (length(x) == 0) {

    return(NA)

  }



  tab <- table(x)



  as.numeric(

    names(tab)[which.max(tab)]

  )

}





continuous_vars <- c(

  "maternal_age",

  "maternal_height_cm",

  "prepregnancy_weight_kg",

  "total_prior_live_births",

  "cigarettes_pre_pregnancy"

)



categorical_numeric_vars <- c(

  "prepregnancy_hypertension_binary",

  "prepregnancy_diabetes_binary",

  "previous_preterm_birth_binary",

  "maternal_education",

  "marital_status",

  "wic_binary",

  "payment_recode"

)





imputation_table <- data.frame(

  variable = character(),

  method = character(),

  value = numeric(),

  stringsAsFactors = FALSE

)





for (v in continuous_vars) {



  value <- median(

    dev[[v]],

    na.rm = TRUE

  )



  imputation_table <- rbind(

    imputation_table,

    data.frame(

      variable = v,

      method = "development_median",

      value = value

    )

  )



  dev[[v]][is.na(dev[[v]])] <- value

  val[[v]][is.na(val[[v]])] <- value

}





for (v in categorical_numeric_vars) {



  value <- get_mode(

    dev[[v]]

  )



  imputation_table <- rbind(

    imputation_table,

    data.frame(

      variable = v,

      method = "development_mode",

      value = value

    )

  )



  dev[[v]][is.na(dev[[v]])] <- value

  val[[v]][is.na(val[[v]])] <- value

}





write.csv(

  imputation_table,

  file.path(

    OUT_DIR,

    "IMPUTATION_VALUES_DEVELOPMENT_ONLY.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# FIT MODELS

# =====================================================================



formula_mfmu <- as.formula(

  paste(

    "vbac ~",

    paste(

      mfmu_vars,

      collapse = " + "

    )

  )

)



formula_full <- as.formula(

  paste(

    "vbac ~",

    paste(

      full_vars,

      collapse = " + "

    )

  )

)





cat("\nFitting MFMU-compatible logistic model...\n")



fit_mfmu <- glm(

  formula_mfmu,

  family = binomial(link = "logit"),

  data = dev

)





cat("Fitting full race-neutral logistic model...\n")



fit_full <- glm(

  formula_full,

  family = binomial(link = "logit"),

  data = dev

)





# =====================================================================

# PREDICTIONS

# =====================================================================



dev$pred_mfmu <- predict(

  fit_mfmu,

  newdata = dev,

  type = "response"

)



val$pred_mfmu <- predict(

  fit_mfmu,

  newdata = val,

  type = "response"

)



dev$pred_full <- predict(

  fit_full,

  newdata = dev,

  type = "response"

)



val$pred_full <- predict(

  fit_full,

  newdata = val,

  type = "response"

)





# =====================================================================

# AUC

# Mann-Whitney formulation

# Avoids external package dependency.

# =====================================================================



auc_binary <- function(y, p) {



  ok <- complete.cases(y, p)



  y <- as.numeric(y[ok])

  p <- as.numeric(p[ok])



  n1 <- as.double(sum(y == 1))

  n0 <- as.double(sum(y == 0))



  if (n1 == 0 || n0 == 0) {

    return(NA_real_)

  }



  ranks <- rank(

    p,

    ties.method = "average"

  )



  rank_sum_positive <- sum(

    ranks[y == 1]

  )



  auc <- (

    rank_sum_positive -

      n1 * (n1 + 1) / 2

  ) / (

    n1 * n0

  )



  as.numeric(auc)

}





# =====================================================================

# BRIER SCORE

# =====================================================================



brier <- function(y, p) {



  mean(

    (y - p)^2,

    na.rm = TRUE

  )

}





# =====================================================================

# CALIBRATION INTERCEPT AND SLOPE

# =====================================================================


calibration_stats <- function(y, p) {



  ok <- complete.cases(y, p)



  y <- as.numeric(y[ok])

  p <- as.numeric(p[ok])



  eps <- 1e-8



  p <- pmin(

    pmax(p, eps),

    1 - eps

  )



  lp <- qlogis(p)



  # Calibration-in-the-large

  fit_intercept <- glm(

    y ~ 1,

    family = binomial(link = "logit"),

    offset = lp

  )



  calibration_intercept <- as.numeric(

    coef(fit_intercept)[1]

  )



  # Calibration slope

  fit_slope <- glm(

    y ~ lp,

    family = binomial(link = "logit")

  )



  calibration_slope <- as.numeric(

    coef(fit_slope)["lp"]

  )



  c(

    intercept = calibration_intercept,

    slope = calibration_slope

  )

}



 




# =====================================================================

# OBSERVED / EXPECTED

# =====================================================================



oe_stats <- function(y, p) {



  observed <- sum(

    y,

    na.rm = TRUE

  )



  expected <- sum(

    p,

    na.rm = TRUE

  )



  c(

    observed = observed,

    expected = expected,

    OE = observed / expected,

    EO = expected / observed

  )

}





# =====================================================================

# OVERALL PERFORMANCE FUNCTION

# =====================================================================



evaluate_model <- function(

  y,

  p,

  model_name,

  dataset_name

) {



  cal <- calibration_stats(

    y,

    p

  )



  oe <- oe_stats(

    y,

    p

  )



  data.frame(

    model = model_name,

    dataset = dataset_name,



    n = length(y),



    observed_rate =

      mean(y),



    mean_predicted =

      mean(p),



    auc =

      auc_binary(y, p),



    brier =

      brier(y, p),



    calibration_intercept =

      unname(cal["intercept"]),



    calibration_slope =

      unname(cal["slope"]),



    observed_expected =

      unname(oe["OE"]),



    expected_observed =

      unname(oe["EO"]),



    stringsAsFactors = FALSE

  )

}





performance <- rbind(



  evaluate_model(

    dev$vbac,

    dev$pred_mfmu,

    "MFMU_compatible_logistic",

    "development_2016_2021"

  ),



  evaluate_model(

    val$vbac,

    val$pred_mfmu,

    "MFMU_compatible_logistic",

    "temporal_validation_2022_2024"

  ),



  evaluate_model(

    dev$vbac,

    dev$pred_full,

    "Full_race_neutral_logistic",

    "development_2016_2021"

  ),



  evaluate_model(

    val$vbac,

    val$pred_full,

    "Full_race_neutral_logistic",

    "temporal_validation_2022_2024"

  )

)





write.csv(

  performance,

  file.path(

    OUT_DIR,

    "OVERALL_MODEL_PERFORMANCE.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# SUBGROUP CALIBRATION

# =====================================================================



subgroups <- c(

  "race_ethnicity_group",

  "age_group",

  "bmi_group",

  "chronic_htn_group",

  "prepregnancy_diabetes_group",

  "education_group"

)





subgroup_calibration <- function(

  data,

  predvar,

  model_name,

  subgroup_variable

) {



  group <- as.character(

    data[[subgroup_variable]]

  )



  group[is.na(group)] <- "Missing"



  levels_found <- sort(

    unique(group)

  )



  out <- list()



  for (i in seq_along(levels_found)) {



    g <- levels_found[i]



    idx <- group == g



    y <- data$vbac[idx]

    p <- data[[predvar]][idx]



    if (

      length(y) < 100 ||

      length(unique(y)) < 2

    ) {

      next

    }



    cal <- calibration_stats(

      y,

      p

    )



    oe <- oe_stats(

      y,

      p

    )



    out[[i]] <- data.frame(



      model =

        model_name,



      subgroup_variable =

        subgroup_variable,



      subgroup_level =

        g,



      n =

        length(y),



      observed_rate =

        mean(y),



      mean_predicted =

        mean(p),



      calibration_intercept =

        unname(

          cal["intercept"]

        ),



      calibration_slope =

        unname(

          cal["slope"]

        ),



      observed_expected =

        unname(

          oe["OE"]

        ),



      brier =

        brier(y, p),



      auc =

        auc_binary(y, p),



      stringsAsFactors = FALSE

    )

  }



  do.call(

    rbind,

    out

  )

}





subgroup_results <- list()



counter <- 0L



for (sg in subgroups) {



  counter <- counter + 1L



  subgroup_results[[counter]] <-

    subgroup_calibration(

      val,

      "pred_mfmu",

      "MFMU_compatible_logistic",

      sg

    )



  counter <- counter + 1L



  subgroup_results[[counter]] <-

    subgroup_calibration(

      val,

      "pred_full",

      "Full_race_neutral_logistic",

      sg

    )

}





subgroup_results <- do.call(

  rbind,

  subgroup_results

)



rownames(

  subgroup_results

) <- NULL





write.csv(

  subgroup_results,

  file.path(

    OUT_DIR,

    "TEMPORAL_VALIDATION_SUBGROUP_CALIBRATION.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# DECILE CALIBRATION TABLE

# =====================================================================



calibration_deciles <- function(

  y,

  p,

  model_name

) {



  breaks <- quantile(

    p,

    probs = seq(

      0,

      1,

      by = 0.1

    ),

    na.rm = TRUE

  )



  breaks <- unique(

    breaks

  )



  if (length(breaks) < 3) {



    groups <- cut(

      rank(p),

      breaks = 10,

      labels = FALSE

    )



  } else {



    groups <- cut(

      p,

      breaks = breaks,

      include.lowest = TRUE,

      labels = FALSE

    )

  }



  result <- aggregate(

    cbind(

      observed = y,

      predicted = p

    ) ~ groups,

    FUN = mean

  )



  counts <- as.data.frame(

    table(groups)

  )



  names(counts) <- c(

    "groups",

    "n"

  )



  counts$groups <-

    as.numeric(

      as.character(

        counts$groups

      )

    )



  result <- merge(

    result,

    counts,

    by = "groups",

    all.x = TRUE

  )



  result$model <-

    model_name



  result

}





deciles <- rbind(



  calibration_deciles(

    val$vbac,

    val$pred_mfmu,

    "MFMU_compatible_logistic"

  ),



  calibration_deciles(

    val$vbac,

    val$pred_full,

    "Full_race_neutral_logistic"

  )

)





write.csv(

  deciles,

  file.path(

    OUT_DIR,

    "TEMPORAL_VALIDATION_CALIBRATION_DECILES.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# DECISION CURVE

#

# Here "positive" means predicted successful VBAC.

#

# Net benefit:

#

# TP/N - FP/N * pt/(1-pt)

#

# Threshold range is deliberately broad at this exploratory stage.

# We will later restrict this to clinically justified thresholds.

# =====================================================================



decision_curve <- function(

  y,

  p,

  model_name,

  thresholds = seq(

    0.30,

    0.90,

    by = 0.01

  )

) {



  N <- length(y)



  result <- lapply(

    thresholds,

    function(pt) {



      predicted_positive <-

        p >= pt



      TP <- sum(

        predicted_positive &

          y == 1

      )



      FP <- sum(

        predicted_positive &

          y == 0

      )



      NB_model <-

        TP / N -

        FP / N *

        (

          pt /

            (1 - pt)

        )



      # Treat all as successful VBAC candidates

      TP_all <- sum(

        y == 1

      )



      FP_all <- sum(

        y == 0

      )



      NB_all <-

        TP_all / N -

        FP_all / N *

        (

          pt /

            (1 - pt)

        )



      data.frame(

        model = model_name,

        threshold = pt,

        net_benefit = NB_model,

        net_benefit_all = NB_all,

        net_benefit_none = 0

      )

    }

  )



  do.call(

    rbind,

    result

  )

}





dca <- rbind(



  decision_curve(

    val$vbac,

    val$pred_mfmu,

    "MFMU_compatible_logistic"

  ),



  decision_curve(

    val$vbac,

    val$pred_full,

    "Full_race_neutral_logistic"

  )

)





write.csv(

  dca,

  file.path(

    OUT_DIR,

    "TEMPORAL_VALIDATION_DECISION_CURVE.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# COEFFICIENTS

# =====================================================================



extract_coefs <- function(

  fit,

  model_name

) {



  s <- summary(fit)$coefficients



  data.frame(

    model = model_name,

    term = rownames(s),

    estimate = s[, 1],

    std_error = s[, 2],

    z = s[, 3],

    p_value = s[, 4],

    row.names = NULL

  )

}





coefficients_all <- rbind(



  extract_coefs(

    fit_mfmu,

    "MFMU_compatible_logistic"

  ),



  extract_coefs(

    fit_full,

    "Full_race_neutral_logistic"

  )

)





write.csv(

  coefficients_all,

  file.path(

    OUT_DIR,

    "MODEL_COEFFICIENTS.csv"

  ),

  row.names = FALSE

)





# =====================================================================

# SAVE VALIDATION PREDICTIONS

# =====================================================================



prediction_output <- data.frame(



  source_year =

    val$source_year,



  vbac =

    val$vbac,



  race_ethnicity_group =

    val$race_ethnicity_group,



  age_group =

    val$age_group,



  bmi_group =

    val$bmi_group,



  chronic_htn_group =

    val$chronic_htn_group,



  prepregnancy_diabetes_group =

    val$prepregnancy_diabetes_group,



  education_group =

    val$education_group,



  pred_mfmu =

    val$pred_mfmu,



  pred_full =

    val$pred_full

)





con <- gzfile(

  file.path(

    OUT_DIR,

    "TEMPORAL_VALIDATION_PREDICTIONS.csv.gz"

  ),

  open = "wt"

)



write.csv(

  prediction_output,

  con,

  row.names = FALSE

)



close(con)





# =====================================================================

# FINAL REPORT

# =====================================================================



cat("\n============================================================\n")

cat("OVERALL PERFORMANCE\n")

cat("============================================================\n")



print(

  performance,

  row.names = FALSE

)



cat("\nOutputs written to:\n")

cat(OUT_DIR, "\n")



cat("\nFinished:", format(Sys.time()), "\n")

cat("============================================================\n")
