# Reviewer analysis reproduction tests

Verification date: 2026-11-14

Environment:
- R 4.5.1
- pROC 1.19.0.1
- Existing HPC environment and original analysis inputs

## R1.2 — Decision-curve audit

- Execution: successful
- Bootstrap repetitions: 400
- Random seed: 20261114
- CSV files compared: 6
- Result: all six files byte-for-byte identical

Status: PASS

## R1.8 — Paired DeLong comparison

- Execution: successful
- Test sample: 94,071
- Race-neutral AUC: 0.7066925
- Race-inclusive AUC: 0.7128193
- AUC difference: 0.00612684
- Paired DeLong p-value: 1.640287e-22
- Result: CSV byte-for-byte identical

Status: PASS

## R1.9 — Subgroup calibration

- Execution: successful
- Bootstrap repetitions: 400
- Random seed: 20261114
- CSV files compared: 4
- Numerical comparison tolerance: 1e-10
- Maximum absolute difference: 1.021e-14
- One CSV identical; three numerically equivalent

Status: PASS — numerical equivalence

## Limitations

These tests used the existing HPC environment and
previously generated analysis inputs.

They do not establish independent reproduction from
raw CDC Natality files in a fresh software environment.

The decision-curve audit does not establish clinical
utility for choosing TOLAC versus planned repeat cesarean.

## R1.7 — Cross-validation imputation and missing race sensitivity

- Execution date: 2026-11-14
- R version: 4.5.1
- Development sample: 499,360
- Intermediate validation sample: 179,310
- Final test sample: 94,071
- Cross-validation: all five folds completed successfully
- Original outputs: `REVIEWER_REVISION_2026/R1_7_CV_IMPUTATION_MISSING_RACE/`
- Reproduced outputs: `REVIEWER_REVISION_2026/R1_12_REPRODUCIBILITY/R1_7_REPRO_TEST/`
- CSV files compared: 6
- Byte-identical CSV files: 5
- Numerically equivalent CSV files: 1
- Maximum absolute numerical difference: 1.871587995694e-09
- Comparison tolerance: 1e-8
- Result: PASS within numerical tolerance

The reproduction used a separate output directory to preserve the original
reviewer analysis. No statistical model specifications were changed.

## R1.7 — Cross-validation imputation and missing race sensitivity

- Execution date: 2026-11-14
- R version: 4.5.1
- Development sample: 499,360
- Intermediate validation sample: 179,310
- Final test sample: 94,071
- Cross-validation: all five folds completed successfully
- Original outputs: `REVIEWER_REVISION_2026/R1_7_CV_IMPUTATION_MISSING_RACE/`
- Reproduced outputs: `REVIEWER_REVISION_2026/R1_12_REPRODUCIBILITY/R1_7_REPRO_TEST/`
- CSV files compared: 6
- Byte-identical CSV files: 5
- Numerically equivalent CSV files: 1
- Maximum absolute numerical difference: 1.871587995694e-09
- Comparison tolerance: 1e-8
- Result: PASS within numerical tolerance

The reproduction used a separate output directory to preserve the original
reviewer analysis. No statistical model specifications were changed.

## R1.1 / R1.10 — Prediction-landmark and predictor-availability audit

- Execution date: 2026-11-14
- R version: 4.5.1
- Compute node: node600.hydra.os
- Original outputs: `REVIEWER_REVISION_2026/R1_1_R1_10_LANDMARK/`
- Reproduced outputs: `REVIEWER_REVISION_2026/R1_12_REPRODUCIBILITY/R1_1_REPRO_TEST/`
- Files compared: 4
- Byte-identical files: 4
- Result: PASS — exact reproduction

This audit documents predictor classification, availability assumptions,
and missingness by year. It does not establish when predictors became
clinically available during pregnancy.

## R1.3 — Nonlinearity sensitivity analysis

- Execution date: 2026-11-14
- R version: 4.5.1
- CSV files compared: 6
- Exact CSV matches: 2
- Numerically equivalent CSV files: 4
- Maximum CSV absolute difference: 4.583741053388e-09
- Numerical comparison tolerance: 1e-8
- Saved fitted models compared: 5
- Model coefficients, deviance, AIC and convergence: PASS
- PDF generated: adjusted_age_bmi_effects.pdf (8,106 bytes)
- PDF visual equivalence: not yet verified
- Result: PASS for CSV and fitted-model reproducibility

Important limitation:
The original R1.3 AUC function is susceptible to integer overflow.
Development and intermediate-validation AUC values require correction
in a separately documented analysis.

### R1.3 — Corrected AUC sensitivity analysis

- Execution date: 2026-11-14
- R version: 4.5.1
- Compute node: node711.hydra.os
- Corrected script: R1_3_nonlinearity_sensitivity_AUC_FIXED.R
- Corrected output directory: R1_12_REPRODUCIBILITY/R1_3_AUC_CORRECTED/
- Correction: Convert event and non-event counts to double precision
  before AUC rank arithmetic to prevent 32-bit integer overflow.
- All 15 AUC values: finite and within [0,1].
- All non-AUC CSV results: numerically equivalent within 1e-8.
- Original R1.3 outputs: preserved.
- Result: PASS — corrected AUC calculation validated.

The highest 2024 AUC was 0.707118 for the height/weight/age-spline
specification, versus 0.706692 for the original linear model.
The absolute improvement was approximately 0.000425.
This exploratory sensitivity analysis does not establish a clinically
meaningful improvement in prediction performance.

## R1.4 — Temporal design evidence audit

- Execution date: 2026-11-14
- R version: 4.5.1
- Implemented split audit: exact reproduction
- README interpretation: exact reproduction
- Implemented development period: 2016–2021
- Intermediate evaluation: 2022–2023
- Final temporal test: 2024
- Git evidence: model script committed 2026-08-18
- Original Git-history extraction warning identified and corrected.
- Result: PASS for stable audit outputs.

Interpretation:
The historical harmonisation workbook describes an earlier proposed
temporal split, whereas the model script implements 2016–2021
development, 2022–2023 intermediate evaluation, and 2024 testing.

The available Git history does not independently establish that the
model was prospectively frozen before the 2024 outcomes were inspected.
The manuscript must not overstate this evidence.

## R1.5 — Cohort selection audit

- Execution date: 2026-11-14
- R version: 4.5.1
- Compute node: node605.hydra.os
- Annual input files: 2016–2024 (9/9 processed)
- Original cohort flow: exact reproduction
- Final-stage categories by year: exact reproduction
- Final-stage category totals: exact reproduction
- Interpretation README: exact reproduction
- Reproduced output files: 4/4 exact matches
- Final identifiable TOLAC cohort: 772741
- Successful VBAC: 584407
- Failed TOLAC: 188334
- Result: PASS — exact output reproducibility

Interpretation:
The final selection is based on identifiable TOLAC status.
Excluded records must not all be described as elective
repeat cesarean deliveries.

## R1.6 — Anthropometric specification and collinearity sensitivity

- Execution date: 2026-11-14
- R version: 4.5.1
- Original BMI comparison: 4/4 CSV files numerically reproduced (tolerance 1e-8).
- Original collinearity sensitivity: 4/4 CSV files numerically reproduced (tolerance 1e-8).
- Original AUC defect: integer overflow in class-count multiplication.
- Correction: convert class counts to double precision before AUC arithmetic.
- Corrected BMI comparison: 15/15 valid AUC values.
- Corrected collinearity sensitivity: 12/12 valid AUC values.
- Non-AUC results: verified unchanged within tolerance 1e-8.
- RDS model equivalence: not yet independently verified.

2024 AUC:
- BMI only: 0.701286
- Height + weight: 0.706706
- Height + BMI: 0.706696
- Weight + BMI: 0.706167
- Height + weight + BMI: 0.706692

Interpretation:
BMI-only discrimination was slightly lower than specifications
retaining height and weight. Adding BMI to height and weight
did not materially improve discrimination. These are post-review
sensitivity analyses, not grounds for selecting a model based
on final-test performance.

## R1.10 — Baseline-only predictor-timing sensitivity

- Execution date: 2026-11-14
- Compute node: node715.hydra.os
- R version: 4.5.1
- CSV reproduction: 5/5 passed (2 exact, 3 numerical at tolerance 1e-8).
- Fitted RDS models: 2/2 equivalent for coefficients, deviance, AIC and convergence.
- Result: PASS.

2024 test-set performance (n=94071):
- Original refit: AUC 0.706692, Brier 0.167550, O/E 1.011295.
- Baseline only: AUC 0.705611, Brier 0.167830, O/E 1.008974.
- Mean absolute prediction difference: 0.009121.
- 95th percentile absolute prediction difference: 0.066293.

Interpretation:
Removing gestational diabetes and gestational hypertension produced
a small reduction in aggregate discrimination and a small increase
in Brier score. The sensitivity analysis is post-review and does
not validate an early-pregnancy clinical prediction tool.

## R1.6 — Final verification

Execution date: 2026-11-14
R version: 4.5.1

- Original BMI analysis: 4/4 CSV files reproduced.
- Original collinearity analysis: 4/4 CSV files reproduced.
- Fitted RDS models: 9/9 equivalent.
- AUC integer-overflow correction: verified.
- Corrected AUC values: 27/27 valid.
- Non-AUC CSV outputs: 8/8 equivalent within tolerance 1e-8.
- Overall result: PASS.

The original analyses and AUC-corrected analyses were retained
separately to preserve the audit trail.

The correction changes AUC arithmetic only and does not alter
the fitted regression models or other reported statistics.
