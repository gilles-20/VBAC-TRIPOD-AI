# VBAC-TRIPOD-AI

Reproducible analysis code accompanying the study:

**Trade-offs in Race-Neutral VBAC Prediction: Temporal Performance, Subgroup Calibration, and Clinical Net Benefit in U.S. Natality Data, 2016-2024**

## Overview

This repository contains the code used to:

1. extract and harmonise CDC/NCHS Natality public-use files for 2016-2024;
2. construct the prespecified TOLAC/VBAC cohort;
3. reproduce the TRIPOD participant flow;
4. perform variable-level quality control and prepare the model-ready dataset;
5. fit the protocol-frozen race-neutral counselling model;
6. perform the one-time 2024 temporal test;
7. conduct the post-hoc race/ethnicity-inclusive comparison;
8. generate calibration, subgroup, decision-curve, manuscript, and supplementary outputs.

## Data

The analysis uses CDC/NCHS National Vital Statistics System Natality public-use files for 2016-2024.

The raw Natality microdata and derived person-level datasets are **not redistributed in this repository**. Users should obtain the annual public-use Natality files directly from the original CDC/NCHS source.

The analysis expects annual extracted text files arranged as:

```text
Nat2016us/
Nat2017us/
...
Nat2024us/
```

## Verified participant flow

The upstream cohort-construction pipeline reproduced:

| Stage | N |
|---|---:|
| All births, 2016-2024 | 33,589,282 |
| Previous cesarean | 5,183,929 |
| Singleton, cephalic, and term | 4,239,837 |
| Exactly one previous cesarean | 2,940,630 |
| Identifiable TOLAC outcome | 772,741 |

The final reconstructed cohort exactly matched the analytical cohort (`N = 772,741`).

## Temporal design

- Development: 2016-2021
- Intermediate temporal evaluation: 2022-2023
- Final temporal test: 2024
- Race/ethnicity-inclusive model: post-hoc comparative/sensitivity analysis performed after review of the frozen race-neutral 2024 results.

## Repository structure

```text
code/
  01_cohort/       cohort construction and participant flow
  02_qc_prep/      quality control and model-ready preparation
  03_models/       frozen primary and post-hoc comparative models
  04_reporting/    manuscript figures and supplementary tables
  hpc/             VSC/Slurm execution wrappers
  reviewer_sensitivity/ Reviewer 1 sensitivity analyses

docs/              harmonisation documentation
outputs/           small, non-identifiable analysis outputs
manuscript_outputs/ publication tables/figures where available
```

## Recommended execution order

1. `code/01_cohort/01_extract_natality_vbac_TRIPOD_flow.R`
2. `code/01_cohort/02_aggregate_natality_tripod_flow_2016_2024.R`
3. `code/01_cohort/03_combine_natality_tolac_2016_2024.R`
4. `code/02_qc_prep/01_qc_natality_variables.R`
5. `code/02_qc_prep/02_prepare_natality_model_data.R`
6. `code/03_models/01_vbac_logistic_protocol_frozen.R`
7. `code/03_models/02_vbac_race_inclusive_posthoc.R`
8. `code/04_reporting/01_make_manuscript_tables_figures.py`
9. `code/04_reporting/02_build_supplementary_tables_S4_S8.py`
10. `code/04_reporting/03_finalize_supplementary_table_S4.py`

## Reproducibility note

The scripts deposited here are the analysis scripts used for the study. VSC-specific paths may need to be replaced by a local project path when reproducing the workflow outside the original computing environment.

No raw or individual-level Natality records are stored in this repository.

## License

Code is released under the MIT License.

## Citation

A versioned Zenodo DOI will be added after the GitHub `v1.0.0` release is archived.



## Canonical manuscript reporting pipeline



The canonical reporting script for the current manuscript is:



`code/04_reporting/MAKE_VBAC_MANUSCRIPT_TABLES_FIGURES_PUBLICATION_OOF_FIXED_LEGEND.py`



Development performance reported in Table 2 and the development AUC reported in Figure 4 are based on five-fold out-of-fold predictions from:



`outputs/model_results/TABLE_INTERNAL_VALIDATION_5FOLD.csv`



The combined race-neutral versus race/ethnicity-inclusive performance results used for manuscript reporting are available in:



`outputs/model_results/TABLE_RACE_NEUTRAL_VS_RACE_INCLUSIVE_OVERALL.csv`



Earlier reporting scripts and apparent-fit development outputs are retained for provenance only and should not be used to reproduce the manuscript's development performance estimates. In particular, the manuscript development estimates are based on five-fold out-of-fold predictions rather than predictions obtained by scoring the fitted development model on its own training data.



The final 2022–2023 intermediate temporal evaluation and 2024 temporal test remain out-of-sample evaluations and are unchanged by this reporting correction.


## Project configuration

Set the parent project directory before running analyses:

    export VBAC_ROOT="/path/to/TRIPOD-AI"
    export TRIPOD_ROOT="$VBAC_ROOT"

The parent directory should contain `VBAC-TRIPOD-AI/`, `us_birth_files/`, and `REVIEWER_REVISION_2026/`.

The VSC/Slurm wrappers in `code/hpc/` may require adaptation on other systems.

## Prediction target and comparator

The outcome is successful vaginal birth among individuals undergoing a trial of labour after cesarean (TOLAC). The models do not predict whether an individual will attempt TOLAC.

The MFMU-compatible comparator uses a reduced predictor set available in Natality data and is not the complete clinical MFMU calculator.

## Reproducibility limitations

The 2024 evaluation is the final temporal test. Available provenance records do not independently establish prospective model locking before access to the 2024 data.

The canonical manuscript reporting script is `code/04_reporting/MAKE_VBAC_MANUSCRIPT_TABLES_FIGURES_PUBLICATION_OOF_FIXED_LEGEND.py`.

Development performance uses five-fold out-of-fold predictions. Reviewer sensitivity analyses and their verification records are provided in `code/reviewer_sensitivity/` and `docs/reproducibility/`.

Saved-output comparisons do not establish independent, clean-environment, end-to-end reproduction.

`run_pipeline.sh` documents execution order but does not automatically execute the full workflow.

## Software environment

Verified on VSC with R 4.5.1, pROC 1.19.0.1, data.table 1.18.4, and Python 3.13.1.

Python package versions: NumPy 2.3.1, pandas 2.3.0, SciPy 1.16.0, matplotlib 3.10.3, openpyxl 3.1.5.

See `requirements.txt` and `docs/reproducibility/R_package_versions.txt` for dependency records.

VSC modules: `R-bundle-CRAN/2025.10-foss-2025a` and `matplotlib/3.10.3-gfbf-2025a`.
