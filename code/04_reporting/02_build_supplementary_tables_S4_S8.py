#!/usr/bin/env python3
"""
BUILD VBAC SUPPLEMENTARY TABLES S4-S8
=====================================

Creates:
  Supplementary Table S4:
    Full 2016-2024 CDC/NCHS harmonisation crosswalk, preserving the
    MHISPX/MRACEHISP changes, California 2019 reporting note, and MBRACE changes.

  Supplementary Table S5:
    2021 MFMU-to-Natality predictor crosswalk, classified as:
      Exact / Partial / Non-recoverable

  Supplementary Table S6:
    Crosswalk against the 47 predictors reported by Anand (2025).

    IMPORTANT:
    The 2025 preprint states that 47 prenatal-period predictors were used,
    but the manuscript text does NOT enumerate all 47 predictor names.
    Therefore this script WILL NOT invent them.

    Preferred input:
      ANAND_2025_FEATURE_LIST_47.csv

    Required columns:
      feature_number
      preprint_feature

    Optional columns:
      preprint_domain
      preprint_notes

    If this file is absent, the script creates a template and an evidence-based
    partial crosswalk using only features explicitly identifiable in the preprint.

  Supplementary Table S7:
    Full coefficients and 95% confidence intervals for:
      - race-neutral counselling model
      - race/ethnicity-inclusive counselling model

  Supplementary Table S8:
    Complete race/ethnicity subgroup decision-curve analysis at ALL thresholds.

Outputs:
  harmonised/manuscript_outputs/supplementary_tables_S4_S8/
      Supplementary_Table_S4_CDC_Harmonisation_2016_2024.csv
      Supplementary_Table_S5_MFMU_Natality_Crosswalk.csv
      Supplementary_Table_S6_Anand2025_47Feature_Crosswalk.csv
      Supplementary_Table_S7_Full_Model_Coefficients.csv
      Supplementary_Table_S8_Race_Ethnicity_DCA_All_Thresholds.csv
      VBAC_Supplementary_Tables_S4_S8.xlsx
      README_SUPPLEMENTARY_TABLES.txt

This script DOES NOT fit or refit any prediction model.
"""

from pathlib import Path
import sys
import re
import numpy as np
import pandas as pd


# ============================================================================
# 1. PATHS
# ============================================================================

BASE_DIR = Path(
    "/data/brussel/vo/000/bvo00010/vsc11778/"
    "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
)

LATEST_RESULTS = (
    BASE_DIR
    / "harmonised/model_results/race_inclusive_comparison_2026_08"
)

FROZEN_RESULTS = (
    BASE_DIR
    / "harmonised/model_results/logistic_protocol_frozen_2026_08"
)

OUT_DIR = (
    BASE_DIR
    / "harmonised/manuscript_outputs/supplementary_tables_S4_S8"
)
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Search several plausible locations for the harmonisation workbook.
HARMONISATION_CANDIDATES = [
    BASE_DIR / "VBAC_Natality_2016_2024_Harmonisation.xlsx",
    BASE_DIR / "harmonised/VBAC_Natality_2016_2024_Harmonisation.xlsx",
    BASE_DIR / "harmonised/crosswalks/VBAC_Natality_2016_2024_Harmonisation.xlsx",
]

ANAND_FEATURE_FILE = BASE_DIR / "ANAND_2025_FEATURE_LIST_47.csv"

COEF_FILE = (
    LATEST_RESULTS
    / "MODEL_COEFFICIENTS_BOTH_MODELS_WITH_95CI.csv"
)

DCA_FILE = (
    LATEST_RESULTS
    / "TABLE_2024_RACE_ETHNICITY_SUBGROUP_DCA.csv"
)


# ============================================================================
# 2. HELPERS
# ============================================================================

def first_existing(paths):
    for path in paths:
        if path.exists():
            return path
    return None


def clean_text(x):
    if pd.isna(x):
        return ""
    return str(x).strip()


def canonical(s):
    """Aggressive canonical form for approximate crosswalk matching."""
    s = clean_text(s).lower()
    s = s.replace("pre-pregnancy", "prepregnancy")
    s = s.replace("pre pregnancy", "prepregnancy")
    s = s.replace("gestational age", "gestational_age")
    s = s.replace("birth weight", "birth_weight")
    s = s.replace("race/ethnicity", "race_ethnicity")
    s = s.replace("race and ethnicity", "race_ethnicity")
    s = s.replace("number of", "n")
    s = re.sub(r"[^a-z0-9]+", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    return s


def safe_read_excel(path, sheet_name):
    try:
        return pd.read_excel(path, sheet_name=sheet_name)
    except ImportError as e:
        raise SystemExit(
            "\nERROR: Reading .xlsx requires openpyxl in this Python environment.\n"
            "Activate your Tigramite environment and test:\n\n"
            "  python -c \"import openpyxl; print(openpyxl.__version__)\"\n\n"
            "If it is unavailable, install it in that environment or load the\n"
            "Python environment where you originally created/read the workbook.\n"
        ) from e


def write_csv(df, filename):
    path = OUT_DIR / filename
    df.to_csv(path, index=False)
    print("WROTE:", path)
    return path


# ============================================================================
# 3. LOCATE HARMONISATION WORKBOOK
# ============================================================================

harm_file = first_existing(HARMONISATION_CANDIDATES)

if harm_file is None:
    print("\nERROR: Harmonisation workbook not found.")
    print("Looked in:")
    for p in HARMONISATION_CANDIDATES:
        print("  ", p)
    print(
        "\nCopy VBAC_Natality_2016_2024_Harmonisation.xlsx into BASE_DIR "
        "or edit HARMONISATION_CANDIDATES in this script."
    )
    sys.exit(1)

print("=" * 78)
print("VBAC SUPPLEMENTARY TABLES S4-S8")
print("Harmonisation workbook:", harm_file)
print("Latest results:", LATEST_RESULTS)
print("Output:", OUT_DIR)
print("=" * 78)


# ============================================================================
# 4. SUPPLEMENTARY TABLE S4
# FULL 2016-2024 CDC HARMONISATION CROSSWALK
# ============================================================================

master = safe_read_excel(harm_file, "Master_Harmonisation")

# Drop accidental exported index columns.
master = master.loc[:, ~master.columns.astype(str).str.match(r"^Unnamed")].copy()

if "Year" not in master.columns:
    raise ValueError(
        "Master_Harmonisation sheet does not contain a 'Year' column."
    )

master["Year"] = pd.to_numeric(master["Year"], errors="coerce")

s4 = master.loc[
    master["Year"].between(2016, 2024, inclusive="both")
].copy()

# Ensure the critical harmonisation notes are visually discoverable in the supplement.
search_cols = [
    c for c in [
        "Variable_original",
        "Variable_standardized",
        "Description",
        "Harmonisation_notes",
        "Source_guide",
    ]
    if c in s4.columns
]

combined_text = (
    s4[search_cols]
    .fillna("")
    .astype(str)
    .agg(" | ".join, axis=1)
)

critical_patterns = {
    "MHISPX_change": r"\bMHISPX\b",
    "MRACEHISP_change": r"\bMRACEHISP\b",
    "California_2019_Hispanic_reporting": r"California|California.*Hispanic|Hispanic.*California",
    "MBRACE_change": r"\bMBRACE\b|bridged",
}

for col, pattern in critical_patterns.items():
    s4[col] = combined_text.str.contains(
        pattern,
        case=False,
        regex=True,
        na=False,
    )

s4["Key_harmonisation_change"] = s4[
    list(critical_patterns.keys())
].any(axis=1)

# Put key audit columns first.
preferred_front = [
    "Year",
    "Variable_original",
    "Variable_standardized",
    "Description",
    "Coding",
    "Missing/Unknown",
    "Role_in_project",
    "Available_pre_decision",
    "MFMU_component",
    "Harmonisation_notes",
    "Source_guide",
    "Key_harmonisation_change",
    "MHISPX_change",
    "MRACEHISP_change",
    "California_2019_Hispanic_reporting",
    "MBRACE_change",
]

front = [c for c in preferred_front if c in s4.columns]
rest = [c for c in s4.columns if c not in front]
s4 = s4[front + rest]

write_csv(
    s4,
    "Supplementary_Table_S4_CDC_Harmonisation_2016_2024.csv",
)

# Separate small audit extract for manuscript QC.
s4_key = s4.loc[s4["Key_harmonisation_change"]].copy()
write_csv(
    s4_key,
    "Supplementary_Table_S4_KEY_RaceEthnicity_Changes_Audit.csv",
)


# ============================================================================
# 5. SUPPLEMENTARY TABLE S5
# 2021 MFMU -> NATALITY CROSSWALK
# ============================================================================

mfmu = safe_read_excel(harm_file, "MFMU_Mapping")
mfmu = mfmu.loc[:, ~mfmu.columns.astype(str).str.match(r"^Unnamed")].copy()

required_mfmu = [
    "Published 2021 MFMU predictor",
    "Natality variable(s)",
    "Mapping status",
]

missing_mfmu = [x for x in required_mfmu if x not in mfmu.columns]
if missing_mfmu:
    raise ValueError(
        "MFMU_Mapping is missing columns: " + ", ".join(missing_mfmu)
    )

def simplified_mapping_status(status):
    s = clean_text(status).lower()

    if "exact" in s and "not identifiable" not in s:
        return "Exact"

    if (
        "partial" in s
        or "proxy" in s
        or "not identifiable exactly" in s
    ):
        return "Partial"

    if (
        "unavailable" in s
        or "no direct" in s
        or "cannot reproduce" in s
    ):
        return "Non-recoverable"

    return "Other / review"

mfmu["Supplementary_mapping_class"] = mfmu["Mapping status"].map(
    simplified_mapping_status
)

# Explicitly classify the well-established non-recoverable concepts.
pred_lower = mfmu["Published 2021 MFMU predictor"].astype(str).str.lower()

mfmu.loc[
    pred_lower.str.contains("prior cesarean indication", na=False),
    "Supplementary_mapping_class",
] = "Non-recoverable"

mfmu.loc[
    pred_lower.str.contains("previous vaginal delivery", na=False),
    "Supplementary_mapping_class",
] = "Non-recoverable"

mfmu.loc[
    pred_lower.str.contains("previous vbac", na=False),
    "Supplementary_mapping_class",
] = "Non-recoverable"

# Chronic hypertension requiring treatment is partial.
mfmu.loc[
    pred_lower.str.contains("chronic hypertension", na=False),
    "Supplementary_mapping_class",
] = "Partial"

s5_front = [
    "Published 2021 MFMU predictor",
    "Natality variable(s)",
    "Supplementary_mapping_class",
    "Mapping status",
    "Required transformation / issue",
    "Recommended project treatment",
    "Source",
]
s5_front = [c for c in s5_front if c in mfmu.columns]
s5_rest = [c for c in mfmu.columns if c not in s5_front]
s5 = mfmu[s5_front + s5_rest].copy()

write_csv(
    s5,
    "Supplementary_Table_S5_MFMU_Natality_Crosswalk.csv",
)


# ============================================================================
# 6. SUPPLEMENTARY TABLE S6
# ANAND 2025 47-FEATURE CROSSWALK
# ============================================================================

# Features/concepts explicitly named in the paper text.
# These are NOT claimed to be the complete 47-feature list.
EXPLICIT_PREPRINT_CONCEPTS = [
    ("Maternal age", "Demographic"),
    ("Maternal race/ethnicity", "Demographic"),
    ("Paternal race", "Demographic"),
    ("Maternal education", "Demographic / socioeconomic"),
    ("Marital status", "Demographic / socioeconomic"),
    ("State", "Geographic"),
    ("Urbanization level", "Geographic"),
    ("U.S. Census region", "Geographic"),
    ("Prepregnancy BMI", "Clinical"),
    ("Tobacco use", "Clinical"),
    ("Prepregnancy diabetes", "Clinical"),
    ("Gestational diabetes", "Clinical"),
    ("Chronic/prepregnancy hypertension", "Clinical"),
    ("Gestational hypertension", "Clinical"),
    ("Eclampsia", "Clinical"),
    ("Anemia", "Clinical"),
    ("Prior live births / parity", "Obstetric history"),
    ("Number of previous cesareans", "Obstetric history"),
    ("Prior preterm birth", "Obstetric history"),
    ("Interpregnancy / interval since last live birth", "Obstetric history"),
    ("Gestational age", "Current pregnancy"),
    ("Prenatal care utilization / visits", "Prenatal care"),
    ("Prepregnancy / pregnancy weight gain", "Current pregnancy"),
    ("Insurance / payment source", "Health system / socioeconomic"),
    ("Place / setting of delivery", "Health system"),
    ("Birth weight", "Delivery-associated / not counselling-time"),
    ("Infertility treatment", "Clinical / obstetric"),
]

# Candidate mapping from paper concept -> harmonised standardized field(s).
# This is deliberately explicit and conservative.
CONCEPT_TO_STANDARDIZED = {
    "Maternal age": ["maternal_age"],
    "Maternal race/ethnicity": ["race_ethnicity_group", "maternal_race_ethnicity"],
    "Paternal race": ["paternal_race", "father_race"],
    "Maternal education": ["maternal_education"],
    "Marital status": ["marital_status"],
    "State": ["state", "state_code", "residence_state"],
    "Urbanization level": ["urbanization", "urbanization_level", "metro_status"],
    "U.S. Census region": ["census_region", "region"],
    "Prepregnancy BMI": ["prepregnancy_bmi", "prepregnancy_bmi_cat"],
    "Tobacco use": ["cigarettes_pre_pregnancy", "cigarettes_trimester1"],
    "Prepregnancy diabetes": ["prepregnancy_diabetes"],
    "Gestational diabetes": ["gestational_diabetes"],
    "Chronic/prepregnancy hypertension": ["prepregnancy_hypertension"],
    "Gestational hypertension": ["gestational_hypertension"],
    "Eclampsia": ["eclampsia"],
    "Anemia": ["anemia"],
    "Prior live births / parity": [
        "prior_live_births_living",
        "prior_live_births_dead",
        "live_birth_order",
    ],
    "Number of previous cesareans": ["n_previous_cesareans"],
    "Prior preterm birth": ["previous_preterm_birth"],
    "Interpregnancy / interval since last live birth": [
        "interval_since_last_live_birth",
        "interpregnancy_interval",
    ],
    "Gestational age": ["obstetric_gestational_age", "gestational_age"],
    "Prenatal care utilization / visits": [
        "prenatal_visits",
        "prenatal_care_start_month",
    ],
    "Prepregnancy / pregnancy weight gain": ["pregnancy_weight_gain_lb"],
    "Insurance / payment source": ["payment_source", "payment_recode"],
    "Place / setting of delivery": ["place_of_delivery", "facility_type"],
    "Birth weight": ["birth_weight", "birth_weight_g"],
    "Infertility treatment": ["infertility_treatment"],
}

# Build lookup from harmonisation standardized names.
std_col = (
    "Variable_standardized"
    if "Variable_standardized" in master.columns
    else None
)

if std_col is None:
    raise ValueError(
        "Master_Harmonisation lacks Variable_standardized; cannot build S6."
    )

master_lookup = master.copy()
master_lookup["_std_canonical"] = master_lookup[std_col].map(canonical)

def harmonisation_match_for_concept(concept):
    candidates = CONCEPT_TO_STANDARDIZED.get(concept, [])
    if not candidates:
        return []

    candidate_keys = {canonical(x) for x in candidates}

    hit = master_lookup.loc[
        master_lookup["_std_canonical"].isin(candidate_keys)
    ].copy()

    return hit


if ANAND_FEATURE_FILE.exists():
    feature_source = pd.read_csv(ANAND_FEATURE_FILE)

    required = ["feature_number", "preprint_feature"]
    miss = [c for c in required if c not in feature_source.columns]

    if miss:
        raise ValueError(
            f"{ANAND_FEATURE_FILE.name} is missing: " + ", ".join(miss)
        )

    if len(feature_source) != 47:
        raise ValueError(
            f"{ANAND_FEATURE_FILE.name} must contain exactly 47 rows; "
            f"found {len(feature_source)}."
        )

    feature_source["source_status"] = (
        "Exact 47-feature list supplied separately from preprint/source materials"
    )

else:
    # Do not fabricate 20 unnamed predictors.
    # Build a 47-row template with paper-explicit concepts first.
    rows = []

    for i, (feature, domain) in enumerate(EXPLICIT_PREPRINT_CONCEPTS, start=1):
        rows.append(
            {
                "feature_number": i,
                "preprint_feature": feature,
                "preprint_domain": domain,
                "preprint_notes":
                    "Feature/concept explicitly identifiable in Anand 2025 text.",
                "source_status":
                    "Explicitly identifiable from published preprint text",
            }
        )

    for i in range(len(rows) + 1, 48):
        rows.append(
            {
                "feature_number": i,
                "preprint_feature":
                    "NOT INDIVIDUALLY ENUMERATED IN PREPRINT TEXT",
                "preprint_domain": "",
                "preprint_notes":
                    "Replace using author code, supplement, or exact source list. "
                    "Do not infer this predictor.",
                "source_status":
                    "Unresolved: paper states 47 predictors but does not name all 47",
            }
        )

    feature_source = pd.DataFrame(rows)

    template = OUT_DIR / "ANAND_2025_FEATURE_LIST_47_TEMPLATE.csv"
    feature_source.to_csv(template, index=False)
    print(
        "\nNOTICE: Exact Anand 47-feature list was not supplied."
        "\nThe preprint states that 47 predictors were used but does not enumerate"
        "\nall 47 in the manuscript text. A template was written to:"
        f"\n  {template}\n"
    )


s6_rows = []

for _, row in feature_source.iterrows():
    feature = clean_text(row["preprint_feature"])

    hits = harmonisation_match_for_concept(feature)

    if len(hits) == 0:
        s6_rows.append(
            {
                "feature_number": row.get("feature_number", ""),
                "preprint_feature": feature,
                "preprint_domain": row.get("preprint_domain", ""),
                "source_status": row.get("source_status", ""),
                "Natality_variable_original": "",
                "Natality_variable_standardized": "",
                "years_available_in_harmonisation": "",
                "available_at_frozen_counselling_timepoint": "",
                "used_in_race_neutral_model": "",
                "used_in_race_inclusive_model": "",
                "crosswalk_status":
                    (
                        "Unresolved exact preprint feature"
                        if feature == "NOT INDIVIDUALLY ENUMERATED IN PREPRINT TEXT"
                        else "No direct harmonisation match found"
                    ),
                "crosswalk_notes": row.get("preprint_notes", ""),
            }
        )
        continue

    years = sorted(
        pd.to_numeric(hits["Year"], errors="coerce")
        .dropna()
        .astype(int)
        .unique()
        .tolist()
    )

    originals = sorted(
        set(
            hits.get("Variable_original", pd.Series(dtype=str))
            .dropna()
            .astype(str)
            .tolist()
        )
    )

    standardized = sorted(
        set(
            hits["Variable_standardized"]
            .dropna()
            .astype(str)
            .tolist()
        )
    )

    predecision_vals = (
        hits.get("Available_pre_decision", pd.Series(dtype=str))
        .dropna()
        .astype(str)
        .str.strip()
        .unique()
        .tolist()
    )

    predecision = "; ".join(sorted(set(predecision_vals)))

    # Current frozen model variables.
    frozen_neutral = {
        "maternal_age",
        "maternal_height_cm",
        "prepregnancy_weight_kg",
        "prepregnancy_bmi",
        "total_prior_live_births",
        "previous_preterm_birth",
        "prepregnancy_diabetes",
        "prepregnancy_hypertension",
        "gestational_diabetes",
        "gestational_hypertension",
    }

    standardized_canon = {canonical(x) for x in standardized}
    neutral_canon = {canonical(x) for x in frozen_neutral}

    used_neutral = bool(standardized_canon & neutral_canon)

    # Race-inclusive uses same model + race/ethnicity.
    used_inclusive = used_neutral or feature == "Maternal race/ethnicity"

    notes = []
    if feature in [
        "Gestational age",
        "Birth weight",
        "Place / setting of delivery",
        "Prenatal care utilization / visits",
        "Prepregnancy / pregnancy weight gain",
    ]:
        notes.append(
            "Paper availability/timing should not be assumed equivalent to the "
            "late-third-trimester counselling timepoint used in the present study."
        )

    s6_rows.append(
        {
            "feature_number": row.get("feature_number", ""),
            "preprint_feature": feature,
            "preprint_domain": row.get("preprint_domain", ""),
            "source_status": row.get("source_status", ""),
            "Natality_variable_original": "; ".join(originals),
            "Natality_variable_standardized": "; ".join(standardized),
            "years_available_in_harmonisation":
                (
                    f"{min(years)}-{max(years)}"
                    if years
                    else ""
                ),
            "available_at_frozen_counselling_timepoint": predecision,
            "used_in_race_neutral_model": used_neutral,
            "used_in_race_inclusive_model": used_inclusive,
            "crosswalk_status":
                "Mapped to harmonisation workbook",
            "crosswalk_notes":
                " ".join(
                    [clean_text(row.get("preprint_notes", ""))] + notes
                ).strip(),
        }
    )

s6 = pd.DataFrame(s6_rows)

write_csv(
    s6,
    "Supplementary_Table_S6_Anand2025_47Feature_Crosswalk.csv",
)


# ============================================================================
# 7. SUPPLEMENTARY TABLE S7
# FULL MODEL COEFFICIENTS
# ============================================================================

if not COEF_FILE.exists():
    raise FileNotFoundError(
        f"S7 coefficient file not found:\n{COEF_FILE}"
    )

coef = pd.read_csv(COEF_FILE)

models_to_keep = [
    "race_neutral_counselling_logistic",
    "race_inclusive_counselling_logistic",
]

s7 = coef.loc[
    coef["model"].isin(models_to_keep)
].copy()

# Compact journal-friendly columns while retaining raw estimates.
s7["estimate_95CI"] = s7.apply(
    lambda r:
        f"{r['estimate']:.6f} "
        f"({r['lower_95']:.6f} to {r['upper_95']:.6f})",
    axis=1,
)

s7["OR_95CI"] = s7.apply(
    lambda r:
        f"{r['odds_ratio']:.4f} "
        f"({r['odds_ratio_lower_95']:.4f} to "
        f"{r['odds_ratio_upper_95']:.4f})",
    axis=1,
)

preferred_s7 = [
    "model",
    "term",
    "estimate",
    "std_error",
    "lower_95",
    "upper_95",
    "estimate_95CI",
    "odds_ratio",
    "odds_ratio_lower_95",
    "odds_ratio_upper_95",
    "OR_95CI",
    "z",
    "p_value",
]

s7 = s7[[c for c in preferred_s7 if c in s7.columns]].copy()

write_csv(
    s7,
    "Supplementary_Table_S7_Full_Model_Coefficients.csv",
)


# ============================================================================
# 8. SUPPLEMENTARY TABLE S8
# COMPLETE RACE/ETHNICITY SUBGROUP DCA AT ALL THRESHOLDS
# ============================================================================

if not DCA_FILE.exists():
    raise FileNotFoundError(
        f"S8 subgroup DCA file not found:\n{DCA_FILE}"
    )

dca = pd.read_csv(DCA_FILE)

required_dca = [
    "race_ethnicity_group",
    "model",
    "threshold",
    "false_positive_weight",
    "outcome_prevalence",
    "action",
    "true_positive_definition",
    "false_positive_definition",
    "TP",
    "FP",
    "FN",
    "TN",
    "net_benefit",
    "net_benefit_all",
    "net_benefit_none",
    "standardized_net_benefit",
]

missing_dca = [c for c in required_dca if c not in dca.columns]
if missing_dca:
    raise ValueError(
        "DCA file is missing columns: " + ", ".join(missing_dca)
    )

s8 = dca.copy()

# Add interpretable deltas.
s8["net_benefit_vs_treat_all"] = (
    s8["net_benefit"] - s8["net_benefit_all"]
)

s8["net_benefit_vs_treat_none"] = (
    s8["net_benefit"] - s8["net_benefit_none"]
)

s8 = s8.sort_values(
    ["race_ethnicity_group", "threshold", "model"]
).reset_index(drop=True)

write_csv(
    s8,
    "Supplementary_Table_S8_Race_Ethnicity_DCA_All_Thresholds.csv",
)


# ============================================================================
# 9. EXCEL WORKBOOK WITH S4-S8
# ============================================================================

excel_out = OUT_DIR / "VBAC_Supplementary_Tables_S4_S8.xlsx"

try:
    with pd.ExcelWriter(excel_out, engine="openpyxl") as writer:
        s4.to_excel(writer, sheet_name="S4_CDC_Harmonisation", index=False)
        s5.to_excel(writer, sheet_name="S5_MFMU_Crosswalk", index=False)
        s6.to_excel(writer, sheet_name="S6_Anand2025_Crosswalk", index=False)
        s7.to_excel(writer, sheet_name="S7_Coefficients", index=False)
        s8.to_excel(writer, sheet_name="S8_Subgroup_DCA", index=False)

        # Add the key S4 audit as a convenience sheet.
        s4_key.to_excel(
            writer,
            sheet_name="S4_Key_Changes_Audit",
            index=False,
        )

    print("WROTE:", excel_out)

except ImportError:
    print(
        "\nWARNING: openpyxl is not available, so the combined XLSX file "
        "was not created. All S4-S8 CSV files were created successfully."
    )


# ============================================================================
# 10. README / SOURCE-INTEGRITY NOTES
# ============================================================================

readme = f"""
VBAC SUPPLEMENTARY TABLES S4-S8
===============================

S4
--
Source:
  {harm_file}
  Sheet: Master_Harmonisation

Contains the full 2016-2024 crosswalk and explicit audit flags for:
  - MHISPX
  - MRACEHISP
  - California 2019 Hispanic-origin reporting
  - MBRACE / bridged race changes

S5
--
Source:
  {harm_file}
  Sheet: MFMU_Mapping

The simplified classification is:
  Exact
  Partial
  Non-recoverable

The information-restricted comparator must NOT be labelled the published
2021 MFMU calculator.

S6
--
Source positioning:
  Anand A. Predicting VBAC Outcomes from U.S. Natality Data using Deep and
  Classical Machine Learning Models. arXiv:2507.21330 (2025).

IMPORTANT SOURCE-INTEGRITY NOTE:
The preprint states that complete-case filtering was performed across 47
prenatal-period predictors, but the manuscript text does not enumerate all
47 predictor names.

This script therefore does NOT fabricate the missing feature names.

Exact-list input expected at:
  {ANAND_FEATURE_FILE}

If that file is absent, the generated S6 contains:
  - concepts explicitly identifiable in the preprint text, and
  - unresolved placeholder rows for features that are not individually named.

For the final submitted supplement, replace unresolved rows only after obtaining
the exact list from author code, supplementary material, or another authoritative
source.

S7
--
Source:
  {COEF_FILE}

Includes coefficients and 95% confidence intervals for both:
  - race-neutral counselling logistic model
  - race/ethnicity-inclusive counselling logistic model

S8
--
Source:
  {DCA_FILE}

Includes every evaluated threshold for every race/ethnicity subgroup and both
models, with:
  threshold
  false-positive weight
  TP/FP/FN/TN
  net benefit
  treat-all net benefit
  treat-none net benefit
  standardized net benefit
  incremental net benefit versus default strategies

No model is fitted or modified by this script.
""".strip()

(OUT_DIR / "README_SUPPLEMENTARY_TABLES.txt").write_text(readme)

print("\n" + "=" * 78)
print("SUPPLEMENTARY TABLE GENERATION COMPLETED")
print("Output directory:", OUT_DIR)
print("=" * 78)
