#!/usr/bin/env python3
"""
FINALIZE SUPPLEMENTARY TABLE S4 FOR PUBLICATION
===============================================

Takes the full S4 crosswalk created by BUILD_VBAC_SUPPLEMENTARY_TABLES_S4_S8.py
and makes the harmonisation audit publication-ready.

Outputs
-------
1. Full electronic supplement:
   Supplementary_Table_S4_CDC_Harmonisation_2016_2024_PUBLICATION_READY.csv

2. Compact manuscript/audit summary:
   Supplementary_Table_S4_Key_Harmonisation_Changes_MANUSCRIPT.csv

This script does not change analytical data or refit any model.
"""

from pathlib import Path
import csv

BASE_DIR = Path(
    "/data/brussel/vo/000/bvo00010/vsc11778/"
    "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
)

SUPP_DIR = (
    BASE_DIR
    / "harmonised/manuscript_outputs/supplementary_tables_S4_S8"
)

INPUT = SUPP_DIR / "Supplementary_Table_S4_CDC_Harmonisation_2016_2024.csv"
OUTPUT_FULL = SUPP_DIR / "Supplementary_Table_S4_CDC_Harmonisation_2016_2024_PUBLICATION_READY.csv"
OUTPUT_KEY = SUPP_DIR / "Supplementary_Table_S4_Key_Harmonisation_Changes_MANUSCRIPT.csv"

if not INPUT.exists():
    raise SystemExit(
        f"S4 input not found:\n  {INPUT}\n\n"
        "Run BUILD_VBAC_SUPPLEMENTARY_TABLES_S4_S8.py first."
    )

with INPUT.open(newline="", encoding="utf-8-sig") as f:
    reader = csv.DictReader(f)
    rows = list(reader)
    fieldnames = list(reader.fieldnames or [])

for col in ["Publication_harmonisation_note", "Primary_analysis_action"]:
    if col not in fieldnames:
        fieldnames.append(col)

CALIFORNIA_2019_NOTE = (
    "The 2019 NCHS user guide documented a marked California occurrence-birth "
    "redistribution between the detailed Central/South American and Other/unknown "
    "Hispanic categories after implementation of a new state reporting system and "
    "greater specificity of literal Hispanic-origin responses. This is a reporting "
    "discontinuity and should not be interpreted as a clinical or biological change."
)

MHISPX_2018_NOTE = (
    "Detailed Hispanic-origin coding including a separate Dominican category became "
    "available from 2018; it is not consistently available for 2016-2017."
)

MBRACE_2020_NOTE = (
    "From 2020, the bridged maternal-race field MBRACE was removed/filler in the "
    "public-use layout; harmonisation therefore relies on MRACEHISP/MRACE6 rather "
    "than bridged MBRACE."
)

MHISPX_2023_NOTE = (
    "From 2023, the detailed MHISPX definition changed so that Latin American was "
    "moved from Central/South American to Other/unknown Hispanic; detailed-origin "
    "trends spanning 2022-2023 require caution."
)

def append_note(old, new):
    old = (old or "").strip()
    if not old:
        return new
    if new in old:
        return old
    return old + " " + new

for r in rows:
    year = str(r.get("Year", "")).strip()
    var = str(r.get("Variable_original", "")).strip()
    note = str(r.get("Harmonisation_notes", "") or "")

    r.setdefault("Publication_harmonisation_note", "")
    r.setdefault("Primary_analysis_action", "")

    if year == "2018" and var == "MHISPX":
        r["Publication_harmonisation_note"] = append_note(
            r["Publication_harmonisation_note"], MHISPX_2018_NOTE
        )
        r["Primary_analysis_action"] = (
            "Use broad MRACEHISP for the primary 2016-2024 race/ethnicity grouping; "
            "reserve MHISPX for secondary detailed-origin analyses from 2018 onward."
        )
        r["Key_harmonisation_change"] = "True"
        r["MHISPX_change"] = "True"

    if year == "2019" and var in {"MRACEHISP", "MHISP_R", "MHISPX"}:
        r["Publication_harmonisation_note"] = append_note(
            r["Publication_harmonisation_note"], CALIFORNIA_2019_NOTE
        )
        r["Harmonisation_notes"] = append_note(note, CALIFORNIA_2019_NOTE)
        r["California_2019_Hispanic_reporting"] = "True"
        r["Key_harmonisation_change"] = "True"
        r["Primary_analysis_action"] = (
            "Retain broad harmonised MRACEHISP as the primary race/ethnicity grouping; "
            "inspect year-specific subgroup counts and avoid attributing a 2019 "
            "detailed-origin discontinuity to clinical change."
        )

    if year == "2020" and var in {"MRACE6", "MRACEHISP"}:
        r["Publication_harmonisation_note"] = append_note(
            r["Publication_harmonisation_note"], MBRACE_2020_NOTE
        )
        r["Primary_analysis_action"] = (
            "Use MRACEHISP/MRACE6 for pooled harmonisation; do not depend on bridged MBRACE."
        )
        r["Key_harmonisation_change"] = "True"
        if "MBRACE_change" in r:
            r["MBRACE_change"] = "True"

    if year == "2023" and var == "MHISPX":
        r["Publication_harmonisation_note"] = append_note(
            r["Publication_harmonisation_note"], MHISPX_2023_NOTE
        )
        r["Primary_analysis_action"] = (
            "Do not interpret detailed Central/South American versus Other/unknown "
            "Hispanic changes across 2022-2023 as directly comparable without qualification."
        )
        r["Key_harmonisation_change"] = "True"
        r["MHISPX_change"] = "True"

with OUTPUT_FULL.open("w", newline="", encoding="utf-8-sig") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
    writer.writeheader()
    writer.writerows(rows)

key_rows = [
    {
        "Years": "2016-2017 vs 2018+",
        "Variable_or_area": "MHISPX",
        "Change": "Detailed Hispanic-origin variable with a separate Dominican category becomes available from 2018.",
        "Impact_on_analysis": "Use MRACEHISP for consistent primary 2016-2024 race/ethnicity subgrouping; use MHISPX only for secondary detailed-origin analyses from 2018 onward.",
        "Source": "CDC/NCHS Natality User Guides 2016-2018",
    },
    {
        "Years": "2019",
        "Variable_or_area": "California Hispanic-origin reporting",
        "Change": "NCHS documented a marked redistribution of California occurrence births between Central/South American and Other/unknown Hispanic detailed-origin categories after a new state reporting system increased response specificity.",
        "Impact_on_analysis": "Treat this as a reporting discontinuity; retain broad MRACEHISP for the primary subgroup definition and avoid interpreting the detailed-origin shift as clinical change.",
        "Source": "CDC/NCHS Natality User Guide 2019, Hispanic origin and race technical notes",
    },
    {
        "Years": "2020+",
        "Variable_or_area": "MBRACE / maternal race",
        "Change": "Bridged maternal-race field MBRACE is removed/filler; MRACE6/MRACEHISP remain available.",
        "Impact_on_analysis": "Use MRACEHISP/MRACE6 for pooled harmonisation; do not use bridged MBRACE.",
        "Source": "CDC/NCHS Natality User Guides 2020-2024",
    },
    {
        "Years": "2022+",
        "Variable_or_area": "Eclampsia / infertility-type reporting",
        "Change": "NCHS notes broader/full jurisdictional availability beginning in 2022 for some fields including eclampsia and infertility-treatment type.",
        "Impact_on_analysis": "Do not rely on incompletely reported earlier-year fields as core 2016-2024 predictors without reporting-flag checks.",
        "Source": "CDC/NCHS Natality User Guides 2022-2024",
    },
    {
        "Years": "2023+",
        "Variable_or_area": "MHISPX definition",
        "Change": "Latin American is moved from Central/South American to Other/unknown Hispanic in the detailed Hispanic-origin definition.",
        "Impact_on_analysis": "Detailed Hispanic-origin trends spanning 2022-2023 require caution; broad MRACEHISP remains the primary harmonised grouping.",
        "Source": "CDC/NCHS Natality User Guides 2023-2024",
    },
]

with OUTPUT_KEY.open("w", newline="", encoding="utf-8-sig") as f:
    writer = csv.DictWriter(
        f,
        fieldnames=["Years", "Variable_or_area", "Change", "Impact_on_analysis", "Source"],
    )
    writer.writeheader()
    writer.writerows(key_rows)

print("Publication-ready full S4:")
print(" ", OUTPUT_FULL)
print("\nCompact manuscript/audit S4:")
print(" ", OUTPUT_KEY)
print("\nDone. No analytical data were modified.")
