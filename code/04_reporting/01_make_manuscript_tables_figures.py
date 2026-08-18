#!/usr/bin/env python3
"""
VBAC NATALITY — PUBLICATION TABLES AND FIGURES

Purpose
-------
Generate manuscript-ready tables and figures from the already-frozen
VBAC analysis outputs. This script does NOT refit, recalibrate, tune,
or otherwise modify the prediction model.

Outputs
-------
Table 1  Cohort characteristics
Table 2  Overall predictive performance
Figure 1 Calibration plots: 2022–2023 and 2024
Figure 2 Forest plot of subgroup O/E ratios with 95% CIs
Figure 3 Decision-curve analysis: final temporal test 2024

Formats
-------
PDF  : vector figure for manuscript production
TIFF : 600 dpi, journal/submission quality
PNG  : 300 dpi preview for quick inspection

Authoring note
--------------
The visual design intentionally follows a restrained scientific-journal
style: white background, accessible colors, minimal grid lines, explicit
reference lines, and no decorative effects.
"""

from pathlib import Path
import shutil
import textwrap

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.ticker import PercentFormatter, MultipleLocator


# ======================================================================
# 1. PATHS
# ======================================================================

BASE_DIR = Path(
    "/data/brussel/vo/000/bvo00010/vsc11778/"
    "AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"
)

DATA_FILE = (
    BASE_DIR
    / "harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz"
)

RESULT_DIR = (
    BASE_DIR
    / "harmonised/model_results/logistic_protocol_frozen_2026_08"
)

MANUSCRIPT_DIR = BASE_DIR / "harmonised/manuscript_outputs"
FIG_DIR = MANUSCRIPT_DIR / "figures"
TAB_DIR = MANUSCRIPT_DIR / "tables"
SUPP_DIR = MANUSCRIPT_DIR / "supplement"

for directory in [MANUSCRIPT_DIR, FIG_DIR, TAB_DIR, SUPP_DIR]:
    directory.mkdir(parents=True, exist_ok=True)


# ======================================================================
# 2. PUBLICATION FIGURE STYLE
# ======================================================================

# Color-blind-safe Okabe–Ito derived colors.
NAVY = "#0072B2"
VERMILLION = "#D55E00"
GREEN = "#009E73"
PURPLE = "#CC79A7"
CHARCOAL = "#222222"
MID_GREY = "#666666"
LIGHT_GREY = "#D9D9D9"
VERY_LIGHT_GREY = "#F2F2F2"
WHITE = "#FFFFFF"

mpl.rcParams.update({
    "font.family": "DejaVu Sans",
    "font.size": 10.5,
    "axes.titlesize": 12.5,
    "axes.titleweight": "bold",
    "axes.labelsize": 10.5,
    "axes.labelweight": "normal",
    "xtick.labelsize": 9.5,
    "ytick.labelsize": 9.5,
    "legend.fontsize": 9.3,
    "legend.title_fontsize": 9.5,
    "figure.titlesize": 13.5,
    "figure.titleweight": "bold",
    "axes.facecolor": WHITE,
    "figure.facecolor": WHITE,
    "savefig.facecolor": WHITE,
    "axes.edgecolor": CHARCOAL,
    "axes.linewidth": 0.8,
    "xtick.color": CHARCOAL,
    "ytick.color": CHARCOAL,
    "text.color": CHARCOAL,
    "axes.labelcolor": CHARCOAL,
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
})

def clean_axis(ax, grid_axis="both"):
    """Journal-style axis formatting."""
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color("#555555")
    ax.spines["bottom"].set_color("#555555")
    ax.tick_params(length=3.5, width=0.7)
    if grid_axis:
        ax.grid(
            axis=grid_axis,
            color=LIGHT_GREY,
            linewidth=0.55,
            alpha=0.65,
            zorder=0,
        )
    ax.set_axisbelow(True)


def save_publication_figure(fig, stem):
    """Save vector, submission-quality raster, and preview formats."""
    pdf = FIG_DIR / f"{stem}.pdf"
    tiff = FIG_DIR / f"{stem}.tiff"
    png = FIG_DIR / f"{stem}_preview.png"

    fig.savefig(pdf, bbox_inches="tight", pad_inches=0.08)
    fig.savefig(
        tiff,
        dpi=600,
        bbox_inches="tight",
        pad_inches=0.08,
        format="tiff",
        pil_kwargs={"compression": "tiff_lzw"},
    )
    fig.savefig(
        png,
        dpi=300,
        bbox_inches="tight",
        pad_inches=0.08,
        format="png",
    )

    print(f"  WROTE: {pdf}")
    print(f"  WROTE: {tiff}")
    print(f"  WROTE: {png}")


# ======================================================================
# 3. HELPERS
# ======================================================================

def assert_file(path):
    if not Path(path).exists():
        raise FileNotFoundError(f"Required file does not exist:\n{path}")


def find_first_existing(candidates):
    for name in candidates:
        path = RESULT_DIR / name
        if path.exists():
            return path
    return None


def fmt_n(x):
    return f"{int(round(x)):,}"


def fmt_mean_sd(series, digits=1):
    s = pd.to_numeric(series, errors="coerce").dropna()
    if s.empty:
        return "NA"
    return f"{s.mean():.{digits}f} ({s.std(ddof=1):.{digits}f})"


def fmt_median_iqr(series, digits=1):
    s = pd.to_numeric(series, errors="coerce").dropna()
    if s.empty:
        return "NA"
    q1, med, q3 = s.quantile([0.25, 0.50, 0.75])
    return f"{med:.{digits}f} [{q1:.{digits}f}, {q3:.{digits}f}]"


def fmt_binary(series):
    s = pd.to_numeric(series, errors="coerce").dropna()
    if s.empty:
        return "NA"
    yes = int((s == 1).sum())
    return f"{yes:,} ({100 * yes / len(s):.1f}%)"


def fmt_event(series):
    s = pd.to_numeric(series, errors="coerce").dropna()
    yes = int((s == 1).sum())
    return f"{yes:,} ({100 * yes / len(s):.2f}%)"


print("=" * 76)
print("VBAC PUBLICATION TABLES AND FIGURES")
print(f"Results: {RESULT_DIR}")
print(f"Output : {MANUSCRIPT_DIR}")
print("=" * 76)


# ======================================================================
# 4. TABLE 1 — COHORT CHARACTERISTICS
# ======================================================================

assert_file(DATA_FILE)
print("\nCreating Table 1...")

data = pd.read_csv(DATA_FILE, compression="gzip", low_memory=False)

if "source_year" not in data.columns:
    raise ValueError("source_year is missing from model-ready dataset.")

data["source_year"] = pd.to_numeric(data["source_year"], errors="coerce")

def assign_period(year):
    if 2016 <= year <= 2021:
        return "Development 2016–2021"
    if 2022 <= year <= 2023:
        return "Intermediate validation 2022–2023"
    if year == 2024:
        return "Final temporal test 2024"
    return np.nan

data["manuscript_period"] = data["source_year"].apply(assign_period)
data = data.loc[data["manuscript_period"].notna()].copy()

period_levels = [
    "Development 2016–2021",
    "Intermediate validation 2022–2023",
    "Final temporal test 2024",
]

table1_vars = [
    "vbac",
    "maternal_age",
    "maternal_height_cm",
    "prepregnancy_weight_kg",
    "prepregnancy_bmi",
    "total_prior_live_births",
    "previous_preterm_birth_binary",
    "prepregnancy_diabetes_binary",
    "prepregnancy_hypertension_binary",
    "gestational_diabetes_binary",
    "gestational_hypertension_binary",
]

missing = [v for v in table1_vars if v not in data.columns]
if missing:
    raise ValueError(
        "Table 1 variables missing from model-ready dataset: "
        + ", ".join(missing)
    )

def period_summary(df):
    return {
        "N": fmt_n(len(df)),
        "Successful VBAC, n (%)": fmt_event(df["vbac"]),
        "Maternal age, years, mean (SD)": fmt_mean_sd(df["maternal_age"], 1),
        "Maternal height, cm, mean (SD)": fmt_mean_sd(df["maternal_height_cm"], 1),
        "Prepregnancy weight, kg, mean (SD)": fmt_mean_sd(df["prepregnancy_weight_kg"], 1),
        "Prepregnancy BMI, kg/m², mean (SD)": fmt_mean_sd(df["prepregnancy_bmi"], 1),
        "Prior live births, median [IQR]": fmt_median_iqr(df["total_prior_live_births"], 0),
        "Previous preterm birth, n (%)": fmt_binary(df["previous_preterm_birth_binary"]),
        "Prepregnancy diabetes, n (%)": fmt_binary(df["prepregnancy_diabetes_binary"]),
        "Prepregnancy hypertension, n (%)": fmt_binary(df["prepregnancy_hypertension_binary"]),
        "Gestational diabetes, n (%)": fmt_binary(df["gestational_diabetes_binary"]),
        "Gestational hypertension, n (%)": fmt_binary(df["gestational_hypertension_binary"]),
    }

summary = {
    period: period_summary(data.loc[data["manuscript_period"] == period])
    for period in period_levels
}

characteristics = list(summary[period_levels[0]].keys())
table1 = pd.DataFrame({"Characteristic": characteristics})
for period in period_levels:
    table1[period] = [summary[period][x] for x in characteristics]

table1_path = TAB_DIR / "TABLE1_COHORT_CHARACTERISTICS.csv"
table1.to_csv(table1_path, index=False)
print(f"  WROTE: {table1_path}")


# ======================================================================
# 5. TABLE 2 — OVERALL PREDICTIVE PERFORMANCE
# ======================================================================

print("\nCreating Table 2...")

perf_file = find_first_existing([
    "OVERALL_MODEL_PERFORMANCE_WITH_95CI.csv",
    "OVERALL_MODEL_PERFORMANCE.csv",
])

if perf_file is None:
    raise FileNotFoundError("Overall model performance CSV not found.")

perf = pd.read_csv(perf_file)

main = perf.loc[
    perf["model"] == "race_neutral_counselling_logistic"
].copy()

period_map = {
    "development_2016_2021": "Development 2016–2021",
    "intermediate_validation_2022_2023": "Intermediate validation 2022–2023",
    "final_temporal_test_2024": "Final temporal test 2024",
}

main["Period"] = main["dataset"].map(period_map).fillna(main["dataset"])
main["Observed VBAC, %"] = (100 * main["observed_rate"]).map(lambda x: f"{x:.2f}")
main["Mean predicted, %"] = (100 * main["mean_predicted"]).map(lambda x: f"{x:.2f}")

main["AUC (95% CI)"] = main.apply(
    lambda r: f"{r.auc:.3f} ({r.auc_lower:.3f}–{r.auc_upper:.3f})", axis=1
)

main["Brier score (95% CI)"] = main.apply(
    lambda r: f"{r.brier:.4f} ({r.brier_lower:.4f}–{r.brier_upper:.4f})", axis=1
)

main["Calibration intercept (95% CI)"] = main.apply(
    lambda r: (
        f"{r.calibration_intercept:.3f} "
        f"({r.calibration_intercept_lower:.3f}–"
        f"{r.calibration_intercept_upper:.3f})"
    ),
    axis=1,
)

main["Calibration slope (95% CI)"] = main.apply(
    lambda r: (
        f"{r.calibration_slope:.3f} "
        f"({r.calibration_slope_lower:.3f}–"
        f"{r.calibration_slope_upper:.3f})"
    ),
    axis=1,
)

main["O/E (95% CI)"] = main.apply(
    lambda r: (
        f"{r.observed_expected:.3f} "
        f"({r.observed_expected_lower:.3f}–"
        f"{r.observed_expected_upper:.3f})"
    ),
    axis=1,
)

table2 = main[
    [
        "Period",
        "n",
        "vbac_events",
        "failed_tolac",
        "Observed VBAC, %",
        "Mean predicted, %",
        "AUC (95% CI)",
        "Brier score (95% CI)",
        "Calibration intercept (95% CI)",
        "Calibration slope (95% CI)",
        "O/E (95% CI)",
    ]
].rename(
    columns={
        "n": "N",
        "vbac_events": "VBAC events",
        "failed_tolac": "Failed TOLAC",
    }
)

table2_path = TAB_DIR / "TABLE2_OVERALL_PREDICTIVE_PERFORMANCE.csv"
table2.to_csv(table2_path, index=False)
print(f"  WROTE: {table2_path}")


# ======================================================================
# 6. FIGURE 1 — CALIBRATION
# ======================================================================

print("\nCreating Figure 1: calibration...")

cal_int_file = find_first_existing([
    "INTERMEDIATE_VALIDATION_CALIBRATION_CURVES.csv",
    "INTERMEDIATE_VALIDATION_CALIBRATION_BINS.csv",
    "TEMPORAL_VALIDATION_CALIBRATION_DECILES.csv",
])

cal_final_file = find_first_existing([
    "FINAL_TEST_2024_CALIBRATION_CURVES.csv",
    "FINAL_TEST_2024_CALIBRATION_BINS.csv",
])

if cal_int_file is None or cal_final_file is None:
    raise FileNotFoundError("Both intermediate and final calibration files are required.")

cal = pd.concat(
    [pd.read_csv(cal_int_file), pd.read_csv(cal_final_file)],
    ignore_index=True,
)

cal = cal.loc[
    cal["model"] == "race_neutral_counselling_logistic"
].copy()

cal["Period"] = cal["period"].map({
    "intermediate_validation_2022_2023": "2022–2023",
    "final_temporal_test_2024": "2024",
}).fillna(cal["period"])

fig, axes = plt.subplots(
    1,
    2,
    figsize=(10.8, 5.25),
    sharex=True,
    sharey=True,
)

for panel, (ax, period) in enumerate(zip(axes, ["2022–2023", "2024"]), start=1):
    sub = cal.loc[cal["Period"] == period].sort_values("mean_predicted").copy()

    # Perfect calibration
    ax.plot(
        [0.40, 1.00],
        [0.40, 1.00],
        linestyle="--",
        linewidth=1.25,
        color=MID_GREY,
        label="Perfect calibration",
        zorder=1,
    )

    yerr = np.vstack([
        sub["observed"] - sub["observed_lower"],
        sub["observed_upper"] - sub["observed"],
    ])

    ax.errorbar(
        sub["mean_predicted"],
        sub["observed"],
        yerr=yerr,
        fmt="o-",
        color=NAVY if period == "2022–2023" else VERMILLION,
        ecolor=NAVY if period == "2022–2023" else VERMILLION,
        markersize=4.8,
        markeredgecolor=WHITE,
        markeredgewidth=0.65,
        linewidth=1.65,
        elinewidth=0.85,
        capsize=2.1,
        alpha=0.96,
        zorder=3,
    )

    ax.set_xlim(0.40, 1.00)
    ax.set_ylim(0.40, 1.00)
    ax.set_aspect("equal", adjustable="box")
    ax.xaxis.set_major_locator(MultipleLocator(0.10))
    ax.yaxis.set_major_locator(MultipleLocator(0.10))
    ax.xaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
    ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
    ax.set_xlabel("Mean predicted VBAC probability")
    ax.set_title(f"{chr(64 + panel)}. {period}")
    clean_axis(ax, grid_axis="both")

axes[0].set_ylabel("Observed VBAC proportion")

fig.suptitle(
    "Calibration of the race-neutral counselling model",
    y=1.00,
)

fig.text(
    0.5,
    0.005,
    "Points represent twentieths of predicted probability; vertical bars are 95% confidence intervals.",
    ha="center",
    fontsize=9.2,
    color=MID_GREY,
)

fig.tight_layout(rect=[0, 0.035, 1, 0.965])
save_publication_figure(fig, "FIGURE1_CALIBRATION_2022_2023_AND_2024")
plt.close(fig)


# ======================================================================
# 7. FIGURE 2 — SUBGROUP O/E FOREST PLOT
# ======================================================================

print("\nCreating Figure 2: subgroup O/E forest plot...")

sg_int_file = find_first_existing([
    "INTERMEDIATE_VALIDATION_SUBGROUP_CALIBRATION_WITH_95CI.csv",
    "INTERMEDIATE_VALIDATION_SUBGROUP_CALIBRATION.csv",
])

sg_final_file = find_first_existing([
    "FINAL_TEST_2024_SUBGROUP_CALIBRATION_WITH_95CI.csv",
    "FINAL_TEST_2024_SUBGROUP_CALIBRATION.csv",
])

if sg_int_file is None or sg_final_file is None:
    raise FileNotFoundError("Both subgroup calibration files are required.")

sg = pd.concat(
    [pd.read_csv(sg_int_file), pd.read_csv(sg_final_file)],
    ignore_index=True,
)

sg = sg.loc[
    sg["model"] == "race_neutral_counselling_logistic"
].copy()

sg["Period"] = sg["period"].map({
    "intermediate_validation_2022_2023": "2022–2023",
    "final_temporal_test_2024": "2024",
}).fillna(sg["period"])

domain_map = {
    "race_ethnicity_group": "Race/ethnicity",
    "age_group": "Maternal age",
    "bmi_group": "Prepregnancy BMI",
    "chronic_htn_group": "Chronic hypertension",
    "prepregnancy_diabetes_group": "Prepregnancy diabetes",
    "education_group": "Maternal education",
}

sg["Domain"] = sg["subgroup_variable"].map(domain_map).fillna(sg["subgroup_variable"])

# Reader-friendly labels.
label_replacements = {
    "NH White": "Non-Hispanic White",
    "NH Black": "Non-Hispanic Black",
    "NH Asian": "Non-Hispanic Asian",
    "NH AIAN": "Non-Hispanic AI/AN",
    "NH NHOPI": "Non-Hispanic NH/PI",
    "NH Multiracial": "Non-Hispanic multiracial",
    "Normal 18.5-24.9": "Normal (18.5–24.9)",
    "Overweight 25-29.9": "Overweight (25.0–29.9)",
    "Obesity I 30-34.9": "Obesity class I (30.0–34.9)",
    "Obesity II 35-39.9": "Obesity class II (35.0–39.9)",
    "Obesity III 40+": "Obesity class III (≥40)",
}

sg["Subgroup"] = (
    sg["subgroup_level"]
    .astype(str)
    .str.replace("_", " ", regex=False)
)

sg["Subgroup"] = sg["Subgroup"].replace(label_replacements)

main_domains = [
    "Race/ethnicity",
    "Maternal age",
    "Prepregnancy BMI",
    "Chronic hypertension",
    "Prepregnancy diabetes",
]

sg_main = sg.loc[
    sg["Domain"].isin(main_domains)
    & (sg["Subgroup"] != "Missing")
].copy()

# Explicit clinically logical ordering.
preferred_order = {
    "Race/ethnicity": [
        "Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic Asian",
        "Non-Hispanic AI/AN",
        "Non-Hispanic NH/PI",
        "Non-Hispanic multiracial",
    ],
    "Maternal age": ["<25", "25-29", "30-34", "35-39", "40+"],
    "Prepregnancy BMI": [
        "Underweight <18.5",
        "Normal (18.5–24.9)",
        "Overweight (25.0–29.9)",
        "Obesity class I (30.0–34.9)",
        "Obesity class II (35.0–39.9)",
        "Obesity class III (≥40)",
    ],
    "Chronic hypertension": ["No", "Yes"],
    "Prepregnancy diabetes": ["No", "Yes"],
}

display_rows = []
for domain in main_domains:
    present = sg_main.loc[sg_main["Domain"] == domain, "Subgroup"].drop_duplicates().tolist()
    wanted = preferred_order.get(domain, present)
    ordered = [x for x in wanted if x in present] + [x for x in present if x not in wanted]
    for subgroup in ordered:
        display_rows.append((domain, subgroup))

# Build forest positions with spacing between domains.
y_map = {}
domain_midpoints = {}
current_y = 0.0
for domain in main_domains:
    domain_rows = [r for r in display_rows if r[0] == domain]
    ys = []
    for _, subgroup in domain_rows:
        y_map[(domain, subgroup)] = current_y
        ys.append(current_y)
        current_y += 1.0
    if ys:
        domain_midpoints[domain] = np.mean(ys)
        current_y += 0.65

sg_main["y"] = [
    y_map[(d, s)] for d, s in zip(sg_main["Domain"], sg_main["Subgroup"])
]

fig_h = max(8.5, 0.34 * len(display_rows) + 2.4)
fig, ax = plt.subplots(figsize=(10.4, fig_h))

# Ideal calibration band/reference.
ax.axvspan(
    0.95,
    1.05,
    color=VERY_LIGHT_GREY,
    zorder=0,
)
ax.axvline(
    1.0,
    color=CHARCOAL,
    linestyle="--",
    linewidth=1.1,
    zorder=1,
)

period_style = {
    "2022–2023": dict(color=NAVY, marker="o", offset=-0.14),
    "2024": dict(color=VERMILLION, marker="s", offset=0.14),
}

for period, style in period_style.items():
    sub = sg_main.loc[sg_main["Period"] == period].copy()

    x = sub["observed_expected"].to_numpy()
    y = sub["y"].to_numpy() + style["offset"]

    xerr = np.vstack([
        x - sub["observed_expected_lower"].to_numpy(),
        sub["observed_expected_upper"].to_numpy() - x,
    ])

    ax.errorbar(
        x,
        y,
        xerr=xerr,
        fmt=style["marker"],
        color=style["color"],
        ecolor=style["color"],
        markersize=5.5,
        markeredgecolor=WHITE,
        markeredgewidth=0.7,
        elinewidth=1.1,
        capsize=2.4,
        linewidth=0,
        label=period,
        zorder=4,
    )

# Y labels
yticks = []
yticklabels = []
for domain, subgroup in display_rows:
    yticks.append(y_map[(domain, subgroup)])
    yticklabels.append(subgroup)

ax.set_yticks(yticks)
ax.set_yticklabels(yticklabels)
ax.invert_yaxis()

# Domain headings placed just outside left plotting area.
xmin_data = min(
    sg_main["observed_expected_lower"].min(),
    0.84,
)
xmax_data = max(
    sg_main["observed_expected_upper"].max(),
    1.12,
)
span = xmax_data - xmin_data
ax.set_xlim(xmin_data - 0.03 * span, xmax_data + 0.02 * span)

for domain, midpoint in domain_midpoints.items():
    ax.text(
        ax.get_xlim()[0],
        midpoint - 0.53,
        domain,
        fontsize=10.0,
        fontweight="bold",
        color=CHARCOAL,
        ha="left",
        va="bottom",
    )

ax.set_xlabel("Observed-to-expected ratio (95% CI)")
ax.set_title("Subgroup calibration across temporal evaluation periods", pad=10)
clean_axis(ax, grid_axis="x")

legend = ax.legend(
    title="Evaluation period",
    loc="lower right",
    frameon=False,
    handletextpad=0.6,
)

fig.text(
    0.5,
    0.008,
    "O/E < 1 indicates overprediction of VBAC; O/E > 1 indicates underprediction. Shaded band: O/E 0.95–1.05.",
    ha="center",
    fontsize=9.1,
    color=MID_GREY,
)

fig.tight_layout(rect=[0, 0.03, 1, 0.98])
save_publication_figure(fig, "FIGURE2_SUBGROUP_OE_FOREST")
plt.close(fig)


# ======================================================================
# 8. SUPPLEMENTARY FULL FOREST PLOT INCLUDING EDUCATION
# ======================================================================

sg_full = sg.loc[sg["Subgroup"] != "Missing"].copy()

all_domains = [
    "Race/ethnicity",
    "Maternal age",
    "Prepregnancy BMI",
    "Chronic hypertension",
    "Prepregnancy diabetes",
    "Maternal education",
]

display_rows_full = []
for domain in all_domains:
    present = sg_full.loc[sg_full["Domain"] == domain, "Subgroup"].drop_duplicates().tolist()
    wanted = preferred_order.get(domain, present)
    ordered = [x for x in wanted if x in present] + [x for x in present if x not in wanted]
    display_rows_full.extend((domain, subgroup) for subgroup in ordered)

y_map_full = {}
current_y = 0.0
for domain in all_domains:
    rows_d = [r for r in display_rows_full if r[0] == domain]
    for _, subgroup in rows_d:
        y_map_full[(domain, subgroup)] = current_y
        current_y += 1.0
    current_y += 0.6

sg_full["y"] = [
    y_map_full.get((d, s), np.nan)
    for d, s in zip(sg_full["Domain"], sg_full["Subgroup"])
]
sg_full = sg_full.loc[sg_full["y"].notna()].copy()

fig_h = max(11, 0.31 * len(display_rows_full) + 2.2)
fig, ax = plt.subplots(figsize=(10.5, fig_h))
ax.axvspan(0.95, 1.05, color=VERY_LIGHT_GREY, zorder=0)
ax.axvline(1, color=CHARCOAL, linestyle="--", linewidth=1.0)

for period, style in period_style.items():
    sub = sg_full.loc[sg_full["Period"] == period].copy()
    x = sub["observed_expected"].to_numpy()
    xerr = np.vstack([
        x - sub["observed_expected_lower"].to_numpy(),
        sub["observed_expected_upper"].to_numpy() - x,
    ])
    ax.errorbar(
        x,
        sub["y"].to_numpy() + style["offset"],
        xerr=xerr,
        fmt=style["marker"],
        color=style["color"],
        ecolor=style["color"],
        markersize=4.8,
        markeredgecolor=WHITE,
        markeredgewidth=0.6,
        elinewidth=0.95,
        capsize=2,
        label=period,
    )

ax.set_yticks([y_map_full[r] for r in display_rows_full])
ax.set_yticklabels([r[1] for r in display_rows_full])
ax.invert_yaxis()
ax.set_xlabel("Observed-to-expected ratio (95% CI)")
ax.set_title("Full subgroup calibration")
clean_axis(ax, grid_axis="x")
ax.legend(title="Evaluation period", frameon=False, loc="lower right")
fig.tight_layout()

supp_pdf = SUPP_DIR / "FIGURE_S_FULL_SUBGROUP_OE_FOREST.pdf"
supp_png = SUPP_DIR / "FIGURE_S_FULL_SUBGROUP_OE_FOREST_preview.png"
fig.savefig(supp_pdf, bbox_inches="tight", pad_inches=0.08)
fig.savefig(supp_png, dpi=300, bbox_inches="tight", pad_inches=0.08)
print(f"  WROTE: {supp_pdf}")
print(f"  WROTE: {supp_png}")
plt.close(fig)


# ======================================================================
# 9. FIGURE 3 — DECISION-CURVE ANALYSIS
# ======================================================================

print("\nCreating Figure 3: decision-curve analysis...")

dca_file = find_first_existing(["FINAL_TEST_2024_DECISION_CURVE.csv"])
if dca_file is None:
    raise FileNotFoundError("FINAL_TEST_2024_DECISION_CURVE.csv not found.")

dca = pd.read_csv(dca_file)

main_dca = dca.loc[
    dca["model"] == "race_neutral_counselling_logistic"
].sort_values("threshold")

comp_dca = dca.loc[
    dca["model"] == "information_restricted_comparator"
].sort_values("threshold")

fig, ax = plt.subplots(figsize=(9.2, 5.8))

# Prespecified primary interpretation range.
ax.axvspan(
    0.50,
    0.80,
    color=VERY_LIGHT_GREY,
    alpha=0.9,
    zorder=0,
)

ax.plot(
    main_dca["threshold"],
    main_dca["net_benefit"],
    color=NAVY,
    linewidth=2.25,
    label="Race-neutral counselling model",
    zorder=5,
)

ax.plot(
    comp_dca["threshold"],
    comp_dca["net_benefit"],
    color=VERMILLION,
    linewidth=1.8,
    linestyle="--",
    label="Information-restricted comparator",
    zorder=4,
)

ax.plot(
    main_dca["threshold"],
    main_dca["net_benefit_all"],
    color=MID_GREY,
    linewidth=1.5,
    linestyle="-.",
    label="Treat all",
    zorder=3,
)

ax.plot(
    main_dca["threshold"],
    main_dca["net_benefit_none"],
    color=CHARCOAL,
    linewidth=1.25,
    linestyle=":",
    label="Treat none",
    zorder=2,
)

# Prespecified reference thresholds shown without clutter.
for threshold in [0.50, 0.60, 0.70, 0.80]:
    ax.axvline(
        threshold,
        color=LIGHT_GREY,
        linewidth=0.65,
        linestyle=":",
        zorder=1,
    )

# Highlight main model values at prespecified thresholds.
for threshold in [0.50, 0.60, 0.70, 0.80]:
    row = main_dca.iloc[(main_dca["threshold"] - threshold).abs().argmin()]
    ax.scatter(
        row["threshold"],
        row["net_benefit"],
        s=36,
        color=NAVY,
        edgecolor=WHITE,
        linewidth=0.8,
        zorder=7,
    )

ax.axhline(0, color=CHARCOAL, linewidth=0.8)
ax.set_xlim(0.20, 0.80)
ax.set_xlabel("Threshold probability")
ax.set_ylabel("Net benefit")
ax.set_title("Decision-curve analysis in the 2024 final temporal test")
ax.xaxis.set_major_locator(MultipleLocator(0.10))
ax.xaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
clean_axis(ax, grid_axis="y")

ax.text(
    0.65,
    ax.get_ylim()[1] * 0.96,
    "Prespecified primary\ninterpretation range",
    ha="center",
    va="top",
    fontsize=9.0,
    color=MID_GREY,
)

ax.legend(
    loc="upper right",
    frameon=False,
    handlelength=3.0,
)

fig.text(
    0.5,
    0.008,
    "Thresholds are decision-analytic counselling scenarios and should not be interpreted as TOLAC eligibility cutoffs.",
    ha="center",
    fontsize=9.0,
    color=MID_GREY,
)

fig.tight_layout(rect=[0, 0.035, 1, 0.98])
save_publication_figure(fig, "FIGURE3_DECISION_CURVE_FINAL_2024")
plt.close(fig)


# ======================================================================
# 10. MANUSCRIPT-READY 2024 SUBGROUP TABLE
# ======================================================================

print("\nCreating subgroup calibration table...")

sg_2024 = sg.loc[
    (sg["Period"] == "2024")
    & (sg["Subgroup"] != "Missing")
].copy()

sg_2024["Observed VBAC, %"] = (100 * sg_2024["observed_rate"]).map(
    lambda x: f"{x:.1f}"
)
sg_2024["Mean predicted, %"] = (100 * sg_2024["mean_predicted"]).map(
    lambda x: f"{x:.1f}"
)

sg_2024["O/E (95% CI)"] = sg_2024.apply(
    lambda r: (
        f"{r.observed_expected:.3f} "
        f"({r.observed_expected_lower:.3f}–{r.observed_expected_upper:.3f})"
    ),
    axis=1,
)

sg_2024["Calibration intercept (95% CI)"] = sg_2024.apply(
    lambda r: (
        f"{r.calibration_intercept:.3f} "
        f"({r.calibration_intercept_lower:.3f}–"
        f"{r.calibration_intercept_upper:.3f})"
    ),
    axis=1,
)

sg_2024["Calibration slope (95% CI)"] = sg_2024.apply(
    lambda r: (
        f"{r.calibration_slope:.3f} "
        f"({r.calibration_slope_lower:.3f}–"
        f"{r.calibration_slope_upper:.3f})"
    ),
    axis=1,
)

sg_2024["AUC (95% CI)"] = sg_2024.apply(
    lambda r: f"{r.auc:.3f} ({r.auc_lower:.3f}–{r.auc_upper:.3f})",
    axis=1,
)

subgroup_table = sg_2024[
    [
        "Domain",
        "Subgroup",
        "n",
        "vbac_events",
        "failed_tolac",
        "Observed VBAC, %",
        "Mean predicted, %",
        "O/E (95% CI)",
        "Calibration intercept (95% CI)",
        "Calibration slope (95% CI)",
        "AUC (95% CI)",
    ]
].rename(
    columns={
        "n": "N",
        "vbac_events": "VBAC events",
        "failed_tolac": "Failed TOLAC",
    }
)

subgroup_table_path = TAB_DIR / "TABLE_SUBGROUP_CALIBRATION_FINAL_2024.csv"
subgroup_table.to_csv(subgroup_table_path, index=False)
print(f"  WROTE: {subgroup_table_path}")


# ======================================================================
# 11. SUPPLEMENTARY FILES
# ======================================================================

supp_files = [
    "PROTOCOL_FROZEN_PREDICTOR_SET.csv",
    "MODEL_COEFFICIENTS_WITH_95CI.csv",
    "INTERMEDIATE_VALIDATION_SUBGROUP_CALIBRATION_WITH_95CI.csv",
    "FINAL_TEST_2024_SUBGROUP_CALIBRATION_WITH_95CI.csv",
    "INTERMEDIATE_VALIDATION_DECISION_CURVE.csv",
    "FINAL_TEST_2024_DECISION_CURVE.csv",
    "SUPERVISOR_REQUESTED_RACE_DIABETES_COUNTS_BY_YEAR.csv",
]

for filename in supp_files:
    src = RESULT_DIR / filename
    if src.exists():
        shutil.copy2(src, SUPP_DIR / filename)
        print(f"  COPIED: {filename}")

(SUPP_DIR / "README_CROSSWALKS_TO_ADD.txt").write_text(
    textwrap.dedent(
        """
        SUPPLEMENTARY CROSSWALKS TO ADD

        1. CDC harmonisation crosswalk
           Use the existing Natality 2016–2024 harmonisation workbook.

        2. 2021 MFMU-to-Natality crosswalk
           Use the finalized mapping documented in the harmonisation workbook.

        3. Anand 2025 47-feature crosswalk
           This is literature-derived rather than a fitted-model output.
           Recommended columns:
             - reported feature
             - CDC source field
             - availability by year
             - available at frozen counselling timepoint
             - included in current model
             - reason for inclusion/exclusion
        """
    ).strip()
)

(MANUSCRIPT_DIR / "MANUSCRIPT_OUTPUT_MANIFEST.txt").write_text(
    textwrap.dedent(
        """
        VBAC MANUSCRIPT OUTPUTS

        TABLES
        - TABLE1_COHORT_CHARACTERISTICS.csv
        - TABLE2_OVERALL_PREDICTIVE_PERFORMANCE.csv
        - TABLE_SUBGROUP_CALIBRATION_FINAL_2024.csv

        MAIN FIGURES
        - FIGURE1_CALIBRATION_2022_2023_AND_2024.pdf/.tiff/_preview.png
        - FIGURE2_SUBGROUP_OE_FOREST.pdf/.tiff/_preview.png
        - FIGURE3_DECISION_CURVE_FINAL_2024.pdf/.tiff/_preview.png

        SUPPLEMENT
        - Full subgroup metrics
        - Model coefficients
        - Frozen predictor set
        - Decision-curve data
        - Requested subgroup counts
        - Full subgroup forest including education
        - Crosswalk README
        """
    ).strip()
)

print("\n" + "=" * 76)
print("PUBLICATION OUTPUT GENERATION COMPLETED")
print(f"Tables     : {TAB_DIR}")
print(f"Figures    : {FIG_DIR}")
print(f"Supplement : {SUPP_DIR}")
print("=" * 76)
