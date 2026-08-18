#!/bin/bash

#SBATCH --job-name=NATALITY_HARM

#SBATCH --output=NATALITY_HARM_%j.out

#SBATCH --error=NATALITY_HARM_%j.err

#SBATCH --time=12:00:00

#SBATCH --cpus-per-task=1

#SBATCH --mem=12G



set -euo pipefail



# =====================================================================

# CDC NATALITY 2016-2024

# HARMONISATION / TOLAC EXTRACTION PIPELINE

# =====================================================================



BASE_DIR="/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"



R_SCRIPT="${BASE_DIR}/extract_natality_vbac.R"



cd "${BASE_DIR}"



echo "============================================================"

echo "CDC Natality 2016-2024 harmonisation"

echo "Job ID : ${SLURM_JOB_ID}"

echo "Node   : $(hostname)"

echo "Start  : $(date)"

echo "Path   : ${BASE_DIR}"

echo "============================================================"



# ---------------------------------------------------------------------

# Load R

# ---------------------------------------------------------------------



module purge



module load R-bundle-CRAN/2025.10-foss-2025a



echo

echo "R version:"

Rscript --version



# ---------------------------------------------------------------------

# Check script

# ---------------------------------------------------------------------



if [[ ! -f "${R_SCRIPT}" ]]; then

    echo "ERROR: R script not found:"

    echo "${R_SCRIPT}"

    exit 1

fi



# ---------------------------------------------------------------------

# Input sanity check

# ---------------------------------------------------------------------



echo

echo "Checking Natality files..."



for YEAR in {2016..2024}; do



    YEAR_DIR="${BASE_DIR}/Nat${YEAR}us"



    if [[ ! -d "${YEAR_DIR}" ]]; then

        echo "ERROR: Missing directory ${YEAR_DIR}"

        exit 1

    fi



    TXT_COUNT=$(find "${YEAR_DIR}" -maxdepth 1 -name "*.txt" | wc -l)



    if [[ "${TXT_COUNT}" -eq 0 ]]; then

        echo "ERROR: No TXT file found for ${YEAR}"

        exit 1

    fi



    echo "${YEAR}: OK"



done



# ---------------------------------------------------------------------

# Create output directory

# ---------------------------------------------------------------------



mkdir -p "${BASE_DIR}/harmonised"



# ---------------------------------------------------------------------

# Run years sequentially

#

# Sequential processing is deliberate:

# - files are ~5 GB each

# - extraction is I/O intensive

# - avoids hammering shared storage

# ---------------------------------------------------------------------



for YEAR in {2016..2024}; do



    echo

    echo "============================================================"

    echo "PROCESSING ${YEAR}"

    echo "Started: $(date)"

    echo "============================================================"



    Rscript "${R_SCRIPT}" "${YEAR}"



    EXIT_CODE=$?



    if [[ ${EXIT_CODE} -ne 0 ]]; then

        echo "ERROR: ${YEAR} failed with exit code ${EXIT_CODE}"

        exit ${EXIT_CODE}

    fi



    echo

    echo "${YEAR} completed successfully."

    echo "Finished: $(date)"



done



# ---------------------------------------------------------------------

# Show resulting files

# ---------------------------------------------------------------------



echo

echo "============================================================"

echo "ALL YEARS COMPLETED"

echo "============================================================"



echo

echo "Generated files:"

ls -lh "${BASE_DIR}/harmonised"



echo

echo "QC files:"

ls -lh "${BASE_DIR}/harmonised"/natality_qc_*.csv



echo

echo "Finished: $(date)"

echo "============================================================"
