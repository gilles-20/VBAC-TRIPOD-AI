#!/bin/bash

#SBATCH --job-name=NAT_LOGIT_FINAL2024
#SBATCH --output=NAT_LOGIT_FINAL2024_%j.out
#SBATCH --error=NAT_LOGIT_FINAL2024_%j.err
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=128G

set -euo pipefail

BASE_DIR="/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"

R_SCRIPT="${BASE_DIR}/VBAC_LOGISTIC_PROTOCOL_FROZEN_2026_08.R"

DATA_FILE="${BASE_DIR}/harmonised/model_ready/NATALITY_TOLAC_MODEL_READY_2016_2024.csv.gz"

RESULT_DIR="${BASE_DIR}/harmonised/model_results/logistic_protocol_frozen_2026_08"

echo "============================================================"
echo "VBAC FINAL TEMPORAL TEST - 2024"
echo "Job ID: ${SLURM_JOB_ID:-NA}"
echo "Node: $(hostname)"
echo "Started: $(date)"
echo "============================================================"
echo

if [[ ! -f "${R_SCRIPT}" ]]; then
    echo "ERROR: R script not found:"
    echo "  ${R_SCRIPT}"
    exit 1
fi

if [[ ! -f "${DATA_FILE}" ]]; then
    echo "ERROR: model-ready dataset not found:"
    echo "  ${DATA_FILE}"
    exit 1
fi

mkdir -p "${RESULT_DIR}"

cd "${BASE_DIR}"

module purge
module load R-bundle-CRAN/2025.10-foss-2025a

echo "R executable: $(which Rscript)"
Rscript --version
echo

echo "Study periods:"
echo "  Development:             2016-2021"
echo "  Intermediate validation: 2022-2023"
echo "  Final temporal test:     2024"
echo

echo "Checking final-test unlock status..."

if ! grep -Eq '^[[:space:]]*RUN_FINAL_TEST[[:space:]]*<-[[:space:]]*TRUE' "${R_SCRIPT}"; then
    echo "ERROR: RUN_FINAL_TEST is not TRUE."
    exit 1
fi

if ! grep -Fq 'FINAL_TEST_UNLOCK_TEXT <- "I_CONFIRM_MODEL_AND_PROTOCOL_ARE_FROZEN"' "${R_SCRIPT}"; then
    echo "ERROR: FINAL_TEST_UNLOCK_TEXT is not correctly set."
    exit 1
fi

echo "Final-test unlock check: PASSED"
echo

echo "============================================================"
echo "RUNNING ONE-TIME 2024 FINAL TEMPORAL TEST"
echo "============================================================"
echo

Rscript "${R_SCRIPT}"

echo
echo "============================================================"
echo "2024 FINAL TEMPORAL TEST COMPLETED"
echo "Finished: $(date)"
echo "Results directory:"
echo "  ${RESULT_DIR}"
echo "============================================================"