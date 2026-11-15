#!/bin/bash

#SBATCH --job-name=NAT_MODEL_PREP

#SBATCH --output=NAT_MODEL_PREP_%j.out

#SBATCH --error=NAT_MODEL_PREP_%j.err

#SBATCH --time=02:00:00

#SBATCH --cpus-per-task=1

#SBATCH --mem=16G



set -euo pipefail



BASE_DIR="${VBAC_ROOT:?Set VBAC_ROOT to the project directory}/us_birth_files"
REPO_DIR="${VBAC_ROOT}/VBAC-TRIPOD-AI"



cd "${BASE_DIR}"



echo "============================================================"

echo "CDC Natality model-ready preparation"

echo "Job ID : ${SLURM_JOB_ID}"

echo "Node   : $(hostname)"

echo "Started: $(date)"

echo "============================================================"



module purge

module load R-bundle-CRAN/2025.10-foss-2025a



Rscript "${REPO_DIR}/code/02_qc_prep/02_prepare_natality_model_data.R"



echo

echo "============================================================"

echo "MODEL PREPARATION COMPLETED"

echo "Finished: $(date)"

echo "============================================================"



echo

echo "Generated files:"

ls -lh "${BASE_DIR}/harmonised/model_ready/"
