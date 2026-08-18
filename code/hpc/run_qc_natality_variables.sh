#!/bin/bash

#SBATCH --job-name=NAT_VBAC_QC

#SBATCH --output=NAT_VBAC_QC_%j.out

#SBATCH --error=NAT_VBAC_QC_%j.err

#SBATCH --time=02:00:00

#SBATCH --cpus-per-task=1

#SBATCH --mem=16G



set -euo pipefail



BASE_DIR="/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"



cd "${BASE_DIR}"



echo "============================================================"

echo "CDC Natality Variable QC"

echo "Job ID: ${SLURM_JOB_ID}"

echo "Node: $(hostname)"

echo "Started: $(date)"

echo "============================================================"



module purge

module load R-bundle-CRAN/2025.10-foss-2025a



Rscript QC_NATALITY_VARIABLES.R



echo "============================================================"

echo "QC finished successfully"

echo "Finished: $(date)"

echo "============================================================"
