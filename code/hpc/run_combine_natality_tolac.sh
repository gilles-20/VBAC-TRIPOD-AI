#!/bin/bash

#SBATCH --job-name=NAT_COMBINE

#SBATCH --output=NAT_COMBINE_%j.out

#SBATCH --error=NAT_COMBINE_%j.err

#SBATCH --time=02:00:00

#SBATCH --cpus-per-task=1

#SBATCH --mem=16G



set -euo pipefail



BASE_DIR="/data/brussel/vo/000/bvo00010/vsc11778/AI_MORTALITY_Tom/TRIPOD-AI/us_birth_files"



cd "${BASE_DIR}"



module purge

module load R-bundle-CRAN/2025.10-foss-2025a



echo "============================================================"

echo "Combining Natality TOLAC datasets"

echo "Job ID: ${SLURM_JOB_ID}"

echo "Node: $(hostname)"

echo "Started: $(date)"

echo "============================================================"



Rscript COMBINE_NATALITY_TOLAC.R



echo "============================================================"

echo "Combination completed successfully"

echo "Finished: $(date)"

echo "============================================================"
