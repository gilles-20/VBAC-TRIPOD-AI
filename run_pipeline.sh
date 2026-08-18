#!/bin/bash
set -euo pipefail

echo "This wrapper documents the intended execution order."
echo "Because the raw CDC Natality files are large, run cohort extraction by year on HPC."

echo "1. Extract annual cohorts: 2016-2024"
echo "2. Aggregate TRIPOD participant flow"
echo "3. Combine annual TOLAC cohorts"
echo "4. Run QC"
echo "5. Prepare model-ready data"
echo "6. Run frozen race-neutral model"
echo "7. Run post-hoc race-inclusive comparison"
echo "8. Generate manuscript and supplementary outputs"
