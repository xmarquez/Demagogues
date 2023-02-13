#!/bin/bash
#
#SBATCH --job-name=run_pipeline
#SBATCH --output=pipeline.out
#SBATCH --error=pipeline.err
#SBATCH --time=06:00:00
#SBATCH --partition=parallel
#SBATCH --cpus-per-task=2
#SBATCH --mem=40G
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=xavier.marquez@vuw.ac.nz

cd /nfs/scratch/marquexa
module load R/4.0.2
module load R/CRAN

Rscript run.R

# Capture its exit value

exit_value=$?

# Check the exit value and abort if it's not 0
if [ $exit_value -ne 0 ]; then
  echo "The last command failed with exit value $exit_value"
  exit $exit_value
fi

git add graph_document.md _targets/meta/meta figure/
git add _targets/objects/all_model_weights
git add _targets/objects/combined_weights
git add _targets/objects/combined_performance
git commit -m "graph document, figures, and metadata about targets"
git push https://github.com/xmarquez/Demagogues.git master

