#!/bin/bash
#
#SBATCH --job-name=run_pipeline
#SBATCH --output=pipeline.out
#SBATCH --error=pipeline.err
#SBATCH --time=03:00:00
#SBATCH --partition=quicktest
#SBATCH --cpus-per-task=2
#SBATCH --mem=20G
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=xavier.marquez@vuw.ac.nz

cd /nfs/scratch/marquexa
module load R/4.0.2
module load R/CRAN

Rscript run.R

git add graph_document.md _targets/meta/meta figure/
git commit -m "graph document, figures, and metadata about targets"
git push https://github.com/xmarquez/demagogues.git

