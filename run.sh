#!/bin/bash
#
#SBATCH --job-name=run_pipeline
#SBATCH --output=pipeline.out
#SBATCH --error=pipeline.err
#SBATCH --time=2-00:00:00
#SBATCH --partition=bigmem
#SBATCH --cpus-per-task=2
#SBATCH --mem=128G
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=xavier.marquez@vuw.ac.nz

# cd /beegfs-volatile/marquexa
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
git commit -m "graph document, figures, and metadata about targets"
git push https://github.com/xmarquez/Demagogues.git master

module load dropbox
dbxcli put _targets/objects/democracy_worksets_meta Research/Demagogues/_targets/objects/democracy_worksets_meta
dbxcli put _targets/objects/all_model_weights Research/Demagogues/_targets/objects/all_model_weights
dbxcli put _targets/objects/combined_weights Research/Demagogues/_targets/objects/combined_weights
dbxcli put _targets/objects/combined_performance Research/Demagogues/_targets/objects/combined_performance
dbxcli put _targets/objects/num_htids_per_author Research/Demagogues/_targets/objects/num_htids_per_author
dbxcli put _targets/objects/num_libraries Research/Demagogues/_targets/objects/num_libraries
dbxcli put _targets/objects/num_ht_bib_keys Research/Demagogues/_targets/objects/num_ht_bib_keys
dbxcli put _targets/objects/num_author_title Research/Demagogues/_targets/objects/num_author_title
dbxcli put _targets/objects/combined_dem_dfm Research/Demagogues/_targets/objects/combined_dem_dfm
dbxcli put Paper/Appendix.md Research/Demagogues/Paper/Appendix.md
ls  _targets/objects/democracy_samples* | xargs -I {} dbxcli put {} Research/Demagogues/{}
