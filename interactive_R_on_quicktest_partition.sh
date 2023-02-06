#!/bin/bash
module load R/4.0.2
module load R/CRAN

srun --pty --cpus-per-task=2 --mem=2G  --time=01:00:00 --partition=quicktest R
