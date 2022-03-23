#!/bin/bash
#SBATCH --job-name=postpg              # create a short name for your job
#SBATCH --nodes=1                           # node count
#SBATCH --ntasks=1                          # total number of tasks across all nodes
#SBATCH --cpus-per-task=3                   # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=10G                    # memory per cpu-core
#SBATCH --time=6:00:00                     # total run time limit (HH:MM:SS)
#SBATCH --output="test.out" 
#SBATCH --error="test.err" 
#SBATCH --mail-type=end                    # notifications for job done & fail
#SBATCH --mail-user=qingyux@princeton.edu  # send-to address
module purge
module add R/4.0.5
Rscript ./code/genx_post_processing.R
date
