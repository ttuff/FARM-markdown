#!/bin/bash


#SBATCH --nodes=1
#SBATCH --job-name=Tuff_simulation

#SBATCH -o slurm.%N.%j.out
#SBATCH -e slurm.%N.%j.err
#SBATCH --array=1-100

echo $SLURM_JOB_ID
echo $SLURM_ARRAY_JOB_ID
echo $SLURM_ARRAY_TASK_ID


Rscript --vanilla ./FARM_four_model_compare_EACH_TIME.R ${SLURM_ARRAY_TASK_ID}



