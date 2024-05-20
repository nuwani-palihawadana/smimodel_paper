#! /bin/bash
#SBATCH --job-name=smimodelSolarFourierTuneLinear
#SBATCH --time=48:00:00
#SBATCH --mem-per-cpu=16000MB
#SBATCH --partition=comp
#SBATCH --output=array_%A_%a.out
#SBATCH --error=array_%A_%a.err
#SBATCH --array=1-10
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
module load  R/4.4.0-mkl
Rscript solar_fourier_tune_HPC.R $SLURM_ARRAY_TASK_ID 