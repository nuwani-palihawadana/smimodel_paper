#! /bin/bash
#SBATCH --job-name=smimodelSolar
#SBATCH --time=48:00:00
#SBATCH --mem-per-cpu=16000MB
#SBATCH --partition=comp
#SBATCH --output=output.out
#SBATCH --error=smimodelSolar.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
module load  R/4.4.0-mkl
Rscript code_HPC_solar.R
