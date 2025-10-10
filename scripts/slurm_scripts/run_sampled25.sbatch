#!/bin/bash
#Job parameters

#SBATCH --job-name=simRB
#SBATCH --mail-user=rr9932@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH -o slurm_outs_%A/LogFile_%x-%A_%a.out                 
#SBATCH -e slurm_outs_%A/errors_%x-%A_%a.err  
#SBATCH --chdir=./logs
#SBATCH --array=1-2

#Needed resources
#SBATCH -C cascade
#SBATCH --cpus-per-task=32 # 4 runs x 4 chains = 16 chains -> 2 cpus / chain
##SBATCH --mem-per-cpu=4000M          
#SBATCH --mem=18G
#SBATCH -t 2-00:00:00                   
#SBATCH --no-requeue                     #What to do in case of failure

export PATH=/scratch/gpfs/SIMOES/spack/spack/opt/spack/linux-skylake_avx512/revbayes-1.2.2-cr4nlouibdfhza73azisczz3zlziavsz/bin/:$PATH

cd /home/rr9932/RevBayes/simulation_analysis
srun rb-mpi scripts/rel_mvBM_job${SLURM_ARRAY_TASK_ID}.Rev
