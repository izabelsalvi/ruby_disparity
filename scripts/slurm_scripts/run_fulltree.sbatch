#!/bin/bash
#Job parameters

#SBATCH --job-name=RBsim100
#SBATCH --mail-user=rr9932@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH -o slurm_outs_%A/LogFile_%x-%A_%a.out                 
#SBATCH -e slurm_outs_%A/errors_%x-%A_%a.err  
#SBATCH --chdir=./logs
#SBATCH --array=1-2

#Needed resources
#SBATCH -C cascade
#SBATCH --cpus-per-task=1 
#SBATCH --ntasks-per-node=32
#SBATCH --mem-per-cpu=32G
#SBATCH -t 6-00:00:00                   
#SBATCH --no-requeue                     #What to do in case of failure

export PATH=/scratch/gpfs/SIMOES/spack/spack/opt/spack/linux-skylake_avx512/revbayes-1.2.2-cr4nlouibdfhza73azisczz3zlziavsz/bin/:$PATH

module load openmpi/gcc/4.1.6

cd /home/rr9932/RevBayes/simulation_analysis
mpirun -np 32 rb-mpi scripts/rel_mvBM_fulltree_moreGens_job${SLURM_ARRAY_TASK_ID}.Rev
