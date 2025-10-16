#!/bin/bash
#SBATCH --job-name=RBsim_sampled50
#SBATCH --mail-user=is7448@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-2

# Logs
#SBATCH --output=/workdir/ruby_disparity/err_out/Log_%x-%A_%a.out
#SBATCH --error=/workdir/ruby_disparity/err_out/Err_%x-%A_%a.err

# Resources: 1 node, 4 MPI ranks, 8G total RAM, 2 days
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=2-00:00:00
#SBATCH --no-requeue

# Working directory
cd /workdir/ruby_disparity/

# Admin-preferred launch: explicit mpirun + RevBayes path
mpirun -np 4 /programs/revbayes-1.3.0/rb-mpi \
  "scripts/rel_mvBM_sampled50_mpi_job${SLURM_ARRAY_TASK_ID}.Rev"
