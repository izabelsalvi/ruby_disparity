#!/bin/bash
# ---------------- Job parameters ----------------
#SBATCH --job-name=RBsim100
#SBATCH --mail-user=is7448@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-2

# Logging (relative to the directory from which you submit the job)
#SBATCH --output=logs/slurm_outs_%A/LogFile_%x-%A_%a.out
#SBATCH --error=logs/slurm_outs_%A/errors_%x-%A_%a.err

# ---------------- Resources ----------------
# 32 MPI ranks on one Cascade node; 2 days walltime
#SBATCH --ntasks=32
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=2-00:00:00
#SBATCH --no-requeue

# ---------------- Prep logs ----------------
# SLURM won't create directories for you
mkdir -p "logs/slurm_outs_${SLURM_ARRAY_JOB_ID}"

# ---------------- Environment ----------------
module purge

# ---------------- Work dir ----------------
cd /workdir/ruby_disparity/

# ---------------- Run ----------------
# Launch 32 MPI ranks; pick the .Rev script by array index (1..2)
mpirun -np 32 /programs/revbayes-1.3.0/rb-mpi "scripts/rel_mvBM_sampled50_mpi_job${SLURM_ARRAY_TASK_ID}.Rev"



