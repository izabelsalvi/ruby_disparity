#!/bin/bash
#SBATCH --job-name=RBsim100
#SBATCH --mail-user=is7448@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-2

# Run from your workdir so relative paths are consistent
#SBATCH --chdir=/workdir/ruby_disparity/

# Logs (dir *must* exist before sbatch)
#SBATCH --output=err_out/Log_%x-%A_%a.out
#SBATCH --error=err_out/Err_%x-%A_%a.err

# Resources:
#SBATCH --nodes=1
#SBATCH --ntasks=32
#SBATCH --ntasks-per-node=32
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=32G
#SBATCH --time=9-00:00:00
#SBATCH --no-requeue

set -euxo pipefail

# Light diagnostics to the SLURM log
date
hostname
echo "SLURM_NTASKS=${SLURM_NTASKS}  SLURM_ARRAY_TASK_ID=${SLURM_ARRAY_TASK_ID}"

# Confirm tools are present (fail early if not)
command -v mpirun
test -x /programs/revbayes-1.3.0/rb-mpi
/programs/revbayes-1.3.0/rb-mpi --version || true
mpirun --version || true

# ---------------- Run ----------------
# Launch 32 MPI ranks; pick the .Rev script by array index (1..2)
mpirun -np 32 /programs/revbayes-1.3.0/rb-mpi \
  "scripts/rel_mvBM_fulltree_moreGens_job${SLURM_ARRAY_TASK_ID}.Rev"
