#!/bin/bash
#SBATCH --job-name=RBsim50
#SBATCH --mail-user=is7448@princeton.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-2

# Run from your workdir so relative paths are consistent
#SBATCH --chdir=/workdir/ruby_disparity/

# Logs (dir *must* exist before sbatch)
#SBATCH --output=err_out/Log_%x-%A_%a.out
#SBATCH --error=err_out/Err_%x-%A_%a.err

# Resources: 1 node, 4 MPI ranks, 8G total RAM, 2 days
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=1
#SBATCH --mem=70G
#SBATCH --time=2-00:00:00
#SBATCH --no-requeue
# If your site requires a partition/account, uncomment:
# #SBATCH -p <partition>
# #SBATCH -A <account>

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

# Launch 4 MPI ranks; pick script by array index (1..2)
mpirun -np 8 /programs/revbayes-1.3.0/rb-mpi \
  "scripts/rel_mvBM_sampled50_mpi_job${SLURM_ARRAY_TASK_ID}.Rev"
