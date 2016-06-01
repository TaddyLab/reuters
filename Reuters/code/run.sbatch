#!/bin/bash
#SBATCH --time=36:0:0
#SBATCH --mem-per-cpu=2000
#SBATCH --output=%j.err
#SBATCH --constraint=ib
#SBATCH --exclusive

module load parallel

LOG=output/an_sbatch.log
echo `date` > $LOG
echo "$SLURM_NNODES nodes" >> $LOG

export para="parallel -j $SLURM_NNODES --joblog output/para.log"
export srun="srun --exclusive -N1 -c16 --ntasks=1"  
$para $srun "Rscript reutersMNIR.R {1} > output/r{1}.log" ::: {0..20}

echo "done @" `date` >> $LOG

