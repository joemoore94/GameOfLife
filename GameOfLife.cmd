#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N game
#PBS -j oe
#PBS -l nodes=2:ppn=2
#PBS -l walltime=00:05:00

cd #PBS_O_WORKDIR
cd AM250_project_Moore
mpirun -np 4 game
