# PAUSED 2026-08-20T16:49:28Z -- operator request, NOT convergence

Case: kcs_fine (4.35M, 8 ranks)
Stopped cleanly with `stopAt writeNow`; the solver logged End and
Finalising parallel run, and wrote its fields at iteration $LAST.

This is a PAUSE, not a discard. Nothing is lost.

## Why it stopped
The owner paused benchmark validation work to free both hosts for client
work. It did NOT converge and its ITTC criterion was never met -- do not
read the stop as a result.

## To resume
    cd $HOME/cfd/dm1173/kcs_cases/kcs_fine
    source /usr/lib/openfoam/openfoam2312/etc/bashrc   # no set -e / set -u
    setsid nohup mpirun -np NRANKS interFoam -parallel > log.interFoam.resume 2>&1 < /dev/null &
    setsid nohup ~/cfd/dm1173/ittc_watch.sh >/dev/null 2>&1 < /dev/null &

controlDict already carries startFrom latestTime, so it picks up at $LAST.
NRANKS: 8 for kcs_fine, 16 for kcs_prod_yplus -- must match the existing
processor* decomposition, which is untouched.

## Watch out
The watcher reads the NEWEST coefficient file. A resumed run writes
coefficient_N.dat rather than appending, so confirm it resolves the live file
before trusting a verdict -- that exact mistake armed a dead run once already.
