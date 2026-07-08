#!/usr/bin/env bash
# verify-cfd-box.sh — confirm a provisioned box can run interFoam + MPI, then
# (optionally) run the 3D scaling benchmark labelled for this host so it can be
# compared head-to-head with ace-linux-2 (digitalmodel #1495).
#
# Run AFTER provisioning, with OpenFOAM sourced:
#   source /usr/lib/openfoam/openfoam2312/etc/bashrc
#   scripts/setup/verify-cfd-box.sh                        # smoke + 2-rank MPI
#   scripts/setup/verify-cfd-box.sh --benchmark WORKDIR    # + full scaling sweep
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"
WORK_DIR="${2:-${WORK_DIR:-$HOME/cfd_work}}"
PY="$REPO_ROOT/.venv/bin/python"; [[ -x "$PY" ]] || PY="python"

ok()  { printf '  \033[1;32m✓\033[0m %s\n' "$*"; }
bad() { printf '  \033[1;31m✗\033[0m %s\n' "$*" >&2; FAIL=1; }
FAIL=0

echo "== toolchain =="
for t in blockMesh interFoam decomposePar reconstructPar mpirun; do
  if command -v "$t" >/dev/null 2>&1; then ok "$t ($(command -v "$t"))"; else bad "$t NOT on PATH — did you 'source .../etc/bashrc'?"; fi
done
[[ -x "$PY" || "$PY" == python ]] && ok "python: $($PY --version 2>&1)"
(( FAIL )) && { echo "toolchain incomplete — fix before benchmarking."; exit 1; }

echo "== smoke: tiny 3D interFoam, serial + 2-rank MPI =="
# reuse the benchmark harness at a trivial size + window (fast machinery check)
if $PY scripts/cfd/run_sloshing_3d_benchmark.py --work-dir "$WORK_DIR" \
     --cpb 24 --end-time 0.02 --dt 0.001 --ranks 1 2 --label "$(hostname -s)-smoke" >/tmp/cfdsmoke.log 2>&1; then
  if grep -q "np= 2.*completed" /tmp/cfdsmoke.log && grep -q "np= 1.*completed" /tmp/cfdsmoke.log; then
    ok "serial + 2-rank (decomposePar + mpirun -parallel) both completed"
    grep -E "np=[[:space:]]*[12]" /tmp/cfdsmoke.log | sed 's/^/    /'
  else
    bad "smoke run did not complete both ranks — see /tmp/cfdsmoke.log"; tail -15 /tmp/cfdsmoke.log
  fi
else
  bad "smoke run failed — see /tmp/cfdsmoke.log"; tail -20 /tmp/cfdsmoke.log
fi
(( FAIL )) && exit 1

if [[ "${1:-}" == "--benchmark" ]]; then
  echo "== full 3D scaling benchmark (labelled '$(hostname -s)') =="
  echo "   (matches the a-l-2 case: cpb=60/216k cells, 0.30s window, core ladder)"
  ranks=$(nproc); ladder="1 2 4 8 16"; (( ranks >= 24 )) && ladder="$ladder 24"; (( ranks >= 32 )) && ladder="$ladder 32"
  $PY scripts/cfd/run_sloshing_3d_benchmark.py --work-dir "$WORK_DIR" \
      --cpb 60 --end-time 0.30 --dt 0.001 --ranks $ladder --label "$(hostname -s)"
  echo ""
  echo "Compare vs a-l-2 baseline:"
  echo "  $PY scripts/setup/compare-cfd-benchmark.py \\"
  echo "      docs/api/cfd/sloshing-3d-benchmark.json \\"
  echo "      docs/api/cfd/sloshing-3d-benchmark-$(hostname -s).json"
fi
echo "verify-cfd-box: OK"
