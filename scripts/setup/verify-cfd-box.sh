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
OPENFOAM_PACKAGE_VERSION="2312.260127-2"
OPENMPI_PACKAGE_VERSION="4.1.6-7ubuntu2"
WORK_DIR="${2:-${WORK_DIR:-$HOME/cfd_work}}"
PY="$REPO_ROOT/.venv/bin/python"; [[ -x "$PY" ]] || PY="python"

ok()  { printf '  \033[1;32m✓\033[0m %s\n' "$*"; }
bad() { printf '  \033[1;31m✗\033[0m %s\n' "$*" >&2; FAIL=1; }
package_version() { dpkg-query -W -f='${Version}' "$1" 2>/dev/null || true; }
FAIL=0

echo "== toolchain =="
for t in blockMesh interFoam decomposePar reconstructParMesh mpirun; do
  if command -v "$t" >/dev/null 2>&1; then ok "$t ($(command -v "$t"))"; else bad "$t NOT on PATH — did you 'source .../etc/bashrc'?"; fi
done
[[ "$(package_version openfoam2312-default)" == "$OPENFOAM_PACKAGE_VERSION" ]] \
  && ok "openfoam2312-default $OPENFOAM_PACKAGE_VERSION" \
  || bad "openfoam2312-default must equal $OPENFOAM_PACKAGE_VERSION"
[[ "$(package_version openmpi-bin)" == "$OPENMPI_PACKAGE_VERSION" ]] \
  && ok "openmpi-bin $OPENMPI_PACKAGE_VERSION" \
  || bad "openmpi-bin must equal $OPENMPI_PACKAGE_VERSION"
[[ "$(package_version libopenmpi-dev)" == "$OPENMPI_PACKAGE_VERSION" ]] \
  && ok "libopenmpi-dev $OPENMPI_PACKAGE_VERSION" \
  || bad "libopenmpi-dev must equal $OPENMPI_PACKAGE_VERSION"
git diff --quiet || bad "working tree has tracked modifications"
git diff --cached --quiet || bad "working tree has staged modifications"
[[ -z "$(git ls-files --others --exclude-standard)" ]] \
  || bad "working tree has untracked files"
if uv run --frozen --extra cfd python -c \
  'import gmsh; assert gmsh.__version__ == "4.15.1"' >/dev/null; then
  ok "locked Python gmsh 4.15.1"
else
  bad "locked CFD environment cannot import gmsh 4.15.1"
fi
(( FAIL )) && { echo "toolchain incomplete — fix before benchmarking."; exit 1; }

echo "== smoke: synthetic Gmsh bridge + 2-rank MPI machinery =="
mkdir -p "$WORK_DIR"
smoke_log="$WORK_DIR/verify-synthetic-smoke.log"
if CFD_DISPATCH_RANKS=2 CFD_EXECUTION_CLASS=test \
  uv run --frozen --extra cfd python scripts/cfd/run_synthetic_tank_3d_smoke.py \
    --ranks 2 \
    --work-dir "$WORK_DIR/verify-synthetic-smoke" \
    --evidence "$WORK_DIR/verify-synthetic-smoke.json" \
    --cpb 6 --end-time 0.02 --delta-t 0.001 >"$smoke_log" 2>&1; then
  ok "Gmsh bridge + non-oversubscribed 2-rank smoke completed"
else
  bad "synthetic smoke failed — see $smoke_log"
  tail -20 "$smoke_log"
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
