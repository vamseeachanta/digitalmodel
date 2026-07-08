# CFD execution box — onboarding runbook (digitalmodel #1495)

Bring a fresh Ubuntu 24.04 box to a working, **reproducible** CFD-execution state
(matching `ace-linux-2`), benchmark it against a-l-2, and — if it wins — make it
the dedicated CFD node. interFoam is **CPU/MPI-bound**; "compute power" = cores +
memory bandwidth, not GPU.

## 1. Provision (on the new box)
```bash
# copy this repo's provisioner over, or clone first then run it:
curl -fsSL https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/scripts/setup/provision-cfd-box.sh -o provision-cfd-box.sh
REPO_DIR=$HOME/digitalmodel WORK_DIR=$HOME/cfd_work bash provision-cfd-box.sh
```
Installs OpenFOAM ESI **v2312** (`dl.openfoam.com`), OpenMPI 4.1.x, uv, gh, git,
clones `digitalmodel`, installs its Python deps. Idempotent — safe to re-run.

## 2. Verify
```bash
source /usr/lib/openfoam/openfoam2312/etc/bashrc
cd $HOME/digitalmodel
scripts/setup/verify-cfd-box.sh                 # toolchain + tiny serial/2-rank MPI smoke
```

## 3. Benchmark vs a-l-2
```bash
scripts/setup/verify-cfd-box.sh --benchmark $HOME/cfd_work
# → writes docs/api/cfd/sloshing-3d-benchmark-<hostname>.json (per-box, doesn't clobber a-l-2's)
```
`docs/api/cfd/sloshing-3d-benchmark.json` is the **a-l-2 baseline** (already in the
repo). Compare:
```bash
.venv/bin/python scripts/setup/compare-cfd-benchmark.py \
    docs/api/cfd/sloshing-3d-benchmark.json \
    docs/api/cfd/sloshing-3d-benchmark-<hostname>.json
```
Matched case (cpb=60 / 216k cells, 0.30 s window, core ladder). Judge on **peak
throughput AND wall-clock predictability** — a-l-2's real limit was contention on
a shared box, not the solver.

## 4. Migrate (if the new box wins)
- Commit the new box's benchmark manifest via PR (reference #1495).
- Update the CFD-execution routing: `workspace-hub` `routing-rules.yaml` (`domain:cfd`
  → new host) and the `cfd-execution-box` memory note.
- Re-calibrate the run-time estimator to the new box.
- Move heavy sweeps (e.g. the gated 3D L-tank, #1433, once #640 geometry lands) here;
  free a-l-2 for interactive/dev work.

## Reproducibility notes
- The benchmark is only comparable at the **same OpenFOAM build** (v2312 2312.260127-2)
  and the **same mesh size**; the provisioner pins v2312 for exactly this reason.
- Run the benchmark on an **idle** box for clean high-core numbers (that was the
  a-l-2 failure mode).
