# CFD execution box — onboarding runbook (digitalmodel #1495)

Bring a fresh Ubuntu 24.04 box to a working, **reproducible** CFD-execution state
(matching the shared workstation), benchmark it against that reference, and — if it wins — make it
the dedicated CFD node. interFoam is **CPU/MPI-bound**; "compute power" = cores +
memory bandwidth, not GPU.

## Topology
The box is **headless** and reached by **SSH from the control workstation** (`ssh <cfdbox>`).
Because it's headless: `gh` and cloud auth use **tokens, not browser
flows**. Because CFD outputs must survive the box, **key data (inputs / reduced
outputs / reports) is synced to a cloud remote** (see §5).

## 1. Provision (on the new box)
```bash
# copy this repo's provisioner over, or clone first then run it:
curl -fsSL https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/scripts/setup/provision-cfd-box.sh -o provision-cfd-box.sh
REPO_DIR=$HOME/digitalmodel WORK_DIR=$HOME/cfd_work bash provision-cfd-box.sh
```
Installs OpenFOAM ESI **v2312** (`dl.openfoam.com`), OpenMPI 4.1.x, uv, gh, git,
clones `digitalmodel`, installs its Python deps. Idempotent — can be re-run.

## 2. Verify
```bash
source /usr/lib/openfoam/openfoam2312/etc/bashrc
cd $HOME/digitalmodel
scripts/setup/verify-cfd-box.sh                 # toolchain + tiny serial/2-rank MPI smoke
```

## 3. Benchmark vs the shared workstation
```bash
scripts/setup/verify-cfd-box.sh --benchmark $HOME/cfd_work
# → writes docs/api/cfd/sloshing-3d-benchmark-<cores>-core-workstation.json
```
`docs/api/cfd/sloshing-3d-benchmark.json` is the **shared-workstation reference** (already in the
repo). Compare:
```bash
.venv/bin/python scripts/setup/compare-cfd-benchmark.py \
    docs/api/cfd/sloshing-3d-benchmark.json \
    docs/api/cfd/sloshing-3d-benchmark-<cores>-core-workstation.json
```
Matched case (cpb=60 / 216k cells, 0.30 s window, core ladder). Judge on **peak
throughput AND wall-clock predictability** — the reference workstation's limit was contention on
a shared box, not the solver.

The suffix uses the Python CPU count, or `workstation` if it is unavailable.
Refreshes preserve the unsuffixed reference. Workstations with equal CPU counts
share a default output name; use the collector's explicit `--label` with a
public role tag when their results must coexist.

## 4. Dispatch between CFD Linux hosts
Dispatch from the control workstation when dedicated and shared compute are
available over SSH. It probes each host for reachability, core count, and current
load, then selects the fastest eligible host from the benchmark manifests.

```bash
uv run python scripts/setup/dispatch-cfd-run.py --ranks 8 --dry-run -- \
    echo run-on-{host}-np-{ranks}
```

Remove `--dry-run` to execute the command on the selected host. The remote
command runs with OpenFOAM v2312 sourced, from `~/digitalmodel`, and receives:
`CFD_DISPATCH_HOST`, `CFD_DISPATCH_RANKS`, and `CFD_DISPATCH_S_PER_STEP`.
Host defaults:
- `dedicated-cfd`: SSH role alias `dedicated-cfd`; manifest
  `docs/api/cfd/sloshing-3d-benchmark-8-core-workstation.json`.
- `shared-cfd`: SSH role alias `shared-cfd`;
  manifest `docs/api/cfd/sloshing-3d-benchmark.json`.

Configure these role aliases in SSH configuration, retain the existing SSH
override variables, or supply `--host role=ssh-target,manifest,dedicated` (use
`shared` for shared compute). A different CPU capacity or explicit benchmark
label requires the corresponding manifest through `--host`.

For offline planning or tests, bypass SSH probes:
```bash
uv run python scripts/setup/dispatch-cfd-run.py \
    --assume-probe dedicated-cfd=8,0.5 \
    --assume-probe shared-cfd=32,8.0 \
    --ranks 8 --dry-run -- echo run-on-{host}-np-{ranks}
```

## 5. Data sync to the cloud (always-on)
Key data survives the box via `rclone`. One-time (HITL — provides a cloud secret):
```bash
gh auth login --with-token < token.txt   # headless: PAT, not browser
rclone config                             # create a remote (GDrive / S3 / Dropbox / …), e.g. "cfdcloud"
```
Then push (additive `copy` by default — never deletes cloud files):
```bash
RCLONE_REMOTE=cfdcloud:cfd-box scripts/setup/sync-cfd-data.sh
# always-on (cron every 30 min):
RCLONE_REMOTE=cfdcloud:cfd-box scripts/setup/sync-cfd-data.sh --install-timer 30m
```
Syncs the **small durable set** — input configs, result manifests, reports/HTML —
**not** the multi-GB raw case trees (regenerable; opt in with `SYNC_RAW=1`).

## 6. Migrate (if the new box wins)
- Commit the new box's benchmark manifest via PR (reference #1495).
- Update the CFD-execution routing: `workspace-hub` `routing-rules.yaml` (`domain:cfd`
  → new host) and the `cfd-execution-box` memory note.
- Re-calibrate the run-time estimator to the new box.
- Move heavy sweeps (e.g. the gated 3D L-tank, #1433, once #640 geometry lands) here;
  free the shared workstation for interactive work.

## Reproducibility notes
- The benchmark is only comparable at the **same OpenFOAM build** (v2312 2312.260127-2)
  and the **same mesh size**; the provisioner pins v2312 for exactly this reason.
- Run the benchmark on an **idle** box for clean high-core numbers (that was the
  shared-workstation failure mode).
