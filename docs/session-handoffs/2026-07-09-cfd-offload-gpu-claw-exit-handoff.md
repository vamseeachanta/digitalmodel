# 2026-07-09 CFD Offload Exit Handoff

## Active Task

Offload current CFD execution from `ace-linux-2` to `gpu-claw` while preserving
`ace-linux-2` as a fallback. Tracking issue:
[#1495](https://github.com/vamseeachanta/digitalmodel/issues/1495).

## Completed This Session

- Provisioned and verified `gpu-claw` as the dedicated CFD node with OpenFOAM ESI
  v2312 package `2312.260127-2` and OpenMPI 4.1.6.
- Landed the `gpu-claw` benchmark manifest in
  <https://github.com/vamseeachanta/digitalmodel/pull/1500>.
- Landed the CFD dispatcher in
  <https://github.com/vamseeachanta/digitalmodel/pull/1501>.
- Updated workspace routing in
  <https://github.com/vamseeachanta/workspace-hub/pull/3405>.
- Fast-forwarded `gpu-claw:~/digitalmodel` to `origin/main` at `4d2713f8`.
- Archived local generated manifests out of the remote git checkout into the CFD
  work area so the node checkout is clean.
- Recorded the operational offload decision on
  [#1495](https://github.com/vamseeachanta/digitalmodel/issues/1495):
  <https://github.com/vamseeachanta/digitalmodel/issues/1495#issuecomment-4924717377>.

## Verified State

- `gpu-claw` git state: `main...origin/main`, HEAD `4d2713f8`, clean.
- `gpu-claw` visible compute: `nproc=8`; Linux CPU possible/present/online is
  only `0-7`.
- `gpu-claw` is a Proxmox/QEMU KVM VM. The 8-CPU limit is VM allocation, not a
  Linux boot, cgroup, or systemd affinity cap.
- Current valid routing benchmark:
  - `gpu-claw`: `0.5899 s/step @ 8 MPI ranks`
  - `ace-linux-2`: `1.0582 s/step @ 8 MPI ranks`
- Operational policy for now: route CFD to `gpu-claw`, cap at `--ranks 8`,
  keep `ace-linux-2` as fallback only.

Operator pattern from `ace-linux-1`:

```bash
CFD_GPU_CLAW_SSH=<vpn-ssh-target> uv run python scripts/setup/dispatch-cfd-run.py --ranks 8 -- <remote CFD command>
```

## Remaining Blockers

- Do not route above 8 MPI ranks until the Proxmox VM exposes more vCPUs and a
  fresh 3D benchmark manifest is committed.
- Cloud sync still needs owner-provided `rclone` remote/folder and any required
  secret.
- GitHub operations directly on `gpu-claw` still need a headless PAT if required.
- The runtime estimator is calibrated from 2D fan-out case-tree stats on
  `ace-linux-2`; do not substitute the 3D MPI benchmark into that model.

## Exact Next Checkpoint

If continuing CFD infrastructure:

1. If more compute is desired, increase the Proxmox VM vCPU allocation for
   `gpu-claw`.
2. Rerun `scripts/setup/verify-cfd-box.sh --benchmark ~/cfd_work` on an idle
   `gpu-claw`.
3. Commit the updated benchmark manifest and let the dispatcher select higher
   ranks only after the manifest and live CPU count agree.
4. For estimator recalibration, either run the 2D calibration workload on
   `gpu-claw` or split the estimator into separate 2D fan-out and 3D MPI modes.

If running CFD now:

1. Use `gpu-claw` for known 8-rank-or-smaller CFD work.
2. Use the dispatcher when load/fallback gating matters.
3. Avoid direct heavy CFD dispatch to `ace-linux-2` unless `gpu-claw` is
   unavailable or the run is explicitly fallback work.

## Repo And Residue Notes

- This handoff was written from a clean worktree based on latest `origin/main`.
- The primary local `digitalmodel` checkout had unrelated pre-existing dirty
  state and was not modified.
- Remote `gpu-claw:~/digitalmodel` is clean on `main`.

## Suggested Skills

- `handoff` for resuming from this note.
- `diagnose` if investigating Proxmox CPU/vCPU exposure or benchmark anomalies.
- `github:gh-fix-ci` if a follow-up benchmark or dispatcher PR fails checks.
- `pre-completion-cleanup-audit` before final closeout.
