# scripts/cfd: the solve-host chain

Bash that runs ON a CFD lane under OpenFOAM v2312, driven by a case registry at
the campaign root and the environment `DM_CFD_ROOT`, `DM_CFD_CONFIG`,
`DM_CFD_CASES_DIR`. `DM_CFD_ROOT` and `DM_CFD_CAMPAIGN` are authoritative when
set. Otherwise the shared resolver infers the first component below
`~/cfd/<campaign>/` from the working directory (campaign root, `scripts/`, or
`cases/<case>`) and prints the reason to stderr; only an unrelated working
directory uses the literal `campaign` fallback. `deploy_lane.sh` retains its
positional campaign override. Private project configuration supplies the real
campaign name and registry path. `DM_CFD_LANES_FILE` optionally selects the lane mapping
(default: the shipped `src/digitalmodel/solvers/openfoam/data/lanes.example.yml`),
and `DM_CFD_LANE` selects an entry for compute-stat collection.
Deployed from this directory with `deploy_lane.sh`; never edited on a lane.

| Script | Role |
|---|---|
| `stage45_driver.sh` | Stage 4 (mesh) and Stage 5 (solve) for one case. Mesh phase: mesh-store lookup -> link, or build -> promote; refinement ladder from `topoSetDict.N`; snappy; strict checkMesh text verdict; face-resolution gate; restore0Dir/setFields; decomposition + renumber per case. |
| `mesh_store.sh` | Master mesh store: `id find promote link dedupe verify status drop`; `promote --copy` preserves a running case's real mesh and `pull` imports one unambiguous entry from another lane. After a finished case's serial mesh was promoted with `--copy`, use `mesh_store.sh dedupe <case> <store-dir>` to replace that private copy with a link. Library twin: `digitalmodel.solvers.openfoam.mesh_store`, tested against this script. |
| `prune_case.sh` | Remove program artefacts of an assessed case (dry run by default); keeps inputs, logs, postProcessing; writes PRUNED.md. |
| `solve_case.sh` | Detached solver launcher (file-delivered, never piped: mpirun eats stdin); arms `ittc_watch.sh` and `poller.sh`. |
| `gate_case.sh` | Terminal waiter + two-window settling gate for a free-surface case, runs on the host reparented to PID 1. |
| `fs_gate.sh` | Free-surface acceptance gates read from the case log and force history: maximum water-content drift against a caller limit, settling and force plausibility. |
| `domain_mass_balance.py` | Solver-free phase-fraction log assessment; text or JSON, explicit PASS / FAIL / NOT_EVALUATED. |
| `continue_case.sh` | Continue a stopped parallel solve from its latest write (startFrom latestTime, raised endTime, detached, CONT_DONE/CONT_FAILED). The driver and solve_case.sh are fresh starts and discard processor time dirs. |
| `lane_probe.sh` | One status row per case (iteration, s/it, pseudo-dt range raw and smoothed, Courant caps, mass drift, componentwise max U, hull forces, p_rgh residual) for a lanes table; LTS-aware. Pass `--header` for column names. |
| `queue_after.sh` | Detached marker/ledger queue: waits for any prerequisite, waits for exact-name solver processes to clear, then launches with `QUEUE_*` status markers and a campaign ledger entry; includes `status` and PID-specific `cancel`. |
| `bench_prep.sh`, `bench_run.sh` | Prepare rank/PIMPLE/smoother variants from identical serial initial states, then run them one at a time and write `bench_results.tsv`, `BENCH_STATUS`, and `BENCH_DONE`; supports a readiness marker, host-load guard, and resume epilogue. |
| `ittc_watch.sh`, `poller.sh` | Convergence watcher and wall-clock budget poller for a running solve. |
| `check_face_resolution.py` | The mesh-phase face-resolution gate the driver runs before decomposition. |
| `deploy_lane.sh` | rsync this directory to `<host>:<data-root>/cfd/<campaign>/scripts/`; refuses while a chain driver runs there. |
| `lib/cfd_chain.sh`, `lib/cfd_config.py` | Shared shell library and registry reader. |
| `setup_*.sh`, `stage1_dtchull.sh`, `yplus_after.sh`, `auto_solve.sh`, `chain_stage45.sh`, `solve_chain.sh` | KCS/DTC verification chain (#1173). |
| `run_sloshing_*.py`, `reduce_*.py`, `render_sloshing_viz.py`, `run_spheric_test10.py`, `run_outlet_vent_study.py`, `collect_compute_stats.py` | Sloshing and vent studies (see their headers). |

The mass gate requires `DM_CFD_FIXED_DOMAIN=true` and a finite nonnegative
`DM_CFD_MASS_DRIFT_RTOL` expressed as a fraction. The tolerance is a case-specific
input; no universal engineering acceptance limit is supplied. Missing inputs,
missing or malformed phase records, an unidentified water phase, fewer than two
samples, a zero initial water content, or decreasing logged time produce
`NOT_EVALUATED` and prevent acceptance.
Every phase record is compared with the first, so recovery cannot hide an earlier
excursion. The complete log is required; an extracted tail changes the baseline.

The quantity is sourced from [OpenFOAM v2312 (2023), VoF/alphaEqn.H,
lines 124–128](https://api.openfoam.com/2312/VoF_2alphaEqn_8H_source.html):
the printed fraction is the volume-weighted mean of the water fraction. On a
fixed-volume domain, relative water-volume drift equals relative fraction drift.
The gate checks water-content stability; prescribed filling or drainage needs a
flux-aware residual instead. A moving domain needs actual integrated water
volumes. Passing this gate establishes neither interface sharpness nor boundary
flux closure, and does not establish the other acceptance gates.

Standalone use: `python3 scripts/cfd/domain_mass_balance.py log.interFoam
--fixed-domain --max-relative-drift <case-specific-fraction> --json`.
Only PASS returns success; FAIL and NOT_EVALUATED return a nonzero exit status.

Rules the scripts encode, learned on the lanes:

- Poll markers and `/proc/<pid>/cwd`, never `pgrep -f <path>`: it matches the
  ssh command carrying the path and has reported a dead run alive for 13.5 h
  and an idle host busy.
- Write a terminal marker on every exit path; absence of a marker is never
  readable as an outcome.
- No `set -u` in anything that sources the OpenFOAM bashrc (it dereferences
  unset variables); the driver saves and restores flags around it.
- `runApplication` skips a stage whose log exists; the mesh store makes reuse
  explicit instead of relying on that.
- Never edit a running bash script in place; bash resumes at a byte offset in
  the rewritten file.

Tests: `tests/scripts/test_cfd_*_guards.py` assert on the TEXT of these
scripts (a script that cannot run still reads correctly, so the guards pin
the invariants); `tests/scripts/test_cfd_chain_lib.py` executes the library;
`tests/solvers/openfoam/test_mesh_store.py` executes `mesh_store.sh` and the
Python twin on one fixture case and asserts equal identities.
`tests/solvers/openfoam/test_cfd_queue_bench_tools.py` exercises queue release
and cancellation plus benchmark preparation/results using stub OpenFOAM tools.

Design notes: `docs/domains/openfoam/mesh_store_case_layout.md`. PIMPLE loop / cost facts
(why 2/3/2 costs 3.4x 1/2/0, why residualControl is not a lever at nOuter 2):
`docs/domains/openfoam/pimple_loop_cost_v2312.md`. Bringing up a new lane:
`docs/domains/openfoam/new_lane_onboarding_prompt.md`.

## Status line for multi-lane operation

The generic tools in `status/` supersede the old lane-local `status-tools/`
folder. Copy `status/status.example.yml` to a private configuration file,
replace the lane SSH aliases, `<campaign>` placeholders, and case paths, then
set `DM_CFD_STATUS_CONFIG` for the collector and renderer. A missing, empty, or
`local` lane `ssh` value runs `lane_probe.sh` locally. Remote probe paths use
`DM_CFD_CAMPAIGN`, then the YAML `campaign` key, then the campaign inferred from
the first `<data-root>/cfd/<campaign>/cases/...` path. The collector atomically writes and
both renderers read the cache selected by `DM_CFD_STATUS_CACHE` (override) or the
YAML `cache` key.
Optional `bench_status` and `queue_status` paths are also cached. Install the
shipped `cfd_status_cron.example` entry to refresh it periodically.

Claude Code receives one fast, cache-only line from `cfd_statusline.sh`. To
prepend an existing workspace-hub segment, set `WORKSPACE_STATUSLINE` to that
executable and point `settings.json` at the combined command:

```json
{
  "statusLine": {
    "type": "command",
    "command": "/absolute/path/to/digitalmodel/scripts/cfd/status/cfd_statusline_combined.sh",
    "padding": 0
  }
}
```

The renderer emits one `lane:case it/end s/it w<write> m<drift%>` segment per
lane, separated by ` │ `, then cache age. It marks unreachable lanes and stale
caches without blocking the prompt. Use `--width COLUMNS` to bound the segment.
