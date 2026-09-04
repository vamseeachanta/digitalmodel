# scripts/cfd: the solve-host chain

Bash that runs ON a CFD lane (gpu-claw, ace-linux-2) under OpenFOAM v2312,
driven by a case registry (`db_chain.yml` / `b1552_chain.yml` at the campaign
root) and the environment `DM_CFD_ROOT`, `DM_CFD_CONFIG`, `DM_CFD_CASES_DIR`.
Deployed from this directory with `deploy_lane.sh`; never edited on a lane.

| Script | Role |
|---|---|
| `stage45_driver.sh` | Stage 4 (mesh) and Stage 5 (solve) for one case. Mesh phase: mesh-store lookup -> link, or build -> promote; refinement ladder from `topoSetDict.N`; snappy; strict checkMesh text verdict; face-resolution gate; restore0Dir/setFields; decomposition + renumber per case. |
| `mesh_store.sh` | Master mesh store: `id find promote link dedupe verify status drop`. Library twin: `digitalmodel.solvers.openfoam.mesh_store`, tested against this script. |
| `prune_case.sh` | Remove program artefacts of an assessed case (dry run by default); keeps inputs, logs, postProcessing; writes PRUNED.md. |
| `solve_case.sh` | Detached solver launcher (file-delivered, never piped: mpirun eats stdin); arms `ittc_watch.sh` and `poller.sh`. |
| `gate_case.sh` | Terminal waiter + two-window settling gate for a free-surface case, runs on the host reparented to PID 1. |
| `fs_gate.sh` | Free-surface acceptance gates read from the case's own log and force history: mass drift <= 0.5 %, two 400-iteration windows < 0.2 % on viscous and pressure, 0.6 < Cf/ITTC < 1.3. |
| `ittc_watch.sh`, `poller.sh` | Convergence watcher and wall-clock budget poller for a running solve. |
| `check_face_resolution.py` | The mesh-phase face-resolution gate the driver runs before decomposition. |
| `deploy_lane.sh` | rsync this directory to `<host>:~/cfd/<campaign>/scripts/`; refuses while a chain driver runs there. |
| `lib/cfd_chain.sh`, `lib/cfd_config.py` | Shared shell library and registry reader. |
| `setup_*.sh`, `stage1_dtchull.sh`, `yplus_after.sh`, `auto_solve.sh`, `chain_stage45.sh`, `solve_chain.sh` | KCS/DTC verification chain (#1173). |
| `run_sloshing_*.py`, `reduce_*.py`, `render_sloshing_viz.py`, `run_spheric_test10.py`, `run_outlet_vent_study.py`, `collect_compute_stats.py` | Sloshing and vent studies (see their headers). |

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

Design notes: `docs/domains/openfoam/mesh_store_case_layout.md`.
