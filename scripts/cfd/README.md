# scripts/cfd: the solve-host chain

## Ship-hydrodynamics ladder, step 1

Step 1 prepares the Tokyo 2015 Case 2.1 calm-water QA gate. Native runs
attempted/completed: **0/0**; qualified results: **0**. This slice runs no CFD,
generates no mesh and downloads no data file. No licensed software was used.

The reference is row B of
`tests/fixtures/test_vectors/marine/kcs_resistance_efd.yaml`, free to heave and
pitch with rudder. The [Case 2.1 instructions](https://t2015.nmri.go.jp/Instructions_KCS/Case_2.1/Case_2-1.html),
Requested computations 2.1-2, 2.1-3 and 2.1-4, link `vary_Fr_2-1.xls`.
The existing six numeric rows are retained. The workbook has not been reopened,
its worksheet/cells and digest are unverified, and the inherited centimetre
interpretation of sinkage needs confirmation. No licence statement was found;
the data is cited and read back, not redistributed. The committed reference
therefore **refuses medium and fine admission**. A reviewed source readback must
record the actual worksheet/cells, source SHA-256 and confirmed units before
release. Its `table_sha256` must bind the canonical JSON of `points`, `units`
and `sign_convention`; changing those fields invalidates the source readback.
The page gives sinkage positive upwards and trim positive bow up. Reynolds
number varies with speed; the inherited Reynolds value applies at Fr 0.260.

`config/cfd/kcs_step1_qa.json` declares the criteria before any future run.
At Fr 0.260, Ct uses a 3.0% relative tolerance; sinkage uses an absolute 0.1 cm
and trim an absolute 0.02 deg tolerance. These criteria are **assumed, based on
previous experience**, not experimental uncertainty bounds. Unverified sinkage
units prevent their use for native qualification. Force components must settle
over two trailing 10 s windows, with at least 20 regularly spaced physical-time
samples in each, drift at most 0.2% and standard deviation at most 1.0% in both.
Motion uses the same windows: sinkage drift at most 0.02 cm and standard
deviation at most 0.05 cm; trim drift at most 0.004 deg and standard deviation
at most 0.01 deg. A matching mean alone does not establish settled motion.
The preceding transient is excluded by this fixed window rule. Declared final
residual limits are 0.0001 for pressure and 0.00001 for velocity. Measured y+
maximum must be at most 2, dx/Lpp at most 0.01 and dz/Lpp at most 0.001.
These mesh and residual limits are provisional engineering choices.

`scripts/cfd/readback_kcs_step1.py` reads offline CSV histories. Required columns
are `time_s,ct,cp,cv,sinkage_cm,trim_deg`: time is in seconds, coefficients are
dimensionless signed full-hull coefficients, sinkage is provisionally in cm,
and trim is in degrees. Ct must equal Cp + Cv. Native extraction must use the
reference density, wetted area including rudder and model speed. Missing,
nonfinite, duplicate, reversed or irregular samples fail. Metadata JSON declares
`schema_version: 1`, `evidence_kind` (`synthetic` or `native`), `mesh`,
`reference_sha256`, `criteria_sha256`, `conditions`, `units`, `sign_convention`,
`history_sha256` (the actual CSV bytes),
`residuals`, `y_plus` (`min`, `mean`, `max`) and `free_surface` (`dx_m`, `dz_m`).
`reference_conditions()` defines the condition schema. `digest()` computes
SHA-256 of canonical sorted compact JSON for the Case 2.1 row and criteria.
The v1 policy content is pinned in the validator. Loosening limits under the
same revision is refused; a reviewed criteria revision must update that pin.
Observed diagnostics and declared targets remain separate in the output.

With `PYTHONPATH=src`, run the following against supplied local evidence:

```sh
python scripts/cfd/readback_kcs_step1.py compare \
  --reference tests/fixtures/test_vectors/marine/kcs_resistance_efd.yaml \
  --criteria config/cfd/kcs_step1_qa.json \
  --history coarse.csv --metadata coarse.json --output comparison.json
```

`compare` exit 0 means the numerical comparison and convergence criteria passed;
`qualified` remains false for a single run. It does not authorize a run.
`admit --next-mesh medium` or `fine`, with the same arguments, calls
`require_run_admission()` and rereads the coarse history. Missing reference,
missing criteria, failed smoke evidence, changed digests, mismatched conditions,
or synthetic evidence refuse admission. `admit --next-mesh coarse` requires a
qualified reference and policy but no history/metadata pair. No command launches
a solver. A future step 1 launcher must call this gate at its execution boundary;
it is a supplied-evidence check, not an authenticity or execution transaction.
Callers must check the command's exit status and current input digests; a
previous output file is never evidence that the current invocation passed.

`triplet` takes three repeated `--history` and matching `--metadata` arguments.
Metadata must also declare `h_m`, `mesh_family`, `numerics` and
`refinement_axes: xyz`. The representative h is the volume-derived cell size;
the evidence producer must establish a common topology and all-axis refinement.
The readback checks common family/numerics and matching h, dx and dz ratios.
Only three monotone-converging constant-r meshes support GCI here; unequal
ratios beyond relative 0.000001, oscillation, divergence and identical results
fail. The existing GCI implementation supplies its three-grid safety factor.
The method follows the [discretization-uncertainty procedure](https://www.asme.org/wwwasmeorg/media/resourcefiles/shop/journals/jfenumaccuracy.pdf),
Steps 2 to 5, and [ITTC 7.5-03-01-01](https://ittc.info/media/11950/75-03-01-01.pdf),
2024 revision 05, Convergence Studies. Fine-grid GCI must be at most 1.0% for
Ct, Cp and Cv. The provisional observed-order range is 0.5 to 3.0; the
asymptotic ratio must
be within 0.05 of unity. For constant-r three-point fitting, the asymptotic
ratio is algebraically dependent and does not independently prove asymptotic
behavior. Motion grid uncertainty is not established by this resistance GCI.
The synthetic quadratic regression establishes the arithmetic; synthetic
evidence never qualifies a native result. Decisions use unrounded
values; JSON retains calculation precision. The original fixed-bare V2a/V2b
thresholds and V3 pair check remain separate and do not qualify Case 2.1.

Future compute will run on the GPU host or Grace-class host, never on the
licensed-solver workstation. Heavy inputs, plotted arrays, meshes, fields and
native outputs will go to the shared data store with a digest manifest and fetch
script; the repository will retain the manifest. No storage copy is claimed here.
Steps 2 to 4 have no qualified reference table in this slice: step 2 waits for
the Gothenburg 2010 table; step 3 has openly accessible head-wave data, but the
table/time histories have not been obtained; step 4 waits for SIMMAN 2008
credentials and its raw series. Access does not establish reuse rights.

## Existing solve-host chain

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
