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
| `fs_gate.sh` | Free-surface acceptance gates read from the case's own log and force history: mass drift <= 0.5 %, two 400-iteration windows < 0.2 % on viscous and pressure, 0.6 < Cf/ITTC < 1.3. |
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

## Step 3: Case 2.10 regular head waves (offline)

The Case 2.10 fixture is
`tests/fixtures/test_vectors/marine/kcs_head_wave_efd.json`. It records the six
published conditions, C0 through C5, with field units and source locators.
C0 is the calm-water baseline; the wave reader deliberately rejects C0 because
normalization by wave amplitude is undefined there. Case 2.1 results cannot be
substituted: the model and test conditions differ.

Sources: [Case 2.10 instructions](https://t2015.nmri.go.jp/Instructions_KCS/Case_2.10/Case_2-10.html),
Conditions, Requested computations 2.10-1, Fourier Series Instructions and Symbols;
[workshop corrections](https://t2015.nmri.go.jp/LatestNews.html), September 9,
10 and 14, 2015. The corrected workbook is
`[Identifier]_6conditions_2-10_20150914.xlsx`; `Case2.10-2.zip` contains reconstructed
Fourier histories, not raw experimental signals. No licence statement was found:
the data is cited and read back, not redistributed. Neither file was downloaded.
Every experimental response remains a required `NOT_EVALUATED` input, with null
value, sheet and cell. The source SHA-256 and response-table SHA-256 also remain
pending. The public condition page does not supply the experimental response
values; no plot-axis limit or prescribed wave height is substituted for a result.

The reader covers the quantities requested by 2.10-1: resistance coefficient
scaled by 1000, normalized heave and pitch, harmonic amplitudes zero through four,
phases one through four, and the incident-wave first harmonic. It does not invent
an added-resistance coefficient or treat a total coefficient as added resistance.
A future native-output adapter must supply CSV columns `time_over_Te`, `ct_x1000`,
`heave_over_a`, `pitch_over_ka`, and `wave_over_lpp`. Here `a=Hs/2`,
`k=2*pi/lambda`, and `Te=1/(sqrt(g/(2*pi*lambda))+U/lambda)`, as specified under
Symbols. Pitch is converted to radians before normalization. Heave and pitch are
at the centre of gravity, positive upward and bow upward; the incident wave is
measured at FP. Metadata must declare the exact fixture conditions, conventions,
units, mesh, evidence kind, history hash, reference hash and criteria hash.

The signed zero harmonic equals twice the mean in waves. The C0 entries retain
means. Harmonics are fitted in physical encounter time; phase is shifted by
`n * incident_fundamental_phase` to the workshop crest-at-FP time origin, then
wrapped into a circular angle. This avoids applying one fundamental phase shift
to every harmonic. A harmonic below the declared amplitude floor has undefined
phase and cannot produce a passing complete comparison.

Response cells inherit shared source and condition metadata from each row's
`response_provenance`. Cell-specific sheet and cell locators remain explicit;
cell overrides are checked against the row condition before qualification.
`readback.table_sha256` is computed with `response_table_digest(row)`, which
expands inherited fields into each response before hashing. Shared-source edits
therefore invalidate readback, and expanded legacy tables retain the same digest.

Criteria are declared before any run in `config/cfd/kcs_step3_qa.json` and pinned
by digest. Each group's basis is recorded in `threshold_basis`. The retained
choices are assumed, based on previous experience; a maintainer decision on the
new threshold groups is not recorded. These are provisional diagnostic thresholds, not
experimental uncertainty: absolute limits are 0.1 for scaled resistance and
phase (radians), 0.05 for normalized motions, and 0.001 for normalized incident
wave amplitude. Each quantity's allowed window drift is one fifth of its
comparison limit. At least 32 regularly spaced samples per encounter cycle are
required, with five discarded cycles followed by two complete five-cycle
windows. Fit-residual RMS limits are 0.02 for scaled resistance, 0.01 for each
normalized motion and 0.0002 for normalized incident wave elevation. These
limits reject unstable or noisy content outside the retained harmonic basis.
The CSV omits a duplicated terminal cycle endpoint. Residual, y+ and
free-surface checks reuse step 1's observed-diagnostics routine and declared
limits. These checks do not establish wave-resolution adequacy or reflection
uncertainty for a future production campaign.

At the fourth retained harmonic, 32 samples per encounter cycle give 8 samples
per harmonic period, above the Nyquist minimum of 2. This does not derive the
choice of 32 or establish adequacy for higher-frequency content. The window
settings require at least fifteen complete cycles; this duration does not prove
transients have decayed. Fit-residual limits equal the corresponding non-phase
window-drift limits. These arithmetic relationships do not supply a physical or
experimental uncertainty basis for the retained numerical thresholds.

Offline CLI (all arguments are existing local evidence; no solver is launched):

```text
PYTHONPATH=src python scripts/cfd/readback_kcs_step3.py compare --condition C1 --reference tests/fixtures/test_vectors/marine/kcs_head_wave_efd.json --criteria config/cfd/kcs_step3_qa.json --history history.csv --metadata metadata.json --output comparison.json
```

Use `admit --next-mesh medium` or `admit --next-mesh fine` with a native coarse
history/metadata pair. The existing step-1 `require_run_admission` API routes
explicit `case="2.10", condition_id="C1"` requests to the wave gate. Missing
reference readback, criteria or passing coarse evidence refuses admission;
the fixture shipped here therefore refuses all native admission. File readback
recomputes the comparison instead of trusting a saved PASS. Evidence declarations
and digests establish consistency, not external authenticity. A future launcher
must enforce this API immediately before execution; no existing launcher is
claimed to be protected by this offline change.

Use `triplet` with three repeated `--history`/`--metadata` pairs, one per mesh.
Metadata also requires `h_m`, identical `mesh_family` and `numerics`,
`refinement_axes="xyz"`, and observed `free_surface.dx_m`/`dz_m` proportional to
cell size. Exactly thirteen positive nonzero amplitudes reuse step 1's three-grid
GCI procedure: four per response signal and the incident-wave fundamental. The
declared minimum order, maximum order and asymptotic-deviation bounds are passed
to that procedure. For equal refinement ratios, the fitted three-point power law
makes the asymptotic ratio an algebraic consistency check; it does not independently
establish the asymptotic range.
Phases and signed zero harmonics have no grid-uncertainty method in this slice;
`qualified` therefore stays false even when amplitude GCI passes. Degenerate,
oscillatory, divergent and nonpositive amplitude series refuse GCI.

Compute belongs on the GPU host or Grace-class host, never on the
licensed-solver workstation. Heavy outputs belong in the shared data store with
a digest manifest and fetch script; only the manifest belongs in the repository.
No CFD run or mesh generation is part of this slice. Remaining ladder work:
step 1 needs verified source readback and native systematic-grid evidence;
step 2 needs the proceedings reference tables; step 3 needs response readback,
source qualification, native-output conversion and coarse evidence before any
medium/fine run; step 4 needs authorized access to the captive-test data;
step 5 remains unscoped. Overall engineering qualification remains unestablished.
