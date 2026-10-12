# Safe warm starts for calm-water resistance

`scripts/cfd/warm_start.py` prepares and supervises OpenFOAM v2312 warm starts. It is
fail-closed: a case is touched only after all admissibility checks pass and the expected
iteration saving clears the configured margin. Campaign and lane identity remain external.
`DM_CFD_ROOT` and `DM_CFD_CAMPAIGN` are authoritative when set. Without either override,
commands run below `~/cfd/<campaign>/` infer the first directory below `~/cfd` (including
the campaign root, `scripts/`, and `cases/<case>`); otherwise they retain the literal
`~/cfd/campaign` fallback. The resolver writes one explanatory line to stderr. Keep lane
identity in `DM_CFD_LANES_FILE`; do not put deployment identifiers in cases.

## Usage

The concise forms are:

```bash
scripts/cfd/warm_start.py --from speed SOURCE --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py --from geometry SOURCE --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py plan --dry-run --from case --hop speed --source SOURCE --target CASE --mesh-level r3 --source-mesh-level r3
scripts/cfd/warm_start.py --from potential --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py --from analytic --eta eta.csv --u velocity.csv --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py check --target CASE --mesh-level L2 --cold-ref COLD_CASE
scripts/cfd/warm_start.py record add-cold --case COLD_CASE --mesh-level L2
scripts/cfd/warm_start.py record --record CAMPAIGN/warm_start
```

The section-8 spelling is also supported: `prepare --from case --hop speed|geometry
--source SOURCE`. Use `plan` for a read-only verdict, `prepare` to create fields, and `run`
to prepare and launch the case's `solve_chain.sh`. `--dry-run` prints the verdict,
expected-value calculation, and exact commands. `--relaunch COMMAND` overrides the case
driver. Reference data live in `warm_start/level_<tag>.yml`; `--n-cold` may supply the
level budget explicitly. `record add-cold` reduces a cold history with at least three
pressure extrema and stores its first-cycle pressure amplitude, settled viscous force,
and settling iteration for that level. At check time `--cold-ref` takes precedence,
followed by the source case recorded by the warm hop, then the per-level record. If none
can provide all three statistics, the command prints one error line and exits 2. The
explicit cold reference may be a different condition at the same speed when the
same-condition cold run does not yet exist; it is used as a force-history shape reference,
and the verdict identifies that fact.

## The three safety layers

Layer 1 checks source convergence and a clean solver exit, wall treatment, numerics,
reference frame, decomposition/reconstruction, hop type, speed difference, ladder level,
mapped alpha mass, rewritten speed fields, `pcorr`, and level references. Speed hops are
limited to `|dU/U| <= 10%`. Geometry hops require equal speed and different meshes.
Only `alpha.water U p_rgh k omega nut` transfer. `phi`, `alphaPhi0.water`, `rDeltaT`, `p`,
and `uniform/` are removed. Boundary values for inlet and outlet `U`, inlet `k`, and inlet
`omega` come from the target and are applied with `changeDictionary -time 0`. For an
identical decomposed speed hop, all six fields copy directly from each
`processorN/<time>/` to the corresponding `processorN/0/`; serial `0/` is not used.
Matching processor names are insufficient: each partition must also have the same
`constant/polyMesh` hash or cell count. The implicit latest time considers both serial
and processor directories and never selects time 0; if the newest time exists only under
`processor*/`, all six fields are reconstructed unless that strong decomposition check
permits direct copying. Use `--source-time 0` to request time 0 explicitly. A5 reports the
chosen time and tests reconstruction at that exact time. Otherwise all six fields are
reconstructed together before the serial copy. Every copied
field must have a `nonuniform List` internal field and contain no `$` macro after the
boundary rewrite, or preparation restores the cold fields and writes `COLD_FALLBACK`.
Geometry
hops use `mapFieldsPar -consistent -mapMethod cellVolumeWeight` and equal rank counts.

A1 accepts a cleanly ended source when the convergence audit says `settled` or
`extrapolable`, or when the damped-fit asymptote and latest full-cycle total mean
agree within 2%. The verdict states which criterion passed. A3 strips comments and
the `FoamFile` header and compares parsed dictionary entries, so formatting-only
changes to `fvSchemes` or `fvSolution` do not refuse a hop; a real mismatch reports
its first differing entry.

`--source-settled-override "REASON"` is an explicit operator exception for A1.
Plans render A1 as `OVERRIDDEN`; preparations/runs record the reason in markers,
the YAML history, and the TSV ledger. It never overrides another admissibility
gate, and execution requires `--calibrate` even if historical expected value would
otherwise permit a warm start.

A7 compares normalized mesh classes, not merely raw labels. Explicit source provenance is
used first; `--source-mesh-level` overrides it for an older source. If no source level is
recorded, the finest value in `case_provenance.json` `refinement.levels` maps to 80-class
at 60 or more cells per wavelength, 40-class at 30 through below 60, and 20-class below
30. As a final legacy fallback, source and target meshes within 10% in cell count are
treated as the target class. Labels `r3`/`L3`/`80`, `r2`/`L2`/`40`, and
`r1`/`L1`/`20` normalize to those three classes.

For `plan --dry-run` only, a staged target without `constant/polyMesh` (including a
mesh-store link not yet installed) reports A6 as `PENDING` and defers the identity check;
it is not a refusal. Once both meshes exist, A6 prints both identity hashes and any
mesh-store link targets. Prepare and run remain fail-closed when a target mesh is absent.

Layer 2 uses a Beta(2,2) success prior. Default saving fractions are 40% for geometry,
25% for speed, and 15% for potential or analytic starts. The default abort point is
`floor((N_cold/3)/400)*400`, and the EV margin is 10% of `N_cold`. A first hop is therefore
refused unless `--calibrate` buys the single bounded calibration allowed per hop type and
level. A calibration is spent only by a terminal `WARM_OK`, `WARM_ABORTED`, or
`WARM_FAILED_CAP` result. Invalid and `NOT_ATTEMPTED` entries affect neither this guard nor
the Beta posterior.

Layer 3 evaluates R1--R6 at every 400-iteration write: pressure excursion, viscous level,
force asymptotes, mass/solver health, POWER GATE success, and the `N_cold` cap. POWER GATE
means total force changes by less than 1% between two 400-iteration windows and pressure
wobble is below 2% of total. With `--act`, an abort requests `stopAt writeNow`, waits using
the known PID's `/proc/<pid>/cwd`, writes `WARM_ABORTED`, archives only logs and force
output, restores `0.cold`, and launches `--relaunch` when supplied. Without `--act`, check
is read-only.

## Markers and scheduling

Preparation and execution use `WARM_PLANNED` and `WARM_RUNNING`. Terminal markers are
`WARM_OK`, `WARM_ABORTED`, `WARM_FAILED_CAP`, and `COLD_FALLBACK`. Events append to the
campaign `warm_start.tsv`; per-level/type Beta histories are YAML under `warm_start/`.

After verification, `run` writes `WARM_FIELDS` beside `0/`, recording the resolved source,
time, and SHA-256 of every installed field. A relaunch script must mention `WARM_FIELDS`
or guard `internalField nonuniform`; otherwise `run` exits 3 with
`relaunch script resets 0/ from 0.orig; use a warm-aware chain`. The reference
`scripts/cfd/solve_chain_warm_aware.sh CASE COMMAND [ARG ...]` keeps `0/`, skips
`setFields`, and touches `WARM_FIELDS_KEPT` when both the marker and nonuniform `0/U` are
present; otherwise it follows the cold `0.orig`/`setFields` path.

Calibration mode never launches a cold fallback: it writes `COLD_FALLBACK` with the
reason and exits 3 so the lane agent can decide. Production refusals exit non-zero and may
launch the requested cold chain. Cold restoration also forces `stopAt endTime`, preventing
a stale `writeNow` request from ending the fallback after one iteration.

The matrix scheduler must call the checkpoint command after every solver write, not only
at the nominal abort point:

```bash
scripts/cfd/warm_start.py check --target "$case" --mesh-level "$level" --act --pid "$solver_pid" --relaunch "$cold_command"
```

The command prints exactly one `CONTINUE|ABORT|OK reason` verdict line. Exit 0 means
CONTINUE or OK, 3 means ABORT, and 2 means missing reference input or another usage/I/O
failure. Plans retain exit 3 for cold by admissibility and exit 4 for cold by EV.
Production `run` refusals exit 2 after launching `--relaunch` when supplied; calibration
refusals exit 3 without launching it.
