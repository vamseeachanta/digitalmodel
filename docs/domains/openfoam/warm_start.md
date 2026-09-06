# Safe warm starts for calm-water resistance

`scripts/cfd/warm_start.py` prepares and supervises OpenFOAM v2312 warm starts. It is
fail-closed: a case is touched only after all admissibility checks pass and the expected
iteration saving clears the configured margin. Campaign and lane identity remain external:
set `DM_CFD_CAMPAIGN` and `DM_CFD_LANES_FILE`; do not put deployment identifiers in cases.

## Usage

The concise forms are:

```bash
scripts/cfd/warm_start.py --from speed SOURCE --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py --from geometry SOURCE --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py --from potential --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py --from analytic --eta eta.csv --u velocity.csv --target CASE --mesh-level L2 --calibrate
scripts/cfd/warm_start.py check --target CASE --mesh-level L2
scripts/cfd/warm_start.py record --record CAMPAIGN/warm_start
```

The section-8 spelling is also supported: `prepare --from case --hop speed|geometry
--source SOURCE`. Use `plan` for a read-only verdict, `prepare` to create fields, and `run`
to prepare and launch the case's `solve_chain.sh`. `--dry-run` prints the verdict,
expected-value calculation, and exact commands. `--relaunch COMMAND` overrides the case
driver. Reference data live in `warm_start/level_<tag>.yml`; `--n-cold` may supply the
level budget explicitly.

## The three safety layers

Layer 1 checks source convergence and a clean solver exit, wall treatment, numerics,
reference frame, decomposition/reconstruction, hop type, speed difference, ladder level,
mapped alpha mass, rewritten speed fields, `pcorr`, and level references. Speed hops are
limited to `|dU/U| <= 10%`. Geometry hops require equal speed and different meshes.
Only `alpha.water U p_rgh k omega nut` transfer. `phi`, `alphaPhi0.water`, `rDeltaT`, `p`,
and `uniform/` are removed. Boundary values for inlet and outlet `U`, inlet `k`, and inlet
`omega` come from the target and are applied with `changeDictionary -time 0`. Geometry
hops use `mapFieldsPar -consistent -mapMethod cellVolumeWeight` and equal rank counts.

Layer 2 uses a Beta(2,2) success prior. Default saving fractions are 40% for geometry,
25% for speed, and 15% for potential or analytic starts. The default abort point is
`floor((N_cold/3)/400)*400`, and the EV margin is 10% of `N_cold`. A first hop is therefore
refused unless `--calibrate` buys the single bounded calibration allowed per hop type and
level.

Layer 3 evaluates R1--R6 at every 400-iteration write: pressure excursion, viscous level,
force asymptotes, mass/solver health, POWER GATE success, and the `N_cold` cap. POWER GATE
means total force changes by less than 1% between two 400-iteration windows and pressure
wobble is below 2% of total. An early abort requests `stopAt writeNow`, waits using the
known PID's `/proc/<pid>/cwd`, archives only logs and force output, restores `0.cold`, and
launches the normal cold chain.

## Markers and scheduling

Preparation and execution use `WARM_PLANNED` and `WARM_RUNNING`. Terminal markers are
`WARM_OK`, `WARM_ABORTED`, `WARM_FAILED_CAP`, and `COLD_FALLBACK`. Events append to the
campaign `warm_start.tsv`; per-level/type Beta histories are YAML under `warm_start/`.

The matrix scheduler must call the checkpoint command after every solver write, not only
at the nominal abort point:

```bash
scripts/cfd/warm_start.py check --target "$case" --mesh-level "$level" --fallback --pid "$solver_pid"
```

Exit 0 means CONTINUE or OK, 3 is cold by admissibility, 4 is cold by EV, 5 means the warm
attempt aborted and cold fallback launched, and 2 is a usage or I/O failure.
