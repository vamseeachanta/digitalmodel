# Resonant-roll top-vent assessment

The static placement study does not establish that the top vent retains liquid
during rolling run-up. The study driver can now preserve the prescribed motion,
select the highest-fill near-resonant rows, and assess initialized-volume
conservation and measured free-surface clearance. Rolling solver evidence is
**NOT_EVALUATED**: OpenFOAM is unavailable in the implementation environment.
The coupled matrix remains gated by this evidence and the exchange-path work
in #1978. Refs #1980 and #1528.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-19 | Add motion-enabled preparation and evidence assessment because a static hold does not establish run-up clearance. |
| 2 | 2026-10-25 | Retain liquid boundary settings in input digests and preserve completed variants during subset reduction to prevent reuse of mismatched evidence or loss of other records. |

## Scope and inputs

Slice 1 adds the zero-flow rolling hold to
`scripts/cfd/run_outlet_vent_study.py`. Static mode remains the default. Resonant
mode retains the mapper's prescribed motion, requires a cycle count and maximum
sampling gap, and rejects imposed flow so volume change isolates leakage and
numerical conservation error. Those duration and sampling choices must come
from the run plan; no default establishes sufficient resonant run-up duration.

Slice 2 adds `vent_runup.assess_vent_hold` and a geometric freeboard screen.
`worst_fill_resonant_cases` retains **all** near-resonant rows at maximum fill.
The driver selects exact period ratio unity first, then lowest conduit capacity,
then stable case ID. The complete candidate list is retained in the manifest:
one selection does not establish a worst dynamic response across that list.

The existing synthetic study and `SweepConfig` supply the geometry and motion
inputs (not measured geometry): tank length 20.0 m, width 6.0 m, height 10.0 m,
prescribed amplitude 5.0 degrees. The maximum fill is 80.0 %, giving nominal
depth 8.0 m, initialized inventory 960.0 m3 and nominal freeboard 2.0 m.
These inputs are defined in the placement study's `_coupling_spec` and
`_study_sweep`; the fill levels align with the existing vertical cell faces.
Conduit coefficients are inherited metadata and are inactive in the zero-flow
hold. They are not a new engineering basis for exchange calculations.

Slice 3 adds a case-matched run-up evidence contract, a remedy recommendation,
and the worked example below. Actual rolling CFD, interface extraction and
mesh/time-step sensitivity remain later execution work. No solver result is
inferred from the examples or the dry-run build.

## Conservation and geometry

The following are definitions and independent geometric derivations, with no
empirical amplification factor:

| Quantity | Method | Basis |
|---|---|---|
| Liquid volume | `V(t) = integral(alpha.water dV)` | OpenFOAM v2312, `volFieldValue`, `volIntegrate` operation [1] |
| Signed deviation | `d(t) = 100*(V(t)-V0)/V0` in percent | Relative change definition; percent is a fraction multiplied by 100 |
| Volume check | `max(abs(d(t))) <= tolerance_pct` | Existing `ExtractionConfig.mass_balance_rtol` in `time_history.py`; the study converts it to percent |
| Nominal depth | `h = F*H` | Rectangular prism: `V0 = F*L*W*H = L*W*h`, cancelling plan area |
| Nominal freeboard | `b = H-h` | Difference of roof and nominal free-surface elevations |
| Peak run-up | `eta_max = z_max-h` | Definition of excursion above nominal level, in tank coordinates |
| Remaining headroom | `c = H-z_max = b-eta_max` | Difference of roof and measured spatial-maximum elevations |
| Required duration | `t_required = N*T_roll` | Definition of cycle count and period |

The volume reference is initialized `V0`, rather than the first written sample.
Both endpoint drift and the maximum absolute excursion are retained; a recovered
endpoint does not erase an earlier volume loss. Maximum loss is reported as well.
The existing tolerance is 0.1 %; it is a repository conservation criterion,
not a sourced design allowance for intentional vent loss. Overrides are recorded.

Coverage requires at least two strictly ordered finite samples, reaching the
requested end time, with no gap larger than the required input (including the
gap from initialization to the first written sample). Short, sparse or absent
histories yield `NOT_EVALUATED`. Duration alone does not prove that resonant
response has saturated; the run plan must assess successive-cycle growth.

Clearance passes only for `c > 0`. Touching the roof fails the geometric check;
no safety margin is assumed. This clearance criterion is geometric and does
not qualify an impact pressure or a structural design.

## Preparation and evidence contract

Use an output directory outside the repository. The numbers in this command
are synthetic demonstration choices (two cycles, one-second maximum sampling
gap, one rank), not a qualified run duration or sampling criterion:

```bash
PYTHONPATH=src python scripts/cfd/run_outlet_vent_study.py \
  --work-root "$OUTPUT_DIR/cases" --output "$OUTPUT_DIR/build.json" \
  --motion resonant --sweep-case worst-fill-resonant \
  --cycles 2 --max-sample-gap-s 1 --ranks 1 --variants vent_top --dry-run
```

The build manifest records motion, geometry, selected matrix row, candidate
rows, requested duration and explicit `NOT_EVALUATED` checks. Removing
`--dry-run` requires an already available OpenFOAM v2312 environment. A real
run writes its volume series through `liquidVolume` and the manifest records
the initialized inventory. Fatal solver markers or a nonzero solver exit are
errors. A real resonant run returns status 2 until all case checks pass; a
dry-run returns status 0 for a successful build without claiming a CFD pass.

After extracting the interface evidence, repeat the same command with
`--reduce-only --runup-evidence "$OUTPUT_DIR/evidence.json"` and the original
`--output` manifest; omit `--dry-run`. Offline reduction reads the completed
volume history without rebuilding or starting OpenFOAM. Recorded input and
history digests must match. The liquid-field digest retains dimensions, field
type and boundary conditions, including `inletValue`. Initialization-generated
`internalField` values and field-header location metadata are omitted; comments
and whitespace are normalized. Generated `constant/polyMesh` files are excluded.
The setFields specification, other boundary fields and motion dictionary remain
included. Older digests that omitted the liquid boundary settings cannot qualify
completed histories under this check; they are rejected without changing the
manifest. A completed hold is not run again using the same output manifest.
Each completed variant is recorded before moving to the next variant.

Reduction of selected variants preserves the unselected completed records,
including their original digests and assessment criteria, for later reuse. The
command exit status applies to the selected variants; preserved records retain
their own assessment status. No case pass releases the coupled matrix.

To assess headroom, provide `--runup-evidence evidence.json` with this schema
(shown with synthetic worked-example values, not an actual matrix measurement):

```json
{
  "case_id": "synthetic",
  "variant": "vent_top",
  "frame": "tank",
  "start_time_s": 0.0,
  "end_time_s": 4.0,
  "peak_elevation_m": 9.5,
  "source": "Synthetic worked example; replace with extraction method and uncertainty"
}
```

For a real run replace the identity and time interval with the selected case
and complete rolling observation interval. `peak_elevation_m` must be the
maximum over the **whole** interface and interval in the **tank frame**,
including the initialized surface. A fixed global-height trace or a few probes
cannot establish this maximum. Record the interface definition, extraction
method, spatial and temporal resolution, and numerical uncertainty in `source`.
Wrong case, variant, frame, time coverage or absent source is rejected. Missing
evidence remains `NOT_EVALUATED`; no static-tilt estimate substitutes for it.

## Worked example: inputs, method and answer

This independent synthetic example uses a rectangular tank 5.0 by 2.5 by
10.0 m at fill 80.0 %: `V0 = 5*2.5*10*0.8 = 100.0 m3`. The prescribed period
is 2.0 s and the observation is two cycles. Assume one volume sample per second
from initialization through 4.0 s, all at 100.0 m3. The supplied synthetic
spatial-maximum elevation is 9.5 m in tank coordinates, covering that interval.
The example acceptance tolerance is 0.1 %, matching the existing repository
criterion. The one-second sampling gap is an explicit example input.

```python
from digitalmodel.solvers.openfoam.vent_runup import assess_vent_hold

result = assess_vent_hold(
    [(t, 100.0) for t in range(5)],
    case_id="synthetic", initial_volume_m3=100.0,
    tank_height_m=10.0, fill_fraction=0.8,
    roll_period_s=2.0, cycles=2, tolerance_pct=0.1, max_sample_gap_s=1.0,
    runup_evidence={
        "case_id": "synthetic", "variant": "vent_top", "frame": "tank",
        "start_time_s": 0.0, "end_time_s": 4.0, "peak_elevation_m": 9.5,
        "source": "Synthetic hand calculation in this worked example",
    },
)
assert result["case_status"] == "PASS"
assert result["remaining_headroom_m"] == 0.5
assert result["matrix_status"] == "NOT_EVALUATED"
```

Hand calculation: `h = 0.8*10 = 8.0 m`, `b = 10-8 = 2.0 m`,
`eta_max = 9.5-8 = 1.5 m`, `c = 10-9.5 = 0.5 m`, and volume deviation is
0.0 %. This case passes the specified checks. For a second history
`100, 98, 100, 100, 100 m3`, endpoint drift is still 0.0 % but the maximum
loss is `100*(100-98)/100 = 2.0 %`; conservation fails. For `z_max = 10.0 m`,
clearance is 0.0 m and headroom fails even if the inventory is constant.
These calculations are repeated independently in `test_vent_runup.py`.

## Remedy and remaining execution

If the interface reaches the top, the assessment recommends
`raise_domain_and_repeat`: move the atmospheric boundary above the measured
run-up envelope, with additional clearance specified by the run plan, then
repeat the rolling hold and sensitivity checks. This avoids treating an
`alpha.water` boundary edit as a physical liquid barrier. The existing
`inletOutlet` condition extrapolates the interior field on outflow [2]; an air
inflow value does not prevent liquid outflow. No geometry or boundary condition
is changed automatically. No intentional loss allowance has been supplied, so
accepting and quantifying a permitted loss remains a separate decision.

Before launching the matrix:

1. Confirm the motion axis and resonance basis. The existing sweep computes a
   longitudinal period using tank length; `MotionType.ROLL` rotates about x and
   excites the transverse dimension. Its near-resonance label alone does not
   establish physical roll resonance. Resolve the axis/period consistency and
   measure the response peak before claiming a resonant validation.
2. Run a motion-enabled zero-flow top-vent hold and preserve advancing solver
   times, a successful end, case-input hashes, full volume history and tank-frame
   spatial-maximum interface extraction. Demonstrate sufficient run-up duration
   and refine the mesh/time step. Neither is available here.
3. Evaluate every highest-fill near-resonant candidate and other response peaks
   as required. A freeboard screen does not bound resonant amplification.
4. If wetting occurs, apply and retest the remedy. Review #1978's drain/overflow
   precondition separately. Record the measured outcome against #1528.

Even a passing case retains `matrix_status = NOT_EVALUATED`. A no-flow hold
does not establish the behaviour of the coupled filling/draining matrix.

## Sources

1. OpenCFD, OpenFOAM v2312 documentation (2023),
   [volFieldValue, volume integral operation](https://doc.openfoam.com/2312/tools/post-processing/function-objects/field/volFieldValue/).
   Opened 2026-10-19; HTTP 200.
2. OpenCFD, OpenFOAM v2312 documentation (2023),
   [inletOutlet, flux-sign behaviour](https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/inletOutlet/inletOutlet/).
   Opened 2026-10-19; HTTP 200.
3. Repository `time_history.ExtractionConfig.mass_balance_rtol`,
   `sloshing_sweep.SweepConfig`, `motion.MotionType`, and the existing static
   placement-study report in `docs/reports/issue-1528-outlet-vent/` provide the
   existing tolerance, synthetic matrix inputs, axes and static-only scope.
   Static evidence is not reused as a rolling measurement.
