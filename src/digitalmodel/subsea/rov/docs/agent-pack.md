# ROV module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/rov` |
| Code pin | `09089a121a16069c4b657d1a320a630a23ab1dd5` |
| Status | DRAFT; software screening behavior, engineering qualification incomplete |
| Analysis family | EC |
| Import | `digitalmodel.subsea.rov` |

Summary: caller-supplied interface geometry, tool capabilities, sea-state
operability and serial intervention windows are screened. A compatibility
Boolean establishes only the encoded geometric conditions. Installed manipulator
qualification, environmental suitability and standards compliance are not
established by this pack.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-28 | Document public contracts, synthetic reach arithmetic and consistency checks so screening results can be reproduced with explicit qualification limits. |
| 1 | 2026-10-28 | Declare the YAML test dependency, refresh execution counts and map the source sweep to detection checks and remaining gaps. |

## 1. Purpose and scope

This pack covers every Python source file in the ROV package: the initializer,
`interface_checks.py`, `tool_library.py`, `seastate_limits.py` and
`intervention_sequence.py`. The initializer re-exports interface, tool and
planning APIs and the Hs result/functions. The RAO result/function remain
submodule APIs. Imported names and enum constants are not additional AST
function/class definitions; generated dataclass methods and private helpers are
not public definitions under the existing checker contract.
[src/digitalmodel/subsea/rov/__init__.py:L29-L61]
[scripts/docs/check_agent_pack_functions.py:L76-L120]

Interface geometry is a fixed-orientation box and spherical reach screen.
Capacity lookup and weather planning are separate calls, with no automatic
connection between equipment labels and interface/capacity results. Tree and
connector knowledge packs remain outside this scope.
[src/digitalmodel/subsea/rov/interface_checks.py:L4-L16]
[src/digitalmodel/subsea/rov/tool_library.py:L4-L13]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L175-L239]

## 2. Inputs and outputs

All lengths are metres unless stated otherwise. Units are conventions, not
runtime unit types. Dataclass construction stores values without validation;
function entry points perform the checks described below. Public definitions
include public class members across every package source file. Each inventory
row cites the definition and is checked by the existing AST checker.

| Public symbol | Inputs, units and validation | Output convention | Evidence |
|---|---|---|---|
| `InterfaceKind` | String enum: torque, hot_stab, override, paddle, docking, parking; no geometry or ratings | Interface family label | [src/digitalmodel/subsea/rov/interface_checks.py:L25-L33] |
| `ToolInterface` | Required `kind`, `class_id`, `envelope_m`, `required_approach_m`; dimensions and approach in m | Frozen tool demand; checked when consumed | [src/digitalmodel/subsea/rov/interface_checks.py:L37-L43] |
| `InterfaceSpec` | Required `kind`, `class_id`, `envelope_m`, `approach_clearance_m`, base-relative signed `location_m`, in m | Frozen available target geometry; checked when consumed | [src/digitalmodel/subsea/rov/interface_checks.py:L47-L54] |
| `InterfaceResult` | Required `compatible` bool and `reasons` tuple of strings | Frozen screen result, empty reasons on compatibility | [src/digitalmodel/subsea/rov/interface_checks.py:L58-L62] |
| `check_interface` | Tool, target and required `manipulator_reach_m`; enum family, nonempty class, positive finite three-axis boxes, finite nonnegative approach/reach and finite three-coordinate target; torque labels restricted by source | `InterfaceResult`; every failure retained in kind, class, envelope, approach, reach order; invalid domain raises `ValueError` | [src/digitalmodel/subsea/rov/interface_checks.py:L86-L122] |
| `ToolKind` | String enum: torque, hot_stab, pull, cutting, cleaning | Function label for capability filtering | [src/digitalmodel/subsea/rov/tool_library.py:L25-L32] |
| `TaskDuty` | Required `kind`; optional torque Nm, turns, flow m³/s, pressure Pa, line pull N default to `None`; supplied capacities finite and nonnegative when consumed | Frozen required duty; `None` means no requirement for that field | [src/digitalmodel/subsea/rov/tool_library.py:L36-L44] |
| `ToolSpec` | Required name, kind and interface; same optional capacity fields/units/defaults as duty | Frozen candidate; `None` means unknown capacity, not unlimited | [src/digitalmodel/subsea/rov/tool_library.py:L48-L58] |
| `find_tools` | Iterable of tools and duty; validates every candidate's capacities, interface demands and nonempty name, even if kind mismatches | Ordered tuple meeting all specified capacities; empty tuple if none; invalid supplied domain raises `ValueError`; geometry is checked separately | [src/digitalmodel/subsea/rov/tool_library.py:L84-L106] |
| `SeaStateResult` | Required `hs`, `hs_oplim`, `margin` m, dimensionless `utilisation`, `operable` bool | Frozen Hs result; positive margin is remaining height allowance | [src/digitalmodel/subsea/rov/seastate_limits.py:L64-L85] |
| `seastate_operability` | `hs` m nonnegative, `hs_limit` m positive, `alpha=1.0` dimensionless in (0,1]; comparison checks only, no general finite check | `SeaStateResult`; operational limit alpha times rated limit, inclusive comparison; invalid checked domains raise `ValueError` | [src/digitalmodel/subsea/rov/seastate_limits.py:L88-L133] |
| `OperabilityResult` | Required `operability_pct` percent, `hs_oplim` m, `n_operable`, `n_total` cell counts | Frozen weighted percentage plus unweighted counts | [src/digitalmodel/subsea/rov/seastate_limits.py:L137-L155] |
| `scatter_operability` | Nonempty sequence of mappings with `hs` m and `probability` nonnegative weight; positive total weight; positive `hs_limit` m; `alpha=1.0` in (0,1]; no general finite check | `OperabilityResult`; normalizes arbitrary weights; missing mapping keys raise `KeyError`, invalid checked domains raise `ValueError` | [src/digitalmodel/subsea/rov/seastate_limits.py:L158-L222] |
| `RaoOperabilityResult` | Required significant single-amplitude `v_significant`, `v_oplim`, `margin` m/s, dimensionless `utilisation`, `operable` bool | Frozen response screen; velocity amplitude convention must match caller limit | [src/digitalmodel/subsea/rov/seastate_limits.py:L226-L248] |
| `rao_based_operability` | Forecast and RAO provider; required positive `velocity_limit` m/s; keyword `deployment_offset=(0.0,0.0,0.0)` m, `alpha=1.0` in (0,1], `dt=0.2` s; finite/shape/time checks not comprehensive here | `RaoOperabilityResult` from reconstructed vertical-velocity variability; imports NumPy and motion_forecast lazily; dependency errors propagate | [src/digitalmodel/subsea/rov/seastate_limits.py:L251-L318] |
| `InterventionTask` | Required task/equipment/interface labels, `duration_steps`, `weather_window_steps`; `dependencies=()`; counts and labels validated by factory/planner | Frozen task; identifiers are labels, not validated asset/interface connections | [src/digitalmodel/subsea/rov/intervention_sequence.py:L40-L78] |
| `InterventionTask.from_operation_periods` | Required labels, keyword positive integer `planned_steps`, nonnegative integer `contingency_steps`; excludes bool; `dependencies=()`; validates labels and duplicate dependencies | Task duration equals planned count; weather cover equals planned plus contingency; graph resolution remains in planner | [src/digitalmodel/subsea/rov/intervention_sequence.py:L51-L78] |
| `ScheduledTask` | Required labels, `start_hours`, `end_hours` h from forecast origin | Frozen scheduled task; execution end excludes contingency cover | [src/digitalmodel/subsea/rov/intervention_sequence.py:L82-L89] |
| `InterventionPlan` | Required schedule tuple, unscheduled `(task_id, reason)` tuple, complete bool and optional campaign metrics | Frozen plan; duration h and waiting fraction are `None` when incomplete, not zero or a successful verdict | [src/digitalmodel/subsea/rov/intervention_sequence.py:L93-L100] |
| `plan_intervention` | Task sequence, Hs interval sequence m, positive finite `hs_limit` m; required keyword positive finite `step_hours` h; `alpha=1.0` in (0,1]; positive integer duration/window counts, window at least duration; unique task IDs and resolvable acyclic dependencies | `InterventionPlan`; stable dependency order and earliest serial windows; failures `no_weather_window` and `dependency_unavailable`; invalid graph/count/weather domains raise `ValueError` | [src/digitalmodel/subsea/rov/intervention_sequence.py:L175-L239] |

The literal defaults above cite software definitions, not qualified equipment
ratings or recommended forecast allowances. Torque class identifiers are labels,
not capacity ratings. No manufacturer catalogue is embedded.
[src/digitalmodel/subsea/rov/interface_checks.py:L73-L79]
[src/digitalmodel/subsea/rov/tool_library.py:L4-L13]

## 3. Method and governing sources

Source basis C is the pinned package implementation. Its claims are described as
software behavior; they do not independently verify a standard. Source basis S
in the next section is entirely synthetic and supplies every dimensional
constant in the worked example. Section and source line numbers are locators,
not engineering constants.

- Geometry C: each tool box dimension must fit the corresponding target axis;
  required approach must not exceed available approach; the Euclidean norm of
  the base-relative target coordinates must not exceed caller-supplied reach.
  Equality passes. Class and family must match. The axes remain aligned in a
  fixed mating frame. [src/digitalmodel/subsea/rov/interface_checks.py:L109-L122]
- Capabilities C: every requested field requires a known rating at least equal
  to the duty. Matching tools retain input order, without certifying geometry.
  [src/digitalmodel/subsea/rov/tool_library.py:L72-L106]
- Weather C: operational Hs limit equals alpha times rated Hs limit;
  utilisation is Hs divided by operational limit, margin is their difference.
  Scatter percentage is one hundred times accepted weight divided by total
  weight, including zero-weight cells in the cell counts.
  [src/digitalmodel/subsea/rov/seastate_limits.py:L122-L133]
  [src/digitalmodel/subsea/rov/seastate_limits.py:L190-L222]
- Response C: vertical motion at the deployment offset is differentiated;
  significant single velocity amplitude is twice NumPy's population standard
  deviation. It is compared inclusively with the alpha-adjusted velocity limit.
  This does not calculate contact or snatch loads.
  [src/digitalmodel/subsea/rov/seastate_limits.py:L300-L318]
- Planning C: each Hs sample covers a full uniform interval. Execution occupies
  the task duration; contingency reserves forecast cover without occupying the
  resource. Complete-campaign waiting fraction is elapsed minus working time,
  divided by elapsed time, including initial idle time and excluding unused
  trailing forecast. The empty plan has zero duration and waiting fraction;
  incomplete plans retain independent tasks and withhold campaign metrics.
  [src/digitalmodel/subsea/rov/intervention_sequence.py:L13-L24]
  [src/digitalmodel/subsea/rov/intervention_sequence.py:L202-L239]

API 17H, IMCA R 004 / R 015 and DNV-ST-N001 are references named in package
source. Editions, official clause contents, installed-equipment compliance and
forecast allowance qualification are NOT_EVALUATED here. No licensed standard
values are reproduced or used as fixture input. The planner's cited edition and
clause verification remain explicitly deferred in its source.
[src/digitalmodel/subsea/rov/__init__.py:L4-L26]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L4-L11]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L26-L30]

## 4. Worked examples and regression examples

Synthetic basis S: every length below is synthetic, not a standard dimension,
vendor rating or field observation. Tool and target have matching docking family
and `synthetic` class, equal envelopes (0.2, 0.3, 0.4) m and adequate approach:
required 0.5 m is below available 0.6 m. The target is (3, 4, 0) m relative to the
manipulator base. Independent Pythagoras gives distance
sqrt(3² + 4² + 0²) = sqrt(9 + 16) = sqrt(25) = 5 m.
Thus reach 5 m passes at equality, reach 6 m passes, and reach 4 m fails only
`reach`. For a separate envelope case, only the tool's first dimension becomes
0.3 m, exceeding the target's 0.2 m; reach remains 6 m and only `envelope` fails.
These comparisons supply the expected results independently of the code.

```python
from dataclasses import replace
from digitalmodel.subsea.rov import (
    InterfaceKind, InterfaceSpec, ToolInterface, check_interface,
)

# Every length is synthetic basis S in section 4, including reach limits.
tool = ToolInterface(InterfaceKind.DOCKING, "synthetic", (0.2, 0.3, 0.4), 0.5)
target = InterfaceSpec(
    InterfaceKind.DOCKING, "synthetic", (0.2, 0.3, 0.4), 0.6, (3.0, 4.0, 0.0),
)
at_boundary = check_interface(tool, target, manipulator_reach_m=5.0)
beyond_boundary = check_interface(tool, target, manipulator_reach_m=6.0)
short_reach = check_interface(tool, target, manipulator_reach_m=4.0)
oversize = check_interface(
    replace(tool, envelope_m=(0.3, 0.3, 0.4)), target, manipulator_reach_m=6.0,
)
assert at_boundary.compatible is True and at_boundary.reasons == ()
assert beyond_boundary.compatible is True and beyond_boundary.reasons == ()
assert short_reach.compatible is False and short_reach.reasons == ("reach",)
assert oversize.compatible is False and oversize.reasons == ("envelope",)
```

The Markdown block itself is executed by
`test_documented_example_matches_independent_calculation`; its Booleans and exact
reason tuples are also checked outside the example. Orientation assessment,
obstacles, contact mechanics, currents and actual manipulator qualification are
NOT_EVALUATED. The assumed fixed orientation does not establish a reachable
manipulator pose or a collision-free approach.

## 5. QA gates and executable checks

Run the required roots with `PYTHONPATH=src uv run --no-project --with pytest
--with pyyaml python -m pytest tests/subsea/rov tests/docs/test_rov_agent_pack.py
tests/docs/test_agent_pack_functions.py tests/test_main_module_routing.py -q -rA
-p no:cacheprovider`, directing `--basetemp`, logs and optional JUnit output to
configured scratch. Run the unchanged checker separately:
`uv run --no-project python scripts/docs/check_agent_pack_functions.py --pack
src/digitalmodel/subsea/rov/docs/agent-pack.md`.

The package-specific tests check the real inventory, numbered structure and
registry pointer. A clean copied synthetic package is checked before separate
added-definition and stale-definition probes. Each drift probe requires the
unchanged checker CLI to return failure with the exact directional diagnostic.
Negative calibration never edits the real source tree. Expected example results
come from synthetic basis S, not from a second call to the implementation.
[tests/docs/test_rov_agent_pack.py]

Observed counts at the fix-up base and after the documentation change, under the
commands above; `PYTHONUTF8` is unset:

| Test root | Before collected / passed / failed / skipped | After collected / passed / failed / skipped |
|---|---|---|
| `tests/subsea/rov` | 98 / 92 / 6 / 0 | 98 / 92 / 6 / 0 |
| `tests/docs/test_agent_pack_functions.py` | 19 / 19 / 0 / 0 | 19 / 19 / 0 / 0 |
| `tests/test_main_module_routing.py` | 9 / 4 / 5 / 0 | 9 / 4 / 5 / 0 |
| `tests/docs/test_rov_agent_pack.py` | 5 / 5 / 0 / 0 | 5 / 5 / 0 / 0 |

Counts are software execution observations, not physical acceptance limits.
The baseline response tests encounter missing `pydantic` through
`motion_forecast`; routing tests encounter absent distribution metadata or
missing `pandas` through `run_contract`. The affected files are unchanged and
these call paths are present on main. Response execution and complete CLI
routing are therefore not established in this environment.
[src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L33-L33]
[src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L12-L12]
[src/digitalmodel/__main__.py:L47-L68]
[src/digitalmodel/run_contract.py:L41-L46]

Supplying the declared runtime dependencies additionally runs all 98 ROV tests
successfully, including the six response cases. This separate command is:

```sh
PYTHONPATH=src uv run --no-project --with pytest --with pyyaml \
  --with 'numpy<2' --with 'pandas<3' --with 'pydantic>=2.7,<3' \
  --with 'pydantic-settings<3' --with 'scipy<2' \
  python -m pytest tests/subsea/rov -q -p no:cacheprovider
```

The following existing checks are the detection references for section 6. A
check demonstrates only its asserted arithmetic or branch, not unit conversion,
forecast qualification or comprehensive non-finite-input rejection.

| Check | Executable test node |
|---|---|
| G1 | `tests/subsea/rov/test_interface_checks.py::test_matching_interface_at_reach_boundary` |
| G2 | `tests/subsea/rov/test_interface_checks.py::test_signed_coordinates_and_origin_are_valid` |
| C1 | `tests/subsea/rov/test_tool_library.py::test_no_tool_can_satisfy_duty` |
| C2 | `tests/subsea/rov/test_tool_library.py::test_missing_capacity_does_not_mean_unlimited` |
| C3 | `tests/subsea/rov/test_tool_library.py::test_other_tool_types` |
| H1 | `tests/subsea/rov/test_seastate_limits.py::test_alpha_factor_derates_limit` |
| S1 | `tests/subsea/rov/test_seastate_limits.py::test_scatter_operability_percentage` |
| S2 | `tests/subsea/rov/test_seastate_limits.py::test_scatter_operability_alpha_excludes_more_cells` |
| R1 | `tests/subsea/rov/test_seastate_limits.py::test_rao_operability_operable_and_not` |
| R2 | `tests/subsea/rov/test_seastate_limits.py::test_rao_operability_lever_arm_increases_velocity` |
| P1 | `tests/subsea/rov/test_intervention_sequence.py::test_weather_wait_dependencies_and_fraction` |
| P2 | `tests/subsea/rov/test_intervention_sequence.py::test_no_window_is_explicit_and_blocks_dependents` |
| P3 | `tests/subsea/rov/test_intervention_sequence.py::test_longer_continuous_window_and_horizon_boundary` |
| P4 | `tests/subsea/rov/test_intervention_sequence.py::test_alpha_allowance_is_delegated` |
| P5 | `tests/subsea/rov/test_intervention_sequence.py::test_partial_plan_retains_independent_tasks_without_campaign_metrics` |
| P6 | `tests/subsea/rov/test_intervention_sequence.py::test_empty_horizon_and_empty_campaign` |

## 6. Known failure modes and pitfalls

sweep: 33 found

The sweep covers all five package Python files at the code pin in the header.
Candidate lines are obtained with the following command and inspected in their
containing functions, including adjacent defaults and return expressions:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|return \(\)|= 0([.]0)?\b|1970|int\(|float\(|alpha.*1[.]0|metre|metres|m/s|Nm|Pa|m³|cache' src/digitalmodel/subsea/rov --glob '*.py'
```

Each row identifies one behavior or assumption; related fields sharing a single
unit convention are grouped. Zero accumulators are identities, not hidden
engineering allowances. No swallowed exceptions, external-text numeric
conversions, epoch defaults or cache writes occur in this package. The one
`float()` conversion consumes a calculated NumPy scalar, not external text.
Open detection gaps remain under #3335; no production behavior changes here.

| Pinned location | Symptom or assumption | Detection in section 5 or remaining gap |
|---|---|---|
| `src/digitalmodel/subsea/rov/interface_checks.py:12` | Geometry and reach share metres in one fixed frame; mixed units remain indistinguishable | G1 checks synthetic geometry only; no detection check exists for mixed units (#3335) |
| `src/digitalmodel/subsea/rov/interface_checks.py:120` | Reach is measured from the zero manipulator-base origin | G2 checks signed coordinates and the origin |
| `src/digitalmodel/subsea/rov/interface_checks.py:108` | Empty reason collection means every encoded comparison passed, not qualified equipment | G1 checks the empty success tuple |
| `src/digitalmodel/subsea/rov/tool_library.py:36` | Duty capacities assume Nm, turns, m³/s, Pa and N; omitted fields impose no constraint | C3 checks kind-only duties; no detection check exists for mixed units (#3335) |
| `src/digitalmodel/subsea/rov/tool_library.py:48` | Candidate capacities use the same units; unknown ratings do not imply unlimited capacity | C2 rejects a requested unknown rating; no detection check exists for mixed units (#3335) |
| `src/digitalmodel/subsea/rov/tool_library.py:98` | Empty candidate input or no qualifying candidate yields an empty result tuple | C1 checks both empty input and no match |
| `src/digitalmodel/subsea/rov/seastate_limits.py:54` | Hs values and limits share metres; alpha is dimensionless and scatter weights share one scale | H1 and S1 check numeric examples; no detection check exists for mixed units (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:91` | Default alpha 1.0 leaves the Hs limit unchanged; allowance adequacy is external | H1 compares default and reduced limits |
| `src/digitalmodel/subsea/rov/seastate_limits.py:115` | Comparison-only Hs/limit checks can propagate NaN/Inf or misleading margins | No detection check exists for non-finite direct Hs inputs (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:161` | Scatter default alpha 1.0 retains the supplied Hs limit | S2 checks explicit reduction against S1's default case |
| `src/digitalmodel/subsea/rov/seastate_limits.py:194` | Operable weight begins at zero before accepted cells are accumulated | S1 checks weighted acceptance |
| `src/digitalmodel/subsea/rov/seastate_limits.py:195` | Total weight begins at zero and must become positive before division | S1 checks the weighted denominator |
| `src/digitalmodel/subsea/rov/seastate_limits.py:196` | Operable-cell count begins at zero and includes accepted zero-weight cells | S1 checks ordinary counts; no detection check exists for accepted zero-weight cells (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:197` | Total-cell count begins at zero and counts all records, independent of weight | S1 checks ordinary counts; no detection check exists for zero-weight counting (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:201` | Scatter heights, weights and limits lack comprehensive finite validation | No detection check exists for non-finite scatter inputs (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:255` | Default deployment offset is the zero reference point in metres | R1 exercises the default; R2 compares an explicit origin and lever arm |
| `src/digitalmodel/subsea/rov/seastate_limits.py:257` | Response default alpha 1.0 is an identity, not a qualified allowance | R1 checks default-limit decisions; no detection check exists for response alpha reduction (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:258` | Default dt 0.2 s assumes an adequate reconstruction interval | R1 exercises the default; no detection check exists for sampling adequacy (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:310` | `float()` converts calculated population variability, without parsing external text | R1 checks the resulting operability decisions |
| `src/digitalmodel/subsea/rov/seastate_limits.py:233` | Caller velocity limit must use the same significant single-amplitude m/s convention | R1 checks inequalities only; no detection check exists for a mismatched amplitude convention (#3335) |
| `src/digitalmodel/subsea/rov/seastate_limits.py:295` | Response limits, offsets and computed outputs lack comprehensive finite checks here | No detection check exists for non-finite response inputs or outputs (#3335) |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:14` | Samples are full uniform intervals; step_hours and output times use hours | P3 checks half-hour interval arithmetic; no detection check exists for mixed units (#3335) |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:167` | Window search begins with zero consecutive operable samples | P3 checks the required continuous window |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:169` | A failed sample resets the contiguous run to zero | P3 checks a broken window followed by a complete one |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:172` | No continuous window returns None, which the caller converts to an explicit failure | P2 checks no_weather_window and absent campaign metrics |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:182` | Planner default alpha 1.0 requires caller qualification | P4 checks allowance delegation |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:204` | Serial cursor begins at the forecast's zero time origin | P1 checks initial waiting and resulting times |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:205` | Working count begins at the empty-sum identity zero | P1 checks elapsed-minus-working arithmetic |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:212` | A task with an unavailable dependency is recorded and skipped | P2 checks dependency_unavailable explicitly |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:216` | A task without weather cover is recorded and skipped; independent work can continue | P5 checks independent work in a partial schedule |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:230` | Incomplete campaign duration is None, not a zero-duration success | P5 checks unavailable duration |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:232` | Empty-campaign waiting fraction is defined as zero, avoiding division by zero | P6 checks empty-campaign metrics |
| `src/digitalmodel/subsea/rov/intervention_sequence.py:238` | Incomplete campaign waiting fraction is withheld as None | P2 checks withheld waiting metrics |

A passed tool-capability filter does not imply a passed interface check. Unknown
ratings fail requested capacities; unspecified duty fields impose no capacity
constraint. Any malformed supplied candidate can abort selection, rather than
being silently omitted. Caller types outside the annotated contracts can raise
native Python type/attribute errors.
[src/digitalmodel/subsea/rov/tool_library.py:L64-L106]

The geometry check validates dimensions and signed finite target coordinates,
but does not rotate boxes, resolve obstacles or calculate loads. Its reasons
are accumulated in fixed order, not ranked by severity. Dataclass construction
alone does not validate geometry or establish qualified ratings.
[src/digitalmodel/subsea/rov/interface_checks.py:L65-L122]

Direct Hs/scatter and response limit checks use comparisons without comprehensive
finite validation. NaN/Inf can propagate or yield misleading margins and
Booleans. Scatter mappings require both documented keys. The planner separately
checks finite Hs, limit, alpha and interval values before delegation. Extreme
finite arithmetic and output finiteness are not comprehensively guarded.
[src/digitalmodel/subsea/rov/seastate_limits.py:L115-L126]
[src/digitalmodel/subsea/rov/seastate_limits.py:L185-L214]
[src/digitalmodel/subsea/rov/seastate_limits.py:L295-L318]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L146-L161]

Tasks are processed in stable dependency order with input-order ties. This is
serial scheduling, not resource optimization; an early task's window can delay
later independent work. A reserved weather window is longer than execution when
contingency is present; it need not prevent subsequent execution inside that
cover. Partial schedules are retained but their campaign metrics are `None`.
[src/digitalmodel/subsea/rov/intervention_sequence.py:L120-L143]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L209-L239]

## 7. Lessons log

Reach and envelope cases isolate independent failure categories. Equal envelopes
and sufficient approach leave only the hand-derived reach comparison active;
the separate oversized axis at adequate reach isolates envelope rejection.
Boundary equality is therefore distinguishable from an unintended approach,
family or class failure. This is arithmetic evidence, not a dimensional design
recommendation. [tests/docs/test_rov_agent_pack.py]

A documentation inventory can drift in either direction. Running the unchanged
AST checker on a clean copied synthetic fixture before the drift probes
separates checker failures from fixture setup failures. A passing inventory
only establishes definition/locator consistency, not correct physics or complete
engineering qualification. [scripts/docs/check_agent_pack_functions.py:L123-L171]

## 8. Ownership

The ROV package is the authoritative location for these software contracts;
`docs/registry/module-routing.yaml` routes to this pack. The maintainer resolves
qualification gaps and reviews changes to behavior. Caller-qualified interface
dimensions, tool ratings, weather limits and forecast allowances remain required
inputs; no catalogue or standards-based design authority is established here.

## 9. Dependencies and other implementations

The deterministic geometry, capability, Hs/scatter and sequence paths use the
standard library and local package imports. The planner calls
`seastate_limits.seastate_operability`; tool selection uses local interface
validation. The optional RAO path imports NumPy and `motion_forecast` only when
called, with that package's transitive dependencies. The AST checker imports no
ROV package. Tests use pytest and registry parsing uses PyYAML.
[src/digitalmodel/subsea/rov/tool_library.py:L16-L22]
[src/digitalmodel/subsea/rov/intervention_sequence.py:L30-L36]
[src/digitalmodel/subsea/rov/seastate_limits.py:L300-L309]
[scripts/docs/check_agent_pack_functions.py:L76-L91]

Public call routes are explicit imports from `digitalmodel.subsea.rov`, with
`rao_based_operability` and its result imported from `.seastate_limits`. The
shipped `examples/domains/subsea/rov_tool_selection.py` combines `find_tools`
with `check_interface`. A source/example search found no other cross-package
ROV imports beyond the package and this example; no dedicated ROV engine/YAML
route or solver qualification is claimed.
[src/digitalmodel/subsea/rov/__init__.py:L29-L61]
[examples/domains/subsea/rov_tool_selection.py:L39-L47]

## 10. Coverage and open items

The interface tests cover isolated mismatch categories, aggregated reasons,
boundary reach, signed coordinates and invalid finite/shape/class inputs.
Tool-library tests cover capacity equality, missing capacities, function kinds
and invalid capacities/approach. The shipped example tests distinguish capacity
selection from geometry and test adjacent representable reach limits.
[tests/subsea/rov/test_interface_checks.py:L23-L71]
[tests/subsea/rov/test_tool_library.py:L19-L62]
[tests/subsea/rov/test_tool_selection_example.py:L38-L96]

Hs/scatter tests cover inclusive limits, alpha reduction, unnormalized weights
and invalid comparison domains. All six response tests pass when the declared
runtime dependencies in section 5 are supplied. Sequence and period tests cover dependency
order, continuous windows, partial metrics, contingency cover, interval counts,
empty campaigns and invalid graphs/weather inputs. These observations are not
line-coverage measurements or hardware validation.
[tests/subsea/rov/test_seastate_limits.py:L20-L175]
[tests/subsea/rov/test_intervention_sequence.py:L18-L113]
[tests/subsea/rov/test_intervention_periods.py:L22-L248]

NOT_EVALUATED: standards edition/clause compliance; rated interface dimensions,
capacity data and tolerances; nonfixed orientation, obstacles, contact mechanics,
currents and actual manipulator qualification; seals, forces, torque reaction,
joint limits and collision-free trajectories; installed LARS dynamics and load
acceptance; qualified forecast time basis, alpha and contingency adequacy;
response accuracy and complete CLI routing under the baseline environment;
exhaustive finite/extreme-value robustness, uncertainty propagation and
cross-package integration. Trees and connectors remain follow-up under the
parent issue; this ROV-only pack does not complete that multi-module scope.
