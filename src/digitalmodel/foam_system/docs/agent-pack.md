# Foam-system module knowledge pack

| Header | Value |
| --- | --- |
| Package | `src/digitalmodel/foam_system` |
| Code pin | `1d9d0018aaf61db4cbba3d6c4d5864a34adbf4b0` |
| Status | DRAFT; software contracts documented, engineering qualification incomplete |
| Import | `digitalmodel.foam_system` |

Summary: this pack records the implemented screening interfaces and their
limits. Source locators establish software behavior; they do not establish
engineering acceptance. Synthetic examples check arithmetic only. Governing
criteria and applicability remain NOT_EVALUATED.

### Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-11-05 | Record the public inventory, synthetic example and qualification gaps so documentation drift is detectable. |

## 1. Purpose and scope

The package calculates protected-area solution demand, concentrate quantity,
proportioner envelope checks and hydraulic demand for a tree network. The
workflow combines these calculations and writes CSV outputs. Criteria may be
inline or selected from the shipped YAML library. This is screening only.
[src/digitalmodel/foam_system/sizing.py:L1-L29]
[src/digitalmodel/foam_system/workflow.py:L102-L262]

Inventory policy: section 2 includes public function/class definitions in
all package Python files and explicit public class members. Re-exports have
one defining-source row. Constants, private helpers, nested local functions and
generated dataclass methods have no separate rows. Field defaults and function
signatures below describe implementation choices, not qualified design values.
[scripts/docs/check_agent_pack_functions.py:L1-L10]

## 2. Inputs and outputs

The table records exact Python fields or signatures. Required fields have no
assignment; explicit assignments are software defaults. Dataclass fields are
also the result schema when a calculation returns that class. The method and
validation notes following the table explain units and constraints. Property
rows show getter signatures; access those members as attributes.

| Public symbol | Fields or call signature | Output contract | Evidence |
| --- | --- | --- | --- |
| `LibraryEntry` | `key: str; criterion: FoamCriterion; application: str; verify_against_source: bool` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/criteria_library.py:L53-L59] |
| `load_criteria_library` | `load_criteria_library(path: Path \| None=None)` | dict[str, LibraryEntry] | [src/digitalmodel/foam_system/criteria_library.py:L62-L90] |
| `get_criterion` | `get_criterion(key: str, path: Path \| None=None)` | FoamCriterion | [src/digitalmodel/foam_system/criteria_library.py:L93-L106] |
| `list_criteria` | `list_criteria(path: Path \| None=None)` | tuple[LibraryEntry, ...] | [src/digitalmodel/foam_system/criteria_library.py:L109-L112] |
| `Citation` | `standard: str; edition: str; clause: str; note: str = ''` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L46-L65] |
| `Citation.label` | `Citation.label(self)` | str | [src/digitalmodel/foam_system/sizing.py:L63-L65] |
| `FoamCriterion` | `key: str; application_rate_lpm_per_m2: float; discharge_time_min: float; citation: Citation` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L72-L86] |
| `ProtectedArea` | `name: str; area_m2: float; criterion: FoamCriterion` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L90-L105] |
| `ProtectedArea.solution_flow_lpm` | `ProtectedArea.solution_flow_lpm(self)` | float | [src/digitalmodel/foam_system/sizing.py:L100-L101] |
| `ProtectedArea.solution_volume_l` | `ProtectedArea.solution_volume_l(self)` | float | [src/digitalmodel/foam_system/sizing.py:L104-L105] |
| `HoseStream` | `name: str; flow_lpm: float; count: int = 1; duration_min: float = 0.0; citation: Citation \| None = None` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L109-L124] |
| `HoseStream.total_flow_lpm` | `HoseStream.total_flow_lpm(self)` | float | [src/digitalmodel/foam_system/sizing.py:L123-L124] |
| `FoamDemandResult` | `per_area: tuple[ProtectedArea, ...]; demand_policy: str; governing_area: str; governing_area_flow_lpm: float; hose_flow_lpm: float; design_solution_flow_lpm: float` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L128-L134] |
| `foam_demand` | `foam_demand(areas: list[ProtectedArea], hose_streams: list[HoseStream] \| None=None, demand_policy: str='max')` | FoamDemandResult | [src/digitalmodel/foam_system/sizing.py:L137-L172] |
| `ConcentrateResult` | `concentration_percent: float; reserve_percent: float; area_concentrate_l: float; hose_concentrate_l: float; base_concentrate_l: float; reserve_concentrate_l: float; total_concentrate_l: float; injection_rate_lpm: float` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L179-L187] |
| `concentrate_quantity` | `concentrate_quantity(demand: FoamDemandResult, concentration_percent: float, reserve_percent: float=0.0, hose_streams: list[HoseStream] \| None=None)` | ConcentrateResult | [src/digitalmodel/foam_system/sizing.py:L190-L231] |
| `Proportioner` | `min_solution_flow_lpm: float; max_solution_flow_lpm: float; max_concentrate_flow_lpm: float \| None = None; rated_concentration_percent: float \| None = None` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L238-L250] |
| `ProportionerCheck` | `design_solution_flow_lpm: float; injection_rate_lpm: float; utilization_percent: float; solution_flow_ok: bool; concentrate_flow_ok: bool; concentration_match_ok: bool; ok: bool; messages: tuple[str, ...]` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L254-L262] |
| `proportioner_check` | `proportioner_check(proportioner: Proportioner, design_solution_flow_lpm: float, injection_rate_lpm: float, concentration_percent: float \| None=None)` | ProportionerCheck | [src/digitalmodel/foam_system/sizing.py:L265-L319] |
| `pipe_velocity_m_s` | `pipe_velocity_m_s(flow_lpm: float, diameter_mm: float)` | float | [src/digitalmodel/foam_system/sizing.py:L325-L330] |
| `hazen_williams_headloss_m` | `hazen_williams_headloss_m(flow_lpm: float, length_m: float, diameter_mm: float, c_factor: float=120.0)` | float | [src/digitalmodel/foam_system/sizing.py:L333-L353] |
| `darcy_weisbach_headloss_m` | `darcy_weisbach_headloss_m(flow_lpm: float, length_m: float, diameter_mm: float, roughness_mm: float=0.045, kinematic_viscosity_m2_s: float=1e-06)` | float | [src/digitalmodel/foam_system/sizing.py:L356-L379] |
| `PipeRun` | `from_node: str; to_node: str; length_m: float; diameter_mm: float; equivalent_length_m: float = 0.0` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L386-L401] |
| `Terminal` | `node: str; flow_lpm: float; required_pressure_bar: float; elevation_m: float = 0.0` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L405-L418] |
| `Pump` | `node: str; rated_flow_lpm: float; rated_head_m: float; elevation_m: float = 0.0` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L422-L430] |
| `RunResult` | `from_node: str; to_node: str; flow_lpm: float; velocity_m_s: float; headloss_m: float` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L434-L439] |
| `TerminalResult` | `node: str; flow_lpm: float; path: tuple[str, ...]; friction_headloss_m: float; static_head_m: float; pressure_head_m: float; required_pump_head_m: float` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L443-L450] |
| `HydraulicsResult` | `method: str; runs: tuple[RunResult, ...]; terminals: tuple[TerminalResult, ...]; total_flow_lpm: float; governing_terminal: str; required_pump_head_m: float; pump_head_margin_m: float; pump_head_margin_percent: float; pump_flow_margin_lpm: float; pump_head_ok: bool; pump_flow_ok: bool; max_velocity_m_s: float` | Dataclass record; fields shown at left. | [src/digitalmodel/foam_system/sizing.py:L454-L466] |
| `solve_tree_network` | `solve_tree_network(pump: Pump, runs: list[PipeRun], terminals: list[Terminal], method: str='hazen_williams', hazen_williams_c: float=120.0, roughness_mm: float=0.045, kinematic_viscosity_m2_s: float=1e-06, density_kg_m3: float=1000.0)` | HydraulicsResult | [src/digitalmodel/foam_system/sizing.py:L469-L598] |
| `router` | `router(cfg: dict)` | dict | [src/digitalmodel/foam_system/workflow.py:L102-L259] |

Table 1. Public definitions, including methods and properties.

Units follow field suffixes: m2, L/min, min, L, m, mm, bar, kg/m3 and m2/s.
Percentages are entered as percentages, not fractions. Network head is metres
of the supplied solution; velocities are m/s. Citation requires nonempty
standard, edition and clause text; note is optional. This checks text presence,
not the cited document. Criterion rate/time and protected area must be positive.
Hose flow must be positive and count at least one; duration has no equivalent
constructor guard. Proportioner requires an ordered nonnegative flow envelope.
[src/digitalmodel/foam_system/sizing.py:L46-L124]
[src/digitalmodel/foam_system/sizing.py:L238-L251]

Pipe lengths are nonnegative, diameters positive, terminal flows positive,
terminal pressure nonnegative, and pump flow/head positive. Other validation is
local to each call, rather than a common finite-value/unit system. The library
requires schema_version=1, nonempty criteria, explicit boolean verification flags,
application text and complete citation fields. Unknown selection keys raise
ValueError; listing returns entries in key order. File errors propagate.
[src/digitalmodel/foam_system/sizing.py:L386-L431]
[src/digitalmodel/foam_system/criteria_library.py:L62-L157]

The router takes a mutable cfg dictionary under foam_system_sizing. It requires
criteria or criteria_library, protected_areas and concentrate concentration;
proportioner and hydraulics are optional. It returns the same cfg with nested
results and CSV locations. Library/inline collisions and repeated library keys
are rejected. Config-relative output selection is implemented by private helpers.
[src/digitalmodel/foam_system/workflow.py:L102-L262]
[src/digitalmodel/foam_system/workflow.py:L265-L318]
[src/digitalmodel/foam_system/workflow.py:L437-L455]

## 3. Method and governing sources

Area solution flow is area times application rate. Demand selects the maximum
area flow or sums all flows, then adds concurrent hoses. Concentrate quantity
uses the maximum area **volume** under max policy, or summed volumes under sum
policy. Thus the flow-governing and volume-governing areas can differ. Hose
volume is added separately; reserve multiplies the entire base concentrate.
Injection rate uses design solution flow and concentration. Concentration must
lie strictly between zero and one hundred; reserve must be nonnegative.
[src/digitalmodel/foam_system/sizing.py:L137-L231]

Proportioner checks compare the duty against supplied flow, concentrate-flow
and concentration ratings. Missing optional ratings leave those checks true;
that does not establish vendor suitability. Tree hydraulics sums downstream
terminal flows, adds path friction, elevation difference and terminal pressure
head, then selects the maximum required head. Pump head and flow margins are
separate checks. The topology rejects duplicate feeds, disconnected nodes,
cycles and duplicate terminals. No network balancing iteration is performed.
[src/digitalmodel/foam_system/sizing.py:L265-L317]
[src/digitalmodel/foam_system/sizing.py:L469-L598]

Pipe friction uses the implemented Hazen-Williams or Darcy-Weisbach expression;
the latter selects laminar or explicit turbulent friction by Reynolds number.
Gravity, fluid properties, roughness and friction parameters are software inputs
or defaults, not values independently qualified here. The source restricts use
to water-like solution screening and excludes loops and eductor losses.
[src/digitalmodel/foam_system/sizing.py:L19-L29]
[src/digitalmodel/foam_system/sizing.py:L325-L379]

| Topic | Status | Evidence needed before engineering use |
| --- | --- | --- |
| Application rates and durations | NOT_EVALUATED | Governing edition, clause, fuel and discharge-device applicability. |
| Shipped criteria library | NOT_EVALUATED | Independently confirm each selected value and its verification flag against the governing source. |
| Concentration and reserve policy | NOT_EVALUATED | Qualified product data and applicable reserve requirements. |
| Hydraulic applicability | NOT_EVALUATED | Fluid properties, losses, topology and vendor operating curves for the intended system. |

Table 2. The implementation is the source for software behavior. No primary
engineering publication was independently qualified for this pack; licensed
criteria values are not reproduced. Citation labels alone do not close these gaps.

## 4. Worked examples and regression examples

This synthetic arithmetic example is unrelated to a fire-protection criterion.
Choose area 10 m2, rate 2 L/min/m2 and duration 5 min. Independently, flow is
10 times 2 = 20 L/min and volume is 20 times 5 = 100 L. At a synthetic
5 percent concentration the base concentrate is 5 L; an equal reserve gives
10 L total and injection is 1 L/min. Citation text identifies the synthetic basis.

```python
from digitalmodel.foam_system.sizing import (
    Citation, FoamCriterion, ProtectedArea, concentrate_quantity, foam_demand,
)

criterion = FoamCriterion("synthetic", 2.0, 5.0, Citation("Synthetic arithmetic", "1", "section 4"))
area = ProtectedArea("synthetic area", 10.0, criterion)
demand = foam_demand([area])
quantity = concentrate_quantity(demand, concentration_percent=5.0, reserve_percent=100.0)
print(demand.design_solution_flow_lpm, quantity.total_concentrate_l)
```

Expected printed values: `20.0 10.0`. The documentation test executes this block
and checks flow, base/total quantity and injection against the hand calculation.

## 5. QA gates and executable checks

Run with the declared test dependencies available; scratch outputs belong under
TMPDIR. The shared AST checker requires no package imports or engineering solver.

```bash
uv run --no-project --with pytest python -m pytest \
  tests/fire_safety tests/docs/test_screening_agent_packs.py \
  tests/docs/test_agent_pack_functions.py tests/docs/test_digitalmodel_routing_contract.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/foam-pack-tests"
uv run --no-project python scripts/docs/check_agent_pack_functions.py \
  --pack src/digitalmodel/foam_system/docs/agent-pack.md
```

The screening pack tests check registry uniqueness, ten-section form, locator
bounds, real-package inventories, the executed example and CLI rejection of
added source definitions and stale rows in temporary copies. Inventory equality
checks documentation identity; it does not authenticate engineering criteria.
`tests/docs/test_screening_agent_packs.py`

## 6. Known failure modes and pitfalls

- Maximum-flow demand does not select maximum-volume concentrate duty. Treat
  the two governing quantities independently; the same hose list must be supplied
  to demand and concentrate calls to retain its volume contribution.
  [src/digitalmodel/foam_system/sizing.py:L137-L231]
- Comparison guards do not constitute general NaN/infinity rejection. Citation
  presence does not verify editions or applicability. Hose duration is not
  validated by HoseStream; zero is its software default.
  [src/digitalmodel/foam_system/sizing.py:L46-L124]
- Hydraulic terminals are supplied separately from protected-area demand. The
  caller must establish scenario consistency. Rated head is a single value,
  not a pump curve; no loop solver, transient calculation or eductor model is
  present. Optional checks can be absent from router results.
  [src/digitalmodel/foam_system/workflow.py:L110-L149]
  [src/digitalmodel/foam_system/sizing.py:L469-L598]
- Library verification flags append a warning to citation notes rather than
  preventing calculation. CSV writes create output directories and can replace
  existing files; choose an appropriate output directory.
  [src/digitalmodel/foam_system/criteria_library.py:L115-L157]
  [src/digitalmodel/foam_system/workflow.py:L437-L469]

## 7. Lessons log

A complete citation string and a passing arithmetic test are different evidence
from a qualified design input. Preserve library warnings in downstream outputs.
Tests first exposed the absent pack; the existing corrosion checker provides a
shared definition of public inventory instead of a second parsing convention.

## 8. Maintenance and review

The maintainers own this pack. When definitions change, update section 2 and
run the checker; when equations or validation change, review sections 3 and 6.
A reissue adds a revision-history row. Engineering review must establish the
unverified items in section 3 before a screening output supports design use.

## 9. Dependencies and other implementations

The calculation module uses Python math and dataclasses. The criteria loader
adds PyYAML; the router uses CSV and pathlib and calls the same calculation
functions. Initializer re-exports do not add definitions. No independent solver
comparison is supplied by this pack.
[src/digitalmodel/foam_system/sizing.py:L31-L34]
[src/digitalmodel/foam_system/criteria_library.py:L34-L42]
[src/digitalmodel/foam_system/workflow.py:L74-L96]
[src/digitalmodel/foam_system/__init__.py:L1-L17]

Existing operating guidance: `docs/domains/fire_safety/foam-system-sizing.md`.
Routing: `docs/registry/module-routing.yaml`, module foam_system, agent_pack.

## 10. Coverage and open items

The focused software tests cover sizing, criterion loading and router output.
New documentation tests cover inventory drift, source locators, registration and
synthetic arithmetic. These tests do not establish source-table correctness,
physical fire performance, product approval, hydraulic calibration or full-system
code compliance. Those remain NOT_EVALUATED; no runtime algorithm is changed.
