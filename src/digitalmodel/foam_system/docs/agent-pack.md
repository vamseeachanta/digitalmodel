# Foam system sizing knowledge pack

This pack documents the pinned code contract. Arithmetic evidence does not
qualify physical criteria, equipment selection or a final design. UNVERIFIED
means the numerical implementation is readable but its claimed governing basis
has not been established. NOT_EVALUATED means the required source was unreadable.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Initial pinned API inventory and hand anchors for repeatable review. |

| Header | Value |
| --- | --- |
| Code pin (`origin/main`) | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Package | `src/digitalmodel/foam_system/` |
| QA family | EC — engineering calculation screening |
| Evidence basis | Source lines at code pin; synthetic offline tests |

## 1. Purpose and scope

Demand, concentrate inventory, proportioner envelope and tree-network hydraulic screening. Library citation labels are metadata, not source verification. The report under separate review is outside this code-contract pack. [src/digitalmodel/foam_system/sizing.py:L1-L28]

## 2. Inputs and outputs

The table includes every package-owned public class, function, method and
property in direct Python files, including definitions not re-exported by
`__init__`. Imported utilities and local closures are excluded. Constructor
fields and function signatures record implementation defaults, not recommended
engineering inputs. Suffixes declare units: `_lpm` L/min, `_l` L, `_m` m,
`_mm` mm, `_m2` m², `_mpa` MPa, `_kn` kN, `_v` V; other strings,
flags and fractions are dimensionless. Types are annotations, not enforced
runtime schemas. Output record fields have the same suffix convention.

| Public definition | Inputs, units, validation and defaults | Output contract | Evidence |
| --- | --- | --- | --- |
| `criteria_library.LibraryEntry` | key, application strings; criterion object; verify flag; no own validation<br>Fields: key: str; criterion: FoamCriterion; application: str; verify_against_source: bool | Frozen selection metadata with criterion; no defaults | [src/digitalmodel/foam_system/criteria_library.py:L53-L59] |
| `criteria_library.load_criteria_library` | Optional path; shipped YAML by default; file/mapping/schema/nonempty rows, citation, usage and explicit bool verified<br>Signature: (path: Path &#124; None=None) | dict keyed by criterion key; native I/O/YAML errors or ValueError | [src/digitalmodel/foam_system/criteria_library.py:L62-L90] |
| `criteria_library.get_criterion` | key string, optional library path; unknown key raises ValueError with known keys<br>Signature: (key: str, path: Path &#124; None=None) | FoamCriterion; reloads library on each call | [src/digitalmodel/foam_system/criteria_library.py:L93-L106] |
| `criteria_library.list_criteria` | Optional library path, same validation as loader<br>Signature: (path: Path &#124; None=None) | tuple of LibraryEntry in sorted key order | [src/digitalmodel/foam_system/criteria_library.py:L109-L112] |
| `sizing.Citation` | Required standard, edition, clause: str(value).strip() must be nonempty; optional note empty<br>Fields: standard: str; edition: str; clause: str; note: str = '' | Frozen citation record; values are not verified against publications | [src/digitalmodel/foam_system/sizing.py:L46-L65] |
| `sizing.Citation.label` | self only; no further validation<br>Signature: (self) | str: standard (edition) clause, optional note | [src/digitalmodel/foam_system/sizing.py:L63-L65] |
| `sizing.FoamCriterion` | key and citation; rate L/(min m²), time min both >0; no finite check<br>Fields: key: str; application_rate_lpm_per_m2: float; discharge_time_min: float; citation: Citation | Frozen criterion; no defaults | [src/digitalmodel/foam_system/sizing.py:L72-L86] |
| `sizing.ProtectedArea` | name, area m² >0, criterion; no finite check<br>Fields: name: str; area_m2: float; criterion: FoamCriterion | Frozen area; properties give flow L/min and volume L | [src/digitalmodel/foam_system/sizing.py:L90-L105] |
| `sizing.ProtectedArea.solution_flow_lpm` | self only: area m² and criterion rate L/(min m²)<br>Signature: (self) | float L/min, area × rate | [src/digitalmodel/foam_system/sizing.py:L100-L101] |
| `sizing.ProtectedArea.solution_volume_l` | self only: flow L/min and criterion duration min<br>Signature: (self) | float L, flow × time | [src/digitalmodel/foam_system/sizing.py:L104-L105] |
| `sizing.HoseStream` | name; flow L/min >0, count >=1; duration min unchecked; citation optional<br>Fields: name: str; flow_lpm: float; count: int = 1; duration_min: float = 0.0; citation: Citation &#124; None = None | Frozen hose record; count=1, duration=0, citation=None | [src/digitalmodel/foam_system/sizing.py:L109-L124] |
| `sizing.HoseStream.total_flow_lpm` | self only: flow L/min and count<br>Signature: (self) | float L/min, flow × count | [src/digitalmodel/foam_system/sizing.py:L123-L124] |
| `sizing.FoamDemandResult` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: per_area: tuple[ProtectedArea, ...], demand_policy: str, governing_area: str, governing_area_flow_lpm: float, hose_flow_lpm: float, design_solution_flow_lpm: float | [src/digitalmodel/foam_system/sizing.py:L128-L134] |
| `sizing.foam_demand` | Nonempty areas; optional hoses None; policy max or sum only, default max<br>Signature: (areas: list[ProtectedArea], hose_streams: list[HoseStream] &#124; None=None, demand_policy: str='max') | FoamDemandResult: tuple areas, policy, governing name/flow, hose flow, design flow (L/min) | [src/digitalmodel/foam_system/sizing.py:L137-L172] |
| `sizing.ConcentrateResult` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: concentration_percent: float, reserve_percent: float, area_concentrate_l: float, hose_concentrate_l: float, base_concentrate_l: float, reserve_concentrate_l: float, total_concentrate_l: float, injection_rate_lpm: float | [src/digitalmodel/foam_system/sizing.py:L179-L187] |
| `sizing.concentrate_quantity` | Demand; concentration percent strictly between zero and 100; reserve percent >=0 (default 0); optional hoses None<br>Signature: (demand: FoamDemandResult, concentration_percent: float, reserve_percent: float=0.0, hose_streams: list[HoseStream] &#124; None=None) | ConcentrateResult: percent inputs; area/hose/base/reserve/total L and injection L/min; max volume governs max policy | [src/digitalmodel/foam_system/sizing.py:L190-L231] |
| `sizing.Proportioner` | min/max solution flow L/min: 0<=min<max; optional concentrate flow and rated percent unchecked, both None<br>Fields: min_solution_flow_lpm: float; max_solution_flow_lpm: float; max_concentrate_flow_lpm: float &#124; None = None; rated_concentration_percent: float &#124; None = None | Frozen rated envelope | [src/digitalmodel/foam_system/sizing.py:L238-L250] |
| `sizing.ProportionerCheck` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: design_solution_flow_lpm: float, injection_rate_lpm: float, utilization_percent: float, solution_flow_ok: bool, concentrate_flow_ok: bool, concentration_match_ok: bool, ok: bool, messages: tuple[str, ...] | [src/digitalmodel/foam_system/sizing.py:L254-L262] |
| `sizing.proportioner_check` | Envelope, design solution/injection L/min; optional concentration percent None; no own range validation<br>Signature: (proportioner: Proportioner, design_solution_flow_lpm: float, injection_rate_lpm: float, concentration_percent: float &#124; None=None) | ProportionerCheck: flows, utilization %, three subchecks, combined ok, tuple messages; omitted rating/cap treated as passing | [src/digitalmodel/foam_system/sizing.py:L265-L319] |
| `sizing.pipe_velocity_m_s` | flow L/min unchecked sign; diameter mm >0<br>Signature: (flow_lpm: float, diameter_mm: float) | float m/s | [src/digitalmodel/foam_system/sizing.py:L325-L330] |
| `sizing.hazen_williams_headloss_m` | flow L/min and length m >=0; diameter mm and C>0; C default 120; zero flow/length returns zero<br>Signature: (flow_lpm: float, length_m: float, diameter_mm: float, c_factor: float=120.0) | float friction head m; water-like turbulent screening | [src/digitalmodel/foam_system/sizing.py:L333-L353] |
| `sizing.darcy_weisbach_headloss_m` | flow L/min, length m >=0; diameter mm>0, roughness mm>=0 (default 0.045), viscosity m²/s>0 (default 1e-6)<br>Signature: (flow_lpm: float, length_m: float, diameter_mm: float, roughness_mm: float=0.045, kinematic_viscosity_m2_s: float=1e-06) | float friction head m; zero flow/length returns zero; laminar or explicit turbulent branch | [src/digitalmodel/foam_system/sizing.py:L356-L379] |
| `sizing.PipeRun` | from/to strings; length m and equivalent length m>=0; diameter mm>0; equivalent length default 0<br>Fields: from_node: str; to_node: str; length_m: float; diameter_mm: float; equivalent_length_m: float = 0.0 | Frozen pipe run | [src/digitalmodel/foam_system/sizing.py:L386-L401] |
| `sizing.Terminal` | node; flow L/min>0, required pressure bar>=0; elevation m default 0 unchecked<br>Fields: node: str; flow_lpm: float; required_pressure_bar: float; elevation_m: float = 0.0 | Frozen discharge point | [src/digitalmodel/foam_system/sizing.py:L405-L418] |
| `sizing.Pump` | node; rated flow L/min and head m>0; elevation m default 0 unchecked<br>Fields: node: str; rated_flow_lpm: float; rated_head_m: float; elevation_m: float = 0.0 | Frozen fixed rating; no pump curve | [src/digitalmodel/foam_system/sizing.py:L422-L430] |
| `sizing.RunResult` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: from_node: str, to_node: str, flow_lpm: float, velocity_m_s: float, headloss_m: float | [src/digitalmodel/foam_system/sizing.py:L434-L439] |
| `sizing.TerminalResult` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: node: str, flow_lpm: float, path: tuple[str, ...], friction_headloss_m: float, static_head_m: float, pressure_head_m: float, required_pump_head_m: float | [src/digitalmodel/foam_system/sizing.py:L443-L450] |
| `sizing.HydraulicsResult` | All fields required; no validation; dimensions follow field suffixes | Frozen output record: method: str, runs: tuple[RunResult, ...], terminals: tuple[TerminalResult, ...], total_flow_lpm: float, governing_terminal: str, required_pump_head_m: float, pump_head_margin_m: float, pump_head_margin_percent: float, pump_flow_margin_lpm: float, pump_head_ok: bool, pump_flow_ok: bool, max_velocity_m_s: float | [src/digitalmodel/foam_system/sizing.py:L454-L466] |
| `sizing.solve_tree_network` | Pump, nonempty runs and terminals; rejects multiple feeds, cycles, disconnected runs/terminals and duplicate terminals; positive density kg/m³<br>Signature: (pump: Pump, runs: list[PipeRun], terminals: list[Terminal], method: str='hazen_williams', hazen_williams_c: float=120.0, roughness_mm: float=0.045, kinematic_viscosity_m2_s: float=1e-06, density_kg_m3: float=1000.0) | HydraulicsResult; methods hazen_williams (default) or darcy_weisbach; defaults C=120, roughness=0.045 mm, viscosity=1e-6 m²/s, density=1000 kg/m³ | [src/digitalmodel/foam_system/sizing.py:L469-L598] |
| `workflow.router` | cfg mapping with foam_system_sizing: criteria/library keys, nonempty protected_areas, concentrate.concentration_percent required; optional hoses, max policy, reserve 0, proportioner and hydraulics omitted by default<br>Signature: (cfg: dict) | Mutates/returns cfg; demand_result, concentrate_result, optional proportioner_result/hydraulics_result, notice and output paths; always demand CSV, optional hydraulic CSV pair; output_dir results relative to config directory; stem config filename or basename | [src/digitalmodel/foam_system/workflow.py:L102-L259] |

## 3. Method and governing standards

| Method / constants | Basis and status | Code evidence |
| --- | --- | --- |
| Q=A r; V=Q t | Dimensional arithmetic. Selected library row `fss_deck_foam_largest_tank_section`: library cites FSS Code, 2015 consolidated edition, Ch.14 §2.2.2.2. Public original resolution MSC.98(73), adopted 2000, Ch.14 §2.2.1.1.2 and §2.2.1.2 supports the rate and inert-gas duration used below. Later edition/clause identity UNVERIFIED. | [src/digitalmodel/foam_system/sizing.py:L89-L105] [src/digitalmodel/foam_system/data/foam_criteria_library.yml:L119-L135] |
| max or sum area flows plus hose flow; max or sum area volumes | Caller-selected simultaneous-fire policy; max flow and max volume can govern on different areas. No universal fire policy is asserted. | [src/digitalmodel/foam_system/sizing.py:L137-L172] [src/digitalmodel/foam_system/sizing.py:L190-L231] |
| Concentrate = solution volume × percent/100; reserve = base × reserve percent/100; injection = design flow × percent/100 | Definition of percentage; concentration, reserve, hose count and duration are required engineering decisions. Zero reserve/default hose duration are software defaults. | [src/digitalmodel/foam_system/sizing.py:L108-L124] [src/digitalmodel/foam_system/sizing.py:L205-L231] |
| Proportioner inclusive envelope; utilization = design/max ×100 | Vendor envelope supplied by caller; no vendor data verified. Match uses relative/absolute tolerance 1e-9 (software numerical policy, UNVERIFIED basis). | [src/digitalmodel/foam_system/sizing.py:L265-L319] |
| v=Q/(πd²/4); flow/60000, diameter/1000 | Dimensional conversions L/min to m³/s, mm to m; circular geometry. No engineering acceptance velocity is imposed. | [src/digitalmodel/foam_system/sizing.py:L325-L330] |
| Hazen–Williams: h=10.67 L Q^1.852/(C^1.852 d^4.87); default C=120 | NOT_EVALUATED: no primary document, edition or clause is named for coefficient/exponents or default C in code. Empirical turbulent water-like model, not qualified foam rheology. | [src/digitalmodel/foam_system/sizing.py:L333-L353] |
| Darcy–Weisbach: h=f(L/d)v²/(2g); laminar f=64/Re below Re=2300; otherwise Swamee–Jain f=0.25/[log10(ε/(3.7d)+5.74/Re^0.9)]² | NOT_EVALUATED: code names methods but no document/edition/clause. Transition and empirical coefficients not independently source-verified. Roughness 0.045 mm, viscosity 1e-6 m²/s are software defaults with unverified physical basis. | [src/digitalmodel/foam_system/sizing.py:L356-L379] |
| Tree continuity, downstream flow summation; terminal head = path losses + elevation difference + pressure/(ρg); pump margin = rating − demand | Algebraic model, not a loop or pump-curve solution. Density 1000 kg/m³, standard gravity G_M_S2=9.80665, BAR_TO_PA=1e5 are implementation constants; external provenance NOT_EVALUATED. Head margin percent divides by rated head. | [src/digitalmodel/foam_system/sizing.py:L36-L39] [src/digitalmodel/foam_system/sizing.py:L469-L598] |
| Library schema 1, marker text, default paths, string keys, result flags | Software conventions, not physical criteria. YAML rows except the selected row have NOT_EVALUATED external provenance in this pack; no other library values reproduced. | [src/digitalmodel/foam_system/criteria_library.py:L43-L49] [src/digitalmodel/foam_system/criteria_library.py:L62-L90] [src/digitalmodel/foam_system/workflow.py:L435-L472] |

Public source read: [IMO, MSC.98(73), original FSS Code (2000), printed p.31](https://wwwcdn.imo.org/localresources/en/KnowledgeCentre/IndexofIMOResolutions/MSCResolutions/MSC.98(73).pdf).
The selected row's values are consistent with this original resolution; its
later consolidated-edition locator was not verified. The original also requires
a greatest-of-three supply comparison; a single area example is not that complete check.

## 4. Validated worked examples and regression examples

All dimensions and concentrate policies below are synthetic inputs. The selected
row supplies r=6 L/(min m²) and t=20 min, with the inert-gas condition from the
public original FSS Code clauses above. No other library row is restated.

| Example | Inputs, hand method and literal answer | Executable evidence |
| --- | --- | --- |
| Solution rate | A=100 m²; Q=100×6=600 L/min | `tests/fire_safety/test_foam_agent_pack.py::test_library_solution_rate` |
| Solution volume | Same area and rate; V=600×20=12000 L | `tests/fire_safety/test_foam_agent_pack.py::test_library_solution_volume` |
| Concentrate and reserve | Same volume, synthetic concentration 3%, reserve 50%; base=12000×3/100=360 L; total=360×1.5=540 L; injection=600×3/100=18 L/min | `tests/fire_safety/test_foam_agent_pack.py::test_synthetic_concentrate_policy` |

These assert independent literal arithmetic. The source status is separate from
software equality; no assertion authenticates the later edition or a full design.

## 5. QA gates and executable checks

| Gate | Executable check | Meaning |
| --- | --- | --- |
| Required citation fields | `tests/fire_safety/test_foam_system_sizing.py::test_citation_is_mandatory` | Missing metadata refused, not verification of cited text |
| Network topology | `tests/fire_safety/test_foam_system_sizing.py::test_tree_network_validation` | Invalid tree cases refused |
| Optional scope | `tests/fire_safety/test_foam_system_sizing.py::test_router_optional_sections` | Omitted hydraulic/proportioner results explicitly None |
| Public contract | `tests/docs/test_screening_agent_packs.py::test_public_contract_and_locators[foam_system]` | AST names and defining locators match code |
| Example and gate collection | `tests/docs/test_screening_agent_packs.py::test_all_cited_nodes_collect[foam_system]` | Every section-4/5 citation collects; no physical qualification implied |

Run the node-collection gate through `tests/docs/test_agent_pack_node_ids.py`;
the additional local check opts these packs into section-5 citation collection.

## 6. Known failure modes and pitfalls

| File and lines | Defect / limitation | Follow-up |
| --- | --- | --- |
| [src/digitalmodel/foam_system/sizing.py:L80-L97] [src/digitalmodel/foam_system/sizing.py:L386-L430] | Most positivity comparisons admit NaN or positive infinity; annotated types are not enforced. | Add finite-value validation before using results for decisions. |
| [src/digitalmodel/foam_system/sizing.py:L118-L124] [src/digitalmodel/foam_system/workflow.py:L356-L366] | Hose duration may be negative; fractional counts are accepted directly and truncated by router int conversion. | Validate finite nonnegative duration and integer count. |
| [src/digitalmodel/foam_system/sizing.py:L205-L231] | Hoses are supplied separately from demand; omission can understate inventory while injection includes hose flow. Manually constructed demand policy falls into sum branch for any non-max string. | Tie hose inventory to demand and validate result inputs. |
| [src/digitalmodel/foam_system/sizing.py:L283-L319] | Missing optional concentrate capacity or rating leaves its subcheck true; absent requested concentration bypasses rating comparison. | Treat omitted checks as unevaluated when required. |
| [src/digitalmodel/foam_system/sizing.py:L325-L330] [src/digitalmodel/foam_system/sizing.py:L356-L379] | Velocity accepts negative flow; hydraulic transition is abrupt and empirical defaults lack citations. | Declare valid ranges, transition treatment and physical source basis. |
| [src/digitalmodel/foam_system/workflow.py:L118-L150] [src/digitalmodel/foam_system/workflow.py:L215-L259] | Optional checks can be omitted, and hydraulic terminal flow is not reconciled with calculated foam demand. No combined design acceptance is returned. | Require scope and demand reconciliation for an engineering use case. |
| [src/digitalmodel/foam_system/data/foam_criteria_library.yml:L119-L135] | Selected library locator is a later-edition claim; readable original has different clause numbering. | Verify the declared edition before changing metadata; retain source status. |
| [src/digitalmodel/foam_system/criteria_library.py:L69-L90] | YAML duplicate keys are not explicitly rejected; schema and citation strings alone cannot authenticate data. | Reject duplicates and retain independent source evidence. |

## 7. Lessons log

A citation object proves presence of metadata only. Keep unit arithmetic,
source verification and design acceptance as separate evidence. Max flow and
max volume need not have the same governing area.

## 8. Ownership

Maintainers own the public API and source reconciliation. Engineering reviewers
must establish governing criteria, simultaneous scenarios, equipment ratings and
fluid properties before design use. This pack grants no engineering approval.

## 9. Dependencies and other implementations

`criteria_library` uses PyYAML and local `sizing` dataclasses; `workflow` uses
CSV/path utilities and those pure calculations. No solver is required.
[src/digitalmodel/foam_system/criteria_library.py:L33-L41]
[src/digitalmodel/foam_system/workflow.py:L68-L90]

Routing and implementation-map `agent_packs` metadata register this pack as EC.
The implementation map contains rejected foam candidates for unrelated families;
registration does not promote those candidates into accepted implementations.
Other-module numerical equivalence is NOT_EVALUATED.

## 10. Coverage and open items

Read all package Python files, the criteria YAML, sizing and criteria-library
tests, routing entries and the report diff. Public definitions and methods are
exhaustively checked; imported utilities, private helpers and local closures are
excluded. The local AST checker is temporary until the shared checker is merged.
The pack intentionally leaves governing hydraulic constants and other library
rows NOT_EVALUATED. Resolve section-6 findings and source status before wider use.
