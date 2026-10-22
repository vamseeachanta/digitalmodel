# Subsea rov knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/rov` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT; arithmetic regression evidence only |
| Analysis family | EC |
| Scope authority | Issue #3335 |

This pack documents the pinned implementation. It supplies no equipment approval,
qualified operational limit or verified licensed-standard clause basis.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-22 | Record existing contracts, independent examples and qualification gaps. |

## 1. Purpose and scope

Caller-limit LARS sea-state and weighted scatter screens, optional motion-response screen, fixed-orientation interface/reach checks, caller-supplied tool lookup and serial weather-window scheduling. API 17H, IMCA R 004 and DNV-ST-N001 are context references, not verified acceptance criteria. Geometry compatibility and tool capacity are separate checks; the scheduler does not infer either from task labels. [src/digitalmodel/subsea/rov/__init__.py:L4-L27]

## 2. Inputs and outputs

Every locally defined public class/function and public method/property is listed,
including submodule APIs absent from package exports. Explicit exported data is also
listed. Imported library helpers and private names are excluded. Defaults below are
implementation observations; NOT_EVALUATED defaults are not design recommendations.

| Public API | Inputs, units, validation and defaults | Output contract | Evidence at code pin |
|---|---|---|---|
| `SeaStateResult` | hs and hs_oplim m, utilisation dimensionless, margin m, operable bool; all required. | Frozen unchecked result. | [src/digitalmodel/subsea/rov/seastate_limits.py:L63-L85] |
| `seastate_operability` | hs and hs_limit m required; alpha defaults 1 (dimensionless no-allowance convention). Rejects hs<0, hs_limit≤0, alpha outside (0,1]. | SeaStateResult: operational limit=alpha*limit, utilisation=hs/operational limit, margin=difference; inclusive comparison. | [src/digitalmodel/subsea/rov/seastate_limits.py:L88-L133] |
| `OperabilityResult` | operability_pct percent, hs_oplim m, n_operable and n_total counts; all required. | Frozen unchecked result; counts include zero-weight cells. | [src/digitalmodel/subsea/rov/seastate_limits.py:L136-L155] |
| `scatter_operability` | scatter_diagram sequence with hs m and probability dimensionless keys, hs_limit m required; alpha defaults 1. Rejects empty diagram, negative heights/weights, nonpositive limit or total weight, alpha outside (0,1]. Missing keys raise KeyError. | OperabilityResult; normalized occurrence-weight percentage, operational limit and cell counts. Not temporal weather-window probability. | [src/digitalmodel/subsea/rov/seastate_limits.py:L158-L222] |
| `RaoOperabilityResult` | v_significant and v_oplim m/s, utilisation dimensionless, margin m/s, operable bool; all required. | Frozen unchecked result for significant single amplitude (2*population standard deviation); available from submodule only. | [src/digitalmodel/subsea/rov/seastate_limits.py:L225-L248] |
| `rao_based_operability` | forecast WaveForecast, rao provider required; keyword-only velocity_limit m/s required; deployment_offset m triple defaults (0,0,0), alpha defaults 1, dt seconds defaults 0.2. Checks positive velocity limit and alpha in (0,1]; other validation delegated. | RaoOperabilityResult using reconstructed motion and point vertical velocity. Limit must use 2*sigma convention. Available from submodule, absent from package __all__. | [src/digitalmodel/subsea/rov/seastate_limits.py:L251-L318] |
| `InterfaceKind` | String enum torque/hot_stab/override/paddle/docking/parking. | Interface label; no geometry or capacity. | [src/digitalmodel/subsea/rov/interface_checks.py:L25-L33] |
| `ToolInterface` | kind, class_id label, envelope_m triple m, required_approach_m m all required. | Frozen unchecked demand record; validation deferred to consumers. | [src/digitalmodel/subsea/rov/interface_checks.py:L36-L43] |
| `InterfaceSpec` | kind, class_id, envelope_m triple m, approach_clearance_m m, location_m triple m all required. | Frozen unchecked target record; location relative to manipulator base; validation deferred. | [src/digitalmodel/subsea/rov/interface_checks.py:L46-L54] |
| `InterfaceResult` | compatible bool and reasons tuple of strings; required. | Frozen unchecked result with all failed categories. | [src/digitalmodel/subsea/rov/interface_checks.py:L57-L62] |
| `check_interface` | tool, target and manipulator_reach_m m required. Enum kinds, nonempty classes, torque class labels in embedded set, positive finite envelope triples, finite nonnegative approach/reach, finite location triple. | InterfaceResult; reasons in order kind, class, envelope, approach, reach; fixed orientation, inclusive limits, Euclidean base reach. | [src/digitalmodel/subsea/rov/interface_checks.py:L86-L122] |
| `ToolKind` | String enum torque/hot_stab/pull/cutting/cleaning. | Function label, no rating. | [src/digitalmodel/subsea/rov/tool_library.py:L25-L32] |
| `TaskDuty` | kind required; torque_nm N m, turns count, flow_m3_s m³/s, pressure_pa Pa, line_pull_n N each default None. | Frozen unchecked duty; None means no requirement for that field, validation deferred. | [src/digitalmodel/subsea/rov/tool_library.py:L35-L44] |
| `ToolSpec` | name, kind, interface required; torque_nm, turns, flow_m3_s, pressure_pa, line_pull_n each default None in the same units as TaskDuty. | Frozen unchecked tool record; None means unknown capacity, validation deferred. | [src/digitalmodel/subsea/rov/tool_library.py:L47-L58] |
| `find_tools` | tools iterable of ToolSpec and duty TaskDuty required. Validates enum kind, each specified capacity finite nonnegative, nonempty tool name and tool interface. | Tuple preserving supplied order; matches kind and every specified capacity; unknown capacity fails a specified requirement. Does not check a target interface or reach. | [src/digitalmodel/subsea/rov/tool_library.py:L84-L106] |
| `InterventionTask` | task_id, equipment, interface labels and duration_steps/weather_window_steps integer sample counts required; dependencies defaults empty tuple. | Frozen unchecked task; scheduler validates nonempty labels, positive exact integers, window≥duration and dependency graph. | [src/digitalmodel/subsea/rov/intervention_sequence.py:L38-L47] |
| `ScheduledTask` | task_id, equipment, interface labels, start_hours/end_hours h from forecast origin, all required. | Frozen unchecked scheduled record. | [src/digitalmodel/subsea/rov/intervention_sequence.py:L50-L58] |
| `InterventionPlan` | schedule tuple, unscheduled (task_id,reason) tuple, complete bool, campaign_duration_hours h or None, waiting_on_weather_fraction dimensionless or None; all required. | Frozen unchecked result; partial campaigns retain feasible tasks but aggregate metrics are None. | [src/digitalmodel/subsea/rov/intervention_sequence.py:L61-L69] |
| `plan_intervention` | tasks and hs sequence m, hs_limit m required; keyword-only step_hours h required, alpha defaults 1. Validates task counts/IDs/dependencies/cycles; finite positive step/limit, alpha in (0,1], finite nonnegative heights; empty horizon allowed. | InterventionPlan in stable topological order; earliest contiguous window for each task; only duration occupies vehicle; unscheduled reasons dependency_unavailable/no_weather_window. Complete elapsed time ends at last finish, idle/elapsed includes initial waiting; empty campaign metrics zero. | [src/digitalmodel/subsea/rov/intervention_sequence.py:L130-L177] |

## 3. Method and governing standards

| Method / constants | Implemented relation | Basis and qualification |
|---|---|---|
| Single sea state | operational limit=alpha*Hs_limit; utilisation=Hs/operational limit; margin=limit−Hs; inclusive comparison | Caller-supplied limit, elementary multiplication, subtraction and ratio. API 17H/IMCA R 004 editions and clauses NOT_EVALUATED. Default alpha=1 means no derating, not an approved forecast factor. [src/digitalmodel/subsea/rov/seastate_limits.py:L88-L133] |
| Scatter weighting | percentage=100Σaccepted weights/Σall weights | Independent normalized weighted count; 100 follows percent definition. No temporal independence or persistence model. Standards qualification NOT_EVALUATED. [src/digitalmodel/subsea/rov/seastate_limits.py:L193-L222] |
| Motion-response screen | Reconstruct motion; point vertical motion; time derivative; significant single velocity amplitude=2*population std(v); compare with alpha*velocity_limit | NOT_EVALUATED: convention is documented in code, but no independently opened spectral/standard source or approved LARS limit. Deployment offset defaults zero; dt default is a numerical sampling choice, not a qualified convergence criterion. [src/digitalmodel/subsea/rov/seastate_limits.py:L251-L318] |
| Interface and reach | Kind/class equality; each box dimension and approach clearance sufficient; sqrt(x²+y²+z²)≤reach | Independent aligned-box containment and Euclidean distance. API 17H edition, clauses and embedded torque-label range NOT_EVALUATED; labels impose no torque capacity or tolerances. [src/digitalmodel/subsea/rov/interface_checks.py:L70-L83] [src/digitalmodel/subsea/rov/interface_checks.py:L104-L122] |
| Tool filtering | Kind equality; every specified demand must have a known capacity ≥ demand | Algorithmic lookup over caller records. None demand disables only that field; unknown capacity is never unlimited. No embedded ratings or qualification equation. [src/digitalmodel/subsea/rov/tool_library.py:L64-L106] |
| Serial planning | Stable dependency order; first contiguous accepted interval run of window width; end=start+duration; elapsed=last end; waiting=(elapsed−working)/elapsed | Independent discrete enumeration; task durations/window widths are caller integer inputs. No optimization, standard-derived durations or equipment inference. API 17H and DNV-ST-N001 clauses NOT_EVALUATED. [src/digitalmodel/subsea/rov/intervention_sequence.py:L85-L127] [src/digitalmodel/subsea/rov/intervention_sequence.py:L153-L177] |

DNV-ST-N001 edition 2023-12 is mentioned by the existing scheduler docstring, but
its landing page was not independently opened for this pack; edition/clause and
operational applicability remain UNVERIFIED. No licensed text, operating limit,
alpha table or catalogue rating is supplied. Synthetic example inputs are assumed,
based on previous experience, and test arithmetic only.

## 4. Worked examples and regression evidence

1. **Sea state**: supplied Hs_limit=4 m and alpha=0.5 yield **2.0 m**.
   Hs=1 m gives U=1/2=**0.5**, margin=2−1=**1.0 m**, operable.
   Independent multiplication/ratio; the allowance is synthetic, not a standard factor.
   `tests/subsea/rov/test_agent_pack_examples.py::test_seastate_literals`.
2. **Scatter**: Hs=(1,2,3) m with weights=(2,3,5), supplied limit=2 m.
   First two cells pass including equality; accepted weight=5 of total 10.
   100×5/10=**50.0 %**, n_operable=**2**, n_total=**3**.
   Independent normalized weighted count; not continuous-window probability.
   `tests/subsea/rov/test_agent_pack_examples.py::test_scatter_literal`.
3. **Reach boundary**: matching synthetic docking labels, equal (1,1,1) m
   tool/target boxes and equal 1 m approach space. Location=(3,4,0) m gives
   sqrt(3²+4²)=**5.0 m**. Reach=5 m accepts equality; reach=4 m returns
   only **reach**. Independent Euclidean geometry; collision/tolerance acceptance
   NOT_EVALUATED.
   `tests/subsea/rov/test_agent_pack_examples.py::test_reach_boundary_literal`.
4. **Serial weather**: Hs=(3,1,1,1) m, limit=2 m, alpha=1, step=0.5 h.
   Mask=(false,true,true,true). Task a duration/window=1/2 starts sample 1,
   ends 2; dependent b duration/window=1/1 starts 2, ends 3. Starts=(**0.5,1.0 h**),
   ends=(**1.0,1.5 h**), elapsed=**1.5 h**, idle=0.5 h, waiting fraction=**1/3**
   (33.3 %). The two-sample forecast cover for a does not occupy the vehicle
   for two samples. Independent interval enumeration; no operating approval implied.
   `tests/subsea/rov/test_agent_pack_examples.py::test_serial_weather_literals`.

## 5. Executable QA gates

| Gate | Executable check | Meaning |
|---|---|---|
| Public inventory, pin and headings | `tests/docs/test_subsea_agent_packs.py::test_pack_contracts[rov]` | Includes submodule-only motion-response APIs |
| Missing API rejection | `tests/docs/test_subsea_agent_packs.py::test_missing_public_name_fails[rov]` | Negative section-2 inventory check |
| Example/gate collection | `tests/docs/test_subsea_agent_packs.py::test_all_example_and_gate_nodes_collect[rov]` | Every cited node resolves, including gate selectors |
| Inclusive screen | `tests/subsea/rov/test_agent_pack_examples.py::test_seastate_literals` | Caller-limit arithmetic |
| Temporal enumeration | `tests/subsea/rov/test_agent_pack_examples.py::test_serial_weather_literals` | Explicit task occupancy and waiting fraction |

Run `python -m pytest tests/subsea/rov` for the full sea-state, interface, lookup and
scheduler regressions. These software checks establish neither weather forecast
quality nor a safe LARS response limit. Motion-response applicability remains
NOT_EVALUATED; no new native-response benchmark is claimed.

## 6. Failure modes and silent assumptions

| ID / evidence | Finding | Detection / way forward |
|---|---|---|
| R1 [src/digitalmodel/subsea/rov/seastate_limits.py:L115-L125] | Standalone screen lacks finite hs/limit guards: infinite limit can pass finite Hs with zero utilisation; NaN can propagate. | Scheduler adds finite guards but direct screen does not; add direct invalid-domain tests and validation separately. |
| R2 [src/digitalmodel/subsea/rov/seastate_limits.py:L199-L215] | Scatter accepts NaN/infinite heights or weights through inequality guards; zero-weight accepted cells still count as operable. | Add finite guards; interpret counts separately from weighted percentage. |
| R3 [src/digitalmodel/subsea/rov/seastate_limits.py:L295-L317] | Response limit finite check and finite output check absent; 2*sigma is a single-amplitude convention, not peak or double amplitude. | Require finite positive limit/output and matching qualified response convention; no peak-load claim. |
| R4 [src/digitalmodel/subsea/rov/interface_checks.py:L104-L122] | Aligned boxes and spherical reach omit collisions, joint limits, tolerances, orientation variation and mating/seal performance. | A compatible flag is a geometry screen; separate full interface qualification. |
| R5 [src/digitalmodel/subsea/rov/tool_library.py:L73-L105] | Unspecified duty fields do not filter; kind-only match is allowed, and target interface/reach is never checked by lookup. | Call check_interface separately and supply all required duties. |
| R6 [src/digitalmodel/subsea/rov/intervention_sequence.py:L153-L176] | Fixed stable order is not a globally optimal schedule; oversized windows reserve forecast cover only. Equipment/interface labels are not validated against capacities. | Explicit sequence example; separately validate tool and interface capabilities and campaign objectives. |
| R7 [src/digitalmodel/subsea/rov/intervention_sequence.py:L160-L177] | Partial campaigns retain independent feasible tasks but expose no campaign aggregates; unused trailing samples excluded from waiting fraction. | Handle complete flag and explicit unscheduled reasons before consuming metrics. |
| R8 [src/digitalmodel/subsea/rov/interface_checks.py:L36-L62] [src/digitalmodel/subsea/rov/tool_library.py:L35-L58] [src/digitalmodel/subsea/rov/intervention_sequence.py:L38-L69] | Data classes themselves do not validate construction; validation happens at consumer calls. | Route records through check_interface, find_tools or plan_intervention before operational use. |
| R9 [src/digitalmodel/subsea/rov/__init__.py:L29-L61] [src/digitalmodel/subsea/rov/seastate_limits.py:L225-L259] | Motion-response result/function are public in submodule but absent from package exports. | Import explicitly from seastate_limits; inventory gate includes these definitions. |

## 7. Lessons log

| Date | Observation | Pack consequence |
|---|---|---|
| 2026-10-22 | Arithmetic regression evidence does not establish equipment or standards applicability. | Keep independent literals, input assumptions and NOT_EVALUATED engineering bases explicit. |

## 8. Ownership and review

Maintainers own API and documentation drift. Qualified intervention reviewers supply
ROV/tool/interface capabilities, LARS and weather limits, forecast allowance, time
resolution and task durations. Review must establish motion conventions and
operating approval separately; no approval is inferred from these deterministic flags.

## 9. Dependencies and other implementations

Sea-state, interface, tool lookup and scheduler use standard-library math, dataclasses,
enums and typing. The response path lazily imports NumPy and `motion_forecast`;
its optional runtime dependencies are not required by the scalar screens. Tests use
pytest and NumPy for response regressions; pack/registry checks use PyYAML and Git.
Use `PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio --with pyyaml --with numpy python -m pytest tests/subsea/rov -q -rA -p no:cacheprovider`
with temporary output outside the repository. No licensed solver is required.


The optional response calculation delegates to `motion_forecast.reconstruct_motion`,
`vertical_motion_at` and `time_derivative`. Sea-state scheduling delegates to the
same scalar screen, not an independently qualified weather model. Interface and tool
records are caller supplied and are not inferred from tree/connector catalogues.
The implementation map's rejected pattern matches for private validators do not
establish physical method membership. The new knowledge-pack index adds navigation
only. Manifold documentation is not on this code pin and is not cross-referenced.

## 10. Coverage and open items

Coverage inventory: all five package Python files and four existing test modules. Four new
examples cover scalar sea state, weighted count, reach equality and serial timing.
Resolve R1–R9 and verify API 17H, IMCA R 004 and DNV-ST-N001 edition/clause titles
and applicability. Public tool ratings, collision/joint models, approved forecast
limits, response convergence and a checked integrated intervention example remain
open. Tests and docs do not change the production implementation.

The [existing-pack compatibility audit](../../../../../docs/registry/agent-pack-drift.json)
records the unchanged baseline packs and collection limitations; its strict-schema
diagnostics require individual review before being classified as documentation defects.
