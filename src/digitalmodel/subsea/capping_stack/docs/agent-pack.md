# Capping-stack module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/capping_stack` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT; arithmetic evidence, engineering qualification incomplete |
| Analysis family | EC |
| Import | `digitalmodel.subsea.capping_stack` |

Summary: static pressure adequacy, nominal interface compatibility, a deployment
lift screen, constant-density IPR–VLP/choke intersections and elastic well
transients are implemented. Passing a boolean screen does not establish API
17W, API STD 53, API 17D or DNV-ST-N001 compliance. No licensed text or qualified
equipment data is supplied. Source qualification is explicitly bounded below.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Record pinned contracts, independent arithmetic and qualification gaps to support reproducible use. |

## 1. Purpose and scope

Use this pack to identify the actual input basis and limitations before composing
pressure, interface, lift and flow calculations. The static pressure check does
not predict a flowing rate. The staged model consumes an external transient
allowance; the separate native transient computes an elastic history. Neither is
a multiphase, compositional, thermal or cavitation model.
[src/digitalmodel/subsea/capping_stack/__init__.py:L4-L23]
[src/digitalmodel/subsea/capping_stack/containment_flow.py:L4-L44]

All source locators refer to the code pin. Test citations refer to the tests
shipped with this pack. The public inventory comprises package-defined functions,
classes and public members, including definitions accessible through submodules;
imported library types and numeric constants are not function definitions.

## 2. Inputs and outputs

No unit conversion is implicit except the pressure-class property and the
explicit gravity/depth conversion in the flow-to-pressure handoff. Constructors
below are plain dataclasses: annotations do not enforce runtime types or ranges.
Unless stated otherwise every parameter is required; finite checks in the flow
functions are provided by HIPPS helpers. All flow pressures are **absolute Pa**,
flow rates are **local m³/s**, and openings are capacity fractions, not travel.

| Public symbol | Inputs, units, defaults and validation | Output contract | Evidence |
|---|---|---|---|
| `ContainmentResult` | `siwhp`, `rated_working_pressure`, `hydrostatic_head` Pa; `utilisation` dimensionless; `is_contained` bool; `column_density` kg/m³; `notes=[]` per instance; no validation | Mutable result with the supplied fields | [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L60-L70] |
| `shut_in_wellhead_pressure` | `reservoir_pressure` Pa, `datum_to_mudline_tvd` m, `column_density=200` kg/m³ (UNVERIFIED assumption); no domain/finite validation | float Pa, clamped at zero | [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L73-L96] |
| `check_containment` | `rated_working_pressure` Pa must be >0 by comparison only; pressure/depth/density as preceding row, same default; others unchecked | `ContainmentResult`: head, clamped SIWHP, SIWHP/RWP, `siwhp<=RWP`, density and notes; invalid nonpositive RWP raises `ValueError` | [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L99-L152] |
| `PressureClass` | integer Enum values in psi; see constant qualification below | Enum member, not evidence of qualified equipment rating | [src/digitalmodel/subsea/capping_stack/interface_check.py:L37-L47] |
| `PressureClass.pascals` | property, no arguments; uses member integer and code conversion factor | float Pa; property access, not a method call | [src/digitalmodel/subsea/capping_stack/interface_check.py:L45-L47] |
| `ConnectorClass` | string Enum hub-family identifiers; no geometry or tolerance supplied | member of the encoded nominal families | [src/digitalmodel/subsea/capping_stack/interface_check.py:L50-L56] |
| `WellheadSpec` | `bore_inch` in; `pressure_class: PressureClass`, `connector_class: ConnectorClass`; no validation/defaults | Frozen nominal wellhead/tree record | [src/digitalmodel/subsea/capping_stack/interface_check.py:L59-L65] |
| `CappingStackSpec` | `bore_inch` in; pressure and connector enums as above; no validation/defaults | Frozen nominal stack-interface record | [src/digitalmodel/subsea/capping_stack/interface_check.py:L68-L74] |
| `InterfaceResult` | `compatible` bool; `reasons=[]` per instance; no validation | Mutable verdict/reason list | [src/digitalmodel/subsea/capping_stack/interface_check.py:L77-L82] |
| `check_compatibility` | `stack`, `wellhead` records; `bore_tolerance_inch=1/25.4` in (UNVERIFIED acceptance tolerance); no range/finite checks | `InterfaceResult`, compatible if pressure >= wellhead, absolute bore difference <= tolerance and connector enum identity matches; reasons accumulate | [src/digitalmodel/subsea/capping_stack/interface_check.py:L85-L130] |
| `CappingStack` | `mass_air` kg; `displaced_volume=None` m³; no validation | Frozen lift-property record; absent volume is estimated by `feasibility` | [src/digitalmodel/subsea/capping_stack/deployment.py:L53-L58] |
| `DeploymentScenario` | `crane_swl` N, `water_depth` m, `hs`, `hs_limit` m; `daf=2` dimensionless (UNVERIFIED); no validation | Frozen scenario; water depth is stored but unused | [src/digitalmodel/subsea/capping_stack/deployment.py:L61-L69] |
| `DeploymentResult` | `feasible` bool, `submerged_weight`, `hook_load` N, `lift_utilisation` dimensionless, `governing_constraint` string, `notes=[]`; no validation | Mutable result; governing string is lift, sea_state or none | [src/digitalmodel/subsea/capping_stack/deployment.py:L72-L81] |
| `submerged_weight` | `mass_air` kg, `displaced_volume` m³, `seawater_density=1025` kg/m³ (UNVERIFIED); no validation | float N, can be negative for buoyant input | [src/digitalmodel/subsea/capping_stack/deployment.py:L84-L90] |
| `feasibility` | `stack`, `scenario`; `seawater_density=1025` kg/m³; SWL <=0 rejected; other fields unchecked; missing volume becomes mass/4000 (UNVERIFIED kg/m³) | `DeploymentResult`: Wsub, DAF*Wsub, hook/SWL, simultaneous lift/sea verdict; lift takes priority if both constraints fail | [src/digitalmodel/subsea/capping_stack/deployment.py:L93-L146] |
| `FlowOperatingPoint` | `flow_rate_m3_s`, `wellhead_pressure_pa`, `bottomhole_pressure_pa`, `opening`, `ipr_residual_m3_s`, `choke_residual_m3_s`; no defaults/validation | Frozen SI result; residuals are rates, not pressure tolerances | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L59-L68] |
| `StagedContainment` | tuples `time_s`, `stages`, `transient_pressure_allowance_pa`; `peak_pressure_pa`, `peak_time_s`, `terminal_containment`, `peak_containment`; no defaults/validation | Frozen staged results and two pressure checks | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L71-L81] |
| `linear_inflow` | `pressure` Pa finite >=0, `reservoir_pressure` Pa finite >0, `productivity` m³/(s Pa) finite >0 | Local m³/s, PI*max(pres-p,0); reverse production clamped | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L90-L103] |
| `vogel_inflow` | pressure/reservoir checks as above; `maximum_rate` m³/s finite >0; no defaults | Local m³/s polynomial, ratio capped at one; empirical qualification UNVERIFIED | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L106-L120] |
| `gas_backpressure_inflow` | pressure/reservoir checks as above; `coefficient` m³/(s Pa^(2n)) finite >0; `exponent` finite in encoded [0.5,1] range (NOT_EVALUATED basis) | Local m³/s from pressure-squared law; overflow may escape validation | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L123-L144] |
| `solve_operating_point` | keyword-only `inflow(pbottom)` and `choke_capacity(pwellhead)` callbacks, `reservoir_pressure`, `column_density` kg/m³, `gravity` m/s², `bore_diameter` m all finite >0; `vertical_depth`, `bore_length` m, `friction_factor` Darcy, `outlet_pressure`, `opening` finite >=0; opening <=1. Callbacks must be continuous/monotonic and finite nonnegative; inflow at reservoir exactly zero; positive static pressure required. No defaults | `FlowOperatingPoint`; blocked/closed path returns static zero flow. `ValueError` on checked inputs or root/domain failure; callback exceptions propagate | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L154-L198] |
| `staged_shut_in` | keyword-only `time_s` nonempty, finite >=0 strictly increasing; `openings` same length, nonincreasing [0,1], end zero; `transient_pressure_allowance_pa` same length finite >=0; `rated_working_pressure` finite >0; `**well_inputs` exactly the operating-point arguments except opening. No defaults | `StagedContainment`; max(stage pressure + supplied allowance), first maximum time; terminal check is static SIWHP; peak check uses equivalent reservoir pressure. Allowances are never estimated | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L228-L269] |
| `TransientContainment` | `time_s`, `wellhead_pressure_pa`, `flow_rate_m3_s` tuples; peak pressure/time; terminal/peak `ContainmentResult`; no defaults/validation | Frozen computed history; peak covers stored wellhead samples, not every bore node | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L288-L298] |
| `transient_shut_in` | keyword-only stage times/openings as above but start at zero; `acoustic_speed` m/s, `duration` s, `rated_working_pressure` Pa finite >0; `cells` int >=2 excluding bool; `reverse_productivity` m³/(s Pa) finite >=0; `**well_inputs` as operating point. Positive initial flow and length; duration >= last knot and one grid step. No defaults | `TransientContainment`; grid rounds end up by <dt, nonpositive/nonfinite bore pressure raises `ValueError`; terminal check uses static SIWHP, not final oscillating pressure | [src/digitalmodel/subsea/capping_stack/containment_flow.py:L301-L353] |

Constants are deliberately outside the function table. These are descriptions
of the pinned implementation, not newly approved design values.

| Constant / encoded rule | Basis and qualification | Evidence |
|---|---|---|
| `G=9.80665` m/s² | NIST SP 811 (2008), Appendix B.8, standard acceleration of free fall; not a local gravity measurement | [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L52-L57] |
| `G=9.80665` m/s² in lift | Same NIST source | [src/digitalmodel/subsea/capping_stack/deployment.py:L48-L50] |
| GAS/OIL/BRINE densities `200/800/1030` kg/m³ | UNVERIFIED; assumed, based on previous experience; no property/pressure/temperature qualification | [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L54-L57] |
| Sea/bulk densities `1025/4000` kg/m³, DAF `2` | UNVERIFIED; assumed, based on previous experience; no DNV clause or sea-state derivation in hand | [src/digitalmodel/subsea/capping_stack/deployment.py:L36-L50] [src/digitalmodel/subsea/capping_stack/deployment.py:L61-L69] |
| `PSI_TO_PA=6894.757`, `INCH_TO_M=0.0254` | NIST SP 811 (2008), B.8 inch and pound-force/square-inch entries; psi conversion is rounded in code; inch constant is not used by the interface check | [src/digitalmodel/subsea/capping_stack/interface_check.py:L33-L34] |
| Pressure enum `5000/10000/15000/20000` psi; hub strings | NOT_EVALUATED: API 17W / API 17D edition and clause text unavailable; nominal membership cannot qualify physical hardware | [src/digitalmodel/subsea/capping_stack/interface_check.py:L37-L56] |
| Bore tolerance `1/25.4` in | Conversion gives one mm using NIST B.8; acceptance magnitude NOT_EVALUATED, no API clause basis in hand | [src/digitalmodel/subsea/capping_stack/interface_check.py:L85-L99] |

## 3. Method and governing sources

Sources below were opened on 2026-10-22. A readable source establishes only the
stated equation basis, not suitability for the well or standards compliance.
Values tagged UNVERIFIED or NOT_EVALUATED must not be adopted as qualified
engineering limits. User-supplied parameters remain required for flow methods.

| Method / equation or constant | Public source, edition and location | Disposition and implementation |
|---|---|---|
| Hydrostatic head `rho*g*h`; SIWHP `max(pres-head,0)`; U=SIWHP/RWP, verdict <=RWP | [DOE-HDBK-1012/3-92 (1992)](https://www.energy.gov/documents/doe-hdbk-1012-92vol3), HT-03 fluid statics and extended Bernoulli eq. 3-12; [NIST SP 811 (2008)](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf), B.8 for g | Hydrostatic algebra sourced; zero clamp and rating comparison are code policy. API 17W/API STD 53 clause qualification NOT_EVALUATED. [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L95-L151] |
| Pressure, bore and connector matching | No readable API 17D/17W edition/clause establishing these nominal rules | NOT_EVALUATED for interface adequacy; enum identity and nominal bore cannot establish seal/lock integrity. [src/digitalmodel/subsea/capping_stack/interface_check.py:L109-L130] |
| Wsub=(mass-rho*V)*g; hook=DAF*Wsub; U=hook/SWL; Hs<=limit and U<=1 | DOE-HDBK-1012/3-92, HT-03 §Buoyancy, p.2; NIST B.8 for g. DAF multiplication and combined acceptance are implementation policy | Buoyancy algebra only; DAF, density defaults, SWL, sea-state limits and DNV-ST-N001 edition/clause qualification NOT_EVALUATED. [src/digitalmodel/subsea/capping_stack/deployment.py:L84-L146] |
| Linear PI `q=PI*max(pres-pbottom,0)` | [TR94-12, Multiphase Flow Production Model (1994)](https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/300ae.pdf), §2.4.1 eq. 2-7 | Equation form sourced; local-rate PI must be qualified by caller; clamp is code policy. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L90-L103] |
| Polynomial inflow `q=qmax*(1-0.2*r-0.8*r²)`, r=min(p/pres,1) | TR94-12 §2.4.2 is a candidate public source; equation coefficients not independently transcribed here | UNVERIFIED; coefficients, range and local-volume conversion NOT_EVALUATED. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L106-L120] |
| Gas backpressure `q=C*max(pres²-p²,0)^n`; encoded n in [0.5,1] | TR94-12 §2.4.3 is a candidate; source describes a standard gas-rate basis, unlike local rate here | UNVERIFIED for this implementation; exponent bounds, coefficient units and local gas conversion NOT_EVALUATED. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L123-L144] |
| Area=pi*D²/4; VLP `p_bottom=p_wh+rho*g*h+f*(L/D)*rho*(q/A)²/2` | DOE-HDBK-1012/3-92 HT-03 eqs. 3-1, 3-12, 3-14; circular area definition supplies pi/4, kinetic head supplies divisor 2 | Single-density Darcy form sourced; friction is input, not a correlation/default. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L189-L225] |
| Nested root: IPR=flow and opening*capacity=flow | [NIST DLMF (2010), version 1.2.8, §3.8(iii), Bisection Method](https://dlmf.nist.gov/3.8.iii) | Interval halving (divisor 2), finite brackets and sign preservation; floating-point stagnation is numerical termination, not a physical tolerance. Continuity/monotonicity remain caller preconditions. [src/digitalmodel/subsea/capping_stack/_root_finding.py:L17-L52] |
| Staged peak=max(pstage+allowance); equivalent depth=h*g/G; equivalent reservoir=peak+rho*g*h | Algebraic maximum and hydrostatic identity using DOE eq. 3-12 | Arithmetic only. Allowances and physical conservatism NOT_EVALUATED. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L250-L285] |
| Native characteristics `p±Zq`, Z=rho*a/A, dt=(L/cells)/a; Darcy loss `f*dx/D*rho*q*abs(q)/(2*A²)`; interior half-sums/differences | Characteristic source, edition and clause NOT_EVALUATED; Darcy component follows DOE eq. 3-14 | UNVERIFIED for coupled transient; uniform grid, linearly interpolated opening, production/injection boundary and positive-pressure exit are code algorithms. Numerical 2 and 4 factors follow characteristic algebra and area definition; engineering qualification remains open. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L356-L438] |
| Choke callbacks, reverse PI, acoustic speed, opening knots | Explicit input; no default empirical constants | NOT_EVALUATED until caller supplies qualified data; capacity basis must be local m³/s. [src/digitalmodel/subsea/capping_stack/containment_flow.py:L154-L180] [src/digitalmodel/subsea/capping_stack/containment_flow.py:L301-L335] |

## 4. Worked examples and regression examples

All examples are synthetic independent hand calculations, not qualified designs.
Inputs are specified here rather than inferred from equipment or fluid tables.
The tests assert the literal answers below using `pytest.approx` (ordinary
floating-point comparison); residual checks use an explicit numerical tolerance.

| Example | Inputs, independent method and answer | Executable literal check |
|---|---|---|
| Static pressure | pres=10000000 Pa, rho=200 kg/m³, h=1000 m, RWP=10000000 Pa. NIST g gives head=200*9.80665*1000=1961330 Pa. SIWHP=10000000-1961330=8038670 Pa; below RWP. DOE hydrostatics/NIST basis in §3 | `tests/subsea/capping_stack/test_agent_pack_examples.py::test_pressure_hand_calculation` |
| Lift | mass=200000 kg, V=100 m³, rho=1000 kg/m³, DAF=1, SWL=1961330 N, Hs=1 m, limit=2 m. W=(200000-1000*100)*9.80665=980665 N; hook=980665 N; U=0.5, both comparisons pass. Buoyancy/NIST basis in §3; all design factors synthetic | `tests/subsea/capping_stack/test_agent_pack_examples.py::test_lift_hand_calculation` |
| IPR–VLP/choke | pres=1000 Pa, rho=10 kg/m³, g=10 m/s², h=1 m; head=100 Pa. PI=0.01 m³/(s Pa), f=0, L=D=1 m, outlet=100 Pa, opening=1; specified algebraic choke q=0.01*(p-100). TR94-12 linear PI gives q=0.01*(900-p). Equating yields 900-p=p-100, p=500 Pa, bottom=600 Pa, q=4 m³/s. Choke is a manufactured linear callback | `tests/subsea/capping_stack/test_agent_pack_examples.py::test_operating_point_hand_calculation` |
| Staged bound governs | Same well, times=(0,1) s, openings=(1,0), external allowances=(500,0) Pa, RWP=950 Pa. Initial pressure=500 Pa; terminal=1000-100=900 Pa. max(500+500,900+0)=1000 Pa at zero seconds. Terminal passes; bounded peak fails. Maximum is hand algebra, not a surge prediction | `tests/subsea/capping_stack/test_agent_pack_examples.py::test_staged_peak_hand_calculation` |

## 5. QA gates and executable checks

| Gate | Executable check | What it establishes |
|---|---|---|
| Independent arithmetic | The four section-4 nodes | Reproduce explicit synthetic arithmetic; no standards qualification |
| Pressure boundary | `tests/subsea/capping_stack/test_capping_stack.py::test_containment_boundary_exactly_unity` | Equality passes; clamp and invalid-rating tests remain in the same suite |
| Nominal mismatch | `tests/subsea/capping_stack/test_capping_stack.py::test_interface_bore_and_connector_mismatch` | Both mismatch reasons recorded |
| Friction and closed state | `tests/subsea/capping_stack/test_containment_flow_slice1.py::test_frictional_ipr_vlp_intersection_has_independent_expected_values` | Independent frictional equilibrium, separate from zero-friction example |
| Native transient regression | `tests/subsea/capping_stack/test_containment_flow.py::test_native_first_step_matches_joukowsky_at_valve` | First-step elastic response only; source qualification open |
| Root bracket | `tests/subsea/capping_stack/test_containment_flow_slice1.py::test_sign_product_underflow_must_not_accept_an_invalid_bracket` | Invalid signs cannot be hidden by multiplying subnormal values |
| Inventory and citations | `tests/docs/test_agent_pack_functions.py::test_new_packs_match_code`; `tests/subsea/capping_stack/test_agent_pack_examples.py::test_pack_node_ids_collect` | AST inventory and collection of examples plus gates |

Direct inventory command:
`python scripts/docs/check_agent_pack_functions.py --pack src/digitalmodel/subsea/capping_stack/docs/agent-pack.md`.
Run all of `tests/subsea/capping_stack`, the node-ID collection and registry tests
before revising this pack. Collection alone never proves that an assertion passes.

## 6. Known failure modes and pitfalls

These are observations at the pin; this pack changes no calculation code.

| File and lines | Failure mode / effect | Required treatment or follow-up |
|---|---|---|
| [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L95-L96] | No finite, sign or unit checks before hydrostatic subtraction and clamp; a negative reservoir/depth/density can create a misleading result | Validate inputs outside this implementation; add domain checks separately |
| [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L128-L141] | RWP is only compared to zero; nonfinite RWP and unchecked density bypass meaningful engineering checks. Gas-default note is not a property model | Supply qualified RWP and the lightest credible column explicitly |
| [src/digitalmodel/subsea/capping_stack/interface_check.py:L85-L130] | No finite bore/tolerance validation; NaN bore makes mismatch comparison false and may allow `compatible=True` with matching enums | Reject nonfinite geometry separately; resolve seal and connector qualification |
| [src/digitalmodel/subsea/capping_stack/deployment.py:L115-L130] | Missing volume silently estimated; no mass, density, DAF, Hs or limit domain checks; negative buoyant weight can pass the lift comparison | Provide explicit displaced volume and qualified factors; validate domains |
| [src/digitalmodel/subsea/capping_stack/deployment.py:L61-L69] [src/digitalmodel/subsea/capping_stack/deployment.py:L123-L146] | Water depth is unused; lift screen cannot establish reach, line load or seabed accessibility | Separate deployment feasibility and dynamics work |
| [src/digitalmodel/subsea/capping_stack/containment_flow.py:L101-L144] | Finite input validation does not guarantee finite products or squared pressures; overflow can propagate | Numerical range qualification remains open |
| [src/digitalmodel/subsea/capping_stack/containment_flow.py:L189-L225] | Continuity/monotonicity not checked; blocked/closed return sets both residuals to zero without evaluating choke. Local and standard gas volume can be confused | Respect callback preconditions; zero residual is not independent physics evidence |
| [src/digitalmodel/subsea/capping_stack/containment_flow.py:L250-L269] | Supplied allowances alone govern staged peak; zero allowance is permitted and can omit real surge | Qualify the entire bound; use native transient only within its narrower assumptions |
| [src/digitalmodel/subsea/capping_stack/containment_flow.py:L328-L335] [src/digitalmodel/subsea/capping_stack/containment_flow.py:L428-L438] | Closure faster than dt is unresolved; no vapor-pressure margin or cavitation model; reported peak samples only the wellhead | Grid study and qualified phase margin needed; distinguish bore peak from wellhead peak |
| [src/digitalmodel/subsea/capping_stack/containment_capacity.py:L38-L42] | Static docstring still defers a transient model that exists in containment_flow | Reconcile description in a separate change |

## 7. Lessons log

Pressure containment, flowing capacity, mechanical compatibility and deployment
are different evidence questions. A common boolean output does not make their
assumptions interchangeable. A staged maximum with supplied allowances and a
computed native transient must remain distinguishable. Numerical convergence
and passing synthetic arithmetic are prerequisites, not qualification receipts.

## 8. Ownership

The subsea maintainers curate the code, pack and registry. Engineering review is
needed for pressure integrity, well-control assumptions, connector qualification
and lifting. No individual or external equipment approval is assigned here.

## 9. Dependencies and other implementations

The AST inventory checker uses only Python's standard library; it never imports
this package. Runtime dependencies are `dataclasses`, `enum`, `math`, typing and
collections abstractions, plus package-local imports. The only cross-package
runtime import found in this package is HIPPS validation helpers
`_nonnegative` and `_positive`; actual choke sizing is a caller callback, not an
automatic HIPPS capacity import.
[src/digitalmodel/subsea/capping_stack/containment_flow.py:L49-L56]

Grep of `hipps` throughout the source found pipeline pressure sizing's optional
`hipps_setpoint_MPa` screen. It compares setpoint to design pressure and reports
a margin; **it does not import the HIPPS package or reduce wall-sizing pressure**.
No automatic surge or response-time coupling is present there.
[src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py:L270-L310]

Grep of imports in capping_stack found no imports of the tree package or a
wellhead package. `WellheadSpec` is defined locally; its docstring allows a
nominal tree interface. There is no source package named wellhead at this pin.
The tree package imports its own design_checks, flow_capacity, pressure_control
and tree_catalog modules; no capping-stack coupling was found. Thus the relation
is conceptual interface data, with integration still deferred in #484, rather
than an implemented dependency.
[src/digitalmodel/subsea/capping_stack/interface_check.py:L59-L74]
[src/digitalmodel/subsea/capping_stack/__init__.py:L20-L23]
[src/digitalmodel/subsea/trees/__init__.py:L23-L42]

The implementation-map entries for `_validate_stages`, `transient_shut_in` and
`feasibility` are search-pattern observations, not method qualification. This
pack is separately registered without regenerating that historical inventory.

## 10. Coverage and open items

All defined public classes/functions and the public enum property are inventoried
in section 2. Numeric exports and module constants are separately qualified.
All section-4 answers have literal tests; section-5 nodes are collected too.
Coverage is functional inventory, not a line-coverage claim.

Open: API 17W, API STD 53, API 17D and DNV-ST-N001 edition/clause qualification;
column and deployment assumptions; finite-domain validation in static screens;
polynomial/gas inflow source transcription; transient source and boundary
qualification; phase changes, compositional gradients, connector geometry,
ROV/current/line dynamics and tree integration. HIPPS valve-sizing corrections
#2835 and #3174 are in flight at pack preparation; this pin intentionally
predates those branches. Re-pin and rerun both inventories and worked examples
before adopting their numerical behavior. No standards-compliance claim is made.
