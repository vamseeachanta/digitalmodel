# HIPPS module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/hipps` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT; arithmetic evidence, engineering qualification incomplete |
| Analysis family | EC |
| Import | `digitalmodel.subsea.hipps` |

Summary: low-demand PFD arithmetic and SIL-band classification, a linear
closure-pressure screen, single-reach liquid characteristics and caller-factor
valve sizing are available. API 17O, IEC 61508 and IEC 61511 qualification is
unfinished. A returned SIL enum is an encoded band, not an achieved system SIL;
a passed envelope is conditional on a qualified pressure-rise bound.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Record pinned contracts, independent arithmetic and qualification gaps to support reproducible use. |

## 1. Purpose and scope

This pack describes code behavior at the pin, including native liquid surge and
coefficient-based valve sizing. It does not establish functional-safety,
architectural, installation or equipment compliance. FMEDA roll-up, multiphase
transients and standards qualification are deferred.
[src/digitalmodel/subsea/hipps/__init__.py:L4-L27]

Test citations refer to the tests shipped with this pack. The inventory covers
public functions/classes defined in the package, including submodule APIs;
imported library types and constants are not additional public definitions.
All dimensional and empirical inputs must have a caller-qualified basis.

## 2. Inputs and outputs

No runtime unit system is enforced. Reliability uses hours, the envelope uses
one consistent caller pressure/time basis, surge uses SI absolute pressure, and
valve units are determined by N1/N6. Never pass a gas mass rate as a liquid/local
volume rate. Frozen result records have no constructor validation; defaults and
all function validation are stated below. Unstated parameters are required.

| Public symbol | Inputs, units, defaults and validation | Output contract | Evidence |
|---|---|---|---|
| `SIL` | IntEnum values NONE=0, SIL1=1, SIL2=2, SIL3=3, SIL4=4; IEC basis UNVERIFIED | Encoded band member, no architectural proof | [src/digitalmodel/subsea/hipps/sil_analysis.py:L48-L55] |
| `SILResult` | `pfd_avg` dimensionless, `sil` enum, `rrf` dimensionless; `target_sil=None`, `meets_target=None`; no checks | Frozen record; absent target leaves both optional fields None | [src/digitalmodel/subsea/hipps/sil_analysis.py:L68-L90] |
| `risk_reduction_factor` | `pfd_avg` in (0,1], rejects invalid comparisons including NaN/Inf; no default | float reciprocal; `ValueError` outside interval; tiny valid values can overflow output | [src/digitalmodel/subsea/hipps/sil_analysis.py:L93-L103] |
| `pfd_avg_1oo1` | `lambda_du` dangerous-undetected failures/h, `ti` h; each >0 by comparison; no finite or small-rate condition; no defaults | Dimensionless simplified average PFD; can exceed one or be nonfinite | [src/digitalmodel/subsea/hipps/sil_analysis.py:L106-L120] |
| `pfd_avg_1oo2` | `lambda_du`, `ti` as above; `beta=0` dimensionless, checked [0,1]; rates/time not checked finite | Dimensionless independent term plus common-cause term; not a full SIF calculation | [src/digitalmodel/subsea/hipps/sil_analysis.py:L123-L144] |
| `classify_sil` | `pfd_avg` in (0,1]; `target_sil=None` or SIL/int convertible through `SIL(int(...))`; no exact-integer check | `SILResult`; >=0.1 -> NONE, <0.00001 -> capped SIL4, other encoded bands below; RRF reciprocal; target pass is integer band comparison | [src/digitalmodel/subsea/hipps/sil_analysis.py:L147-L198] |
| `OverpressureResult` | `t_response` input time unit; `p_at_close`, `margin` input pressure unit; `utilisation` dimensionless; `passed` bool; no defaults/checks | Frozen envelope result | [src/digitalmodel/subsea/hipps/overpressure.py:L45-L67] |
| `overpressure_envelope` | `p_initial`, `p_design`, `dpdt` pressure/time, `t_close`; `t_detect=0`, `t_logic=0`; p_design>0, dpdt/times>=0 by comparisons only; p_initial unchecked | `OverpressureResult` with t=sum delays, closure pressure, margin, ratio and <=design verdict; `ValueError` for checked negative domains | [src/digitalmodel/subsea/hipps/overpressure.py:L70-L126] |
| `SurgeResult` | tuples `time_s`, `valve_pressure_pa`, `valve_flow_m3_s`; `peak_pressure_pa`, `dpdt_pa_s`, `time_step_s`; no defaults/checks | Frozen valve history; peak is over all grid nodes, dpdt only sampled positive valve slope | [src/digitalmodel/subsea/hipps/pressure_surge.py:L37-L46] |
| `wave_speed` | keyword-only `density` kg/m³, `bulk_modulus` Pa, `diameter` m, `wall_thickness` m, `elastic_modulus` Pa finite >0; `restraint_factor` finite >=0 | float m/s from combined compliance; intermediate overflow/underflow not normalized | [src/digitalmodel/subsea/hipps/pressure_surge.py:L49-L72] |
| `joukowsky_rise` | `density` kg/m³, `acoustic_speed` m/s finite >0; `velocity_change` m/s finite >=0 | float Pa from density*speed*reduction; return not checked finite | [src/digitalmodel/subsea/hipps/pressure_surge.py:L75-L88] |
| `simulate_surge` | keyword-only `length`, `diameter` m, `density` kg/m³, `acoustic_speed` m/s, `initial_flow` m³/s, `reservoir_pressure` Pa absolute, `duration` s finite >0; `downstream_pressure` Pa and `closure_time` s finite >=0; `cells` int>=2 excluding bool; reservoir>downstream; duration>=dt. `characteristic='linear'`; `rangeability=None`, required finite >1 for equal_percentage | `SurgeResult`; dt=L/(cells*a), end rounds up by <dt; invalid grid/domain -> `ValueError`; positive absolute pressure required at every node; step closure permitted but sampled slope is grid-dependent | [src/digitalmodel/subsea/hipps/pressure_surge.py:L101-L147] |
| `ValveCapacity` | `rate` caller flow/mass units, `effective_drop` caller pressure units, `choked` bool; no defaults/checks | Frozen capacity result; gas effective_drop is pressure, not ratio | [src/digitalmodel/subsea/hipps/valve_sizing.py:L32-L38] |
| `liquid_capacity` | keyword-only `coefficient` Cv/Kv >=0; `p_upstream` absolute >0, `p_downstream` absolute >=0 <=upstream; `specific_gravity`, `unit_constant` N1, `piping_factor` Fp, `recovery_factor` FLP, `critical_pressure_ratio` FF all finite >0; `vapor_pressure` finite >=0 <upstream. All nonnegative inputs also finite. Positive head and representable positive choking limit required. No defaults | `ValveCapacity`: volumetric rate per N1, min(actual, choking drop), choked if actual>=limit; scaled limit calculation rejects unrepresentable final limits; final rate not checked finite | [src/digitalmodel/subsea/hipps/valve_sizing.py:L55-L109] |
| `required_liquid_coefficient` | keyword-only `rate` finite >=0 in N1 units; `**inputs` all liquid_capacity inputs except coefficient; no defaults | float Cv/Kv holding factors fixed; zero demand returns zero after unit-capacity evaluation; positive demand/zero capacity raises `ValueError`; not a catalogue iteration | [src/digitalmodel/subsea/hipps/valve_sizing.py:L112-L131] |
| `gas_mass_capacity` | keyword-only `coefficient` finite >=0; `p_upstream`, `density`, `unit_constant` N6, `piping_factor`, `choked_ratio` finite >0; `p_downstream` finite >=0 <=upstream; choked_ratio<=1. Absolute pressure and density units consistent with N6; no defaults | `ValveCapacity`: **mass** rate, effective pressure drop and choking verdict; no final finite check | [src/digitalmodel/subsea/hipps/valve_sizing.py:L134-L166] |
| `stroke_time` | keyword-only `stroke_length` m or rad and `actuator_speed` matching units/s, both finite >0 | seconds, excludes detection/logic; ratio return not checked finite | [src/digitalmodel/subsea/hipps/valve_sizing.py:L169-L180] |
| `closure_fraction` | `elapsed`, `closure_time` finite >=0 s; `characteristic` linear/equal_percentage; keyword-only `rangeability=None`, required finite >1 for equal_percentage even after closure; linear ignores rangeability | Dimensionless remaining conductance; step closure if time=0; linear 1-elapsed/time, equal-percentage R^(-elapsed/time) until full closure then exactly zero | [src/digitalmodel/subsea/hipps/valve_sizing.py:L183-L215] |

## 3. Method and governing sources

The public handbook was opened on 2026-10-22, and its equation pages 113 and 117
were read. Standard titles in docstrings are not evidence of clause compliance.
No licensed standard values are newly supplied here. UNVERIFIED equations below
are described so callers can identify the implementation, not adopt it as a
qualified engineering method.

| Method / every encoded coefficient or limit | Public source, edition and clause | Disposition / evidence |
|---|---|---|
| 1oo1 `lambda*ti/2`; 1oo2 `((1-beta)*lambda)^2*ti²/3+beta*lambda*ti/2`; beta default zero, allowed [0,1] | Code cites IEC 61508-6 Annex B, B.3.2.2.2 and Annex D; edition/clause text not in hand | UNVERIFIED; repair time, full proof-test coverage and approximation range not modeled. Divisors 2 and 3 are recorded code constants, source qualification NOT_EVALUATED. [src/digitalmodel/subsea/hipps/sil_analysis.py:L17-L39] [src/digitalmodel/subsea/hipps/sil_analysis.py:L106-L144] |
| RRF=1/PFD; PFD domain (0,1] | Reciprocal definition in code; IEC 61508-5 attribution not independently qualified | Arithmetic definition only, functional-safety qualification NOT_EVALUATED. [src/digitalmodel/subsea/hipps/sil_analysis.py:L93-L103] |
| SIL bounds 0.01,0.001,0.0001,0.00001; upper bound 10*lower; >=0.1 gives NONE; below floor capped SIL4; integer levels 0..4 | Code cites IEC 61508-1:2010 Table 2 / IEC 61511-1:2016 Table 4; readable table not in hand | UNVERIFIED table attribution, no compliance claim. Capping below floor is explicit code policy. [src/digitalmodel/subsea/hipps/sil_analysis.py:L48-L65] [src/digitalmodel/subsea/hipps/sil_analysis.py:L169-L197] |
| Response=sum(detect,logic,close); p_close=p_initial+dpdt*response; margin=design-p_close; U=p_close/design; pass<=design; default detect/logic zero | Constant-slope arithmetic; code cites API 17O/IEC 61511 without qualified edition/clause | NOT_EVALUATED as a safety envelope. A finite sampled slope is not a proved worst-case bound. Delays default to zero as implementation policy. [src/digitalmodel/subsea/hipps/overpressure.py:L106-L126] |
| Compliance `1/(rho*a²)=1/K+restraint*D/(E*t)`; pressure rise `rho*a*dv` | Public source/edition/clause not established for this implementation | UNVERIFIED. Geometry, properties and restraint are required, not default constants. Thin-wall support and liquid applicability need qualification. [src/digitalmodel/subsea/hipps/pressure_surge.py:L49-L88] |
| Liquid choking limit `(FLP/Fp)²*(p1-FF*pv)`; dp=min(actual,limit); q=N1*Fp*C*sqrt(dp/SG); inverse C=q/unit_capacity | [Emerson, Control Valve Handbook, sixth edition (2023)](https://www.emerson.com/is/content/emerson/en/final-control/flow-controls/documents/d101881x012.pdf), §§5.8.2–5.8.3, p.113; applicability §5.5; worked example §5.8.4 | Equation form sourced. N1, Fp, FLP, FF and SG are **required inputs** with no tabulated values supplied here. Scaled binary exponent algorithm preserves the same limit. Equality-choked flag is code policy; equipment/applicability qualification NOT_EVALUATED. [src/digitalmodel/subsea/hipps/valve_sizing.py:L55-L131] |
| Gas x=min((p1-p2)/p1,x_choked); Y=1-x/(3*x_choked); w=N6*Fp*C*Y*sqrt(x*p1*rho) | Same handbook §§5.9.2–5.9.3, p.117 (including expansion divisor 3) | Equation form sourced; N6 and choking ratio remain qualified caller inputs. Encoded 0<x_choked<=1 is the implementation domain, not an equipment acceptance rule. [src/digitalmodel/subsea/hipps/valve_sizing.py:L134-L166] |
| Stroke time=travel/speed | Definition of uniform speed; no equipment-response source in hand | Arithmetic identity; measured loaded speed and delay basis NOT_EVALUATED. [src/digitalmodel/subsea/hipps/valve_sizing.py:L169-L180] |
| Linear conductance 1-t/T; equal-percentage R^(-t/T), then zero at t>=T; R>1; T=0 means step | Idealized laws in code; no published dynamic-travel/installed-flow evidence qualified here | UNVERIFIED; equal-percentage law has a seating discontinuity. Fractions zero/one are normalized endpoints, not licensed acceptance limits. [src/digitalmodel/subsea/hipps/valve_sizing.py:L183-L215] |
| A=pi*D²/4; Z=rho*a/A; dt=L/(cells*a); p±Zq invariants, half-sum/difference intersections, fixed reservoir, one-way sqrt valve | Characteristic derivation and boundary source/edition/clause NOT_EVALUATED | UNVERIFIED native transient. Numerical 2 and 4 follow the implemented intersections/circular area; cells>=2 is a numerical grid requirement. Conductance=initial_flow/sqrt(reservoir-downstream). [src/digitalmodel/subsea/hipps/pressure_surge.py:L91-L98] [src/digitalmodel/subsea/hipps/pressure_surge.py:L130-L182] |
| Peak=max(all grid pressures); dpdt=max(0,positive sampled valve slopes); reject pressure<=0 | Implementation reduction and domain policies, not standard acceptance thresholds | NOT_EVALUATED as a physical safety bound or cavitation screen. [src/digitalmodel/subsea/hipps/pressure_surge.py:L155-L182] |

## 4. Worked examples and regression examples

Synthetic values below are independent arithmetic fixtures. They are not fluid,
failure-rate, equipment or protection-design recommendations. Tests contain the
full hand derivation and literal unrounded target where a repeating number occurs.
`pytest.approx` is a numerical comparison only, not an engineering tolerance.

| Example | Inputs, independent method and answer | Executable literal check |
|---|---|---|
| PFD arithmetic, UNVERIFIED method | lambda=0.000002/h, ti=1000 h, beta=0.5. 1oo1=0.000002*1000/2=0.001. 1oo2 independent term=(0.5*0.000002)²*1000²/3=0.000001/3; common term=0.5*0.000002*1000/2=0.0005. Total=(0.000001/3)+0.0005; test stores the literal unrounded sum. Source gap is §3; this checks algebra only | `tests/subsea/hipps/test_agent_pack_examples.py::test_pfd_hand_calculation` |
| Closure envelope, NOT_EVALUATED safety basis | Pressures MPa, time s: p_initial=10, design=20, dpdt=2, detect=0.5, logic=0.5, stroke=2. Response=0.5+0.5+2=3 s; closure=10+2*3=16 MPa; margin=20-16=4 MPa; U=16/20=0.8, passes. Constant-slope hand algebra | `tests/subsea/hipps/test_agent_pack_examples.py::test_envelope_hand_calculation` |
| Liquid choking and inversion, sourced equation | Consistent synthetic units: p1=100,p2=0,SG=1,N1=Fp=FF=1,FLP=0.5,pv=0,C=2. Handbook §5.8.2 gives limit=(0.5/1)²*(100-1*0)=25. Effective=min(100,25)=25; §5.8.3 gives q=1*1*2*sqrt(25/1)=10. Unit-coefficient rate=5; inverse C=10/5=2. No handbook table value used | `tests/subsea/hipps/test_agent_pack_examples.py::test_liquid_hand_calculation` |
| Closure-law arithmetic, UNVERIFIED dynamics | Travel=0.3 m, speed=0.1 m/s -> 3 s. Linear law at t=1,T=2 s ->1-1/2=0.5. Equal-percentage R=100 ->100^(-1/2)=0.1, then zero at t=2. Specified mathematical laws, not measured stroke performance | `tests/subsea/hipps/test_agent_pack_examples.py::test_closure_hand_calculation` |
| Encoded classification boundary, UNVERIFIED table | PFD=0.001 is the encoded SIL2 lower endpoint and RRF=1/0.001=1000; target=2 passes the code's band comparison. No full-SIF capability inference | `tests/subsea/hipps/test_agent_pack_examples.py::test_classification_literal` |

## 5. QA gates and executable checks

| Gate | Executable check | Scope |
|---|---|---|
| Independent arithmetic | The five section-4 nodes | Literal synthetic answers, with source status retained |
| Band boundaries | `tests/subsea/hipps/test_sil_analysis.py::test_band_boundaries` | Encoded table endpoints; does not qualify standard attribution |
| Closure boundary | `tests/subsea/hipps/test_overpressure.py::test_boundary_exactly_at_design_passes` | Code equality acceptance |
| Scaled choking limit | `tests/subsea/hipps/test_valve_sizing.py::test_decimal_intermediate_underflow_and_inverse` | Intermediate scaling preserves a representable final drop |
| Liquid surge regression | `tests/subsea/hipps/test_flow_hydraulics.py::test_moc_step_closure_reproduces_joukowsky_bound` | Existing analytical-response regression, not a safety-bound qualification |
| Cross-workflow regression | `tests/subsea/hipps/test_flow_hydraulics.py::test_hipps_workflow_setpoint_screen_preserves_wall_sizing` | Setpoint screen gives no wall-sizing credit |
| Inventory and citation collection | `tests/docs/test_agent_pack_functions.py::test_new_packs_match_code`; `tests/subsea/hipps/test_agent_pack_examples.py::test_pack_node_ids_collect` | Public definition locators; collect examples and gates |

Direct inventory command:
`python scripts/docs/check_agent_pack_functions.py --pack src/digitalmodel/subsea/hipps/docs/agent-pack.md`.
Run the full `tests/subsea/hipps` suite, documentation citation collection and
registry consistency tests before re-pinning. Collection does not execute a gate.

## 6. Known failure modes and pitfalls

The following defects/limitations are recorded without modifying calculation code.

| File and lines | Defect or limitation / effect | Follow-up |
|---|---|---|
| [src/digitalmodel/subsea/hipps/sil_analysis.py:L116-L144] | NaN/Inf rate/time can bypass <=0 checks; products may exceed one or overflow; no small-rate/proof-test applicability check | Add finite-domain and model-applicability validation separately |
| [src/digitalmodel/subsea/hipps/sil_analysis.py:L169-L197] | Below-floor PFD capped at SIL4; target coerced with int(), so fractional values can be truncated | Validate exact target values; retain band-only interpretation |
| [src/digitalmodel/subsea/hipps/sil_analysis.py:L101-L103] | Positive subnormal PFD can yield infinite reciprocal | Qualify representable output range |
| [src/digitalmodel/subsea/hipps/overpressure.py:L106-L118] | No finite checks and no domain check on initial pressure; infinite design can pass, negative initial pressure can pass; NaN may return unusable results without raising | Validate finite pressure/time inputs and qualified rise bound |
| [src/digitalmodel/subsea/hipps/overpressure.py:L32-L34] | Docstring says the characteristic solver is deferred although pressure_surge exists | Reconcile stale description separately |
| [src/digitalmodel/subsea/hipps/valve_sizing.py:L104-L109] [src/digitalmodel/subsea/hipps/valve_sizing.py:L125-L131] | Final liquid-rate product/inverse is not finite-checked; intermediate underflow can give zero, overflow can give infinity or NaN, including zero demand during unit-capacity evaluation | Corrections are in flight in #2835 and #3174; this pack stays pinned |
| [src/digitalmodel/subsea/hipps/valve_sizing.py:L160-L166] | Gas product and square-root intermediate may overflow; coefficient zero does not guarantee an exact zero when an intermediate is infinite | Add gas finite-output qualification separately |
| [src/digitalmodel/subsea/hipps/valve_sizing.py:L208-L215] | Equal-percentage law jumps from the nonzero seating limit to zero; generic conductance is not installed flow or measured travel | Qualify actual valve/actuator closure profile |
| [src/digitalmodel/subsea/hipps/pressure_surge.py:L69-L72] | Finite inputs can produce zero/infinite intermediate compliance; arithmetic exceptions or unphysical speed can escape | Add representability checks and support-model qualification |
| [src/digitalmodel/subsea/hipps/pressure_surge.py:L130-L147] [src/digitalmodel/subsea/hipps/pressure_surge.py:L175-L182] | Grid slope depends on dt, step closure has no finite physical slope; positivity is not a vapor-pressure margin | Never treat sampled dpdt as a qualified worst-case envelope without additional evidence |
| [src/digitalmodel/subsea/hipps/pressure_surge.py:L155-L182] | Friction, gas, column separation and cavitation absent; time can round beyond requested duration; only valve history returned though peak covers all nodes | Grid and applicability studies needed; retain result-field distinctions |

## 7. Lessons log

Keep probability classification separate from system safety qualification. Keep
mass flow, local volumetric flow and standard volumetric flow distinct. A finite
grid slope from an instantaneous closure cannot bound the physical derivative.
Required sizing factors avoid invented equipment data but do not validate caller
values or intermediate floating-point range.

## 8. Ownership

The subsea maintainers curate the code and pack. Reliability, hydraulic transient
and valve-application review remain separate engineering responsibilities. No
certification authority or equipment approval is implied by the module exports.

## 9. Dependencies and other implementations

Runtime imports are the standard library plus intra-package references:
pressure_surge imports `_nonnegative`, `_positive` and `closure_fraction` from
valve_sizing. The package initializer re-exports the listed definitions.
[src/digitalmodel/subsea/hipps/pressure_surge.py:L29-L34]
[src/digitalmodel/subsea/hipps/__init__.py:L30-L67]

Grep of `hipps` in the full source found the pipeline pressure-sizing workflow's
optional `hipps_setpoint_MPa`. It validates a positive finite setpoint and design
pressure, compares setpoint<=design and returns `hipps_setpoint_check`. It **does
not import HIPPS functions**, run a surge/closure envelope, or reduce the design
pressure used in wall sizing. The relationship is a configuration screen rather
than a runtime package dependency.
[src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py:L270-L310]
[src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py:L312-L354]

The cross-package import actually found is capping_stack.containment_flow using
HIPPS `_nonnegative` and `_positive`. Its docstring permits liquid/gas capacity
callbacks after conversion to local m³/s; it does not import those capacity
functions. That package defines WellheadSpec locally, with no tree/wellhead
package import. Tree integration is still deferred; the tree initializer
imports design_checks, flow_capacity, pressure_control and tree_catalog only.
[src/digitalmodel/subsea/capping_stack/containment_flow.py:L12-L17]
[src/digitalmodel/subsea/capping_stack/containment_flow.py:L49-L56]
[src/digitalmodel/subsea/capping_stack/interface_check.py:L59-L74]
[src/digitalmodel/subsea/capping_stack/__init__.py:L20-L23]
[src/digitalmodel/subsea/trees/__init__.py:L23-L42]

No licensed solver is needed for the documented checks. The AST checker uses
only Python's standard library and does not import the package. Registry tests
use PyYAML; numerical/collection tests use pytest.

## 10. Coverage and open items

Section 2 inventories every defined public function and class. Sections 4–5
provide literal arithmetic and collected gates; this is not a percentage line-
coverage or standards qualification claim. Equation sources for the liquid/gas
valve relations are read; reliability tables/approximations, pressure-rise bound,
wave speed and native characteristic qualification remain UNVERIFIED or
NOT_EVALUATED as specified in §3.

Open: API 17O and IEC 61508/61511 edition/clause qualification; architectural and
FMEDA evidence; failure/proof-test assumptions; installed valve and actuator
profiles; catalogue-dependent factor iteration; cavitation, viscosity, multiphase
and gas transient applicability; representable arithmetic and finite validation.
PRs #2835 and #3174 change liquid valve-sizing/choking results and are in flight
at preparation. This pack describes the base code pin, not either proposed head;
a future merge requires a new pin and refreshed contracts, examples and gates.
