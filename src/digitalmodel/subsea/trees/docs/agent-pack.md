# Subsea trees knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/trees` |
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

Pressure-temperature envelope, caller-sourced equipment registry, liquid Cv and choking screens, velocity ceilings and separate production/annulus handoff. The package names API 6A and API 17D but supplies no public equipment catalogue or verified clause-level basis. The rating check uses psi/°F, catalogue uses Pa/°C, valve screens use U.S. gpm/psi, and capacity uses SI; no automatic conversion connects them. Gas, multiphase transients, material qualification and coupled networks are outside scope. [src/digitalmodel/subsea/trees/__init__.py:L4-L21]

## 2. Inputs and outputs

Every locally defined public class/function and public method/property is listed,
including submodule APIs absent from package exports. Explicit exported data is also
listed. Imported library helpers and private names are excluded. Defaults below are
implementation observations; NOT_EVALUATED defaults are not design recommendations.

| Public API | Inputs, units, validation and defaults | Output contract | Evidence at code pin |
|---|---|---|---|
| `TreeRatingResult` | All required: rwp psi, temperature_class label, t_class_min/max °F, pressure_ok/temperature_ok/passed booleans, margin psi, utilisation dimensionless. | Frozen result fields; direct constructor has no validation or defaults. | [src/digitalmodel/subsea/trees/design_checks.py:L86-L118] |
| `select_rated_working_pressure` | p_design psi; required; negative or above the embedded maximum raises ValueError. | Smallest embedded integer rating containing demand; no external catalogue lookup. | [src/digitalmodel/subsea/trees/design_checks.py:L121-L140] |
| `tree_rating_check` | p_design psi, rwp integer psi, temperature_class label, t_well_min/max °F; all required. Rejects negative pressure, unknown rating/class and reversed temperatures; class uppercased. | TreeRatingResult; pressure_ok and temperature_ok use inclusive comparisons; passed is their conjunction; margin=rwp-p_design, utilisation=p_design/rwp. | [src/digitalmodel/subsea/trees/design_checks.py:L143-L216] |
| `BoreCapacity` | velocity_m_s m/s, rate_m3_s m³/s, both required finite positive. | Frozen validated ceilings; ValueError on invalid direct construction. | [src/digitalmodel/subsea/trees/flow_capacity.py:L31-L42] |
| `bore_capacity` | diameter_m m, density_kg_m3 kg/m³, erosional_coefficient_si in (m/s)√(kg/m³); all required finite positive. | BoreCapacity with v=C/√rho and Q=area*v; ValueError on invalid inputs or invalid derived ceilings. | [src/digitalmodel/subsea/trees/flow_capacity.py:L45-L52] |
| `FlowHandoff` | production_rate_m3_s, annulus_rate_m3_s m³/s; downstream_pressure_pa Pa; within_capacity bool, all required. | Frozen data record; direct constructor performs no validation. | [src/digitalmodel/subsea/trees/flow_capacity.py:L55-L62] |
| `flow_handoff` | Required production/annulus rates m³/s, production_capacity, annulus_capacity (may be None), upstream_pressure_pa and pressure_drop_pa Pa. Finite nonnegative numbers; drop ≤ upstream; nonzero annulus needs capacity. | FlowHandoff retains actual rates, subtracts pressure drop and flags both inclusive ceilings; no clipping or hydraulic solve. | [src/digitalmodel/subsea/trees/flow_capacity.py:L65-L79] |
| `liquid_critical_drop` | upstream_psi and vapor_pressure_psi absolute psi, recovery_factor FL and critical_pressure_ratio FF dimensionless; required. Finite P1>0, 0≤Pv<P1, 0<FL,FF≤1. | Positive finite differential psi; rejects zero underflow. Uses FL*(FL*(P1-FF*Pv)). | [src/digitalmodel/subsea/trees/pressure_control.py:L52-L85] |
| `trim_cv` | full_open_cv U.S. gpm/√psi for SG=1, opening fraction, characteristic string required; rangeability dimensionless defaults None. Finite Cv>0, 0≤opening≤1; linear or equal-percentage; equal-percentage requires finite R>1. | Effective Cv float; exact shutoff at opening zero. Linear ignores rangeability. | [src/digitalmodel/subsea/trees/pressure_control.py:L88-L100] |
| `valve_pressure_drop` | flow_gpm U.S. gpm, cv effective Cv, specific_gravity dimensionless; required finite Q,Cv≥0 and SG>0. | Loss psi; zero Cv/zero flow returns zero; zero Cv/nonzero flow raises ValueError. | [src/digitalmodel/subsea/trees/pressure_control.py:L103-L111] |
| `series_pressure_drop` | flow_gpm U.S. gpm, valve_cvs tuple of effective Cv, specific_gravity dimensionless; all required. Flow finite nonnegative, SG finite positive; each Cv checked by valve_pressure_drop. | Sum of losses psi, same flow through each valve; empty tuple returns integer zero. | [src/digitalmodel/subsea/trees/pressure_control.py:L114-L119] |
| `ChokeFlow` | flow_gpm U.S. gpm, effective_drop_psi psi, regime string; all required. | Frozen record; direct construction is unchecked. | [src/digitalmodel/subsea/trees/pressure_control.py:L122-L128] |
| `liquid_choke_flow` | upstream_psi, downstream_psi absolute psi, cv, specific_gravity and critical_drop_psi psi; all required. Finite pressures/Cv≥0, SG/drop>0; reverse gradient rejected. | ChokeFlow: Q=Cv√(min(P1-P2,critical)/SG); critical at equality, otherwise subcritical; no flashing prediction. | [src/digitalmodel/subsea/trees/pressure_control.py:L131-L145] |
| `TreeEntry` | Required key, configuration single-bore/dual-bore, orientation horizontal/vertical, production_bore_m, annulus_bore_m or None, rwp_pa, temperature_range_c pair, envelope_m triple, dry_mass_kg, wet_mass_kg, valve_count, source HTTPS URL. | Frozen record. Bores/rating/envelope/dry mass finite positive; temperatures finite ordered; wet apparent mass finite signed; positive integer valve_count; topology consistency and URL syntax checked, source content unverified. | [src/digitalmodel/subsea/trees/tree_catalog.py:L30-L71] |
| `TreeCatalog` | entries iterable of TreeEntry; default empty tuple; freezes iterable and rejects duplicate keys. | Frozen registry; no seeded equipment data or entry type/source verification. | [src/digitalmodel/subsea/trees/tree_catalog.py:L74-L101] |
| `TreeCatalog.get` | key string, required; exact match. | TreeEntry or KeyError; no default result. | [src/digitalmodel/subsea/trees/tree_catalog.py:L86-L91] |
| `TreeCatalog.select` | orientation horizontal/vertical, bore_m m and pressure_pa Pa; all required; thresholds finite positive. | Tuple of entries meeting orientation and minimum production bore/rating; input order retained; temperature and annulus excluded. | [src/digitalmodel/subsea/trees/tree_catalog.py:L93-L101] |

## 3. Method and governing standards

| Method / constants | Implemented relation | Public basis and qualification |
|---|---|---|
| Discrete pressure selection / temperature containment | First rating ≥ demand; inclusive lower/upper temperatures; margin=rating−demand, utilisation=demand/rating; both flags must pass | NOT_EVALUATED: API 6A / API 17D editions and clause titles unverified. Embedded rating and temperature tables are implementation data, not independently sourced here; no table values are reproduced. [src/digitalmodel/subsea/trees/design_checks.py:L61-L83] [src/digitalmodel/subsea/trees/design_checks.py:L178-L216] |
| Registry and selection | Exact key lookup; orientation equality and bore/rating lower bounds | Algorithmic filtering; no physical equation. Source URL syntax does not establish source accessibility, authenticity or equipment qualification. No seeded records. [src/digitalmodel/subsea/trees/tree_catalog.py:L47-L101] |
| Liquid Cv / series loss | Δp=SG(Q/Cv)²; total=ΣΔp at common Q; Q=Cv√(Δp/SG) | Fisher, *Control Valve Sourcebook — Oil & Gas* (2013), ch. 3 pp. 3-2–3-3, Table 3-2: N1=1 for U.S. gpm/psia. No attached fittings (Fp=1), turbulent incompressible liquid. [public source](https://www.emerson.com/is/content/emerson/en/final-control/pressure-management/jt-valves/documents/guide-control-valve-sourcebook-oil-gas-fisher-en-135836.pdf). [src/digitalmodel/subsea/trees/pressure_control.py:L103-L119] |
| Liquid choking differential and flow cap | Δpcrit=FL²(P1−FF Pv); effective=min(P1−P2,Δpcrit); equality critical | Same Fisher source (2013), ch. 3 p. 3-5, “Determining Allowable Sizing Pressure Drop”, no attached fittings. FL and FF are required inputs; their positive-unit-interval validation is an implementation domain, not a qualified equipment acceptance range. [src/digitalmodel/subsea/trees/pressure_control.py:L52-L85] [src/digitalmodel/subsea/trees/pressure_control.py:L131-L145] |
| Ideal inherent trim | Linear Cv=Cvfull*x; equal percentage Cv=Cvfull*R^(x−1) for x>0; zero travel returns zero | Emerson, *Control Valve Handbook*, sixth edition (2023), §1.4 “Control Valve Functions and Characteristics Terminology” defines inherent linear/equal-percentage characteristics. [public source](https://www.emerson.com/documents/automation/control-valve-handbook-en-3661206.pdf). Exponential formula follows constant relative change per travel and full-open normalisation; imposed tight shutoff and installed behaviour UNVERIFIED. [src/digitalmodel/subsea/trees/pressure_control.py:L88-L100] |
| Velocity / circle area | v=C/√rho; A=πd²/4; Q=A*v | NOT_EVALUATED: API RP 14E edition, clause and applicability unverified; C is a required SI input, never an embedded oilfield-unit constant. Circle area follows integration of concentric rings; π/4 is geometric, not a code coefficient. [src/digitalmodel/subsea/trees/flow_capacity.py:L4-L17] [src/digitalmodel/subsea/trees/flow_capacity.py:L45-L52] |
| Separate-port handoff | Pdown=Pup−Δp; rates each ≤ supplied ceiling; missing annulus capacity implies zero annulus limit | Arithmetic bookkeeping and caller limits, not a network solve. NOT_EVALUATED for hydraulic delivery and coupled pressure feasibility. [src/digitalmodel/subsea/trees/flow_capacity.py:L65-L79] |

The two public valve documents were opened on 2026-10-22. No API or ISO table,
licensed source, equipment factor or acceptance limit was inferred from memory.
Synthetic inputs below are assumed, based on previous experience; they describe
arithmetic only and are not equipment records.

## 4. Worked examples and regression evidence

1. **Series loss**: Q=20 U.S. gpm, SG=1, Cv=(10,20).
   Δp=1(20/10)²+1(20/20)²=4+1=**5.0 psi**.
   Fisher (2013), ch. 3 pp. 3-2–3-3/Table 3-2 supplies the equation/unit basis.
   `tests/subsea/trees/test_agent_pack_examples.py::test_series_loss_literal`.
2. **Choking screen**: absolute P1=100 psi, Pv=20 psi, FL=0.5, FF=0.8.
   Δpcrit=0.5²(100−0.8×20)=**21.0 psi**. P2=70 psi gives actual drop 30 psi;
   Cv=10 and SG=0.84 give Q=10√(21/0.84)=**50.0 U.S. gpm**, critical.
   Fisher (2013), ch. 3 p. 3-5 supplies the no-fittings equation; factors are synthetic.
   `tests/subsea/trees/test_agent_pack_examples.py::test_critical_drop_and_flow_literals`.
3. **Velocity ceiling**: d=2 m, rho=100 kg/m³, C=20 in required SI units.
   v=20/√100=**2.0 m/s**; A=π(2/2)²=π m²; Q=**2π m³/s**, about 6.3 m³/s.
   The test asserts an independently evaluated literal for 2π. Model applicability
   remains NOT_EVALUATED; this checks algebra, not erosion acceptance.
   `tests/subsea/trees/test_agent_pack_examples.py::test_capacity_literals`.
4. **Linear trim**: full-open Cv=80, opening=0.25 → Cv=80×0.25=**20.0**.
   The inherent linear definition is in Emerson (2023), §1.4; installed behaviour
   and shutoff qualification remain UNVERIFIED.
   `tests/subsea/trees/test_agent_pack_examples.py::test_trim_literal`.

## 5. Executable QA gates

| Gate | Executable check | What passing establishes |
|---|---|---|
| Contracts / source pin / locator bounds | `tests/docs/test_subsea_agent_packs.py::test_pack_contracts[trees]` | Public inventory and evidence positions match unchanged pinned code |
| Missing function detection | `tests/docs/test_subsea_agent_packs.py::test_missing_public_name_fails[trees]` | Removing a section-2 API row is rejected |
| Examples and gates collect | `tests/docs/test_subsea_agent_packs.py::test_all_example_and_gate_nodes_collect[trees]` | All example/gate selectors resolve without executing cited checks |
| Hand arithmetic | `tests/subsea/trees/test_agent_pack_examples.py::test_series_loss_literal` | Literal series loss |
| Choking arithmetic | `tests/subsea/trees/test_agent_pack_examples.py::test_critical_drop_and_flow_literals` | Literal differential and capped flow |

The collection gate collects its own selector without executing its body; no
recursive test execution occurs. Run `python -m pytest tests/subsea/trees` for the complete
rating, catalogue, pressure-control and capacity regression suite. These gates
establish software behaviour; standards/equipment qualification remains NOT_EVALUATED.

## 6. Failure modes and silent assumptions

| ID / evidence | Finding | Detection / way forward |
|---|---|---|
| T1 [src/digitalmodel/subsea/trees/design_checks.py:L178-L203] | Rating check has no finite-value guard; NaN pressure produces NaN utilisation/margin, invalid conditions produce booleans rather than an input error. | No new finite-input detection gate; add explicit finite validation in a separate change. |
| T2 [src/digitalmodel/subsea/trees/tree_catalog.py:L69-L71] | HTTPS syntax alone does not prove public access or verify the record; internal-looking destinations can satisfy syntax. | Require reviewed public equipment provenance outside the registry. |
| T3 [src/digitalmodel/subsea/trees/tree_catalog.py:L93-L101] | Selection omits temperature, annulus duty and valve arrangement. | Apply those checks separately; returned records are candidates. |
| T4 [src/digitalmodel/subsea/trees/pressure_control.py:L95-L100] | Linear trim ignores rangeability; equal-percentage zero-opening tight shutoff is imposed. | Independent valve data and installed-characteristic checks required. |
| T5 [src/digitalmodel/subsea/trees/pressure_control.py:L111-L119] | Finite inputs may overflow during loss squaring; empty valve list silently gives zero loss. | Caller must require a complete path and finite derived output; no overflow gate here. |
| T6 [src/digitalmodel/subsea/trees/pressure_control.py:L138-L145] | Zero Cv can yield zero flow labelled critical; regime describes pressure differential, not delivered flow. | Preserve both fields when interpreting result. |
| T7 [src/digitalmodel/subsea/trees/flow_capacity.py:L65-L79] | Supplied pressure drop is independent of carried rates; over-capacity flows remain in output. | Interpret within_capacity and solve downstream hydraulics separately. |
| T8 [src/digitalmodel/subsea/trees/design_checks.py:L53-L54] [src/digitalmodel/subsea/trees/tree_catalog.py:L14-L15] [src/digitalmodel/subsea/trees/pressure_control.py:L12-L18] | Mixed unit systems are not converted at boundaries. | Explicit caller conversions and fluid reference conditions required. |
| T9 [src/digitalmodel/subsea/trees/pressure_control.py:L122-L128] [src/digitalmodel/subsea/trees/flow_capacity.py:L55-L62] [src/digitalmodel/subsea/trees/design_checks.py:L86-L118] | Direct result construction is unchecked except BoreCapacity. | Construct via calculation APIs and validate serialized results. |

## 7. Lessons log

| Date | Observation | Pack consequence |
|---|---|---|
| 2026-10-22 | Arithmetic regression evidence does not establish equipment or standards applicability. | Keep independent literals, input assumptions and NOT_EVALUATED engineering bases explicit. |

## 8. Ownership and review

Maintainers own API drift, gate maintenance and separate defect corrections.
A qualified subsea reviewer must establish the applicable equipment basis, fluid
properties and independent public provenance before engineering release. No such
clearance is claimed by this pack.

## 9. Dependencies and other implementations

Core calculations use Python standard-library dataclasses, math and URL parsing.
Tests use pytest; pack/registry gates also use PyYAML and read-only Git.
The AST checker is test-local because the shared checker is absent at the pin.
Run tests with `PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio --with pyyaml python -m pytest tests/subsea/trees tests/docs/test_subsea_agent_packs.py -q -rA -p no:cacheprovider`
with a temporary base outside the repository. No solver is required.


Connectors consume geometry/load data, not this registry directly; ROV interfaces
and tool duties remain caller supplied. No tree-to-connector or downstream network
composition is implemented. Manifold documentation is excluded: its pack is not
on the code pin. The implementation map records no tree-specific method family;
the new knowledge-pack index provides navigation only, without adding physical
implementation equivalence claims.

## 10. Coverage and open items

Coverage inventory: all five package Python files and four existing test modules. Four new
independent examples cover loss, choking, capacity and trim. Existing rating tests
check implementation-table behaviour, not independent API table provenance.
Open items are T1–T9, verified API 6A/API 17D/API RP 14E editions and clause titles,
public equipment records, and an independently checked integrated network example.
No production calculation code changes in this pack. See section 5 for executable
gates and the change description for actual results and unavailable dependencies.

The [existing-pack compatibility audit](../../../../../docs/registry/agent-pack-drift.json)
records the unchanged baseline packs and collection limitations; its strict-schema
diagnostics require individual review before being classified as documentation defects.
