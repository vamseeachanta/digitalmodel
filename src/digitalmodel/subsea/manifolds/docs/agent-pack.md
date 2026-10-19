# Subsea manifolds knowledge pack

| Header | Value |
|---|---|
| Module | subsea/manifolds |
| Repository | digitalmodel |
| Path | `src/digitalmodel/subsea/manifolds` |
| Pack revision | 0, 2026-10-18 |
| Code revision | `f4032a2ecaac110244919db23c5656d1c0d1cd5a` |
| Analysis family | Engineering calculations (EC); Python calculations |
| Family quality manager role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map ref | `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations resolve at the code revision above; a citation containing `@<revision>` resolves at that explicit revision.

## 1. Purpose and scope

The package exports additive production gathering, well-to-header pressure feasibility, a caller-populated equipment registry, elementary capacity screens and proposed chemical distribution with shared supply hydraulics. [src/digitalmodel/subsea/manifolds/__init__.py:L4-L20] [src/digitalmodel/subsea/manifolds/__init__.py:L46-L52]

The routing registry marks the enclosing `subsea` domain stable; this is domain metadata, not evidence of qualified manifold equipment or API design compliance. [docs/registry/module-routing.yaml:L464-L469] [src/digitalmodel/subsea/manifolds/__init__.py:L14-L20]

There is no supplied equipment catalogue or verified API 17P edition/clause basis; allowable capacities require independent qualification, controls layout belongs outside this package, and field-network wiring and transient multiphase solving remain deferred. [src/digitalmodel/subsea/manifolds/__init__.py:L14-L20]

Mudmat results assume uniform compression and linear foundation springs; header hoop results assume caller-qualified thin-wall applicability; load cases omit interaction, dynamic amplification and impact response. [src/digitalmodel/subsea/manifolds/design_checks.py:L12-L20] [src/digitalmodel/subsea/manifolds/design_checks.py:L78-L89]

Chemical rates are proposals rather than a pressure-driven network solution; branch isolation, steady Newtonian smooth-bore supply flow and an inherited transitional-friction convention bound their use. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L10-L24]

## 2. Inputs and outputs

Inputs are Python objects; aggregation and pressure balance perform no unit conversion, while the catalogue, design screens and chemical distribution declare SI quantities. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L27-L28] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L37-L38] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L14-L15] [src/digitalmodel/subsea/manifolds/design_checks.py:L20-L20] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L46-L65]

| Input / function | Unit | Allowed range and default | Source / evidence |
|---|---|---|---|
| `aggregate_flow.wells` | Common caller-selected rate unit | Nonempty list; each `q_oil`, `q_gas`, `q_water` defaults to 0.0; negative values rejected, non-finite values not rejected | Caller dictionaries; [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L91-L108] |
| `q_capacity` | Same total-rate unit | Optional; default `None`; negative rejected; `None` leaves capacity unchecked | Caller; [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L70-L84] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L91-L113] |
| `well_balance`: `well_id`, `p_tree`, `p_header` | Identifier; consistent pressure unit | Identifier passed through; pressures nonnegative by comparison only; no finite-value guard | Caller; [src/digitalmodel/subsea/manifolds/pressure_balance.py:L99-L149] |
| `dp_jumper`, `dp_choke_max` | Same pressure unit | Jumper loss defaults to 0.0; authority defaults to unbounded `None`; negative values rejected | Caller; [src/digitalmodel/subsea/manifolds/pressure_balance.py:L103-L141] |
| `manifold_balance.wells` | Same pressure unit | Nonempty dictionaries; `well_id`, `p_tree` required; optional jumper/authority defaults as above; numeric strings passed through `float()` | Caller; [src/digitalmodel/subsea/manifolds/pressure_balance.py:L164-L191] |
| `ManifoldEntry.key`, `kind`, `slots`, `tie_in`, `source` | Text; slots count | Required key and tie-in; kind in gathering/template/multi-slot/plem-integrated; integer slots ≥1; HTTPS locator with host and no username | Caller evidence locator retained, not verified; [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L25-L63] |
| `header_bore_m`, `rating_pa`, `footprint_m`, `dry_mass_kg` | m, Pa, two m dimensions, kg | Required finite positive values; no defaults | Caller; [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L37-L58] |
| `wet_mass_kg` | kg apparent mass | Required finite value; zero and negative apparent mass allowed | Caller; [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L14-L15] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L58-L59] |
| `ManifoldCatalog.entries`; `select(kind, slots, pressure_pa)` | Records; count; Pa | Entries default empty; unique keys; select requires supported kind, integer slots ≥1 and positive finite pressure | Caller; [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L66-L90] |
| `mudmat_screen`: load, area, bearing capacity, subgrade modulus, settlement limit | N, m², Pa, Pa/m, m | Finite nonnegative load; all other arguments finite positive; no defaults | Independently supplied capacities; [src/digitalmodel/subsea/manifolds/design_checks.py:L29-L37] [src/digitalmodel/subsea/manifolds/design_checks.py:L54-L62] |
| `header_screen`: internal/external pressure, OD, wall, allowable hoop stress, collapse capacity | Pa, Pa, m, m, Pa, Pa | Finite nonnegative pressures; remaining inputs finite positive; `2*wall < OD`; no defaults | Caller-qualified capacities and thin-wall regime; [src/digitalmodel/subsea/manifolds/design_checks.py:L78-L89] |
| `load_case_screen`: installation/trawl force, dropped mass, speed; force/energy capacities | N, N, kg, m/s; N, N, J | Finite nonnegative demands, finite positive capacities; no defaults | Independent cases; [src/digitalmodel/subsea/manifolds/design_checks.py:L107-L116] |
| `InjectionDemand`: water rate, target fraction, backpressure, branch resistance | m³/s, 1, Pa, Pa·s²/m⁶ | Finite positive water rate; fraction in [0,1]; finite nonnegative pressure and resistance; no defaults | Caller; [src/digitalmodel/subsea/manifolds/chemical_injection.py:L46-L65] |
| `distribute_chemical`: total rate, demands, available pressure, vertical drop | m³/s, records, Pa, m | Finite nonnegative rate; nonempty demands; positive finite pressure; finite signed drop; nonzero supply requires nonzero target | Caller; [src/digitalmodel/subsea/manifolds/chemical_injection.py:L104-L126] |
| `UmbilicalTube`, `ChemicalFluid` | Bore/length m; density kg/m³; viscosity Pa·s | Required finite positive properties checked by `distribute_chemical` | Upstream data objects; [src/digitalmodel/subsea/manifolds/chemical_injection.py:L111-L113] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L76-L118] |

| Output | Unit / convention | Evidence |
|---|---|---|
| `AggregateResult` | Phase, liquid and total rates in input units; water cut and GOR are ratios; zero denominators give `None`; capacity flag may be `None` | [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L36-L67] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L107-L125] |
| `WellBalance`, `ManifoldBalanceResult` | Arrival/choke/header pressures in input units; negative choke differential means backpressure; ordered well tuple and infeasible count | [src/digitalmodel/subsea/manifolds/pressure_balance.py:L46-L96] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L152] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L179-L199] |
| `ManifoldCatalog.get/select` | Matching record / tuple; absent lookup raises `KeyError`; no matches returns empty tuple | [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L78-L90] |
| `MudmatScreen`, `HeaderScreen`, `LoadCaseScreen` | Utilisations dimensionless; settlement and limit m; inclusive capacity limits | [src/digitalmodel/subsea/manifolds/design_checks.py:L40-L51] [src/digitalmodel/subsea/manifolds/design_checks.py:L65-L75] [src/digitalmodel/subsea/manifolds/design_checks.py:L92-L104] |
| `ChemicalDistribution`, `WellDose` | Node pressure/deficit Pa; proposed flow m³/s; fractions/ratios dimensionless; restriction Pa·s²/m⁶ or `None`; optional upstream `DeliveryResult` | [src/digitalmodel/subsea/manifolds/chemical_injection.py:L68-L101] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L119-L127] |

A positive vertical drop means injection below the supply outlet and adds hydrostatic head; pressure losses are subtracted from arrival pressure and supply pressure. [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L138] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L120-L124]

The routines consume scalar pressures, rates and capacities rather than coordinates, origins, axes or phase angles; the code defines no spatial frame or harmonic phase convention and implements no file reader/writer in this package. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L70-L125] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L99-L199] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L25-L90] [src/digitalmodel/subsea/manifolds/design_checks.py:L29-L116] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L41-L127]

## 3. Method and governing standards

| Method / implementation | Equations or algorithm | Basis and qualification |
|---|---|---|
| `aggregate_flow` | Add phases; liquid=oil+water; total=liquid+gas; water cut=water/liquid; GOR=gas/oil; total≤capacity | Code equations (1)–(5), requiring common rate units and compatible volume basis; [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L3-L28] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L96-L113] |
| `well_balance`; `manifold_balance` | Arrival=tree−jumper; choke=arrival−header; feasible iff arrival≥header and choke≤optional authority; count infeasible wells | Algebraic screen, not an iterative network solution; [src/digitalmodel/subsea/manifolds/pressure_balance.py:L10-L38] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L143] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L179-L199] |
| `ManifoldEntry`; `ManifoldCatalog` | Validate attributes and locator syntax; unique keys; filter by kind, minimum slots and pressure rating | Caller-populated registry with no seeded manufacturer records; [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L4-L16] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L48-L90] |
| `mudmat_screen` | Pressure=load/area; bearing utilisation=pressure/capacity; settlement=pressure/subgrade modulus | Uniform linear spring screen with separate bearing and settlement limits; [src/digitalmodel/subsea/manifolds/design_checks.py:L12-L13] [src/digitalmodel/subsea/manifolds/design_checks.py:L49-L62] |
| `header_screen` | Tensile hoop stress=max(internal−external,0)·OD/(2·wall); external utilisation=max(external−internal,0)/collapse capacity | Collapse capacity is supplied; no collapse-strength solution or wall-sizing code equation; [src/digitalmodel/subsea/manifolds/design_checks.py:L14-L16] [src/digitalmodel/subsea/manifolds/design_checks.py:L78-L89] |
| `load_case_screen` | Independent force/capacity ratios; impact energy=mass·speed²/2; energy/capacity ratio | No coupled interaction or impact response; [src/digitalmodel/subsea/manifolds/design_checks.py:L17-L20] [src/digitalmodel/subsea/manifolds/design_checks.py:L107-L116] |
| `InjectionDemand.target_rate_m3_s`; `distribute_chemical`; `_well_dose` | Demand=water rate·target fraction; supply allocated proportionally; node=supply+static head−common friction; branch margin=node−backpressure−R·Q²; restriction=margin/Q² if feasible, otherwise explicit deficit | Shared supply delegates to `check_delivery`; inactive branches isolated and zero flow bypasses the upstream zero-Reynolds calculation; [src/digitalmodel/subsea/manifolds/chemical_injection.py:L10-L24] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L62-L65] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L93-L127] |

API 17P, *Design and Operation of Subsea Production Systems: Subsea Structures and Manifolds*, is named as equipment scope; the code supplies neither verified edition nor clause locator and expressly denies API qualification. Clause text not checked against the licensed copy. [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L10] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L6-L10] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L6-L8]

The plan proposes API 17P §5 without an edition or clause title, whereas the implementation reports no verified clause basis; no conflicting editions can be established from these statements. The plan's proposed solver and integration are not implemented by the algebraic pressure screen; #2005 and #2006 retain those qualification and composition gaps. [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L20-L24] [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L10] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L143] [src/digitalmodel/subsea/manifolds/__init__.py:L19-L20]

No `Citation` sidecar is emitted by these routines; the current screens use caller-supplied capacities and make no standards-derived design-factor claim. API clause qualification and a future sidecar basis remain with #2005. [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L20] [src/digitalmodel/subsea/manifolds/design_checks.py:L54-L116] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L93-L127]

UNVERIFIED: API 17P edition, clause titles and applicability of any intended design check remain unconfirmed; no licensed clause text was opened (#2005). [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L10]

## 4. Validated worked examples and regression examples

The five validated examples below use independent scalar arithmetic or closed-form expectations written in the tests; they validate those bounded equations, not equipment qualification or the deferred field network. [tests/subsea/manifolds/test_flow_aggregator.py:L12-L28] [tests/subsea/manifolds/test_pressure_balance.py:L15-L22] [tests/subsea/manifolds/test_design_checks.py:L15-L45] [src/digitalmodel/subsea/manifolds/__init__.py:L14-L20]

Recorded execution: all listed nodes PASS at the code pin, both before and after the documentation change; full module suite 43 run / 43 passed / 0 failed / 0 skipped / 0 NOT_RUN, including parametrised cases. The source tests define the cases below. [tests/subsea/manifolds/test_chemical_injection.py:L24-L68] [tests/subsea/manifolds/test_design_checks.py:L15-L56] [tests/subsea/manifolds/test_flow_aggregator.py:L12-L73] [tests/subsea/manifolds/test_manifold_catalog.py:L15-L59] [tests/subsea/manifolds/test_pressure_balance.py:L15-L98]

| Case / class | Independent basis or regression basis; inputs and expected output | Tolerance and basis | Existing test node ID; result |
|---|---|---|---|
| V1 validated: additive gathering | Direct addition of test constants: oil 8000, gas 8000000, water 3000, liquid 11000, total 8011000 common rate units; water cut=3000/11000; GOR=1000 | `math.isclose`, relative 1e-9, absolute 0; code-test arithmetic, not mixed engineering units | `tests/subsea/manifolds/test_flow_aggregator.py::test_aggregate_two_wells_hand_verified`; PASS/PASS. [tests/subsea/manifolds/test_flow_aggregator.py:L12-L28] |
| V2 validated: pressure subtraction | Tree 320, jumper 15, header 250, authority 80 common pressure units; arrival 305, choke 55; feasible | `math.isclose`, relative 1e-9, absolute 0; independent subtraction | `tests/subsea/manifolds/test_pressure_balance.py::test_well_within_limit_hand_verified`; PASS/PASS. [tests/subsea/manifolds/test_pressure_balance.py:L15-L22] |
| V3 validated: mudmat | 100 N / 2 m² = 50 Pa; capacity 100 Pa gives 0.5 utilisation; modulus 1000 Pa/m gives 0.05 m settlement; limit 0.1 m | Exact asserted values; 0 dimensionless and 0 m difference in this fixture | `tests/subsea/manifolds/test_design_checks.py::test_bearing_and_settlement_have_independent_criteria`; PASS/PASS. [tests/subsea/manifolds/test_design_checks.py:L15-L22] |
| V4 validated: header | Differential 100 Pa, OD 1 m, wall 0.01 m gives hoop 5000 Pa; allowable 10000 Pa gives 0.5; reversed differential / collapse capacity 50 Pa gives utilisation 2 | Exact ratios; 0 dimensionless difference in this fixture | `tests/subsea/manifolds/test_design_checks.py::test_header_hoop_and_external_pressure_are_separate_cases`; PASS/PASS. [tests/subsea/manifolds/test_design_checks.py:L25-L33] |
| V5 validated: independent loads | Installation 10/20 N gives 0.5; trawl 20/20 N gives 1; kinetic energy 2 kg·(3 m/s)²/2=9 J, divided by 10 J gives 0.9 | Exact ratios; 0 dimensionless difference in this fixture | `tests/subsea/manifolds/test_design_checks.py::test_installation_trawl_and_impact_limits_are_explicit`; PASS/PASS. [tests/subsea/manifolds/test_design_checks.py:L36-L45] |
| R1 regression: chemical allocation | Test-owned laminar calculation and proportional demands; total 3e-6 m³/s split 1e-6 and 2e-6 m³/s; dose ratios 1; friction compared with `128*mu*L*Q/(pi*d**4)` | `pytest.approx` defaults: relative 1e-6, absolute 1e-12 in compared quantity's units; hydraulic publication basis not independently opened | `tests/subsea/manifolds/test_chemical_injection.py::test_multi_well_distribution_and_balancing_hand_example`; PASS/PASS. [tests/subsea/manifolds/test_chemical_injection.py:L18-L40] |
| R2 regression: equipment selection | Synthetic arithmetic entry, gathering, 4 slots; selection returns 1 record at 4 slots / 1e6 Pa, 0 at 5 slots or 2e6 Pa | Exact record equality and counts; 0 record-count difference; no manufacturer validation | `tests/subsea/manifolds/test_manifold_catalog.py::test_registry_filters_capacity_and_type[gathering]`; PASS/PASS. [tests/subsea/manifolds/test_manifold_catalog.py:L21-L40] |
| R3 regression: absent phases | Oil-only 4000 common rate units; absent gas/water become 0; total 4000 | `math.isclose`, relative 1e-9, absolute 0; default behaviour | `tests/subsea/manifolds/test_flow_aggregator.py::test_missing_phase_keys_default_zero`; PASS/PASS. [tests/subsea/manifolds/test_flow_aggregator.py:L59-L64] |
| R4 regression: inactive dosing | Water 1 m³/s, target fraction 0, supply 0; delivery and dose ratio `None`, well rate 0; nonzero supply rejected | Exact flags and 0 m³/s; documents isolation/zero-Reynolds bypass | `tests/subsea/manifolds/test_chemical_injection.py::test_zero_demand_does_not_call_zero_reynolds_delivery`; PASS/PASS. [tests/subsea/manifolds/test_chemical_injection.py:L52-L61] |

Reproduction form: `PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/manifolds -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; default repository addopts retained, no solver import or licence needed by these test files. [pytest.ini:L48-L52] [tests/subsea/manifolds/test_flow_aggregator.py:L3-L9] [tests/subsea/manifolds/test_pressure_balance.py:L3-L12] [tests/subsea/manifolds/test_chemical_injection.py:L6-L15] [tests/subsea/manifolds/test_design_checks.py:L6-L12] [tests/subsea/manifolds/test_manifold_catalog.py:L6-L12]

## 5. QA gates

Gate selection follows AQG r0 §10, Python calculations, and §9/§11 for evidence and independent clearance; the following are pack-defined executable checks, with results from the recorded offline execution.

| Gate | Applicability | Checks |
|---|---|---|
| G0 | Applicable: inputs, units and citations | C01–C04; input tables in section 2 |
| G1 | Applicable: Python imports and tests; no program deck is written | C05 |
| G2 | Applicable: independent arithmetic/closed-form checks | C06–C10, with C11–C14 recording boundary/regression checks |
| G3 | NOT_APPLICABLE: no dynamics solver in scope | Package defers transient solving. [src/digitalmodel/subsea/manifolds/__init__.py:L19-L20] |
| G4 | NOT_APPLICABLE: no diffraction body or mesh in scope | Exported surface is gathering, balance, registry, screens and dosing. [src/digitalmodel/subsea/manifolds/__init__.py:L46-L52] |
| G5 | Applicable: evidence appendix and independent clearance | C15; clearance remains pending under AQG r0 §11 |

| Check | Gate | Executable route and numeric pass criterion with unit | Evidence / recorded result |
|---|---|---|---|
| C01 | G0 | `tests/subsea/manifolds/test_flow_aggregator.py::test_invalid_inputs_raise` and `tests/subsea/manifolds/test_pressure_balance.py::test_invalid_inputs_raise`: 8 invalid calls rejected, 0 unexpected acceptances (calls) | PASS; negative/empty inputs only, not NaN/inf coverage. [tests/subsea/manifolds/test_flow_aggregator.py:L67-L73] [tests/subsea/manifolds/test_pressure_balance.py:L88-L98] |
| C02 | G0 | `tests/subsea/manifolds/test_manifold_catalog.py::test_invalid_manifold_data`: 7 parametrised invalid records rejected; 0 accepted records | PASS; locator syntax and catalogue attributes. [tests/subsea/manifolds/test_manifold_catalog.py:L43-L50] |
| C03 | G0 | `tests/subsea/manifolds/test_design_checks.py::test_invalid_mudmat_input` plus `tests/subsea/manifolds/test_design_checks.py::test_impossible_pipe_geometry_rejected`: 4 invalid calls rejected; 0 accepted calls | PASS; zero capacity/area, NaN load and no-bore geometry. [tests/subsea/manifolds/test_design_checks.py:L48-L56] |
| C04 | G0 | `tests/subsea/manifolds/test_chemical_injection.py::test_invalid_demand`: 4 invalid demand records rejected; 0 accepted records | PASS; nonpositive water, negative fraction/resistance, NaN pressure. [tests/subsea/manifolds/test_chemical_injection.py:L64-L68] |
| C05 | G1 | Run section-4 reproduction command: 43 collected cases pass, 0 failures and 0 skips (cases); export test requires 9 callable symbols, 0 missing symbols | PASS; direct Python package-import route. [tests/subsea/manifolds/test_manifold_catalog.py:L53-L59] |
| C06 | G2 | V1 node: phase/liquid/total rates, water cut and GOR equal table expectations with relative 1e-9 and absolute 0 in the compared units | PASS; independent addition and ratios. [tests/subsea/manifolds/test_flow_aggregator.py:L12-L28] |
| C07 | G2 | V2 node: arrival=305 and choke=55 common pressure units, relative 1e-9, absolute 0; 0 incorrect feasibility flags | PASS; subtraction reference only for finite, consistent-unit inputs. [tests/subsea/manifolds/test_pressure_balance.py:L15-L22] |
| C08 | G2 | V3 node: bearing ratio=0.5 exactly, settlement=0.05 m exactly; 0 incorrect pass/fail flags across 3 calls | PASS; independent division. [tests/subsea/manifolds/test_design_checks.py:L15-L22] |
| C09 | G2 | V4 node: hoop ratio=0.5, external ratio=0 then 2 exactly; 0 incorrect capacity flags | PASS; separates internal hoop and external capacity comparisons. [tests/subsea/manifolds/test_design_checks.py:L25-L33] |
| C10 | G2 | V5 node: force ratios=0.5 and 1, impact ratio=0.9 exactly; 0 incorrect capacity flags across 4 calls | PASS; independent force/energy comparisons. [tests/subsea/manifolds/test_design_checks.py:L36-L45] |
| C11 | G2 | R1 and `tests/subsea/manifolds/test_chemical_injection.py::test_under_dose_and_hydraulic_deficit_are_reported`: allocated sum=3e-6 m³/s within relative 1e-6 / absolute 1e-12 m³/s; underdose ratios=0.5 within same tolerances dimensionless; deficit>0 Pa and restriction=None for deficient branch | PASS; regression evidence, not independent hydraulic qualification. [tests/subsea/manifolds/test_chemical_injection.py:L24-L49] |
| C12 | G2 | R3 plus `tests/subsea/manifolds/test_flow_aggregator.py::test_zero_liquid_and_zero_oil_give_none_ratios` and `tests/subsea/manifolds/test_flow_aggregator.py::test_boundary_capacity_equal_is_within`: 0 incorrect defaults/undefined-ratio/capacity flags (flags); missing phase rates=0 common rate units | PASS; does not detect mistakenly omitted measurements. [tests/subsea/manifolds/test_flow_aggregator.py:L44-L64] |
| C13 | G2 | `tests/subsea/manifolds/test_pressure_balance.py::test_unbounded_choke_authority_default`, `tests/subsea/manifolds/test_pressure_balance.py::test_well_boundary_arrival_equals_header_is_feasible`, `tests/subsea/manifolds/test_pressure_balance.py::test_well_boundary_choke_equals_max_is_feasible`: 0 incorrect flags (flags); choke=650, 0 and 80 common pressure units within relative 1e-9 / absolute 0 | PASS; records permissive defaults and inclusive boundaries, not finite-input validation. [tests/subsea/manifolds/test_pressure_balance.py:L43-L63] |
| C14 | G2 | R2, R4 and `tests/subsea/manifolds/test_manifold_catalog.py::test_registry_starts_empty`: 0 incorrect empty-selection/inactive-dose states (states); initial entries=0 records; missing key raises 1 KeyError; inactive well rate=0 m³/s | PASS; empty catalogue/no match and isolated branch conventions are explicit. [tests/subsea/manifolds/test_manifold_catalog.py:L15-L40] [tests/subsea/manifolds/test_chemical_injection.py:L52-L61] |
| C15 | G5 | Resolve every citation at its pin; compare section-4 nodes with offline logs and section-6 detections with checks here: 0 broken citations, 0 missing case results, 0 untracked coverage items (counts); require independent clearance before engineering use | Producer evidence check PASS; independent clearance pending; AQG r0 §9/§11 |

## 6. Known failure modes and pitfalls

sweep: 29 found

The count denotes reviewed code sites or unit policies S01–S29, including intentional defaults; it is not a count of confirmed defects. The package contains no swallowed `except`, record-skipping `continue`, explicit `return None`, empty-list/dict error return, cache write, epoch default or percentage fallback; the optional values and zero branches below still require interpretation. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L70-L125] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L99-L199] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L25-L90] [src/digitalmodel/subsea/manifolds/design_checks.py:L29-L116] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L41-L127]

Exact sweep commands (followed by reading every package Python file):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%' src/digitalmodel/subsea/manifolds
rg -n 'float\(|int\(|\.get\(|None|unit|pressure|rate|cache|write' src/digitalmodel/subsea/manifolds
```

| Site | Symptom and cause at code pin | Existing detection / coverage | Tracking / date |
|---|---|---|---|
| S01 | Absent oil silently becomes 0.0; `float()` accepts numeric text and non-finite text, but not thousands/decimal separator syntax. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L98-L102] | C12 covers default behaviour collectively; no detection check exists for accidental omission, locale input or non-finite text | issue pending (1), issue pending (2); 2026-10-18 |
| S02 | Same missing/default/conversion policy for gas. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L99-L102] | C12 asserts missing gas=0; no detection check exists for accidental omission or non-finite text | issue pending (1), issue pending (2); 2026-10-18 |
| S03 | Same missing/default/conversion policy for water. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L100-L102] | C12 asserts missing water=0; no detection check exists for accidental omission or non-finite text | issue pending (1), issue pending (2); 2026-10-18 |
| S04 | Phase accumulators initialise at 0.0; intentional additive identity. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L96-L105] | C06 independently checks summed phases | No defect; 2026-10-18 |
| S05 | Omitted capacity gives `within_capacity=None`, not a passed capacity check. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L72-L72] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L111-L113] | C06 asserts the missing-capacity flag | No defect; 2026-10-18 |
| S06 | Zero liquid returns undefined water cut as `None`; NaN liquid also takes this branch. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L109-L109] | C12 covers gas-only input; no detection check exists for NaN | issue pending (1); 2026-10-18 |
| S07 | Zero oil returns undefined GOR as `None`; NaN oil also takes this branch. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L110-L110] | C12 covers gas-only input; no detection check exists for NaN | issue pending (1); 2026-10-18 |
| S08 | Negative-only capacity guard accepts NaN/inf; comparisons may report an unchecked or misleading capacity outcome. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L93-L94] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L111-L113] | C01 covers negatives; no detection check exists for non-finite capacity | issue pending (1); 2026-10-18 |
| S09 | Direct `well_balance` jumper loss defaults to 0.0, omitting real loss when caller leaves it out. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L103-L103] | C13 records zero-loss default; no detection check exists for omitted physical loss | issue pending (2); 2026-10-18 |
| S10 | Direct choke authority defaults unbounded `None`; an unspecified rating cannot reject an over-ranged choke. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L104-L104] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L140-L143] | C13 checks default behaviour; no detection check exists for an omitted real rating | issue pending (2); 2026-10-18 |
| S11 | Comparison-only pressure guards accept NaN/inf; NaN tree pressure yields `feasible=True` with NaN outputs. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L128-L143] | C01 covers negative inputs; no detection check exists for non-finite pressures | issue pending (1); 2026-10-18 |
| S12 | Opening prose excludes arrival=header, but formula/code/tests accept equality; a screening pass must not imply positive physical flow. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L5-L8] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L143] | C13 records equality acceptance; no detection check exists for prose consistency | issue pending (2); 2026-10-18 |
| S13 | Dictionary tree pressure uses bare `float()`: numeric text accepted, locale text rejected, `nan` accepted. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L181-L182] | No detection check exists | issue pending (1), issue pending (2); 2026-10-18 |
| S14 | Dictionary jumper loss uses bare `float()` and defaults to 0.0. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L184-L184] | C13 tests direct default, not dictionary omission; no detection check exists for dictionary text/default validation | issue pending (1), issue pending (2); 2026-10-18 |
| S15 | Dictionary choke authority preserves `None` or uses bare `float()`; missing is unbounded and non-finite text can pass. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L185-L188] | No detection check exists for dictionary conversion/omitted rating | issue pending (1), issue pending (2); 2026-10-18 |
| S16 | Empty initial catalogue is intentional; no manufacturer equipment is supplied. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L4-L16] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L69-L75] | C14 checks 0 entries and missing-key exception | #2005; 2026-10-18 |
| S17 | A selector with no qualifying equipment returns an empty tuple, without raising. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L84-L90] | C14 checks empty selections | No defect; 2026-10-18 |
| S18 | Source validation checks HTTPS syntax, not availability, provenance or rating truth. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L14-L15] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L60-L62] | C02 covers one invalid source; no detection check exists for source verification | #2005; 2026-10-18 |
| S19 | Wet mass means apparent mass and is only required finite; negative values are not rejected. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L14-L15] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L58-L59] | No detection check exists for wet-mass convention | #2005; 2026-10-18 |
| S20 | Zero proposed rate returns zero dose/deficit and `None` restriction; zero-target ratio is `None`, positive-target ratio is 0.0; inactive branches assumed isolated. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L22-L24] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L93-L96] | C14 covers zero target; no detection check exists for positive-target zero supply or isolation assumption | #2005; 2026-10-18 |
| S21 | Negative branch margin gives restriction `None` and positive deficit instead of changing the proposed rate. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L97-L101] | C11 checks dose shortfall and pressure-deficit flags | #2005; 2026-10-18 |
| S22 | Shared `delivery=None` on zero supply bypasses undefined zero-Reynolds hydraulics. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L119-L124] | C14 checks intentional bypass | No defect; 2026-10-18 |
| S23 | Zero summed target allocates 0.0; nonzero supply with zero target raises. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L116-L126] | C14 checks both paths | No defect; 2026-10-18 |
| S24 | Gathering assumes common rate/reference-condition basis but docstring gives differing phase-unit examples; direct phase addition has no conversion. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L27-L28] [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L107-L108] | C06 checks arithmetic only; no detection check exists for incompatible units or volume basis | issue pending (2); 2026-10-18 |
| S25 | Pressure units and datum are caller-consistent, with no conversion. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L37-L38] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L138] | C07 checks scalar subtraction only; no detection check exists for mixed units/datum | issue pending (2); 2026-10-18 |
| S26 | SI loads, areas, spring stiffness and capacities are assumed; foundation eccentricity/consolidation and load interaction absent. [src/digitalmodel/subsea/manifolds/design_checks.py:L12-L20] | C08/C10 check elementary ratios; no detection check exists for independently qualified capacities/applicability | #2005; 2026-10-18 |
| S27 | Opposite pressure differentials are clamped to 0 in each independent header ratio; thin-wall regime is caller-qualified. [src/digitalmodel/subsea/manifolds/design_checks.py:L78-L89] | C09 checks directional zeroing; no detection check exists for thin-wall applicability | #2005; 2026-10-18 |
| S28 | Dosing assumes SI and volume fraction, `R*Q²` branch loss and isolated inactive branches; positive vertical drop assists delivery using upstream gravity. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L10-L24] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L46-L65] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L97-L101] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L163-L169] | C11/C14 check zero-drop allocation and inactivity; no detection check exists for nonzero-drop, nonzero-resistance or transitional-regime manifold coverage | #2005; 2026-10-18 |
| S29 | `balanced` uses 0 infeasible wells; permissive per-well defaults/NaN flags propagate through that count. [src/digitalmodel/subsea/manifolds/pressure_balance.py:L179-L199] | C05 runs aggregate feasibility tests; no detection check exists for NaN dictionary propagation | issue pending (1); 2026-10-18 |

Issue review treats #485 as historical scope, #2005 as incomplete qualification/equipment evidence and #2006 as deferred topology composition; they are feature/scope records, not evidence of a numerical incident. The stale coverage claim “No manifold gathering/balancing” conflicts with delivered gathering and balance functions; no detection check exists for that documentation drift (#2005). [docs/field-development/subsea-production-systems-mapping.md:L97-L104] [src/digitalmodel/subsea/manifolds/__init__.py:L4-L20]

The plan's proposed integrated example is not a qualified reference for these routines; the package explicitly defers network wiring, and no detection check exists for a complete field-chain example (#2006). [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L24-L33] [src/digitalmodel/subsea/manifolds/__init__.py:L19-L20]

## 7. Lessons log

Append-only entries use a revision reference instead of a personal or execution identifier.

| Date | Revision reference | Lesson | Evidence | Pack change | Shared-log reference |
|---|---|---|---|---|---|
| 2026-10-18 | Pack revision 0 | A passing algebraic screen does not establish API qualification, physical positive flow at an equality boundary, or realised chemical delivery | [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L20] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L143] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L14-L24] | Sections 1, 3, 4 and 6 distinguish scope and evidence | Not created; #2005 |

## 8. Ownership

Assigned review roles for this draft: engineering calculations quality manager; deputy role: engineering calculations deputy quality manager; producing function: engineering calculations documentation. The family quality manager resolves method classifications and ambiguous references; maintainers coordinate cross-family changes. Clearance requires a provider independent of the producer (AQG r0 §11).

These are role assignments, not a recorded review, approval or named appointment.

## 9. Dependencies and implementation inventory

| Relationship | Package / inventory row | Interface and boundary |
|---|---|---|
| Owning row | `subsea/manifolds`, engineering calculations | Six Python files export the bounded surface in sections 1–3. [src/digitalmodel/subsea/manifolds/__init__.py:L4-L20] [src/digitalmodel/subsea/manifolds/__init__.py:L23-L52] |
| Actual upstream | `subsea/umbilicals`, engineering calculations | `ChemicalFluid`, `UmbilicalTube`, `DeliveryResult`, `check_delivery`, `static_pressure`; steady SI tube hydraulics. [src/digitalmodel/subsea/manifolds/chemical_injection.py:L32-L38] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L76-L118] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L172-L190] [src/digitalmodel/subsea/umbilicals/chemical_flow.py:L201-L255] |
| Proposed upstream/downstream | `subsea/trees`, `subsea/pipeline`, engineering calculations | Tree-to-manifold-to-flowline topology remains deferred, not an active import or executable handoff; #2006. [src/digitalmodel/subsea/manifolds/__init__.py:L19-L20] [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L10-L12] |
| Python runtime | Python ≥3.11; dataclasses/math and URL parsing | No solver version is required by the package code; dependencies here are scalar calculation and validation helpers. [pyproject.toml:L5-L10] [src/digitalmodel/subsea/manifolds/__init__.py:L23-L44] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L18-L22] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L27-L38] |

A8 map inventory: **empty**. At map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, none of the 2348 implementation rows across 13 families has a path inside `src/digitalmodel/subsea/manifolds/`. Therefore there are 0 module map families and 0 counted families; A8(c) is **NOT_APPLICABLE**, with no in-package rows from which to select search signals. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4] [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

The map's `InjectionDemand` match is a rejected candidate for on-bottom stability, marked `needs_review` and explicitly excluded from implementation counts; it is a data holder, not an implementation of that family. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L52941-L52947] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L46-L65]

Map follow-up, issue pending (3): add the section-3 families first—additive phase gathering, well/header pressure feasibility, equipment validation/selection, mudmat bearing/settlement, header pressure screening, independent force/impact screening, chemical-demand allocation and balancing restrictions. All are outside the counted set until a map revision gives them in-package implementation rows. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L96-L113] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L137-L143] [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L48-L90] [src/digitalmodel/subsea/manifolds/design_checks.py:L54-L116] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L93-L127]

Additional repository search, including `scripts/`, used the commands below; these are discovery findings, not counted-family greps.

```sh
rg -n 'def (aggregate_flow|well_balance|manifold_balance|mudmat_screen|header_screen|load_case_screen|distribute_chemical)|dp_choke|bearing_utilisation|branch_resistance_pa_s2_m6' --glob '*.py' src scripts tests
rg -n 'manifold|mudmat' scripts --glob '*.py'
```

The parametric mudmat route wraps a geotechnical bearing-capacity calculation, unlike this package's supplied-capacity comparison; upstream chemical tube friction/static head are delegated to, not reimplemented by, chemical distribution. Carry these relationships into the map follow-up, issue pending (3). [src/digitalmodel/parametric/generate.py:L478-L501] [src/digitalmodel/subsea/manifolds/design_checks.py:L54-L62] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L119-L127]

## 10. Coverage and open items

Files read at the code pin: all six package `.py` files (including `__init__.py`), all five test files plus the empty test-package initializer, the upstream chemical-flow implementation, the routing entry, and the two relevant documents below. The package source and tests define the covered calculation surface. [src/digitalmodel/subsea/manifolds/__init__.py:L4-L52] [tests/subsea/manifolds/test_manifold_catalog.py:L53-L59]

| Documentation discovered and read | Coverage / limitation |
|---|---|
| `docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md` | Historical proposal, including an unverified clause locator and unimplemented network scope; not a numerical design reference. [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L16-L33] |
| `docs/field-development/subsea-production-systems-mapping.md` | Historical coverage map; its no-gathering claim is stale; #2005. [docs/field-development/subsea-production-systems-mapping.md:L97-L104] [src/digitalmodel/subsea/manifolds/__init__.py:L4-L12] |

Discovery commands: `rg -l -i 'subsea/manifolds|subsea\.manifolds|flow_aggregator|manifold_catalog|manifold_balance' docs --glob '*.md' --glob '*.rst'`, supplemented by a concept search for `manifold` in Markdown/reStructuredText; tests were located with `rg -l 'subsea\.manifolds|subsea/manifolds' tests --glob '*.py'`. The five module test files were read and run; section 4 records nine example nodes and section 5 names additional checks. [tests/subsea/manifolds/test_chemical_injection.py:L24-L68] [tests/subsea/manifolds/test_design_checks.py:L15-L56] [tests/subsea/manifolds/test_flow_aggregator.py:L12-L73] [tests/subsea/manifolds/test_manifold_catalog.py:L15-L59] [tests/subsea/manifolds/test_pressure_balance.py:L15-L98]

The calculation-citation contract was readable; no licensed standard or external equipment page was opened, and no external document is used as a checked numerical reference. The code explicitly leaves API edition/clause qualification and equipment-source completeness unresolved. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L6-L15] [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L10]

| Open item | Responsibility | Tracking / closure criterion |
|---|---|---|
| U1: the unverified API 17P edition/clause/application claim in section 3; no licensed text checked | Maintainers with engineering calculations quality manager | #2005; establish edition, clause titles and qualification scope before claiming API compliance. [src/digitalmodel/subsea/manifolds/design_checks.py:L6-L10] |
| Non-finite numeric inputs and text conversion can yield non-finite rates or falsely feasible pressures; S01–S03, S06–S08, S11, S13–S15, S29 | Maintainers | issue pending (1); add finite-input rejection and tests across direct and dictionary routes. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L91-L113] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L128-L143] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L179-L199] |
| Unit/reference-condition contract, omitted-input policy and pressure-equality prose; S01–S03, S09–S10, S12–S15, S24–S25 | Maintainers | issue pending (2); explicit common units/datum, locale conversion policy, rated-input policy and consistent equality documentation. [src/digitalmodel/subsea/manifolds/flow_aggregator.py:L27-L28] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L5-L8] [src/digitalmodel/subsea/manifolds/pressure_balance.py:L103-L121] |
| Public equipment evidence, source verification, apparent-mass convention, independently qualified capacities and model applicability; S16, S18–S21, S26–S28 | Maintainers with engineering calculations quality manager | #2005; real public-source equipment records, qualification and additional tests including nonzero branch resistance/drop and positive-target zero supply. [src/digitalmodel/subsea/manifolds/manifold_catalog.py:L6-L15] [src/digitalmodel/subsea/manifolds/design_checks.py:L12-L20] [src/digitalmodel/subsea/manifolds/chemical_injection.py:L20-L24] |
| Field-network integration and independently checked end-to-end worked example | Maintainers | #2006; executable tree/connector/manifold/flowline handoff; current validated cases are scalar screens only. [src/digitalmodel/subsea/manifolds/__init__.py:L19-L20] [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L24-L33] |
| Implementation map lacks all section-3 method families; rejected data-holder hit needs classification review | Maintainers with engineering calculations quality manager | issue pending (3); add method families and rows, then repeat the map's recorded signals. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502] [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L52941-L52947] |
| Historical scope/coverage documentation and future shared lesson record | Maintainers | #2005; reconcile the plan and coverage map with delivered exports, and retain the qualification limits. [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L16-L33] [docs/field-development/subsea-production-systems-mapping.md:L97-L104] [src/digitalmodel/subsea/manifolds/__init__.py:L4-L20] |

Draft references are local issue proposals, not issued numbers; independent acceptance of issue coverage remains pending until maintainers assign numbers. This draft records no engineering clearance (AQG r0 §11).
