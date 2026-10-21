# Alternative-fuel sizing knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | alt_fuel_ship_sizing / digitalmodel |
| Package | `src/digitalmodel/alt_fuel_ship_sizing` |
| Pack revision / status | 0 / DRAFT |
| Code revision | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | EC; engineering calculations, AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Implementation-map ref | `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

## 1. Purpose and scope

The module sizes alternative-fuel inventories and tanks, estimates heat-leak boil-off, applies supplied wind-assist thrust data, sweeps speed and wind options, and estimates gross/net tonnage. [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L5-L15] [src/digitalmodel/alt_fuel_ship_sizing/__init__.py:L10-L51]

The registry assigns development maturity; the workflow labels its results uncalibrated concept-design screening. [docs/registry/module-routing.yaml:L535-L540] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L250-L255]

Shaft power is supplied; tank structure, supports and hold arrangement are excluded, while the workflow also excludes resistance prediction, weight groups and stability. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L42-L44] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L13-L15]

The presets distinguish cryogenic hydrogen, fully refrigerated ammonia and ambient-liquid methanol; the methanol documentation explicitly excludes normal use of the boil-off model. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L96-L147]

## 2. Inputs and outputs

| Input | Unit | Allowed range / default | Source and evidence |
| --- | --- | --- | --- |
| Route distance, speed | nm, kn | Both positive; required for a single passage | `endurance_hours`; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L336-L342] |
| Profile legs | nm, kn, h, kW | Ordered sea/port legs; positive distance, speed and dwell; optional nonnegative sea-leg power; at least one sea leg | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L239-L277] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L460-L487] |
| Design service speed | kn | Positive even with explicit legs | [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L131-L139] |
| Shaft power / hotel load | kW | API accepts nonnegative values; router requires positive design shaft power and nonnegative hotel load | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L345-L357] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L141-L149] |
| Drivetrain / plant efficiency | 1 | Both in (0, 1]; plant efficiency uses LHV basis | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L345-L373] |
| Fuel properties | kg/m3, MJ/kg, kJ/kg, K | Positive comparisons; default LH2 constants, preset selection then explicit overrides; finite-value caveat S01 | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L72-L93] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L430-L451] |
| Ullage / slenderness | 1 | Ullage [0, 1), default 0.08; L/D positive, default 5.0 | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L172-L208] |
| Heat leak | W/m2 or W/(m2 K), K | Positive direct flux or positive U; direct flux takes precedence; ambient default 293.15 K, U route requires ambient above storage temperature | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L181-L218] |
| BOG mode / reserve | text / 1 | consumed or lost; default consumed; nonnegative fuel margin default 0.10 | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L181-L208] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L428-L461] |
| Iteration controls, library API | count, kg | Defaults 50 iterations and 1e-6 kg stopping tolerance; exhaustion raises | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L436-L438] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L502-L516] |
| Thrust matrix | m/s, deg, kN | At least two strictly increasing points per axis; speed nonnegative, angles [0, 180]; rectangular thrust array | [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L39-L73] |
| Wind rose / propulsion efficiency | m/s, deg, 1 | Nonnegative speed; probability [0, 1]; nonempty rose; sum at most 1 + 1e-6; efficiency (0, 1] | [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L76-L88] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L157-L178] |
| Trade inputs | kn, kW, 1, currency/kg | Nonempty speed/options lists; supplied curve has at least two points and bounds speed; otherwise exponent default 3.0; price optional | [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L40-L94] |
| Tonnage inputs | m3, m, count | Positive total/cargo volumes and draught/depth; cargo no greater than total; nonnegative passenger counts default zero | [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L56-L80] |
| Configuration / destination | mapping / path | Router consumes a dictionary; relative output directory defaults to results beneath config directory, falling back to current directory | [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L126-L155] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L526-L544] |

UNVERIFIED (U1): fuel-property defaults and presets below reproduce the code; external source qualification remains open. Values retain the precision of the configured inputs. The default is `lh2`; explicit property overrides are applied after preset selection. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L72-L156] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L430-L451]

| Preset / storage condition | Density (kg/m3) | LHV (MJ/kg) | h_vap (kJ/kg) | Storage temperature (K) | Evidence / flagged range |
| --- | --- | --- | --- | --- | --- |
| `lh2` / cryogenic parahydrogen at normal boiling point | 70.85 | 119.96 | 446.0 | 20.28 | [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L72-L81] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L96-L105] |
| `nh3_refrigerated` / fully refrigerated ammonia | 682.0 | 18.6 | 1370.0 | 239.82 | Docstring flags LHV 18.6–18.8 MJ/kg by reference state; preset uses gaseous-NH3 basis. [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L85-L109] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L108-L128] |
| `methanol` / ambient liquid | 791.4 | 19.9 | 1100.0 | 293.15 | Docstring flags LHV 19.9–20.1 MJ/kg; h_vap is an NBP placeholder and the BOG model does not apply to normal ambient storage. [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L113-L140] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L131-L147] |

| Output | Unit | Content / format | Evidence |
| --- | --- | --- | --- |
| Fuel result | h, kW, kg/h, kg, m3, m, m2, W, kg/day, %/day | Energy demand, burn plus reserve, BOG loss, net/gross tank geometry, heat leak, warnings | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L299-L333] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L633-L648] |
| Voyage accounting | h, nm, kn, kg | Ordered sea/port records, generated/consumed/lost BOG and total duration | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L283-L315] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L567-L593] |
| Wind result | kW, 1 | Expected saving, remaining shaft demand and per-bin contributions | [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L91-L106] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L180-L208] |
| Trade table | kn, h, kW, kg/h, kg, m3, %/day, %, currency | One row per speed/option; optional fuel cost; relative-saving caveat S20 | [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L127-L163] |
| Tonnage result | 1 | GT/NT, coefficients, cargo/passenger terms and floor flags | [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L42-L53] [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L108-L119] |
| Enriched config / CSV | Named-column units above | Summary always; optional leg, wind-bin, trade and tonnage CSVs; input module mapping replaced by result mapping | [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L250-L280] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L309-L344] |

True wind angle is measured off the bow, folded for port/starboard symmetry; thrust is along track, with negative thrust receiving no propulsion credit; there are no position, origin or phase fields in these scalar calculations. [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L5-L9] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L109-L119] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L180-L200] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L283-L333]

CSV writing uses dictionary keys as headers and Python's text-mode default encoding; the module performs no automatic conversion of supplied units or locale-formatted numeric strings. [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L357-L385] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L547-L554]

## 3. Method and governing standards

| Method family / implementation inventory | Method and provenance | Evidence |
| --- | --- | --- |
| Endurance and energy chain: `endurance_hours`, `electrical_power_kw`, `fuel_mass_flow_kg_per_h` | Distance/speed; shaft/efficiency + hotel; electrical/plant efficiency multiplied by 3.6/LHV | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L336-L373] |
| Tank geometry: `cylindrical_tank_geometry` | Flat-ended cylinder, D = (4V/(pi L/D))^(1/3), L = (L/D)D, A = pi DL + pi D²/2 | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L376-L397] |
| Heat leak: `TankParameters.heat_flux_w_per_m2`, `boil_off` | q or U times temperature difference; Q = qA; mass rate = Q/latent heat, converted to daily rate and full-load mass fraction | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L210-L218] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L400-L425] |
| Voyage sizing: `size_fuel_chain_voyage`, nested `_bog_lost_total` | Sea burn plus margin; all port BOG lost; sea BOG lost only in lost mode; fixed-point tank sizing with absolute mass tolerance | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L428-L521] |
| Single passage: `size_fuel_chain` | Delegates tank sizing to a one-sea-leg voyage, with energy-chain summary recomputed through the named helpers | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L596-L648] |
| Wind lookup: `fold_twa_deg`, `interpolate_thrust`, `_bracket` | Bilinear interpolation; angle folding and clamping; high-speed edge clamp; squared low-speed scaling | [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L109-L154] |
| Wind integration: `wind_assist_saving` | Per-bin thrust times ship speed divided by propulsive efficiency; caps saving at shaft demand; sums probability weights, uncovered mass treated as calm | [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L157-L208] |
| Speed/power: `shaft_power_at_speed` | Piecewise-linear interpolation or reference power times speed ratio raised to a supplied exponent | [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L57-L78] |
| Sweep: `run_trade_study` | Delegates wind and fuel calculations, multiplies required mass by optional price, compares mass with a no-wind row | [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L81-L163] |
| Tonnage: `gross_tonnage` AND `tonnage` | Both independently compute gross-tonnage coefficient and product; `tonnage` also applies net-tonnage factors, passenger treatment and floors; code cites International Convention on Tonnage Measurement of Ships, 1969, Annex I, Regulations 3 (gross tonnage) and 4 (net tonnage) | [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L3-L33] [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L56-L106] |
| Router composition and cost summary: `router` | Delegates the four calculators, derives aggregate distance and energy summary, multiplies price by required mass, serialises results | [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L126-L248] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L257-L344] |

The constants docstring names NBS Monograph 168 (1981), NIST thermophysical data including Leachman et al. (2009), DOE hydrogen data, NIST ammonia/methanol data, CRC and BIPM; these are repository attributions, not independently checked source qualification. [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L5-L65]

The sole ISO 14687 mention is a hydrogen-property attribution without an edition, title, clause or table; it is not implemented as a hydrogen-quality check. [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L19-L22] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L72-L93]

UNVERIFIED: ISO 14687 edition/clause attribution and external qualification of preset constants remain unresolved; clause text not checked against the licensed copy; no edition conflict can be adjudicated from an unversioned mention; issue pending (1).

UNVERIFIED (U3): the tonnage source is cited by its convention title, year and regulation titles above; clause text not checked against the licensed copy; issue pending (1). [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L3-L33]

Preset factories return property dataclasses, and result dataclasses carry quantities and warnings without a `Citation` sidecar; the report demonstration supplies its citation list downstream. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L96-L147] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L299-L333] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L80-L89]

## 4. Validated worked examples

Execution record at the code pin: the three test files below collected 55 cases, all PASS at BASE; the final execution record is in section 10. Closed-form arithmetic with synthetic inputs qualifies the seven cases in the first table; this does not qualify the physical presets or full-scale performance. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L5-L9] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L87-L147] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L485] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L534-L545]

| Validated case / independent expected basis | Inputs / expected result and tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| V1 energy balance; dimensional closed form | Test constants and helper: 4000 nm, 12 kn, 4750/500 kW, efficiencies 0.95/0.50, synthetic LHV 120 MJ/kg; electrical 5500 kW, mass flow 330 kg/h, required mass 121000 kg; relative error at most 1e-12 (1) for these outputs | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelChain::test_energy_chain_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L56-L62] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L71-L110] |
| V2 cylindrical geometry; volume identity | Test literal gross volume and L/D 5; displayed references D 7.8 m, L 38.9 m, A 1048.5 m2; D = (4V/(5pi))^(1/3), L = 5D; volume closure relative error at most 1e-12 (1), rounded area reference relative error at most 1e-6 (1), area in m2 | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelChain::test_cylindrical_tank_geometry_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L118-L133] |
| V3 steady heat balance | Same test geometry, q = 1.5 W/m2; Q = qA, daily BOG = Q times seconds/day divided by latent heat; displayed references 1572.8 W and 304.7 kg/day; unrounded test literals govern relative error at most 1e-6 (1), BOR reference at most 1e-5 (1) | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelChain::test_boil_off_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L135-L147] |
| V4 bilinear midpoint; four-corner average | Supplied synthetic grid, 7.5 m/s at 45 deg; expected 37.5 kN; C08 bound 1e-6 kN supplements the test approximation | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestWindAssist::test_bilinear_interpolation_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L64-L68] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L450] |
| V5 probability-weighted work/power identity | Two bins with weights 0.50 and 0.25; expected saving = (0.50 times 100 + 0.25 times 20) times (12 times 1852/3600) / 0.70 kW (displayed 485.0 kW); test's rounded reference compared within relative 1e-6 (1) | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestWindAssist::test_wind_rose_saving_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L462-L485] |
| V6 speed-law arithmetic | Reference 4750 kW at 12 kn; halving speed with exponent 3 gives 4750/8 kW (displayed 593.8 kW); C10 bound 1e-6 kW supplements the test approximation; this validates arithmetic, not the exponent's empirical applicability | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestTradeStudy::test_cube_law_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L513-L538] |
| V7 straight-line interpolation | (10 kn, 3000 kW) and (14 kn, 7000 kW); 12 kn gives 5000 kW; C10 bound 1e-6 kW supplements the test approximation; one out-of-range speed raises | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestTradeStudy::test_speed_power_curve_interpolation` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L540-L545] |

For V4, V6 and V7 the pinned tests use `pytest.approx`; direct checks C08 and C10 additionally apply the stated absolute bounds. Displayed references use one decimal place; assertions use the unrounded values in the cited tests. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L450] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L534-L545]

| Regression case | Expected behaviour / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| R1 lost BOG fixed point | Required mass equals voyage fuel plus BOG loss, absolute error at most 1e-5 kg; rounded regression references also checked | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelChain::test_bog_lost_grows_tank_by_fixed_point` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L156-L175] |
| R2 port dwell | Synthetic two-sea-leg profile with 720 h port dwell; tank grows; total mass closure at most 1e-5 kg; no independent schedule benchmark | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestVoyageProfile::test_port_dwell_drives_tank_size` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L232-L237] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L256-L287] |
| R3 tonnage provisos | Both floor flags true; GT 30000 and NT 9000 (unit 1); numerical references compared within relative 1e-12 (1); convention compliance remains externally unchecked | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestTonnage::test_net_tonnage_floors_hand_verified` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L584-L595] |
| R4 serialisation | Four expected CSV files exist and trade CSV has 6 rows, positive required mass | `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py::test_router_writes_csv_outputs` | PASS; [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L96-L112] |
| R5 checked-in output replay | Three committed CSVs match fresh row/column structure and numeric values within relative 1e-12 (1); same implementation generated the comparison | `tests/report_pack/test_alt_fuel_report_pack_demo.py::test_checked_in_demo_csvs_match_a_fresh_run` | PASS; [tests/report_pack/test_alt_fuel_report_pack_demo.py:L25-L29] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L99-L119] |

## 5. QA gates

AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, G3 and G4 not applicable; G5 applies by AQG r0 sections 9 and 10.

| Gate | Applicability / module route | Checks |
| --- | --- | --- |
| G0 | Applicable: dictionary schema, scalar units, fuel-source provenance | C01–C03; external qualification remains open in section 10; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L350-L385] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L72-L93] |
| G1 | Applicable: Python package imports and direct-router tests; no solver deck or solver load route is emitted | C04; the test for engine registration reads source rather than executing the engine; [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L275-L286] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L257-L344] |
| G2 | Applicable: closed-form checks and separately marked regression checks | C05–C12; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L87-L215] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L545] |
| G3 | NOT_APPLICABLE: no time-domain dynamics solver; ordered legs are inventory accounting | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L467-L555] |
| G4 | NOT_APPLICABLE: no diffraction mesh/body solution; tank geometry is a scalar cylinder estimate | [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L376-L397] |
| G5 | Applicable: CSV trace and downstream generated report/citation evidence; full input-to-gate provenance remains open | C13–C14; [tests/report_pack/test_alt_fuel_report_pack_demo.py:L55-L96] |

Each check below is executable through the referenced existing tests, except the additional explicit bounds in C08 and C10, executable as `abs(interpolate_thrust(MATRIX, 7.5, 45.0) - 37.5) <= 1e-6` (kN), `abs(shaft_power_at_speed(inputs, 6.0) - 593.75) <= 1e-6` (kW, V6 inputs) and `abs(shaft_power_at_speed(inputs, 12.0) - 5000.0) <= 1e-6` (kW, V7 curve inputs); tolerances are software arithmetic criteria, not uncertainties in the physical inputs. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L100-L110] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L450]

| Check / gate | Function, quantity and numerical pass criterion | Evidence / existing executable |
| --- | --- | --- |
| C01 / G0 | Invalid endurance, efficiency, tank mode/ullage and absent heat-leak inputs: 6 of 6 probes raise `ValueError` (count); malformed router blocks and leg profiles also raise | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelChain::test_input_validation`; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L203-L215] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L126-L148] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L214-L230] |
| C02 / G0 | `fuel_preset`: 3 recognised presets agree exactly with configured constants (0 differing fields, count); unknown preset raises once | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelPresets::test_lh2_preset_is_the_default`; other preset tests in [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L388-L417] |
| C03 / G0 | Methanol U-value tank at equal ambient/storage temperature: exactly 1 `ValueError` (count), no BOG output accepted | `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestFuelPresets::test_methanol_u_value_tank_rejected_at_ambient`; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L432-L440] |
| C04 / G1 | Three related files collect 55 tests, failures 0, skipped 0 (count); imports and direct-router execution complete | Files and full execution record in section 10; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L16-L38] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L17-L26] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L19-L20] |
| C05 / G2 | `size_fuel_chain`: V1 electrical demand 5500 kW, mass flow 330 kg/h and required mass 121000 kg, relative error at most 1e-12 (1) | V1; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L99-L110] |
| C06 / G2 | `cylindrical_tank_geometry`: cylinder volume recomputation relative error at most 1e-12 (1), volume in m3 | V2; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L124-L133] |
| C07 / G2 | `boil_off`: heat leak and daily BOG against V3 test literals within relative 1e-6 (1), respectively W and kg/day | V3; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L142-L147] |
| C08 / G2 | `interpolate_thrust` at (7.5 m/s, 45 deg): absolute difference from 37.5 kN at most 1e-6 kN; low/high-speed and folded-angle tests also pass | Direct comparison and V4 plus `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestWindAssist::test_edge_handling`; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L443-L460] |
| C09 / G2 | `wind_assist_saving`: V5 saving against rounded test reference within relative 1e-6 (1), kW; cap test returns 500 kW saving and 0 kW remaining demand | V5 and `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestWindAssist::test_saving_capped_at_required_power`; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L479-L496] |
| C10 / G2 | `shaft_power_at_speed`: absolute difference from V6/V7 references at most 1e-6 kW in direct arithmetic checks; existing approximation assertions also pass; out-of-curve speed yields 1 exception (count) | V6/V7; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L534-L545] |
| C11 / G2 | `size_fuel_chain_voyage`: R1/R2 mass closure at most 1e-5 kg; port burn 0 kg, sea and port losses follow selected handling; excessive consumed-mode BOG produces at least 1 warning (count) | R1/R2; `tests/naval_architecture/test_alt_fuel_ship_sizing.py::TestVoyageProfile::test_per_leg_accounting`; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L173-L188] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L280-L305] [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L349-L373] |
| C12 / G2 | `tonnage`: 2 floor flags true (count), cargo and net outputs match stored arithmetic within relative 1e-12 (1); regression only, no independent compliance sign-off | R3; [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L584-L595] |
| C13 / G5 | `router`: 4 CSVs exist, 6 trade rows; voyage profile yields 3 leg rows (count) and positive port BOG in kg | R4 and `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py::test_voyage_profile_writes_legs_csv`; [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L96-L112] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L190-L199] |
| C14 / G5 | Sizing-to-report demonstration: 3 result tables render; 2 source IDs in citation JSON, 1 manifest with expected synthetic source identifier (count); report includes calculation-record appendix | `tests/report_pack/test_alt_fuel_report_pack_demo.py::test_demo_end_to_end_sizing_run_to_report_pack`; [tests/report_pack/test_alt_fuel_report_pack_demo.py:L55-L89] |

## 6. Known failure modes and pitfalls

sweep: 52 found

The count denotes reviewed semantic sites, including intentional defaults, guarded exceptions and unit assumptions; it is not a defect count. Docstring duplicates and zero comparisons that only raise are excluded from the count. Audit commands were:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|0\.10|0\.08|default_factory|cache' src/digitalmodel/alt_fuel_ship_sizing --glob '*.py'
rg -n '\b(int|float)\(|\.get\(|units?|kg|m3|nm|kn|kw|kW|deg|percent' src/digitalmodel/alt_fuel_ship_sizing --glob '*.py'
```

| Site | Symptom / assumption and cause at the pin | Detection / tracking |
| --- | --- | --- |
| S01 | Nonfinite positive-only properties can pass comparisons; positivity is not a finite-value contract; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L72-L93] | no detection check exists; issue pending (3) |
| S02 | Preset storage state and default LH2 properties are assumed unless overridden; ambient methanol heat flux still enters BOG arithmetic; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L96-L156] | C02/C03 cover preset equality and U rejection only; direct-flux misuse: no detection check exists; issue pending (2) |
| S03 | Tank defaults silently supply ullage 0.08, L/D 5.0, ambient 293.15 K and consumed handling; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L181-L208] | C05 covers the nominal fixture, no detection check exists for omitted-field intent; issue pending (3) |
| S04 | Supplied direct heat flux wins over U and bypasses temperature-difference validation; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L197-L218] | no detection check exists for conflicting heat-leak inputs; issue pending (3) |
| S05 | Missing per-leg power inherits voyage-level power even when leg speed differs; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L467-L487] | C11 includes a tested explicit override; no detection check exists for speed-dependent inherited power; issue pending (2) |
| S06 | Endurance and energy assume nm/kn, kW, LHV MJ/kg and hours without unit metadata; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L336-L373] | C05 checks dimensional arithmetic for synthetic inputs; issue pending (2) |
| S07 | Margin 0.10, maximum 50 iterations and 1e-6 kg tolerance are API defaults; exhausted iteration raises rather than returning a fallback; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L428-L438] | C11 covers converged mass closure; no detection check exists for invalid iteration controls; issue pending (3) |
| S08 | A manually constructed nonpositive-volume TankGeometry can make BOR fall back to 0 percent/day; typed dataclass fields are not validated; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L400-L425] | no detection check exists; issue pending (3) |
| S09 | Port legs set fuel consumption to 0 kg/h, excluding hotel energy during dwell; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L483-L490] | C11 checks zero port burn; scope qualification: issue pending (2) |
| S10 | Both lost-BOG accumulators (fixed-point loss calculation and final per-leg accounting) start at 0 kg; empty profiles are rejected first; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L460-L525] | C01/C11; intentional initialisation, issue pending (2) |
| S11 | Consumed-mode sea BOG is fully credited even above burn; warning is emitted, but extra loss is not added; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L530-L565] | C11 detects warning, no detection check exists for physical excess disposal sizing; issue pending (2) |
| S12 | Per-leg unconsumed/lost quantities and port distance/speed use literal zeros; these represent states, not missing data; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L530-L553] | C11; issue pending (2) |
| S13 | Leg and warning fields use empty-list factories; warnings remain empty unless excessive consumed BOG is found; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L299-L333] | C11 covers nominal empty warnings and excess warnings; issue pending (2) |
| S14 | Unknown fuel key catches KeyError and raises ValueError; this is a translated exception, not swallowed failure; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L159-L169] | C02 unknown-preset assertion; issue pending (3) |
| S15 | ThrustMatrix.from_lists converts all axes and rows through float; locale separators raise and nonfinite thrust may propagate; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L65-L73] | C08 covers numeric inputs only; no detection check exists for text/nonfinite conversion; issue pending (3) |
| S16 | Angle fold and range clamp assume symmetric off-bow degrees; out-of-range angle information is discarded; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L109-L119] | C08 folded-angle checks; issue pending (2) |
| S17 | Below-grid speed scales quadratically; above-grid speed silently holds the final row; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L121-L146] | C08 edge checks; physical polar qualification: issue pending (2) |
| S18 | Missing wind probability mass is assigned zero thrust rather than normalised; only excess total is rejected; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L163-L188] | C09 uses incomplete mass and C01 includes excessive mass; issue pending (2) |
| S19 | Power saving accumulator starts at zero; negative thrust is clipped to zero, saving capped at demand, zero-demand fraction returns zero; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L180-L205] | C09 tests cap; drag and zero-demand boundary: no detection check exists; issue pending (3) |
| S20 | Absent same-speed no-wind comparison silently writes 0 percent saving, indistinguishable from measured zero saving; [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L149-L163] | no detection check exists; issue pending (3) |
| S21 | Trade inputs default to LH2, reserve 0.10 and exponent 3.0; a law for small speed changes is not a resistance prediction; [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L40-L54] | C10 covers chosen exponent arithmetic; applicability: issue pending (2) |
| S22 | Curve converts tuple values through float, sorts speed/power tuples and returns the lowest power at the repeated queried speed; empty direct-API curve falls through to exponent law; [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L57-L78] | C10 covers ordinary interpolation and outside-range error; duplicate/empty curve: no detection check exists; issue pending (3) |
| S23 | No-wind rows initialise saving to 0 kW and fraction to 0; optional price carries caller currency without a currency field; [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L96-L146] | C05/C09 and existing fuel-cost regression; currency qualification: issue pending (2) |
| S24 | Tonnage defaults passenger counts to zero; direct API compares sign but does not enforce integer type; [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L64-L80] | no detection check exists for fractional passenger counts; issue pending (3) |
| S25 | Tonnage applies factor cap, cargo floor, zero-passenger proviso and NT floor; these are implemented criteria, not missing-data replacements; [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L89-L106] | C12; external convention qualification: issue pending (1) |
| S26 | Gross and net tonnage assume enclosed/cargo m3 and moulded dimensions m; no coordinate or geometry integration is performed; [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L56-L89] | C12 exercises supplied scalars; issue pending (2) |
| S27 | False-valued or absent top-level module settings collapse to an empty dictionary, then reject; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L126-L129] | C01 covers missing route, no detection check exists for false-valued module mapping; issue pending (3) |
| S28 | False-valued optional fuel mapping becomes defaults; margin float conversion supplies 0.10 when absent; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L151-L153] | C02 covers presets, no detection check exists for malformed optional mapping; issue pending (3) |
| S29 | False-valued wind mapping becomes empty; enabled uses Python truthiness, so string false is true; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L157-L159] | no detection check exists for string booleans; issue pending (3) |
| S30 | False-valued economics mapping becomes empty; optional price float accepts negative/nonfinite price without an explicit check; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L161-L163] | no detection check exists for invalid price; issue pending (3) |
| S31 | Disabled wind is serialised as 0 kW and zero saving fraction, while an enabled flag separately records the state; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L215-L224] | C13 and optional-block regression; issue pending (2) |
| S32 | False-valued trade settings silently omit the table; exponent float defaults to 3.0; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L282-L307] | no detection check exists for malformed false-valued trade mapping; issue pending (3) |
| S33 | False-valued tonnage mapping silently omits tonnage; int conversions default counts to zero and truncate numeric fractions; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L313-L334] | no detection check exists for fractional parser inputs; issue pending (3) |
| S34 | _positive converts external values using float; separator text raises, NaN passes the nonpositive comparison; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L357-L365] | C01 covers negative inputs only; no detection check exists for NaN/locale values; issue pending (3) |
| S35 | _non_negative converts external values using float; NaN passes the negative comparison; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L368-L378] | no detection check exists for NaN/locale values; issue pending (3) |
| S36 | _float_list converts every item through float; no finite-value or locale normalisation; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L381-L385] | no detection check exists for text/nonfinite list values; issue pending (3) |
| S37 | _bool_list converts every item with bool; nonempty textual false enables the option; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L388-L392] | no detection check exists; issue pending (3) |
| S38 | Absent or explicit null legs return None and choose single passage; missing type becomes empty text and rejects; supplied power uses float; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L395-L427] | C01 and single-passage regression cover absent legs; no detection check exists for null/invalid-power conversion; issue pending (3) |
| S39 | Four property overrides convert through float and inherit individual preset fields when omitted; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L430-L451] | C02 plus explicit-override regression; no detection check exists for nonfinite overrides; issue pending (3) |
| S40 | Tank parser converts five numeric settings with float; absent flux/U becomes None, other defaults inherit screening assumptions; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L454-L468] | C01/C03; no detection check exists for all malformed text paths; issue pending (3) |
| S41 | Missing or false-valued thrust rows become an empty list before shape rejection; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L471-L477] | Shape guard reviewed; no detection check exists for missing parser thrust rows; issue pending (3) |
| S42 | Missing wind-rose angle defaults to 0 degrees before float conversion; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L478-L490] | no detection check exists; issue pending (3) |
| S43 | Absent or explicit null speed-power curve returns None and selects the exponent law; supplied curve points use positive scalar converters; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L500-L520] | C10 covers direct curve arithmetic; no detection check exists for null parser intent; issue pending (3) |
| S44 | Output directory defaults to results and relative paths use configuration directory; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L526-L530] | C13; output-destination intent: issue pending (3) |
| S45 | No configuration path falls back to current directory; no input filename falls back to basename or module label; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L533-L544] | no detection check exists for unintended current-directory writes; issue pending (3) |
| S46 | CSV writer raises for empty rows; nonempty writes use mode w, default encoding and first-row keys, without atomic replacement; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L547-L554] | C13 covers successful files; partial-write detection: no detection check exists; issue pending (3) |
| S47 | _display_path catches ValueError outside repository and returns an absolute path; export may disclose local layout; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L557-L562] | no detection check exists; issue pending (3) |
| S48 | Workflow summary energy fields use design-point demand even when explicit per-leg powers differ; total inventory uses each leg; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L178-L196] | no detection check exists for interpreting summary flow as voyage average; issue pending (2) |
| S49 | Wind speed/angle and thrust assume m/s, degrees and kN; ship speed converts from knots before power integration; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L157-L188] | C09 checks synthetic dimensional identity; issue pending (2) |
| S50 | Geometry and BOG assume gross m3, area m2, latent kJ/kg and fixed full-load liquid mass; no depletion-dependent thermal model; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L376-L425] | C06/C07; model qualification: issue pending (2) |
| S51 | Empty bins factory and zero bracket fallback are valid result/lookup plumbing, not error returns; [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L100-L106] | C08/C09; bracket implementation [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L149-L154]; issue pending (2) |
| S52 | Explicit sea-leg `shaft_power_kw` bypasses wind reduction: the router supplies reduced power only as the voyage default, while the summary still reports wind savings; 4000 nm at 12 kn with explicit 4750 kW retains 121000.0 kg fuel while reporting 485.0 kW saving; [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L473-L475] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L167-L196] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L220-L228] | no detection check exists; issue pending (2) |

No record-skipping `continue`, error-path empty-list/dictionary return, epoch default or empty-result cache writer was found in the seven-file package; `_write_csv` rejects empty rows rather than caching them. [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L547-L554]

| Incident | Confirmed evidence / limit | Detection / tracking |
| --- | --- | --- |
| Engine configuration gap | The base-configuration entry is absent at the pin; the engine branch exists, while the regression test reads that branch as text and the demo calls the router directly | no detection check exists for the CLI path; #1606; [src/digitalmodel/engine.py:L311-L318] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L275-L286] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L19-L20] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L32-L37] |
| Profile versus trade-study scope | Design point includes port dwell; trade study deliberately sweeps aggregate sea distance as a single passage | no detection check exists for interpreting trade rows as full-schedule costs; issue pending (2); [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L74-L81] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L282-L308] |

## 7. Lessons log

| Date | Record | Lesson / change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-19 | Initial pack | Keep direct-router regression distinct from engine CLI coverage; document the configuration gap and absent detection | #1606; [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L275-L286] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L32-L37] | Proposed entry; no identifier assigned |
| 2026-10-19 | Initial pack | Separate closed-form arithmetic from empirical calibration, preset qualification and same-implementation CSV replay | [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L87-L110] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L99-L119] [docs/domains/ship-design/alt-fuel-lh2-sizing.md:L105-L109] | Proposed entry; no identifier assigned |

## 8. Ownership

Proposed review responsibilities: engineering calculations quality manager; deputy role: independent calculations reviewer designated by the maintainer; producer: calculation documentation contributor; escalation: unresolved source clauses, physical-model assumptions or tolerances go to the family quality manager, and cross-family scope goes to the maintainer. These are review assignments for this draft, not evidence of completed approval. (AQG r0, section 11, Sign-off and independence.)

## 9. Dependencies and implementation inventory

Upstream inputs are supplied route/powering data, fuel properties, tank thermal assumptions and user thrust/wind-rose tables; the module's internal dependencies are its constants, fuel-chain, wind, trade and tonnage components. [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L98-L120] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L131-L163]

The engine has a basename dispatch branch; the workflow returns CSV paths and results consumed by the report demonstration; the demo asserts two citation identifiers in `input_citations.json` and a synthetic source identifier in the provenance manifest. [src/digitalmodel/engine.py:L311-L318] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L340-L344] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L55-L89]

The sizing implementation uses Python arithmetic, math, dataclasses, csv and pathlib; no solver version is an input. The downstream demonstration uses YAML and the separate report workflow; the report dependency is tested without importing the engine. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L47-L67] [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L36-L39] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L32-L36] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L91-L120] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L12-L20]

### Map inventory and counted-family audit

Audit record: implementation-map ref and data revision are pinned in the header. All 13 map families were inspected; zero implementation rows have a path inside this package, so the section-9 map inventory is empty, there are zero counted families, and the counted grep is `NOT_APPLICABLE` because no map family has an in-package row. A8 inventory passes with that empty inventory; this is not evidence that the module lacks methods. All definitions implementing the package methods are listed in section 3, including both independent gross-tonnage computations. [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L56-L106]

Map follow-up: issue pending (4) covers energy/endurance, cylinder geometry and BOG heat balance, voyage inventory/fixed-point sizing, wind interpolation/integration, speed-power trade sweeps, tonnage and router summary/cost arithmetic; these families are not counted until the map has in-package rows. [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L336-L425] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L428-L593] [src/digitalmodel/alt_fuel_ship_sizing/wind_assist.py:L109-L208] [src/digitalmodel/alt_fuel_ship_sizing/trade_study.py:L57-L163] [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L56-L119] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L178-L248]

The additional repository-wide discovery search included `scripts/` and every tracked Python file; results outside the package are follow-up findings and do not change A8. Reproduce the name/formula search with:

```bash
git grep -nE 'alt_fuel_ship_sizing|fuel.chain|boil.off|gross_tonnage|net_tonnage|shaft_power_at_speed|interpolate_thrust|0\.02.*log10|[Hh]eat.*[Ll]eak' -- '*.py'
```

The other matched production definitions inspected were vessel API data readers holding a supplied gross-tonnage field, rather than computing the convention formula; these are data holders, not additional tonnage implementations. [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/marinetraffic_client.py:L226-L242] [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/shipxplorer_client.py:L154-L168]

## 10. Coverage and open items

Read coverage: all seven package Python files (`__init__.py`, `constants.py`, `lh2_fuel_chain.py`, `tonnage.py`, `trade_study.py`, `wind_assist.py`, `workflow.py`), including their docstrings; the package had no existing README, docs or nested Python package at the pin. The relevant external documentation found under `docs/` was the domain guide, operator map and routing registry. [src/digitalmodel/alt_fuel_ship_sizing/__init__.py:L1-L88] [docs/domains/ship-design/alt-fuel-lh2-sizing.md:L1-L148] [docs/maps/digitalmodel-operator-map.md:L83-L83] [docs/registry/module-routing.yaml:L535-L540]

The test inventory is 38 library tests, 14 workflow tests and 3 report-demonstration tests, totalling 55, rather than an assumed approximate count; seven selected validated arithmetic cases and five selected regression cases appear in section 4. The whole three-file inventory is exercised, not only those twelve examples. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L87-L636] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L36-L286] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L55-L128]

Execution record: BASE is `f1d58a2ac0db65e68b5b690f0a609ef4746d114e`. The BASE command used `PYTHONPATH=src uv run --no-sources python -m pytest tests/naval_architecture/test_alt_fuel_ship_sizing.py tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py tests/report_pack/test_alt_fuel_report_pack_demo.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base`; 55 run / 55 passed / 0 failed / 0 skipped / 0 NOT_RUN. The absolute executable and scratch locations are environment-specific. AFTER used the same command form with a separate scratch base directory on the uncommitted documentation tree: 55 run / 55 passed / 0 failed / 0 skipped / 0 NOT_RUN. The executed module code remains identical to the pin. Direct C08/C10 bounds also passed with zero absolute error. The routing/contracts domain completed with 1066 passed / 0 failed / 61 pre-existing skips after supplying the declared pytest-timeout plugin; its initial missing-plugin collection failure was resolved without changing test configuration.

Documentation recheck at starting commit `be1d317d0394799e2be9d84ca0e471b6f1295051`: the three cited files passed 55/55 before and 55/55 after the corrections, with zero failures or skips. The pack node-ID collection test also passed before and after; all 20 unique test citations across sections 4 and 5 collected separately. Runs used `PYTHONPATH=src uv run --no-project --with pytest --with pyyaml --with pydantic --with jinja2 --with plotly python -m pytest` with the three file paths above, `-q -rA -p no:cacheprovider` and a separate scratch `--basetemp` for each run. These checks ran on the uncommitted documentation tree; all seven package Python files still match the code pin. Direct C08/C10 checks passed with zero absolute error; probes reproduced S22 and S52 without changing their calculation behaviour.

No licensed interface, solver or private dataset is required by these three test files; the demo writes its results under the supplied temporary directory. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L12-L54] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L22-L26] [tests/report_pack/test_alt_fuel_report_pack_demo.py:L32-L51]

| Item | Limit / required follow-up | Tracking / responsible role |
| --- | --- | --- |
| U1 | Source qualification U1 from section 3: ISO 14687 edition/clause attribution and physical preset source accuracy; no licensed clause text checked; no source-level `Citation` sidecar in sizing outputs | issue pending (1); calculations maintainer; [src/digitalmodel/alt_fuel_ship_sizing/constants.py:L19-L22] [src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py:L299-L333] |
| U2 | UNVERIFIED: empirical applicability and independent full-voyage validation, including port hotel demand, methanol thermal model, excess BOG, inherited power, wind polars and full-schedule economics; sidecar entry: to be created if restricted data is needed | issue pending (2); calculations maintainer; [docs/domains/ship-design/alt-fuel-lh2-sizing.md:L105-L109] [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L74-L81] |
| Input/fallback coverage | S01–S52 enumerate coercion, unit and default assumptions; source-contract, physical-scope and parser/output gaps are grouped by cause in drafts 1–3 | issue pending (3); calculations maintainer; [src/digitalmodel/alt_fuel_ship_sizing/workflow.py:L350-L562] |
| Engine route | Existing source-presence assertion does not detect missing base configuration or exercise the CLI | #1606; calculations maintainer; [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L275-L286] |
| Map / dependency inventory | Method families have no in-package map rows; upstream/downstream dependency rows are not qualified by this pack | issue pending (4); registry maintainer; section-3 implementation citations and section-9 audit |
| U3 / Report provenance and convention qualification | C14 verifies downstream generated evidence but not input-to-gate fingerprints or preset source accuracy; convention criteria need external qualification before compliance claims | issue pending (1); calculations maintainer; [tests/report_pack/test_alt_fuel_report_pack_demo.py:L80-L89] [src/digitalmodel/alt_fuel_ship_sizing/tonnage.py:L3-L33] |

The source documents needed for this pack's template, QA gates and citation-sidecar contract were readable; external publications and licensed copies were not opened. Source claims above are explicitly limited to repository attributions. No private-sidecar entry identifier was available or created.
