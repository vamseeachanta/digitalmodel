# Production chemistry knowledge pack

| Header | Value |
| --- | --- |
| Module | production_chemistry |
| Repository | digitalmodel |
| Package path | src/digitalmodel/production_chemistry |
| Pack revision | 0 |
| Code revision | 309d343e08a2f83aebc6ea5344e586407b80c1f8 |
| Analysis family | Engineering calculations (EC), AQG r0 Python calculations |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The module computes mineral saturation indices for calcite, gypsum, hemihydrate, anhydrite, barite, celestite and halite, along with fixed-composition temperature/pressure profiles and ideal brine-mixing sweeps. [src/digitalmodel/production_chemistry/scale_si.py:L110-L118] [src/digitalmodel/production_chemistry/scale_si.py:L392-L435] [src/digitalmodel/production_chemistry/scale_si.py:L443-L471] [src/digitalmodel/production_chemistry/scale_si.py:L479-L543]

A positive SI denotes supersaturation tendency; absent participating ions yield NaN rather than a finite negative SI. [src/digitalmodel/production_chemistry/scale_si.py:L307-L326] [src/digitalmodel/production_chemistry/scale_si.py:L398-L403]

The routing registry records maturity `development`; the default coefficients require calibration before quantitative use and are explicitly not transcribed paper values. [docs/registry/module-routing.yaml:L399-L404] [src/digitalmodel/production_chemistry/scale_si.py:L28-L48]

The calculations assume mol/L can substitute for molality, approximate CO2 fugacity by partial pressure and absorb water activity into conditional constants; mixing and profiles do not simulate precipitation or chemical re-equilibration. [src/digitalmodel/production_chemistry/scale_si.py:L50-L66] [src/digitalmodel/production_chemistry/scale_si.py:L448-L456] [src/digitalmodel/production_chemistry/scale_si.py:L482-L488]

The exported API contains no inhibitor-dose calculation, transport solver or additional mineral families beyond the seven listed above; the explorer is a synthetic-data demonstration of the same Python calculations. [src/digitalmodel/production_chemistry/scale_si.py:L77-L118] [scripts/production_chemistry/build_scale_si_explorer.py:L4-L12] [scripts/production_chemistry/build_scale_si_explorer.py:L55-L112]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Allowed range and default | Source and evidence |
| --- | --- | --- | --- |
| `BrineComposition.na, k, mg, ca, sr, ba, fe, cl, so4, hco3` | mg/L | Each defaults to 0.0; negative values rejected; finite-value and charge-balance checks absent | Caller; bicarbonate is expressed as HCO3-, not CaCO3 alkalinity. [src/digitalmodel/production_chemistry/scale_si.py:L121-L160] |
| `tds` | mg/L | Optional; negative supplied values rejected; absent value becomes sum of listed ions | Caller; independent of the ionic-strength sum. [src/digitalmodel/production_chemistry/scale_si.py:L142-L173] |
| `ph` | dimensionless | Optional, default None; no numerical bounds enforced | Caller; measured-pH route only when positive gas CO2 is absent. [src/digitalmodel/production_chemistry/scale_si.py:L143-L155] [src/digitalmodel/production_chemistry/scale_si.py:L349-L389] |
| `co2_mole_fraction_gas` | mole fraction | Optional, default None; supplied range 0 to 1; positive value takes precedence over pH; zero falls back to pH | Caller; gas partial pressure is fraction times absolute pressure. [src/digitalmodel/production_chemistry/scale_si.py:L144-L155] [src/digitalmodel/production_chemistry/scale_si.py:L353-L389] |
| `t_f, p_psia` | degF, psia absolute | Required for `saturation_indices`; pressure must exceed 0; no temperature envelope or finite-value check | Caller. [src/digitalmodel/production_chemistry/scale_si.py:L392-L409] |
| `coefficients` | Mixed polynomial coefficient dimensions | Optional mapping overlay on defaults; empty mapping retains defaults; unspecified keys remain default | Caller calibration; coefficient dimensions must make the polynomial dimensionless with degF, psia and mol/L arguments. [src/digitalmodel/production_chemistry/scale_si.py:L186-L217] [src/digitalmodel/production_chemistry/scale_si.py:L404-L409] |
| `tp_profile` | Ordered (degF, psia) pairs | At least 1 pair; no interpolation; input order retained | Caller. [src/digitalmodel/production_chemistry/scale_si.py:L443-L471] |
| `fraction_b` / `fractions` | Volume fraction | Each in [0, 1]; sweep requires at least 1 value; converted with Python `float` | Caller; sweep defaults to 77.0 degF, 14.7 psia. [src/digitalmodel/production_chemistry/scale_si.py:L479-L515] [src/digitalmodel/production_chemistry/scale_si.py:L518-L543] |

### Outputs

| Output | Unit / range | Meaning and evidence |
| --- | --- | --- |
| `molarity(ion)` | mol/L | mg/L divided by molecular weight in g/mol and by 1000. [src/digitalmodel/production_chemistry/scale_si.py:L96-L108] [src/digitalmodel/production_chemistry/scale_si.py:L157-L160] |
| `ionic_strength` | mol/L | Half the sum of listed ionic molarities times squared charge; explicit TDS is not used. [src/digitalmodel/production_chemistry/scale_si.py:L169-L178] |
| `ScaleIndexResult` | SI, log10 IAP and conditional constant: dimensionless logarithmic quantities | Frozen result fields `family, si, log10_iap, pk_cond, basis, details`; carbonate pH route subtracts `pk_cond`, unlike sulfate and gas routes. [src/digitalmodel/production_chemistry/scale_si.py:L295-L304] [src/digitalmodel/production_chemistry/scale_si.py:L322-L338] [src/digitalmodel/production_chemistry/scale_si.py:L353-L384] |
| `details` | Ionic strength mol/L, gas partial pressure psia, estimated/used pH dimensionless | Ionic strength always supplied by the result builders; carbonate route adds route-specific fields. [src/digitalmodel/production_chemistry/scale_si.py:L332-L338] [src/digitalmodel/production_chemistry/scale_si.py:L349-L384] |
| `si_profile` DataFrame | Step integer; degF; psia; SI and delta SI dimensionless | One row per supplied pair; `dsi_<family>` subtracts the first row; no basis column. [src/digitalmodel/production_chemistry/scale_si.py:L458-L471] |
| `mix_brines` composition | mg/L, dimensionless pH and gas fraction | Linear ions/TDS, hydrogen-ion mixing for pH; missing route data may be dropped inside the interval. [src/digitalmodel/production_chemistry/scale_si.py:L490-L515] |
| `mixing_sweep` DataFrame | Fraction and SI dimensionless | `fraction_b, si_<family>` columns; no basis column. [src/digitalmodel/production_chemistry/scale_si.py:L533-L543] |

These scalar APIs read and write no external file format or spatial coordinate frame; the profile convention is supplied step order, with SI change measured from its first state. [src/digitalmodel/production_chemistry/scale_si.py:L392-L471] [src/digitalmodel/production_chemistry/scale_si.py:L479-L543]

## 3. Method and governing standards

The code names Oddo and Tomson, “Why Scale Forms in the Oil Field and Methods To Predict It,” *SPE Production & Facilities* 9(1), 1994, SPE-21710-PA, as its framework reference; this is a code attribution, not evidence that the default coefficients reproduce that paper. [src/digitalmodel/production_chemistry/scale_si.py:L6-L48]

UNVERIFIED: Correspondence of the implemented polynomial and carbonate routes with the published equations, and quantitative accuracy of the default coefficients, remain unchecked; the public publisher endpoint returned HTTP 403, and no full text was read; issue pending (2).

| Method | Implementation / equation as coded | Reference and citation sidecar |
| --- | --- | --- |
| Ionic bookkeeping | `c_i = concentration_i / molecular_weight_i / 1000`; `I = 0.5 * sum(c_i * z_i**2)` | Code-stated definition; independent arithmetic example in section 4; no standard clause named. [src/digitalmodel/production_chemistry/scale_si.py:L96-L108] [src/digitalmodel/production_chemistry/scale_si.py:L157-L178] |
| Conditional polynomial | `CorrelationCoefficients.value`: `a0 + a_t*T + a_t2*T**2 + a_t3*T**3 + a_p*P + a_sqrt_i*sqrt(I) + a_i*I` | Code calls this the Oddo–Tomson form; defaults are constructed values requiring calibration. [src/digitalmodel/production_chemistry/scale_si.py:L186-L224] |
| Sulfates and halite | `_sulfate_type_result`: `log10(c_cation*c_anion) + polynomial(T,P,I)`; halite uses Na/Cl | Conditional concentration product, with nonpositive product mapped to NaN. [src/digitalmodel/production_chemistry/scale_si.py:L307-L339] [src/digitalmodel/production_chemistry/scale_si.py:L414-L434] |
| Calcite gas route | `_calcite_result`: `log10(c_Ca*c_HCO3**2/fCO2) + K`; `fCO2 = yCO2*P`; estimated pH has its own coefficient set | Positive gas fraction selected first; fugacity coefficient fixed to 1.0 in the computation. [src/digitalmodel/production_chemistry/scale_si.py:L349-L371] |
| Calcite pH route | `_calcite_result`: `log10(c_Ca*c_HCO3) + ph - K`; otherwise raises ValueError | Caller pH assumed valid at the stated T/P. [src/digitalmodel/production_chemistry/scale_si.py:L373-L389] |
| Profile | `si_profile`: repeated `saturation_indices`; subtract first state for each family's delta | Fixed composition, no interpolation or precipitation. [src/digitalmodel/production_chemistry/scale_si.py:L448-L471] |
| Mixing | `mix_brines`: `(1-f)*c_A + f*c_B`; `pH_mix = -log10((1-f)*10**(-pH_A) + f*10**(-pH_B))`; `mixing_sweep` repeats SI | Ideal volumetric mixing, no carbonate re-speciation. [src/digitalmodel/production_chemistry/scale_si.py:L482-L515] [src/digitalmodel/production_chemistry/scale_si.py:L533-L543] |

No governing licensed standard or clause is named in the module's basis; its result provenance is a `basis` string, with no structured `Citation` sidecar emitted by either result builder; provenance follow-up: issue pending (4). [src/digitalmodel/production_chemistry/scale_si.py:L6-L66] [src/digitalmodel/production_chemistry/scale_si.py:L295-L339] [src/digitalmodel/production_chemistry/scale_si.py:L349-L389]

## 4. Validated worked examples and regression examples

The four-ion arithmetic case is a closed-form check of ionic strength, not a validation of mineral SI or empirical coefficients; the remaining cases are contract, algebra or qualitative regressions with no independent quantitative SI dataset. [tests/production_chemistry/test_scale_si.py:L6-L13] [tests/production_chemistry/test_scale_si.py:L84-L95] [src/digitalmodel/production_chemistry/scale_si.py:L28-L48]

Execution record at the header code pin: `PYTHONPATH=src uv run --no-sources python -m pytest tests/production_chemistry/ -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base` passed all 18 nodes at BASE and again on the uncommitted documentation tree using `bt-after` as the scratch basename; each run had 0 failed, 0 skipped and 0 NOT_RUN nodes. The existing pytest addopts were retained; no solver was imported or called.

The input file for every case is the cited test source; its synthetic fixtures are `formation_water` and `seawater`, except for the inline four-ion composition; the seawater fixture's literature attribution is not treated as independent SI validation. [tests/production_chemistry/test_scale_si.py:L33-L95]

| Case / classification | Expected result, units and tolerance basis | Existing test node ID | Offline result at pin / evidence |
| --- | --- | --- | --- |
| Validated arithmetic only: four-ion closed form | 0.14 mol/L; 0.1 mol/L Na and 0.01 mol/L Ca; relative tolerance 1e-3, as asserted | `tests/production_chemistry/test_scale_si.py::test_ionic_strength_hand_check` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L84-L95] |
| Input rejection regression | Na = -1.0 mg/L raises ValueError | `tests/production_chemistry/test_scale_si.py::test_negative_concentration_rejected` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L98-L101] |
| Result contract regression | 7 finite family SIs; each basis contains the coefficient warning | `tests/production_chemistry/test_scale_si.py::test_all_families_returned_with_basis` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L109-L117] |
| Missing-route regression | Both route inputs absent raises ValueError | `tests/production_chemistry/test_scale_si.py::test_calcite_requires_carbonate_route` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L120-L123] |
| Gas-route regression | Gas route chosen; estimated pH between 3.0 and 9.0 | `tests/production_chemistry/test_scale_si.py::test_calcite_gas_route_preferred_and_reports_estimated_ph` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L126-L129] |
| Coefficient algebra regression | Barite a0 increase 1.0 changes SI by 1.0; absolute tolerance 1e-12 SI | `tests/production_chemistry/test_scale_si.py::test_coefficient_override_shifts_si_by_a0_delta` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L132-L144] |
| Temperature trend regression | SI(250 degF) > SI(100 degF) at 3000 psia, both routes | `tests/production_chemistry/test_scale_si.py::test_calcite_si_increases_with_temperature` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L152-L163] |
| Pressure trend regression | SI(500 psia) > SI(6000 psia) at 180 degF | `tests/production_chemistry/test_scale_si.py::test_pressure_drop_increases_calcite_si` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L166-L173] |
| Barite trend regression | SI(200 degF) < SI(80 degF) at 2000 psia | `tests/production_chemistry/test_scale_si.py::test_barite_si_decreases_with_temperature_at_low_t` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L176-L182] |
| Phase ordering regression | Anhydrite SI below gypsum at 70 degF and above at 250 degF, 1000 psia | `tests/production_chemistry/test_scale_si.py::test_anhydrite_overtakes_gypsum_at_high_temperature` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L185-L193] |
| Halite behavior regression | Typical SI < -0.5; modified composition gives larger SI with absolute SI < 0.4 | `tests/production_chemistry/test_scale_si.py::test_halite_only_near_saturation_tds` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L196-L207] |
| Profile contract regression | 10 ordered rows; 7 initial deltas within 1e-12 of 0 SI | `tests/production_chemistry/test_scale_si.py::test_si_profile_shape_order_and_dsi` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L215-L232] |
| Profile trend regression | 5 pressure steps at 180 degF have strictly increasing calcite SI | `tests/production_chemistry/test_scale_si.py::test_si_profile_pure_depressurization_is_monotonic_for_calcite` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L235-L241] |
| Empty-profile regression | Empty profile raises ValueError | `tests/production_chemistry/test_scale_si.py::test_si_profile_empty_rejected` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L244-L246] |
| Endpoint self-comparison regression | 7 family endpoints match scalar outputs within 1e-9 SI | `tests/production_chemistry/test_scale_si.py::test_mixing_endpoints_equal_pure_brines` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L254-L268] |
| Mixing trend regression | Barite maximum above both endpoints; peak fraction strictly between 0 and 1 | `tests/production_chemistry/test_scale_si.py::test_seawater_mixing_drives_barite_si_up` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L271-L283] |
| Mixing algebra regression | Fraction 0.25: Ba, sulfate and H-ion pH mix agree at relative tolerance 1e-12; gas fraction dropped | `tests/production_chemistry/test_scale_si.py::test_mix_brines_linear_concentrations_and_h_ion_mixing` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L286-L296] |
| Fraction rejection regression | Fraction 1.5 raises ValueError | `tests/production_chemistry/test_scale_si.py::test_mixing_fraction_out_of_range_rejected` | BASE PASS; AFTER PASS. [tests/production_chemistry/test_scale_si.py:L299-L301] |

## 5. QA gates

Gate mapping: AQG r0, section 10, Python calculations; G5 applies under section 9. The checks below use the existing module tests and a retained execution/citation manifest; passing arithmetic or regression checks does not establish calibration of the default SI correlations. [src/digitalmodel/production_chemistry/scale_si.py:L28-L48]

| Gate | Applicability and route | Module-specific checks |
| --- | --- | --- |
| G0 inputs | Applicable: caller concentration units and carbonate-route selection | C01, C02, C03 |
| G1 imports and tests | Applicable: Python import through offline pytest; no target solver program | C04, C05 |
| G2 hand or published check | Applicable: closed-form ionic-strength arithmetic; coefficient and mixing regressions are supplemental | C06, C07, C08, C09 |
| G3 short dynamics | NOT_APPLICABLE: scalar equilibrium-tendency calculations have no time integrator. [src/digitalmodel/production_chemistry/scale_si.py:L392-L435] | None |
| G4 diffraction | NOT_APPLICABLE: outputs contain chemistry results rather than body/mesh/diffraction quantities. [src/digitalmodel/production_chemistry/scale_si.py:L295-L304] | None |
| G5 evidence | Applicable: retain input/code fingerprints, test outcomes and basis warnings; automated output sidecar remains open, issue pending (4) | C10 |

The table defines 10 checks: G0 = 3, G1 = 2, G2 = 4 and G5 = 1; test node suffixes below resolve to the full IDs in section 4.

| Check | Executable comparison | Numeric pass criterion and unit | Evidence / gate |
| --- | --- | --- | --- |
| C01 | `test_negative_concentration_rejected`: construct negative sodium | 1 ValueError for -1.0 mg/L sodium; 0 accepted negative cases | Node outcome; G0. [tests/production_chemistry/test_scale_si.py:L98-L101] |
| C02 | `test_calcite_requires_carbonate_route`: call `saturation_indices` with both carbonate inputs absent | 1 ValueError containing calcite; 0 result dictionaries | Node outcome; G0. [tests/production_chemistry/test_scale_si.py:L120-L123] |
| C03 | `test_mixing_fraction_out_of_range_rejected`: call `mix_brines` with fraction 1.5 | 1 ValueError; 0 accepted out-of-range fractions (dimensionless) | Node outcome; G0. [tests/production_chemistry/test_scale_si.py:L299-L301] |
| C04 | `test_all_families_returned_with_basis`: import and evaluate `saturation_indices` | 7 returned families, 7 finite SI values (dimensionless), 0 missing basis warnings | Node outcome; G1. [tests/production_chemistry/test_scale_si.py:L109-L117] |
| C05 | Run all section-4 node IDs with the section-4 command | 18 passed nodes; 0 failed, skipped or NOT_RUN nodes | Full pytest log; G1. [tests/production_chemistry/test_scale_si.py:L84-L301] |
| C06 | `test_ionic_strength_hand_check`: independent sum `0.5*(0.1+0.1+4*0.01+4*0.01)` | I = 0.14 mol/L within relative 1e-3 (0.1 %); also Na = 0.1 and Ca = 0.01 mol/L at the same relative tolerance | Node outcome; G2. [tests/production_chemistry/test_scale_si.py:L84-L95] |
| C07 | `test_coefficient_override_shifts_si_by_a0_delta`: barite a0 increment | Delta SI = 1.0 within absolute 1e-12 SI (dimensionless) | Node outcome; G2. [tests/production_chemistry/test_scale_si.py:L132-L144] |
| C08 | `test_mix_brines_linear_concentrations_and_h_ion_mixing`: fractional ions and hydrogen mixing | Ba and sulfate in mg/L and mixed pH (dimensionless) agree at relative 1e-12 for fraction 0.25; 1 missing gas field | Node outcome; G2. [tests/production_chemistry/test_scale_si.py:L286-L296] |
| C09 | `test_si_profile_shape_order_and_dsi`: supplied profile order and deltas | 10 rows in exact T/P order; 7 first-row delta SIs within absolute 1e-12 of zero (dimensionless) | Node outcome; G2. [tests/production_chemistry/test_scale_si.py:L215-L232] |
| C10 | Retain run manifest with SHA-256 of the package and test inputs, pin, both test counts and section-4 IDs; C04 checks scalar warnings | 18 node outcomes per run; 0 missing input digests; 7 scalar results containing the coefficient warning | Execution manifest and C04 node; G5. [tests/production_chemistry/test_scale_si.py:L109-L117] [src/digitalmodel/production_chemistry/scale_si.py:L284-L304] |

## 6. Known failure modes and pitfalls

sweep: 39 found

The following inventory counts distinct defaults, unit assumptions and error/provenance paths rather than matching lines; each zero-valued ion and polynomial coefficient has its own row. Discovery command, run against the pinned package before documentation was added:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5\s*%|int\(|float\(|mg/L|mol/L|molality|degF|psia|cache|fugacity' src/digitalmodel/production_chemistry
```

A subsequent AST inspection covered every definition and default in both Python files; there are no swallowed exceptions, record-skipping `continue` statements, `return None`, empty-container error returns or cache writes in the package; the empty details/rows containers initialize normal result construction. [src/digitalmodel/production_chemistry/__init__.py:L19-L41] [src/digitalmodel/production_chemistry/scale_si.py:L121-L543]

| Finding | Symptom / cause at pin | Existing detection check or gap | Disposition |
| --- | --- | --- | --- |
| S01 | `na=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L132-L132] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S02 | `k=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L133-L133] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S03 | `mg=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L134-L134] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S04 | `ca=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L135-L135] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S05 | `sr=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L136-L136] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S06 | `ba=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L137-L137] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S07 | `fe=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L138-L138] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S08 | `cl=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L139-L139] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S09 | `so4=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L140-L140] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S10 | `hco3=0.0` mg/L default can silently omit that ion from ionic strength and its participating products. [src/digitalmodel/production_chemistry/scale_si.py:L141-L141] | C04 checks only the complete fixture; no detection check exists for omitted ions | issue pending (1) |
| S11 | Absent TDS becomes sum of listed ions, potentially understating omitted species. [src/digitalmodel/production_chemistry/scale_si.py:L163-L167] | no detection check exists | issue pending (1) |
| S12 | pH defaults to None; calculation depends on another carbonate route. [src/digitalmodel/production_chemistry/scale_si.py:L143-L143] | C02 detects when both routes are absent | issue pending (1) |
| S13 | Gas CO2 defaults to None, allowing the measured-pH fallback. [src/digitalmodel/production_chemistry/scale_si.py:L144-L144] | C02 detects absence of both routes; no detection check exists for all route combinations | issue pending (1) |
| S14 | Zero gas CO2 selects pH rather than a gas-route result. [src/digitalmodel/production_chemistry/scale_si.py:L353-L389] | no detection check exists for zero gas fraction | issue pending (1) |
| S15 | `a0=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L198-L198] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S16 | `a_t=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L199-L199] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S17 | `a_t2=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L200-L200] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S18 | `a_t3=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L201-L201] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S19 | `a_p=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L202-L202] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S20 | `a_sqrt_i=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L203-L203] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S21 | `a_i=0.0` default silently removes its polynomial term. [src/digitalmodel/production_chemistry/scale_si.py:L204-L204] | C07 checks only barite a0 override; no detection check exists for each omitted term | issue pending (2) |
| S22 | Constructed coefficient defaults can appear quantitatively authoritative without calibration. [src/digitalmodel/production_chemistry/scale_si.py:L220-L272] | C04 checks warning transport, not accuracy; no detection check exists for independent SI accuracy | issue pending (2) |
| S23 | Nonpositive concentration product becomes NaN, then propagates through SI and delta SI. [src/digitalmodel/production_chemistry/scale_si.py:L307-L326] | C04 checks finite values only for complete fixture; no detection check exists for missing-ion NaN behavior | issue pending (1) |
| S24 | Implicit molecular-weight and mg/L-to-mol/L conversion; caller unit errors are not distinguishable. [src/digitalmodel/production_chemistry/scale_si.py:L96-L108] | C06 checks the four-ion arithmetic; no detection check exists for mislabeled caller units | issue pending (2) |
| S25 | Molarity substituted for molality; high-TDS density effects omitted. [src/digitalmodel/production_chemistry/scale_si.py:L52-L54] | no detection check exists | issue pending (2) |
| S26 | Fugacity coefficient fixed at 1.0 even for nonideal gas conditions. [src/digitalmodel/production_chemistry/scale_si.py:L353-L369] | no detection check exists | issue pending (2) |
| S27 | Water activity and hydration absorbed in conditional constants. [src/digitalmodel/production_chemistry/scale_si.py:L62-L63] | no detection check exists | issue pending (2) |
| S28 | Profile fixes composition; precipitation or depletion along the path omitted. [src/digitalmodel/production_chemistry/scale_si.py:L448-L456] | C09 checks order and deltas only; no detection check exists for chemical depletion | issue pending (2) |
| S29 | Linear volumetric ion and TDS mixing assumes no volume or equilibrium correction. [src/digitalmodel/production_chemistry/scale_si.py:L482-L497] | C08 checks arithmetic only; no detection check exists for physical mixing accuracy | issue pending (2) |
| S30 | Hydrogen-ion mixing uses ideal concentration arithmetic without re-speciation. [src/digitalmodel/production_chemistry/scale_si.py:L499-L501] | C08 checks arithmetic only; no detection check exists for physical mixing accuracy | issue pending (2) |
| S31 | Missing pH on one input is dropped at interior fractions. [src/digitalmodel/production_chemistry/scale_si.py:L499-L505] | no detection check exists for one-sided missing pH | issue pending (1) |
| S32 | Missing gas fraction on one input is dropped at interior fractions. [src/digitalmodel/production_chemistry/scale_si.py:L507-L513] | C08 checks one such interior mixture; no detection check exists for the complete route matrix | issue pending (1) |
| S33 | Sweep defaults to 77.0 degF if caller omits temperature. [src/digitalmodel/production_chemistry/scale_si.py:L522-L522] | no detection check exists for omitted temperature | issue pending (1) |
| S34 | Sweep defaults to 14.7 psia if caller omits pressure. [src/digitalmodel/production_chemistry/scale_si.py:L523-L523] | no detection check exists for omitted pressure | issue pending (1) |
| S35 | External fraction converted by float before mixing: formatted decimal/thousands text is not normalized. [src/digitalmodel/production_chemistry/scale_si.py:L535-L535] | no detection check exists for external text parsing | issue pending (1) |
| S36 | Fraction converted by float again when serializing row metadata. [src/digitalmodel/production_chemistry/scale_si.py:L537-L537] | no detection check exists for repeated external-text conversion | issue pending (1) |
| S37 | Concentration, TDS, pH, temperature and pressure lack a complete finite-value/domain envelope. [src/digitalmodel/production_chemistry/scale_si.py:L146-L155] | C01 and C03 cover negative sodium and one fraction only; no detection check exists for nonfinite inputs | issue pending (1) |
| S38 | Profile and sweep return SI columns without scalar basis or coefficient provenance. [src/digitalmodel/production_chemistry/scale_si.py:L458-L471] | C10 records this audit only; no detection check exists for output provenance retention | issue pending (4) |
| S39 | Explicit TDS is not used in the ionic-strength sum; omitted ions remain omitted. [src/digitalmodel/production_chemistry/scale_si.py:L163-L173] | C06 checks the explicit-ion sum only; no detection check exists for TDS/ion inconsistency | issue pending (1) |

No incident issue was supplied for this module; repository issue search found the original capability request #1295, whose closed state does not demonstrate completion of independent SI calibration, additional mineral families or inhibitor dosing; the public API and tests document the delivered subset. [src/digitalmodel/production_chemistry/__init__.py:L1-L17] [src/digitalmodel/production_chemistry/scale_si.py:L77-L118] [tests/production_chemistry/test_scale_si.py:L6-L13]

The gas-route coefficient anchor comment contains a written sum that does not equal its stated total; this comment must not be used as an independent reference value; no detection check exists, issue pending (2). [src/digitalmodel/production_chemistry/scale_si.py:L231-L241]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change / shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | A qualitative trend suite does not validate empirical SI accuracy; preserve the calibration warning with every result | [src/digitalmodel/production_chemistry/scale_si.py:L28-L48] [tests/production_chemistry/test_scale_si.py:L6-L13] | Sections 3–6; provenance follow-up issue pending (4) |
| 2026-11-07 | Initial pack | Mixed-water route selection can change when only one input supplies gas CO2 | [src/digitalmodel/production_chemistry/scale_si.py:L507-L513] [tests/production_chemistry/test_scale_si.py:L286-L296] | Sections 2 and 6; input-coverage follow-up issue pending (1) |

## 8. Ownership

Review responsibility for this draft: engineering calculations family quality manager; deputy responsibility: engineering calculations deputy quality manager; production responsibility: module maintainers. Clearance requires a different provider from the producer; ambiguous methods, reference tolerances and missing data go to the family quality manager, with unresolved cross-family decisions going to the maintainers (AQG r0, section 11, “Sign-off and independence”).

## 9. Dependencies and implementation inventory

The computation module imports Python `math`, `dataclasses`, typing interfaces and pandas; the package initializer re-exports its API, and the explorer generator delegates calculation to that API before serializing rounded results. [src/digitalmodel/production_chemistry/scale_si.py:L69-L75] [src/digitalmodel/production_chemistry/__init__.py:L19-L41] [scripts/production_chemistry/build_scale_si_explorer.py:L19-L28] [scripts/production_chemistry/build_scale_si_explorer.py:L55-L112]

| Dependency direction | Module / data / version | Evidence |
| --- | --- | --- |
| Upstream runtime | pandas, declared >=2.0.0 and <3.0.0; standard-library math/dataclasses | [pyproject.toml:L52-L59] [src/digitalmodel/production_chemistry/scale_si.py:L69-L75] |
| Input data | Caller `BrineComposition`, coefficients and state/fraction sequences; synthetic test fixtures | [src/digitalmodel/production_chemistry/scale_si.py:L121-L144] [src/digitalmodel/production_chemistry/scale_si.py:L392-L409] [tests/production_chemistry/test_scale_si.py:L33-L76] |
| Downstream consumer | `scripts/production_chemistry/build_scale_si_explorer.py`, generated explorer | [scripts/production_chemistry/build_scale_si_explorer.py:L22-L31] [scripts/production_chemistry/build_scale_si_explorer.py:L55-L112] |
| Inventory / routing | `production_chemistry`, development, module tests in `tests/production_chemistry/` | [docs/registry/module-routing.yaml:L399-L404] |
| Solver / external package inventory rows | No solver interface in the computation path; pandas is an external dependency rather than an engineering module row | [src/digitalmodel/production_chemistry/scale_si.py:L69-L75] [src/digitalmodel/production_chemistry/scale_si.py:L392-L543] |

### Map inventory and counted-family audit

Audit source: `docs/registry/implementation-map.yaml` at ref `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; package scope is the header package with no excluded subpackages.

Audit result: 13 map families examined; 0 implementation rows have a `path` inside `src/digitalmodel/production_chemistry/`; therefore the module-family inventory is empty, counted families = 0, counted-family grep = `NOT_APPLICABLE` because no map family has an in-package row; A8 inventory condition passes with an empty inventory. This records an audit result at the separate map pin, not an assertion that chemistry has no method families.

| Module map family | Every implementation row for those families | Counted-family rank / grep |
| --- | --- | --- |
| None at the stated map pin | Empty inventory | NOT_APPLICABLE: 0 in-package rows |

Map follow-up: add ionic-strength bookkeeping, conditional-solubility polynomials, carbonate gas/pH routes, mineral saturation indices and ideal volumetric brine mixing before they become counted families; include profile and mixing wrappers as consumers; issue pending (3). [src/digitalmodel/production_chemistry/scale_si.py:L157-L178] [src/digitalmodel/production_chemistry/scale_si.py:L186-L217] [src/digitalmodel/production_chemistry/scale_si.py:L307-L435] [src/digitalmodel/production_chemistry/scale_si.py:L443-L543]

### Repository-wide further search

The additional search covered tracked text throughout the repository, including `scripts/`, at the code pin:

```sh
git grep -n -I -E 'Oddo|Tomson|def (saturation_indices|ionic_strength|mix_brines|mixing_sweep)|calcite_co2|ph_from_co2' HEAD -- . ':!uv.lock'
```

Audit result: 36 hits in 9 files; the Python hits are the package, its tests, the explorer generator and capability-card generation. The generator's chemistry calculations delegate to the listed package functions; the capability generator describes and links the explorer rather than evaluating chemistry. No additional independent mineral-SI computation was found by this search; this is a bounded further-search result and does not alter A8. [scripts/production_chemistry/build_scale_si_explorer.py:L55-L112] [scripts/capabilities/build_onepagers.py:L400-L410] [tests/production_chemistry/test_scale_si.py:L20-L30]

## 10. Coverage and open items

### Read coverage

Both package Python files and both test Python files were inspected; there is no separate module README or pre-existing package documentation at the pin; the package's documentation is its module docstrings. [src/digitalmodel/production_chemistry/__init__.py:L1-L41] [src/digitalmodel/production_chemistry/scale_si.py:L1-L67] [tests/production_chemistry/__init__.py:L1-L2] [tests/production_chemistry/test_scale_si.py:L1-L301]

| Surface read | Module-related content located at the code pin |
| --- | --- |
| `README.md` | Generated architecture node for the explorer producer and source. [README.md:L455-L455] |
| `docs/registry/module-routing.yaml` | Routing and maturity. [docs/registry/module-routing.yaml:L399-L404] |
| `docs/maps/digitalmodel-operator-map.md` | API/test routing and dependency summary. [docs/maps/digitalmodel-operator-map.md:L61-L61] |
| `docs/api/production/scale-si-explorer.html` | Demonstration, assumptions and warning; embedded outputs are not independent reference values. [docs/api/production/scale-si-explorer.html:L45-L90] |
| `docs/api/ffs/build-wave-2026-07.html` | Capability summary. [docs/api/ffs/build-wave-2026-07.html:L119-L125] |
| `docs/capability-map/capability-cards.json` | Chemistry capability descriptions. [docs/capability-map/capability-cards.json:L335-L335] [docs/capability-map/capability-cards.json:L355-L355] |
| `docs/capability-map/repo-architecture.json` | Producer, calculation source and inventory limitation. [docs/capability-map/repo-architecture.json:L1203-L1220] |
| `docs/capability-map/repo-architecture.mmd` | Generator/source node. [docs/capability-map/repo-architecture.mmd:L215-L215] |
| `docs/capability-map/repo-architecture.html` | Generated report routing and execution limitation. [docs/capability-map/repo-architecture.html:L58-L58] |
| `docs/small_operator/problems.yml` | Package listed as relevant to chemistry screening. [docs/small_operator/problems.yml:L326-L342] |
| `docs/audits/module-report-evidence.json` | Explorer generator recorded in the audit inventory. [docs/audits/module-report-evidence.json:L1535-L1538] |
| `scripts/production_chemistry/build_scale_si_explorer.py` | Synthetic brines, caller, serialization and page generation; read only, not regenerated. [scripts/production_chemistry/build_scale_si_explorer.py:L1-L112] |
| `scripts/capabilities/build_onepagers.py` | Chemistry capability description for further-implementation classification. [scripts/capabilities/build_onepagers.py:L400-L410] |

Discovery used `rg -l -F -e production_chemistry -e Oddo -e Tomson docs`, followed by the same literal-term search in the root and routing READMEs; the table records ten documentation surfaces and one root README match. The module test inventory is the 18 full node IDs in section 4; its existing domain route is `marine-ops-other`. [tests/production_chemistry/__init__.py:L1-L2] [tests/production_chemistry/test_scale_si.py:L84-L301]

### Open items

| Item | Evidence / limit | Responsible role | Issue |
| --- | --- | --- | --- |
| Nonfinite inputs, zero-ion behavior, route/default coverage and text-fraction parsing | Section-6 rows S01–S14, S23, S30–S37 and S39; existing tests do not cover the full input matrix. [src/digitalmodel/production_chemistry/scale_si.py:L132-L155] [src/digitalmodel/production_chemistry/scale_si.py:L499-L543] | Module maintainers | issue pending (1) |
| Unchecked published basis and quantitative SI calibration | The single UNVERIFIED claim in section 3; no independent full-SI worked example; polynomial anchor-comment inconsistency and all approximation limits remain open. [src/digitalmodel/production_chemistry/scale_si.py:L28-L63] [src/digitalmodel/production_chemistry/scale_si.py:L231-L241] [tests/production_chemistry/test_scale_si.py:L6-L13] | Engineering calculations quality manager and module maintainers | issue pending (2) |
| Missing implementation-map families | Empty inventory at the map pin; section-3 methods need map entries before counted-family searches apply. [src/digitalmodel/production_chemistry/scale_si.py:L157-L217] [src/digitalmodel/production_chemistry/scale_si.py:L307-L543] | Registry maintainers | issue pending (3) |
| Structured citations, fingerprints and generated QA evidence for scalar/profile/sweep outputs | Basis strings exist for scalar results; tabular outputs omit them; no structured citation sidecar or generated gate appendix is emitted. [src/digitalmodel/production_chemistry/scale_si.py:L295-L304] [src/digitalmodel/production_chemistry/scale_si.py:L458-L471] [src/digitalmodel/production_chemistry/scale_si.py:L533-L543] | Module maintainers | issue pending (4) |

The original capability issue #1295 is a scope reference rather than an independent benchmark or a detection check; only the implemented subset is described here. [src/digitalmodel/production_chemistry/__init__.py:L1-L17] [src/digitalmodel/production_chemistry/scale_si.py:L77-L118]

No client-derived values are included; the worked inputs are the repository's synthetic fixtures, so no private sidecar entry is required for these examples. [tests/production_chemistry/test_scale_si.py:L33-L76]
