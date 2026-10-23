# Production chemistry knowledge pack

| Header | Value |
| --- | --- |
| Module | `production_chemistry` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/production_chemistry` |
| Pack revision | 0 |
| Code revision | `705857af62385a4567c96be99a129cdff3051a52` |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Analysis family | Engineering calculations; EC |
| Quality manager | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package computes conditional-solubility saturation indices for calcite, gypsum, hemihydrate, anhydrite, barite, celestite and halite; positive SI denotes supersaturation, and absent ion products yield NaN. [src/digitalmodel/production_chemistry/scale_si.py:L110-L118] [src/digitalmodel/production_chemistry/scale_si.py:L307-L339] [src/digitalmodel/production_chemistry/scale_si.py:L392-L435]

`si_profile` evaluates a fixed composition along an ordered temperature/pressure path, and `mixing_sweep` evaluates ideal volumetric brine mixtures without precipitation or re-equilibration. [src/digitalmodel/production_chemistry/scale_si.py:L443-L471] [src/digitalmodel/production_chemistry/scale_si.py:L479-L543]

Registry maturity is `development`; the defaults require calibration before quantitative use and are explicitly not verified paper coefficients. [docs/registry/module-routing.yaml:L354-L359] [src/digitalmodel/production_chemistry/scale_si.py:L28-L48]

## 2. Inputs and outputs

| Input | Unit | Implemented range or condition | Default | Source |
| --- | --- | --- | --- | --- |
| `na`, `k`, `mg`, `ca`, `sr`, `ba`, `fe`, `cl`, `so4`, `hco3` | mg/L | Negative values rejected; finite-value validation absent | Each 0.0 | [src/digitalmodel/production_chemistry/scale_si.py:L121-L160] |
| `hco3` interpretation | mg/L as bicarbonate | Bicarbonate alkalinity expressed as HCO3 | 0.0 | [src/digitalmodel/production_chemistry/scale_si.py:L123-L135] |
| `tds` | mg/L | Negative values rejected; supplied TDS does not replace individual ions in ionic strength | `None`, then sum of listed ions | [src/digitalmodel/production_chemistry/scale_si.py:L142-L173] |
| `ph` | 1 | Optional measured pH; no range or finiteness check | `None` | [src/digitalmodel/production_chemistry/scale_si.py:L143-L155] [src/digitalmodel/production_chemistry/scale_si.py:L373-L389] |
| `co2_mole_fraction_gas` | 1 | Range 0 to 1; positive value selects gas route before pH | `None` | [src/digitalmodel/production_chemistry/scale_si.py:L144-L155] [src/digitalmodel/production_chemistry/scale_si.py:L353-L384] |
| `t_f` | degF | No temperature range check | Required except `mixing_sweep`: 77.0 | [src/digitalmodel/production_chemistry/scale_si.py:L392-L409] [src/digitalmodel/production_chemistry/scale_si.py:L518-L524] |
| `p_psia` | psia, absolute | Values at or below 0 rejected | Required except `mixing_sweep`: 14.7 | [src/digitalmodel/production_chemistry/scale_si.py:L392-L409] [src/digitalmodel/production_chemistry/scale_si.py:L518-L524] |
| `coefficients` | Terms consistent with degF, psia and mol/L | Supplied keys overlay defaults | `DEFAULT_COEFFICIENTS` | [src/digitalmodel/production_chemistry/scale_si.py:L186-L217] [src/digitalmodel/production_chemistry/scale_si.py:L404-L408] |
| `tp_profile` | Ordered pairs: degF, psia | At least one pair; order retained | Required | [src/digitalmodel/production_chemistry/scale_si.py:L443-L471] |
| `fraction_b` / `fractions` | 1, volume fraction | Each value from 0 to 1; sweep must be nonempty | Required | [src/digitalmodel/production_chemistry/scale_si.py:L479-L497] [src/digitalmodel/production_chemistry/scale_si.py:L518-L543] |

| Output | Unit | Structure and convention | Source |
| --- | --- | --- | --- |
| `molarity(ion)` | mol/L | mg/L divided by molecular weight and 1000 | [src/digitalmodel/production_chemistry/scale_si.py:L157-L160] |
| `ionic_strength` | mol/L | One half of the charge-squared weighted concentration sum | [src/digitalmodel/production_chemistry/scale_si.py:L169-L178] |
| `saturation_indices` | SI, logarithms and constants: 1 | Dictionary of seven `ScaleIndexResult` objects: family, SI, log-product, conditional constant, basis and details | [src/digitalmodel/production_chemistry/scale_si.py:L295-L304] [src/digitalmodel/production_chemistry/scale_si.py:L392-L435] |
| Calcite details | Fugacity: psia; pH: 1; ionic strength: mol/L | Gas route exposes fugacity and estimated pH; measured route exposes `ph_used` | [src/digitalmodel/production_chemistry/scale_si.py:L349-L384] |
| `si_profile` | Step: 1; temperature: degF; pressure: psia; SI and dSI: 1 | DataFrame: `step`, `t_f`, `p_psia`, seven SI columns, seven dSI columns; dSI relative to first row | [src/digitalmodel/production_chemistry/scale_si.py:L448-L471] |
| `mix_brines` | Composition: mg/L; pH and gas fraction: 1 | A new `BrineComposition` | [src/digitalmodel/production_chemistry/scale_si.py:L479-L515] |
| `mixing_sweep` | Fraction and SI: 1 | DataFrame: `fraction_b` and seven SI columns | [src/digitalmodel/production_chemistry/scale_si.py:L518-L543] |

The APIs operate on compositions and scalar states rather than coordinates; profile order is supplied by the caller, conventionally bottomhole to wellhead. [src/digitalmodel/production_chemistry/scale_si.py:L121-L144] [src/digitalmodel/production_chemistry/scale_si.py:L443-L456]

## 3. Method and governing standards

| Method | Implementing definition | Basis and limitations |
| --- | --- | --- |
| Concentration conversion and ionic strength | `BrineComposition.molarity`, `BrineComposition.ionic_strength`; module `ionic_strength` delegates | `c = concentration / molecular_weight / 1000`; `I = 0.5 sum(c z²)`; molarity substitutes for molality under the documented dilute-solution assumption. [src/digitalmodel/production_chemistry/scale_si.py:L50-L54] [src/digitalmodel/production_chemistry/scale_si.py:L157-L178] |
| Conditional polynomial | `CorrelationCoefficients.value` | `a0 + a_t T + a_t2 T² + a_t3 T³ + a_p P + a_sqrt_i sqrt(I) + a_i I`; negative ionic strength rejected. [src/digitalmodel/production_chemistry/scale_si.py:L186-L217] |
| Sulfates and halite | `_sulfate_type_result`; selected by `saturation_indices` | `SI = log10(c_cation c_anion) + pK_cond`; halite uses sodium/chloride, sulfates use sulfate. [src/digitalmodel/production_chemistry/scale_si.py:L311-L339] [src/digitalmodel/production_chemistry/scale_si.py:L414-L435] |
| Calcite with gas CO2 | `_calcite_result` | `fCO2 = gas_fraction P`; `SI = log10(c_Ca c_HCO3² / fCO2) + K`; estimated pH is also returned; fugacity coefficient fixed at 1.0. [src/digitalmodel/production_chemistry/scale_si.py:L349-L371] |
| Calcite with measured pH | `_calcite_result` | `SI = log10(c_Ca c_HCO3) + pH - K`; missing both routes raises `ValueError`. [src/digitalmodel/production_chemistry/scale_si.py:L373-L389] |
| Profile | `si_profile` | Repeated SI evaluation at fixed composition; `dSI = SI - SI_first`. [src/digitalmodel/production_chemistry/scale_si.py:L458-L471] |
| Ideal mixing | `mix_brines` | Ions and TDS mix linearly; hydrogen concentration mixes before conversion back to pH; incomplete pH or CO2 information is dropped at interior fractions. [src/digitalmodel/production_chemistry/scale_si.py:L479-L515] |
| Mixing sweep | `mixing_sweep` | Delegates chemistry to `mix_brines` and `saturation_indices`, then assembles a DataFrame. [src/digitalmodel/production_chemistry/scale_si.py:L533-L543] |

The code names Oddo, J. E. and Tomson, M. B., *Why Scale Forms in the Oil Field and Methods To Predict It*, SPE Production & Facilities 9(1), 1994, SPE-21710-PA; the implemented coefficient values are explicitly constructed defaults rather than transcriptions from that paper. [src/digitalmodel/production_chemistry/scale_si.py:L8-L48]

`ScaleIndexResult` carries a textual `basis` and a numeric details mapping rather than a structured `Citation` sidecar; the basis warns that calibration is required. [src/digitalmodel/production_chemistry/scale_si.py:L284-L304] [src/digitalmodel/production_chemistry/scale_si.py:L327-L338]

UNVERIFIED: Equation-level agreement with the published paper, coefficient calibration, handbook provenance and the validity envelope have not been established; quantitative acceptance requires independent reference evidence, issue pending (1).

## 4. Validated worked examples

Execution record: BASE and AFTER runs are NOT_RUN because creation of the mandatory scratch directory failed with `Read-only file system`; the prescribed environment, test logs and temporary outputs could not be created, issue pending (4).

There are 18 statically identified test definitions in the module test file; the historical page's test-pass count is not execution evidence for this revision. [tests/production_chemistry/test_scale_si.py:L84-L301] [docs/api/ffs/build-wave-2026-07.html:L122-L126]

Validated examples: 0; the following closed-form reference case remains a validation candidate until its offline run is recorded.

| Case | Independent expectation and input source | Tolerance | Test node ID | BASE / AFTER |
| --- | --- | --- | --- | --- |
| Ionic-strength closed form | Four-ion composition; `0.5 (0.1 + 0.1 + 4 × 0.01 + 4 × 0.01) = 0.14 mol/L`; Na 0.1 mol/L and Ca 0.01 mol/L. [tests/production_chemistry/test_scale_si.py:L84-L95] | Relative 0.001, as asserted by the test; arithmetic check only, not SI calibration. [tests/production_chemistry/test_scale_si.py:L93-L95] | `tests/production_chemistry/test_scale_si.py::test_ionic_strength_hand_check` | NOT_RUN / NOT_RUN, scratch prerequisite denied |

Regression examples: 17; expected values below are contracts, algebraic identities or qualitative anchors rather than independently calibrated mineral SI results. [tests/production_chemistry/test_scale_si.py:L4-L13] [tests/production_chemistry/test_scale_si.py:L98-L301]

| Case | Expected result, units and tolerance; fixture source | Test node ID | BASE / AFTER |
| --- | --- | --- | --- |
| Negative concentration | `na = -1 mg/L` raises 1 `ValueError`. [tests/production_chemistry/test_scale_si.py:L98-L101] | `tests/production_chemistry/test_scale_si.py::test_negative_concentration_rejected` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Families and basis | Synthetic formation-water fixture; 7 finite SI values, matching names and warning text. [tests/production_chemistry/test_scale_si.py:L38-L56] [tests/production_chemistry/test_scale_si.py:L109-L117] | `tests/production_chemistry/test_scale_si.py::test_all_families_returned_with_basis` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Missing carbonate route | Both pH and CO2 absent: 1 `ValueError` mentioning calcite. [tests/production_chemistry/test_scale_si.py:L120-L123] | `tests/production_chemistry/test_scale_si.py::test_calcite_requires_carbonate_route` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Gas-route preference | Fixture supplies both routes; gas route selected and `3 < estimated_ph < 9`, unit 1. [tests/production_chemistry/test_scale_si.py:L126-L129] | `tests/production_chemistry/test_scale_si.py::test_calcite_gas_route_preferred_and_reports_estimated_ph` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Coefficient offset | Barite SI change 1.0, unit 1; absolute tolerance `1e-12` for unit intercept increment. [tests/production_chemistry/test_scale_si.py:L132-L144] | `tests/production_chemistry/test_scale_si.py::test_coefficient_override_shifts_si_by_a0_delta` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Calcite heating | Both routes: SI at 250 degF exceeds SI at 100 degF at 3000 psia; strict inequality, SI unit 1. [tests/production_chemistry/test_scale_si.py:L152-L163] | `tests/production_chemistry/test_scale_si.py::test_calcite_si_increases_with_temperature` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Calcite depressurization | SI at 500 psia exceeds SI at 6000 psia at 180 degF; strict inequality, SI unit 1. [tests/production_chemistry/test_scale_si.py:L166-L173] | `tests/production_chemistry/test_scale_si.py::test_pressure_drop_increases_calcite_si` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Barite heating | SI at 200 degF is below SI at 80 degF at 2000 psia; strict inequality, SI unit 1. [tests/production_chemistry/test_scale_si.py:L176-L182] | `tests/production_chemistry/test_scale_si.py::test_barite_si_decreases_with_temperature_at_low_t` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Sulfate crossover | At 1000 psia anhydrite SI is below gypsum at 70 degF and above at 250 degF; strict inequalities, SI unit 1. [tests/production_chemistry/test_scale_si.py:L185-L193] | `tests/production_chemistry/test_scale_si.py::test_anhydrite_overtakes_gypsum_at_high_temperature` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Halite trend | Synthetic dilute-fixture SI below -0.5; concentrated-fixture SI increases and absolute SI below 0.4, unit 1. [tests/production_chemistry/test_scale_si.py:L196-L207] | `tests/production_chemistry/test_scale_si.py::test_halite_only_near_saturation_tds` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Profile contract | 10 input states retained in order; 7 SI/dSI families; first dSI 0 within `1e-12`, unit 1. [tests/production_chemistry/test_scale_si.py:L215-L232] | `tests/production_chemistry/test_scale_si.py::test_si_profile_shape_order_and_dsi` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Isothermal pressure path | Five pressure states at 180 degF; every successive calcite SI increment above 0, unit 1. [tests/production_chemistry/test_scale_si.py:L235-L241] | `tests/production_chemistry/test_scale_si.py::test_si_profile_pure_depressurization_is_monotonic_for_calcite` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Empty profile | Empty state sequence raises 1 `ValueError` mentioning `tp_profile`. [tests/production_chemistry/test_scale_si.py:L244-L246] | `tests/production_chemistry/test_scale_si.py::test_si_profile_empty_rejected` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Mixing endpoints | Fractions 0 and 1 match pure-brine SI in 7 families, absolute tolerance `1e-9`, unit 1; same calculation supplies expectations. [tests/production_chemistry/test_scale_si.py:L254-L268] | `tests/production_chemistry/test_scale_si.py::test_mixing_endpoints_equal_pure_brines` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Interior barite peak | 21 fractions; maximum SI exceeds both endpoints and its fraction is strictly between 0 and 1, unit 1. [tests/production_chemistry/test_scale_si.py:L271-L283] | `tests/production_chemistry/test_scale_si.py::test_seawater_mixing_drives_barite_si_up` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Linear mixing | Fraction 0.25; barium/sulfate in mg/L and hydrogen-based pH match weighted expectations at relative tolerance `1e-12`; incomplete gas fraction becomes `None`. [tests/production_chemistry/test_scale_si.py:L286-L296] | `tests/production_chemistry/test_scale_si.py::test_mix_brines_linear_concentrations_and_h_ion_mixing` | NOT_RUN / NOT_RUN, scratch prerequisite denied |
| Invalid fraction | Fraction 1.5, unit 1, raises 1 `ValueError` mentioning `fraction_b`. [tests/production_chemistry/test_scale_si.py:L299-L301] | `tests/production_chemistry/test_scale_si.py::test_mixing_fraction_out_of_range_rejected` | NOT_RUN / NOT_RUN, scratch prerequisite denied |

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, and section 9 for the QA appendix.

| Gate | Applicability | Module-specific route | Current execution status |
| --- | --- | --- | --- |
| G0 | Applicable: inputs, units, citations | EC-G0-1 to EC-G0-3; section 2 unit review and section 3 source limitation | NOT_RUN; issue pending (4) |
| G1 | Applicable: imports and tests | EC-G1-1; Python pytest route for `tests/production_chemistry/`, `tests/contracts/` and `tests/docs/test_digitalmodel_routing_contract.py` | NOT_RUN; issue pending (4) |
| G2 | Applicable: hand or published check | EC-G2-1 to EC-G2-3; arithmetic checks do not qualify the default SI coefficients | NOT_RUN; issue pending (1), issue pending (4) |
| G3 | NOT_APPLICABLE | Scalar equilibrium-tendency calculations and tabular sweeps contain no dynamic time integration. [src/digitalmodel/production_chemistry/scale_si.py:L392-L543] | NOT_APPLICABLE |
| G4 | NOT_APPLICABLE | Composition/state inputs have no diffraction body or mesh. [src/digitalmodel/production_chemistry/scale_si.py:L392-L397] [src/digitalmodel/production_chemistry/scale_si.py:L443-L447] | NOT_APPLICABLE |
| G5 | Applicable: traceable evidence appendix | EC-G5-1 and EC-G5-2 | NOT_RUN; issue pending (4) |

| Check ID | Executable check | Numeric pass criterion and unit | Evidence |
| --- | --- | --- | --- |
| EC-G0-1 | Section-4 negative-concentration node: `BrineComposition(na=-1)` | 1 `ValueError` for -1 mg/L | Pytest node result. [tests/production_chemistry/test_scale_si.py:L98-L101] |
| EC-G0-2 | Section-4 invalid-fraction node: `mix_brines(..., 1.5)` | 1 `ValueError` for fraction 1.5, unit 1 | Pytest node result. [tests/production_chemistry/test_scale_si.py:L299-L301] |
| EC-G0-3 | Section-4 missing-carbonate-route node: `saturation_indices` | 1 `ValueError` with both route inputs absent, exception count | Pytest node result. [tests/production_chemistry/test_scale_si.py:L120-L123] |
| EC-G1-1 | Run every section-4 node through Python pytest, retaining import/collection output | 18 module nodes passed; 0 failed and 0 skipped, test counts | Full pytest log and environment versions; node definitions. [tests/production_chemistry/test_scale_si.py:L84-L301] |
| EC-G2-1 | `ionic_strength` against the four-ion closed form | 0.14 mol/L at relative tolerance 0.001 | Hand arithmetic and pytest result. [tests/production_chemistry/test_scale_si.py:L84-L95] |
| EC-G2-2 | `saturation_indices` barite response to coefficient-intercept increment | SI change 1.0 ± `1e-12`, unit 1 | Algebraic perturbation and pytest result. [tests/production_chemistry/test_scale_si.py:L132-L144] |
| EC-G2-3 | `mix_brines` at fraction 0.25 against weighted ion and hydrogen concentrations | Relative error at most `1e-12` for Ba/SO4 (mg/L) and pH (1); gas fraction absent | Expected arithmetic and pytest result. [tests/production_chemistry/test_scale_si.py:L286-L296] |
| EC-G5-1 | Inspect all results from the section-4 families-and-basis node | 7 of 7 results carry the coefficient warning; 0 missing warnings, result count | Basis strings and pytest result. [tests/production_chemistry/test_scale_si.py:L109-L117] |
| EC-G5-2 | Manual evidence audit before calculation use: retain input values/units, code pin, coefficient mapping, each node result and gate disposition | 18 of 18 node results and 6 of 6 gate dispositions; 0 uncited coefficient sets, record count | Signed appendix required by AQG r0, sections 9 and 11; incomplete evidence remains issue pending (4) |

## 6. Known failure modes and pitfalls

sweep: 38 found

The sweep inventory below includes documented defaults and assumptions as well as missing guards; an inventory row does not assert that a documented default is an unexpected defect. [src/digitalmodel/production_chemistry/scale_si.py:L28-L66] [src/digitalmodel/production_chemistry/scale_si.py:L132-L173] [src/digitalmodel/production_chemistry/scale_si.py:L198-L217]

Exact package sweep commands, with each match reviewed in its enclosing definition:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|mg/L|mol/L|molality|psia|degF|cache|default|else' src/digitalmodel/production_chemistry --glob '*.py'
rg -n 'except|continue|return|0\.0|1970|0\.05|int\(|float\(|unit|activity|fugacity|precipitation' src/digitalmodel/production_chemistry --glob '*.py'
```

| ID | Symptom or assumption; source at code pin | Existing detection and disposition |
| --- | --- | --- |
| SF01 | Omitted sodium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L132-L132] | No detection check exists for omitted-versus-measured-zero input; issue pending (2) |
| SF02 | Omitted potassium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L133-L133] | No detection check exists; issue pending (2) |
| SF03 | Omitted magnesium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L134-L134] | No detection check exists; issue pending (2) |
| SF04 | Omitted calcium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L135-L135] | No detection check exists; issue pending (2) |
| SF05 | Omitted strontium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L136-L136] | No detection check exists; issue pending (2) |
| SF06 | Omitted barium becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L137-L137] | No detection check exists; issue pending (2) |
| SF07 | Omitted iron becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L138-L138] | No detection check exists; issue pending (2) |
| SF08 | Omitted chloride becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L139-L139] | No detection check exists; issue pending (2) |
| SF09 | Omitted sulfate becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L140-L140] | No detection check exists; issue pending (2) |
| SF10 | Omitted bicarbonate becomes 0 mg/L. [src/digitalmodel/production_chemistry/scale_si.py:L141-L141] | No detection check exists; issue pending (2) |
| SF11 | Unspecified `a0` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L198-L198] | No detection check exists for unintentionally omitted coefficients; issue pending (2) |
| SF12 | Unspecified `a_t` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L199-L199] | No detection check exists; issue pending (2) |
| SF13 | Unspecified `a_t2` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L200-L200] | No detection check exists; issue pending (2) |
| SF14 | Unspecified `a_t3` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L201-L201] | No detection check exists; issue pending (2) |
| SF15 | Unspecified `a_p` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L202-L202] | No detection check exists; issue pending (2) |
| SF16 | Unspecified `a_sqrt_i` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L203-L203] | No detection check exists; issue pending (2) |
| SF17 | Unspecified `a_i` becomes 0.0. [src/digitalmodel/production_chemistry/scale_si.py:L204-L204] | No detection check exists; issue pending (2) |
| SF18 | Missing TDS is replaced by the sum of ten listed ions; a supplied TDS is returned without ion-sum reconciliation. [src/digitalmodel/production_chemistry/scale_si.py:L142-L142] [src/digitalmodel/production_chemistry/scale_si.py:L162-L173] | No detection check exists; issue pending (2) |
| SF19 | pH defaults to `None`; absent carbonate routes raise rather than produce partial-family results. [src/digitalmodel/production_chemistry/scale_si.py:L143-L143] [src/digitalmodel/production_chemistry/scale_si.py:L386-L389] | EC-G0-3 checks both routes absent; no detection check exists for partial-family availability; issue pending (2) |
| SF20 | Gas fraction defaults to `None`; measured pH is then used if present. [src/digitalmodel/production_chemistry/scale_si.py:L144-L144] [src/digitalmodel/production_chemistry/scale_si.py:L353-L389] | No detection check exists for missing-gas provenance; issue pending (2) |
| SF21 | Built-in coefficients are constructed defaults, not verified paper values. [src/digitalmodel/production_chemistry/scale_si.py:L220-L272] | EC-G5-1 detects loss of the warning; no detection check exists for quantitative calibration; issue pending (1) |
| SF22 | Constructed `ScaleIndexResult` can have empty `details`. [src/digitalmodel/production_chemistry/scale_si.py:L295-L304] | No detection check exists for direct result construction; issue pending (2) |
| SF23 | Nonpositive ion product becomes NaN without an exception. [src/digitalmodel/production_chemistry/scale_si.py:L307-L308] | EC-G5-1 checks finite outputs only for the complete fixture; no detection check exists for absent-ion cases; issue pending (2) |
| SF24 | mol/L substitutes for molality under a dilute-solution assumption. [src/digitalmodel/production_chemistry/scale_si.py:L52-L54] | No detection check exists for validity at high salinity; issue pending (1) |
| SF25 | Ideal volumetric mixing omits precipitation and re-equilibration. [src/digitalmodel/production_chemistry/scale_si.py:L55-L58] [src/digitalmodel/production_chemistry/scale_si.py:L479-L488] | EC-G2-3 checks mixing arithmetic only; no detection check exists for physical equilibrium error; issue pending (1) |
| SF26 | Fugacity coefficient fixed at 1.0; gas partial pressure substitutes for fugacity. [src/digitalmodel/production_chemistry/scale_si.py:L353-L369] | No detection check exists for non-ideal-gas error; issue pending (1) |
| SF27 | Gas fraction 0 selects the measured-pH route or raises when pH is absent. [src/digitalmodel/production_chemistry/scale_si.py:L353-L389] | No detection check exists for the zero-gas boundary; issue pending (2) |
| SF28 | `coefficients=None` or an empty mapping retains all defaults. [src/digitalmodel/production_chemistry/scale_si.py:L396-L408] | No detection check exists for intended complete calibration; issue pending (1) |
| SF29 | Partial coefficient mappings silently retain remaining defaults; unused keys are not validated. [src/digitalmodel/production_chemistry/scale_si.py:L406-L408] | EC-G2-2 checks one valid override, not completeness or unknown keys; issue pending (2) |
| SF30 | Ion validation checks negativity, pressure checks nonpositivity, and polynomial ionic strength checks negativity; no finiteness checks or temperature/pH bounds are applied. [src/digitalmodel/production_chemistry/scale_si.py:L146-L155] [src/digitalmodel/production_chemistry/scale_si.py:L206-L217] [src/digitalmodel/production_chemistry/scale_si.py:L392-L405] | EC-G0-1 covers one negative ion only; no detection check exists for nonfinite inputs or extrapolation; issue pending (2) |
| SF31 | Interior mixing drops pH if only one endpoint defines it. [src/digitalmodel/production_chemistry/scale_si.py:L499-L505] | No detection check exists; issue pending (2) |
| SF32 | Interior mixing drops gas fraction if only one endpoint defines it. [src/digitalmodel/production_chemistry/scale_si.py:L507-L513] | EC-G2-3 checks this loss for its fixture; no detection check exists for loss of both carbonate routes; issue pending (2) |
| SF33 | Omitted sweep temperature defaults to 77.0 degF. [src/digitalmodel/production_chemistry/scale_si.py:L518-L524] | No detection check exists for an unintentionally omitted state; issue pending (2) |
| SF34 | Omitted sweep pressure defaults to 14.7 psia. [src/digitalmodel/production_chemistry/scale_si.py:L518-L524] | No detection check exists for an unintentionally omitted state; issue pending (2) |
| SF35 | Sweep fraction is coerced with `float(f)` before mixing; no locale parsing is supplied. [src/digitalmodel/production_chemistry/scale_si.py:L533-L536] | No detection check exists for text with thousands or decimal separators; issue pending (2) |
| SF36 | Output fraction is coerced again with `float(f)` when the row is built. [src/digitalmodel/production_chemistry/scale_si.py:L537-L537] | No detection check exists for repeat-conversion differences; issue pending (2) |
| SF37 | Water activity and hydration are absorbed into conditional constants. [src/digitalmodel/production_chemistry/scale_si.py:L62-L63] | No detection check exists for independent activity error; issue pending (1) |
| SF38 | Public input units are degF, psia, mg/L and mol/L; no unit-tagged input conversion is performed. [src/digitalmodel/production_chemistry/scale_si.py:L65-L66] [src/digitalmodel/production_chemistry/scale_si.py:L157-L173] [src/digitalmodel/production_chemistry/scale_si.py:L392-L409] | EC-G2-1 checks mg/L conversion for its fixture only; no detection check exists for mislabeled units; issue pending (2) |

Explicit rejection paths include negative ions/TDS, gas fractions outside the allowed range, negative polynomial ionic strength, nonpositive pressure, missing carbonate routes, empty profiles, invalid mixing fractions and empty mixing sweeps. [src/digitalmodel/production_chemistry/scale_si.py:L146-L155] [src/digitalmodel/production_chemistry/scale_si.py:L206-L208] [src/digitalmodel/production_chemistry/scale_si.py:L386-L405] [src/digitalmodel/production_chemistry/scale_si.py:L458-L460] [src/digitalmodel/production_chemistry/scale_si.py:L490-L491] [src/digitalmodel/production_chemistry/scale_si.py:L541-L542]

## 7. Lessons log

| Date | Change reference | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-23 | Initial pack, code pin in header | A coefficient warning and qualitative trends do not establish quantitative SI calibration. [src/digitalmodel/production_chemistry/scale_si.py:L28-L48] [tests/production_chemistry/test_scale_si.py:L4-L13] | Sections 3 to 6 distinguish arithmetic references, regressions and calibration gaps | Not created; issue pending (4) |

## 8. Ownership

Review role: engineering calculations quality manager; deputy role: engineering calculations deputy quality manager; producing group: calculation maintainers; technical questions and disputed implementation classifications go to the family quality manager. Independent clearance is required under AQG r0, section 11.

Pack status remains DRAFT pending completed acceptance evidence and second-provider clearance, issue pending (4).

## 9. Dependencies and implementation inventory

The implementation imports Python `math`, dataclasses and typing plus pandas; its package initializer re-exports the calculation API. [src/digitalmodel/production_chemistry/scale_si.py:L69-L89] [src/digitalmodel/production_chemistry/__init__.py:L20-L41]

The repository declares pandas as a dependency; the explorer generator calls `si_profile`, `mixing_sweep` and `saturation_indices`, then serializes their returned data, so it is a downstream consumer rather than an independent chemistry implementation. [pyproject.toml:L54-L54] [scripts/production_chemistry/build_scale_si_explorer.py:L56-L95]

Implementation-map evidence is pinned separately to `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; its embedded searched-code revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L6]

| A8 inventory item | Result |
| --- | --- |
| Package boundary | `src/digitalmodel/production_chemistry/`; both Python files included; no subpackage exclusions. [src/digitalmodel/production_chemistry/__init__.py:L1-L41] [src/digitalmodel/production_chemistry/scale_si.py:L1-L543] |
| Map families with implementation paths inside package | Empty inventory, 0 rows, after parsing every family's implementation paths at the separate map pin. [docs/registry/implementation-map.yaml:L1-L53690] |
| Counted families | 0; ranking and tie breaking yield an empty set because the inventory is empty. [docs/registry/implementation-map.yaml:L1-L53690] |
| Counted-family grep | NOT_APPLICABLE: no map family has an implementation row inside the package. [docs/registry/implementation-map.yaml:L1-L53690] |
| Map follow-up | Add ionic-strength calculation, conditional-solubility polynomial/SI, calcite carbonate routes, fixed-composition profiling and ideal brine mixing from section 3; issue pending (3). [src/digitalmodel/production_chemistry/scale_si.py:L157-L217] [src/digitalmodel/production_chemistry/scale_si.py:L307-L543] |

Supplementary repository search, including scripts, used the following command; this vocabulary search does not extend the counted A8 families.

```sh
rg -n 'scale_si|saturation_indices|ionic_strength|mix_brines|mixing_sweep|Oddo|Tomson' src scripts --glob '*.py'
rg -l 'scale_si|saturation_indices|ionic_strength|mix_brines|mixing_sweep|Oddo|Tomson' . --glob '*.py'
```

Within the package, the module-level `ionic_strength` delegates to the class method; `_sulfate_type_result`, `_calcite_result`, `CorrelationCoefficients.value` and `BrineComposition.ionic_strength` compute their respective results, all inventoried in section 3. [src/digitalmodel/production_chemistry/scale_si.py:L169-L217] [src/digitalmodel/production_chemistry/scale_si.py:L311-L389]

## 10. Coverage and open items

| Evidence read | Coverage and use |
| --- | --- |
| `src/digitalmodel/production_chemistry/__init__.py` | Package purpose and exports. [src/digitalmodel/production_chemistry/__init__.py:L1-L41] |
| `src/digitalmodel/production_chemistry/scale_si.py` | Whole implementation: assumptions, all classes/functions, defaults, guards and sweep inventory. [src/digitalmodel/production_chemistry/scale_si.py:L1-L543] |
| `tests/production_chemistry/test_scale_si.py` | All 18 static test definitions and both fixture constructors; node IDs and NOT_RUN dispositions appear in section 4. [tests/production_chemistry/test_scale_si.py:L38-L301] |
| `tests/production_chemistry/__init__.py` | Test package marker. [tests/production_chemistry/__init__.py:L1-L1] |
| `docs/registry/module-routing.yaml` | Existing domain, test route and development maturity. [docs/registry/module-routing.yaml:L354-L359] |
| `docs/maps/digitalmodel-operator-map.md` | Module discovery row. [docs/maps/digitalmodel-operator-map.md:L59-L59] |
| `docs/capability-map/capabilities-ia-spec-1444.md` | Production-chemistry capability placement. [docs/capability-map/capabilities-ia-spec-1444.md:L41-L48] |
| `docs/api/production/scale-si-explorer.html` | Synthetic demonstration and coefficient warning; displayed outputs are not independent references. [docs/api/production/scale-si-explorer.html:L46-L90] |
| `docs/api/ffs/build-wave-2026-07.html` | Historical workflow and test-count claim; not a current test result. [docs/api/ffs/build-wave-2026-07.html:L122-L126] |
| `scripts/production_chemistry/build_scale_si_explorer.py` | Downstream data assembly delegates chemistry to this package. [scripts/production_chemistry/build_scale_si_explorer.py:L56-L95] |

| Open item | Required evidence or action | Tracking |
| --- | --- | --- |
| UNVERIFIED: published numerical basis | Open the primary paper and the handbook sources, identify equation locators and coefficient provenance, qualify an independently sourced SI reference, and define temperature/pressure/salinity limits; no quantitative calibration is claimed | Issue pending (1) |
| UNVERIFIED: seawater source attribution | The fixture docstring names Millero et al. 2008, but the underlying publication and composition have not been checked; its mixing results remain regressions. [tests/production_chemistry/test_scale_si.py:L59-L76] | Issue pending (1) |
| Input and fallback coverage | Review SF01 to SF38 and add missing completeness, finiteness, unit and boundary checks without confusing documented assumptions with unexpected defects | Issue pending (2) |
| Missing map families | Add the section-3 method families to the implementation map before they become counted A8 families | Issue pending (3) |
| Offline execution and acceptance evidence | Run all 18 module tests at BASE and AFTER, run the routing contract tests, retain citation-resolution and lint scripts/results, and finish the acceptance checks; current test results are NOT_RUN because mandatory scratch creation failed | Issue pending (4) |
| Independent clearance and index | Obtain independent gate clearance, replace draft tracking references with issue numbers, and record the cleared pack index and shared lesson entry | Issue pending (4) |
