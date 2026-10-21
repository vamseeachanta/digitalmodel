# VIV analysis knowledge pack

| Header | Value |
|---|---|
| Module | subsea/viv_analysis |
| Repository | digitalmodel |
| Package | src/digitalmodel/subsea/viv_analysis |
| Pack revision | 1, 2026-10-18; [audit correction](https://github.com/vamseeachanta/digitalmodel/issues/2752) |
| Code revision | Historical inventory: 97dc074bad7e464d4b26ccd04bb83796ef4d0060; corrected calculations and gates: revision 1, symbols below |
| Analysis family | Engineering calculations (EC); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager; deputy: engineering calculations deputy quality manager |
| Status | DRAFT; independent review pending |

## 1. Purpose and scope

The public API computes tubular-member beam or tension-string frequencies, added mass, Strouhal shedding, reduced-velocity screening and amplitude-based fatigue; the engine instead routes configurations through the separate legacy tubular implementation. [src/digitalmodel/subsea/viv_analysis/__init__.py:L11-L48] [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L21-L37]

The API models a uniform tubular section and a scalar current; profile analysis samples depth along the member under a vertical or near-vertical assumption, rather than solving a coupled structural response. [src/digitalmodel/subsea/viv_analysis/models.py:L56-L92] [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L194] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L50-L106]

The routing registry marks the containing subsea domain stable; this is domain metadata, while the package README's production-readiness and coverage assertions are README claims, unverified. [docs/registry/module-routing.yaml:L410-L425] [src/digitalmodel/subsea/viv_analysis/README.md:L132-L145]

UNVERIFIED: U1 — package-specific maturity, geometry/load applicability bounds and production qualification are not established by the README assertions; issue pending (6). [src/digitalmodel/subsea/viv_analysis/README.md:L132-L145]

## 2. Inputs and outputs

Inputs to the newer API use the following declarations; “unrestricted” means the dataclass supplies no range validation, not an engineering permission. [src/digitalmodel/subsea/viv_analysis/models.py:L39-L150]

| Input | Unit / convention | Range enforced; default | Source |
|---|---|---|---|
| Material modulus, density, Poisson ratio | MPa declaration; modulus above the implementation threshold is treated as Pa; kg/m³; dimensionless | Unrestricted; Poisson ratio defaults in code | [src/digitalmodel/subsea/viv_analysis/models.py:L39-L52] |
| Length, outer diameter, thickness | m | Unrestricted at construction; no geometry validation | [src/digitalmodel/subsea/viv_analysis/models.py:L55-L92] |
| Boundary, effective length factor, top tension | enum; dimensionless; N | Pinned-pinned; inferred factor unless explicit; tension optional | [src/digitalmodel/subsea/viv_analysis/models.py:L13-L20] [src/digitalmodel/subsea/viv_analysis/models.py:L55-L72] |
| Fluid density, viscosity, added-mass coefficient | kg/m³; m²/s; dimensionless | Built-in fluid defaults; unrestricted | [src/digitalmodel/subsea/viv_analysis/models.py:L95-L101] |
| Profile depth, velocity | m positive downward from surface; m/s | Uniform, power-law, linear or custom; reference depth and gradient default to zero | [src/digitalmodel/subsea/viv_analysis/models.py:L104-L150] |
| Mode / number of modes | count | Individual mode 1–6; list methods do not prevalidate count | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L59-L92] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L220-L246] |
| Shedding diameter, velocity, Strouhal input | m; m/s; dimensionless | Finite diameter and Strouhal positive; finite velocity nonnegative; zero flow retained | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L38-L63] |
| Screening natural frequency, diameter, velocity | Hz; m; m/s | Frequency and diameter finite and positive; velocity finite and nonnegative | [src/digitalmodel/subsea/viv_analysis/screening.py:L51-L76] |
| Amplitude, stress range, fatigue frequency, duration | Ambiguous m or A/D at stress function; MPa; Hz; s (wrapper accepts h) | Amplitude interpreted by magnitude; defaults and curve selection in code | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L50-L106] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L108-L165] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L209-L247] |
| Legacy tubular configuration | Span ft; section geometry in; E psi; weight lb/in; external density lb/in³; current in/s by default, explicit m/s supported; nominal OD used hydrodynamically | Required dictionary keys; plotting enabled unless disabled | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L64-L96] [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L271-L312] |
| Legacy modal/current/wave files | Modal header integer counts, whitespace columns; spreadsheet data routed through external reader | Required column/key names; no schema validation in these readers | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L9-L109] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L234-L243] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L453-L462] |

Outputs are dataclasses for frequency (Hz, s, rad/s, m and mass ratio), shedding (Hz and Reynolds number), screening (dimensionless velocity, margin/status and factor), and fatigue (MPa, Hz, s, cycles, damage and years). [src/digitalmodel/subsea/viv_analysis/models.py:L153-L212]

| Output route | Format and units | Evidence |
|---|---|---|
| API | Dataclasses above; profile methods return lists | [src/digitalmodel/subsea/viv_analysis/models.py:L153-L212] [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L194] |
| CLI | Console text and optional JSON; frequency and period rounded, screening values cast and rounded | [src/digitalmodel/subsea/viv_analysis/cli.py:L60-L92] [src/digitalmodel/subsea/viv_analysis/cli.py:L121-L150] |
| Engine tubular path | Dataframe JSON strings in configuration; three CSVs and optional plots; span column restored to ft, frequency Hz | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L36-L69] [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L226-L269] |
| Modal reader | Angular frequency divided by twice pi; x derived from node index, model length and start | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L43-L82] |

UNVERIFIED: U2 — the legacy modal-file length unit, absolute origin, vector frame and phase convention are not declared by the reader; sidecar entry: to be created only if private reference data are needed; issue pending (3). [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L43-L82]

## 3. Method and governing standards

The following is an implementation description, not a standards-compliance attestation; coefficients and tabulated standard values are intentionally referenced at their code locations rather than reproduced. [src/digitalmodel/subsea/viv_analysis/models.py:L267-L289] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L27-L44]

| Method / definition | Implemented operation and reference boundary | Evidence |
|---|---|---|
| TubularMember properties | Annular area, second moment and mass from outer/inner diameters and density | [src/digitalmodel/subsea/viv_analysis/models.py:L74-L92] |
| MaterialProperties.youngs_modulus_pa | Magnitude-based modulus conversion | [src/digitalmodel/subsea/viv_analysis/models.py:L47-L52] |
| CurrentProfile.velocity_at_depth | Uniform value, power law, linear gradient or interpolation; no force calculation | [src/digitalmodel/subsea/viv_analysis/models.py:L121-L150] |
| FrequencyCalculator._frequency_beam_vibration | Euler–Bernoulli frequency from eigenvalue, effective length, EI and structural plus displaced-fluid mass; eigenvalues attributed to Blevins, edition unspecified in code | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L29-L47] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L94-L162] |
| FrequencyCalculator._frequency_tension_controlled | Uniform-tension string frequency, mode divided by twice length times square root of tension/mass | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L164-L218] |
| FrequencyCalculator.calculate_effective_tension / check_stability | Top tension less submerged weight times depth; bottom tension compared with requested minimum | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L248-L316] |
| VortexSheddingAnalyzer.shedding_frequency / reynolds_number / strouhal_from_reynolds | Strouhal velocity/diameter relationship, Reynolds ratio, surface-key lookup; Reynolds number warns at low values but does not vary the lookup | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L38-L113] |
| VortexSheddingAnalyzer profile statistics | Sample along length, maximum or RMS shedding frequency | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L236] |
| VIVScreening.reduced_velocity / check_lock_in / calculate_safety_factor | Velocity divided by frequency and diameter, inclusive band and marginal status, piecewise ratio factor | [src/digitalmodel/subsea/viv_analysis/screening.py:L51-L158] |
| VIVScreening.screen_with_current_profile / critical_current_velocity | Sampled maximum or RMS current delegated into screening; target reduced velocity multiplied by frequency and diameter | [src/digitalmodel/subsea/viv_analysis/screening.py:L278-L339] |
| VIVFatigueCalculator.calculate_stress_range | First-mode curvature, outer-fibre bending range and Pa-to-MPa conversion; amplitude units guessed | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L50-L106] |
| VIVFatigueCalculator.calculate_damage / calculate_fatigue_life | Logarithmic S-N evaluation, frequency-duration cycle count and Miner ratio; life to target damage converted from seconds to years | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L108-L207] |
| VIVFatigueCalculator.analyze_viv_fatigue / cumulative_damage | Wrapper applies identical curve-specific thickness correction to damage and life; sum of zipped condition lists | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L209-L300] |
| VIVTubularMembers.get_simply_supported_modes / get_clamped_modes / get_free_free_modes | Separate imperial beam-frequency implementations; free-free loop repeats its first-mode expression for every mode | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L134-L269] |
| VIVTubularMembers.get_vs_frequencies / get_safety_factors | Cross-flow St U/D, in-line 2 St U/D, Reynolds ratio and natural/excitation ratios; added mass included; KC null for steady current | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L271-L364] |
| VIVAnalysisComponents.modal_analysis | Header parsing, modal frequency conversion, optional strict node-window selection and per-mode maxima | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L9-L109] |
| VIVAnalysisComponents.plot_current_non_exceedance / plot_current_exceedance / plot_wave_exceedance | Sort probabilities and form cumulative columns; names/settings are interchanged for current plots | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L245-L401] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L501-L560] |

| Standard named by code or documentation | Edition and locator recorded by repository; limits |
|---|---|
| DNV-RP-C205, Environmental Conditions and Environmental Loads | The corrected legacy mass and excitation formulas use October 2010 Sections 9.1.3, 9.1.11, 9.3.3.2, 9.7.1.3 and Appendix D. Historical summary documentation names 2019; equivalence is not established. |
| DNV-RP-F105, Free Spanning Pipelines | Documentation names 2006 and method topics without a numbered locator; code names the standard without edition; clause text not checked against the licensed copy. [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L639-L642] [src/digitalmodel/subsea/viv_analysis/screening.py:L26-L32] |
| DNV-RP-C203, Fatigue Design of Offshore Steel Structures | The VIV adapter now uses October 2011 Table 2-1 and Eqs. (2.4.1), (2.4.3), with canonical values from `digitalmodel.fatigue.c203_sn_tables`; historical summary documentation names 2016 and is not its governing basis. |
| API RP 2A, Planning, Designing and Constructing Fixed Offshore Platforms | Documentation names 22nd edition and Vortex shedding; enum and screening docstring name the code, while calculations do not branch on design_code; clause text not checked against the licensed copy. [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L652-L654] [src/digitalmodel/subsea/viv_analysis/models.py:L31-L36] [src/digitalmodel/subsea/viv_analysis/screening.py:L34-L49] [src/digitalmodel/subsea/viv_analysis/screening.py:L160-L244] |
| ISO 13819-2, Petroleum and natural gas industries — Offshore structures — Part 2: Fixed steel structures | Documentation names 1995 and VIV requirements without clause locator; enum lists the code without an implemented code-specific branch; clause text not checked against the licensed copy. [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L656-L657] [src/digitalmodel/subsea/viv_analysis/models.py:L31-L36] [src/digitalmodel/subsea/viv_analysis/screening.py:L160-L244] |

UNVERIFIED: U3 — correspondence of these formulas, coefficients and criteria to the named licensed editions, and any conflicting editions, remains unresolved; code omissions do not establish an edition conflict; issue pending (6). [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L634-L657] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L27-L44]

The fatigue adapter now consumes the existing `digitalmodel.fatigue.c203_sn_tables` verified-value source, **DNV-RP-C203 October 2011 Table 2-1**, Section 2.4, Eqs. (2.4.1), (2.4.3), and Miner damage Eq. (2.2.1). Stress is in MPa without reference-stress normalization; the second slope applies beyond the 1e7-cycle knee. This is an **in-air plated welded-detail** model; the tubular-joint T curve and seawater environments are outside this adapter. Rounded tabulated intercepts allow a small knee discontinuity, bounded by 0.2% in the regression. The m2 branch is continued for damage without a constant-amplitude endurance cutoff. [src/digitalmodel/subsea/viv_analysis/sn_parameters.py] [src/digitalmodel/subsea/viv_analysis/fatigue.py]

`VIVFatigueCalculator.citations` contains the curve-table `Citation`; `VIVFatigueResult.citations` emits its serialized sidecar. Every endurance calculation validates the **2011-10** resolver target `wikis/engineering-standards/wiki/standards/dnv-rp-c203-2011.md`. Its frontmatter shall match `code_id: DNV-RP-C203`, `publisher: DNV`, `revision: 2011-10`. Missing or mismatched metadata raises `CitationResolutionError`; the existing 2021 resolver page is not substituted. Offline tests provision metadata only, without a standards copy. Deployment of that metadata in the generic wiki is a maintainer prerequisite outside this repository change. U4's reference-stress defect is corrected; later-edition equivalence remains unestablished.

The legacy route uses **DNV-RP-C205 October 2010**, Section 9.1.3 (shedding), Section 9.3.3.2 (lift/drag excitation frequencies), Section 9.1.11 and Appendix D (effective/added mass), and Section 9.7.1.3 (KC). Nominal OD remains the hydrodynamic diameter; coatings contribute structural weight. Current defaults to in/s, preserving the registered fixture interpretation; `viv.current_speed_unit: m/s` explicitly converts before evaluation. No wave period is supplied, so KC is null rather than inferred from shedding. These narrower choices do not qualify coated-cylinder hydrodynamics. [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py]

Publisher references: [C203](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c203-fatigue-design-of-offshore-steel-structures/), [C205](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c205-environmental-conditions-and-environmental-loads/). Edition-specific locators above govern this change; the publisher's current editions are not asserted equivalent.

## 4. Validated worked examples

The following independent arithmetic checks exercise package outputs. Their scope is the stated fixture and criterion; a signed engineering qualification is not established. The existing geometry, shedding, reduced-velocity and cumulative-damage identities remain supplementary regression checks.

Let `A = tests/subsea/viv_analysis/test_audit_regressions.py`, `L = tests/subsea/viv_analysis/test_legacy_audit.py`, and `U = tests/subsea/viv_analysis/test_viv_analysis_unit.py`.

| Case | Package output and independent expected value | Executable test |
|---|---|---|
| R1 pinned beam | `FrequencyCalculator.calculate_natural_frequency`: L=50 m, OD=0.5 m, t=0.025 m, E=207 GPa, steel density 7850 kg/m³, fluid density 1025 kg/m³, Ca=1. Annular I=0.001055092 m⁴, effective mass=494.113620 kg/m; Euler–Bernoulli f=π/(2L²)√(EI/m)=0.41773 Hz ±1e-4 Hz | `A::test_independent_pinned_frequency` |
| R2 D curve in air | `VIVFatigueCalculator.calculate_allowable_cycles(100)`: N=1.46e6 cycles ±1%; D at 0.5 Hz for 86400 s=0.029613091382755; life=0.09245406876793 years, relative tolerance 1e-9 | `A::test_d_air_endurance_and_damage` |
| R3 knee and long life | `calculate_allowable_cycles`: knee=1e7 cycles ±0.2% near 52.642115454 MPa; N(30 MPa)=166109215.213006 cycles; `calculate_fatigue_life(30,0.3)`=17.54561132796 years, relative tolerance 1e-9 | `A::test_d_air_knee_and_second_slope` |
| R4 thickness | `calculate_damage`: 50/25 mm damage ratio=2^(m1*k) ±1e-9; D curve k=.20, C=.15, F=.25, B1=0; `analyze_viv_fatigue`: damage × life equals exposure years | `A::test_thickness_ratio`, `A::test_thick_member_life_matches_damage` |
| R5 legacy route | `VIVAnalysis.router` and saved CSV/JSON: L=720 in, OD=12.75 in, E=30e6 psi, I=361.5439319 in⁴, structural weight 5.589872732 lb/in, added weight 4.750834711 lb/in. Wet f=1.9290322268 Hz, cross-flow=.56470588235 Hz, in-line=1.1294117647 Hz; ratios 3.41599456830 and 1.70799728415; absolute tolerance 1e-9 | `L::test_legacy_wet_frequencies_and_excitation`; `tests/subsea/viv_analysis/test_viv_analysis.py::test_run_process`; durable workflow below |

Historical C04 asserted **2.623697 Hz, 0.564706 Hz, 4.646130**, using `pytest.approx` defaults (relative 1e-6, absolute 1e-12). Those air-mass/in-line baselines are superseded by R5. The issue's approximate 1.944 Hz API comparison omits the coating weight retained in R5; it is not used as the legacy reference.

Current offline verification: the package suite exercises actual package calculations, not a mocked engine. The PR records final pass counts; counts alone are not numerical gates. Durable node: `tests/workflows/test_durable_workflows.py::test_workflow_registry[viv-analysis]`. No commercial solver result is required or claimed.

## 5. QA gates

G0 applies to input checks, G1 to executable offline routes, G2 to independent numerical checks, and G5 to citation/output traceability. G3/G4 are not applicable: these routes perform no time-domain or diffraction solve. Every gate below binds an asserted package function or output to a criterion.

| ID | Executable check and asserted package result | Gate |
|---|---|---|
| C01 | `U::TestMaterialProperties::test_get_material_invalid_name` asserts `get_material` raises ValueError for an unknown material | G0 |
| C02 | `tests/subsea/viv_analysis/test_viv_analysis_cli.py::TestNaturalFrequencyCommand::test_natural_freq_invalid_material` asserts `cli.natural_frequency` returns nonzero exit and an error for an invalid material | G0 |
| C03 | `U::TestVIVScreening::test_recommendation_lock_in` asserts `VIVScreening.screen_member` returns lock-in, susceptible=True and REQUIRED at 1.5 m/s; `test_recommendation_safe` asserts safe at 0.5 m/s | G1 |
| C04 | `tests/subsea/viv_analysis/test_viv_analysis.py::test_run_process` and durable node R5 assert `VIVAnalysis.router` CSV/JSON frequency=1.9290322268 Hz, in-line=1.1294117647 Hz and in-line ratio=1.70799728415, absolute tolerance 1e-9 | G1 |
| C05 | `A::test_independent_pinned_frequency` asserts `FrequencyCalculator.calculate_natural_frequency`=0.41773 Hz ±1e-4 Hz for R1 | G2 |
| C06 | `A::test_d_air_endurance_and_damage` and `test_d_air_knee_and_second_slope` assert `VIVFatigueCalculator.calculate_allowable_cycles`=1.46e6 ±1% at 100 MPa and the 1e7-cycle knee ±0.2% | G2 |
| C07 | `A::test_d_air_endurance_and_damage` asserts `VIVFatigueCalculator.calculate_damage`=0.029613091382755 and `calculate_fatigue_life`=0.09245406876793 years for R2, relative tolerance 1e-9 | G2 |
| C08 | `A::test_thickness_ratio` asserts `VIVFatigueCalculator.calculate_damage` 50/25 mm ratio=2^(m1*k) ±1e-9 for four classes; `test_thick_member_life_matches_damage` asserts `analyze_viv_fatigue` damage/life reciprocity, relative tolerance 1e-12 | G2 |
| C09 | `A::test_curve_citation_sidecar` asserts `VIVFatigueResult.citations` serializes the Citation with revision 2011-10, Table 2-1 and generic source; `test_missing_citation_fails_closed` asserts `calculate_damage` raises CitationResolutionError for a missing target | G5 |
| C10 | `A::test_nan_velocity_fails_without_status` and `test_tension_screening_fails_without_status` assert `VIVScreening.screen_member` raises ValueError for NaN current and negative/zero/nonfinite tension | G0/G5 |

A4 — identity checks: the original C05–C08 were geometry, shedding, reduced-velocity and summation identities, not independent frequency or fatigue validation. They remain supplementary regressions and cannot establish the absolute numerical criteria now covered by R1–R5. The former assertion-free mocked-engine test contributes no historical engineering evidence.

UNVERIFIED: U5 — a signed calculation qualification appendix remains absent; the bounded arithmetic checks above do not establish qualification for arbitrary geometry, environment or boundary conditions.

## 6. Known failure modes and pitfalls

sweep: 44 recorded (42 historical sites plus two confirmed audit defects)

The count is source-audit sites requiring an explicit interpretation, including defaults and unit assumptions; it is not a count of independently reproduced defects. All rows were examined at the header pin; source-audit date: 2026-10-17.

The remaining line-range locators in the historical inventory below refer to the original code pin. Corrected rows use current symbols. Exact package sweeps (text only; no solver imports):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|assum|default|interp|zip\(' src/digitalmodel/subsea/viv_analysis --glob '*.py'
rg -n 'except|continue|return|0\.0|1970|0\.05|int\(|float\(|\.get\(|linspace|density|viscosity|amplitude|thickness|reference_depth' src/digitalmodel/subsea/viv_analysis --glob '*.py'
```

| Site | Symptom / cause | Detection coverage; follow-up | Pin evidence |
|---|---|---|---|
| S01 | Modulus unit guessed by magnitude; an incorrectly scaled input can be accepted | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L47-L52] |
| S02 | Material Poisson default and unconstrained geometry permit omitted or nonphysical inputs | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L40-L72] |
| S03 | Implicit boundary effective-length factor differs from the factor applied by beam calculator unless explicitly supplied | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L108-L116] |
| S04 | Default fluid properties and added mass silently apply when no fluid is supplied | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L95-L101] |
| S05 | Reference depth defaults to zero and is clamped for power law; nonpositive depth returns surface current | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L109-L138] |
| S06 | Power exponent and zero linear gradient supply default current shapes without input validation | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L111-L141] |
| S07 | Custom profile interpolation clamps outside endpoints and does not validate ordering, equal lengths or units | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/models.py:L143-L147] |
| S08 | Empty or negative requested mode count yields empty frequency list without an error | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L220-L246] |
| S09 | Stability check defaults minimum tension to zero and treats non-tension members as stable without a stability calculation | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L288-L316] |
| S10 | Unknown surface condition silently falls back to a base Strouhal constant | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L85-L113] |
| S11 | Reynolds dependence is described but only a low-Re warning and unchanged surface lookup are implemented | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L97-L113] |
| S12 | Profile sample count zero returns an empty result; vertical geometry is assumed | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L194] |
| S13 | Finite, nonnegative reduced velocity is required; no-flow zero retains zero factor | Rejection covered by `test_screening_helpers_reject_invalid_reduced_velocity`; no-flow by `test_zero_flow_remains_valid` | `VIVScreening.calculate_safety_factor` |
| S14 | Frequency-ratio detail substitutes zero when frequency is nonpositive | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/screening.py:L233-L243] |
| S15 | Fixed screening defaults are used even when another design_code enum is selected | no detection check exists; issue pending (6) | [src/digitalmodel/subsea/viv_analysis/screening.py:L34-L49] [src/digitalmodel/subsea/viv_analysis/screening.py:L160-L244] |
| S16 | Empty requested mode list is returned by screening without validating count | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/screening.py:L246-L276] |
| S17 | Profile screening uses a fixed sample count and maximum or RMS; a localized current peak may be missed | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/viv_analysis/screening.py:L298-L308] |
| S18 | Amplitude is interpreted as A/D or metres according to its magnitude | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L73-L80] |
| S19 | Any non-crown location is treated as springline with the same fibre radius | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L87-L91] |
| S20 | Corrected: thickness multiplies stress by max(1,t/25)^k with the Table 2-1 class exponent; missing/nonpositive/nonfinite requested thickness raises ValueError | `test_thickness_ratio`, `test_thickness_requested_without_value_fails` | `VIVFatigueCalculator._corrected_stress` |
| S21 | Zero stress gives infinite endurance; negative/nonfinite stress raises ValueError | `calculate_allowable_cycles` input guard; zero is a true no-loading input | `VIVFatigueCalculator.calculate_allowable_cycles` |
| S22 | Removed: no nonpositive-endurance fallback silently returns zero damage | Endurance evaluated directly, C06/C07 | `VIVFatigueCalculator.calculate_damage` |
| S23 | Corrected: zero stress gives infinite life; nonpositive/nonfinite frequency raises ValueError | Shared endurance and frequency validation | `VIVFatigueCalculator.calculate_fatigue_life` |
| S24 | Cumulative damage starts at zero; zip truncates unequal lists and empty input returns zero | `test_cumulative_damage` covers equal-length sums only; no detection check exists for truncation; issue pending (2) | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L289-L300] |
| S25 | Hours-to-seconds, thickness-to-mm and fixed year duration are assumed by the wrappers | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/viv_analysis/fatigue.py:L204-L207] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L239-L247] |
| S26 | Corrected: damage and life use the same corrected stress; reported stress remains the physical uncorrected range | `test_thick_member_life_matches_damage` | `VIVFatigueCalculator.analyze_viv_fatigue` |
| S27 | Header int conversions require whitespace counts without schema or separator normalization | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L19-L22] |
| S28 | Modal frequency and coordinate unit assumptions, strict node-window endpoints, and signed maxima can hide peak magnitudes | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L43-L109] |
| S29 | Current non-exceedance plot skips missing probabilities with a print; no structured failure result | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L262-L314] |
| S30 | Current exceedance plot skips missing probabilities with a print; no structured failure result | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L339-L383] |
| S31 | Wave exceedance plot skips missing probabilities with a print; no structured failure result | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L514-L552] |
| S32 | Current non-exceedance uses surface depth zero regardless of reference_depth; incomplete profiles only print a warning | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L245-L286] |
| S33 | Current exceedance uses surface depth zero regardless of reference_depth; incomplete profiles only print a warning | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L324-L363] |
| S34 | Zero-initialized current exceedance accumulation assumes nonempty, normalized probabilities and reuses opposite plot settings | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L250-L301] |
| S35 | Zero-initialized current non-exceedance accumulation assumes nonempty, normalized probabilities and reuses opposite plot settings | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L330-L378] |
| S36 | Zero-initialized wave probability accumulation assumes nonempty, normalized probabilities | no detection check exists; issue pending (3) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L521-L535] |
| S37 | Legacy plotting replaces actual depth array with fixed endpoint depths, omitting other depths from plots | no detection check exists; issue pending (4) | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L460-L466] |
| S38 | Corrected: declared in/s or m/s; cross-flow St U/D, in-line 2 St U/D; KC null without imposed wave period | `test_legacy_wet_frequencies_and_excitation`, `test_legacy_speed_units` | `VIVTubularMembers.get_vs_frequencies` |
| S39 | Corrected: imperial beam frequencies include added mass with Ca=1 by default; nominal hydrodynamic diameter and coating structural weight retained | `test_legacy_wet_frequencies_and_excitation`, `test_legacy_added_mass_coefficient` | `VIVTubularMembers.get_natural_frequencies` |
| S40 | Missing plot switch defaults to enabled, requiring visualization outputs and dependencies | no detection check exists; issue pending (4) | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L64-L69] |
| S41 | Free-free mode loop ignores mode number, repeating the same frequency | no detection check exists; issue pending (4) | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L145-L177] |
| S42 | No configuration variations returns an empty list, with no count validation or explicit no-work status | no detection check exists; issue pending (5) | [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L30-L49] |
| S43 | Corrected: reference-stress normalization inflated endurance by about 6.4e8; in-air dimensional S-N branches now use the canonical table | Absolute N, damage, life and knee checks C06/C07 | `VIVFatigueCalculator.calculate_allowable_cycles` |
| S44 | Corrected: NaN current and invalid tension could reach a `safe` status; finite input checks now raise ValueError before status selection | C10 and parameterized helper/Strouhal/frequency regressions | `VIVScreening.screen_member`, `FrequencyCalculator.calculate_natural_frequency`, `VortexSheddingAnalyzer.shedding_frequency` |

The two broad CLI exception handlers emit the error and exit with status 1, and the material lookup converts KeyError to ValueError; they do not swallow errors. JSON float conversions operate on computed results, not external text. No `continue`, `return None`, error-path empty-dictionary return, epoch default or empty-result cache write was found in the inspected Python definitions. [src/digitalmodel/subsea/viv_analysis/cli.py:L90-L92] [src/digitalmodel/subsea/viv_analysis/cli.py:L125-L150] [src/digitalmodel/subsea/viv_analysis/models.py:L248-L264]

| Additional confirmed source limitation | Detection / incident scope | Evidence |
|---|---|---|
| Legacy fallback calls a function as a no-argument constructor and then expects an analyze method | no detection check exists; issue pending (5) | [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L27-L29] [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L52-L59] |
| Former mocked engine test is replaced by a real offline tubular-router calculation with CSV/JSON readback | C04 asserts absolute frequency and excitation ratios | [tests/subsea/viv_analysis/test_viv_analysis.py:L13-L47] |
| Specialized CLI suite skips at collection because its legacy helper import is absent | no detection check exists in section 5; collection skip is recorded in section 4; issue pending (7) | [tests/specialized/cli/test_viv_analysis_cli.py:L11-L16] |
| Implementation-summary profile example uses an unsupported constructor keyword and a field absent from VortexSheddingResult; fatigue example divides hour-based durations again before the hour-accepting wrapper | no detection check exists; issue pending (6) | [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L295-L310] [src/digitalmodel/subsea/viv_analysis/models.py:L104-L118] [src/digitalmodel/subsea/viv_analysis/models.py:L168-L175] [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L328-L339] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L209-L247] |
| Fatigue curve provenance now emits a validated 2011-10 sidecar; other pre-existing constants remain outside that citation coverage | C09 asserts fatigue runtime provenance only; broader provenance remains issue pending (6) | [src/digitalmodel/subsea/viv_analysis/models.py:L267-L289] [src/digitalmodel/subsea/viv_analysis/fatigue.py:L27-L44] |

Issue #1837 describes a broader VIV/fatigue failure chain and #1642 a separate result-parser port; neither establishes an incident in these specific calculation definitions, so no unconfirmed incident is attributed to them. The relevant source boundaries are the unvalidated fatigue path and delegated legacy result processing. [src/digitalmodel/subsea/viv_analysis/fatigue.py:L108-L165] [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L66-L71]

The implementation summary gives a different safety-factor expression from the piecewise calculation; this is documentation drift, issue pending (6), and the summary expression is excluded as a reference. [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L145-L154] [src/digitalmodel/subsea/viv_analysis/screening.py:L127-L158]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-17 | Initial source audit | Treat the API and engine tubular workflow as separate numerical implementations; passing one does not qualify the other | [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L21-L37] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L94-L162] | Sections 3–6 distinguish regression checks from qualification | Not assigned; issue pending (6) |

## 8. Maintenance responsibility

Proposed review responsibility: engineering calculations quality manager; deputy responsibility: engineering calculations deputy quality manager; producing discipline: subsea engineering calculations. Method classification and tolerance questions go to the family quality manager; cross-family changes go to the maintainers.

UNVERIFIED: U6 — a named, maintained assignment or deputy roster is not present in the module's maintenance statement; issue pending (6). [src/digitalmodel/subsea/viv_analysis/README.md:L155-L157]

## 9. Dependencies and implementation inventory

| Dependency / interface | Relationship and limit | Evidence |
|---|---|---|
| NumPy and dataclasses | Calculation arrays, interpolation and result schemas | [src/digitalmodel/subsea/viv_analysis/models.py:L7-L10] [src/digitalmodel/subsea/viv_analysis/models.py:L143-L147] |
| Click and JSON | CLI argument conversion and output serialization | [src/digitalmodel/subsea/viv_analysis/cli.py:L33-L92] [src/digitalmodel/subsea/viv_analysis/cli.py:L96-L150] |
| PipeProperties | Engine populates cross-section and system properties before tubular analysis | [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L21-L37] |
| pandas and assetutilities visualization | Legacy tabular results and plots | [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L36-L69] [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L366-L408] |
| Legacy common readers / visualizations | Modal, spreadsheet and visualization paths import older common modules inside functions | [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L117-L122] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L234-L243] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L453-L462] |
| SHEAR7 reference 1 | Configuration flag selects delegated fatigue processing, read as text only; solver version not specified | [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L66-L66] |
| SHEAR7 reference 2 | Calls shear7_viv_fatigue_analysis on an external legacy fatigue component; read as text only; no solver or interface executed for this pack | [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L67-L71] |
| Durable workflow | Engine input and CSV outputs registered with offline runtime | [docs/registry/workflows.yaml:L227-L236] |

Implementation-map source: branch `codex/implmap-work`, ref commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; its map bytes equal the map at the code pin, so the following citations resolve at the code pin as well. [docs/registry/implementation-map.yaml:L1-L4]

Inventory scope is the header package and all its Python files; no sub-package was excluded. Three map families have in-package rows: effective-tension-riser-stack-up (4), sn-curves-and-miner-damage (4), and wind-and-current-force-routes (3); the tied families retain map order. Each inventory below lists every row of that family, including rows outside the package. The family-local ordinal plus exact map-row locator identifies each entry; outside-package identities are retained by locator to avoid repeating identifying discovery names. [docs/registry/implementation-map.yaml:L4433-L4472] [docs/registry/implementation-map.yaml:L16419-L16446] [docs/registry/implementation-map.yaml:L19908-L19948]

### effective-tension-riser-stack-up — 264 rows, 4 in package

| Map row | Definition identity / scope | Relation | Evidence |
|---|---|---|---|
| 1 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` / `FrequencyCalculator.calculate_natural_frequency` | caller | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` / `FrequencyCalculator._frequency_tension_controlled` | independent | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` / `FrequencyCalculator.calculate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` / `FrequencyCalculator.check_stability` | caller | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L4908-L4917] |

### sn-curves-and-miner-damage — 392 rows, 4 in package

| Map row | Definition identity / scope | Relation | Evidence |
|---|---|---|---|
| 1 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | Outside package; identity at cited map row | wrapper | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` / `VIVFatigueCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` / `VIVFatigueCalculator.calculate_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` / `VIVFatigueCalculator.analyze_viv_fatigue` | independent | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` / `VIVFatigueCalculator.cumulative_damage` | caller | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L20939-L20948] |

### wind-and-current-force-routes — 135 rows, 3 in package

| Map row | Definition identity / scope | Relation | Evidence |
|---|---|---|---|
| 1 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | Outside package; identity at cited map row | example | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | Outside package; identity at cited map row | copy | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16157-L16168] |
| 76 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16169-L16178] |
| 77 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | Outside package; identity at cited map row | caller | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | `src/digitalmodel/subsea/viv_analysis/screening.py` / `VIVScreening.screen_with_current_profile` | independent | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` / `viv_analysis` | independent | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` / `VortexSheddingAnalyzer.analyze_with_profile` | independent | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | Outside package; identity at cited map row | test_helper | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | Outside package; identity at cited map row | independent | [docs/registry/implementation-map.yaml:L16782-L16791] |

### Counted-family grep and classifications

At the code pin each family's search_signals from that map revision was applied exactly, using case-sensitive Python `re.finditer` over every package `.py` file; hits were assigned to the smallest enclosing AST function/class, with remaining statements grouped as module-level. A signal would be omitted if it matched `subsea/viv_analysis` case-insensitively; none of these three families' signals matched that path. The algorithm is reproducible as follows; the data revision is the one above. [docs/registry/implementation-map.yaml:L1-L4]

```python
families = [f for f in map_data['families'] if any(
    r['path'].startswith(package + '/') for r in f['implementations'])]
families = sorted(families, key=lambda f: -sum(
    r['path'].startswith(package + '/') for r in f['implementations']))[:3]
for family in families:
    signals = [s['pattern'] for s in family['search_signals']
               if not re.search(s['pattern'], 'subsea/viv_analysis', re.I)]
    for path in pinned_python_files:
        for signal in signals:
            for match in re.finditer(signal, pinned_source[path]):
                assign_to_smallest_enclosing_definition(path, match)
```

| Counted family | Definitions already inventoried | Remaining hit classifications | Further second implementations |
|---|---|---|---|
| effective-tension-riser-stack-up | calculate_natural_frequency, _frequency_tension_controlled, calculate_effective_tension, check_stability | TubularMember.top_tension is data; no formula in that class for the family's tension result | 0; [src/digitalmodel/subsea/viv_analysis/models.py:L55-L72] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L59-L92] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L164-L218] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L248-L316] |
| sn-curves-and-miner-damage | calculate_damage, calculate_fatigue_life, analyze_viv_fatigue, cumulative_damage | VIVFatigueCalculator class holds the curve data; VIVFatigueResult and DesignCode hold data; screening class and module hits are docstrings | 0; [src/digitalmodel/subsea/viv_analysis/fatigue.py:L19-L44] [src/digitalmodel/subsea/viv_analysis/models.py:L31-L36] [src/digitalmodel/subsea/viv_analysis/models.py:L204-L212] [src/digitalmodel/subsea/viv_analysis/screening.py:L26-L32] |
| wind-and-current-force-routes | screen_with_current_profile, viv_analysis, analyze_with_profile | analyze_member delegates shedding; screen_member and screen_multiple_modes delegate; reduced_velocity and critical_current_velocity calculate other section-3 quantities; current plot functions serialize or accumulate probabilities rather than compute forces; result classes hold data; module hits are docstrings | 0; [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L115-L158] [src/digitalmodel/subsea/viv_analysis/screening.py:L51-L76] [src/digitalmodel/subsea/viv_analysis/screening.py:L160-L276] [src/digitalmodel/subsea/viv_analysis/screening.py:L310-L339] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L245-L451] [src/digitalmodel/subsea/viv_analysis/models.py:L168-L186] |

A8 inventory/grep result: 791 rows inventoried, 11 in-package rows, 3 counted families, 0 further unlisted in-package second implementations; the producer's classification passes the at-most-one threshold, subject to independent family review. [docs/registry/implementation-map.yaml:L4433-L4472] [docs/registry/implementation-map.yaml:L16419-L16446] [docs/registry/implementation-map.yaml:L19908-L19948]

### Repository-wide follow-up

The whole-repository search included `src/`, `tests/` and `scripts/`, using `rg -n 'class (FrequencyCalculator|VortexSheddingAnalyzer|VIVFatigueCalculator|VIVTubularMembers|VIVAnalysisComponents)|def (shedding_frequency|reduced_velocity|calculate_damage|viv_analysis)' src scripts tests`; the infrastructure VIV modal reader is another implementation outside this package, and other fatigue routes are map follow-up rather than extra in-package misses. [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L4-L25] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L70-L105] [src/digitalmodel/solvers/openfoam/validation/cylinder.py:L81-L106]

Map follow-up: add explicit annular-geometry, beam/string-frequency, Strouhal and reduced-velocity screening, modal-result processing, and cumulative-probability families before counting those methods in a later map revision; the duplicate legacy methods are already described in section 3, issue pending (8). [src/digitalmodel/subsea/viv_analysis/models.py:L74-L92] [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L134-L364] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L9-L109] [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L245-L401]

## 10. Coverage and open items

Every Python file and the README under the package was read; there are no nested source sub-packages in this inventory. The paths below enumerate the source coverage. [src/digitalmodel/subsea/viv_analysis/__init__.py:L11-L48] [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L1-L37]

| File read | Coverage |
|---|---|
| `src/digitalmodel/subsea/viv_analysis/__init__.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/__init__.py:L1-L88] |
| `src/digitalmodel/subsea/viv_analysis/cli.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/cli.py:L1-L159] |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/fatigue.py:L1-L300] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L1-L316] |
| `src/digitalmodel/subsea/viv_analysis/models.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/models.py:L1-L289] |
| `src/digitalmodel/subsea/viv_analysis/screening.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/screening.py:L1-L339] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/viv_analysis.py:L1-L37] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/viv_analysis_components.py:L1-L560] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L1-L103] |
| `src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py:L1-L515] |
| `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | Whole file; [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L1-L236] |
| `src/digitalmodel/subsea/viv_analysis/README.md` | Whole file; claims reconciled against code/tests; [src/digitalmodel/subsea/viv_analysis/README.md:L1-L157] |

Test coverage: package-local unit, CLI and mocked engine tests and fixtures; specialized CLI suite inspected as text despite its collection skip; durable workflow node inspected and executed in scratch. Other workflow-family tests that exercise separate VIV packages are not claimed as this package's tests. [tests/subsea/viv_analysis/conftest.py:L1-L41] [tests/subsea/viv_analysis/test_viv_analysis_unit.py:L1-L615] [tests/subsea/viv_analysis/test_viv_analysis_cli.py:L1-L371] [tests/subsea/viv_analysis/test_viv_analysis.py:L1-L49] [tests/specialized/cli/test_viv_analysis_cli.py:L1-L419] [tests/workflows/test_durable_workflows.py:L460-L469]

Documentation discovery used `rg -n -i 'viv_analysis|viv-analysis|viv analysis' docs --glob '*.md' --glob '*.yaml' --glob '*.toml'`; matching contexts were inspected, with the core VIV documents and workflow registry read for this pack; broader document reconciliation remains issue pending (8). The inventory below lists neutral discovery paths, without endorsing their historical counts or maturity statements. [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L26-L40] [docs/registry/workflows.yaml:L227-L236]

| Documentation discovered | Locator of module mention |
|---|---|
| `docs/capability-map/capability-report-2026-02-14.md` | [docs/capability-map/capability-report-2026-02-14.md:L88-L88] |
| `docs/domains/development/pre_development_testing_checklist.md` | [docs/domains/development/pre_development_testing_checklist.md:L29-L29] |
| `docs/domains/development/test_summary_with_validations.md` | [docs/domains/development/test_summary_with_validations.md:L72-L72] |
| `docs/domains/infrastructure/SKILL_VERSIONING.md` | [docs/domains/infrastructure/SKILL_VERSIONING.md:L236-L236] |
| `docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md` | [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L228-L228] |
| `docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md` | [docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md:L176-L176] |
| `docs/domains/migration/signal_analysis_migration_plan.md` | [docs/domains/migration/signal_analysis_migration_plan.md:L32-L32] |
| `docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md:L396-L396] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L258-L258] |
| `docs/domains/orcaflex/reference/line-types-drag-lift.md` | [docs/domains/orcaflex/reference/line-types-drag-lift.md:L32-L32] |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L39-L39] |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md:L66-L66] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L77-L77] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L4-L4] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L16-L16] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | [docs/domains/project-docs/MODULE_SURVEY.md:L62-L62] |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L96-L96] |
| `docs/domains/subsea/viv_analysis/README.md` | [docs/domains/subsea/viv_analysis/README.md:L1-L1] |
| `docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md` | [docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md:L36-L36] |
| `docs/domains/testing/CLI_TESTING_PHASE2_SESSION_SUMMARY.md` | [docs/domains/testing/CLI_TESTING_PHASE2_SESSION_SUMMARY.md:L34-L34] |
| `docs/domains/testing/engine_test_implementation_plan.md` | [docs/domains/testing/engine_test_implementation_plan.md:L382-L382] |
| `docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L1-L1] |
| `docs/field-development/subsea-production-systems-mapping.md` | [docs/field-development/subsea-production-systems-mapping.md:L82-L82] |
| `docs/migration/aceengineercode-migration.md` | [docs/migration/aceengineercode-migration.md:L102-L102] |
| `docs/migration/legacy-batch-workflows.md` | [docs/migration/legacy-batch-workflows.md:L69-L69] |
| `docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md` | [docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md:L67-L67] |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | [docs/plans/parametric-refresh/adding-an-atlas.md:L75-L75] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L18-L18] |
| `docs/registry/implementation-map.yaml` | [docs/registry/implementation-map.yaml:L4433-L4433] |
| `docs/registry/workflows.yaml` | [docs/registry/workflows.yaml:L227-L227] |

1 discovery file with identifying names is omitted by name; codenaming and the remaining historical/non-text documentation review are tracked by issue pending (8).

| Open item / responsibility | Evidence or boundary | Tracking |
|---|---|---|
| U1 package qualification; maintainer | README readiness assertions are not current engineering qualification | issue pending (6) |
| U2 legacy frames and file units; maintainer | Explicit modal-data contract and any required private sidecar entry remain to be created | issue pending (3); #1642 is related parser work |
| U3 standards editions, clause correspondence and output citations; maintainer | Licensed clause text not checked; documentation expression drift also needs correction | issue pending (6) |
| U4 normalization defect corrected; maintainer | Independent Table 2-1 endurance, damage and life checks now apply; wider method qualification remains outside scope | issue pending (2); related #1837 |
| U5 signed evidence record; maintainer | Bounded independent arithmetic examples R1–R5 now apply; signed qualification remains absent | issue pending (6) |
| U6 maintained quality-role assignment; maintainer | Proposed role responsibilities in section 8 require assignment | issue pending (6) |
| Input, default and profile sites S01–S17; maintainer | Numeric range, no-flow and sampling detection gaps | issue pending (1); S15 provenance belongs to issue pending (6) |
| Fatigue sites S18–S26; maintainer | Unit guessing and list truncation remain; S20–S23 and S26 are corrected as recorded above | issue pending (2) |
| Legacy modal/probability sites S27–S36; maintainer | Record omission, schemas and cumulative-probability semantics | issue pending (3) |
| Legacy tubular sites S37–S41; maintainer | S38/S39 frequency and units corrected; plotting depth and free-free mode repetition remain outside this audit scope | issue pending (4) |
| Legacy dispatch and S42; maintainer | Function/object mismatch and no-work status | issue pending (5) |
| Specialized CLI collection; maintainer | Mocked engine replaced by C04 numeric router assertions; specialized CLI helper remains outside scope | issue pending (7) |
| Map families, repository analogues and broader documentation/issue-history review; maintainer | Section 9 follow-up, discovery codenaming, remaining binary documents and historical issue bodies | issue pending (8) |

UNVERIFIED register: U1–U6 above, each with a draft reference; no additional engineering qualification is implied by the passing offline regressions. [tests/subsea/viv_analysis/test_viv_analysis_unit.py:L502-L519] [tests/workflows/test_durable_workflows.py:L460-L469]
