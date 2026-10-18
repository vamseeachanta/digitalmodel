# Fatigue module knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | `fatigue` / `digitalmodel` |
| Package / scope | `src/digitalmodel/fatigue`; unique package interfaces and methods; shared method families reference the structural fatigue pack (#2552) |
| Pack revision / date | 2 / 2026-10-15 |
| Code revision | `c5a1d23fb37f5c0748983fe7aea43ba3640b63e1` |
| Analysis family / routing code | Engineering calculations / `EC` |
| Family quality manager | Engineering calculations quality manager |
| Deputy | Engineering calculations deputy quality manager; assignment pending, O3 |
| Status | DRAFT; independent review pending |
| Implementation-map source pin | `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407` |
| Implementation-map data revision | `947fa50ff9a9e210de7e8f90e01b37add85c5954` |

Code citations resolve at the code revision above; citations to `docs/registry/implementation-map.yaml` resolve at the separately pinned map source revision. The shared-method reference is [the structural fatigue pack](../../structural/fatigue/docs/agent-pack.md), merged in #2552; its boundary distinguishes the two packages. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L14-L30]

## 1. Purpose and scope

The native assessment converts additive histogram or ordered stress-history cases into per-cycle endurance, block Miner damage, unfactored life, design utilisation and the governing case. [src/digitalmodel/fatigue/assessment.py:L116-L160]

The retained engineering scope is SCF evaluation, hotspot stress, non-critical-plane multiaxial criteria, weld classification, environmental correction, crack growth and the legacy FATG route; the native assessment/export is retained as an input, exposure and artifact contract around shared fatigue methods. [src/digitalmodel/fatigue/__init__.py:L30-L71] [src/digitalmodel/fatigue/assessment.py:L58-L83] [src/digitalmodel/fatigue/assessment_export.py:L159-L186]

| Family / interface | Boundary and reference |
| --- | --- |
| S-N curves and catalogue; Miner damage; thickness correction; rainflow; spectral fatigue; S-N fitting; critical plane; plotting and fatigue summary reports | The structural fatigue pack (#2552), sections 3–6, is the reference for each shared family. This package's duplicate definitions are inventoried in section 3 and recorded as defects in section 6; their shared checks, incidents and test records are referenced rather than repeated, except local defect detection and evidence gaps recorded in sections 5, 6 and O11. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L20-L30] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L190-L226] |
| Native assessment and export | This pack covers input validation, explicit exposure/DFF scaling, JSON/workbook readback and digest provenance; shared endurance/counting/damage remain referenced to #2552. [src/digitalmodel/fatigue/assessment.py:L58-L160] [src/digitalmodel/fatigue/assessment_export.py:L142-L186] |
| SCFs, hotspot, weld classes, environmental corrections, crack growth, FATG | Retained here as distinct implementations; the structural example consumes a supplied SCF rather than calculating joint SCFs. [src/digitalmodel/fatigue/__init__.py:L30-L71] [src/digitalmodel/fatigue/fatg.py:L153-L311] [src/digitalmodel/structural/fatigue/worked_examples.py:L405-L415] |
| Structural compatibility API | The eight re-exported names and structural modules reachable by import redirect belong to #2552, including compatibility tests and damage refusal. [src/digitalmodel/fatigue/__init__.py:L73-L89] [src/digitalmodel/_compat.py:L92-L93] [src/digitalmodel/_compat.py:L237-L320] |

The routing registry labels this domain `production`; this is metadata, not independent method qualification. The code-pin locator below predates the four-line pack registration retained at the reviewed head. [docs/registry/module-routing.yaml:L109-L114]

`AssessmentInput` requires the engineer to select the curve and environment; geometry does not establish a class and the native assessment does not automatically apply mean-stress, thickness or other corrections beyond the selected environment. [src/digitalmodel/fatigue/assessment.py:L58-L74]

## 2. Inputs and outputs

Native assessment inputs:

| Name | Unit | Allowed range / default | Source and evidence |
| --- | --- | --- | --- |
| Histogram ranges / counts | MPa / cycles | Nonnegative finite paired arrays, at least one entry; no default | `HistogramCase`; [src/digitalmodel/fatigue/assessment.py:L24-L47] |
| Ordered history | MPa | At least two finite stress samples; no default | `HistoryCase`; [src/digitalmodel/fatigue/assessment.py:L50-L55] |
| `block_years`, `design_years` | years | Strictly positive finite values, both required | `AssessmentInput`; [src/digitalmodel/fatigue/assessment.py:L23-L30] [src/digitalmodel/fatigue/assessment.py:L66-L74] |
| `design_fatigue_factor` | dimensionless | At least 1, default 1 | `AssessmentInput`; [src/digitalmodel/fatigue/assessment.py:L69-L74] |
| Code, curve, environment | identifiers | Code fixed to DNV-RP-C203; curve default D; environment default air, alternatives seawater_cp and free_corrosion | `AssessmentInput`; [src/digitalmodel/fatigue/assessment.py:L26-L28] [src/digitalmodel/fatigue/assessment.py:L66-L74] |
| Case identity | identifier | Nonempty, unique names; cases additive within the declared block | Runtime validators; [src/digitalmodel/fatigue/assessment.py:L33-L47] [src/digitalmodel/fatigue/assessment.py:L76-L83] |
| Workflow curves / stress grid | identifiers / MPa | Required curve_id or nonempty curve_ids; nonnegative strictly increasing stress list or grid with at least two points | Engine adapter; [src/digitalmodel/fatigue/workflow.py:L43-L96] |
| FATG angular frequency / stress RAO | rad/s / stress per wave height | Positive increasing frequency, equal lengths of at least two points; RAO nonnegative; legacy intercepts require psi and ft | Legacy path; [src/digitalmodel/fatigue/fatg.py:L48-L50] [src/digitalmodel/fatigue/fatg.py:L153-L182] [src/digitalmodel/fatigue/fatg.py:L314-L322] |
| Crack geometry / stress | m / MPa | Initial crack positive and below critical; coefficient units consistent with exponent and MPa√m | Crack-growth API; [src/digitalmodel/fatigue/crack_growth.py:L19-L54] [src/digitalmodel/fatigue/crack_growth.py:L60-L103] |
| Hotspot / joint geometry | mm, degrees, MPa | Local plate or chord geometry; supplied stresses paired with distances | Hotspot and SCF models; [src/digitalmodel/fatigue/hotspot_stress.py:L37-L60] [src/digitalmodel/fatigue/scf_library.py:L36-L56] |

Native and exported outputs:

| Name | Unit | Meaning / encoding | Evidence |
| --- | --- | --- | --- |
| `block_damage`, case damage, per-cycle damage | dimensionless | Sum of applied cycles divided by allowable cycles; exact counted ranges retained | [src/digitalmodel/fatigue/assessment.py:L116-L160] [src/digitalmodel/fatigue/damage.py:L13-L39] |
| `life_years`, `utilisation`, `passed` | years / dimensionless / boolean | Life = block duration / block damage; utilisation = damage × design/block duration × fatigue factor; pass at utilisation ≤ 1; zero damage gives null life | [src/digitalmodel/fatigue/assessment.py:L102-L113] [src/digitalmodel/fatigue/assessment.py:L136-L160] |
| `allowable_cycles` | cycles | Infinite native endurance is exported as null in the per-cycle result | [src/digitalmodel/fatigue/assessment.py:L86-L93] [src/digitalmodel/fatigue/assessment.py:L124-L133] |
| Assessment artifacts | JSON / XLSX / HTML / optional PDF | Native JSON read back before report construction; manifest carries native digest and PDF availability | [src/digitalmodel/fatigue/assessment_export.py:L142-L186] |

The structural spectral input/output contract is referenced to #2552; the workflow registry locates the two native S-N CSV adapter routes. #2552 does not yet record those adapter nodes or their row-count check; O11 tracks that evidence gap. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L40-L64] [docs/registry/workflows.yaml:L452-L467]

Stress histories are ordered scalar samples; `time` is optional and unused by the counter, so callers must supply the exposure duration separately through the assessment contract. [src/digitalmodel/fatigue/rainflow.py:L22-L48] [src/digitalmodel/fatigue/assessment.py:L69-L74]

The multiaxial input uses peak-to-peak ranges in local x/y/z components; the principal-stress and Findley paths use the in-plane components, while von Mises also uses the out-of-plane components. [src/digitalmodel/fatigue/multiaxial_fatigue.py:L37-L59] [src/digitalmodel/fatigue/multiaxial_fatigue.py:L129-L140] [src/digitalmodel/fatigue/multiaxial_fatigue.py:L166-L168] [src/digitalmodel/fatigue/multiaxial_fatigue.py:L215-L217]

Hotspot surface extrapolation uses distance zero at the weld toe; through-thickness positions run from the weld-toe surface to the root and bending stress uses the signed first moment about mid-thickness. [src/digitalmodel/fatigue/hotspot_stress.py:L180-L242] [src/digitalmodel/fatigue/hotspot_stress.py:L250-L300]

## 3. Method and governing standards

This table records implementation locators and the references named by the code; clause text not checked against the licensed copy applies to every standards entry below. Standards-derived coefficients, table values and detail mappings are intentionally referenced rather than reproduced. [src/digitalmodel/fatigue/c203_sn_tables.py:L3-L24] [src/digitalmodel/fatigue/sn_library.py:L32-L50]

| Method / implementing definitions | Reference named in source and implementation evidence |
| --- | --- |
| Shared S-N evaluation/catalogue: this package's `sn_curves.get_sn_curve`, `sn_library.SNCurveRecord.cycles`, `_bilinear_cycles`, catalogue builders and `sn_library_api.calculate_endurance` | Reference: #2552, section 3. Structural counterparts: `PowerLawSNCurve.get_allowable_cycles`, `BilinearSNCurve.get_allowable_cycles`, `StandardSNCurves`, `MultislopeSNCurve.get_allowable_cycles`; cross-package numerical agreement is not established here. [src/digitalmodel/fatigue/sn_curves.py:L80-L134] [src/digitalmodel/fatigue/sn_library.py:L162-L250] [src/digitalmodel/fatigue/sn_library.py:L293-L970] [src/digitalmodel/fatigue/sn_library_api.py:L238-L281] [src/digitalmodel/structural/fatigue/sn_curves.py:L152-L184] [src/digitalmodel/structural/fatigue/sn_curves.py:L272-L288] [src/digitalmodel/structural/fatigue/analysis.py:L171-L208] |
| Shared Miner and thickness: this package's `miner_damage`, `design_life_check`, `thickness_correction`; assessment and quick-check wrappers | Reference: #2552, sections 3–5. Structural counterparts: `LinearDamageAccumulation.calculate_damage`, `ModifiedMinersRule`, `NonlinearDamageAccumulation`, `ThicknessCorrection.apply_thickness_effect`; equivalence is not established here. [src/digitalmodel/fatigue/damage.py:L13-L125] [src/digitalmodel/fatigue/assessment.py:L136-L160] [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L183-L238] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L109-L217] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L259-L564] [src/digitalmodel/structural/fatigue/sn_curves.py:L863-L916] |
| Shared rainflow: this package's `rainflow_count`, `stress_histogram`, `fatigue_life`, `reversals_with_endpoints`, `peak_to_valley_span`, `check_counting_contract`, `check_exact_distribution`, `refuse_damage_calculation` | Reference: #2552, rainflow-reference and counting-refusal. Structural counterpart: `RainflowCounter.count_cycles` and `rainflow_count`; the recorded divergence precludes claiming agreement. [src/digitalmodel/fatigue/rainflow.py:L22-L201] [src/digitalmodel/fatigue/counting_contract.py:L160-L407] [src/digitalmodel/structural/fatigue/rainflow.py:L230-L310] [src/digitalmodel/structural/fatigue/rainflow.py:L729-L752] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L202-L203] |
| Shared spectral: `compute_spectral_moments`, `SpectralMoments`, `narrow_band_damage`, `wirsching_light_damage`, `dirlik_damage`, `benasciutti_tovo_damage` | Hz moments now use Hz-consistent crossing and peak rates. The line-spectrum and published single-mode regressions cover narrow-band and Dirlik damage; #2583 and F1 track the corrected rate defect. The independent Benasciutti-Tovo wide-band correction remains unqualified; #2775 tracks that separate limitation. |
| `fatg_significant_stress_range` including nested `ordinate`, `fatg_heading_cycles`, `fatg_sea_state_damage`, `fatg_annual_damage` | Separate legacy spectrum/RAO integration, heading distribution and block Miner algorithm; the historical transliteration is a regression oracle, not independent solver validation; [src/digitalmodel/fatigue/fatg.py:L153-L311] [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L64-L88] |
| `short_chord_factor_f1`, `efthymiou_ty_axial`, `efthymiou_ty_ipb`, `efthymiou_ty_opb`, `efthymiou_k_axial` | Code names Efthymiou (1988), DNV-RP-C203 2021 Appendix B and ISO 19902 2020 Annex A.16.5; F1 covers fixed-end axial saddle terms; F2/F3 remain absent; [src/digitalmodel/fatigue/scf_library.py:L108-L297] |
| `scf_butt_weld_misalignment`, `scf_cruciform_joint`, `scf_fillet_weld_toe`, `scf_shoulder_fillet`, `scf_circumferential_groove` | Code names DNV-RP-C203 2021 Appendix D, IIW-2259-15 Section 3.3, and Peterson charts; the shoulder implementation is explicitly a simplified approximation, with independent range qualification open as O2; [src/digitalmodel/fatigue/scf_library.py:L318-L464] |
| `extrapolate_hotspot_linear`, `extrapolate_hotspot_quadratic`, `extrapolate_hotspot`, `through_thickness_linearisation`, `recommended_readout_distances` | DNV-RP-C203 2021, Derivation of hot spot stress; IIW-2259-15, Hot-spot stress approach; dedicated coefficient forms, general polynomial extrapolation and numerical through-thickness integrals; [src/digitalmodel/fatigue/hotspot_stress.py:L104-L337] |
| `_principal_stresses_2d`, `von_mises_equivalent`, `principal_stress_range`, `shear_stress_correction`, `multiaxial_damage_interaction` | DNV-RP-C203 2021, Simplified fatigue analysis for combined loading; IIW-2259-15 Multiaxial fatigue; Findley (1959); principal-stress, invariant and interaction criteria each compute their own result; critical-plane coverage is handed over below; [src/digitalmodel/fatigue/multiaxial_fatigue.py:L92-L319] |
| `zone_factor`, `temperature_derating_factor`, `environment_correction`, `apply_environment_to_sn` | DNV-RP-C203 2021, S-N curves in seawater / Effect of elevated temperature; API RP 2A-WSD 2014, Environmental effects; NORSOK N-004 2013, Environmental factors; empirical environment, zone and temperature branches, with the slope-transfer defect F3; [src/digitalmodel/fatigue/environmental_correction.py:L17-L22] [src/digitalmodel/fatigue/environmental_correction.py:L85-L292] |
| `stress_intensity_factor`, `paris_law_life`, `inspection_interval` | BS 7910:2019, Guide to methods for assessing the acceptability of flaws in metallic structures; code labels the method Assessment of flaws by fracture mechanics; [docs/standards/subsea-pipelines-inventory.yaml:L1043-L1049] explicit crack-increment integration and detectable-life division; the simplified geometry factors and absent threshold treatment are not qualified here; [src/digitalmodel/fatigue/crack_growth.py:L11-L196] |
| Shared fitting / critical plane / plotting and summary reports: this package's `generate_test_data`, `_fallback_log_regression`, `fit_woehler_curve`, `design_curve`, `findley_critical_plane`, `plot_sn_curve`, `plot_damage_histogram`, `generate_report`, `inspection_recommendations` | Reference: #2552. Structural counterparts: `SNDataFitting.fit_power_law`, `CriticalPlaneAnalysis`, `plot_sn_curve` and report modules; agreement is not established here. [src/digitalmodel/fatigue/woehler_fitting.py:L18-L311] [src/digitalmodel/fatigue/multiaxial_fatigue.py:L187-L252] [src/digitalmodel/fatigue/plotting.py:L17-L83] [src/digitalmodel/fatigue/fatigue_reporting.py:L110-L157] [src/digitalmodel/fatigue/fatigue_reporting.py:L288-L339] [src/digitalmodel/structural/fatigue/sn_curves.py:L927-L1043] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L567-L649] |
| `classify_weld_detail`, `list_dnv_detail_categories` | DNV-RP-C203 2021 Appendix A, AWS D1.1/D1.1M 2020 Table 2.5, BS 7608:2014+A1:2015 Table 2; keyword/condition scoring and a static description list; numeric detail mappings are not reproduced here; [src/digitalmodel/fatigue/weld_classification.py:L16-L20] [src/digitalmodel/fatigue/weld_classification.py:L207-L315] |
| `export_assessment` | Native readback, digest and workbook/HTML export contract; shared summary-report methods are referenced above. [src/digitalmodel/fatigue/assessment_export.py:L142-L186] |

Catalogue reference inventory (source labels, not independent standards qualification); clause text not checked against the licensed copy applies to every entry. [src/digitalmodel/fatigue/sn_library.py:L34-L50]

| Catalogue / edition | Title and source locator |
| --- | --- |
| DNV-RP-C203 / 2021 | Fatigue Design of Offshore Steel Structures; [src/digitalmodel/fatigue/sn_library.py:L34-L34] |
| API RP 2A-WSD / 2014 | Planning, Designing, and Constructing Fixed Offshore Platforms – Working Stress Design, Section 5.4; [docs/standards/hydrodynamics-inventory.yaml:L447-L453] [src/digitalmodel/fatigue/sn_library.py:L384-L389] |
| BS 7608 / 2014+A1:2015 in references, 2014 in records | Fatigue design and assessment of steel structures; [src/digitalmodel/fatigue/sn_library.py:L36-L36] [src/digitalmodel/fatigue/sn_library.py:L439-L454] |
| IIW-2259-15 / 2016 | Recommendations for Fatigue Design of Welded Joints and Components; [src/digitalmodel/fatigue/hotspot_stress.py:L21-L25] [src/digitalmodel/fatigue/sn_library.py:L37-L37] |
| EN 1993-1-9 / 2005+A1:2009 in references, 2005 in records | Eurocode 3: Design of steel structures – Part 1-9: Fatigue; [European Commission JRC, Eurocode 3, EN 1993-1-9:2005](https://eurocodes.jrc.ec.europa.eu/EN-Eurocodes/eurocode-3-design-steel-structures); [src/digitalmodel/fatigue/sn_library.py:L38-L38] [src/digitalmodel/fatigue/sn_library.py:L515-L520] |
| NORSOK N-004 / 2013 | Design of steel structures, Annex C; [src/digitalmodel/fatigue/sn_library.py:L39-L39] |
| AWS D1.1/D1.1M / 2020 | Structural Welding Code – Steel; [src/digitalmodel/fatigue/sn_library.py:L40-L40] |
| ASME BPVC Section VIII, Division 2 / 2021 | Alternative Rules; Annex 3-F is the catalogue locator; [src/digitalmodel/ansys/docs/agent-pack.md:L94-L94] [src/digitalmodel/fatigue/sn_library.py:L41-L41] |
| EN 13445-3 / 2021 | Unfired pressure vessels – Part 3, Section 18; [src/digitalmodel/fatigue/sn_library.py:L42-L42] |
| PD 5500 / 2021 | Specification for unfired pressure vessels; [src/digitalmodel/fatigue/sn_library.py:L43-L43] |
| ABS Rules / 2023 | Rules for Building and Classing Steel Vessels; [src/digitalmodel/fatigue/sn_library.py:L44-L44] |
| ISO 19902 / 2020 | Fixed steel offshore structures; [src/digitalmodel/fatigue/sn_library.py:L45-L45] |
| CSA S16 / 2019 | Design of steel structures; [src/digitalmodel/fatigue/sn_library.py:L46-L46] |
| HSE OTH 92 390 / 1992 | Background to new fatigue guidance for steel joints and connections in offshore structures; [src/digitalmodel/fatigue/sn_library.py:L835-L840] [src/digitalmodel/fatigue/sn_library.py:L856-L859] |
| Department of Energy guidance / 1990 | Offshore Installations: Guidance on Design, Construction and Certification; [src/digitalmodel/fatigue/sn_library.py:L882-L886] |
| GL IV-2 / 2012 | Rules for Classification and Construction; [src/digitalmodel/fatigue/sn_library.py:L49-L49] |
| JIS B 8266 / 2003 | Construction of pressure vessels – General principles; [src/digitalmodel/fatigue/sn_library.py:L50-L50] |

Shared C203 edition/value qualification (#1588) and amendment history are referenced to #2552, sections 3 and 6; the catalogue's BS 7608 and Eurocode amendment labels still differ between references and records, F9/O7. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L226-L226] [src/digitalmodel/fatigue/sn_library.py:L36-L38] [src/digitalmodel/fatigue/sn_library.py:L439-L454] [src/digitalmodel/fatigue/sn_library.py:L515-L520]

The assessment export adds a native-calculation digest and a standard label; these are not calculation-time `Citation` sidecars resolving each standards-derived value, retained under #1588. [src/digitalmodel/fatigue/assessment_export.py:L172-L186]

## 4. Validated worked examples

Validated here means a closed-form synthetic limit with an independently constructed expected result; it does not qualify standards-derived tables or a real component. Regression examples derive expected values from the same implementation or its earlier arithmetic. [tests/fatigue/test_hotspot_stress.py:L172-L195] [tests/fatigue/test_multiaxial_fatigue.py:L19-L45] [tests/fatigue/test_assessment.py:L22-L44] [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L64-L88]

Each result below is PASS at BASE and AFTER on the pinned code, with no solver required; BASE and AFTER test totals are recorded in section 10.

| Kind / case | Independent basis or regression origin; inputs; expected value and tolerance | Existing test node ID; result; evidence |
| --- | --- | --- |
| Validated V1: uniform membrane | Integral of a constant field: 100 MPa over a 20 mm plate yields membrane and hotspot 100 MPa, bending 0 MPa; absolute tolerance 0.5 MPa is the shipped quadrature assertion | `tests/fatigue/test_hotspot_stress.py::TestThroughThicknessLinearisation::test_uniform_tension_membrane_only`; PASS; [tests/fatigue/test_hotspot_stress.py:L172-L181] |
| Validated V2: pure bending | Integral of a signed linear field: ±100 MPa across 20 mm yields membrane 0 MPa, bending and hotspot 100 MPa; absolute tolerances 1 MPa membrane and 2 MPa bending/hotspot are shipped quadrature allowances | `tests/fatigue/test_hotspot_stress.py::TestThroughThicknessLinearisation::test_pure_bending_distribution`; PASS; [tests/fatigue/test_hotspot_stress.py:L183-L195] |
| Validated V3: uniaxial invariant | Uniaxial reduction of the deviatoric-stress invariant: 100 MPa input gives 100 MPa equivalent; absolute tolerance 0.01 MPa from test | `tests/fatigue/test_multiaxial_fatigue.py::TestVonMisesEquivalent::test_uniaxial_returns_same`; PASS; [tests/fatigue/test_multiaxial_fatigue.py:L19-L23] |
| Regression R1: duration and fatigue factor | Input cycles are one quarter of the native curve's endurance, so expected block damage 0.25, life 8 years and utilisation 1 are constructed from the module itself; pytest.approx tolerances: relative 1e-6 or absolute 1e-12, in the reported quantity | `tests/fatigue/test_assessment.py::test_duration_and_design_factor_are_explicit`; PASS; [tests/fatigue/test_assessment.py:L22-L44] |
| Regression R2: native-output readback | Same synthetic load block; JSON and workbook damage 0.25 dimensionless, literal input echo and manifest/PDF availability checks; pytest.approx tolerances: relative 1e-6 or absolute 1e-12, in the reported quantity | `tests/fatigue/test_assessment.py::test_export_reads_native_output_and_workbook_back`; PASS; [tests/fatigue/test_assessment.py:L100-L136] |

## 5. QA gates

AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, G3 NOT_APPLICABLE, G4 NOT_APPLICABLE; G5 QA appendix applies to every family. These are proposed checks for subsequent use; a passing code test does not constitute independent engineering clearance.

| Gate / check ID | Module-specific executable check | Numeric pass criterion and unit | Evidence / present result |
| --- | --- | --- | --- |
| G0 / EC-01 | Run `test_invalid_input_fails_before_computation` and `test_duplicate_case_names_are_rejected` against `AssessmentInput` | 12 invalid inputs rejected, 0 accepted cases; duplicate names rejected once; counts in cases | Pytest assertions; PASS; [tests/fatigue/test_assessment.py:L66-L87] |
| G1 / EC-03 | Run all four `test_package_imports.py` tests and the retained test selection in section 10; import route is Python/pyLife, with no generated solver input deck | 4 import checks pass; 0 unexpected failed tests, count in tests; existing expected failures recorded separately | PASS for imports; retained-selection counts in section 10; [tests/fatigue/test_package_imports.py:L14-L37] |
| G1 / EC-12 | At the code pin, resolve both cited definitions for each pair D1–D8 with `git show`; count one duplicated family when both definitions compute their own result, excluding delegating helpers | 0 duplicated families, count in families | FAIL on existing code: 8 duplicated families; section 6 supplies the 16 definition locators; issue pending (7); [src/digitalmodel/fatigue/sn_curves.py:L80-L134] [src/digitalmodel/structural/fatigue/sn_curves.py:L152-L184] |
| G2 / EC-05 | Run V1 and V2 for `through_thickness_linearisation` against constant/linear-field integrals | V1 errors ≤ 0.5 MPa; V2 membrane error ≤ 1 MPa, bending/hotspot errors ≤ 2 MPa | PASS; [tests/fatigue/test_hotspot_stress.py:L172-L195] |
| G2 / EC-06 | Run V3 for `von_mises_equivalent` | Uniaxial equivalent-stress error < 0.01 MPa | PASS; [tests/fatigue/test_multiaxial_fatigue.py:L19-L23] |
| G2 / EC-08 | Run `tests/fatigue/test_spectral_audit.py::test_hz_line_rate_and_annual_damage`; compare `SpectralMoments.nu_0` with sqrt(m2/m0) for Hz moments | Relative rate error <= 1e-12 and annual-damage error <= 1e-6 | PASS: the line-spectrum rate is 0.1 Hz; #2583. The published single-mode benchmark has a separate 1% damage criterion. |
| G2 / EC-09 | Classify an unrecognized description with no joint type using `classify_weld_detail` | 0 unsupported high/medium-confidence class assignments, count in assignments | FAIL on existing code: base-material class and medium confidence; shipped test only checks nonempty output; issue pending (2); [src/digitalmodel/fatigue/weld_classification.py:L237-L285] [tests/fatigue/test_weld_classification.py:L69-L75] |
| G2 / EC-10 | Transfer an environment correction reporting `slope_change=True` through `apply_environment_to_sn` using a supplied slope that differs from the intended single-slope basis | 0 lost slope-change instructions, count in instructions | FAIL on existing code: helper returns input slope; issue pending (3); [src/digitalmodel/fatigue/environmental_correction.py:L210-L217] [src/digitalmodel/fatigue/environmental_correction.py:L278-L291] |
| G3 | NOT_APPLICABLE: this package's native path performs no dynamic solver run | No simulation time criterion | [src/digitalmodel/fatigue/assessment.py:L136-L160] |
| G4 | NOT_APPLICABLE: this package's native path performs no diffraction model or mesh calculation | No mesh/hydrostatic criterion | [src/digitalmodel/fatigue/assessment.py:L136-L160] |
| G5 / EC-11 | Run R2 and native-readback tamper test; compare native JSON to workbook/manifest and attach gate dispositions | 0 native readback mismatches; exactly 1 native digest in the export manifest; workbook damage error ≤ 2.5e-7 dimensionless (the executed assertion tolerance) | Export tests PASS; independent gate disposition remains pending; [tests/fatigue/test_assessment.py:L100-L136] [tests/fatigue/test_assessment.py:L149-L164] [src/digitalmodel/fatigue/assessment_export.py:L142-L156] [src/digitalmodel/fatigue/assessment_export.py:L172-L186] |

Shared-method QA is referenced to #2552; EC-02 and EC-07 are retired from this pack. EC-08 remains the local detection for #2583 because the reference pack has no spectral check. The retired EC-04 workflow row-count check is not yet recorded there; O11 tracks its placement with the two adapter nodes. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L198-L203]

## 6. Known failure modes and pitfalls
sweep: not run (legacy pack)

| ID / symptom | Cause and detection | Tracking / evidence |
| --- | --- | --- |
| F1: corrected Hz crossing-rate defect | Crossing and peak rates now follow the Hz moment convention. EC-08 detects recurrence through `test_hz_line_rate_and_annual_damage`; `test_published_single_mode_example_within_one_percent` independently checks Dirlik damage against its 1% criterion. These checks do not qualify the Benasciutti-Tovo broad-band correction, which remains a follow-up. | #2583; #2775 |
| F2: unrecognized weld text can receive a base-material class with medium confidence | Default joint-type and satisfied conditions supply score without a keyword match; EC-09 detects it; no rejection assertion exists in the shipped test | issue pending (2); [src/digitalmodel/fatigue/weld_classification.py:L237-L285] [tests/fatigue/test_weld_classification.py:L69-L75] |
| F3: environment slope-change instruction lost | `EnvironmentResult.slope_change` is set but the convenience function returns the supplied slope; EC-10 detects the transfer mismatch; independent qualification of the approximate correction is also open | issue pending (3); [src/digitalmodel/fatigue/environmental_correction.py:L210-L217] [src/digitalmodel/fatigue/environmental_correction.py:L278-L291] |
| F4: silent bin-centre approximation affects life | `fatigue_life` bins counts before damage; the native assessment avoids this approximation through exact ranges; compare histogram damage to exact-cycle damage when choosing a bin resolution | #1843; no detection check exists for a bounded binning error; [src/digitalmodel/fatigue/rainflow.py:L105-L128] [src/digitalmodel/fatigue/rainflow.py:L173-L189] [src/digitalmodel/fatigue/assessment.py:L116-L121] |
| F5: counting-incident handover | Reference only: #2552, section-5 counting-refusal and section-6 counting incident; detection is `tests/fatigue/test_rainflow_invariants.py::test_13_failing_path_refuses_damage_calculation`. No duplicate incident analysis or test record is retained here. | workspace-hub#3839 is the incident; #1843 is the epic; [src/digitalmodel/fatigue/counting_contract.py:L3-L3] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L203-L203] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L219-L219] |
| F6: C203 branch-incident handover | Reference only: #2552, section-6 #2165 row and env-curve. No local section-5 check is defined; named detection node: `tests/fatigue/test_sn_curves.py::TestEnvironments::test_free_corrosion_single_slope` covers free-corrosion reuse of CP parameters only. No detection check exists here for the CP-knee half of #2165; edition qualification remains #1588 in the reference pack. | #2165 (closed), #1588; [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L198-L198] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L217-L217] [tests/fatigue/test_sn_curves.py:L82-L85] |
| F7: fixed-end axial correction mistaken for pinned or bending correction | F1 scope excludes F2/F3; an accepted geometry/load basis is required; no detection check exists for the missing F2/F3 implementations | #1148; [src/digitalmodel/fatigue/scf_library.py:L108-L126] [src/digitalmodel/fatigue/scf_library.py:L185-L199] |
| F8: fitted curve silently substitutes a different statistical basis | Caught pyLife errors warn and fall back to fracture-only regression; missing scatter warns and returns an unshifted curve; inspect warnings and fitting method before use | #1719; no detection check exists that rejects fallback for an engineering deliverable; [src/digitalmodel/fatigue/woehler_fitting.py:L137-L180] [src/digitalmodel/fatigue/woehler_fitting.py:L221-L246] [src/digitalmodel/fatigue/woehler_fitting.py:L281-L289] |
| F9: catalogue/header counts or amendment claims are stale | Several individual builder summaries disagree with generated entries, and amendment labels differ between reference lists and records | issue pending (4); no detection check exists for the per-builder summary/amendment reconciliation; [src/digitalmodel/fatigue/sn_library.py:L600-L627] [src/digitalmodel/fatigue/sn_library.py:L690-L732] [src/digitalmodel/fatigue/sn_library.py:L733-L802] |
| F10: C203 table-identifier incident handover | Reference only: #2552, section-6 #2161 row; detection node `tests/fatigue/test_c203_citations.py::test_sn_table_by_edition` checks the edition/environment map; no duplicate incident analysis is retained. | #2161 (closed); [src/digitalmodel/fatigue/c203_editions.py:L2-L2] [src/digitalmodel/fatigue/c203_editions.py:L19-L37] [tests/fatigue/test_c203_citations.py:L36-L54] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L218-L218] |
| F11: third-party two-sample loss contradicts the counting registry status | `pypi_rainflow` loses the single residual half cycle on a two-sample input. Detection node: `tests/fatigue/test_rainflow_invariants.py::test_12b_pypi_rainflow_two_point_signal` (strict expected failure). Its `conforming` label is incorrect for the full declared retained-residual input domain; no source status is changed by this pack. Shared counting QA remains referenced to #2552. | workspace-hub#3839; [tests/fatigue/test_rainflow_invariants.py:L383-L395] [src/digitalmodel/fatigue/counting_contract.py:L562-L566] |
| F12: compatibility smoke tests can pass without reaching assertions | `test_basic_sn_curve`, `test_damage_accumulation` and `test_time_domain_simple` catch missing imports and return `False`; their nominal passes do not establish computation. No detection check exists that requires these three tests to reach their assertions; #2552 lists the file only in its inventory, not its offline selection. | issue pending (7), O11; [tests/test_fatigue_basic.py:L21-L54] [tests/test_fatigue_basic.py:L98-L133] [tests/test_fatigue_basic.py:L178-L220] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L789-L789] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902] |
| D1: duplicate S-N curves/catalogue implementation | EC-12, family 1: resolve the two definitions `sn_curves.get_sn_curve` here and `sn_curves.PowerLawSNCurve.get_allowable_cycles` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/sn_curves.py:L80-L134] [src/digitalmodel/structural/fatigue/sn_curves.py:L152-L184] |
| D2: duplicate Miner damage implementation | EC-12, family 2: resolve the two definitions `damage.miner_damage` here and `damage_accumulation.LinearDamageAccumulation.calculate_damage` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/damage.py:L13-L39] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L109-L217] |
| D3: duplicate Thickness correction implementation | EC-12, family 3: resolve the two definitions `damage.thickness_correction` here and `sn_curves.ThicknessCorrection.apply_thickness_effect` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/damage.py:L91-L125] [src/digitalmodel/structural/fatigue/sn_curves.py:L863-L916] |
| D4: duplicate Rainflow counting implementation | EC-12, family 4: resolve the two definitions `rainflow.rainflow_count` here and `rainflow.RainflowCounter.count_cycles` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/rainflow.py:L22-L75] [src/digitalmodel/structural/fatigue/rainflow.py:L230-L310] |
| D5: duplicate Spectral fatigue implementation | EC-12, family 5: resolve the two definitions `spectral_fatigue.narrow_band_damage` here and `frequency_domain.NarrowBandMethod.calculate_damage_rate` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established; spectral disagreement is F1. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/spectral_fatigue.py:L150-L200] [src/digitalmodel/structural/fatigue/frequency_domain.py:L180-L226] |
| D6: duplicate S-N fitting implementation | EC-12, family 6: resolve the two definitions `woehler_fitting.fit_woehler_curve` here and `sn_curves.SNDataFitting.fit_power_law` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/woehler_fitting.py:L184-L247] [src/digitalmodel/structural/fatigue/sn_curves.py:L927-L1005] |
| D7: duplicate Critical plane implementation | EC-12, family 7: resolve the two definitions `multiaxial_fatigue.findley_critical_plane` here and `damage_accumulation.CriticalPlaneAnalysis` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/multiaxial_fatigue.py:L187-L252] [src/digitalmodel/structural/fatigue/damage_accumulation.py:L567-L649] |
| D8: duplicate Plotting and summary reports implementation | EC-12, family 8: resolve the two definitions `plotting.plot_sn_curve` here and `sn_curves.plot_sn_curve` in `structural/fatigue` at the code pin; 2 independent definitions present, target 1 definition per family. Agreement is not established. Method reference: #2552. | issue pending (7); [src/digitalmodel/fatigue/plotting.py:L17-L54] [src/digitalmodel/structural/fatigue/sn_curves.py:L1008-L1043] |

Incident review date: 2026-10-15; issues #818 and #391 describe workflow capabilities rather than additional confirmed incidents; #1719 describes earlier test gaps and does not establish the present count. [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L1-L26] [docs/skills/fatigue-assessment/SKILL.md:L5-L43] [tests/fatigue/test_package_imports.py:L1-L10]

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack | Separate exact native ranges from histogram-bin-centre damage; distinguish table-number edition from value-verification edition | [src/digitalmodel/fatigue/assessment.py:L116-L121] [src/digitalmodel/fatigue/rainflow.py:L105-L128] [src/digitalmodel/fatigue/c203_editions.py:L19-L37] | Pending; issue pending (5) |
| 2026-10-15 | Pack revision 1 | Reference shared methods to #2552; preserve local interface and unique-method evidence, and record spectral disagreement as a defect | [src/digitalmodel/fatigue/__init__.py:L18-L89] [src/digitalmodel/structural/fatigue/frequency_domain.py:L128-L177] | Pending; issue pending (5) |
| 2026-10-15 | Pack revision 2 | Gate duplicate-family and spectral-defect detection in section 5; keep incident and lesson tables contiguous; record incomplete cross-pack evidence placement | [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] [tests/test_fatigue_basic.py:L21-L54] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902] | Pending; issue pending (5), issue pending (7) |

## 8. Ownership

Proposed family quality manager: engineering calculations quality manager; proposed deputy: engineering calculations deputy quality manager. The producing role maintains code citations, test evidence and this pack; the maintainer assigns people and disposes cross-module follow-up. This is a proposed role allocation, not a record of review; see O3.

## 9. Dependencies

The native path uses NumPy, pandas, Pydantic and pyLife; spectral methods also use SciPy; workbook export uses openpyxl and the shared reporting engine. [src/digitalmodel/fatigue/assessment.py:L15-L21] [src/digitalmodel/fatigue/sn_curves.py:L24-L28] [src/digitalmodel/fatigue/spectral_fatigue.py:L31-L37] [src/digitalmodel/fatigue/assessment_export.py:L14-L24]

The separate structural fatigue package and its compatibility tests are covered by #2552. It is re-exported for compatibility, and the counting-contract adapters reference additional counters in signal processing, structural fatigue applications and marine engineering. These inventories are package relationships, not solver-version qualification. [src/digitalmodel/fatigue/__init__.py:L73-L89] [src/digitalmodel/fatigue/counting_contract.py:L432-L502]

Every textual OrcaFlex occurrence inside the pinned package is in the counting registry: `OrcaFlexAdapter.RainflowHalfCycles`, `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` and `OrcaFlexTimeTraceProcessor._analyze_single_trace`; these identify consumers of a refused counting path and do not specify a supported solver version. All were inspected as text only; no licensed interface was imported or queried. [src/digitalmodel/fatigue/counting_contract.py:L593-L603]

The `sn-curve` and `sn-curve-comparison` workflow inventory entries route to basename `sn_curve` and identify their input files, output CSVs and offline tests. Their native adapter test nodes and former EC-04 row-count evidence are not yet recorded by #2552; O11 tracks their placement. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902] [docs/registry/workflows.yaml:L452-L467]

### Implementation-map inventory and search

Repository-wide discovery uses the same exact map signals over tracked text, including `scripts/`; the in-package grep parses all 24 source files. Matching files outside the package are map follow-up, not additional engineering validation.

The map families with in-package rows are ranked by in-package row count: `sn-curves-and-miner-damage` (64 of 387 rows), then `wave-spectra` (1 of 139 rows). The 65 in-package rows are inventoried here, with shared methods referenced to #2552; the 72 `structural/fatigue` rows are covered by that pack. Both families are counted; the complete 526-row inventory below preserves map order and row identity, including helpers and test definitions rather than endorsing the map's classifications.

The grep uses Python `re`, case-sensitive, on every package `.py` file at the code pin; each full search signal is omitted only if it matches `fatigue` case-insensitively. No signal is omitted for these two families. Each matching line is assigned to its innermost enclosing function or class, or one module-level definition.

| Counted family | Exact signals (`re`; full pattern retained) |
| --- | --- |
| sn-curves-and-miner-damage / algorithm | `rainflow\|miner\|cycles_to_failure\|log_a\|log10.*(?:cycles\|stress)\|(?:stress\|stress_range\|delta_sigma).*\*\*.*\bm\b\|damage_sum\|thickness_correction\|endurance_limit\|sn_curve\|SNcurve\|sn_data\|stress_life` |
| sn-curves-and-miner-damage / vocabulary | `S.N.curve\|fatigue.damage\|C203\|BS.?7608\|RP.?2A\|SNcurve\|SN.?Curve` |
| sn-curves-and-miner-damage / constants_or_keys | `thickness_correction\|endurance_limit\|n_cycles\|sn_curve\|stress_ranges` |
| wave-spectra / algorithm | `jonswap\|pierson.moskowitz\|bretschneider\|ochi.hubble\|torsethaugen\|gaussian.swell\|\bISSC\b\|spectral_moment\|normalize_to_significant\|spectral_density\|gamma\s*\*\*\|S_PM\|S_pm\|np\.exp\|math\.exp\|spectral_density\|spectrum\s*\*\|sigma\s*=` |
| wave-spectra / vocabulary | `spectrum\|spectral.density\|wave.spectra\|JONSWAP` |
| wave-spectra / constants_or_keys | `0\.07\|0\.09\|0\.287\|peak_enhancement` |

The first family hits 136 definitions (62 inventoried, 74 additional); the second hits nine (one inventoried, eight additional). The algorithm definitions among additional hits are listed in section 3; data models, imports, docstrings, serialization, plotting and delegating wrappers are helpers. The nested FATG spectrum ordinate and `compute_spectral_moments` compute wave-family results and are explicitly listed in section 3. There are 0 further second implementations absent from both sections 3 and 9, summed across counted families. A8 result: PASS for this inventory and grep; classification disputes require the family quality manager's disposition.

| Family | Additional hit | Classification and citation at code pin |
| --- | --- | --- |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/__init__.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/__init__.py:L4-L4] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/assessment.py:L19-L19] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment.py::HistogramCase` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/assessment.py:L34-L47] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment.py::HistogramCase.matching_lengths` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/assessment.py:L43-L47] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment.py::AssessmentInput` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/assessment.py:L58-L81] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment_export.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/assessment_export.py:L18-L18] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment_export.py::_report_sections` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/assessment_export.py:L100-L137] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/assessment_export.py::export_assessment` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/assessment_export.py:L153-L186] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_editions.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/c203_editions.py:L1-L1] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_editions.py::c203_sn_table` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/c203_editions.py:L62-L75] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_editions.py::c203_label` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/c203_editions.py:L78-L85] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_sn_tables.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/c203_sn_tables.py:L1-L1] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_sn_tables.py::BilinearClass` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/c203_sn_tables.py:L45-L54] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/c203_sn_tables.py::FreeCorrosionClass` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/c203_sn_tables.py:L77-L81] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/counting_contract.py::check_counting_contract` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/counting_contract.py:L181-L286] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/counting_contract.py::refuse_damage_calculation` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/counting_contract.py:L393-L416] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/crack_growth.py::paris_law_life` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/crack_growth.py:L60-L134] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/damage.py::thickness_correction` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/damage.py:L91-L125] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/environmental_correction.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/environmental_correction.py:L14-L14] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/environmental_correction.py::EnvironmentResult` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/environmental_correction.py:L58-L78] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/environmental_correction.py::zone_factor` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/environmental_correction.py:L93-L111] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/environmental_correction.py::temperature_derating_factor` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/environmental_correction.py:L118-L158] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatg.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/fatg.py:L8-L8] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatigue_reporting.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/fatigue_reporting.py:L13-L13] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatigue_reporting.py::FatigueCheckLocation` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/fatigue_reporting.py:L37-L79] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatigue_reporting.py::FatigueReport` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/fatigue_reporting.py:L82-L102] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatigue_reporting.py::generate_report` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/fatigue_reporting.py:L110-L157] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/fatigue_reporting.py::report_to_markdown` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/fatigue_reporting.py:L160-L243] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/hotspot_stress.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/hotspot_stress.py:L6-L6] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/hotspot_stress.py::extrapolate_hotspot_linear` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/hotspot_stress.py:L105-L139] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/hotspot_stress.py::extrapolate_hotspot_quadratic` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/hotspot_stress.py:L142-L180] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/hotspot_stress.py::recommended_readout_distances` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/hotspot_stress.py:L307-L337] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/multiaxial_fatigue.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/multiaxial_fatigue.py:L10-L10] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/multiaxial_fatigue.py::von_mises_equivalent` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/multiaxial_fatigue.py:L109-L146] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/multiaxial_fatigue.py::shear_stress_correction` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/multiaxial_fatigue.py:L259-L288] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/plotting.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/plotting.py:L4-L4] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/plotting.py::plot_sn_curve` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/plotting.py:L17-L54] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/plotting.py::plot_damage_histogram` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/plotting.py:L57-L83] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/rainflow.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/rainflow.py:L4-L4] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/rainflow.py::rainflow_count` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/rainflow.py:L22-L75] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/rainflow.py::stress_histogram` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/rainflow.py:L78-L127] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/scf_library.py:L9-L9] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::short_chord_factor_f1` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L108-L136] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_axial` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L139-L200] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_ipb` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L203-L234] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_opb` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L237-L259] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::efthymiou_k_axial` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L266-L295] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::scf_butt_weld_misalignment` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L318-L347] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/scf_library.py::scf_cruciform_joint` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L350-L375] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_curves.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/sn_curves.py:L2-L2] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/sn_library.py:L5-L5] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::SNCurveRecord` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L80-L177] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::SNCatalog` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L253-L281] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::SNCatalog.filter` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L267-L281] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::get_catalog` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L1018-L1037] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::get_library_curve` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L1040-L1063] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library.py::search_curves` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library.py:L1066-L1103] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/sn_library_api.py:L6-L6] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::CurveInfo` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library_api.py:L44-L68] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::SNCurve` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library_api.py:L71-L118] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::ComparisonData` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library_api.py:L121-L139] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::list_curves` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library_api.py:L181-L207] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/sn_library_api.py::compare_curves` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/sn_library_api.py:L284-L325] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/spectral_fatigue.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/spectral_fatigue.py:L5-L5] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/spectral_fatigue.py::narrow_band_damage` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/spectral_fatigue.py:L150-L200] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_classification.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/weld_classification.py:L6-L6] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_classification.py::ClassificationResult` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/weld_classification.py:L64-L88] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_classification.py::classify_weld_detail` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/weld_classification.py:L206-L284] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_classification.py::list_dnv_detail_categories` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/weld_classification.py:L291-L315] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L2-L2] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py::result_to_html` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L295-L337] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/woehler_fitting.py::design_curve` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/woehler_fitting.py:L250-L311] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/workflow.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/workflow.py:L1-L1] |
| sn-curves-and-miner-damage | `src/digitalmodel/fatigue/workflow.py::_validate_environment` | Data/validation, delegation, refusal, filtering, plotting or serialization helper; [src/digitalmodel/fatigue/workflow.py:L88-L96] |
| wave-spectra | `src/digitalmodel/fatigue/__init__.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/__init__.py:L46-L46] |
| wave-spectra | `src/digitalmodel/fatigue/fatg.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/fatg.py:L7-L7] |
| wave-spectra | `src/digitalmodel/fatigue/fatg.py::fatg_significant_stress_range.ordinate` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/fatg.py:L172-L174] |
| wave-spectra | `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_axial` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L139-L200] |
| wave-spectra | `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_opb` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L237-L259] |
| wave-spectra | `src/digitalmodel/fatigue/scf_library.py::scf_shoulder_fillet` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/scf_library.py:L410-L438] |
| wave-spectra | `src/digitalmodel/fatigue/spectral_fatigue.py::(module-level)` | Imports, labels, constants or module docstring; no further result algorithm; [src/digitalmodel/fatigue/spectral_fatigue.py:L5-L5] |
| wave-spectra | `src/digitalmodel/fatigue/spectral_fatigue.py::compute_spectral_moments` | Computes its own quantity; method explicitly covered in section 3; [src/digitalmodel/fatigue/spectral_fatigue.py:L116-L143] |

The recomputed repository discovery covers 5,562 tracked Python files, including scripts; six have AST parse failures. Matching text files (Python/Markdown/reStructuredText/YAML) include 32 S-N/Miner and 60 wave-spectra files in `scripts/`; these are discovery counts, not implementation counts.

The map calls `short_chord_factor_f1` a wave-spectrum implementation, although its code evaluates a joint SCF correction; preserve the inventory row and correct the map classification separately, issue pending (6). [src/digitalmodel/fatigue/scf_library.py:L108-L136]

Repository-wide discovery searched 5,562 tracked Python files at the code pin, including `src/`, `tests/`, `examples/`, `docs/` and `scripts/`; syntax failures and opaque artifacts are recorded as search limits, not evidence of absence. Additional implementations outside this package are map follow-up only; section-3 families without appropriate map rows include hotspot extrapolation, SCFs, crack growth, weld classification, multiaxial criteria, Woehler fitting, environmental corrections and FATG, issue pending (6). [src/digitalmodel/fatigue/hotspot_stress.py:L104-L300] [src/digitalmodel/fatigue/scf_library.py:L108-L464] [src/digitalmodel/fatigue/crack_growth.py:L19-L196] [src/digitalmodel/fatigue/weld_classification.py:L207-L285] [src/digitalmodel/fatigue/multiaxial_fatigue.py:L92-L319] [src/digitalmodel/fatigue/woehler_fitting.py:L137-L311]

| Family / map ordinal | Implementation path | Symbol | Map relation / pinned evidence |
| --- | --- | --- | --- |
| wave-spectra / 1 | `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | independent; [docs/registry/implementation-map.yaml:L12155-L12165] |
| wave-spectra / 2 | `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | independent; [docs/registry/implementation-map.yaml:L12166-L12176] |
| wave-spectra / 3 | `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | example; [docs/registry/implementation-map.yaml:L12177-L12187] |
| wave-spectra / 4 | `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | example; [docs/registry/implementation-map.yaml:L12188-L12198] |
| wave-spectra / 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example; [docs/registry/implementation-map.yaml:L12199-L12212] |
| wave-spectra / 6 | `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | example; [docs/registry/implementation-map.yaml:L12213-L12223] |
| wave-spectra / 7 | `scripts/_seed_eq_mock.py` | `mock_time_series` | independent; [docs/registry/implementation-map.yaml:L12224-L12234] |
| wave-spectra / 8 | `scripts/benchmark/extract_pdf_plots.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12235-L12245] |
| wave-spectra / 9 | `scripts/benchmark/validate_owd_vs_spec.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12246-L12256] |
| wave-spectra / 10 | `scripts/extract_property_inventory.py` | `build_dormancy_rules` | independent; [docs/registry/implementation-map.yaml:L12257-L12267] |
| wave-spectra / 11 | `scripts/generate_env_files.py` | `create_wave_file` | independent; [docs/registry/implementation-map.yaml:L12268-L12278] |
| wave-spectra / 12 | `scripts/generate_env_files.py` | `main` | independent; [docs/registry/implementation-map.yaml:L12279-L12291] |
| wave-spectra / 13 | `scripts/ingest_orcina_help.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12292-L12302] |
| wave-spectra / 14 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12303-L12313] |
| wave-spectra / 15 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12314-L12324] |
| wave-spectra / 16 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12325-L12335] |
| wave-spectra / 17 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12336-L12346] |
| wave-spectra / 18 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12347-L12357] |
| wave-spectra / 19 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12358-L12368] |
| wave-spectra / 20 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12369-L12379] |
| wave-spectra / 21 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12380-L12390] |
| wave-spectra / 22 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12391-L12401] |
| wave-spectra / 23 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12402-L12412] |
| wave-spectra / 24 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L12413-L12423] |
| wave-spectra / 25 | `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | independent; [docs/registry/implementation-map.yaml:L12424-L12436] |
| wave-spectra / 26 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L12437-L12447] |
| wave-spectra / 27 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; [docs/registry/implementation-map.yaml:L12448-L12458] |
| wave-spectra / 28 | `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | independent; [docs/registry/implementation-map.yaml:L12459-L12469] |
| wave-spectra / 29 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | independent; [docs/registry/implementation-map.yaml:L12470-L12480] |
| wave-spectra / 30 | `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | independent; [docs/registry/implementation-map.yaml:L12481-L12491] |
| wave-spectra / 31 | `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | independent; [docs/registry/implementation-map.yaml:L12492-L12502] |
| wave-spectra / 32 | `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | independent; [docs/registry/implementation-map.yaml:L12503-L12513] |
| wave-spectra / 33 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | independent; [docs/registry/implementation-map.yaml:L12514-L12524] |
| wave-spectra / 34 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | independent; [docs/registry/implementation-map.yaml:L12525-L12537] |
| wave-spectra / 35 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | independent; [docs/registry/implementation-map.yaml:L12538-L12549] |
| wave-spectra / 36 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | independent; [docs/registry/implementation-map.yaml:L12550-L12561] |
| wave-spectra / 37 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | independent; [docs/registry/implementation-map.yaml:L12562-L12573] |
| wave-spectra / 38 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | wrapper; [docs/registry/implementation-map.yaml:L12574-L12589] |
| wave-spectra / 39 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | independent; [docs/registry/implementation-map.yaml:L12590-L12600] |
| wave-spectra / 40 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | independent; [docs/registry/implementation-map.yaml:L12601-L12611] |
| wave-spectra / 41 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | independent; [docs/registry/implementation-map.yaml:L12612-L12622] |
| wave-spectra / 42 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L12623-L12635] |
| wave-spectra / 43 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | independent; [docs/registry/implementation-map.yaml:L12636-L12646] |
| wave-spectra / 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | independent; [docs/registry/implementation-map.yaml:L12647-L12659] |
| wave-spectra / 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | independent; [docs/registry/implementation-map.yaml:L12660-L12673] |
| wave-spectra / 46 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | independent; [docs/registry/implementation-map.yaml:L12674-L12684] |
| wave-spectra / 47 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent; [docs/registry/implementation-map.yaml:L12685-L12697] |
| wave-spectra / 48 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | independent; [docs/registry/implementation-map.yaml:L12698-L12710] |
| wave-spectra / 49 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | independent; [docs/registry/implementation-map.yaml:L12711-L12722] |
| wave-spectra / 50 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L12723-L12733] |
| wave-spectra / 51 | `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | copy; [docs/registry/implementation-map.yaml:L12734-L12746] |
| wave-spectra / 52 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L12747-L12757] |
| wave-spectra / 53 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; [docs/registry/implementation-map.yaml:L12758-L12768] |
| wave-spectra / 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L12769-L12779] |
| wave-spectra / 55 | `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | independent; [docs/registry/implementation-map.yaml:L12780-L12792] |
| wave-spectra / 56 | `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | independent; [docs/registry/implementation-map.yaml:L12793-L12803] |
| wave-spectra / 57 | `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | independent; [docs/registry/implementation-map.yaml:L12804-L12816] |
| wave-spectra / 58 | `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L12817-L12827] |
| wave-spectra / 59 | `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | independent; [docs/registry/implementation-map.yaml:L12828-L12839] |
| wave-spectra / 60 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | independent; [docs/registry/implementation-map.yaml:L12840-L12850] |
| wave-spectra / 61 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | independent; [docs/registry/implementation-map.yaml:L12851-L12861] |
| wave-spectra / 62 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | independent; [docs/registry/implementation-map.yaml:L12862-L12872] |
| wave-spectra / 63 | `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | independent; [docs/registry/implementation-map.yaml:L12873-L12883] |
| wave-spectra / 64 | `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | independent; [docs/registry/implementation-map.yaml:L12884-L12894] |
| wave-spectra / 65 | `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | independent; [docs/registry/implementation-map.yaml:L12895-L12907] |
| wave-spectra / 66 | `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | wrapper; [docs/registry/implementation-map.yaml:L12908-L12919] |
| wave-spectra / 67 | `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | independent; [docs/registry/implementation-map.yaml:L12920-L12930] |
| wave-spectra / 68 | `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | independent; [docs/registry/implementation-map.yaml:L12931-L12941] |
| wave-spectra / 69 | `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | independent; [docs/registry/implementation-map.yaml:L12942-L12954] |
| wave-spectra / 70 | `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | independent; [docs/registry/implementation-map.yaml:L12955-L12971] |
| wave-spectra / 71 | `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | independent; [docs/registry/implementation-map.yaml:L12972-L12982] |
| wave-spectra / 72 | `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | independent; [docs/registry/implementation-map.yaml:L12983-L12993] |
| wave-spectra / 73 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | independent; [docs/registry/implementation-map.yaml:L12994-L13004] |
| wave-spectra / 74 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | independent; [docs/registry/implementation-map.yaml:L13005-L13015] |
| wave-spectra / 75 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | wrapper; [docs/registry/implementation-map.yaml:L13016-L13028] |
| wave-spectra / 76 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | independent; [docs/registry/implementation-map.yaml:L13029-L13039] |
| wave-spectra / 77 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | copy; [docs/registry/implementation-map.yaml:L13040-L13053] |
| wave-spectra / 78 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | independent; [docs/registry/implementation-map.yaml:L13054-L13066] |
| wave-spectra / 79 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | independent; [docs/registry/implementation-map.yaml:L13067-L13077] |
| wave-spectra / 80 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | independent; [docs/registry/implementation-map.yaml:L13078-L13088] |
| wave-spectra / 81 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | copy; [docs/registry/implementation-map.yaml:L13089-L13101] |
| wave-spectra / 82 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | copy; [docs/registry/implementation-map.yaml:L13102-L13115] |
| wave-spectra / 83 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | independent; [docs/registry/implementation-map.yaml:L13116-L13126] |
| wave-spectra / 84 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent; [docs/registry/implementation-map.yaml:L13127-L13139] |
| wave-spectra / 85 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | independent; [docs/registry/implementation-map.yaml:L13140-L13150] |
| wave-spectra / 86 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L13151-L13161] |
| wave-spectra / 87 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L13162-L13172] |
| wave-spectra / 88 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L13173-L13183] |
| wave-spectra / 89 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L13184-L13194] |
| wave-spectra / 90 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | independent; [docs/registry/implementation-map.yaml:L13195-L13205] |
| wave-spectra / 91 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | independent; [docs/registry/implementation-map.yaml:L13206-L13216] |
| wave-spectra / 92 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | independent; [docs/registry/implementation-map.yaml:L13217-L13227] |
| wave-spectra / 93 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | independent; [docs/registry/implementation-map.yaml:L13228-L13238] |
| wave-spectra / 94 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | independent; [docs/registry/implementation-map.yaml:L13239-L13249] |
| wave-spectra / 95 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L13250-L13262] |
| wave-spectra / 96 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | independent; [docs/registry/implementation-map.yaml:L13263-L13276] |
| wave-spectra / 97 | `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | independent; [docs/registry/implementation-map.yaml:L13277-L13287] |
| wave-spectra / 98 | `src/digitalmodel/workflows/installation_full_report.py` | `_design` | independent; [docs/registry/implementation-map.yaml:L13288-L13298] |
| wave-spectra / 99 | `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | independent; [docs/registry/implementation-map.yaml:L13299-L13309] |
| wave-spectra / 100 | `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | independent; [docs/registry/implementation-map.yaml:L13310-L13320] |
| wave-spectra / 101 | `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | independent; [docs/registry/implementation-map.yaml:L13321-L13331] |
| wave-spectra / 102 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | independent; [docs/registry/implementation-map.yaml:L13332-L13342] |
| wave-spectra / 103 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | independent; [docs/registry/implementation-map.yaml:L13343-L13353] |
| wave-spectra / 104 | `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L13354-L13364] |
| wave-spectra / 105 | `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L13365-L13375] |
| wave-spectra / 106 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L13376-L13386] |
| wave-spectra / 107 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L13387-L13397] |
| wave-spectra / 108 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | test_helper; [docs/registry/implementation-map.yaml:L13398-L13408] |
| wave-spectra / 109 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | test_helper; [docs/registry/implementation-map.yaml:L13409-L13419] |
| wave-spectra / 110 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | test_helper; [docs/registry/implementation-map.yaml:L13420-L13430] |
| wave-spectra / 111 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | test_helper; [docs/registry/implementation-map.yaml:L13431-L13441] |
| wave-spectra / 112 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | test_helper; [docs/registry/implementation-map.yaml:L13442-L13452] |
| wave-spectra / 113 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; [docs/registry/implementation-map.yaml:L13453-L13465] |
| wave-spectra / 114 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; [docs/registry/implementation-map.yaml:L13466-L13478] |
| wave-spectra / 115 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; [docs/registry/implementation-map.yaml:L13479-L13491] |
| wave-spectra / 116 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; [docs/registry/implementation-map.yaml:L13492-L13504] |
| wave-spectra / 117 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; [docs/registry/implementation-map.yaml:L13505-L13517] |
| wave-spectra / 118 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; [docs/registry/implementation-map.yaml:L13518-L13530] |
| wave-spectra / 119 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; [docs/registry/implementation-map.yaml:L13531-L13543] |
| wave-spectra / 120 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; [docs/registry/implementation-map.yaml:L13544-L13556] |
| wave-spectra / 121 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; [docs/registry/implementation-map.yaml:L13557-L13569] |
| wave-spectra / 122 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; [docs/registry/implementation-map.yaml:L13570-L13582] |
| wave-spectra / 123 | `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | test_helper; [docs/registry/implementation-map.yaml:L13583-L13593] |
| wave-spectra / 124 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | test_helper; [docs/registry/implementation-map.yaml:L13594-L13604] |
| wave-spectra / 125 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | test_helper; [docs/registry/implementation-map.yaml:L13605-L13615] |
| wave-spectra / 126 | `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | test_helper; [docs/registry/implementation-map.yaml:L13616-L13626] |
| wave-spectra / 127 | `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | test_helper; [docs/registry/implementation-map.yaml:L13627-L13637] |
| wave-spectra / 128 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | test_helper; [docs/registry/implementation-map.yaml:L13638-L13648] |
| wave-spectra / 129 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L13649-L13659] |
| wave-spectra / 130 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L13660-L13670] |
| wave-spectra / 131 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L13671-L13681] |
| wave-spectra / 132 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | test_helper; [docs/registry/implementation-map.yaml:L13682-L13692] |
| wave-spectra / 133 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | test_helper; [docs/registry/implementation-map.yaml:L13693-L13703] |
| wave-spectra / 134 | `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | test_helper; [docs/registry/implementation-map.yaml:L13704-L13714] |
| wave-spectra / 135 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper; [docs/registry/implementation-map.yaml:L13715-L13725] |
| wave-spectra / 136 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L13726-L13736] |
| wave-spectra / 137 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper; [docs/registry/implementation-map.yaml:L13737-L13749] |
| wave-spectra / 138 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper; [docs/registry/implementation-map.yaml:L13750-L13760] |
| wave-spectra / 139 | `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L13761-L13771] |
| sn-curves-and-miner-damage / 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | independent; [docs/registry/implementation-map.yaml:L15063-L15071] |
| sn-curves-and-miner-damage / 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | independent; [docs/registry/implementation-map.yaml:L15072-L15080] |
| sn-curves-and-miner-damage / 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L15081-L15089] |
| sn-curves-and-miner-damage / 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | independent; [docs/registry/implementation-map.yaml:L15090-L15098] |
| sn-curves-and-miner-damage / 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | independent; [docs/registry/implementation-map.yaml:L15099-L15111] |
| sn-curves-and-miner-damage / 6 | `examples/demos/demo_sn_library.py` | `main` | example; [docs/registry/implementation-map.yaml:L15112-L15122] |
| sn-curves-and-miner-damage / 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | example; [docs/registry/implementation-map.yaml:L15123-L15131] |
| sn-curves-and-miner-damage / 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | example; [docs/registry/implementation-map.yaml:L15132-L15140] |
| sn-curves-and-miner-damage / 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | example; [docs/registry/implementation-map.yaml:L15141-L15149] |
| sn-curves-and-miner-damage / 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | example; [docs/registry/implementation-map.yaml:L15150-L15158] |
| sn-curves-and-miner-damage / 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | example; [docs/registry/implementation-map.yaml:L15159-L15167] |
| sn-curves-and-miner-damage / 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | example; [docs/registry/implementation-map.yaml:L15168-L15182] |
| sn-curves-and-miner-damage / 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | example; [docs/registry/implementation-map.yaml:L15183-L15191] |
| sn-curves-and-miner-damage / 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | example; [docs/registry/implementation-map.yaml:L15192-L15200] |
| sn-curves-and-miner-damage / 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | example; [docs/registry/implementation-map.yaml:L15201-L15209] |
| sn-curves-and-miner-damage / 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | example; [docs/registry/implementation-map.yaml:L15210-L15218] |
| sn-curves-and-miner-damage / 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | example; [docs/registry/implementation-map.yaml:L15219-L15227] |
| sn-curves-and-miner-damage / 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | example; [docs/registry/implementation-map.yaml:L15228-L15236] |
| sn-curves-and-miner-damage / 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | example; [docs/registry/implementation-map.yaml:L15237-L15245] |
| sn-curves-and-miner-damage / 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | example; [docs/registry/implementation-map.yaml:L15246-L15254] |
| sn-curves-and-miner-damage / 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | example; [docs/registry/implementation-map.yaml:L15255-L15263] |
| sn-curves-and-miner-damage / 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | example; [docs/registry/implementation-map.yaml:L15264-L15272] |
| sn-curves-and-miner-damage / 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | example; [docs/registry/implementation-map.yaml:L15273-L15281] |
| sn-curves-and-miner-damage / 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | example; [docs/registry/implementation-map.yaml:L15282-L15290] |
| sn-curves-and-miner-damage / 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | example; [docs/registry/implementation-map.yaml:L15291-L15299] |
| sn-curves-and-miner-damage / 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | example; [docs/registry/implementation-map.yaml:L15300-L15308] |
| sn-curves-and-miner-damage / 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | example; [docs/registry/implementation-map.yaml:L15309-L15317] |
| sn-curves-and-miner-damage / 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | example; [docs/registry/implementation-map.yaml:L15318-L15333] |
| sn-curves-and-miner-damage / 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | example; [docs/registry/implementation-map.yaml:L15334-L15342] |
| sn-curves-and-miner-damage / 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | example; [docs/registry/implementation-map.yaml:L15343-L15351] |
| sn-curves-and-miner-damage / 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | example; [docs/registry/implementation-map.yaml:L15352-L15363] |
| sn-curves-and-miner-damage / 32 | `scripts/capabilities/build_onepagers.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L15364-L15372] |
| sn-curves-and-miner-damage / 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L15373-L15381] |
| sn-curves-and-miner-damage / 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15382-L15390] |
| sn-curves-and-miner-damage / 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | independent; [docs/registry/implementation-map.yaml:L15391-L15399] |
| sn-curves-and-miner-damage / 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | independent; [docs/registry/implementation-map.yaml:L15400-L15408] |
| sn-curves-and-miner-damage / 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | independent; [docs/registry/implementation-map.yaml:L15409-L15417] |
| sn-curves-and-miner-damage / 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15418-L15429] |
| sn-curves-and-miner-damage / 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | independent; [docs/registry/implementation-map.yaml:L15430-L15438] |
| sn-curves-and-miner-damage / 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | independent; [docs/registry/implementation-map.yaml:L15439-L15449] |
| sn-curves-and-miner-damage / 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | independent; [docs/registry/implementation-map.yaml:L15450-L15459] |
| sn-curves-and-miner-damage / 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | independent; [docs/registry/implementation-map.yaml:L15460-L15468] |
| sn-curves-and-miner-damage / 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | independent; [docs/registry/implementation-map.yaml:L15469-L15479] |
| sn-curves-and-miner-damage / 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | independent; [docs/registry/implementation-map.yaml:L15480-L15490] |
| sn-curves-and-miner-damage / 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15491-L15499] |
| sn-curves-and-miner-damage / 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15500-L15508] |
| sn-curves-and-miner-damage / 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15509-L15517] |
| sn-curves-and-miner-damage / 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15518-L15526] |
| sn-curves-and-miner-damage / 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L15527-L15535] |
| sn-curves-and-miner-damage / 50 | `scripts/riser_database/build_tables.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L15536-L15544] |
| sn-curves-and-miner-damage / 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | independent; [docs/registry/implementation-map.yaml:L15545-L15553] |
| sn-curves-and-miner-damage / 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L15554-L15562] |
| sn-curves-and-miner-damage / 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L15563-L15573] |
| sn-curves-and-miner-damage / 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | independent; [docs/registry/implementation-map.yaml:L15574-L15582] |
| sn-curves-and-miner-damage / 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | independent; [docs/registry/implementation-map.yaml:L15583-L15591] |
| sn-curves-and-miner-damage / 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | independent; [docs/registry/implementation-map.yaml:L15592-L15600] |
| sn-curves-and-miner-damage / 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | independent; [docs/registry/implementation-map.yaml:L15601-L15609] |
| sn-curves-and-miner-damage / 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | independent; [docs/registry/implementation-map.yaml:L15610-L15620] |
| sn-curves-and-miner-damage / 59 | `src/digitalmodel/engine.py` | `engine` | caller; [docs/registry/implementation-map.yaml:L15621-L15637] |
| sn-curves-and-miner-damage / 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L15638-L15649] |
| sn-curves-and-miner-damage / 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | independent; [docs/registry/implementation-map.yaml:L15650-L15658] |
| sn-curves-and-miner-damage / 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | independent; [docs/registry/implementation-map.yaml:L15659-L15667] |
| sn-curves-and-miner-damage / 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | independent; [docs/registry/implementation-map.yaml:L15668-L15680] |
| sn-curves-and-miner-damage / 64 | `src/digitalmodel/fatigue/counting_contract.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L15681-L15689] |
| sn-curves-and-miner-damage / 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | independent; [docs/registry/implementation-map.yaml:L15690-L15698] |
| sn-curves-and-miner-damage / 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | independent; [docs/registry/implementation-map.yaml:L15699-L15707] |
| sn-curves-and-miner-damage / 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | independent; [docs/registry/implementation-map.yaml:L15708-L15716] |
| sn-curves-and-miner-damage / 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | independent; [docs/registry/implementation-map.yaml:L15717-L15725] |
| sn-curves-and-miner-damage / 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | independent; [docs/registry/implementation-map.yaml:L15726-L15734] |
| sn-curves-and-miner-damage / 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | independent; [docs/registry/implementation-map.yaml:L15735-L15743] |
| sn-curves-and-miner-damage / 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | independent; [docs/registry/implementation-map.yaml:L15744-L15752] |
| sn-curves-and-miner-damage / 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | independent; [docs/registry/implementation-map.yaml:L15753-L15761] |
| sn-curves-and-miner-damage / 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | independent; [docs/registry/implementation-map.yaml:L15762-L15770] |
| sn-curves-and-miner-damage / 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | independent; [docs/registry/implementation-map.yaml:L15771-L15781] |
| sn-curves-and-miner-damage / 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | independent; [docs/registry/implementation-map.yaml:L15782-L15790] |
| sn-curves-and-miner-damage / 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | independent; [docs/registry/implementation-map.yaml:L15791-L15801] |
| sn-curves-and-miner-damage / 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | independent; [docs/registry/implementation-map.yaml:L15802-L15810] |
| sn-curves-and-miner-damage / 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | independent; [docs/registry/implementation-map.yaml:L15811-L15821] |
| sn-curves-and-miner-damage / 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | independent; [docs/registry/implementation-map.yaml:L15822-L15830] |
| sn-curves-and-miner-damage / 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | independent; [docs/registry/implementation-map.yaml:L15831-L15842] |
| sn-curves-and-miner-damage / 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | independent; [docs/registry/implementation-map.yaml:L15843-L15851] |
| sn-curves-and-miner-damage / 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | caller; [docs/registry/implementation-map.yaml:L15852-L15862] |
| sn-curves-and-miner-damage / 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | caller; [docs/registry/implementation-map.yaml:L15863-L15872] |
| sn-curves-and-miner-damage / 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | wrapper; [docs/registry/implementation-map.yaml:L15873-L15883] |
| sn-curves-and-miner-damage / 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | wrapper; [docs/registry/implementation-map.yaml:L15884-L15894] |
| sn-curves-and-miner-damage / 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | wrapper; [docs/registry/implementation-map.yaml:L15895-L15905] |
| sn-curves-and-miner-damage / 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | independent; [docs/registry/implementation-map.yaml:L15906-L15914] |
| sn-curves-and-miner-damage / 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | caller; [docs/registry/implementation-map.yaml:L15915-L15924] |
| sn-curves-and-miner-damage / 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | independent; [docs/registry/implementation-map.yaml:L15925-L15933] |
| sn-curves-and-miner-damage / 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | independent; [docs/registry/implementation-map.yaml:L15934-L15942] |
| sn-curves-and-miner-damage / 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | independent; [docs/registry/implementation-map.yaml:L15943-L15951] |
| sn-curves-and-miner-damage / 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | independent; [docs/registry/implementation-map.yaml:L15952-L15960] |
| sn-curves-and-miner-damage / 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | independent; [docs/registry/implementation-map.yaml:L15961-L15971] |
| sn-curves-and-miner-damage / 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | independent; [docs/registry/implementation-map.yaml:L15972-L15981] |
| sn-curves-and-miner-damage / 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | independent; [docs/registry/implementation-map.yaml:L15982-L15991] |
| sn-curves-and-miner-damage / 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | caller; [docs/registry/implementation-map.yaml:L15992-L16001] |
| sn-curves-and-miner-damage / 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | independent; [docs/registry/implementation-map.yaml:L16002-L16010] |
| sn-curves-and-miner-damage / 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | independent; [docs/registry/implementation-map.yaml:L16011-L16019] |
| sn-curves-and-miner-damage / 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | independent; [docs/registry/implementation-map.yaml:L16020-L16029] |
| sn-curves-and-miner-damage / 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | caller; [docs/registry/implementation-map.yaml:L16030-L16039] |
| sn-curves-and-miner-damage / 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | independent; [docs/registry/implementation-map.yaml:L16040-L16048] |
| sn-curves-and-miner-damage / 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | independent; [docs/registry/implementation-map.yaml:L16049-L16058] |
| sn-curves-and-miner-damage / 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | independent; [docs/registry/implementation-map.yaml:L16059-L16067] |
| sn-curves-and-miner-damage / 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | independent; [docs/registry/implementation-map.yaml:L16068-L16077] |
| sn-curves-and-miner-damage / 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | caller; [docs/registry/implementation-map.yaml:L16078-L16087] |
| sn-curves-and-miner-damage / 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | independent; [docs/registry/implementation-map.yaml:L16088-L16096] |
| sn-curves-and-miner-damage / 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | independent; [docs/registry/implementation-map.yaml:L16097-L16105] |
| sn-curves-and-miner-damage / 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | independent; [docs/registry/implementation-map.yaml:L16106-L16114] |
| sn-curves-and-miner-damage / 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | caller; [docs/registry/implementation-map.yaml:L16115-L16125] |
| sn-curves-and-miner-damage / 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | independent; [docs/registry/implementation-map.yaml:L16126-L16134] |
| sn-curves-and-miner-damage / 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | independent; [docs/registry/implementation-map.yaml:L16135-L16143] |
| sn-curves-and-miner-damage / 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | wrapper; [docs/registry/implementation-map.yaml:L16144-L16154] |
| sn-curves-and-miner-damage / 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | wrapper; [docs/registry/implementation-map.yaml:L16155-L16166] |
| sn-curves-and-miner-damage / 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | independent; [docs/registry/implementation-map.yaml:L16167-L16175] |
| sn-curves-and-miner-damage / 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | independent; [docs/registry/implementation-map.yaml:L16176-L16184] |
| sn-curves-and-miner-damage / 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | independent; [docs/registry/implementation-map.yaml:L16185-L16194] |
| sn-curves-and-miner-damage / 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | independent; [docs/registry/implementation-map.yaml:L16195-L16203] |
| sn-curves-and-miner-damage / 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | independent; [docs/registry/implementation-map.yaml:L16204-L16212] |
| sn-curves-and-miner-damage / 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | independent; [docs/registry/implementation-map.yaml:L16213-L16221] |
| sn-curves-and-miner-damage / 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L16222-L16238] |
| sn-curves-and-miner-damage / 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | independent; [docs/registry/implementation-map.yaml:L16239-L16247] |
| sn-curves-and-miner-damage / 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | independent; [docs/registry/implementation-map.yaml:L16248-L16256] |
| sn-curves-and-miner-damage / 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | independent; [docs/registry/implementation-map.yaml:L16257-L16265] |
| sn-curves-and-miner-damage / 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | independent; [docs/registry/implementation-map.yaml:L16266-L16276] |
| sn-curves-and-miner-damage / 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | independent; [docs/registry/implementation-map.yaml:L16277-L16285] |
| sn-curves-and-miner-damage / 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | caller; [docs/registry/implementation-map.yaml:L16286-L16297] |
| sn-curves-and-miner-damage / 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | independent; [docs/registry/implementation-map.yaml:L16298-L16306] |
| sn-curves-and-miner-damage / 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | independent; [docs/registry/implementation-map.yaml:L16307-L16315] |
| sn-curves-and-miner-damage / 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L16316-L16327] |
| sn-curves-and-miner-damage / 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L16328-L16336] |
| sn-curves-and-miner-damage / 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L16337-L16345] |
| sn-curves-and-miner-damage / 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | wrapper; [docs/registry/implementation-map.yaml:L16346-L16356] |
| sn-curves-and-miner-damage / 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L16357-L16367] |
| sn-curves-and-miner-damage / 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | wrapper; [docs/registry/implementation-map.yaml:L16368-L16378] |
| sn-curves-and-miner-damage / 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | independent; [docs/registry/implementation-map.yaml:L16379-L16387] |
| sn-curves-and-miner-damage / 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L16388-L16396] |
| sn-curves-and-miner-damage / 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L16397-L16405] |
| sn-curves-and-miner-damage / 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | independent; [docs/registry/implementation-map.yaml:L16406-L16414] |
| sn-curves-and-miner-damage / 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | independent; [docs/registry/implementation-map.yaml:L16415-L16423] |
| sn-curves-and-miner-damage / 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | independent; [docs/registry/implementation-map.yaml:L16424-L16432] |
| sn-curves-and-miner-damage / 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | independent; [docs/registry/implementation-map.yaml:L16433-L16441] |
| sn-curves-and-miner-damage / 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | independent; [docs/registry/implementation-map.yaml:L16442-L16450] |
| sn-curves-and-miner-damage / 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | independent; [docs/registry/implementation-map.yaml:L16451-L16459] |
| sn-curves-and-miner-damage / 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | independent; [docs/registry/implementation-map.yaml:L16460-L16468] |
| sn-curves-and-miner-damage / 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | independent; [docs/registry/implementation-map.yaml:L16469-L16477] |
| sn-curves-and-miner-damage / 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | independent; [docs/registry/implementation-map.yaml:L16478-L16486] |
| sn-curves-and-miner-damage / 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | independent; [docs/registry/implementation-map.yaml:L16487-L16495] |
| sn-curves-and-miner-damage / 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | independent; [docs/registry/implementation-map.yaml:L16496-L16504] |
| sn-curves-and-miner-damage / 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L16505-L16517] |
| sn-curves-and-miner-damage / 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | independent; [docs/registry/implementation-map.yaml:L16518-L16527] |
| sn-curves-and-miner-damage / 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | independent; [docs/registry/implementation-map.yaml:L16528-L16536] |
| sn-curves-and-miner-damage / 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | independent; [docs/registry/implementation-map.yaml:L16537-L16545] |
| sn-curves-and-miner-damage / 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L16546-L16555] |
| sn-curves-and-miner-damage / 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | independent; [docs/registry/implementation-map.yaml:L16556-L16564] |
| sn-curves-and-miner-damage / 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | independent; [docs/registry/implementation-map.yaml:L16565-L16573] |
| sn-curves-and-miner-damage / 156 | `src/digitalmodel/parametric/refresh.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L16574-L16582] |
| sn-curves-and-miner-damage / 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L16583-L16593] |
| sn-curves-and-miner-damage / 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | independent; [docs/registry/implementation-map.yaml:L16594-L16602] |
| sn-curves-and-miner-damage / 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L16603-L16613] |
| sn-curves-and-miner-damage / 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | independent; [docs/registry/implementation-map.yaml:L16614-L16623] |
| sn-curves-and-miner-damage / 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | independent; [docs/registry/implementation-map.yaml:L16624-L16632] |
| sn-curves-and-miner-damage / 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | independent; [docs/registry/implementation-map.yaml:L16633-L16641] |
| sn-curves-and-miner-damage / 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L16642-L16650] |
| sn-curves-and-miner-damage / 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | independent; [docs/registry/implementation-map.yaml:L16651-L16659] |
| sn-curves-and-miner-damage / 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | independent; [docs/registry/implementation-map.yaml:L16660-L16668] |
| sn-curves-and-miner-damage / 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | independent; [docs/registry/implementation-map.yaml:L16669-L16677] |
| sn-curves-and-miner-damage / 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | independent; [docs/registry/implementation-map.yaml:L16678-L16686] |
| sn-curves-and-miner-damage / 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | independent; [docs/registry/implementation-map.yaml:L16687-L16695] |
| sn-curves-and-miner-damage / 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | independent; [docs/registry/implementation-map.yaml:L16696-L16704] |
| sn-curves-and-miner-damage / 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L16705-L16719] |
| sn-curves-and-miner-damage / 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | independent; [docs/registry/implementation-map.yaml:L16720-L16730] |
| sn-curves-and-miner-damage / 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | independent; [docs/registry/implementation-map.yaml:L16731-L16740] |
| sn-curves-and-miner-damage / 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | independent; [docs/registry/implementation-map.yaml:L16741-L16750] |
| sn-curves-and-miner-damage / 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | independent; [docs/registry/implementation-map.yaml:L16751-L16759] |
| sn-curves-and-miner-damage / 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | independent; [docs/registry/implementation-map.yaml:L16760-L16768] |
| sn-curves-and-miner-damage / 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L16769-L16777] |
| sn-curves-and-miner-damage / 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | independent; [docs/registry/implementation-map.yaml:L16778-L16786] |
| sn-curves-and-miner-damage / 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | independent; [docs/registry/implementation-map.yaml:L16787-L16795] |
| sn-curves-and-miner-damage / 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | independent; [docs/registry/implementation-map.yaml:L16796-L16804] |
| sn-curves-and-miner-damage / 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | independent; [docs/registry/implementation-map.yaml:L16805-L16816] |
| sn-curves-and-miner-damage / 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | independent; [docs/registry/implementation-map.yaml:L16817-L16825] |
| sn-curves-and-miner-damage / 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | independent; [docs/registry/implementation-map.yaml:L16826-L16834] |
| sn-curves-and-miner-damage / 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | independent; [docs/registry/implementation-map.yaml:L16835-L16843] |
| sn-curves-and-miner-damage / 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | independent; [docs/registry/implementation-map.yaml:L16844-L16852] |
| sn-curves-and-miner-damage / 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | independent; [docs/registry/implementation-map.yaml:L16853-L16861] |
| sn-curves-and-miner-damage / 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | independent; [docs/registry/implementation-map.yaml:L16862-L16870] |
| sn-curves-and-miner-damage / 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | independent; [docs/registry/implementation-map.yaml:L16871-L16879] |
| sn-curves-and-miner-damage / 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | independent; [docs/registry/implementation-map.yaml:L16880-L16890] |
| sn-curves-and-miner-damage / 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | independent; [docs/registry/implementation-map.yaml:L16891-L16899] |
| sn-curves-and-miner-damage / 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | independent; [docs/registry/implementation-map.yaml:L16900-L16908] |
| sn-curves-and-miner-damage / 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | copy; [docs/registry/implementation-map.yaml:L16909-L16919] |
| sn-curves-and-miner-damage / 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L16920-L16930] |
| sn-curves-and-miner-damage / 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L16931-L16939] |
| sn-curves-and-miner-damage / 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | independent; [docs/registry/implementation-map.yaml:L16940-L16948] |
| sn-curves-and-miner-damage / 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L16949-L16959] |
| sn-curves-and-miner-damage / 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | independent; [docs/registry/implementation-map.yaml:L16960-L16968] |
| sn-curves-and-miner-damage / 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | caller; [docs/registry/implementation-map.yaml:L16969-L16981] |
| sn-curves-and-miner-damage / 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | independent; [docs/registry/implementation-map.yaml:L16982-L16990] |
| sn-curves-and-miner-damage / 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L16991-L16999] |
| sn-curves-and-miner-damage / 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | wrapper; [docs/registry/implementation-map.yaml:L17000-L17009] |
| sn-curves-and-miner-damage / 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | independent; [docs/registry/implementation-map.yaml:L17010-L17018] |
| sn-curves-and-miner-damage / 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | independent; [docs/registry/implementation-map.yaml:L17019-L17027] |
| sn-curves-and-miner-damage / 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | independent; [docs/registry/implementation-map.yaml:L17028-L17038] |
| sn-curves-and-miner-damage / 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent; [docs/registry/implementation-map.yaml:L17039-L17047] |
| sn-curves-and-miner-damage / 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | independent; [docs/registry/implementation-map.yaml:L17048-L17056] |
| sn-curves-and-miner-damage / 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | independent; [docs/registry/implementation-map.yaml:L17057-L17065] |
| sn-curves-and-miner-damage / 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | independent; [docs/registry/implementation-map.yaml:L17066-L17074] |
| sn-curves-and-miner-damage / 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | independent; [docs/registry/implementation-map.yaml:L17075-L17083] |
| sn-curves-and-miner-damage / 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | independent; [docs/registry/implementation-map.yaml:L17084-L17092] |
| sn-curves-and-miner-damage / 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17093-L17101] |
| sn-curves-and-miner-damage / 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L17102-L17112] |
| sn-curves-and-miner-damage / 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | independent; [docs/registry/implementation-map.yaml:L17113-L17121] |
| sn-curves-and-miner-damage / 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L17122-L17130] |
| sn-curves-and-miner-damage / 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | wrapper; [docs/registry/implementation-map.yaml:L17131-L17143] |
| sn-curves-and-miner-damage / 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | independent; [docs/registry/implementation-map.yaml:L17144-L17152] |
| sn-curves-and-miner-damage / 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | independent; [docs/registry/implementation-map.yaml:L17153-L17161] |
| sn-curves-and-miner-damage / 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | independent; [docs/registry/implementation-map.yaml:L17162-L17170] |
| sn-curves-and-miner-damage / 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | independent; [docs/registry/implementation-map.yaml:L17171-L17179] |
| sn-curves-and-miner-damage / 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | independent; [docs/registry/implementation-map.yaml:L17180-L17188] |
| sn-curves-and-miner-damage / 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | independent; [docs/registry/implementation-map.yaml:L17189-L17197] |
| sn-curves-and-miner-damage / 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | independent; [docs/registry/implementation-map.yaml:L17198-L17206] |
| sn-curves-and-miner-damage / 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | independent; [docs/registry/implementation-map.yaml:L17207-L17215] |
| sn-curves-and-miner-damage / 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17216-L17224] |
| sn-curves-and-miner-damage / 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L17225-L17233] |
| sn-curves-and-miner-damage / 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L17234-L17242] |
| sn-curves-and-miner-damage / 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L17243-L17251] |
| sn-curves-and-miner-damage / 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L17252-L17260] |
| sn-curves-and-miner-damage / 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | independent; [docs/registry/implementation-map.yaml:L17261-L17269] |
| sn-curves-and-miner-damage / 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | independent; [docs/registry/implementation-map.yaml:L17270-L17278] |
| sn-curves-and-miner-damage / 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | independent; [docs/registry/implementation-map.yaml:L17279-L17287] |
| sn-curves-and-miner-damage / 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | independent; [docs/registry/implementation-map.yaml:L17288-L17296] |
| sn-curves-and-miner-damage / 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17297-L17305] |
| sn-curves-and-miner-damage / 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | caller; [docs/registry/implementation-map.yaml:L17306-L17316] |
| sn-curves-and-miner-damage / 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | independent; [docs/registry/implementation-map.yaml:L17317-L17325] |
| sn-curves-and-miner-damage / 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | caller; [docs/registry/implementation-map.yaml:L17326-L17336] |
| sn-curves-and-miner-damage / 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | independent; [docs/registry/implementation-map.yaml:L17337-L17345] |
| sn-curves-and-miner-damage / 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | independent; [docs/registry/implementation-map.yaml:L17346-L17354] |
| sn-curves-and-miner-damage / 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L17355-L17363] |
| sn-curves-and-miner-damage / 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | independent; [docs/registry/implementation-map.yaml:L17364-L17374] |
| sn-curves-and-miner-damage / 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | independent; [docs/registry/implementation-map.yaml:L17375-L17383] |
| sn-curves-and-miner-damage / 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17384-L17392] |
| sn-curves-and-miner-damage / 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._find_sn_data_file` | independent; [docs/registry/implementation-map.yaml:L17393-L17401] |
| sn-curves-and-miner-damage / 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | independent; [docs/registry/implementation-map.yaml:L17402-L17410] |
| sn-curves-and-miner-damage / 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | independent; [docs/registry/implementation-map.yaml:L17411-L17419] |
| sn-curves-and-miner-damage / 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | independent; [docs/registry/implementation-map.yaml:L17420-L17428] |
| sn-curves-and-miner-damage / 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | independent; [docs/registry/implementation-map.yaml:L17429-L17437] |
| sn-curves-and-miner-damage / 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | independent; [docs/registry/implementation-map.yaml:L17438-L17446] |
| sn-curves-and-miner-damage / 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | independent; [docs/registry/implementation-map.yaml:L17447-L17455] |
| sn-curves-and-miner-damage / 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | independent; [docs/registry/implementation-map.yaml:L17456-L17464] |
| sn-curves-and-miner-damage / 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | independent; [docs/registry/implementation-map.yaml:L17465-L17473] |
| sn-curves-and-miner-damage / 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | independent; [docs/registry/implementation-map.yaml:L17474-L17482] |
| sn-curves-and-miner-damage / 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | independent; [docs/registry/implementation-map.yaml:L17483-L17491] |
| sn-curves-and-miner-damage / 253 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | independent; [docs/registry/implementation-map.yaml:L17492-L17500] |
| sn-curves-and-miner-damage / 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | independent; [docs/registry/implementation-map.yaml:L17501-L17509] |
| sn-curves-and-miner-damage / 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | independent; [docs/registry/implementation-map.yaml:L17510-L17518] |
| sn-curves-and-miner-damage / 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | independent; [docs/registry/implementation-map.yaml:L17519-L17527] |
| sn-curves-and-miner-damage / 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | caller; [docs/registry/implementation-map.yaml:L17528-L17539] |
| sn-curves-and-miner-damage / 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | caller; [docs/registry/implementation-map.yaml:L17540-L17551] |
| sn-curves-and-miner-damage / 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | caller; [docs/registry/implementation-map.yaml:L17552-L17563] |
| sn-curves-and-miner-damage / 260 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | independent; [docs/registry/implementation-map.yaml:L17564-L17572] |
| sn-curves-and-miner-damage / 261 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | independent; [docs/registry/implementation-map.yaml:L17573-L17583] |
| sn-curves-and-miner-damage / 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | independent; [docs/registry/implementation-map.yaml:L17584-L17592] |
| sn-curves-and-miner-damage / 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L17593-L17601] |
| sn-curves-and-miner-damage / 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L17602-L17610] |
| sn-curves-and-miner-damage / 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | independent; [docs/registry/implementation-map.yaml:L17611-L17619] |
| sn-curves-and-miner-damage / 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17620-L17628] |
| sn-curves-and-miner-damage / 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | independent; [docs/registry/implementation-map.yaml:L17629-L17637] |
| sn-curves-and-miner-damage / 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | independent; [docs/registry/implementation-map.yaml:L17638-L17646] |
| sn-curves-and-miner-damage / 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L17647-L17655] |
| sn-curves-and-miner-damage / 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L17656-L17666] |
| sn-curves-and-miner-damage / 271 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | independent; [docs/registry/implementation-map.yaml:L17667-L17675] |
| sn-curves-and-miner-damage / 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | independent; [docs/registry/implementation-map.yaml:L17676-L17684] |
| sn-curves-and-miner-damage / 273 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17685-L17693] |
| sn-curves-and-miner-damage / 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent; [docs/registry/implementation-map.yaml:L17694-L17702] |
| sn-curves-and-miner-damage / 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | independent; [docs/registry/implementation-map.yaml:L17703-L17711] |
| sn-curves-and-miner-damage / 276 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent; [docs/registry/implementation-map.yaml:L17712-L17720] |
| sn-curves-and-miner-damage / 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `(module-level)` | independent; [docs/registry/implementation-map.yaml:L17721-L17729] |
| sn-curves-and-miner-damage / 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | caller; [docs/registry/implementation-map.yaml:L17730-L17740] |
| sn-curves-and-miner-damage / 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | independent; [docs/registry/implementation-map.yaml:L17741-L17749] |
| sn-curves-and-miner-damage / 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | independent; [docs/registry/implementation-map.yaml:L17750-L17758] |
| sn-curves-and-miner-damage / 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | independent; [docs/registry/implementation-map.yaml:L17759-L17767] |
| sn-curves-and-miner-damage / 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | independent; [docs/registry/implementation-map.yaml:L17768-L17776] |
| sn-curves-and-miner-damage / 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | independent; [docs/registry/implementation-map.yaml:L17777-L17787] |
| sn-curves-and-miner-damage / 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | independent; [docs/registry/implementation-map.yaml:L17788-L17796] |
| sn-curves-and-miner-damage / 285 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | independent; [docs/registry/implementation-map.yaml:L17797-L17805] |
| sn-curves-and-miner-damage / 286 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent; [docs/registry/implementation-map.yaml:L17806-L17814] |
| sn-curves-and-miner-damage / 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | independent; [docs/registry/implementation-map.yaml:L17815-L17823] |
| sn-curves-and-miner-damage / 288 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | independent; [docs/registry/implementation-map.yaml:L17824-L17832] |
| sn-curves-and-miner-damage / 289 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | independent; [docs/registry/implementation-map.yaml:L17833-L17841] |
| sn-curves-and-miner-damage / 290 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | independent; [docs/registry/implementation-map.yaml:L17842-L17850] |
| sn-curves-and-miner-damage / 291 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | caller; [docs/registry/implementation-map.yaml:L17851-L17861] |
| sn-curves-and-miner-damage / 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | independent; [docs/registry/implementation-map.yaml:L17862-L17870] |
| sn-curves-and-miner-damage / 293 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | independent; [docs/registry/implementation-map.yaml:L17871-L17881] |
| sn-curves-and-miner-damage / 294 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | caller; [docs/registry/implementation-map.yaml:L17882-L17892] |
| sn-curves-and-miner-damage / 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L17893-L17901] |
| sn-curves-and-miner-damage / 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | independent; [docs/registry/implementation-map.yaml:L17902-L17910] |
| sn-curves-and-miner-damage / 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | independent; [docs/registry/implementation-map.yaml:L17911-L17922] |
| sn-curves-and-miner-damage / 298 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | caller; [docs/registry/implementation-map.yaml:L17923-L17933] |
| sn-curves-and-miner-damage / 299 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | independent; [docs/registry/implementation-map.yaml:L17934-L17942] |
| sn-curves-and-miner-damage / 300 | `src/digitalmodel/viv_screening/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L17943-L17955] |
| sn-curves-and-miner-damage / 301 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | independent; [docs/registry/implementation-map.yaml:L17956-L17965] |
| sn-curves-and-miner-damage / 302 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | independent; [docs/registry/implementation-map.yaml:L17966-L17974] |
| sn-curves-and-miner-damage / 303 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | independent; [docs/registry/implementation-map.yaml:L17975-L17983] |
| sn-curves-and-miner-damage / 304 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | test_helper; [docs/registry/implementation-map.yaml:L17984-L17992] |
| sn-curves-and-miner-damage / 305 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | test_helper; [docs/registry/implementation-map.yaml:L17993-L18003] |
| sn-curves-and-miner-damage / 306 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | test_helper; [docs/registry/implementation-map.yaml:L18004-L18012] |
| sn-curves-and-miner-damage / 307 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | test_helper; [docs/registry/implementation-map.yaml:L18013-L18021] |
| sn-curves-and-miner-damage / 308 | `tests/fatigue/test_assessment.py` | `histogram_input` | test_helper; [docs/registry/implementation-map.yaml:L18022-L18032] |
| sn-curves-and-miner-damage / 309 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | test_helper; [docs/registry/implementation-map.yaml:L18033-L18043] |
| sn-curves-and-miner-damage / 310 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | test_helper; [docs/registry/implementation-map.yaml:L18044-L18052] |
| sn-curves-and-miner-damage / 311 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | test_helper; [docs/registry/implementation-map.yaml:L18053-L18065] |
| sn-curves-and-miner-damage / 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | test_helper; [docs/registry/implementation-map.yaml:L18066-L18074] |
| sn-curves-and-miner-damage / 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | test_helper; [docs/registry/implementation-map.yaml:L18075-L18083] |
| sn-curves-and-miner-damage / 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | test_helper; [docs/registry/implementation-map.yaml:L18084-L18094] |
| sn-curves-and-miner-damage / 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | test_helper; [docs/registry/implementation-map.yaml:L18095-L18103] |
| sn-curves-and-miner-damage / 316 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | test_helper; [docs/registry/implementation-map.yaml:L18104-L18112] |
| sn-curves-and-miner-damage / 317 | `tests/fatigue/test_damage.py` | `f_curve` | test_helper; [docs/registry/implementation-map.yaml:L18113-L18122] |
| sn-curves-and-miner-damage / 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | test_helper; [docs/registry/implementation-map.yaml:L18123-L18131] |
| sn-curves-and-miner-damage / 319 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | test_helper; [docs/registry/implementation-map.yaml:L18132-L18140] |
| sn-curves-and-miner-damage / 320 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | test_helper; [docs/registry/implementation-map.yaml:L18141-L18151] |
| sn-curves-and-miner-damage / 321 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | test_helper; [docs/registry/implementation-map.yaml:L18152-L18160] |
| sn-curves-and-miner-damage / 322 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | test_helper; [docs/registry/implementation-map.yaml:L18161-L18170] |
| sn-curves-and-miner-damage / 323 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | test_helper; [docs/registry/implementation-map.yaml:L18171-L18179] |
| sn-curves-and-miner-damage / 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | test_helper; [docs/registry/implementation-map.yaml:L18180-L18191] |
| sn-curves-and-miner-damage / 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | test_helper; [docs/registry/implementation-map.yaml:L18192-L18200] |
| sn-curves-and-miner-damage / 326 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | test_helper; [docs/registry/implementation-map.yaml:L18201-L18209] |
| sn-curves-and-miner-damage / 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L18210-L18219] |
| sn-curves-and-miner-damage / 328 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L18220-L18229] |
| sn-curves-and-miner-damage / 329 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | test_helper; [docs/registry/implementation-map.yaml:L18230-L18238] |
| sn-curves-and-miner-damage / 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | test_helper; [docs/registry/implementation-map.yaml:L18239-L18247] |
| sn-curves-and-miner-damage / 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | test_helper; [docs/registry/implementation-map.yaml:L18248-L18256] |
| sn-curves-and-miner-damage / 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L18257-L18265] |
| sn-curves-and-miner-damage / 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | test_helper; [docs/registry/implementation-map.yaml:L18266-L18274] |
| sn-curves-and-miner-damage / 334 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | test_helper; [docs/registry/implementation-map.yaml:L18275-L18283] |
| sn-curves-and-miner-damage / 335 | `tests/parametric/test_atlas.py` | `_small_atlas` | test_helper; [docs/registry/implementation-map.yaml:L18284-L18292] |
| sn-curves-and-miner-damage / 336 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | test_helper; [docs/registry/implementation-map.yaml:L18293-L18304] |
| sn-curves-and-miner-damage / 337 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | test_helper; [docs/registry/implementation-map.yaml:L18305-L18316] |
| sn-curves-and-miner-damage / 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `(module-level)` | test_helper; [docs/registry/implementation-map.yaml:L18317-L18325] |
| sn-curves-and-miner-damage / 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | test_helper; [docs/registry/implementation-map.yaml:L18326-L18334] |
| sn-curves-and-miner-damage / 340 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L18335-L18345] |
| sn-curves-and-miner-damage / 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | test_helper; [docs/registry/implementation-map.yaml:L18346-L18354] |
| sn-curves-and-miner-damage / 342 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | test_helper; [docs/registry/implementation-map.yaml:L18355-L18363] |
| sn-curves-and-miner-damage / 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | test_helper; [docs/registry/implementation-map.yaml:L18364-L18372] |
| sn-curves-and-miner-damage / 344 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | test_helper; [docs/registry/implementation-map.yaml:L18373-L18383] |
| sn-curves-and-miner-damage / 345 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L18384-L18397] |
| sn-curves-and-miner-damage / 346 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | test_helper; [docs/registry/implementation-map.yaml:L18398-L18406] |
| sn-curves-and-miner-damage / 347 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L18407-L18415] |
| sn-curves-and-miner-damage / 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | test_helper; [docs/registry/implementation-map.yaml:L18416-L18424] |
| sn-curves-and-miner-damage / 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | test_helper; [docs/registry/implementation-map.yaml:L18425-L18433] |
| sn-curves-and-miner-damage / 350 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | test_helper; [docs/registry/implementation-map.yaml:L18434-L18442] |
| sn-curves-and-miner-damage / 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | test_helper; [docs/registry/implementation-map.yaml:L18443-L18451] |
| sn-curves-and-miner-damage / 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | test_helper; [docs/registry/implementation-map.yaml:L18452-L18460] |
| sn-curves-and-miner-damage / 353 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | test_helper; [docs/registry/implementation-map.yaml:L18461-L18469] |
| sn-curves-and-miner-damage / 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | test_helper; [docs/registry/implementation-map.yaml:L18470-L18480] |
| sn-curves-and-miner-damage / 355 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | test_helper; [docs/registry/implementation-map.yaml:L18481-L18489] |
| sn-curves-and-miner-damage / 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | test_helper; [docs/registry/implementation-map.yaml:L18490-L18498] |
| sn-curves-and-miner-damage / 357 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | test_helper; [docs/registry/implementation-map.yaml:L18499-L18509] |
| sn-curves-and-miner-damage / 358 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L18510-L18518] |
| sn-curves-and-miner-damage / 359 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | test_helper; [docs/registry/implementation-map.yaml:L18519-L18527] |
| sn-curves-and-miner-damage / 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | test_helper; [docs/registry/implementation-map.yaml:L18528-L18538] |
| sn-curves-and-miner-damage / 361 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | test_helper; [docs/registry/implementation-map.yaml:L18539-L18549] |
| sn-curves-and-miner-damage / 362 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | test_helper; [docs/registry/implementation-map.yaml:L18550-L18558] |
| sn-curves-and-miner-damage / 363 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | test_helper; [docs/registry/implementation-map.yaml:L18559-L18567] |
| sn-curves-and-miner-damage / 364 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | test_helper; [docs/registry/implementation-map.yaml:L18568-L18576] |
| sn-curves-and-miner-damage / 365 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | test_helper; [docs/registry/implementation-map.yaml:L18577-L18585] |
| sn-curves-and-miner-damage / 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | test_helper; [docs/registry/implementation-map.yaml:L18586-L18594] |
| sn-curves-and-miner-damage / 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L18595-L18603] |
| sn-curves-and-miner-damage / 368 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | test_helper; [docs/registry/implementation-map.yaml:L18604-L18612] |
| sn-curves-and-miner-damage / 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | test_helper; [docs/registry/implementation-map.yaml:L18613-L18621] |
| sn-curves-and-miner-damage / 370 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | test_helper; [docs/registry/implementation-map.yaml:L18622-L18630] |
| sn-curves-and-miner-damage / 371 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | test_helper; [docs/registry/implementation-map.yaml:L18631-L18639] |
| sn-curves-and-miner-damage / 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | test_helper; [docs/registry/implementation-map.yaml:L18640-L18649] |
| sn-curves-and-miner-damage / 373 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | test_helper; [docs/registry/implementation-map.yaml:L18650-L18659] |
| sn-curves-and-miner-damage / 374 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | test_helper; [docs/registry/implementation-map.yaml:L18660-L18668] |
| sn-curves-and-miner-damage / 375 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | test_helper; [docs/registry/implementation-map.yaml:L18669-L18677] |
| sn-curves-and-miner-damage / 376 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | test_helper; [docs/registry/implementation-map.yaml:L18678-L18686] |
| sn-curves-and-miner-damage / 377 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | test_helper; [docs/registry/implementation-map.yaml:L18687-L18695] |
| sn-curves-and-miner-damage / 378 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | test_helper; [docs/registry/implementation-map.yaml:L18696-L18704] |
| sn-curves-and-miner-damage / 379 | `tests/test_complete_fatigue_pipeline.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L18705-L18715] |
| sn-curves-and-miner-damage / 380 | `tests/test_fatigue_basic.py` | `run_all_tests` | test_helper; [docs/registry/implementation-map.yaml:L18716-L18724] |
| sn-curves-and-miner-damage / 381 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper; [docs/registry/implementation-map.yaml:L18725-L18733] |
| sn-curves-and-miner-damage / 382 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L18734-L18743] |
| sn-curves-and-miner-damage / 383 | `tests/test_reservoir_framework.py` | `run_performance_tests` | test_helper; [docs/registry/implementation-map.yaml:L18744-L18752] |
| sn-curves-and-miner-damage / 384 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | test_helper; [docs/registry/implementation-map.yaml:L18753-L18763] |
| sn-curves-and-miner-damage / 385 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper; [docs/registry/implementation-map.yaml:L18764-L18773] |
| sn-curves-and-miner-damage / 386 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L18774-L18783] |
| sn-curves-and-miner-damage / 387 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper; [docs/registry/implementation-map.yaml:L18784-L18793] |

## 10. Coverage and open items

The retained test selection covers assessment/export, crack growth, environmental correction, FATG, hotspot, non-critical-plane multiaxial criteria, SCFs, weld classification and package imports. Shared-method suites are referenced to #2552; that pack does not yet record the two S-N workflow adapter nodes or the compatibility smoke-test evidence (F12), so O11 tracks those gaps rather than claiming a completed handover. The prior runs used BASE `5d32a6b7c9985abf7db8d140edd1eaf7ca1bc865` and AFTER the documentation tree committed as `4f152d7beba2f33cefe868c61dfd2cd4fec4cb97`. This revision reruns at BASE `4f152d7beba2f33cefe868c61dfd2cd4fec4cb97` and its uncommitted documentation tree; module source and fatigue test bytes equal the unchanged code pin. [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902]

| Run group | Generic command form / paths | BASE: passed / failed / skipped / expected failures / NOT_RUN | AFTER: passed / failed / skipped / expected failures / NOT_RUN |
| --- | --- | --- | --- |
| Retained scope | `PYTHONPATH=src uv run --no-sources python -m pytest tests/fatigue -q -rA -p no:cacheprovider --basetemp=<scratch>`; counts below are the retained subset of this command, not the shared-family results | 176 / 0 / 0 / 0 / 0 | 176 / 0 / 0 / 0 / 0 |
| Contracts domain | Same form; full contracts-domain paths from `tests/DOMAINS.md` | 918 / 2 / 61 / 0 / 0 | 918 / 2 / 61 / 0 / 0 |

The contracts failures are baseline: `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` fails the xlsxwriter call-site discovery; `tests/test_no_corrupted_py_files.py::test_all_src_py_files_parse` rejects 13 unchanged source files under Python 3.12 with warnings treated as errors. The test/scanner and rejected files are byte-identical on `origin/main` as read for this revision. The 61 skips comprise 59 stale-layout exclusions, one missing `digitalmodel.catenary` compatibility module and one collection-time API STD 2RD migration skip. [tests/contracts/test_string_addressed_dependencies.py:L620-L643] [tests/test_no_corrupted_py_files.py:L14-L24] [pytest.ini:L68-L71]

Retained selection: `test_assessment.py`, `test_crack_growth.py`, `test_environmental_correction.py`, `test_fatg.py`, `test_fatg_workflow.py`, `test_hotspot_stress.py`, `test_scf_library.py`, `test_weld_classification.py`, `test_package_imports.py` and every `test_multiaxial_fatigue.py` node except `TestFindleyCriticalPlane`; all paths are under `tests/fatigue/`. The section-4 assessment cases remain regression examples; no independently sourced assessment-level validation is claimed, O2.

Open items below are assigned to the maintainer for issue disposition; draft numbers remain issue-pending markers until issue creation.

| Item | Coverage / qualification still open | Tracking |
| --- | --- | --- |
| O1 | UNVERIFIED: shared S-N standards qualification is referenced to #2552; licensed clauses and individual catalogue sources are not independently checked by this pack | #1588; [src/digitalmodel/fatigue/c203_editions.py:L19-L37] [src/digitalmodel/fatigue/sn_library.py:L32-L50] |
| O2 | UNVERIFIED: independent qualification of the retained SCF, environment, weld-classification, multiaxial, FATG and crack-growth methods across their intended engineering ranges; passing regression tests are insufficient | #1843, #1719, #1148; issue pending (5) for missing independent benchmarks; [src/digitalmodel/fatigue/scf_library.py:L108-L126] [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L64-L88] |
| O3 | UNVERIFIED: named deputy assignment, independent clearance and shared-log registration; the ownership table states proposed roles only | issue pending (5) |
| O4 | All four spectral damage routes understate Hz-consistent rate/annual damage by a factor of 2π (6.3), including the cross-package disagreement; duration documentation needs reconciliation | #2583; section 6 F1 |
| O5 | Unknown weld input receives an unsupported base-material classification | issue pending (2); section 6 F2 |
| O6 | Environment slope-change metadata is not transferred to the returned curve | issue pending (3); section 6 F3 |
| O7 | Amendment labels and per-builder documentation counts require reconciliation | issue pending (4); section 3 and section 6 F9 |
| O8 | Map classification, omitted methods and repository-wide discovery gaps require map updates; this does not extend the counted-family grep | issue pending (6); section 9 |
| O9 | Historical FATG executable output, ancillary independent worked examples, and full downstream-consumer qualification are not established by this pack | issue pending (5); [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L64-L88] |
| O10 | Discovery documents below include historical claims and unrelated module content; those claims are not adopted as engineering evidence; broader document reconciliation is deferred | issue pending (5) |
| O11 | Shared-family implementations D1–D8 require consolidation (EC-12). In #2552, reconcile section-1 boundary rows for native C203 records, catalogue/pyLife, Miner, retained-residual counting and spectral functions; replace 65 in-package inventory covering-pack `none` marks; split the offline record to identify the 176 retained tests covered here. Add native spectral/catalogue method descriptions there or explicit references to this pack for them. Place the two S-N workflow adapter nodes and retired EC-04 row-count evidence explicitly; compatibility smoke-test false passes need an assertion-reaching route (F12) | issue pending (7); [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L20-L30] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L295-L295] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L471-L534] [src/digitalmodel/structural/fatigue/docs/agent-pack.md:L897-L902] [tests/test_fatigue_basic.py:L21-L54] [tests/test_fatigue_basic.py:L98-L133] [tests/test_fatigue_basic.py:L178-L220] |
| O12 | Discovery includes 2 documentation files with identifying names; replace them with documented codenames before listing their paths | issue pending (8) |

### File coverage

All 24 package Python files were inspected at the pin; shared implementations are inventory entries with #2552 as their method reference, and unique-method qualification limits are above.

- `src/digitalmodel/fatigue/__init__.py` [src/digitalmodel/fatigue/__init__.py:L1-L5]
- `src/digitalmodel/fatigue/assessment.py` [src/digitalmodel/fatigue/assessment.py:L1-L5]
- `src/digitalmodel/fatigue/assessment_export.py` [src/digitalmodel/fatigue/assessment_export.py:L1-L5]
- `src/digitalmodel/fatigue/c203_editions.py` [src/digitalmodel/fatigue/c203_editions.py:L1-L5]
- `src/digitalmodel/fatigue/c203_sn_tables.py` [src/digitalmodel/fatigue/c203_sn_tables.py:L1-L5]
- `src/digitalmodel/fatigue/counting_contract.py` [src/digitalmodel/fatigue/counting_contract.py:L1-L5]
- `src/digitalmodel/fatigue/crack_growth.py` [src/digitalmodel/fatigue/crack_growth.py:L1-L5]
- `src/digitalmodel/fatigue/damage.py` [src/digitalmodel/fatigue/damage.py:L1-L5]
- `src/digitalmodel/fatigue/environmental_correction.py` [src/digitalmodel/fatigue/environmental_correction.py:L1-L5]
- `src/digitalmodel/fatigue/fatg.py` [src/digitalmodel/fatigue/fatg.py:L1-L5]
- `src/digitalmodel/fatigue/fatigue_reporting.py` [src/digitalmodel/fatigue/fatigue_reporting.py:L1-L5]
- `src/digitalmodel/fatigue/hotspot_stress.py` [src/digitalmodel/fatigue/hotspot_stress.py:L1-L5]
- `src/digitalmodel/fatigue/multiaxial_fatigue.py` [src/digitalmodel/fatigue/multiaxial_fatigue.py:L1-L5]
- `src/digitalmodel/fatigue/plotting.py` [src/digitalmodel/fatigue/plotting.py:L1-L5]
- `src/digitalmodel/fatigue/rainflow.py` [src/digitalmodel/fatigue/rainflow.py:L1-L5]
- `src/digitalmodel/fatigue/scf_library.py` [src/digitalmodel/fatigue/scf_library.py:L1-L5]
- `src/digitalmodel/fatigue/sn_curves.py` [src/digitalmodel/fatigue/sn_curves.py:L1-L5]
- `src/digitalmodel/fatigue/sn_library.py` [src/digitalmodel/fatigue/sn_library.py:L1-L5]
- `src/digitalmodel/fatigue/sn_library_api.py` [src/digitalmodel/fatigue/sn_library_api.py:L1-L5]
- `src/digitalmodel/fatigue/spectral_fatigue.py` [src/digitalmodel/fatigue/spectral_fatigue.py:L1-L5]
- `src/digitalmodel/fatigue/weld_classification.py` [src/digitalmodel/fatigue/weld_classification.py:L1-L5]
- `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L1-L5]
- `src/digitalmodel/fatigue/woehler_fitting.py` [src/digitalmodel/fatigue/woehler_fitting.py:L1-L5]
- `src/digitalmodel/fatigue/workflow.py` [src/digitalmodel/fatigue/workflow.py:L1-L5]

The following native test files were inspected; only the retained selection is recorded here. Shared-family tests are referenced to #2552. Its offline selection does not yet include the compatibility group or the two durable S-N workflow adapter nodes; O11 records the evidence gap. F12 records `test_basic_sn_curve`, `test_damage_accumulation` and `test_time_domain_simple`, whose caught import failures return `False` without exercising their assertions; their nominal passes are not engineering evidence, O11. [tests/test_fatigue_basic.py:L21-L54] [tests/test_fatigue_basic.py:L98-L133] [tests/test_fatigue_basic.py:L178-L220]

- `tests/fatigue/test_assessment.py` [tests/fatigue/test_assessment.py:L1-L5]
- `tests/fatigue/test_c203_citations.py` [tests/fatigue/test_c203_citations.py:L1-L5]
- `tests/fatigue/test_c203_consumers.py` [tests/fatigue/test_c203_consumers.py:L1-L5]
- `tests/fatigue/test_c203_sn_values.py` [tests/fatigue/test_c203_sn_values.py:L1-L5]
- `tests/fatigue/test_c203_variable_amplitude.py` [tests/fatigue/test_c203_variable_amplitude.py:L1-L5]
- `tests/fatigue/test_crack_growth.py` [tests/fatigue/test_crack_growth.py:L1-L5]
- `tests/fatigue/test_damage.py` [tests/fatigue/test_damage.py:L1-L5]
- `tests/fatigue/test_environmental_correction.py` [tests/fatigue/test_environmental_correction.py:L1-L5]
- `tests/fatigue/test_fatg.py` [tests/fatigue/test_fatg.py:L1-L5]
- `tests/fatigue/test_fatg_workflow.py` [tests/fatigue/test_fatg_workflow.py:L1-L5]
- `tests/fatigue/test_fatigue_reporting.py` [tests/fatigue/test_fatigue_reporting.py:L1-L5]
- `tests/fatigue/test_hotspot_stress.py` [tests/fatigue/test_hotspot_stress.py:L1-L5]
- `tests/fatigue/test_multiaxial_fatigue.py` [tests/fatigue/test_multiaxial_fatigue.py:L1-L5]
- `tests/fatigue/test_package_imports.py` [tests/fatigue/test_package_imports.py:L1-L5]
- `tests/fatigue/test_rainflow.py` [tests/fatigue/test_rainflow.py:L1-L5]
- `tests/fatigue/test_rainflow_invariants.py` [tests/fatigue/test_rainflow_invariants.py:L1-L5]
- `tests/fatigue/test_scf_library.py` [tests/fatigue/test_scf_library.py:L1-L5]
- `tests/fatigue/test_sn_curve_workflow.py` [tests/fatigue/test_sn_curve_workflow.py:L1-L5]
- `tests/fatigue/test_sn_curves.py` [tests/fatigue/test_sn_curves.py:L1-L5]
- `tests/fatigue/test_sn_library.py` [tests/fatigue/test_sn_library.py:L1-L5]
- `tests/fatigue/test_sn_library_api.py` [tests/fatigue/test_sn_library_api.py:L1-L5]
- `tests/fatigue/test_spectral_fatigue.py` [tests/fatigue/test_spectral_fatigue.py:L1-L5]
- `tests/fatigue/test_weld_classification.py` [tests/fatigue/test_weld_classification.py:L1-L5]
- `tests/fatigue/test_weld_fatigue_quickcheck.py` [tests/fatigue/test_weld_fatigue_quickcheck.py:L1-L5]
- `tests/fatigue/test_woehler_fitting.py` [tests/fatigue/test_woehler_fitting.py:L1-L5]

Documentation discovery searched for explicit package paths/imports and the two durable workflow IDs, plus README references to fatigue; the following files were discovered, with module-relevant passages inspected. Discovery alone does not validate historical maturity, dataset counts or standards claims.

- `docs/capability-map/capabilities-sections.yml` [docs/capability-map/capabilities-sections.yml:L1-L5]
- `docs/domains/efthymiou-scf-correction-2026-06-28.md` [docs/domains/efthymiou-scf-correction-2026-06-28.md:L1-L5]
- `docs/domains/efthymiou-short-chord-f1-2026-06-29.md` [docs/domains/efthymiou-short-chord-f1-2026-06-29.md:L1-L5]
- `docs/domains/fatigue/PHASE4_FATIGUE_FINDINGS.md` [docs/domains/fatigue/PHASE4_FATIGUE_FINDINGS.md:L1-L5]
- `docs/domains/fatigue/fatg-legacy-spectral-fatigue.md` [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L1-L5]
- `docs/domains/fatigue/fatigue_migration_summary.md` [docs/domains/fatigue/fatigue_migration_summary.md:L1-L5]
- `docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md` [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L1-L5]
- `docs/domains/mooring/screening/README.md` [docs/domains/mooring/screening/README.md:L1-L5]
- `docs/domains/mooring/screening/README.template.md` [docs/domains/mooring/screening/README.template.md:L1-L5]
- `docs/domains/offshore-engineering/README.md` [docs/domains/offshore-engineering/README.md:L1-L5]
- `docs/domains/orcaflex/examples/README.md` [docs/domains/orcaflex/examples/README.md:L1-L5]
- `docs/domains/orcaflex/library/model_library/frps_ssr_global_riser/README.md` [docs/domains/orcaflex/library/model_library/frps_ssr_global_riser/README.md:L1-L5]
- `docs/domains/orcaflex/templates/mooring_systems/salm_hybrid/README.md` [docs/domains/orcaflex/templates/mooring_systems/salm_hybrid/README.md:L1-L5]
- `docs/domains/orcaflex/templates/mooring_systems/spread_mooring_hybrid/README.md` [docs/domains/orcaflex/templates/mooring_systems/spread_mooring_hybrid/README.md:L1-L5]
- `docs/domains/orcaflex/templates/platforms/tlp_hybrid/README.md` [docs/domains/orcaflex/templates/platforms/tlp_hybrid/README.md:L1-L5]
- `docs/domains/orcaflex/templates/umbilicals/umbilical_hybrid/README.md` [docs/domains/orcaflex/templates/umbilicals/umbilical_hybrid/README.md:L1-L5]
- `docs/domains/project-docs/phase1-api-reference.md` [docs/domains/project-docs/phase1-api-reference.md:L1-L5]
- `docs/domains/project-docs/phase1-implementation-report.md` [docs/domains/project-docs/phase1-implementation-report.md:L1-L5]
- `docs/domains/structural/fatigue_analysis/README.md` [docs/domains/structural/fatigue_analysis/README.md:L1-L5]
- `docs/domains/test_automation/README.md` [docs/domains/test_automation/README.md:L1-L5]
- `docs/maps/digitalmodel-operator-map.md` [docs/maps/digitalmodel-operator-map.md:L1-L5]
- `docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md` [docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md:L1-L5]
- `docs/plans/evidence/2026-08-10-issue-1983-triage.md` [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L1-L5]
- `docs/registry/module-routing.yaml` [docs/registry/module-routing.yaml:L1-L5]
- `docs/registry/workflows.yaml` [docs/registry/workflows.yaml:L1-L5]
- `docs/skills/fatigue-assessment/SKILL.md` [docs/skills/fatigue-assessment/SKILL.md:L1-L5]
