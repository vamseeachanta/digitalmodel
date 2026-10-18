# BEMRosetta module knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/bemrosetta |
| Repository | digitalmodel |
| Path | `src/digitalmodel/hydrodynamics/bemrosetta` |
| Pack revision | 0 |
| Code revision | `7d15df7734aaa1e3e74035789661abd37d866597` |
| Analysis family | diffraction; DF |
| Quality role | diffraction quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The native path reads AQWA listing and QTF text, converts GDF/DAT/STL meshes, and exports coefficient tables; the Python converter delegates its main export to the diffraction package. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L43-L96] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L243-L297] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L144]

The CLI `convert` writes added-mass, damping and RAO CSV files, plus optional QTF CSV; it does not select a different writer when `--output-format` changes and explicitly skips coefficient validation for the raw parse result. [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L25-L120]

The routing registry describes the containing hydrodynamics domain as production; it does not supply a separate maturity statement for this submodule at the pin. [docs/registry/module-routing.yaml:L155-L177]

`AQWAParser` advertises only `.lis`/`.LIS`; `DATHandler` reads embedded AQWA deck geometry but writes NEMOH-style mesh data, rather than a complete AQWA analysis deck. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L57-L65] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L46-L81] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L350-L391]

UNVERIFIED: acceptance of generated files by a named target solver version and physical accuracy of converted hydrodynamic responses remain unestablished; see section 10, items O1 and O2.

## 2. Inputs and outputs

### Inputs

| Input | Units and range | Default and source | Evidence |
| --- | --- | --- | --- |
| AQWA listing | Frequencies rad/s, periods s; extraction admits `0.01 < frequency < 100`, `0.1 < period < 1000`; headings filtered to −180 through 360 degrees | File required; missing sections may yield empty collections | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L190-L266] |
| Metadata | Depth m; density kg/m³; gravity m/s²; default units m, kg, s, N, N.m, deg, rad/s | Depth infinity, density 1025.0, gravity 9.80665; counts zero, one body, reference point `[0,0,0]`; code defaults, not an approved design basis | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L49-L89] |
| QTF | Two frequency axes rad/s, heading degrees, component phase radians; DOF indices 0–5 in the model | Difference-frequency and heading 0.0 degrees if headers are absent; one missing axis is copied from the other | [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L24-L60] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L110-L159] |
| PanelMesh | Vertices N×3, panels M×3 or M×4; negative indices filtered out by derived-property calculations | Normals, areas and centres computed if absent; reference point `[0,0,0]` | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L27-L61] [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L102-L173] |
| DiffractionResults | Separate typed input to converter and validators; raw AQWAParseResult has dictionaries instead | Output directory defaults to current directory; no implicit adapter in this converter | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L24-L31] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L70-L105] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L22-L42] |
| Mesh files | GDF header unit length and gravity ignored on read; DAT/STL coordinates consumed without unit conversion | GDF symmetry absent fields default zero; DAT uses one-based indices, −1 marks absent vertices internally | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L87-L118] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L147-L180] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L120-L157] |

### Outputs and conventions

| Output | Content and units | Limit | Evidence |
| --- | --- | --- | --- |
| CLI coefficient CSV | rad/s frequency column; 36 matrix entries, or six amplitude/phase pairs and heading degrees | Numerical entries copied without a matrix-unit or RAO-unit conversion | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L123-L210] |
| CLI QTF CSV | Two frequencies rad/s, heading degrees, one-based DOF, amplitude and phase radians | Amplitude unit is not declared by the QTF model | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L213-L237] [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L24-L43] |
| Python export | Main YAML and RAO/added-mass/damping CSV delegated to OrcaFlexExporter | Separate `convert_to_vessel_yaml` writes a small dictionary with zero RAO origin and `AQWA` phase convention | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L206] |
| Python QTF CSV | Six real/imaginary DOF pairs; amplitude·cos(phase), amplitude·sin(phase) | Uses the single container frequency axis for both dimensions; missing components become zeros | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L245-L281] |
| GDF/DAT/STL | GDF/DAT coordinates rounded to six decimal places; STL ASCII scientific notation or native binary float layout | GDF writer sets unit length 1.0 and gravity 9.81; STL splits quads into triangles | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L200-L230] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L364-L389] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L239-L325] |
| ValidationReport / MeshQualityReport / ConversionResult | Errors, warnings, metrics / geometric metrics / status and output paths, duration s | Warnings alone leave ValidationReport valid; conversion wrapper catches exceptions and returns FAILED | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L188-L221] [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L230-L281] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L304-L319] |

UNVERIFIED: source-axis orientation, origin consistency, wave-heading sense and RAO phase sign are not established by the native parsing/export path; SI metadata alone does not establish the units of every matrix coupling or QTF amplitude; see O3. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L357-L387] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L191-L205]

## 3. Method and governing standards

| Method / implementing definitions | Actual operation and applicability limit | Evidence |
| --- | --- | --- |
| `AQWAParser.parse`, `_parse_metadata`, `_extract_frequencies_and_periods`, `_extract_headings`, `_extract_added_mass`, `_extract_damping`, `_extract_raos` | Regex extraction; fixed character windows; frequency/heading filtering; displacement RAO sections excluding velocity/acceleration markers; no completeness reconciliation | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L144-L266] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L268-L355] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L389-L515] |
| `AQWAParser._build_symmetric_matrix` | Six diagonal and six paired off-diagonal coefficients populate a zero-initialised 6×6 matrix; symmetry is constructed, not independently verified | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L357-L387] |
| `QTFParser._parse_qtf_content`, `_extract_frequencies`, `_extract_generic_frequencies`, `_extract_headings`, `_parse_components`, `_parse_matrix`, `_parse_simple_format` | REAL/IMAG blocks become complex magnitudes/phases; extra matrix numbers truncated, short blocks can fall back to zero placeholders | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L98-L159] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L185-L411] |
| `QTFComponent.complex_qtf`, `get_mean_drift`; `QTFData.get_component`, `get_mean_drift_forces` | Reconstruct amplitude·exp(i·phase); diagonal amplitude is labelled mean drift only for difference QTF; heading lookup uses closeness at 0.1 degrees | [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L62-L78] [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L124-L154] |
| `PanelMesh._compute_normals`, `_compute_areas`, `_compute_centers`, `translate`, `scale`, `flip_normals` | Cross-product normal; sum of triangle areas for quads; arithmetic vertex centroid; area scales with factor squared; flipping stored normals does not reverse connectivity | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L102-L201] |
| `GDFHandler._parse_gdf`, `write`; `DATHandler._parse_dat`, `_parse_aqwa_deck`, `_parse_vertex_line`, `write`; `STLHandler._is_ascii_stl`, `_read_ascii_stl`, `_read_binary_stl`, `_write_ascii_stl`, `_write_binary_stl`; `convert_mesh` | Geometry readers/writers, vertex deduplication, index remapping and triangle fans; none implements irregular-frequency removal | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L59-L230] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L83-L391] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L77-L325] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L243-L297] |
| `BaseMeshHandler.validate_mesh`, `_count_degenerate_panels`, `_count_duplicate_vertices`, `_calculate_max_aspect_ratio`, `_check_normal_consistency`, `_calculate_quality_score` | Area threshold, KDTree or pairwise duplicate count, edge ratio, centroid-normal heuristic and penalised score; these are screening metrics, not a closed-volume hydrostatics calculation | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L50-L240] |
| `CoefficientValidator.validate`, `_validate_added_mass`, `_validate_damping`, `_validate_raos`, `validate_coefficients`; CLI `validate` | Symmetry uses allclose; diagonal signs, finite values and RAO ranges checked; diagonal checks do not compute all eigenvalues | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L56-L245] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L292-L389] |
| `CausalityChecker._check_kramers_kronig`, `validate`, `check_causality`; CLI `_check_kramers_kronig` | Simplified cumulative trapezoid of B/ω with a small frequency floor, compared against variation from the last added-mass value; class checks 36 pairs, CLI six diagonal pairs; this is not a validated principal-value dispersion integral | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L50-L89] [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L103-L185] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L392-L434] |
| `OrcaFlexConverter._build_vessel_yaml`, `convert_to_vessel_yaml`, `_export_qtf`, `_write_qtf_data`; CLI `_export_added_mass_csv`, `_export_damping_csv`, `_export_raos_csv`, `_export_qtf_csv` | Dictionary construction and serialisation of supplied data; the main converter's `_call_orcaflex_exporter` wraps the diffraction exporter | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L281] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L123-L237] |

No governing engineering standard or edition is cited by the implemented parser, mesh or validator algorithms above; the documented Kramers–Kronig description is a code-level heuristic rather than an independently cited validation reference. [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L103-L172] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L50-L240] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L357-L387]

AQG r0 supplies workflow gates, not a coefficient formula; no licensed standard text or tabulated value is reproduced here. (AQG r0, sections 3, 8, 10.)

## 4. Validated worked examples

Coordinate unit `u` is retained for synthetic meshes; assigning metres requires an explicit input basis. The expected areas below are closed forms of the fixture rectangles, independent of the module's triangulation implementation. [tests/hydrodynamics/bemrosetta/test_mesh.py:L49-L58] [tests/hydrodynamics/bemrosetta/test_models.py:L410-L420]

| ID / kind | Source and input | Expected value / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 / validated geometry | Unit square: side differences 1 u × 1 u; inline fixture | Total, min, max, mean area 1 u²; relative tolerance 1e-6 specified by test | `tests/hydrodynamics/bemrosetta/test_mesh.py::TestBaseMeshHandler::test_validate_mesh_calculates_areas` | PASS, BASE and AFTER; [tests/hydrodynamics/bemrosetta/test_mesh.py:L49-L58] [tests/hydrodynamics/bemrosetta/test_mesh.py:L135-L141] |
| V2 / validated geometry | Open unit cube: five unit faces; shared `sample_panel_mesh` | 5 × (1 u × 1 u) = 5 u²; relative tolerance 0.01 specified by test | `tests/hydrodynamics/bemrosetta/test_models.py::TestPanelMesh::test_total_area` | PASS, BASE and AFTER; [tests/hydrodynamics/bemrosetta/conftest.py:L103-L126] [tests/hydrodynamics/bemrosetta/test_models.py:L410-L420] |
| R1 / regression | Repository synthetic listing, `fixtures/sample_aqwa.lis`; fixture revision = code pin | At least 5 frequencies, 3 headings, 5 matrices of each kind and 15 RAO points; exact count lower bounds | `tests/hydrodynamics/bemrosetta/test_integration.py::TestFixtureFilesIntegration::test_sample_aqwa_file_parses_correctly` | PASS, BASE and AFTER; [tests/hydrodynamics/bemrosetta/test_integration.py:L675-L699] |
| R2 / regression | GDF writer/read roundtrip; inline quadrilateral | Vertex/panel counts equal; sorted coordinate arrays equal at decimal=5, u | `tests/hydrodynamics/bemrosetta/test_mesh.py::TestGDFHandler::test_gdf_roundtrip` | PASS, BASE and AFTER; [tests/hydrodynamics/bemrosetta/test_mesh.py:L270-L291] |
| R3 / regression | Repository synthetic `fixtures/sample.qtf`; fixture revision = code pin | Difference QTF; at least 4 frequencies and 1 component; dimensions equal frequency count | `tests/hydrodynamics/bemrosetta/test_integration.py::TestFixtureFilesIntegration::test_sample_qtf_file_parses_correctly` | PASS, BASE and AFTER; [tests/hydrodynamics/bemrosetta/test_integration.py:L701-L723] |

V1–V2 establish elementary geometry only; R1–R3 do not supply an independent BEM coefficient reference. [tests/hydrodynamics/bemrosetta/test_mesh.py:L135-L141] [tests/hydrodynamics/bemrosetta/test_models.py:L410-L420] [tests/hydrodynamics/bemrosetta/test_integration.py:L675-L723]

## 5. QA gates

The required diffraction gates are G0, G1, G4 and G5; G2 and G3 are NOT_APPLICABLE because diffraction database conversion has no line statics or time-domain dynamics stage. (AQG r0, sections 3, 8–10.)

The following are acceptance procedures; an offline regression PASS is not a target-program gate clearance. Unit `count` denotes an exact integer; `u` is the input coordinate unit. (AQG r0, sections 2, 5, 8, 13.)

| Gate | Check ID / executable procedure | Numeric pass criterion and unit | Evidence and current result |
| --- | --- | --- | --- |
| G0 | C00: hash sorted relative input paths and file digests before parsing and before export; compare to retained manifest | 0 digest mismatches, count | Required record; NOT_RUN for a production input set; appendix gap O4. (AQG r0, section 4.) |
| G0 | C01: run `AQWAParser.parse` on R1 and `QTFParser.parse` on R3; compare with fixture declarations | R1 ≥5 frequencies, ≥3 headings, ≥5 added-mass and damping matrices, ≥15 RAO entries; R3 ≥4 frequencies, ≥1 component; count | PASS fixture assertions; not a completeness test for arbitrary input. [tests/hydrodynamics/bemrosetta/test_integration.py:L675-L723] |
| G0 | C02: hand-audit each parsed listing/QTF/mesh against declared records, frequency axes, coordinate units and nonempty coefficient blocks; compare source values with output | 0 missing, padded, truncated, unconverted or unexplained entries, count; 0 undeclared units, count | Proposed input audit, NOT_RUN on external inputs; no automated detection for the malformed-input paths in section 6. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L335-L411] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L87-L118] |
| G1 | C10: load the written GDF/DAT/STL in the intended OrcaWave version, then load its complete diffraction data and validate | 0 load/validation errors and 0 warnings, count | NOT_RUN: licensed target program unavailable; #610. (AQG r0, sections 5, 10.) [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L243-L297] |
| G1 | C11: incorporate converted geometry into an independently prepared AQWA deck and run only through hydrostatics | 0 load errors and 0 warnings, count; hydrostatics stage reached once, count | NOT_RUN: licensed target program unavailable; this module does not write the full deck. (AQG r0, section 10.) [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L350-L391] |
| G1 | C12: import converter YAML/CSV and QTF data into the intended OrcaFlex version; reconcile imported coefficient grids | 0 load errors/warnings and 0 missing frequency/heading/DOF entries, count | NOT_RUN: licensed target program unavailable; issue pending (6). (AQG r0, sections 5, 8.) [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L144] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L245-L281] |
| G2 | NOT_APPLICABLE | Diffraction output and mesh conversion; no static line equilibrium | (AQG r0, section 10.) |
| G3 | NOT_APPLICABLE | Diffraction output and mesh conversion; no short dynamics solve | (AQG r0, section 10.) |
| G4 | C40: call `validate_mesh`; execute V1–V2 and degenerate/duplicate test cases | V1 area 1 u² within rel 1e-6; V2 area 5 u² within rel 0.01; bad fixtures produce >0 corresponding defect counts | PASS offline fixtures; production criterion is 0 degenerate panels and 0 unintended duplicate vertices, count. [tests/hydrodynamics/bemrosetta/test_mesh.py:L135-L153] [tests/hydrodynamics/bemrosetta/test_models.py:L410-L420] |
| G4 | C41: execute `CoefficientValidator` asymmetric, negative-diagonal, NaN/Inf and non-causal-data tests; inspect errors AND warnings/metric coverage | ≥1 expected diagnostic per injected defect, count; clean production input requires 0 errors, 0 unexplained warnings and 36 available class K-K pair metrics, count | PASS existing injection tests; metric completeness is an additional hand check, not an existing fail-closed validator. [tests/hydrodynamics/bemrosetta/test_validators.py:L209-L310] [tests/hydrodynamics/bemrosetta/test_validators.py:L656-L689] |
| G4 | C42: compare each converted body with independent mesh integration and target hydrostatics; record actual mass, density, gravity, centres, draft and waterline | Weight/buoyancy ≤1.0%; volume ≤0.5%; horizontal centre ≤0.5% of waterline length; vertical centre ≤1% of draft; GM >0 m and within 2%; waterline ≤1 mm; draft ≤0.01 m; 0 inward normals and 0 zero-area panels, count | NOT_RUN: no paired independent hydrostatics/reference input; #608 and #610; no module function computes this complete check. (AQG r0, section 8.) [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L50-L99] |
| G4 | C43: inspect mesh and frequency band before accepting `validate_mesh` output; use target checks for irregular frequencies | Maximum aspect ratio ≤3 dimensionless (≤5 at waterline only with second-order justification); panel dimension ≤shortest wavelength/6, u; 0 untreated irregular frequencies in range, count | NOT_RUN for production data; heuristic score and normal consistency are insufficient; #608. (AQG r0, section 8.) [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L138-L240] |
| G5 | C50: assemble QA appendix from retained ConversionResult, ValidationReport and mesh report, adding version, input digests, gate records, images and dispositions | 0 missing required records, count; 0 unexplained differences from source records, count | NOT_RUN: no production QA appendix; issue pending (6). (AQG r0, sections 9, 13.) [src/digitalmodel/hydrodynamics/bemrosetta/models/conversion_result.py:L98-L116] [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L188-L221] |

Eleven checks are defined: G0=3, G1=3, G2=0, G3=0, G4=4, G5=1. (AQG r0, sections 3, 10.)

## 6. Known failure modes and pitfalls

sweep: 170 found

The count includes reviewed defaults, intentional filters and error paths, rather than distinct confirmed defects; each location below is retained for rechecking. The sweep reads source only and does not execute a solver. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L392-L411]

Exact discovery command, followed by AST review of returns, exception handlers, numeric conversions, defaults and unit assumptions:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|errors=|units|unit|cache' src/digitalmodel/hydrodynamics/bemrosetta --glob '*.py'
```

Detection column distinguishes existing fixture coverage from missing detection of malformed input; C02 is a manual acceptance procedure, not an automated regression test. [tests/hydrodynamics/bemrosetta/test_parsers.py:L215-L264] [tests/hydrodynamics/bemrosetta/test_mesh.py:L143-L159]

| Location / kind | Symptom or retained assumption | Detection | Follow-up | Observed date |
| --- | --- | --- | --- | --- |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L75-L109]; bypass | convert defaults validate=True but detailed validation skipped; format option does not select YAML/CSV writer. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L199-L207]; default | Missing RAO DOF exported as zero amplitude/phase. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L311-L382]; criterion | Missing result groups not errors; asymmetry/negative damping warnings still exit successfully; RAO phase not checked. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L397-L432]; fallback | Missing/short coefficient arrays return empty K-K result; constant variation yields zero error; exceptions yield None. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L431-L432]; except | K-K result absent or None on insufficient data or integration failure; warnings alone do not invalidate the report. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L476-L536]; bypass | Mesh normal/watertight flags unused; low score only printed and does not fail command. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L24-L31]; default | Omitted output directory selects current directory. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L66-L75]; completeness | Missing RAO/added-mass/damping produce warning strings rather than blocking export. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L94-L109]; status | Conversion logs validation warnings then exports; custom name applied to QTF but not delegated exporter. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L189-L205]; unit/convention | Minimal YAML fixes zero origin and AQWA phase convention; QTF presence alone selects Full QTF. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L253-L279]; default | Both exported QTF axes use container frequencies; missing component emitted as real/imaginary zero. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L312-L319]; except | Convenience conversion catches any exception and returns FAILED; caller must inspect status/errors rather than assume a raised error. | Existing failed-result regression; C50 must retain status/errors; [tests/hydrodynamics/bemrosetta/test_converters.py:L361-L375] | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L54-L58]; default | Empty context/suggestions used for falsey optional arguments; this is diagnostic storage, not recovered engineering data. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L199-L213]; default | Empty ValidationReport starts valid; warnings do not change is_valid. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L43-L44]; except | Availability or version detection failure is reduced to False or an unset version; the cached runner may retain unavailable state. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L56-L61]; cache | Singleton caches a runner even when no executable was found; there is no empty-result data cache write. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L103-L109]; availability | Availability checks path existence, not executable permission or command success. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L149-L155]; status | Nonzero BEMRosetta return code is logged and returned rather than raised. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L247-L261]; default | Version remains None when path absent, command nonzero or no stdout; caught failures only logged. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L260-L261]; except | Availability or version detection failure is reduced to False or an unset version; the cached runner may retain unavailable state. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L64-L81]; encoding/detection | Invalid bytes replaced; AQWA auto-detection examines first ten lines only. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L110-L145]; default | Missing panel count defaults zero; declared counts not reconciled and blank vertex consumes an expected slot. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L113-L113]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L114-L114]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L134-L134]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L138-L138]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L153-L153]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L155-L180]; filter | Short or nonnumeric panel fields filtered, missing indices padded −1; terminator ends ingestion. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L159-L159]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L162-L162]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L179-L180]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L253-L253]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L258-L258]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L262-L262]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L267-L267]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L272-L272]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L274-L283]; default | Unknown node ID becomes −1; missing vertices padded and surplus nodes truncated. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L280-L280]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L319-L348]; filter | Short, invalid-ID or insufficient-coordinate vertex line returns silently; duplicate sequence ID overwrites mapping. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L321-L321]; return | Invalid vertex fields return without a diagnostic; downstream mesh can be incomplete. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L324-L324]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L325-L326]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L326-L326]; return | Invalid vertex fields return without a diagnostic; downstream mesh can be incomplete. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L344-L344]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L347-L348]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L372-L389]; default | Negative/absent panel vertices written as zeros, which also form the file terminator. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L87-L104]; unit/default | ULEN/GRAV ignored and missing symmetry flag defaults zero. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L102-L102]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L103-L103]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L135-L160]; truncation | Premature EOF, blank or malformed coordinate lines discard panels without matching declared count. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L143-L143]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L147-L147]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L155-L156]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L192-L205]; unit/convention | Writer symmetry derives from characters in plane string rather than inverse read mapping; fixed ULEN and gravity replace source header. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L210-L228]; filter | Panels with fewer than three valid vertices omitted; triangles repeat last vertex; rounding changes coordinates. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L86-L99]; default | Empty mesh area statistics become zero; report is_valid and several flags/statistics remain dataclass defaults. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L110-L136]; tolerance | Degenerate area threshold 1e-10 coordinate-unit squared; duplicate distance 1e-6 coordinate units; pairwise fallback uses strict comparison. | C40: existing degenerate/duplicate fixture tests; source-unit qualification still manual | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L128-L136]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L149-L169]; default | Aspect ratio starts at 1; short or near-zero-edge panels omitted from ratio computation. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L154-L154]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L183-L201]; heuristic | Empty mesh normal check passes; centre-based heuristic allows a fraction of inward normals. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L223-L240]; heuristic | Score starts at 100 and subtracts bounded penalties; empty geometry can retain a high score. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L86-L101]; detection | ASCII/binary heuristic inspects only first 1000 bytes; undecodable header means binary. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L99-L100]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L123-L142]; filter | Malformed or incomplete ASCII facets discarded; no input-facet count reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L127-L127]; external numeric | External coordinate/index tokens use Python syntax; unsupported or malformed fields may be lost or reject the file. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L135-L136]; except | Fallback or ignored malformed input can change geometry or report behaviour without invalidating the result. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L168-L204]; truncation | Short binary vertex payload breaks facet read; earlier complete facets can still be returned; native endian unpack used. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L254-L269]; filter | ASCII writer ignores short panels and fans valid quads into triangles. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L257-L257]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L290-L323]; filter | Binary writer counts/fans only valid vertices, skips short panels and uses native endian packing. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L308-L308]; continue | Blank/structural lines or short panels are skipped; record count or connectivity completeness is not guaranteed. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/conversion_result.py:L40-L50]; default | Default status SUCCESS, empty files/errors and duration 0.0 s do not prove any output exists. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/conversion_result.py:L160-L171]; default | Missing serialised status defaults SUCCESS; duration defaults 0.0 s and files/errors empty. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L43-L61]; default | Zero reference point, empty metadata and supplied normals/areas/centres accepted without shape/consistency checks. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L84-L88]; default | Missing panel areas give total area 0.0. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L106-L126]; fallback | Short or collinear panels assigned +z unit normal. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L110-L110]; continue | Optional or absent value returns a sentinel; caller must distinguish missing data. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L136-L140]; default | Short panel assigned zero area. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L140-L140]; continue | Optional or absent value returns a sentinel; caller must distinguish missing data. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L165-L170]; default | Short panel assigned zero centroid. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L169-L169]; continue | Optional or absent value returns a sentinel; caller must distinguish missing data. | no detection check exists; C02 manual reconciliation | issue pending (5) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L175-L178]; geometry | flip_normals changes stored normals only; connectivity and writer vertex ordering stay unchanged. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L230-L248]; default | Quality report defaults valid, zero counts/statistics and score 100; unpopulated flags can disagree with populated counts. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L95-L107]; default | Empty frequencies/headings/components and standard six DOF names are allowed without completeness validation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L134-L137]; tolerance | Heading lookup uses atol=0.1 degrees and NumPy relative tolerance. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L137-L137]; return | Missing DOF/heading lookup returns None; exporters can replace it by zero. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L192-L217]; default | Absent deserialised components/axes become empty collections; no source completeness flag. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L49-L89]; default | Unknown solver, infinite depth, density/gravity, zero frequency/heading counts/ranges, one body, zero origin and implicit SI units are accepted defaults. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L91-L94]; criterion | Depth above 500 m classed deep independent of wavelength. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L111-L128]; serialisation | Finite depth above 500 m serialised as infinite, losing the finite value. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L146-L147]; except | Unknown solver label is replaced by UNKNOWN without preserving a parse error. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L154-L176]; default | Missing deserialised depth, physical parameters, counts, ranges and reference point are replaced by defaults. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L84-L86]; unit/encoding | Invalid UTF-8 bytes are ignored. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L156-L188]; default | Missing version/depth/name use empty string, infinity or input stem; metadata counts and physical defaults remain independent of parsed grids. | no detection check exists; C02 manual reconciliation | issue pending (3) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L167-L167]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L201-L237]; extent | Fixed 5000-character window; restricted frequency/period range and duplicate frequencies silently omitted. | no detection check exists; C02 manual reconciliation | #2107 | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L210-L210]; return | Missing section returns an empty collection; a different nonempty section can still allow parse success. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L222-L222]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L223-L223]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L248-L266]; extent | Fixed 10000-character heading window and limited heading range. | no detection check exists; C02 manual reconciliation | #2107 | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L253-L253]; return | Missing section returns an empty collection; a different nonempty section can still allow parse success. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L262-L262]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L277-L306]; extent | Added-mass table limited to 5000 characters; repeated frequency overwrites previous matrix. | no detection check exists; C02 manual reconciliation | #2107 | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L281-L281]; return | Missing section returns an empty collection; a different nonempty section can still allow parse success. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L299-L299]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L302-L302]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L324-L350]; extent | Damping table limited to 5000 characters; repeated frequency overwrites previous matrix. | no detection check exists; C02 manual reconciliation | #2107 | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L328-L328]; return | Missing section returns an empty collection; a different nonempty section can still allow parse success. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L346-L346]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L348-L348]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L369-L387]; default | Unrepresented matrix coupling entries stay zero; symmetry is constructed from paired coefficients. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L416-L416]; continue | Rows skipped: headers, page numbers, velocity/acceleration sections or out-of-range frequency/heading; expected-record totals are not reconciled. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L422-L422]; return | Missing section returns an empty collection; a different nonempty section can still allow parse success. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L426-L431]; extent | Last RAO section limited to 50000 characters. | no detection check exists; C02 manual reconciliation | #2107 | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L468-L468]; continue | Rows skipped: headers, page numbers, velocity/acceleration sections or out-of-range frequency/heading; expected-record totals are not reconciled. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L471-L471]; continue | Rows skipped: headers, page numbers, velocity/acceleration sections or out-of-range frequency/heading; expected-record totals are not reconciled. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L473-L508]; default | Continuation rows require a previous heading; duplicate frequency/heading keys overwrite data. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L476-L476]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L477-L477]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L480-L480]; continue | Rows skipped: headers, page numbers, velocity/acceleration sections or out-of-range frequency/heading; expected-record totals are not reconciled. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L485-L485]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L486-L486]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L487-L487]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L488-L488]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L489-L489]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L490-L490]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L496-L496]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L499-L499]; continue | Rows skipped: headers, page numbers, velocity/acceleration sections or out-of-range frequency/heading; expected-record totals are not reconciled. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L502-L502]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L503-L503]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L504-L504]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L505-L505]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L506-L506]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L507-L507]; external numeric | External tokens use Python numeric syntax after regex matching; comma decimals, thousands separators and unsupported exponents are not normalised. | no detection check exists; C02 manual reconciliation | issue pending (2) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L60-L62]; unit/encoding | Invalid UTF-8 bytes are ignored. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L110-L118]; default | Missing QTF type defaults to difference-frequency. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L123-L138]; default | Missing frequency axis copied from other axis or generic extraction. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L143-L146]; default | Missing headings default to 0.0 degrees. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L203-L203]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L206-L207]; except | Rejected numeric data can be ignored before a fallback path is selected. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L207-L207]; continue | Rejected frequency, heading or matrix token skipped; an empty parse can trigger defaults or zero coefficients. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L220-L234]; filter | Generic frequency extraction filters to 0.01 through 10.0 rad/s, exclusive. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L226-L226]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L231-L232]; except | Rejected numeric data can be ignored before a fallback path is selected. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L255-L255]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L260-L261]; except | Rejected numeric data can be ignored before a fallback path is selected. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L261-L261]; continue | Rejected frequency, heading or matrix token skipped; an empty parse can trigger defaults or zero coefficients. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L301-L301]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L302-L302]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L310-L331]; fallback | No structured component can select simple parser instead of rejecting missing coefficient content. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L354-L354]; external numeric | External frequency, heading, DOF or coefficient token converted without locale validation or declared range reconciliation. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L355-L356]; except | Rejected numeric data can be ignored before a fallback path is selected. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L356-L356]; continue | Rejected frequency, heading or matrix token skipped; an empty parse can trigger defaults or zero coefficients. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L358-L365]; truncation | Extra matrix values truncated; insufficient values produce None. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L362-L363]; except | Rejected numeric data can be ignored before a fallback path is selected. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L365-L365]; return | Unparseable matrix returns None; default path can create coefficients without source values. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L392-L409]; default | Simple-format fallback fabricates six zero amplitude/phase matrices per heading. | no detection check exists; C02 manual reconciliation | issue pending (1) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L36-L48]; default | Tolerance defaults 0.1; stored n_integration_points=1000 does not control trapezoid grid. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L61-L87]; completeness | Missing matrices or K-K violations warn without invalidation; missing component metrics omitted. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L128-L172]; fallback | Fewer than three samples or integration errors give None; constant added mass yields 0.0 regardless of damping integral; last sample used as infinity. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L131-L131]; return | K-K result absent or None on insufficient data or integration failure; warnings alone do not invalidate the report. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L170-L172]; except | K-K result absent or None on insufficient data or integration failure; warnings alone do not invalidate the report. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L172-L172]; return | K-K result absent or None on insufficient data or integration failure; warnings alone do not invalidate the report. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L65-L95]; completeness | Absent matrices only warn; absent RAOs not diagnosed. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L125-L143]; criterion | Added-mass symmetry only warns; negative diagonal is checked using tolerance as an absolute threshold; eigenvalues untested. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L163-L179]; criterion | Asymmetric or negative damping only warns; report can remain valid. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L195-L222]; criterion | Missing DOF skipped; negative magnitude and phase outside range only warn. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L201-L201]; continue | Missing RAO component is skipped; no completeness error is added. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |
| [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L240-L245]; default | Convenience default strict=False disables diagonal-sign check. | no detection check exists; C02 manual reconciliation | issue pending (4) | 2026-10-18 |

| Additional confirmed pitfall | Detection | Follow-up |
| --- | --- | --- |
| Raw AQWAParseResult dictionaries do not implement the DiffractionResults interface expected by the README validation/conversion quick start. [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L33-L46] [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L54-L75] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L35-L42] | no detection check exists for README example; C02 requires explicit adapter | issue pending (5) |
| Mesh-quality booleans can remain valid/default even when counts and warnings report defects; score is not watertightness or hydrostatic evidence. [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L86-L99] [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L230-L248] | C40 detects counts; no detection check exists for contradictory default flags | #608; issue pending (4) |
| The bounded-window defect described by #2107 in the adjacent diffraction parser also occurs here; this package uses its own 5000/10000/50000-character windows. [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L201-L255] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L277-L330] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L426-L431] | no detection check exists for a listing exceeding these windows; C02 compares expected grid | #2107 |

No 1970 default or file-backed empty-result cache write was found in the source sweep; the runner singleton is the retained availability-cache risk. [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L25-L61]

Issue leads #321, #325 and #300 concern input conversion and mesh interchange; they are not evidence that current output has passed licensed loading. The retained tests check fixture parsing and file existence rather than licensed solver execution. [tests/hydrodynamics/bemrosetta/test_integration.py:L675-L809]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Inspect missing-data fallbacks and diagnostic counts before accepting an output file or a valid flag | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L392-L411] [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L199-L213] | Initial failure inventory and gate procedures | issue pending (6) |

## 8. Ownership

Review responsibility: diffraction quality manager, with a diffraction deputy quality manager; the producer supplies evidence and the maintainer resolves publication follow-up. Independent review must precede clearance; no named individual or completed clearance is asserted. (AQG r0, section 11.)

Escalate ambiguous conventions, absent independent references and proposed tolerance changes to the diffraction quality manager; cross-family effects require maintainer disposition. These are responsibilities for this draft, not claims of completed approval. (AQG r0, sections 8, 11–12.)

## 9. Dependencies and alternative implementations

The code pin is the header revision. The implementation-map ref is `origin/codex/implmap-work`, ref commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; map claims below refer to that explicitly pinned map, not the header code revision. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4]

### Map family inventory

#### rao-readers-and-orcaflex-converters

In-package rows: 10; repository-wide rows: 295. Every repository-wide row is enumerated below by its one-based ordinal within this family and a map-source citation; external locators remain in the map to avoid repeating identifying discovery paths. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9509-L13255]

| In-package definition | Map classification | Code evidence |
| --- | --- | --- |
| `get_module_info` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L149-L162] |
| `convert` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L51-L120] |
| `_export_raos_csv` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L181-L210] |
| `status` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L547-L575] |
| `OrcaFlexConverter._call_orcaflex_exporter` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L144] |
| `OrcaFlexConverter._build_vessel_yaml` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L172-L206] |
| `DATHandler._parse_aqwa_deck` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L221-L306] |
| `AQWAParser.supported_extensions` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L58-L60] |
| `AQWAParser.parse` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] |
| `AQWAParser._extract_raos` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L389-L515] |

Complete map-row enumeration (ordinal locates the implementation, and the citation resolves its full path, symbol and relation):

| Map row | Pinned map evidence |
| --- | --- |
| 1 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9516-L9527] |
| 2 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9528-L9539] |
| 3 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9540-L9551] |
| 4 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9552-L9563] |
| 5 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9564-L9578] |
| 6 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9579-L9589] |
| 7 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9590-L9601] |
| 8 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9602-L9613] |
| 9 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9614-L9625] |
| 10 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9626-L9637] |
| 11 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9638-L9652] |
| 12 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9653-L9664] |
| 13 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9665-L9678] |
| 14 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9679-L9690] |
| 15 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9691-L9704] |
| 16 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9705-L9715] |
| 17 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9716-L9727] |
| 18 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9728-L9739] |
| 19 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9740-L9753] |
| 20 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9754-L9765] |
| 21 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9766-L9777] |
| 22 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9778-L9788] |
| 23 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9789-L9800] |
| 24 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9801-L9812] |
| 25 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9813-L9826] |
| 26 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9827-L9838] |
| 27 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9839-L9849] |
| 28 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9850-L9861] |
| 29 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9862-L9873] |
| 30 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9874-L9885] |
| 31 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9886-L9897] |
| 32 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9898-L9909] |
| 33 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9910-L9921] |
| 34 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9922-L9932] |
| 35 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9933-L9943] |
| 36 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9944-L9954] |
| 37 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9955-L9965] |
| 38 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9966-L9977] |
| 39 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9978-L9989] |
| 40 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9990-L10004] |
| 41 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10005-L10017] |
| 42 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10018-L10032] |
| 43 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10033-L10046] |
| 44 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10047-L10060] |
| 45 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10061-L10072] |
| 46 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10073-L10087] |
| 47 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10088-L10099] |
| 48 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10100-L10111] |
| 49 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10112-L10122] |
| 50 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10123-L10134] |
| 51 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10135-L10146] |
| 52 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10147-L10158] |
| 53 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10159-L10169] |
| 54 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10170-L10181] |
| 55 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10182-L10193] |
| 56 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10194-L10207] |
| 57 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10208-L10222] |
| 58 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10223-L10235] |
| 59 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10236-L10246] |
| 60 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10247-L10260] |
| 61 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10261-L10272] |
| 62 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10273-L10284] |
| 63 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10285-L10296] |
| 64 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10297-L10307] |
| 65 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10308-L10319] |
| 66 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10320-L10333] |
| 67 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10334-L10345] |
| 68 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10346-L10356] |
| 69 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10357-L10368] |
| 70 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10369-L10380] |
| 71 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10381-L10391] |
| 72 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10392-L10403] |
| 73 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10404-L10415] |
| 74 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10416-L10427] |
| 75 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10428-L10439] |
| 76 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10440-L10451] |
| 77 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10452-L10465] |
| 78 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10466-L10477] |
| 79 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10478-L10489] |
| 80 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10490-L10501] |
| 81 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10502-L10513] |
| 82 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10514-L10527] |
| 83 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10528-L10539] |
| 84 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10540-L10551] |
| 85 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10552-L10563] |
| 86 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10564-L10575] |
| 87 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10576-L10587] |
| 88 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10588-L10599] |
| 89 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10600-L10611] |
| 90 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10612-L10623] |
| 91 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10624-L10635] |
| 92 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10636-L10647] |
| 93 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10648-L10659] |
| 94 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10660-L10671] |
| 95 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10672-L10683] |
| 96 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10684-L10699] |
| 97 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10700-L10711] |
| 98 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10712-L10725] |
| 99 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10726-L10737] |
| 100 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10738-L10749] |
| 101 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10750-L10761] |
| 102 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10762-L10775] |
| 103 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10776-L10789] |
| 104 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10790-L10801] |
| 105 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10802-L10815] |
| 106 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10816-L10828] |
| 107 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10829-L10841] |
| 108 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10842-L10853] |
| 109 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10854-L10865] |
| 110 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10866-L10877] |
| 111 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10878-L10891] |
| 112 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10892-L10905] |
| 113 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10906-L10917] |
| 114 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10918-L10933] |
| 115 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10934-L10948] |
| 116 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10949-L10962] |
| 117 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10963-L10979] |
| 118 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10980-L10991] |
| 119 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10992-L11003] |
| 120 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11004-L11015] |
| 121 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11016-L11027] |
| 122 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11028-L11039] |
| 123 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11040-L11051] |
| 124 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11052-L11063] |
| 125 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11064-L11075] |
| 126 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11076-L11089] |
| 127 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11090-L11101] |
| 128 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11102-L11113] |
| 129 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11114-L11125] |
| 130 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11126-L11137] |
| 131 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11138-L11149] |
| 132 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11150-L11163] |
| 133 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11164-L11175] |
| 134 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11176-L11187] |
| 135 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11188-L11202] |
| 136 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11203-L11213] |
| 137 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11214-L11225] |
| 138 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11226-L11237] |
| 139 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11238-L11248] |
| 140 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11249-L11260] |
| 141 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11261-L11274] |
| 142 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11275-L11286] |
| 143 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11287-L11298] |
| 144 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11299-L11310] |
| 145 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11311-L11322] |
| 146 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11323-L11334] |
| 147 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11335-L11346] |
| 148 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11347-L11360] |
| 149 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11361-L11374] |
| 150 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11375-L11388] |
| 151 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11389-L11400] |
| 152 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11401-L11414] |
| 153 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11415-L11426] |
| 154 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11427-L11438] |
| 155 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11439-L11450] |
| 156 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11451-L11462] |
| 157 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11463-L11474] |
| 158 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11475-L11486] |
| 159 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11487-L11498] |
| 160 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11499-L11510] |
| 161 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11511-L11524] |
| 162 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11525-L11536] |
| 163 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11537-L11548] |
| 164 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11549-L11560] |
| 165 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11561-L11574] |
| 166 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11575-L11590] |
| 167 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11591-L11603] |
| 168 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11604-L11619] |
| 169 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11620-L11633] |
| 170 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11634-L11648] |
| 171 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11649-L11662] |
| 172 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11663-L11674] |
| 173 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11675-L11686] |
| 174 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11687-L11698] |
| 175 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11699-L11710] |
| 176 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11711-L11724] |
| 177 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11725-L11736] |
| 178 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11737-L11748] |
| 179 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11749-L11762] |
| 180 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11763-L11774] |
| 181 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11775-L11786] |
| 182 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11787-L11798] |
| 183 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11799-L11812] |
| 184 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11813-L11824] |
| 185 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11825-L11836] |
| 186 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11837-L11850] |
| 187 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11851-L11862] |
| 188 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11863-L11874] |
| 189 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11875-L11888] |
| 190 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11889-L11900] |
| 191 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11901-L11912] |
| 192 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11913-L11926] |
| 193 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11927-L11938] |
| 194 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11939-L11950] |
| 195 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11951-L11964] |
| 196 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11965-L11976] |
| 197 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11977-L11990] |
| 198 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11991-L12002] |
| 199 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12003-L12014] |
| 200 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12015-L12026] |
| 201 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12027-L12038] |
| 202 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12039-L12052] |
| 203 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12053-L12064] |
| 204 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12065-L12078] |
| 205 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12079-L12095] |
| 206 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12096-L12107] |
| 207 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12108-L12119] |
| 208 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12120-L12131] |
| 209 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12132-L12143] |
| 210 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12144-L12159] |
| 211 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12160-L12173] |
| 212 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12174-L12186] |
| 213 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12187-L12199] |
| 214 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12200-L12211] |
| 215 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12212-L12226] |
| 216 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12227-L12238] |
| 217 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12239-L12254] |
| 218 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12255-L12266] |
| 219 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12267-L12278] |
| 220 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12279-L12290] |
| 221 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12291-L12302] |
| 222 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12303-L12314] |
| 223 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12315-L12326] |
| 224 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12327-L12338] |
| 225 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12339-L12350] |
| 226 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12351-L12362] |
| 227 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12363-L12376] |
| 228 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12377-L12388] |
| 229 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12389-L12400] |
| 230 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12401-L12412] |
| 231 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12413-L12427] |
| 232 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12428-L12441] |
| 233 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12442-L12453] |
| 234 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12454-L12465] |
| 235 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12466-L12477] |
| 236 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12478-L12489] |
| 237 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12490-L12503] |
| 238 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12504-L12515] |
| 239 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12516-L12529] |
| 240 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12530-L12541] |
| 241 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12542-L12555] |
| 242 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12556-L12567] |
| 243 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12568-L12582] |
| 244 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12583-L12594] |
| 245 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12595-L12606] |
| 246 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12607-L12620] |
| 247 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12621-L12632] |
| 248 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12633-L12646] |
| 249 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12647-L12658] |
| 250 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12659-L12670] |
| 251 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12671-L12682] |
| 252 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12683-L12696] |
| 253 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12697-L12710] |
| 254 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12711-L12721] |
| 255 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12722-L12733] |
| 256 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12734-L12745] |
| 257 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12746-L12759] |
| 258 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12760-L12773] |
| 259 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12774-L12787] |
| 260 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12788-L12799] |
| 261 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12800-L12813] |
| 262 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12814-L12827] |
| 263 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12828-L12839] |
| 264 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12840-L12850] |
| 265 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12851-L12862] |
| 266 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12863-L12874] |
| 267 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12875-L12886] |
| 268 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12887-L12898] |
| 269 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12899-L12910] |
| 270 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12911-L12922] |
| 271 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12923-L12934] |
| 272 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12935-L12946] |
| 273 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12947-L12958] |
| 274 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12959-L12973] |
| 275 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12974-L12986] |
| 276 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12987-L12998] |
| 277 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12999-L13010] |
| 278 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13011-L13022] |
| 279 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13023-L13034] |
| 280 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13035-L13048] |
| 281 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13049-L13062] |
| 282 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13063-L13074] |
| 283 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13075-L13086] |
| 284 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13087-L13101] |
| 285 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13102-L13113] |
| 286 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13114-L13125] |
| 287 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13126-L13139] |
| 288 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13140-L13153] |
| 289 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13154-L13167] |
| 290 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13168-L13181] |
| 291 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13182-L13193] |
| 292 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13194-L13204] |
| 293 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13205-L13216] |
| 294 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13217-L13228] |
| 295 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13229-L13240] |

#### orcaflex-text-model-writers

In-package rows: 1; repository-wide rows: 440. Every repository-wide row is enumerated below by its one-based ordinal within this family and a map-source citation; external locators remain in the map to avoid repeating identifying discovery paths. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4933-L9508]

| In-package definition | Map classification | Code evidence |
| --- | --- | --- |
| `OrcaFlexConverter.convert_to_vessel_yaml` | independent | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L146-L170] |

Complete map-row enumeration (ordinal locates the implementation, and the citation resolves its full path, symbol and relation):

| Map row | Pinned map evidence |
| --- | --- |
| 1 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4940-L4949] |
| 2 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4950-L4958] |
| 3 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4959-L4968] |
| 4 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4969-L4978] |
| 5 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4979-L4988] |
| 6 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4989-L4998] |
| 7 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4999-L5008] |
| 8 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5009-L5020] |
| 9 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5021-L5030] |
| 10 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5031-L5040] |
| 11 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5041-L5050] |
| 12 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5051-L5060] |
| 13 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5061-L5070] |
| 14 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5071-L5080] |
| 15 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5081-L5090] |
| 16 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5091-L5100] |
| 17 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5101-L5110] |
| 18 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5111-L5120] |
| 19 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5121-L5132] |
| 20 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5133-L5142] |
| 21 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5143-L5151] |
| 22 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5152-L5161] |
| 23 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5162-L5171] |
| 24 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5172-L5181] |
| 25 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5182-L5191] |
| 26 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5192-L5201] |
| 27 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5202-L5211] |
| 28 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5212-L5223] |
| 29 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5224-L5233] |
| 30 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5234-L5243] |
| 31 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5244-L5253] |
| 32 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5254-L5263] |
| 33 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5264-L5273] |
| 34 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5274-L5283] |
| 35 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5284-L5293] |
| 36 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5294-L5303] |
| 37 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5304-L5313] |
| 38 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5314-L5323] |
| 39 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5324-L5333] |
| 40 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5334-L5343] |
| 41 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5344-L5353] |
| 42 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5354-L5363] |
| 43 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5364-L5372] |
| 44 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5373-L5382] |
| 45 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5383-L5392] |
| 46 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5393-L5402] |
| 47 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5403-L5415] |
| 48 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5416-L5425] |
| 49 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5426-L5437] |
| 50 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5438-L5447] |
| 51 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5448-L5457] |
| 52 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5458-L5467] |
| 53 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5468-L5477] |
| 54 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5478-L5487] |
| 55 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5488-L5497] |
| 56 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5498-L5507] |
| 57 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5508-L5519] |
| 58 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5520-L5531] |
| 59 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5532-L5540] |
| 60 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5541-L5550] |
| 61 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5551-L5560] |
| 62 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5561-L5570] |
| 63 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5571-L5580] |
| 64 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5581-L5593] |
| 65 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5594-L5603] |
| 66 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5604-L5615] |
| 67 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5616-L5628] |
| 68 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5629-L5638] |
| 69 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5639-L5648] |
| 70 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5649-L5658] |
| 71 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5659-L5667] |
| 72 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5668-L5677] |
| 73 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5678-L5687] |
| 74 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5688-L5697] |
| 75 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5698-L5707] |
| 76 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5708-L5717] |
| 77 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5718-L5727] |
| 78 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5728-L5737] |
| 79 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5738-L5749] |
| 80 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5750-L5759] |
| 81 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5760-L5769] |
| 82 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5770-L5781] |
| 83 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5782-L5791] |
| 84 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5792-L5801] |
| 85 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5802-L5811] |
| 86 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5812-L5823] |
| 87 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5824-L5833] |
| 88 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5834-L5843] |
| 89 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5844-L5853] |
| 90 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5854-L5863] |
| 91 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5864-L5872] |
| 92 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5873-L5882] |
| 93 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5883-L5891] |
| 94 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5892-L5901] |
| 95 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5902-L5911] |
| 96 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5912-L5921] |
| 97 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5922-L5933] |
| 98 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5934-L5943] |
| 99 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5944-L5953] |
| 100 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5954-L5965] |
| 101 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5966-L5975] |
| 102 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5976-L5985] |
| 103 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5986-L5994] |
| 104 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5995-L6004] |
| 105 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6005-L6014] |
| 106 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6015-L6024] |
| 107 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6025-L6034] |
| 108 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6035-L6044] |
| 109 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6045-L6054] |
| 110 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6055-L6064] |
| 111 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6065-L6074] |
| 112 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6075-L6084] |
| 113 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6085-L6096] |
| 114 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6097-L6109] |
| 115 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6110-L6119] |
| 116 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6120-L6129] |
| 117 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6130-L6141] |
| 118 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6142-L6153] |
| 119 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6154-L6163] |
| 120 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6164-L6173] |
| 121 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6174-L6183] |
| 122 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6184-L6193] |
| 123 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6194-L6203] |
| 124 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6204-L6213] |
| 125 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6214-L6223] |
| 126 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6224-L6233] |
| 127 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6234-L6243] |
| 128 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6244-L6253] |
| 129 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6254-L6263] |
| 130 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6264-L6273] |
| 131 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6274-L6283] |
| 132 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6284-L6293] |
| 133 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303] |
| 134 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313] |
| 135 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6314-L6323] |
| 136 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6324-L6333] |
| 137 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6334-L6343] |
| 138 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6344-L6353] |
| 139 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6354-L6363] |
| 140 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6364-L6373] |
| 141 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6374-L6383] |
| 142 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6384-L6393] |
| 143 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6394-L6403] |
| 144 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6404-L6413] |
| 145 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6414-L6423] |
| 146 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6424-L6436] |
| 147 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6437-L6448] |
| 148 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6449-L6458] |
| 149 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6459-L6468] |
| 150 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6469-L6478] |
| 151 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6479-L6490] |
| 152 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6491-L6500] |
| 153 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6501-L6510] |
| 154 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6511-L6520] |
| 155 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6521-L6530] |
| 156 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6531-L6540] |
| 157 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6541-L6550] |
| 158 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6551-L6560] |
| 159 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6561-L6570] |
| 160 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6571-L6580] |
| 161 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6581-L6590] |
| 162 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6591-L6600] |
| 163 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6601-L6610] |
| 164 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6611-L6620] |
| 165 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6621-L6630] |
| 166 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6631-L6640] |
| 167 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6641-L6650] |
| 168 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6651-L6659] |
| 169 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6660-L6672] |
| 170 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6673-L6682] |
| 171 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6683-L6692] |
| 172 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6693-L6704] |
| 173 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6705-L6714] |
| 174 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6715-L6724] |
| 175 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6725-L6734] |
| 176 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6735-L6744] |
| 177 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6745-L6754] |
| 178 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6755-L6764] |
| 179 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6765-L6774] |
| 180 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6775-L6784] |
| 181 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6785-L6794] |
| 182 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6795-L6804] |
| 183 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6805-L6814] |
| 184 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6815-L6824] |
| 185 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6825-L6834] |
| 186 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6835-L6844] |
| 187 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6845-L6854] |
| 188 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6855-L6864] |
| 189 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6865-L6874] |
| 190 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6875-L6884] |
| 191 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6885-L6894] |
| 192 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6895-L6904] |
| 193 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6905-L6914] |
| 194 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6915-L6927] |
| 195 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6928-L6937] |
| 196 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6938-L6947] |
| 197 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6948-L6957] |
| 198 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6958-L6967] |
| 199 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6968-L6977] |
| 200 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6978-L6987] |
| 201 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6988-L6997] |
| 202 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6998-L7007] |
| 203 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7008-L7017] |
| 204 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7018-L7031] |
| 205 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7032-L7041] |
| 206 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7042-L7051] |
| 207 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7052-L7061] |
| 208 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7062-L7071] |
| 209 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7072-L7081] |
| 210 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7082-L7091] |
| 211 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7092-L7101] |
| 212 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7102-L7111] |
| 213 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7112-L7121] |
| 214 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7122-L7131] |
| 215 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7132-L7141] |
| 216 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7142-L7153] |
| 217 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7154-L7163] |
| 218 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7164-L7176] |
| 219 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7177-L7186] |
| 220 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7187-L7196] |
| 221 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7197-L7207] |
| 222 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7208-L7217] |
| 223 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7218-L7227] |
| 224 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7228-L7237] |
| 225 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7238-L7247] |
| 226 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7248-L7257] |
| 227 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7258-L7269] |
| 228 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7270-L7279] |
| 229 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7280-L7289] |
| 230 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7290-L7299] |
| 231 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7300-L7309] |
| 232 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7310-L7319] |
| 233 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7320-L7329] |
| 234 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7330-L7339] |
| 235 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7340-L7349] |
| 236 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7350-L7359] |
| 237 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7360-L7369] |
| 238 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7370-L7379] |
| 239 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7380-L7389] |
| 240 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7390-L7399] |
| 241 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7400-L7409] |
| 242 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7410-L7419] |
| 243 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7420-L7429] |
| 244 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7430-L7439] |
| 245 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7440-L7449] |
| 246 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7450-L7459] |
| 247 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7460-L7469] |
| 248 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7470-L7479] |
| 249 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7480-L7489] |
| 250 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7490-L7499] |
| 251 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7500-L7509] |
| 252 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7510-L7519] |
| 253 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7520-L7529] |
| 254 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7530-L7539] |
| 255 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7540-L7551] |
| 256 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7552-L7561] |
| 257 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7562-L7571] |
| 258 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7572-L7581] |
| 259 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7582-L7591] |
| 260 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7592-L7603] |
| 261 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7604-L7613] |
| 262 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7614-L7626] |
| 263 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7627-L7636] |
| 264 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7637-L7646] |
| 265 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7647-L7656] |
| 266 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7657-L7666] |
| 267 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7667-L7676] |
| 268 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7677-L7686] |
| 269 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7687-L7696] |
| 270 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7697-L7706] |
| 271 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7707-L7716] |
| 272 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7717-L7729] |
| 273 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7730-L7739] |
| 274 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7740-L7749] |
| 275 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7750-L7762] |
| 276 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7763-L7774] |
| 277 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7775-L7784] |
| 278 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7785-L7794] |
| 279 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7795-L7804] |
| 280 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7805-L7816] |
| 281 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7817-L7826] |
| 282 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7827-L7836] |
| 283 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7837-L7846] |
| 284 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7847-L7856] |
| 285 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7857-L7866] |
| 286 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7867-L7878] |
| 287 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7879-L7888] |
| 288 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7889-L7898] |
| 289 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7899-L7908] |
| 290 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7909-L7921] |
| 291 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7922-L7931] |
| 292 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7932-L7941] |
| 293 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7942-L7951] |
| 294 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7952-L7961] |
| 295 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7962-L7971] |
| 296 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7972-L7981] |
| 297 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7982-L7991] |
| 298 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7992-L8001] |
| 299 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8002-L8011] |
| 300 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8012-L8021] |
| 301 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8022-L8031] |
| 302 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8032-L8043] |
| 303 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8044-L8053] |
| 304 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8054-L8063] |
| 305 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8064-L8073] |
| 306 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8074-L8083] |
| 307 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8084-L8093] |
| 308 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8094-L8103] |
| 309 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8104-L8113] |
| 310 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8114-L8123] |
| 311 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8124-L8133] |
| 312 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8134-L8143] |
| 313 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8144-L8155] |
| 314 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8156-L8165] |
| 315 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8166-L8175] |
| 316 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8176-L8185] |
| 317 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8186-L8195] |
| 318 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8205] |
| 319 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8206-L8215] |
| 320 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8216-L8227] |
| 321 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8228-L8237] |
| 322 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8238-L8247] |
| 323 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8248-L8257] |
| 324 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8258-L8267] |
| 325 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8268-L8279] |
| 326 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8280-L8290] |
| 327 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8291-L8300] |
| 328 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8301-L8310] |
| 329 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8311-L8323] |
| 330 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8324-L8333] |
| 331 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8334-L8343] |
| 332 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8344-L8353] |
| 333 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8354-L8363] |
| 334 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8364-L8373] |
| 335 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8374-L8383] |
| 336 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8384-L8393] |
| 337 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8394-L8403] |
| 338 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8404-L8413] |
| 339 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8414-L8423] |
| 340 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8424-L8435] |
| 341 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8436-L8445] |
| 342 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8446-L8457] |
| 343 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8458-L8470] |
| 344 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8471-L8480] |
| 345 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8481-L8492] |
| 346 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8493-L8505] |
| 347 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8506-L8517] |
| 348 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8518-L8527] |
| 349 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8528-L8537] |
| 350 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8538-L8547] |
| 351 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8548-L8560] |
| 352 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8561-L8572] |
| 353 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8573-L8582] |
| 354 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8583-L8596] |
| 355 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8597-L8608] |
| 356 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8609-L8619] |
| 357 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8620-L8632] |
| 358 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8633-L8647] |
| 359 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8648-L8657] |
| 360 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8658-L8667] |
| 361 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8668-L8677] |
| 362 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8678-L8687] |
| 363 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8688-L8701] |
| 364 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8702-L8714] |
| 365 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8715-L8724] |
| 366 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8725-L8739] |
| 367 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8740-L8749] |
| 368 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8750-L8759] |
| 369 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8760-L8773] |
| 370 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8774-L8783] |
| 371 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8784-L8793] |
| 372 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8794-L8803] |
| 373 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8804-L8813] |
| 374 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8814-L8823] |
| 375 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8824-L8833] |
| 376 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8834-L8843] |
| 377 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8844-L8856] |
| 378 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8857-L8866] |
| 379 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8867-L8876] |
| 380 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8877-L8886] |
| 381 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8887-L8896] |
| 382 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8897-L8908] |
| 383 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8909-L8918] |
| 384 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8919-L8928] |
| 385 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8929-L8938] |
| 386 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8939-L8948] |
| 387 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8949-L8958] |
| 388 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8959-L8970] |
| 389 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8971-L8980] |
| 390 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8981-L8990] |
| 391 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8991-L9000] |
| 392 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9001-L9010] |
| 393 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9011-L9020] |
| 394 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9021-L9030] |
| 395 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9031-L9040] |
| 396 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9041-L9050] |
| 397 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9051-L9060] |
| 398 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9061-L9070] |
| 399 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9071-L9080] |
| 400 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9081-L9090] |
| 401 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9091-L9100] |
| 402 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9101-L9110] |
| 403 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9111-L9120] |
| 404 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9121-L9130] |
| 405 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9131-L9140] |
| 406 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9141-L9152] |
| 407 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9153-L9162] |
| 408 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9163-L9172] |
| 409 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9173-L9182] |
| 410 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9183-L9192] |
| 411 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9193-L9202] |
| 412 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9203-L9212] |
| 413 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9213-L9222] |
| 414 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9223-L9232] |
| 415 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9233-L9242] |
| 416 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9243-L9252] |
| 417 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9253-L9262] |
| 418 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9263-L9272] |
| 419 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9273-L9282] |
| 420 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9283-L9292] |
| 421 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9293-L9302] |
| 422 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9303-L9312] |
| 423 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9313-L9322] |
| 424 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9323-L9332] |
| 425 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9333-L9342] |
| 426 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9343-L9352] |
| 427 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9353-L9362] |
| 428 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9363-L9372] |
| 429 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9373-L9382] |
| 430 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9383-L9392] |
| 431 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9393-L9402] |
| 432 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9403-L9412] |
| 433 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9413-L9422] |
| 434 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9423-L9432] |
| 435 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9433-L9442] |
| 436 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9443-L9452] |
| 437 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9453-L9462] |
| 438 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9463-L9472] |
| 439 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9473-L9482] |
| 440 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9483-L9493] |

#### irregular-frequency-treatment

In-package rows: 1; repository-wide rows: 31. Every repository-wide row is enumerated below by its one-based ordinal within this family and a map-source citation; external locators remain in the map to avoid repeating identifying discovery paths. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13256-L13592]

| In-package definition | Map classification | Code evidence |
| --- | --- | --- |
| `GDFHandler.write` | independent in map; vocabulary false positive, geometry writer only | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L178-L230] |

Complete map-row enumeration (ordinal locates the implementation, and the citation resolves its full path, symbol and relation):

| Map row | Pinned map evidence |
| --- | --- |
| 1 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13263-L13272] |
| 2 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13273-L13282] |
| 3 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13283-L13292] |
| 4 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13293-L13302] |
| 5 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13303-L13312] |
| 6 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13313-L13322] |
| 7 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13323-L13332] |
| 8 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13333-L13342] |
| 9 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13343-L13352] |
| 10 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13353-L13362] |
| 11 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13363-L13372] |
| 12 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13373-L13382] |
| 13 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13383-L13392] |
| 14 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13393-L13402] |
| 15 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13403-L13412] |
| 16 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13413-L13422] |
| 17 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13423-L13434] |
| 18 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13435-L13444] |
| 19 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13445-L13454] |
| 20 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13455-L13467] |
| 21 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13468-L13477] |
| 22 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13478-L13487] |
| 23 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13488-L13497] |
| 24 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13498-L13507] |
| 25 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13508-L13517] |
| 26 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13518-L13527] |
| 27 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13528-L13537] |
| 28 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13538-L13547] |
| 29 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13548-L13557] |
| 30 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13558-L13567] |
| 31 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13568-L13577] |

### Exact signal audit

Families rank by in-package rows: RAO readers/converters (10), text-model writers (1), irregular-frequency treatment (1); the tied families retain map order. Python `re.search` is case-sensitive on every package `.py` line at the code pin; hits are assigned to the narrowest enclosing function/class, with module-level statements combined. A signal is omitted if its exact pattern matches `hydrodynamics/bemrosetta` under `re.IGNORECASE`. The original pattern strings are retained verbatim below. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4]

#### Signal set: rao-readers-and-orcaflex-converters

- used, algorithm: `\.lis\b|\.hyd\b|\.owr\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\.content|_table_span|parse_aqwa_lis_file|AQWALISParser`
- used, constants_or_keys: `ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection`
- omitted, vocabulary: `\bRAO\b|\braos?\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine`

| Hit definition | Classification | Evidence |
| --- | --- | --- |
| `_export_raos_csv` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L181-L210] |
| `validate` | Computes/parses directly; already listed in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L292-L389] |
| `OrcaFlexConverter._build_vessel_yaml` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L172-L206] |
| `ParserInterface.supported_extensions` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L51-L56] |
| `DATHandler.read` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L46-L72] |
| `DATHandler._parse_aqwa_deck` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L221-L306] |
| `AQWAParser.supported_extensions` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L58-L60] |
| `AQWAParser._extract_frequencies_and_periods` | Computes/parses directly; already listed in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L190-L237] |
| `AQWAParser._extract_added_mass` | Computes/parses directly; already listed in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L268-L313] |
| `BaseParser.supported_extensions` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py:L46-L51] |

#### Signal set: orcaflex-text-model-writers

- used, algorithm: `(?:yaml\.(?:dump|safe_dump)|write_text|open\(|\.write\(|save\(|SaveData|dump\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\.write\(|\.ya?ml|yaml\.safe_dump|shutil\.copy|\.write\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\.safe_dump|\.write_text`
- used, vocabulary: `orcaflex|OrcaFlex|includefile|modular_generator`
- used, constants_or_keys: `[\"\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\"\']|%YAML`

| Hit definition | Classification | Evidence |
| --- | --- | --- |
| `<module>` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L1-L162] |
| `get_module_info` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L149-L162] |
| `cli` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L16-L22] |
| `<module>` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L1-L584] |
| `convert` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L51-L120] |
| `_export_added_mass_csv` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L123-L149] |
| `_export_damping_csv` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L152-L178] |
| `_export_raos_csv` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L181-L210] |
| `_export_qtf_csv` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L213-L237] |
| `status` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L547-L575] |
| `<module>` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py:L1-L19] |
| `BaseConverter.output_format` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L35-L40] |
| `<module>` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L1-L319] |
| `OrcaFlexConverter` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L23-L281] |
| `OrcaFlexConverter.output_format` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L37-L39] |
| `OrcaFlexConverter.__init__` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L41-L59] |
| `OrcaFlexConverter.convert` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L70-L121] |
| `OrcaFlexConverter._call_orcaflex_exporter` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L144] |
| `OrcaFlexConverter.convert_to_vessel_yaml` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L146-L170] |
| `OrcaFlexConverter._build_vessel_yaml` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L172-L206] |
| `OrcaFlexConverter._export_qtf` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L208-L243] |
| `convert_to_orcaflex` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L284-L319] |
| `ConverterInterface` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L59-L102] |
| `ConverterInterface.output_format` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L97-L102] |
| `DATHandler.read` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L46-L72] |
| `DATHandler.write` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L350-L391] |
| `GDFHandler.read` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L36-L57] |
| `GDFHandler.write` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L178-L230] |
| `convert_mesh` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L243-L297] |
| `STLHandler._is_ascii_stl` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L77-L101] |
| `STLHandler._read_ascii_stl` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L103-L157] |
| `STLHandler._read_binary_stl` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L159-L219] |
| `STLHandler._write_ascii_stl` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L239-L276] |
| `STLHandler._write_binary_stl` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L278-L325] |
| `AQWAParser.parse` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] |
| `QTFParser.parse` | Prose/interface, serialiser, geometry helper or wrapper; covered in section 3 | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L43-L96] |

#### Signal set: irregular-frequency-treatment

- used, algorithm: `irregular_frequenc|irregularFrequency|IrregularFrequency|lid_mesh|lid_panels|generate_lid|lid_z|lid_faces|interior_surface|control_surface`
- used, vocabulary: `irregular.frequency|free.surface.panel|lid`
- used, constants_or_keys: `generate_lid|remove_irregular|lid_z|lid_faces|irregular_frequencies`

| Hit definition | Classification | Evidence |
| --- | --- | --- |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L1-L162] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L1-L584] |
| `cli` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L16-L22] |
| `convert` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L51-L120] |
| `validate` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L292-L389] |
| `validate_mesh` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L480-L543] |
| `status` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L547-L575] |
| `BaseConverter` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L13-L83] |
| `BaseConverter.validate_input` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L54-L75] |
| `OrcaFlexConverter.convert` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L70-L121] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/__init__.py:L1-L53] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L1-L294] |
| `ParserError` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L90-L128] |
| `ConverterError` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L131-L169] |
| `ValidationError` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L172-L211] |
| `ValidationError.__init__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L182-L211] |
| `MeshError` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L214-L253] |
| `ConverterInterface.validate_input` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L82-L93] |
| `ValidatorInterface` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L158-L185] |
| `ValidatorInterface.validate` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L166-L174] |
| `ValidatorInterface.is_valid` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L177-L185] |
| `ValidationReport` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L188-L228] |
| `ValidationReport.__init__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L199-L204] |
| `ValidationReport.add_error` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L206-L209] |
| `ValidationReport.set_metric` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L219-L221] |
| `ValidationReport.__repr__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L223-L228] |
| `DATHandler._parse_dat` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L83-L203] |
| `DATHandler.write` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L350-L391] |
| `GDFHandler._parse_gdf` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L59-L176] |
| `GDFHandler.write` | Inventoried definition | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L178-L230] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L1-L297] |
| `BaseMeshHandler` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L19-L240] |
| `BaseMeshHandler.validate_mesh` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L50-L99] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L1-L325] |
| `STLHandler._is_ascii_stl` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L77-L101] |
| `STLHandler._read_ascii_stl` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L103-L157] |
| `STLHandler._read_binary_stl` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L159-L219] |
| `STLHandler._write_ascii_stl` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L239-L276] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/__init__.py:L1-L27] |
| `PanelMesh._compute_normals` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L102-L130] |
| `PanelMesh._compute_areas` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L132-L159] |
| `PanelMesh._compute_centers` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L161-L173] |
| `MeshQualityReport` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L205-L281] |
| `MeshQualityReport.add_error` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L250-L253] |
| `MeshQualityReport.to_dict` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L259-L281] |
| `QTFComponent.__post_init__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L45-L60] |
| `QTFComponent.get_mean_drift` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L67-L78] |
| `BEMSolverMetadata.__post_init__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L77-L89] |
| `AQWAParser.parse` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py:L1-L112] |
| `BaseParser._validate_file_exists` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py:L85-L98] |
| `QTFParser.parse` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L43-L96] |
| `QTFParser._extract_headings` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L236-L263] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/__init__.py:L1-L20] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L1-L185] |
| `CausalityChecker` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L21-L172] |
| `CausalityChecker.validate` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L50-L89] |
| `CausalityChecker.is_valid` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L91-L101] |
| `check_causality` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L175-L185] |
| `<module>` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L1-L245] |
| `CoefficientValidator` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L20-L224] |
| `CoefficientValidator.__init__` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L36-L54] |
| `CoefficientValidator.validate` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L56-L95] |
| `CoefficientValidator.is_valid` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L97-L107] |
| `CoefficientValidator._validate_added_mass` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L109-L145] |
| `CoefficientValidator._validate_damping` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L147-L181] |
| `CoefficientValidator._validate_raos` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L183-L224] |
| `validate_coefficients` | Not a treatment algorithm: substring in validation/solid/geometry or prose | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L227-L245] |

The audit found 0 further unlisted implementations after section 3 coverage; raw definition hits were 10, 36 and 68 respectively. The irregular-frequency vocabulary `lid` also matches `valid` and `solid`; it establishes no lid-treatment capability here. [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L210-L228] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L249-L274]

### Repository search and map follow-up

Whole-repository discovery, including `scripts/`, uses `git grep -n -I -E 'AQWAParser|QTFParser|class .*LISParser|def .*rao|def .*gdf|kramers|Kramers' <code-pin> -- '*.py'`; external implementations are map follow-up, not additional in-package misses. The adjacent `AQWALISParser` implements separate listing extraction and `OrcaFlexExporter` implements the delegated output path. [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L1-L36] [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L1-L45]

Map follow-up: native QTF parsing, complex QTF export, mesh geometry/quality algorithms and K-K screening lack dedicated families with in-package rows in the selected map; propose those families before counting them. Correct the GDF writer's irregular-frequency classification and distinguish wrappers/status functions from independent result computations; issue pending (7). [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L265-L411] [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L103-L172]

### Upstream, downstream and solver-name dependency census

The upstream Python libraries used directly are NumPy, SciPy, PyYAML, Click and Loguru; diffraction schemas/exporter form the main package dependency; the optional executable is BEMRosetta rather than a solver import in this package. [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L9-L19] [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L8-L18] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L7-L11] [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L19-L26]

`BEMSolverType` names AQWA, WAMIT, NEMOH, OrcaWave, HAMS and Capytaine; this is metadata vocabulary, not evidence of six native readers. `MeshFormat.MSH` names Gmsh but convert_mesh implements only GDF/DAT/STL. [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L13-L22] [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L15-L23] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L278-L288]

Every package source/README line containing OrcaFlex, OrcaWave, AQWA, Capytaine or Gmsh is enumerated below as a textual dependency occurrence; ranges are grouped only when adjacent. No solver version compatibility is inferred from these names. [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L13-L22]

| File | Named dependency | Occurrence evidence |
| --- | --- | --- |
| `src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L3-L3] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L11-L11] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L29-L29] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L44-L45] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L52-L52] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L87-L87] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L194-L194] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L295-L295] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L298-L298] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L302-L302] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L309-L309] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L319-L319] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L443-L443] |
| `src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L3-L3] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L8-L8] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L28-L28] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L33-L34] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L52-L52] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L80-L80] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L94-L94] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L114-L114] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L193-L193] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L199-L199] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L202-L202] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L204-L204] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L303-L303] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L310-L310] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L335-L335] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L391-L391] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L394-L394] [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L441-L441] |
| `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L5-L5] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L11-L11] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L19-L19] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L27-L28] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L77-L77] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L132-L133] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L160-L160] |
| `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L5-L5] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L8-L8] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L18-L18] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L23-L24] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L74-L74] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L127-L128] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L159-L159] |
| `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L19-L19] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L30-L31] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L59-L59] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L66-L66] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L83-L83] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L93-L93] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L564-L564] |
| `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L19-L19] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L59-L59] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L65-L65] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L70-L70] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L72-L72] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L79-L79] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L243-L243] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L248-L248] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L251-L251] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L299-L299] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L303-L303] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L560-L560] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L568-L568] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py:L5-L5] [src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py:L13-L13] [src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py:L17-L18] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L39-L39] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L1-L1] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L3-L3] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L16-L16] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L23-L24] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L26-L26] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L39-L39] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L47-L47] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L75-L75] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L77-L77] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L103-L103] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L105-L105] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L118-L118] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L120-L120] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L123] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L126-L126] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L132-L132] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L153-L153] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L177-L177] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L179-L179] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L209-L209] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L211-L211] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L229-L229] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L284-L284] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L289-L289] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L299-L299] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L309-L309] [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L316-L316] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L204-L204] |
| `src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py` | OrcaFlex; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L63-L63] [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L101-L101] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/__init__.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/__init__.py:L5-L5] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L1-L1] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L11-L11] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L17-L17] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L34-L34] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L67-L70] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L75-L76] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L221-L221] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L224-L224] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L244-L244] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L287-L287] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L293-L293] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L298-L298] [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L314-L314] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L19-L19] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py` | Gmsh; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L21-L21] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py` | OrcaWave; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L19-L19] [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L102-L102] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L16-L16] [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L100-L100] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py` | Capytaine; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L21-L21] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py:L5-L5] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py:L12-L12] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py:L17-L18] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L2-L2] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L4-L4] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L23-L24] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L45-L46] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L48-L48] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L65-L65] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L68] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L71-L71] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L74-L74] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L82-L82] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L111-L111] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L126-L126] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L135-L135] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L147-L147] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L156-L157] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L183-L183] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L193-L193] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L240-L240] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L269-L269] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L316-L316] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L358-L358] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L360-L360] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L392-L392] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py:L59-L59] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py` | AQWA; format, interface or metadata reference | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L30-L30] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L41-L41] [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L85-L85] |

## 10. Coverage and open items

### Read coverage

All package Python files, the module README, the ten test modules, test configuration and fixture data were inspected; the lists below define the read boundary and test evidence, without adopting README maturity/count claims. [src/digitalmodel/hydrodynamics/bemrosetta/MODULE_README.md:L371-L386]

| Package file read | Evidence |
| --- | --- |
| `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L1-L162] |
| `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L1-L584] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/converters/__init__.py:L1-L19] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py` | [src/digitalmodel/hydrodynamics/bemrosetta/converters/base.py:L1-L83] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L1-L319] |
| `src/digitalmodel/hydrodynamics/bemrosetta/core/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/core/__init__.py:L1-L53] |
| `src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py` | [src/digitalmodel/hydrodynamics/bemrosetta/core/exceptions.py:L1-L294] |
| `src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py` | [src/digitalmodel/hydrodynamics/bemrosetta/core/interfaces.py:L1-L228] |
| `src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py` | [src/digitalmodel/hydrodynamics/bemrosetta/core/runner.py:L1-L261] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/__init__.py:L1-L23] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py` | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L1-L391] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py` | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L1-L230] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py` | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/mesh_handler.py:L1-L297] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py` | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/stl_handler.py:L1-L325] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/models/__init__.py:L1-L27] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/conversion_result.py` | [src/digitalmodel/hydrodynamics/bemrosetta/models/conversion_result.py:L1-L172] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py` | [src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py:L1-L281] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py` | [src/digitalmodel/hydrodynamics/bemrosetta/models/qtf_data.py:L1-L217] |
| `src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py` | [src/digitalmodel/hydrodynamics/bemrosetta/models/solver_metadata.py:L1-L177] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/__init__.py:L1-L20] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L1-L515] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py` | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/base.py:L1-L112] |
| `src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py` | [src/digitalmodel/hydrodynamics/bemrosetta/parsers/qtf_parser.py:L1-L411] |
| `src/digitalmodel/hydrodynamics/bemrosetta/validators/__init__.py` | [src/digitalmodel/hydrodynamics/bemrosetta/validators/__init__.py:L1-L20] |
| `src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py` | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L1-L185] |
| `src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py` | [src/digitalmodel/hydrodynamics/bemrosetta/validators/coefficient_validator.py:L1-L245] |

| Test file read | Baseline result | Evidence |
| --- | --- | --- |
| `tests/hydrodynamics/bemrosetta/__init__.py` | 0 passed test nodes | [tests/hydrodynamics/bemrosetta/__init__.py:L1-L1] |
| `tests/hydrodynamics/bemrosetta/conftest.py` | 0 passed test nodes | [tests/hydrodynamics/bemrosetta/conftest.py:L1-L143] |
| `tests/hydrodynamics/bemrosetta/test_cli.py` | 49 passed test nodes | [tests/hydrodynamics/bemrosetta/test_cli.py:L1-L638] |
| `tests/hydrodynamics/bemrosetta/test_converters.py` | 26 passed test nodes | [tests/hydrodynamics/bemrosetta/test_converters.py:L1-L602] |
| `tests/hydrodynamics/bemrosetta/test_exceptions.py` | 28 passed test nodes | [tests/hydrodynamics/bemrosetta/test_exceptions.py:L1-L352] |
| `tests/hydrodynamics/bemrosetta/test_integration.py` | 52 passed test nodes | [tests/hydrodynamics/bemrosetta/test_integration.py:L1-L809] |
| `tests/hydrodynamics/bemrosetta/test_interfaces.py` | 28 passed test nodes | [tests/hydrodynamics/bemrosetta/test_interfaces.py:L1-L391] |
| `tests/hydrodynamics/bemrosetta/test_mesh.py` | 39 passed test nodes | [tests/hydrodynamics/bemrosetta/test_mesh.py:L1-L652] |
| `tests/hydrodynamics/bemrosetta/test_models.py` | 44 passed test nodes | [tests/hydrodynamics/bemrosetta/test_models.py:L1-L724] |
| `tests/hydrodynamics/bemrosetta/test_parsers.py` | 40 passed test nodes | [tests/hydrodynamics/bemrosetta/test_parsers.py:L1-L520] |
| `tests/hydrodynamics/bemrosetta/test_runner.py` | 18 passed test nodes | [tests/hydrodynamics/bemrosetta/test_runner.py:L1-L277] |
| `tests/hydrodynamics/bemrosetta/test_validators.py` | 34 passed test nodes | [tests/hydrodynamics/bemrosetta/test_validators.py:L1-L842] |

| Documentation discovery file read | Evidence window naming the module |
| --- | --- |
| `docs/plans/2026-06-29-cfd-capability-assessment.md` | [docs/plans/2026-06-29-cfd-capability-assessment.md:L74-L76] |
| `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` | [docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md:L35-L37] |
| `docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md` | [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L13-L15] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L18-L22] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L8-L10] |
| `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` | [docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md:L36-L42] |
| `docs/plans/reviews/2319/code-review.md` | [docs/plans/reviews/2319/code-review.md:L18-L20] |
| `docs/research/hydrodynamics-industry-practice.md` | [docs/research/hydrodynamics-industry-practice.md:L23-L168] |
| `docs/capability-map/capability-report-2026-02-14.md` | [docs/capability-map/capability-report-2026-02-14.md:L88-L90] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | [docs/domains/project-docs/MODULE_SURVEY.md:L38-L40] |
| `docs/domains/orcawave/LICENSED_E2E_ACCEPTANCE.md` | [docs/domains/orcawave/LICENSED_E2E_ACCEPTANCE.md:L7-L9] |
| `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md` | [docs/domains/hull_library/hullprod-curvature-screening-evaluation.md:L172-L174] |
| `docs/domains/hull_library/hull_scaling_guide.md` | [docs/domains/hull_library/hull_scaling_guide.md:L147-L148] |
| `docs/domains/ship-design/software/intro.md` | [docs/domains/ship-design/software/intro.md:L3-L10] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L402-L404] |
| `docs/domains/BEMRosetta/README.md` | [docs/domains/BEMRosetta/README.md:L1-L2] |
| `docs/domains/hydrodynamics/bemrosetta/README.md` | [docs/domains/hydrodynamics/bemrosetta/README.md:L1-L13] |
| `docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md` | [docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md:L4-L8] |

### Execution record

BASE and AFTER use `PYTHONPATH=src uv run --no-sources python -m pytest tests/hydrodynamics/bemrosetta -q -rA -p no:cacheprovider --basetemp=<scratch>` with the configured `addopts` unchanged. Each run collected 358, passed 358, failed 0, skipped 0 and NOT_RUN 0 at the header code revision; AFTER runs on the uncommitted documentation tree. The section-4 test-node results are taken from these runs. The 682-test inventory lead is not reproduced as a measured count. [tests/hydrodynamics/bemrosetta/test_integration.py:L675-L809]

### Open-item register

The maintainer is responsible for issue creation and assignment; draft markers denote issue text retained outside the repository, and acceptance of issue numbering remains pending until numbers replace them. (AQG r0, section 11.)

| Item | Coverage limit or required follow-up | Tracking |
| --- | --- | --- |
| O1 | UNVERIFIED: target-version loading of written mesh and coefficient outputs; no licensed program or interface run; C10–C12 | #610; issue pending (6) |
| O2 | UNVERIFIED: independent BEM coefficient benchmark and complete hydrostatics/irregular-frequency checks; elementary areas establish geometry only; C42–C43 | #608; #141 |
| O3 | UNVERIFIED: source axes/origins, wave-heading sense, RAO phase sign and dimensional normalisation of mixed matrices/QTF; C02 | #1582; issue pending (3) |
| O4 | QA appendix, shared lesson registration, missing program/version records and review assignment; no clearance asserted; C00/C50 | issue pending (6) |
| O5 | Zero-filled QTF fallback, absent components, rectangular axes and parse completeness; section 6 locations | issue pending (1) |
| O6 | Malformed/short record handling, unsupported numeric forms, geometry count reconciliation and missing-data export; bounded listing windows | #2107; issue pending (2) |
| O7 | Metadata depth/unit/header/symmetry preservation and convention audit | issue pending (3) |
| O8 | Warning-only validators, K-K limitations, mesh validity flags and unused CLI flags | #608; issue pending (4) |
| O9 | README/API mismatch, CLI format behaviour, status/availability and output-provenance contracts | issue pending (5) |
| O10 | Map family additions, false-positive classifications and remaining external alternatives; external identifying discovery locators remain in the pinned map | issue pending (7) |

No private sidecar was read or created; this pack uses synthetic test inputs and contains no client-derived values. [tests/hydrodynamics/bemrosetta/test_mesh.py:L49-L58] [tests/hydrodynamics/bemrosetta/conftest.py:L103-L126]
