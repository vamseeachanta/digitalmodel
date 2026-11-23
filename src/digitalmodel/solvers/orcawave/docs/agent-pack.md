# OrcaWave legacy diffraction helpers — module knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcawave |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/orcawave` |
| Pack revision | 6; 2026-10-25 |
| Code revision | `7aad603be1c6136106dbb2381ce5096b050654f8` |
| Analysis family | Diffraction; registry code DF |
| Quality role | Diffraction quality manager; deputy: diffraction quality manager on another provider |
| Status | DRAFT; clearance pending |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; recorded data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |

Revision 1 adds solver-free regression tests and an empty-CSV refusal. The code
revision above remains the historical audit pin for unqualified line citations
and observed failures; current behavior is identified explicitly below. The
regressions use synthetic inputs only and establish no solver qualification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-18 | Added solver-free gates and empty-CSV refusal to expose missing-data failures. |
| 2 | 2026-10-18 | Required CSV grids and a supported coefficient file; tightened evidence checks and retained unresolved limitations. |
| 3 | 2026-10-19 | Rejected absent requested regular geometry files before worker creation; retained partial-input, parser and topology limitations. |
| 4 | 2026-10-20 | Recorded complete requested-input refusal, atomic parser results, edge-closure and QA-state repairs; retained the historical audit pin and unresolved engineering qualifications. |
| 5 | 2026-10-20 | Bound the 100-test result to revision 64f5380d, which includes the submission-failure regressions. |
| 6 | 2026-10-25 | Required worker termination and joining before sequential retry after partial startup; added real-executor regressions for fork, spawn and forkserver. |

## 1. Purpose and scope

The orchestrator sequences geometry setup, a batch analysis, result conversion, quality checks and packaging; the domain guide identifies this as the historical workflow and directs new work to the separate canonical diffraction CLI. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L138-L182] [docs/domains/orcawave/README.md:L202-L209]

This pack covers the package initializer, orchestrator, geometry validator, converter, batch script and configuration templates; it does not qualify the canonical CLI, reporting package or their numerical methods. [src/digitalmodel/solvers/orcawave/__init__.py:L1-L13] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L32-L69] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L44-L73] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L44-L49] [docs/domains/orcawave/README.md:L28-L46]

The existing `solvers` domain is beta; the separate `orcawave` domain is also beta and routes to a different package. [docs/registry/module-routing.yaml:L249-L254] [docs/registry/module-routing.yaml:L303-L320]

UNVERIFIED: supported solver versions, geometry envelope, water-depth envelope and the README's production suitability have not been established by independent examples; see section 10, issue pending (1).

## 2. Inputs and outputs

Allowed ranges below distinguish implemented validation from comments; an unguarded input is not an approved engineering range. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L127-L136] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L75-L120] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L113-L213]

| Input | Unit / convention | Implemented range or shape | Default / source | Evidence |
| --- | --- | --- | --- | --- |
| Configuration YAML | Mixed; comments label depth m, density kg/m³, gravity m/s² | `safe_load`; no schema or duplicate-key rejection here | Base template unless an explicit configuration or dataset selector is supplied | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L35-L62] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L127-L136] [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L5-L18] |
| Dataset override | Identifier and paths; no physical unit | Recursively replaces only keys already in the base | Missing override falls back to base; template uses `Template.safe_substitute` | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L71-L125] |
| Geometry path | Native coordinates assumed m by report labels | Binary STL, ASCII STL or OBJ; discovery hard-codes three filenames | Missing path accepted by orchestrator; validator accepts explicit path | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L354-L370] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L51-L105] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L329-L347] |
| HDF5 | Frequency rad/s and direction deg are assigned on export | Required frequency, direction, 6-by-6 matrix and real/imaginary excitation datasets; no unit attributes checked | First matching HDF5 preferred; physical attributes have defaults | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L52-L120] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L305-L313] |
| CSV | Frequency and direction copied without rescaling | Separate frequency, direction, matrix component and excitation files; excitation reshaped frequency first, then direction | Synthetic grids, zero coefficient arrays and physical-property defaults when files are absent | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L122-L205] |
| Excel | Same export labels; source units unchecked | Optional sheets; integer frequency indices; named matrix columns | Synthetic grids, zeros and physical-property defaults | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L207-L284] |
| Hydrodynamic arrays | Six degrees of freedom; mixed matrix units require a declared basis | Matrices `(n_frequency,6,6)`; excitation `(n_frequency,n_direction,6)` complex | `VesselData` holds arrays without shape/unit validation | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L27-L42] |

| Output | Unit / convention | Range / destination | Default / source | Evidence |
| --- | --- | --- | --- | --- |
| Generated configuration | Base template units; no solver schema qualification | Intermediate and merged YAML beside templates | Dataset selector creates files before workflow execution | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L91-L116] |
| Mesh statistics and validation report | Volume m³, area m², coordinates m in labels | Results keyed by discovered filename; text plus JSON | CLI writes beneath package results, including an empty result mapping | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L329-L350] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L457-L507] |
| OrcaFlex vessel-type YAML | Mass labelled tonnes in summary; frequencies rad/s; headings deg | Full and compact structures; no target-program load performed | Main sets dataset name in data, while writer retains its independent filename default | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L465] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L475-L480] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L523-L533] |
| Matrix export | No source-to-output unit conversion | Upper triangular coefficients; compact arrays have 21 entries per frequency | Missing compact coefficients become zero | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L346-L368] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L440-L465] |
| Wave-load export | Complex magnitude; angle degrees; exported phase convention Lag | Translational components use force; rotational components use first three moment components | Export sets direction Coming from, origin zero, x along first axis and z along third axis | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L332-L340] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L370-L400] |
| Workflow report and package | Phase flags, elapsed minutes, file lists | Root-level glob copies only; results directory is dataset-specific after setup | Fixed heading count in final report; plotting function is a placeholder | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L295-L317] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L410-L430] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L471-L483] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L545-L585] |

UNVERIFIED: source force, moment, matrix and mesh length units, source phase sign, source heading convention and coordinate-origin compatibility are not established by the loaders; output labels alone cannot establish them; issue pending (2).

## 3. Method and governing standards

| Method / definition | Implemented operation | Qualification boundary | Evidence |
| --- | --- | --- | --- |
| `OrcaWaveOrchestrator._create_vessel_config`, `_merge_configs`, `_load_config`, `list_available_vessels` | `Template.safe_substitute`, recursive existing-key merge, YAML read, identifier discovery | Configuration helpers; no diffraction equations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L71-L136] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L650-L666] |
| `_phase2_orcawave_execution` | Subprocess launch of batch script; dry run sets execution flag | Actual solver launch; no solver execution performed for this pack | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L208-L249] |
| `_validate_geometry_files`, `_convert_to_orcaflex` | Subprocess delegation to local scripts | Wrappers, not independent geometry/conversion methods | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L343-L408] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L433-L469] |
| `GeometryValidator._validate_stl_binary`, `_validate_stl_ascii`, `_validate_obj` | Binary little-endian triangles, floating-text vertices, integer OBJ indices | Polygon statistics use only the first triangle; OBJ negative indices are decremented | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L113-L213] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L239-L254] |
| `GeometryValidator._compute_mesh_statistics` | Bounds, arithmetic vertex centroid, triangle cross-product area, signed tetrahedral volume then absolute value, edge-length ratio, duplicate positions, volume-based closure proxy, convex-hull waterline estimate | Centroid is not the centre of buoyancy; absolute volume conceals orientation; no independent hydrostatics validation | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L215-L309] |
| `GeometryValidator._assess_quality` | Additive score penalties clamped at zero | Heuristic score; no gate rejection linked to score | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L380-L399] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L475-L518] |
| `OrcaFlexConverter._load_from_hdf5`, `_load_from_csv`, `_load_from_excel` | Independently construct arrays and properties from each format | CSV and Excel are independent readers absent from the map inventory; explicitly inventoried here | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L75-L284] |
| `OrcaFlexConverter.convert_to_orcaflex` | Constructs vessel-type fields; displaced volume uses displacement divided by a fixed density assumption | Source units and target schema remain unqualified | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L344] |
| `OrcaFlexConverter._format_matrices`, `_format_load_raos` | Upper triangle export, complex absolute value and complex angle | Export transformations, not a hydrodynamic solver | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L346-L400] |
| `save_orcaflex_file`, `_save_compact_format` | YAML serialization; compact arrays omit the load-RAO table | Data export helpers; compact output is not equivalent to full output | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L465] |

UNVERIFIED: DNV-RP-C205, *Environmental conditions and environmental loads*, edition and clause title unresolved; the nearby benchmark notes name it without an edition or clause, and no implementing clause is established for these helpers; clause text not checked against the licensed copy; issue pending (1). [docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md:L266-L273]

No licensed-standard table values are used as acceptance limits in this pack; section 5 uses AQG r0 and distinguishes specified checks from executed results. (AQG r0, sections 4–10.)

## 4. Validated worked examples

Validated numerical examples: 0; the existing setup test performs path reporting and dependency imports without engineering-value assertions, so it is recorded only as a regression example. [tests/solvers/orcawave/diffraction/test_setup.py:L9-L101]

| Case | Independent source / expected value | Input | Tolerance | Existing test node | Offline result |
| --- | --- | --- | --- | --- | --- |
| R-SETUP, regression only | None; normal return and no unhandled exception; no numerical expected value | Test-local discovery paths and Python packages | 0 unhandled exceptions; no engineering tolerance | `tests/solvers/orcawave/diffraction/test_setup.py::test_setup` | PASS at BASE and AFTER on the code revision in the header; 1 run / 1 passed / 0 failed / 0 skipped / 0 NOT_RUN in each command; missing paths printed without failure |

The result row records the observed solver-free test; its path checks only print booleans and its missing-package handlers print warnings, so PASS does not establish geometry availability or a successful conversion. [tests/solvers/orcawave/diffraction/test_setup.py:L17-L53] [tests/solvers/orcawave/diffraction/test_setup.py:L71-L101]

Reproduction form: `uv run --no-project --no-build --with 'pytest>=7.4.3,<9' --with 'pytest-asyncio==0.21.1' --with numpy --with pandas --with pyyaml --with h5py --with scipy python -m pytest tests/solvers/orcawave/diffraction/ -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-after`; helper dependencies and the declared pytest/asyncio versions are supplied, without overriding repository addopts. [pyproject.toml:L164-L187] [pytest.ini:L49-L54]

The synthetic probes in section 5 are diagnostic checks, not additional validated section-4 examples. Revision 1 commits the six requested solver-free gates as repository test nodes; three defects remain strict expected failures. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L215-L309] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L122-L205] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L271-L286]

## 5. QA gates

Criteria in this section distinguish committed solver-free checks from qualified-data checks still NOT_RUN; a criterion is not a claim of a passing result. The family set is G0, G1, G4 and G5, with G2 and G3 inapplicable for diffraction. (AQG r0, sections 3–10.)

| Gate | Applicability and program route | Current evidence / limit |
| --- | --- | --- |
| G0 | Applicable: configuration, source arrays, mesh files and output provenance | Solver-free checks below; missing-data and unit qualification defects remain open. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L343-L408] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L75-L284] |
| G1 | Applicable to OrcaWave input and to both generated OrcaFlex vessel-type formats | OrcaWave: open generated input in the intended version and collect validation messages; OrcaFlex: import full and compact vessel types separately through the documented Vessel Types UI, save and compare populated fields. NOT_RUN: solver programs and licences unavailable; no program interface queried. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L65-L94] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L603-L611] |
| G1 AQWA | NOT_APPLICABLE: this package launches OrcaWave and serializes OrcaFlex data; no AQWA deck emitter is implemented here | The AQWA short hydrostatics route belongs to the separate canonical workflow. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L65-L78] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L344] [docs/domains/orcawave/README.md:L38-L46] |
| G2 | NOT_APPLICABLE: no statics model is solved by these diffraction helpers | Vessel-type export is not a complete mooring or installation model. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L344] (AQG r0, section 10.) |
| G3 | NOT_APPLICABLE: no time-domain dynamics model is solved here | The batch route is diffraction. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L65-L78] (AQG r0, section 10.) |
| G4 | Applicable: mesh, hydrostatic balance, centres, irregular frequencies and hydrodynamic output | Local score and closure proxies cannot substitute for the independent checks. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L215-L309] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L380-L399] (AQG r0, section 8.) |
| G5 | Applicable: evidence, fingerprints, provenance and gate dispositions | Final report reads workflow flags; no qualified appendix is established here. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L545-L585] (AQG r0, section 9.) |

| Check ID | Module-specific executable procedure and numeric pass criterion | Evidence / observed status |
| --- | --- | --- |
| G0-SYNTAX | Parse each package `.py` at the pin with `ast.parse`; criterion 0 syntax errors, unit: errors | PASS: all 4 Python files parse; no module execution required. [src/digitalmodel/solvers/orcawave/__init__.py:L1-L13] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L1-L740] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L1-L553] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L1-L521] |
| G0-CONFIG | Compare `OrcaWaveOrchestrator._load_config` and `_merge_configs` outputs with independently parsed configuration using duplicate-key rejection; expand substitutions; compare every override key and requested geometry file against the merged output; criterion 0 duplicate keys, 0 unresolved placeholders, 0 dropped requested fields, unit: records | NOT_RUN on a qualified dataset; `safe_load`, safe substitution and existing-key merge do not enforce these criteria. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L71-L136] |
| G0-GEOMETRY | Call `GeometryValidator.validate_all(parallel=False)` on an empty directory and require an explicit missing-input exception; for populated input compare result keys to requested filenames, requiring all 3 discovered files represented and 0 failed files, unit: files | Diagnostic FAIL at the pin: empty directory returns 0 records without raising; not an accepted production input. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L51-L105] |
| G0-COMPONENTS | Run `OrcaWaveOrchestrator._create_output_directories` then `_phase3_results_processing` with no result files; require False, `results_converted == False`, converter exit 1 and 0 exported files (0 fabricated channels). Direct `OrcaFlexConverter._load_from_csv` and `load_orcawave_results` shall raise for empty CSV, including when Excel is present, and for missing grids or a missing supported coefficient file. For nonempty CSV and Excel, compare every source grid/component before export; criterion 0 missing or fabricated channels | Revision 2 also refuses properties-only CSV, missing grids and sources without any supported coefficient file before array allocation. Explicit zero and nonzero values remain readable for added mass, damping and excitation force. Partial-channel CSV and Excel qualification remains deferred under #2738; at the historical pin empty input produced 100 frequencies by 13 headings of zero arrays. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L122-L284] |
| G0-UNITS | Before `convert_to_orcaflex`, compare source metadata with `FrequencyUnits`, `DirectionUnits`, density basis, force/moment units, phase and reference axes; criterion 0 unresolved unit or convention fields, unit: fields | NOT_RUN: source contracts absent; no authoritative conversion basis available. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L340] |
| G0-CONTROL | Assert that `gate-dispositions.json` covers exactly every section-6 package function/output mechanism ID, with a nonempty reason, explicit status and tracking issue; criterion 0 undisposed rows, 0 stale IDs and 0 duplicate IDs | `test_g0_control_dispositions` asserts the checked-in list; `test_g0_control_missing_disposition_is_rejected` proves omission fails. Deferred means unresolved, not qualified or accepted. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L179-L182] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L51-L105] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L122-L284] |
| G1-OW | Open the configuration and mesh selected by `OrcaWaveOrchestrator._phase2_orcawave_execution` in the intended solver version; criterion 0 load errors and 0 validation errors, unit: messages | NOT_RUN: licensed program unavailable; batch options and configuration compatibility unqualified. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L40-L78] (AQG r0, sections 5 and 10.) |
| G1-OF | Import `save_orcaflex_file` full output and `_save_compact_format` output separately in the target OrcaFlex version; criterion 0 load errors, 0 warnings, 0 missing required fields, unit: messages / fields | NOT_RUN: licensed program unavailable; compact output drops the load-RAO table and requires explicit qualification. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L465] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L603-L611] (AQG r0, section 5.) |
| G4-CLOSURE | Independently count boundary edges of the synthetic triangle with vertices (1,0,0), (0,1,0), (0,0,1) m; it has 3 boundary edges, so require `_compute_mesh_statistics(...).is_watertight == False`; criterion 0 false closure labels, unit: labels | Diagnostic FAIL at the pin: returned True; the volume-only proxy does not prove closure. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L239-L277] |
| G4-BALANCE | Compare `VesselData.displacement`, source density and independently integrated signed mesh volume; compare force balance to AQG, plus independent centres and GM; criterion balance error ≤1.0 %, volume error ≤0.5 %, horizontal centre error ≤0.5 % of waterline length, vertical error ≤1.0 % of draft, GM positive and error ≤2.0 % | NOT_RUN: qualified mesh, mass basis and solver hydrostatics unavailable; arithmetic vertex centroid is not a hydrostatic reference. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L27-L42] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L298-L303] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L226-L230] (AQG r0, sections 8.1–8.3.) |
| G4-MESH | Compare `GeometryValidator._compute_mesh_statistics` output and its source panels, signed orientation, waterline and frequency range to an independent mesh audit; criterion 0 zero-area panels, 0 inward panels, waterline error ≤1 mm, draft error ≤0.01 m, aspect ratio ≤3, panel extent ≤ shortest wavelength/6; record installed panel-count limit; lid treatment if range reaches 0.9 of first irregular frequency | NOT_RUN: no qualified mesh or target-version limit; local waterline band and absolute volume do not satisfy this check. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L239-L290] [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L20-L46] (AQG r0, sections 8.4–8.7.) |
| G4-RESPONSE | Compare exported matrices and complex load data with qualified source data before `_format_matrices` drops the lower triangle; criterion added-mass asymmetry ≤2.0 %, negative diagonal damping entries 0; separately require qualified displacement heave RAO within 2.0 % of 1 at longest period and no irregular-frequency spike within 5.0 % of estimated frequency | NOT_RUN: no qualified solver result; wave-load RAOs cannot stand in for displacement RAOs. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L346-L400] (AQG r0, section 8.8.) |
| G5-STATE | With a fresh object, empty results directory and `_check_data_completeness() == False`, call `_phase4_quality_assurance`; criterion 0 successful phase flags, unit: flags; also require 0 uncomputed physics or visualization PASS results | Diagnostic FAIL: phase returns True and sets `quality_checked`; three physics checks and visualization return True without computation. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L271-L286] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L471-L483] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L514-L542] |
| G5-PACKAGE | Compare required nested converter and validation outputs to `_phase5_packaging` inventory; require 0 omitted required files, unit: files | NOT_RUN: no qualified result set; root-only glob patterns do not visit nested conversion and validation outputs. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L295-L317] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L448-L454] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L457-L457] |
| G5-TRACE | Compare `OrcaWaveOrchestrator._generate_final_report` output against a recomputed input SHA-256 list and manifest fingerprint; require 0 mismatches and 0 result rows lacking a signed gate disposition, unit: digests / rows | NOT_RUN: no signed evidence record or qualified dataset available; final report lists files and flags rather than fingerprints. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L545-L585] (AQG r0, sections 4.7, 9 and 13.) |

The committed diagnostics load only the three audited helper files with `importlib.util`, avoiding unrelated solver initializers. A minimal synthetic configuration and temporary result directories are used. The phase-3 test runs only the Python converter subprocess and reads its exit status and output files; no batch script, execution phase, installation check or licensed interface is invoked. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L44-L49] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L47-L52] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L271-L286]

Committed checks (run with `python -m pytest tests/solvers/orcawave/diffraction/ -q -rA`):

| Gate / lint | Executable assertion | Disposition |
| --- | --- | --- |
| G0-SYNTAX | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g0_syntax` parses every package Python output into an AST | Required pass |
| G0-GEOMETRY | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g0_geometry_rejects_empty_directory` calls `GeometryValidator.validate_all`; populated-input cases exercise partial discovery and sequential/parallel parser failures | Required pass: every requested regular file is required and results are published only after all parsers succeed |
| G0-COMPONENTS | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g0_components_phase3_refuses_empty_output` asserts converter CLI and `_phase3_results_processing` refusal with 0 output files | Required pass; `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g0_components_properties_only_csv_rejected` and `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g0_components_missing_sources_rejected_before_allocation` cover incomplete sources |
| G0-CONTROL | `tests/solvers/orcawave/diffraction/test_pack_lint.py::test_g0_control_dispositions` asserts every package mechanism has a disposition | Required pass; omission and stale-reason negative controls |
| Source selection | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_format_selection_preserved` checks HDF5 before CSV and Excel, populated CSV before Excel, and Excel when neither higher-priority source exists | Required pass; source-reader stubs isolate routing from format parsing |
| Gate preconditions | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g4_open_triangle_preconditions` checks three boundary edges and volume of one sixth; `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g5_incomplete_results_precondition` checks incomplete inputs | Required pass; independent fixture preconditions |
| G4-CLOSURE | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g4_closure_rejects_open_triangle` asserts `_compute_mesh_statistics().is_watertight` is false | Required pass; edge incidence replaces the volume proxy, with duplicate and zero-area faces refused |
| G5-STATE | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_g5_state_refuses_incomplete_results` asserts `_phase4_quality_assurance` return and state flag are false | Required pass; stale success is cleared and uncomputed physics and visualization checks return false |
| Dependency citation | `tests/solvers/orcawave/diffraction/test_pack_lint.py::test_dependency_citation_content` asserts cited text contains the Python requirement and exact runtime requirements used by the helpers | Required pass; metadata-only negative control; `tests/solvers/orcawave/diffraction/test_pack_lint.py::test_reproduction_citation_content` checks the test-extra and pytest-setting tokens; `tests/solvers/orcawave/diffraction/test_pack_lint.py::test_all_citation_ranges_at_pins` bounds-checks every ranged citation at its stated revision |

Table. Solver-free gate tests; no strict expected failures remain. Software assertions do not establish engineering acceptance. Other qualified-data gates remain NOT_RUN.

Revision 4 binds G0-GEOMETRY to `GeometryValidator.validate_all`: absent or partial
requested regular-file sets raise `FileNotFoundError` before parsing or worker
creation, including when a prior result mapping exists. Parser failures propagate
and leave no partial or stale result mapping in sequential or parallel mode.
The presence tests cover missing directories, unrelated files, matching directory
names and complete requested sets. Process-pool construction or submission
`OSError` triggers sequential validation after pool shutdown; worker/parser errors
remain fatal. Submission can start workers lazily, so successful construction
alone does not establish pool availability. Repeated reads after a partial
submission are permitted because these parsers only read geometry files.
The two geometry-discovery log messages are retained for diagnostics.

Revision 6 requires workers started before a submission failure to be stopped
and joined before sequential parsing. Context-manager shutdown alone does not
reap workers if startup fails before the executor manager thread exists.
The repair snapshots the executor's worker registry before shutdown clears it;
this uses a private Python implementation detail where no public abort API is
available. Real-executor regressions inject failure at the second and third
process starts and assert no surviving workers or unjoined children before
sequential parsing. Normal parallel completion is also exercised for each
available start method: fork, spawn and forkserver on the tested interpreter.

The historical diagnostics and section-6 catalogue remain bound to the code pin
above; their repaired contracts are identified in
[the issue-2738 qualification note](issue-2738-qualification.html). The suite
passed 100 tests at `64f5380d` in the second-provider review (Python 3.12.3,
NumPy 1.26.4). The cleanup repair passed 109 tests on Python 3.11.17 and 3.12.15
with NumPy 1.26.4: 88 gate tests, 20 pack-lint tests and one setup test.
Closure is an edge-incidence criterion, not hydrostatic or native
solver qualification. The small-area heuristic, numerical physics, units,
caller bypasses, partial CSV/Excel channels, packaging and provenance remain open.

## 6. Known failure modes and pitfalls

sweep: 96 found

The count is the number of mechanism rows below, including fail-closed exceptions and benign initialization candidates explicitly retained for audit; repeated locations within one mechanism are grouped, and the Python sweep also has a manual batch-script and test supplement. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L179-L182] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L233-L237] [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L106-L116] [tests/solvers/orcawave/diffraction/test_setup.py:L17-L101]

Exact package search commands (Python regular-expression interpretation is used separately for the map audit in section 9):

```sh
rg -n -i --glob '*.py' 'except|\bcontinue\b|return None|return \[\]|return \{\}|\b(?:0(?:\.0)?|1970)\b|\b(?:int|float)\(|unit|assuming|assum|cache' src/digitalmodel/solvers/orcawave
rg -n --glob '*.py' 'zeros\(|\.get\(|return self.results|return True|if .*exists|if .*sheet_names|if .* in row' src/digitalmodel/solvers/orcawave
```

| ID | Symptom / cause at code pin | Detection in section 5 | Tracking / observation date |
| --- | --- | --- | --- |
| S001 | Missing selected override silently uses base configuration. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L77-L80] | G0-CONFIG | issue pending (3); 2026-10-16 |
| S002 | `Template.safe_substitute` preserves missing placeholders. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L91-L99] | G0-CONFIG | issue pending (3); 2026-10-16 |
| S003 | Override keys absent from base are ignored. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L118-L125] | G0-CONFIG | issue pending (3); 2026-10-16 |
| S004 | YAML load does not reject duplicate keys or enforce input units. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L127-L136] | G0-CONFIG / G0-UNITS | issue pending (3); 2026-10-16 |
| S005 | Workflow exception becomes False after logging and attempted state save; a failed state-save can replace the original error. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L179-L182] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S006 | Dry execution records solver-executed True. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L215-L218] | G5-STATE | issue pending (3); 2026-10-16 |
| S007 | Launch exception is logged and returns False; fail-closed, no swallowed success. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L247-L249] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S008 | Visualization failure is non-critical and conversion phase still completes. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L263-L269] | G5-STATE | issue pending (3); 2026-10-16 |
| S009 | Failed quality checks still set completed state and return True. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L278-L286] | G5-STATE | issue pending (3); 2026-10-16 |
| S010 | Absent executable emits warnings but installation check returns True; no licence validation is established. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L319-L341] | no detection check exists | issue pending (3); 2026-10-16 |
| S011 | Missing geometry setting bypasses validation successfully. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L354-L359] | G0-GEOMETRY | issue pending (3); 2026-10-16 |
| S012 | Nonexistent geometry directory bypasses validation successfully. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L367-L370] | G0-GEOMETRY | issue pending (3); 2026-10-16 |
| S013 | No validation JSON still returns success; statistics lookup is optional. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L395-L404] | G0-GEOMETRY | issue pending (3); 2026-10-16 |
| S014 | Geometry subprocess exception is logged and returns False; fail-closed. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L406-L408] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S015 | Cache directory is created; no empty-result cache write occurs in this block. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L410-L430] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S016 | Conversion subprocess exception is logged and returns False; fail-closed. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L467-L469] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S017 | Visualization placeholder emits success without producing plots. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L471-L483] | G5-STATE | issue pending (3); 2026-10-16 |
| S018 | Reciprocity placeholder returns True without a calculation. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L514-L517] | G5-STATE | issue pending (3); 2026-10-16 |
| S019 | Energy-conservation placeholder returns True without a calculation. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L519-L522] | G5-STATE | issue pending (3); 2026-10-16 |
| S020 | Asymptotic placeholder returns True without a calculation. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L524-L527] | G5-STATE | issue pending (3); 2026-10-16 |
| S021 | Missing start time becomes reported zero elapsed time. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L550-L550] | no detection check exists | issue pending (3); 2026-10-16 |
| S022 | Reported heading count is hard-coded rather than read from configured headings. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L567-L568] | no detection check exists | issue pending (3); 2026-10-16 |
| S023 | Absent configuration directory becomes an empty discovery list. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L658-L659] | G0-CONFIG | issue pending (3); 2026-10-16 |
| S024 | CLI exception is logged and returns failure status; fail-closed. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L735-L737] | G0-CONTROL | issue pending (3); 2026-10-16 |
| S025 | Root-only package copy omits nested validation and converter outputs. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L299-L310] | G5-PACKAGE | issue pending (3); 2026-10-16 |
| S026 | Selection order is HDF5 first, then an existing `csv_outputs/` path, then Excel; within HDF5 or Excel, the first filesystem match is used without sorting or ambiguity rejection. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L58-L71] | `tests/solvers/orcawave/diffraction/test_pack_gates.py::test_format_selection_preserved` checks HDF5, populated CSV with Excel present, and Excel selection; ambiguity rejection has no detection check | issue pending (2); 2026-10-16 |
| S027 | Missing HDF5 attribute supplies a displacement default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L98-L98] | G0-UNITS | issue pending (2); 2026-10-16 |
| S028 | Missing HDF5 attribute supplies a centre of gravity default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L99-L99] | G0-UNITS | issue pending (2); 2026-10-16 |
| S029 | Missing HDF5 attribute supplies a centre of buoyancy default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L100-L100] | G0-UNITS | issue pending (2); 2026-10-16 |
| S030 | Missing HDF5 attribute supplies a waterplane area default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L101-L101] | G0-UNITS | issue pending (2); 2026-10-16 |
| S031 | Missing HDF5 attribute supplies a metacentric height default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L102-L102] | G0-UNITS | issue pending (2); 2026-10-16 |
| S032 | Missing HDF5 attribute supplies a dataset identifier default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L103-L103] | no detection check exists | issue pending (2); 2026-10-16 |
| S033 | Missing frequency CSV manufactures a logarithmic grid. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L130-L133] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S034 | Missing direction CSV manufactures a heading grid. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L135-L137] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S035 | CSV added mass array starts at zero and missing components are not rejected. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L143-L143] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S036 | CSV damping array starts at zero and missing components are not rejected. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L144-L144] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S037 | CSV complex force array starts at zero and missing components are not rejected. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L145-L145] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S038 | CSV complex moment array starts at zero and missing components are not rejected. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L146-L146] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S039 | Absent added-mass component file leaves that coefficient zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L148-L154] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S040 | Absent damping component file leaves that coefficient zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L156-L162] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S041 | Absent excitation file leaves force zero; available rows assume frequency-major ordering; no separate moment file is read. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L164-L173] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S042 | CSV missing property file preserves a displacement default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L175-L175] | G0-UNITS | issue pending (2); 2026-10-16 |
| S043 | CSV missing property file preserves a centre of gravity default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L176-L176] | G0-UNITS | issue pending (2); 2026-10-16 |
| S044 | CSV missing property file preserves a centre of buoyancy default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L177-L177] | G0-UNITS | issue pending (2); 2026-10-16 |
| S045 | CSV missing property file preserves a waterplane area default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L178-L178] | G0-UNITS | issue pending (2); 2026-10-16 |
| S046 | CSV missing property file preserves a metacentric height default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L179-L179] | G0-UNITS | issue pending (2); 2026-10-16 |
| S047 | CSV property strings use library parsing without explicit locale, unit or finiteness validation. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L181-L189] | G0-UNITS | issue pending (2); 2026-10-16 |
| S048 | Missing Excel frequency sheet manufactures a grid. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L216-L218] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S049 | Missing Excel direction sheet manufactures headings. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L221-L223] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S050 | Excel added mass array starts at zero and absent sheets remain zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L230-L230] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S051 | Excel damping array starts at zero and absent sheets remain zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L231-L231] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S052 | Excel excitation array starts at zero and absent sheets remain zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L232-L232] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S053 | Excel added-mass index int conversion accepts numeric truncation; missing columns remain zero; locale and range not checked. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L235-L243] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S054 | Excel damping index int conversion accepts numeric truncation; missing columns remain zero; locale and range not checked. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L246-L254] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S055 | Excel initializes a displacement default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L258-L258] | G0-UNITS | issue pending (2); 2026-10-16 |
| S056 | Excel initializes a centre of gravity default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L259-L259] | G0-UNITS | issue pending (2); 2026-10-16 |
| S057 | Excel initializes a centre of buoyancy default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L260-L260] | G0-UNITS | issue pending (2); 2026-10-16 |
| S058 | Excel initializes a waterplane area default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L261-L261] | G0-UNITS | issue pending (2); 2026-10-16 |
| S059 | Excel initializes a metacentric height default. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L262-L262] | G0-UNITS | issue pending (2); 2026-10-16 |
| S060 | Excel property sheet updates only displacement, waterplane and GM; centres remain defaults. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L264-L269] | G0-UNITS | issue pending (2); 2026-10-16 |
| S061 | Excel excitation is not populated and moment is aliased to force as an approximation. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L273-L277] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S062 | Mass-to-volume conversion assumes a fixed seawater density independent of configured density. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L298-L303] | G0-UNITS / G4-BALANCE | issue pending (2); 2026-10-16 |
| S063 | Frequency and direction units are assigned without source-unit conversion. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L305-L313] | G0-UNITS | issue pending (2); 2026-10-16 |
| S064 | Export fixes direction, phase and reference origin/axes without checking source conventions. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L332-L339] | G0-UNITS | issue pending (2); 2026-10-16 |
| S065 | Only upper matrix triangle survives; float conversions do not reject nonfinite data or expose asymmetry. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L357-L364] | G4-RESPONSE | issue pending (2); 2026-10-16 |
| S066 | Rotational components use moment indices offset by three; complex magnitude/angle and float conversion assume a compatible source basis. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L385-L395] | G0-UNITS / G4-RESPONSE | issue pending (2); 2026-10-16 |
| S067 | Writer default filename is independent of data name; CLI changes data name without passing filename. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L405-L406] | no detection check exists | issue pending (2); 2026-10-16 |
| S068 | Missing compact added-mass key becomes zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L456-L456] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S069 | Missing compact damping key becomes zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L457-L457] | G0-COMPONENTS | issue pending (2); 2026-10-16 |
| S070 | Compact export omits load RAOs and other full-output fields. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L426-L465] | G1-OF | issue pending (2); 2026-10-16 |
| S071 | Report labels source values as tonnes, square metres, metres, radians per second and degrees without unit validation. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L475-L490] | G0-UNITS | issue pending (2); 2026-10-16 |
| S072 | Conversion CLI exception is logged and returns status 1; fail-closed. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L546-L548] | G0-CONTROL | issue pending (2); 2026-10-16 |
| S073 | Parallel discovery logs missing geometry and skips submission. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L73-L74] | G0-GEOMETRY | issue pending (4); 2026-10-16 |
| S074 | Per-future exception is logged; failed geometry is omitted from result mapping. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L83-L84] | G0-GEOMETRY | issue pending (4); 2026-10-16 |
| S075 | Process-pool error falls back to sequential validation; caller receives no degraded-mode status. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L85-L87] | G0-CONTROL | issue pending (4); 2026-10-16 |
| S076 | Sequential parse exception or absent file is logged; result omitted. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L96-L99] | G0-GEOMETRY | issue pending (4); 2026-10-16 |
| S077 | Binary STL assumes little-endian counts and floating coordinates; no embedded length unit is read. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L127-L145] | G0-UNITS | issue pending (4); 2026-10-16 |
| S078 | ASCII STL float conversion uses whitespace-delimited native numeric syntax; incomplete final vertex group is discarded. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L166-L176] | no detection check exists | issue pending (4); 2026-10-16 |
| S079 | OBJ float/int conversions do not define locale handling; negative indices are decremented as if positive. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L194-L203] | no detection check exists | issue pending (4); 2026-10-16 |
| S080 | Edge count is triangle-based approximation; centroid is an unweighted vertex mean. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L221-L230] | G4-CLOSURE / G4-BALANCE | issue pending (4); 2026-10-16 |
| S081 | Zero volume/area/extrema and infinity minimum are accumulators, not missing-data defaults; empty or sub-triangular faces leave degenerate statistics. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L233-L237] | G4-MESH | issue pending (4); 2026-10-16 |
| S082 | Faces with fewer than three vertices are ignored; polygons use only their first triangle. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L239-L254] | G4-MESH | issue pending (4); 2026-10-16 |
| S083 | Zero-length edge skips aspect ratio evaluation and leaves earlier maximum. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L262-L264] | G4-MESH | issue pending (4); 2026-10-16 |
| S084 | Absolute volume conceals reversed orientation. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L267-L267] | G4-MESH | issue pending (4); 2026-10-16 |
| S085 | Position duplicates are flagged without topology context; nonzero volume is used as closure proxy. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L272-L277] | G4-CLOSURE | issue pending (4); 2026-10-16 |
| S086 | Waterline assumes z=0 and a fixed broad coordinate band; no unit scaling. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L279-L280] | G4-MESH / G0-UNITS | issue pending (4); 2026-10-16 |
| S087 | Convex-hull exception is swallowed and waterline area becomes None. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L285-L288] | no detection check exists | issue pending (4); 2026-10-16 |
| S088 | No waterline vertices yields None without a rejection status. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L289-L290] | G4-MESH | issue pending (4); 2026-10-16 |
| S089 | Report assigns metre-based geometry units; absent or zero waterline area is omitted. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L329-L350] | G0-UNITS | issue pending (4); 2026-10-16 |
| S090 | Hard-coded score penalties and clamp are a heuristic, disconnected from a rejection threshold. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L380-L399] | G4-MESH | issue pending (4); 2026-10-16 |
| S091 | Repository discovery failure uses a fixed ancestor fallback. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L449-L451] | no detection check exists | issue pending (4); 2026-10-16 |
| S092 | Empty result mapping is serialized; float conversions of statistics are numeric serialization, not external-text parsing. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L488-L507] | G0-GEOMETRY | issue pending (4); 2026-10-16 |
| S093 | CLI exception is logged and returns status 1; fail-closed. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L511-L513] | G0-CONTROL | issue pending (4); 2026-10-16 |
| S094 | Conversion failure is warning-only; batch can finish with status zero. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L106-L116] | G5-STATE | issue pending (3); 2026-10-16 |
| S095 | Setup test checks relocated test-local paths, prints missing dependencies and returns success without assertions. [tests/solvers/orcawave/diffraction/test_setup.py:L17-L101] | no detection check exists | issue pending (5); 2026-10-16 |
| S096 | `_create_output_directories` creates an empty dataset `csv_outputs/` and changes `results_dir`; `_convert_to_orcaflex` passes that directory to `load_orcawave_results`, selecting CSV ahead of a supplied Excel workbook when no HDF5 is present. The batch writes to `..\results`, not the dataset directory. Revision 2 requires both grids and a supported coefficient file before allocation; Excel selection, the batch-path mismatch and partial channels remain unqualified. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L410-L430] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L448-L454] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L58-L71] [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L17-L17] [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L75-L75] | G0-COMPONENTS | #2738; 2026-10-18 |

The broad first expression also matches version literals, array subscripts, successful CLI exit codes and formatting indices; these are not defaults on an error path. The explicit zero-origin convention is recorded above, and the only cache operation in package Python is directory setup rather than a write of cached results. [src/digitalmodel/solvers/orcawave/__init__.py:L5-L13] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L337-L339] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L550-L553] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L518-L521] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L410-L430]

Read-only issue discovery confirmed #788 as a historical syntax/BOM incident mentioning the orchestrator; G0-SYNTAX now passes on all four package Python files. The existing #608 mesh-QA and #611 output-contract issues concern adjacent canonical implementations and are follow-up context, not evidence that this legacy implementation has been repaired. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L1-L32] [docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md:L1-L20] [docs/plans/2026-05-15-issue-611-orcawave-result-contract.md:L1-L20]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-16 | Initial source audit | A passing setup discovery test cannot qualify solver results; the test prints missing paths, and quality placeholders return True | [tests/solvers/orcawave/diffraction/test_setup.py:L17-L53] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L514-L527] | Regression boundary, explicit gates and failure catalogue added | Not assigned; issue pending (5) |

## 8. Ownership

The diffraction quality manager is the clearance role; its deputy is a diffraction quality manager on another provider. The producer documents evidence; the maintainer handles publication and cross-family scope. Any ambiguous criterion, unsupported standard clause or missing qualified input remains open for the diffraction quality manager, rather than being resolved by an assumed value. (AQG r0, section 11.)

UNVERIFIED: specific appointments for the quality manager and deputy, the shared-log entry and the clearance date have not been supplied; issue pending (1).

## 9. Dependencies and implementation inventory

Dependency names below are read as source text; no solver, program interface, installation check or licence check is executed. The batch file is the actual OrcaWave launch dependency; OrcaFlex is the converter's intended downstream consumer, and the documented minimum-version claim remains unqualified. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L40-L94] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L344] [src/digitalmodel/solvers/orcawave/diffraction/README.md:L155-L164]

| Dependency | Upstream / downstream role | Version / unit qualification | Evidence |
| --- | --- | --- | --- |
| Python, PyYAML, NumPy, pandas, h5py | Configuration, geometry and result formats | Python requirement and project runtime dependencies; no solver interface imported by these helpers | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L8-L20] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L7-L19] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L7-L16] [pyproject.toml:L10-L10] [pyproject.toml:L21-L71] |
| SciPy convex hull | Optional waterline-area computation inside mesh statistics | Hull failure becomes None | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L280-L290] |
| OrcaWave | Batch process launched by orchestrator | Installed version, command-line flags and generated input compatibility NOT_RUN | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L220-L249] [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L40-L94] |
| OrcaFlex | Full and compact YAML handoff | Target version and field acceptance NOT_RUN | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L465] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L603-L611] |
| Separate canonical diffraction CLI | Alternative workflow, not a numerical dependency of the legacy helpers | Kept outside this package's method inventory | [docs/domains/orcawave/README.md:L28-L46] [docs/domains/orcawave/README.md:L198-L205] |

### Solver-name occurrence inventory

Every Python-source line naming OrcaWave, OrcaFlex or OrcFxAPI is enumerated below, followed by the batch script and generic configuration mentions; citations identify the exact occurrence without copying dataset identifiers. [src/digitalmodel/solvers/orcawave/__init__.py:L1-L13] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L1-L740] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L1-L553] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L1-L521] [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L1-L154] [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L1-L137]

| Source | Solver dependency mention | Evidence |
| --- | --- | --- |
| `src/digitalmodel/solvers/orcawave/__init__.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/__init__.py:L2-L2] |
| `src/digitalmodel/solvers/orcawave/__init__.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/__init__.py:L6-L6] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L3-L3] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L32-L32] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L33-L33] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L66-L66] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L148-L148] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L156-L156] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L157-L157] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L191-L191] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L192-L192] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L193-L193] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L208-L208] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L209-L209] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L212-L212] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L216-L216] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L217-L217] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L240-L240] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L243-L243] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L244-L244] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L248-L248] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L258-L258] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L259-L259] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L260-L260] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L301-L301] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L319-L319] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L320-L320] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L322-L322] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L324-L324] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L325-L325] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L326-L326] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L327-L327] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L330-L330] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L331-L331] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L333-L333] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L334-L334] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L337-L337] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L338-L338] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L339-L339] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L419-L419] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L433-L433] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L434-L434] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L436-L436] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L438-L438] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L453-L453] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L464-L464] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L535-L535] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L555-L555] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L596-L596] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L597-L597] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L606-L606] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L610-L610] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L612-L612] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L628-L628] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L671-L671] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L702-L702] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L713-L713] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L726-L726] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave, OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L3-L3] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave, OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L4-L4] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L44-L44] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave, OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L45-L45] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L53-L53] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L54-L54] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L56-L56] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L73-L73] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L286] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L287-L287] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L289-L289] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L291-L291] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L346-L346] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L357-L357] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L371-L371] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L402] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L403-L403] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L410-L410] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L423-L423] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L433-L433] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L472-L472] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave, OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L501-L501] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L506-L506] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L511-L511] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L516-L516] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L523-L523] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L525-L525] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L526-L526] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L529-L529] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L530-L530] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L533-L533] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L4-L4] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L47-L47] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L419-L419] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L451-L451] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L4-L4] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L10-L10] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L32-L32] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L40-L40] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L41-L41] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L42-L42] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L43-L43] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L54-L54] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L55-L55] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L65-L65] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L67-L67] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L72-L72] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L84-L84] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L107-L107] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L108-L108] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L110-L110] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L112-L112] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L114-L114] |
| `src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L1-L1] |
| `src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L71-L71] |
| `src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L123-L123] |
| `src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml` | OrcaWave | [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L130-L130] |
| `src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml` | OrcaFlex | [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L137-L137] |

### Map basis and counted families

The map is read at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, with recorded data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the package pin remains the header's code revision. The map inventories implementations outside the package as well as its own rows. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4]

| Counted family, ranked by in-package rows | In package | Complete map inventory | Exact-signal hit definitions | Omitted path-matching signals |
| --- | ---: | ---: | ---: | --- |
| orcaflex-text-model-writers | 4 | 440 | 26 | None |
| rao-readers-and-orcaflex-converters | 3 | 295 | 3 | None |
| external-solver-batch-run-stacks | 1 | 137 | 5 | vocabulary and constants_or_keys: both match `solvers/orcawave` when compared case-insensitively |

The complete inventories that follow enumerate every implementation by its one-based position within the named family's map list; each row points at the map entry containing its path, symbol, relation and digests. This preserves exact discoverability without reproducing identifying names in outside-package paths. Presence is a path-existence check at the code pin, not a validity or digest-equivalence verdict. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8237]

The reproducible counted grep is: for each family, read `search_signals` from the map revision, discard each whole signal whose exact `pattern` matches `solvers/orcawave` using `re.search(pattern, package, re.I)`, then apply each remaining `re.search(pattern, line)` without flags to every package `.py` line at the code pin; assign a hit to the smallest enclosing AST function/class range, with other statements assigned to the module. Escapes are used as stored, with no normalization. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4935-L4942]

| Family | Enclosing definition | Classification and disposition | Hit-line evidence at code pin |
| --- | --- | --- | --- |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator.__init__` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L59-L59] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._create_vessel_config` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L75-L75] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L80-L80] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L83-L83] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L87-L87] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L88-L88] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L102-L102] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L103-L103] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L104-L104] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L111-L111] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L112-L112] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L113-L113] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._load_config` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L132-L132] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._phase3_results_processing` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L258-L258] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L259-L259] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L260-L260] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._phase5_packaging` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L301-L301] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L310-L310] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L314-L314] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._validate_geometry_files` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L399-L399] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._create_output_directories` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L419-L419] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._convert_to_orcaflex` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L433-L433] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L434-L434] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L436-L436] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L438-L438] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L453-L453] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L464-L464] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._check_data_completeness` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L535-L535] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._generate_final_report` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L580-L580] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._generate_package_readme` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L596-L596] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L597-L597] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L598-L598] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L606-L606] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L610-L610] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L612-L612] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L615-L615] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator._save_error_state` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L646-L646] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L647-L647] |
| orcaflex-text-model-writers | `OrcaWaveOrchestrator.list_available_vessels` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L662-L662] |
| orcaflex-text-model-writers | `<module>` | Import/docstring/container context; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L3-L3] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L4-L4] |
| orcaflex-text-model-writers | `OrcaFlexConverter` | Import/docstring/container context; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L44-L44] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L45-L45] |
| orcaflex-text-model-writers | `OrcaFlexConverter.convert_to_orcaflex` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L286] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L287-L287] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L289-L289] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L291-L291] |
| orcaflex-text-model-writers | `OrcaFlexConverter._format_matrices` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L346-L346] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L357-L357] |
| orcaflex-text-model-writers | `OrcaFlexConverter._format_load_raos` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L371-L371] |
| orcaflex-text-model-writers | `OrcaFlexConverter.save_orcaflex_file` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L402] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L403-L403] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L406-L406] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L410-L410] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L412-L412] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L413-L413] |
| orcaflex-text-model-writers | `OrcaFlexConverter._save_compact_format` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L423-L423] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L427-L427] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L464-L464] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L465-L465] |
| orcaflex-text-model-writers | `main` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L501-L501] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L511-L511] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L516-L516] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L523-L523] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L529-L529] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L530-L530] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L533-L533] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L541-L541] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L542-L542] |
| orcaflex-text-model-writers | `GeometryValidator._validate_stl_binary` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L124-L124] |
| orcaflex-text-model-writers | `GeometryValidator._validate_stl_ascii` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L164-L164] |
| orcaflex-text-model-writers | `GeometryValidator._validate_obj` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L191-L191] |
| orcaflex-text-model-writers | `GeometryValidator.save_report` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L408-L408] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L409-L409] |
| orcaflex-text-model-writers | `main` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L506-L506] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L507-L507] |
| rao-readers-and-orcaflex-converters | `OrcaFlexConverter._load_from_csv` | Independent method explicitly listed in section 3; map follow-up, 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L130-L130] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L135-L135] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L153-L153] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L161-L161] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L168-L168] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L184-L184] |
| rao-readers-and-orcaflex-converters | `OrcaFlexConverter._load_from_excel` | Independent method explicitly listed in section 3; map follow-up, 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L216-L216] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L221-L221] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L235-L235] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L246-L246] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L266-L266] |
| rao-readers-and-orcaflex-converters | `OrcaFlexConverter._format_load_raos` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L373-L373] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L378-L378] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L393-L393] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L398-L398] [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L400-L400] |
| external-solver-batch-run-stacks | `OrcaWaveOrchestrator._phase2_orcawave_execution` | Independent method explicitly listed in section 3; map follow-up, 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L231-L231] |
| external-solver-batch-run-stacks | `OrcaWaveOrchestrator._validate_geometry_files` | Delegation, serialization, metadata, report or file-discovery helper; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L374-L374] |
| external-solver-batch-run-stacks | `OrcaWaveOrchestrator._convert_to_orcaflex` | Mapped definition or section-3 method; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L448-L448] |
| external-solver-batch-run-stacks | `<module>` | Import/docstring/container context; 0 further implementations | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L15-L15] |
| external-solver-batch-run-stacks | `GeometryValidator.validate_all` | Geometry read/report or CPU-pool helper; no model writer, RAO solver or external solver launch | [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L67-L67] |

The counted audit finds 0 further second implementations after mapped entries and section-3 methods are included; this satisfies the at-most-one criterion. The two independent result readers and the solver-launch phase are explicitly recorded in section 3 and remain map follow-up, issue pending (6). [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L122-L284] [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L208-L249]

A supplemental whole-repository search, including `scripts/`, applied all three stored signals for each counted family and found respectively 2612, 793 and 2354 Python files with textual hits, including 439, 103 and 305 under `scripts/`. These are discovery counts, not additional implementation claims or changes to the counted result; classification beyond the complete map inventory remains open, issue pending (6). [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4935-L4942]

Map follow-up also includes geometry parsing, mesh topology/quality, waterline and mesh-statistics methods that have no in-package family rows; they are not counted until mapped. The implementation maps and public identifiers require continuing reconciliation; missing references in the inventory are retained as absent, not silently dropped; issue pending (6). [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L113-L309] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L380-L399]

### orcaflex-text-model-writers — complete map inventory

| Map row | Package relation | Present at code pin | Map evidence |
| --- | --- | --- | --- |
| 1 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4940-L4949] |
| 2 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4950-L4958] |
| 3 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4959-L4968] |
| 4 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4969-L4978] |
| 5 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4979-L4988] |
| 6 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4989-L4998] |
| 7 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4999-L5008] |
| 8 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5009-L5020] |
| 9 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5021-L5030] |
| 10 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5031-L5040] |
| 11 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5041-L5050] |
| 12 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5051-L5060] |
| 13 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5061-L5070] |
| 14 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5071-L5080] |
| 15 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5081-L5090] |
| 16 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5091-L5100] |
| 17 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5101-L5110] |
| 18 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5111-L5120] |
| 19 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5121-L5132] |
| 20 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5133-L5142] |
| 21 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5143-L5151] |
| 22 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5152-L5161] |
| 23 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5162-L5171] |
| 24 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5172-L5181] |
| 25 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5182-L5191] |
| 26 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5192-L5201] |
| 27 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5202-L5211] |
| 28 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5212-L5223] |
| 29 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5224-L5233] |
| 30 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5234-L5243] |
| 31 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5244-L5253] |
| 32 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5254-L5263] |
| 33 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5264-L5273] |
| 34 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5274-L5283] |
| 35 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5284-L5293] |
| 36 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5294-L5303] |
| 37 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5304-L5313] |
| 38 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5314-L5323] |
| 39 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5324-L5333] |
| 40 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5334-L5343] |
| 41 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5344-L5353] |
| 42 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5354-L5363] |
| 43 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5364-L5372] |
| 44 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5373-L5382] |
| 45 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5383-L5392] |
| 46 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5393-L5402] |
| 47 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5403-L5415] |
| 48 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5416-L5425] |
| 49 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5426-L5437] |
| 50 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5438-L5447] |
| 51 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5448-L5457] |
| 52 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5458-L5467] |
| 53 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5468-L5477] |
| 54 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5478-L5487] |
| 55 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5488-L5497] |
| 56 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5498-L5507] |
| 57 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5508-L5519] |
| 58 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5520-L5531] |
| 59 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5532-L5540] |
| 60 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5541-L5550] |
| 61 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5551-L5560] |
| 62 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5561-L5570] |
| 63 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5571-L5580] |
| 64 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5581-L5593] |
| 65 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5594-L5603] |
| 66 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5604-L5615] |
| 67 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5616-L5628] |
| 68 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5629-L5638] |
| 69 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5639-L5648] |
| 70 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5649-L5658] |
| 71 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5659-L5667] |
| 72 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5668-L5677] |
| 73 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5678-L5687] |
| 74 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5688-L5697] |
| 75 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5698-L5707] |
| 76 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5708-L5717] |
| 77 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5718-L5727] |
| 78 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5728-L5737] |
| 79 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5738-L5749] |
| 80 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5750-L5759] |
| 81 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5760-L5769] |
| 82 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5770-L5781] |
| 83 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5782-L5791] |
| 84 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5792-L5801] |
| 85 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5802-L5811] |
| 86 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5812-L5823] |
| 87 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5824-L5833] |
| 88 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5834-L5843] |
| 89 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5844-L5853] |
| 90 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5854-L5863] |
| 91 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5864-L5872] |
| 92 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5873-L5882] |
| 93 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5883-L5891] |
| 94 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5892-L5901] |
| 95 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5902-L5911] |
| 96 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5912-L5921] |
| 97 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5922-L5933] |
| 98 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5934-L5943] |
| 99 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5944-L5953] |
| 100 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5954-L5965] |
| 101 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5966-L5975] |
| 102 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5976-L5985] |
| 103 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5986-L5994] |
| 104 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5995-L6004] |
| 105 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6005-L6014] |
| 106 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6015-L6024] |
| 107 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6025-L6034] |
| 108 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6035-L6044] |
| 109 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6045-L6054] |
| 110 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6055-L6064] |
| 111 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6065-L6074] |
| 112 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6075-L6084] |
| 113 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6085-L6096] |
| 114 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6097-L6109] |
| 115 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6110-L6119] |
| 116 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6120-L6129] |
| 117 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6130-L6141] |
| 118 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6142-L6153] |
| 119 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6154-L6163] |
| 120 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6164-L6173] |
| 121 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6174-L6183] |
| 122 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6184-L6193] |
| 123 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6194-L6203] |
| 124 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6204-L6213] |
| 125 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6214-L6223] |
| 126 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6224-L6233] |
| 127 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6234-L6243] |
| 128 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6244-L6253] |
| 129 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6254-L6263] |
| 130 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6264-L6273] |
| 131 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6274-L6283] |
| 132 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6284-L6293] |
| 133 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303] |
| 134 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313] |
| 135 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6314-L6323] |
| 136 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6324-L6333] |
| 137 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6334-L6343] |
| 138 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6344-L6353] |
| 139 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6354-L6363] |
| 140 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6364-L6373] |
| 141 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6374-L6383] |
| 142 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6384-L6393] |
| 143 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6394-L6403] |
| 144 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6404-L6413] |
| 145 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6414-L6423] |
| 146 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6424-L6436] |
| 147 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6437-L6448] |
| 148 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6449-L6458] |
| 149 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6459-L6468] |
| 150 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6469-L6478] |
| 151 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6479-L6490] |
| 152 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6491-L6500] |
| 153 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6501-L6510] |
| 154 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6511-L6520] |
| 155 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6521-L6530] |
| 156 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6531-L6540] |
| 157 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6541-L6550] |
| 158 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6551-L6560] |
| 159 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6561-L6570] |
| 160 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6571-L6580] |
| 161 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6581-L6590] |
| 162 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6591-L6600] |
| 163 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6601-L6610] |
| 164 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6611-L6620] |
| 165 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6621-L6630] |
| 166 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6631-L6640] |
| 167 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6641-L6650] |
| 168 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6651-L6659] |
| 169 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6660-L6672] |
| 170 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6673-L6682] |
| 171 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6683-L6692] |
| 172 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6693-L6704] |
| 173 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6705-L6714] |
| 174 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6715-L6724] |
| 175 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6725-L6734] |
| 176 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6735-L6744] |
| 177 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6745-L6754] |
| 178 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6755-L6764] |
| 179 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6765-L6774] |
| 180 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6775-L6784] |
| 181 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6785-L6794] |
| 182 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6795-L6804] |
| 183 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6805-L6814] |
| 184 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6815-L6824] |
| 185 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6825-L6834] |
| 186 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6835-L6844] |
| 187 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6845-L6854] |
| 188 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6855-L6864] |
| 189 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6865-L6874] |
| 190 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6875-L6884] |
| 191 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6885-L6894] |
| 192 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6895-L6904] |
| 193 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6905-L6914] |
| 194 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6915-L6927] |
| 195 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6928-L6937] |
| 196 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6938-L6947] |
| 197 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6948-L6957] |
| 198 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6958-L6967] |
| 199 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6968-L6977] |
| 200 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6978-L6987] |
| 201 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6988-L6997] |
| 202 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6998-L7007] |
| 203 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7008-L7017] |
| 204 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7018-L7031] |
| 205 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7032-L7041] |
| 206 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7042-L7051] |
| 207 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7052-L7061] |
| 208 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7062-L7071] |
| 209 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7072-L7081] |
| 210 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7082-L7091] |
| 211 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7092-L7101] |
| 212 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7102-L7111] |
| 213 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7112-L7121] |
| 214 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7122-L7131] |
| 215 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7132-L7141] |
| 216 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7142-L7153] |
| 217 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7154-L7163] |
| 218 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7164-L7176] |
| 219 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7177-L7186] |
| 220 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7187-L7196] |
| 221 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7197-L7207] |
| 222 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7208-L7217] |
| 223 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7218-L7227] |
| 224 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7228-L7237] |
| 225 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7238-L7247] |
| 226 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7248-L7257] |
| 227 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7258-L7269] |
| 228 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7270-L7279] |
| 229 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7280-L7289] |
| 230 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7290-L7299] |
| 231 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7300-L7309] |
| 232 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7310-L7319] |
| 233 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7320-L7329] |
| 234 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7330-L7339] |
| 235 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7340-L7349] |
| 236 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7350-L7359] |
| 237 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7360-L7369] |
| 238 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7370-L7379] |
| 239 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7380-L7389] |
| 240 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7390-L7399] |
| 241 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7400-L7409] |
| 242 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7410-L7419] |
| 243 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7420-L7429] |
| 244 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7430-L7439] |
| 245 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7440-L7449] |
| 246 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7450-L7459] |
| 247 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7460-L7469] |
| 248 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7470-L7479] |
| 249 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7480-L7489] |
| 250 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7490-L7499] |
| 251 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7500-L7509] |
| 252 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7510-L7519] |
| 253 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7520-L7529] |
| 254 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7530-L7539] |
| 255 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7540-L7551] |
| 256 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7552-L7561] |
| 257 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7562-L7571] |
| 258 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7572-L7581] |
| 259 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7582-L7591] |
| 260 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7592-L7603] |
| 261 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7604-L7613] |
| 262 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7614-L7626] |
| 263 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7627-L7636] |
| 264 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7637-L7646] |
| 265 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7647-L7656] |
| 266 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7657-L7666] |
| 267 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7667-L7676] |
| 268 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7677-L7686] |
| 269 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7687-L7696] |
| 270 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7697-L7706] |
| 271 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7707-L7716] |
| 272 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7717-L7729] |
| 273 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7730-L7739] |
| 274 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7740-L7749] |
| 275 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7750-L7762] |
| 276 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7763-L7774] |
| 277 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7775-L7784] |
| 278 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7785-L7794] |
| 279 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7795-L7804] |
| 280 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7805-L7816] |
| 281 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7817-L7826] |
| 282 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7827-L7836] |
| 283 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7837-L7846] |
| 284 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7847-L7856] |
| 285 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7857-L7866] |
| 286 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7867-L7878] |
| 287 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7879-L7888] |
| 288 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7889-L7898] |
| 289 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7899-L7908] |
| 290 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7909-L7921] |
| 291 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7922-L7931] |
| 292 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7932-L7941] |
| 293 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7942-L7951] |
| 294 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7952-L7961] |
| 295 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7962-L7971] |
| 296 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7972-L7981] |
| 297 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7982-L7991] |
| 298 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7992-L8001] |
| 299 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8002-L8011] |
| 300 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8012-L8021] |
| 301 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8022-L8031] |
| 302 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8032-L8043] |
| 303 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8044-L8053] |
| 304 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8054-L8063] |
| 305 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8064-L8073] |
| 306 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8074-L8083] |
| 307 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8084-L8093] |
| 308 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8094-L8103] |
| 309 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8104-L8113] |
| 310 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8114-L8123] |
| 311 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8124-L8133] |
| 312 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8134-L8143] |
| 313 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8144-L8155] |
| 314 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8156-L8165] |
| 315 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8166-L8175] |
| 316 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8176-L8185] |
| 317 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8186-L8195] |
| 318 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8205] |
| 319 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8206-L8215] |
| 320 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8216-L8227] |
| 321 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8228-L8237] |
| 322 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8238-L8247] |
| 323 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8248-L8257] |
| 324 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8258-L8267] |
| 325 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8268-L8279] |
| 326 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8280-L8290] |
| 327 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8291-L8300] |
| 328 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8301-L8310] |
| 329 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8311-L8323] |
| 330 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8324-L8333] |
| 331 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8334-L8343] |
| 332 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8344-L8353] |
| 333 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8354-L8363] |
| 334 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8364-L8373] |
| 335 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8374-L8383] |
| 336 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8384-L8393] |
| 337 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8394-L8403] |
| 338 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8404-L8413] |
| 339 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8414-L8423] |
| 340 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8424-L8435] |
| 341 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8436-L8445] |
| 342 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8446-L8457] |
| 343 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8458-L8470] |
| 344 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8471-L8480] |
| 345 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8481-L8492] |
| 346 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8493-L8505] |
| 347 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8506-L8517] |
| 348 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8518-L8527] |
| 349 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8528-L8537] |
| 350 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8538-L8547] |
| 351 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8548-L8560] |
| 352 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8561-L8572] |
| 353 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8573-L8582] |
| 354 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8583-L8596] |
| 355 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8597-L8608] |
| 356 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8609-L8619] |
| 357 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8620-L8632] |
| 358 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8633-L8647] |
| 359 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8648-L8657] |
| 360 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8658-L8667] |
| 361 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8668-L8677] |
| 362 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8678-L8687] |
| 363 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8688-L8701] |
| 364 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8702-L8714] |
| 365 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8715-L8724] |
| 366 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8725-L8739] |
| 367 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8740-L8749] |
| 368 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8750-L8759] |
| 369 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8760-L8773] |
| 370 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8774-L8783] |
| 371 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8784-L8793] |
| 372 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8794-L8803] |
| 373 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8804-L8813] |
| 374 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8814-L8823] |
| 375 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8824-L8833] |
| 376 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8834-L8843] |
| 377 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8844-L8856] |
| 378 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8857-L8866] |
| 379 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8867-L8876] |
| 380 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8877-L8886] |
| 381 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8887-L8896] |
| 382 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8897-L8908] |
| 383 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8909-L8918] |
| 384 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8919-L8928] |
| 385 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8929-L8938] |
| 386 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8939-L8948] |
| 387 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8949-L8958] |
| 388 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8959-L8970] |
| 389 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8971-L8980] |
| 390 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8981-L8990] |
| 391 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8991-L9000] |
| 392 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9001-L9010] |
| 393 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9011-L9020] |
| 394 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9021-L9030] |
| 395 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9031-L9040] |
| 396 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9041-L9050] |
| 397 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9051-L9060] |
| 398 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9061-L9070] |
| 399 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9071-L9080] |
| 400 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9081-L9090] |
| 401 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9091-L9100] |
| 402 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9101-L9110] |
| 403 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9111-L9120] |
| 404 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9121-L9130] |
| 405 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9131-L9140] |
| 406 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9141-L9152] |
| 407 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9153-L9162] |
| 408 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9163-L9172] |
| 409 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9173-L9182] |
| 410 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9183-L9192] |
| 411 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9193-L9202] |
| 412 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9203-L9212] |
| 413 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9213-L9222] |
| 414 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9223-L9232] |
| 415 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9233-L9242] |
| 416 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9243-L9252] |
| 417 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9253-L9262] |
| 418 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9263-L9272] |
| 419 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9273-L9282] |
| 420 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9283-L9292] |
| 421 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9293-L9302] |
| 422 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9303-L9312] |
| 423 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9313-L9322] |
| 424 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9323-L9332] |
| 425 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9333-L9342] |
| 426 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9343-L9352] |
| 427 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9353-L9362] |
| 428 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9363-L9372] |
| 429 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9373-L9382] |
| 430 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9383-L9392] |
| 431 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9393-L9402] |
| 432 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9403-L9412] |
| 433 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9413-L9422] |
| 434 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9423-L9432] |
| 435 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9433-L9442] |
| 436 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9443-L9452] |
| 437 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9453-L9462] |
| 438 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9463-L9472] |
| 439 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9473-L9482] |
| 440 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9483-L9493] |

### rao-readers-and-orcaflex-converters — complete map inventory

| Map row | Package relation | Present at code pin | Map evidence |
| --- | --- | --- | --- |
| 1 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9516-L9527] |
| 2 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9528-L9539] |
| 3 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9540-L9551] |
| 4 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9552-L9563] |
| 5 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9564-L9578] |
| 6 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9579-L9589] |
| 7 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9590-L9601] |
| 8 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9602-L9613] |
| 9 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9614-L9625] |
| 10 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9626-L9637] |
| 11 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9638-L9652] |
| 12 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9653-L9664] |
| 13 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9665-L9678] |
| 14 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9679-L9690] |
| 15 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9691-L9704] |
| 16 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9705-L9715] |
| 17 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9716-L9727] |
| 18 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9728-L9739] |
| 19 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9740-L9753] |
| 20 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9754-L9765] |
| 21 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9766-L9777] |
| 22 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9778-L9788] |
| 23 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9789-L9800] |
| 24 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9801-L9812] |
| 25 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9813-L9826] |
| 26 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9827-L9838] |
| 27 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9839-L9849] |
| 28 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9850-L9861] |
| 29 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9862-L9873] |
| 30 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9874-L9885] |
| 31 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9886-L9897] |
| 32 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9898-L9909] |
| 33 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9910-L9921] |
| 34 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9922-L9932] |
| 35 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9933-L9943] |
| 36 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9944-L9954] |
| 37 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9955-L9965] |
| 38 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9966-L9977] |
| 39 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9978-L9989] |
| 40 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9990-L10004] |
| 41 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10005-L10017] |
| 42 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10018-L10032] |
| 43 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10033-L10046] |
| 44 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10047-L10060] |
| 45 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10061-L10072] |
| 46 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10073-L10087] |
| 47 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10088-L10099] |
| 48 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10100-L10111] |
| 49 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10112-L10122] |
| 50 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10123-L10134] |
| 51 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10135-L10146] |
| 52 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10147-L10158] |
| 53 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10159-L10169] |
| 54 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10170-L10181] |
| 55 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10182-L10193] |
| 56 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10194-L10207] |
| 57 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10208-L10222] |
| 58 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10223-L10235] |
| 59 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10236-L10246] |
| 60 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10247-L10260] |
| 61 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10261-L10272] |
| 62 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10273-L10284] |
| 63 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10285-L10296] |
| 64 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10297-L10307] |
| 65 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10308-L10319] |
| 66 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10320-L10333] |
| 67 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10334-L10345] |
| 68 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10346-L10356] |
| 69 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10357-L10368] |
| 70 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10369-L10380] |
| 71 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10381-L10391] |
| 72 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10392-L10403] |
| 73 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10404-L10415] |
| 74 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10416-L10427] |
| 75 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10428-L10439] |
| 76 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10440-L10451] |
| 77 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10452-L10465] |
| 78 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10466-L10477] |
| 79 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10478-L10489] |
| 80 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10490-L10501] |
| 81 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10502-L10513] |
| 82 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10514-L10527] |
| 83 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10528-L10539] |
| 84 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10540-L10551] |
| 85 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10552-L10563] |
| 86 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10564-L10575] |
| 87 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10576-L10587] |
| 88 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10588-L10599] |
| 89 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10600-L10611] |
| 90 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10612-L10623] |
| 91 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10624-L10635] |
| 92 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10636-L10647] |
| 93 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10648-L10659] |
| 94 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10660-L10671] |
| 95 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10672-L10683] |
| 96 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10684-L10699] |
| 97 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10700-L10711] |
| 98 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10712-L10725] |
| 99 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10726-L10737] |
| 100 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10738-L10749] |
| 101 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10750-L10761] |
| 102 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10762-L10775] |
| 103 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10776-L10789] |
| 104 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10790-L10801] |
| 105 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10802-L10815] |
| 106 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10816-L10828] |
| 107 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10829-L10841] |
| 108 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10842-L10853] |
| 109 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10854-L10865] |
| 110 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10866-L10877] |
| 111 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10878-L10891] |
| 112 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10892-L10905] |
| 113 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10906-L10917] |
| 114 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10918-L10933] |
| 115 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10934-L10948] |
| 116 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10949-L10962] |
| 117 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10963-L10979] |
| 118 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10980-L10991] |
| 119 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10992-L11003] |
| 120 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11004-L11015] |
| 121 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11016-L11027] |
| 122 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11028-L11039] |
| 123 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11040-L11051] |
| 124 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11052-L11063] |
| 125 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11064-L11075] |
| 126 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11076-L11089] |
| 127 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11090-L11101] |
| 128 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11102-L11113] |
| 129 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11114-L11125] |
| 130 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11126-L11137] |
| 131 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11138-L11149] |
| 132 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11150-L11163] |
| 133 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11164-L11175] |
| 134 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11176-L11187] |
| 135 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11188-L11202] |
| 136 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11203-L11213] |
| 137 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11214-L11225] |
| 138 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11226-L11237] |
| 139 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11238-L11248] |
| 140 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11249-L11260] |
| 141 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11261-L11274] |
| 142 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11275-L11286] |
| 143 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11287-L11298] |
| 144 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11299-L11310] |
| 145 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11311-L11322] |
| 146 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11323-L11334] |
| 147 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11335-L11346] |
| 148 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11347-L11360] |
| 149 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11361-L11374] |
| 150 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11375-L11388] |
| 151 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11389-L11400] |
| 152 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11401-L11414] |
| 153 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11415-L11426] |
| 154 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11427-L11438] |
| 155 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11439-L11450] |
| 156 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11451-L11462] |
| 157 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11463-L11474] |
| 158 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11475-L11486] |
| 159 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11487-L11498] |
| 160 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11499-L11510] |
| 161 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11511-L11524] |
| 162 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11525-L11536] |
| 163 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11537-L11548] |
| 164 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11549-L11560] |
| 165 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11561-L11574] |
| 166 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11575-L11590] |
| 167 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11591-L11603] |
| 168 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11604-L11619] |
| 169 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11620-L11633] |
| 170 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11634-L11648] |
| 171 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11649-L11662] |
| 172 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11663-L11674] |
| 173 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11675-L11686] |
| 174 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11687-L11698] |
| 175 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11699-L11710] |
| 176 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11711-L11724] |
| 177 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11725-L11736] |
| 178 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11737-L11748] |
| 179 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11749-L11762] |
| 180 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11763-L11774] |
| 181 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11775-L11786] |
| 182 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11787-L11798] |
| 183 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11799-L11812] |
| 184 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11813-L11824] |
| 185 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11825-L11836] |
| 186 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11837-L11850] |
| 187 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11851-L11862] |
| 188 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11863-L11874] |
| 189 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11875-L11888] |
| 190 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11889-L11900] |
| 191 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11901-L11912] |
| 192 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11913-L11926] |
| 193 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11927-L11938] |
| 194 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11939-L11950] |
| 195 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11951-L11964] |
| 196 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11965-L11976] |
| 197 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11977-L11990] |
| 198 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11991-L12002] |
| 199 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12003-L12014] |
| 200 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12015-L12026] |
| 201 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12027-L12038] |
| 202 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12039-L12052] |
| 203 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12053-L12064] |
| 204 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12065-L12078] |
| 205 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12079-L12095] |
| 206 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12096-L12107] |
| 207 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12108-L12119] |
| 208 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12120-L12131] |
| 209 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12132-L12143] |
| 210 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12144-L12159] |
| 211 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12160-L12173] |
| 212 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12174-L12186] |
| 213 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12187-L12199] |
| 214 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12200-L12211] |
| 215 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12212-L12226] |
| 216 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12227-L12238] |
| 217 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12239-L12254] |
| 218 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12255-L12266] |
| 219 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12267-L12278] |
| 220 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12279-L12290] |
| 221 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12291-L12302] |
| 222 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12303-L12314] |
| 223 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12315-L12326] |
| 224 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12327-L12338] |
| 225 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12339-L12350] |
| 226 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12351-L12362] |
| 227 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12363-L12376] |
| 228 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12377-L12388] |
| 229 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12389-L12400] |
| 230 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12401-L12412] |
| 231 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12413-L12427] |
| 232 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12428-L12441] |
| 233 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12442-L12453] |
| 234 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12454-L12465] |
| 235 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12466-L12477] |
| 236 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12478-L12489] |
| 237 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12490-L12503] |
| 238 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12504-L12515] |
| 239 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12516-L12529] |
| 240 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12530-L12541] |
| 241 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12542-L12555] |
| 242 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12556-L12567] |
| 243 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12568-L12582] |
| 244 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12583-L12594] |
| 245 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12595-L12606] |
| 246 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12607-L12620] |
| 247 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12621-L12632] |
| 248 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12633-L12646] |
| 249 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12647-L12658] |
| 250 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12659-L12670] |
| 251 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12671-L12682] |
| 252 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12683-L12696] |
| 253 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12697-L12710] |
| 254 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12711-L12721] |
| 255 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12722-L12733] |
| 256 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12734-L12745] |
| 257 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12746-L12759] |
| 258 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12760-L12773] |
| 259 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12774-L12787] |
| 260 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12788-L12799] |
| 261 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12800-L12813] |
| 262 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12814-L12827] |
| 263 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12828-L12839] |
| 264 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12840-L12850] |
| 265 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12851-L12862] |
| 266 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12863-L12874] |
| 267 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12875-L12886] |
| 268 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12887-L12898] |
| 269 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12899-L12910] |
| 270 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12911-L12922] |
| 271 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12923-L12934] |
| 272 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12935-L12946] |
| 273 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12947-L12958] |
| 274 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12959-L12973] |
| 275 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12974-L12986] |
| 276 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12987-L12998] |
| 277 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12999-L13010] |
| 278 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13011-L13022] |
| 279 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13023-L13034] |
| 280 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13035-L13048] |
| 281 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13049-L13062] |
| 282 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13063-L13074] |
| 283 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13075-L13086] |
| 284 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13087-L13101] |
| 285 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13102-L13113] |
| 286 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13114-L13125] |
| 287 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13126-L13139] |
| 288 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13140-L13153] |
| 289 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13154-L13167] |
| 290 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13168-L13181] |
| 291 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13182-L13193] |
| 292 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13194-L13204] |
| 293 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13205-L13216] |
| 294 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13217-L13228] |
| 295 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13229-L13240] |

### external-solver-batch-run-stacks — complete map inventory

| Map row | Package relation | Present at code pin | Map evidence |
| --- | --- | --- | --- |
| 1 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24511-L24520] |
| 2 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24521-L24530] |
| 3 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24531-L24540] |
| 4 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24541-L24550] |
| 5 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24551-L24560] |
| 6 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24561-L24570] |
| 7 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24571-L24580] |
| 8 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24581-L24590] |
| 9 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24591-L24600] |
| 10 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24601-L24610] |
| 11 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24611-L24620] |
| 12 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24621-L24630] |
| 13 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24631-L24640] |
| 14 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24641-L24650] |
| 15 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24651-L24660] |
| 16 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24661-L24670] |
| 17 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24671-L24680] |
| 18 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24681-L24690] |
| 19 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24691-L24700] |
| 20 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24701-L24712] |
| 21 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24713-L24722] |
| 22 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24723-L24732] |
| 23 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24733-L24742] |
| 24 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24743-L24751] |
| 25 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24752-L24760] |
| 26 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24761-L24769] |
| 27 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24770-L24778] |
| 28 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24779-L24787] |
| 29 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24788-L24796] |
| 30 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24797-L24805] |
| 31 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24806-L24814] |
| 32 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24815-L24823] |
| 33 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24824-L24832] |
| 34 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24833-L24841] |
| 35 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24842-L24850] |
| 36 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24851-L24859] |
| 37 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24860-L24868] |
| 38 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24869-L24877] |
| 39 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24878-L24886] |
| 40 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24887-L24895] |
| 41 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24896-L24904] |
| 42 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24905-L24913] |
| 43 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24914-L24922] |
| 44 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24923-L24932] |
| 45 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24933-L24942] |
| 46 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24943-L24952] |
| 47 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24953-L24962] |
| 48 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24963-L24974] |
| 49 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24975-L24984] |
| 50 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24985-L24997] |
| 51 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24998-L25007] |
| 52 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25008-L25017] |
| 53 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25018-L25027] |
| 54 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25028-L25037] |
| 55 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25038-L25047] |
| 56 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25048-L25057] |
| 57 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25058-L25067] |
| 58 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25068-L25079] |
| 59 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25080-L25089] |
| 60 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25090-L25099] |
| 61 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25100-L25109] |
| 62 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25110-L25119] |
| 63 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25120-L25129] |
| 64 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25130-L25139] |
| 65 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25140-L25149] |
| 66 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25150-L25159] |
| 67 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25160-L25169] |
| 68 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25170-L25179] |
| 69 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25180-L25189] |
| 70 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25190-L25199] |
| 71 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25200-L25209] |
| 72 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25210-L25219] |
| 73 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25220-L25229] |
| 74 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25230-L25239] |
| 75 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25240-L25249] |
| 76 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25250-L25259] |
| 77 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25260-L25269] |
| 78 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25270-L25279] |
| 79 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25280-L25289] |
| 80 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25290-L25299] |
| 81 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25300-L25309] |
| 82 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25310-L25319] |
| 83 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25320-L25329] |
| 84 | in package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25330-L25339] |
| 85 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25340-L25349] |
| 86 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25350-L25359] |
| 87 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25360-L25369] |
| 88 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25370-L25379] |
| 89 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25380-L25389] |
| 90 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25390-L25399] |
| 91 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25400-L25409] |
| 92 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25410-L25419] |
| 93 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25420-L25429] |
| 94 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25430-L25439] |
| 95 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25440-L25455] |
| 96 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25456-L25465] |
| 97 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25466-L25475] |
| 98 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25476-L25485] |
| 99 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25486-L25495] |
| 100 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25496-L25505] |
| 101 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25506-L25518] |
| 102 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25519-L25528] |
| 103 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25529-L25538] |
| 104 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25539-L25548] |
| 105 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25549-L25558] |
| 106 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25559-L25568] |
| 107 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25569-L25578] |
| 108 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25579-L25588] |
| 109 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25589-L25598] |
| 110 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25599-L25608] |
| 111 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25609-L25618] |
| 112 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25619-L25628] |
| 113 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25629-L25638] |
| 114 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25639-L25648] |
| 115 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25649-L25658] |
| 116 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25659-L25668] |
| 117 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25669-L25678] |
| 118 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25679-L25688] |
| 119 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25689-L25698] |
| 120 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25699-L25708] |
| 121 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25709-L25718] |
| 122 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25719-L25728] |
| 123 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25729-L25738] |
| 124 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25739-L25748] |
| 125 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25749-L25758] |
| 126 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25759-L25770] |
| 127 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25771-L25780] |
| 128 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25781-L25790] |
| 129 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25791-L25800] |
| 130 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25801-L25810] |
| 131 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25811-L25820] |
| 132 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25821-L25830] |
| 133 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25831-L25840] |
| 134 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25841-L25850] |
| 135 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25851-L25860] |
| 136 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25861-L25870] |
| 137 | outside package | yes | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25871-L25880] |

## 10. Coverage and open items

The read inventory is bounded to these legacy helpers; larger canonical diffraction and reporting implementations keep their own scope. The module's own regression folder has one test function; the similarly named top-level tests import a different package. [tests/solvers/orcawave/diffraction/test_setup.py:L9-L101] [tests/orcawave/test_rao_processing.py:L1-L25]

| Material read | Coverage / evidence |
| --- | --- |
| Package initializer | Entire file. [src/digitalmodel/solvers/orcawave/__init__.py:L1-L13] |
| Orchestrator | Entire file, every phase and CLI handler. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L1-L740] |
| Converter | Entire file, all three readers, both writers, summary and CLI. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L1-L553] |
| Geometry validator | Entire file, discovery, three parsers, statistics, score, reports and CLI. [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L1-L521] |
| Batch launch | Entire script, read only; no launch or licence probe. [src/digitalmodel/solvers/orcawave/diffraction/scripts/run_diffraction_analysis.bat:L1-L154] |
| Generic configuration | Entire template. [src/digitalmodel/solvers/orcawave/diffraction/configs/base_diffraction_config.yml:L1-L137] |
| Package documentation | README, QUICK_START and TEST_SUMMARY read in full; historical counts are unverified rather than re-used as results. [src/digitalmodel/solvers/orcawave/diffraction/README.md:L1-L219] [src/digitalmodel/solvers/orcawave/diffraction/QUICK_START.md:L1-L111] [src/digitalmodel/solvers/orcawave/diffraction/TEST_SUMMARY.md:L1-L137] |
| Module tests | One setup test and package markers; only one collected node for this package. [tests/solvers/orcawave/diffraction/test_setup.py:L1-L104] |
| Routing and domain documentation | Existing solvers entry and canonical-domain distinction. [docs/registry/module-routing.yaml:L249-L254] [docs/registry/module-routing.yaml:L303-L328] [docs/domains/orcawave/README.md:L1-L239] |
| Additional documentation discovered under docs | `docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md`, `docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md`, `docs/registry/implementation-map.md`, plus the domain README above and 1 discovery document with identifying names; search expression below; codenaming issue pending (7). [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L145-L160] [docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md:L238-L260] [docs/registry/implementation-map.md:L1-L59] |
| Configuration discovery | 3 additional discovery configuration files with identifying names read; values and filenames are not reproduced; codenaming issue pending (7). [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L74-L116] |
| Issue leads | #788 historical syntax/BOM incident; #608 mesh qualification and #611 output-contract context; no current repair inferred from closure. [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L1-L32] [docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md:L1-L20] [docs/plans/2026-05-15-issue-611-orcawave-result-contract.md:L1-L20] |

Documentation discovery command: `rg -l -i 'solvers/orcawave|src/modules/orcawave|OrcaWaveOrchestrator|OrcaWave Generic Diffraction' docs --glob '*.md'`; this found 5 documents. Broader references to OrcaWave that describe other packages are outside this pack's numerical claims. [docs/domains/orcawave/README.md:L198-L205]

| Open item / qualification | Responsible role | Tracking |
| --- | --- | --- |
| UNVERIFIED: independent numerical examples, supported solver versions and applicability envelope, and historical suitability claims in section 1 | Diffraction quality manager | issue pending (1) |
| UNVERIFIED: DNV-RP-C205 edition and implementing clause title; clause text not checked against the licensed copy; no edition conflict can be resolved without that basis | Diffraction quality manager | issue pending (1) |
| UNVERIFIED: source force/moment/matrix/mesh units, phase sign, heading convention and coordinate origin compatibility in section 2 | Maintainer and diffraction quality manager | issue pending (2) |
| UNVERIFIED: named quality appointments, shared-log entry and clearance date in section 8 | Maintainer | issue pending (1) |
| Orchestrator fail-open states, missing-data bypasses, incomplete packaging and report defaults in section 6 | Maintainer | issue pending (3) |
| Converter missing-data fabrication, moment handling, index validation, density assumption, default filenames and compact/full mismatch in section 6 | Maintainer | issue pending (2) |
| Geometry omission, swallowed hull error, closure/centroid/orientation proxies and parser/unit limitations in section 6 | Maintainer | issue pending (4); related #608 |
| Assertion-free setup regression, stale command paths and historical test/maturity claims; no independently validated example can be promoted from these records | Maintainer | issue pending (5) |
| Complete map inventories retained; section-3 independent methods and unmapped geometry families need map reconciliation; supplemental matches remain discovery only | Maintainer | issue pending (6) |
| 4 discovery files with identifying names require codenaming before their identifiers or values can be referenced publicly; no client-derived values are included; sidecar entry: to be created if needed | Maintainer | issue pending (7) |
| Existing duplicate `submodules` keys under `solvers` can hide earlier registry entries with ordinary YAML loading; this addition preserves existing parsed entries | Maintainer | issue pending (8) |
| Licensed G1 and result-dependent G4/G5 checks remain NOT_RUN; no solver result, signed QA appendix, target-version acceptance or private reference data is established | Diffraction quality manager | #611; related #608; issue pending (1) |

The calculation-citation contract was read as guidance; no standards-derived numeric constant is introduced in module code by this documentation change. Existing numerical heuristics and defaults are source-cited and left unqualified, rather than promoted to a checked licensed-standard clause. [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L301-L301] [src/digitalmodel/solvers/orcawave/diffraction/scripts/validate_geometry.py:L380-L399]
