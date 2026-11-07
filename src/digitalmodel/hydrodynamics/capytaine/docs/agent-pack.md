# Capytaine module knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/capytaine |
| Repository | digitalmodel |
| Package | `src/digitalmodel/hydrodynamics/capytaine` |
| Pack revision | 0 |
| Code pin | `21942e8cfc7dfb985c5bfa693245df7d197cf76c` |
| Analysis family | diffraction (DF) |
| Quality role | diffraction family quality manager |
| Status | DRAFT; independent clearance pending |

## 1. Purpose and scope

The public entry points configure floating bodies, solve radiation and diffraction problems, compute complex response amplitude operators (RAOs), and export coefficient tables, NetCDF datasets and plots. [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L8-L55]

The solver delegates boundary-element calculations to Capytaine and passes frequencies, wave directions, active rigid-body motions, depth, density and gravity to its dataset interface. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L189-L217]

The domain registry records hydrodynamics as production; that domain classification does not establish qualification of a particular mesh or response calculation. [docs/registry/module-routing.yaml:L178-L183] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L62-L84]

Mesh preparation clips to the immersed part, optionally applies reflection symmetry, and can attach an interior lid; the wrapper requires a Capytaine major version of at least 3. [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L38-L110]

This package's solve interface has no forward-speed argument; it supplies the environmental coordinates shown in section 2. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L189-L214]

## 2. Inputs and outputs

Input limits below distinguish enforced checks from merely documented dimensions. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L62-L84] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L112-L156]

| Input | Unit and shape | Range / default | Source and evidence |
| --- | --- | --- | --- |
| MeshConfig | path or predefined sphere, cylinder or box; coordinates passed without rescaling | AUTO format; symmetry false; source required | [src/digitalmodel/hydrodynamics/capytaine/models.py:L54-L70] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L119-L177] |
| BodyConfig.dofs | named rigid-body motions | six by default; empty list rejected | [src/digitalmodel/hydrodynamics/capytaine/models.py:L29-L51] [src/digitalmodel/hydrodynamics/capytaine/models.py:L74-L93] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L78-L84] |
| Mass and reference points | kg; centre-of-mass and optional rotation centre in mesh coordinates | mass absent; centre defaults to the origin; rotation centre falls back to centre of mass | [src/digitalmodel/hydrodynamics/capytaine/models.py:L74-L93] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L94-L101] |
| Structural matrices | 6 by 6 mass/inertia and stiffness; mixed translational/rotational dimensions | absent by default; configured inertia must be finite and exactly 6 by 6 | [src/digitalmodel/hydrodynamics/capytaine/models.py:L80-L83] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L143-L153] |
| WaveConditions | periods s OR angular frequency rad/s; headings rad; depth m; density kg/m³; gravity m/s² | exactly one frequency specification; positive frequency, depth and density checks; deep-water depth is infinity; heading defaults to zero | [src/digitalmodel/hydrodynamics/capytaine/models.py:L96-L126] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L62-L76] |
| Range factories | headings supplied in degrees | uniform period or frequency spacing; 30 samples by default | [src/digitalmodel/hydrodynamics/capytaine/models.py:L128-L162] |
| SolverConfig | strings and integer settings | Delhommeau, LU, indirect method, one worker and cache setting 1; see ignored settings in section 6 | [src/digitalmodel/hydrodynamics/capytaine/models.py:L165-L177] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L155-L187] |
| Interior lid | elevation m and optional positive panel radius m | disabled; absent elevation becomes zero; above-surface elevation and empty lid rejected | [src/digitalmodel/hydrodynamics/capytaine/models.py:L84-L88] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L75-L110] |
| Manual RAO arrays | A and B: frequency × motion × motion; F: frequency × heading × motion; M, K and extra B: motion × motion | finite numeric arrays with nonempty axes and exact shapes; absent extra damping becomes zero | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L91-L110] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L112-L156] |

| Output | Unit / ordering | Writer or consumer | Evidence |
| --- | --- | --- | --- |
| BEMResult | coefficients: frequency × influenced row × radiating column; excitation: frequency × heading × influenced motion | requested coordinate ordering retained; absent dataset variables leave optional fields unset | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L219-L278] |
| Native coefficients | mixed kg, kg·m and kg·m² mass terms; compatible force/moment damping and restoring dimensions | native arrays retained; the manual interface documents matrix dimensions | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L95-L102] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L245-L255] |
| RAOResult | complex motion per wave amplitude, magnitude, phase degrees | manual operator uses exp(-iωt); rotations remain radians per metre before schema conversion | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L83-L102] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L174-L182] [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L80-L104] |
| NetCDF | native dataset with complex components retained by the dependency exporter | `export_netcdf`; readback control in section 5 | [src/digitalmodel/hydrodynamics/capytaine/results.py:L27-L44] [tests/hydrodynamics/capytaine/test_lid_and_export.py:L56-L69] |
| Tables / plots | period s; heading degrees; magnitude of complex forces; plot labels need the cautions in section 6 | `added_mass_table`, `excitation_force_table`, and plot functions | [src/digitalmodel/hydrodynamics/capytaine/results.py:L52-L127] [src/digitalmodel/hydrodynamics/capytaine/results.py:L135-L307] |
| DiffractionResults | SI coefficient matrices; rotational magnitudes converted to deg/m; phase metadata `unknown` with exp(-iωt) note | `diffraction_results` uses active-motion positional indexing | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L98-L125] |
| Held-reference input conversion | tonne and kilonewton coefficients multiplied into SI; rad/m response retained | workbook units are explicitly an interpretation requiring acceptance | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L4-L8] [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L49-L77] |

UNVERIFIED: mesh-file axis orientation, wave-heading sense and origin agreement across every supported reader have not been independently established; no reader-side coordinate transformation is present in this wrapper. See issue pending (1). [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L148-L177]

## 3. Method and governing standards

DNV-RP-C205, *Environmental Conditions and Environmental Loads*, edition 2021, is the manifest's named standard; clause text not checked against the licensed copy. The source supplies clause numbers but not independently checked clause titles. [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L4-L26]

| Method | Implementing definitions | Reference recorded by the repository / qualification limit | Evidence |
| --- | --- | --- | --- |
| Diffraction and radiation | `CapytaineSolver.solve`, `_run_fill_dataset`, `run_bem_analysis` | dependency BEM operator; source names DNV-RP-C205 §7.1 and manifest names §7.1.2; clause text not checked against the licensed copy | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L34-L40] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L86-L122] [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L9-L14] |
| Hydrostatics and inertia | `_compute_and_attach_hydrostatics`, `_configured_inertia` | dependency hydrostatics with configured density and gravity; source names §7.2.1 without an edition; clause text not checked against the licensed copy | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L86-L92] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L124-L153] |
| Wave specification | `WaveConditions`, its conversion properties and range factories | source names §3.3 and §2.3.1 without an edition; these are unconfirmed clause identifiers, not verified standard requirements; clause text not checked against the licensed copy | [src/digitalmodel/hydrodynamics/capytaine/models.py:L96-L162] |
| Coupled RAO | `compute_rao_manual` solves Zξ=F, with Z=K−ω²(M+A)−iω(B+B_ext); `compute_rao` delegates | source names §7.2.5 and Eq 7.2.1; the manifest prints the opposite damping sign; clause text not checked against the licensed copy; issue pending (2) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L18-L50] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L83-L89] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L160-L172] [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L15-L23] |
| Interior lid | `resolved_lid`, `_component_lids`, `merge_lid_cells`, `_grid_rectangles`; called by `create_floating_body` | native waterline components, repeated boundary refinement and area-preserving rectangle aggregation; coverage tolerance is an implementation criterion, not a quoted standard value | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L16-L121] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L75-L89] |
| Mass reference transfer | `rigid_body_mass_matrix` | code attributes the parallel-axis relation to Moebs, Ling and Sanny, *University Physics Volume 1*, 2016, §10.5 Eq 10.20; synthetic tensor control in section 4 | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L41-L69] |
| Finite-depth wave number | `finite_depth_wave_number` | root of gk tanh(kh)−ω² using Brent iteration; code attributes the dispersion relation to MIT, *Marine Hydrodynamics*, 2005, Lecture 20 §6.2.4 Eq (6) | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L72-L94] |
| Natural period | `natural_period` | code attributes T=2π√(m/k) to Moebs, Ling and Sanny, 2016, §15.1 Eq 15.10; stored added-mass interpolation and endpoint continuation are assumptions | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L97-L132] |
| Comparison / resonance flags | `normalised_difference`, `drag_band_flags` | held maximum over periods and a twenty-percent band are explicitly assumed, based on previous experience | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L17-L38] [src/digitalmodel/hydrodynamics/capytaine/screening.py:L135-L151] |

UNVERIFIED: the cited standard clauses, their titles and the density attribution have not been confirmed against the licensed edition; no alternate edition is named in the package, and the planning documents do not supply an alternate edition. See issue pending (2). [src/digitalmodel/hydrodynamics/capytaine/models.py:L96-L104] [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L4-L26] [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L28-L38]

UNVERIFIED: standards-derived calculation outputs have no emitted `Citation` sidecar in the inspected solve and RAO return paths; engineering acceptance of the clause mapping remains open under issue pending (2). [src/digitalmodel/hydrodynamics/capytaine/solver.py:L233-L257] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L174-L182]

## 4. Validated worked examples and regression examples

Validated examples below use independently written closed-form arithmetic with synthetic inputs, rather than a saved output of the implementation; they validate the stated arithmetic paths, not a physical hull. [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L137-L164] [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L32-L45] [tests/hydrodynamics/capytaine/test_screening_comparison.py:L40-L44]

| Case / kind | Independent basis, inputs and expected result | Tolerance and basis | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| V1 coupled RAO / validated | two coupled synthetic motions, unit angular frequency; solution `[0.6+0.8i, −0.4−0.2i]` in unit response | absolute 1e-12 response units, arithmetic precision | `tests/hydrodynamics/capytaine/test_rao_manual_sign.py::test_coupled_response_uses_minus_i_omega_damping` | PASS; [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L32-L45] |
| V2 diagonal RAO / validated | two frequencies and headings; direct diagonal complex division; expected `[1+i,−i]` and `[−1+i,2]`, amplitudes and phases independently tabulated | absolute 1e-12 response units and degrees; double-precision rounding | `tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py::test_manual_rao_diagonal_hand_solution` | PASS; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L137-L164] |
| V3 constant-added-mass period / validated | synthetic mass 1 kg, added mass 3 kg, stiffness 1 N/m gives T=4π s | relative 1e-10, closed-form comparison | `tests/hydrodynamics/capytaine/test_screening_comparison.py::test_natural_period_matches_constant_added_mass_hand_example` | PASS; [tests/hydrodynamics/capytaine/test_screening_comparison.py:L40-L44] |
| R1 coordinate extraction / regression | synthetic reversed coordinates and transposed coefficient axes; literal array reordering | zero mismatched array elements (count) | `tests/hydrodynamics/capytaine/test_result_coordinates.py::test_coefficients_follow_requested_coordinates` | PASS; [tests/hydrodynamics/capytaine/test_result_coordinates.py:L19-L41] |
| R2 singular operator / regression | zero impedance with unit force deliberately returns minimum-norm zero | zero differing elements (count); does not establish physical equilibrium | `tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py::test_manual_rao_preserves_singular_least_squares` | PASS; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L167-L176] |
| R3 schema readback / regression | synthetic unit response converted to deg/m; `solver` source, SI and unknown phase metadata survive serialisation | zero round-trip differences (count); pitch magnitude 180/π deg/m with pytest default tolerance | `tests/hydrodynamics/capytaine/test_screening_storage.py::test_si_schema_rotations_and_solver_source_round_trip` | PASS; [tests/hydrodynamics/capytaine/test_screening_storage.py:L39-L59] |

Evidence record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/hydrodynamics/capytaine -q -rA -p no:cacheprovider --basetemp=<scratch>` collected 96 executable cases, all PASS, and six import-time skipped files. The identical command after the documentation edits again passed 96 cases with the same six import-time skipped files; both runs used the code pin above with source files unchanged. Dependency attempt `uv run --no-sources --with capytaine==3.0.0 --no-build-package capytaine python -m pytest ...` failed before collection: `Because capytaine==3.0.0 has no usable wheels and you require capytaine==3.0.0, we can conclude that your requirements are unsatisfiable.` Native cases are NOT_RUN, not passing validations.

## 5. QA gates and executable checks

Gate application is the diffraction analogue of AQG r0 §10: G0, G1, G4 and G5 apply; G2 and G3 are NOT_APPLICABLE because the native path is frequency-domain BEM with a separate algebraic response calculation. The exported objects are NetCDF, tables, plots and a solver-neutral schema; no OrcaWave or AQWA deck writer occurs in the public exports. (AQG r0 §§3, 8–10.) [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L18-L55] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L160-L182]

| Gate | Check ID | Module-specific execution and numeric pass criterion | Evidence and observed status |
| --- | --- | --- | --- |
| G0 | C01 | Run manual RAO input-contract tests: invalid shapes, missing arrays and nonfinite values raise before either linear solver; zero invalid arrays reach solve (count) | input-contract suite PASS; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L55-L134] |
| G0 | C02 | Run `test_old_solver_rejected_before_body_creation`: zero body constructions (count) for an old dependency version | compatibility suite PASS; [tests/hydrodynamics/capytaine/test_solver_compatibility.py:L26-L41] |
| G0 | C03 | Run `test_coefficients_follow_requested_coordinates`: zero array element mismatches (count), frequency rad/s and heading rad order retained | R1 PASS; [tests/hydrodynamics/capytaine/test_result_coordinates.py:L19-L41] |
| G0 | C04 | Run held-reference shape/units test: 32 periods (count), 9 headings (count), six motions (count) and exact source digest equality | storage suite PASS; digest guard present; engineering unit interpretation remains open; [tests/hydrodynamics/capytaine/test_screening_storage.py:L13-L26] [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L38-L53] |
| G1 | C05 | Native route: `test_one_period_sphere_has_finite_coefficients_and_analytical_added_mass` creates the body through `create_floating_body`, assembles the dataset through `_run_fill_dataset`, and solves one frequency; zero nonfinite A/B entries (count) | NOT_RUN: no usable dependency wheel; [tests/hydrodynamics/capytaine/test_solver_smoke.py:L24-L40] |
| G1 | C06 | NetCDF route: `export_netcdf` then `xr.open_dataset`, recombine real and imaginary excitation; zero differing complex force entries (count) | NOT_RUN: no usable dependency wheel; [tests/hydrodynamics/capytaine/test_lid_and_export.py:L56-L69] |
| G2 | — | NOT_APPLICABLE: no nonlinear static equilibrium stage in this solve path | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L86-L122] |
| G3 | — | NOT_APPLICABLE: no time-stepping dynamics stage in this solve path | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L189-L217] |
| G4 | C07 | Manual coupled RAO, V1: absolute complex response error ≤1e-12 unit response | PASS; [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L40-L45] |
| G4 | C08 | Manual diagonal RAO, V2: amplitude and complex error ≤1e-12 unit response, phase error ≤1e-12 degrees | PASS; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L154-L164] |
| G4 | C09 | Singular operator control, R2: zero response entries differing from minimum-norm zero (count); flag physical equilibrium as unresolved | PASS for documented fallback only; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L167-L176] |
| G4 | C10 | Parallel-axis control: rotational block `[[31,−4,−6],[−4,26,−12],[−6,−12,17]]` kg·m² and symmetry have zero unequal entries (count) | PASS; [tests/hydrodynamics/capytaine/test_screening_comparison.py:L22-L29] |
| G4 | C11 | Finite-depth dispersion residual relative error ≤1e-10 (dimensionless); constant-added-mass natural period relative error ≤1e-10 (dimensionless) | PASS; [tests/hydrodynamics/capytaine/test_screening_comparison.py:L32-L44] |
| G4 | C12 | Interior-lid area differs from waterplane area by ≤5.0 %; merged rectangular area relative error ≤1e-12 and downward normal z-component −1 (dimensionless) | NOT_RUN: no usable dependency wheel; [tests/hydrodynamics/capytaine/test_reference_lid.py:L20-L41] |
| G4 | C13 | `test_empty_lid_raises`: zero untreated bodies returned (count) for a requested empty lid | NOT_RUN: no usable dependency wheel; [tests/hydrodynamics/capytaine/test_lid_and_export.py:L45-L48] |
| G4 | C14 | Configured gravity retained exactly (m/s²), K33/(ρgAwp) error ≤1e-12 (dimensionless), zero configured-inertia element differences (count) | NOT_RUN: no usable dependency wheel; [tests/hydrodynamics/capytaine/test_lid_and_export.py:L90-L107] |
| G4 | C15 | Long-period sphere heave RAO 0.5–1.5 m/m in existing test; this is weaker than AQG r0 G4.8 and does not clear that gate | NOT_RUN; qualification gap #465; [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L208-L227] |
| G4 | C16 | Comparison normalisation returns `[10,−20]` % exactly and resonance-band flags have zero differing flags (count) | PASS for the stated assumptions; [tests/hydrodynamics/capytaine/test_screening_comparison.py:L14-L19] [tests/hydrodynamics/capytaine/test_screening_comparison.py:L47-L51] |
| G5 | C17 | Serialise `diffraction_results` then reconstruct: zero round-trip differences (count), zero schema-validator findings (count) | R3 PASS; schema evidence alone does not clear the appendix gate; [tests/hydrodynamics/capytaine/test_screening_storage.py:L51-L59] |
| G5 | C18 | Before using `export_netcdf` output in a report, enumerate every exported dataset and its input fingerprint, dependency version, G0/G1/G4 record, views, deviations and independent signature; zero missing fields or unmatched fingerprints (count) | executable evidence checklist, NOT_RUN: no report or native dataset generated; AQG r0 §§9, 13; output route [src/digitalmodel/hydrodynamics/capytaine/results.py:L27-L44] |

Native G1 loading is an in-process Capytaine body/dataset solve and NetCDF readback, as C05/C06 specify; the module writes no licensed-program input requiring an OrcaWave validation route or AQWA hydrostatics run. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L100-L122] [src/digitalmodel/hydrodynamics/capytaine/results.py:L27-L44] [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L18-L55]

Full G4 remains uncleared: independent displaced volume, weight–buoyancy balance, centres and GM, normal orientation, waterline closure, shortest-wavelength mesh resolution, irregular-frequency range treatment and image evidence must be supplied per AQG r0 §8; the existing tests above do not jointly assert these quantities. Follow-up #465 and issue pending (5). [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L142-L167] [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L194-L250] [tests/hydrodynamics/capytaine/test_reference_lid.py:L20-L41]

## 6. Known failure modes and pitfalls

The sweep records error suppression, defaults and assumptions separately from confirmed defects; a listed default is not by itself a defect. Locations refer to the code pin; `no detection check exists` means no matching shipped assertion was identified. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L140-L141] [src/digitalmodel/hydrodynamics/capytaine/models.py:L74-L104]

sweep: 49 found

| ID | Symptom / assumption and cause | Detection check | Tracking | Evidence |
| --- | --- | --- | --- | --- |
| S01 | Hydrostatics error is logged and execution continues with partially assigned body properties | no detection check exists | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L124-L141] |
| S02 | Unrecognised Green-function setting silently selects Delhommeau | no detection check exists | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L166-L171] |
| S03 | SolverConfig.engine is stored but DefaultMatrixEngine is always constructed | no detection check exists for alternate engine requests | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L155-L182] |
| S04 | SolverConfig.method is logged but not supplied to fill_dataset | no detection check exists | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L183-L214] |
| S05 | Absent dataset coefficient variables leave optional result arrays unset | no detection check exists; C03 covers ordering only | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L233-L255] |
| S06 | Native RAO values are extracted without explicit dimension reordering and use metadata from the BEM result | no detection check exists for noncanonical native RAO axes | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L50-L65] |
| S07 | Singular impedance changes to least squares and can return zero despite nonzero force | C09 | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L168-L172] |
| S08 | Optional damping None skips the structural-array loop intentionally | C08 | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L143-L152] |
| S09 | Absent additional damping defaults to a zero matrix | C07 | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L154-L158] |
| S10 | Structural arrays are converted to real float without a separately stated real-only contract | no detection check exists for finite complex structural matrices | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L149-L152] |
| S11 | Unparseable dependency major version becomes zero and is then rejected; not silent success | C02 covers old versions; no detection check exists for malformed version text | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L38-L48] |
| S12 | Explicit non-meshio file format is detected or logged but the loader uses extension detection | no detection check exists for suffix/explicit-format disagreement | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L148-L163] |
| S13 | Unknown extension falls back to meshio; missing meshio raises rather than returning an empty mesh | no detection check exists for the fallback route | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L166-L192] |
| S14 | Unused format-string lookup can return None for AUTO or MESHIO | no detection check exists | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L195-L203] |
| S15 | Centre of mass defaults to zero mesh coordinates | no detection check exists for unintended origin defaults | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L74-L79] |
| S16 | Missing rotation centre selects centre of mass | no detection check exists for offset rotation centres | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L90-L93] |
| S17 | Default heading is zero radians | no detection check exists for physical heading sense | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L96-L112] |
| S18 | Default water depth is infinity (deep water) | no detection check exists for unintentionally omitted finite depth | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L99-L104] |
| S19 | Code-set density defaults recur in WaveConditions, both factories and BEMResult; no unit conversion is applied | C04 checks a held reference, not the standard attribution | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L96-L162] |
| S20 | Default gravity in WaveConditions and BEMResult is code-set; propagation needs a consistent value | C14 | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L99-L104] |
| S21 | BEMResult repeats environmental defaults even for caller-constructed results | no detection check exists for inconsistent manually assembled metadata | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L196-L203] |
| S22 | Period/frequency conversions divide directly and convert dtype=float, with no locale-text, finite-value or monotonic-grid checks | C01 covers manual arrays only; no detection check exists for these factories | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L114-L162] |
| S23 | Solver validation uses nonpositive comparisons but does not reject NaN or nonfinite headings/gravity | no detection check exists | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L62-L84] |
| S24 | Sphere dimensions, centre and resolution have implicit shape defaults | no detection check exists for omitted shape parameters | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L121-L128] |
| S25 | Cylinder radius, length, centre and resolution have implicit defaults | no detection check exists for omitted shape parameters | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L129-L136] |
| S26 | Box dimensions, centre and resolution have implicit defaults | no detection check exists for omitted shape parameters | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L137-L143] |
| S27 | Lid defaults disabled and optional lid radius is unset | C13 covers an explicit empty-lid request, not selection of treatment | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L84-L88] |
| S28 | Requested lid with absent elevation uses zero metres | C12 and C13 | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L75-L89] |
| S29 | Native boundary refinement accepts the authored five-percent area criterion after at most eight attempts | C12 | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L35-L45] |
| S30 | No waterline-component lids returns an empty native Mesh; outer checks must reject it | C13 covers the outer rejection, not disconnected-component coverage | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L48-L70] |
| S31 | Lid geometric float/int conversions and zero face offset are internal mesh indexing, not external-text parsing | C12 | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L35-L100] |
| S32 | Zero held comparison maximum yields NaN, rather than a fabricated relative difference | no detection check exists for zero reference or nonfinite arrays | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L17-L38] |
| S33 | Mass-transfer inputs are float arrays with SI dimensions; dimensions and finiteness are checked but locale text is not normalised | C10 covers numeric reference transfer; no detection check exists for locale text | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L49-L69] |
| S34 | Natural period uses endpoint added mass outside the held period range; caller must flag unbracketed results | C11 covers a constant curve; no detection check exists for unbracketed results | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L97-L132] |
| S35 | Resonance flags use an assumed twenty-percent band and return all false for an empty natural-period list | C16 covers band endpoints; no detection check exists for empty/nonfinite periods | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L135-L151] |
| S36 | Held workbook coefficient and response units are interpreted, not declared by the workbook | C04 is regression only; no independent unit detection check exists | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L4-L8] |
| S37 | BodyMass float conversion and CoM, stiffness, damping and inertia dtype=float conversions assume parseable SI-convertible YAML numerics | C04; no detection check exists for thousands/decimal separators in text | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L65-L77] |
| S38 | Schema rotation conversion uses array position below three rather than motion identity and assumes six canonical motions | C17 covers canonical order; no detection check exists for shuffled or partial motions | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L98-L119] |
| S39 | Schema phase is unknown despite an exp(-iωt) note | C17 preserves this limit; no physical phase-convention check exists | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L122-L125] |
| S40 | Matrix records zip arrays and frequencies, silently truncating a length mismatch; float conversion assumes numeric omega | no detection check exists | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L128-L132] |
| S41 | Added-mass table zips periods and stored values and labels a requested rotational pair as kg | no detection check exists for truncation or rotational table units | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/results.py:L62-L84] |
| S42 | Rotational RAO plots use unconverted amplitudes but label them deg/m; mass/damping plots use translation-only axis labels | no detection check exists | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/results.py:L161-L175] |
| S43 | The same rotational plotting mismatch is visible in plot_rao indexing, unit selection and direct amplitude plot | no detection check exists | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/results.py:L220-L232] |
| S44 | Plot helpers use all motions when supplied an empty list; tables and RAO/excitation plots default to heading index zero | no detection check exists for empty selection; C03 covers explicit table coordinates | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/results.py:L89-L94] [src/digitalmodel/hydrodynamics/capytaine/results.py:L135-L155] [src/digitalmodel/hydrodynamics/capytaine/results.py:L191-L205] [src/digitalmodel/hydrodynamics/capytaine/results.py:L255-L269] |
| S45 | Configured hydrostatic_stiffness is declared on BodyConfig but native hydrostatics overwrites body stiffness without reading that input | no detection check exists | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L80-L88] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L124-L141] |
| S46 | An individual waterline component with no generated lid faces is omitted while other component lids can still be returned | no detection check exists for per-component completeness | issue pending (5) | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L53-L70] |
| S47 | A true symmetry flag with no symmetry plane silently leaves the mesh unwrapped | no detection check exists | issue pending (1) | [src/digitalmodel/hydrodynamics/capytaine/models.py:L63-L65] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L70-L74] |
| S48 | When both mesh sources are supplied, the predefined shape takes precedence over the path | no detection check exists for ambiguous sources | issue pending (3) | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L63-L68] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L78-L80] |
| S49 | Matrix serialisation hardcodes source=solver even for caller-assembled BEMResult arrays; the label is not evidence of a native run | C17 confirms the label, not provenance; no native-origin detection check exists | issue pending (4) | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L128-L132] [tests/hydrodynamics/capytaine/test_screening_storage.py:L44-L55] |

Additional fail-closed handlers rethrow import errors in `solver._import_capytaine`, `mesh_adapter._import_capytaine` and `_load_via_meshio`; manual-array conversion raises ValueError; `compute_rao` tries a second import path rather than replacing results. No error-path `return None`, empty-list/dict return, 1970 sentinel or empty-result cache write was found in the inspected package. [src/digitalmodel/hydrodynamics/capytaine/solver.py:L24-L31] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L26-L35] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L166-L177] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L45-L50] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L112-L120]

Exact sweep commands, followed by full source inspection:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|\.get\(' src/digitalmodel/hydrodynamics/capytaine --glob '*.py'
rg -n 'int\(|float\(|dtype=float|unit|kg|rad|deg|metre|radius|rho|gravity|np\.interp|zip\(' src/digitalmodel/hydrodynamics/capytaine --glob '*.py'
```

| Incident / lead | Confirmed scope at the code pin | Detection / disposition | Evidence |
| --- | --- | --- | --- |
| #2147 | historical coupled-RAO damping-sign concern has an explicit negative-imaginary operator and coupled hand controls | C07/C08 detect its return; manifest inconsistency remains issue pending (2) | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L160-L172] [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L32-L51] |
| #898 | mesh-orientation concern is external to this package's numerical implementation | no detection check exists in this package for uniform normals and positive independent volume; follow-up #898 | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L70-L101] |
| #464 | feature lead rather than an incident; public exports exist but source does not emit the planned calculation sidecar | no detection check exists for citation emission; issue pending (2) | [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L30-L55] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L233-L257] |
| #465 | benchmark expansion lead; current semi-sub tests explicitly exclude quantitative cross-program agreement | C15 is only a weak limiting check; no detection check exists for full independent hull qualification | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L1-L15] |
| #3316 | module documentation/qualification lead, not evidence of accepted results | C18; independent evidence remains required before engineering use | [src/digitalmodel/hydrodynamics/capytaine/results.py:L27-L44] |

All new observations are dated 2026-11-06; existing issue numbers identify read-only leads, not an assertion that a new incident occurred on that date.

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change / shared record |
| --- | --- | --- | --- | --- |
| 2026-11-06 | Initial documentation | A singular response solve can return a zero minimum-norm response to nonzero forcing; do not interpret successful return as equilibrium | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L168-L172] [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L167-L176] | Sections 4–6 distinguish numerical regression from physical qualification; issue pending (4) |
| 2026-11-06 | Initial documentation | Preserve the negative-time damping convention across complex loads and the response operator | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L83-L89] [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L32-L51] | C07/C08 and #2147; shared-log entry not created, issue pending (6) |

## 8. Ownership

The diffraction family quality manager reviews G4 and pack clearance; a verifier independent of the producer is required. (AQG r0 §11.)

UNVERIFIED: a named deputy and producing-maintainer assignment are not recorded in the package; use the deputy diffraction quality role for coverage and the maintainers for assignment. Ambiguous clauses, input conflicts and proposed tolerance changes require family review before use. Assignment and shared-log registration: issue pending (6).

## 9. Dependencies and implementation inventory

| Dependency / direction | Inventory correspondence and interface | Evidence |
| --- | --- | --- |
| Capytaine / numerical backend | module hydrodynamics/capytaine; code requires major version ≥3; optional `fowt` dependency pins 3.0.0; no native solver executed for this pack | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L38-L48] [pyproject.toml:L123-L129] |
| NumPy, xarray / array layer | external-library inventory rows not supplied; extraction orders labelled native arrays before exposing NumPy values | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L219-L278] |
| SciPy / root solve | external-library inventory row not supplied; `finite_depth_wave_number` uses `brentq` | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L87-L94] |
| hydrodynamics/hull_library / upstream | `PipelineXlsxReader` reads held reference coefficients and responses; workbook digests and grids are checked locally | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L26-L62] |
| hydrodynamics/diffraction / downstream | `diffraction_results` constructs `DiffractionResults` and matrix records with source `solver` | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L80-L132] |
| Matplotlib / presentation | external-library inventory row not supplied; plotting is lazy and does not confer qualification | [src/digitalmodel/hydrodynamics/capytaine/results.py:L152-L188] |

### Every dependency-name occurrence in package source

This is a case-insensitive text inventory of every line containing `capytaine` in the package's Python files and manifest; it is not an import or solver probe.

| File | Lines containing the dependency name (including symbols, docstrings and comments) |
| --- | --- |
| `__init__.py` | [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L2-L2]; [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L19-L19]; [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L43-L43] |
| `lid.py` | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L6-L6]; [src/digitalmodel/hydrodynamics/capytaine/lid.py:L30-L30]; [src/digitalmodel/hydrodynamics/capytaine/lid.py:L50-L50]; [src/digitalmodel/hydrodynamics/capytaine/lid.py:L51-L51]; [src/digitalmodel/hydrodynamics/capytaine/lid.py:L83-L83] |
| `manifest.yaml` | [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L2-L2]; [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L3-L3] |
| `mesh_adapter.py` | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L4-L4]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L6-L6]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L9-L9]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L10-L10]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L26-L26]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L27-L27]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L29-L29]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L33-L33]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L34-L34]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L38-L38]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L47-L47]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L52-L52]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L58-L58]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L60-L60]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L61-L61]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L92-L92]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L149-L149]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L161-L161]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L173-L173]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L195-L195]; [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L196-L196] |
| `models.py` | [src/digitalmodel/hydrodynamics/capytaine/models.py:L4-L4]; [src/digitalmodel/hydrodynamics/capytaine/models.py:L20-L20]; [src/digitalmodel/hydrodynamics/capytaine/models.py:L183-L183] |
| `rao.py` | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L19-L19]; [src/digitalmodel/hydrodynamics/capytaine/rao.py:L21-L21]; [src/digitalmodel/hydrodynamics/capytaine/rao.py:L23-L23]; [src/digitalmodel/hydrodynamics/capytaine/rao.py:L46-L46]; [src/digitalmodel/hydrodynamics/capytaine/rao.py:L48-L48]; [src/digitalmodel/hydrodynamics/capytaine/rao.py:L88-L88] |
| `results.py` | [src/digitalmodel/hydrodynamics/capytaine/results.py:L4-L4]; [src/digitalmodel/hydrodynamics/capytaine/results.py:L37-L37] |
| `screening_io.py` | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L107-L107]; [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L123-L123] |
| `solver.py` | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L4-L4]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L19-L19]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L24-L24]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L26-L26]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L30-L30]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L34-L34]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L35-L35]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L100-L100]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L101-L101]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L156-L156]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L157-L157]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L161-L161]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L174-L174]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L176-L176]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L266-L266]; [src/digitalmodel/hydrodynamics/capytaine/solver.py:L298-L298] |

### Map basis, ranking and complete family inventory

Map data pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work`; map's declared source revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. This is a separate data pin from the code pin in the header. The following 31 rows transcribe every implementation assigned by that map to `irregular-frequency-treatment`; exactly one row is inside this package. All other families have zero in-package rows in that map revision. The same family count is exposed in the code-pin map overview. [docs/registry/implementation-map.md:L16-L23]

Consequently the counted-family ranking has one entry: `irregular-frequency-treatment` (1 in-package row, 31 total rows). No subpackage is excluded from this pack's scope. [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L51-L116] [src/digitalmodel/hydrodynamics/capytaine/lid.py:L16-L121]

The map rows below retain their map symbols and map line coordinates; the supporting citations resolve the actual source at the header's code pin, so shifted locations do not silently change the inventory meaning.

| Map path | Map symbol and line | Source at code pin |
| --- | --- | --- |
| `docs/api/cfd/cases/wave_excited_body/generate_rao_reference.py` | `compute_reference`; 65 | [docs/api/cfd/cases/wave_excited_body/generate_rao_reference.py:L65-L116] |
| `docs/reports/2026-10-09-compute-program/capytaine_barge.py` | `solve`; 44 | [docs/reports/2026-10-09-compute-program/capytaine_barge.py:L44-L78] |
| `scripts/benchmark/digitize_validation_plots.py` | `build_case_27`; 59 | [scripts/benchmark/digitize_validation_plots.py:L59-L122] |
| `scripts/benchmark/digitize_validation_plots.py` | `build_case_32`; 201 | [scripts/benchmark/digitize_validation_plots.py:L201-L271] |
| `scripts/benchmark/solver_metadata.py` | `_extract_from_spec`; 79 | [scripts/benchmark/solver_metadata.py:L79-L133] |
| `scripts/benchmark/solver_metadata.py` | `_build_common`; 199 | [scripts/benchmark/solver_metadata.py:L199-L214] |
| `scripts/benchmark/solver_metadata.py` | `_build_aqwa_meta`; 217 | [scripts/benchmark/solver_metadata.py:L217-L251] |
| `src/digitalmodel/floating_wind/solver_tier.py` | `build_orcawave_spec`; 190 | [src/digitalmodel/floating_wind/solver_tier.py:L190-L260] |
| `src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py` | `GDFHandler.write`; 178 | [src/digitalmodel/hydrodynamics/bemrosetta/mesh/gdf_handler.py:L178-L230] |
| `src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py` | `create_floating_body`; 51 | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L51-L116] |
| `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | `AQWABackend._build_options`; 569 | [src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:L517-L526] |
| `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | `AQWABackend.build_deck2`; 639 | [src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:L587-L681] |
| `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | `SolverOptions._migrate_irregular_frequency_fields`; 609 | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L650-L687] |
| `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | `ControlSurfaceSpec.check_control_surface_defined`; 885 | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L942-L961] |
| `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | `BodySpec.resolve_control_surface`; 950 | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1006-L1017] |
| `src/digitalmodel/hydrodynamics/diffraction/mesh_orientation.py` | `_refuse_if_unreliable`; 414 | [src/digitalmodel/hydrodynamics/diffraction/mesh_orientation.py:L414-L452] |
| `src/digitalmodel/hydrodynamics/diffraction/mesh_orientation.py` | `repair_gdf_text`; 493 | [src/digitalmodel/hydrodynamics/diffraction/mesh_orientation.py:L493-L609] |
| `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` | `_slot_passthrough_extensions`; 57 | [src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py:L57-L61] |
| `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` | `_iter_mesh_slots`; 64 | [src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py:L64-L100] |
| `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` | `_check_supported_formats`; 145 | [src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py:L145-L166] |
| `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | `_build_body_dict`; 263 | [src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:L301-L455] |
| `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | `_build_general_section`; 444 | [src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:L477-L567] |
| `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` | `AQWAInputParser._parse_solver_options`; 190 | [src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:L190-L209] |
| `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` | `OrcaWaveInputParser._parse_solver_options`; 557 | [src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:L567-L598] |
| `src/digitalmodel/hydrodynamics/hull_library/line_generator/exporter.py` | `export_gdf`; 41 | [src/digitalmodel/hydrodynamics/hull_library/line_generator/exporter.py:L41-L86] |
| `src/digitalmodel/hydrodynamics/hull_library/line_generator/exporter.py` | `export_orcawave`; 94 | [src/digitalmodel/hydrodynamics/hull_library/line_generator/exporter.py:L94-L125] |
| `src/digitalmodel/hydrodynamics/hull_library/mesh_refiner.py` | `_subdivide_once`; 85 | [src/digitalmodel/hydrodynamics/hull_library/mesh_refiner.py:L95-L195] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` | `CathodicProtection.ABS_gn_offshore_2018`; 811 | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L811-L903] |
| `src/digitalmodel/solvers/benchmark/cases.py` | `diffraction_spec`; 122 | [src/digitalmodel/solvers/benchmark/cases.py:L122-L150] |
| `tests/hydrodynamics/diffraction/test_aqwa_deck0_card_order.py` | `_spec`; 23 | [tests/hydrodynamics/diffraction/test_aqwa_deck0_card_order.py:L23-L61] |
| `tests/hydrodynamics/diffraction/test_spec_converter_packaging.py` | `_spec_with_aux`; 38 | [tests/hydrodynamics/diffraction/test_spec_converter_packaging.py:L38-L63] |

### Exact counted-family grep and classification

Use Python `re.search` case-sensitively over every package `.py` line at the code pin with these exact patterns from the map data pin:

```text
irregular_frequenc|irregularFrequency|IrregularFrequency|lid_mesh|lid_panels|generate_lid|lid_z|lid_faces|interior_surface|control_surface
irregular.frequency|free.surface.panel|lid
generate_lid|remove_irregular|lid_z|lid_faces|irregular_frequencies
```

No signal matches `hydrodynamics/capytaine` case-insensitively, so no signal is excluded. Hits are assigned to the smallest enclosing function or class; module statements form one definition. The vocabulary pattern also matches the substring in “valid” and “validation”; these are not new lid algorithms.

| Hit definition | Classification and reason | Evidence |
| --- | --- | --- |
| `lid.py` / `module` (hit lines 6) | module docstring, not an implementation | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L1-L121] |
| `lid.py` / `resolved_lid` (hit lines 16, 17, 25, 33, 38, 41, 42, 43, 45) | listed explicitly in section 3; native area-coverage criterion | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L16-L45] |
| `lid.py` / `_component_lids` (hit lines 48, 52, 60, 61, 62, 63, 66, 67, 68, 69) | listed explicitly in section 3; disconnected-component assembly | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L48-L70] |
| `lid.py` / `merge_lid_cells` (hit lines 73, 74) | listed explicitly in section 3; rectangular aggregation | [src/digitalmodel/hydrodynamics/capytaine/lid.py:L73-L100] |
| `mesh_adapter.py` / `create_floating_body` (hit lines 75, 76, 77, 78, 79, 80, 82, 84, 86, 87, 88, 89, 99, 109, 110) | map inventory row; native lid orchestration | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L51-L116] |
| `mesh_adapter.py` / `_create_predefined_mesh` (hit lines 120) | docstring vocabulary match only; predefined-geometry wrapper | [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L119-L145] |
| `models.py` / `BodyConfig` (hit lines 84, 85, 86, 88) | configuration data, not a lid implementation | [src/digitalmodel/hydrodynamics/capytaine/models.py:L74-L93] |
| `rao.py` / `compute_rao_manual` (hit lines 110) | validation docstring match, unrelated response calculation listed in section 3 | [src/digitalmodel/hydrodynamics/capytaine/rao.py:L72-L182] |
| `screening.py` / `rigid_body_mass_matrix` (hit lines 60) | invalid-dimensions error text match, unrelated method listed in section 3 | [src/digitalmodel/hydrodynamics/capytaine/screening.py:L41-L69] |
| `screening_io.py` / `read_reference` (hit lines 27) | validate docstring match; input reader | [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L26-L62] |
| `solver.py` / `CapytaineSolver` (hit lines 37) | validate docstring match; wrapper | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L34-L267] |
| `solver.py` / `validate_inputs` (hit lines 62, 63) | validation symbol/text match; input checks | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L62-L84] |
| `solver.py` / `solve` (hit lines 97, 103, 104, 105, 106) | validation text match; delegates native solve | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L86-L122] |
| `solver.py` / `run_bem_analysis` (hit lines 287) | validate docstring match; convenience wrapper | [src/digitalmodel/hydrodynamics/capytaine/solver.py:L281-L299] |

A8 self-check: complete inventory 31/31 rows; 14 hit definitions classified; 0 further unlisted second implementations after section 3's explicit listing. `_grid_rectangles` is listed in section 3 even though the exact signals do not hit its body. [src/digitalmodel/hydrodynamics/capytaine/lid.py:L103-L121]

### Whole-repository follow-up

Repository searches include `scripts/`, not only package code. The map inventory above already identifies other lid handling in benchmark tooling, solver metadata, deck writers and mesh exporters; these are map rows, not package misses. [scripts/benchmark/solver_metadata.py:L79-L116] [scripts/benchmark/digitize_validation_plots.py:L59-L80]

The additional whole-repository command was `git grep -n -E 'generate_lid|lid_mesh|compute_rao_manual|finite_depth_wave_number|rigid_body_mass_matrix' HEAD -- '*.py'`; it returned 61 matching lines, including tests and imports. Outside-package map gaps include the example's `generate_lid` mesh launch and the `cylinder_added_mass` and `solve_hydrodynamics` native launches; these are not counted package misses. See issue pending (7). [examples/hydrodynamics/oc4-semisub/prepare_staged_v3.py:L83-L122] [src/digitalmodel/marine_ops/fowt_quasi_static/hydrodynamics.py:L27-L40] [src/digitalmodel/marine_ops/fowt_quasi_static/hydrodynamics.py:L43-L70]

Map follow-up: add the newer `resolved_lid`, `_component_lids`, `merge_lid_cells` and `_grid_rectangles` definitions; assess absent in-package families for coupled RAOs, boundary-element launch/extraction, rigid-body mass transfer, finite-depth dispersion, natural-period interpolation, response serialisation and comparison/resonance screening. These do not become counted families until map rows enter this package. See issue pending (7). [src/digitalmodel/hydrodynamics/capytaine/lid.py:L16-L121] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L72-L182] [src/digitalmodel/hydrodynamics/capytaine/screening.py:L17-L151] [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L80-L132]

## 10. Coverage and open items

### Source and documentation coverage

All nine package Python files and the manifest were read as text; no package-local test folder or pre-existing README/documentation file was found in the package tree at the code pin. The file inventory is the source set cited throughout sections 1–9. [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L1-L55] [src/digitalmodel/hydrodynamics/capytaine/models.py:L1-L217] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L1-L215] [src/digitalmodel/hydrodynamics/capytaine/lid.py:L1-L121] [src/digitalmodel/hydrodynamics/capytaine/solver.py:L1-L299] [src/digitalmodel/hydrodynamics/capytaine/rao.py:L1-L182] [src/digitalmodel/hydrodynamics/capytaine/results.py:L1-L307] [src/digitalmodel/hydrodynamics/capytaine/screening.py:L1-L151] [src/digitalmodel/hydrodynamics/capytaine/screening_io.py:L1-L132] [src/digitalmodel/hydrodynamics/capytaine/manifest.yaml:L1-L26]

Documentation discovery command: `rg -l -i 'capytaine' docs --glob '*.md'`; the results below are references and planning claims, not independent validation evidence.

| Discovery file | Relevant lines inspected at code pin |
| --- | --- |
| `docs/cad-inventory/project-domain-map.md` | [docs/cad-inventory/project-domain-map.md:L59-L59] |
| `docs/cad-inventory/README.md` | [docs/cad-inventory/README.md:L142-L142] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L22-L22] |
| `docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md` | [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L6-L6]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L13-L13]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L19-L19]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L33-L33]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L40-L40]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L45-L45]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L48-L48]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L49-L49]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L53-L53]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L61-L61]; [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L62-L62] |
| `docs/development/report-qualification-audit.md` | [docs/development/report-qualification-audit.md:L16-L16]; [docs/development/report-qualification-audit.md:L18-L18] |
| `docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md` | [docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md:L11-L11]; [docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md:L46-L46]; [docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md:L68-L68] |
| `docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md` | [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L1-L1]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L6-L6]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L10-L10]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L12-L12]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L17-L17]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L23-L23]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L24-L24]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L26-L26]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L28-L28]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L40-L40]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L42-L42]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L43-L43]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L45-L45]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L47-L47]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L53-L53]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L55-L55]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L61-L61]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L63-L63]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L64-L64]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L68-L68]; [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L69-L69] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L9-L9] |
| `docs/plans/2026-06-29-cfd-capability-assessment.md` | [docs/plans/2026-06-29-cfd-capability-assessment.md:L75-L75]; [docs/plans/2026-06-29-cfd-capability-assessment.md:L153-L153] |
| `docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md` | [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L6-L6] |
| `docs/research/hydrodynamics-industry-practice.md` | [docs/research/hydrodynamics-industry-practice.md:L19-L19]; [docs/research/hydrodynamics-industry-practice.md:L31-L31]; [docs/research/hydrodynamics-industry-practice.md:L78-L78]; [docs/research/hydrodynamics-industry-practice.md:L83-L83]; [docs/research/hydrodynamics-industry-practice.md:L87-L87]; [docs/research/hydrodynamics-industry-practice.md:L152-L152]; [docs/research/hydrodynamics-industry-practice.md:L160-L160]; [docs/research/hydrodynamics-industry-practice.md:L161-L161] |
| `docs/plans/2026-05-05-issue-464-capytaine-bem-module.md` | [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L1-L1]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L9-L9]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L16-L16]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L32-L32]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L38-L38]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L40-L40]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L41-L41]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L42-L42]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L45-L45]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L47-L47]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L48-L48]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L53-L53]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L54-L54]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L55-L55]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L59-L59]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L64-L64]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L67-L67]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L72-L72]; [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L73-L73] |
| `docs/reports/2026-10-09-compute-program/README.md` | [docs/reports/2026-10-09-compute-program/README.md:L10-L10]; [docs/reports/2026-10-09-compute-program/README.md:L13-L13]; [docs/reports/2026-10-09-compute-program/README.md:L14-L14] |
| `docs/plans/analysis-evidence-boundary-audit.md` | [docs/plans/analysis-evidence-boundary-audit.md:L50-L50] |
| `docs/api/cfd/cases/wave_excited_body/README.md` | [docs/api/cfd/cases/wave_excited_body/README.md:L119-L119]; [docs/api/cfd/cases/wave_excited_body/README.md:L123-L123]; [docs/api/cfd/cases/wave_excited_body/README.md:L183-L183] |
| `docs/domains/web/authentication.md` | [docs/domains/web/authentication.md:L134-L134] |
| `docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md` | [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L31-L31]; [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L65-L65]; [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L68-L68]; [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L112-L112] |
| `docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md` | [docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md:L9-L9] |
| `docs/domains/hydrodynamics/added-resistance-analysis.md` | [docs/domains/hydrodynamics/added-resistance-analysis.md:L175-L175] |

One discovery file has an identifying name and is not reproduced here; codenaming and the restricted coverage pointer are issue pending (6). No private reference repository was read, no client-derived value is reproduced, and no private sidecar entry is claimed.

The calculation-citation contract was readable from the planning repository; no external standard, licensed copy or published hydrodynamic paper was opened. Adjacent discovery documents were inspected for their module references; complete qualification of their other claims is outside this package pack and remains issue pending (6). Historical plans propose interfaces and acceptance thresholds that are not established by the present exports. [docs/plans/2026-05-05-issue-464-capytaine-bem-module.md:L36-L59] [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L21-L55]

### Test coverage and limitations

The test discovery used `rg -n -i 'capytaine' tests --glob '*.py'` and the complete `tests/hydrodynamics/capytaine` directory. Twelve test files contain 68 static test definitions; parameterisation expands the six solver-free files to 96 cases. Six import-guarded files contain 40 static definitions that could not be collected without the native dependency; no assertion in them is reported as passed.

| Test file | Coverage and result |
| --- | --- |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L1-L252] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_lid_and_export.py:L1-L119] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L1-L180] |
| `tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_rao_manual_input_contract.py:L1-L189] |
| `tests/hydrodynamics/capytaine/test_rao_manual_sign.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_rao_manual_sign.py:L1-L62] |
| `tests/hydrodynamics/capytaine/test_reference_lid.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_reference_lid.py:L1-L41] |
| `tests/hydrodynamics/capytaine/test_result_coordinates.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_result_coordinates.py:L1-L74] |
| `tests/hydrodynamics/capytaine/test_screening_comparison.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_screening_comparison.py:L1-L51] |
| `tests/hydrodynamics/capytaine/test_screening_storage.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_screening_storage.py:L1-L59] |
| `tests/hydrodynamics/capytaine/test_solver_compatibility.py` | PASS: all collected parameterisations; [tests/hydrodynamics/capytaine/test_solver_compatibility.py:L1-L119] |
| `tests/hydrodynamics/capytaine/test_solver_smoke.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_solver_smoke.py:L1-L40] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py` | NOT_RUN: dependency wheel unavailable; [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L1-L227] |

Other discovery matches are consumers in diffraction, hull-library, parametric hull analysis, floating-wind and CFD tests; they are outside this package's test run. Broader consumer coverage is issue pending (5). No worked physical-hull case is claimed as validated. #465 remains the quantitative benchmark follow-up.

### Native test nodes not run

Every node below is NOT_RUN because Capytaine 3.0.0 has no usable wheel; source inspection identifies the definitions, not a successful native collection. The six file-level import skips are recorded separately from these 40 static nodes.

| Existing node | Source definition |
| --- | --- |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderAddedMass::test_surge_added_mass_correlation` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L135-L146] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderAddedMass::test_heave_added_mass_correlation` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L148-L158] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderAddedMass::test_heave_added_mass_magnitude` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L160-L176] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderDamping::test_heave_damping_correlation` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L182-L192] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderDamping::test_damping_mostly_non_negative` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L194-L205] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderExcitation::test_excitation_nonzero_surge_heave` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L211-L220] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderExcitation::test_heave_excitation_peak` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L222-L228] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderSymmetry::test_surge_sway_symmetry` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L234-L242] |
| `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderSymmetry::test_roll_pitch_symmetry` | [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L244-L252] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_requested_lid_is_attached` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L37-L42] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_empty_lid_raises` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L45-L48] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_prescribed_mass_is_retained` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L51-L53] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_native_export_retains_complex_excitation` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L56-L69] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_gravity_reaches_boundary_element_problem` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L90-L93] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_hydrostatics_use_configured_gravity` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L96-L101] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_prescribed_inertia_is_retained` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L104-L107] |
| `tests/hydrodynamics/capytaine/test_lid_and_export.py::test_invalid_prescribed_inertia_raises` | [tests/hydrodynamics/capytaine/test_lid_and_export.py:L110-L119] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4SolveCompletion::test_solve_completes` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L99-L103] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4SolveCompletion::test_correct_dimensions` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L105-L112] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4SolveCompletion::test_hydrostatics_computed` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L114-L120] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4PhysicalSanity::test_heave_added_mass_order_of_magnitude` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L126-L135] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4PhysicalSanity::test_surge_added_mass_order_of_magnitude` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L137-L142] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4PhysicalSanity::test_translational_damping_non_negative` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L144-L150] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4PhysicalSanity::test_excitation_varies_with_heading` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L152-L160] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4RAO::test_rao_computable` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L166-L170] |
| `tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py::TestOC4RAO::test_heave_rao_bounded` | [tests/hydrodynamics/capytaine/test_oc4_semisub_benchmark.py:L172-L180] |
| `tests/hydrodynamics/capytaine/test_reference_lid.py::test_reference_lid_covers_waterplane` | [tests/hydrodynamics/capytaine/test_reference_lid.py:L20-L29] |
| `tests/hydrodynamics/capytaine/test_reference_lid.py::test_lid_rectangle_merging_preserves_area_and_normals` | [tests/hydrodynamics/capytaine/test_reference_lid.py:L32-L41] |
| `tests/hydrodynamics/capytaine/test_solver_smoke.py::test_one_period_sphere_has_finite_coefficients_and_analytical_added_mass` | [tests/hydrodynamics/capytaine/test_solver_smoke.py:L24-L40] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_solver_runs_without_error` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L84-L89] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_added_mass_shape` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L91-L96] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_heave_added_mass_analytical` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L98-L140] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_radiation_damping_positive` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L142-L147] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_excitation_force_nonzero` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L149-L153] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_hydrostatic_stiffness_computed` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L155-L158] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_dataset_contains_expected_variables` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L160-L167] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSolverValidation::test_rejects_negative_frequencies` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L173-L182] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSolverValidation::test_rejects_empty_dofs` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L184-L193] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestRAOComputation::test_rao_from_bem_result` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L199-L206] |
| `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestRAOComputation::test_heave_rao_at_long_period` | [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L208-L227] |

### Unexecuted physical benchmark candidates

These candidates are not section-4 examples or passing validations; dependency and provenance limits remain open under #465 and issue pending (2).

| Case / kind | Source and expected result | Existing test tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| N1 sphere / candidate benchmark, unvalidated | repository analytic-expression claim attributed to Hulme (1982); independent source not opened or confirmed | existing relative bound 15.0 %, an authored test tolerance | `tests/hydrodynamics/capytaine/test_sphere_benchmark.py::TestSphereValidation::test_heave_added_mass_analytical` | NOT_RUN: Capytaine 3.0.0 has no usable wheel; [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L98-L139] |
| N2 cylinder / candidate cross-program comparison, unvalidated | held solver-output comparison; reference provenance requires separate qualification | added-mass magnitude relative error <20.0 % in the existing test | `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py::TestCylinderAddedMass::test_heave_added_mass_magnitude` | NOT_RUN: same dependency limitation; [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L160-L175] |

### Open-item register

Each row is assigned to the module maintainers for disposition and the diffraction quality role for technical acceptance; issue numbers are read-only references and numbered drafts require issue creation before independent A7 clearance.

| Item | Evidence / qualification gap | Tracking |
| --- | --- | --- |
| Coordinate and unit interpretation | UNVERIFIED: mesh coordinate frame, origin, heading sense and held-reference units; rotational plot and summary labels need dimension-aware checks | issue pending (1); sections 2 and 6 |
| Standard traceability | UNVERIFIED: clause titles, density attribution and sphere analytic-source attribution; manifest's opposite damping sign; missing emitted Citation sidecars | issue pending (2); sections 3, 4 and 6 |
| Partial and ignored settings | hydrostatics warning fallback, ignored solver settings, missing coefficient fields and native response ordering lack dedicated assertions | issue pending (3); section 6 |
| Numerical completeness | singular-response qualification, factory finiteness, shape/order/length and interpolation-range checks remain incomplete | issue pending (4); section 6 |
| Physical qualification and native portability | native dependency wheel unavailable; G1/G4 checks NOT_RUN; full mesh/hydrostatics/irregular-frequency evidence and consumer tests remain open | issue pending (5), #465 and #898; sections 4–6 |
| Documentation governance | identifying discovery file needs codenaming; deputy assignment, shared-log record and external-library inventory links not established | issue pending (6); sections 7–10 |
| Map follow-up | new lid definitions and missing in-package method-family rows require map maintenance at its next data revision | issue pending (7); section 9 |
| Report appendix | no engineering report produced; C18 not executed and no independent G4 clearance claimed | #3316; section 5 |

UNVERIFIED: the sphere test's analytic attribution has not been independently confirmed from the cited paper; a repository docstring alone is insufficient to qualify that physical benchmark. See issue pending (2). [tests/hydrodynamics/capytaine/test_sphere_benchmark.py:L1-L14]
