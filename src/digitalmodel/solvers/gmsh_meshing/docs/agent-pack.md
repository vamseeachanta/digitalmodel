# Gmsh meshing knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/gmsh_meshing |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/gmsh_meshing` |
| Pack revision | 0, 2026-10-18 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | FE; mesh preparation also feeds CFD |
| Quality role | FEA quality manager |
| Status | DRAFT; independent clearance pending |
| Map ref | `origin/codex/implmap-work`, `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Citations without a separate revision refer to the code revision above; numerical checks below distinguish code criteria from independently qualified engineering limits. [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L52]

## 1. Purpose and scope

The generator constructs box and cylinder volume meshes, imports STEP through OCC, generates STL surface meshes, delegates MSH/VTK writes to Gmsh, and writes a mesh-only CalculiX INP. [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L68-L157] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L255]

`MeshQualityAnalyzer` evaluates four-node tetrahedra and characteristic edge sizes; the separate synthetic L-tank path validates geometry, physical groups and serialized first-order ASCII MSH2 before atomic promotion. [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L26-L74] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L249-L279] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L76-L107] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L371-L399]

The routing registry assigns the solvers domain maturity `beta`; the generic module README still has description/API placeholders, so neither that README nor historical portfolio counts establish validation. [docs/registry/module-routing.yaml:L354-L359] [docs/domains/solvers/gmsh_meshing/README.md:L5-L17]

Scope includes all eight Python files in this package, including the fixture and parser; the generic methods generate meshes and the INP writer emits nodes/elements/sets rather than a complete analysis with material, load or step definitions. [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L234-L255] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L254-L269]

## 2. Inputs and outputs

| Input | Unit and convention | Range / default | Source and evidence |
| --- | --- | --- | --- |
| Box dimensions | CLI labels length/width/height in m; OCC origin (0,0,0), axes x/y/z | Caller tuple; no local positivity check; target size defaults to 1.0 | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L27-L46] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L68-L82] |
| Cylinder radius and height | CLI m; cylinder axis +z from origin | Caller values; target size 1.0; no local positivity check | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L122-L140] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L92-L102] |
| STEP or STL path | Coordinates passed to OCC/merge; no explicit scale conversion | Must exist; STEP dimension defaults 3; size 1.0 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L111-L157] |
| Analyzer nodes/connectivity | N by 3 coordinates; M by 4 zero-based indices; size in coordinate length unit | No shape, finiteness or empty-input validation before indexing/reduction | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L26-L74] |
| Tank YAML | Explicit m; x transverse breadth, y vertical up, z longitudinal span/roll axis | Schema 1 and exact frame; positive finite numeric dimensions/size; openings must fit | [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L13-L34] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L77-L99] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L150-L178] |
| Tank origin | OCC legs start at (0,0,0); no time or phase variable | Breadth/height/length and leg dimensions required; no dimensional defaults | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L115-L135] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L206-L241] |
| Export mapping | Dimensionless element-name mapping | Function map has six types; configuration dataclass adds quadrilateral and offset/set fields, but is not accepted by the writer | [src/digitalmodel/solvers/gmsh_meshing/models.py:L191-L205] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L205-L229] |

| Output | Unit, shape and convention | Evidence |
| --- | --- | --- |
| Mesh dictionary | Nodes N by 3 in source coordinate units; element names map to tags, connectivity and dimension; generic extraction subtracts 1 from node tags | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L159-L179] |
| Mesh quality | Dimensionless min/max/mean shape, aspect and skewness metrics, poor-element counts and composite score | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L56-L74] [src/digitalmodel/solvers/gmsh_meshing/models.py:L110-L134] |
| Statistics | Node/element counts; max-edge characteristic sizes in coordinate length unit; analyzer chooses only first dictionary element type | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L193-L203] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L93-L129] |
| Generic MSH / VTK | `gmsh.write(output_file)`; suffix determines serialization; no explicit unit/version contract in these methods | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L191] |
| INP | One-based sequential node records; element IDs restart per block; coordinate values written unchanged; NALL generated unconditionally | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L234-L253] |
| Tank MSH2 and summary | ASCII 2.2, first order, SI m; determinant m³ and fluid volume m³; groups fluid=(3,1), walls=(2,2), atmosphere=(2,3) | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L22-L30] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L44-L57] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L12-L16] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L142-L150] |

UNVERIFIED: A common explicit unit contract for generic STEP/STL/MSH/VTK/INP round trips and target program versions has not been established; caller unit consistency must be checked before use; issue pending (5). [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L111-L157] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L234-L253]

## 3. Method and governing standards

| Method / definition | Implemented rule and limit | Evidence |
| --- | --- | --- |
| Generic mesh generation | OCC box/cylinder, STEP import or STL merge; local algorithm enum maps to Algorithm3D codes; unknown enum lookup falls back to 1 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L25-L33] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L61-L66] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L68-L157] |
| `_tetrahedron_jacobian` | V = absolute scalar triple product / 6; q = 3(6V)^(2/3) / sum of six squared edges; near-zero absolute volume/edge sum returns 0.0; this is unsigned shape quality, not a signed determinant | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L131-L168] |
| `_tetrahedron_aspect_ratio` | Largest edge / minimum altitude; altitude = 3V / face area; absolute degeneracy threshold 1e-12; degenerate sentinel 999.0 | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L170-L215] |
| `_tetrahedron_skewness` | 1 - min(face area)/max(face area); maximum face area below 1e-12 returns 1.0 | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L217-L247] |
| `_compute_element_sizes` / statistics | Largest pairwise edge per element; dictionary statistics select the first type and direct arrays assume dimension 3 | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L93-L118] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L249-L279] |
| `MeshQuality` criteria | Good: q > 0.3, aspect < 5.0, skew < 0.7; minimum acceptance: q > 0.1, aspect < 10.0, skew < 0.9; strict comparisons | [src/digitalmodel/solvers/gmsh_meshing/models.py:L69-L85] |
| `quality_score` | Weighted Jacobian/aspect/skew scores 0.50/0.30/0.20; lower Jacobian score is not clamped; the docstring implication from good to score >= 80 needs correction, not use as a reference | [src/digitalmodel/solvers/gmsh_meshing/models.py:L87-L108] |
| `TankFixtureSpec` volumes | Gross L-plan union volume minus two perforated rectangular-member solids; ellipse opening area uses pi times its radii; `scaled(f)` multiplies every length by f | [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L105-L148] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L180-L203] |
| Tank geometry checks | OCC fuse/cut; one connected fluid volume; member bounding-box/opening and net-volume checks; fluid volume error <= max(1e-10 m³, absolute expected volume times 1e-8) | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L115-L135] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L199-L233] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L315-L324] |
| Tank signed orientation | Signed scalar triple product; generated determinant > max(element_size³,1.0) times 1e-12 m³; MSH contract additionally requires finite positive minimum determinant | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L327-L368] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L60-L74] |
| Serialization contract | Structural MSH section/node/physical/element parsing, group/type checks, native reopen, then atomic replacement | [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L33-L139] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L371-L399] |

UNVERIFIED: The docstring attributes quality thresholds to Gmsh best practices without an edition, clause or published derivation; treat them as code thresholds, not governing-standard acceptance limits; issue pending (1). [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L52]

No standards-derived `Citation` sidecar is returned by the quality or fixture result constructors; this pack introduces no standards-derived coefficient and does not reproduce licensed standard values. [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L61-L74] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L96-L104]

## 4. Validated worked examples

One validated closed-form example and two independent-reference candidates use elementary closed-form geometry, not saved output from the generator; the analytic specification case passed, while native geometry comparisons remain NOT_RUN and are not claimed as validated executions. [tests/solvers/gmsh_meshing/test_tank_fixture.py:L140-L154] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L201-L215] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L268-L288]

| Case / classification | Independent source or regression basis; input; expected value and tolerance | Existing test node ID | Recorded offline result at code pin |
| --- | --- | --- | --- |
| V1, closed-form validated example | Synthetic dimensions from `valid_spec_data`; L-prism H(wL+Bl-wl), less thickness times two rectangular-minus-elliptical areas; m³; test uses `pytest.approx` without explicit tolerance; separate hand check abs<=1e-12 m³ | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_spec_uses_frozen_si_y_up_roll_frame_and_analytic_volume` | PASS; independently written arithmetic in the test [tests/solvers/gmsh_meshing/test_tank_fixture.py:L22-L59] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L140-L154] |
| V2, independent-reference candidate | Same synthetic specification and closed form; OCC fluid/member/opening volume error <= 1e-8 relative (dimensionless), bounds <= 1e-6 m; 1 fluid, 2 members, 2 openings; 0 unassigned/overlap surfaces | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_occ_fixture_has_expected_volume_members_openings_and_surface_coverage` | NOT_RUN; pytest setup ERROR from missing gmsh [tests/solvers/gmsh_meshing/test_tank_fixture.py:L62-L143] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L201-L215] |
| V3, independent-reference candidate | `valid_spec_data` scaled by 0.5; fluid volume scales by 0.5³; test uses `pytest.approx` without explicit tolerance; separate hand check abs<=1e-12 m³; same member checks as V2 | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_scaled_fixture_keeps_groups_and_cubic_volume` | NOT_RUN; pytest FAIL before geometry from missing gmsh [tests/solvers/gmsh_meshing/test_tank_fixture.py:L268-L288] |
| R1, regression | Synthetic statistics 1000 nodes, 5000 elements; expected elements/node=5.0 exactly, dimensionless | `tests/solvers/gmsh_meshing/test_gmsh_unit.py::TestMeshStatistics::test_statistics_creation` | PASS [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L69-L86] |
| R2, regression | Synthetic quality fields; expected good and minimum-acceptance flags true and score > 80.0 points; fixture-specific assertion only | `tests/solvers/gmsh_meshing/test_gmsh_unit.py::TestMeshQuality::test_quality_creation` | PASS [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L33-L49] |
| R3, regression | Unit-edge regular tetrahedron coordinates; q > 0.3 and aspect < 5.0, both dimensionless; no independently derived exact metric expected in test | `tests/solvers/gmsh_meshing/test_gmsh_unit.py::TestMeshQualityAnalyzer::test_simple_tetrahedron_quality` | PASS [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L92-L115] |
| R4, regression | Default `INPExportConfig`; offset=1 index, include_sets true, C3D4/C3D8/C3D10 mappings | `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py::TestINPExportConfig::test_inp_export_config_defaults` | PASS; data-model check only [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L38-L46] |
| R5, regression | `examples/cfd/synthetic_l_tank/input.yml`; exact synthetic metadata and specification equality | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_public_fixture_is_synthetic_si_methodology_input` | PASS [tests/solvers/gmsh_meshing/test_tank_fixture.py:L313-L329] |
| R6, regression | Fixture serialization; ASCII 2.2, types 2/4, groups 1/2/3, zero untagged/duplicate elements, signed determinant > 0 m³ | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_serialized_mesh_is_tagged_first_order_ascii_msh2_with_positive_tets` | NOT_RUN; pytest setup ERROR from missing gmsh [tests/solvers/gmsh_meshing/test_tank_fixture.py:L218-L239] |
| R7, regression | Same fixture twice; byte difference=0 bytes and shape q > 0.17 dimensionless; exact prescribed options | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_mesh_options_and_repeat_build_are_deterministic` | NOT_RUN; pytest setup ERROR from missing gmsh [tests/solvers/gmsh_meshing/test_tank_fixture.py:L242-L265] |
| R8, regression | Fixture output promotion; exactly 1 atomic replacement from a different sibling file | `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_source_mesh_promotion_uses_temporary_sibling` | NOT_RUN; pytest FAIL before promotion from missing gmsh [tests/solvers/gmsh_meshing/test_tank_fixture.py:L291-L310] |

Test execution record: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/solvers/gmsh_meshing/ -q -rA -p no:cacheprovider --basetemp=<scratch>` on the header code revision, at BASE and on the uncommitted documentation tree; 41 collected, 20 attempted, 15 passed, 2 failed, 3 setup errors, 21 skipped; native-dependent cases are NOT_RUN for engineering acceptance, not numerical failures. [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L20-L23] [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L157-L229] [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L22-L25] [tests/solvers/gmsh_meshing/test_gmsh_cli.py:L28-L94] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L82]

The class-level CLI guard also skips help/version despite those commands needing no native mesh; separate unchanged public CLI invocations `python -m digitalmodel.solvers.gmsh_meshing.cli --help` and `--version` both exited 0. These are command checks, not extra pytest passes; the original guards were preserved. [tests/solvers/gmsh_meshing/test_gmsh_cli.py:L28-L43] [src/digitalmodel/solvers/gmsh_meshing/cli.py:L19-L24]

The separate `--extra test --extra cfd` setup attempt stopped before collection: `Distribution gmsh==4.15.1 cannot be installed because it does not have a source distribution or wheel for the current platform`; the declared CFD pin is 4.15.1 and native fixture entry raises `ImportError: Gmsh 4.x is required for the tank fixture` when unavailable; issue pending (5). [pyproject.toml:L235-L241] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L73]

## 5. QA gates

This is a proposed mesh-preparation mapping of AQG r0, sections 3–10; its section 10 has no FE row, so the Python-calculation gates cover local checks and the OpenFOAM G1 mesh route covers the downstream handoff; it is not an independent clearance. (AQG r0, sections 3, 5, 8–10.)

| Gate | Applicability and module-specific check IDs | Basis |
| --- | --- | --- |
| G0 inputs | Applicable: C01, C02; fixture schema, explicit SI/frame, invalid-input rejection | [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L27-L34] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L77-L99] |
| G1 load | Applicable: C11–C15; serialized/native target-format checks with program-specific routes below | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L371-L399] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L255] |
| G2 reference calculation | Applicable to local calculations: C21, C22; closed-form volume and shape regression; structural statics are outside this mesh-only output | [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L121-L148] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L131-L168] |
| G3 short dynamics | NOT_APPLICABLE: package constructs meshes and quality results; it has no time-integration output | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L84-L107] [src/digitalmodel/solvers/gmsh_meshing/models.py:L110-L134] |
| G4 diffraction | NOT_APPLICABLE: generic volume/STL mesh preparation and tank fixture do not compute diffraction/hydrostatics/RAOs; downstream diffraction consumers need their own G4 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L111-L155] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L96-L104] |
| G5 evidence appendix | Applicable to evidence completeness: C51; generated signed report appendix remains a downstream requirement | [src/digitalmodel/solvers/gmsh_meshing/models.py:L110-L134] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L44-L57] (AQG r0, section 9.) |

Checks below preserve the repository's tolerances; they do not establish a design-code qualification. [tests/solvers/gmsh_meshing/test_tank_fixture.py:L127-L143] [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L111-L115]

| Check | Executable route and quantity | Numeric pass criterion and unit | Evidence / observed status |
| --- | --- | --- | --- |
| C01 / G0 | Run R5: `load_tank_fixture_spec` against the public synthetic YAML | 0 differing specification/metadata fields (count); length_unit=m and exact three-axis frame | Test assertions; PASS [tests/solvers/gmsh_meshing/test_tank_fixture.py:L313-L329] |
| C02 / G0 | Run `tests/solvers/gmsh_meshing/test_tank_fixture.py::test_spec_rejects_unit_frame_dimension_and_opening_mismatch` | 5/5 mutations rejected (cases), including height=0 m; exact expected error match | Parameterized tests; PASS [tests/solvers/gmsh_meshing/test_tank_fixture.py:L157-L186] |
| C11 / G1 | Run R6 and R7: `inspect_msh2`, `validate_msh2_contract`, native reopen and repeat serialization | 0 missing groups/untagged/duplicate IDs (count); minimum signed determinant > 0 m³; byte difference=0 bytes; q > 0.17 (1) | Test/native reopen records; NOT_RUN, gmsh unavailable [tests/solvers/gmsh_meshing/test_tank_fixture.py:L218-L265] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L371-L399] |
| C12 / G1 | Run `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py::TestINPExport::test_inp_file_has_correct_node_count` and `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py::TestINPExport::test_inp_file_has_correct_element_type` | INP node-count difference=0 nodes; >=1 C3D4 type declaration (count) | Export structure assertions only; NOT_RUN, gmsh unavailable [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L85-L122] |
| C13 / G1 | Downstream OpenFOAM: `prepare_gmsh_poly_mesh` uses `gmshToFoam`, `changeDictionary`, then `checkMesh -allGeometry -allTopology`; compare source MSH and converted boundary contract | 0 command errors and 0 failed mesh checks (count); patch/type/coverage must match exactly | Native conversion log + manifest; NOT_RUN, external binary route [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L60-L74] [src/digitalmodel/solvers/openfoam/gmsh_bridge.py:L29-L34] [src/digitalmodel/solvers/openfoam/gmsh_bridge.py:L299-L328] |
| C14 / G1 | CalculiX: place mesh-only INP in a complete analysis input and use the downstream `FEMChain.solve` (`ccx -i <stem>`) input route; count rejected/duplicate/dangling IDs before any run | 0 load/parse errors, 0 duplicate element IDs and 0 unresolved node references (count) | Proposed native acceptance record; NOT_RUN under solver restriction; no automated load detection check exists for this exporter; issue pending (2) [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L189] |
| C15 / G1 | VTK: generate with `save_mesh_vtk`, reopen in the intended visualization reader/version and compare nodes/connectivity against source | 0 missing nodes/elements and 0 reader errors (count) | Proposed reader record; NOT_RUN, target reader/version unqualified; no detection check exists; issue pending (5) [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L185] |
| C21 / G2 | Run V1, then V2/V3 when native Gmsh is available; `TankFixtureSpec.expected_fluid_volume` compared to test-written closed form and OCC | Separate V1 hand check abs<=1e-12 m³; V2 fluid rel<=1e-8 (1), member bounds abs<=1e-6 m; V3 hand check abs<=1e-12 m³ against original volume times 0.5³ | V1 PASS; separate closed-form/cubic-scaling hand checks PASS at abs<=1e-12 m³; native comparisons NOT_RUN [tests/solvers/gmsh_meshing/test_tank_fixture.py:L122-L154] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L201-L215] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L268-L288] |
| C22 / G2 | Run R3: `analyze_tetrahedral_mesh` on regular tetrahedron | q > 0.3, aspect < 5.0, skew < 0.7 (all 1); quality.is_good true | Regression assertions; PASS; unsigned metric cannot detect inversion [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L92-L115] [src/digitalmodel/solvers/gmsh_meshing/models.py:L78-L85] |
| C51 / G5 | Hand audit section-4 records against pytest summaries at the code pin, preserving native blockers separately | 0 cases without node ID/status/input/reference/tolerance (count); 0 unreported failures/errors (count) | Case/evidence table audit; PASS for this pack; automatic analysis appendix/signature not implemented; issue pending (5) [tests/solvers/gmsh_meshing/test_tank_fixture.py:L140-L154] (AQG r0, section 9.) |

Gmsh native MSH loading is the reopen route C11; OpenFOAM conversion is C13; CalculiX mesh loading is C14, and VTK reader loading is C15. ANSYS and Abaqus appear in the package overview, but the concrete save methods here are VTK/MSH/INP; no ANSYS-specific writer or load path is established by that overview, so an ANSYS/Abaqus native acceptance route is NOT_RUN until format/target qualification; issue pending (5). [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L3-L4] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L255]

## 6. Known failure modes and pitfalls

sweep: 53 found

The count is reviewed risk/guard sites, including defensive and intentional defaults; it is not a count of confirmed defects. Each row retains the source location and a detection route or an explicit missing check. [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L240-L243] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L60-L74]

Exact package sweep commands (text inspection only):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b1970\b|\b(?:int|float)\(|=\s*0(?:\.0)?\b|default|assum|unit|1e-|0\.0[0-9]|0\.0?5\b|cache' src/digitalmodel/solvers/gmsh_meshing --glob '*.py'
rg -n 'return|except|continue|finally|pass|setdefault|\.get\(' src/digitalmodel/solvers/gmsh_meshing --glob '*.py'
```

| Site | Symptom / cause | Existing detection or gap | Tracking |
| --- | --- | --- | --- |
| S01 [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L17-L22] | ImportError/OSError silently replaces both exported classes with None, obscuring which import failed | no detection check exists | issue pending (5) |
| S02 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L12-L16] | Native import failure becomes GMSH_AVAILABLE=False; dependent tests self-skip | C11 (native absence is exposed, not a completed load) | issue pending (5) |
| S03 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L61-L66] | Unknown algorithm lookup falls back to code 1 without rejecting the request | no detection check exists | issue pending (5) |
| S04 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L68-L82] | Box size defaults to 1.0 with no local finite/positive or unit guard | no detection check exists | issue pending (5) |
| S05 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L92-L102] | Cylinder size defaults to 1.0; no local finite/positive or unit guard | no detection check exists | issue pending (5) |
| S06 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L111-L123] | STL size defaults to 1.0; coordinates imported without explicit unit conversion | no detection check exists | issue pending (5) |
| S07 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L132-L149] | STEP size defaults to 1.0 and dimension to 3; source-unit interpretation delegated | no detection check exists | issue pending (5) |
| S08 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L159-L179] | Generic extraction assumes node tags equal row index plus 1; sparse or reordered tags corrupt connectivity; zero element types return an unchecked empty element dictionary | no detection check exists | issue pending (2) |
| S09 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L224-L229] | Function default export map omits quadrilaterals listed by INPExportConfig | no detection check exists | issue pending (2) |
| S10 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L240-L243] | Unmapped or lower-dimensional element blocks silently skipped by continue | no detection check exists | issue pending (2) |
| S11 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L237-L253] | INP node/connectivity casts assume integer tags and unchanged units; element IDs restart for every block | C12 checks node count only; no detection check exists for sparse tags or duplicate element IDs | issue pending (2) |
| S12 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L268-L269] | STEP box construction defaults dimensions to (1,1,1) without a unit contract | no detection check exists | issue pending (5) |
| S13 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L270-L273] | STEP cylinder defaults radius=1.0 and height=2.0 without a unit contract | no detection check exists | issue pending (5) |
| S14 [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L274-L276] | STEP sphere defaults radius=1.0 without a unit contract | no detection check exists | issue pending (5) |
| S15 [src/digitalmodel/solvers/gmsh_meshing/cli.py:L28-L31] | Box CLI labels lengths m and defaults size=1.0; positivity is not checked by Click type=float | no detection check exists | issue pending (5) |
| S16 [src/digitalmodel/solvers/gmsh_meshing/cli.py:L76-L90] | Box analysis falls back among element types; four-node connectivity is used as the tetrahedron criterion | C22 checks only known tetrahedra; no detection check exists for a four-node non-tet | issue pending (1) |
| S17 [src/digitalmodel/solvers/gmsh_meshing/cli.py:L113-L119] | Box no-tetrahedron branch only prints status; broad exception catch prints error then exits 1 | no detection check exists for fallback/error message paths | issue pending (5) |
| S18 [src/digitalmodel/solvers/gmsh_meshing/cli.py:L123-L125] | Cylinder CLI assumes m and defaults target size=1.0 | no detection check exists | issue pending (5) |
| S19 [src/digitalmodel/solvers/gmsh_meshing/cli.py:L159-L174] | Cylinder analyze output absent if tetrahedron key absent; broad catch prints error then exits 1 | no detection check exists | issue pending (5) |
| S20 [src/digitalmodel/solvers/gmsh_meshing/models.py:L65-L65] | Poor-Jacobian count defaults to 0 without deriving it from metric fields | no detection check exists | issue pending (1) |
| S21 [src/digitalmodel/solvers/gmsh_meshing/models.py:L66-L66] | Poor-aspect count defaults to 0 without deriving it from metric fields | no detection check exists | issue pending (1) |
| S22 [src/digitalmodel/solvers/gmsh_meshing/models.py:L67-L67] | Poor-skew count defaults to 0 without deriving it from metric fields | no detection check exists | issue pending (1) |
| S23 [src/digitalmodel/solvers/gmsh_meshing/models.py:L69-L108] | Thresholds and weighted score are local constants; is_good does not imply score>=80 despite docstring | R2/C22 exercise one good fixture only; no detection check exists for the general implication | issue pending (1) |
| S24 [src/digitalmodel/solvers/gmsh_meshing/models.py:L159-L162] | Zero or negative node count returns elements_per_node=0 rather than rejecting invalid counts | no detection check exists | issue pending (1) |
| S25 [src/digitalmodel/solvers/gmsh_meshing/models.py:L191-L205] | Config include_sets=True and node_offset=1 are data defaults; writer signature does not consume them | no detection check exists beyond data-model defaults | issue pending (2) |
| S26 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L93-L104] | Statistics choose first element dictionary value; direct array assumes dimension=3 | no detection check exists for mixed element dictionaries | issue pending (1) |
| S27 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L146-L153] | Absolute volume hides inversion; volume below 1e-12 returns q=0, dependent on length units | no detection check exists | issue pending (1) |
| S28 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L156-L162] | Zero edge-sum initialization; squared-edge sum below 1e-12 returns q=0 at another dimensional cutoff | no detection check exists | issue pending (1) |
| S29 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L191-L198] | Absolute volume below 1e-12 returns aspect=999.0, a scale-dependent sentinel | no detection check exists | issue pending (1) |
| S30 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L200-L213] | Face area <=1e-12 skipped; minimum-altitude fallback returns 999.0 | no detection check exists | issue pending (1) |
| S31 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L229-L245] | Max face area below 1e-12 returns skew=1.0; scale-dependent area guard | no detection check exists | issue pending (1) |
| S32 [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L264-L277] | Element-size array and max-edge accumulator start at 0; empty/underspecified connectivity has no explicit validation | no detection check exists | issue pending (1) |
| S33 [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L27-L34] | float conversion is guarded by numeric-type and finite-positive checks; external numeric strings are refused | C02 covers zero height; no detection check exists for all numeric-string/nonfinite cases | issue pending (3) |
| S34 [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L77-L98] | Schema equality admits True as 1; m and exact frame enforced; unknown keys not rejected | C01/C02 cover units and frame; no detection check exists for boolean schema or unknown fields | issue pending (3) |
| S35 [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L180-L203] | Scaling reconstructs the dataclass directly, preserving SI label without re-parsing mapping | C21 scaled native path | issue pending (5) |
| S36 [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L244-L251] | yaml.safe_load overwrites duplicate keys before validation; no strict duplicate-key loader | no detection check exists | issue pending (3) |
| S37 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L77-L97] | Repeated section names overwrite dictionary entries; lines outside sections are ignored; missing terminator is re-raised | no detection check exists for duplicate sections | issue pending (3) |
| S38 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L103-L103] | Declared node count is converted from external integer text; separators or malformed counts raise without a format-specific diagnostic | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S39 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L108-L108] | External node-tag int conversion enters a dictionary: duplicate node IDs overwrite earlier rows | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S40 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L108-L109] | Coordinate float conversion assumes whitespace/dot numeric syntax; width and finiteness are not explicitly validated | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S41 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L114-L114] | External physical-name count is converted with int; declaration is checked against unique dictionary entries rather than raw row count | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S42 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L117-L118] | Physical dimension uses external int conversion; no explicit allowed-dimension guard | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S43 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L118-L121] | Physical ID uses external int conversion; duplicate names overwrite before final unique-count comparison | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S44 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L127-L127] | External element count uses int conversion and is compared with appended row count | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S45 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L129-L138] | Every element field uses external int conversion; tag_count and node-arity slices lack explicit range/type guards | C11 checks valid output only; no detection check exists for this negative-input path | issue pending (3) |
| S46 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L73] | Import error is re-raised with required version context; it is not swallowed | C11; native blocker recorded | issue pending (5) |
| S47 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L22-L30] | Zero binary/save-all/seed and one-thread reproducibility options are intentional constants, not error defaults | C11 exact mesh-option assertions | issue pending (5) |
| S48 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L207-L213] | Bounding-box/volume tolerances have absolute scale floors; comparisons assume finite native geometry values | C21 validates nominal fixture; no detection check exists for nonfinite native mass/bounds | issue pending (3) |
| S49 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L280-L306] | Every non-top boundary becomes walls; empty/overlap summary tuples follow construction, not an independent surface inventory | C11 nominal groups; no detection check exists for a misclassified surface | issue pending (3) |
| S50 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L315-L324] | Volume comparison uses absolute/relative floors; abs(NaN-expected)>tolerance would be false | C21 nominal volume; no detection check exists for nonfinite native volume | issue pending (3) |
| S51 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L327-L340] | Determinant cutoff has element-size³/1.0 floor in assumed m³; shape result is not itself gated here | C11/C22 cover nominal shape/orientation; no detection check exists for all scale regimes | issue pending (1) |
| S52 [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L343-L357] | Native node tags converted to integer lookup; first-order tets required; empty determinants raise explicitly | C11 nominal native mesh | issue pending (5) |
| S53 [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L33-L57] | Empty tetrahedron determinant list returns NaN; validator then rejects it as nonfinite | C11 validator path; no detection check exists for explicit empty-MSH negative fixture | issue pending (3) |

There are 6 exception handlers, 1 `continue`, no explicit `return None`, no direct empty-list/dict return on an error path, no 1970 default and no cache-write implementation in the inspected package; the six handlers include two re-raises and two CLI exits, so those are not silent success. [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L17-L22] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L12-L16] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L240-L243] [src/digitalmodel/solvers/gmsh_meshing/cli.py:L117-L119] [src/digitalmodel/solvers/gmsh_meshing/cli.py:L172-L174] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L73] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L86-L89]

Quality probes at the code pin reproduced unchanged positive q after swapping two vertices, q=0 after scaling the same shape by 1e-5, and good=true with score=71.7 points for q=0.31, aspect=4.9 and skew=0.69; these paths must not be an orientation or minimum-score hand-check reference; issue pending (1). [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L146-L168] [src/digitalmodel/solvers/gmsh_meshing/models.py:L78-L108]

Issue search treated #153 and #155 as capability requests and #662 as delivered bridge context, rather than four presumed incidents from the inventory estimate; confirmed package-specific problems above carry drafts rather than an invented incident history. [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L18-L23] [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L69-L81]

## 7. Lessons log

| Date | Evidence key | Lesson / pack change | Shared-log reference |
| --- | --- | --- | --- |
| 2026-10-18 | mesh-quality-orientation | Distinguish unsigned shape q from signed orientation; section 3 names both and section 6 records the risk | issue pending (1); no shared-log entry created [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L146-L168] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L142-L150] |
| 2026-10-18 | mesh-export-contract | Node-count assertions alone do not prove element numbering/completeness; section 5 separates structural tests from target-program load | issue pending (2) [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L237-L253] |
| 2026-10-18 | mesh-native-coverage | Preserve missing-native results separately from passed schema/quality tests; section 4 records the blocked geometry cases | issue pending (5) [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L82] |

## 8. Ownership

Proposed review roles: FEA quality manager; deputy: calculations quality manager for solver-free formula checks, with CFD quality manager consultation for the OpenFOAM handoff; unresolved classifications and tolerances go to the family quality manager, then the maintainer for cross-family scope. (AQG r0, section 11; role assignment proposed in this draft.)

UNVERIFIED: Deputy assignment and durable shared-log/index entry are not confirmed; maintainer action is tracked by issue pending (5).

## 9. Dependencies

Upstream dependencies are NumPy for array/metric calculations, PyYAML for fixture ingestion and Click for the CLI; OCC and Gmsh provide geometry, meshing and serialization. [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L7-L9] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L244-L251] [src/digitalmodel/solvers/gmsh_meshing/cli.py:L19-L39] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L75-L83]

The module version is 1.0.0; the solver extra couples Gmsh with a licensed interface, while the separate CFD extra pins only gmsh 4.15.1; no solver extra or licensed interface was installed for these checks. [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L7-L7] [pyproject.toml:L235-L241]

### Implementation-map inventory and counted-family result

Map evidence uses `docs/registry/implementation-map.yaml` at the separate map ref and data revision in the header; the code pin remains unchanged. The computed inventory is empty: all 13 map families have 0 implementation rows inside this package, so there are 0 counted families and the counted-family grep is `NOT_APPLICABLE` for that reason. The six package entries in `rejected_candidates` are name matches for irregular-frequency treatment, not implementation rows; they do not make a counted family. (Implementation map, schema 1, header `revision`, `families[].implementations`, `rejected_candidates`, map ref pinned above.)

| Inventory | In-package implementation rows | Counted-family grep |
| --- | --- | --- |
| No module map family | 0 | NOT_APPLICABLE; no family has an implementation path in the header package |

Map follow-up: add the section-3 method families before counting them: primitive/STEP/STL mesh generation and OCC booleans; tetrahedral shape/aspect/skewness/size/statistics criteria; L-plan/perforated-member analytic geometry and signed determinants; MSH structural parsing, physical groups, deterministic serialization and INP numbering/export. Include both `_signed_determinant` functions and the differing INP maps; issue pending (4). [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L68-L179] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L131-L279] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L105-L148] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L167-L196] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L360-L368] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L100-L150] [src/digitalmodel/solvers/gmsh_meshing/models.py:L191-L205] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L224-L253]

### Repository-wide related implementations

Repository search included tracked Python in `src/`, `scripts/`, `tests/` and examples, with this command; matches are discovery evidence, not proof of equivalent units, element types or qualification. [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L159-L179] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L167]

```bash
git grep -nE 'GMSHMeshGenerator|MeshQualityAnalyzer|gmsh[.]model[.]mesh|def _signed_determinant|class INPWriter|tetrahedron_(jacobian|aspect_ratio|skewness)' -- '*.py'
```

| Related definition | Relationship and limit | Evidence |
| --- | --- | --- |
| `GmshMeshBuilder.build` | Separate wetted-panel mesh/export route; GDF, DAT and MSH; not the volume-mesh generator | [src/digitalmodel/hydrodynamics/diffraction/gmsh_mesh_builder.py:L289-L330] |
| `remesh_coarsen` | Separate STL-mediated Gmsh surface-remeshing algorithm | [src/digitalmodel/hydrodynamics/hull_library/decimation_gmsh.py:L162-L212] |
| `INPWriter._write_elements` | Separate writer keeps element ID across blocks, unlike this package writer; do not silently substitute other element mappings | [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L167] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L240-L250] |
| `prepare_gmsh_poly_mesh` | Downstream conversion delegates to this package parser/validator; it is not a duplicate parser | [src/digitalmodel/solvers/openfoam/gmsh_bridge.py:L260-L266] [src/digitalmodel/solvers/openfoam/gmsh_bridge.py:L299-L328] |
| Synthetic smoke script | Consumes tank fixture generation and downstream bridge rather than reimplementing volume/quality checks | [scripts/cfd/run_synthetic_tank_3d_smoke.py:L95-L104] [scripts/cfd/run_synthetic_tank_3d_smoke.py:L183-L190] |

### Solver-name occurrence inventory

The inventory below enumerates every matching source line for `OpenFOAM|ANSYS|CalculiX|[Gg][Mm][Ss][Hh]`, including comments, docstrings, imports, identifiers and API calls, grouped by enclosing definition; all are text-only dependencies, not executed acceptance evidence. [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L3-L4] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L12-L16]

| Definition / source location | Dependency text occurrences (source line numbers) | Citation |
| --- | --- | --- |
| `__init__.py` / `module statements` | 3, 4, 18, 21, 30 | [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L3-L30] |
| `_msh2_contract.py` / `module statements` | 1 | [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L1-L1] |
| `_tank_fixture_gmsh.py` / `module statements` | 1 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L1-L1] |
| `_tank_fixture_gmsh.py` / `_gmsh_module` | 68, 70, 72, 73 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L73] |
| `_tank_fixture_gmsh.py` / `build_tank_fixture` | 82, 83, 85, 87, 88, 89, 90, 91, 92, 93, 94, 107 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L82-L107] |
| `_tank_fixture_gmsh.py` / `_set_mesh_options` | 110, 112 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L110-L112] |
| `_tank_fixture_gmsh.py` / `_build_occ_geometry` | 115, 117, 128 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L115-L128] |
| `_tank_fixture_gmsh.py` / `_build_perforated_members` | 138, 139, 148, 157, 162, 163 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L138-L163] |
| `_tank_fixture_gmsh.py` / `_validated_member` | 200, 208, 211, 216, 217 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L200-L217] |
| `_tank_fixture_gmsh.py` / `_longitudinal_opening` | 236, 244, 252 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L236-L252] |
| `_tank_fixture_gmsh.py` / `_transverse_opening` | 258, 266, 274 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L258-L274] |
| `_tank_fixture_gmsh.py` / `_classify_and_tag_surfaces` | 280, 281, 288, 293, 294, 295 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L280-L295] |
| `_tank_fixture_gmsh.py` / `_is_top_surface` | 304, 305 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L304-L305] |
| `_tank_fixture_gmsh.py` / `_add_physical_group` | 309, 311, 312 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L309-L312] |
| `_tank_fixture_gmsh.py` / `_validate_occ_volume` | 316, 318 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L316-L318] |
| `_tank_fixture_gmsh.py` / `_generate_and_check_mesh` | 328, 330, 331, 332 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L328-L332] |
| `_tank_fixture_gmsh.py` / `_mesh_arrays` | 343, 346, 349 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L343-L349] |
| `_tank_fixture_gmsh.py` / `_write_verified_mesh` | 371, 378, 381, 382, 383 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L371-L383] |
| `_tank_fixture_gmsh.py` / `_validate_reopened_mesh` | 390, 392, 393, 397 | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L390-L397] |
| `cli.py` / `module statements` | 3, 14, 20 | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L3-L20] |
| `cli.py` / `cli` | 22, 23, 24 | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L22-L24] |
| `cli.py` / `box_cmd` | 37, 38, 42 | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L37-L42] |
| `cli.py` / `cylinder_cmd` | 131, 132, 136 | [src/digitalmodel/solvers/gmsh_meshing/cli.py:L131-L136] |
| `mesh_generator.py` / `module statements` | 3, 4, 13, 14, 16 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L3-L16] |
| `mesh_generator.py` / `GMSHMeshGenerator` | 19, 20, 25 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L19-L25] |
| `mesh_generator.py` / `GMSHMeshGenerator.__init__` | 36, 38 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L36-L38] |
| `mesh_generator.py` / `GMSHMeshGenerator.initialize` | 43, 45, 46 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L43-L46] |
| `mesh_generator.py` / `GMSHMeshGenerator.finalize` | 50, 52 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L50-L52] |
| `mesh_generator.py` / `GMSHMeshGenerator._setup_mesh` | 57, 58 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L57-L58] |
| `mesh_generator.py` / `GMSHMeshGenerator._apply_mesh_settings` | 63, 66 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L63-L66] |
| `mesh_generator.py` / `GMSHMeshGenerator.generate_simple_box_mesh` | 79, 80, 82 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L79-L82] |
| `mesh_generator.py` / `GMSHMeshGenerator.generate_cylinder_mesh` | 99, 100, 102 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L99-L102] |
| `mesh_generator.py` / `GMSHMeshGenerator.generate_surface_mesh_from_stl` | 121, 123 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L121-L123] |
| `mesh_generator.py` / `GMSHMeshGenerator.generate_mesh_from_step` | 146, 147, 149 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L146-L149] |
| `mesh_generator.py` / `GMSHMeshGenerator._extract_mesh_data` | 160, 161, 165, 166, 169 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L160-L169] |
| `mesh_generator.py` / `GMSHMeshGenerator.save_mesh_vtk` | 184, 185 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L184-L185] |
| `mesh_generator.py` / `GMSHMeshGenerator.save_mesh_msh` | 188, 190, 191 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L188-L191] |
| `mesh_generator.py` / `GMSHMeshGenerator.get_mesh_statistics` | 196 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L196-L196] |
| `mesh_generator.py` / `GMSHMeshGenerator.export_mesh_inp` | 209, 211, 215, 222 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L209-L222] |
| `mesh_generator.py` / `GMSHMeshGenerator.create_step_geometry` | 260, 269, 273, 276, 279, 281 | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L260-L281] |
| `models.py` / `module statements` | 3 | [src/digitalmodel/solvers/gmsh_meshing/models.py:L3-L3] |
| `models.py` / `MeshAlgorithm` | 34 | [src/digitalmodel/solvers/gmsh_meshing/models.py:L34-L34] |
| `models.py` / `MeshQuality` | 48 | [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L48] |
| `models.py` / `INPExportConfig` | 193, 205 | [src/digitalmodel/solvers/gmsh_meshing/models.py:L193-L205] |
| `quality_analyzer.py` / `MeshQualityAnalyzer.compute_mesh_statistics` | 97 | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L97-L97] |
| `tank_fixture.py` / `module statements` | 1, 254 | [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L1-L254] |

Inventory result: 145 source lines in 46 definition/module groups; ANSYS and OpenFOAM occur only in package/fixture description text here, while CalculiX also labels INP interfaces; dependency versions beyond the declared Gmsh constraints remain unqualified; issue pending (5). [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L3-L4] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L1-L1] [src/digitalmodel/solvers/gmsh_meshing/models.py:L191-L205] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L211-L219]

## 10. Coverage and open items

All eight package Python files were read in full; the package contains no pre-existing local README/docs subtree at the code pin. Coverage records below retain the source/test ranges rather than historical module counts. [src/digitalmodel/solvers/gmsh_meshing/__init__.py:L1-L33] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L1-L150] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L1-L399] [src/digitalmodel/solvers/gmsh_meshing/cli.py:L1-L183] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L1-L293] [src/digitalmodel/solvers/gmsh_meshing/models.py:L1-L205] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L1-L279] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L1-L269]

The four test files define 37 test functions and collect 41 cases after the five-way invalid-input parameterization; historical figures are therefore not used as executed-test counts. [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L1-L229] [tests/solvers/gmsh_meshing/test_gmsh_cli.py:L1-L94] [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L1-L261] [tests/solvers/gmsh_meshing/test_tank_fixture.py:L157-L186]

| Test file read | Collected cases | BASE / after result | Citation |
| --- | --- | --- | --- |
| `tests/solvers/gmsh_meshing/test_gmsh_unit.py` | 10 | 6 PASS, 4 SKIP; native SKIP/FAIL/ERROR remain NOT_RUN for engineering acceptance | [tests/solvers/gmsh_meshing/test_gmsh_unit.py:L1-L229] |
| `tests/solvers/gmsh_meshing/test_gmsh_cli.py` | 5 | 5 SKIP; native SKIP/FAIL/ERROR remain NOT_RUN for engineering acceptance | [tests/solvers/gmsh_meshing/test_gmsh_cli.py:L1-L94] |
| `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py` | 14 | 2 PASS, 12 SKIP; native SKIP/FAIL/ERROR remain NOT_RUN for engineering acceptance | [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L1-L261] |
| `tests/solvers/gmsh_meshing/test_tank_fixture.py` | 12 | 7 PASS, 2 FAIL, 3 ERROR; native SKIP/FAIL/ERROR remain NOT_RUN for engineering acceptance | [tests/solvers/gmsh_meshing/test_tank_fixture.py:L1-L329] |

Documentation discovery/read ledger: entries below identify the relevant sections read; historical test totals and maturity claims were not adopted as qualification. [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L52] [docs/registry/module-routing.yaml:L354-L359]

| Documentation found | Relevant lines read |
| --- | --- |
| `docs/cad-inventory/automation-opportunity-report.md` | [docs/cad-inventory/automation-opportunity-report.md:L71-L77] |
| `docs/cad-inventory/ecosystem-and-automation-surface.md` | [docs/cad-inventory/ecosystem-and-automation-surface.md:L80-L86] |
| `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` | [docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md:L140-L146] |
| `docs/domains/infrastructure/SKILL_VERSIONING.md` | [docs/domains/infrastructure/SKILL_VERSIONING.md:L228-L234] |
| `docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md` | [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L205-L211] [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L232-L238] [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L249-L255] |
| `docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md` | [docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md:L163-L169] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L265-L271] [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L283-L289] [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L313-L319] |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L41-L47] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L103-L109] [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L186-L192] [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L216-L222] [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L237-L243] [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L261-L267] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L54-L60] [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L298-L304] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | [docs/domains/project-docs/MODULE_SURVEY.md:L50-L56] [docs/domains/project-docs/MODULE_SURVEY.md:L119-L125] [docs/domains/project-docs/MODULE_SURVEY.md:L185-L191] |
| `docs/domains/solvers/gmsh_meshing/README.md` | [docs/domains/solvers/gmsh_meshing/README.md:L1-L5] [docs/domains/solvers/gmsh_meshing/README.md:L10-L16] |
| `docs/plans/2026-06-29-cfd-capability-assessment.md` | [docs/plans/2026-06-29-cfd-capability-assessment.md:L12-L18] [docs/plans/2026-06-29-cfd-capability-assessment.md:L35-L41] [docs/plans/2026-06-29-cfd-capability-assessment.md:L66-L75] [docs/plans/2026-06-29-cfd-capability-assessment.md:L85-L91] |
| `docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md` | [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L16-L25] [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L99-L105] [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L149-L165] [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L288-L294] [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L298-L304] |
| `docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md` | [docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md:L32-L38] [docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md:L313-L319] |
| `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L20-L26] |
| `docs/plans/evidence/issue-2082-ci-discovery.md` | [docs/plans/evidence/issue-2082-ci-discovery.md:L8-L14] |
| `docs/domains/gmsh/gmsh.md` | General normal/logging guidance; not a package-specific format contract [docs/domains/gmsh/gmsh.md:L1-L17] |

| Open item / responsible role | Tracking and required resolution | Evidence |
| --- | --- | --- |
| Quality criteria and statistics / FEA quality manager | issue pending (1): qualify thresholds; correct inversion/scale, score implication, mixed-type and zero-count behavior; add missing negative checks | [src/digitalmodel/solvers/gmsh_meshing/models.py:L65-L108] [src/digitalmodel/solvers/gmsh_meshing/models.py:L159-L162] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L93-L118] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L146-L247] |
| INP and node-tag contract / maintainer | issue pending (2): numbering across blocks, sparse-tag remapping, dropped types and effective export configuration; C12 is insufficient native qualification | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L159-L179] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L224-L253] [src/digitalmodel/solvers/gmsh_meshing/models.py:L191-L205] |
| Strict input/parser contract / maintainer | issue pending (3): reject duplicate sections/nodes/YAML keys, invalid shape/tag arity, schema booleans and nonfinite native quantities; add negative cases | [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L77-L139] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L77-L99] [src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py:L244-L251] [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L207-L213] |
| Map completeness / FEA quality manager | issue pending (4): index all section-3 method families and related definitions from section 9 before counting new families | [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L131-L279] [src/digitalmodel/solvers/gmsh_meshing/_msh2_contract.py:L33-L150] |
| Native tests, G1/G5 records and format units / maintainer | issue pending (5): qualify a compatible native environment and target versions; complete C11–C15 and generated evidence/signature routes; retain raw skips/errors as NOT_RUN | [src/digitalmodel/solvers/gmsh_meshing/_tank_fixture_gmsh.py:L68-L82] [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L181-L255] |
| UNVERIFIED unit/target contract / FEA quality manager | issue pending (5): resolve section-2 generic coordinate units and versions before analysis use | [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L111-L157] |
| UNVERIFIED threshold authority / FEA quality manager | issue pending (1): resolve section-3 best-practice attribution with a published source; no licensed clause text was checked | [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L52] |
| UNVERIFIED deputy/shared-log assignment / maintainer | issue pending (5): confirm section-8 role assignment and durable index/shared-log entry | Proposed review administration, not an engineering code claim |
| Independent benchmark expansion / FEA quality manager | #153: V1 is the sole executed independent-reference example; add mesh convergence and independent quality references, then run native V2/V3 | [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L69-L81] |
| End-to-end CAD/CFD handoff / CFD quality manager | #155: downstream conversion and target load evidence are outside the current offline result | [docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md:L69-L75] |
| Historical documentation reconciliation / maintainer | issue pending (5): requalify portfolio/API claims and audit the remaining sections of the discovery documents; only relevant package passages were used here | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L261-L265] [docs/domains/solvers/gmsh_meshing/README.md:L5-L17] |

The calculation-citation contract was read as a reference for standards-derived outputs; the current methods supply local code criteria rather than an identified licensed governing-standard formula, so no new citation sidecar or private sidecar entry is introduced by this documentation change. [src/digitalmodel/solvers/gmsh_meshing/models.py:L48-L52] [src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py:L131-L168]
