# CalculiX module knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/calculix |
| Repository | digitalmodel |
| Path | src/digitalmodel/solvers/calculix |
| Pack revision | 0, 2026-10-18 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | FE (finite element analysis) |
| Quality role | FEA family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public API exports `INPWriter`, `CalculiXResultParser` and `FEMChain`. [src/digitalmodel/solvers/calculix/__init__.py:L7-L11]
The writer serialises nodes, mapped solid and beam connectivity, elastic materials, sets, boundary conditions, concentrated loads and steps to INP; the chain builds a quarter plate with a hole, invokes `ccx` and extracts stress/displacement summaries. [src/digitalmodel/solvers/calculix/inp_writer.py:L122-L138] [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L167] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L249] [src/digitalmodel/solvers/calculix/fem_chain.py:L50-L120] [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L213]
The routing domain is beta; this is domain metadata, not a demonstrated accuracy rating for every element or section. [docs/registry/module-routing.yaml:L354-L359]
The package has no shell-element mapping or plastic-material writer in its accepted mapping/material routines; nonlinear, shell and buckling work remains tracked in #2173. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L25] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L218] [docs/capability-map/capabilities-added.yml:L77-L77]
Scope includes all four package Python files, the direct test directory and the downstream design-table FEM tests; the separate surface-crack study scripts are dependency evidence rather than an implementation of `FEMChain`. [src/digitalmodel/solvers/calculix/__init__.py:L7-L11] [tests/solvers/calculix/test_surface_crack_spike.py:L12-L21] [tests/visualization/design_tools/test_design_table_fem.py:L11-L12]

## 2. Inputs and outputs

Input contract (L = caller's length unit, F = caller's force unit; these are dimensional requirements for a consistent model, not conversions performed by the writer). [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L167] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L187] [src/digitalmodel/solvers/calculix/inp_writer.py:L241-L249]

| Input | Unit / range | Default | Source and behaviour |
| --- | --- | --- | --- |
| `nodes`, `elements` | x,y,z in L; zero-based connectivity; supported names only | Required; no shape/range validation here | Arrays retained by reference; output IDs shifted by 1. [src/digitalmodel/solvers/calculix/inp_writer.py:L28-L38] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L167] |
| `youngs_modulus`, `poissons_ratio` | F/L², dimensionless; no enforced interval | Required | `add_material` stores values verbatim. [src/digitalmodel/solvers/calculix/inp_writer.py:L47-L55] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L187] |
| Node/element sets | Zero-based integer IDs; no enforced bounds | Empty collections | IDs shifted by 1 on output. [src/digitalmodel/solvers/calculix/inp_writer.py:L39-L45] [src/digitalmodel/solvers/calculix/inp_writer.py:L57-L63] [src/digitalmodel/solvers/calculix/inp_writer.py:L169-L181] |
| Boundary conditions | DOF index; displacement in L or rotation convention supplied by caller | Value 0.0 | `setup_analysis` expands inclusive DOF ranges; writer emits each component unchanged. [src/digitalmodel/solvers/calculix/fem_chain.py:L146-L149] [src/digitalmodel/solvers/calculix/inp_writer.py:L65-L71] [src/digitalmodel/solvers/calculix/inp_writer.py:L232-L239] |
| Concentrated load | F times each direction component; no normalisation or nonzero check | Direction +x when absent or false-valued | Only `cload` generates output; components with absolute value ≤ 1e-15 are omitted. [src/digitalmodel/solvers/calculix/inp_writer.py:L73-L86] [src/digitalmodel/solvers/calculix/inp_writer.py:L241-L249] |
| Beam section | RECT dimensions, CIRC and PIPE tuples; orientation vector | Direction (0,0,1) | No compatibility or dimension validation; docstring radius wording is not a verified solver contract (#1676). [src/digitalmodel/solvers/calculix/inp_writer.py:L88-L112] [src/digitalmodel/solvers/calculix/inp_writer.py:L199-L206] |
| Plate geometry | `plate_w`, `plate_h`, `hole_r`, `thickness`, `element_size`, all in the same L | Required | Quarter box minus cylinder, x and y half-widths, mesh refinement near curves. [src/digitalmodel/solvers/calculix/fem_chain.py:L50-L105] |
| `work_dir`, solver threads | Directory, integer thread count | Temporary directory; 1 thread | Directory is created; `solve` sets OMP thread count and 300 s timeout. [src/digitalmodel/solvers/calculix/fem_chain.py:L34-L38] [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L183] |

The plate's origin is the hole centre on the bottom z face, with quarter symmetry at x=0 and y=0, load at x=half-width and bottom-z constraint; coordinates are emitted in input x,y,z order. [src/digitalmodel/solvers/calculix/fem_chain.py:L78-L85] [src/digitalmodel/solvers/calculix/fem_chain.py:L309-L325] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L151]
The beam solver test explicitly uses m, N and Pa; the general writer and FRD/DAT parser perform no dimensional conversion or unit metadata checks. [tests/solvers/calculix/test_fem_chain.py:L675-L683] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L151] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L187] [src/digitalmodel/solvers/calculix/result_parser.py:L138-L171] [src/digitalmodel/solvers/calculix/result_parser.py:L177-L192]
UNVERIFIED: a general unit/sign/section-dimension contract across all external INP/FRD variants is not established; issue pending (1).
Phase convention is NOT_APPLICABLE to the static example; arbitrary step-type text is uppercased, without a dynamic time-integration contract. [src/digitalmodel/solvers/calculix/inp_writer.py:L114-L116] [src/digitalmodel/solvers/calculix/inp_writer.py:L208-L230] [src/digitalmodel/solvers/calculix/fem_chain.py:L144-L144]

| Output | Unit / interpretation | Source |
| --- | --- | --- |
| INP path | Text file; no embedded unit declaration | `write` emits a trailing newline and returns its path. [src/digitalmodel/solvers/calculix/inp_writer.py:L122-L138] |
| `solve` status | Boolean exit-code success; last 500 characters of stdout/stderr | No convergence, warning or version parsing. [src/digitalmodel/solvers/calculix/fem_chain.py:L173-L189] |
| FRD displacements | `dx`, `dy`, `dz`, magnitude in L; integer node keys | ASCII fixed-width fields. [src/digitalmodel/solvers/calculix/result_parser.py:L30-L43] [src/digitalmodel/solvers/calculix/result_parser.py:L138-L152] |
| FRD stresses | Six tensor components in F/L² | ASCII fixed-width fields; maximum component is absolute. [src/digitalmodel/solvers/calculix/result_parser.py:L77-L83] [src/digitalmodel/solvers/calculix/result_parser.py:L154-L171] |
| DAT reactions | `set`, fx,fy,fz in F | Regex-matched force summaries; an unmatched file yields an empty list. [src/digitalmodel/solvers/calculix/result_parser.py:L45-L53] [src/digitalmodel/solvers/calculix/result_parser.py:L177-L192] |
| Chain summary | Maximum equivalent stress, absolute sxx, near-hole sxx; displacement; field counts | Includes the hardcoded near-hole radius filter discussed in section 6. [src/digitalmodel/solvers/calculix/fem_chain.py:L195-L231] |
| Plate shortcut | `kt = hole_sxx/sigma_applied`, dimensionless; mesh counts | Applied traction converted to equal nodal forces. [src/digitalmodel/solvers/calculix/fem_chain.py:L237-L287] |

## 3. Method and governing standards

| Method | Definition and implementation | Reference status |
| --- | --- | --- |
| INP serialisation | Gmsh-name lookup; node/connectivity +1; beam and solid sets separated; first material used for solids. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L25] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L167] [src/digitalmodel/solvers/calculix/inp_writer.py:L189-L206] | Format implementation only; solver compatibility remains a test obligation (#2080, #1676). |
| Quarter-plate meshing | OpenCASCADE box/cylinder subtraction, 3D mesh and coordinate-based boundary sets. [src/digitalmodel/solvers/calculix/fem_chain.py:L74-L118] [src/digitalmodel/solvers/calculix/fem_chain.py:L309-L325] | No governing design standard is named in these routines. |
| Solver launch | `ccx -i <deck stem>`, inherited environment and explicit thread count, 300 s timeout. [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L189] | Native solver execution was not performed for this pack. |
| Equivalent stress | sqrt(0.5 × ((sxx−syy)²+(syy−szz)²+(szz−sxx)²+6(sxy²+syz²+szx²))). [src/digitalmodel/solvers/calculix/result_parser.py:L195-L207] | Algebra implemented locally; no acceptance allowable or design-code criterion. |
| Displacement norm | sqrt(dx²+dy²+dz²), then maximum over parsed nodes. [src/digitalmodel/solvers/calculix/result_parser.py:L68-L75] [src/digitalmodel/solvers/calculix/result_parser.py:L138-L152] | Synthetic fixture checks are regressions, not solver benchmarks. [tests/solvers/calculix/test_fem_chain.py:L545-L553] |
| Cantilever reference | δ=PL³/(3EI), I=bh³/12; length and inertia scaling checks are separate from the FE code. [tests/solvers/calculix/test_fem_chain.py:L588-L621] | Closed-form reference arithmetic only; native comparison is availability-gated. [tests/solvers/calculix/test_fem_chain.py:L671-L739] |
| Plate stress concentration | Equal nodal load = applied stress × thickness × half-height / load-node count; Kt is filtered peak sxx / applied stress. [src/digitalmodel/solvers/calculix/fem_chain.py:L253-L280] | Not an independent hand-check implementation; geometry/load/filter limitations are in section 6. |

No standards-derived allowable is emitted by the elastic writer or result-summary routines; no `Citation` sidecar is emitted on these paths. [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L249] [src/digitalmodel/solvers/calculix/fem_chain.py:L195-L213] [src/digitalmodel/solvers/calculix/result_parser.py:L195-L207]
The separate program note names API 579-1/ASME FFS-1 (2007, 2016/2021 numbering), API 653 Annex B and BS 7910:2013 as planned assessment context, not current solver acceptance criteria; clause text not checked against the licensed copy. [docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md:L4-L5] [docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md:L31-L35]
UNVERIFIED: supported native CalculiX/Gmsh versions are not pinned by these APIs; issue pending (1).

## 4. Validated worked examples and regression examples

Results below are offline outcomes at the code revision in the header; the reference-only arithmetic case does not validate native finite elements. [tests/solvers/calculix/test_fem_chain.py:L588-L605]

| ID / kind | Source and input | Expected value, tolerance and basis | Existing test node ID | Last offline result |
| --- | --- | --- | --- | --- |
| V1 validated reference arithmetic | Independent closed form δ=PL³/(3EI), I=bh³/12; L=1 m, b=h=0.1 m, E=210 GPa, P=1000 N. [tests/solvers/calculix/test_fem_chain.py:L588-L605] | δ=1.9048e-4 m; relative error <0.1%, rounded hand-calculation tolerance in the test. [tests/solvers/calculix/test_fem_chain.py:L596-L605] | `tests/solvers/calculix/test_fem_chain.py::TestCantileverAnalytical::test_cantilever_deflection_formula` | PASS, BASE and AFTER |
| R1 regression | Synthetic tetrahedron, `simple_mesh`. [tests/solvers/calculix/test_fem_chain.py:L29-L44] | First node ID 1 and 4 node rows; exact integer criteria. [tests/solvers/calculix/test_fem_chain.py:L155-L180] | `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_node_section_1based` | PASS, BASE and AFTER |
| R2 regression | Synthetic `sample_frd_content`, not a solver receipt. [tests/solvers/calculix/test_fem_chain.py:L75-L111] | 3 stress records; exact count. [tests/solvers/calculix/test_fem_chain.py:L523-L530] | `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_parse_frd_stresses` | PASS, BASE and AFTER |
| R3 regression | Synthetic displacement records. [tests/solvers/calculix/test_fem_chain.py:L95-L97] | Maximum displacement 2.06155e-3 input length units; absolute error <1e-6 input length units. [tests/solvers/calculix/test_fem_chain.py:L545-L553] | `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_max_displacement` | PASS, BASE and AFTER |
| R4 regression | Synthetic DAT fixture, not a solver receipt. [tests/solvers/calculix/test_fem_chain.py:L114-L123] | Dictionary contains `reactions`; test does not check its values or count. [tests/solvers/calculix/test_fem_chain.py:L565-L571] | `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_parse_dat_reactions` | PASS, BASE and AFTER |

Unexecuted native candidates are listed separately from validated examples. [tests/solvers/calculix/test_fem_chain.py:L639-L675] [tests/solvers/calculix/test_surface_crack_spike.py:L81-L82]

| Candidate | Expected criterion and source | Existing test node ID | Last offline result |
| --- | --- | --- | --- |
| N1 plate with hole | Kt within 5.0% of 3.0, test's infinite-plate approximation; finite geometry/filter issues remain #1675. [tests/solvers/calculix/test_fem_chain.py:L643-L650] [src/digitalmodel/solvers/calculix/fem_chain.py:L215-L231] | `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_plate_with_hole_kt` | NOT_RUN: requires Gmsh and CalculiX; pytest SKIP at BASE and AFTER |
| N2 full-chain launch | `success` true (exit code 0 only). [tests/solvers/calculix/test_fem_chain.py:L656-L669] [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L189] | `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_solve_returns_success` | NOT_RUN: requires Gmsh and CalculiX; pytest SKIP at BASE and AFTER |
| N3 B31 cantilever | Relative tip-deflection error <1.0% against PL³/(3EI), m/N/Pa fixture. [tests/solvers/calculix/test_fem_chain.py:L675-L739] | `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_cantilever_beam_b31_deflection` | NOT_RUN: requires CalculiX; pytest SKIP at BASE and AFTER |
| N4 separate crack-study readback | Stress/displacement counts equal mesh nodes; force residual within 1e-6 relative; native crack opens. [tests/solvers/calculix/test_surface_crack_spike.py:L81-L111] | `tests/solvers/calculix/test_surface_crack_spike.py::test_native_displacement_and_stress_readback` | NOT_RUN: CalculiX binary absent; pytest SKIP at BASE and AFTER |

## 5. QA gates

Gate adaptation: AQG r0, sections 3–10, provides G0–G5 but no FEA row; the following finite-element mapping is a proposal for review, not a cleared family standard (issue pending (4)).

| Gate | Applicability and load route | Checks / current evidence |
| --- | --- | --- |
| G0 inputs | Applicable: supported mesh counts, connectivity, material and section text. [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L206] | C01–C04 PASS on their supported fixtures; general rejection/units/fingerprint coverage remains open. |
| G1 load | Applicable: write INP via `setup_analysis` or `INPWriter.write`, then native `ccx -i <stem>`; Gmsh is the geometry/mesh producer, not a second analysis-deck target. [src/digitalmodel/solvers/calculix/fem_chain.py:L126-L160] [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L183] [src/digitalmodel/solvers/calculix/fem_chain.py:L74-L108] | C05 PASS for offline serialization only; C06 NOT_RUN (solver absent). Native version, zero warnings and object-count evidence remain required. |
| G2 static response | Applicable: parsed fields, reference arithmetic and eventual native comparisons. [src/digitalmodel/solvers/calculix/result_parser.py:L55-L83] [tests/solvers/calculix/test_fem_chain.py:L588-L621] [tests/solvers/calculix/test_fem_chain.py:L675-L739] | C07–C09 PASS; C10–C11 NOT_RUN (solver absent). No convergence proof follows from the exit-code check. [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L189] |
| G3 short dynamics | NOT_APPLICABLE to the static main path; no transient response or time-step contract in the plate setup. [src/digitalmodel/solvers/calculix/fem_chain.py:L126-L160] [src/digitalmodel/solvers/calculix/inp_writer.py:L208-L230] | 0 dynamic cases; other arbitrary step strings require separate qualification. |
| G4 diffraction | NOT_APPLICABLE: plate solid/beam mesh and stress/displacement output, with no diffraction-body hydrostatics route. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L25] [src/digitalmodel/solvers/calculix/fem_chain.py:L195-L213] | 0 diffraction bodies. |
| G5 evidence appendix | Applicable when an analysis report is produced; proposed C12 requires complete gate/version/fingerprint/result records (AQG r0 §9). | C12 NOT_RUN: summary dictionaries do not implement the appendix record or generator; issue pending (4). [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L213] |

Criteria below retain the existing tests' tolerances; they are not proposed design allowables. [tests/solvers/calculix/test_fem_chain.py:L532-L553] [tests/solvers/calculix/test_fem_chain.py:L596-L621] [tests/solvers/calculix/test_fem_chain.py:L643-L650] [tests/solvers/calculix/test_fem_chain.py:L736-L739]

| Check | Gate | Function/output and executable evidence | Numeric pass criterion / unit | Result |
| --- | --- | --- | --- | --- |
| C01 | G0 | `INPWriter._write_nodes`; R1 | First ID = 1 node ID; 4 node rows, exact. [tests/solvers/calculix/test_fem_chain.py:L155-L180] | PASS |
| C02 | G0 | `INPWriter._write_elements`; `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_element_connectivity_1based` | All emitted IDs ≥1, integer IDs. [tests/solvers/calculix/test_fem_chain.py:L191-L210] | PASS |
| C03 | G0 | `INPWriter._write_materials`; `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_material_section` | 0 missing keywords/value strings for STEEL, 210000.0 and 0.3, count. [tests/solvers/calculix/test_fem_chain.py:L212-L221] | PASS, fixture text only |
| C04 | G0 | `INPWriter._write_beam_sections`; `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_beam_section_output` | 0 missing section/dimension/orientation strings; 0 solid-section keywords, count. [tests/solvers/calculix/test_fem_chain.py:L390-L411] | PASS, no native compatibility assertion |
| C05 | G1 | `FEMChain.setup_analysis`; `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_setup_analysis_writes_inp` | 1 INP file; 0 missing HEADING/NODE/C3D4 keywords, count. [tests/solvers/calculix/test_fem_chain.py:L742-L760] | PASS, does not satisfy native load gate |
| C06 | G1 | `FEMChain.solve`; N2 | Exit code = 0, dimensionless status code. [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L189] [tests/solvers/calculix/test_fem_chain.py:L656-L669] | NOT_RUN: CalculiX/Gmsh absent; no warning/version gate implemented |
| C07 | G2 | `get_max_von_mises`; `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_max_von_mises` | 250 < equivalent stress < 300, fixture stress units. [tests/solvers/calculix/test_fem_chain.py:L532-L543] | PASS, broad fixture bound |
| C08 | G2 | `get_max_displacement`; R3 | Absolute error <1e-6 in input length units. [tests/solvers/calculix/test_fem_chain.py:L545-L553] | PASS |
| C09 | G2 | Independent cantilever formula; V1 and `tests/solvers/calculix/test_fem_chain.py::TestCantileverAnalytical::test_cantilever_deflection_scales_with_length_cubed` | V1 error <0.1%; length-scaling ratio error from 8 <1e-10, dimensionless. [tests/solvers/calculix/test_fem_chain.py:L596-L612] | PASS, reference arithmetic only |
| C10 | G2 | `INPWriter`/parser native beam; N3 | Tip-deflection error <1.0%, m-based reference. [tests/solvers/calculix/test_fem_chain.py:L675-L739] | NOT_RUN: CalculiX absent |
| C11 | G2 | `run_plate_validation`; N1 | Kt relative error <5.0%, dimensionless; unsuitable as an independent reference until #1675 is resolved. [tests/solvers/calculix/test_fem_chain.py:L643-L650] [src/digitalmodel/solvers/calculix/fem_chain.py:L215-L231] | NOT_RUN: CalculiX/Gmsh absent |
| C12 | G5 | Proposed record completeness review: compare report records with AQG r0 §9 and section-4 cases | 0 missing gate statuses, native versions, input digests, case results or dispositions, count | NOT_RUN: no appendix records/generator on summary path; issue pending (4). [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L213] |

## 6. Known failure modes and pitfalls

sweep: 32 found

The sweep reviewed the whole four-file package using the exact commands below; the table counts behavioural sites, including intentional defaults and external-text conversions, rather than matching lines. [src/digitalmodel/solvers/calculix/__init__.py:L7-L11] [src/digitalmodel/solvers/calculix/fem_chain.py:L126-L325] [src/digitalmodel/solvers/calculix/inp_writer.py:L28-L249] [src/digitalmodel/solvers/calculix/result_parser.py:L19-L207]

```sh
rg -n -i 'except|continue|return None|return \[\]|return \{\}|\b0(\.0)?\b|1970|0\.05|int\(|float\(|units?|cache' src/digitalmodel/solvers/calculix --glob '*.py'
rg -n 'return|read_text|write_text|_parsed|_displacements|_stresses|\.get\(' src/digitalmodel/solvers/calculix --glob '*.py'
```

| Site | Symptom / cause at pin | Detection actually available | Tracking |
| --- | --- | --- | --- |
| S01 | Unsupported mesh types disappear at `continue`. [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L158] | no detection check exists for unsupported-type rejection | #2080 |
| S02 | All solids use the first material, without region assignment. [src/digitalmodel/solvers/calculix/inp_writer.py:L189-L197] | no detection check exists for ambiguous materials | #2080 |
| S03 | A load type other than `cload` emits no load or error. [src/digitalmodel/solvers/calculix/inp_writer.py:L241-L249] | no detection check exists | issue pending (1) |
| S04 | Missing/false direction becomes +x, masking omitted input. [src/digitalmodel/solvers/calculix/inp_writer.py:L245-L249] | no detection check exists for omitted/zero direction | issue pending (1) |
| S05 | Load components ≤1e-15 are suppressed without unit-scaled tolerance. [src/digitalmodel/solvers/calculix/inp_writer.py:L246-L249] | no detection check exists for small load components | issue pending (1) |
| S06 | `add_boundary_condition` defaults displacement to 0.0. [src/digitalmodel/solvers/calculix/inp_writer.py:L65-L71] | C05 covers an explicit zero-support fixture, not omitted-value intent | issue pending (1) |
| S07 | `setup_analysis` supplies missing BC value as 0.0. [src/digitalmodel/solvers/calculix/fem_chain.py:L146-L149] | C05; no detection check exists for accidental omission | issue pending (1) |
| S08 | Beam orientation defaults to (0,0,1), without compatibility validation. [src/digitalmodel/solvers/calculix/inp_writer.py:L88-L112] | C04 tests text only; no detection check exists for invalid orientation | #1676 |
| S09 | `int(n)` can truncate fractional external connectivity values. [src/digitalmodel/solvers/calculix/inp_writer.py:L164-L166] | C02 covers integer fixtures; no detection check exists for fractional IDs | issue pending (1) |
| S10 | Missing chain mesh returns near-hole stress 0.0. [src/digitalmodel/solvers/calculix/fem_chain.py:L215-L218] | no detection check exists | issue pending (3) |
| S11 | Empty near-hole selection leaves maximum 0.0. [src/digitalmodel/solvers/calculix/fem_chain.py:L219-L231] | no detection check exists | issue pending (3) |
| S12 | Near-hole selection assumes radius 10.0 and cutoff 30.0 input length units, irrespective of generated hole radius. [src/digitalmodel/solvers/calculix/fem_chain.py:L220-L229] | no detection check exists | issue pending (3) |
| S13 | Stress IDs outside array bounds are silently skipped; IDs are assumed contiguous and ordered. [src/digitalmodel/solvers/calculix/fem_chain.py:L222-L227] | no detection check exists | issue pending (3) |
| S14 | Gmsh node tags are read but ignored; connectivity subtracts 1 instead of using a tag-to-row map; all element dimensions are retained. [src/digitalmodel/solvers/calculix/fem_chain.py:L293-L307] | no detection check exists offline; native N1/N2 are NOT_RUN | #1675; issue pending (3) |
| S15 | Missing LOAD set uses `[1]` as count fallback; an empty LOAD set divides by zero rather than producing a qualified load. [src/digitalmodel/solvers/calculix/fem_chain.py:L253-L256] | no detection check exists | issue pending (3) |
| S16 | Shortcut assumes fixed geometry, material and applied stress; no unit declaration or load-redistribution validation is emitted. [src/digitalmodel/solvers/calculix/fem_chain.py:L237-L269] | C11 NOT_RUN; no detection check exists for a unit mismatch | issue pending (1) |
| S17 | Empty stress data leaves maximum von Mises stress 0.0. [src/digitalmodel/solvers/calculix/result_parser.py:L55-L66] | C07 is a populated fixture; no detection check exists for empty data | issue pending (2) |
| S18 | Empty displacement data leaves maximum displacement 0.0. [src/digitalmodel/solvers/calculix/result_parser.py:L68-L75] | C08 is a populated fixture; no detection check exists for empty data | issue pending (2) |
| S19 | Displacement lines shorter than 37 characters return without an error or record. [src/digitalmodel/solvers/calculix/result_parser.py:L138-L145] | no detection check exists | issue pending (2) |
| S20 | Displacement lines of 37–48 characters supply dz=0.0. [src/digitalmodel/solvers/calculix/result_parser.py:L145-L149] | no detection check exists for a truncated third component | issue pending (2) |
| S21 | Stress lines shorter than 85 characters return without a record. [src/digitalmodel/solvers/calculix/result_parser.py:L154-L161] | no detection check exists | issue pending (2) |
| S22 | Displacement node ID and fields use direct int/float parsing; locale separators and non-finite values have no explicit contract. [src/digitalmodel/solvers/calculix/result_parser.py:L145-L152] | no detection check exists for malformed external numbers | issue pending (2) |
| S23 | Stress node ID and six fields use direct int/float parsing; non-finite fields are not rejected explicitly. [src/digitalmodel/solvers/calculix/result_parser.py:L161-L171] | no detection check exists | issue pending (2) |
| S24 | DAT force regex accepts only digits, decimal point, E/e and signs before direct float conversion; alternate exponent/separator forms are not supported. [src/digitalmodel/solvers/calculix/result_parser.py:L177-L191] | R4 only checks a key; no detection check exists for omitted/malformed reactions | issue pending (2) |
| S25 | No matching DAT force summary returns an empty list with no error. [src/digitalmodel/solvers/calculix/result_parser.py:L177-L192] | no detection check exists | issue pending (2) |
| S26 | Unrecognised FRD headers/lines are advanced over and parsing is still marked complete. [src/digitalmodel/solvers/calculix/result_parser.py:L30-L43] [src/digitalmodel/solvers/calculix/result_parser.py:L97-L107] | no detection check exists for missing result blocks | issue pending (2) |
| S27 | Parsed dictionaries are not cleared on reparse; repeated blocks overwrite matching IDs while preserving unmatched old IDs. [src/digitalmodel/solvers/calculix/result_parser.py:L19-L24] [src/digitalmodel/solvers/calculix/result_parser.py:L30-L43] [src/digitalmodel/solvers/calculix/result_parser.py:L138-L171] | no detection check exists for mixed frames or stale nodes | issue pending (2) |
| S28 | `solve` equates success to exit code 0 and retains only the last 500 characters per stream. [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L189] | C06 NOT_RUN; no detection check exists for earlier warnings or unconverged output | issue pending (4) |
| S29 | Boundary-set coordinate tolerance 1e-6 is absolute in caller length units. [src/digitalmodel/solvers/calculix/fem_chain.py:L309-L324] | no detection check exists for rescaled geometry | issue pending (3) |
| S30 | Missing explicit steps but present BCs/loads silently selects a static step. [src/digitalmodel/solvers/calculix/inp_writer.py:L220-L230] | no detection check exists for accidental step omission | issue pending (1) |
| S31 | Gmsh terminal output is disabled, without retained mesher diagnostics in the returned mesh statistics. [src/digitalmodel/solvers/calculix/fem_chain.py:L71-L72] [src/digitalmodel/solvers/calculix/fem_chain.py:L110-L120] | no detection check exists for suppressed mesher warnings | issue pending (4) |
| S32 | The chain supplies missing load direction as +x before passing the load to the writer. [src/digitalmodel/solvers/calculix/fem_chain.py:L151-L155] | C05 covers the +x fixture; no detection check exists for omitted-direction intent | issue pending (1) |

The Gmsh import handler raises an `ImportError` with its cause and `finally` finalises the mesher; it does not swallow the exception. [src/digitalmodel/solvers/calculix/fem_chain.py:L66-L69] [src/digitalmodel/solvers/calculix/fem_chain.py:L117-L118]
No empty-result cache write, 1970 fallback or `return None` error path occurs in the package; the persistent in-memory parsed-state concern is S27. [src/digitalmodel/solvers/calculix/result_parser.py:L19-L43] [src/digitalmodel/solvers/calculix/result_parser.py:L97-L192] [src/digitalmodel/solvers/calculix/fem_chain.py:L34-L45] [src/digitalmodel/solvers/calculix/inp_writer.py:L122-L138]
Loop counters initialised to zero, empty construction buffers and zero origin coordinates are not additional error fallbacks; the default static step is intentional and automatically repeats the same loads/BCs in each requested step. [src/digitalmodel/solvers/calculix/result_parser.py:L97-L107] [src/digitalmodel/solvers/calculix/inp_writer.py:L39-L45] [src/digitalmodel/solvers/calculix/inp_writer.py:L208-L230] [src/digitalmodel/solvers/calculix/fem_chain.py:L78-L85]

| Incident / capability limit | Evidence and consequence | Detection / tracking |
| --- | --- | --- |
| Plate solve failures | #1675 describes the native plate tests; #462 describes a historical dimension filter, but the pinned extraction routine has no dimension filter. [src/digitalmodel/solvers/calculix/fem_chain.py:L293-L307] | C06/C11 NOT_RUN; #1675 remains the current follow-up. |
| Beam compatibility/connectivity | Mapping includes B31/B32 only; raw connectivity is shifted without reordering and CIRC is documented as radius; #1676 records these concerns. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L25] [src/digitalmodel/solvers/calculix/inp_writer.py:L96-L104] [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L167] | no detection check exists for invalid combinations or required reordering; #1676. |
| Native circular/quadratic coverage | Serialization tests cover B32/CIRC/PIPE strings; direct native cantilever test covers B31 only. [tests/solvers/calculix/test_fem_chain.py:L370-L449] [tests/solvers/calculix/test_fem_chain.py:L671-L739] | no detection check exists for native B32/CIRC/PIPE in this directory; #1673, #1674. |
| Nonlinear/crack extensions | Nonlinear solver extension is planned; separate crack-study tests load study scripts instead of extending the package writer. [docs/capability-map/capabilities-added.yml:L77-L77] [tests/solvers/calculix/test_surface_crack_spike.py:L12-L21] | no detection check exists for production nonlinear acceptance; #2173, #2178. |

## 7. Lessons log

| Date | Record | Lesson / evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Separate arithmetic and string-fixture passes from native FE evidence; native tests are gated on installed tools. [tests/solvers/calculix/test_fem_chain.py:L588-L605] [tests/solvers/calculix/test_fem_chain.py:L639-L675] | Sections 4–6 retain explicit NOT_RUN and missing-detection coverage. | Not assigned; issue pending (4) |

## 8. Ownership

Proposed responsibility: FEA family quality manager, deputy FEA family quality manager, and solver-interface maintainers; unresolved tolerances, input contracts or competing-implementation classifications go to the family quality manager before the maintainer merges.
UNVERIFIED: a named deputy and cleared FEA-specific gate revision are not evidenced in the inspected repository routing entry; issue pending (4). [docs/registry/module-routing.yaml:L354-L359]

## 9. Dependencies and implementation inventory

### Implementation map

Map source: digitalmodel maintainers, Implementation map, revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, sections `families`, `rejected_candidates` and root `revision`; read as text only.
Map snapshot: ref `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data/code revision `270c8759c374b67a7f032953e7fcdb14e0224461`; this is a separate snapshot from the pack code pin.
Inventory of qualifying implementation rows for families with at least one row inside this package: **empty**; counted families: **0**; counted-family grep: **NOT_APPLICABLE**, because no family has an implementation row in the package.
The snapshot places `FEMChain.solve` in `rejected_candidates` with disposition `needs_review`, outside implementation counts; this candidate still launches the solver in the code pin. [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L189]
The map audit enumerated every family's `implementations` list and tested the prefix `src/digitalmodel/solvers/calculix/`; a rejected-candidate occurrence does not create an inventory family.
Map follow-up: add or adjudicate the solver-launch family candidate, INP serialisation, FRD/DAT result parsing, equivalent-stress evaluation and plate stress-concentration orchestration from section 3; issue pending (4). [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L231] [src/digitalmodel/solvers/calculix/inp_writer.py:L122-L249] [src/digitalmodel/solvers/calculix/result_parser.py:L30-L207]

### Repository-wide further search

The additional search below covers all tracked Python files, including `scripts/`, at the pack pin; findings outside the package are map follow-ups and do not change the counted-family result.

```sh
git grep -n -E 'def .*von_mises|class INPWriter|def export_mesh_inp|def (write_deck|read_fields)|\[.*ccx.*-i' c099a5e1317e6bed63d876252d87cbb12c774372 -- '*.py'
```

| Finding outside package | Classification and evidence |
| --- | --- |
| `GMSHMeshGenerator.export_mesh_inp` | Independent mesh-to-INP writer with its own mapping and loops; map gap. [src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py:L205-L255] |
| `surface_crack_mesh.write_deck` | Independent study deck writer including distributed loads/reactions; map gap. [scripts/studies/surface_crack_mesh.py:L64-L90] |
| `surface_crack_post.solve` | Separate native launcher, resource limits and receipt; map gap. [scripts/studies/surface_crack_post.py:L91-L117] |
| `surface_crack_post.read_fields` | Delegates to the listed package parser; helper, not another parser implementation. [scripts/studies/surface_crack_post.py:L120-L122] |
| `compute_von_mises` | Independent three-dimensional equivalent-stress arithmetic; map gap. [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L13-L51] |
| `panel_buckling._von_mises` | Independent plane-stress equivalent-stress arithmetic; map gap. [src/digitalmodel/structural/structural_analysis/panel_buckling.py:L155-L161] |

Other equivalent-stress-named definitions found by this additional grep remain classification follow-ups, rather than an exhaustive claim of independent implementations; issue pending (4).

### Upstream and downstream routes

| Dependency | Contract / version evidence |
| --- | --- |
| NumPy | Mesh arrays/reshape; no mesher import needed for writing a supplied mesh. [src/digitalmodel/solvers/calculix/fem_chain.py:L13-L16] [src/digitalmodel/solvers/calculix/fem_chain.py:L293-L307] [src/digitalmodel/solvers/calculix/inp_writer.py:L36-L38] |
| Gmsh | Native geometry and meshing API; imported lazily, initialised and finalised by the plate route. [src/digitalmodel/solvers/calculix/fem_chain.py:L66-L118] |
| CalculiX | External `ccx` executable called with deck stem; binary version is not recorded by `solve`. [src/digitalmodel/solvers/calculix/fem_chain.py:L166-L189] |
| Python standard library | Paths/text IO, regex and math in parser; subprocess and temporary directory in chain. [src/digitalmodel/solvers/calculix/result_parser.py:L30-L53] [src/digitalmodel/solvers/calculix/result_parser.py:L177-L207] [src/digitalmodel/solvers/calculix/fem_chain.py:L34-L38] [src/digitalmodel/solvers/calculix/fem_chain.py:L173-L183] |
| Design-table consumer | Calls the chain for FEM parameter studies; separate component from this package. [src/digitalmodel/visualization/design_tools/design_table.py:L302-L312] |
| Crack-study consumer | Loads the package parser for field readback; committed synthetic tests also exercise the separate study arithmetic. [scripts/studies/surface_crack_post.py:L120-L122] [tests/solvers/calculix/test_surface_crack_spike.py:L24-L79] |

The complete occurrence inventory below includes every line matching case-insensitive `CalculiX|gmsh|ccx` in the four package Python files at the code pin; comments, imports and symbols are retained as text dependency references, without importing or querying native tools.

| File / source occurrence | Dependency text matched |
| --- | --- |
| `__init__.py` line 3 | calculix. [src/digitalmodel/solvers/calculix/__init__.py:L3-L3] |
| `__init__.py` line 4 | calculix, ccx. [src/digitalmodel/solvers/calculix/__init__.py:L4-L4] |
| `__init__.py` line 8 | calculix. [src/digitalmodel/solvers/calculix/__init__.py:L8-L8] |
| `__init__.py` line 11 | calculix. [src/digitalmodel/solvers/calculix/__init__.py:L11-L11] |
| `fem_chain.py` line 3 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L3-L3] |
| `fem_chain.py` line 4 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L4-L4] |
| `fem_chain.py` line 16 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L16-L16] |
| `fem_chain.py` line 19 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L19-L19] |
| `fem_chain.py` line 20 | calculix, ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L20-L20] |
| `fem_chain.py` line 21 | ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L21-L21] |
| `fem_chain.py` line 26 | ccx, gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L26-L26] |
| `fem_chain.py` line 36 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L36-L36] |
| `fem_chain.py` line 59 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L59-L59] |
| `fem_chain.py` line 67 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L67-L67] |
| `fem_chain.py` line 69 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L69-L69] |
| `fem_chain.py` line 71 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L71-L71] |
| `fem_chain.py` line 72 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L72-L72] |
| `fem_chain.py` line 75 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L75-L75] |
| `fem_chain.py` line 76 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L76-L76] |
| `fem_chain.py` line 82 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L82-L82] |
| `fem_chain.py` line 83 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L83-L83] |
| `fem_chain.py` line 86 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L86-L86] |
| `fem_chain.py` line 87 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L87-L87] |
| `fem_chain.py` line 89 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L89-L89] |
| `fem_chain.py` line 91 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L91-L91] |
| `fem_chain.py` line 92 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L92-L92] |
| `fem_chain.py` line 94 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L94-L94] |
| `fem_chain.py` line 96 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L96-L96] |
| `fem_chain.py` line 97 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L97-L97] |
| `fem_chain.py` line 98 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L98-L98] |
| `fem_chain.py` line 99 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L99-L99] |
| `fem_chain.py` line 100 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L100-L100] |
| `fem_chain.py` line 101 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L101-L101] |
| `fem_chain.py` line 102 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L102-L102] |
| `fem_chain.py` line 104 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L104-L104] |
| `fem_chain.py` line 105 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L105-L105] |
| `fem_chain.py` line 107 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L107-L107] |
| `fem_chain.py` line 108 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L108-L108] |
| `fem_chain.py` line 118 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L118-L118] |
| `fem_chain.py` line 167 | ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L167-L167] |
| `fem_chain.py` line 168 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L168-L168] |
| `fem_chain.py` line 169 | calculix, ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L169-L169] |
| `fem_chain.py` line 177 | ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L177-L177] |
| `fem_chain.py` line 197 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L197-L197] |
| `fem_chain.py` line 215 | calculix. [src/digitalmodel/solvers/calculix/fem_chain.py:L215-L215] |
| `fem_chain.py` line 275 | ccx. [src/digitalmodel/solvers/calculix/fem_chain.py:L275-L275] |
| `fem_chain.py` line 293 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L293-L293] |
| `fem_chain.py` line 294 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L294-L294] |
| `fem_chain.py` line 295 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L295-L295] |
| `fem_chain.py` line 299 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L299-L299] |
| `fem_chain.py` line 300 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L300-L300] |
| `fem_chain.py` line 301 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L301-L301] |
| `fem_chain.py` line 309 | gmsh. [src/digitalmodel/solvers/calculix/fem_chain.py:L309-L309] |
| `inp_writer.py` line 3 | calculix. [src/digitalmodel/solvers/calculix/inp_writer.py:L3-L3] |
| `inp_writer.py` line 4 | calculix, ccx. [src/digitalmodel/solvers/calculix/inp_writer.py:L4-L4] |
| `inp_writer.py` line 12 | calculix, gmsh. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L12] |
| `inp_writer.py` line 24 | calculix. [src/digitalmodel/solvers/calculix/inp_writer.py:L24-L24] |
| `inp_writer.py` line 30 | calculix. [src/digitalmodel/solvers/calculix/inp_writer.py:L30-L30] |
| `inp_writer.py` line 32 | gmsh. [src/digitalmodel/solvers/calculix/inp_writer.py:L32-L32] |
| `inp_writer.py` line 33 | calculix. [src/digitalmodel/solvers/calculix/inp_writer.py:L33-L33] |
| `inp_writer.py` line 146 | calculix. [src/digitalmodel/solvers/calculix/inp_writer.py:L146-L146] |
| `inp_writer.py` line 155 | gmsh. [src/digitalmodel/solvers/calculix/inp_writer.py:L155-L155] |
| `inp_writer.py` line 156 | ccx, gmsh. [src/digitalmodel/solvers/calculix/inp_writer.py:L156-L156] |
| `inp_writer.py` line 157 | ccx. [src/digitalmodel/solvers/calculix/inp_writer.py:L157-L157] |
| `inp_writer.py` line 159 | ccx. [src/digitalmodel/solvers/calculix/inp_writer.py:L159-L159] |
| `inp_writer.py` line 160 | ccx. [src/digitalmodel/solvers/calculix/inp_writer.py:L160-L160] |
| `inp_writer.py` line 163 | ccx. [src/digitalmodel/solvers/calculix/inp_writer.py:L163-L163] |
| `result_parser.py` line 3 | calculix. [src/digitalmodel/solvers/calculix/result_parser.py:L3-L3] |
| `result_parser.py` line 12 | calculix. [src/digitalmodel/solvers/calculix/result_parser.py:L12-L12] |
| `result_parser.py` line 14 | calculix. [src/digitalmodel/solvers/calculix/result_parser.py:L14-L14] |

## 10. Coverage and open items

The reviewed package source inventory is `__init__.py`, `fem_chain.py`, `inp_writer.py` and `result_parser.py`; the exports, writer, parser and chain are covered in sections 1–6. [src/digitalmodel/solvers/calculix/__init__.py:L7-L11] [src/digitalmodel/solvers/calculix/fem_chain.py:L24-L325] [src/digitalmodel/solvers/calculix/inp_writer.py:L28-L249] [src/digitalmodel/solvers/calculix/result_parser.py:L12-L207]
Direct tests are in `tests/solvers/calculix/test_fem_chain.py` and `tests/solvers/calculix/test_surface_crack_spike.py`; downstream FEM use is in `tests/visualization/design_tools/test_design_table_fem.py`. [tests/solvers/calculix/test_fem_chain.py:L3-L4] [tests/solvers/calculix/test_surface_crack_spike.py:L12-L21] [tests/visualization/design_tools/test_design_table_fem.py:L11-L23]
Adjacent `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py` tests a separate exporter and remains outside this package's test count. [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L3-L4] [tests/solvers/gmsh_meshing/test_gmsh_step_inp.py:L32-L48]

Documentation discovery used `rg -l -i calculix docs` and found the following context files; module-specific passages were compared with source, and no roadmap or historical native-result number is promoted to current validated evidence.

- `docs/archive/archive-blob-map-2026-10-08.csv`. [docs/archive/archive-blob-map-2026-10-08.csv:L1262-L1262]
- `docs/archive/archive-candidates-2026-10-08.csv`. [docs/archive/archive-candidates-2026-10-08.csv:L1262-L1262]
- `docs/capability-map/capabilities-added.yml`. [docs/capability-map/capabilities-added.yml:L77-L77]
- `docs/domains/asset-integrity/ffs-offering-catalog.md`. [docs/domains/asset-integrity/ffs-offering-catalog.md:L62-L62]
- `docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md`. [docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md:L4-L4]
- `docs/domains/asset-integrity/surface-crack-calculix-results.json`. [docs/domains/asset-integrity/surface-crack-calculix-results.json:L1-L1]
- `docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html`. [docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html:L2-L2]
- `docs/domains/fem/nastran/nastran.md`. [docs/domains/fem/nastran/nastran.md:L44-L44]
- `docs/plans/2026-05-05-issue-277-freecad-deep-parametric-engineering.md`. [docs/plans/2026-05-05-issue-277-freecad-deep-parametric-engineering.md:L9-L9]
- `docs/plans/2026-06-29-cfd-capability-assessment.md`. [docs/plans/2026-06-29-cfd-capability-assessment.md:L74-L74]
- `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`. [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L22-L22]
- `docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md`. [docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md:L137-L137]
- One discovery locator is omitted because its filename contains restricted role wording; documentation locator cleanup: issue pending (4).
- `docs/plans/evidence/issue-2082-ci-discovery.md`. [docs/plans/evidence/issue-2082-ci-discovery.md:L10-L10]
- `docs/registry/implementation-map.yaml`. [docs/registry/implementation-map.yaml:L42777-L42777]
- `docs/session-handoffs/2026-10-06-issue-2287-ffs-study-preparation.md`. [docs/session-handoffs/2026-10-06-issue-2287-ffs-study-preparation.md:L61-L61]

The separate spike page reports a one-solver research result; it is historical context, not a run of this pack or a native validation of `FEMChain`. [docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html:L9-L16]
The calculation-output citation contract was readable; this pack adds no standards-derived production constant, and no new numeric sidecar is claimed. [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L249] [src/digitalmodel/solvers/calculix/result_parser.py:L195-L207]

### Offline run record

Generic form for all runs: `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, worker option, solver invocation or test deselection was used.
Both BASE and AFTER refer to the header code revision; AFTER executes on the uncommitted documentation tree, because the pack and routing metadata are the only changed files.

| Paths | BASE collected / passed / failed / skipped | AFTER collected / passed / failed / skipped | NOT_RUN interpretation |
| --- | --- | --- | --- |
| `tests/solvers/calculix` | 51 / 47 / 0 / 4 | 51 / 47 / 0 / 4 | 4 native tests, N1–N4, require the absent solver; N1–N2 also require Gmsh. |
| `tests/visualization/design_tools/test_design_table_fem.py tests/contracts tests/docs/test_digitalmodel_routing_contract.py` | 503 / 500 / 0 / 3 | 503 / 500 / 0 / 3 | 3 downstream native FEM tests require CalculiX/Gmsh. |

The direct directory count is 51 collected cases rather than the inventory lead of about 53; the inventory figure is not treated as evidence of current test coverage.

### Direct and downstream test-node inventory

PASS and NOT_RUN entries below record both BASE and AFTER; NOT_RUN corresponds to the repository's unchanged tool-availability skip guards. [tests/solvers/calculix/test_fem_chain.py:L639-L675] [tests/solvers/calculix/test_surface_crack_spike.py:L81-L82] [tests/visualization/design_tools/test_design_table_fem.py:L20-L23]

| Existing test node ID | Offline outcome |
| --- | --- |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_init_stores_mesh` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_write_creates_file` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_heading_section` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_node_section_1based` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_element_section_c3d4` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_element_connectivity_1based` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_material_section` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_solid_section` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_node_set` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_element_set` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_boundary_condition` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_concentrated_load` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_output_requests` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_multi_element_mesh` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_hex_element_type` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_tet10_element_type` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_b31_element_type` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_b32_element_type` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_beam_section_output` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_beam_section_output_for_circular_sections[CIRC-dimensions0-0.05]` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_beam_section_output_for_circular_sections[PIPE-dimensions1-0.08, 0.01]` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_mixed_solid_and_beam` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestINPWriter::test_beam_element_connectivity_1based` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_parse_frd_displacements` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_parse_frd_stresses` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_max_von_mises` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_max_displacement` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_stress_at_node` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_parse_dat_reactions` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_missing_frd_raises` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestResultParser::test_missing_dat_raises` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestCantileverAnalytical::test_cantilever_deflection_formula` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestCantileverAnalytical::test_cantilever_deflection_scales_with_length_cubed` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestCantileverAnalytical::test_cantilever_deflection_scales_with_inertia` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_is_calculix_available_returns_bool` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_init_creates_work_dir` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_init_with_custom_dir` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_setup_analysis_writes_inp` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_solver_version_label_in_committed_results_and_generated_page` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_recovers_known_k_from_textbook_field` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_reference_wrapper_matches_package` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_rejects_invalid_sampling[radii0-stresses0]` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_rejects_invalid_sampling[radii1-stresses1]` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_rejects_invalid_sampling[radii2-stresses2]` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_rejects_invalid_sampling[radii3-stresses3]` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_removes_linear_radial_correction` | PASS |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_mesh_preserves_rectangular_section_and_crack_geometry` | PASS |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_fem_parameter_validation` | PASS |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_unknown_fem_parameter_raises` | PASS |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_negative_fem_values_rejected` | PASS |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_plate_with_hole_kt` | NOT_RUN: CalculiX/Gmsh required |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_solve_returns_success` | NOT_RUN: CalculiX/Gmsh required |
| `tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_cantilever_beam_b31_deflection` | NOT_RUN: CalculiX required |
| `tests/solvers/calculix/test_surface_crack_spike.py::test_native_displacement_and_stress_readback` | NOT_RUN: CalculiX binary absent |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_batch_fem_runs` | NOT_RUN: CalculiX/Gmsh required |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_kt_monotonic_with_hole_ratio` | NOT_RUN: CalculiX/Gmsh required |
| `tests/visualization/design_tools/test_design_table_fem.py::TestDesignTableFEMMode::test_export_results_yaml` | NOT_RUN: CalculiX/Gmsh required |

### Open-item register

The following work is assigned to solver-interface maintainers; quality-role and gate decisions belong to the FEA family quality manager.

| Item | Evidence / limit | Tracking |
| --- | --- | --- |
| General dimensional/section/sign contract | UNVERIFIED statement in section 2; no writer-side unit validation. [src/digitalmodel/solvers/calculix/inp_writer.py:L88-L112] [src/digitalmodel/solvers/calculix/inp_writer.py:L148-L151] [src/digitalmodel/solvers/calculix/inp_writer.py:L183-L187] | issue pending (1) |
| Native version compatibility | UNVERIFIED statement in section 3; version absent from solver return data. [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L189] | issue pending (1) |
| Result completeness/numeric syntax | S17–S27 lack malformed/empty/repeated-frame detection. [src/digitalmodel/solvers/calculix/result_parser.py:L30-L43] [src/digitalmodel/solvers/calculix/result_parser.py:L55-L75] [src/digitalmodel/solvers/calculix/result_parser.py:L138-L192] | issue pending (2) |
| Mesh identity/hole region | S10–S15/S29 need sparse tags, geometry-dependent filter and scaling checks. [src/digitalmodel/solvers/calculix/fem_chain.py:L215-L231] [src/digitalmodel/solvers/calculix/fem_chain.py:L253-L256] [src/digitalmodel/solvers/calculix/fem_chain.py:L293-L325] | issue pending (3); native failures #1675 |
| Input fidelity | Unsupported mesh loss and implicit first solid material. [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L167] [src/digitalmodel/solvers/calculix/inp_writer.py:L189-L197] | #2080 |
| Beam source/section qualification | Missing compatibility/reordering; native quadratic/circular/pipe reference coverage. [src/digitalmodel/solvers/calculix/inp_writer.py:L12-L25] [src/digitalmodel/solvers/calculix/inp_writer.py:L96-L104] [src/digitalmodel/solvers/calculix/inp_writer.py:L153-L167] [tests/solvers/calculix/test_fem_chain.py:L671-L739] | #1676, #1673, #1674 |
| Production nonlinear / crack acceptance | Planned extension; separate study is not production acceptance. [docs/capability-map/capabilities-added.yml:L77-L77] [tests/solvers/calculix/test_surface_crack_spike.py:L12-L21] | #2173, #2178 |
| Gate adaptation, deputy and evidence records | UNVERIFIED statement in section 8; G1/G2 native evidence and G5 records remain absent in this pack. [docs/registry/module-routing.yaml:L354-L359] [src/digitalmodel/solvers/calculix/fem_chain.py:L185-L213] | issue pending (4) |
| Map and broader reference review | Empty counted inventory; add/adjudicate section-3 methods, separate writers and remaining equivalent-stress candidates before extending counted-family coverage. [src/digitalmodel/solvers/calculix/inp_writer.py:L122-L249] [src/digitalmodel/solvers/calculix/result_parser.py:L195-L207] | issue pending (4) |
| Independent FE worked examples | V1 validates reference arithmetic only; N1–N4 and three downstream tests have no current native result. [tests/solvers/calculix/test_fem_chain.py:L588-L605] [tests/solvers/calculix/test_fem_chain.py:L639-L739] [tests/solvers/calculix/test_surface_crack_spike.py:L81-L111] [tests/visualization/design_tools/test_design_table_fem.py:L47-L88] | #1675, #1673, #1674, #2178; issue pending (4) |

No client-derived values or private sidecar entries are introduced; all numeric examples above use public synthetic fixtures or the package's own default values. [tests/solvers/calculix/test_fem_chain.py:L29-L63] [tests/solvers/calculix/test_fem_chain.py:L75-L123] [tests/solvers/calculix/test_fem_chain.py:L596-L605] [src/digitalmodel/solvers/calculix/fem_chain.py:L237-L269]
