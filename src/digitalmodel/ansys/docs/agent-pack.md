# ANSYS module knowledge pack

| Header | Value |
| --- | --- |
| Module | `ansys` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/ansys/docs/agent-pack.md` |
| Pack revision | 0 |
| Code revision | `4a96969590e625fbd00392851d0a4dd29b24c4f9` |
| Analysis family | FE |
| Quality manager role | FEA quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The main path produces APDL text, reads material/section and journal metadata, parses text results, and computes thickness, weld-linearization and fatigue quantities; the routing registry classifies the module as beta. [src/digitalmodel/ansys/apdl_generator.py:L317-L339] [src/digitalmodel/ansys/apdl_reader.py:L42-L76] [src/digitalmodel/ansys/wbjn_reader.py:L49-L59] [src/digitalmodel/ansys/results_extractor.py:L169-L214] [src/digitalmodel/ansys/weld_assessment.py:L312-L361] [src/digitalmodel/ansys/fatigue_postprocessor.py:L222-L284] [docs/registry/module-routing.yaml:L5-L10]

`AnsysWorkflow.router` supports only `run_ansys`; a failed result or an unrequested dry-run fallback raises rather than reporting a completed solve. [src/digitalmodel/ansys/workflow.py:L24-L77]

The three examples are screening idealizations: an axisymmetric cylinder wall without nozzle/head discontinuities, a plane-stress plate without pin/contact or weld-throat qualification, and a shell plate with a prescribed effective-area soil reaction. [src/digitalmodel/ansys/pressure_vessel.py:L807-L822] [src/digitalmodel/ansys/padeye.py:L15-L17] [src/digitalmodel/ansys/mudmat.py:L106-L143]

The diffraction-to-structural workflow is a documented stage contract whose end-to-end validation is explicitly not established; it is not a capability demonstrated by these direct-load examples. [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L3-L11] [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L57-L67]

Capability-claim audit of the main path (claim versus executable implementation):

| Claim requiring correction or qualification | Implemented boundary and disposition |
| --- | --- |
| Pressure-vessel internal/external pressure capability | Internal loading has a generator; no external-pressure generator or buckling check occurs in the class body. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L11-L18] [src/digitalmodel/ansys/pressure_vessel.py:L629-L996] |
| WRC 537 nozzle reinforcement loads | `generate_nozzle_loads` emits forces and moments on named pilot components; no WRC calculation is implemented there. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L15-L15] [src/digitalmodel/ansys/pressure_vessel.py:L681-L702] |
| Thermal gradient in the pressure-vessel helper | `generate_thermal_loading` assigns one uniform temperature, including `BF,ALL,TEMP`; it does not build a spatial gradient. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L660-L679] |
| Head geometry and reinforcement-pad modeling | The ellipsoidal branch writes parameters and comments only; the pad branch writes a comment only. No torispherical branch appears in `generate_vessel_geometry`. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L562-L627] |
| Wall slice represents longitudinal membrane stress | The complete deck applies inner-wall pressure without an end-cap load; the independent comparator explicitly uses zero axial stress. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L809-L816] [src/digitalmodel/ansys/pressure_vessel.py:L944-L948] [tests/ansys/test_example_goldens.py:L81-L93] |
| Thickness result includes `pressure_rating` | `ug27_cylindrical_shell` documents that key but returns only required thickness, thickness with allowance, and a formula string. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L134-L151] |
| Mudmat Winkler stiffness and soil-bearing output | The opening docstring describes an elastic foundation and bearing utilization; the generator removes that foundation, prescribes an effective-area pressure patch, and delegates bearing capacity. Issue pending (1). [src/digitalmodel/ansys/mudmat.py:L3-L31] [src/digitalmodel/ansys/mudmat.py:L106-L143] [src/digitalmodel/ansys/mudmat.py:L252-L268] |
| Flange setup includes ring-type/full-face behavior | `generate_flange_model` emits an annular body, optional raised face and weld-neck hub, without a ring-groove branch. Issue pending (1). [src/digitalmodel/ansys/bolt_analysis.py:L8-L14] [src/digitalmodel/ansys/bolt_analysis.py:L252-L297] |
| Complete generic APDL and thermal scripts | Assembly routines append blocks without generating geometry; the thermal coupling block leaves structural properties and supports as comments. Issue pending (1). [src/digitalmodel/ansys/apdl_generator.py:L317-L339] [src/digitalmodel/ansys/thermal_analysis.py:L400-L473] |
| Every pressure-vessel function returns APDL text | Thickness methods return dictionaries and MAWP returns a numeric value. Issue pending (1). [src/digitalmodel/ansys/pressure_vessel.py:L28-L28] [src/digitalmodel/ansys/pressure_vessel.py:L143-L151] [src/digitalmodel/ansys/pressure_vessel.py:L413-L422] |
| Every extractor method accepts text, rather than paths | Text parsing is implemented, but `extract_max_values` consumes a DataFrame and comparison/export routines consume structured objects. Issue pending (1). [src/digitalmodel/ansys/results_extractor.py:L162-L167] [src/digitalmodel/ansys/results_extractor.py:L339-L364] [src/digitalmodel/ansys/results_extractor.py:L406-L432] [src/digitalmodel/ansys/results_extractor.py:L470-L508] |
| Fatigue rainflow, extra curve classes/design code, contours and below-threshold infinite life | These historical claims are removed at the pin; tests check the caller-input boundary, documented classes, absent claims, finite positive-range extrapolation and scalar-difference limitation. Incidents #2098 and #2410. [src/digitalmodel/ansys/fatigue_postprocessor.py:L8-L26] [src/digitalmodel/ansys/fatigue_postprocessor.py:L158-L192] [tests/ansys/test_fatigue_postprocessor.py:L243-L280] [tests/ansys/test_fatigue_postprocessor.py:L321-L355] |

UNVERIFIED: completeness of the capability audit beyond the main path is open; the cylinder utilities are indexed rather than fully audited, and extended evidence transitions need a separate review (section 10, issue pending (3)).

## 2. Inputs and outputs

Inputs:

| Input | Unit | Allowed range / default | Source and citation |
| --- | --- | --- | --- |
| `ansys.script`, output directory, timeout, `dry_run` | path, s, Boolean | Script required; timeout 7200 s; dry-run false; relative script resolved against configuration directory | `AnsysWorkflow._run_ansys`, path resolvers. [src/digitalmodel/ansys/workflow.py:L44-L54] [src/digitalmodel/ansys/workflow.py:L79-L102] |
| `VesselGeometry`, `DesignConditions` | mm, MPa, degrees C, dimensionless efficiency | Example uses inner diameter 1500 mm and wall 60 mm; pressure 10 MPa; no general validation routine on these dataclasses | Public example geometry and pressure; allowables are caller inputs and are not reproduced here. [examples/ansys/pressure-vessel/build.py:L22-L38] [src/digitalmodel/ansys/pressure_vessel.py:L42-L79] |
| `PadeyeGeometry` | mm, kN, degrees, MPa | Positive dimensions, hole strictly inside the plate, positive strength and factor; example load 50 kN and mesh 10 mm | `validate` does not validate every field. [src/digitalmodel/ansys/padeye.py:L56-L70] [examples/ansys/padeye/build.py:L26-L36] |
| `MudmatGeometry` | mm, kN, kN m, MPa | Positive dimensions/load/mesh/stiffness/material parameters, nonnegative moment, eccentricity below half length; example load 800 kN and moment 400 kN m | `validate`; stiffness remains an input although the deck uses a pressure patch. [src/digitalmodel/ansys/mudmat.py:L48-L91] [src/digitalmodel/ansys/mudmat.py:L106-L126] |
| Generic materials, elements, mesh, BCs and solution | kg/m³ input density; mm/N/MPa or declared SI | Caller-supplied dataclasses; default analysis STATIC; unknown analysis names fall back to static | Unit conversion needs review before SI use (section 10). [src/digitalmodel/ansys/apdl_generator.py:L34-L115] [src/digitalmodel/ansys/apdl_generator.py:L151-L168] [src/digitalmodel/ansys/apdl_generator.py:L251-L278] |
| Batch sweeps / load cases | Caller-declared units | Cartesian product or shortest-length zip; placeholders replaced as strings | `BatchRunner.generate_run_matrix`, `generate_runs`; load-case parsing is a limited line parser rather than a general YAML parser. [src/digitalmodel/ansys/batch_runner.py:L149-L218] [src/digitalmodel/ansys/batch_runner.py:L295-L342] |
| Fatigue load cases / S-N curve | MPa, cycles; dimensionless factors | Stress ranges and cycle counts supplied by caller; positive stress ranges extrapolate finitely; annual load-case assumption for life in years | `calculate_fatigue_damage` and `calculate_cycles_to_failure`. [src/digitalmodel/ansys/fatigue_postprocessor.py:L78-L85] [src/digitalmodel/ansys/fatigue_postprocessor.py:L158-L192] [src/digitalmodel/ansys/fatigue_postprocessor.py:L244-L284] |

Outputs:

| Output | Unit / encoding | Boundary and source |
| --- | --- | --- |
| APDL deck strings / `.inp` | Plain command text; example writers use UTF-8 | Generator returns text; writers create parent directories. [src/digitalmodel/ansys/apdl_generator.py:L317-L339] [src/digitalmodel/ansys/padeye.py:L221-L226] [src/digitalmodel/ansys/mudmat.py:L274-L279] |
| `ANSYSRunResult` | Status, paths, return code, stdout/stderr, duration s | Fresh result suffixes include CSV; binary result files are collected as paths, not parsed as binary stresses. [src/digitalmodel/ansys/runner.py:L31-L65] [src/digitalmodel/ansys/runner.py:L193-L213] [src/digitalmodel/ansys/runner.py:L251-L275] |
| Result summaries / digest | MPa, mm, N, dimensionless UC, node IDs | Text reader leaves unmatched optional fields unset; digest parser accepts flat label/value pairs. [src/digitalmodel/ansys/results_extractor.py:L60-L72] [src/digitalmodel/ansys/results_extractor.py:L182-L214] [src/digitalmodel/ansys/results_extractor.py:L434-L468] |
| APDL materials / sections | Declared MPa, density field `density_kg_mm3`, mm², mm⁴ | Raw MP numbers are retained without unit conversion; SECDATA positions are interpreted as area/inertias, so this is not a universal section-command evaluator. [src/digitalmodel/ansys/apdl_reader.py:L27-L32] [src/digitalmodel/ansys/apdl_reader.py:L60-L69] [src/digitalmodel/ansys/apdl_reader.py:L105-L128] |
| Journal metadata | Version/system strings and unevaluated expressions | Regex extraction and continuation joining; no journal execution. [src/digitalmodel/ansys/wbjn_reader.py:L78-L128] [src/digitalmodel/ansys/wbjn_reader.py:L134-L155] |
| Evidence packages / response CSV | Canonical typed JSON with decimal strings; hashes | Strict packaging checks references and membership; diagnostic lookup reports `qualified=False`; engineering lookup requires additional live authority and qualification checks. [src/digitalmodel/ansys/analysis_evidence.py:L37-L100] [src/digitalmodel/ansys/analysis_lookup.py:L151-L183] [src/digitalmodel/ansys/analysis_lookup.py:L211-L232] |

Coordinate conventions are local to each generator: the wall slice uses X radial and Y axial; the exact cylinder reference maps hoop to Z and makes tension positive; the padeye lies in X-Y with sling angle from vertical toward +X and local cylindrical selection at the hole; the mudmat lies in X-Y with vertical load downward and soil pressure upward. No harmonic phase convention is defined by these static examples. [src/digitalmodel/ansys/pressure_vessel.py:L887-L910] [src/digitalmodel/ansys/cylinder_reference.py:L1-L5] [src/digitalmodel/ansys/padeye.py:L35-L39] [src/digitalmodel/ansys/padeye.py:L79-L84] [src/digitalmodel/ansys/padeye.py:L118-L139] [src/digitalmodel/ansys/mudmat.py:L185-L225]

UNVERIFIED: consistent thermal and generic-SI file units, allowable ranges absent from dataclass validation, and the APDL-reader density label require dimensional review before use (section 10, issue pending (4)).

## 3. Method and governing standards

### Example decks and physical intent

| Example / generator | Elements and geometry | Restraints and loads | Solution / extraction | Evidence |
| --- | --- | --- | --- | --- |
| Pressure vessel / `generate_pv_apdl` | PLANE182, KEYOPT(3)=1; rectangular radial/axial slice; mesh size derived from wall | Bottom UY only; inner-wall line pressure; optional uniform operating temperature relative to ambient; no end cap or radial restraint | STATIC, SOLVE, last result; sorted peak equivalent stress, node, UC and force sums in CSV | [src/digitalmodel/ansys/pressure_vessel.py:L841-L859] [src/digitalmodel/ansys/pressure_vessel.py:L883-L932] [src/digitalmodel/ansys/pressure_vessel.py:L934-L969] |
| Padeye / `generate_padeye_apdl` | PLANE182, KEYOPT(3)=3, thickness real constant; rectangular plate minus circular hole | Weld base y=0 fixed; equal nodal components over upper hole semicircle; sum equals sling components if the selection is nonempty | STATIC, SOLVE; global-frame stress/node/mesh counts; NODE RF reactions separately from FSUM | [src/digitalmodel/ansys/padeye.py:L79-L84] [src/digitalmodel/ansys/padeye.py:L101-L139] [src/digitalmodel/ansys/padeye.py:L142-L215] |
| Mudmat / `generate_mudmat_apdl` | SHELL181, shell section thickness, rectangular X-Y plate | Six translational constraints at three corners; downward linear pressure field and opposing effective-area soil patch; no Winkler stiffness | STATIC, SOLVE; peak plate stress/UC, node, force sum, prescribed soil pressure and eccentricity | [src/digitalmodel/ansys/mudmat.py:L175-L230] [src/digitalmodel/ansys/mudmat.py:L233-L268] |

For the mudmat, the implemented prescription is `e=M/V`, `L_eff=L-2e`, `A_eff=L_eff*B`, `q=V/A_eff`, with a linear applied pressure whose integral supplies V and M; its comment prints a different intercept from the operative field, so model identity follows commands, not comments. [src/digitalmodel/ansys/mudmat.py:L117-L143] [src/digitalmodel/ansys/mudmat.py:L213-L224]

### Closed form and code checks

The open-ended Lamé reference uses `A=p*a²/(b²-a²)`, `B=A*b²`, radial stress `A-B/r²`, hoop stress `A+B/r²`, and zero axial stress; equivalent stress is the square root of `sigma_r²+sigma_theta²-sigma_r*sigma_theta`. These are elasticity identities rather than tabulated standard allowables. [src/digitalmodel/ansys/cylinder_reference.py:L48-L61] (A. F. Bower, *Applied Mechanics of Solids*, online edition, sections 4.1.7–4.1.9, [axially symmetric elasticity and pressurized cylinders](https://solidmechanics.org/Text/Chapter4_1/Chapter4_1.htm)).

The reference also computes radial and axial displacement from the same elastic basis; its fixed basis and domain reject pressures or locations outside the supported diagnostic inputs. [src/digitalmodel/ansys/cylinder_reference.py:L17-L38] [src/digitalmodel/ansys/cylinder_reference.py:L48-L71]

| Method | Implementing function / emitted check | Standard locator and limitation |
| --- | --- | --- |
| Shell/head required thickness; MAWP; opening reinforcement; hydrotest | `ASMEThicknessCalc.ug27_cylindrical_shell`, `ug32_ellipsoidal_head`, `ug32_hemispherical_head`, `calculate_mawp`, `check_nozzle_reinforcement`, `calculate_hydrotest_pressure` | ASME BPVC Section VIII Division 1, *Rules for Construction of Pressure Vessels*, edition unspecified in code; UG-27 cylindrical shells, UG-32 formed heads, UG-37 openings, UG-99 hydrostatic tests; clause text not checked against the licensed copy. [src/digitalmodel/ansys/pressure_vessel.py:L112-L205] [src/digitalmodel/ansys/pressure_vessel.py:L380-L549] |
| Example screening UC | Peak equivalent stress divided by configured allowable; padeye/mudmat use supplied yield strength divided by supplied design factor | These are screening ratios, not a complete ASME stress-category assessment. [src/digitalmodel/ansys/pressure_vessel.py:L851-L859] [src/digitalmodel/ansys/pressure_vessel.py:L934-L942] [src/digitalmodel/ansys/padeye.py:L79-L84] [src/digitalmodel/ansys/mudmat.py:L100-L102] |
| Weld paths and stress-category ratios | `generate_stress_linearization`, `generate_asme_stress_check`; `calculate_linearized_stresses` integrates equally spaced through-thickness samples | ASME BPVC Section VIII Division 2, *Alternative Rules*, edition unspecified, Part 5 stress classification; clause text not checked against the licensed copy. [src/digitalmodel/ansys/weld_assessment.py:L170-L216] [src/digitalmodel/ansys/weld_assessment.py:L262-L361] |
| Hotspot extraction and weld-toe SCF | `generate_hotspot_stress_extraction`, `generate_weld_toe_scf` emit paths and parameters | DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, and IIW hotspot method, editions/clauses unspecified here; SCF docstring names one attribution while implementation comments name another; resolve in issue pending (5), not by choosing from memory; clause text not checked against the licensed copy. [src/digitalmodel/ansys/weld_assessment.py:L8-L24] [src/digitalmodel/ansys/weld_assessment.py:L104-L168] [src/digitalmodel/ansys/weld_assessment.py:L218-L248] |
| Fatigue damage | `calculate_cycles_to_failure`, `calculate_thickness_correction`, `calculate_fatigue_damage`: caller ranges, corrections, two-slope lookup, Miner sum, design fatigue factor | DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, October 2011, clause 2.4.4, S-N curves in air; clause text not checked against the licensed copy; no tabulated values reproduced. [src/digitalmodel/ansys/fatigue_postprocessor.py:L43-L48] [src/digitalmodel/ansys/fatigue_postprocessor.py:L158-L284] |
| Bolts/gaskets; contact; thermal | PRETS179/INTER195 generation; CONTA174 or CONTA175 with TARGE170; thermal commands and sequential coupling | Bolt defaults cite ASME B16.5 and gasket comments cite ASME BPVC gasket factors without an edition; these are setup helpers, not independently verified design-code coverage; clause text not checked against the licensed copy. [src/digitalmodel/ansys/bolt_analysis.py:L8-L20] [src/digitalmodel/ansys/bolt_analysis.py:L136-L250] [src/digitalmodel/ansys/contact_analysis.py:L93-L183] [src/digitalmodel/ansys/thermal_analysis.py:L383-L431] |

UNVERIFIED: governing editions, missing clause locators, weld-SCF attribution and calculation Citation sidecar coverage remain open; no licensed clause text was opened (section 10, issue pending (5)).

## 4. Validated worked examples

Offline results below are observations from the module test command at the header pin; a PASS is limited to the named assertion and does not establish a new native solve or engineering qualification. The native re-solve test is separately guarded. [tests/ansys/test_example_goldens.py:L155-L179] [src/digitalmodel/ansys/analysis_lookup.py:L151-L183]

| ID / class | Independent basis or regression input | Expected quantity / criterion | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 / validated closed-form comparison | Lamé open-ended thick cylinder; `examples/ansys/pressure-vessel/build.py`, radii 750 and 810 mm, internal pressure 10 MPa | Derived inner hoop 130.2 MPa and equivalent stress 135.5 MPa (display rounding only); compare committed equivalent stress within 1.0 %, with the test using unrounded values | `tests/ansys/test_example_goldens.py::test_pressure_vessel_golden_matches_closed_form` | PASS at BASE and AFTER; native evidence is historical. [examples/ansys/pressure-vessel/build.py:L22-L38] [tests/ansys/test_example_goldens.py:L72-L93] [tests/ansys/golden_acceptance.py:L93-L101] |
| R1 / regression | Pressure-vessel committed CSV and bound deck | UC equals peak/configured allowable within relative 1e-4; no independent code-allowable qualification is asserted | `tests/ansys/test_example_goldens.py::test_golden_unity_check_is_internally_consistent[pressure-vessel]` | PASS at BASE and AFTER. [tests/ansys/test_example_goldens.py:L99-L104] [tests/ansys/golden_acceptance.py:L51-L68] |
| R2 / regression | Mudmat committed CSV and bound deck | Same UC consistency within relative 1e-4; stress lacks an independent physical comparator | `tests/ansys/test_example_goldens.py::test_golden_unity_check_is_internally_consistent[mudmat]` | PASS at BASE and AFTER. [tests/ansys/test_example_goldens.py:L99-L104] [tests/ansys/golden_acceptance.py:L51-L68] |
| R3 / regression | `examples/ansys/padeye/build.py` and `padeye.inp` | 0 character differences after line-ending normalization; no stress-validation inference | `tests/ansys/test_padeye.py::test_committed_example_inp_matches_generator` | PASS at BASE and AFTER. [tests/ansys/test_padeye.py:L86-L109] |

The V1 calculation follows the open-cylinder equations in section 3; it validates a mathematical comparison at one geometry, not material capacity, a general mesh study or the private datasets. (A. F. Bower, *Applied Mechanics of Solids*, online edition, sections 4.1.7–4.1.9, [pressurized cylinder solution](https://solidmechanics.org/Text/Chapter4_1/Chapter4_1.htm)). [tests/ansys/test_example_goldens.py:L81-L93] [src/digitalmodel/ansys/cylinder_reference.py:L1-L5]

Native reproductions `tests/ansys/test_example_goldens.py::test_committed_golden_is_stable[pressure-vessel]` and `[mudmat]`: NOT_RUN, solver binary and licence required; pytest reports these two cases as SKIPPED because native capture is not enabled. No solver or licensed interface was invoked for this pack. [tests/ansys/test_example_goldens.py:L158-L179]

UNVERIFIED: private dataset IDs `ansys-cylinder-benchmark` and `ccx-ansys-lame-cylinder`, at the supplied repository revision `afd21de6b05f676495b9895d533cd5d918d98b02`, were not opened; their stated qualification state is 0 qualified responses, not independently confirmed here; dataset-specific revision tags and physical validation remain unconfirmed (section 10, #2121).

## 5. QA gates

UNVERIFIED: AQG r0 does not contain a FEA family row, and the family-specific revision was not supplied; the following G0–G5 placement is a proposed FEA mapping requiring confirmation, rather than a claim of clearance (section 10, issue pending (2)).

Each row defines a check; proposed thresholds are acceptance proposals, not values taken from a licensed standard. Applicable checks with no executable module implementation are gaps; a source citation identifies the relevant generation/extraction boundary rather than proving a gate passed.

| Check | Gate / applicability | Function, output and executable route | Numeric pass criterion and unit | Evidence / present state |
| --- | --- | --- | --- | --- |
| FE-01 | G0 applicable: input identity | `golden_acceptance.load_golden` recomputes SHA-256 of the committed deck; compare input binding | 0 mismatched input hashes (count); exactly 1 result CSV (file count) | Existing golden loader/tamper tests PASS offline. [tests/ansys/golden_acceptance.py:L51-L68] [tests/ansys/test_example_goldens.py:L38-L54] |
| FE-02 | G0 applicable: deck physical intent | Generate each example to scratch; inspect operative selections, restraints, element formulation and loads against section 3; integrate the mudmat field and sum padeye forces | Proposed: 0 mismatched element/BC/load specifications (count); force and moment errors ≤0.1 %; 0 empty loaded-node selections (count) | Partial tests exist for axisymmetry, absence of radial restraint, force direction and integrated field; whole-deck semantic check is a gap, #2094 and issue pending (2). [tests/ansys/test_pressure_vessel_template.py:L124-L169] [tests/ansys/test_padeye.py:L46-L59] [tests/ansys/test_mudmat.py:L144-L182] |
| FE-03 | G1 applicable: MAPDL load | On an authorized target version, read the exact deck through MAPDL batch input; retain version, input fingerprint, parse/mesh log and counts; use a preparation-only deck where available | Proposed: 0 errors, 0 undispositioned warnings, 0 missing selections (counts) | NOT_RUN: native program required; runner scans error markers but is not a load-only qualification gate. [src/digitalmodel/ansys/runner.py:L156-L175] [src/digitalmodel/ansys/runner.py:L231-L249] [src/digitalmodel/ansys/padeye_pressure.py:L11-L32] |
| FE-04 | G1 applicable: Workbench journal load | On an authorized target version, open the journal and referenced system/geometry; compare loaded parameter/system counts with manifest; `WBJNReader` provides advisory metadata only | Proposed: 0 execution errors/warnings and 0 system-count mismatches (counts) | Gap / NOT_RUN: regex metadata extraction does not execute journals. [src/digitalmodel/ansys/wbjn_reader.py:L49-L76] [src/digitalmodel/ansys/wbjn_reader.py:L87-L128] |
| FE-05 | G2 applicable: free-free modes | Build unconstrained modal variants of each mechanical example with explicit mass; compare extracted mode count/frequency against an independently declared rigid-mode basis | Proposed: 0 unexplained zero-stiffness modes (count); declare the expected mode count and numerical zero-frequency band before allowing PASS | Gap / NOT_RUN: `generate_solution` maps MODAL to ANTYPE 2 but supplies no dedicated modal extraction or mass-basis check; modal criteria need confirmation in issue pending (2). [src/digitalmodel/ansys/apdl_generator.py:L251-L283] [src/digitalmodel/ansys/pressure_vessel.py:L896-L905] |
| FE-06 | G2 applicable: unit-load route | Apply 1 N to a scratch padeye variant using the same support/load selections; sum NODE RF from the digest; compare force/displacement response with the full-load elastic scaling | Proposed: force residual ≤0.001 N; displacement scaling discrepancy ≤1.0 % | Gap / NOT_RUN: native unit-load solve needed; supported-load balance arithmetic already has offline tests. [src/digitalmodel/ansys/padeye.py:L164-L188] [tests/ansys/golden_acceptance.py:L109-L120] [tests/ansys/test_golden_acceptance.py:L122-L156] |
| FE-07 | G2 applicable: example reaction balance | `validate_equilibrium` consumes golden reactions and recomputes the resultant; supported loads use applied plus reaction components | Self-balanced vessel/mat resultant ≤1 N; padeye residual ≤0.1 % of applied load | Golden reaction tests PASS offline; synthetic padeye balance is not a solved padeye golden. [tests/ansys/golden_acceptance.py:L109-L120] [tests/ansys/golden_acceptance.py:L155-L177] [tests/ansys/test_example_goldens.py:L128-L132] |
| FE-08 | G2 applicable: independent stress reference | Recompute open-ended Lamé equivalent stress independently of the deck generator; `validate_pv_comparator` compares digest | Relative stress discrepancy ≤1.0 % at V1; geometry-specific MPa result | V1 PASS offline; no general design-code qualification. [tests/ansys/test_example_goldens.py:L81-L93] [tests/ansys/golden_acceptance.py:L93-L101] |
| FE-09 | G2 applicable: fatigue input/extraction boundary | Execute the scalar-versus-component equivalent-stress counterexample and inspect generated limitation text | Reversal example: scalar difference 0 MPa versus component difference 200 MPa; 0 missing limitation strings (count) | Existing counterexample test PASS; scalar subtraction is not accepted as a general stress-range reference, #2410. [tests/ansys/test_fatigue_postprocessor.py:L337-L355] |
| FE-10 | G3 applicable: mesh convergence | `cylinder_criteria.evaluate_canary` compares n4/n8/n16 primary station quantities, refinement magnitude and decrease; use native evidence, not synthetic fixtures, for qualification | Fine-minus-medium magnitude ≤max(0.5 % of reference, dimensional floor); next difference no larger than previous plus floor; expected-zero stress floor 0.010 MPa | Existing numerical evaluator and synthetic tests; native convergence not established by this pack, #2121. [src/digitalmodel/ansys/cylinder_criteria.py:L98-L140] [tests/ansys/test_cylinder_criteria.py:L9-L35] |
| FE-11 | G3 applicable: padeye mesh qualification | `prepare_mesh_study` produces coarse/fine decks; compare identical stress-recovery definitions at a fixed physical location, applied load and peak location | Proposed reuse of cylinder refinement check: fixed-location stress change ≤max(0.5 % of reference, dimensional floor), reaction residual ≤0.1 %; at least 3 mesh levels (count) | Gap: current preparation writes 2 decks and expressly leaves convergence unestablished, #2117 and issue pending (2). [examples/ansys/padeye/build.py:L8-L11] [examples/ansys/padeye/build.py:L38-L46] [src/digitalmodel/ansys/cylinder_criteria.py:L98-L110] |
| FE-12 | G3 NOT_APPLICABLE to short dynamics for these examples | These example solve blocks are static structural; thermal-transient helper coverage is separate | 0 dynamic-response claims (count) for section-4 cases | Static route cited; no short-dynamics clearance claimed. [src/digitalmodel/ansys/pressure_vessel.py:L925-L932] [src/digitalmodel/ansys/padeye.py:L136-L140] [src/digitalmodel/ansys/mudmat.py:L227-L231] |
| FE-13 | G4 applicable: element quality | Before accepting native results, evaluate the generated mesh's Jacobian, shape warnings and aspect ratios on every element; use a target-version mesh report | Proposed: 0 nonpositive Jacobians, 0 undispositioned shape warnings (counts), 100 % of elements pass the target-version default shape limits | General example gate is a gap; `assess_shape_output` already checks a complete default-limit PLANE182 shape summary with 0 warnings/errors, but establishes no stress solution, issue pending (2). [tests/ansys/padeye_pressure_quality.py:L5-L34] [src/digitalmodel/ansys/padeye_pressure_mesh.py:L108-L127] [src/digitalmodel/ansys/padeye_pressure.py:L11-L32] |
| FE-14 | G4 NOT_APPLICABLE to diffraction checks | Direct-load example generators do not generate a hydrodynamic diffraction solution | 0 diffraction-clearance claims (count) | The documented diffraction chain explicitly separates direct-load cases. [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L57-L84] |
| FE-15 | G5 applicable: evidence appendix | `analysis_evidence.validate_package` plus engineering `analysis_lookup.lookup`: recheck hashes, rights, required roles and independent comparison before any qualified result enters an appendix | 0 missing/mismatched required references and 0 unresolved inherited findings (counts); 100 % of reported quantities bound to accepted evidence | Existing evidence validation supports checks; automated AQG appendix/signature integration remains a gap, issue pending (2). [src/digitalmodel/ansys/analysis_evidence.py:L87-L100] [src/digitalmodel/ansys/analysis_lookup.py:L151-L208] |

G1 for the direct examples means MAPDL, and journal loading means Workbench; the AQWA workflow's pressure transfer and global/submodel route is a separate unvalidated workflow, not satisfied by either example load check. [src/digitalmodel/ansys/runner.py:L156-L175] [src/digitalmodel/ansys/wbjn_reader.py:L8-L16] [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L9-L11] [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L19-L31]

## 6. Known failure modes and pitfalls

| Symptom / cause | Detection or missing check | Incident / disposition |
| --- | --- | --- |
| A deterministic deck has incorrect physics: original pressure wall over-constrained; original mat foundation stiffness ineffective | FE-02 physical intent, FE-07 reactions, FE-08 Lamé; byte matching alone is insufficient; current generator removes radial restraint and obsolete foundation constant | #2094, original incident and subsequent fixes; native qualification remains separate. [tests/ansys/test_example_goldens.py:L1-L14] [src/digitalmodel/ansys/pressure_vessel.py:L896-L910] [src/digitalmodel/ansys/mudmat.py:L106-L143] |
| Missing solver appears as dry-run success | Router rejects unrequested dry-run fallback; check 0 completed solve records from an unavailable executable | Current fail-closed route; queue integration tracked by #1724. [src/digitalmodel/ansys/workflow.py:L65-L76] |
| CSV digest previously discarded; stale outputs mistaken for fresh results | FE-01; fresh-output snapshots and CSV content hashes; runner error markers even when return code is zero | #2094; CSV capture and digest reader now exist. [src/digitalmodel/ansys/runner.py:L31-L33] [src/digitalmodel/ansys/runner.py:L193-L213] [src/digitalmodel/ansys/runner.py:L251-L275] [src/digitalmodel/ansys/results_extractor.py:L434-L468] |
| Rainflow advertised without a counting implementation | FE-09 plus `test_module_docstring_states_caller_input_boundary`; caller provides ranges and counts | #2098, closed; claim removed at the pin. [src/digitalmodel/ansys/fatigue_postprocessor.py:L14-L18] [tests/ansys/test_fatigue_postprocessor.py:L243-L256] |
| Extra fatigue curves/code, contour generation, environment label and infinite-life claims misstate implemented behavior | `test_module_does_not_advertise_absent_capabilities`, documented/stored class equality, environment and extrapolation tests; FE-09 catches the remaining scalar extraction limitation | #2410, open; documentation/table tests now cover the historical claims, but no licensed clause verification performed here. [tests/ansys/test_fatigue_postprocessor.py:L259-L280] [tests/ansys/test_fatigue_postprocessor.py:L302-L355] |
| Equivalent-stress scalar subtraction can hide a reversed component range | FE-09; never use the generated scalar difference as a general component-range hand reference | #2410; limitation documented in code. [src/digitalmodel/ansys/fatigue_postprocessor.py:L286-L345] |
| Queue handoff not established by local runner or batch launcher | No end-to-end queue detection check exists in the main route; require manifest/dispatch/retrieval test before treating local completion as queue integration | #1724; router calls the local runner and batch helper emits launcher text. [src/digitalmodel/ansys/workflow.py:L41-L54] [src/digitalmodel/ansys/batch_runner.py:L506-L550] |
| Outdated capability descriptions conceal changed idealization | Section-1 audit; no comprehensive capability-to-implementation check exists | Issue pending (1). [src/digitalmodel/ansys/mudmat.py:L3-L31] [src/digitalmodel/ansys/mudmat.py:L106-L126] |
| Unit label accepted without dimensional conversion; unsupported general-SI density path | Compare `generate_header` SI declaration with unconditional density conversion; FE-02 needs a dimensional extension | Issue pending (4). [src/digitalmodel/ansys/apdl_generator.py:L133-L168] |
| Diagnostic numerical PASS promoted as engineering qualification | FE-15; `evaluate_canary` always reports engineering qualification false; lookup requires independent authority and resolved findings | #2121 / #2123. [src/digitalmodel/ansys/cylinder_criteria.py:L65-L68] [src/digitalmodel/ansys/analysis_lookup.py:L151-L208] |
| Extractor silently returns empty/default results for unmatched text or overwrites duplicate digest labels | Require field presence, finiteness and unique keys through `analysis_evidence.parse_digest`; do not treat zero defaults as retrieved physical zero | #2123; strict evidence parser differs from generic extractor. [src/digitalmodel/ansys/results_extractor.py:L182-L214] [src/digitalmodel/ansys/results_extractor.py:L356-L357] [src/digitalmodel/ansys/results_extractor.py:L453-L468] [src/digitalmodel/ansys/analysis_evidence.py:L20-L34] |

## 7. Lessons log

Append-only entries; shared-log identifiers remain pending rather than asserting an external write.

| Date | Entry | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-15 | L1 | Separate text reproducibility, numerical comparison and engineering qualification in sections 4–6 | Golden tests distinguish comparator classes; engineering lookup checks live qualification. [tests/ansys/test_example_goldens.py:L135-L152] [src/digitalmodel/ansys/analysis_lookup.py:L151-L183] | Pending; issue pending (3) |
| 2026-10-15 | L2 | Audit descriptions against operative commands before selecting hand references | Mudmat description and generated pressure-patch implementation differ; section 1 records the discrepancy. [src/digitalmodel/ansys/mudmat.py:L3-L31] [src/digitalmodel/ansys/mudmat.py:L106-L143] | Pending; issue pending (1) |

## 8. Ownership

Proposed assignments: family quality manager — FEA quality manager; deputy — FEA deputy quality manager; production — analysis and software maintainers; escalation — the maintainer after a quality-manager disposition. UNVERIFIED: appointments to these proposed roles are not confirmed; confirmation is issue pending (2), listed as U5 in section 10.

Escalate missing data, ambiguous clauses, altered tolerances and any unavailable gate evidence before making an engineering qualification claim; engineering lookup already refuses incomplete verification and unresolved inherited findings. [src/digitalmodel/ansys/analysis_lookup.py:L145-L182] [src/digitalmodel/ansys/analysis_lookup.py:L186-L208]

## 9. Dependencies and alternative implementations

| Boundary | Observed dependency / alternative | Scope |
| --- | --- | --- |
| Direct solve | Workflow delegates to `run_ansys`; runner uses subprocess MAPDL with caller-supplied or detected executable and timeout | Local execution, not queue dispatch. [src/digitalmodel/ansys/workflow.py:L41-L54] [src/digitalmodel/ansys/runner.py:L156-L175] [src/digitalmodel/ansys/runner.py:L215-L229] |
| Numeric/text helpers | Batch uses NumPy parameter grids; result extraction uses pandas; fatigue uses supplied load cases; weld linearization uses NumPy integration | Pure computation and text processing on the main path. [src/digitalmodel/ansys/batch_runner.py:L99-L110] [src/digitalmodel/ansys/results_extractor.py:L233-L251] [src/digitalmodel/ansys/fatigue_postprocessor.py:L244-L284] [src/digitalmodel/ansys/weld_assessment.py:L336-L361] |
| Thick-cylinder cross-check | Independent Lamé arithmetic in the golden test and rational/Decimal implementation in `cylinder_reference` | Different representations of the same open-ended mathematical problem; agreement alone does not authenticate solver evidence. [tests/ansys/test_example_goldens.py:L81-L93] [src/digitalmodel/ansys/cylinder_reference.py:L48-L71] |
| Soil-bearing downstream check | `geotechnical.mudmat.mudmat_bearing_capacity` handles drained/undrained bearing and chooses eccentricity axis | Separate from prescribed shell reaction; units m, kN and kN m at this interface require explicit conversion from the APDL mm/N model. [src/digitalmodel/geotechnical/mudmat.py:L96-L119] [src/digitalmodel/geotechnical/mudmat.py:L161-L175] [src/digitalmodel/ansys/mudmat.py:L252-L268] |
| Cycle counting elsewhere | `structural.fatigue_apps.rainflow_counter` accepts time series and extracts reversals/cycles | Alternative upstream preparation, not an in-process counting capability of the ANSYS fatigue module. [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L49-L79] [src/digitalmodel/ansys/fatigue_postprocessor.py:L14-L18] |
| Other hoop-stress implementation | `structural.structural_analysis.stress_calculator.StressCalculator.hoop_stress` | Compare the function's own thin-wall assumptions before substituting it for the thick-wall reference. [src/digitalmodel/structural/structural_analysis/stress_calculator.py:L93-L110] |
| Diffraction-to-structure | Documented AQWA / Mechanical / Workbench transfer and submodel chain | Separate workflow; release basis is documented as 2024 R2 / 2025 R1, with validation not established. [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L9-L11] [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L19-L42] |

Repository-wide discovery included `src/`, `tests/`, `scripts/` and documentation for Lamé/thick-cylinder arithmetic, hoop stress, rainflow, effective area and linearization; `scripts/example_reporting.py` uses a rainflow analysis identifier rather than implementing cycle counting. [scripts/example_reporting.py:L110-L123] [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L49-L79]

Private source access is a reading boundary: `wikis/engineering/wiki/tools/fea-open-source/libredwg-build.md` was read without copying its text; it provides no public citation for solver behavior in this pack. UNVERIFIED: recommendations or capabilities dependent on that private reading are excluded pending public-source coverage (section 10, issue pending (3)).

## 10. Coverage and open items

Coverage: main-path workflow/runner, batch/APDL generation, APDL/journal readers, text results, pressure wall, padeye and mudmat generation, bolt/contact/thermal helpers, weld and fatigue methods; evidence packaging and qualification predicates were inspected, while the expanded transition machinery remains indexed for follow-up. [src/digitalmodel/ansys/workflow.py:L27-L77] [src/digitalmodel/ansys/analysis_evidence.py:L20-L100] [src/digitalmodel/ansys/analysis_lookup.py:L151-L232]

The documentation reading includes the domain index, Workbench guidelines/lessons and the diffraction/submodel contract; index file counts are README claims, unverified, and are not used as test or maturity evidence. Workbench lessons themselves leave the parameter-table question-mark cause unknown. [docs/domains/ansys/_index.md:L3-L9] [docs/domains/ansys/workbench/guidelines.md:L18-L28] [docs/domains/ansys/workbench/lessons.md:L5-L9]

Offline execution record against the header pin (BASE before edits; AFTER on the uncommitted documentation tree): `PYTHONPATH=src uv run --no-sources python -m pytest tests/ansys/ -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, no worker option. BASE: 2801 selected, 2389 passed, 0 failed, 412 skipped; native NOT_RUN: 2 of the skipped cases. AFTER: 2801 selected, 2389 passed, 0 failed, 412 skipped; the same 2 native cases remain NOT_RUN. Results describe test execution, not solver execution. [tests/ansys/conftest.py:L1-L10] [tests/ansys/test_example_goldens.py:L158-L179]

The platform-gated and native test names and their reasons are retained in the test logs; all section-4 case node IDs are listed explicitly above. The existing platform gate applies individual test names, leaving other tests in each file runnable. [tests/ansys/conftest.py:L1-L10] [tests/ansys/conftest.py:L291-L308]

| Item | Coverage or action required | Responsible role / issue |
| --- | --- | --- |
| U1 | UNVERIFIED: completeness of capability coverage outside the main path; fully audit cylinder and extended evidence transitions, complete shared-log registration and remaining dependency inventory before clearance | FEA quality manager / issue pending (3) |
| U2 | UNVERIFIED: dimensional consistency of generic SI density, APDL-reader density labels and thermal properties/BCs; validate ranges not enforced by the dataclasses | Software and analysis maintainers / issue pending (4) |
| U3 | UNVERIFIED: governing standard editions, missing clauses, SCF attribution and Citation sidecars; obtain licensed-copy checks without publishing tabulated values | FEA quality manager / issue pending (5) |
| U4 | UNVERIFIED: private `ansys-cylinder-benchmark` and `ccx-ansys-lame-cylinder` revision tags, independent comparator provenance and stated 0 qualified responses at supplied revision `afd21de6b05f676495b9895d533cd5d918d98b02`; no private data read | FEA quality manager / #2121 |
| U5 | UNVERIFIED: FEA-specific AQG revision, gate placement, proposed numeric thresholds and deputy assignment; free-free, unit-load, generic element-quality and appendix integration need implementation/evidence | FEA quality manager / issue pending (2) |
| U6 | UNVERIFIED: public support for any capability inferred from the private FEA tool page; no private page text included | Documentation maintainers / issue pending (3) |
| O1 | Correct stale/nonimplementing capability statements listed in section 1; these are evidence-backed discrepancies, not new engineering capabilities | Software maintainers / issue pending (1) |
| O2 | Obtain native example rerun and physical-intent evidence; do not promote byte-match or archived CSV tests to physical qualification | Analysis maintainers / #2094 |
| O3 | Keep caller-input and scalar-range limitations explicit; independently check the cited fatigue edition before any broader design claim | FEA quality manager / #2410; historical #2098 closed |
| O4 | Establish queue-package/dispatch/retrieval evidence independently of the local runner | Software maintainers / #1724 |
| O5 | Complete padeye mesh/contact/three-dimensional qualification and a nonsingular response basis | Analysis maintainers / #2117 |
| O6 | Extend strict retrieval/precision and native-source verification beyond the bounded evidence route | Software maintainers / #2123 |

### Cylinder utility index

Index entries identify existing files and their first defined callable/class; they do not assert that a native solver operation or full behavior audit was performed.

| File / first definition | Pinned source |
| --- | --- |
| `cylinder_absence_collection.py` / `CollectionError` | [src/digitalmodel/ansys/cylinder_absence_collection.py:L42-L45] |
| `cylinder_absence_population.py` / `PopulationError` | [src/digitalmodel/ansys/cylinder_absence_population.py:L10-L13] |
| `cylinder_active_names.py` / `_codec` | [src/digitalmodel/ansys/cylinder_active_names.py:L5-L8] |
| `cylinder_adapter.py` / `reference_table` | [src/digitalmodel/ansys/cylinder_adapter.py:L18-L21] |
| `cylinder_agreement.py` / `compare_reference` | [src/digitalmodel/ansys/cylinder_agreement.py:L14-L17] |
| `cylinder_benchmark.py` / `frozen_basis` | [src/digitalmodel/ansys/cylinder_benchmark.py:L27-L28] |
| `cylinder_canary.py` / `runtime_sources` | [src/digitalmodel/ansys/cylinder_canary.py:L21-L24] |
| `cylinder_cfd_owner_evidence.py` / `_pairs` | [src/digitalmodel/ansys/cylinder_cfd_owner_evidence.py:L15-L18] |
| `cylinder_criteria.py` / `_checked` | [src/digitalmodel/ansys/cylinder_criteria.py:L18-L21] |
| `cylinder_deck.py` / `marked` | [src/digitalmodel/ansys/cylinder_deck.py:L13-L14] |
| `cylinder_deck_exports.py` / `record` | [src/digitalmodel/ansys/cylinder_deck_exports.py:L11-L12] |
| `cylinder_diagnostic_admission.py` / `_path` | [src/digitalmodel/ansys/cylinder_diagnostic_admission.py:L18-L21] |
| `cylinder_diagnostic_preflight.py` / `_now` | [src/digitalmodel/ansys/cylinder_diagnostic_preflight.py:L29-L30] |
| `cylinder_diagnostic_resources.py` / `_decimal` | [src/digitalmodel/ansys/cylinder_diagnostic_resources.py:L13-L16] |
| `cylinder_diagnostic_snapshot.py` / `_enumerate` | [src/digitalmodel/ansys/cylinder_diagnostic_snapshot.py:L11-L14] |
| `cylinder_diagnostic_snapshot_v2.py` / `_created` | [src/digitalmodel/ansys/cylinder_diagnostic_snapshot_v2.py:L18-L21] |
| `cylinder_expression.py` / `ExpressionError` | [src/digitalmodel/ansys/cylinder_expression.py:L25-L26] |
| `cylinder_forwarder_resources.py` / `PEReader` | [src/digitalmodel/ansys/cylinder_forwarder_resources.py:L10-L13] |
| `cylinder_intermediate_lineage.py` / `_require` | [src/digitalmodel/ansys/cylinder_intermediate_lineage.py:L19-L20] |
| `cylinder_mesh.py` / `mapped_mesh` | [src/digitalmodel/ansys/cylinder_mesh.py:L17-L19] |
| `cylinder_operational_reservation.py` / `ReservationError` | [src/digitalmodel/ansys/cylinder_operational_reservation.py:L13-L14] |
| `cylinder_parent_identities.py` / `_validated` | [src/digitalmodel/ansys/cylinder_parent_identities.py:L13-L16] |
| `cylinder_parent_map.py` / `ParentMapChangeError` | [src/digitalmodel/ansys/cylinder_parent_map.py:L26-L29] |
| `cylinder_preparation.py` / `PreparationError` | [src/digitalmodel/ansys/cylinder_preparation.py:L24-L27] |
| `cylinder_preparation_history.py` / `_entries` | [src/digitalmodel/ansys/cylinder_preparation_history.py:L19-L22] |
| `cylinder_preparation_streams.py` / `expected_stream_paths` | [src/digitalmodel/ansys/cylinder_preparation_streams.py:L12-L15] |
| `cylinder_pressure_admission.py` / `required_source_inventory` | [src/digitalmodel/ansys/cylinder_pressure_admission.py:L20-L23] |
| `cylinder_pressure_capture.py` / `_capture_files` | [src/digitalmodel/ansys/cylinder_pressure_capture.py:L27-L30] |
| `cylinder_pressure_journal.py` / `owned_path` | [src/digitalmodel/ansys/cylinder_pressure_journal.py:L10-L13] |
| `cylinder_pressure_lineage.py` / `_bound` | [src/digitalmodel/ansys/cylinder_pressure_lineage.py:L21-L24] |
| `cylinder_pressure_resources.py` / `_timestamp` | [src/digitalmodel/ansys/cylinder_pressure_resources.py:L22-L25] |
| `cylinder_pressure_resume.py` / `_empty` | [src/digitalmodel/ansys/cylinder_pressure_resume.py:L23-L26] |
| `cylinder_pressure_scope.py` / `pressure_step` | [src/digitalmodel/ansys/cylinder_pressure_scope.py:L10-L13] |
| `cylinder_process.py` / `_resume` | [src/digitalmodel/ansys/cylinder_process.py:L17-L18] |
| `cylinder_process_inventory.py` / `_text` | [src/digitalmodel/ansys/cylinder_process_inventory.py:L19-L22] |
| `cylinder_process_inventory_v2.py` / `_base` | [src/digitalmodel/ansys/cylinder_process_inventory_v2.py:L8-L10] |
| `cylinder_process_windows.py` / `BasicLimit` | [src/digitalmodel/ansys/cylinder_process_windows.py:L13-L16] |
| `cylinder_reference.py` / `_input` | [src/digitalmodel/ansys/cylinder_reference.py:L21-L24] |
| `cylinder_reference_provenance.py` / `_json` | [src/digitalmodel/ansys/cylinder_reference_provenance.py:L24-L27] |
| `cylinder_refinement_replay.py` / `_path` | [src/digitalmodel/ansys/cylinder_refinement_replay.py:L22-L25] |
| `cylinder_resource_absence.py` / `validate_absence_descriptor` | [src/digitalmodel/ansys/cylinder_resource_absence.py:L17-L20] |
| `cylinder_results.py` / `EvidenceError` | [src/digitalmodel/ansys/cylinder_results.py:L15-L16] |
| `cylinder_results_audit.py` / `_lines` | [src/digitalmodel/ansys/cylinder_results_audit.py:L13-L16] |
| `cylinder_results_cdb.py` / `_require` | [src/digitalmodel/ansys/cylinder_results_cdb.py:L12-L14] |
| `cylinder_results_diagnostics.py` / `_events` | [src/digitalmodel/ansys/cylinder_results_diagnostics.py:L13-L16] |
| `cylinder_results_listings.py` / `extract_block` | [src/digitalmodel/ansys/cylinder_results_listings.py:L12-L15] |
| `cylinder_results_native_listings.py` / `_rows` | [src/digitalmodel/ansys/cylinder_results_native_listings.py:L11-L14] |
| `cylinder_results_native_loads.py` / `verify_zero_loads` | [src/digitalmodel/ansys/cylinder_results_native_loads.py:L7-L10] |
| `cylinder_results_native_pressure.py` / `_expected_faces` | [src/digitalmodel/ansys/cylinder_results_native_pressure.py:L23-L26] |
| `cylinder_results_native_reactions.py` / `_prefix` | [src/digitalmodel/ansys/cylinder_results_native_reactions.py:L10-L13] |
| `cylinder_results_native_status.py` / `lines` | [src/digitalmodel/ansys/cylinder_results_native_status.py:L16-L19] |
| `cylinder_results_native_values.py` / `summary_fields` | [src/digitalmodel/ansys/cylinder_results_native_values.py:L5-L8] |
| `cylinder_results_state.py` / `parse_state_values` | [src/digitalmodel/ansys/cylinder_results_state.py:L9-L12] |
| `cylinder_results_validation.py` / `_recovery` | [src/digitalmodel/ansys/cylinder_results_validation.py:L20-L23] |
| `cylinder_runner.py` / `_paths` | [src/digitalmodel/ansys/cylinder_runner.py:L21-L24] |
| `cylinder_runtime_bundle.py` / `_owning_root` | [src/digitalmodel/ansys/cylinder_runtime_bundle.py:L21-L24] |
| `cylinder_wrapper_consoles.py` / `_branches` | [src/digitalmodel/ansys/cylinder_wrapper_consoles.py:L8-L11] |

### Extended evidence transition index

| File / first definition | Pinned source |
| --- | --- |
| `analysis_evidence.py` / `parse_digest` | [src/digitalmodel/ansys/analysis_evidence.py:L20-L23] |
| `analysis_intake.py` / `_relative` | [src/digitalmodel/ansys/analysis_intake.py:L17-L20] |
| `analysis_lookup.py` / `create_ledger` | [src/digitalmodel/ansys/analysis_lookup.py:L20-L23] |
| `analysis_matrix_publish.py` / `_evidence` | [src/digitalmodel/ansys/analysis_matrix_publish.py:L15-L18] |
| `analysis_observed.py` / `_reference` | [src/digitalmodel/ansys/analysis_observed.py:L17-L20] |
| `analysis_pending.py` / `_validate_pending` | [src/digitalmodel/ansys/analysis_pending.py:L18-L21] |
| `analysis_pending_inputs.py` / `_reference` | [src/digitalmodel/ansys/analysis_pending_inputs.py:L10-L13] |
| `analysis_pressure_inputs.py` / `document` | [src/digitalmodel/ansys/analysis_pressure_inputs.py:L28-L31] |
| `analysis_pressure_observed.py` / `intake_code_files` | [src/digitalmodel/ansys/analysis_pressure_observed.py:L10-L12] |
| `analysis_pressure_source.py` / `source_inventory` | [src/digitalmodel/ansys/analysis_pressure_source.py:L20-L21] |
| `analysis_records.py` / `canonical_bytes` | [src/digitalmodel/ansys/analysis_records.py:L12-L15] |
| `analysis_recovery_note.py` / `require` | [src/digitalmodel/ansys/analysis_recovery_note.py:L24-L26] |
| `analysis_replay.py` / `canonical_measurement` | [src/digitalmodel/ansys/analysis_replay.py:L16-L19] |
| `analysis_replay_inputs.py` / `source_inventory` | [src/digitalmodel/ansys/analysis_replay_inputs.py:L24-L27] |
