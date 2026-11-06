# Mooring analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | `subsea/mooring_analysis` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/subsea/mooring_analysis/docs/agent-pack.md` |
| Pack revision | 0, 2026-10-15 |
| Code revision | `f9c058d6ef4e69d01630d90ea90033b47ce9d30f` |
| Analysis family | MR, OrcaFlex riser and mooring |
| Quality manager role | Riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`CatenaryAnalyzer` returns single-material geometry, tensions and stiffness; `MooringDesigner` screens intact and damaged line tensions; `OrcaFlexModelGenerator` writes model dictionaries and YAML. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L119] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L178-L238] [src/digitalmodel/subsea/mooring_analysis/designer.py:L209-L356] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L165-L247]

The separate legacy static route builds load-excursion tables, restoring forces, fender forces, wind forces and passing-ship forces; its time-domain integrator is not ported. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L10-L62] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L85-L93]

The routing registry marks the enclosing `subsea` domain stable, rather than providing a separate maturity for this module; the README's production-readiness statement is a README claim, unverified (U1–U4 in section 10 qualify its engineering limits). [docs/registry/module-routing.yaml:L287-L292] [src/digitalmodel/subsea/mooring_analysis/README.md:L1-L9]

The designer uses the first segment only, does not apply yaw rotation, and substitutes a geometric estimate after a catenary exception; the environmental calculation sums scalar loads without solving vessel equilibrium. [src/digitalmodel/subsea/mooring_analysis/designer.py:L145-L207] [src/digitalmodel/subsea/mooring_analysis/designer.py:L85-L124]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Range or guard | Default | Source and evidence |
| --- | --- | --- | --- | --- |
| Segment length, diameter, breaking load, submerged mass per length, axial stiffness | m, mm, kN, kg/m, kN | Dataclass fields have no numeric range validation | Required; drag coefficient has a coded default | Caller or `MOORING_MATERIALS`; source provenance remains U2. [src/digitalmodel/subsea/mooring_analysis/models.py:L35-L49] [src/digitalmodel/subsea/mooring_analysis/models.py:L278-L302] |
| Horizontal tension; vertical span | kN; m | H > 0 kN; absolute submerged force per length >= 0.000001 kN/m; negative weight is not rejected | Span defaults to water depth | `solve_catenary`. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L62-L76] |
| Target fairlead tension; convergence controls | kN; iterations; kN | Raises after the iteration limit | 100 iterations; 0.01 kN residual | `solve_for_horizontal_tension`. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L121-L175] |
| Anchor and fairlead locations; pretension | m; kN | No dataclass coordinate/range guard | Pretension 0 kN | `MooringLine`, `AnchorProperties`. [src/digitalmodel/subsea/mooring_analysis/models.py:L52-L82] |
| Vessel dimensions, displacement, windage | m, tonnes, m² | Required fields, no numeric validation | Caller supplies values | `VesselParticulars`. [src/digitalmodel/subsea/mooring_analysis/models.py:L103-L116] |
| Wave height, peak period, current and wind speed; headings | m, s, m/s, degrees from North | Required fields, no numeric validation | Return period 100 years | `EnvironmentalConditions`. [src/digitalmodel/subsea/mooring_analysis/models.py:L85-L100] |
| Offset and dynamic amplification | surge/sway m; yaw degrees; ratio | Yaw unpacked but unused | Intact/damaged amplification 1.5 | Designer API. [src/digitalmodel/subsea/mooring_analysis/designer.py:L126-L170] [src/digitalmodel/subsea/mooring_analysis/designer.py:L209-L214] [src/digitalmodel/subsea/mooring_analysis/designer.py:L272-L277] |
| Legacy segment data | ft, lb/ft, in², ksi, kips | Material fallback preserves supplied weight; EA converted from area and modulus | Empty segment defaults | `DynmoorSegment`; keep separate from SI inputs. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L180-L208] |
| Legacy layout, offset, pretension | ft; yaw radians in `restoring_force`; percent of minimum breaking strength | Pretension outside populated table raises | Broken-line list optional | `solve_pretension`, `restoring_force`. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L833-L884] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L913-L961] |

### Outputs and conventions

| Output | Unit and convention | Evidence |
| --- | --- | --- |
| `CatenaryResult` | H, top and touchdown tension kN; lengths m; angle computed from horizontal by atan2(w*s,H) | [src/digitalmodel/subsea/mooring_analysis/models.py:L139-L150] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L95-L118] |
| `StiffnessResult` | Horizontal, vertical, geometric, elastic stiffness kN/m | [src/digitalmodel/subsea/mooring_analysis/models.py:L153-L159] |
| `MooringDesignResult` | Maximum tension and required/actual breaking load kN; safety factor and utilisation dimensionless | [src/digitalmodel/subsea/mooring_analysis/models.py:L192-L203] |
| Workflow JSON | Environmental loads, summary, line results; NumPy scalar conversion; explicit output path or result-folder fallback | [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L111-L155] |
| Generator dictionary/YAML | Declares SI; converts diameter mm to m and stiffness/breaking load kN to N; emits mass in kg/m and density as coded; target-program compatibility remains U3 | [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L58-L85] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L155-L162] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L210] |
| End coordinates | End A fixed anchor, end B vessel fairlead; workflow places seabed at negative water depth, fairlead at negative fairlead depth, vessel origin at (0,0,0) | [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L112] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L134-L145] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L83-L105] |
| Legacy tables and forces | Table tensions lb, distances ft; restoring surge/sway lb and yaw lb-ft resolved to body axes; direction convention clockwise from bow in module documentation | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L58-L62] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L708-L744] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L935-L961] |
| CLI JSON | Rounded tension/geometry and stiffness dictionaries; no phase convention is consumed by these calculations | [src/digitalmodel/subsea/mooring_analysis/cli.py:L98-L121] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L43] |

## 3. Method and governing standards

### Line-property sources and safety-factor selection

`MOORING_MATERIALS` is a literal in-code chain/wire/polyester library; `get_material` returns its stored object, while the spread-workflow builder copies it with `dataclasses.replace`. Manufacturer revision, certificates and calculation-output citation sidecars are not attached to these property objects (U2). [src/digitalmodel/subsea/mooring_analysis/models.py:L206-L302] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L63-L68]

The designer attributes its condition/method matrix to DNV-OS-E301, *Position Mooring*, edition unspecified, with a numeric table locator in the source comment; no clause title or edition is supplied there. Clause text not checked against the licensed copy (U1). [src/digitalmodel/subsea/mooring_analysis/designer.py:L34-L48]

Numerical safety-factor constants are identified by their source locations rather than reproduced here; select the `INTACT`, `DAMAGED` or `TRANSIENT` row and the `quasi_static` or `dynamic` column only after confirming the applicable standard basis. [src/digitalmodel/subsea/mooring_analysis/designer.py:L34-L48]

| Limit state | Coded selector and active route | Clause/provenance limit |
| --- | --- | --- |
| Intact | `DesignLoadCase.safety_factor_required` selects intact; `analyze_intact_condition` always selects intact/dynamic | DNV-OS-E301 attribution only; U1. [src/digitalmodel/subsea/mooring_analysis/models.py:L180-L189] [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L252] |
| Damaged | Property selects damaged; damaged analysis removes the named line, applies N/(N−1) redistribution and selects damaged/dynamic | DNV-OS-E301 attribution only; U1. [src/digitalmodel/subsea/mooring_analysis/models.py:L180-L189] [src/digitalmodel/subsea/mooring_analysis/designer.py:L289-L336] |
| Transient | Property selects transient; `analyze_all_conditions` instead reduces amplification and calls intact analysis, retaining intact/dynamic selection | Selection mismatch; issue pending (1). [src/digitalmodel/subsea/mooring_analysis/models.py:L180-L189] [src/digitalmodel/subsea/mooring_analysis/designer.py:L377-L396] [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L252] |
| Quasi-static | Matrix contains a separate column, but these intact/damaged execution routes select dynamic | Method selection is not exposed by these entry points. [src/digitalmodel/subsea/mooring_analysis/designer.py:L34-L48] [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L227] [src/digitalmodel/subsea/mooring_analysis/designer.py:L292-L293] |
| API RP 2SK | README cites API RP 2SK, *Design and Analysis of Stationkeeping Systems for Floating Structures*, edition and clause title unspecified; executable matrix is labelled DNV | No API-specific intact/damaged/transient selector is established; clause text not checked against the licensed copy, U1. [src/digitalmodel/subsea/mooring_analysis/README.md:L361-L368] [src/digitalmodel/subsea/mooring_analysis/designer.py:L34-L48] |

`DesignLoadCase` exposes the required factor, but the design results are built from the designer's own selected factor: required breaking load = maximum tension × selected factor, actual factor = minimum segment breaking load / maximum tension, utilisation = maximum tension / minimum segment breaking load. [src/digitalmodel/subsea/mooring_analysis/models.py:L180-L189] [src/digitalmodel/subsea/mooring_analysis/designer.py:L239-L267]

### Catenary, offset and workflow paths

For submerged mass per length q, the local analyzer converts w=q*g/1000 to kN/m and sets a=H/w; it computes s=a*sinh(z/a), x=a*(cosh(z/a)−1), T=hypot(H,w*s), angle=atan2(w*s,H), and grounded length=max(0,L−s). These are the implemented equations, not an independent hand reference for a stated vertical height. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L65-L118]

By comparison, the canonical simplified solver documents y=a*(cosh(x/a)−1) and T=H+w*y; the general canonical solver accepts length, horizontal span and signed vertical span in metres with force quantities in N. Converting units alone does not reconcile the local height/coordinate formulation. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L9-L14] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L30-L39] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L78-L104]

The target-tension iteration uses an approximate derivative H/T, a positive lower clamp and an absolute tension residual; stiffness combines the coded geometric expression with EA/s in series and approximates vertical stiffness with tan²(angle). [src/digitalmodel/subsea/mooring_analysis/catenary.py:L148-L175] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L196-L238]

For an offset, `analyze_line_tension` translates the fairlead in surge/sway, computes horizontal separation and vertical span, evaluates the first segment at a pretension-based H, then applies at most one squared distance-ratio correction and a dynamic multiplier; it does not solve compatibility to a reported distance residual. [src/digitalmodel/subsea/mooring_analysis/designer.py:L145-L193]

The environmental screen uses a wave-height-squared drift approximation and speed-squared drag forces, adding all three scalar forces along the wave direction; period and separate wind/current directions do not enter those expressions. [src/digitalmodel/subsea/mooring_analysis/designer.py:L85-L124]

The `fpso-spread-mooring` route builds evenly spaced anchors/fairleads, calls the intact screen with its default zero offset, and writes loads beside the tension results; it does not feed the computed environmental force into an equilibrium solve. [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L49] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L63-L105] [src/digitalmodel/subsea/mooring_analysis/designer.py:L209-L237]

The legacy path dispatches among single-segment grounded, elevated full-catenary and three-segment algorithms, interpolates the load-distance tables, and rotates/translates fairleads before summing restoring forces; it must retain its own English units and yaw-in-radians API. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L689-L700] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L708-L744] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L913-L961]

The generator deduplicates line types by material plus integer diameter, sets fixed/vessel end connections and a user-specified statics profile selector, then serializes with `yaml.dump`; section 5 treats native loading and statics as separate gates. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L34-L44] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L121] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L175-L210]

`generate_model_yml` includes stage durations, whereas `generate_model_dict` contains the unit declaration without those durations; dictionary checks alone do not establish the serialized model's runtime settings. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L203] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L239-L247]

## 4. Validated worked examples

Offline result date: 2026-10-15; test sources and node IDs below are pinned to the header revision. Results refer to individual nodes, not to solver qualification. [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L641] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L136-L148] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L183-L193] [tests/subsea/mooring_analysis/test_dynmoor.py:L221-L233]

| ID / kind | Source and input | Expected value and tolerance basis | Existing test node ID, relative to repository | Last offline result |
| --- | --- | --- | --- | --- |
| V1 validated identity | Closed-form vector cancellation of opposing equal line forces in the synthetic symmetric `CASE_A_LINES`; no external dataset claimed | Zero surge/sway within 0.000001 lb and zero yaw within 0.0001 lb-ft; floating-point cancellation tolerance encoded in test | `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_symmetric_pattern_equilibrium` | PASS at BASE and AFTER. [tests/subsea/mooring_analysis/test_dynmoor.py:L82-L95] [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L641] |
| R1 regression | In-code chain fixture with prescribed horizontal tension; only identities and positivity checked | H=1000 kN exactly; top tension > H; touchdown=H; positive lengths; no independent top-tension value | `tests/subsea/mooring_analysis/test_mooring_analysis_unit.py::TestCatenaryAnalyzer::test_solve_catenary_basic` | PASS at BASE and AFTER. [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L124-L148] |
| R2 regression | Same material fixture; requested target tension | Top tension 2000 kN within 5 kN, with iteration request 1 kN; tolerance is the existing assertion, not an independent accuracy bound | `tests/subsea/mooring_analysis/test_mooring_analysis_unit.py::TestCatenaryAnalyzer::test_solve_for_target_tension` | PASS at BASE and AFTER. [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L183-L193] |
| R3 regression | Legacy static table snapshot; test docstring attributes values to a compiled predecessor, with no archived public benchmark revision (U4) | Frozen CASE_A table columns in ft and kips; absolute tolerance 0.011 + 0.00006*abs(expected), in each compared column's unit | `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatinbVsLegacyBinary::test_catenary_table` | PASS at BASE and AFTER; predecessor snapshot is not independent physical validation. [tests/subsea/mooring_analysis/test_dynmoor.py:L3-L38] [tests/subsea/mooring_analysis/test_dynmoor.py:L69-L71] [tests/subsea/mooring_analysis/test_dynmoor.py:L221-L233] |

No independent numerical top-tension benchmark is asserted by R1/R2; propose a common-case test of local geometry against the canonical simplified touchdown identities after unit conversion, keeping the known general-solver elongation assumption outside the reference. This is a proposed test, not an existing section-4 case (#1811, #2050). [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L136-L193] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L9-L14] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L294-L295]

## 5. QA gates

The following are proposed acceptance checks, not a claim that generated models have passed AQG; the family gate set is G0, G1, G2, G3, conditional G4 and G5. (AQG r0, sections 3–10.)

| Gate | Applicability and route |
| --- | --- |
| G0 | Applicable: input properties, references, units, profiles and factor selection; checks M01–M04. (AQG r0, section 4.) |
| G1 | Applicable to OrcaFlex YAML: open each `generate_model_yml` output in the exact target OrcaFlex version using the demo where supported, otherwise the licensed native load route; M05. No AQWA deck is emitted by this writer. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L165-L216] (AQG r0, section 5.) |
| G2 | Applicable: native convergence, static views, independent tension/angle/touchdown comparison, offset sweep and component inventory; M06–M10. (AQG r0, section 6.) |
| G3 | Applicable after native statics: short dynamic run of generated lines with recorded settings; M11. (AQG r0, section 7.) |
| G4 | NOT_APPLICABLE to this review: no new diffraction dataset is introduced; a later vessel-type import requires its diffraction record and M12. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L125-L146] (AQG r0, sections 8 and 10.) |
| G5 | Applicable to subsequent reports using these calculations: traceable evidence and signatures; M13. (AQG r0, section 9.) |

Proposed checks use pass criteria below; changing them requires an engineering disposition. Where no existing automated check implements the criterion, the evidence column defines the required script/table or native-program action. (AQG r0, sections 6 and 11.)

| ID / gate | Module-specific check | Numeric pass criterion and unit | Evidence and present result |
| --- | --- | --- | --- |
| M01 / G0 | Strict-parse `generate_model_yml`; inspect all `Lines[].LineType` references and the returned `VesselType` | 0 duplicate keys, 0 unresolved references, 0 disallowed null values (counts) | Schema and reference ledger; existing generation tests only check dictionary contents, native name table NOT_RUN; U3. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L175-L210] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L483-L519] |
| M02 / G0 | Reverse `generate_line_type` diameter, mass, EA and breaking-load conversions against input, then confirm target-program units; compare segment lengths and end coordinates | 0 unit-definition mismatches (count); coordinate residual <= 0.01 m; length residual <= 0.1 % | Per-property readback table; target-version readback NOT_RUN; U3. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L58-L85] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L121] |
| M03 / G0 | Change same-diameter material grade or stiffness between two segments; inspect deduplication and user-specified starting-profile data | 0 unequal property sets sharing one line-type name; 0 selected profiles without point data (counts) | Mutated two-segment model and reference/profile audit; structural hazard at source, issue pending (3). [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L34-L44] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L111-L121] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L175-L184] |
| M04 / G0 | For each condition, compare `min_mbl_required/max_tension` from `analyze_all_conditions` with the matching `SAFETY_FACTORS[condition]['dynamic']` and `DesignLoadCase` property; check quasi-static routes separately | 0 selector mismatches over 3 dynamic conditions (count); ratio residual <= 0.000000000001 (dimensionless) | Intact/damaged covered only partially by existing tests; transient mismatch follows directly from call routing, issue pending (1). [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L252] [src/digitalmodel/subsea/mooring_analysis/designer.py:L377-L396] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L382-L391] |
| M05 / G1 | Load every emitted YAML in the chosen OrcaFlex version; reconcile line, line-type and vessel counts | 0 errors and 0 warnings; 0 object-count differences (counts) | Native load record with version and messages; NOT_RUN: licensed program/interface prohibited here. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L203] (AQG r0, section 5.) |
| M06 / G2.1–G2.2 | Run statics for every generated `Lines` entry and retain plan/elevation views per distinct configuration | 0 unconverged models; 2 views/configuration (counts) | Native statics record and image hashes; NOT_RUN: solver required. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L121] (AQG r0, section 6.) |
| M07 / G2.3 | Compare `CatenaryAnalyzer.solve_catenary` with canonical touchdown geometry at matched H,w,height, then compare each native line's top tension, angle converted to the same reference, and touchdown span | Local/canonical tension and span <= 1.0 %; native/reference top tension <= 5.0 %, angle <= 1.0 degree, touchdown span <= 5.0 % | Common-case comparison and native table; local formula discrepancy established by source, #1811; native comparison NOT_RUN. Do not use the local path as the hand reference. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L78-L107] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L9-L14] (AQG r0, sections 6.3–6.4.) |
| M08 / G2.5 | Sweep each generated configuration from zero to design offset in far, near and cross directions; compare resolved native restoring force and line/touchdown trends with an independent low-load estimate | >= 6 offset steps/direction; 0 convergence failures, trend reversals, unexplained touchdown jumps or unplanned slack/compression (counts); first-step restoring-force error <= 20.0 % | Offset/tension/touchdown table and plots; NOT_RUN: native solver and design maximum required; `analyze_line_tension` is not an equilibrium sweep. [src/digitalmodel/subsea/mooring_analysis/designer.py:L145-L193] (AQG r0, section 6.5.) |
| M09 / G2.6 | Reconcile all generator model objects with static tension, clearance, position and capacity rows | 0 unchecked objects (count); 0 negative clearances (m); required/actual breaking-load ratio <= 1.0 (dimensionless) | Component ledger; NOT_RUN: native statics required. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L203] [src/digitalmodel/subsea/mooring_analysis/designer.py:L239-L267] (AQG r0, section 6.6.) |
| M10 / G2 | Run the synthetic symmetric `restoring_force` identity V1 and broken-line test | Residuals <= 0.000001 lb surge/sway and <= 0.0001 lb-ft yaw; broken-line H,T = 0 lb | Existing node results PASS; this identity does not establish absolute catenary accuracy. [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L655] |
| M11 / G3 | Start generated configuration from converged statics, preserving production dynamic settings; inspect every line tension | Reach >= 1.0 s; 0 errors, warnings, non-finite results or new compression/slack (counts); tension change <= 25.0 % | Native short-run log and final view; NOT_RUN: solver required. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L203] (AQG r0, section 7.) |
| M12 / G4 | For any new vessel diffraction data used by `generate_vessel_data`, attach the matched hydrostatic/mesh record | 0 missing diffraction records (count); weight/buoyancy residual <= 1.0 % | NOT_APPLICABLE for this documentation change; conditional external record. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L125-L146] (AQG r0, section 8.) |
| M13 / G5 | Tie workflow summary and every exported model to input digest, code revision, gate records and review signature | 0 results lacking matching fingerprint or disposition (count) | Proposed QA appendix index; NOT_RUN: no engineering report or native run delivered. [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L111-L126] (AQG r0, section 9.) |

## 6. Known failure modes and pitfalls
sweep: not run (legacy pack)

| Symptom / cause | Detection | Incident or open item; recorded 2026-10-15 |
| --- | --- | --- |
| A catenary exception returns an estimated finite load or infinity; no fallback marker enters the design-result details | M07 plus exception-path audit; require 0 unlabelled fallbacks before qualification | #1811; first-segment approximation, fallback and result details at source. [src/digitalmodel/subsea/mooring_analysis/designer.py:L195-L207] [src/digitalmodel/subsea/mooring_analysis/designer.py:L263-L267] |
| A transient case retains intact factor selection after reducing dynamic amplification | M04; current safety-factor test checks constants rather than dispatch | issue pending (1). [src/digitalmodel/subsea/mooring_analysis/designer.py:L390-L396] [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L227] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L382-L391] |
| Different same-diameter line properties collapse to one generated name; a profile selector is emitted without a starting-point table | M01–M03, then M05; no native qualification claimed | issue pending (3), #2426. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L34-L44] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L121] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L175-L184] |
| CLI model-generation command prints an unimplemented message and returns without generating a model | Check output file exists and parses: 1 expected file, 0 missing files | issue pending (4). [src/digitalmodel/subsea/mooring_analysis/cli.py:L327-L333] |
| CLI design mutates the material-library object, so later in-process users inherit its length | Compare original library length before/after: 0 m difference expected | issue pending (5). [src/digitalmodel/subsea/mooring_analysis/cli.py:L176-L179] [src/digitalmodel/subsea/mooring_analysis/models.py:L289-L302] |
| Local kN analyzer differs from canonical N-based boundary-value API; #1811 lists it as a distinct API to retain, not one of the five shadow variants proposed for deletion | M07; the local height formulas differ from canonical touchdown geometry, so this is a numerically divergent computation even though the issue does not classify it among the four shadow variants | #1811, read 2026-10-15; no interpretation that its distinct API proves physical correctness. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L65-L104] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L30-L39] |
| General canonical elongation is H*L/EA; a shared implementation or inconsistent fixture cannot serve as independent validation | Use closed-form geometry and force balance before choosing reference data; no local detection test exists for the reference-selection hazard | #2050, read 2026-10-15. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L295] [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L136-L170] |
| Generic spread/turret workflow needs native runs and an independent reference; the local workflow only screens intact zero-offset tensions | M05–M09 and M11; no existing end-to-end detection check establishes the complete issue scope | #2426, read 2026-10-15, acceptance and public-reference sections. [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L49] |
| Single-point worked example requires combined loading, hawser/anchor-leg tensions and hose curvature; these quantities are not produced by the local design-result dataclass | No detection check exists for the full worked example; require a case/result inventory with 0 missing required responses | #2432, read 2026-10-15, scope and acceptance sections. [src/digitalmodel/subsea/mooring_analysis/models.py:L192-L203] |
| Legacy out-of-table lookup clamps to the last row; time-domain tests are skipped because the dynamics port is unfinished | M10 plus bound check: 0 accepted excursions beyond populated table; inspect all four existing skipped nodes | issue pending (6). [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L708-L723] [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] |

## 7. Lessons log

| Date | Record | Lesson and pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-15 | Initial documentation | Separate programmed factor constants from dispatch behaviour; section 3 records transient routing | [src/digitalmodel/subsea/mooring_analysis/designer.py:L226-L227] [src/digitalmodel/subsea/mooring_analysis/designer.py:L390-L396] | Proposed entry; issue pending (1) |
| 2026-10-15 | Initial documentation | Separate predecessor snapshots from independent physical checks; section 4 classifies R3 as regression | [tests/subsea/mooring_analysis/test_dynmoor.py:L3-L38] | Proposed entry; issue pending (7) |

## 8. Ownership

Proposed review assignment: family quality manager for riser and mooring; deputy quality manager for installation; producing role: engineering documentation; escalation: the maintainer for unresolved references and the family quality manager for criteria or method changes. Clearance requires a different provider from the producer. (AQG r0, section 11.)

## 9. Dependencies and other implementations

Upstream calculation inputs are the local dataclasses and material library; direct numerical dependencies include NumPy and the generator's PyYAML serialization. The routed workflow consumes vessel/environment/mooring dictionaries and emits summary JSON. [src/digitalmodel/subsea/mooring_analysis/models.py:L35-L136] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L7-L15] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L205-L210] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L68]

The inventory identifies `solvers/orcaflex/mooring_analysis` separately; its package exports natural-period analysis, while its comprehensive stiffness route parses tabulated line stiffness and sums matrix terms. This is not an alias of `subsea/mooring_analysis`'s geometry API. [src/digitalmodel/solvers/orcaflex/mooring_analysis/__init__.py:L1-L25] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L32-L89] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L94-L125] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L43]

`orcaflex/mooring_design.py::solve_catenary` is another computation of line geometry/tension: it converts N/m to kN/m, uses the touchdown height relation and returns rounded results. Its `check_mbl` returns unscaled utilisation, whereas `check_mbl_with_safety_factor` applies condition selection and returns citation sidecars; these are separate routes from this module's designer. [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] [src/digitalmodel/orcaflex/mooring_design.py:L405-L419] [src/digitalmodel/orcaflex/mooring_design.py:L522-L564] [src/digitalmodel/subsea/mooring_analysis/designer.py:L239-L267]

The berth-line terminology page describes restraint roles and links to a different analysis namespace; the mooring overview links a line-setup wizard and suggests a static Python solution before native analysis, without defining an independent numerical case. [docs/domains/mooring/MOORING_LINE_TYPES.md:L10-L23] [docs/domains/mooring/mooring.md:L15-L26]

Inventory-selection metadata: `docs/registry/implementation-map.yaml` from `codex/implmap-work`, map source revision `947fa50ff9a9e210de7e8f90e01b37add85c5954`; all locations below are re-resolved against this pack's code revision, not that later inventory revision.

The following location ledger includes every mapped entry for the three families containing this module: catenary/line statics, text-model writing, and wind/current forces. Entries labelled caller, wrapper, example or test helper identify coverage context rather than establishing a second physical implementation; a mapped independent entry remains an inventory classification, not an accuracy qualification. Locations include repository scripts, examples, tests and source code.

### Catenary and mooring line statics

| Mapped symbol / classification | Location at code revision |
| --- | --- |
| `solve_static`; mapped independent | [docs/domains/mooring/screening/run_screening.py:L35-L48] |
| `compute_record`; mapped independent | [docs/domains/mooring/screening/run_screening.py:L98-L136] |
| `calc_catenary`; mapped example | [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L349-L418] |
| `solve_catenary_equations`; mapped example | [examples/domains/fpso/fpso_mooring_analysis.py:L285-L344] |
| `main`; mapped example | [examples/domains/fpso/fpso_mooring_analysis.py:L497-L562] |
| `main`; mapped example | [examples/domains/mooring/lazy_wave_example.py:L20-L124] |
| `analyze_mooring_line`; mapped example | [examples/domains/workflows/01_spread_mooring_8point.py:L180-L248] |
| `analyze_turret_at_heading`; mapped example | [examples/domains/workflows/02_turret_mooring.py:L76-L162] |
| `analyze_calm_buoy`; mapped example | [examples/domains/workflows/03_calm_buoy.py:L53-L113] |
| `demo_spline_pipe`; mapped independent | [scripts/examples/pyvista_3d_evaluation.py:L111-L147] |
| `general_arc`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L58-L75] |
| `scr_reference`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L78-L101] |
| `slwr_reference`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L373-L415] |
| `catenary`; mapped independent | [scripts/orcaflex/prepare_slay_campaign.py:L27-L51] |
| `catenaryEquation`; mapped independent | [scripts/python/digitalmodel/analysis/catenary_equation.py:L5-L78] |
| `catenary_curve`; mapped independent | [scripts/python/digitalmodel/analysis/catenary_solver.py:L8-L10] |
| `catenary_derivative`; mapped independent | [scripts/python/digitalmodel/analysis/catenary_solver.py:L12-L14] |
| `arc_length.integrand`; mapped independent | [scripts/python/digitalmodel/analysis/catenary_solver.py:L18-L19] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/analysis/catenary_solver.py:L102-L114] |
| `catenary`; mapped independent | [scripts/python/digitalmodel/analysis/simple_catenary_equation.py:L6-L17] |
| `catenary`; mapped independent | [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L7-L20] |
| `solve_a`; mapped independent | [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L22-L34] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L40-L52] |
| `PerformanceProfiler.profile_catenary_solver`; mapped independent | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L263-L332] |
| `demonstrate_usage`; mapped independent | [scripts/python/digitalmodel/legacy/validate_catenary_solver.py:L118-L162] |
| `calculate_catenary_anchor_position`; mapped independent | [scripts/update_baltic_moorings_10m_catenary.py:L60-L134] |
| `_catenary_riser_polyline`; mapped independent | [src/digitalmodel/field_development/layout_model.py:L443-L490] |
| `OrcaflexModelComponents.spring_properties`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L619-L675] |
| Module-level entry; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L35-L47] |
| `catenaryEquation`; mapped caller | [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L50-L115] |
| `_angle_based_method`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L197-L259] |
| `catenaryForces`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L262-L337] |
| `LazyWaveSolver._solve_hangoff_section`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L454-L503] |
| `LazyWaveSolver._solve_sag_hog_sections`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L505-L618] |
| `SimplifiedCatenarySolver.solve_from_angle`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L71-L147] |
| `SimplifiedCatenarySolver.solve_from_force`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L149-L229] |
| `solve_catenary_dict`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L299-L363] |
| `calculate_forces_dict`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L366-L393] |
| `CatenarySolver.solve`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L77-L118] |
| `CatenarySolver._geometry_for_a`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L203-L219] |
| `CatenarySolver._solve_simplified.length_error`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L234-L256] |
| `CatenarySolver._compute_results`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L275-L332] |
| `safe_sinh`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L12-L48] |
| `safe_cosh`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L51-L83] |
| `calculate_elongation`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L192-L217] |
| `touchdown_point`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L220-L249] |
| `PerformanceProfiler.profile_catenary_solver`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L256-L326] |
| Module-level entry; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_catenary.py:L12-L24] |
| `demonstrate_usage`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_catenary.py:L123-L167] |
| Module-level entry; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py:L40-L52] |
| `FPSOMooringFullWorkflow._baseline_span`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L205-L233] |
| `FPSOMooringFullWorkflow._line_tensions`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L289-L296] |
| `solve_catenary`; mapped independent | [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] |
| `MooringLineDesign.estimate_catenary`; mapped independent | [src/digitalmodel/orcaflex/mooring_design.py:L385-L403] |
| `JLayConfig.calculate_sagbend`; mapped independent | [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] |
| `SCRDesignInput.calculate_geometry`; mapped independent | [src/digitalmodel/orcaflex/riser_config.py:L170-L229] |
| `_lazy`; mapped independent | [src/digitalmodel/riser_configuration_screen/static.py:L48-L63] |
| `_lazy_shape`; mapped independent | [src/digitalmodel/riser_configuration_screen/static.py:L72-L89] |
| `scr_offsets`; mapped independent | [src/digitalmodel/riser_configuration_screen/static.py:L166-L220] |
| `scr_offsets.residual`; mapped independent | [src/digitalmodel/riser_configuration_screen/static.py:L193-L195] |
| `OrcaflexModelComponents.spring_properties`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L917-L1046] |
| `LazyWaveAnalyzer.analyze`; mapped independent | [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L34-L171] |
| `catenaryEquation`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L12-L68] |
| `catenaryForces`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L71-L95] |
| `sagHogEquation`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L98-L158] |
| `lazyWaveCatenaryEquation`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L161-L199] |
| `gethorizontalDistanceHangofftoTDPrange`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L434-L508] |
| `CatenaryCalculator._calculate_from_angle`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L81] |
| `catenary_riser`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L32-L167] |
| `orcaflexModel`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L357] |
| `lineTypes`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L555-L629] |
| `SimpleCatenaryAnalyzer.analyze_riser`; mapped caller | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L42-L101] |
| `SimpleCatenaryAnalyzer._solve_catenary_from_H`; mapped independent | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L103-L172] |
| `SimpleCatenaryAnalyzer._solve_H_from_top_tension.calc_top_tension`; mapped independent | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L200-L219] |
| `SimpleCatenaryAnalyzer._solve_H_from_geometry`; mapped independent | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L279-L342] |
| `CatenaryAnalyzer.solve_catenary`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L119] |
| `CatenaryAnalyzer.solve_for_horizontal_tension`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/catenary.py:L121-L176] |
| `CatenaryAnalyzer.calculate_stiffness`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/catenary.py:L178-L238] |
| `CatenaryAnalyzer.check_touchdown`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/catenary.py:L240-L298] |
| `CatenaryAnalyzer.check_touchdown.s_from_a`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/catenary.py:L264-L268] |
| `catenary_analysis`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/cli.py:L50-L142] |
| `MooringDesigner.analyze_line_tension`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/designer.py:L126-L207] |
| `_catinb`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L339-L414] |
| `_stretc`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L417-L434] |
| `_fulcat`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L437-L522] |
| `_catnry`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L525-L657] |
| `_sub`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L660-L686] |
| `build_line_table`; mapped caller | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L689-L700] |
| `interp_table`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L708-L744] |
| `anchor_pull`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L747-L784] |
| `solve_pretension`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L833-L884] |
| `restoring_force`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L913-L961] |
| `_geometry`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L41-L56] |
| `_derive.primitive`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L84-L88] |
| `test_elastic_fixture_geometry_and_force_identities`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L144-L172] |
| `test_inextensible_chord_arc_and_end_forces`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L175-L193] |
| `test_superseded_answers_fail_geometry_and_touchdown_diagnosis`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L196-L215] |
| `test_solver_against_reconciled_fixture`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L226-L247] |
| Module-level entry; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_run_quick.py:L8-L20] |
| `expected_from_angle`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L22-L37] |
| `expected_from_force`; mapped test helper | [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L40-L52] |
| `CatenarySolver.solve.length_error`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L97-L108] |
| `CatenarySolver.solve.length_error_derivative`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L111-L124] |
| `CatenarySolver._compute_results`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L165-L218] |
| `CatenarySolver._calculate_touchdown`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L220-L238] |
| `_signed_anchor_coordinate`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L248-L255] |
| `TestCatenarySolver.test_shape_coordinates`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L378-L403] |
| `TestCatenaryPerformance.test_batch_processing`; mapped test helper | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L526-L552] |
| `test_angle_method`; mapped test helper | [tests/marine_ops/marine_engineering/test_adapter_manual.py:L69-L112] |
| `test_forces`; mapped test helper | [tests/marine_ops/marine_engineering/test_adapter_manual.py:L115-L152] |
| `TestCatenaryEquationAngleMethod.test_angle_method_exact_legacy_match`; mapped test helper | [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L182-L211] |
| `TestCatenaryForces.test_basic_forces_calculation`; mapped test helper | [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L300-L328] |
| `TestCatenaryForces.test_forces_exact_legacy_match`; mapped test helper | [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L330-L355] |
| `TestCatenaryForces.test_forces_various_scenarios`; mapped test helper | [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L357-L378] |
| `CatenarySolver.solve.length_error`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L97-L108] |
| `CatenarySolver.solve.length_error_derivative`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L111-L124] |
| `CatenarySolver._compute_results`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L165-L218] |
| `CatenarySolver._calculate_touchdown`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L220-L238] |
| `_signed_anchor_coordinate`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L248-L255] |
| `TestCatenarySolver.test_shape_coordinates`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L378-L403] |
| `TestCatenaryPerformance.test_batch_processing`; mapped test helper | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L526-L552] |
| `test_scr_matches_independent_inextensible_catenary`; mapped test helper | [tests/riser_configuration_screen/test_static.py:L58-L68] |
| `catenary_system`; mapped test helper | [tests/solve_catenary_correct.py:L12-L30] |
| `catenary_system`; mapped test helper | [tests/solve_general_catenary.py:L39-L77] |
| `test_marine_engineering_imports`; mapped test helper | [tests/structural/analysis/validate_imports.py:L139-L166] |
| `lazy_results`; mapped test helper | [tests/subsea/catenary_riser/test_static_reconciliation.py:L71-L91] |
| `test_scr_design_and_general_solver_in_inextensible_limit`; mapped test helper | [tests/subsea/catenary_riser/test_static_reconciliation.py:L119-L137] |
| `TestLibraryVsOracle.test_catinb_sloped_bottom_matches_oracle`; mapped test helper | [tests/subsea/mooring_analysis/test_dynmoor.py:L552-L572] |
| `test_excel_validation_case`; mapped test helper | [tests/test_catenary_module.py:L26-L69] |
| `TestLazyWaveSolver.test_hangoff_section_calculation`; mapped test helper | [tests/test_lazy_wave_catenary.py:L43-L68] |
| `TestLazyWaveSolver.test_sag_to_buoyancy_segment`; mapped test helper | [tests/test_lazy_wave_catenary.py:L70-L94] |
| `TestLazyWaveSolver.test_buoyancy_to_hog_segment`; mapped test helper | [tests/test_lazy_wave_catenary.py:L96-L121] |
| `TestLazyWaveSolver.test_hog_to_buoyancy_segment`; mapped test helper | [tests/test_lazy_wave_catenary.py:L123-L145] |
| `TestLazyWaveSolver.test_buoyancy_to_touchdown_segment`; mapped test helper | [tests/test_lazy_wave_catenary.py:L147-L167] |
| `test_hangoff_vertical_force_balances_suspended_weight`; mapped test helper | [tests/test_lazy_wave_force_equilibrium.py:L67-L72] |
| `test_hangoff_total_tension_matches_catenary_and_force_triangle`; mapped test helper | [tests/test_lazy_wave_force_equilibrium.py:L76-L87] |
| `solver`; mapped test helper | [tests/test_lazy_wave_kernel_defects.py:L64-L65] |

### OrcaFlex text-model writers

| Mapped symbol / classification | Location at code revision |
| --- | --- |
| `main`; mapped independent | [docs/domains/examples/orcaflex/run_universal_example.py:L218-L264] |
| Module-level entry; mapped independent | [docs/domains/orcaflex/examples/gom_scr_design_study/generate_configs.py:L10-L22] |
| `main`; mapped independent | [docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py:L47-L113] |
| `main`; mapped independent | [docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py:L48-L116] |
| `strengthInputFilePreperation`; mapped independent | [docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py:L2-L20] |
| `run_orcawave_diffraction`; mapped independent | [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py:L29-L187] |
| `run_diffraction_with_monitoring`; mapped independent | [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py:L166-L273] |
| `run_batch_execution`; mapped independent | [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py:L276-L372] |
| `_load_data_compat`; mapped independent | [docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py:L37-L52] |
| `_load_data_compat`; mapped copy | [docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py:L39-L54] |
| `_load_data_compat`; mapped copy | [docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py:L40-L55] |
| `_load_data_compat`; mapped copy | [docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py:L40-L55] |
| `OrcaFlexExporter.create_mooring_yml`; mapped example | [examples/domains/workflows/utils.py:L256-L295] |
| `write_audit_yaml`; mapped independent | [scripts/audit_spec_library.py:L241-L246] |
| `generate_yml`; mapped independent | [scripts/benchmark/generate_owd_ymls.py:L48-L66] |
| `main`; mapped independent | [scripts/benchmark/run_spar_benchmark.py:L155-L341] |
| `solve_and_extract`; mapped independent | [scripts/benchmark/solve_owd_extract.py:L116-L302] |
| `solve_owd`; mapped independent | [scripts/benchmark/validate_owd_vs_spec.py:L426-L543] |
| `solve_spec`; mapped independent | [scripts/benchmark/validate_owd_vs_spec.py:L546-L657] |
| `_solve_spec_via_orcfxapi`; mapped independent | [scripts/benchmark/validate_owd_vs_spec.py:L660-L761] |
| Module-level entry; mapped independent | [scripts/benchmark_model_library.py:L26-L38] |
| `load_and_run`; mapped independent | [scripts/benchmark_model_library.py:L330-L341] |
| `run_spec_driven`; mapped independent | [scripts/benchmark_model_library.py:L346-L386] |
| `load_json`; mapped independent | [scripts/benchmark_model_library.py:L551-L574] |
| `_trace`; mapped independent | [scripts/benchmark_model_library.py:L645-L648] |
| `apply_mesh_preset`; mapped independent | [scripts/benchmark_riser_library.py:L358-L377] |
| `run_mesh_sensitivity`; mapped independent | [scripts/benchmark_riser_library.py:L430-L469] |
| `benchmark_model`; mapped independent | [scripts/benchmark_riser_library.py:L524-L605] |
| `make_chart_js`; mapped independent | [scripts/build_sme_report.py:L70-L102] |
| `make_profile_js`; mapped independent | [scripts/build_sme_report.py:L105-L153] |
| `extract_references`; mapped independent | [scripts/ci/check_include_targets.py:L36-L65] |
| `_build_manual_entries`; mapped independent | [scripts/consolidate_panels.py:L182-L224] |
| `OrcaFlexConverter.convert_file`; mapped independent | [scripts/convert_orcaflex_to_yml.py:L122-L191] |
| `create_flattened_model_dir`; mapped independent | [scripts/create_flattened_models.py:L19-L161] |
| `process_model_dir`; mapped independent | [scripts/extract_library_validation.py:L64-L134] |
| `_field_to_section_name`; mapped independent | [scripts/extract_property_inventory.py:L145-L176] |
| `write_yaml`; mapped independent | [scripts/extract_property_inventory.py:L273-L282] |
| `main`; mapped independent | [scripts/extract_riser_validation.py:L56-L107] |
| `save_spec`; mapped independent | [scripts/extract_s7_specs.py:L164-L178] |
| `create_env_section_wrappers`; mapped independent | [scripts/fix_orcaflex_structure.py:L20-L37] |
| `create_fixed_analysis_model`; mapped independent | [scripts/fix_orcaflex_structure.py:L39-L75] |
| `fix_sections_for_discretised`; mapped independent | [scripts/fix_orcaflex_structure.py:L90-L112] |
| Module-level entry; mapped independent | [scripts/generate_all_specs.py:L39-L51] |
| `extract_spec`; mapped independent | [scripts/generate_all_specs.py:L230-L262] |
| `save_spec_yml`; mapped independent | [scripts/generate_all_specs.py:L265-L281] |
| `create_analysis_file`; mapped independent | [scripts/generate_analysis_files.py:L21-L120] |
| `CALMBuoyProjectGenerator.generate_orcaflex_modules`; mapped independent | [scripts/generate_calm_buoy_project.py:L310-L386] |
| `CALMBuoyProjectGenerator._populate_environment_module`; mapped independent | [scripts/generate_calm_buoy_project.py:L388-L462] |
| `CALMBuoyProjectGenerator._apply_module_customizations`; mapped independent | [scripts/generate_calm_buoy_project.py:L464-L511] |
| `CALMBuoyProjectGenerator._update_base_file_paths`; mapped independent | [scripts/generate_calm_buoy_project.py:L691-L705] |
| `CALMBuoyProjectGenerator.save_updated_config`; mapped independent | [scripts/generate_calm_buoy_project.py:L766-L775] |
| `create_wave_file`; mapped independent | [scripts/generate_env_files.py:L38-L77] |
| `create_current_file`; mapped independent | [scripts/generate_env_files.py:L79-L106] |
| `create_wind_file`; mapped independent | [scripts/generate_env_files.py:L108-L140] |
| `create_version_metadata_block`; mapped independent | [scripts/initialize_skill_metadata.py:L78-L103] |
| `_report_type`; mapped independent | [scripts/inventory/orcaflex_legacy_inventory.py:L28-L42] |
| `classify`; mapped independent | [scripts/inventory/orcaflex_legacy_inventory.py:L118-L143] |
| `main`; mapped independent | [scripts/inventory/orcaflex_legacy_inventory.py:L170-L319] |
| Module-level entry; mapped independent | [scripts/maintenance/archive_candidates.py:L32-L44] |
| `main`; mapped independent | [scripts/maintenance/archive_candidates.py:L663-L999] |
| `apply_mesh_preset`; mapped independent | [scripts/mesh_sensitivity_riser.py:L101-L126] |
| `scr_reference`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L78-L101] |
| `slwr_reference`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L373-L415] |
| `calculate`; mapped independent | [scripts/orcaflex/check_generic_scr_slwr.py:L418-L462] |
| `case_spec`; mapped independent | [scripts/orcaflex/prepare_slay_campaign.py:L69-L79] |
| `prepare_case`; mapped independent | [scripts/orcaflex/prepare_slay_campaign.py:L90-L113] |
| `prepare`; mapped independent | [scripts/orcaflex/prepare_slay_campaign.py:L116-L137] |
| `_generated_sections`; mapped independent | [scripts/prepare_mooring_qualification.py:L37-L51] |
| `ContentAnalyzer._categorize_folder`; mapped independent | [scripts/python/digitalmodel/doc_tools/check_structure_creation.py:L309-L323] |
| `FrontmatterGenerator.frontmatter_to_yaml`; mapped independent | [scripts/python/digitalmodel/doc_tools/frontmatter_generator.py:L380-L387] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py:L4-L16] |
| `make_comparison_loaded_vs_ballast_cxc`; mapped independent | [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L315-L349] |
| `load_config`; mapped independent | [scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py:L12-L19] |
| `generate_optimized_config`; mapped independent | [scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py:L199-L231] |
| `create_config_with_gdf`; mapped independent | [scripts/python/digitalmodel/orcawave/create_gdf_consistent.py:L345-L402] |
| `create_simple_vessel_config`; mapped independent | [scripts/python/digitalmodel/orcawave/create_working_examples.py:L15-L142] |
| `create_oc4_semisub_config`; mapped independent | [scripts/python/digitalmodel/orcawave/create_working_examples.py:L144-L270] |
| `create_minimal_test_config`; mapped independent | [scripts/python/digitalmodel/orcawave/create_working_examples.py:L272-L336] |
| `create_final_working_config`; mapped independent | [scripts/python/digitalmodel/orcawave/final_working_config.py:L18-L165] |
| `create_extended_version`; mapped independent | [scripts/python/digitalmodel/orcawave/final_working_config.py:L167-L184] |
| `create_tug_30m_1_working`; mapped independent | [scripts/python/digitalmodel/orcawave/fix_orcawave_yaml.py:L60-L128] |
| `fix_configuration`; mapped independent | [scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py:L14-L135] |
| `create_minimal_test_config`; mapped independent | [scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py:L137-L178] |
| `fix_working_model`; mapped independent | [scripts/python/digitalmodel/orcawave/fix_working_model.py:L11-L93] |
| `create_minimal_with_test_box`; mapped independent | [scripts/python/digitalmodel/orcawave/fix_working_model.py:L143-L189] |
| `OrcaWaveValidator.run_validation`; mapped independent | [scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py:L189-L285] |
| `create_batch_config`; mapped independent | [scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py:L14-L53] |
| `OrcaFlexTestStandardizer.create_standard_config`; mapped independent | [scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py:L203-L239] |
| `run_library`; mapped independent | [scripts/run_riser_analysis.py:L366-L410] |
| `main`; mapped independent | [scripts/run_semisub_quality_check.py:L14-L97] |
| Module-level entry; mapped independent | [scripts/sanitize_s7_models.py:L89-L101] |
| `process_dat_file`; mapped independent | [scripts/sanitize_s7_models.py:L550-L606] |
| Module-level entry; mapped independent | [scripts/semantic_validate.py:L53-L65] |
| `convert_dat_to_yml`; mapped independent | [scripts/sync_monolithic_yml.py:L72-L85] |
| `update_mooring_lines`; mapped independent | [scripts/update_baltic_moorings_10m.py:L128-L393] |
| `update_mooring_lines`; mapped independent | [scripts/update_baltic_moorings_10m_catenary.py:L207-L483] |
| `update_skill_file`; mapped independent | [scripts/update_skill_versions.py:L147-L236] |
| `ModelValidator.extract_model_info`; mapped independent | [scripts/validate_orcaflex_model.py:L181-L212] |
| `_save_body`; mapped independent | [scripts/verification/verify_orcaflex_campaign.py:L131-L143] |
| `main`; mapped independent | [scripts/verification/verify_orcaflex_campaign.py:L203-L336] |
| `SaveData.saveDataYaml`; mapped independent | [src/digitalmodel/asset_integrity/common/data.py:L417-L433] |
| `SaveData.WriteOrcaFlexYMLFile`; mapped independent | [src/digitalmodel/asset_integrity/common/data.py:L614-L624] |
| Module-level entry; mapped independent | [src/digitalmodel/asset_integrity/common/data.py:L910-L922] |
| `StreamHandler._format_orcaflex_yml`; mapped independent | [src/digitalmodel/data_systems/data_procurement/common/stream_handler.py:L367-L371] |
| `MooringClient.to_orcaflex_mooring`; mapped independent | [src/digitalmodel/data_systems/data_procurement/mooring/client.py:L515-L585] |
| `ChainDatabaseClient.to_orcaflex_line_type`; mapped independent | [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py:L354-L405] |
| `SyntheticRopeDatabaseClient.to_orcaflex_line_type`; mapped independent | [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py:L416-L465] |
| `WireRopeDatabaseClient.to_orcaflex_line_type`; mapped independent | [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py:L339-L377] |
| `PipeSpecificationClient.to_orcaflex_line_type`; mapped independent | [src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py:L440-L500] |
| `VesselClient.to_orcaflex_vessel`; mapped independent | [src/digitalmodel/data_systems/data_procurement/vessel/client.py:L351-L426] |
| `case_spec`; mapped independent | [src/digitalmodel/drilling_riser/campaign.py:L134-L239] |
| `_probe`; mapped independent | [src/digitalmodel/drilling_riser/campaign.py:L313-L325] |
| `_capped_probe`; mapped caller | [src/digitalmodel/drilling_riser/campaign.py:L328-L336] |
| `_statics_path`; mapped independent | [src/digitalmodel/drilling_riser/campaign.py:L400-L565] |
| `physical_state_checks`; mapped independent | [src/digitalmodel/drilling_riser/campaign.py:L758-L810] |
| `_line_type`; mapped independent | [src/digitalmodel/drilling_riser/global_model/build.py:L112-L143] |
| `build_generic_spec`; mapped independent | [src/digitalmodel/drilling_riser/global_model/build.py:L180-L369] |
| `write_model`; mapped caller | [src/digitalmodel/drilling_riser/global_model/build.py:L475-L492] |
| `reference_periods`; mapped independent | [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L216-L232] |
| `build_open_water_generic_spec`; mapped independent | [src/digitalmodel/drilling_riser/global_model/open_water.py:L143-L210] |
| `extract`; mapped independent | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L71-L90] |
| `merge_stress_range`; mapped independent | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L93-L119] |
| `extract_significant`; mapped independent | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392] |
| `prepare_variant`; mapped independent | [src/digitalmodel/floating_wind/solver_tier.py:L292-L325] |
| `OrcaFlexConverter.convert_to_vessel_yaml`; mapped independent | [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L146-L170] |
| `_add_fdf_surface_traces`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py:L274-L317] |
| `run_aqwa`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py:L170-L230] |
| `DiffractionSpec.to_yaml`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1046-L1054] |
| `OrcaFlexExporter.export_vessel_type`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L71-L103] |
| `_write_yml`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:L701-L718] |
| `OrcaWaveRunner._execute_via_api`; mapped independent | [src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:L500-L567] |
| `PanelCatalog.to_yaml`; mapped independent | [src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py:L114-L127] |
| `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file`; mapped independent | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] |
| `RAOAnalysis.get_shape_data`; mapped independent | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579] |
| `FatigueAnalysisComponents.get_viv_modes_excited`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L27-L76] |
| `FatigueAnalysisComponents.tabulate_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L114-L167] |
| `FatigueAnalysisComponents.tabulate_overall_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L169-L204] |
| `FatigueAnalysisComponents.save_viv_histograms`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L364-L371] |
| `FatigueAnalysisComponents.save_combined_histograms`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L467-L475] |
| `FatigueAnalysisComponents.save_fatigue_summary_result`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L493-L519] |
| `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L586-L701] |
| `DNVRPH103_hydrodynamics_circular.save_model`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L56-L69] |
| `DNVRPH103_hydrodynamics_circular.update_6d_buoy`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L71-L94] |
| `DNVRPH103_hydrodynamics_rectangular.save_model`; mapped copy | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L54-L67] |
| `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L69-L91] |
| `OrcaflexModelComponents.orcaflex_model`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L11-L59] |
| `OrcaflexModelComponents.riser_joints_to_line_types`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L64-L136] |
| `OrcaflexModelComponents.GetGeneral`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L138-L167] |
| `OrcaflexModelComponents.lineTypes`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L379-L441] |
| `OrcaflexModelComponents.WriteOrcaflexModel`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L800] |
| `OrcaflexModelComponents.build_model`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L802-L857] |
| `FEAComponents.AssignGeneral`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L51-L54] |
| `FEAComponents.AssignBuoyProperties`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L81-L85] |
| `FEAComponents.AssignLineProperties`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L87-L91] |
| `FEAComponents.AssignLineTypeProperties`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L93-L97] |
| `FEAComponents.AssignVesselProperties`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L99-L103] |
| `FEAComponents.save_output`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211] |
| `FEAComponents.load_variations`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L217-L251] |
| `VIVFatigueAnalysisComponents.get_viv_modes_excited`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L40-L142] |
| `VIVFatigueAnalysisComponents.tabulate_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L200-L317] |
| `VIVFatigueAnalysisComponents.tabulate_overall_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L319-L366] |
| `VIVFatigueAnalysisComponents.save_viv_histograms`; mapped copy | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L618-L629] |
| `VIVFatigueAnalysisComponents.save_combined_histograms`; mapped copy | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L766-L782] |
| `VIVFatigueAnalysisComponents.save_fatigue_summary_result`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L799-L864] |
| `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L982-L1164] |
| `SaveData.saveDataYaml`; mapped copy | [src/digitalmodel/infrastructure/utils/data.py:L416-L432] |
| `SaveData.WriteOrcaFlexYMLFile`; mapped copy | [src/digitalmodel/infrastructure/utils/data.py:L613-L623] |
| Module-level entry; mapped independent | [src/digitalmodel/infrastructure/utils/data.py:L926-L938] |
| `generate_input`; mapped independent | [src/digitalmodel/marine_ops/installation/jumper_input.py:L69-L89] |
| `JumperInstallationSpec.to_yaml`; mapped independent | [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L279-L294] |
| `build_line_types`; mapped independent | [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L91-L166] |
| `build_modular_spec`; mapped independent | [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L209-L353] |
| `UnifiedRAOReader._detect_format`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L252-L282] |
| `VesselConfig.to_orcaflex_dict`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L113-L125] |
| `Buoy6DConfig.to_orcaflex_dict`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L147-L158] |
| `LineConfig.to_orcaflex_dict`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L183-L207] |
| `build_scr_model`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L259-L312] |
| `build_lazy_wave_model`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L315-L367] |
| `build_mooring_line`; mapped independent | [src/digitalmodel/orcaflex/model_builder.py:L370-L422] |
| `build_model_summary`; mapped independent | [src/digitalmodel/orcaflex/reporting/sections/model_summary.py:L20-L48] |
| `build_range_graphs`; mapped independent | [src/digitalmodel/orcaflex/reporting/sections/range_graphs.py:L6-L61] |
| `build_rao_plots`; mapped independent | [src/digitalmodel/orcawave/reporting/sections/rao_plots.py:L23-L129] |
| `_write_result`; mapped independent | [src/digitalmodel/parametric/query.py:L339-L356] |
| `orcawave_depth_sweep.runner`; mapped independent | [src/digitalmodel/parametrics/pilots.py:L100-L120] |
| `_write_diffraction_inputs`; mapped independent | [src/digitalmodel/solvers/benchmark/solvers.py:L118-L129] |
| `LineType.get_general`; mapped independent | [src/digitalmodel/solvers/fea_model/LineType_components.py:L43-L56] |
| `General.getGeneralOrcaFlexSetUp`; mapped independent | [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] |
| `load_csv`; mapped independent | [src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L184-L288] |
| `get_data`; mapped independent | [src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L291-L555] |
| `OrcaFlexConfig.to_yaml`; mapped independent | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L407-L423] |
| `OrcaFlexModelWrapper.save_simulation`; mapped independent | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L509-L549] |
| `FeatureAnalyzer.analyze_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L76-L134] |
| `FeatureAnalyzer.generate_summary`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L169-L195] |
| `OrcaFlexBatchConverter._real_convert`; mapped wrapper | [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L163-L191] |
| `OrcaFlexBatchConverter._mock_convert`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L231] |
| `YamlConverter.convert_batch`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L36-L65] |
| `DownloadedExamplesIntegrator._infer_components`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L108-L168] |
| `DownloadedExamplesIntegrator._extract_common_configs`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L356-L385] |
| `KnowledgeIntegrator._extract_patterns`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L102] |
| `KnowledgeIntegrator.build_searchable_index`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L223-L261] |
| `OrcFxAPIConverter._initialize_api`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L55-L95] |
| `OrcFxAPIConverter.convert_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L105-L165] |
| `detect_format`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L30-L91] |
| `ModularToSingleConverter.convert`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L29-L90] |
| `ModularToSingleConverter._merge_master`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L165] |
| `ModularToSingleConverter._extract_includefiles.traverse`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L191-L200] |
| `ModularToSpecConverter.convert`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L96-L222] |
| `SingleToModularConverter._write_master_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L230-L260] |
| `SingleToModularConverter._write_parameters_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L262-L277] |
| `_resolve_mooring`; mapped independent | [src/digitalmodel/solvers/orcaflex/inverse_resolver.py:L342-L472] |
| `_resolve_riser`; mapped independent | [src/digitalmodel/solvers/orcaflex/inverse_resolver.py:L475-L653] |
| `LibraryGenerator.generate_component`; mapped independent | [src/digitalmodel/solvers/orcaflex/library_generator.py:L234-L283] |
| `LibraryGenerator.write_index`; mapped independent | [src/digitalmodel/solvers/orcaflex/library_generator.py:L379-L401] |
| `object_counts`; mapped independent | [src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L120-L138] |
| `geometry`; mapped independent | [src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L190-L215] |
| `OrcaFlexModelGenerator.generate_from_template`; mapped independent | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L206-L266] |
| `OrcaFlexModelGenerator._assemble_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L268-L335] |
| `ModularModelGenerator.generate`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201] |
| `ModularModelGenerator._write_parameters`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L228-L246] |
| `ModularModelGenerator._write_master`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L248-L262] |
| `ModularModelGenerator.generate_with_overrides`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L264-L373] |
| `GeneralBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L23-L81] |
| `GenericModelBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:L201-L281] |
| `LinesBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L46-L83] |
| `LineTypeBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L85-L158] |
| `RiserLinesBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py:L40-L59] |
| `RiserLineTypeBuilder.build`; mapped caller | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py:L34-L54] |
| `RiserLineTypeBuilder._build_riser_line_type`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py:L56-L137] |
| `RiserVesselBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py:L32-L61] |
| `VesselBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L37-L75] |
| `WinchBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L40-L85] |
| `cmd_generate`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/cli.py:L170-L307] |
| `MonolithicExtractor._extract_simulation`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L368-L385] |
| `MonolithicExtractor._extract_general_properties`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L568-L584] |
| `PostGenerationValidator._check_line_type_references`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L281-L338] |
| `PostGenerationValidator._check_vessel_references`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L340-L412] |
| `PostGenerationValidator._check_winch_references`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L414-L480] |
| `_iter_includefile_refs.traverse`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L747-L756] |
| `MooringRouter.route`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L105-L143] |
| `MooringRouter._chain_linetype`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L161-L199] |
| `MooringRouter._wire_linetype`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L201-L234] |
| `MooringRouter._polyester_linetype`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L236-L268] |
| `VesselRouter.route`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py:L28-L54] |
| `VesselRouter.from_hull_catalog`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py:L57-L112] |
| `CampaignGenerator.generate`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:L539-L662] |
| `write_variation_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py:L194-L222] |
| `extract_includefiles.traverse`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L45-L54] |
| `ModularInputValidator._collect_issues_from_level_1`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L219-L247] |
| `Mooring.prepare_includefile_for_all_lines`; mapped independent | [src/digitalmodel/solvers/orcaflex/mooring.py:L509-L571] |
| `Mooring.prepare_includefile_for_fender_compression`; mapped independent | [src/digitalmodel/solvers/orcaflex/mooring.py:L573-L622] |
| `MockOrcaFlexAPI.load_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L120-L136] |
| `OrcaFlexAPI.save_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L255-L261] |
| `OperabilityAnalyzer.generate_operability_envelope`; mapped independent | [src/digitalmodel/solvers/orcaflex/operability_analysis.py:L83-L198] |
| `OPPVisualization.combine_models`; mapped independent | [src/digitalmodel/solvers/orcaflex/opp_visualization.py:L128-L137] |
| `OrcaFlexAnalysis.save_data`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L107-L125] |
| `OrcaFlexAnalysis.saveSummaryToExcel`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L285-L294] |
| `OrcaFlexAnalysis.save_histograms`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L819-L827] |
| `OrcaFlexAnalysis.save_cfg_files_from_multiple_files`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L853-L860] |
| `OrcaFlexAnalysis.save_RAOs`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L963-L977] |
| `OrcaFlexConverterEnhanced._real_convert`; mapped wrapper | [src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:L234-L263] |
| `OrcaFlexConverterEnhanced._mock_convert`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:L265-L299] |
| `OrcaFlexCustomAnalysis.process_fea`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L83-L196] |
| `OrcaFlexCustomAnalysis.clean_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L198-L215] |
| `OrcaFlexCustomAnalysis.save_model_with_calculated_positions`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L244-L262] |
| `OrcaFlexCustomAnalysis.iterate_to_target_value`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L264-L325] |
| `OrcaFlexCustomAnalysis.save_data`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L342-L379] |
| `OrcInstallation.create_model_for_water_depth`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L29-L103] |
| `OrcInstallation.create_installation_depth_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L159-L197] |
| `OrcInstallation.create_installation_depth_model2`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L199-L240] |
| `OrcaflexIterativeRuns.save_iterative_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L64-L72] |
| `OrcaflexIterativeRuns.run_iterative_simulations`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L152-L173] |
| `OrcaflexLineTypes.get_umbilical_properties`; mapped wrapper | [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L25-L33] |
| `OrcaflexLineTypes.assign_key_umbilical_properties`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L35-L60] |
| `OrcaflexLineTypes.save_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L62-L72] |
| `OrcaflexModelComponents.orcaflex_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L14-L81] |
| `OrcaflexModelComponents.riser_joints_to_line_types`; mapped copy | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L86-L177] |
| `OrcaflexModelComponents.GetGeneral`; mapped copy | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L179-L208] |
| `OrcaflexModelComponents.lineTypes`; mapped copy | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L501-L591] |
| `OrcaflexModelComponents.WriteOrcaflexModel`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1233-L1245] |
| `OrcaflexModelComponents.build_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1247-L1366] |
| `OrcaFlexOptimizedParallelAnalysis.process_single_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py:L89-L187] |
| `process_single_file_worker`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L39-L137] |
| `OrcaFlexParallelAnalysis.process_single_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py:L59-L159] |
| `OrcaflexUtilities.update_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L136-L149] |
| `OrcaflexUtilities.update_input_file`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L164-L222] |
| `OrcaflexUtilities.get_full_yaml_file_and_save`; mapped caller | [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L444-L469] |
| `OrcaflexUtilities.get_general_yml`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L471-L479] |
| `OrcaflexUtilities.save_cfg_files_from_multiple_files`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L725-L734] |
| `convert_to_yml`; mapped independent | [src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py:L15-L48] |
| `PipelineSchematicGenerator._merge_yaml`; mapped wrapper | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L180-L197] |
| `PipelineSchematicGenerator._merge_lines`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L199-L226] |
| `PipelineSchematicGenerator._merge_6dbuoys`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L228-L267] |
| `PipelineSchematicGenerator._parse_pipeline_path`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L269-L291] |
| `PipelineSchematicGenerator._parse_boundary_conditions`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L293-L461] |
| `PipelineSchematicGenerator.create_plan_view`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L513-L602] |
| `PipelineSchematicGenerator.create_elevation_view`; mapped independent | [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L604-L723] |
| `LoadVessel.aqwa_group_item`; mapped independent | [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L24-L100] |
| `_extract_dynamic_results_all`; mapped independent | [src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py:L259-L284] |
| `RiserRenderer._build_riser_dynamic_results`; mapped independent | [src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py:L34-L62] |
| `find_model_files`; mapped independent | [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L236-L268] |
| `AuthoredSpec.to_spec_yaml`; mapped independent | [src/digitalmodel/solvers/orcaflex/spec_author.py:L403-L410] |
| `TemplateGenerator.generate`; mapped caller | [src/digitalmodel/solvers/orcaflex/template_generator.py:L327-L380] |
| `TemplateGenerator._generate_reference`; mapped independent | [src/digitalmodel/solvers/orcaflex/template_generator.py:L382-L420] |
| `InstallationVtoHost.installation_step`; mapped independent | [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L27-L44] |
| `InstallationVtoHost.get_target_line_length`; mapped independent | [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L54-L88] |
| `InstallationVtoHost.get_target_reference_distance`; mapped independent | [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L90-L152] |
| `Lower2ndEnd.installation_step`; mapped independent | [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L27-L44] |
| `Lower2ndEnd.get_target_line_length`; mapped copy | [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L54-L88] |
| `Lower2ndEnd.get_target_reference_distance`; mapped independent | [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L90-L189] |
| `UniversalOrcaFlexRunner._process_single_model`; mapped independent | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |
| `create_config`; mapped independent | [src/digitalmodel/solvers/orcaflex/universal_cli.py:L337-L374] |
| `_mapping_issues`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L97-L112] |
| `inspect_source`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L115-L141] |
| `orcaflex_dump`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L63-L85] |
| `orcaflex_load`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L88-L115] |
| `OrcaFlexYAMLValidator._detect_format`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L266-L290] |
| `OrcaFlexYAMLValidator._check_singular_section_keys`; mapped independent | [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L707-L721] |
| `OrcaWaveOrchestrator._create_vessel_config`; mapped independent | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L71-L116] |
| `OrcaWaveOrchestrator.list_available_vessels`; mapped independent | [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L652-L665] |
| `OrcaFlexConverter.save_orcaflex_file`; mapped independent | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L420] |
| `OrcaFlexConverter._save_compact_format`; mapped independent | [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L422-L465] |
| `_tree`; mapped independent | [src/digitalmodel/solvers/smoke/model_manifest.py:L96-L115] |
| `check_settings`; mapped independent | [src/digitalmodel/solvers/smoke/model_probe.py:L36-L62] |
| `_saved_proof`; mapped independent | [src/digitalmodel/solvers/smoke/probes.py:L137-L165] |
| `prepareFatiguePostProcessing`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L69-L89] |
| `orcaflexModel`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L357] |
| `lineTypes`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L555-L629] |
| `WriteOrcaflexModel`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1211-L1219] |
| `build_model`; mapped independent | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1222-L1288] |
| `OrcaFlexModelGenerator.generate_model_yml`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L165-L216] |
| `OrcaFlexModelGenerator.generate_model_dict`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L218-L247] |
| `VerticalRiser.get_fea_properties`; mapped independent | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L261] |
| `save_echo`; mapped independent | [src/digitalmodel/viv_screening/workflow.py:L254-L273] |
| `OrcaFlex.get`; mapped independent | [src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py:L83-L106] |
| `OrcaFlex.get`; mapped copy | [src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py:L68-L86] |
| `OrcaFlex.get`; mapped copy | [src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py:L75-L98] |
| `AnalysisMetadataGenerator._detect_workflow_type`; mapped independent | [src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py:L340-L360] |
| `integration_basis`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_bracket.py:L21-L28] |
| `named_body`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_bracket.py:L45-L49] |
| `expected_change`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_bracket.py:L94-L98] |
| `read_cases`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_evidence.py:L141-L162] |
| `_write_request`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_materialize.py:L107-L117] |
| `materialize_bracket_case`; mapped independent | [src/digitalmodel/workflows/installation_added_mass_materialize.py:L130-L170] |
| `_source`; mapped independent | [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L24-L47] |
| `_case_files`; mapped independent | [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L88-L118] |
| `_apply_change`; mapped independent | [src/digitalmodel/workflows/installation_numerical_compare.py:L126-L146] |
| `_canonical_tables`; mapped wrapper | [src/digitalmodel/workflows/installation_numerical_compare.py:L166-L178] |
| `_runtime`; mapped independent | [src/digitalmodel/workflows/installation_numerical_compare.py:L214-L233] |
| `_field`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L21-L25] |
| `_line`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L28-L35] |
| `_value`; mapped caller | [src/digitalmodel/workflows/installation_numerical_prepare.py:L38-L49] |
| `_anchors`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L61-L86] |
| `_timing`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L89-L97] |
| `_ordered`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L108-L138] |
| `_apply_native`; mapped wrapper | [src/digitalmodel/workflows/installation_numerical_prepare.py:L161-L165] |
| `_counts`; mapped wrapper | [src/digitalmodel/workflows/installation_numerical_prepare.py:L168-L176] |
| `_variation`; mapped caller | [src/digitalmodel/workflows/installation_numerical_prepare.py:L179-L187] |
| `_native_case`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L190-L211] |
| `_request`; mapped independent | [src/digitalmodel/workflows/installation_numerical_prepare.py:L214-L223] |
| `_snapshot`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L41-L44] |
| `_structure`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L47-L57] |
| `_request`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L154-L166] |
| `generate_case`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L169-L200] |
| `prepare_matrix`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L203-L241] |
| `_change_payload`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L244-L258] |
| `materialize_case`; mapped independent | [src/digitalmodel/workflows/installation_seastates.py:L261-L316] |
| `_verify_master`; mapped independent | [src/digitalmodel/workflows/installation_variant_study.py:L72-L79] |
| `_verify_settings`; mapped independent | [src/digitalmodel/workflows/installation_variant_study.py:L82-L93] |
| `prepare_variant_study`; mapped independent | [src/digitalmodel/workflows/installation_variant_study.py:L117-L165] |
| `OrcaWaveMCPServer._register_tools.export_to_orcaflex`; mapped independent | [src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py:L486-L542] |
| `_prepare`; mapped independent | [src/digitalmodel/workflows/orcaflex_reproduce.py:L230-L237] |
| `extract_results`; mapped independent | [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L153-L184] |
| `_write_yaml`; mapped independent | [src/digitalmodel/workflows/parametric_run.py:L210-L212] |
| `SkillResolver._parse_skill_file`; mapped independent | [src/digitalmodel/workflows/skills/skill_resolver.py:L98-L134] |
| `_model`; mapped independent | [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L152-L167] |
| `_apply`; mapped independent | [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L170-L185] |
| `build_candidate`; mapped independent | [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L208-L257] |
| `_dependencies`; mapped independent | [src/digitalmodel/workflows/structure_installation_prepare.py:L30-L42] |
| `base_spec`; mapped test helper | [tests/drilling_riser/global_model/test_batch_params.py:L23-L28] |
| `base_spec`; mapped test helper | [tests/drilling_riser/global_model/test_campaign.py:L21-L26] |
| `_case`; mapped test helper | [tests/drilling_riser/global_model/test_event_models.py:L20-L23] |
| `_generic`; mapped test helper | [tests/drilling_riser/global_model/test_hang_off.py:L75-L77] |
| `_case`; mapped test helper | [tests/drilling_riser/global_model/test_hang_off.py:L110-L115] |
| `_ow_case`; mapped test helper | [tests/drilling_riser/global_model/test_open_water.py:L354-L362] |
| `base_spec`; mapped test helper | [tests/drilling_riser/global_model/test_statics_path.py:L28-L33] |
| `_line`; mapped test helper | [tests/drilling_riser/global_model/test_tensioner_representation.py:L31-L32] |
| `base_spec`; mapped test helper | [tests/drilling_riser/global_model/test_w5_channels.py:L73-L78] |
| `_doc`; mapped test helper | [tests/drilling_riser/postprocess/test_w510_significant_range.py:L174-L185] |
| `setup`; mapped test helper | [tests/legal/test_sanitize_s7_audit_private.py:L47-L64] |
| `test_deep_reference_and_registry_paths_are_excluded`; mapped test helper | [tests/maintenance/test_archive_safety.py:L59-L70] |
| `test_generator_partitions_move_keep_hold_and_excluded`; mapped test helper | [tests/maintenance/test_archive_safety.py:L256-L303] |
| `test_specs_match_basis_and_static_geometry`; mapped test helper | [tests/orcaflex/test_generic_scr_slwr.py:L59-L107] |
| `test_analytical_reference_reproduces_from_basis`; mapped test helper | [tests/orcaflex/test_generic_scr_slwr.py:L196-L205] |
| `TestModularToSpecBoundaryReporting.test_expected_gaps_classified`; mapped test helper | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L393-L409] |
| `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified`; mapped test helper | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L411-L425] |
| `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized`; mapped test helper | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L436-L450] |
| `_build`; mapped test helper | [tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py:L54-L58] |
| `_build`; mapped test helper | [tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py:L70-L77] |
| `_build`; mapped test helper | [tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py:L72-L79] |
| `main`; mapped test helper | [tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py:L79-L135] |
| `_write_floating_campaign`; mapped test helper | [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L30-L120] |
| `_write_slay_campaign`; mapped test helper | [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L126-L219] |
| `_write_campaign_yaml`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_campaign_generator.py:L98-L102] |
| `_write_model`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_collection_collision.py:L52-L74] |
| `_sections`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py:L45-L48] |
| `test_generation_writes_master_and_includes`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py:L87-L118] |
| `TestJumperLineTypes.test_three_jumper_variants_emitted`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py:L108-L120] |
| `TestJumperLineReferences.test_jumper_line_uses_all_three_variants`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py:L187-L209] |
| `_assert_line_type_references_resolve`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py:L74-L87] |
| `_write_mono_yaml`; mapped test helper | [tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py:L31-L36] |
| `find_model_files`; mapped test helper | [tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/run_models_to_sim.py:L198-L228] |
| `_build_boundary_conditions`; mapped test helper | [tests/solvers/orcaflex/reporting/fixture_helpers.py:L126-L158] |
| `SingleFileParallelStrategy._get_line_results`; mapped test helper | [tests/solvers/orcaflex/single_file_parallel_test.py:L135-L147] |
| `SingleFileParallelStrategy._get_vessel_results`; mapped test helper | [tests/solvers/orcaflex/single_file_parallel_test.py:L149-L159] |
| `test_structural_owners_and_path_references`; mapped test helper | [tests/solvers/orcaflex/test_model_catalogue.py:L203-L227] |
| `test_same_named_unrelated_folders_are_not_references`; mapped test helper | [tests/solvers/orcaflex/test_model_catalogue_references.py:L24-L45] |
| `header_repository_findings`; mapped test helper | [tests/solvers/orcaflex/test_model_header_guard.py:L25-L45] |
| `master_with_missing_include`; mapped test helper | [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L51-L54] |
| `test_support_types_are_defined_before_vessel_references`; mapped test helper | [tests/solvers/orcaflex/test_slay_layout.py:L90-L108] |
| `_resolve_case.read`; mapped test helper | [tests/solvers/orcaflex/test_slay_load_form.py:L38-L48] |
| `_pipe_seed_points`; mapped test helper | [tests/solvers/orcaflex/test_slay_load_form.py:L117-L134] |
| `sample_base_template`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L42-L70] |
| `sample_variation`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L74-L97] |
| `sample_case_template`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L101-L120] |
| `TestTemplateGenerator.test_generate_with_basefile_reference`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L261-L283] |
| `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L285-L317] |
| `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative`; mapped test helper | [tests/solvers/orcaflex/test_template_generator.py:L319-L343] |
| `manifest`; mapped test helper | [tests/solvers/smoke/test_model_manifest.py:L16-L31] |
| `temp_config_file`; mapped test helper | [tests/test_engine.py:L814-L829] |
| `_solve_input`; mapped test helper | [tests/test_main_exit_status.py:L110-L132] |
| `_write_skill`; mapped test helper | [tests/test_skill_resolver.py:L22-L65] |
| `test_workflow_registry`; mapped test helper | [tests/workflows/test_durable_workflows.py:L52-L1710] |
| `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3`; mapped test helper | [tests/workflows/test_durable_workflows.py:L1790-L1852] |
| `test_spectral_fatigue_workflow_matches_narrow_band_closed_form`; mapped test helper | [tests/workflows/test_durable_workflows.py:L1855-L1911] |
| `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening`; mapped test helper | [tests/workflows/test_durable_workflows.py:L1914-L1981] |
| `test_vessel_seakeeping_significant_amplitude_matches_library`; mapped test helper | [tests/workflows/test_durable_workflows.py:L1984-L2058] |
| `fake`; mapped test helper | [tests/workflows/test_installation_instability_diagnostic.py:L47-L58] |
| `test_duration_render_preserves_native_default_spelling_and_source`; mapped test helper | [tests/workflows/test_orcaflex_batch_contract.py:L97-L121] |
| `resume_case`; mapped test helper | [tests/workflows/test_orcaflex_reproduce.py:L107-L127] |
| `case`; mapped test helper | [tests/workflows/test_structure_hydrodynamic_candidate.py:L16-L52] |

### Wind and current force routes

| Mapped symbol / classification | Location at code revision |
| --- | --- |
| `calc_splash_zone`; mapped example | [examples/demos/gtm/demo_03_deepwater_mudmat_installation.py:L312-L384] |
| `calc_phase_3_splash`; mapped example | [examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py:L375-L441] |
| Module-level entry; mapped example | [examples/demos/gtm/results_store_demo05.py:L51-L63] |
| `_good_case`; mapped example | [examples/demos/gtm/tests/test_demo_05_results_store.py:L80-L126] |
| `calculate_environmental_forces`; mapped example | [examples/domains/fpso/fpso_mooring_analysis.py:L194-L236] |
| `main`; mapped example | [examples/domains/fpso/fpso_mooring_analysis.py:L497-L562] |
| `main`; mapped example | [examples/domains/ocimf/ocimf_demo.py:L24-L341] |
| `calculate_environmental_forces`; mapped example | [examples/domains/workflows/01_spread_mooring_8point.py:L123-L173] |
| `analyze_calm_buoy`; mapped example | [examples/domains/workflows/03_calm_buoy.py:L53-L113] |
| `analyze_jackup_mooring`; mapped example | [examples/domains/workflows/05_jackup_installation.py:L37-L63] |
| `analyze_floating_wind_mooring`; mapped example | [examples/domains/workflows/06_floating_wind_turbine.py:L37-L72] |
| `analyze_wec_mooring`; mapped example | [examples/domains/workflows/07_wave_energy_converter.py:L36-L66] |
| `analyze_aquaculture_mooring`; mapped example | [examples/domains/workflows/08_aquaculture_mooring.py:L35-L62] |
| `analyze_spm_terminal`; mapped example | [examples/domains/workflows/09_spm_terminal.py:L34-L67] |
| `analyze_dp_backup_mooring`; mapped example | [examples/domains/workflows/10_dp_backup_mooring.py:L36-L81] |
| Module-level entry; mapped independent | [scripts/capabilities/build_onepagers.py:L36-L48] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/legacy/extract_ocimf_database.py:L25-L37] |
| `extract_ocimf_database`; mapped independent | [scripts/python/digitalmodel/legacy/extract_ocimf_database.py:L220-L283] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L40-L52] |
| `main`; mapped independent | [scripts/python/digitalmodel/legacy/generate_ocimf_report.py:L16-L52] |
| `PerformanceProfiler._setup_test_data`; mapped independent | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L112-L119] |
| `PerformanceProfiler.profile_ocimf_lookup`; mapped independent | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L196-L261] |
| `PerformanceProfiler.run_all`; mapped independent | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L499-L515] |
| `main`; mapped independent | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L518-L544] |
| `Phase2Validator.generate_excel_reference_data`; mapped independent | [scripts/python/digitalmodel/legacy/validate_phase2.py:L104-L226] |
| `Phase2Validator.export_statistics_to_csv`; mapped independent | [scripts/python/digitalmodel/legacy/validate_phase2.py:L853-L882] |
| `Phase2Validator.export_json_summary`; mapped independent | [scripts/python/digitalmodel/legacy/validate_phase2.py:L1191-L1214] |
| `Phase2Validator.run_complete_validation`; mapped independent | [scripts/python/digitalmodel/legacy/validate_phase2.py:L1216-L1256] |
| Module-level entry; mapped independent | [scripts/python/digitalmodel/legacy/visualize_ocimf_data.py:L22-L34] |
| `main`; mapped independent | [scripts/python/digitalmodel/legacy/visualize_ocimf_data.py:L371-L413] |
| `make_polar_overlay`; mapped independent | [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L406-L476] |
| `parse_args`; mapped independent | [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L802-L826] |
| `PressureVesselGenerator.generate_wind_load`; mapped independent | [src/digitalmodel/ansys/pressure_vessel.py:L704-L729] |
| `SyntheticRopeDatabaseClient._generate_rope_spec`; mapped independent | [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py:L171-L246] |
| `main`; mapped independent | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L367-L415] |
| `environmental_force_n`; mapped independent | [src/digitalmodel/drilling_riser/drift_off.py:L97-L131] |
| `CurrentProfile.drag_load_n_per_m`; mapped independent | [src/digitalmodel/drilling_riser/envelope.py:L108-L119] |
| `as_analysed_sections`; mapped independent | [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L248-L289] |
| `router`; mapped independent | [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L272] |
| `engine`; mapped caller | [src/digitalmodel/engine.py:L90-L892] |
| `ocimf_wind_cmd`; mapped independent | [src/digitalmodel/hydrodynamics/cli.py:L115-L160] |
| `ocimf_current_cmd`; mapped independent | [src/digitalmodel/hydrodynamics/cli.py:L173-L218] |
| `combined_env_cmd`; mapped independent | [src/digitalmodel/hydrodynamics/cli.py:L229-L287] |
| `FatigueAnalysisComponents.get_total_viv_fatigue`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L99-L112] |
| `FatigueAnalysisComponents.tabulate_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L114-L167] |
| `VIVFatigueAnalysisComponents.get_total_viv_fatigue`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L176-L198] |
| `VIVFatigueAnalysisComponents.tabulate_viv_results`; mapped independent | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L200-L317] |
| `ETL_components.transform_viv_current_reformat_1`; mapped independent | [src/digitalmodel/infrastructure/utils/ETL_components.py:L746-L822] |
| `compute_splashzone_envelope`; mapped independent | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L456-L556] |
| `calc_phase_3_splash`; mapped independent | [src/digitalmodel/installation/jumper_installation/calculations.py:L163-L205] |
| `main`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/__main__.py:L27-L144] |
| `OCIMFDatabase.get_coefficients`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L306-L357] |
| `EnvironmentalForces.calculate_total_forces`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L628-L677] |
| `create_sample_database`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L861-L932] |
| `OCIMFWorkflow.router`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py:L48-L64] |
| Module-level entry; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py:L25-L37] |
| `extract_ocimf_database`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py:L220-L283] |
| `PerformanceProfiler._setup_test_data`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L110-L117] |
| `PerformanceProfiler.profile_ocimf_lookup`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L188-L254] |
| `PerformanceProfiler.run_all`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L494-L510] |
| `main`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L513-L539] |
| `main`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py:L644-L656] |
| `Phase2Validator.generate_excel_reference_data`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L104-L226] |
| `Phase2Validator.export_statistics_to_csv`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L853-L882] |
| `Phase2Validator.export_json_summary`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L1191-L1214] |
| `Phase2Validator.run_complete_validation`; mapped copy | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L1216-L1256] |
| Module-level entry; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py:L40-L52] |
| `_output_dir`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L25-L31] |
| `_cli_output_dir`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L34-L36] |
| `load_data`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L55-L64] |
| `main`; mapped independent | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L448-L493] |
| `OCIMFDatabase.get_coefficients`; mapped copy | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L323-L374] |
| `EnvironmentalForces.calculate_total_forces`; mapped copy | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L645-L694] |
| `create_sample_database`; mapped copy | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952] |
| `FPSOMooringFullWorkflow._environmental_forces`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L78-L102] |
| `FPSOMooringFullWorkflow._ocimf_database`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L104-L120] |
| `_resolve_arrow_direction_in_body_frame`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L60] |
| `polar_force_overlay`; mapped independent | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L357] |
| `threshold_speed_for_steerage`; mapped independent | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L246] |
| `run_maneuvering_envelope_sweep`; mapped independent | [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L233-L345] |
| `SplashZoneInput.calculate_splash_zone_loads`; mapped independent | [src/digitalmodel/orcaflex/installation_analysis.py:L245-L298] |
| `compute_wind_current_drift`; mapped independent | [src/digitalmodel/orcawave/drift_forces.py:L224-L298] |
| `KnowledgeIntegrator._extract_configurations`; mapped independent | [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L104-L121] |
| `EnvironmentBuilder.build`; mapped independent | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L171-L249] |
| `extract_loads`; mapped independent | [src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py:L12-L60] |
| `calculate_dual_chute_drag`; mapped independent | [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L100] |
| `assess_all_load_cases`; mapped caller | [src/digitalmodel/structural/parachute/chute_assessment.py:L131-L186] |
| `simulate_deployment_transient`; mapped independent | [src/digitalmodel/structural/parachute/chute_assessment.py:L189-L293] |
| `export_results_yaml`; mapped independent | [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] |
| `calculate_drag_force`; mapped independent | [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L74] |
| `design_verification`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/cli.py:L159-L324] |
| `MooringDesigner.calculate_environmental_loads`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/designer.py:L60-L124] |
| `build_wind_model.force`; mapped independent | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L1094-L1096] |
| `_phase_demand`; mapped independent | [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L205] |
| `hydrodynamic_loads`; mapped independent | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L171-L211] |
| `drag_force_per_meter`; mapped independent | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L214-L234] |
| `lateral_stability_check`; mapped independent | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L291-L360] |
| `check_lateral_stability`; mapped independent | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L363-L395] |
| `run_on_bottom_stability`; mapped independent | [src/digitalmodel/subsea/pipeline/integrity_workflows.py:L7-L71] |
| `VIVScreening.screen_with_current_profile`; mapped independent | [src/digitalmodel/subsea/viv_analysis/screening.py:L278-L308] |
| `viv_analysis`; mapped independent | [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L52-L97] |
| `VortexSheddingAnalyzer.analyze_with_profile`; mapped independent | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L194] |
| `create_mooring_design_template`; mapped independent | [src/digitalmodel/workflows/workflow_automation/templates.py:L306-L349] |
| `MooringSystemDesignWorkflow.create`; mapped independent | [src/digitalmodel/workflows/workflow_automation/workflows.py:L169-L256] |
| Module-level entry; mapped test helper | [tests/contracts/test_force_units_boundary_contract.py:L52-L64] |
| `_load_producer`; mapped test helper | [tests/legal/test_published_pages_have_no_internal_paths.py:L148-L156] |
| `sample_database_path`; mapped test helper | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L31-L47] |
| `ocimf_db`; mapped test helper | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L51-L53] |
| `env_forces`; mapped test helper | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L57-L59] |
| `main`; mapped test helper | [tests/marine_ops/marine_engineering/integration/integration_example.py:L32-L399] |
| `TestOCIMFMooringIntegration.test_combined_environmental_forces`; mapped test helper | [tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py:L105-L152] |
| `ocimf_database`; mapped test helper | [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L33-L39] |
| `pytest_benchmark_update_json`; mapped test helper | [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L364-L373] |
| `test_refactored_polar_signatures_match_fixture`; mapped test helper | [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L72-L129] |
| `_build_loads`; mapped test helper | [tests/solvers/orcaflex/reporting/fixture_helpers.py:L161-L186] |
| `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc`; mapped test helper | [tests/structural/parachute/test_chute_assessment.py:L136-L148] |
| `TestDragForce.test_drag_200_mph`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L70-L80] |
| `TestDragForce.test_drag_250_mph`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L82-L91] |
| `TestDragForce.test_force_scales_with_v_squared`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L98-L103] |
| `TestDragForce.test_result_has_newtons`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L105-L108] |
| `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L173-L186] |
| `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs`; mapped test helper | [tests/structural/parachute/test_parachute_drag.py:L188-L201] |
| `catenary_environment`; mapped test helper | [tests/subsea/catenary_riser/conftest.py:L19-L30] |
| `TestLiftForce.test_known_lift_force`; mapped test helper | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L103-L110] |
| `TestDragForce.test_scenario_1`; mapped test helper | [tests/subsea/pipeline/test_on_bottom_stability.py:L72-L78] |
| `TestLiftForce.test_scenario_1`; mapped test helper | [tests/subsea/pipeline/test_on_bottom_stability.py:L82-L88] |
| `get_dnv_circular_mock`; mapped test helper | [tests/test_automation/fixtures/engineering_fixtures.py:L145-L167] |
| `get_dnv_rectangular_mock`; mapped test helper | [tests/test_automation/fixtures/engineering_fixtures.py:L170-L193] |
| `TestHydrodynamicLoads.test_drag_force_per_meter`; mapped test helper | [tests/test_on_bottom_stability.py:L54-L66] |
| `TestHydrodynamicLoads.test_lift_force_per_meter`; mapped test helper | [tests/test_on_bottom_stability.py:L68-L80] |
| `test_workflow_registry`; mapped test helper | [tests/workflows/test_durable_workflows.py:L52-L1710] |


## 10. Coverage and open items

The required source files read are `models.py`, `catenary.py`, `designer.py`, `fpso_workflow.py`, `dynmoor.py`, `orcaflex_generator.py`, `cli.py` and `README.md`; the domain README is an API-documentation placeholder. [src/digitalmodel/subsea/mooring_analysis/models.py:L35-L49] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L119] [src/digitalmodel/subsea/mooring_analysis/designer.py:L126-L207] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L49] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L10-L62] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L165-L216] [src/digitalmodel/subsea/mooring_analysis/cli.py:L327-L333] [src/digitalmodel/subsea/mooring_analysis/README.md:L361-L373] [docs/domains/subsea/mooring_analysis/README.md:L5-L17]

The offline scope includes all four module test files and the contracts tests; section 4 names the representative case nodes, while unfinished legacy dynamics remain four explicitly skipped nodes. The durable spread-workflow input and route were read without executing a solver. [tests/subsea/mooring_analysis/test_mooring_analysis_unit.py:L121-L218] [tests/subsea/mooring_analysis/test_mooring_analysis_cli.py:L14-L79] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L73] [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] [examples/workflows/fpso-spread-mooring/input.yml:L1-L47] [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L49]

| ID | Open item / uncertainty | Issue and proposed responsible role |
| --- | --- | --- |
| U1 | UNVERIFIED: Standard editions, clause titles, method applicability and an API RP 2SK condition-factor mapping are not established; README readiness is not qualification. Clause text not checked against the licensed copy. [src/digitalmodel/subsea/mooring_analysis/README.md:L1-L9] [src/digitalmodel/subsea/mooring_analysis/README.md:L361-L368] [src/digitalmodel/subsea/mooring_analysis/designer.py:L34-L48] | #1805; maintainer and family quality manager |
| U2 | UNVERIFIED: Primary manufacturer/source revision and calculation citation sidecars for line properties are not established by the literal library. No private sidecar values are included. [src/digitalmodel/subsea/mooring_analysis/models.py:L206-L302] | issue pending (2); maintainer |
| U3 | UNVERIFIED: A target OrcaFlex version, native unit/schema/profile acceptance, statics and short dynamics have not been established for this generator. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L98-L121] [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L190-L210] | #2426 and issue pending (3); family quality manager |
| U4 | UNVERIFIED: The compiled-predecessor snapshots have no archived public benchmark revision or independent physical validation established by the test file; keep them as regression examples. [tests/subsea/mooring_analysis/test_dynmoor.py:L3-L38] | issue pending (7); maintainer |
| C1 | Local/canonical catenary agreement and an independent numerical top-tension worked example remain open; general-solver stretch must not become an unchecked hand reference. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L78-L104] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L295] | #1811, #2050; family quality manager |
| C2 | Transient factor dispatch differs from the exposed condition property; no numeric standard values are reproduced in this pack. [src/digitalmodel/subsea/mooring_analysis/models.py:L180-L189] [src/digitalmodel/subsea/mooring_analysis/designer.py:L390-L396] | issue pending (1); maintainer |
| C3 | CLI model export is a placeholder; CLI design changes shared material state. [src/digitalmodel/subsea/mooring_analysis/cli.py:L176-L179] [src/digitalmodel/subsea/mooring_analysis/cli.py:L327-L333] | issue pending (4), issue pending (5); maintainer |
| C4 | Legacy time-domain integrator and its associated four tests remain unfinished. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L85-L93] [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] | issue pending (6); maintainer |
| C5 | Full generic-system and single-point worked examples require reference selection, response coverage and native results beyond the intact screen. [src/digitalmodel/subsea/mooring_analysis/fpso_workflow.py:L29-L49] [src/digitalmodel/subsea/mooring_analysis/models.py:L192-L203] | #2426, #2432; family quality manager |

Issue numbers refer to the repository issue tracker; `issue pending (n)` refers to draft n supplied with this draft pack, pending maintainer creation before final acceptance.
