# fea_model knowledge pack

| Header | Value |
| --- | --- |
| Module | `solvers/fea_model` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/solvers/fea_model` |
| Pack revision | 0 |
| Code revision | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Map snapshot | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `origin/codex/implmap-work` |
| Map declared data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Analysis family | OrcaFlex riser and mooring; AQG r0, section 10 |
| Family quality manager | Riser and mooring quality manager |
| Status | DRAFT; independent clearance pending |

Citations resolve at the code revision except implementation-map citations, which resolve at the separate map snapshot above.

## 1. Purpose and scope

`fea_model(cfg)` constructs `FEAComponents`, obtains raw data, prepares the model, saves output, applies load variations and returns the original configuration. [src/digitalmodel/solvers/fea_model/fea_model.py:L4-L14]
The shared assembler selects OrcaFlex by `FEAProgram["name"]`, adds General and Environment, and dispatches assets to buoy, line type, line, vessel type, vessel, shape, group, constraint and variable-data builders. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L41-L79]
The package produces dictionaries; its entry point delegates file creation to the shared assembler and does not itself solve statics or dynamics. [src/digitalmodel/solvers/fea_model/fea_model.py:L4-L14] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211]
The enclosing `solvers` domain records beta maturity; this is routing metadata, not evidence of engineering validation. [docs/registry/module-routing.yaml:L477-L482]
UNVERIFIED: geometric applicability limits, verified solver versions and independent engineering accuracy have not been established; issue pending (1).

## 2. Inputs and outputs

### Inputs

| Input | Unit / range | Default and source |
| --- | --- | --- |
| `cfg.General` | `UnitsSystem` declares SI; numeric ranges are not checked here | Flat override mapping; only recognised, non-None keys replace defaults. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L49] |
| `cfg.Environment` | Units inherited from General; no local dimensional validator | Sea/seabed keys are flat; Wave, Current and Wind use the first list entry. [src/digitalmodel/solvers/fea_model/environment_components.py:L19-L47] [src/digitalmodel/solvers/fea_model/environment_components.py:L77-L85] [src/digitalmodel/solvers/fea_model/environment_components.py:L129-L159] |
| Asset `cfg` wrappers | No range checks in property updater | Line, LineType, Vessel, VesselType, Constraint and VariableData use the nested `cfg` mapping. [src/digitalmodel/solvers/fea_model/line_components.py:L51-L56] [src/digitalmodel/solvers/fea_model/LineType_components.py:L49-L54] [src/digitalmodel/solvers/fea_model/Vessel_components.py:L32-L37] [src/digitalmodel/solvers/fea_model/VesselType_components.py:L17-L22] [src/digitalmodel/solvers/fea_model/Constraint_components.py:L36-L41] [src/digitalmodel/solvers/fea_model/VariableData_components.py:L19-L24] |
| Buoy properties | No local dimensional validator | Flat mapping, with `StackedBuoy.Cylinders` nested records; unspecified marine growth becomes zero. [src/digitalmodel/solvers/fea_model/buoy_components.py:L56-L61] [src/digitalmodel/solvers/fea_model/buoy_components.py:L81-L103] [src/digitalmodel/solvers/fea_model/buoy_components.py:L123-L132] |
| Line tables | Coordinates, lengths and angles have no unit tags in the tables | `connectionData`, `sections` and `statics` are required nested mappings; only the results mapping has an empty fallback. [src/digitalmodel/solvers/fea_model/line_components.py:L60-L74] [src/digitalmodel/solvers/fea_model/line_components.py:L93-L103] [src/digitalmodel/solvers/fea_model/line_components.py:L117-L147] |
| External vessel-type file | No format/version verification in this package | Presence of `filename` suppresses the generated dictionary; assembler checks file existence and adds its path to the output list. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L12] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L105-L116] |
| Load variation | CSV column names drive substring selection | Shared assembler reads CSV; package updates only the first wave/current/wind entries in place. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L217-L237] [src/digitalmodel/solvers/fea_model/environment_components.py:L163-L180] |

UNVERIFIED: full file-format units, axis handedness, coordinate origin, angle signs and wave phase conventions are not established by the unannotated defaults; issue pending (1). [src/digitalmodel/solvers/fea_model/line_components.py:L60-L65] [src/digitalmodel/solvers/fea_model/environment_components.py:L60-L74]
The bundled catenary text declares a different unit selector from `General`; it is not a demonstrated output of the dictionary assembly route. [src/digitalmodel/solvers/fea_model/templates/orcaflex/simple_catenary.yml:L4-L6] [src/digitalmodel/solvers/fea_model/general_components.py:L7-L10]

### Outputs

| Output | Unit / contract | Destination |
| --- | --- | --- |
| `General` and `Environment` dictionaries | General records SI; environment values carry no individual unit metadata | Shared assembler merges the returned dictionaries. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] [src/digitalmodel/solvers/fea_model/environment_components.py:L7-L17] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L51-L59] |
| Asset property dictionaries | Numeric payloads and table keys; no results or convergence record | Shared assembler collects object dictionaries under model object categories. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L81-L140] |
| Model YAML and variation batch CSV | Writer-controlled serialisation; no solver readback in this route | `SaveData.WriteOrcaFlexYMLFile`; batch table includes generated filenames. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L240-L251] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L298-L316] |
| Returned configuration | Same object passed to entry point | `fea_model` returns `cfg`. [src/digitalmodel/solvers/fea_model/fea_model.py:L4-L14] |

## 3. Method and governing standards

### Assembly definitions

All dictionary-producing definitions below are explicitly inventoried here for comparison with the map; they hold model data or assemble it, rather than independently solve a finite-element system. [src/digitalmodel/solvers/fea_model/line_components.py:L5-L35] [src/digitalmodel/solvers/fea_model/fea_model.py:L4-L14]

| Definition | Implemented operation |
| --- | --- |
| `General.getGeneralOrcaFlexSetUp` | Merge units, statics, dynamics and stage duration, then apply overrides. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] |
| `Environment.getOrcaFlexEnvironment`, `seaAndSeabed`, `wave`, `current`, `wind`, `update_with_load_variation` | Merge environmental defaults and caller properties; wave trains are passed to the gamma helper, and variations mutate first-entry loading. [src/digitalmodel/solvers/fea_model/environment_components.py:L7-L98] [src/digitalmodel/solvers/fea_model/environment_components.py:L112-L180] |
| `LineType.get_orcaflex_properties` and its property getters | Merge general, mass/geometry, limits, stiffness, contact, hydrodynamics, stress, friction, damping and drawing groups. [src/digitalmodel/solvers/fea_model/LineType_components.py:L5-L41] [src/digitalmodel/solvers/fea_model/LineType_components.py:L43-L230] |
| `Line.get_orcaflex_properties` and its property getters | Merge line identification, end tables, sections, contents, statics and drawing; `Catenary` is a solver selector, not a local catenary equation. [src/digitalmodel/solvers/fea_model/line_components.py:L5-L35] [src/digitalmodel/solvers/fea_model/line_components.py:L37-L159] |
| `Buoy.get_orcaflex_properties`, `get_general`, `get_inertia`, `get_stacked_cylinder`, `get_cylinder`, `get_drawing` | Merge buoy properties, cylinder records, fixed applied loads and contact fields. [src/digitalmodel/solvers/fea_model/buoy_components.py:L5-L134] [src/digitalmodel/solvers/fea_model/buoy_components.py:L213-L234] |
| `Constraint.get_orcaflex_properties` and its getters | Merge identification, degrees-of-freedom table, stiffness/damping and drawing. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L5-L87] |
| `VariableData.get_orcaflex_properties`, `get_general`, `get_relationData` | Assemble a name and independent/dependent-value table. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L5-L40] |
| `Vessel.get_orcaflex_properties`, `get_general`, `get_caculation` | Merge vessel placement, motion/load switches and drawing settings. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L5-L68] |
| `VesselType.get_orcaflex_properties`, `get_general` | Generate a name/length dictionary only when no external filename is present. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L24] |
| `Shape.get_orcaflex_properties` | Return fixed geometry/drawing data without using configuration overrides. [src/digitalmodel/solvers/fea_model/shape_components.py:L5-L34] |
| `Group.get_orcaflex_properties` | Return a fixed grouping and collapsed-state dictionary without using configuration overrides. [src/digitalmodel/solvers/fea_model/group_components.py:L5-L28] |
| Per-class `update_property_group` | Iterate known defaults, ignore missing/None overrides, copy other values or dynamically call a `Calculated` helper. [src/digitalmodel/solvers/fea_model/buoy_components.py:L225-L234] [src/digitalmodel/solvers/fea_model/environment_components.py:L182-L191] [src/digitalmodel/solvers/fea_model/LineType_components.py:L232-L241] |

### Arithmetic definitions

| Definition | Calculation / provenance boundary |
| --- | --- |
| `Environment.getJONSWAPGamma` | Piecewise gamma from Hs and Tp using an exponential branch and upper cap, rounded to three decimal places; no reference is attached to this function. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] |
| `Environment.RayleighDampingCoefficients` | Select the wave period from the configured wave model, or use a supplied period; returns a parameter dictionary rather than computing Rayleigh coefficients. [src/digitalmodel/solvers/fea_model/environment_components.py:L193-L298] |
| `LineType.get_dragAndLift` | Set diameters from OD, or a steel-specific mass/density expression plus twice marine growth; then apply overrides. [src/digitalmodel/solvers/fea_model/LineType_components.py:L142-L173] |
| `Buoy.get_cylinder` | Add twice marine growth to caller OD in place, round, then populate the cylinder dictionary. [src/digitalmodel/solvers/fea_model/buoy_components.py:L105-L134] |
| `Buoy.get_cylinder_DragAreaMoment` | Return the rounded rectangular projected-area second moment and a symmetry zero. [src/digitalmodel/solvers/fea_model/buoy_components.py:L136-L141] |
| `Buoy.get_inertia_Mass` | Sum cylinder masses and round to three decimal places. [src/digitalmodel/solvers/fea_model/buoy_components.py:L175-L179] |
| `Buoy.get_inertia_CentreOfMass` | Mass-weight multiple centres, divide by total mass, or take the single centre, then round. [src/digitalmodel/solvers/fea_model/buoy_components.py:L181-L211] |
| `Buoy.get_inertia_MomentsOfInertia` | Sum mass times squared mass-diameter, derive the transverse terms using the equivalent centre and aggregate length, and round. [src/digitalmodel/solvers/fea_model/buoy_components.py:L143-L173] |

UNVERIFIED: authoritative references, dimensional consistency and independent validation for the gamma, steel hydrodynamic-diameter and buoy-inertia expressions remain open; none is approved as an independent hand-check reference; #2674 and issue pending (1). [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] [src/digitalmodel/solvers/fea_model/LineType_components.py:L158-L162] [src/digitalmodel/solvers/fea_model/buoy_components.py:L136-L211]
No governing standard or edition is attached to these arithmetic definitions; no licensed clause text was consulted and no standard-derived numerical limits are reproduced. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] [src/digitalmodel/solvers/fea_model/LineType_components.py:L142-L173] [src/digitalmodel/solvers/fea_model/buoy_components.py:L136-L211]

## 4. Validated worked examples

Validated examples: **0**; regression examples executing this package: **0**; independent engineering-value coverage is open in #2674.
No section-4 case is promoted from the bundled templates: no existing test node was found that imports or executes the package; the two fea_model adapters in the rigging test are replaced with failure sentinels. [tests/specialized/rigging/test_component_lookup.py:L28-L55]
The adjacent offline test is `tests/specialized/rigging/test_component_lookup.py`, with 13 synthetic lookup cases; these test the rigging module and are not validation of the property builders. [tests/specialized/rigging/test_component_lookup.py:L107-L163]
The documentation's FEA-test success statement is a documentation claim, unverified for this package; issue pending (1). [docs/domains/development/test_summary_with_validations.md:L24-L29]

## 5. QA gates

The gate set and numerical acceptance criteria below are proposed executable review checks for this module's generated model, using AQG r0 sections 4–10; they are not claims of completed solver QA.
No solver or program interface was started, imported or queried for this pack.

| Gate / check | Applicability and module-specific procedure | Numeric pass criterion / unit | Evidence and current result |
| --- | --- | --- | --- |
| G0 / C0 | Applicable: build General, Environment and each configured asset; compare every numerical default and override with an explicit design-basis field; reject unknown keys and missing required tables; compare input before and after repeated assembly | 0 unexplained defaults, 0 ignored keys, 0 mutated input fields, 0 malformed table rows (counts); units explicitly assigned for 100% of numerical fields | Dictionary and key-difference record; no supplied design-basis fixture, NOT_RUN; issue pending (1). The mutation/default risks are source-confirmed in section 6. [src/digitalmodel/solvers/fea_model/buoy_components.py:L123-L132] [src/digitalmodel/solvers/fea_model/line_components.py:L83-L89] |
| G1 / C1 | Applicable: serialise the exact shared-assembler output and load in the target OrcaFlex version; use a matching demo where available, otherwise the licensed `LoadData` route; compare General/Environment and every object with the manifest | 0 errors, 0 warnings, 0 missing/extra objects (counts), 1 matching target version (count) | Load record, version and fingerprint; NOT_RUN: target program required. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211] |
| G2 / C2 | Applicable: solve the emitted Lines with their selected statics settings; compare every end tension/angle/touchdown with an independently reviewed reference; include every buoy, constraint and vessel; perform governing-direction offset sweeps | 0 unconverged models and 0 unchecked objects (counts); tension and touchdown differences ≤5.0%, angle ≤1.0 degree; ≥6 offset steps (count), no monotonicity violations | Statics, independent calculation, component tables and plan/elevation images; NOT_RUN: solver/licence and independent reference required. The package's own buoy arithmetic is not the reference. [src/digitalmodel/solvers/fea_model/line_components.py:L117-L134] [src/digitalmodel/solvers/fea_model/Constraint_components.py:L5-L23] |
| G3 / C3 | Applicable: use the emitted General dynamics settings and Environment with the converged assembled model; run the short production-setting check and compare line tensions with statics | Reach 1.0 s; 0 warnings/errors/nonfinite responses (counts); tension change ≤25.0%; 0 new slack/compressed lines (counts) | Short-run log, final image and wall time; NOT_RUN: solver/licence required. [src/digitalmodel/solvers/fea_model/general_components.py:L19-L29] [src/digitalmodel/solvers/fea_model/environment_components.py:L7-L17] |
| G4 / C4 | Conditional: when VesselType filename input comes from a new diffraction run, require its cited G4 record; otherwise NOT_APPLICABLE with the explicit absence of new diffraction data | 1 linked record per imported type (count); force-balance error ≤1.0%, independent volume error ≤0.5%; 0 missing mesh/centre checks (counts) | Source-run fingerprint and diffraction evidence; NOT_RUN: no supplied external type or diffraction record; determine applicability per assembled model. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L12] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L105-L116] |
| G5 / C5 | Applicable: join the generated file/batch manifest to all required gate records, component rows and images; recompute file fingerprints rather than treating filename columns as provenance | 0 unmatched fingerprints, 0 absent required records, 0 open deviations (counts); 100% reported results traced to passing evidence | QA appendix and manifest comparison; NOT_RUN: no model QA records or report supplied. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L298-L316] |

Checks defined: **6**, one each for G0, G1, G2, G3, conditional G4 and G5; none is claimed to be a shipped package-specific test.
The dependency inventory and the absence of package-executing cases are separate from successful routing and repository-contract tests. [tests/specialized/rigging/test_component_lookup.py:L28-L55]

## 6. Known failure modes and pitfalls

The following sweep examines source text and AST without executing the solver or its interfaces.

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|\b(int|float)\(|\b0(?:\.0)?\b|\.get\(|unit|[Cc]ache' src/digitalmodel/solvers/fea_model --glob '*.py'
rg -n 'Calculated|is not None|return|Wave|Current|Wind' src/digitalmodel/solvers/fea_model --glob '*.py'
```

The numbered rows count distinct source sites: each fixed numerical/default table field, each empty property table, each missing/None override guard, and the additional semantic risks below; zeros in indices and empty accumulator dictionaries are not error paths.

sweep: 138 found

| ID | Symptom / cause at code pin | Detection | Tracking |
| --- | --- | --- | --- |
| S001 | Implicit `InitialPosition` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L29-L29] | C0; no committed detection check exists | issue pending (2) |
| S002 | Implicit `InitialAttitude` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L30-L30] | C0; no committed detection check exists | issue pending (2) |
| S003 | Empty degrees-of-freedom table survives absent override. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L45-L57] | C0; no committed detection check exists | issue pending (2) |
| S004 | Implicit `TranslationalStiffness` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L61-L61] | C0; no committed detection check exists | issue pending (2) |
| S005 | Implicit `RotationalStiffness` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L62-L62] | C0; no committed detection check exists | issue pending (2) |
| S006 | Implicit `TranslationalDamping` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L63-L63] | C0; no committed detection check exists | issue pending (2) |
| S007 | Implicit `RotationalDamping` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L64-L64] | C0; no committed detection check exists | issue pending (2) |
| S008 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L91-L98] | C0; no committed detection check exists | issue pending (2) |
| S009 | Implicit `OD` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L60-L60] | C0; no committed detection check exists | issue pending (2) |
| S010 | Implicit `ID` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L61-L61] | C0; no committed detection check exists | issue pending (2) |
| S011 | Implicit `CG` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L62-L62] | C0; no committed detection check exists | issue pending (2) |
| S012 | Implicit `MassPerUnitLength` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L64-L64] | C0; no committed detection check exists | issue pending (2) |
| S013 | Implicit `EI` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L94-L94] | C0; no committed detection check exists | issue pending (2) |
| S014 | Implicit `EA` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L95-L95] | C0; no committed detection check exists | issue pending (2) |
| S015 | Implicit `PoissonRatio` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L96-L96] | C0; no committed detection check exists | issue pending (2) |
| S016 | Implicit `GJ` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L97-L97] | C0; no committed detection check exists | issue pending (2) |
| S017 | Implicit `ContactDiameter` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L112-L112] | C0; no committed detection check exists | issue pending (2) |
| S018 | Implicit `ClashStiffness` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L113-L113] | C0; no committed detection check exists | issue pending (2) |
| S019 | Implicit `Ca` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L127-L127] | C0; no committed detection check exists | issue pending (2) |
| S020 | Implicit `Cs` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L129-L129] | C0; no committed detection check exists | issue pending (2) |
| S021 | Implicit `Ce` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L130-L130] | C0; no committed detection check exists | issue pending (2) |
| S022 | Implicit `Cd` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L146-L146] | C0; no committed detection check exists | issue pending (2) |
| S023 | Implicit `Cl` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L147-L147] | C0; no committed detection check exists | issue pending (2) |
| S024 | Missing marine-growth thickness becomes zero. [src/digitalmodel/solvers/fea_model/LineType_components.py:L152-L152] | C0; no committed detection check exists | issue pending (2) |
| S025 | Steel hydrodynamic diameter assumes fixed mass/density conversion with no unit check. [src/digitalmodel/solvers/fea_model/LineType_components.py:L158-L162] | C0; no committed detection check exists | issue pending (2) |
| S026 | Implicit `TensileStressLoadingFactor` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L180-L180] | C0; no committed detection check exists | issue pending (2) |
| S027 | Implicit `BendingStressLoadingFactor` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L181-L181] | C0; no committed detection check exists | issue pending (2) |
| S028 | Implicit `ShearStressLoadingFactor` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L182-L182] | C0; no committed detection check exists | issue pending (2) |
| S029 | Implicit `TorsionalStressLoadingFactor` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L183-L183] | C0; no committed detection check exists | issue pending (2) |
| S030 | Implicit `SeabedNormalFrictionCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L198-L198] | C0; no committed detection check exists | issue pending (2) |
| S031 | Implicit `Pen` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L228-L228] | C0; no committed detection check exists | issue pending (2) |
| S032 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/LineType_components.py:L234-L241] | C0; no committed detection check exists | issue pending (2) |
| S033 | Empty relation table survives absent override. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L28-L40] | C0; no committed detection check exists | issue pending (2) |
| S034 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L44-L51] | C0; no committed detection check exists | issue pending (2) |
| S035 | Filename branch implicitly returns None; assembler intentionally consumes that sentinel. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L12] | C0; no committed detection check exists | issue pending (2) |
| S036 | Implicit `Length` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L15-L15] | C0; no committed detection check exists | issue pending (2) |
| S037 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L28-L35] | C0; no committed detection check exists | issue pending (2) |
| S038 | Implicit `InitialPosition` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L28-L28] | C0; no committed detection check exists | issue pending (2) |
| S039 | Implicit `Orientation` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L29-L29] | C0; no committed detection check exists | issue pending (2) |
| S040 | Implicit `PrimaryMotionDividingPeriod` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L57-L57] | C0; no committed detection check exists | issue pending (2) |
| S041 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L72-L79] | C0; no committed detection check exists | issue pending (2) |
| S042 | Implicit `Origin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L19-L19] | C0; no committed detection check exists | issue pending (2) |
| S043 | Implicit `Force` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L20-L20] | C0; no committed detection check exists | issue pending (2) |
| S044 | Implicit `Moment` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L21-L21] | C0; no committed detection check exists | issue pending (2) |
| S045 | Implicit `Origin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L26-L26] | C0; no committed detection check exists | issue pending (2) |
| S046 | Implicit `Force` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L27-L27] | C0; no committed detection check exists | issue pending (2) |
| S047 | Implicit `Moment` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L28-L28] | C0; no committed detection check exists | issue pending (2) |
| S048 | Implicit `SeabedFrictionCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L32-L32] | C0; no committed detection check exists | issue pending (2) |
| S049 | Implicit `InitialPosition` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L52-L52] | C0; no committed detection check exists | issue pending (2) |
| S050 | Implicit `InitialAttitude` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L53-L53] | C0; no committed detection check exists | issue pending (2) |
| S051 | Implicit `Mass` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L67-L67] | C0; no committed detection check exists | issue pending (2) |
| S052 | Implicit `MomentsOfInertia` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L68-L68] | C0; no committed detection check exists | issue pending (2) |
| S053 | Implicit `CentreOfMass` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L69-L69] | C0; no committed detection check exists | issue pending (2) |
| S054 | Implicit `StackBaseCentre` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L83-L83] | C0; no committed detection check exists | issue pending (2) |
| S055 | Implicit `MunkMomentCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L85-L85] | C0; no committed detection check exists | issue pending (2) |
| S056 | Implicit `CylinderOuterDiameter` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L107-L107] | C0; no committed detection check exists | issue pending (2) |
| S057 | Implicit `CylinderInnerDiameter` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L108-L108] | C0; no committed detection check exists | issue pending (2) |
| S058 | Implicit `CylinderLength` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L109-L109] | C0; no committed detection check exists | issue pending (2) |
| S059 | Implicit `CylinderAxialDragArea` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L110-L110] | C0; no committed detection check exists | issue pending (2) |
| S060 | Implicit `DragForceCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L111-L111] | C0; no committed detection check exists | issue pending (2) |
| S061 | Implicit `DragAreaMoment` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L112-L112] | C0; no committed detection check exists | issue pending (2) |
| S062 | Implicit `DragMomentCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L113-L113] | C0; no committed detection check exists | issue pending (2) |
| S063 | Implicit `CylinderSlamForceDataEntry` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L114-L114] | C0; no committed detection check exists | issue pending (2) |
| S064 | Implicit `CylinderSlamForceDataExit` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L115-L115] | C0; no committed detection check exists | issue pending (2) |
| S065 | Implicit `AddedMassForceCoefficient` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L116-L116] | C0; no committed detection check exists | issue pending (2) |
| S066 | Implicit `AddedMomentOfInertia` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L118-L118] | C0; no committed detection check exists | issue pending (2) |
| S067 | Implicit `UnitDampingForce` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L119-L119] | C0; no committed detection check exists | issue pending (2) |
| S068 | Implicit `UnitDampingMoment` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L120-L120] | C0; no committed detection check exists | issue pending (2) |
| S069 | Missing marine-growth thickness becomes zero. [src/digitalmodel/solvers/fea_model/buoy_components.py:L123-L123] | C0; no committed detection check exists | issue pending (2) |
| S070 | Cylinder input diameter modified in place; repeated assembly compounds growth. [src/digitalmodel/solvers/fea_model/buoy_components.py:L123-L132] | C0; no committed detection check exists | issue pending (2) |
| S071 | Projected-area moment fixes transverse value to symmetry zero. [src/digitalmodel/solvers/fea_model/buoy_components.py:L136-L141] | C0; no committed detection check exists | issue pending (2) |
| S072 | Inertia formula assumes aggregate geometry; float conversion/rounding hides original numerical type. [src/digitalmodel/solvers/fea_model/buoy_components.py:L143-L173] | C0; no committed detection check exists | issue pending (2) |
| S073 | Mass/centre calculation accepts signed or zero total mass; empty cylinders fail; arithmetic floats are not external text parsing. [src/digitalmodel/solvers/fea_model/buoy_components.py:L175-L211] | C0; no committed detection check exists | issue pending (2) |
| S074 | Implicit `Pen` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/buoy_components.py:L214-L214] | C0; no committed detection check exists | issue pending (2) |
| S075 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/buoy_components.py:L227-L234] | C0; no committed detection check exists | issue pending (2) |
| S076 | Implicit `WaterSurfaceZ` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L22-L22] | C0; no committed detection check exists | issue pending (2) |
| S077 | Implicit `KinematicViscosity` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L23-L23] | C0; no committed detection check exists | issue pending (2) |
| S078 | Implicit `SeaTemperature` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L24-L24] | C0; no committed detection check exists | issue pending (2) |
| S079 | Implicit `Density` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L29-L29] | C0; no committed detection check exists | issue pending (2) |
| S080 | Implicit `SeabedOrigin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L32-L32] | C0; no committed detection check exists | issue pending (2) |
| S081 | Implicit `WaterDepth` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L33-L33] | C0; no committed detection check exists | issue pending (2) |
| S082 | Implicit `SeabedSlopeDirection` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L34-L34] | C0; no committed detection check exists | issue pending (2) |
| S083 | Implicit `SeabedSlope` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L35-L35] | C0; no committed detection check exists | issue pending (2) |
| S084 | Implicit `SeabedNormalStiffness` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L37-L37] | C0; no committed detection check exists | issue pending (2) |
| S085 | Implicit `SimulationTimeOrigin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L53-L53] | C0; no committed detection check exists | issue pending (2) |
| S086 | Implicit `WaveKinematicsCutoffDepth` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L54-L54] | C0; no committed detection check exists | issue pending (2) |
| S087 | Implicit `WaveCalculationTimeInterval` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L56-L56] | C0; no committed detection check exists | issue pending (2) |
| S088 | Implicit `WaveCalculationSpatialInterval` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L57-L57] | C0; no committed detection check exists | issue pending (2) |
| S089 | Implicit `WaveTrains` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L62-L73] | C0; no committed detection check exists | issue pending (2) |
| S090 | Implicit `WaveDirection` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L66-L66] | C0; no committed detection check exists | issue pending (2) |
| S091 | Implicit `WaveHeight` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L67-L67] | C0; no committed detection check exists | issue pending (2) |
| S092 | Implicit `WavePeriod` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L68-L68] | C0; no committed detection check exists | issue pending (2) |
| S093 | Implicit `WaveOrigin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L69-L69] | C0; no committed detection check exists | issue pending (2) |
| S094 | Implicit `WaveTimeOrigin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L70-L70] | C0; no committed detection check exists | issue pending (2) |
| S095 | Implicit `WaveStreamFunctionOrder` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L71-L71] | C0; no committed detection check exists | issue pending (2) |
| S096 | Calculated gamma replaces caller wave-train value in place. [src/digitalmodel/solvers/fea_model/environment_components.py:L89-L98] | C0; no committed detection check exists | issue pending (2) |
| S097 | Gamma formula has fixed branch/cap and no physical-domain or zero-height guard. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] | C0; no committed detection check exists | issue pending (2) |
| S098 | Implicit `RefCurrentSpeed` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L120-L120] | C0; no committed detection check exists | issue pending (2) |
| S099 | Implicit `RefCurrentDirection` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L121-L121] | C0; no committed detection check exists | issue pending (2) |
| S100 | Implicit `CurrentDepth, CurrentFactor, CurrentRotation` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L122-L125] | C0; no committed detection check exists | issue pending (2) |
| S101 | Implicit `AirDensity` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L147-L147] | C0; no committed detection check exists | issue pending (2) |
| S102 | Implicit `WindSpeed` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L149-L149] | C0; no committed detection check exists | issue pending (2) |
| S103 | Implicit `WindDirection` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L150-L150] | C0; no committed detection check exists | issue pending (2) |
| S104 | Substring filtering discards unrelated variation columns and updates only first wave/current/wind entries. [src/digitalmodel/solvers/fea_model/environment_components.py:L163-L180] | C0; no committed detection check exists | issue pending (2) |
| S105 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/environment_components.py:L184-L191] | C0; no committed detection check exists | issue pending (2) |
| S106 | Damping helper needs attributes not initialised by constructor; unrecognised wave type leaves customData unset. [src/digitalmodel/solvers/fea_model/environment_components.py:L193-L298] | C0; no committed detection check exists | issue pending (2) |
| S107 | Implicit `Period1` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L215-L217] | C0; no committed detection check exists | issue pending (2) |
| S108 | Implicit `Period1` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L238-L240] | C0; no committed detection check exists | issue pending (2) |
| S109 | Implicit `Period1` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/environment_components.py:L261-L264] | C0; no committed detection check exists | issue pending (2) |
| S110 | SI selector is a label; no per-field dimensional conversion or validation. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] | C0; no committed detection check exists | issue pending (2) |
| S111 | Implicit `StaticsMaxIterations` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/general_components.py:L15-L15] | C0; no committed detection check exists | issue pending (2) |
| S112 | Implicit `ImplicitConstantTimeStep` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/general_components.py:L22-L22] | C0; no committed detection check exists | issue pending (2) |
| S113 | Implicit `TargetLogSampleInterval` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/general_components.py:L24-L24] | C0; no committed detection check exists | issue pending (2) |
| S114 | Implicit `StageDuration` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/general_components.py:L28-L28] | C0; no committed detection check exists | issue pending (2) |
| S115 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/general_components.py:L42-L49] | C0; no committed detection check exists | issue pending (2) |
| S116 | Configuration ignored; grouping references fixed object names. [src/digitalmodel/solvers/fea_model/group_components.py:L5-L28] | C0; no committed detection check exists | issue pending (2) |
| S117 | Empty connection table survives when its exact composite key is absent. [src/digitalmodel/solvers/fea_model/line_components.py:L62-L74] | C0; no committed detection check exists | issue pending (2) |
| S118 | Implicit `ConnectionStiffnessX, ConnectionStiffnessY` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L78-L78] | C0; no committed detection check exists | issue pending (2) |
| S119 | Connection feeding key declares five columns; default rows contain only three cells. [src/digitalmodel/solvers/fea_model/line_components.py:L83-L89] | C0; no committed detection check exists | issue pending (2) |
| S120 | Implicit `ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L85-L88] | C0; no committed detection check exists | issue pending (2) |
| S121 | Implicit `LineType, Length, TargetSegmentLength` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L94-L94] | C0; no committed detection check exists | issue pending (2) |
| S122 | Implicit `ContentsDensity` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L109-L109] | C0; no committed detection check exists | issue pending (2) |
| S123 | Implicit `ContentsPressure` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L111-L111] | C0; no committed detection check exists | issue pending (2) |
| S124 | Implicit `ContentsFlowRate` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L112-L112] | C0; no committed detection check exists | issue pending (2) |
| S125 | Implicit `LayAzimuth` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L123-L123] | C0; no committed detection check exists | issue pending (2) |
| S126 | Implicit `AsLaidTension` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L124-L124] | C0; no committed detection check exists | issue pending (2) |
| S127 | Missing results mapping becomes empty; acceleration-log default retained. [src/digitalmodel/solvers/fea_model/line_components.py:L139-L147] | C0; no committed detection check exists | issue pending (2) |
| S128 | Implicit `SegmentPen` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L155-L155] | C0; no committed detection check exists | issue pending (2) |
| S129 | Implicit `ContactPen` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/line_components.py:L156-L156] | C0; no committed detection check exists | issue pending (2) |
| S130 | Missing/None and unknown keys silently retain defaults; Calculated dispatch is unchecked. [src/digitalmodel/solvers/fea_model/line_components.py:L163-L170] | C0; no committed detection check exists | issue pending (2) |
| S131 | Configuration ignored; geometry and application-relative drawing reference fixed. [src/digitalmodel/solvers/fea_model/shape_components.py:L5-L34] | C0; no committed detection check exists | issue pending (2) |
| S132 | Implicit `Origin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L17-L17] | C0; no committed detection check exists | issue pending (2) |
| S133 | Implicit `Size` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L18-L18] | C0; no committed detection check exists | issue pending (2) |
| S134 | Implicit `Orientation` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L19-L19] | C0; no committed detection check exists | issue pending (2) |
| S135 | Implicit `InsidePen` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L23-L23] | C0; no committed detection check exists | issue pending (2) |
| S136 | Implicit `NumberOfLines` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L23-L23] | C0; no committed detection check exists | issue pending (2) |
| S137 | Implicit `ShadedDrawingDrawSize` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L28-L28] | C0; no committed detection check exists | issue pending (2) |
| S138 | Implicit `ShadedDrawingOrigin` default/table; no requirement for explicit design-basis confirmation. [src/digitalmodel/solvers/fea_model/shape_components.py:L30-L30] | C0; no committed detection check exists | issue pending (2) |

No `except`, `continue`, explicit `return None`, error-path empty-container return, year-epoch fallback or cache write was found in the 12 Python files; the implicit vessel-type sentinel and empty property tables are included above.
All `float()` calls convert calculated mass/centre quantities, not external text; no `int()` conversion was found. [src/digitalmodel/solvers/fea_model/buoy_components.py:L143-L211]
A shared-assembler side effect lies outside the sweep count: enabled load variations call `overwrite_fe_files`, which removes existing matching output files before regeneration; no detection check exists; issue pending (2). [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L217-L237] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L288-L295]
Historical import-path defect #756 is not asserted current: the shared assembler now imports these component classes from the relocated package. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L9-L19]
Engineering-value coverage gap #2674 remains relevant; no committed package-specific value-asserting detection check exists. [tests/specialized/rigging/test_component_lookup.py:L28-L55]
Map metadata mentioning #2543 is not evidence that these functions contain that spectrum-normalisation defect: the package gamma helper returns a scalar and the damping helper returns configuration data. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] [src/digitalmodel/solvers/fea_model/environment_components.py:L288-L298]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- |
| 2026-11-08 | A passing adjacent lookup test does not exercise the property builders when the adapters are replaced. | [tests/specialized/rigging/test_component_lookup.py:L28-L55] | Initial scope and example classification; no shared-log entry assigned. |
| 2026-11-08 | Reusing cylinder configuration changes geometry because growth is added in place. | [src/digitalmodel/solvers/fea_model/buoy_components.py:L123-L132] | Initial silent-failure inventory; issue pending (2). |

## 8. Ownership

Required review role: riser and mooring family quality manager; deputy role: riser and mooring deputy quality manager; producer role: module documentation contributor; these are review requirements, not assertions of assigned personnel or completed review.
Escalate ambiguous criteria or classification to the family quality manager, and cross-family matters to the maintainers; independently clear the pack before merging (Module agent lifecycle, revision 0, section 4, steps Q and E).

## 9. Dependencies and implementation inventory

### Runtime and solver dependency boundary

The entry point calls the shared `FEAComponents` orchestration; that assembler imports these builders and sends model dictionaries to the assetutilities writer. [src/digitalmodel/solvers/fea_model/fea_model.py:L1-L14] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L9-L19] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211]
The old infrastructure import is a compatibility re-export, not another model assembler. [src/digitalmodel/infrastructure/common/FEAComponents.py:L9-L20]
The rigging module imports the line and line-type builders; this establishes a downstream dependency, not a solver execution result. [src/digitalmodel/specialized/rigging/rigging_components.py:L5-L8]
NumPy adds weighted centre vectors in the buoy centre calculation; the remaining arithmetic uses Python and local `math` imports. [src/digitalmodel/solvers/fea_model/buoy_components.py:L181-L211] [src/digitalmodel/solvers/fea_model/LineType_components.py:L142-L162] [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110]
UNVERIFIED: supported OrcaFlex version, binary-template provenance and successful target-program loading are not established; issue pending (1).

Every source occurrence of case-insensitive `orcaflex|orcf` in the package Python files is listed below; these are text references to the target program/property schema, not evidence of imported program interfaces.

| Source occurrence | Dependency |
| --- | --- |
| `Constraint_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L5-L5] |
| `LineType_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/LineType_components.py:L5-L5] |
| `VariableData_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L5-L5] |
| `VesselType_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L5] |
| `Vessel_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L5-L5] |
| `buoy_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/buoy_components.py:L5-L5] |
| `environment_components.py` line 7 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/environment_components.py:L7-L7] |
| `environment_components.py` line 179 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/environment_components.py:L179-L179] |
| `general_components.py` line 7 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L7] |
| `group_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/group_components.py:L5-L5] |
| `line_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/line_components.py:L5-L5] |
| `shape_components.py` line 5 | OrcaFlex property dictionary or local builder call. [src/digitalmodel/solvers/fea_model/shape_components.py:L5-L5] |

The bundled text template also names OrcaFlex in its heading; its input/output contract remains unqualified. [src/digitalmodel/solvers/fea_model/templates/orcaflex/simple_catenary.yml:L1-L6]
The bundled binary model is inventory-only: no solver load, binary interpretation or version query was performed; issue pending (1).

### Map family selection and exact grep

The implementation inventory uses the map snapshot and declared data revision in the header; its line citations refer to that snapshot, independently of the code pin.
The two map families with in-package implementation rows are ranked below; both are counted because there are fewer than three (digitalmodel maintainers, Implementation map, snapshot 3256bab95f84da3d9fadce8d5dcf8458d66d1eb4, families section).

| Rank | Family | In-package rows | All implementation rows |
| --- | --- | --- | --- |
| 1 | `wave-spectra` | 3 | 141. [docs/registry/implementation-map.yaml:L13600-L15377] |
| 2 | `orcaflex-text-model-writers` | 2 | 440. [docs/registry/implementation-map.yaml:L4940-L9493] |

Search procedure: read every package `.py` at the code pin; compile each map `search_signals[].pattern` with Python `re` and no flags; exclude a signal only if it matches `solvers/fea_model` under `re.IGNORECASE`; attribute a matching line to its innermost enclosing function/class, or the single module-level definition.
No signal is excluded for this package path; retained expressions are reproduced verbatim below as code, from the same map snapshot.

#### `wave-spectra` signals

```text
algorithm: jonswap|pierson.moskowitz|bretschneider|ochi.hubble|torsethaugen|gaussian.swell|\bISSC\b|spectral_moment|normalize_to_significant|spectral_density|gamma\s*\*\*|S_PM|S_pm|np\.exp|math\.exp|spectral_density|spectrum\s*\*|sigma\s*=
vocabulary: spectrum|spectral.density|wave.spectra|JONSWAP
constants_or_keys: 0\.07|0\.09|0\.287|peak_enhancement
```

#### `orcaflex-text-model-writers` signals

```text
algorithm: (?:yaml\.(?:dump|safe_dump)|write_text|open\(|\.write\(|save\(|SaveData|dump\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\.write\(|\.ya?ml|yaml\.safe_dump|shutil\.copy|\.write\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\.safe_dump|\.write_text
vocabulary: orcaflex|OrcaFlex|includefile|modular_generator
constants_or_keys: [\"\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\"\']|%YAML
```

The hit inventory lists each enclosing definition once, even when several source lines or signals match.

| Family | Definition | Classification |
| --- | --- | --- |
| `wave-spectra` | `Environment.get_wave_waveTrains` | Listed map implementation. [src/digitalmodel/solvers/fea_model/environment_components.py:L89-L98] |
| `wave-spectra` | `Environment.getJONSWAPGamma` | Listed map implementation. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] |
| `wave-spectra` | `Environment.RayleighDampingCoefficients` | Listed map implementation. [src/digitalmodel/solvers/fea_model/environment_components.py:L193-L298] |
| `orcaflex-text-model-writers` | `Constraint.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L5-L23] |
| `orcaflex-text-model-writers` | `LineType.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/LineType_components.py:L5-L41] |
| `orcaflex-text-model-writers` | `LineType.get_general` | Listed map implementation. [src/digitalmodel/solvers/fea_model/LineType_components.py:L43-L56] |
| `orcaflex-text-model-writers` | `VariableData.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L5-L14] |
| `orcaflex-text-model-writers` | `VesselType.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L5-L12] |
| `orcaflex-text-model-writers` | `Vessel.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L5-L19] |
| `orcaflex-text-model-writers` | `Buoy.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/buoy_components.py:L5-L41] |
| `orcaflex-text-model-writers` | `Environment.getOrcaFlexEnvironment` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L7-L17] |
| `orcaflex-text-model-writers` | `Environment.wave` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L49-L87] |
| `orcaflex-text-model-writers` | `Environment.get_wave_waveTrains` | Helper/delegate already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L89-L98] |
| `orcaflex-text-model-writers` | `Environment.current` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L112-L136] |
| `orcaflex-text-model-writers` | `Environment.wind` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L138-L161] |
| `orcaflex-text-model-writers` | `Environment.update_with_load_variation` | Helper/delegate already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L163-L180] |
| `orcaflex-text-model-writers` | `Environment.RayleighDampingCoefficients` | Helper/delegate already listed in section 3. [src/digitalmodel/solvers/fea_model/environment_components.py:L193-L298] |
| `orcaflex-text-model-writers` | `General.getGeneralOrcaFlexSetUp` | Listed map implementation. [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] |
| `orcaflex-text-model-writers` | `Group.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/group_components.py:L5-L28] |
| `orcaflex-text-model-writers` | `Line.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/line_components.py:L5-L35] |
| `orcaflex-text-model-writers` | `Shape.get_orcaflex_properties` | Dictionary builder already listed in section 3. [src/digitalmodel/solvers/fea_model/shape_components.py:L5-L34] |

Counted-family result: **0 further second implementations** beyond sections 3 and 9; 3 wave-family definitions and 18 writer-family definitions were inspected; map omissions already inventoried in section 3 are follow-ups, not unlisted misses.
Map follow-up: classify remaining object-dictionary writers, buoy mass/centre/inertia, hydrodynamic diameter and mutation helpers; the arithmetic families without in-package map rows are not counted until the map includes them; issue pending (3). [src/digitalmodel/solvers/fea_model/buoy_components.py:L105-L211] [src/digitalmodel/solvers/fea_model/LineType_components.py:L142-L173]

### Complete implementation-record inventory

Every implementation row in both qualifying families is listed once below, in map order within its family, including rows outside the package.
The family ordinal and exact map-record citation identify each row, including its path, symbol, relation and classification; external discovery names are kept in their source records rather than copied into this pack.
These records inventory the map's assertions; they do not independently validate every implementation in other packages.

#### `wave-spectra` — 141 records

| Map row | Scope | Implementation record |
| --- | --- | --- |
| 1 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | outside package | Source path and definition in cited record; `function`, `example`. [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | outside package | Source path and definition in cited record; `function`, `example`. [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | outside package | Source path and definition in cited record; `function`, `example`. [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | outside package | Source path and definition in cited record; `function`, `example`. [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | outside package | Source path and definition in cited record; `function`, `wrapper`. [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | outside package | Source path and definition in cited record; `function`, `copy`. [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14250-L14261] |
| 55 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | outside package | Source path and definition in cited record; `function`, `wrapper`. [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | in package | `Environment.get_wave_waveTrains`; `function`, `wrapper`. [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | in package | `Environment.getJONSWAPGamma`; `function`, `independent`. [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | in package | `Environment.RayleighDampingCoefficients`; `function`, `copy`. [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | outside package | Source path and definition in cited record; `function`, `copy`. [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | outside package | Source path and definition in cited record; `function`, `copy`. [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | outside package | Source path and definition in cited record; `function`, `test_helper`. [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | outside package | Source path and definition in cited record; `function`, `caller`. [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | outside package | Source path and definition in cited record; `function`, `independent`. [docs/registry/implementation-map.yaml:L15362-L15377] |

#### `orcaflex-text-model-writers` — 440 records

| Map row | Scope | Implementation record |
| --- | --- | --- |
| 1 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | outside package | Source path and definition in cited record; `writer`, `example`. [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | outside package | Source path and definition in cited record; `script`, `independent`. [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | in package | `LineType.get_general`; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | in package | `General.getGeneralOrcaFlexSetUp`; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | outside package | Source path and definition in cited record; `writer`, `copy`. [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | outside package | Source path and definition in cited record; `writer`, `wrapper`. [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | outside package | Source path and definition in cited record; `writer`, `caller`. [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | outside package | Source path and definition in cited record; `writer`, `independent`. [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | outside package | Source path and definition in cited record; `writer`, `test_helper`. [docs/registry/implementation-map.yaml:L9483-L9493] |

### Repository-wide further search

Additional discovery scans all tracked UTF-8 text in the repository, including `scripts/`, using the same unmodified expressions; source hits outside the package are map follow-ups and do not alter the counted-family result.
The exact-symbol supplementary search is:

```sh
rg -n 'def getJONSWAPGamma|def get_inertia_MomentsOfInertia|def get_cylinder_DragAreaMoment|def get_inertia_CentreOfMass|def get_dragAndLift' src scripts
```

Only package definitions matched that supplementary search; it does not establish repository-wide algorithm uniqueness. [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] [src/digitalmodel/solvers/fea_model/buoy_components.py:L136-L211] [src/digitalmodel/solvers/fea_model/LineType_components.py:L142-L173]
The shared assembler is a consumer/delegate of listed component builders; its save route delegates serialisation to assetutilities. [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L142-L211]
The broader exact-expression scan found 11,156 family/file matches including 675 in `scripts/`; these are discovery matches, not counts of independent implementations; unresolved external classifications remain issue pending (3).

## 10. Coverage and open items

### Files read and tests discovered

Every Python file is covered by the source inventory below; all package subfolders were enumerated, and no subpackage with its own pack was excluded.

| File | Coverage |
| --- | --- |
| `Constraint_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/Constraint_components.py:L1-L98] |
| `LineType_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/LineType_components.py:L1-L241] |
| `VariableData_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/VariableData_components.py:L1-L51] |
| `VesselType_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/VesselType_components.py:L1-L35] |
| `Vessel_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/Vessel_components.py:L1-L79] |
| `buoy_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/buoy_components.py:L1-L234] |
| `environment_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/environment_components.py:L1-L298] |
| `fea_model.py` | Complete text read. [src/digitalmodel/solvers/fea_model/fea_model.py:L1-L14] |
| `general_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/general_components.py:L1-L49] |
| `group_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/group_components.py:L1-L28] |
| `line_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/line_components.py:L1-L170] |
| `shape_components.py` | Complete text read. [src/digitalmodel/solvers/fea_model/shape_components.py:L1-L34] |

| Additional discovery surface | Coverage |
| --- | --- |
| `templates/orcaflex/simple_catenary.yml` | Text read; not treated as a validated worked example. [src/digitalmodel/solvers/fea_model/templates/orcaflex/simple_catenary.yml:L1-L31] |
| `templates/nastran/generic_beam.nas` | Text read; not executed or established as an assembly-route output. [src/digitalmodel/solvers/fea_model/templates/nastran/generic_beam.nas:L1-L39] |
| Binary template | Inventory only; no binary load or interpretation. Issue pending (1). |
| Module documentation | README still has description/API placeholders. [docs/domains/solvers/fea_model/README.md:L1-L17] |
| Related coverage document | Historical entry names the earlier module location; its coverage numbers are not reused. [docs/domains/testing/coverage_analysis_report.md:L32-L41] |
| Related test-summary document | FEA success statement retained only as an unverified documentation claim. [docs/domains/development/test_summary_with_validations.md:L24-L29] |
| Related capability document | Lists this package among adjacent support modules. [docs/plans/2026-06-29-cfd-capability-assessment.md:L68-L77] |
| Implementation-map summary | Both families list this module among the implementation locations. [docs/registry/implementation-map.md:L20-L23] |
| Rigging test discovery | Both builders are replaced, so no package execution is inferred. [tests/specialized/rigging/test_component_lookup.py:L28-L55] |
| Test-maintenance discovery | Script configuration names an older test basename; it is not itself a package-specific test. [tests/test_automation/scripts/batch_fix_tests.py:L1-L22] |
| Routing contract | The existing exact solver-route inventory requires a coordinated expectation update when adding a new route; issue pending (4). [tests/docs/test_digitalmodel_routing_contract.py:L176-L223] |

### Offline execution record

At code revision `b47e42fbe7208c69b28dd436aa91fb129bc385f7`, BASE ran 603 tests: 603 passed, 0 failed, 0 skipped, 0 NOT_RUN; module-specific test cases discovered: 0.
Command form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/specialized/rigging/test_component_lookup.py tests/docs/test_digitalmodel_routing_contract.py tests/contracts -q -rA -p no:cacheprovider --basetemp=<scratch>`; full-path uv was used, with the shared cache, and no addopts override.
AFTER uses the same command and uncommitted documentation tree; its result is recorded in the accompanying change evidence. No solver, program interface, licensed standard or private repository was accessed.

### Open-item register

All open-item responsibility lies with the maintainers and the family quality manager; draft references are temporary issue identifiers until the maintainers open the drafts.

| Item | Evidence / limit | Tracking |
| --- | --- | --- |
| U1 | UNVERIFIED: applicability limits, target solver versions, binary-template provenance, independent engineering accuracy and complete QA records | Issue pending (1); value-asserting example gap #2674 |
| U2 | UNVERIFIED: complete input/output unit systems, axes, origins, angle and phase conventions | Issue pending (1) |
| U3 | UNVERIFIED: independent formula provenance and dimensional consistency for gamma, steel hydrodynamic diameter and buoy inertia | Issue pending (1); #2674 |
| U4 | UNVERIFIED: the historical documentation claim of passing FEA tests applies to this package | Issue pending (1) |
| Silent-failure sites | Every section-6 row requires an explicit override/schema decision or a regression test; existing defaults are not claimed to be design-approved | Issue pending (2) |
| Map reconciliation | Additional writers, buoy and diameter methods need map classification; repository discovery hits outside this package remain unadjudicated | Issue pending (3) |
| Routing expectation | Additional route conflicts with an exact fixed inventory in the routing-contract test | Issue pending (4) |

No client-derived values or identifying discovery names are reproduced, and no private sidecar entry is asserted to exist.
