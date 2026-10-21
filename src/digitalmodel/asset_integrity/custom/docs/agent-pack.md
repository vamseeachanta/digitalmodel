# Custom asset-integrity calculations knowledge pack

| Header | Value |
| --- | --- |
| Module | asset_integrity/custom |
| Repository | digitalmodel |
| Package path | src/digitalmodel/asset_integrity/custom |
| Pack revision | 1; 2026-10-18 |
| Code revision | `334521c97a21c16950046f52b61586e8e8465787` |
| Analysis family | EC; Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT; independent review pending |

## 1. Purpose and scope

`PipeSizing` mutates configuration dictionaries with annular section geometry, material stiffness and mass per length; `PipeCapacity` dispatches internal pressure, external pressure and propagation checks for outer and inner pipes. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L12-L59] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L62-L101] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L19-L43]

`MaterialProperties` sets temperature factors and per-design mass entries; `common.pipe_components.PipeComponents.evaluate_pipe_capacity` calls sizing, material evaluation and capacity in that order. [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L14-L45] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L96-L114] [src/digitalmodel/asset_integrity/common/pipe_components.py:L60-L70]

The registered domain maturity is `stable`; this is domain routing metadata, not qualification of these legacy calculations. [docs/registry/module-routing.yaml:L15-L30]

Scope includes both `API579` and `ASMEB31` helper folders: they read spreadsheets, draw plots and serialize results; the API579 helper label does not establish a fitness-for-service assessment algorithm here. [src/digitalmodel/asset_integrity/custom/API579/customInputs.py:L9-L17] [src/digitalmodel/asset_integrity/custom/API579/plotContourf.py:L9-L52] [src/digitalmodel/asset_integrity/custom/API579/saveData.py:L9-L32] [src/digitalmodel/asset_integrity/custom/ASMEB31/saveData.py:L9-L29]

Use the single-pipe geometry path as the checked entry point; the two-pipe accumulator, legacy coating helpers and stress sizing have the limitations recorded in section 6. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L80-L96] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L124-L136] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L760]

## 2. Inputs and outputs

Input contract below describes accessed keys; numeric ranges are proposed preflight requirements, not validation performed by this package. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L22-L48] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L297-L341]

| Input | Unit / allowed range | Default and route | Evidence |
| --- | --- | --- | --- |
| `Outer_Pipe`, `Inner_Pipe` | Pipe configuration or `None`; main path requires an outer pipe | Explicit keys; inner pipe disabled with `None` | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L12-L19] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L80-L96] |
| `Geometry.Nominal_OD`, `Nominal_ID`, `Design_WT` | Common length unit; require OD > ID >= 0, WT > 0, OD - ID = 2 WT | One missing value derived from the other two; inconsistent triples not rejected | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L22-L48] |
| `Material`, `Material_Grade`, `E`, `G`, `Rho`, `Poissionsratio`, `Grades` | Consistent length/force/mass basis; E > 0, density > 0, -1 < Poisson ratio < 0.5 | G derived if lookup raises; grade strengths copied | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L62-L78] |
| `Design`, `Code`, `Load Condition`, `DesignFactors` | Indexed load cases; supported selector strings only | Caller supplies dictionaries and factors; no fallback selector | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L25-L43] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L220] |
| `InternalPressure`, `ExternalPressure`, strengths | psi for pressure-capacity fixtures; require finite nonnegative pressure and positive strengths | Scalar pressures for burst; propagation minimum instead accesses density/column mapping | [tests/asset_integrity/test_calculations.py:L136-L174] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L417-L434] |
| `Corrosion_Allowance` | Same length unit; require 0 <= allowance < WT | Required; subtracted for second evaluation | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L45-L74] |
| `BendingMoment`, `AxialForce`, `EffectiveTension` | Moment conversion is hard-coded; force input explicitly converted from kN to lbf | Exactly one force selector must be `None`; problematic reconstruction recorded in section 6 | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L709-L721] |
| `InternalFluid`, `ExternalFluid`, coupling ratio | Consistent density; per-length conversion is implicit | Required by mass evaluation; shared design-level `mass` key | [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L96-L114] |
| Spreadsheet / plot / export dictionaries | Sheet, index, columns, labels and filenames supplied by caller | No physical-unit metadata added by serializers | [src/digitalmodel/asset_integrity/custom/API579/customInputs.py:L9-L17] [src/digitalmodel/asset_integrity/custom/API579/plotCustom.py:L6-L42] [src/digitalmodel/asset_integrity/custom/ASMEB31/saveData.py:L9-L24] |

| Output | Unit and destination | Evidence |
| --- | --- | --- |
| `section_properties.pipe` | A, Ao, Ai in length squared; I, J in length to fourth power; EI, EA, GJ and density times area inherit caller units | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L78] |
| `equivalent_pipe` | Single-pipe section dictionary aliases outer-pipe properties; returned by `get_pipe_system_properties` | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L80-L101] |
| `Result[pipe][condition][code]` | `thickness`, `minimum_thickness` in input length; `Design_WT_Max_Pressure` in input stress unit, keyed by corrosion tag | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L138-L183] |
| `Design[*].mass` | Pipe, coupling, fluid, dry, buoyancy and wet values; factor converts length without a unit object | [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L96-L114] |
| Legacy `equivalentPipe` | Mass and submerged weight per length; converts squared inch geometry to squared metre before multiplying densities and gravity | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L151-L187] |
| Files | JSON, YAML and CSV preserve caller data values; plot labels/origin are caller supplied | [src/digitalmodel/asset_integrity/custom/API579/saveData.py:L9-L25] [src/digitalmodel/asset_integrity/custom/ASMEB31/saveData.py:L9-L24] [src/digitalmodel/asset_integrity/custom/API579/plotContourf.py:L16-L46] |

There is no coordinate/phase calculation in the section-property path; plotting uses dataframe columns as X and index as Y, with a configurable image origin and row reversal. Positive pressure enters the stress path with negative radial stress; submerged weight subtracts displaced-fluid mass before gravity. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L59] [src/digitalmodel/asset_integrity/custom/API579/plotContourf.py:L9-L46] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L739-L756] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L157-L173]

## 3. Method and governing standards

The following are implemented arithmetic routes and selector editions, not an assertion that the licensed provisions have been verified; clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L220] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L345-L402]

| Method / requested reference | Implementing definitions | Edition / clause trace and limit |
| --- | --- | --- |
| Annular geometry and linear elastic properties | `pipe_section_properties`, `sectionProperties`, `get_fea_properties`, `system_properties`, `equivalentPipe` | Algebraic area/inertia and material products; no standards clause identified in these bodies. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L101] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L140-L189] |
| ASME B31 modified Barlow | `evaluate_burst_pressure_modified_burlow_equation`, `evaluate_burst_minimum_thickness_modified_burlow_equation` | Substring dispatch; tests identify B31.4-2016 Chapter IX Platform Piping and B31.8-2016 Chapter VIII Pipeline; clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L188-L192] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L297-L343] [tests/asset_integrity/test_calculations.py:L148-L174] [tests/asset_integrity/test_calculations.py:L186-L210] |
| API STD 2RD | `evaluate_burst_pressure_API_STD_2RD`, `evaluate_burst_minimum_thickness_API_STD_2RD`, `evaluate_collapse_pressure_API_STD_2RD`, `evaluate_collapse_minimum_thickness_API_STD_2RD` | Selector 2013; logarithmic burst/inverse and elastic/plastic collapse blend; inverse collapse is a placeholder. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L194-L198] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L222-L231] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L281-L295] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L361-L391] |
| API RP 1111 | `evaluate_burst_pressure_API_RP_1111`, `evaluate_burst_minimum_thickness_API_RP_1111`, `evaluate_collapse_propagation_pressure_API_RP_1111`, `evaluate_collapse_propagation_minimum_thickness_API_RP_1111` | Selector 2009; diameter/thickness branch, inverse and propagation power law; clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L200-L204] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L233-L279] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L393-L434] |
| API RP 16Q | `API_RP_16Q.get_burst_pressure`, `get_burst_minimum_thickness` delegate to `VonMises_Pipe` | Second edition, April 2017 recorded by constructor; stress evaluation and sampled interpolation are code routes, not an independent check. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L607-L625] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L723-L826] |
| API 5L | Factor dictionary key inside 2RD burst/inverse | No product acceptance or grade-table lookup here; caller supplies factor and strengths. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L222-L230] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L281-L293] |
| API 579 | Spreadsheet, contour, heatmap and serialization helpers | No edition or assessment clause in helper bodies; top-level API579 assessment belongs outside this package. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/API579/customInputs.py:L1-L17] [src/digitalmodel/asset_integrity/custom/API579/plotHeatMap.py:L12-L55] [src/digitalmodel/asset_integrity/API579.py:L6-L24] |
| API TR 5C3 | `API_TR_5C3.get_collapse_pressure`, `get_collapse_minimum_thickness` | Second edition, June 2018 constructor; elastic formula with fixed pressure coefficient and interpolation, not a full multi-regime casing check. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L628-L686] |
| Legacy additional checks | `OtherMethodsTobeIncorporated.ASMEB314InternalPressure`, `ASMEB314LogitudinalStress`, `ASMEB314EquivalentStress`, `ASMEB318InternalPressure`, `ASMEB318LogitudinalStress`, `ASMEB318EquivalentStress` | Restrained thermal/longitudinal and equivalent-stress routines; several stress components fixed to zero. Clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L441-L598] |
| Additional pressure and stress routes | `CFR_30_Part_250.get_burst_pressure`, `get_burst_minimum_thickness`; `VonMises_Pipe.init_calculate_required_properties`, `get_stress`, `get_burst_pressure`, `get_burst_minimum_thickness` | Configured factored hoop route and sampled stress inversions; axial pressure reconstruction is suspect, so these paths must not be used as hand references. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L826] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L829-L867] |
| Material derating | `temperatureDerating`, `set_up_temperature_derating`, `evaluate_mass_and_weight_properties` | Recognized standards receive fixed derating; the `Other` branch changes supplied strengths; no verified temperature applicability envelope. [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L24-L87] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L96-L114] |

The documentation repeats the 2013 2RD and 2009 RP 1111 selectors but points at an older package location; the register lists newer RP 1111 and B31 editions for separate structural strategies. These editions must not be treated as interchangeable; provenance follow-up: issue pending (1), with the cross-module edition history also tracked by #2161. [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L29-L36] [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L80-L85] [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L139-L146] [docs/domains/codes-register.md:L30-L44] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L188-L204]

UNVERIFIED: publication titles, precise clause/table mappings and edition-specific conformity for the requested standards, including helper-only API 579 and factor-only API 5L, remain to be established; no licensed text or tabulated value is reproduced. No `Citation` sidecar is emitted by the audited calculation methods; issue pending (1). [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L222-L295] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L361-L434] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L47-L87]

## 4. Validated worked examples

Validated examples: 0. The selected cases below are regression examples because their expected values recompute the same expressions; the passing pressure tests do not establish licensed-standard conformity. [tests/asset_integrity/test_pipe_sizing.py:L297-L319] [tests/asset_integrity/test_calculations.py:L170-L174] [tests/asset_integrity/test_calculations.py:L207-L210]

All input data for these cases are synthetic dictionaries in the named test, not a measured dataset or file-based worked example. The recorded offline result is PASS at the code pin, before and after this documentation change. [tests/asset_integrity/test_pipe_sizing.py:L23-L60] [tests/asset_integrity/test_pipe_sizing.py:L297-L319] [tests/asset_integrity/test_pipe_sizing.py:L208-L212] [tests/asset_integrity/test_pipe_sizing.py:L268-L274]

| Case | Input / expected value and tolerance | Test node ID | Offline result |
| --- | --- | --- | --- |
| R1 annular area | OD 0.4064 m, ID 0.3556 m; expected `pi/4*(OD**2-ID**2)` m²; relative tolerance 1e-6 | `tests/asset_integrity/test_pipe_sizing.py::TestSectionPropertiesHelper::test_area_formula` | PASS / PASS. [tests/asset_integrity/test_pipe_sizing.py:L297-L301] |
| R2 bending inertia | Same diameters in m; expected `pi/64*(OD**4-ID**4)` m⁴; relative tolerance 1e-6 | `tests/asset_integrity/test_pipe_sizing.py::TestSectionPropertiesHelper::test_moment_of_inertia_formula` | PASS / PASS. [tests/asset_integrity/test_pipe_sizing.py:L315-L319] |
| R3 flexural stiffness | OD 0.3239 m, wall 0.01430 m; expected elastic modulus times section inertia in N m²; relative tolerance 1e-6 | `tests/asset_integrity/test_pipe_sizing.py::TestFEAProperties::test_ei_is_e_times_i` | PASS / PASS. [tests/asset_integrity/test_pipe_sizing.py:L203-L212] |
| R4 single-pipe aggregation | OD 0.3239 m, wall 0.01430 m; equivalent area equals outer annulus in m²; relative tolerance 1e-5 | `tests/asset_integrity/test_pipe_sizing.py::TestGetPipeSystemProperties::test_equivalent_pipe_inherits_outer_pipe_properties` | PASS / PASS. [tests/asset_integrity/test_pipe_sizing.py:L268-L274] |

The pressure fixture tests exercise ASME B31.4/B31.8, RP 1111 burst/propagation and STD 2RD burst; the standalone formula tests near the end of the mixed calculation file do not call the package and cannot establish coverage of its collapse methods. [tests/asset_integrity/test_calculations.py:L133-L389] [tests/asset_integrity/test_calculations.py:L917-L1001]

UNVERIFIED: an independently sourced, edition-qualified pressure/stress benchmark for this package is not established by the selected regression tests; issue pending (2). [tests/asset_integrity/test_calculations.py:L170-L174] [tests/asset_integrity/test_calculations.py:L310-L313] [tests/asset_integrity/test_calculations.py:L350-L351]

## 5. QA gates

Gate applicability follows AQG r0, *Analysis QA gates*, revision 0, sections 9–10, Python calculations: G0 inputs/units/citations, G1 Python imports/tests, G2 hand or published checks, G5 evidence; G3 and G4 are NOT_APPLICABLE because this path is calculation/plot/export code rather than a dynamics or diffraction model. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L78] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L19-L43]

These are reproducible checks of the stated surface, not a blanket qualification of every dispatch route. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L220] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L381-L391]

| Check | Gate | Execution and numeric pass criterion | Evidence and recorded result |
| --- | --- | --- | --- |
| C01 | G0 | Run `TestGeometryDerivation` in the sizing test file; OD - ID - 2 WT within relative tolerance 1e-10 in the consistent input length unit; 0 failed assertions, count | PASS; three missing-value cases only, not invalid-input rejection. [tests/asset_integrity/test_pipe_sizing.py:L156-L192] |
| C02 | G0 | Run `TestFEAProperties.test_shear_modulus_derived_correctly`; derived GJ versus E/(2*(1+nu))*J within relative tolerance 1e-5 N m²; 0 failed assertions, count | PASS for missing G only; arbitrary exceptions and pre-existing invalid G remain undetected. [tests/asset_integrity/test_pipe_sizing.py:L225-L231] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L65-L72] |
| C03 | G1 | Import the custom sizing and pressure classes by running the two test files; exactly 61 collected tests and 0 failures, errors or unexpected skips, count | BASE and AFTER: 61 passed; 32 sizing tests and 29 mixed calculation tests, including 7 direct custom-pressure cases. [tests/asset_integrity/test_pipe_sizing.py:L66-L330] [tests/asset_integrity/test_calculations.py:L133-L389] [tests/asset_integrity/test_calculations.py:L396-L1001] |
| C04 | G2 | Run R1 and R2, comparing `sectionProperties` outputs with the test's analytical annulus expressions: relative error <= 1e-6 in m² and m⁴ | PASS; same-expression regression only. [tests/asset_integrity/test_pipe_sizing.py:L297-L319] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L140-L149] |
| C05 | G2 | Independently integrate concentric-ring area as `integral(2*pi*r dr, ID/2, OD/2)` using numerical quadrature at R1 geometry, compare with `sectionProperties`; absolute difference <= 1e-12 m² | PASS in an offline evidence script; checks geometry only. [tests/asset_integrity/test_pipe_sizing.py:L297-L301] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L140-L149] |
| C06 | G3 | NOT_APPLICABLE: no dynamic model or time integration produced by these calculation methods | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L78] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L19-L43] |
| C07 | G4 | NOT_APPLICABLE: no diffraction panels or hydrodynamic coefficients produced by these methods | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L78] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L19-L43] |
| C08 | G5 | Retain the pin, command, counts, case IDs and outcomes; 0 unmatched section-4 node IDs and exactly 2 passing records per listed case, count; attach input SHA-256 for exported evidence | PASS for the four pack case records; this package's exporters themselves emit 0 gate/signature records and require an external evidence wrapper. [tests/asset_integrity/test_pipe_sizing.py:L297-L319] [tests/asset_integrity/test_pipe_sizing.py:L208-L212] [tests/asset_integrity/test_pipe_sizing.py:L268-L274] [src/digitalmodel/asset_integrity/custom/API579/saveData.py:L9-L25] |

Reproduction: `PYTHONPATH=src uv run --no-sources python -m pytest tests/asset_integrity/test_pipe_sizing.py tests/asset_integrity/test_calculations.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override. Both runs use the header code pin, with the second run on the uncommitted documentation tree. Total per run: 61 run, 61 passed, 0 failed, 0 skipped, 0 NOT_RUN; solver imports are not needed by these nodes. [tests/asset_integrity/test_pipe_sizing.py:L1-L11] [tests/asset_integrity/test_calculations.py:L133-L389]

## 6. Known failure modes and pitfalls

Silent-failure sweep covers every Python file in the package, including both helper folders. Each row is a distinct fallback, implicit result, unit assumption or skipped output; geometric constants, loop indices, color/style settings and initialized output dictionaries are not error defaults. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L8-L10] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L59] [src/digitalmodel/asset_integrity/custom/API579/testcontour.py:L7-L19]

sweep: 37 found

| ID | Symptom / cause | Detection | Tracking and evidence at pin |
| --- | --- | --- | --- |
| S01 | Any exception reading G silently triggers isotropic reconstruction | C02 detects the missing-G arithmetic only; no detection check exists for other exceptions | issue pending (3); [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L65-L68] |
| S02 | Plot reference-line exception silently switches to the outer-pipe dictionary form | no detection check exists | issue pending (7); [src/digitalmodel/asset_integrity/custom/API579/plotCustom.py:L26-L31] |
| S03 | ASME substring derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L61-L63] |
| S04 | exact B31.8 branch shadowed by ASME substring derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L65-L67] |
| S05 | STD 2RD derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L69-L71] |
| S06 | RP 1111 derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L73-L75] |
| S07 | RP 16Q derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L77-L79] |
| S08 | TR 5C3 derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L81-L83] |
| S09 | regulatory selector derating is fixed at unity without checking temperature | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L85-L87] |
| S10 | Unrecognized temperature code implicitly returns None | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L47-L87] |
| S11 | Material lookup method is a pass-only stub returning None | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L89-L94] |
| S12 | Buoyancy factor/diameter method is a pass-only stub returning None | no detection check exists | issue pending (3); [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L193-L194] |
| S13 | Unsupported load-condition selector produces no calculation and logs completion before dispatch | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L25-L43] |
| S14 | STD 2RD inverse collapse returns a constant placeholder instead of solving wall thickness | no detection check exists; #59 records external-pressure concerns | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L381-L391] |
| S15 | B31.4 longitudinal returns None for a condition other than Restrained | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L463-L490] |
| S16 | B31.4 equivalent returns None for a condition other than Restrained | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L492-L519] |
| S17 | B31.8 longitudinal returns None for a condition other than Restrained | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L543-L569] |
| S18 | B31.8 equivalent returns None for a condition other than Restrained | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L571-L598] |
| S19 | B31.4 longitudinal fixes bending/axial stress components to zero; equivalent path also fixes torsion | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L469-L473] |
| S20 | B31.4 equivalent fixes bending/axial stress components to zero; equivalent path also fixes torsion | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L498-L502] |
| S21 | B31.8 longitudinal fixes bending/axial stress components to zero; equivalent path also fixes torsion | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L549-L552] |
| S22 | B31.8 equivalent fixes bending/axial stress components to zero; equivalent path also fixes torsion | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L577-L581] |
| S23 | TR 5C3 interpolation has no out-of-range rejection and ignores its pressure override in the interpolation target | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L656-L686] |
| S24 | Von Mises thickness interpolation has no bounds or monotonicity acceptance check | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L762-L791] |
| S25 | Von Mises pressure interpolation has no bounds or monotonicity acceptance check | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L793-L826] |
| S26 | Low-pressure Von Mises search substitutes a fixed pressure interval with zero lower bound | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L800-L810] |
| S27 | Bare geometry/material products assume consistent units without validation | C01/C02/C04 check only valid synthetic inputs; no detection check exists for mixed units | issue pending (3); [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L78] |
| S28 | Legacy equivalentPipe assumes inch geometry and metric density before its fixed area conversion | no detection check exists | issue pending (3); [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L151-L187] |
| S29 | Mass route multiplies geometry/density by an implicit length conversion; inner-pipe call overwrites design-level mass | no detection check exists | issue pending (4); [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L14-L22] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L96-L114] |
| S30 | Propagation minimum assumes a density/column mapping and implicit length conversion, unlike scalar burst pressure | no detection check exists | issue pending (5); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L417-L434] |
| S31 | Moment input is converted using unlabelled hard-coded multipliers | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L709-L709] |
| S32 | Effective-tension reconstruction multiplies pressures by diameters rather than section areas | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L710-L713] |
| S33 | Axial-force reconstruction mixes converted force with raw input and pressure-diameter terms | no detection check exists | issue pending (6); [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L714-L717] |
| S34 | Heatmap stack drops missing table entries before coloring, without a retained-count check | no detection check exists | issue pending (7); [src/digitalmodel/asset_integrity/custom/API579/plotHeatMap.py:L29-L35] |
| S35 | Contour-line display uses image index coordinates while contour coordinates come from dataframe labels | no detection check exists | issue pending (7); [src/digitalmodel/asset_integrity/custom/API579/plotContourf.py:L10-L29] |
| S36 | API579 default plot labels assume pressure in psi and wall length in inches | no detection check exists | issue pending (7); [src/digitalmodel/asset_integrity/custom/API579/plotDefault.py:L21-L26] |
| S37 | ASMEB31 default plot labels assume pressure in psi and wall length in inches | no detection check exists | issue pending (7); [src/digitalmodel/asset_integrity/custom/ASMEB31/plotDefault.py:L21-L26] |

Sweep commands (run at the pin):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(0|0\.0|1970|0\.05)\b|int\(|float\(|interp|unit|density|Rho|column|0\.0254|\*12|\* 12' src/digitalmodel/asset_integrity/custom --glob '*.py'
rg -n 'return|pass|except|continue|stack|cache|Code|temperature|AxialForce|EffectiveTension' src/digitalmodel/asset_integrity/custom --glob '*.py'
```

The three `float(result)` conversions operate on numerical interpolation results, not external text; no locale-aware external-text conversion is performed there. Export functions write supplied data without an empty-result guard, but there is no cache write in those bodies. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L679-L680] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L784-L785] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L818-L819] [src/digitalmodel/asset_integrity/custom/API579/saveData.py:L9-L25] [src/digitalmodel/asset_integrity/custom/ASMEB31/saveData.py:L9-L24]

| Other pitfall | Detection / tracking | Evidence |
| --- | --- | --- |
| Two-pipe accumulation indexes an uninitialized nested equivalent-pipe dictionary | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L8-L10] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L80-L96] |
| Coating and buoyancy wrappers call an undefined unqualified `sectionProperties` | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L124-L136] |
| Unknown pressure codes leave return variables undefined; both force selectors set raises a string | no detection check exists; issue pending (5) and issue pending (6) | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L220] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L345-L359] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L718-L721] |
| Stress evaluation imports the broad legacy data module and hardcodes Outer_Pipe even for inner-pipe calls | no detection check exists; issue pending (6) | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L723-L737] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L758-L758] |
| `Other` derating setup provides only Code, although the branch needs temperature and strength fields | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L24-L30] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L47-L60] |
| Burst branch selection uses nominal WT instead of the evaluated corrosion-reduced thickness; inverse lacks branch re-evaluation | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L61-L64] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L235-L249] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L306-L315] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L328-L341] |
| Plot demonstration executes a network dataset lookup and file save at import; other demonstration calls show at module scope | no detection check exists; issue pending (7); not imported for the offline arithmetic checks | [src/digitalmodel/asset_integrity/custom/API579/plotContour.py:L3-L18] [src/digitalmodel/asset_integrity/custom/API579/testcontour.py:L28-L32] |

Issue #59 records incomplete basic-test coverage and an external-pressure concern; the confirmed inverse-collapse placeholder and pressure-shape mismatch above have no detection check in the 61-node suite. Cross-module edition concern #2161 is contextual; no edition is silently upgraded here. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L381-L434] [tests/asset_integrity/test_calculations.py:L133-L389] [docs/domains/codes-register.md:L30-L44]

## 7. Lessons log

Append-only record; shared-log references are not assigned by this document.

| Date | Change reference | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack revision | Separate direct implementation tests from extracted formulas; record unsafe stress/iteration paths and avoid using them as hand references | [tests/asset_integrity/test_calculations.py:L133-L174] [tests/asset_integrity/test_calculations.py:L956-L1001] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L826] | Unassigned; issue pending (2) |

## 8. Ownership

Proposed review assignments: engineering calculations quality manager; deputy engineering calculations quality manager; producing role engineering calculations maintainer. Route disputed method classifications and missing clause provenance to the family quality manager; route scope and issue disposition to the maintainers. Independent clearance remains pending.

## 9. Dependencies and implementation inventory

The main caller is `common.pipe_components`, which prepares material fields, evaluates sizing and derating, calls wall checks and saves YAML; the custom methods depend on `digitalmodel.units.Q_` and `common.update_deep.update_deep_dictionary`. [src/digitalmodel/asset_integrity/common/pipe_components.py:L3-L5] [src/digitalmodel/asset_integrity/common/pipe_components.py:L42-L73] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L10-L11] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L5-L6]

NumPy is loaded by interpolation methods, while the stress route loads `common.data.AttributeDict` and calls custom `PipeSizing`; spreadsheet input uses pandas plus `common.yml_utilities.WorkingWithYAML`; helper outputs use matplotlib, seaborn, Bokeh, JSON and YAML. These are Python dependencies, not solver launch requirements in the exercised arithmetic path. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L656-L680] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L723-L735] [src/digitalmodel/asset_integrity/custom/API579/customInputs.py:L3-L17] [src/digitalmodel/asset_integrity/custom/API579/plotContour.py:L3-L18] [src/digitalmodel/asset_integrity/custom/API579/plotHeatMap.py:L3-L9] [src/digitalmodel/asset_integrity/custom/API579/saveData.py:L3-L6]

Inventory mapping for downstream/upstream packages has no module-specific row in the routing registry beyond the parent domain and its existing submodules; adding dependency inventory metadata remains issue pending (8). [docs/registry/module-routing.yaml:L15-L30]

### Map basis and counted family

Map source: `docs/registry/implementation-map.yaml` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` from `origin/codex/implmap-work`; its named data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. Map citations in this section use that map pin; all other source citations use the header code pin. [docs/registry/implementation-map.yaml:L1-L3]

Exactly one family has in-package rows: `effective-tension-riser-stack-up`, with one row, `VonMises_Pipe.init_calculate_required_properties` in `PipeCapacity.py` at line 696; therefore it is the sole counted family. No helper subpackage is excluded. [docs/registry/implementation-map.yaml:L2596-L2606] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L721]

Search signals retained verbatim below; none matches `asset_integrity/custom` case-insensitively, so none is omitted. Python `re.search` is case-sensitive against every package `.py` line at the code pin, with each hit assigned to the smallest enclosing function/class (module statements form one definition). [docs/registry/implementation-map.yaml:L2141-L2147]

```python
r'effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)'
r'effective.tension|tensioner|stack.up'
r'internal_pressure|external_pressure|fluid_density|buoyancy_module'
```

| Matched definition in `PipeCapacity.py` | Classification and disposition | Evidence |
| --- | --- | --- |
| `PipeCapacity.evaluate_load_conditions` | Dispatch helper, not a second effective-tension implementation | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L25-L43] |
| `PipeCapacity.internal_pressure` | Wrapper and result update helper | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L45-L74] |
| `PipeCapacity.external_pressure` | Wrapper and result update helper | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L107-L136] |
| `PipeCapacity.internal_pressure_specification_code_based_evaluation` | Dispatch helper | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L220] |
| `PipeCapacity.external_pressure_specification_code_based_evaluation` | Dispatch helper | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L345-L359] |
| `PipeCapacity.evaluate_collapse_propagation_minimum_thickness_API_RP_1111` | Own propagation sizing arithmetic, listed in section 3; different result family | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L417-L434] |
| `API_TR_5C3.get_collapse_minimum_thickness` | Own collapse inversion, listed in section 3; different result family | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L656-L686] |
| `VonMises_Pipe.get_burst_minimum_thickness` | Own stress inversion, listed in section 3; different result family | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L762-L791] |
| `CFR_30_Part_250.get_burst_minimum_thickness` | Own hoop-pressure sizing, listed in section 3; different result family | [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L836-L852] |

Counted-family sweep result: 9 matched definitions, 0 further unlisted second implementations; the first two patterns have 0 hits. The known map row itself uses differently cased force keys, so the exact signals do not rediscover it; it is retained in the inventory and section 3 regardless. A8 self-check: PASS, subject to independent classification review. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L721] [docs/registry/implementation-map.yaml:L2596-L2606]

### Complete mapped family inventory

All 264 implementation rows follow in map-file order, including wrappers, tests and scripts as the map classifies them; listing is not independent qualification of those definitions. `None` denotes a module-level map row. [docs/registry/implementation-map.yaml:L2141-L2147]

| Row | Implementation path | Definition | Map evidence |
| --- | --- | --- | --- |
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` (line 45) | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` (line 44) | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` (line 47) | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` (line 48) | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` (line 140) | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` (line 187) | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` (line 317) | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` (line 353) | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` (line 273) | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` (line 293) | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` (line 563) | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` (line 777) | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` (line 826) | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `examples/demos/gtm/results_store_demo04.py` | `None` (line 52) | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` (line 79) | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` (line 127) | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` (line 342) | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` (line 51) | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` (line 115) | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` (line 219) | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `None` (line 36) | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` (line 171) | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` (line 222) | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` (line 34) | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` (line 75) | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` (line 48) | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` (line 38) | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` (line 418) | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` (line 27) | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` (line 69) | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` (line 90) | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` (line 116) | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` (line 39) | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` (line 6) | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` (line 205) | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` (line 26) | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` (line 10) | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` (line 10) | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` (line 50) | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` (line 84) | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` (line 151) | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` (line 210) | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `scripts/riser_database/build_tables.py` | `None` (line 24) | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (line 696) | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` (line 91) | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` (line 58) | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` (line 82) | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` (line 208) | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` (line 254) | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` (line 266) | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` (line 134) | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` (line 358) | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` (line 362) | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` (line 371) | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` (line 400) | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` (line 568) | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` (line 734) | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` (line 752) | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` (line 758) | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` (line 855) | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` (line 912) | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` (line 278) | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` (line 35) | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` (line 51) | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` (line 58) | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` (line 180) | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` (line 45) | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` (line 64) | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` (line 164) | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` (line 216) | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` (line 48) | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` (line 113) | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` (line 143) | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` (line 218) | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` (line 272) | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` (line 293) | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` (line 181) | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` (line 309) | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` (line 145) | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` (line 404) | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` (line 140) | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` (line 188) | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` (line 317) | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` (line 409) | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` (line 172) | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` (line 177) | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` (line 198) | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` (line 196) | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` (line 532) | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` (line 85) | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` (line 135) | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` (line 178) | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` (line 19) | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` (line 74) | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` (line 146) | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` (line 32) | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` (line 154) | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` (line 251) | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` (line 308) | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` (line 374) | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` (line 493) | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` (line 521) | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` (line 554) | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` (line 564) | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` (line 11) | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` (line 494) | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` (line 530) | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` (line 535) | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` (line 557) | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` (line 587) | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` (line 619) | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` (line 677) | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` (line 691) | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` (line 704) | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` (line 8) | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` (line 56) | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` (line 95) | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` (line 107) | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` (line 164) | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` (line 173) | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` (line 179) | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` (line 344) | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` (line 415) | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` (line 529) | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` (line 21) | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` (line 58) | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` (line 152) | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` (line 454) | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` (line 533) | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` (line 631) | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` (line 799) | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` (line 866) | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` (line 942) | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` (line 953) | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` (line 90) | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` (line 291) | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` (line 323) | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` (line 369) | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` (line 256) | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` (line 222) | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` (line 170) | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` (line 339) | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` (line 377) | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` (line 244) | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` (line 185) | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` (line 65) | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` (line 62) | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` (line 92) | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` (line 166) | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` (line 28) | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` (line 63) | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` (line 97) | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` (line 93) | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` (line 85) | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` (line 33) | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` (line 40) | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` (line 411) | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` (line 127) | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` (line 289) | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` (line 34) | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` (line 401) | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` (line 14) | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` (line 673) | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` (line 743) | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` (line 748) | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` (line 777) | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` (line 837) | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` (line 917) | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` (line 1048) | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` (line 1071) | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` (line 1110) | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` (line 59) | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` (line 435) | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` (line 161) | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` (line 268) | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` (line 41) | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` (line 60) | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` (line 66) | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` (line 141) | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` (line 156) | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` (line 186) | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` (line 290) | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` (line 81) | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` (line 101) | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` (line 355) | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` (line 458) | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` (line 506) | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` (line 566) | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` (line 83) | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` (line 255) | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (line 985) | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` (line 342) | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (line 1272) | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` (line 31) | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` (line 170) | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` (line 34) | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` (line 42) | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` (line 103) | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` (line 174) | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` (line 396) | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` (line 64) | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` (line 91) | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` (line 109) | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` (line 160) | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` (line 178) | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` (line 294) | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` (line 44) | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` (line 125) | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` (line 27) | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` (line 28) | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` (line 41) | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` (line 50) | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` (line 120) | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` (line 297) | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` (line 325) | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` (line 374) | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` (line 403) | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` (line 59) | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` (line 164) | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` (line 248) | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` (line 288) | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` (line 63) | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` (line 74) | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` (line 257) | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` (line 34) | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` (line 29) | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` (line 32) | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `None` (line 52) | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` (line 454) | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` (line 42) | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` (line 77) | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` (line 65) | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` (line 43) | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` (line 17) | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` (line 17) | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` (line 281) | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` (line 24) | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` (line 20) | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` (line 44) | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` (line 48) | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` (line 57) | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` (line 6) | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` (line 215) | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` (line 84) | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` (line 24) | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` (line 17) | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` (line 13) | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` (line 39) | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` (line 22) | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` (line 21) | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` (line 126) | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` (line 8) | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` (line 165) | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` (line 26) | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` (line 89) | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` (line 23) | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` (line 13) | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` (line 97) | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `None` (line 12) | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` (line 25) | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` (line 117) | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` (line 22) | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` (line 56) | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` (line 17) | [docs/registry/implementation-map.yaml:L4908-L4917] |

### Repository search and map follow-up

The repository-wide search includes `scripts/` and distinguishes independent section-property copies in structural, specialized and pipeline code from the two methods inside this package; the legacy script also calculates annulus properties itself. These are map follow-up findings outside the counted family's result and do not change A8. [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L42-L63] [src/digitalmodel/specialized/custom/PipeSizing.py:L40-L61] [src/digitalmodel/subsea/pipeline/pipe_sizing.py:L42-L63] [scripts/python/digitalmodel/analysis/APISTD2RDMethods.py:L53-L65] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L59] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L140-L149]

```bash
rg -n 'def (init_calculate_required_properties|sectionProperties|pipe_section_properties|evaluate_burst_pressure_modified_burlow_equation|ASMEB314InternalPressure)' src scripts --glob '*.py'
```

Map follow-up: add module-specific families/rows for annular geometry, stiffness/mass aggregation, pressure containment and inverses, collapse/propagation, temperature derating and combined stress; existing map families without in-package rows are not counted yet. Plotting/serialization helpers remain helpers rather than extra calculation implementations. Track the mapping and dependency inventory under issue pending (8). [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L38-L101] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L222-L434] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L723-L826] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L47-L114]

## 10. Coverage and open items

Read inventory: all 16 Python files under the header package, including the two empty `__init__.py` files; no package README or pre-existing package docs were present at the pin. Both helper folders remain in scope. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L1-L195] [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L1-L114] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L1-L867]

Read discovery documents are listed below with the relevant locators; historical counts and claims are not adopted as qualification. Some documents concern different copies of similarly named modules. [docs/domains/testing/coverage_analysis_report.md:L30-L36] [tests/specialized/custom/test_pipesizing.py:L29-L34] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L23-L32]

| Discovery document | Relevant coverage / locator |
| --- | --- |
| `docs/registry/module-routing.yaml` | Parent-domain maturity and routing [docs/registry/module-routing.yaml:L15-L30] |
| `docs/registry/implementation-map.md` and map YAML at separate pin | Family index and implementation inventory [docs/registry/implementation-map.md:L18-L18] |
| `docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md` | Formula summary and stale module route [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L29-L36] |
| `docs/domains/codes-register.md` | Legacy collapse pointer and cross-module editions [docs/domains/codes-register.md:L30-L44] [docs/domains/codes-register.md:L107-L113] |
| `docs/migration/aceengineercode-migration.md` | Migration claim only [docs/migration/aceengineercode-migration.md:L54-L60] |
| `docs/domains/testing/coverage_analysis_report.md` | Historical sizing counts, not current test evidence [docs/domains/testing/coverage_analysis_report.md:L30-L36] |
| `docs/domains/testing/engine_test_implementation_plan.md` | Separate structural capacity route [docs/domains/testing/engine_test_implementation_plan.md:L375-L384] |
| `docs/domains/project-docs/FINAL_TEST_BASELINE.md` | Historical copied sizing module [docs/domains/project-docs/FINAL_TEST_BASELINE.md:L97-L104] |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | Historical copied sizing module [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L56-L62] |
| `docs/domains/guides/user_stories/modernize_batch_files_with_uv.md` | Historical environment setup, not a calculation specification [docs/domains/guides/user_stories/modernize_batch_files_with_uv.md:L50-L58] |
| `src/digitalmodel/asset_integrity/docs/agent-pack.md` | Parent pack separates its own top-level scope [src/digitalmodel/asset_integrity/docs/agent-pack.md:L1-L27] |

The direct module-test discovery found two files: `tests/asset_integrity/test_pipe_sizing.py` (32 nodes) and `tests/asset_integrity/test_calculations.py` (29 nodes, of which 7 call custom pressure methods). The latter also checks common utilities, extracted API579 formula copies and FAD; those are not extra custom implementation tests. [tests/asset_integrity/test_pipe_sizing.py:L66-L330] [tests/asset_integrity/test_calculations.py:L133-L389] [tests/asset_integrity/test_calculations.py:L396-L1001]

The similarly named specialized, engineering-validation and structural pressure suites import other packages and were inspected as discovery leads, not counted as this module's test evidence. No tests directory exists inside this custom package; the plotting demonstration has no `test_` function. [tests/specialized/custom/test_pipesizing.py:L29-L34] [tests/engineering_validation/pipe_sizing/test_formula_validation.py:L13-L17] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L23-L32] [src/digitalmodel/asset_integrity/custom/API579/testcontour.py:L1-L32]

| Open item | Responsible role / tracking | Evidence / disposition |
| --- | --- | --- |
| Unverified standards titles, clauses and edition conformity; no legacy output citation sidecars | Engineering calculations maintainer; issue pending (1); contextual #2161 | Section 3; clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L185-L295] |
| Independent pressure/stress benchmark, shared lessons reference and external evidence wrapper | Engineering calculations maintainer; issue pending (2) | Section 4 remains regression-only. [tests/asset_integrity/test_calculations.py:L170-L174] [tests/asset_integrity/test_calculations.py:L956-L1001] |
| Geometry, units, aggregation and legacy sizing helpers | Engineering calculations maintainer; issue pending (3) | Section-6 rows for sizing and its explicit failures. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L62-L96] [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L124-L136] |
| Derating, material stub and mass bookkeeping | Engineering calculations maintainer; issue pending (4) | Temperature range and two-pipe mass contracts need tests. [src/digitalmodel/asset_integrity/custom/MaterialProperties.py:L24-L114] |
| Dispatch, incomplete pressure inverses, zeroed stress components and pressure input shape | Engineering calculations maintainer; issue pending (5); related #59 | No automatic detection in the selected suite. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L381-L598] |
| Combined-stress units/forces, interpolation and inner-pipe behavior | Engineering calculations maintainer; issue pending (6) | Do not use as a hand reference. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L656-L826] |
| Plotting/import safety, missing cells, fixed output routes and metadata | Engineering calculations maintainer; issue pending (7) | Helpers read but not exercised by the arithmetic suite. [src/digitalmodel/asset_integrity/custom/API579/plotHeatMap.py:L12-L55] [src/digitalmodel/asset_integrity/custom/API579/plotContour.py:L3-L18] [src/digitalmodel/asset_integrity/custom/ASMEB31/saveData.py:L23-L24] |
| Map families/dependency inventory and copied-method reconciliation | Engineering calculations maintainer; issue pending (8) | Section 9 inventory and follow-up; no code consolidation in this change. [src/digitalmodel/asset_integrity/custom/PipeSizing.py:L140-L149] [scripts/python/digitalmodel/analysis/APISTD2RDMethods.py:L53-L65] |

No client-derived values or private sidecar content are included; no private dataset is required by the selected synthetic test fixtures. [tests/asset_integrity/test_pipe_sizing.py:L23-L60] [tests/asset_integrity/test_calculations.py:L28-L127]
