# Geotechnical module knowledge pack

| Header | Value |
| --- | --- |
| Module | geotechnical |
| Repository | digitalmodel |
| Package and scope | `src/digitalmodel/geotechnical/`, including all submodules |
| Pack revision | 0 |
| Code revision | `bb18c1a346b31b4ce975befa41fc84d9a5ff29e5` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT; independent clearance pending |

## 1. Purpose and scope

The package computes axial pile resistance in clay and sand, drag-anchor holding, suction-caisson resistance, pipeline and monopile scour, rock-armour thickness, shallow-foundation bearing/sliding resistance and liquefaction-triggering screening. [src/digitalmodel/geotechnical/piles.py:L107-L302] [src/digitalmodel/geotechnical/anchors.py:L61-L163] [src/digitalmodel/geotechnical/scour.py:L53-L224] [src/digitalmodel/geotechnical/mudmat.py:L96-L226] [src/digitalmodel/geotechnical/liquefaction.py:L101-L156]

The database loader preserves printed source records, qualifiers, units and explicit reasons for unavailable numeric conversions; it does not select soil properties for a design. [src/digitalmodel/geotechnical/database.py:L69-L121] [src/digitalmodel/geotechnical/database.py:L154-L171]

The routing registry records maturity `beta`; the engine-facing adapters call the Python calculators and write JSON summaries. [docs/registry/module-routing.yaml:L131-L136] [src/digitalmodel/geotechnical/workflow.py:L48-L60] [src/digitalmodel/geotechnical/cfg_workflows.py:L20-L33]

The pile calculation is compressive axial resistance with tip bearing from the last layer; mudmat eccentricity acts along one explicitly selected plan direction; liquefaction consumes already-corrected SPT resistance rather than deriving it from field measurements. [src/digitalmodel/geotechnical/piles.py:L215-L242] [src/digitalmodel/geotechnical/piles.py:L285-L302] [src/digitalmodel/geotechnical/mudmat.py:L110-L119] [src/digitalmodel/geotechnical/liquefaction.py:L118-L130]

## 2. Inputs and outputs

Defaults below identify code locations rather than reproduce purported standard-table values; required inputs have no fallback at the cited call site. [src/digitalmodel/geotechnical/workflow.py:L65-L96] [src/digitalmodel/geotechnical/cfg_workflows.py:L223-L258]

| Input route | Names and units | Allowed range / default behavior | Source |
| --- | --- | --- | --- |
| Pile primitives | diameter, embedded length and layer thickness: m; Su and effective vertical stress: kPa; friction angle: degrees; K0, Nc, Nq: dimensionless | `axial_capacity` requires positive dimensions and nonempty layers; optional range validation is off by default; clay/sand fields are selected per layer; coefficients have code defaults | [src/digitalmodel/geotechnical/piles.py:L28-L68] [src/digitalmodel/geotechnical/piles.py:L148-L211] [src/digitalmodel/geotechnical/piles.py:L215-L295] |
| Composite pile | D, L: m; Su, sigma_v: kPa; multilayer tuples in top-to-bottom order | Multilayer list must be nonempty; geometric positivity and thickness-total consistency are not checked here | [src/digitalmodel/geotechnical/pile_capacity.py:L36-L141] |
| Anchors | weight in air: kN; diameter, embedment and wall thickness: m; Su: kPa; efficiency and adhesion/bearing factors: dimensionless | Enumerated soil/anchor keys and positive weight for drag; positive D, L, Su and positive inner diameter for suction; optional factors and wall thickness default in code | [src/digitalmodel/geotechnical/anchors.py:L61-L163] |
| Pipeline scour | diameter: m; current and wave orbital velocity: m/s; sediment diameter: mm; period: s; burial ratio: dimensionless | Positive diameter; burial ratio in [0, 1]; default burial is zero and default period is defined in code; sediment diameter is accepted but unused | [src/digitalmodel/geotechnical/scour.py:L17-L20] [src/digitalmodel/geotechnical/scour.py:L53-L123] |
| Monopile / armour | pile diameter, water depth, rock diameter: m; velocity: m/s; densities: kg/m³ | Positive pile diameter, depth and rock diameter; rock density must exceed water density; water depth has no effect after validation | [src/digitalmodel/geotechnical/scour.py:L126-L224] |
| Mudmat | B, L, embedment: m; V: kN; M: kN·m; strength/cohesion: kPa; submerged unit weight: kN/m³; soil/interface angles: degrees | Finite non-Boolean numerics; B and L positive with B ≤ L; embedment nonnegative; V positive; drained/undrained required fields; axis B by default; omitted M and effective cohesion are zero | [src/digitalmodel/geotechnical/mudmat.py:L45-L51] [src/digitalmodel/geotechnical/mudmat.py:L96-L169] |
| Liquefaction | pga_g: acceleration divided by g; magnitude: dimensionless; layer depth: m; total/effective stress: kPa; corrected N and safety/overburden factors: dimensionless | Positive PGA, magnitude, effective stress and required safety factor; nonnegative depth and corrected N; at least one layer; default factors are at the cited signatures | [src/digitalmodel/geotechnical/liquefaction.py:L58-L130] |
| Database | YAML mapping with meta, sources, parameters, gaps, excluded; printed values and printed-unit strings | Required string cells, unique IDs, known source/license/grade codes and read-status checks; no inferred numeric value for compound/qualified text | [src/digitalmodel/geotechnical/database_schema.py:L12-L79] [src/digitalmodel/geotechnical/database.py:L69-L121] [src/digitalmodel/geotechnical/database.py:L124-L171] |
| Configuration adapters | Named blocks, design factors and optional load demands; unit suffixes as above | `float()` conversion accepts numeric strings without locale normalization; output paths resolve relative to the configuration directory or working directory | [src/digitalmodel/geotechnical/workflow.py:L31-L43] [src/digitalmodel/geotechnical/workflow.py:L65-L105] [src/digitalmodel/geotechnical/cfg_workflows.py:L13-L42] [src/digitalmodel/geotechnical/cfg_workflows.py:L223-L258] |

| Output | Unit / representation | Meaning and convention | Source |
| --- | --- | --- | --- |
| Pile / anchor result dataclasses | kN capacities; kPa unit resistance; dimensionless factors; standard string | Component capacities are added; pile last layer supplies end bearing; suction includes both wall surfaces and reverse end bearing | [src/digitalmodel/geotechnical/piles.py:L71-L104] [src/digitalmodel/geotechnical/pile_capacity.py:L24-L33] [src/digitalmodel/geotechnical/anchors.py:L40-L58] [src/digitalmodel/geotechnical/anchors.py:L151-L163] |
| Scour / armour dataclasses | m depths/thickness; dimensionless KC, ratios, stability number | Zero-flow branches return zero scour; armour stability is returned without an acceptance criterion | [src/digitalmodel/geotechnical/scour.py:L23-L50] [src/digitalmodel/geotechnical/scour.py:L82-L89] [src/digitalmodel/geotechnical/scour.py:L154-L159] [src/digitalmodel/geotechnical/scour.py:L210-L224] |
| Mudmat dataclass | effective dimensions: m; area: m²; ultimate pressure: kPa; vertical/sliding resistance: kN | B/L labels retain original axes; factor calculation uses sorted effective dimensions; eccentricity uses abs(M)/V, so moment sign does not change area | [src/digitalmodel/geotechnical/mudmat.py:L27-L42] [src/digitalmodel/geotechnical/mudmat.py:L161-L224] |
| Liquefaction dataclasses | depth: m; CSR, CRR, rd, MSF and safety factors: dimensionless; Boolean layer verdict; overall status | Minimum safety factor governs; layer failure means safety factor below the required value | [src/digitalmodel/geotechnical/liquefaction.py:L33-L55] [src/digitalmodel/geotechnical/liquefaction.py:L130-L156] |
| Database records | Immutable mappings and tuples; optional numeric value or bounds with units | Conversion targets include Pa and N/m³, but also kPa, kPa/m and kN/m³; `in_si` is not a promise of coherent SI base units | [src/digitalmodel/geotechnical/database.py:L22-L28] [src/digitalmodel/geotechnical/database.py:L92-L104] [src/digitalmodel/geotechnical/database_schema.py:L82-L121] |
| Workflow summaries | UTF-8 JSON; updated configuration dictionaries; dimensionless utilization and PASS/FAIL strings | Summary writes occur before the returned path is inserted in the payload; configuration axes and magnitudes are scalar, with no coordinate transformation or phase convention | [src/digitalmodel/geotechnical/workflow.py:L38-L45] [src/digitalmodel/geotechnical/workflow.py:L93-L115] [src/digitalmodel/geotechnical/cfg_workflows.py:L20-L42] [src/digitalmodel/geotechnical/cfg_workflows.py:L250-L287] |

## 3. Method and governing standards

These are code-attributed methods, not independent confirmations of the named standards; clause text not checked against the licensed copy for every standard below. [src/digitalmodel/geotechnical/piles.py:L1-L10] [src/digitalmodel/geotechnical/anchors.py:L1-L14] [src/digitalmodel/geotechnical/scour.py:L1-L12] [src/digitalmodel/geotechnical/mudmat.py:L3-L18]

| Method / implementation | Reference as recorded; edition and locator | Evidence and limits |
| --- | --- | --- |
| Clay alpha, sand beta, tip pressure and layer summation: `skin_friction_clay`, `skin_friction_sand`, `end_bearing_clay`, `end_bearing_sand`, `axial_capacity` | API RP 2GEO; edition/title not recorded; code locators Sec 7.3, 8.2, 7.4, 8.3 and 7.2 respectively | Branching alpha is capped; sand uses a fixed interface-angle deduction; shaft resistance sums layer contributions and tip pressure uses the final layer. [src/digitalmodel/geotechnical/piles.py:L107-L211] [src/digitalmodel/geotechnical/piles.py:L257-L302] |
| Composite single-/multilayer clay pile: `alpha_method_capacity`, `alpha_method_capacity_multilayer` | API RP 2GEO; edition/title not recorded; code locator Sec 7.3 | Reuses clay unit-resistance functions but independently assembles geometry and layer totals. [src/digitalmodel/geotechnical/pile_capacity.py:L36-L141] |
| Drag holding: `drag_anchor_capacity` | DNV-RP-E302 / API RP 2SK; editions/titles not recorded; code attributes efficiency table to Table 3-1 of the former, no locator for the latter | Multiplies a soil/model lookup efficiency by air weight; table contents are not reproduced here. [src/digitalmodel/geotechnical/anchors.py:L13-L31] [src/digitalmodel/geotechnical/anchors.py:L61-L106] |
| Suction resistance: `suction_anchor_capacity` | DNV-RP-E303; edition/title/clause not recorded | Adds inner and outer wall adhesion plus reverse end bearing; returns a standard tag rather than a resolved Citation sidecar. [src/digitalmodel/geotechnical/anchors.py:L53-L58] [src/digitalmodel/geotechnical/anchors.py:L109-L163] |
| Pipeline/monopile scour and armour: `pipeline_scour_depth`, `monopile_scour_depth`, `rock_armour_thickness` | DNV-RP-F107, “Risk Assessment of Pipeline Protection”, 2019, Section 5 as attributed by code; Sumer and Fredsoe, “The Mechanics of Scour in the Marine Environment”, 2002 | Piecewise screening curves, fixed critical velocity and two-layer thickness; source suitability is unresolved in #2615. [src/digitalmodel/geotechnical/scour.py:L10-L12] [src/digitalmodel/geotechnical/scour.py:L61-L64] [src/digitalmodel/geotechnical/scour.py:L97-L116] [src/digitalmodel/geotechnical/scour.py:L161-L219] |
| Bearing factors, shape/depth factors, effective area, bearing and sliding: `bearing_capacity_factors`, `_shape_factors`, `_depth_factors`, `mudmat_bearing_capacity` | DNV-RP-C212, “Offshore soil mechanics and geotechnical engineering”; edition/clause not recorded; Brinch Hansen, “A revised and extended formula for bearing capacity”, 1970 | Code limits the depth-factor ratio by clipping; selected-axis effective area feeds separate vertical/sliding calculations. [src/digitalmodel/geotechnical/mudmat.py:L9-L18] [src/digitalmodel/geotechnical/mudmat.py:L54-L93] [src/digitalmodel/geotechnical/mudmat.py:L161-L209] |
| `stress_reduction_factor`, `magnitude_scaling_factor`, `cyclic_stress_ratio`, `cyclic_resistance_ratio_75`, `assess_liquefaction` | Seed and Idriss (1971); Youd et al., “Liquefaction Resistance of Soils”, 2001; Liao and Whitman (1986), as recorded in code | Piecewise rd, magnitude scaling, CSR/CRR and per-layer safety factor; finite CRR cap for nominally non-liquefiable layers remains part of the arithmetic. [src/digitalmodel/geotechnical/liquefaction.py:L3-L19] [src/digitalmodel/geotechnical/liquefaction.py:L58-L156] |

UNVERIFIED: U1 — normative correctness, edition selection and clause/table attribution of the six named standards, and independent qualification of the published-method formulas, remain unconfirmed; code tags do not provide that evidence; issue pending (1). [src/digitalmodel/geotechnical/piles.py:L1-L25] [src/digitalmodel/geotechnical/anchors.py:L13-L31] [src/digitalmodel/geotechnical/scour.py:L10-L12] [src/digitalmodel/geotechnical/mudmat.py:L16-L17] [src/digitalmodel/geotechnical/liquefaction.py:L18-L19]

The downstream atlas refresh metadata selects API RP 2GEO edition 2011, DNV-RP-E303 edition 2005 and DNV-RP-C212 edition 2021-09; those are consumer metadata, not editions established by the calculator docstrings; qualify the association under issue pending (1). [src/digitalmodel/parametric/refresh.py:L42-L50]

The calculator result schemas and summary writers carry string standard tags but no per-formula Citation objects or citation sidecar; reference qualification is part of issue pending (1). [src/digitalmodel/geotechnical/piles.py:L71-L104] [src/digitalmodel/geotechnical/anchors.py:L40-L58] [src/digitalmodel/geotechnical/mudmat.py:L27-L42] [src/digitalmodel/geotechnical/workflow.py:L93-L115] [src/digitalmodel/geotechnical/cfg_workflows.py:L311-L328]

## 4. Validated worked examples

Only the following two geometric checks are classified as validated: their expected areas are independently evaluated rectangular-area arithmetic, not a validation of soil coefficients or standard compliance. [tests/geotechnical/test_mudmat_eccentricity.py:L10-L20] [tests/geotechnical/test_mudmat_eccentricity.py:L44-L56]

| Case / source | Inputs / expected quantity / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| V1: selected default axis; closed form A = (B − 2 abs(M)/V)L | Function-local fixture; effective area 8.0 m²; pytest default relative tolerance 1e-6 and absolute 1e-12 m² | `tests/geotechnical/test_mudmat_eccentricity.py::test_default_axis_preserves_width_eccentricity` | PASS at BASE and AFTER; [tests/geotechnical/test_mudmat_eccentricity.py:L10-L20] [tests/geotechnical/test_mudmat_eccentricity.py:L44-L45] |
| V2: long-axis reduction; closed form A = B(L − 2 abs(M)/V) | Function-local fixture; effective length 0.8 m and area 2.4 m²; same pytest default tolerance in the respective units | `tests/geotechnical/test_mudmat_eccentricity.py::test_length_eccentricity_can_exceed_half_original_width` | PASS at BASE and AFTER; geometric assertion only is independently qualified here; [tests/geotechnical/test_mudmat_eccentricity.py:L48-L56] |

The regression cases below compare code formulas, component identities or pinned assertions; a PASS records execution, not independent geotechnical qualification. [tests/geotechnical/test_pile_capacity_dark_intel.py:L5-L19] [tests/workflows/test_durable_workflows.py:L1199-L1275] [tests/workflows/test_durable_workflows.py:L2093-L2137] [tests/workflows/test_durable_workflows.py:L2252-L2301]

| Case | Input / expected quantity and tolerance | Existing test node ID | Offline result / evidence |
| --- | --- | --- | --- |
| R1: previously recorded pile worked example | Test-local inputs and expected total in kN; numeric source values withheld pending provenance qualification; absolute tolerance 1.0 kN from assertion | `tests/geotechnical/test_pile_capacity_dark_intel.py::TestDarkIntelAxialCapacity::test_total_axial_capacity_kn` | PASS BASE and AFTER; regression, independent source not established; [tests/geotechnical/test_pile_capacity_dark_intel.py:L5-L19] [tests/geotechnical/test_pile_capacity_dark_intel.py:L130-L140] |
| R2: composite pile component identity | Function-local inputs; expected total minus shaft minus tip = 0 kN; absolute tolerance 1e-6 kN | `tests/geotechnical/test_pile_capacity_composite.py::TestAlphaMethodCapacitySingleLayer::test_components_sum_to_total` | PASS BASE and AFTER; [tests/geotechnical/test_pile_capacity_composite.py:L61-L65] |
| R3: bearing-factor formula replay | Function-local phi and geometry; factors versus formulas and pressure versus returned shape/depth factors; relative tolerance 1e-12, dimensionless and kPa | `tests/workflows/test_durable_workflows.py::test_mudmat_bearing_capacity_factors_match_brinch_hansen` | PASS BASE and AFTER; not an independent pressure benchmark because factors are read back from the result; [tests/workflows/test_durable_workflows.py:L2093-L2137] |
| R4: liquefaction formula replay | Function-local layer; expected formulas in dimensionless units; final safety-factor relative tolerance 1e-12 | `tests/workflows/test_durable_workflows.py::test_liquefaction_seed_idriss_closed_form` | PASS BASE and AFTER; formulas repeat the implementation's attributed method; [tests/workflows/test_durable_workflows.py:L2252-L2301] |
| R5: pile-axial-capacity | `examples/workflows/pile-axial-capacity/input.yml`; total, shaft, tip and allowable capacity: kN; alpha/utilization: dimensionless; expected residual against pinned assertion = 0 in each unit, tolerance max(1e-12 absolute in that unit, 1e-6 × absolute reference), as pytest default | `tests/workflows/test_durable_workflows.py::test_workflow_registry[pile-axial-capacity]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1199-L1210] |
| R6: drag-anchor-holding | `examples/workflows/drag-anchor-holding/input.yml`; holding and allowable capacity: kN; efficiency: dimensionless; expected residual against pinned assertion = 0 in each unit, tolerance max(1e-12 absolute in that unit, 1e-6 × absolute reference), as pytest default | `tests/workflows/test_durable_workflows.py::test_workflow_registry[drag-anchor-holding]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1211-L1221] |
| R7: anchor-capacity | `examples/workflows/anchor-capacity/input.yml`; drag and suction capacity: kN; expected residual against pinned assertion = 0 in each unit, tolerance max(1e-12 absolute in that unit, 1e-6 × absolute reference), as pytest default | `tests/workflows/test_durable_workflows.py::test_workflow_registry[anchor-capacity]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1229-L1235] |
| R8: pile-capacity | `examples/workflows/pile-capacity/input.yml`; pile total: kN; alpha: dimensionless; expected residual against pinned assertion = 0 in each unit, tolerance max(1e-12 absolute in that unit, 1e-6 × absolute reference), as pytest default | `tests/workflows/test_durable_workflows.py::test_workflow_registry[pile-capacity]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1236-L1240] |
| R9: scour-assessment | `examples/workflows/scour-assessment/input.yml`; scour depth and armour thickness: m; expected residual against pinned assertion = 0 in each unit, tolerance max(1e-12 absolute in that unit, 1e-6 × absolute reference), as pytest default | `tests/workflows/test_durable_workflows.py::test_workflow_registry[scour-assessment]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1241-L1245] |
| R10: mudmat-bearing-capacity | `examples/workflows/mudmat-bearing-capacity/input.yml`; bearing pressure: kPa and vertical capacity: kN, relative tolerance 1e-9; bearing utilization: dimensionless, relative tolerance 1e-6; expected residual = 0 in each unit | `tests/workflows/test_durable_workflows.py::test_workflow_registry[mudmat-bearing-capacity]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1246-L1266] |
| R11: liquefaction-triggering | `examples/workflows/liquefaction/input.yml`; governing depth: m, pytest default relative tolerance 1e-6; layer safety factors: dimensionless, absolute tolerance 0.01; expected residual = 0 in each unit | `tests/workflows/test_durable_workflows.py::test_workflow_registry[liquefaction-triggering]` | PASS BASE and AFTER in a temporary copy; [tests/workflows/test_durable_workflows.py:L1267-L1282] |

Expected snapshot values remain in the cited tests; this pack uses residuals to avoid reproducing restricted-source inputs or standard-table coefficients. [tests/geotechnical/test_pile_capacity_dark_intel.py:L5-L19] [tests/workflows/test_durable_workflows.py:L1199-L1275]

## 5. QA gates

The gate disposition is a checklist for this package under AQG r0, Python calculations; it does not assert completed engineering clearance.

| Gate | Applicability / execution route |
| --- | --- |
| G0 | Applicable: numeric inputs, units and method attribution; C0-M, C0-P, C0-D, C0-U and C0-S |
| G1 | Applicable: Python imports and offline pytest, C1-T and C1-W; the module writes JSON, with no target solver-program load route in these adapters. [src/digitalmodel/geotechnical/workflow.py:L38-L45] [src/digitalmodel/geotechnical/cfg_workflows.py:L20-L33] |
| G2 | Applicable: independently evaluated geometry and explicitly limited formula regressions, C2-A, C2-L, C2-P, C2-B and C2-Q |
| G3 | NOT_APPLICABLE: no time-stepped dynamics in the documented calculation routes. [src/digitalmodel/geotechnical/workflow.py:L53-L60] [src/digitalmodel/geotechnical/cfg_workflows.py:L301-L315] |
| G4 | NOT_APPLICABLE: no diffraction model or panel-mesh output in the documented result schemas. [src/digitalmodel/geotechnical/mudmat.py:L27-L42] [src/digitalmodel/geotechnical/liquefaction.py:L33-L55] |
| G5 | Applicable: retain evidence and source/input fingerprints with C5-E and C5-F before issuing an engineering result; module-level automatic evidence support remains issue pending (5). [src/digitalmodel/geotechnical/workflow.py:L38-L45] [src/digitalmodel/geotechnical/cfg_workflows.py:L20-L33] |

The following checks use existing tests or reproducible evidence audits; thresholds are check criteria, not copied standard limits.

| ID / gate | Quantity and executable check | Numeric pass criterion / unit | Evidence |
| --- | --- | --- | --- |
| C0-M / G0 | Run the complete `tests/geotechnical/test_mudmat_nonfinite_inputs.py` suite; direct `mudmat_bearing_capacity` and `bearing_capacity_factors` must reject non-finite/Boolean inputs | 0 accepted invalid cases; count | Rejection tests and optional-None finite-output tests; [tests/geotechnical/test_mudmat_nonfinite_inputs.py:L55-L125] |
| C0-P / G0 | Run `tests/geotechnical/test_pile_validation.py`; check optional bounds and confirm opt-out behavior, without treating it as an input guarantee | 0 failed assertions; count | Existing boundary and opt-out tests; [tests/geotechnical/test_pile_validation.py:L22-L163] |
| C0-D / G0 | Run database required-cell, source-reference and duplicate-ID tests in `tests/geotechnical/test_geotech_database.py` | 0 accepted invalid required cells, duplicate IDs or unread source references; count | [tests/geotechnical/test_geotech_database.py:L282-L348] |
| C0-U / G0 | Run database quantity/access/conversion tests; preserve unavailable reasons and compare the printed-to-target conversion | 0 missing expected unavailable reasons; conversion agreement with pytest default 1e-6 relative, and explicit 0.005 kN/m³ in the hand conversion | [tests/geotechnical/test_geotech_database.py:L224-L280] |
| C0-S / G0 | Run `tests/test_geotechnical_scour.py`; verify zero-flow outputs and valid-domain diameter rejection | 0.0 m scour at zero flow; 0 accepted zero-diameter cases; count | [tests/test_geotechnical_scour.py:L42-L112] |
| C1-T / G1 | Run the complete module suites and the two formula tests listed in section 4 with Python pytest | 203 passed, 0 failed, 0 skipped, 0 NOT_RUN; tests | Source suite scope and imports; [tests/geotechnical/test_geotechnical.py:L18-L43] |
| C1-W / G1 | Run the seven registry node IDs R5–R11 in a temporary repository copy so generated files stay there | 7 passed, 0 failed, 0 skipped, 0 NOT_RUN; tests | Registry and output assertions; [tests/workflows/test_durable_workflows.py:L56-L96] [docs/registry/workflows.yaml:L820-L837] [docs/registry/workflows.yaml:L847-L864] [docs/registry/workflows.yaml:L874-L925] [docs/registry/workflows.yaml:L934-L942] |
| C2-A / G2 | Run V1 and independently evaluate the rectangular-area expression in section 4 | 8.0 m², relative tolerance 1e-6 | [tests/geotechnical/test_mudmat_eccentricity.py:L44-L45] |
| C2-L / G2 | Run V2 and independently evaluate the long-axis reduced rectangle | 2.4 m² and 0.8 m, relative tolerance 1e-6 | [tests/geotechnical/test_mudmat_eccentricity.py:L48-L56] |
| C2-P / G2 | Run R2 for `alpha_method_capacity`: total equals shaft plus tip | Residual ≤ 1e-6 kN | [tests/geotechnical/test_pile_capacity_composite.py:L61-L65] |
| C2-B / G2 | Run R3: `bearing_capacity_factors` formula check and drained-pressure replay; not an independent qualification of bearing coefficients | Relative error ≤ 1e-12, dimensionless factors and kPa pressure | [tests/workflows/test_durable_workflows.py:L2093-L2137] |
| C2-Q / G2 | Run R4: rd, MSF, CSR, CRR and safety-factor replay in `assess_liquefaction`; not an independent published benchmark | Final safety-factor relative error ≤ 1e-12; dimensionless | [tests/workflows/test_durable_workflows.py:L2252-L2301] |
| C5-E / G5 | Audit this pack's evidence: resolve every code citation at the header pin; match all section-4 nodes to recorded pytest cases; record command, input digest and result | 0 invalid citations, 0 absent node results, 0 unresolved input-digest mismatches; count | Pin and section-4 source tests; [tests/docs/test_agent_pack_node_ids.py:L19-L40] |
| C5-F / G5 | Fingerprint package Python files and the seven workflow input files using SHA-256; retain sorted path/digest pairs and their aggregate digest alongside run results | 0 files missing a 64-hex digest; recomputed aggregate equals recorded aggregate, 0 differing bytes | The selected inputs and output paths are enumerated by the registry; [docs/registry/workflows.yaml:L820-L837] [docs/registry/workflows.yaml:L847-L864] [docs/registry/workflows.yaml:L874-L925] [docs/registry/workflows.yaml:L934-L942] |

## 6. Known failure modes and pitfalls

sweep: 192 found

The count is reviewed sites and semantic assumptions, not a count of distinct defects: repeated numeric conversions and defaults each have their own row; zero accumulators and intentional unavailable values are distinguished from failure paths. No package `continue`, error-path empty-list/dict return, epoch fallback or cache write was found; the handled conversion exception and empty-result summary writes are listed below. [src/digitalmodel/geotechnical/database.py:L69-L121] [src/digitalmodel/geotechnical/database.py:L124-L171] [src/digitalmodel/geotechnical/cfg_workflows.py:L54-L112] [src/digitalmodel/geotechnical/cfg_workflows.py:L186-L208]

Reproduce the lexical sweep over every package Python file, then inspect enclosing definitions and defaults with Python `ast`; comparisons and docstrings that merely mention zero are not error substitutions:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|\b(?:int|float)\(|= 0(?:\.0)?\b|\.get\(' src/digitalmodel/geotechnical --glob '*.py'
rg -n 'default|unit|kPa|kN|degrees|mm|density|cache|write_text|json.dumps' src/digitalmodel/geotechnical --glob '*.py'
```

| Site | Symptom / classification at code pin | Detection check | Tracking / date |
| --- | --- | --- | --- |
| S001: [src/digitalmodel/geotechnical/anchors.py:L36-L36] | literal assumption / initialization: `DEFAULT_WALL_THICKNESS_M`; assumed parameter used without source resolution | no detection check exists | issue pending (1); 2026-10-16 |
| S002: [src/digitalmodel/geotechnical/anchors.py:L113-L113] | argument default: `alpha`; assumed parameter used without source resolution | no detection check exists | issue pending (1); 2026-10-16 |
| S003: [src/digitalmodel/geotechnical/anchors.py:L114-L114] | argument default: `nc`; assumed parameter used without source resolution | no detection check exists | issue pending (1); 2026-10-16 |
| S004: [src/digitalmodel/geotechnical/anchors.py:L115-L115] | argument default: `wall_thickness_m`; assumed parameter used without source resolution | no detection check exists | issue pending (1); 2026-10-16 |
| S005: [src/digitalmodel/geotechnical/cfg_workflows.py:L54-L54] | configuration fallback: `anchor_capacity`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S006: [src/digitalmodel/geotechnical/cfg_workflows.py:L61-L61] | numeric conversion: `drag[weight_kn]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S007: [src/digitalmodel/geotechnical/cfg_workflows.py:L65-L65] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S008: [src/digitalmodel/geotechnical/cfg_workflows.py:L65-L65] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S009: [src/digitalmodel/geotechnical/cfg_workflows.py:L66-L66] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S010: [src/digitalmodel/geotechnical/cfg_workflows.py:L69-L69] | numeric conversion: `required`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S011: [src/digitalmodel/geotechnical/cfg_workflows.py:L80-L80] | numeric conversion: `caisson[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S012: [src/digitalmodel/geotechnical/cfg_workflows.py:L81-L81] | numeric conversion: `caisson[length_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S013: [src/digitalmodel/geotechnical/cfg_workflows.py:L82-L82] | numeric conversion: `soil[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S014: [src/digitalmodel/geotechnical/cfg_workflows.py:L83-L83] | configuration fallback: `alpha`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S015: [src/digitalmodel/geotechnical/cfg_workflows.py:L83-L83] | numeric conversion: `alpha`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S016: [src/digitalmodel/geotechnical/cfg_workflows.py:L84-L84] | configuration fallback: `Nc`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S017: [src/digitalmodel/geotechnical/cfg_workflows.py:L84-L84] | numeric conversion: `Nc`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S018: [src/digitalmodel/geotechnical/cfg_workflows.py:L85-L85] | configuration fallback: `wall_thickness_m`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S019: [src/digitalmodel/geotechnical/cfg_workflows.py:L85-L85] | numeric conversion: `wall_thickness_m`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S020: [src/digitalmodel/geotechnical/cfg_workflows.py:L87-L87] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S021: [src/digitalmodel/geotechnical/cfg_workflows.py:L87-L87] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S022: [src/digitalmodel/geotechnical/cfg_workflows.py:L88-L88] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S023: [src/digitalmodel/geotechnical/cfg_workflows.py:L91-L91] | numeric conversion: `required`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S024: [src/digitalmodel/geotechnical/cfg_workflows.py:L105-L105] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S025: [src/digitalmodel/geotechnical/cfg_workflows.py:L124-L124] | configuration fallback: `pile_capacity`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S026: [src/digitalmodel/geotechnical/cfg_workflows.py:L126-L126] | configuration fallback: `Nc`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S027: [src/digitalmodel/geotechnical/cfg_workflows.py:L126-L126] | numeric conversion: `Nc`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S028: [src/digitalmodel/geotechnical/cfg_workflows.py:L130-L130] | numeric conversion: `layer[thickness_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S029: [src/digitalmodel/geotechnical/cfg_workflows.py:L131-L131] | numeric conversion: `layer[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S030: [src/digitalmodel/geotechnical/cfg_workflows.py:L132-L132] | numeric conversion: `layer[sigma_v_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S031: [src/digitalmodel/geotechnical/cfg_workflows.py:L137-L137] | numeric conversion: `pile[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S032: [src/digitalmodel/geotechnical/cfg_workflows.py:L142-L142] | numeric conversion: `pile[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S033: [src/digitalmodel/geotechnical/cfg_workflows.py:L143-L143] | numeric conversion: `pile[embedded_length_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S034: [src/digitalmodel/geotechnical/cfg_workflows.py:L144-L144] | numeric conversion: `soil[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S035: [src/digitalmodel/geotechnical/cfg_workflows.py:L145-L145] | numeric conversion: `soil[sigma_v_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S036: [src/digitalmodel/geotechnical/cfg_workflows.py:L149-L149] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S037: [src/digitalmodel/geotechnical/cfg_workflows.py:L150-L150] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S038: [src/digitalmodel/geotechnical/cfg_workflows.py:L150-L150] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S039: [src/digitalmodel/geotechnical/cfg_workflows.py:L153-L153] | numeric conversion: `applied`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S040: [src/digitalmodel/geotechnical/cfg_workflows.py:L166-L166] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S041: [src/digitalmodel/geotechnical/cfg_workflows.py:L186-L186] | configuration fallback: `scour`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S042: [src/digitalmodel/geotechnical/cfg_workflows.py:L202-L202] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S043: [src/digitalmodel/geotechnical/cfg_workflows.py:L218-L218] | configuration fallback: `mudmat_bearing_capacity`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S044: [src/digitalmodel/geotechnical/cfg_workflows.py:L224-L224] | numeric conversion: `foundation[width_B_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S045: [src/digitalmodel/geotechnical/cfg_workflows.py:L225-L225] | numeric conversion: `foundation[length_L_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S046: [src/digitalmodel/geotechnical/cfg_workflows.py:L226-L226] | configuration fallback: `embedment_depth_m`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S047: [src/digitalmodel/geotechnical/cfg_workflows.py:L226-L226] | numeric conversion: `embedment_depth_m`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S048: [src/digitalmodel/geotechnical/cfg_workflows.py:L228-L228] | numeric conversion: `soil[submerged_unit_weight_kN_m3]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S049: [src/digitalmodel/geotechnical/cfg_workflows.py:L229-L229] | numeric conversion: `loads[vertical_kN]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S050: [src/digitalmodel/geotechnical/cfg_workflows.py:L230-L230] | configuration fallback: `moment_kNm`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S051: [src/digitalmodel/geotechnical/cfg_workflows.py:L230-L230] | numeric conversion: `moment_kNm`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S052: [src/digitalmodel/geotechnical/cfg_workflows.py:L235-L235] | numeric conversion: `soil[undrained_shear_strength_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S053: [src/digitalmodel/geotechnical/cfg_workflows.py:L240-L240] | numeric conversion: `soil[friction_angle_deg]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S054: [src/digitalmodel/geotechnical/cfg_workflows.py:L242-L242] | configuration fallback: `effective_cohesion_kpa`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S055: [src/digitalmodel/geotechnical/cfg_workflows.py:L242-L242] | numeric conversion: `effective_cohesion_kpa`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S056: [src/digitalmodel/geotechnical/cfg_workflows.py:L246-L246] | numeric conversion: `soil[interface_friction_angle_deg]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S057: [src/digitalmodel/geotechnical/cfg_workflows.py:L250-L250] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S058: [src/digitalmodel/geotechnical/cfg_workflows.py:L250-L250] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S059: [src/digitalmodel/geotechnical/cfg_workflows.py:L251-L251] | numeric conversion: `loads[vertical_kN]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S060: [src/digitalmodel/geotechnical/cfg_workflows.py:L252-L252] | configuration fallback: `horizontal_kN`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S061: [src/digitalmodel/geotechnical/cfg_workflows.py:L252-L252] | numeric conversion: `horizontal_kN`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S062: [src/digitalmodel/geotechnical/cfg_workflows.py:L255-L255] | numeric conversion: `non-finite utilization fallback`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (3); 2026-10-16 |
| S063: [src/digitalmodel/geotechnical/cfg_workflows.py:L257-L257] | conditional numeric fallback; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (3); 2026-10-16 |
| S064: [src/digitalmodel/geotechnical/cfg_workflows.py:L280-L280] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S065: [src/digitalmodel/geotechnical/cfg_workflows.py:L297-L297] | configuration fallback: `liquefaction`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S066: [src/digitalmodel/geotechnical/cfg_workflows.py:L299-L299] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S067: [src/digitalmodel/geotechnical/cfg_workflows.py:L302-L302] | numeric conversion: `seismic[pga_g]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S068: [src/digitalmodel/geotechnical/cfg_workflows.py:L303-L303] | numeric conversion: `seismic[magnitude]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S069: [src/digitalmodel/geotechnical/cfg_workflows.py:L305-L307] | numeric conversion: `required_factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S070: [src/digitalmodel/geotechnical/cfg_workflows.py:L306-L306] | configuration fallback: `required_factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S071: [src/digitalmodel/geotechnical/cfg_workflows.py:L308-L308] | configuration fallback: `k_sigma`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S072: [src/digitalmodel/geotechnical/cfg_workflows.py:L308-L308] | numeric conversion: `k_sigma`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S073: [src/digitalmodel/geotechnical/cfg_workflows.py:L320-L320] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S074: [src/digitalmodel/geotechnical/database.py:L81-L81] | unavailable return; unavailable values remain explicit, parser/exception boundaries lack direct probes | C0-U for normal unavailable text | issue pending (4); 2026-10-16 |
| S075: [src/digitalmodel/geotechnical/database.py:L82-L82] | numeric conversion: `printed numeric bound`; unavailable values remain explicit, parser/exception boundaries lack direct probes | no detection check exists | issue pending (4); 2026-10-16 |
| S076: [src/digitalmodel/geotechnical/database.py:L83-L83] | numeric conversion: `printed numeric bound`; unavailable values remain explicit, parser/exception boundaries lack direct probes | no detection check exists | issue pending (4); 2026-10-16 |
| S077: [src/digitalmodel/geotechnical/database.py:L97-L97] | unavailable return; unavailable values remain explicit, parser/exception boundaries lack direct probes | no detection check exists | issue pending (4); 2026-10-16 |
| S078: [src/digitalmodel/geotechnical/database.py:L119-L120] | caught conversion failure; unavailable values remain explicit, parser/exception boundaries lack direct probes | no detection check exists | issue pending (4); 2026-10-16 |
| S079: [src/digitalmodel/geotechnical/database.py:L154-L154] | Default data-file path; load fails rather than invent records | C0-D via default database-path test | issue pending (4); 2026-10-16 |
| S080: [src/digitalmodel/geotechnical/liquefaction.py:L28-L28] | literal assumption / initialization: `_N_NON_LIQUEFIABLE`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (7); 2026-10-16 |
| S081: [src/digitalmodel/geotechnical/liquefaction.py:L30-L30] | literal assumption / initialization: `_CRR_CAP`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (7); 2026-10-16 |
| S082: [src/digitalmodel/geotechnical/liquefaction.py:L68-L68] | literal return fallback; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (7); 2026-10-16 |
| S083: [src/digitalmodel/geotechnical/liquefaction.py:L105-L105] | argument default: `required_factor_of_safety`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (7); 2026-10-16 |
| S084: [src/digitalmodel/geotechnical/liquefaction.py:L106-L106] | argument default: `k_sigma`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (7); 2026-10-16 |
| S085: [src/digitalmodel/geotechnical/liquefaction.py:L119-L119] | numeric conversion: `layer[depth_m]`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (2); 2026-10-16 |
| S086: [src/digitalmodel/geotechnical/liquefaction.py:L120-L120] | numeric conversion: `layer[sigma_v_kpa]`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (2); 2026-10-16 |
| S087: [src/digitalmodel/geotechnical/liquefaction.py:L121-L121] | numeric conversion: `layer[sigma_v_eff_kpa]`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (2); 2026-10-16 |
| S088: [src/digitalmodel/geotechnical/liquefaction.py:L122-L122] | numeric conversion: `layer[N1_60cs]`; regression covers selected arithmetic, not validity domain | no detection check exists | issue pending (2); 2026-10-16 |
| S089: [src/digitalmodel/geotechnical/mudmat.py:L103-L103] | argument default: `moment_knm`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S090: [src/digitalmodel/geotechnical/mudmat.py:L106-L106] | argument default: `effective_cohesion_kpa`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S091: [src/digitalmodel/geotechnical/mudmat.py:L151-L151] | numeric conversion: `undrained_shear_strength_kpa`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S092: [src/digitalmodel/geotechnical/mudmat.py:L152-L152] | literal assumption / initialization: `phi_deg`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S093: [src/digitalmodel/geotechnical/mudmat.py:L156-L156] | numeric conversion: `effective_cohesion_kpa`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S094: [src/digitalmodel/geotechnical/mudmat.py:L157-L157] | numeric conversion: `friction_angle_deg`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S095: [src/digitalmodel/geotechnical/mudmat.py:L162-L162] | conditional numeric fallback; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S096: [src/digitalmodel/geotechnical/mudmat.py:L163-L163] | conditional numeric fallback; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S097: [src/digitalmodel/geotechnical/mudmat.py:L164-L164] | conditional numeric fallback; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S098: [src/digitalmodel/geotechnical/mudmat.py:L203-L203] | numeric conversion: `interface_friction_angle_deg`; condition/axis handling is explicit, default applicability still needs review | C0-M and C2-A/C2-L for direct finite geometry only | issue pending (3); 2026-10-16 |
| S099: [src/digitalmodel/geotechnical/pile_capacity.py:L41-L41] | argument default: `Nc`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S100: [src/digitalmodel/geotechnical/pile_capacity.py:L87-L87] | argument default: `Nc`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S101: [src/digitalmodel/geotechnical/pile_capacity.py:L113-L113] | Zero shaft-resistance accumulator; intentional initialization before summation | C2-P | issue pending (2); 2026-10-16 |
| S102: [src/digitalmodel/geotechnical/piles.py:L18-L18] | literal assumption / initialization: `DEFAULT_NC`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S103: [src/digitalmodel/geotechnical/piles.py:L19-L19] | literal assumption / initialization: `DEFAULT_NQ`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S104: [src/digitalmodel/geotechnical/piles.py:L22-L22] | Code range assumption used only by optional validation | C0-P | issue pending (2); 2026-10-16 |
| S105: [src/digitalmodel/geotechnical/piles.py:L23-L23] | Code range assumption used only by optional validation | C0-P | issue pending (2); 2026-10-16 |
| S106: [src/digitalmodel/geotechnical/piles.py:L24-L24] | Code range assumption used only by optional validation | C0-P | issue pending (2); 2026-10-16 |
| S107: [src/digitalmodel/geotechnical/piles.py:L25-L25] | Code range assumption used only by optional validation | C0-P | issue pending (2); 2026-10-16 |
| S108: [src/digitalmodel/geotechnical/piles.py:L177-L177] | argument default: `nc`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S109: [src/digitalmodel/geotechnical/piles.py:L197-L197] | argument default: `nq`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S110: [src/digitalmodel/geotechnical/piles.py:L260-L260] | Zero shaft-resistance accumulator; intentional initialization before summation | C2-P | issue pending (2); 2026-10-16 |
| S111: [src/digitalmodel/geotechnical/piles.py:L276-L276] | configuration fallback: `k0`; code coefficient default, not a licensed-source confirmation | no detection check exists | issue pending (1); 2026-10-16 |
| S112: [src/digitalmodel/geotechnical/scour.py:L18-L18] | literal assumption / initialization: `KINEMATIC_VISCOSITY_WATER`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S113: [src/digitalmodel/geotechnical/scour.py:L19-L19] | literal assumption / initialization: `GRAVITY`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S114: [src/digitalmodel/geotechnical/scour.py:L20-L20] | literal assumption / initialization: `DEFAULT_WAVE_PERIOD_S`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S115: [src/digitalmodel/geotechnical/scour.py:L58-L58] | argument default: `burial_ratio`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S116: [src/digitalmodel/geotechnical/scour.py:L59-L59] | argument default: `wave_period_s`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S117: [src/digitalmodel/geotechnical/scour.py:L102-L102] | literal assumption / initialization: `s_over_d`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S118: [src/digitalmodel/geotechnical/scour.py:L107-L107] | literal assumption / initialization: `s_over_d`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S119: [src/digitalmodel/geotechnical/scour.py:L111-L111] | literal assumption / initialization: `s_over_d`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S120: [src/digitalmodel/geotechnical/scour.py:L164-L164] | literal assumption / initialization: `u_cr`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S121: [src/digitalmodel/geotechnical/scour.py:L167-L167] | literal assumption / initialization: `s_over_d_max`; branch or environmental assumption, no sediment/stability qualification | no detection check exists | #2615; 2026-10-16 |
| S122: [src/digitalmodel/geotechnical/workflow.py:L53-L53] | configuration fallback: `geotechnical`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S123: [src/digitalmodel/geotechnical/workflow.py:L68-L68] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S124: [src/digitalmodel/geotechnical/workflow.py:L69-L69] | configuration fallback: `Nc`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S125: [src/digitalmodel/geotechnical/workflow.py:L69-L69] | numeric conversion: `Nc`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S126: [src/digitalmodel/geotechnical/workflow.py:L74-L74] | numeric conversion: `layer[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S127: [src/digitalmodel/geotechnical/workflow.py:L74-L74] | numeric conversion: `layer[sigma_v_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S128: [src/digitalmodel/geotechnical/workflow.py:L74-L74] | numeric conversion: `layer[thickness_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S129: [src/digitalmodel/geotechnical/workflow.py:L78-L78] | numeric conversion: `pile[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S130: [src/digitalmodel/geotechnical/workflow.py:L83-L83] | numeric conversion: `pile[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S131: [src/digitalmodel/geotechnical/workflow.py:L84-L84] | numeric conversion: `pile[embedded_length_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S132: [src/digitalmodel/geotechnical/workflow.py:L85-L85] | numeric conversion: `soil[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S133: [src/digitalmodel/geotechnical/workflow.py:L86-L86] | numeric conversion: `soil[sigma_v_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S134: [src/digitalmodel/geotechnical/workflow.py:L90-L90] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S135: [src/digitalmodel/geotechnical/workflow.py:L90-L90] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S136: [src/digitalmodel/geotechnical/workflow.py:L103-L103] | numeric conversion: `applied_load_kn`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S137: [src/digitalmodel/geotechnical/workflow.py:L109-L109] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S138: [src/digitalmodel/geotechnical/workflow.py:L122-L122] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S139: [src/digitalmodel/geotechnical/workflow.py:L124-L124] | numeric conversion: `anchor[weight_kn]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S140: [src/digitalmodel/geotechnical/workflow.py:L128-L128] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S141: [src/digitalmodel/geotechnical/workflow.py:L128-L128] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S142: [src/digitalmodel/geotechnical/workflow.py:L141-L141] | numeric conversion: `required`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S143: [src/digitalmodel/geotechnical/workflow.py:L147-L147] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S144: [src/digitalmodel/geotechnical/workflow.py:L161-L161] | configuration fallback: `design`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S145: [src/digitalmodel/geotechnical/workflow.py:L163-L163] | numeric conversion: `caisson[diameter_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S146: [src/digitalmodel/geotechnical/workflow.py:L164-L164] | numeric conversion: `caisson[length_m]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S147: [src/digitalmodel/geotechnical/workflow.py:L165-L165] | numeric conversion: `soil[Su_kpa]`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S148: [src/digitalmodel/geotechnical/workflow.py:L166-L166] | configuration fallback: `alpha`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S149: [src/digitalmodel/geotechnical/workflow.py:L166-L166] | numeric conversion: `alpha`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S150: [src/digitalmodel/geotechnical/workflow.py:L167-L167] | configuration fallback: `Nc`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S151: [src/digitalmodel/geotechnical/workflow.py:L167-L167] | numeric conversion: `Nc`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S152: [src/digitalmodel/geotechnical/workflow.py:L168-L168] | configuration fallback: `wall_thickness_m`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S153: [src/digitalmodel/geotechnical/workflow.py:L168-L168] | numeric conversion: `wall_thickness_m`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S154: [src/digitalmodel/geotechnical/workflow.py:L170-L170] | configuration fallback: `factor_of_safety`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S155: [src/digitalmodel/geotechnical/workflow.py:L170-L170] | numeric conversion: `factor_of_safety`; raw float conversion has no locale handling and can admit non-finite values | no detection check exists | issue pending (2); 2026-10-16 |
| S156: [src/digitalmodel/geotechnical/workflow.py:L182-L182] | configuration fallback: `outputs`; omission selects fallback without a complete-input requirement | no detection check exists | issue pending (2); 2026-10-16 |
| S157: [src/digitalmodel/geotechnical/anchors.py:L13-L31] | Empirical efficiency lookup has no resolved per-entry provenance; model and soil keys choose fixed factors | no detection check exists | issue pending (1); 2026-10-16 |
| S158: [src/digitalmodel/geotechnical/anchors.py:L81-L104] | Weight comparison rejects nonpositive finite values but does not reject NaN | no detection check exists | issue pending (2); 2026-10-16 |
| S159: [src/digitalmodel/geotechnical/anchors.py:L138-L159] | Suction positivity checks omit finite/factor/wall-thickness sign constraints | no detection check exists | issue pending (2); 2026-10-16 |
| S160: [src/digitalmodel/geotechnical/piles.py:L107-L133] | Optional clay validation is disabled unless requested | C0-P | issue pending (2); 2026-10-16 |
| S161: [src/digitalmodel/geotechnical/piles.py:L148-L168] | Sand friction uses a fixed degree deduction without input-domain checks | no detection check exists | issue pending (2); 2026-10-16 |
| S162: [src/digitalmodel/geotechnical/piles.py:L215-L255] | Axial optional validation checks geometry, not all layer inputs | no detection check exists | issue pending (2); 2026-10-16 |
| S163: [src/digitalmodel/geotechnical/piles.py:L257-L295] | Layer-thickness sum controls resistance independently of stated pile length; missing sand K0 and tip Nq have defaults | no detection check exists | issue pending (2); 2026-10-16 |
| S164: [src/digitalmodel/geotechnical/pile_capacity.py:L36-L141] | Composite geometry/thickness is not validated; zero accumulator is arithmetic initialization, not an error substitute | C2-P for sum identity only; no detection check exists for invalid geometry | issue pending (2); 2026-10-16 |
| S165: [src/digitalmodel/geotechnical/scour.py:L53-L80] | Unit assumptions: m, m/s, mm and s; sediment diameter is never used | no detection check exists | #2615; 2026-10-16 |
| S166: [src/digitalmodel/geotechnical/scour.py:L82-L89] | Zero total velocity returns zero depth and zero ratios, including cancellation of opposite-signed inputs | C0-S for true zero flow only; no detection check exists for cancellation | issue pending (2); 2026-10-16 |
| S167: [src/digitalmodel/geotechnical/scour.py:L91-L116] | Fixed current/wave branches and linear burial reduction have no sediment-motion criterion | no detection check exists | #2615; 2026-10-16 |
| S168: [src/digitalmodel/geotechnical/scour.py:L126-L176] | Water depth only passes a positivity guard; fixed critical velocity and plateau determine depth | no detection check exists | #2615; 2026-10-16 |
| S169: [src/digitalmodel/geotechnical/scour.py:L154-L159] | Zero-current branch returns zero monopile depth and ratio | C0-S | #2615; 2026-10-16 |
| S170: [src/digitalmodel/geotechnical/scour.py:L185-L224] | Armour uses m/s, kg/m³ and m; returned stability number is never compared with a limit | no detection check exists | #2615; 2026-10-16 |
| S171: [src/digitalmodel/geotechnical/mudmat.py:L45-L65] | Direct finite/Boolean rejection protects capacity-factor input; finite upper-angle domain is not checked | C0-M for finite input only; no detection check exists for upper angle | issue pending (3); 2026-10-16 |
| S172: [src/digitalmodel/geotechnical/mudmat.py:L82-L93] | Depth ratio silently clips at the implemented limit without an applicability note | no detection check exists | issue pending (3); 2026-10-16 |
| S173: [src/digitalmodel/geotechnical/mudmat.py:L96-L119] | Default eccentricity direction is B; scalar moment sign is discarded by absolute-value eccentricity | C2-A and C2-L | #2105; 2026-10-16 |
| S174: [src/digitalmodel/geotechnical/mudmat.py:L146-L157] | Condition selects required strength or friction angle; negative strength/cohesion remains admissible | no detection check exists | issue pending (3); 2026-10-16 |
| S175: [src/digitalmodel/geotechnical/mudmat.py:L198-L209] | Missing interface angle adopts soil angle; no independent interface-property qualification | no detection check exists | issue pending (1); 2026-10-16 |
| S176: [src/digitalmodel/geotechnical/liquefaction.py:L58-L68] | Depth stress-reduction is piecewise with a constant deep-layer fallback | no detection check exists for domain or continuity | issue pending (7); 2026-10-16 |
| S177: [src/digitalmodel/geotechnical/liquefaction.py:L87-L98] | Finite resistance cap substitutes for layers described as non-liquefiable; high demand still enters safety-factor arithmetic | C2-Q checks cap arithmetic only; no detection check exists for high-demand semantics | issue pending (7); 2026-10-16 |
| S178: [src/digitalmodel/geotechnical/liquefaction.py:L109-L145] | Scalar unit assumptions and comparison-only checks permit NaN to propagate and evade the liquefies comparison | no detection check exists | issue pending (2); 2026-10-16 |
| S179: [src/digitalmodel/geotechnical/cfg_workflows.py:L36-L42] | Missing demand omits utilization and status instead of requiring a complete load case | no detection check exists | issue pending (2); 2026-10-16 |
| S180: [src/digitalmodel/geotechnical/cfg_workflows.py:L54-L100] | Empty anchor block yields no results but all(empty statuses) returns PASS | no detection check exists | issue pending (2); 2026-10-16 |
| S181: [src/digitalmodel/geotechnical/cfg_workflows.py:L186-L208] | Empty scour block writes an empty results object without an error | no detection check exists | issue pending (2); 2026-10-16 |
| S182: [src/digitalmodel/geotechnical/cfg_workflows.py:L250-L260] | Zero or negative allowable sliding resistance, or nonpositive horizontal demand, becomes zero utilization | no detection check exists | issue pending (3); 2026-10-16 |
| S183: [src/digitalmodel/geotechnical/cfg_workflows.py:L223-L258] | float conversion precedes mudmat Boolean rejection; safety factor and horizontal load bypass direct calculator validation | no detection check exists | issue pending (2); 2026-10-16 |
| S184: [src/digitalmodel/geotechnical/database.py:L17-L28] | Parser assumes comma thousands and dot decimals; explicit aliases select engineering conversion targets | C0-U for retained fixture only; no detection check exists for locale boundary | issue pending (4); 2026-10-16 |
| S185: [src/digitalmodel/geotechnical/database.py:L41-L47] | Unknown lookup value and unknown row return empty tuples; unsupported lookup field raises | C0-D via immutable lookup test | issue pending (4); 2026-10-16 |
| S186: [src/digitalmodel/geotechnical/database.py:L69-L121] | Formula/qualified/unsupported-unit text stays unavailable with a reason; conversion exceptions are retained rather than raised | C0-U for normal unavailable values; no detection check exists for injected conversion failure | issue pending (4); 2026-10-16 |
| S187: [src/digitalmodel/geotechnical/database.py:L95-L104] | Missing numeric bound returns None; pcf is interpreted as weight density by multiplying registry standard gravity | C0-U for weight conversion; no detection check exists for direct missing-bound probe | issue pending (4); 2026-10-16 |
| S188: [src/digitalmodel/geotechnical/database_schema.py:L61-L79] | Absent optional note becomes empty text; absent required cells and unknown codes raise rather than default | C0-D | issue pending (4); 2026-10-16 |
| S189: [src/digitalmodel/geotechnical/workflow.py:L31-L45] | Relative output paths fall back to current directory; JSON writer has no explicit non-finite-output rejection | no detection check exists | issue pending (2); 2026-10-16 |
| S190: [src/digitalmodel/geotechnical/cfg_workflows.py:L13-L33] | Relative output paths fall back to current directory; empty result payloads can be written; these are summary writes, not a cache | no detection check exists | issue pending (2); 2026-10-16 |
| S191: [src/digitalmodel/geotechnical/workflow.py:L65-L188] | Adapters use scalar kN/kPa/m conventions, default design factors and optional demands; no shared finite/locale input validator | no detection check exists | issue pending (2); 2026-10-16 |
| S192: [src/digitalmodel/geotechnical/database_schema.py:L82-L121] | Nullable numeric values/bounds are intentional; numeric access raises when unavailable | C0-U | issue pending (4); 2026-10-16 |

| Confirmed incident or limitation | Present code disposition | Detection / tracking |
| --- | --- | --- |
| Scour sediment/depth/stability and source suitability | Active limitations: unused sediment, validated-but-unused depth, fixed critical velocity, unqualified stability number | No detection check exists for those deficiencies; ordinary zero-flow regression is C0-S; #2615. [src/digitalmodel/geotechnical/scour.py:L53-L123] [src/digitalmodel/geotechnical/scour.py:L126-L224] |
| Wrong mudmat eccentricity axis | Explicit B/L selection now preserves axis identity and sorts effective dimensions for bearing factors | C2-A/C2-L plus all axis tests; #2105. [src/digitalmodel/geotechnical/mudmat.py:L161-L180] [tests/geotechnical/test_mudmat_eccentricity.py:L24-L113] |
| Non-finite mudmat inputs | Direct calculator now rejects Boolean/non-finite numeric inputs; the open issue does not describe the present direct-calculator behavior | C0-M; #2330. [src/digitalmodel/geotechnical/mudmat.py:L45-L51] [src/digitalmodel/geotechnical/mudmat.py:L121-L132] [tests/geotechnical/test_mudmat_nonfinite_inputs.py:L55-L125] |
| Structural foundation-deck precedent | The separate structural generator's tests expressly delegate bearing checks to this package; its FE result is not an independent bearing-capacity reference | No package detection check exists for external solver decks; #2094. [tests/ansys/test_mudmat.py:L72-L82] |
| Documentation reference mismatch | The vision page names DNVGL RP E301 for anchors while this package names E302/E303; editions are absent, so no edition reconciliation is asserted | No detection check exists; issue pending (1). [docs/vision/CALCULATIONS-VISION.md:L190-L198] [src/digitalmodel/geotechnical/anchors.py:L1-L14] |

## 7. Lessons log

| Date | Change reference | Lesson / evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial pack revision 0 | Separate geometrical hand checks, formula replays and attributed worked examples; summed component agreement does not qualify source coefficients. [tests/geotechnical/test_mudmat_eccentricity.py:L44-L56] [tests/geotechnical/test_pile_capacity_dark_intel.py:L5-L19] | Sections 3–6 distinguish execution evidence and normative qualification | Pending maintainer registration; issue pending (5) |

## 8. Ownership

Proposed responsibilities: engineering calculations quality manager reviews the method and gates; deputy engineering calculations quality manager substitutes for that role; calculation maintainers produce updates and resolve cross-family scope. An independent provider reviews this draft before clearance. Ambiguous standards, unsupported domains and tolerance changes require the family quality manager's disposition; escalation to the maintainer is reserved for a cross-family or authorization boundary.

## 9. Dependencies and implementation inventory

The map source is `docs/registry/implementation-map.yaml` on `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose declared data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`; citations to that file in this section resolve at the map commit, not the code pin. [docs/registry/implementation-map.yaml:L1-L3]

**Inventory:** 0 map families have an implementation row whose path starts with `src/digitalmodel/geotechnical/`; consequently the complete inventory for this package's map families is empty, counted families are empty, and the counted-family grep is `NOT_APPLICABLE` because there is no in-package map row. This satisfies the inventory rule without treating rejected candidates as implementations. [docs/registry/implementation-map.yaml:L345-L26502]

Reproduce the inventory from the map revision with Python and a YAML parser; family order is preserved by the list comprehension:

```python
package = "src/digitalmodel/geotechnical/"
module_families = [
    family for family in implementation_map["families"]
    if any(row["path"].startswith(package)
           for row in family["implementations"])
]
# Observed: len(module_families) == 0; there are no signals to rank or apply.
```

Map follow-up: add the section-3 method families (pile axial resistance, anchor holding, scour/armour, shallow-foundation bearing/sliding, liquefaction screening) and source-preserving unit conversion with in-package rows before counting them; issue pending (5). [src/digitalmodel/geotechnical/piles.py:L1-L10] [src/digitalmodel/geotechnical/anchors.py:L1-L8] [src/digitalmodel/geotechnical/scour.py:L1-L8] [src/digitalmodel/geotechnical/mudmat.py:L3-L18] [src/digitalmodel/geotechnical/liquefaction.py:L3-L19] [src/digitalmodel/geotechnical/database.py:L69-L121]

The wider repository search included `scripts/`, `src/`, `tests/` and `docs/`; its findings supplement the empty map inventory and do not change the counted-family result:

```sh
rg -n 'bearing_capacity|alpha_method_capacity|skin_friction_clay|suction_anchor_capacity|drag_anchor_capacity|pipeline_scour_depth|cyclic_resistance_ratio_75' src scripts tests docs --glob '*.py'
rg -n '^def (.*pile.*capacity|.*anchor.*capacity|.*scour.*|.*bearing_capacity.*|.*liquefaction.*|stress_reduction_factor|cyclic_stress_ratio|cyclic_resistance_ratio_75)' src scripts docs --glob '*.py'
```

| Definition / route | Classification and dependency |
| --- | --- |
| `piles.axial_capacity` and `pile_capacity.alpha_method_capacity` / `alpha_method_capacity_multilayer` | Separate geometry/summation implementations sharing clay unit-resistance helpers; both listed in section 3. [src/digitalmodel/geotechnical/piles.py:L215-L302] [src/digitalmodel/geotechnical/pile_capacity.py:L36-L141] |
| `GeotechnicalWorkflow` and five `cfg_workflows` classes | Calculator delegates, JSON serializers and additional safety-factor/utilization logic; the latter is documented in section 6 rather than silently classed as formula-free wrapping. [src/digitalmodel/geotechnical/workflow.py:L48-L188] [src/digitalmodel/geotechnical/cfg_workflows.py:L36-L328] |
| `parametric.generate._pile_capacity_kn` / `_suction_anchor_capacity_kn` | Downstream delegating response adapters, not new geotechnical capacity formulas. [src/digitalmodel/parametric/generate.py:L178-L205] |
| Capability-page study builder | Delegates drag-anchor calculation; broad exception handling can omit a combination, an external map follow-up rather than an in-package counted miss; issue pending (5). [scripts/capabilities/build_anchor_holding_explorer.py:L33-L53] |
| Separate manifold foundation screen | Computes uniform bearing utilization and spring settlement from caller-supplied resistance/modulus; it does not derive geotechnical bearing capacity; external map follow-up, issue pending (5). [src/digitalmodel/subsea/manifolds/design_checks.py:L54-L62] |
| Structural foundation deck generator | Separate plate/soil-reaction construction; its tests say bearing acceptance belongs to this package, so it is not an independent bearing benchmark. [tests/ansys/test_mudmat.py:L72-L82] |
| Test formula replays | Assertion-only calculations; regression evidence, not production duplicates. [tests/workflows/test_durable_workflows.py:L2093-L2137] [tests/workflows/test_durable_workflows.py:L2252-L2301] |

| Dependency / consumer | Revision and boundary |
| --- | --- |
| Python standard library | math, dataclasses, json and pathlib support formulas, result records and summaries. [src/digitalmodel/geotechnical/anchors.py:L9-L10] [src/digitalmodel/geotechnical/cfg_workflows.py:L7-L10] |
| PyYAML, Pint and shared unit registry | Database parsing/conversion only; quantity provenance remains attached to the original row. [src/digitalmodel/geotechnical/database.py:L9-L13] [src/digitalmodel/geotechnical/database.py:L92-L121] |
| Source transcription | Public repository data consumed by default path; values and identifying source/site text are not reproduced here. [src/digitalmodel/geotechnical/database.py:L15-L28] [tests/geotechnical/test_geotech_database.py:L135-L163] |
| Engine | Dispatches six geotechnical-related basenames to the module adapters. [src/digitalmodel/engine.py:L687-L719] |
| Durable workflows | Seven main offline entries are the R5–R11 examples; three additional atlas-query entries belong to the parametric consumer route rather than the calculator regression set. [docs/registry/workflows.yaml:L820-L837] [docs/registry/workflows.yaml:L847-L942] |
| Parametric atlases | Anchor, pile and mudmat registry entries declare atlas metadata; interpolated results do not independently validate the generating method. [docs/registry/workflows.yaml:L855-L899] [docs/registry/workflows.yaml:L917-L933] |

## 10. Coverage and open items

All 11 Python files in the package were read, including the package initializer, calculator primitives/composite APIs, database/schema and both workflow adapters; there are no scoped-out subpackages. [src/digitalmodel/geotechnical/__init__.py:L1-L1] [src/digitalmodel/geotechnical/anchors.py:L1-L163] [src/digitalmodel/geotechnical/scour.py:L1-L224] [src/digitalmodel/geotechnical/piles.py:L1-L302] [src/digitalmodel/geotechnical/pile_capacity.py:L1-L141] [src/digitalmodel/geotechnical/mudmat.py:L1-L226] [src/digitalmodel/geotechnical/liquefaction.py:L1-L156] [src/digitalmodel/geotechnical/database.py:L1-L171] [src/digitalmodel/geotechnical/database_schema.py:L1-L121] [src/digitalmodel/geotechnical/workflow.py:L1-L188] [src/digitalmodel/geotechnical/cfg_workflows.py:L1-L328]

Test inventory: every node in `tests/geotechnical/` and the three `tests/test_geotechnical_*.py` files, plus the two section-4 formula tests and seven named workflow-registry cases; the test source files and selected node definitions are cited in sections 4–6. On-bottom-stability assertions in the package test file exercise the separate `subsea` package; their execution does not add a method to this pack's scope. [tests/geotechnical/test_geotechnical.py:L18-L43] [tests/geotechnical/test_geotechnical.py:L102-L171] [tests/test_geotechnical_anchors.py:L8-L112] [tests/test_geotechnical_piles.py:L7-L136] [tests/test_geotechnical_scour.py:L7-L121]

Execution record (2026-10-16): the full module/formula command collected 203 tests (201 in the module suites plus 2 workflow formula tests) and the scratch-copy main-workflow command collected 7 tests; BASE and AFTER each have 210 passed, 0 failed, 0 skipped and 0 NOT_RUN at code pin `bb18c1a346b31b4ce975befa41fc84d9a5ff29e5`, with unchanged calculator files. These are execution observations, not standard-validation claims.

Generic command form used for both phases: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <module-suite paths and two formula node IDs> -q -rA -p no:cacheprovider --basetemp=<scratch directory> --junitxml=<scratch file>`; run the seven R5–R11 registry node IDs in a temporary copy with the same form and the same environment. The declared test extra supplies pytest plugins; no addopts override or solver execution was used.

Documentation discovery used `rg -n -i 'geotechnical|pile.capacity|mudmat|liquefaction|anchor.capacity' docs --glob '*.md' --glob '*.rst' --glob 'README*'`; 34 text documents were inspected for relevant statements, with four identifying or process-specific filenames omitted from the public discovery list; issue pending (6). The safe paths below record discovery, not endorsement of their counts or method claims.

| Discovery document | Relevant lines at the code pin |
| --- | --- |
| `docs/capability-map/capabilities-ia-spec-1444.md` | [docs/capability-map/capabilities-ia-spec-1444.md:L31-L110] |
| `docs/domains/SPEC_AUDIT_REPORT.md` | [docs/domains/SPEC_AUDIT_REPORT.md:L61-L61] |
| `docs/domains/ansys/workflows/aqwa-to-structural-submodel.md` | [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L60-L60] |
| `docs/domains/cathodic_protection/_index.md` | [docs/domains/cathodic_protection/_index.md:L102-L102] |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | [docs/domains/marine-engineering/marine_3d_models_catalog.md:L98-L596] |
| `docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md:L368-L368] |
| `docs/domains/mooring/screening/README.md` | [docs/domains/mooring/screening/README.md:L45-L45] |
| `docs/domains/mooring/screening/README.template.md` | [docs/domains/mooring/screening/README.template.md:L40-L40] |
| `docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md` | [docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md:L69-L69] |
| `docs/domains/pipeline-installation/coverage-2026-10.md` | [docs/domains/pipeline-installation/coverage-2026-10.md:L36-L36] |
| `docs/domains/pipelines/freespan/freespan.md` | [docs/domains/pipelines/freespan/freespan.md:L10-L10] |
| `docs/domains/pipelines/plem/plem.md` | [docs/domains/pipelines/plem/plem.md:L8-L14] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md:L922-L922] |
| `docs/domains/project-docs/phase1-implementation-report.md` | [docs/domains/project-docs/phase1-implementation-report.md:L737-L737] |
| `docs/domains/references/geotechnical.md` | [docs/domains/references/geotechnical.md:L9-L17] |
| `docs/field-development/subsea-production-systems-mapping.md` | [docs/field-development/subsea-production-systems-mapping.md:L94-L94] |
| `docs/maps/digitalmodel-operator-map.md` | [docs/maps/digitalmodel-operator-map.md:L43-L43] |
| `docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md` | [docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md:L23-L23] |
| `docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md` | [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L24-L24] |
| `docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md` | [docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md:L19-L444] |
| `docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md` | [docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md:L43-L43] |
| `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21] |
| `docs/plans/2026-10-03-issue-2281-cp-review-report-portfolio.md` | [docs/plans/2026-10-03-issue-2281-cp-review-report-portfolio.md:L73-L83] |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | [docs/plans/parametric-refresh/adding-an-atlas.md:L74-L114] |
| `docs/plans/parametric-refresh/atlas-spec.md` | [docs/plans/parametric-refresh/atlas-spec.md:L29-L29] |
| `docs/research/mooring-academic-references.md` | [docs/research/mooring-academic-references.md:L57-L289] |
| `docs/research/mooring-industry-practice.md` | [docs/research/mooring-industry-practice.md:L135-L135] |
| `docs/research/subsea-pipelines-academic-references.md` | [docs/research/subsea-pipelines-academic-references.md:L169-L522] |
| `docs/research/subsea-pipelines-industry-practice.md` | [docs/research/subsea-pipelines-industry-practice.md:L96-L189] |
| `docs/vision/CALCULATIONS-VISION.md` | [docs/vision/CALCULATIONS-VISION.md:L34-L261] |

The older vision-page counts are historical claims, not the test inventory for this pin; its on-bottom-stability reference also points beyond the present package. [docs/vision/CALCULATIONS-VISION.md:L238-L255] [tests/geotechnical/test_geotechnical.py:L102-L171]

| Open item | Responsible role | Tracking |
| --- | --- | --- |
| U1: qualify editions, titles, clause/table attributions and Citation sidecars; resolve vision-page anchor-reference mismatch; licensed clause text remains unchecked | Calculation maintainer | issue pending (1); source-specific scour concern also #2615 |
| Shared numeric/locale/FoS/demand validation; empty anchor/scour summaries; layer-thickness consistency; non-finite liquefaction status | Calculation maintainer | issue pending (2) |
| Mudmat sliding zero fallback, horizontal-load signs, soil-angle/strength domain and depth clipping | Calculation maintainer | issue pending (3) |
| Database parser/exception/missing-bound probes and engineering-target unit contract | Data maintainer | issue pending (4) |
| Implementation-map families and rows, complete gate-evidence/fingerprint support and shared lesson registration | Calculation maintainer | issue pending (5) |
| Discovery codenaming and restricted example provenance; sidecar entry: to be created where needed; no restricted values are included in this pack | Documentation maintainer | issue pending (6) |
| Liquefaction cap semantics, deep-layer rd and screening applicability | Calculation maintainer | issue pending (7) |
| Scour sediment/depth/stability behavior and reference suitability | Calculation maintainer | #2615 |
| Broader plate-anchor/soil-profile and pile lateral-response coverage remains outside the implemented scope described here | Calculation maintainer | #147, #262 |

Independent provider clearance and issue creation from the numbered drafts remain prerequisites for replacing draft markers with issue numbers; this document records a draft knowledge pack, not an engineering design acceptance.

## Riprap residual scour addition (issues #266 and #148)

`riprap_residual_scour_depth` adds a two-stage steady-current calculation:
first estimate local armour shear from approach shear, then calculate residual
scour using excess shear and dimensionless grain sizes. Inputs use SI units;
water density, both grain densities and viscosity are explicit. The optional
`scour.riprap` workflow block writes the intermediate values to the summary.
`riprap_scour_from_dimensionless` exposes the second stage separately for checking
printed dimensionless tables.
The keyword argument `sediment_d90_m` is mandatory; theta_c = 0.045 remains
an unverified screening assumption pending a checked source equation.

Reference: Yang X., Qin Y., Yang S., Zhang M., Yu G. (2026),
“A semi-empirical prediction model for scour depth around riprap-protected
monopiles under steady currents,” *Applied Ocean Research* **176**, 105302.
[DOI: 10.1016/j.apor.2026.105302](https://doi.org/10.1016/j.apor.2026.105302).
The article is CC BY-NC-ND 4.0. No article text, figures or downloaded dataset
are included in this addition; the example inputs and current tests are synthetic.

The fit covers excess shear 0.04–3.60, sediment D* 3.77–23.87, rock D*
35.87–1339.64 and pile diameter/water depth 0.15–0.80. Outside these bounds,
including below armour motion, both depth outputs are unavailable (`None` in
Python, `null` in JSON). Results name every group outside its range. The physical
setup assumes an intact armour layer of thickness 10 rock diameters and footprint
diameter 5 pile diameters. The 24-hour laboratory tests do not establish an
equilibrium prediction. Use the method for screening and inspection planning;
it is not a design criterion and does not support prototype extrapolation.

**Published benchmark pending:** 0 of the 73 measured scour cases and 0 of the
18 shear cases have been transcribed or tested in this addition because the
source tables were unavailable. Synthetic equation checks do not validate the
fit against observations. The reported density/table inconsistency also needs
checking against the original tables before claiming physical-input reproduction.
Keep this addition in draft until the tables have been checked and a parametrised
benchmark added. Use a fixed 20% relative tolerance on each measured hs/D;
explicitly flag failures and report their identities and errors without increasing
the tolerance. Preserve printed dimensionless inputs independently from any
reconstructed density assumptions.
