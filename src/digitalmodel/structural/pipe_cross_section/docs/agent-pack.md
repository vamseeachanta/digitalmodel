# Pipe cross-section knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/pipe_cross_section |
| Repository | digitalmodel |
| Package | `src/digitalmodel/structural/pipe_cross_section` |
| Pack revision | 0 |
| Code pin | `309d343e08a2f83aebc6ea5344e586407b80c1f8` |
| Map pin | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Review allocation | Engineering calculations quality manager; deputy: engineering calculations deputy quality manager |
| Status | DRAFT |

Citations resolve at the code pin, except implementation-map citations, which resolve at the map pin.

## 1. Purpose and scope

`PipeCrossSection` constructs steel, optional LPP and optional concrete annuli and computes layer geometry, dry material mass, displaced water mass and submerged mass equivalent per axial length. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L170]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L172-L241]`

The model assumes uniform concentric circular layers, full immersion and a sealed bore; dry mass excludes internal contents, and the calculation establishes no code capacity. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]` `[docs/examples/circular-section-weight-buoyancy.md:L124-L138]`

The containing structural domain is recorded as stable; this is registry maturity, not independent qualification of every pipe input or standard claim. `[docs/registry/module-routing.yaml:L528-L533]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]`

The package exports models, conversion functions, the calculator, visualization helpers and a CLI; HTML, image and CSV writers operate on the calculator result. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L59-L104]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L548-L564]`

## 2. Inputs and outputs

Input ranges below are **use constraints for this pack**, not constructor validation: `_build_layers` performs arithmetic directly and includes each coating only when its thickness is positive. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L83-L85]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L170]`

| Input | Unit | Use constraint | Default / source | Evidence |
| --- | --- | --- | --- | --- |
| `steel_od_mm`, `steel_wt_mm` | mm | Finite OD > 0; 0 < wall < OD/2 | Required in direct constructor; dictionary fallback is zero | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L71-L85]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` |
| `lpp_thickness_mm`, `concrete_thickness_mm` | mm, radial | Finite and ≥ 0; zero omits layer | 0 for each | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L74-L77]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L145-L170]` |
| `steel_density`, `lpp_density`, `concrete_density`, `seawater_density` | kg/m³ | Finite and > 0 for every active material and external fluid; supply a separate data basis | Code defaults exist; do not treat them as certified material data | `[src/digitalmodel/structural/pipe_cross_section/models.py:L139-L148]` `[docs/examples/circular-section-weight-buoyancy.md:L16-L30]` |
| `internal_contents`, `internal_fluid_density` | label; kg/m³ | Restrict this pack to sealed air bore with negligible contents mass | `air`; 0; density is accepted but absent from mass sum | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L79-L80]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L221]` `[docs/examples/circular-section-weight-buoyancy.md:L124-L132]` |
| `from_inches` arguments | inch except `lpp_thickness_mm` | Same geometric constraints after conversion | Coatings default to 0; LPP remains metric | `[src/digitalmodel/structural/pipe_cross_section/models.py:L150-L166]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L109-L125]` |
| `from_dict` | mapping, optionally under `pipe_cross_section` | Unitless numerics interpreted as mm / kg/m³; aliases `Nominal_OD`, `Design_WT` | Missing geometry silently becomes zero | `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` |
| CLI geometry and density flags | mm, inch, kg/m³ | Numeric parser; YAML takes precedence, then a complete imperial pair, then a complete metric pair | Coating flags default to 0; density flags use code defaults | `[src/digitalmodel/structural/pipe_cross_section/cli.py:L58-L127]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L192-L223]` |

| Output | Unit / representation | Range and precision | Evidence |
| --- | --- | --- | --- |
| `layers`; `inner_diameter_mm`, `outer_diameter_mm` | ordered material annuli; mm | Steel first, LPP next when present, concrete last when present | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L185]` |
| `cross_sectional_area_m2`, `weight_per_meter_kg` | m²; kg/m | Unrounded properties; density times annulus area | `[src/digitalmodel/structural/pipe_cross_section/models.py:L76-L104]` |
| `total_weight_in_air_kg_m`, `buoyancy_force_kg_m`, `submerged_weight_kg_m` | kg/m mass equivalents | Dry sum; displaced-water mass; dry minus displaced mass | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L231]` |
| `displaced_volume_m3_m` | m³/m | Full exterior disk including sealed bore | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L202-L211]` |
| `*_kN_m`, `weight_per_meter_kN` | kN/m | Fixed code gravity 9.81 m/s² and conversion by 1000 | `[src/digitalmodel/structural/pipe_cross_section/models.py:L101-L104]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L197-L226]` |
| `is_buoyant` | boolean | True strictly below 0 kg/m; zero is False | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L228-L231]` |
| `to_dict`, `get_summary`, `BuoyancyResult.to_dict`, `to_csv_data` | mappings / rows | Derived values rounded; configuration numerics retain precision; full configuration export omits internal-fluid density | `[src/digitalmodel/structural/pipe_cross_section/models.py:L106-L117]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L221-L230]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L244-L300]` |
| HTML / Plotly, static image, CSV, CLI JSON | display / files | Writers use dictionary labels for units; CSV derives from `to_csv_data`; CLI JSON derives from `to_dict` | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L314-L334]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L445-L545]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L239-L266]` |

The plotted cross-section uses x/y in mm with the bore centred at (0, 0); positive submerged mass means downward imbalance, and there is no phase convention in these static circle calculations. `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L32]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L90-L147]` `[docs/examples/circular-section-weight-buoyancy.md:L65-L76]`

YAML is read with `yaml.safe_load`, without an explicit duplicate-key or unit schema check; the fallback parser is used only when PyYAML cannot import. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L172-L185]`

### Public symbol inventory

The table records source definitions; local re-exports share those definitions. Input quantities, output units and use limits are stated above.

| Public symbol | Source description | Evidence |
| --- | --- | --- |
| `PipeCrossSection` | Complete pipe cross-section with multiple coating layers. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L43-L330]` |
| `PipeCrossSection.buoyancy_force_kN_m` | Buoyancy force per meter (kN/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L214-L216]` |
| `PipeCrossSection.buoyancy_force_kg_m` | Buoyancy force per meter (kg/m equivalent). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L209-L211]` |
| `PipeCrossSection.displaced_volume_m3_m` | Volume of water displaced per meter (m³/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L203-L206]` |
| `PipeCrossSection.from_config` | Create from configuration object. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L88-L101]` |
| `PipeCrossSection.from_dict` | Create from dictionary configuration (YAML pattern). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L104-L107]` |
| `PipeCrossSection.from_inches` | Create from imperial units (common in API standards). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L110-L125]` |
| `PipeCrossSection.get_buoyancy_result` | Get structured buoyancy analysis result. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L233-L242]` |
| `PipeCrossSection.get_summary` | Get summary of pipe properties. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L244-L259]` |
| `PipeCrossSection.inner_diameter_mm` | Internal bore diameter in mm. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L178-L180]` |
| `PipeCrossSection.is_buoyant` | True if pipe floats (negative submerged weight). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L229-L231]` |
| `PipeCrossSection.outer_diameter_inch` | Overall outer diameter in inches. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L188-L190]` |
| `PipeCrossSection.outer_diameter_mm` | Overall outer diameter in mm. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L183-L185]` |
| `PipeCrossSection.print_summary` | Print formatted summary to console. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L302-L330]` |
| `PipeCrossSection.steel_layer` | Get the steel pipe layer. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L173-L175]` |
| `PipeCrossSection.submerged_weight_kN_m` | Submerged weight per meter (kN/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L224-L226]` |
| `PipeCrossSection.submerged_weight_kg_m` | Submerged weight per meter (kg/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L219-L221]` |
| `PipeCrossSection.to_csv_data` | Generate CSV-ready data rows. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L280-L300]` |
| `PipeCrossSection.to_dict` | Convert full pipe configuration to dictionary. | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L261-L278]` |
| `PipeCrossSection.total_weight_in_air_kN_m` | Total weight in air per meter (kN/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L198-L200]` |
| `PipeCrossSection.total_weight_in_air_kg_m` | Total weight in air per meter (kg/m). | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L195]` |
| `load_config_from_yaml` | Load configuration from YAML file. | `[src/digitalmodel/structural/pipe_cross_section/cli.py:L172-L185]` |
| `main` | Main CLI entry point. | `[src/digitalmodel/structural/pipe_cross_section/cli.py:L188-L271]` |
| `parse_args` | Parse command line arguments. | `[src/digitalmodel/structural/pipe_cross_section/cli.py:L41-L169]` |
| `BuoyancyResult` | Results from buoyancy analysis. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L202-L230]` |
| `BuoyancyResult.to_dict` | Convert to dictionary. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L221-L230]` |
| `CoatingType` | Standard coating types for subsea pipelines. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L24-L33]` |
| `InternalContents` | Internal pipe contents options. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L36-L43]` |
| `PipeCrossSectionConfig` | Configuration for pipe cross-section analysis. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L121-L198]` |
| `PipeCrossSectionConfig.from_dict` | Create configuration from dictionary (YAML config pattern). | `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` |
| `PipeCrossSectionConfig.from_inches` | Create configuration from imperial units. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L151-L166]` |
| `PipeCrossSectionConfig.to_dict` | Convert to dictionary for YAML export. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L185-L198]` |
| `PipeLayer` | Represents a single layer of a pipe cross-section. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L57-L117]` |
| `PipeLayer.cross_sectional_area_m2` | Cross-sectional area of the layer material in m². | `[src/digitalmodel/structural/pipe_cross_section/models.py:L92-L94]` |
| `PipeLayer.inner_radius_m` | Inner radius in meters. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L82-L84]` |
| `PipeLayer.outer_radius_m` | Outer radius in meters. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L87-L89]` |
| `PipeLayer.thickness_mm` | Wall/coating thickness in mm. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L77-L79]` |
| `PipeLayer.to_dict` | Convert layer to dictionary for export. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L106-L117]` |
| `PipeLayer.weight_per_meter_kN` | Weight per meter in kN/m. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L102-L104]` |
| `PipeLayer.weight_per_meter_kg` | Weight per meter of pipe layer in kg/m. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L97-L99]` |
| `inch_to_mm` | Convert inches to millimeters. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L46-L48]` |
| `mm_to_inch` | Convert millimeters to inches. | `[src/digitalmodel/structural/pipe_cross_section/models.py:L51-L53]` |
| `PipeCrossSectionVisualizer` | Visualization generator for pipe cross-sections. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L35-L545]` |
| `PipeCrossSectionVisualizer.create_cross_section_figure` | Create interactive cross-section diagram with Plotly. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L55-L149]` |
| `PipeCrossSectionVisualizer.create_plotly_figure` | Create combined cross-section and weight distribution figures. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L199-L205]` |
| `PipeCrossSectionVisualizer.create_weight_distribution_figure` | Create weight distribution pie chart with Plotly. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L151-L197]` |
| `PipeCrossSectionVisualizer.export_csv` | Export pipe properties to CSV file. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L534-L545]` |
| `PipeCrossSectionVisualizer.export_static_image` | Export static PNG image of cross-section using matplotlib. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L445-L532]` |
| `PipeCrossSectionVisualizer.generate_html_report` | Generate complete interactive HTML report. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L314-L443]` |
| `PipeCrossSectionVisualizer.get_plots_html` | Generate HTML for both charts in a responsive grid layout. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L207-L233]` |
| `create_circle_points` | Generate x, y coordinates for a circle. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L32]` |
| `export_csv` | Export pipe properties to CSV. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L561-L564]` |
| `export_static_image` | Export static PNG image of pipe cross-section. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L555-L558]` |
| `generate_html_report` | Generate HTML report for a pipe cross-section. | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L549-L552]` |

## 3. Method and governing standards

| Method | Implementation and calculation basis | Evidence |
| --- | --- | --- |
| Annular geometry | `PipeLayer.thickness_mm=(OD-ID)/2`; radii convert mm to m before `A=π(R²-r²)` | `[src/digitalmodel/structural/pipe_cross_section/models.py:L76-L99]`; independent circle integral `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L54-L87]` |
| Material mass | `PipeLayer.weight_per_meter_kg=ρA`; `total_weight_in_air_kg_m` sums material layers only | `[src/digitalmodel/structural/pipe_cross_section/models.py:L91-L104]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L192-L200]`; hand calculation `[docs/examples/circular-section-weight-buoyancy.md:L41-L71]` |
| Exterior displacement and buoyancy | `displaced_volume_m3_m=πR_outer²`; `buoyancy_force_kg_m=ρ_water πR_outer²` | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L202-L216]`; independent integral `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L74-L105]` |
| Submerged mass and sign | `submerged_weight_kg_m=dry-displaced_water`; `is_buoyant` tests < 0 | `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L218-L241]`; hand balance `[docs/examples/circular-section-weight-buoyancy.md:L65-L76]` |
| Force conversion | Mass equivalent multiplied by the code's 9.81 m/s², divided by 1000 to kN/m | `[src/digitalmodel/structural/pipe_cross_section/models.py:L101-L104]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L197-L226]` `[docs/examples/circular-section-weight-buoyancy.md:L32-L39]` |
| Unit conversion | `inch_to_mm` / `mm_to_inch` use the shared Pint registry | `[src/digitalmodel/structural/pipe_cross_section/models.py:L46-L53]` `[src/digitalmodel/units.py:L7-L10]` |
| Plot geometry | `create_circle_points` samples a circle; visualization is a presentation of existing mass properties | `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L32]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L55-L102]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L151-L173]` |

The following are source claims, not established design checks; the package introduction and HTML footer name standards without an edition or clause, while the calculator expressly disclaims code capacity. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L19-L22]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L432-L435]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]`

| Named standard | Traceability limit | Follow-up |
| --- | --- | --- |
| API 5L — Line Pipe Specification (source wording) | UNVERIFIED: compliance, edition and governing clause are not established; clause text not checked against the licensed copy. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L19-L22]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L14-L14]` | #1817 |
| DNV-ST-F101 — Submarine Pipeline Systems (source wording) | UNVERIFIED: compliance, edition and governing clause are not established; clause text not checked against the licensed copy. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L19-L22]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L14-L14]` | #1817 |
| ISO 21809 — External Coatings (source wording) | UNVERIFIED: compliance, part, edition and governing clause are not established; clause text not checked against the licensed copy. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L19-L22]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L13-L13]` | #1817 |

No conflicting editions can be reconciled from those unversioned claims; the listed arithmetic paths emit numeric mappings rather than `Citation` sidecars, so standards provenance remains open. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L233-L278]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L106-L117]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L221-L230]`

## 4. Validated worked examples

These cases use synthetic inputs and independently reduced circle integrals, not licensed material tables; expected values are retained at full precision for assertions, with rounded kg/m values shown here only for identification. `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L19-L43]` `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L54-L117]` `[docs/examples/circular-section-weight-buoyancy.md:L41-L76]`

| Case / kind | Input and independent expected value | Tolerance / basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 bare / validated | Inputs and annulus integral in layered example; submerged mass 15.0 kg/m displayed | Relative 1e-12; absolute floors 1e-15 m² and 1e-10 kg/m, binary arithmetic allowance | `tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_layered_pipe_buoyancy[bare]` | PASS at BASE and after; `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L21-L51]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L85-L117]` |
| V2 coated / validated | Same synthetic steel plus coating; submerged mass 15.6 kg/m displayed | Same explicit numerical tolerances as V1 | `tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_layered_pipe_buoyancy[coated]` | PASS at BASE and after; `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L21-L51]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L85-L117]` |
| V3 three layers / validated | S1 inputs and H1 closed form `11.64π kg/m`, downward positive | `pytest.approx` default numerical tolerance; sign also asserted | `tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_submerged_mass` | PASS at BASE and after; `[docs/examples/circular-section-weight-buoyancy.md:L16-L30]` `[docs/examples/circular-section-weight-buoyancy.md:L65-L76]` `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L306-L311]` |
| R1 bare serialization / regression | Same V1 inputs; retained configuration and rounded masses / areas | Exact dictionary-value equality; tests deliberately distinguish live and rounded mass | `tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_pipe_buoyancy_serialization[bare]` | PASS at BASE and after; `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L120-L161]` |
| R2 coated serialization / regression | Same V2 inputs; retained configuration and rounded masses / areas | Exact dictionary-value equality | `tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_pipe_buoyancy_serialization[coated]` | PASS at BASE and after; `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L120-L161]` |

Execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/structural/pipe_cross_section -q -rA -p no:cacheprovider --basetemp=SCRATCH_CASE_DIRECTORY`, unchanged repository addopts, no extra installation, code pin as above; BASE and after each ran 38 tests: 38 passed, 0 failed, 0 skipped, 0 NOT_RUN. The suite comprises 28 original calculator tests, 6 additional H1 cases and 4 parameterized layered-example cases. `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L21-L258]` `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L274-L327]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L85-L161]`

The older expected-values fixture claims manual and software verification without identifying the software or reference revision; its rounded values are used as regression expectations, not independent validation evidence. `[tests/structural/pipe_cross_section/conftest.py:L72-L96]`

## 5. QA gates

AQG r0, section 10, Python calculations assigns G0 inputs/units/citations, G1 imports/tests and G2 hand/published checks; G3 and G4 are NOT_APPLICABLE, and section 9 requires G5 evidence for every family. (AQG r0, sections 9–10, Python calculations.)

The following are executable review criteria; a check record must retain the input, code pin, node ID or calculation, result, units and tolerance. The numeric criteria concern arithmetic or evidence completeness, not code capacity. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L46-L51]`

| Gate / check | Module-specific execution | Numeric pass criterion and unit | Evidence / result |
| --- | --- | --- | --- |
| G0 / C01 | Inspect every input to `PipeCrossSection.from_dict` / direct constructor before calculation: finite positive OD, 0 < wall < OD/2; nonnegative coatings; finite positive active densities; negligible bore mass | 0 invalid fields [count]; all dimensions declared mm and densities kg/m³; unsupported nonempty-bore cases = 0 [count] | Input worksheet for V1/V2/V3, manually checked PASS; constructor itself has no rejection guarantee. `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L170]` `[docs/examples/circular-section-weight-buoyancy.md:L124-L132]` |
| G0 / C02 | Compare `inch_to_mm` and `mm_to_inch` with the unit-conversion tests | Absolute errors < 0.1 mm and < 0.01 mm for supplied inch cases; inverse errors < 0.01 inch | Conversion assertions PASS; this checks unit conversion, not external text locale parsing. `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L188-L203]` |
| G1 / C03 | Import grouped package and run every node under `tests/structural/pipe_cross_section`; inspect compatibility redirect for flat test imports | 38 collected and passed [tests]; 0 failures/skips/NOT_RUN [tests] | BASE and after pytest node records PASS; Python API is the load route, with no solver load. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L59-L104]` `[src/digitalmodel/_compat.py:L90-L100]` `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L24-L26]` |
| G2 / C04 | Compare V1/V2 live layer areas, material masses and displaced disk against integral coefficients | Relative error ≤ 1e-12 [1], absolute floors 1e-15 m² / 1e-10 kg/m | Both parameterized integral cases PASS. `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L21-L51]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L85-L117]` |
| G2 / C05 | Recompute the three H1 force conversions using independent mass coefficients and the stated fixed gravity | Absolute error ≤ 1e-12 kN/m for each of 3 outputs; coefficients and gravity from H1/G1 | Scratch hand calculation PASS, plus existing force-conversion tests PASS. `[docs/examples/circular-section-weight-buoyancy.md:L32-L39]` `[docs/examples/circular-section-weight-buoyancy.md:L65-L76]` `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L313-L327]` |
| G3 | NOT_APPLICABLE: static section arithmetic, no time-dependent equation or solver run | Not applicable | Scope excludes dynamic/code capacity checks. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L241]` |
| G4 | NOT_APPLICABLE: concentric analytic area, no diffraction mesh or hydrodynamic solution | Not applicable | Displacement is a circle formula. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L202-L216]` |
| G5 / C06 | Execute R1/R2 configuration and rounded mass/area serialization checks separately from live values | 0 mismatched serialized fields [count] across 2 cases | R1/R2 PASS; serialization does not retain unrounded derived values. `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L120-L161]` |
| G5 / C07 | Resolve all pack citation ranges at their named pins; match every section-4 node to passed BASE/after records; retain numeric criteria and open-item references | 0 unresolved citations [count], 0 absent node results [count], 0 unreferenced open items [count] | Citation resolver and evidence audit; appendix manifest requirement, not a claim that the module writes a QA appendix. `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L54-L82]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L244-L278]` |

## 6. Known failure modes and pitfalls

sweep: 36 found

The count is 36 semantic findings in the table; repeated defaults are grouped by quantity with every occurrence cited. These include documented assumptions and benign defaults as well as error paths; the rows do not imply 34 confirmed defects.

All sweep findings were observed during source inspection on 2026-11-07 at the code pin; their citations identify the cause rather than a dated production incident.

Commands run over the entire package, including all subfolders:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(?:int|float)\(|\b1970\b|=\s*0(?:\.0)?\b|0\.05|\b(?:density|inch|mm|kN|kg|cache)\b' src/digitalmodel/structural/pipe_cross_section --glob '*.py'
rg -n 'default=|default_factory|\.get\(|safe_load|default=str|round\(|9\.81|1000|2000|25\.4|internal_fluid|is_buoyant|> 0|< 0' src/digitalmodel/structural/pipe_cross_section --glob '*.py'
```

| ID | Symptom and cause at the code pin | Detection / disposition |
| --- | --- | --- |
| S01 | Missing OD becomes zero. `[src/digitalmodel/structural/pipe_cross_section/models.py:L173-L173]` | C01 preflight; issue pending (1) |
| S02 | Missing wall becomes zero. `[src/digitalmodel/structural/pipe_cross_section/models.py:L174-L174]` | C01 preflight; issue pending (1) |
| S03 | Absent LPP defaults to zero at every constructor/config/CLI entry. `[src/digitalmodel/structural/pipe_cross_section/models.py:L142-L142]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L155-L155]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L176-L176]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L74-L74]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L114-L114]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L70-L75]` | C01; intentional omission, verify layer count with C03 |
| S04 | Absent concrete defaults to zero at every constructor/config/CLI entry. `[src/digitalmodel/structural/pipe_cross_section/models.py:L144-L144]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L156-L156]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L178-L178]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L76-L76]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L115-L115]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L76-L81]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L95-L100]` | C01; intentional omission, verify layer count with C03 |
| S05 | Internal-fluid density defaults to zero; supplied nonzero density is unused. `[src/digitalmodel/structural/pipe_cross_section/models.py:L148-L148]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L182-L182]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L80-L80]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L221]` | C01 excludes unsupported contents; no detection check exists for a fluid-mass contribution; issue pending (2) |
| S06 | Steel density silently uses a code default. `[src/digitalmodel/structural/pipe_cross_section/models.py:L141-L141]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L175-L175]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L73-L73]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L104-L109]` | C01 requires explicit basis; issue pending (1) |
| S07 | LPP density silently uses a code default. `[src/digitalmodel/structural/pipe_cross_section/models.py:L143-L143]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L177-L177]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L75-L75]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L110-L115]` | C01 requires explicit basis; issue pending (1) |
| S08 | Concrete density silently uses a code default. `[src/digitalmodel/structural/pipe_cross_section/models.py:L145-L145]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L179-L179]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L77-L77]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L116-L121]` | C01 requires explicit basis; issue pending (1) |
| S09 | External-water density silently uses a code default. `[src/digitalmodel/structural/pipe_cross_section/models.py:L146-L146]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L180-L180]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L78-L78]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L122-L127]` | C01 requires explicit basis; issue pending (1) |
| S10 | Internal-contents label defaults to air and changes no mass. `[src/digitalmodel/structural/pipe_cross_section/models.py:L147-L147]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L181-L181]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L79-L79]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L221]` | C01 restricts use; issue pending (2) |
| S11 | Zero or negative LPP thickness omits the layer without rejection. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L145-L157]` | C01 rejects negative input; C03 covers zero; issue pending (1) |
| S12 | Zero or negative concrete thickness omits the layer without rejection. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L159-L170]` | C01 rejects negative input; C03 covers zero; issue pending (1) |
| S13 | Direct geometry/density dataclasses accept invalid/nonfinite quantities. `[src/digitalmodel/structural/pipe_cross_section/models.py:L69-L99]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L71-L85]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L141]` | C01 catches unsupported inputs before call; no constructor rejection test exists; issue pending (1) |
| S14 | Legacy OD and wall aliases carry no explicit unit metadata. `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` | C01 declares mm; no alias-unit detection check exists; issue pending (1) |
| S15 | CLI imperial branch has precedence over a complete metric pair and tests truthiness. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L192-L209]` | no detection check exists for mixed or zero-valued CLI inputs; issue pending (1) |
| S16 | CLI metric branch tests truthiness rather than numeric validity. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L209-L223]` | C01 preflight; no CLI rejection check exists; issue pending (1) |
| S17 | Imperial concrete selection uses truthiness fallback and zero. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L197-L208]` | no detection check exists for mixed concrete-unit flags; issue pending (1) |
| S18 | PyYAML loader can accept duplicate keys without a module schema check. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L172-L185]` | no detection check exists; issue pending (1) |
| S19 | YAML parser changes to a utility fallback after ImportError; second ImportError raises visibly. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L174-L185]` | no detection check exists for fallback equivalence; issue pending (1) |
| S20 | PNG exceptions are caught and execution continues; quiet mode suppresses the message. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L245-L253]` | no detection check exists; issue pending (3) |
| S21 | Missing matplotlib prints a message and returns None; CLI does not inspect the return. `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L445-L454]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L245-L253]` | no detection check exists for missing image acknowledgement; issue pending (3) |
| S22 | CLI JSON default=str converts unsupported objects rather than rejecting them. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L261-L266]` | no detection check exists; issue pending (3) |
| S23 | Layer radius conversion assumes bare millimetre numerics. `[src/digitalmodel/structural/pipe_cross_section/models.py:L81-L99]` | C02 unit conversion and C04 area checks |
| S24 | Displaced disk converts outer diameter from mm and includes sealed bore. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L202-L211]` | C04 displacement; use sealed-bore assumption only |
| S25 | Imperial constructors retain LPP thickness in mm while converting other dimensions. `[src/digitalmodel/structural/pipe_cross_section/models.py:L150-L166]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L109-L125]` | C02 conversion coverage; no mixed-argument LPP check exists; issue pending (1) |
| S26 | Layer force conversion fixes gravity rather than receiving a local value. `[src/digitalmodel/structural/pipe_cross_section/models.py:L101-L104]` | C05 convention check; no separate layer-force test exists; issue pending (4) |
| S27 | Total force conversion fixes gravity. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L197-L200]` | C05 dry force, units kN/m |
| S28 | Buoyancy force conversion fixes gravity. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L213-L216]` | C05 buoyancy force, units kN/m |
| S29 | Submerged force conversion fixes gravity. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L223-L226]` | C05 submerged force, units kN/m |
| S30 | Neutral zero mass has inconsistent presentation: is_buoyant is False, HTML card uses FLOATS. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L228-L231]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L235-L239]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L258-L261]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L328-L329]` | no detection check exists at neutral balance; issue pending (3) |
| S31 | Layer serialization rounds geometry/area/mass; kg labels are mass equivalents. `[src/digitalmodel/structural/pipe_cross_section/models.py:L106-L117]` | C06; do not use rounded output as hand-check input |
| S32 | Summary, buoyancy and CSV round independently. `[src/digitalmodel/structural/pipe_cross_section/models.py:L221-L230]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L244-L259]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L280-L300]` | C06 covers summary/buoyancy; no CSV precision check exists; issue pending (4) |
| S33 | Full pipe export omits internal_fluid_density from configuration. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L261-L278]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L185-L198]` | no detection check exists for full configuration round trip; issue pending (2) |
| S34 | HTML force/weight labels contain kg/m and inline inch conversion uses a literal factor. `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L249-L261]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L399-L420]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L494-L498]` | C02 conversion basis; no HTML unit-label or image conversion check exists; issue pending (3) |
| S35 | Changing geometry or density fields after construction does not rebuild the stored layers automatically. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L71-L85]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L170]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L172-L200]` | no detection check exists for stale layer state after mutation; issue pending (1) |
| S36 | CLI dimension and density flags convert external text through `type=float`; locale separators receive no normalization, while nonfinite numeric tokens can reach the calculator. `[src/digitalmodel/structural/pipe_cross_section/cli.py:L58-L127]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L197-L220]` | C01 finite-value preflight; no detection check exists for locale-token policy; issue pending (1) |

Sweep classification: no `continue`, external-text `int()`/`float()`, empty-result cache write, epoch fallback or empty-container error return occurs in these source files. List initialization in `_build_layers` and `to_csv_data`, zero plot origins/annotation positions and `default_factory=list` are ordinary construction or layout, not swallowed errors. Plotly import failures raise visibly; matplotlib and CLI PNG handling are recorded above. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L81-L85]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L141]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L280-L300]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L32]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L55-L60]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L151-L156]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L445-L454]`

Issues #1812 and #1817 are inventory and coverage-audit leads, rather than confirmed failure incidents. (Repository issues #1812 and #1817, bodies read for this pack.)

Issue #3310 requests a broader module report and value-asserting function review; #1847 requests a general section-property interface. Neither is treated as an incident proving this module failed, and no historical incident is assigned a date without evidence. (Repository issues #3310 and #1847, open bodies read for this pack.)

The source docstring advertises moment of inertia and broader coating support, but the implemented properties and layer builder cover the geometry/mass path above; no inertia result is exposed there. The existing report/interface work remains the follow-up, rather than a hand-check reference to an unimplemented property. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L8-L14]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L47-L59]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L241]` (Repository issues #3310 and #1847.)

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Use unrounded annulus integrals for checking and treat the serialized summary as a distinct output contract | `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L85-L161]` | Sections 2, 4, 5 and 6 distinguish numeric properties from rounded exports | Pending; issue pending (5) |

## 8. Ownership

Review allocation for this draft: engineering calculations quality manager; deputy: engineering calculations deputy quality manager. The maintainer assigns named reviewers and the producing engineering calculations contributors. Open engineering interpretations go to the family quality manager; unresolved cross-family scope goes to the maintainers. These are responsibilities assigned by this pack, not a record of a completed review.

## 9. Dependencies and alternative implementations

The calculation path uses Python math/dataclasses and the shared Pint quantity registry; optional Plotly and matplotlib imports occur inside visualization methods. YAML loading can fall back to the utility package, and the CLI serializes calculator output with JSON. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L28-L39]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L16-L21]` `[src/digitalmodel/units.py:L7-L10]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L55-L60]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L449-L454]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L172-L185]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L261-L266]`

The package initializer imports the visualizer and CLI, while the compatibility mapping routes flat `digitalmodel.pipe_cross_section` imports to `structural`; no solver interface appears in the package imports. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L59-L81]` `[src/digitalmodel/_compat.py:L90-L100]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L16-L22]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L29-L38]`

No upstream design dataset or solver version is required by the independent synthetic example: it constructs the calculator with explicit inputs and writes a JSON result in the supplied directory. This is an observed example dependency, not a claim of a production downstream integration. `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L128-L174]`

### Implementation-map inventory and acceptance

At the map pin in the header, the map data revision is recorded in the YAML header; the 13 `families` contain **0 in-package implementation rows**. The six package candidates sit in `rejected_candidates` with `needs_review`, explicitly excluded from implementation counts: `buoyancy_force_kg_m`, `buoyancy_force_kN_m`, `submerged_weight_kg_m`, `submerged_weight_kN_m`, `get_buoyancy_result` and `BuoyancyResult`, all associated with `dnv-rp-e305-on-bottom-stability`. `[docs/registry/implementation-map.yaml:L1-L4]` `[docs/registry/implementation-map.yaml:L346-L26502]` `[docs/registry/implementation-map.yaml:L52787-L52828]`

Inventory: empty. Counted families: 0. Counted-family signal grep: **NOT_APPLICABLE**, because no family has an implementation row inside the header package. Acceptance: empty inventory satisfied; no counted-family miss total is asserted from an unexecuted grep. The rejected candidates are not promoted to on-bottom stability implementations by this pack. `[docs/registry/implementation-map.yaml:L346-L26502]` `[docs/registry/implementation-map.yaml:L52787-L52828]`

Map follow-up: add or classify the section-3 method families for annular geometry and layer mass, exterior displacement and submerged mass, force/unit conversion, and circle plotting; disposition the six rejected candidates first. These uncounted families require map rows before they enter a later counted-family review; issue pending (5). `[src/digitalmodel/structural/pipe_cross_section/models.py:L46-L104]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L241]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L32]` `[docs/registry/implementation-map.yaml:L52787-L52828]`

### Repository-wide supplementary search

The following whole-repository searches include `scripts/` and tests; they are supplementary map findings and do not change the empty counted-family result:

```sh
git grep -n -E 'PipeCrossSection|steel_od_mm|buoyancySectionProperties|def.*submerged_weight|def.*annul' CODE_PIN -- '*.py'
git grep -n -i -E 'pi.*(outer|inner|radius|diameter)|displaced.*(volume|mass)|submerged.weight' CODE_PIN -- '*.py'
```

| Route found | Classification from inspected body | Follow-up |
| --- | --- | --- |
| `subsea/cross_sections/calculations.py`: `calculate_annular_layer`, `_annular_geometry` | Separate geometry/mass calculation, with explicit SI validation and provenance-aware input objects | Uncounted geometry-family map gap; issue pending (5). `[src/digitalmodel/subsea/cross_sections/calculations.py:L27-L61]` |
| `subsea/on_bottom_stability/simplified.py`: `submerged_weight` | Separate annular component and displacement calculation; includes contents mass and returns force per length | Cross-package route; retain separate assumptions, issue pending (5). `[src/digitalmodel/subsea/on_bottom_stability/simplified.py:L87-L130]` |
| `scripts/orcaflex/prepare_slay_campaign.py`: `submerged_weight` | Separate annular mass and water-displacement calculation returning kN/m | Script implementation, issue pending (5). `[scripts/orcaflex/prepare_slay_campaign.py:L54-L66]` |
| `subsea/catenary_riser/effective_weight.py`: `EffectiveWeightCalculator.calculate` | Delegates component formulas to the riser data model and sums force components; not a wrapper of this package | Separate route, issue pending (5). `[src/digitalmodel/subsea/catenary_riser/effective_weight.py:L30-L65]` |
| `PipeCrossSection.from_config`, `from_dict`, `from_inches`; visualizer convenience exports | Constructors, converters and wrappers around the listed calculation path, not second mass algorithms | Listed in sections 2–3; no counted miss. `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L87-L125]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L548-L564]` |

This is an inspected comparison of these routes, not an assertion that every thematic match elsewhere is an interchangeable implementation; the map follow-up owns wider classification. `[src/digitalmodel/subsea/cross_sections/calculations.py:L27-L61]` `[src/digitalmodel/subsea/on_bottom_stability/simplified.py:L87-L130]`

## 10. Coverage and open items

### Files and tests read

| Coverage | Files / evidence |
| --- | --- |
| Entire package, all Python files | `__init__.py`, `models.py`, `calculator.py`, `visualization.py`, `cli.py`; cited throughout sections 1–9. `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L59-L104]` `[src/digitalmodel/structural/pipe_cross_section/models.py:L46-L230]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L71-L330]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L25-L564]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L41-L275]` |
| Package documentation | `docs/worked-layered-pipe-buoyancy.md`; synthetic integral example and limits. `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L19-L215]` |
| Matching repository documentation | `docs/examples/circular-section-weight-buoyancy.md`; `docs/domains/structural/pipe_cross_section/README.md` (placeholder API documentation). `[docs/examples/circular-section-weight-buoyancy.md:L1-L6]` `[docs/examples/circular-section-weight-buoyancy.md:L124-L138]` `[docs/domains/structural/pipe_cross_section/README.md:L1-L17]` |
| Other direct search hits reviewed as context | `docs/domains/risers/scr-slwr-coverage-2026-10.md` refers to a different data-model property named `pipe_cross_section`, not this package. `[docs/domains/risers/scr-slwr-coverage-2026-10.md:L165-L174]` |
| Broader thematic discoveries excluded from package scope | Flexible-pipe plan and field-development mapping discuss other cross-section routes; cathodic-protection inventory and worked calculation discuss pipe wall area for resistance, not this package's API. `[docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L20-L33]` `[docs/field-development/subsea-production-systems-mapping.md:L103-L108]` `[docs/domains/cathodic_protection/standards-inventory.md:L99-L108]` `[docs/domains/cathodic_protection/examples/calc-007-dnvgl-b401-f103-design-basis-all.md:L143-L146]` |
| Test support | Package test initializer and `conftest.py`; no package-local test folder exists in the pinned package tree. `[tests/structural/pipe_cross_section/__init__.py:L1-L2]` `[tests/structural/pipe_cross_section/conftest.py:L1-L111]` |
| Executed module suites | `test_pipe_calculator.py` and `test_documented_layered_pipe_buoyancy.py`; all 38 node IDs below passed at BASE and after. `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L21-L327]` `[tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py:L21-L161]` |

The full module test-node inventory, produced from the BASE pytest summary:

```text
tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_layered_pipe_buoyancy[bare]
tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_layered_pipe_buoyancy[coated]
tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_pipe_buoyancy_serialization[bare]
tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py::test_documented_pipe_buoyancy_serialization[coated]
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeLayer::test_layer_creation
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeLayer::test_layer_thickness
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeLayer::test_layer_cross_sectional_area
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeLayer::test_layer_weight_per_meter
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_pipe_creation
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_steel_dimensions
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_lpp_dimensions
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_concrete_dimensions
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_final_outer_diameter
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_steel_weight
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_lpp_weight
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_concrete_weight
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_total_weight_in_air
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_displaced_volume
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_buoyancy_force
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_submerged_weight
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCrossSection::test_pipe_sinks
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCreationMethods::test_from_inches
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCreationMethods::test_from_dict
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestPipeCreationMethods::test_from_config
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestUnitConversions::test_inch_to_mm
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestUnitConversions::test_mm_to_inch
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestDataExport::test_to_dict
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestDataExport::test_to_csv_data
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestDataExport::test_get_summary
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestDataExport::test_buoyancy_result
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestEdgeCases::test_bare_steel_pipe
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestEdgeCases::test_lpp_only_pipe
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_layer_geometry_and_mass
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_dry_mass_and_displacement
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_submerged_mass
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_force_units[dry]
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_force_units[buoyancy]
tests/structural/pipe_cross_section/test_pipe_calculator.py::TestCircularSectionWorkedExample::test_force_units[submerged]
```

### Open items

The maintainer is the assignee for every row until a named contributor is assigned; draft references are issue drafts to be opened before final clearance.

| Item | Evidence / limit | Reference |
| --- | --- | --- |
| Standards traceability | The three UNVERIFIED claims of section 3: API 5L, DNV-ST-F101 and ISO 21809; missing clauses, editions/part and output citation sidecars | #1817; `[src/digitalmodel/structural/pipe_cross_section/__init__.py:L19-L22]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L432-L435]` |
| Input and parser contract | Missing/invalid geometry, nonpositive coating handling, silent density defaults, unit aliases and mixed CLI precedence; unsupported-input tests missing | issue pending (1); `[src/digitalmodel/structural/pipe_cross_section/models.py:L169-L183]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L127-L170]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L172-L223]` |
| Contents semantics / round trip | Nonzero internal-fluid density contributes no mass and is omitted from pipe configuration export; sealed-bore-only limit retained | issue pending (2); `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L193-L221]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L261-L278]` |
| Export and presentation failure coverage | PNG return/exception handling, neutral-balance status inconsistency, JSON coercion and label/inline-conversion checks | issue pending (3); `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L235-L261]` `[src/digitalmodel/structural/pipe_cross_section/visualization.py:L445-L454]` `[src/digitalmodel/structural/pipe_cross_section/cli.py:L245-L266]` |
| Remaining arithmetic/export coverage | Standalone layer force conversion, CSV precision and edge/neutral numeric checks are not established by the selected suites | issue pending (4); `[src/digitalmodel/structural/pipe_cross_section/models.py:L101-L117]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L280-L300]` `[tests/structural/pipe_cross_section/test_pipe_calculator.py:L206-L258]` |
| Method-family map and shared knowledge index | Empty map inventory, six rejected candidates and separate cross-package implementations require classification; index/shared-log registration remains separate | issue pending (5); `[docs/registry/implementation-map.yaml:L52787-L52828]` `[src/digitalmodel/subsea/cross_sections/calculations.py:L27-L61]` |
| Full module report and broader section-property interface | Worked arithmetic examples do not supply the requested general module report/builder or inertia/capacity interface | #3310 and #1847; `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L8-L14]` `[src/digitalmodel/structural/pipe_cross_section/calculator.py:L53-L59]` `[docs/examples/circular-section-weight-buoyancy.md:L134-L138]` |

No client-derived data or private reference fixture is used by the selected synthetic examples; no private sidecar entry is needed for these public assumptions. `[src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md:L19-L43]` `[docs/examples/circular-section-weight-buoyancy.md:L16-L30]`
