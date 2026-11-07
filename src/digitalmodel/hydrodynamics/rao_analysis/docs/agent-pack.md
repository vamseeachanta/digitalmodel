# RAO analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/rao_analysis |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/hydrodynamics/rao_analysis` |
| Pack revision | 1 |
| Code revision | `aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3` |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Analysis family | diffraction; routing code DF |
| Quality role | diffraction quality manager |
| Status | DRAFT |

Repository citations resolve at the code revision; citations containing `@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` resolve at the separate map revision.

## 1. Purpose and scope

The main path loads `VesselTypes` from YAML, selects a vessel and draughts, assembles displacement RAO tables and writes separate amplitude and phase HTML plots. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L39] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L68-L83] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L194-L195] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L306-L307]

The package also includes rectangular seastate filtering, YAML exports and standalone legacy table/plot scripts; the visual-grid assessment is incomplete and must not be treated as an operational solver route. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L542] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L59-L77] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L40-L58]

The routing registry marks the hydrodynamics domain `production`, rather than giving this submodule a separate maturity assessment. [docs/registry/module-routing.yaml:L178-L184]

The active reader requests YAML `VesselTypes`, rather than parsing an AQWA listing; the latter remains requested functionality in #36. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L35-L39]

UNVERIFIED: supported solver versions, coordinate-frame origin, heading sign and phase lead/lag conventions are not established for this reader; follow-up in section 10, #36.

## 2. Inputs and outputs

### Inputs

| Name | Unit | Allowed range / default | Source and handling |
| --- | --- | --- | --- |
| `cfg`, `operation` | text / mapping | router defaults to `displacement_rao`; absent cfg becomes an empty mapping | `router` dispatches three named operations and otherwise reports completion. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L581-L604] |
| `Files`: `Name`, `Label`, `vessel_name`, `draughts` | paths / identifiers | files must exist to enter processing; empty draught list selects all | Caller mapping, YAML vessel lookup; missing names raise `ValueError`. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L18-L28] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L367-L401] |
| `VesselTypes → Draughts → DisplacementRAOs → RAOs` | source-defined | first direction entry supplies two ordered keys; row widths must fit the derived columns | YAML through `ymlInput`, then pandas; no range validator is called. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L35-L39] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L365] |
| `RAODirections`, `xaxis_range` | source heading / abscissa units | empty direction list selects unique headings; nonempty axis range uses inclusive lower/upper bounds | Displacement plotting config; no period/frequency conversion. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L93-L145] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L205-L257] |
| `filter.cog.x/y`, `l`, `w`, `beyond_boundary` | same length unit as source grid | no numeric default; strict interior inequalities | Caller supplies geometry; only first vessel and draught are used. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L491] |
| `filter.grid_label`, `Shapes` | identifier / source geometry units | exact shape-name lookup | YAML shape origin and size are rewritten in x/y while z is retained. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L569] |
| `Analysis.result_folder`, `file_name_for_overwrite` | path text | concatenated directly; no path default | Both plotting routines construct output names from these keys. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L147-L152] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L259-L264] |

### Outputs

| Name | Unit / convention | Range / default | Destination |
| --- | --- | --- | --- |
| `get_DisplacementRAOs_for_draught` DataFrame | preserves source values | one row per input data row, heading prepended | In memory; headers split on commas and stripped. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L348-L365] |
| Amplitude HTML | labels translations m/m; rotations deg/m; x labelled period s | six rows: heave, surge, sway, yaw, roll, pitch | `_rao_amp.html`; no conversion establishes those labels for frequency input. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L99-L128] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L194-L195] |
| Phase HTML | labels degrees; x labelled period s | six phase columns; title still says amplitude | `_rao_phase.html`. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L209-L240] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L259-L307] |
| Filtered seastate rows / YAML | source units retained | open rectangular boundary; shallow copy of nested vessel data | Return tuple, filtered data and model YAML exports. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L486-L500] |
| Shape YAML | source length units retained | expanded length/width; existing height retained | `get_shape_data` saves the selected shape mapping. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L553-L579] |
| `add_trace_to_fig` | caller-supplied x/y units | no return payload | Adds one Plotly scatter trace to the caller's subplot. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L309-L333] |

UNVERIFIED: input amplitude normalisation, phase convention and length-unit consistency are not enforced by the empty `check_*` methods; #36. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L403-L416]

### Public symbols

| Public symbol | Definition source | Availability |
| --- | --- | --- |
| `Fatigue_plotRAODirection` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L4-L4] | Legacy plotting helper; not executed |
| `SixDOFPlot` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L48-L48] | Legacy plotting helper; not executed |
| `update_deep` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L25-L25] | Import blocked on Python >=3.11: removed `collections.Mapping` |
| `ymlInput` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L7-L7] | Import blocked on Python >=3.11: removed `collections.Mapping` |
| `SixDOFPlot` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L43-L43] | Legacy plotting helper; not executed |
| `plotRAODirection` | [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L4-L4] | Legacy plotting helper; not executed |
| `RAOAnalysis` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L12-L12] | Active module |
| `RAOAnalysis.add_trace_to_fig` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L309-L309] | Active module |
| `RAOAnalysis.assess_orcaflex_seastate_raos` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L418] | Active module |
| `RAOAnalysis.check_amplitudes` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L403-L403] | Active module |
| `RAOAnalysis.check_definitions` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L412-L412] | Active module |
| `RAOAnalysis.check_locations` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L409-L409] | Active module |
| `RAOAnalysis.check_phases` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L406-L406] | Active module |
| `RAOAnalysis.check_units` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L415-L415] | Active module |
| `RAOAnalysis.filter_orcaflex_seastate_raos` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L443] | Active module |
| `RAOAnalysis.filter_orcaflex_seastate_raos_visual_grid` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L502-L502] | Active module |
| `RAOAnalysis.get_DisplacementRAOs_for_draught` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L348-L348] | Active module |
| `RAOAnalysis.get_draught_indices` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L367-L367] | Active module |
| `RAOAnalysis.get_rao_data_for_vessel` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L335] | Active module |
| `RAOAnalysis.get_shape_data` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L544] | Active module |
| `RAOAnalysis.get_vessel_data` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L392-L392] | Active module |
| `RAOAnalysis.plot_displacement_RAOs` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L77-L77] | Active module |
| `RAOAnalysis.plot_displacement_RAOs_amplitudes_for_draught` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L85-L85] | Active module |
| `RAOAnalysis.plot_displacement_RAOs_phases_for_draught` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L197-L197] | Active module |
| `RAOAnalysis.plot_multiple_vessel_data` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L65-L65] | Active module |
| `RAOAnalysis.plot_single_vessel_data` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L68-L68] | Active module |
| `RAOAnalysis.read_orcaflex_displacement_raos` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L16] | Active module |
| `RAOAnalysis.read_vessel_data_from_file` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L35-L35] | Active module |
| `RAOAnalysis.router` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L581-L581] | Active module |
| `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L41] | Active module |

## 3. Method and governing standards

| Method | Implementing definition | Basis / limit |
| --- | --- | --- |
| Displacement reader and selection | `read_orcaflex_displacement_raos`, `read_vessel_data_from_file`, `get_rao_data_for_vessel`, `get_draught_indices`, `get_vessel_data` | Structural lookup and dispatch, with no hydrodynamic solver or physical transformation in these definitions. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L39] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L401] |
| Displacement table assembly | `get_DisplacementRAOs_for_draught` | Iterates direction rows and prepends the heading; relies on the first mapping's key order. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L348-L365] |
| Rectangular grid selection | `filter_orcaflex_seastate_raos` | x/y bounds are centre ± half-size ± margin; retains strictly interior rows; shallow nested copy changes the original data too. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L500] |
| Shape geometry / text writers | `get_shape_data`, `write_filtered_orcaflex_seastate_rao_file` | Expanded width/length and retained vertical coordinates; YAML serialisation by assetutilities. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579] |
| Visual-grid route | `assess_orcaflex_seastate_raos`, `filter_orcaflex_seastate_raos_visual_grid` | Calls the shape writer before referencing unbound table variables; not a hand-check reference. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L436] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L526-L542] |
| Plot admission / wrappers | `add_trace_to_fig`, `plot_single_vessel_data`, `plot_displacement_RAOs`, both draught plotters | Delegates table values to plotting rather than calculating displacement response. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L68-L83] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L165-L195] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L277-L333] |
| Operation dispatch | `router` | Wraps named operations; unknown values claim completion without work. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L581-L604] |
| Legacy independent reader 1 | module-level statements in `legacy/RAOAnalysis.py` | Independently unfolds all vessel/draught/direction/period rows into a DataFrame; retained as an additional implementation in this inventory. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L59-L77] |
| Legacy independent reader 2 | module-level statements in `legacy/FatigueRAOAnalysis.py` | Independently repeats the same table assembly; not a delegation to the active reader. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L40-L58] |
| Legacy plotting helpers | `plotRAODirection`, both `SixDOFPlot` definitions, `Fatigue_plotRAODirection` | Filters DataFrame fields, passes through amplitudes and writes plots. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L4-L63] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L4-L67] |
| Legacy YAML helper | `ymlInput`, `update_deep` | Safe-load and recursive merge helper; catches all update errors and retains defaults. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L7-L36] |

The active package's transformations above are lookup, table assembly, filtering, geometry adjustment and plotting; no governing engineering standard or standards-derived `Citation` sidecar is implemented in those paths. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L416] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L579]

The offline example derives the oscillator amplitude divided by static deflection as `k / abs(k - m*ω² + i*c*ω)`; it does not compute physical diffraction or shielding. [examples/hydrodynamics/rao_analysis/basic_usage.py:L19-L34] [examples/hydrodynamics/rao_analysis/basic_usage.py:L57-L61]

The independent published basis is Lamar University, *Differential Equations: Mechanical Vibrations*, Examples 5–6 (web edition opened 7 November 2026); Example 6 supplies the forced damped equation and its particular response, while the four-point normalised curve is derived in the repository. [Mechanical Vibrations](https://tutorial.math.lamar.edu/Classes/DE/Vibrations.aspx) [examples/hydrodynamics/rao_analysis/README.md:L8-L54]

## 4. Validated worked examples

| Kind / case | Independent basis, inputs and expected result | Tolerance / test node / offline result |
| --- | --- | --- |
| Validated: analytic trace preservation | Closed form `D(ω)=k/abs(k-mω²+i*c*ω)` with example inputs; at ω = 0, 5, 10, 15 rad/s, reference = (1, 1, 1, 1) m/m and dynamic = (1, 1/6, 1/(3√17), 1/(2√97)) m/m; independent real/imaginary stiffness arithmetic in fixture. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L26-L41] [examples/hydrodynamics/rao_analysis/basic_usage.py:L19-L34] | `pytest.approx` at all 8 ordinates: relative tolerance 1e-6, absolute tolerance 1e-12 m/m (pytest 8.4.2 defaults inspected in the test environment; not overridden by the test); PASS. `tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py::test_synthetic_rao_plot_preserves_hand_values`. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L63] |
| Regression: mock workflow | Engine replaced with a fixed return mapping; assertions check those literal directions, frequencies and amplitudes, not the package reader. [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L56] | Exact counts plus inequalities; PASS. `tests/hydrodynamics/rao_analysis/test_rao_analysis.py::test_rao_analysis`. [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L67-L76] |
| Regression: import side effects | Import writes 0 files and opens 0 browsers. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L66-L77] | Exact counts; PASS. `tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py::test_import_writes_nothing`. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L66-L77] |
| Regression: explicit export | Executable writes 1 standalone HTML file with curve and unit labels, with 0 remote script tags. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L80-L100] | Exact counts/text; PASS. `tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py::test_executable_entrypoint_writes_requested_html`. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L80-L100] |
| Regression: browser isolation | Executable writes requested file and makes 0 browser calls. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L103-L115] | Exact counts; PASS. `tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py::test_entrypoint_exports_without_opening_browser`. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L103-L115] |
| Regression: required destination | Missing output argument yields nonzero exit and 0 files/browser calls. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L118-L132] | Exact counts; PASS. `tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py::test_entrypoint_requires_explicit_output_without_opening_browser`. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L118-L132] |

Offline execution record at the code pin: 89 run, 89 passed, 0 failed, 0 skipped, 0 NOT_RUN, both before and after the documentation change; 69 are neighboring-model tests, 6 are the package-associated examples above, and 14 are routing-contract cases. The run set is `tests/unit/hydrodynamics/test_rao_analysis.py`, `tests/hydrodynamics/rao_analysis/`, `tests/docs/test_digitalmodel_routing_contract.py`; the neighboring tests import `models` and `wave_spectra`, rather than `RAOAnalysis`. [tests/unit/hydrodynamics/test_rao_analysis.py:L18-L29] [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L67-L76] [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L132] [tests/docs/test_digitalmodel_routing_contract.py:L71-L79]

Execution form: `PYTHONPATH=src uv run --no-sources python -m pytest <the three paths above> -q -rA -p no:cacheprovider --basetemp=<external scratch>`, with the environment outside the repository and unchanged repository addopts; results are recorded for the uncommitted tree at the code pin.

The analytic example validates transfer of hand-derived values into traces; it does not qualify physical displacement RAOs, seastate filtering or diffraction, and the mock case supplies no independent physics expectation. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L25-L66] [examples/hydrodynamics/rao_analysis/basic_usage.py:L3-L7] [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L33]

## 5. QA gates

Gate applicability follows AQG r0, sections 3, 8–10; the package writes filtered OrcaFlex vessel/shape YAML and plots, rather than OrcaWave data/mesh or AQWA decks. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L194-L195] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L306-L307] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579]

These are acceptance procedures, not a statement that the physical-data gates have passed; use the listed tests where available and retain missing-data statuses with #36.

| Gate | Applicability and module-specific route | Evidence status |
| --- | --- | --- |
| G0 | Applicable: vessel/shape YAML and plotted values; checks C0.1–C0.3 | Offline plot checks PASS; production input contract incomplete (#36). |
| G1 | Applicable: C1.1 offline trace/import route; C1.2 loads each filtered OrcaFlex model in its declared target version through the licensed program's model-data loader, recording all messages | Offline import PASS; target-program load NOT_RUN: licensed program/interface prohibited. No OrcaWave or AQWA model output to load; those program routes are NOT_APPLICABLE. |
| G2 | NOT_APPLICABLE: no statics calculation or statics model generated by these reader/plot/filter methods | AQG r0 section 10 diffraction rows. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L39] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L500] |
| G3 | NOT_APPLICABLE: no time-domain solve in the documented path | AQG r0 section 10 diffraction rows. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L68-L83] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L441] |
| G4 | Applicable to imported diffraction-derived vessel data: C4.1 physical provenance, C4.2 analytic plotting anchor | C4.2 PASS; C4.1 NOT_RUN: no independently qualified public mesh/hydrostatics dataset or target-program record selected (#36). |
| G5 | Applicable: C5.1 evidence completeness for every exported artifact | Procedure defined; report-level signature and target-program evidence are not supplied by the module (#36). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L194-L195] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L306-L307] |

| Check | Executable procedure / numeric criterion and unit | Evidence / gate |
| --- | --- | --- |
| C0.1 | Before `read_orcaflex_displacement_raos` or `assess_orcaflex_seastate_raos`, compare requested file paths with `isfile`: 0 missing paths; require 0 unexpected operations and 0 empty enabled data tables (counts). Before any YAML export, expand the actual output paths including the serializer's appended `.yml`: distinct paths for every input and export kind, 0 paths shared with inputs and 0 overwritten exports, including non-`.yml` inputs and multiple inputs in one directory | Hand inventory against caller config and export-path comparison; detects skipped inputs, permissive dispatch and output collisions; G0. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L18-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L420-L436] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L571-L577] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L588-L602] |
| C0.2 | Hand-compare `get_DisplacementRAOs_for_draught` to every source row: output row-count difference = 0 rows, changed heading/ordinate cells = 0 cells, empty requested heading traces = 0 traces; for each selected draught, selected source name equals requested name with 0 mismatches | Source/DataFrame comparison; selection can violate this criterion; G0. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L348-L390] |
| C0.3 | Read source abscissa/phase/normalisation metadata and HTML labels: 0 inconsistent unit/convention fields; before/after `filter_orcaflex_seastate_raos`, require 0 changed input cells and 0 retained rows outside strict bounds; shape x/y sizes equal requested expanded sizes with 0 source-length-unit discrepancy | Hand comparison; absent metadata remains NOT_RUN, never assumed; G0. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L99-L119] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L211-L240] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L486-L500] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L553-L569] |
| C1.1 | Run section-4 import test and synthetic trace test: 0 import-created files, 0 browser calls, 2 named traces and 4 x values per trace (counts) | Existing test assertions; PASS; G1. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L77] |
| C1.2 | For every YAML produced by `write_filtered_orcaflex_seastate_rao_file` and `get_shape_data`, load in the declared target OrcaFlex version: 0 errors, 0 load warnings, 0 object-count differences from the source manifest (counts); save version and message record | External licensed load, NOT_RUN here; no version is inferred; G1. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579] |
| C4.1 | For each imported vessel/draught, require 0 missing upstream G4 balance/mesh/centre records; where normalisation is confirmed, compare longest-period heave in the table with the unit limit: absolute difference ≤ 0.02 m/m; do not apply to frequency-labelled or differently normalised data | Hand comparison of source, table and upstream AQG r0 section-8 record; NOT_RUN for absent qualified reference data; G4. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L365] |
| C4.2 | Evaluate the section-4 hand fractions against both `build_figure` traces at 0, 5, 10, 15 rad/s; require 0 `pytest.approx` mismatches across 8 m/m ordinates (relative tolerance 1e-6, absolute tolerance 1e-12 m/m) and 0 incorrect axis titles (counts) | Existing `test_synthetic_rao_plot_preserves_hand_values`; PASS; plotting anchor only; G4. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L25-L66] |
| C5.1 | For each HTML/YAML export, record input digest, code revision, units, program version or explicit absence, output path and every gate status: 0 missing fields, 0 digest mismatches, 0 unexplained NOT_APPLICABLE rows, distinct export paths per input and export kind, and 0 overwritten exports (counts); compare planned paths before writing and read back every expected export | Hand evidence manifest and export-path review; retain outstanding production records under #36; G5. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L194-L195] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L306-L307] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L571-L577] |

## 6. Known failure modes and pitfalls

sweep: 45 found

The count denotes separate fallback, empty-output, unit-assumption, export-collision and fixed-count selection sites below, including intentional selection defaults; each site is reviewed rather than equated with a confirmed incident.

Legacy sites 22–32, 38–39 and 43–44 describe text-level behaviour only: the legacy entrypoints fail at import on the supported Python >=3.11 interpreter. The YAML helper imports the removed `collections.Mapping`; independently, both reader scripts import `logs.setLogging`, which is not shipped under `src/` or `tests/` and is unavailable in the checked environment. The legacy plotter sites below are also source-level findings, not executed legacy-entrypoint results; issue pending (4). [pyproject.toml:L10-L10] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L4] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L27-L29] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L8-L10]

The writer collisions in sites 40–41 and 45 are reachable through direct writer calls. The assessment wrapper reaches the shape export in site 42, then fails in the incomplete visual-grid route before calling the filtered-data/model writer; these sites do not imply successful assessment-wrapper execution. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L428-L436] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L502-L542]

Exact source sweep commands at the code pin (all Python files, including legacy):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|Period|Freq|default|cache|pass' src/digitalmodel/hydrodynamics/rao_analysis --glob '*.py'
rg -n 'isfile|return|=|\.get\(' src/digitalmodel/hydrodynamics/rao_analysis --glob '*.py'
rg -n 'Draught.*\[[012]\]' src/digitalmodel/hydrodynamics/rao_analysis --glob '*.py'
```

No external-text `int`/`float` conversions, `continue`, explicit `return None`, error-path empty literal return, numeric epoch fallback or cache write occurs in these Python files; loop indices and dataframe/container initialisation are not error defaults. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L1-L604] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L20-L77] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L1-L58] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L1-L63] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L1-L68] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L36]

| Site | Symptom / cause at pin | Detection / tracking |
| --- | --- | --- |
| 01 | Nonexistent displacement input silently omitted. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L19-L28] | C0.1; issue pending (1). |
| 02 | Nonexistent seastate input silently omitted. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L421-L436] | C0.1; issue pending (1). |
| 03 | Disabled displacement flag returns config without plots; intentional, but does not report a structured status. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L18-L33] | C5.1; issue pending (1). |
| 04 | Disabled seastate flag returns config without assessment; intentional. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L420-L441] | C5.1; issue pending (1). |
| 05 | `plot_multiple_vessel_data` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L65-L66] | no detection check exists; #36. |
| 06 | `check_amplitudes` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L403-L404] | no detection check exists; #36. |
| 07 | `check_phases` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L406-L407] | no detection check exists; #36. |
| 08 | `check_locations` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L409-L410] | no detection check exists; #36. |
| 09 | `check_definitions` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L412-L413] | no detection check exists; #36. |
| 10 | `check_units` is empty. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L415-L416] | C0.3 hand comparison only; #36. |
| 11 | Empty amplitude heading selection expands to all unique directions. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L93-L95] | C0.2; #36. |
| 12 | Empty phase heading selection expands to all unique directions. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L205-L207] | C0.2; #36. |
| 13 | Empty draught selection expands to all draughts. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L367-L374] | C0.2; #36. |
| 14 | Amplitude plot hardcodes seconds, m/m and deg/m without conversion. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L99-L119] | C0.3; #36. |
| 15 | Phase plot hardcodes seconds/degrees without conversion. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L211-L231] | C0.3; #36. |
| 16 | Filter reads only first vessel and first draught. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L467-L470] | C0.2; issue pending (3). |
| 17 | Strict x/y inequalities drop boundary points and may return no rows. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L487-L500] | C0.1/C0.3; issue pending (3). |
| 18 | Shallow copy mutates the caller's nested table. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L495-L498] | C0.3; issue pending (3). |
| 19 | Unknown operation reports successful completion without computation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L596-L602] | C0.1; issue pending (1). |
| 20 | `cfg=None` becomes an empty mapping, then required-key lookup can fail. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L585-L591] | C0.1; issue pending (1). |
| 21 | Absent operation defaults to displacement processing. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L588-L591] | C0.1; issue pending (1). |
| 22 | Legacy fatigue argument errors become no-update defaults. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L15-L21] | no detection check exists; issue pending (4). |
| 23 | Legacy fatigue filename errors fall back to default basename. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L25-L28] | no detection check exists; issue pending (4). |
| 24 | Legacy reader argument errors become no-update defaults. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L34-L40] | no detection check exists; issue pending (4). |
| 25 | Legacy reader filename errors fall back to default basename. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L44-L47] | no detection check exists; issue pending (4). |
| 26 | Legacy YAML helper catches every update-load/merge error and retains defaults. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L12-L23] | no detection check exists; issue pending (4). |
| 27 | Legacy standard plot labels source period/frequency and amplitudes as seconds and m/m or deg/m. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L15-L17] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L43-L62] | C0.3 manual unit audit; #36. |
| 28 | Legacy fatigue plot makes the same unit assumption. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L15-L17] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L48-L67] | C0.3 manual unit audit; #36. |
| 29 | Legacy standard upper plot truncates visible x range to 0–25 s. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L19-L19] | no detection check exists; #36. |
| 30 | Legacy standard lower plot truncates visible x range to 0–25 s. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L32-L35] | no detection check exists; #36. |
| 31 | Legacy fatigue upper plot truncates visible x range to 0–25 s. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L19-L19] | no detection check exists; #36. |
| 32 | Legacy fatigue lower plot truncates its average-period display to 0–25. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L29-L38] | no detection check exists; #36. |
| 33 | A direction entry with no rows contributes nothing to the output frame. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L355-L365] | C0.1/C0.2; issue pending (1). |
| 34 | Requested absent heading creates empty amplitude traces. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L165-L176] | C0.2; issue pending (1). |
| 35 | Requested absent heading creates empty phase traces. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L277-L288] | C0.2; issue pending (1). |
| 36 | Amplitude x-range can remove all rows without rejecting the plot. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L139-L145] | C0.1/C0.2; issue pending (1). |
| 37 | Phase x-range can remove all rows without rejecting the plot. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L251-L257] | C0.1/C0.2; issue pending (1). |
| 38 | Legacy standard `SixDOFPlot` selection can yield empty traces. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L43-L50] | no detection check exists; issue pending (4). |
| 39 | Legacy fatigue `SixDOFPlot` selection can yield empty traces. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L48-L55] | no detection check exists; issue pending (4). |
| 40 | For a non-`.yml` suffix such as `.yaml`, both filtered-data and filtered-model names remain unchanged; the serializer appends `.yml`, so the model silently overwrites the data export at the same suffixed path, rather than overwriting the input. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L48-L63] | C0.1/C5.1: distinct paths per input/export kind, 0 overwritten exports; issue pending (3). |
| 41 | Multiple `.yml` inputs in one directory share `filter_SeaStateRAOs_data.yml`; each later filtered-data export silently overwrites the previous input's rows. An input already named `filter_SeaStateRAOs_data.yml` is itself overwritten by that export. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L48-L56] | C0.1/C5.1: distinct paths per input, 0 paths shared with inputs and 0 overwritten exports; issue pending (3). |
| 42 | Multiple `.yml` inputs in one directory share `shape_data.yml`; each later shape export silently overwrites the previous input's shape. An input already named `shape_data.yml` is itself overwritten. With a non-`.yml` suffix left unchanged by replacement, the name plus appended `.yml` collides with the same input's exports in site 40. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L571-L577] | C0.1/C5.1: distinct paths per input/export kind, 0 paths shared with inputs and 0 overwritten exports; issue pending (3). |
| 43 | Legacy standard plotter selects exactly draught indices 0, 1 and 2: fewer than three raise `IndexError`; additional draughts are silently omitted. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L9-L11] | no detection check exists; text-level behaviour only; issue pending (4). |
| 44 | Legacy fatigue plotter makes the same fixed-three-draught selection, raising `IndexError` for fewer and silently omitting additional draughts. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L9-L11] | no detection check exists; text-level behaviour only; issue pending (4). |
| 45 | The model export for `<stem>.yml` overwrites an existing `<stem>_filtered.yml` in the same directory; if that path is another declared input, its data are changed before later processing can read them. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L60-L63] | C0.1/C5.1: compare derived model paths with every input, 0 paths shared with inputs and 0 overwritten exports; issue pending (3). |

| Additional defect / incident lead | Evidence and detection |
| --- | --- |
| Draught subset or reordering selects wrong index | `get_draught_indices` appends requested-list position instead of position in available list; C0.2; issue pending (2). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L376-L388] |
| Visual-grid path cannot form output table | Undefined `SeaStateRAOs`, `df_columns`, `file_vessel_data` and `SeaStateRAOs_key` follow a shape write; no detection check exists; issue pending (3). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L526-L542] |
| Router filter route passes config as vessel-data argument | `filter_orcaflex_seastate_raos(self.cfg)` conflicts with nested list access; no detection check exists; issue pending (3). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L592-L593] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L468-L470] |
| Export naming silently overwrites earlier exports or other inputs | Non-`.yml` suffixes make the filtered-data and filtered-model exports share the input name plus `.yml`; fixed `filter_SeaStateRAOs_data.yml` and `shape_data.yml` names also collide across inputs in one directory. The non-`.yml` case preserves its input; an input whose name equals a fixed export name or another input's derived `<stem>_filtered.yml` path can itself be overwritten. C0.1/C5.1, sites 40–42 and 45; issue pending (3). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L48-L63] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L571-L577] |
| Phase chart title is incorrect | Phase method sets title to amplitude; no detection check exists; #36. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L197-L209] |
| Mock workflow hides reader defects | The engine result is mocked before assertions; C1.1 only exercises trace admission; #2672 remains relevant to real reader/response coverage. [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L35] [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L63] |
| Legacy entrypoints fail at import on supported Python >=3.11 | Two independent blockers in the checked environment: removed `collections.Mapping` and unavailable `logs.setLogging`, which is not shipped under `src/` or `tests/`; sites 22–32, 38–39 and 43–44 describe text-level behaviour only. No legacy entrypoint was imported; no detection check exists; issue pending (4). [pyproject.toml:L10-L10] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L4] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L27-L29] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L8-L10] |

Issue leads #5 (QA criteria), #6 (seastate plotting) and #15 (grid trimming) are historical requirements, not proof of an incident or working implementation; the missing checks and broken grid route above provide the confirmed source evidence. No detection check exists for the complete physical QA requested in #5; #36 and #2672 retain that coverage gap. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L403-L416] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L526-L542] [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L35]

## 7. Lessons log

| Date | Entry | Lesson / evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial source audit | A passing mocked engine result is not a reader-value check; independent analytic trace preservation has a narrower boundary. [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L35] [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L63] | Separate regression, analytic anchor and missing production gates | Not assigned; #2672 tracks the real-value coverage gap. |

## 8. Ownership

Clearance responsibility: diffraction quality manager; deputy responsibility: alternate diffraction quality manager nominated by the maintainers; producing responsibilities: hydrodynamics implementation and engineering documentation.

Route unresolved data conventions, tolerances and duplicate-implementation classifications to the family quality manager; route cross-module changes to the maintainers. This section assigns review responsibilities and records no clearance or completed review.

## 9. Dependencies and implementation inventory

The package imports pandas, pathlib, Plotly and assetutilities YAML/colour/save utilities; `__init__` exposes `RAOAnalysis`. The engine's `rao_analysis` branch calls only `read_orcaflex_displacement_raos`; it does not dispatch seastate assessment when that flag is enabled. `router`, seastate filtering and `assess_orcaflex_seastate_raos` are reachable only through direct calls, including calls made inside those methods. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L1-L9] [src/digitalmodel/hydrodynamics/rao_analysis/__init__.py:L1-L13] [src/digitalmodel/engine.py:L274-L279] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L436] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L588-L595]

The legacy scripts use pandas, matplotlib and standalone `dataManager`/logging imports; these are text-read dependencies, not permission to import the scripts. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L20-L29] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L1-L10] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L1-L4] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L4]

### OrcaFlex dependency occurrences

Every case-insensitive `orcaflex` occurrence in the package's Python source is accounted for here; these are text/YAML dependencies, not executed program interfaces.

| Source occurrence(s) | Dependency meaning |
| --- | --- |
| Package docstring | Describes displacement-RAO support. [src/digitalmodel/hydrodynamics/rao_analysis/__init__.py:L5-L6] |
| Reader definition | YAML reader/plot entrypoint. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L16] |
| Writer definition | Filtered vessel YAML exporter. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L43] |
| Assessment definition | Seastate processing wrapper. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L418] |
| Commented filter call | Inactive predecessor, counted as text only. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L424-L427] |
| Grid call and writer call | Active assessment dependencies. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L428-L436] |
| Rectangular filter definition | Nested seastate-table filter. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L443] |
| Visual-grid definition | Incomplete grid-dependent route. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L502-L502] |
| Router displacement call | Delegation to the reader. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L590-L591] |
| Router filter selector and call | Delegation with incompatible payload. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L592-L593] |
| Router assessment selector and call | Delegation to assessment. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L594-L595] |

UNVERIFIED: exact supported solver versions and independent upstream dataset inventory rows are not established by these imports or YAML lookups; #36.

### Map scope and counted-family search

The module's package includes legacy Python files; no child package is excluded from this audit. The two mapped families each have 2 in-package records, so the map's original order breaks the tie: `orcaflex-text-model-writers`, then `rao-readers-and-orcaflex-converters`; both are counted. The map contains 440 writer records and 295 reader/converter records. Each record is listed individually below at the map pin, including records outside the package.

The inventory uses exact map-record locators to identify each implementation's path and symbol without reproducing identifying external discovery names. It preserves every record, including multiple implementations in the same file; it makes no claim that a mapped implementation is correct or belongs to this package.

Search procedure: read every tracked repository `.py` file at the code pin, including `scripts/`; apply each retained `search_signals[].pattern` with Python `re.search`, case-sensitive and with no unescaping or other modification. Exclude a signal when `re.search(pattern, 'hydrodynamics/rao_analysis', re.I)` matches the package-relative path. All three writer signals are retained. The reader/converter vocabulary signal is excluded because its `HydroD` alternative matches `hydrodynamics` case-insensitively; therefore the reader counts and classifications cover only the algorithm and constants/key signals, not the full vocabulary search. Assign hits to the narrowest enclosing AST function/class, with top-level statements assigned to one module definition per file.

The audit found writer-signal hits in 3,091 repository Python files (485 under `scripts/`) and reader/converter-signal hits in 432 (67 under `scripts/`); these are search hits, not verified implementation counts. The package has 12 writer-hit definitions and 14 reader/converter-hit definitions, classified below. The exact regex text is preserved at the map locators below.

#### `orcaflex-text-model-writers`

Exact search signals: [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4936-L4938]

| Implementation ID | Kind / relation | Exact map record (path and symbol) |
| --- | --- | --- |
| writer-001 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4940-L4949] |
| writer-002 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4950-L4958] |
| writer-003 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4959-L4968] |
| writer-004 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4969-L4978] |
| writer-005 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4979-L4988] |
| writer-006 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4989-L4998] |
| writer-007 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4999-L5008] |
| writer-008 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5009-L5020] |
| writer-009 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5021-L5030] |
| writer-010 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5031-L5040] |
| writer-011 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5041-L5050] |
| writer-012 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5051-L5060] |
| writer-013 | writer / example | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5061-L5070] |
| writer-014 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5071-L5080] |
| writer-015 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5081-L5090] |
| writer-016 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5091-L5100] |
| writer-017 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5101-L5110] |
| writer-018 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5111-L5120] |
| writer-019 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5121-L5132] |
| writer-020 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5133-L5142] |
| writer-021 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5143-L5151] |
| writer-022 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5152-L5161] |
| writer-023 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5162-L5171] |
| writer-024 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5172-L5181] |
| writer-025 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5182-L5191] |
| writer-026 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5192-L5201] |
| writer-027 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5202-L5211] |
| writer-028 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5212-L5223] |
| writer-029 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5224-L5233] |
| writer-030 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5234-L5243] |
| writer-031 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5244-L5253] |
| writer-032 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5254-L5263] |
| writer-033 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5264-L5273] |
| writer-034 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5274-L5283] |
| writer-035 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5284-L5293] |
| writer-036 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5294-L5303] |
| writer-037 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5304-L5313] |
| writer-038 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5314-L5323] |
| writer-039 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5324-L5333] |
| writer-040 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5334-L5343] |
| writer-041 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5344-L5353] |
| writer-042 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5354-L5363] |
| writer-043 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5364-L5372] |
| writer-044 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5373-L5382] |
| writer-045 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5383-L5392] |
| writer-046 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5393-L5402] |
| writer-047 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5403-L5415] |
| writer-048 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5416-L5425] |
| writer-049 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5426-L5437] |
| writer-050 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5438-L5447] |
| writer-051 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5448-L5457] |
| writer-052 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5458-L5467] |
| writer-053 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5468-L5477] |
| writer-054 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5478-L5487] |
| writer-055 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5488-L5497] |
| writer-056 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5498-L5507] |
| writer-057 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5508-L5519] |
| writer-058 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5520-L5531] |
| writer-059 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5532-L5540] |
| writer-060 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5541-L5550] |
| writer-061 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5551-L5560] |
| writer-062 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5561-L5570] |
| writer-063 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5571-L5580] |
| writer-064 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5581-L5593] |
| writer-065 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5594-L5603] |
| writer-066 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5604-L5615] |
| writer-067 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5616-L5628] |
| writer-068 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5629-L5638] |
| writer-069 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5639-L5648] |
| writer-070 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5649-L5658] |
| writer-071 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5659-L5667] |
| writer-072 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5668-L5677] |
| writer-073 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5678-L5687] |
| writer-074 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5688-L5697] |
| writer-075 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5698-L5707] |
| writer-076 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5708-L5717] |
| writer-077 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5718-L5727] |
| writer-078 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5728-L5737] |
| writer-079 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5738-L5749] |
| writer-080 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5750-L5759] |
| writer-081 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5760-L5769] |
| writer-082 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5770-L5781] |
| writer-083 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5782-L5791] |
| writer-084 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5792-L5801] |
| writer-085 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5802-L5811] |
| writer-086 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5812-L5823] |
| writer-087 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5824-L5833] |
| writer-088 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5834-L5843] |
| writer-089 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5844-L5853] |
| writer-090 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5854-L5863] |
| writer-091 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5864-L5872] |
| writer-092 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5873-L5882] |
| writer-093 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5883-L5891] |
| writer-094 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5892-L5901] |
| writer-095 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5902-L5911] |
| writer-096 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5912-L5921] |
| writer-097 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5922-L5933] |
| writer-098 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5934-L5943] |
| writer-099 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5944-L5953] |
| writer-100 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5954-L5965] |
| writer-101 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5966-L5975] |
| writer-102 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5976-L5985] |
| writer-103 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5986-L5994] |
| writer-104 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5995-L6004] |
| writer-105 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6005-L6014] |
| writer-106 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6015-L6024] |
| writer-107 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6025-L6034] |
| writer-108 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6035-L6044] |
| writer-109 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6045-L6054] |
| writer-110 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6055-L6064] |
| writer-111 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6065-L6074] |
| writer-112 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6075-L6084] |
| writer-113 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6085-L6096] |
| writer-114 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6097-L6109] |
| writer-115 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6110-L6119] |
| writer-116 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6120-L6129] |
| writer-117 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6130-L6141] |
| writer-118 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6142-L6153] |
| writer-119 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6154-L6163] |
| writer-120 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6164-L6173] |
| writer-121 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6174-L6183] |
| writer-122 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6184-L6193] |
| writer-123 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6194-L6203] |
| writer-124 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6204-L6213] |
| writer-125 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6214-L6223] |
| writer-126 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6224-L6233] |
| writer-127 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6234-L6243] |
| writer-128 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6244-L6253] |
| writer-129 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6254-L6263] |
| writer-130 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6264-L6273] |
| writer-131 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6274-L6283] |
| writer-132 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6284-L6293] |
| writer-133 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303] |
| writer-134 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313] |
| writer-135 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6314-L6323] |
| writer-136 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6324-L6333] |
| writer-137 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6334-L6343] |
| writer-138 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6344-L6353] |
| writer-139 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6354-L6363] |
| writer-140 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6364-L6373] |
| writer-141 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6374-L6383] |
| writer-142 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6384-L6393] |
| writer-143 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6394-L6403] |
| writer-144 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6404-L6413] |
| writer-145 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6414-L6423] |
| writer-146 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6424-L6436] |
| writer-147 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6437-L6448] |
| writer-148 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6449-L6458] |
| writer-149 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6459-L6468] |
| writer-150 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6469-L6478] |
| writer-151 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6479-L6490] |
| writer-152 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6491-L6500] |
| writer-153 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6501-L6510] |
| writer-154 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6511-L6520] |
| writer-155 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6521-L6530] |
| writer-156 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6531-L6540] |
| writer-157 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6541-L6550] |
| writer-158 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6551-L6560] |
| writer-159 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6561-L6570] |
| writer-160 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6571-L6580] |
| writer-161 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6581-L6590] |
| writer-162 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6591-L6600] |
| writer-163 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6601-L6610] |
| writer-164 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6611-L6620] |
| writer-165 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6621-L6630] |
| writer-166 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6631-L6640] |
| writer-167 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6641-L6650] |
| writer-168 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6651-L6659] |
| writer-169 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6660-L6672] |
| writer-170 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6673-L6682] |
| writer-171 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6683-L6692] |
| writer-172 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6693-L6704] |
| writer-173 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6705-L6714] |
| writer-174 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6715-L6724] |
| writer-175 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6725-L6734] |
| writer-176 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6735-L6744] |
| writer-177 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6745-L6754] |
| writer-178 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6755-L6764] |
| writer-179 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6765-L6774] |
| writer-180 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6775-L6784] |
| writer-181 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6785-L6794] |
| writer-182 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6795-L6804] |
| writer-183 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6805-L6814] |
| writer-184 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6815-L6824] |
| writer-185 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6825-L6834] |
| writer-186 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6835-L6844] |
| writer-187 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6845-L6854] |
| writer-188 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6855-L6864] |
| writer-189 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6865-L6874] |
| writer-190 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6875-L6884] |
| writer-191 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6885-L6894] |
| writer-192 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6895-L6904] |
| writer-193 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6905-L6914] |
| writer-194 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6915-L6927] |
| writer-195 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6928-L6937] |
| writer-196 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6938-L6947] |
| writer-197 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6948-L6957] |
| writer-198 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6958-L6967] |
| writer-199 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6968-L6977] |
| writer-200 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6978-L6987] |
| writer-201 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6988-L6997] |
| writer-202 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6998-L7007] |
| writer-203 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7008-L7017] |
| writer-204 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7018-L7031] |
| writer-205 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7032-L7041] |
| writer-206 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7042-L7051] |
| writer-207 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7052-L7061] |
| writer-208 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7062-L7071] |
| writer-209 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7072-L7081] |
| writer-210 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7082-L7091] |
| writer-211 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7092-L7101] |
| writer-212 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7102-L7111] |
| writer-213 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7112-L7121] |
| writer-214 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7122-L7131] |
| writer-215 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7132-L7141] |
| writer-216 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7142-L7153] |
| writer-217 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7154-L7163] |
| writer-218 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7164-L7176] |
| writer-219 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7177-L7186] |
| writer-220 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7187-L7196] |
| writer-221 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7197-L7207] |
| writer-222 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7208-L7217] |
| writer-223 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7218-L7227] |
| writer-224 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7228-L7237] |
| writer-225 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7238-L7247] |
| writer-226 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7248-L7257] |
| writer-227 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7258-L7269] |
| writer-228 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7270-L7279] |
| writer-229 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7280-L7289] |
| writer-230 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7290-L7299] |
| writer-231 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7300-L7309] |
| writer-232 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7310-L7319] |
| writer-233 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7320-L7329] |
| writer-234 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7330-L7339] |
| writer-235 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7340-L7349] |
| writer-236 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7350-L7359] |
| writer-237 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7360-L7369] |
| writer-238 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7370-L7379] |
| writer-239 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7380-L7389] |
| writer-240 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7390-L7399] |
| writer-241 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7400-L7409] |
| writer-242 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7410-L7419] |
| writer-243 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7420-L7429] |
| writer-244 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7430-L7439] |
| writer-245 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7440-L7449] |
| writer-246 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7450-L7459] |
| writer-247 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7460-L7469] |
| writer-248 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7470-L7479] |
| writer-249 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7480-L7489] |
| writer-250 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7490-L7499] |
| writer-251 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7500-L7509] |
| writer-252 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7510-L7519] |
| writer-253 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7520-L7529] |
| writer-254 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7530-L7539] |
| writer-255 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7540-L7551] |
| writer-256 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7552-L7561] |
| writer-257 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7562-L7571] |
| writer-258 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7572-L7581] |
| writer-259 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7582-L7591] |
| writer-260 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7592-L7603] |
| writer-261 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7604-L7613] |
| writer-262 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7614-L7626] |
| writer-263 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7627-L7636] |
| writer-264 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7637-L7646] |
| writer-265 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7647-L7656] |
| writer-266 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7657-L7666] |
| writer-267 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7667-L7676] |
| writer-268 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7677-L7686] |
| writer-269 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7687-L7696] |
| writer-270 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7697-L7706] |
| writer-271 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7707-L7716] |
| writer-272 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7717-L7729] |
| writer-273 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7730-L7739] |
| writer-274 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7740-L7749] |
| writer-275 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7750-L7762] |
| writer-276 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7763-L7774] |
| writer-277 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7775-L7784] |
| writer-278 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7785-L7794] |
| writer-279 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7795-L7804] |
| writer-280 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7805-L7816] |
| writer-281 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7817-L7826] |
| writer-282 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7827-L7836] |
| writer-283 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7837-L7846] |
| writer-284 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7847-L7856] |
| writer-285 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7857-L7866] |
| writer-286 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7867-L7878] |
| writer-287 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7879-L7888] |
| writer-288 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7889-L7898] |
| writer-289 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7899-L7908] |
| writer-290 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7909-L7921] |
| writer-291 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7922-L7931] |
| writer-292 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7932-L7941] |
| writer-293 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7942-L7951] |
| writer-294 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7952-L7961] |
| writer-295 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7962-L7971] |
| writer-296 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7972-L7981] |
| writer-297 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7982-L7991] |
| writer-298 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7992-L8001] |
| writer-299 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8002-L8011] |
| writer-300 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8012-L8021] |
| writer-301 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8022-L8031] |
| writer-302 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8032-L8043] |
| writer-303 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8044-L8053] |
| writer-304 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8054-L8063] |
| writer-305 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8064-L8073] |
| writer-306 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8074-L8083] |
| writer-307 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8084-L8093] |
| writer-308 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8094-L8103] |
| writer-309 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8104-L8113] |
| writer-310 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8114-L8123] |
| writer-311 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8124-L8133] |
| writer-312 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8134-L8143] |
| writer-313 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8144-L8155] |
| writer-314 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8156-L8165] |
| writer-315 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8166-L8175] |
| writer-316 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8176-L8185] |
| writer-317 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8186-L8195] |
| writer-318 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8205] |
| writer-319 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8206-L8215] |
| writer-320 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8216-L8227] |
| writer-321 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8228-L8237] |
| writer-322 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8238-L8247] |
| writer-323 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8248-L8257] |
| writer-324 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8258-L8267] |
| writer-325 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8268-L8279] |
| writer-326 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8280-L8290] |
| writer-327 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8291-L8300] |
| writer-328 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8301-L8310] |
| writer-329 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8311-L8323] |
| writer-330 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8324-L8333] |
| writer-331 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8334-L8343] |
| writer-332 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8344-L8353] |
| writer-333 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8354-L8363] |
| writer-334 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8364-L8373] |
| writer-335 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8374-L8383] |
| writer-336 | writer / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8384-L8393] |
| writer-337 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8394-L8403] |
| writer-338 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8404-L8413] |
| writer-339 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8414-L8423] |
| writer-340 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8424-L8435] |
| writer-341 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8436-L8445] |
| writer-342 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8446-L8457] |
| writer-343 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8458-L8470] |
| writer-344 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8471-L8480] |
| writer-345 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8481-L8492] |
| writer-346 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8493-L8505] |
| writer-347 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8506-L8517] |
| writer-348 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8518-L8527] |
| writer-349 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8528-L8537] |
| writer-350 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8538-L8547] |
| writer-351 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8548-L8560] |
| writer-352 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8561-L8572] |
| writer-353 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8573-L8582] |
| writer-354 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8583-L8596] |
| writer-355 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8597-L8608] |
| writer-356 | writer / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8609-L8619] |
| writer-357 | writer / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8620-L8632] |
| writer-358 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8633-L8647] |
| writer-359 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8648-L8657] |
| writer-360 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8658-L8667] |
| writer-361 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8668-L8677] |
| writer-362 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8678-L8687] |
| writer-363 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8688-L8701] |
| writer-364 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8702-L8714] |
| writer-365 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8715-L8724] |
| writer-366 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8725-L8739] |
| writer-367 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8740-L8749] |
| writer-368 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8750-L8759] |
| writer-369 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8760-L8773] |
| writer-370 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8774-L8783] |
| writer-371 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8784-L8793] |
| writer-372 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8794-L8803] |
| writer-373 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8804-L8813] |
| writer-374 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8814-L8823] |
| writer-375 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8824-L8833] |
| writer-376 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8834-L8843] |
| writer-377 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8844-L8856] |
| writer-378 | writer / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8857-L8866] |
| writer-379 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8867-L8876] |
| writer-380 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8877-L8886] |
| writer-381 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8887-L8896] |
| writer-382 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8897-L8908] |
| writer-383 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8909-L8918] |
| writer-384 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8919-L8928] |
| writer-385 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8929-L8938] |
| writer-386 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8939-L8948] |
| writer-387 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8949-L8958] |
| writer-388 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8959-L8970] |
| writer-389 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8971-L8980] |
| writer-390 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8981-L8990] |
| writer-391 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8991-L9000] |
| writer-392 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9001-L9010] |
| writer-393 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9011-L9020] |
| writer-394 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9021-L9030] |
| writer-395 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9031-L9040] |
| writer-396 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9041-L9050] |
| writer-397 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9051-L9060] |
| writer-398 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9061-L9070] |
| writer-399 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9071-L9080] |
| writer-400 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9081-L9090] |
| writer-401 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9091-L9100] |
| writer-402 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9101-L9110] |
| writer-403 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9111-L9120] |
| writer-404 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9121-L9130] |
| writer-405 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9131-L9140] |
| writer-406 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9141-L9152] |
| writer-407 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9153-L9162] |
| writer-408 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9163-L9172] |
| writer-409 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9173-L9182] |
| writer-410 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9183-L9192] |
| writer-411 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9193-L9202] |
| writer-412 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9203-L9212] |
| writer-413 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9213-L9222] |
| writer-414 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9223-L9232] |
| writer-415 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9233-L9242] |
| writer-416 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9243-L9252] |
| writer-417 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9253-L9262] |
| writer-418 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9263-L9272] |
| writer-419 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9273-L9282] |
| writer-420 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9283-L9292] |
| writer-421 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9293-L9302] |
| writer-422 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9303-L9312] |
| writer-423 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9313-L9322] |
| writer-424 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9323-L9332] |
| writer-425 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9333-L9342] |
| writer-426 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9343-L9352] |
| writer-427 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9353-L9362] |
| writer-428 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9363-L9372] |
| writer-429 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9373-L9382] |
| writer-430 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9383-L9392] |
| writer-431 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9393-L9402] |
| writer-432 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9403-L9412] |
| writer-433 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9413-L9422] |
| writer-434 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9423-L9432] |
| writer-435 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9433-L9442] |
| writer-436 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9443-L9452] |
| writer-437 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9453-L9462] |
| writer-438 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9463-L9472] |
| writer-439 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9473-L9482] |
| writer-440 | writer / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9483-L9493] |

In-package records:

- `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file`. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303]
- `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.get_shape_data`. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313]

#### `rao-readers-and-orcaflex-converters`

Exact search signals: [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9512-L9514]

| Implementation ID | Kind / relation | Exact map record (path and symbol) |
| --- | --- | --- |
| reader-001 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9516-L9527] |
| reader-002 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9528-L9539] |
| reader-003 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9540-L9551] |
| reader-004 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9552-L9563] |
| reader-005 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9564-L9578] |
| reader-006 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9579-L9589] |
| reader-007 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9590-L9601] |
| reader-008 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9602-L9613] |
| reader-009 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9614-L9625] |
| reader-010 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9626-L9637] |
| reader-011 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9638-L9652] |
| reader-012 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9653-L9664] |
| reader-013 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9665-L9678] |
| reader-014 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9679-L9690] |
| reader-015 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9691-L9704] |
| reader-016 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9705-L9715] |
| reader-017 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9716-L9727] |
| reader-018 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9728-L9739] |
| reader-019 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9740-L9753] |
| reader-020 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9754-L9765] |
| reader-021 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9766-L9777] |
| reader-022 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9778-L9788] |
| reader-023 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9789-L9800] |
| reader-024 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9801-L9812] |
| reader-025 | reader / example | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9813-L9826] |
| reader-026 | reader / example | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9827-L9838] |
| reader-027 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9839-L9849] |
| reader-028 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9850-L9861] |
| reader-029 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9862-L9873] |
| reader-030 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9874-L9885] |
| reader-031 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9886-L9897] |
| reader-032 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9898-L9909] |
| reader-033 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9910-L9921] |
| reader-034 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9922-L9932] |
| reader-035 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9933-L9943] |
| reader-036 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9944-L9954] |
| reader-037 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9955-L9965] |
| reader-038 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9966-L9977] |
| reader-039 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9978-L9989] |
| reader-040 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9990-L10004] |
| reader-041 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10005-L10017] |
| reader-042 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10018-L10032] |
| reader-043 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10033-L10046] |
| reader-044 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10047-L10060] |
| reader-045 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10061-L10072] |
| reader-046 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10073-L10087] |
| reader-047 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10088-L10099] |
| reader-048 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10100-L10111] |
| reader-049 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10112-L10122] |
| reader-050 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10123-L10134] |
| reader-051 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10135-L10146] |
| reader-052 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10147-L10158] |
| reader-053 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10159-L10169] |
| reader-054 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10170-L10181] |
| reader-055 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10182-L10193] |
| reader-056 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10194-L10207] |
| reader-057 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10208-L10222] |
| reader-058 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10223-L10235] |
| reader-059 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10236-L10246] |
| reader-060 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10247-L10260] |
| reader-061 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10261-L10272] |
| reader-062 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10273-L10284] |
| reader-063 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10285-L10296] |
| reader-064 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10297-L10307] |
| reader-065 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10308-L10319] |
| reader-066 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10320-L10333] |
| reader-067 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10334-L10345] |
| reader-068 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10346-L10356] |
| reader-069 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10357-L10368] |
| reader-070 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10369-L10380] |
| reader-071 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10381-L10391] |
| reader-072 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10392-L10403] |
| reader-073 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10404-L10415] |
| reader-074 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10416-L10427] |
| reader-075 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10428-L10439] |
| reader-076 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10440-L10451] |
| reader-077 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10452-L10465] |
| reader-078 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10466-L10477] |
| reader-079 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10478-L10489] |
| reader-080 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10490-L10501] |
| reader-081 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10502-L10513] |
| reader-082 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10514-L10527] |
| reader-083 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10528-L10539] |
| reader-084 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10540-L10551] |
| reader-085 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10552-L10563] |
| reader-086 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10564-L10575] |
| reader-087 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10576-L10587] |
| reader-088 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10588-L10599] |
| reader-089 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10600-L10611] |
| reader-090 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10612-L10623] |
| reader-091 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10624-L10635] |
| reader-092 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10636-L10647] |
| reader-093 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10648-L10659] |
| reader-094 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10660-L10671] |
| reader-095 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10672-L10683] |
| reader-096 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10684-L10699] |
| reader-097 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10700-L10711] |
| reader-098 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10712-L10725] |
| reader-099 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10726-L10737] |
| reader-100 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10738-L10749] |
| reader-101 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10750-L10761] |
| reader-102 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10762-L10775] |
| reader-103 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10776-L10789] |
| reader-104 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10790-L10801] |
| reader-105 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10802-L10815] |
| reader-106 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10816-L10828] |
| reader-107 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10829-L10841] |
| reader-108 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10842-L10853] |
| reader-109 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10854-L10865] |
| reader-110 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10866-L10877] |
| reader-111 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10878-L10891] |
| reader-112 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10892-L10905] |
| reader-113 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10906-L10917] |
| reader-114 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10918-L10933] |
| reader-115 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10934-L10948] |
| reader-116 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10949-L10962] |
| reader-117 | reader / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10963-L10979] |
| reader-118 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10980-L10991] |
| reader-119 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10992-L11003] |
| reader-120 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11004-L11015] |
| reader-121 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11016-L11027] |
| reader-122 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11028-L11039] |
| reader-123 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11040-L11051] |
| reader-124 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11052-L11063] |
| reader-125 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11064-L11075] |
| reader-126 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11076-L11089] |
| reader-127 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11090-L11101] |
| reader-128 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11102-L11113] |
| reader-129 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11114-L11125] |
| reader-130 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11126-L11137] |
| reader-131 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11138-L11149] |
| reader-132 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11150-L11163] |
| reader-133 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11164-L11175] |
| reader-134 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11176-L11187] |
| reader-135 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11188-L11202] |
| reader-136 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11203-L11213] |
| reader-137 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11214-L11225] |
| reader-138 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11226-L11237] |
| reader-139 | script / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11238-L11248] |
| reader-140 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11249-L11260] |
| reader-141 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11261-L11274] |
| reader-142 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11275-L11286] |
| reader-143 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11287-L11298] |
| reader-144 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11299-L11310] |
| reader-145 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11311-L11322] |
| reader-146 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11323-L11334] |
| reader-147 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11335-L11346] |
| reader-148 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11347-L11360] |
| reader-149 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11361-L11374] |
| reader-150 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11375-L11388] |
| reader-151 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11389-L11400] |
| reader-152 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11401-L11414] |
| reader-153 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11415-L11426] |
| reader-154 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11427-L11438] |
| reader-155 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11439-L11450] |
| reader-156 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11451-L11462] |
| reader-157 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11463-L11474] |
| reader-158 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11475-L11486] |
| reader-159 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11487-L11498] |
| reader-160 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11499-L11510] |
| reader-161 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11511-L11524] |
| reader-162 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11525-L11536] |
| reader-163 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11537-L11548] |
| reader-164 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11549-L11560] |
| reader-165 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11561-L11574] |
| reader-166 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11575-L11590] |
| reader-167 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11591-L11603] |
| reader-168 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11604-L11619] |
| reader-169 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11620-L11633] |
| reader-170 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11634-L11648] |
| reader-171 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11649-L11662] |
| reader-172 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11663-L11674] |
| reader-173 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11675-L11686] |
| reader-174 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11687-L11698] |
| reader-175 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11699-L11710] |
| reader-176 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11711-L11724] |
| reader-177 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11725-L11736] |
| reader-178 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11737-L11748] |
| reader-179 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11749-L11762] |
| reader-180 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11763-L11774] |
| reader-181 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11775-L11786] |
| reader-182 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11787-L11798] |
| reader-183 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11799-L11812] |
| reader-184 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11813-L11824] |
| reader-185 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11825-L11836] |
| reader-186 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11837-L11850] |
| reader-187 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11851-L11862] |
| reader-188 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11863-L11874] |
| reader-189 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11875-L11888] |
| reader-190 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11889-L11900] |
| reader-191 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11901-L11912] |
| reader-192 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11913-L11926] |
| reader-193 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11927-L11938] |
| reader-194 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11939-L11950] |
| reader-195 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11951-L11964] |
| reader-196 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11965-L11976] |
| reader-197 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11977-L11990] |
| reader-198 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11991-L12002] |
| reader-199 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12003-L12014] |
| reader-200 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12015-L12026] |
| reader-201 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12027-L12038] |
| reader-202 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12039-L12052] |
| reader-203 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12053-L12064] |
| reader-204 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12065-L12078] |
| reader-205 | reader / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12079-L12095] |
| reader-206 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12096-L12107] |
| reader-207 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12108-L12119] |
| reader-208 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12120-L12131] |
| reader-209 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12132-L12143] |
| reader-210 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12144-L12159] |
| reader-211 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12160-L12173] |
| reader-212 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12174-L12186] |
| reader-213 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12187-L12199] |
| reader-214 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12200-L12211] |
| reader-215 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12212-L12226] |
| reader-216 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12227-L12238] |
| reader-217 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12239-L12254] |
| reader-218 | reader / copy | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12255-L12266] |
| reader-219 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12267-L12278] |
| reader-220 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12279-L12290] |
| reader-221 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12291-L12302] |
| reader-222 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12303-L12314] |
| reader-223 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12315-L12326] |
| reader-224 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12327-L12338] |
| reader-225 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12339-L12350] |
| reader-226 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12351-L12362] |
| reader-227 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12363-L12376] |
| reader-228 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12377-L12388] |
| reader-229 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12389-L12400] |
| reader-230 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12401-L12412] |
| reader-231 | reader / caller | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12413-L12427] |
| reader-232 | reader / wrapper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12428-L12441] |
| reader-233 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12442-L12453] |
| reader-234 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12454-L12465] |
| reader-235 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12466-L12477] |
| reader-236 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12478-L12489] |
| reader-237 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12490-L12503] |
| reader-238 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12504-L12515] |
| reader-239 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12516-L12529] |
| reader-240 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12530-L12541] |
| reader-241 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12542-L12555] |
| reader-242 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12556-L12567] |
| reader-243 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12568-L12582] |
| reader-244 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12583-L12594] |
| reader-245 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12595-L12606] |
| reader-246 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12607-L12620] |
| reader-247 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12621-L12632] |
| reader-248 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12633-L12646] |
| reader-249 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12647-L12658] |
| reader-250 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12659-L12670] |
| reader-251 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12671-L12682] |
| reader-252 | reader / independent | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12683-L12696] |
| reader-253 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12697-L12710] |
| reader-254 | script / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12711-L12721] |
| reader-255 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12722-L12733] |
| reader-256 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12734-L12745] |
| reader-257 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12746-L12759] |
| reader-258 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12760-L12773] |
| reader-259 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12774-L12787] |
| reader-260 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12788-L12799] |
| reader-261 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12800-L12813] |
| reader-262 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12814-L12827] |
| reader-263 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12828-L12839] |
| reader-264 | script / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12840-L12850] |
| reader-265 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12851-L12862] |
| reader-266 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12863-L12874] |
| reader-267 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12875-L12886] |
| reader-268 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12887-L12898] |
| reader-269 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12899-L12910] |
| reader-270 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12911-L12922] |
| reader-271 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12923-L12934] |
| reader-272 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12935-L12946] |
| reader-273 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12947-L12958] |
| reader-274 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12959-L12973] |
| reader-275 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12974-L12986] |
| reader-276 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12987-L12998] |
| reader-277 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12999-L13010] |
| reader-278 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13011-L13022] |
| reader-279 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13023-L13034] |
| reader-280 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13035-L13048] |
| reader-281 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13049-L13062] |
| reader-282 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13063-L13074] |
| reader-283 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13075-L13086] |
| reader-284 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13087-L13101] |
| reader-285 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13102-L13113] |
| reader-286 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13114-L13125] |
| reader-287 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13126-L13139] |
| reader-288 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13140-L13153] |
| reader-289 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13154-L13167] |
| reader-290 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13168-L13181] |
| reader-291 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13182-L13193] |
| reader-292 | script / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13194-L13204] |
| reader-293 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13205-L13216] |
| reader-294 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13217-L13228] |
| reader-295 | reader / test_helper | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13229-L13240] |

In-package records:

- `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.read_orcaflex_displacement_raos`. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11775-L11786]
- `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.get_rao_data_for_vessel`. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11787-L11798]

### Package grep classifications

| Family / definition | Hit lines and classification |
| --- | --- |
| orcaflex-text-model-writers / `__init__.py` `<module>` | Lines 6; docstring/import text only. [src/digitalmodel/hydrodynamics/rao_analysis/__init__.py:L6-L6] |
| orcaflex-text-model-writers / `legacy/FatigueRAOAnalysis.py` `<module>` | Lines 8, 13; independent legacy table assembly, listed in section 3; map follow-up. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L8-L13] |
| orcaflex-text-model-writers / `legacy/RAOAnalysis.py` `<module>` | Lines 11, 27, 32; independent legacy table assembly, listed in section 3; map follow-up. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L11-L32] |
| orcaflex-text-model-writers / `legacy/dataManager/ymlInput.py` `ymlInput` | Lines 9, 15; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L9-L15] |
| orcaflex-text-model-writers / `rao_analysis.py` `<module>` | Lines 7; docstring/import text only. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L7-L7] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.read_orcaflex_displacement_raos` | Lines 16; mapped implementation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L16] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | Lines 41, 52, 61; mapped implementation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L61] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.assess_orcaflex_seastate_raos` | Lines 418, 426, 431, 432; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L418-L432] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.filter_orcaflex_seastate_raos` | Lines 443; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L443] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.filter_orcaflex_seastate_raos_visual_grid` | Lines 502; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L502-L502] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.get_shape_data` | Lines 575; mapped implementation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L575-L575] |
| orcaflex-text-model-writers / `rao_analysis.py` `RAOAnalysis.router` | Lines 591, 592, 593, 594, 595; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L591-L595] |
| rao-readers-and-orcaflex-converters / `legacy/FatigueRAOAnalysis.py` `<module>` | Lines 9, 46, 47, 48, 52, 54, 57, 58; independent legacy table assembly, listed in section 3; map follow-up. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L9-L58] |
| rao-readers-and-orcaflex-converters / `legacy/Fatigue_plotRAODirection.py` `Fatigue_plotRAODirection` | Lines 4, 9, 10, 11, 13, 43, 44; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L4-L44] |
| rao-readers-and-orcaflex-converters / `legacy/Fatigue_plotRAODirection.py` `SixDOFPlot` | Lines 48, 49; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L48-L49] |
| rao-readers-and-orcaflex-converters / `legacy/RAOAnalysis.py` `<module>` | Lines 29, 65, 66, 67, 71, 73, 76, 77; independent legacy table assembly, listed in section 3; map follow-up. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L29-L77] |
| rao-readers-and-orcaflex-converters / `legacy/plotRAODirection.py` `plotRAODirection` | Lines 4, 9, 10, 11, 13, 40, 41; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L4-L41] |
| rao-readers-and-orcaflex-converters / `legacy/plotRAODirection.py` `SixDOFPlot` | Lines 43, 44; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L43-L44] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.read_orcaflex_displacement_raos` | Lines 16; mapped implementation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L16] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.plot_single_vessel_data` | Lines 74; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L74-L74] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.plot_displacement_RAOs_amplitudes_for_draught` | Lines 93, 94, 95, 117, 119, 121, 141, 162, 165, 166, 167, 169, 170, 171, 175, 176; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L93-L176] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.plot_displacement_RAOs_phases_for_draught` | Lines 205, 206, 207, 229, 231, 233, 253, 274, 277, 278, 279, 281, 282, 283, 287, 288; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L205-L288] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.get_rao_data_for_vessel` | Lines 335, 340, 343; mapped implementation. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L343] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.get_DisplacementRAOs_for_draught` | Lines 348, 357, 358, 359, 362; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L348-L362] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.filter_orcaflex_seastate_raos` | Lines 467, 472, 473; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L467-L473] |
| rao-readers-and-orcaflex-converters / `rao_analysis.py` `RAOAnalysis.router` | Lines 588, 590, 591; helper / delegation / data presentation, listed in section 3. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L588-L591] |

Counted-family result: 0 further unlisted in-package implementations after the section-3 inventory; the two independent legacy table assemblers are explicitly listed there. Helper plotters and serializers do not independently calculate a diffraction response. The criterion of at most 1 additional unlisted implementation is met; map follow-up remains open for the legacy reader records. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L59-L77] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L40-L58] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L309-L365]

Map follow-up: reconcile the two legacy readers and the active table-assembly helper with the reader family, and classify the repository-wide/script candidates separately; exact signal hits alone do not establish a map gap. Plot-only presentation and rectangular seastate/shape geometry have no separate in-package map family and do not alter this counted-family result; issue pending (5). [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L500] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579]

## 10. Coverage and open items

Read scope: all 8 package Python files, including the empty legacy package initializer; active module, package initializer, two legacy reader scripts, two legacy plotters and the legacy YAML helper. No legacy entrypoint or licensed solver interface was imported. [src/digitalmodel/hydrodynamics/rao_analysis/__init__.py:L1-L13] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L1-L604] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L20-L77] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L1-L58] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/plotRAODirection.py:L1-L63] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/Fatigue_plotRAODirection.py:L1-L68] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L36]

Additional in-package artefact inventory at the code pin follows. These are unqualified legacy data and generated artefacts with unknown provenance, not valid engineering inputs; no YAML dataset was loaded and no plot was used as physical evidence. The glob locators were checked against every tracked package path at the pin: all 7 YAML datasets and all 12 PNG/SVG plots match these groups, with no additional nested datasets or plots. The locators avoid repeating discovery filenames requiring codenaming; issue pending (6).

| Package-relative locator | Count | Qualification gap |
| --- | --- | --- |
| `legacy/dataManager/*.yml` | 7 YAML datasets | Source rights, units, conventions and physical provenance are unverified. |
| `legacy/*.png` | 6 generated plots | Source data, generation revision and physical provenance are unverified. |
| `legacy/*.svg` | 6 generated plots | Source data, generation revision and physical provenance are unverified. |
| `legacy/RunAll` | 1 runner | Legacy execution instructions are inventoried only; runtime and data provenance are unverified. |

Read tests: both `tests/hydrodynamics/rao_analysis/test_*.py` files, their `conftest.py`, `tests/unit/hydrodynamics/test_rao_analysis.py`, and `tests/docs/test_digitalmodel_routing_contract.py`; section 4 distinguishes the directly exercised path from neighboring calculations. [tests/hydrodynamics/rao_analysis/conftest.py:L1-L39] [tests/unit/hydrodynamics/test_rao_analysis.py:L18-L29] [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L76] [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L17-L132]

The external example README and implementation were read; package-local README/docs at the code pin: none. [examples/hydrodynamics/rao_analysis/README.md:L1-L6] [examples/hydrodynamics/rao_analysis/basic_usage.py:L3-L7]

Documentation discovery used `rg -n 'rao_analysis|RAOAnalysis' docs --glob '*.md'`; these are discovery documents, not acceptance evidence for maturity or historical test totals:

- `docs/registry/implementation-map.md`. [docs/registry/implementation-map.md:L20-L20]
- `docs/migration/legacy-engineering-flow-catalog.md`. [docs/migration/legacy-engineering-flow-catalog.md:L32-L32]
- `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md`. [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L9-L9]
- `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md`. [docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md:L10-L10]
- `docs/domains/development/stability_check_results.md`. [docs/domains/development/stability_check_results.md:L64-L64]
- `docs/domains/development/test_summary_with_validations.md`. [docs/domains/development/test_summary_with_validations.md:L15-L15]
- `docs/domains/development/pre_development_testing_checklist.md`. [docs/domains/development/pre_development_testing_checklist.md:L39-L39]
- `docs/domains/development/test_validation_patterns.md`. [docs/domains/development/test_validation_patterns.md:L12-L12]
- `docs/domains/testing/engine_test_strategy.md`. [docs/domains/testing/engine_test_strategy.md:L89-L89]
- `docs/domains/testing/engine_test_implementation_plan.md`. [docs/domains/testing/engine_test_implementation_plan.md:L375-L375]
- `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md`. [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L89-L89]

One discovery document has an identifying name and is intentionally not reproduced; codenaming follow-up is tracked as issue pending (6).

| Open item / evidence | Responsible role | Tracking |
| --- | --- | --- |
| Section-1 uncertainty: solver versions, coordinate origin, heading and phase conventions; no supported-version declaration in the reader. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L39] | Hydrodynamics maintainer | #36 |
| Section-2 uncertainty: input normalisation and unit consistency, empty validators. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L403-L416] | Hydrodynamics maintainer | #36 |
| Section-9 uncertainty: upstream dataset inventory and exact solver versions; no selected independently qualified physical-data record. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L35-L39] | Diffraction quality manager | #36 |
| Missing physical reader/response worked example; analytic plot anchor covers trace admission only. [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L44-L63] | Hydrodynamics maintainer | #2672 |
| Missing-input and permissive-operation status handling; section-6 sites 01–04, 19–21, 33–37. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L18-L33] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L585-L604] | Hydrodynamics maintainer | issue pending (1) |
| Incorrect draught subset/reordering index. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L376-L388] | Hydrodynamics maintainer | issue pending (2) |
| Incomplete visual-grid and unsafe filter/export behavior; input mutation, first-record selection and suffix handling. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L542] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L571-L577] | Hydrodynamics maintainer | issue pending (3) |
| Legacy update-error swallowing and compatibility; none of the supplied tests exercises legacy entrypoints. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L1-L36] [tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py:L17-L23] | Hydrodynamics maintainer | issue pending (4) |
| Full map inventory and grep candidates need reconciliation beyond the package; legacy independent readers require map records. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/RAOAnalysis.py:L59-L77] [src/digitalmodel/hydrodynamics/rao_analysis/legacy/FatigueRAOAnalysis.py:L40-L58] | Diffraction quality manager | issue pending (5) |
| One identifying documentation discovery path and unqualified legacy data must be assessed for public codenaming; no data values from them reproduced; sidecar entry: to be created if needed | Hydrodynamics maintainer | issue pending (6) |
| Physical source G4 records, target-program loading, version and report evidence not verified; no licensed standard clause is claimed checked | Diffraction quality manager | #36 |

The calculation-citation contract was readable; this change introduces no standards-derived calculation constant, implementation change or private sidecar value. The existing source's geometry/filter transformations, rather than a standards formula, define the documented main path. [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L443-L500] [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L553-L569]
