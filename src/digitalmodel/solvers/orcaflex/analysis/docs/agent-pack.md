# OrcaFlex comparative analysis knowledge pack

| Header | Value |
|---|---|
| Module | solvers/orcaflex/analysis |
| Repository | digitalmodel |
| Package | src/digitalmodel/solvers/orcaflex/analysis |
| Pack revision | 0 |
| Code revision | 309d343e08a2f83aebc6ea5344e586407b80c1f8 |
| Analysis family | OrcaFlex riser and mooring; exported-data comparison |
| Quality role | OrcaFlex riser and mooring quality manager |
| Status | DRAFT; no native solver qualification |

Source locators resolve at the code revision above; the implementation-map revision in section 9 is a separate data pin.

## 1. Purpose and scope

`MooringComparativeAnalysis` discovers exported mooring CSVs, reduces pretension and fender data, copies stiffness summaries, and builds line-group statistics. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L53-L83]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]`
`create_visualizations` writes three comparison figures, and `generate_report` delegates to `ReportGenerator`. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L267-L294]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L365-L368]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L434-L437]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L516-L534]`
The command entry point calls these reductions and output methods; it contains no model load or solver launch. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L223-L286]`
The enclosing solver domain is recorded as beta; no separate maturity field for this child package exists in that entry. `[docs/registry/module-routing.yaml:L461-L515]`
Use this pack for the five Python source files and three configuration templates in this package; sibling solver, model-generation, mooring-analysis and results-analysis packages have separate scope. `[src/digitalmodel/solvers/orcaflex/analysis/__init__.py:L10-L20]` `[src/digitalmodel/solvers/orcaflex/analysis/__main__.py:L1-L9]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L35-L99]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L25-L51]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L14-L27]`

UNVERIFIED: suitability for a particular geometry, depth, operating load or OrcaFlex version has not been established; the comparative functions accept tables without those applicability checks; issue pending (4). `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]`

## 2. Inputs and outputs

Input ranges below describe what the code accepts, rather than approved engineering ranges. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]`

| Input | Unit / convention | Allowed range and default | Source / evidence |
|---|---|---|---|
| CSV directory | filesystem path | Constructor accepts a path; CLI checks existence, not that it is a directory; scans immediate `*.csv` only | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L36-L71]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L200-L207]` |
| Pretension CSV | numeric values; report labels tension and force kN, deviation % | `target_tension` required; optional `current_tension`, `tension_diff_percent`, `end_Gx_force`, `end_Gy_force`, `end_Gz_force` become missing statistics when absent; no bounds enforced | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L100-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L150-L179]` |
| Stiffness summary CSV | report labels kN/m and seconds | First row only; `K_xx_total`, `K_xx_net`, `K_yy_total`, `K_zz_total`, `K_xy_total`, `K_xz_total`, `K_yz_total`, `T_surge`, `T_sway`, `T_heave`; missing columns become `None` | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L85-L94]` |
| Detailed stiffness CSV | plot labels stiffness kN/m, force kN | `ObjectName` required and parsed as `Line` plus integer; optional `k_axial`, `k_x`, `k_y`, `k_z`, `Fx`, `Fy`, `Fz`; no numeric range guard | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L181-L213]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L456-L475]` |
| Fender CSV | report labels force kN, compression m | Optional `current_fender_force`, `current_compression`; empty tables are omitted and read errors printed | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L230-L252]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L221-L241]` |
| Configuration identifier | string | First `filename_stem` when available, otherwise file stem; plots use substring `pb`, otherwise label `SB` | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L100-L102]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L145]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L181-L183]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L233-L234]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L304-L305]` |
| CLI configuration | YAML mapping of paths and `pattern` | `file_management` and direct path keys supported; relative paths joined to configuration directory; output report and visualization paths similarly resolved | `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L35-L99]` |
| CLI controls | paths, glob, Boolean flags | Input required; output defaults to input parent; default report `report/analysis_report.md`, figures `comparative_analysis`; `--pattern` default `*.csv` is parsed but not passed to the loader | `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L118-L175]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L229]` |
| Template controls | dimensionless / paths | Nested analysis, report and visualization settings exist in templates but the CLI reads paths and top-level `pattern` only | `[src/digitalmodel/solvers/orcaflex/analysis/configs/basic_analysis.yml:L12-L47]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L12-L57]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L198]` |

UNVERIFIED: the CSV coordinate frame, origin, positive force direction and phase convention are not specified by the consumer; axis column names do not establish these conventions; issue pending (4). `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L200-L208]`
UNVERIFIED: input units and numeric locale are not validated or converted; kN, kN/m, m and s are display assumptions, not verified file metadata; issue pending (4). `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L101-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L141-L160]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L182-L208]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L232-L243]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L87-L94]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L232-L241]`

| Output | Unit | Range / default | Evidence |
|---|---|---|---|
| `analysis_results['pretension']` | count, kN, % by display convention | One row per file with means, extrema, strict convergence count and force sums; absent files return `None` | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L94-L123]` |
| `analysis_results['stiffness']` | kN/m, s by display convention | One row per nonempty summary, copied without solving; header-only summaries give an empty cached DataFrame | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L134-L164]` |
| `analysis_results['line_stiffness']` | kN/m, kN by display convention | One row per input line with group and resultant; header-only detail can give an empty cached DataFrame | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L175-L213]` |
| `analysis_results['fender']` | kN, m by display convention | One row per successfully read nonempty file; no replacement of a prior cached result when no new rows survive | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L224-L252]` |
| PNG files | pixels / labels | `stiffness_comparison.png`, `pretension_comparison.png`, `line_group_analysis.png`; absent result categories omit their figures | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L296-L301]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L365-L375]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L434-L444]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L516-L519]` |
| Markdown and returned string | text | Default input-parent `report/analysis_report.md`; contains timestamp and input directory; report list is retained on the generator instance | `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L17-L68]` |
| CLI return code | integer code | 0 after completion, 1 for missing path or caught analysis error; configuration parse occurs outside the catch | `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L207]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L223-L286]` |

## 3. Method and governing standards

| Method / implementation | Computation or criterion | Basis and limit |
|---|---|---|
| `load_files` | Four filename substrings select disjoint categories; repeated calls append to existing file lists | Implementation convention, not schema validation. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L43-L83]` |
| `analyze_pretension` | Column means, max/min, sums; `converged_lines = count(abs(tension_diff_percent) < 10)` | Implementation criterion; equality at the boundary does not count as converged. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` |
| `analyze_stiffness` | Copies the first summary row, including periods and coupling terms | No stiffness matrix or natural-period solution occurs here. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` |
| `analyze_line_stiffness_distribution` | Line number groups: at most 4, at most 8, at most 12, otherwise final group; force magnitude is square root of `Fx² + Fy² + Fz²` | Number-based arrangement assumption; not inferred from geometry. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L185-L208]` |
| `analyze_fender_forces` | Sum, maximum, mean force and maximum compression | Per-file exceptions do not invalidate other files. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L230-L252]` |
| `_create_stiffness_plots` / `_add_stiffness_findings` | Directional ratios and percentage differences; report comparison assumes exactly one `pb` and one `sb` when there are two rows | Denominators and labels are not guarded. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L348-L363]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L102-L129]` |
| `_create_line_group_plots` / `_add_line_group_analysis` | Group means by configuration and line group | Report rounds group results to one decimal place; plotting iterates each configuration's groups separately. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L447-L506]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L200-L219]` |
| Report conclusions | Maximum natural period selected as optimal; coupling ratio compared with 0.2; convergence above 90%; resultant force below 50 kN; period recommendations outside 20–100 s; relative tension range above 0.5 | These are hardcoded heuristics, not demonstrated design acceptance limits. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L252-L334]` |

UNVERIFIED: no governing standard, edition, clause or scientific basis is attached to the hardcoded report heuristics; they must not be used as independent hand-check references; issue pending (3). `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L252-L334]`
The upstream mooring exporter computes the stiffness sums and estimates natural periods using a fixed assumed mass; the comparative module only reads those periods, so they are not an independent reference for that exporter. `[src/digitalmodel/solvers/orcaflex/mooring.py:L796-L828]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]`
No standards-derived value is introduced by this pack; any later standards-based acceptance criterion needs a source and citation sidecar before use.

## 4. Validated worked examples

Validated examples: **0**. Regression examples for this comparative API: **0**. A bounded source search for `MooringComparativeAnalysis` and its import path found no test consumer; the discovered analysis-folder test exercises the engine instead. `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L16-L16]` `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L26-L69]`

The discovered native workflow is not a comparative worked example: it expects a generated SIM file and returns Booleans rather than asserting them; its result is NOT_RUN at BASE and after because native OrcaFlex statics and SIM output require a licensed solver. No solver or licensed interface was imported. `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L33-L69]` `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L73-L124]`

The separate availability helper explicitly imports the licensed interface and instantiates a model; it is also NOT_RUN, and is not a pytest test node. `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L131-L149]`
Synthetic hand checks in section 5 audit the CSV methods; they are not existing test nodes or validated engineering examples.
Missing solver-free test coverage and an independently sourced example: issue pending (4).

## 5. QA gates

Gate applicability follows **AQG r0**, sections 4–10, family “OrcaFlex riser and mooring”; G4 is conditional on newly derived diffraction data. (AQG r0, sections 4–10.)
The criteria below are proposed executable hand-audit instructions for this module and its upstream evidence; they are not claims that the package enforces the gates.
Synthetic inputs must be anonymous, finite, and written outside the repository; no licensed interface is needed for checks Q0a–Q0e, Q2b or Q5a–Q5c.

| Gate | Applicability | Module-specific checks and numeric criterion | Evidence / execution status |
|---|---|---|---|
| G0 inputs | Applicable | Q0a–Q0e | CSV audit and configuration trace; no production-input qualification. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L53-L83]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L35-L99]` |
| G1 load | Applicable to every upstream OrcaFlex model | Q1 | NOT_RUN: native load needs target program and licence; CSV loading cannot establish G1. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L229-L253]` |
| G2 statics | Applicable to upstream equilibrium; Q2b audits reduction only | Q2a, Q2b | Native checks NOT_RUN; copied CSV statistics do not prove equilibrium. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` |
| G3 short dynamics | Applicable to upstream dynamic runs | Q3 | NOT_RUN: no native model, licence or simulation evidence. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L229-L270]` |
| G4 diffraction | NOT_APPLICABLE to this exported-data audit; no new diffraction run is performed | If imported data derive from a new diffraction run, require 1 passing record per body, 0 missing body records, before reporting | Retain upstream G4 record; CSV summary has no diffraction provenance field. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L147-L160]` (AQG r0, sections 8, 10.) |
| G5 QA appendix | Applicable to every report | Q5a–Q5c | Native evidence absent; current report assembly does not generate an AQG appendix. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L46-L62]` |

| Check | Executable audit, numeric pass criterion and unit | Evidence to retain / implementation locator |
|---|---|---|
| Q0a | Compare input manifest with `load_files` lists after 1 call: 0 missing files, 0 duplicates, 0 surplus files (files); compare requested pattern against actual membership | File lists and manifest; repeated calls currently append. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L43-L83]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L197-L229]` |
| Q0b | For every `analyze_*` table, require 0 missing mandatory/consumed columns, 0 nonfinite numeric cells, 0 silently omitted input records (columns, cells, records); audit locale before `read_csv`; a malformed fender file must invalidate qualification | Column, finite-value and row-cardinality table; current missing-column fallback and exception behavior require a separate audit. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L100-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L181-L213]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L230-L252]` |
| Q0c | Hand trace `load_config`: each relative path receives exactly 1 configuration-directory prefix; 0 path mismatches (paths); the requested components and pattern must have 0 ignored settings (settings) | Raw/resolved paths and settings consumed by CLI. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L47-L99]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L229]` |
| Q0d | Confirm 1 declared unit/frame record per CSV, 0 unit or frame mismatches (records); all `LineN` identifiers parse and agree with declared layout, 0 misassigned groups (lines) | Unit and layout manifest; consumer currently applies its own numbering rule. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L185-L208]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L87-L94]` |
| Q0e | Compare before/after result keys and report figures: 0 stale categories and 0 omitted required figures (categories, figures); all warnings retained, 0 discarded diagnostic messages (messages) | Source audit, category manifest and captured diagnostics. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L16-L22]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L94-L96]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L224-L252]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L296-L301]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L370-L373]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L439-L442]` |
| Q1 | Load the upstream model in the exact target OrcaFlex version via native `LoadData` on the licensed route, or matching-version demo only where qualified; require 0 errors, 0 warnings and 0 object-count difference (messages, objects) | Route/version, input fingerprint, messages, object manifest; execute upstream, never through `analyze_stiffness`. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` (AQG r0, section 5.) |
| Q2a | Compare `analyze_pretension` outputs with upstream static evidence: 0 unconverged models (models), every line checked (0 unchecked lines); compare top tension, angle and touchdown to an independent reference using the configuration's approved tolerances, with 0 out-of-tolerance quantities (quantities) | Static images, convergence and per-component table; the module's `<10` percentage count is not statics convergence. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` (AQG r0, section 6.) |
| Q2b | Anonymous two-row pretension input: targets 10 and 10 kN, currents 9 and 11 kN, deviations -10 and 10%; require mean 10.0 kN, range 2.0 kN and convergence count 0 lines; force components 3, 4, 0 kN give magnitude 5.0 kN; tolerance 0.0 in these exact arithmetic quantities | Hand arithmetic and actual table values; do not present this as physical validation. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L200-L208]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L175-L179]` |
| Q3 | Trace each reported dynamic result to upstream short-dynamics evidence: stop time at least 1 s, 0 warnings/errors, 0 nonfinite results (s, messages, values); 0 missing end images (images) | Native record and image digest; no such record is consumed by the CLI. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L229-L270]` (AQG r0, section 7.) |
| Q5a | Audit `ReportGenerator.generate`: 0 missing required QA appendix fields, 0 untraced reported results and exactly 1 approved gate record per represented model (fields, results, records) | Report versus upstream record manifest; current section builder lacks this appendix. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L46-L62]` (AQG r0, section 9.) |
| Q5b | Require 0 false positive engineering verdicts on missing/nonfinite data, 0 zero-denominator operations and 0 mislabelled configurations (verdicts, operations, rows); report thresholds need 1 approved basis each (basis records) | Read report helpers and exercise missing values, zero stiffness and arbitrary labels before accepting any conclusion. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L102-L129]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L252-L334]` |
| Q5c | Generate twice from the same `ReportGenerator` and compare section counts: 0 duplicated sections (sections); compare report claims with PNG manifest: 0 missing referenced images and 0 promised but absent output types (images, types) | Report/figure manifest and retained report-list state. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L25-L27]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L46-L68]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L284-L294]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L365-L368]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L434-L437]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L516-L519]` |

Defined checks: G0 **5**, G1 **1**, G2 **2**, G3 **1**, G4 **0** unconditional checks (conditional record requirement above), G5 **3**; total **12**.
No production gate is signed or passed by this pack.

## 6. Known failure modes and pitfalls

sweep: 38 found

Each numbered row is one reviewed risk site; related alternatives within one expression are grouped explicitly. This inventory records behavior, including deliberate fallbacks, rather than asserting that every site is an exception or defect.
The whole-package sweep used the following commands, followed by reading all five Python files and all three configuration templates:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\bint\(|\bfloat\(' src/digitalmodel/solvers/orcaflex/analysis --glob '*.py'
rg -n 'read_csv|filterwarnings|None|empty|sum\(|mean\(|threshold|tolerance|percent|kN|write|save|pattern' src/digitalmodel/solvers/orcaflex/analysis --glob '*.py' --glob '*.yml'
```

| Site | Symptom / cause | Detection audit from section 5; follow-up |
|---|---|---|
| S01 | Global warning suppression hides all warning categories. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L16-L16]` | Q0e; issue pending (1) |
| S02 | Bare style exception substitutes another style without retaining the diagnostic. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L19-L22]` | Q0e; issue pending (1) |
| S03 | Absent pretension files return `None`; an earlier cached result is not cleared. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L94-L96]` | Q0e; issue pending (1) |
| S04 | Optional current tension, deviation and axis-force columns produce missing statistics; required target column is accessed without schema validation. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` | Q0b; issue pending (1) |
| S05 | Hardcoded strict percentage threshold excludes equality; user template tolerance is not consumed. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L115-L115]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L17-L20]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L198]` | Q2b, Q0c; issue pending (2) |
| S06 | Absent stiffness files return `None` without clearing prior results. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L134-L136]` | Q0e; issue pending (1) |
| S07 | Empty stiffness CSV tables are skipped, additional summary rows discarded. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L145]` | Q0b; issue pending (1) |
| S08 | Ten stiffness/period columns each fall back to `None` when absent. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L147-L160]` | Q0b; issue pending (1) |
| S09 | Zero retained stiffness rows are cached as an empty DataFrame. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L163-L164]` | Q0e; issue pending (1) |
| S10 | Absent detailed stiffness files return `None` without clearing prior results. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L175-L177]` | Q0e; issue pending (1) |
| S11 | `int` on external `ObjectName` requires an integer after removing `Line`; no locale/layout validation. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L185-L198]` | Q0d; issue pending (4) |
| S12 | Four stiffness values and resultant force fall back to `None` on missing columns. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L200-L208]` | Q0b; issue pending (1) |
| S13 | Header-only detailed tables can cache an empty DataFrame. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L181-L213]` | Q0e; issue pending (1) |
| S14 | Absent fender files return `None` without clearing prior results. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L224-L226]` | Q0e; issue pending (1) |
| S15 | Empty fender tables are omitted. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L230-L234]` | Q0b; issue pending (1) |
| S16 | Four fender metrics fall back to `None` for absent force/compression columns. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L236-L244]` | Q0b; issue pending (1) |
| S17 | Any fender read/processing exception is printed and the loop advances; partial results can be returned. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L246-L251]` | Q0b; issue pending (1) |
| S18 | No retained fender rows return `None` without clearing the cached category. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L249-L252]` | Q0e; issue pending (1) |
| S19 | Four `read_csv` calls use default parsing, without an explicit decimal/thousands convention or units record. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L101-L101]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L141-L141]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L182-L182]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L232-L232]` | Q0b, Q0d; issue pending (4) |
| S20 | Stiffness plot silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L298-L299]` | Q0e; issue pending (1) |
| S21 | Pretension plot silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L372-L373]` | Q0e; issue pending (1) |
| S22 | Line-group plot silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L441-L442]` | Q0e; issue pending (1) |
| S23 | Every non-`pb` label becomes `SB`; two-row report requires both substrings and chooses first matching row. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L305-L305]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L377-L377]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L460-L460]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L472-L472]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L488-L488]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L504-L504]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L90-L91]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L106-L108]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L137-L138]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L155-L156]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L175-L176]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L184-L185]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L209-L210]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L235-L236]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L259-L265]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L285-L286]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L322-L323]` | Q5b; issue pending (3) |
| S24 | Plot ratios and report ratios/differences divide by unchecked stiffness denominators. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L348-L352]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L110-L112]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L124-L129]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L268-L275]` | Q5b; issue pending (3) |
| S25 | Stiffness report silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L79-L80]` | Q0e; issue pending (1) |
| S26 | Pretension report silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L144-L145]` | Q0e; issue pending (1) |
| S27 | Both convergence-report helpers replace falsy counts with 0; missing and truly zero counts are not distinguished. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L157-L160]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L285-L292]` | Q5b; issue pending (3) |
| S28 | Force-balance report omits its table if axis columns are missing; column membership does not establish finite cells. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L171-L179]` | Q0b, Q5b; issue pending (1) |
| S29 | Line-group report silently returns on absent/empty category. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L194-L195]` | Q0e; issue pending (1) |
| S30 | Fender report has separate absent/`None` and empty-table returns. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L223-L228]` | Q0e; issue pending (1) |
| S31 | Fender formatting tests only `is not None`, allowing nonfinite values to appear as numeric text. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L235-L241]` | Q5b; issue pending (3) |
| S32 | Maximum period is called optimal and coupling ratio has an uncited cutoff; missing categories omit this evaluation. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L256-L275]` | Q5b; issue pending (3) |
| S33 | Pretension conclusion returns silently when absent, uses a fixed convergence target and fixed force-balance threshold. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L279-L300]` | Q5b; issue pending (3) |
| S34 | Period/tension recommendations have fixed thresholds; missing or nonfinite evidence can leave the recommendation list empty and produce an acceptable-ranges sentence. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L306-L334]` | Q5b; issue pending (3) |
| S35 | Configuration loader can join a relative path twice; empty/nonmapping YAML fails before CLI analysis error handling. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L44-L82]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L198]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L223-L223]` | Q0c; issue pending (2) |
| S36 | Parsed `pattern` is never passed to `load_files`; template analysis/report/visualization controls and zero fender threshold are unused. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L229]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L63-L71]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/basic_analysis.yml:L12-L47]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L15-L57]` | Q0a, Q0c; issue pending (2) |
| S37 | Missing all input categories can still return CLI success; caught analysis exceptions produce only text and exit 1, without a structured failure record. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L229-L286]` | Q0b, Q0e; issue pending (1) |
| S38 | Generation appends to retained report state, embeds the input directory and writes even when evidence categories are absent; no QA appendix is assembled. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L25-L68]` | Q5a, Q5c; issue pending (3); upstream evidence admission tracked by #2520 |

Collection initializers (`[]`, `{}`) in the constructor are deliberate state setup, not error returns; no `continue`, `float()` conversion, year sentinel or disk cache write occurs in the package's Python files; the empty-result writes are the in-memory DataFrames in S09 and S13. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L43-L48]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L163-L164]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L212-L213]`
Plot bar widths, opacity, DPI and figure sizes are presentation constants; the engineering default/threshold risks are S05 and S32–S36. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L301-L313]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L367-L367]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L375-L385]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L444-L461]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/basic_analysis.yml:L38-L47]`
The row-count lead is not an incident list; #2425 concerns provenance/identifier cleanup of discovered model fixtures, while #2520 concerns complete upstream gate evidence; neither establishes this module's numerical accuracy. `[tests/solvers/orcaflex/analysis/moorings/PROVENANCE.md:L1-L21]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L46-L62]`
For fixture identity/provenance, no detection check exists in this package; #2425 remains the follow-up. `[tests/solvers/orcaflex/analysis/moorings/PROVENANCE.md:L13-L21]`

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
|---|---|---|---|---|---|
| 2026-11-07 | Initial pack | CSV comparisons cannot independently validate the exporter that supplied their periods | `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L140-L164]` `[src/digitalmodel/solvers/orcaflex/mooring.py:L809-L828]` | Separated reductions from native evidence and physical validation | issue pending (4) |

Future rows are append-only; private-derived lessons require a sidecar identifier and must not carry source values here.

## 8. Ownership

Proposed assignments for clearance: OrcaFlex riser and mooring quality manager; deputy, a second riser and mooring reviewer; producing discipline, mooring analysis; escalation to the family reviewer for ambiguous criteria, missing inputs or method classifications, then to the maintainer for cross-family scope.
This DRAFT assigns no completed review or approval.

## 9. Dependencies and other implementations

### Dependency inventory

| Dependency / downstream consumer | Evidence and boundary |
|---|---|
| pandas, NumPy, matplotlib | Imported for table reductions, resultants and plots; read as source before offline audit. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L8-L14]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L7-L11]` |
| PyYAML, argparse, pathlib | Configuration/argument/path handling. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L27-L32]` |
| Sibling mooring CSV exporter | Writes detailed stiffness and summary CSVs matching discovery suffixes; no solver invocation is required to read the source. `[src/digitalmodel/solvers/orcaflex/mooring.py:L851-L873]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L63-L71]` |
| Markdown / PNG consumers | Report written as text and plots saved as image files. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L56-L62]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L365-L368]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L434-L437]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L516-L519]` |

Every literal `OrcaFlex` occurrence in this package at the code pin is indexed below; these are textual dependency mentions, not API invocations.

| File and lines containing the literal | Meaning |
|---|---|
| Package initializer, lines 2, 4 | Exported-data analysis description. `[src/digitalmodel/solvers/orcaflex/analysis/__init__.py:L1-L8]` |
| CLI, lines 2, 50, 112 | Tool description and compatible configuration comment. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L1-L5]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L50-L52]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L111-L115]` |
| Mooring configuration, lines 1, 2, 4, 48 | Template compatibility and report-author string. `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L1-L5]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L45-L48]` |
| README, lines 1, 3, 75, 127, 129, 132 | Description, template and upstream integration instructions; none is solver qualification. `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L1-L3]` `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L70-L76]` `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L127-L138]` |

No literal dependency mention was found in the other Python files or two remaining templates; no target solver version is enforced by the comparative CLI. `[src/digitalmodel/solvers/orcaflex/analysis/__main__.py:L1-L9]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L8-L22]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L118-L175]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L7-L11]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/basic_analysis.yml:L1-L47]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/quick_analysis.yml:L1-L14]`

### Implementation map at its own revision

Map ref: `origin/codex/implmap-work`; map commit **3256bab95f84da3d9fadce8d5dcf8458d66d1eb4**; map data revision **270c8759c374b67a7f032953e7fcdb14e0224461**. The map was read from that ref and parsed independently of the code pin; its `revision` field is the data revision. `[docs/registry/implementation-map.yaml:L1-L4]`
In-package implementation inventory: **empty**. A path-prefix scan of every family's implementation rows found **0** rows within `src/digitalmodel/solvers/orcaflex/analysis/`; consequently there are **0** counted families and the family-signal grep is **NOT_APPLICABLE**. This is a map-inventory result, not a claim of unique implementation.
Map follow-up, issue pending (4): register CSV pretension aggregation, stiffness-summary comparison, numbered line grouping/force resultants, fender reductions and heuristic report assessment from section 3 before treating those method families as counted families. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L102-L334]`

### Whole-repository discovery, including scripts

```sh
rg -n 'pretension_analysis|mooring_stiffness_summary|fender_force_analysis|converged_lines|K_xx_total' . --glob '*.py'
rg -n 'def .*stiffness|class ComparativeAnalysisService|def .*comparison' src scripts --glob '*.py'
```

| Other implementation or relation | Classification |
|---|---|
| `mooring.py::calculate_mooring_stiffness_3d` | Upstream independent calculation of sums and periods; compare units and assumed mass before reuse; map follow-up, issue pending (4). `[src/digitalmodel/solvers/orcaflex/mooring.py:L796-L828]` |
| `StiffnessAnalyzer.compute_stiffness_matrix` | Sibling independently assembles translational/coupling entries; not a delegate of this package; map follow-up, issue pending (4). `[src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L94-L130]` |
| `ComparativeAnalysisService._calculate_descriptive_statistics` | Sibling descriptive statistics with separate result semantics; candidate for comparison, not a substitute physical reference; map follow-up, issue pending (4). `[src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L548-L570]` |
| Plot and report helpers inside this package | Serialize or summarize section-3 outputs; their own criteria are already listed in section 3 and risks in section 6. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L267-L534]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L102-L334]` |

## 10. Coverage and open items

Source coverage: all five package Python files, the README and all three YAML templates were read; no package-local tests were found by file discovery. `[src/digitalmodel/solvers/orcaflex/analysis/__init__.py:L1-L20]` `[src/digitalmodel/solvers/orcaflex/analysis/__main__.py:L1-L9]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L1-L290]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L1-L534]` `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L1-L347]` `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L1-L193]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/basic_analysis.yml:L1-L47]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/mooring_analysis.yml:L1-L57]` `[src/digitalmodel/solvers/orcaflex/analysis/configs/quick_analysis.yml:L1-L14]`
The exact module-path/name search under `docs/` found **0** Markdown documents naming this comparative package; references to the sibling engine analysis or reporting package were kept separate.
The README points to two specification documents, but the referenced specification tree is absent at the code pin; do not treat those links as opened sources; issue pending (5). `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L140-L144]`
Test inventory: `tests/solvers/orcaflex/analysis/test_dat_analysis.py::test_dat_file_analysis` is the one discovered pytest node, NOT_RUN for the licensed-solver reason in section 4. Test discovery read the one analysis-folder test and three support scripts; the two runner scripts delegate to a native runner and were NOT_RUN, while configuration verification is a standalone helper rather than a test node. `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L26-L149]` `[tests/solvers/orcaflex/analysis/scripts/run_analysis.py:L12-L27]` `[tests/solvers/orcaflex/analysis/scripts/run_orcaflex_analysis.py:L12-L27]` `[tests/solvers/orcaflex/analysis/scripts/verify_configs.py:L13-L112]`
The calculation citation contract was readable; no licensed publication was opened, and no licensed-standard tabulated value is reproduced.

| Open item | Responsible role | Issue / draft | Evidence or required closure |
|---|---|---|---|
| Missing categories, partial reads, stale cached tables and suppressed diagnostics (S01–S04, S06–S10, S12–S22, S25–S26, S28–S30, S37) | Module maintainer | issue pending (1) | Add fail-closed schema/cardinality/finite-value and stale-state tests; Q0b, Q0e. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L94-L252]` |
| Configuration path double join, ignored controls/pattern, mismatch of template tolerance (S05, S35–S36) | Module maintainer | issue pending (2) | Test relative paths, effective settings and file membership; Q0a, Q0c. `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L47-L99]` `[src/digitalmodel/solvers/orcaflex/analysis/cli.py:L179-L229]` |
| Mislabelled comparisons, unchecked ratios, fixed verdicts, repeated report state and absent appendix (S23–S24, S27, S31–S34, S38) | Module maintainer | issue pending (3) | Finite/zero/missing-label controls and evidence-bound report assertions; Q5a–Q5c. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L102-L334]` |
| UNVERIFIED applicability to geometry, depth, loads and solver versions | Family reviewer | issue pending (4) | Define applicability and demonstrate an independent case. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]` |
| UNVERIFIED coordinate frame, origin, force sign and phase | Family reviewer | issue pending (4) | Add explicit input conventions rather than infer them from axes. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L104-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L200-L208]` |
| UNVERIFIED units and decimal/thousands convention | Family reviewer | issue pending (4) | Input metadata and locale controls; Q0b, Q0d. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L101-L119]` `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L232-L243]` |
| UNVERIFIED engineering basis of report heuristics | Family reviewer | issue pending (3) | Cite a basis or withdraw acceptance language. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L252-L334]` |
| No independently validated example or comparative API regression tests; test-count lead not reproduced as tests | Module maintainer | issue pending (4) | Existing discovered node is an engine workflow, not a comparative-API test. `[tests/solvers/orcaflex/analysis/test_dat_analysis.py:L26-L69]` |
| Method-family map gaps and sibling calculation overlap | Family reviewer | issue pending (4) | Section-9 follow-up; empty counted-family inventory must not hide these methods. `[src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L252]` |
| Specification links and routing inventory contract | Documentation maintainer | issue pending (5) | Restore/retarget referenced documents and align enumerated submodule routes; the routing test currently asserts exact equality with its fixed inventory. `[src/digitalmodel/solvers/orcaflex/analysis/README.md:L140-L144]` `[tests/docs/test_digitalmodel_routing_contract.py:L215-L254]` |
| Native load/statics/dynamics, upstream fingerprints/signatures and conditional diffraction evidence | Family reviewer | #2520 | Native execution remains NOT_RUN; report output alone is insufficient. `[src/digitalmodel/solvers/orcaflex/analysis/report_generator.py:L46-L62]` |
| Model-fixture identity/provenance and anonymous replacement data | Module maintainer | #2425 | Discovery files with identifying names are excluded from examples and path listings; private-derived sidecar entry: to be created if later needed. `[tests/solvers/orcaflex/analysis/moorings/PROVENANCE.md:L13-L21]` |

Clearance requires issue numbers to replace draft markers and an independent review; this document records no completed clearance.
