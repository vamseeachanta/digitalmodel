# Consolidated Test Plan: Diffraction Benchmark Modules

## Overview

This test plan covers 11 benchmark source files in the `digitalmodel.hydrodynamics.diffraction` package.
Target: 80%+ line coverage using mock data structures (no OrcFxAPI dependency).

---

## Mock Data Factories Required (conftest.py)

All tests share a common set of mock data factories. These should live in a
shared conftest.py at `tests/hydrodynamics/diffraction/conftest.py`.

### 1. `make_frequency_data(n=10, fmin=0.2, fmax=2.0) -> FrequencyData`
Creates a FrequencyData with `n` linearly-spaced frequencies from fmin to fmax.

### 2. `make_heading_data(values=[0, 45, 90, 135, 180]) -> HeadingData`
Creates a HeadingData with specified heading angles.

### 3. `make_rao_component(dof, nfreq=10, nhead=5, peak_freq_idx=5, peak_val=1.0) -> RAOComponent`
Creates a RAOComponent with synthetic magnitude (Gaussian peak at peak_freq_idx)
and phase (linear ramp) arrays. Shape: [nfreq x nhead].

### 4. `make_rao_set(tool="OrcaWave", nfreq=10, nhead=5) -> RAOSet`
Creates a full RAOSet with all 6 DOFs populated via make_rao_component.

### 5. `make_diffraction_results(tool="OrcaWave", nfreq=10, nhead=5) -> DiffractionResults`
Creates a DiffractionResults with RAOSet, dummy AddedMassSet, DampingSet.

### 6. `make_solver_results(solver_names, nfreq=10, nhead=5) -> Dict[str, DiffractionResults]`
Creates a dict mapping solver name -> DiffractionResults. Second solver gets
slightly perturbed data to simulate realistic differences.

### 7. `make_deviation_statistics(correlation=0.999) -> DeviationStatistics`
Creates a DeviationStatistics with given correlation and small errors.

### 8. `make_pairwise_rao_comparison(dof, corr=0.999) -> PairwiseRAOComparison`
Creates a PairwiseRAOComparison for a single DOF.

### 9. `make_benchmark_report(solver_names, consensus="FULL") -> BenchmarkReport`
Creates a complete BenchmarkReport with pairwise results and consensus metrics.

---

## File-by-File Test Plan

---

### FILE 1: benchmark_helpers.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `_is_phase_at_negligible_amplitude` | `mag_at_phase_diff: float, peak_mag: float` | `bool` |
| `_parse_fdf_panels` | `fdf_path: Path` | `list[list[list[float]]]` |
| `generate_dof_observations` | `dof_name: str, consensus: str, mag_corr: float, phase_corr: float, max_mag_diff: float, max_phase_diff: float, unit: str, magnitude_at_max_phase_diff: float, peak_magnitude: float, phase_diff_at_visible_heading: bool` | `str` (HTML) |

**Constants:** `DOF_ORDER`, `_AMPLITUDE_UNITS`, `_SOLVER_STYLES`, `_NEGLIGIBLE_AMPLITUDE_RATIO`, `_FILE_DESCRIPTIONS`

**Mock data needed:** None (pure functions) except tmp_path for FDF file tests.

**Test cases:**

1. `test_is_phase_negligible_zero_peak` — peak_mag=0 → True
2. `test_is_phase_negligible_below_threshold` — mag/peak < 0.05 → True
3. `test_is_phase_negligible_above_threshold` — mag/peak >= 0.05 → False
4. `test_is_phase_negligible_exact_boundary` — mag/peak == 0.05 → False
5. `test_parse_fdf_panels_valid_file` — write a 4-line header + 3 data rows (8 values each) to tmp_path → returns 3 panels with 4 vertices each
6. `test_parse_fdf_panels_empty_file` — empty file → []
7. `test_parse_fdf_panels_missing_file` — nonexistent path → []
8. `test_parse_fdf_panels_bad_data` — file with non-numeric lines → skips bad, returns valid
9. `test_generate_dof_observations_full_consensus` — consensus="FULL" → "full agreement" in output
10. `test_generate_dof_observations_majority_consensus` — consensus="MAJORITY" → "majority agreement"
11. `test_generate_dof_observations_no_consensus` — consensus="NO_CONSENSUS" → "no consensus"
12. `test_generate_dof_observations_excellent_corr` — mag_corr=0.9999 → "virtually identical"
13. `test_generate_dof_observations_good_corr` — mag_corr=0.995 → "excellent"
14. `test_generate_dof_observations_moderate_corr` — mag_corr=0.96 → "good"
15. `test_generate_dof_observations_poor_corr` — mag_corr=0.90 → "moderate"
16. `test_generate_dof_observations_phase_low` — max_phase_diff=10 → "within 10.0°"
17. `test_generate_dof_observations_phase_medium` — max_phase_diff=30 → "near resonance"
18. `test_generate_dof_observations_phase_high_negligible` — max_phase_diff=100, negligible amplitude → "can be ignored"
19. `test_generate_dof_observations_phase_high_significant` — max_phase_diff=100, significant amplitude → "check phase convention"
20. `test_generate_dof_observations_hidden_heading` — phase_diff_at_visible_heading=False, max_phase_diff=30 → "omitted from the plot"
21. `test_dof_order_length` — DOF_ORDER has 6 elements
22. `test_amplitude_units_keys` — _AMPLITUDE_UNITS covers all 6 DOFs
23. `test_file_descriptions_keys` — _FILE_DESCRIPTIONS has expected solver keys

---

### FILE 2: benchmark_dof_tables.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `_compute_dof_amplitude_rows` | `dof: DOF, h_indices: List[int], solver_results: Dict, solver_names: List[str]` | `List[Dict]` |
| `_compute_dof_phase_rows` | `dof: DOF, h_indices: List[int], solver_results: Dict, solver_names: List[str]` | `List[Dict]` |
| `_build_solver_column_table` | `rows: List[Dict], mode: str, solver_names: List[str]` | `str` (HTML) |

**Mock data needed:** `make_solver_results` (2 solvers)

**Test cases:**

1. `test_compute_amplitude_rows_basic` — 2 solvers, 2 headings → returns rows with peak_amp, peak_period, long_period_amp
2. `test_compute_amplitude_rows_single_heading` — 1 heading → correct row count
3. `test_compute_phase_rows_basic` — 2 solvers, 2 headings → returns rows with phase_at_peak, long_period_phase
4. `test_compute_phase_rows_values` — verify phase_at_peak matches expected index
5. `test_build_solver_column_table_amplitude_mode` — mode="amplitude" → HTML with "Peak", "T(s)", "LP" headers
6. `test_build_solver_column_table_phase_mode` — mode="phase" → HTML with "@Peak", "LP" headers
7. `test_build_solver_column_table_empty_rows` — empty rows → "<em>No data</em>"
8. `test_build_solver_column_table_multi_heading` — multiple headings produce separate table rows per heading

---

### FILE 3: benchmark_dof_sections.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `_add_phase_annotations` | `fig: go.Figure, dof: DOF, max_phase_diff: float, mag_at_max_pd: float, peak_mag: float, max_pd_freq: float, unit: str, x_axis: str, solver_results: Dict, solver_names: List[str], phase_diff_heading_idx: int, visible_heading_indices: Optional[List[int]]` | `None` (mutates fig) |
| `build_dof_report_sections` | `report: BenchmarkReport, solver_results: Dict, solver_names: List[str], x_axis: str, heading_x_axis: bool, headings: Optional[List[float]]` | `str` (HTML) |

**Mock data needed:** `make_benchmark_report`, `make_solver_results`, `go.Figure` with subplots

**Test cases:**

1. `test_add_phase_annotations_below_threshold` — max_phase_diff=10 → no annotation added (fig.layout.annotations empty)
2. `test_add_phase_annotations_negligible_amplitude` — max_phase_diff=100, negligible mag → annotation with "Ignore" text
3. `test_add_phase_annotations_significant_amplitude` — max_phase_diff=100, significant mag → annotation with "Review phase convention"
4. `test_add_phase_annotations_hidden_heading` — heading_idx not in visible_heading_indices → no annotation
5. `test_add_phase_annotations_period_x_axis` — x_axis="period" → x_val converted via rad_per_s_to_period_s
6. `test_build_dof_report_sections_contains_all_dofs` — output contains "Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"
7. `test_build_dof_report_sections_has_consensus_badges` — output contains consensus badge HTML
8. `test_build_dof_report_sections_has_stats_table` — output contains "Magnitude correlation"
9. `test_build_dof_report_sections_skipped_headings_note` — when headings have negligible response → "Headings with negligible response omitted"

---

### FILE 4: benchmark_correlation.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `plot_pairwise_correlation_heatmap` | `report: BenchmarkReport, output_dir: Path` | `Path` |
| `render_6x6_matrix` | `corr_dict: dict, labels: List[str]` | `str` (HTML) |
| `build_hydro_coefficients_html` | `report: BenchmarkReport` | `str` (HTML) |
| `build_coupling_heatmap_html` | `output_dir: Path, am_corr: list, damp_corr: list, body_i_name: str, body_j_name: str` | `Path` |
| `build_raw_rao_data_html` | `solver_results: Dict, solver_names: List[str], headings: Optional[List[float]]` | `str` (HTML) |

**Re-exports:** `_compute_dof_amplitude_rows`, `_compute_dof_phase_rows`, `_build_solver_column_table`, `_add_phase_annotations`, `build_dof_report_sections`

**Mock data needed:** `make_benchmark_report`, `make_solver_results`, `tmp_path`

**Test cases:**

1. `test_plot_pairwise_correlation_heatmap_creates_file` — returns Path that exists, is .html
2. `test_plot_pairwise_correlation_heatmap_symmetric` — heatmap matrix is symmetric (diagonal=1)
3. `test_render_6x6_matrix_all_perfect` — all values=1.0 → all cells green (#d5f5e3)
4. `test_render_6x6_matrix_mixed_values` — mix of >=0.999, >=0.99, <0.99 → correct bg colors
5. `test_render_6x6_matrix_missing_values` — missing keys → "-" cells
6. `test_render_6x6_matrix_string_keys` — corr_dict with "1,2" string keys → works
7. `test_build_hydro_coefficients_html_empty_report` — no pairwise_results → returns ""
8. `test_build_hydro_coefficients_html_has_added_mass` — output contains "Added Mass"
9. `test_build_hydro_coefficients_html_has_damping` — output contains "Radiation Damping"
10. `test_build_coupling_heatmap_html_creates_file` — returns Path that exists
11. `test_build_coupling_heatmap_html_filename` — filename matches body names
12. `test_build_raw_rao_data_html_all_dofs` — output contains all 6 DOF names
13. `test_build_raw_rao_data_html_truncation` — >100 freq → truncation message
14. `test_build_raw_rao_data_html_heading_filter` — pass specific headings → only those shown

---

### FILE 5: benchmark_input_comparison.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `build_input_comparison_html` | `solver_names: List[str], solver_results: Dict[str, DiffractionResults], solver_metadata: Dict[str, Dict]` | `str` (HTML) |
| `build_semantic_equivalence_html` | `solver_names: List[str], solver_metadata: Dict[str, Dict]` | `str` (HTML) |

**Mock data needed:** `make_diffraction_results`, metadata dicts with various fields

**Test cases:**

1. `test_input_comparison_basic` — 2 solvers with metadata → HTML contains "<h2>Input Comparison</h2>"
2. `test_input_comparison_water_depth` — solver_results have water_depth → row with depth value
3. `test_input_comparison_freq_range` — shows frequency range "(min–max (count))"
4. `test_input_comparison_heading_range` — shows heading range
5. `test_input_comparison_missing_metadata` — metadata missing keys → "-" cells
6. `test_input_comparison_section_hiding` — sections with all-dash values are hidden
7. `test_input_comparison_phase_convention` — AQWA → "ISO 6954 (lead)", OrcaWave → "Orcina (lag)"
8. `test_semantic_equivalence_no_data` — no _semantic_equivalence in metadata → returns ""
9. `test_semantic_equivalence_equivalent` — sig_count=0 → badge "EQUIVALENT" (green)
10. `test_semantic_equivalence_few_diffs` — sig_count=3 → badge "3 SIGNIFICANT DIFF(S)" (orange)
11. `test_semantic_equivalence_many_diffs` — sig_count=10 → badge color #e74c3c (red)
12. `test_semantic_equivalence_diff_tables` — diffs list with significant/convention/cosmetic → all 3 sections rendered
13. `test_semantic_equivalence_key_comments` — known keys (e.g. "DivideNonPlanarPanels") get descriptive comments

---

### FILE 6: benchmark_input_files.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `build_input_files_html` | `solver_names: List[str], solver_metadata: Dict[str, Dict]` | `str` (HTML) |

**Mock data needed:** tmp_path for creating fake input files, metadata dicts

**Test cases:**

1. `test_input_files_no_files` — metadata without input_file → returns ""
2. `test_input_files_single_solver` — one solver with valid tmp file → HTML with file content
3. `test_input_files_two_solvers` — two solvers → two file viewer sections
4. `test_input_files_missing_file` — input_file path doesn't exist → skipped (warning logged)
5. `test_input_files_truncation` — file > 2000 lines → "Showing first 2000 lines" note
6. `test_input_files_html_escaping` — file content with <script> tags → properly escaped
7. `test_input_files_includes_semantic_html` — metadata with _semantic_equivalence → semantic section included
8. `test_input_files_file_description` — solver name matching _FILE_DESCRIPTIONS → description shown
9. `test_input_files_open_in_new_window_script` — output contains "openFileWindow_" JS function

---

### FILE 7: benchmark_input_reports.py

**Public Functions:** None — this is a backward-compat shim that re-exports from:
- `benchmark_input_comparison.build_input_comparison_html`
- `benchmark_input_comparison.build_semantic_equivalence_html`  
- `benchmark_input_files.build_input_files_html`
- `benchmark_mesh_schematic.build_mesh_schematic_html`

**Test cases:**

1. `test_reexports_available` — all 4 names importable from benchmark_input_reports
2. `test_reexports_are_same_functions` — each re-export `is` the original function

---

### FILE 8: benchmark_mesh_schematic.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `build_mesh_schematic_html` | `solver_names: List[str], solver_results: Dict, solver_metadata: Dict, get_solver_style: callable` | `str` (HTML) |
| `_build_panel_scatter_html` | `panel_geometry_data: list, title: str, height: int` | `str` (HTML) |
| `_add_fdf_surface_traces` | `fig: go.Figure, fdf_panels: list` | `None` (mutates fig) |
| `_build_panel_mesh3d_html` | `panel_geometry_data: list, fdf_path: Optional[str], title: str, height: int` | `str` (HTML) |

**Mock data needed:** Panel geometry dicts with centroids/areas/vertices, tmp_path for FDF files

**Test cases:**

1. `test_build_panel_scatter_html_basic` — 5 panels with centroids/areas → HTML with Scatter3d
2. `test_build_panel_scatter_html_multi_body` — panels with different objectName → multiple traces
3. `test_build_panel_mesh3d_html_basic` — panels with 4 vertices each → HTML with Mesh3d
4. `test_build_panel_mesh3d_html_triangles` — panels with 3 vertices → works
5. `test_build_panel_mesh3d_html_with_fdf` — valid fdf_path → includes free-surface zone traces
6. `test_build_panel_mesh3d_html_no_fdf` — fdf_path=None → no FS traces
7. `test_add_fdf_surface_traces_mirror` — panels with positive y → also adds mirrored negative-y vertices
8. `test_build_mesh_schematic_with_panel_geometry_vertices` — metadata has panel_geometry with vertices → returns Mesh3d HTML
9. `test_build_mesh_schematic_with_panel_geometry_centroids` — metadata has panel_geometry without vertices → returns Scatter3d HTML
10. `test_build_mesh_schematic_no_metadata` — no mesh data at all → returns ""
11. **NOTE:** Tests requiring GDF file loading (fallback path with MeshPipeline) should be skipped or mocked via `unittest.mock.patch` for the MeshPipeline import.

---

### FILE 9: benchmark_rao_helpers.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `get_solver_style` | `solver_idx: int` | `dict` (dash, color_base) |
| `get_x_values` | `component: RAOComponent, x_axis: str` | `np.ndarray` |
| `x_axis_label` | `x_axis: str, heading_x_axis: bool` | `str` |
| `get_heading_indices` | `component: RAOComponent, headings: Optional[List[float]]` | `List[int]` |
| `get_significant_heading_indices` | `dof: DOF, solver_results: Dict, solver_names: List[str], headings: Optional[List[float]], threshold: float` | `List[int]` |
| `add_solver_traces` | `fig: go.Figure, dof: DOF, headings: Optional, row: int, col: int, value_type: str, show_legend: bool, solver_results: Dict, solver_names: List[str], x_axis: str, heading_x_axis: bool, heading_indices: Optional[List[int]]` | `None` (mutates fig) |
| `apply_layout` | `fig: go.Figure, title: str` | `None` (mutates fig) |
| `save_figure` | `fig: go.Figure, filename: str, output_dir: Path` | `Path` |

**Mock data needed:** `make_rao_component`, `make_solver_results`

**Test cases:**

1. `test_get_solver_style_idx_0` — returns solid dash, blue color
2. `test_get_solver_style_wraps` — idx=4 wraps to idx=0 style
3. `test_get_x_values_period` — x_axis="period" → returns periods array
4. `test_get_x_values_frequency` — x_axis="frequency" → returns frequencies array
5. `test_x_axis_label_period` — returns "Period (s)"
6. `test_x_axis_label_frequency` — returns "Frequency (rad/s)"
7. `test_x_axis_label_heading` — heading_x_axis=True → "Heading (deg)"
8. `test_get_heading_indices_none` — headings=None → all indices returned
9. `test_get_heading_indices_exact_match` — headings=[0, 90] → indices for those headings
10. `test_get_heading_indices_close_match` — headings=[0.5] (within 1°) → snaps to 0°
11. `test_get_heading_indices_no_match` — headings=[999] → falls back to all indices
12. `test_get_significant_heading_indices_all_significant` — all headings have large response → all returned
13. `test_get_significant_heading_indices_some_negligible` — one heading with zero response → excluded
14. `test_get_significant_heading_indices_all_zero` — all zero response → returns all (fallback)
15. `test_add_solver_traces_amplitude` — value_type="amplitude" → fig gets traces with magnitude data
16. `test_add_solver_traces_phase` — value_type="phase" → fig gets traces with phase data
17. `test_add_solver_traces_heading_x_axis` — heading_x_axis=True → x values are heading angles
18. `test_add_solver_traces_custom_heading_indices` — heading_indices=[0,2] → only those headings plotted
19. `test_apply_layout_sets_title` — fig.layout.title.text == title
20. `test_apply_layout_white_template` — fig.layout.template set
21. `test_save_figure_creates_file` — returns Path, file exists

---

### FILE 10: benchmark_rao_plots.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `plot_amplitude_overlay` | `solver_results: Dict, solver_names: List[str], output_dir: Path, x_axis: str, heading_x_axis: bool, headings: Optional[List[float]]` | `Path` |
| `plot_phase_overlay` | (same signature as above) | `Path` |
| `plot_combined_overlay` | (same signature as above) | `Path` |
| `plot_difference` | `reference_solver: str, solver_results: Dict, solver_names: List[str], output_dir: Path, x_axis: str, heading_x_axis: bool, headings: Optional[List[float]]` | `Path` |
| `plot_per_dof` | `solver_results: Dict, solver_names: List[str], output_dir: Path, x_axis: str, heading_x_axis: bool, headings: Optional[List[float]]` | `Dict[str, Path]` |

**Re-exports:** All of benchmark_rao_helpers + benchmark_rao_summary functions

**Mock data needed:** `make_solver_results`, `tmp_path`

**Test cases:**

1. `test_plot_amplitude_overlay_creates_file` — returns Path, file exists, is HTML
2. `test_plot_amplitude_overlay_content` — HTML contains "Amplitude"
3. `test_plot_phase_overlay_creates_file` — returns Path, file exists
4. `test_plot_phase_overlay_content` — HTML contains "Phase"
5. `test_plot_combined_overlay_creates_file` — returns Path
6. `test_plot_combined_overlay_6x2_subplots` — HTML contains all 6 DOF names
7. `test_plot_difference_creates_file` — returns Path
8. `test_plot_difference_invalid_ref` — reference_solver not in results → raises ValueError
9. `test_plot_difference_content` — HTML contains "Difference"
10. `test_plot_per_dof_creates_6_files` — returns dict with 6 keys (surge..yaw), all paths exist
11. `test_plot_per_dof_keys` — dict keys are lowercase DOF names
12. `test_reexports_available` — all helper/summary names importable from benchmark_rao_plots

---

### FILE 11: benchmark_rao_summary.py

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `compute_amplitude_summary` | `solver_results: Dict, solver_names: List[str], headings: Optional[List[float]]` | `List[Dict]` (6 sections) |
| `compute_phase_summary` | `solver_results: Dict, solver_names: List[str], headings: Optional[List[float]]` | `List[Dict]` (6 sections) |
| `build_summary_table` | `sections: List[Dict], mode: Literal["Amplitude","Phase"]` | `str` (HTML) |
| `render_html_with_table` | `fig, summary: List[Dict], filename: str, mode: Literal["Amplitude","Phase"], output_dir: Path` | `Path` |

**Mock data needed:** `make_solver_results`, `tmp_path`, Plotly Figure

**Test cases:**

1. `test_compute_amplitude_summary_6_sections` — returns list of 6 dicts
2. `test_compute_amplitude_summary_structure` — each section has "dof", "unit", "rows" keys
3. `test_compute_amplitude_summary_row_keys` — each row has heading, solver, peak_amp, peak_period, long_period_amp, diff_pct
4. `test_compute_amplitude_summary_diff_pct_ref` — first solver diff_pct should be "0.0" (ref vs itself)
5. `test_compute_amplitude_summary_heading_filter` — pass specific headings → only those in rows
6. `test_compute_phase_summary_6_sections` — returns list of 6 dicts
7. `test_compute_phase_summary_structure` — each section has "dof", "rows" keys
8. `test_compute_phase_summary_row_keys` — each row has heading, solver, phase_at_peak, long_period_phase, phase_diff
9. `test_build_summary_table_amplitude` — mode="Amplitude" → contains "Peak Amp", "Peak T(s)", "LP Amp", "Diff(%)"
10. `test_build_summary_table_phase` — mode="Phase" → contains "Phase@Peak", "LP Phase", "Diff(deg)"
11. `test_build_summary_table_all_dofs` — contains all 6 DOF names (Surge..Yaw)
12. `test_render_html_with_table_creates_file` — returns Path, file exists
13. `test_render_html_with_table_content` — HTML has grid layout, plot div, table panel

---

## BONUS: diffraction_units.py (included for completeness)

**Public Functions:**

| Function | Parameters | Returns |
|---|---|---|
| `kg_to_tonnes` | `v: ArrayLike` | `ArrayLike` |
| `tonnes_to_kg` | `v: ArrayLike` | `ArrayLike` |
| `density_kg_m3_to_t_m3` | `v: ArrayLike` | `ArrayLike` |
| `density_t_m3_to_kg_m3` | `v: ArrayLike` | `ArrayLike` |
| `inertia_kg_m2_to_t_m2` | `v: ArrayLike` | `ArrayLike` |
| `inertia_t_m2_to_kg_m2` | `v: ArrayLike` | `ArrayLike` |
| `hz_to_rad_per_s` | `v: ArrayLike` | `ArrayLike` |
| `rad_per_s_to_hz` | `v: ArrayLike` | `ArrayLike` |
| `rad_per_s_to_period_s` | `v: ArrayLike` | `ArrayLike` |
| `period_s_to_rad_per_s` | `v: ArrayLike` | `ArrayLike` |
| `radians_to_degrees` | `v: ArrayLike` | `ArrayLike` |
| `degrees_to_radians` | `v: ArrayLike` | `ArrayLike` |
| `complex_phase_degrees` | `v: ArrayLike` | `ArrayLike` |

**Test cases:** (13 functions × 2 cases each = 26 tests)

Each function tested with both scalar float and numpy array input.
Round-trip tests: `tonnes_to_kg(kg_to_tonnes(v)) ≈ v`, etc.

---

## Summary Statistics

| File | Public Functions | Proposed Tests | Priority |
|---|---|---|---|
| benchmark_helpers.py | 3 + constants | 23 | HIGH (leaf module) |
| benchmark_dof_tables.py | 3 | 8 | HIGH (leaf module) |
| benchmark_dof_sections.py | 2 | 9 | MEDIUM |
| benchmark_correlation.py | 5 | 14 | MEDIUM |
| benchmark_input_comparison.py | 2 | 13 | MEDIUM |
| benchmark_input_files.py | 1 | 9 | LOW |
| benchmark_input_reports.py | 0 (shim) | 2 | LOW |
| benchmark_mesh_schematic.py | 4 | 11 | MEDIUM |
| benchmark_rao_helpers.py | 8 | 21 | HIGH (leaf module) |
| benchmark_rao_plots.py | 5 | 12 | MEDIUM |
| benchmark_rao_summary.py | 4 | 13 | HIGH |
| diffraction_units.py | 13 | 26 | HIGH (leaf module) |
| **TOTAL** | **50** | **161** | |

---

## Implementation Order (recommended)

1. **conftest.py** — Build all mock factories first
2. **diffraction_units.py** — Pure math, no dependencies
3. **benchmark_helpers.py** — Leaf module, pure functions
4. **benchmark_rao_helpers.py** — Leaf module, Plotly helpers
5. **benchmark_dof_tables.py** — Leaf module, table builders
6. **benchmark_rao_summary.py** — Depends on rao_helpers
7. **benchmark_input_comparison.py** — HTML builders
8. **benchmark_dof_sections.py** — Depends on helpers, rao_helpers, dof_tables
9. **benchmark_correlation.py** — Depends on most modules
10. **benchmark_rao_plots.py** — Depends on rao_helpers, rao_summary
11. **benchmark_input_files.py** — File I/O, uses input_comparison
12. **benchmark_mesh_schematic.py** — Most complex (mock MeshPipeline)
13. **benchmark_input_reports.py** — Shim, 2 trivial tests

---

## Key Testing Notes

- All Plotly-producing functions should verify output is non-empty HTML string
  containing expected div IDs / keywords, NOT pixel-level rendering.
- Functions that write files (save_figure, render_html_with_table, etc.) use
  pytest `tmp_path` fixture.
- `build_mesh_schematic_html` imports MeshPipeline lazily — mock it with
  `unittest.mock.patch` to avoid OrcFxAPI dependency.
- Use `numpy.testing.assert_allclose` for numeric comparisons.
- Tests should run with `uv run pytest` — no OrcFxAPI in test environment.
