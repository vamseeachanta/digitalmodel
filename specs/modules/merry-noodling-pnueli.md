---
title: "Pipeline Operational Phases with Multi-Code Utilisation Analysis"
description: "Extend wall_thickness module with 5 standard operational phases, multi-code side-by-side comparison, and interactive T-M interaction diagrams per phase."
version: "2.0"
module: "wall-thickness-phases"

session:
  id: "20260131-wrk044-phases"
  agent: "claude-opus-4.5"

status: "planning"
priority: "medium"
tags: [wall-thickness, phases, multi-code, interaction-diagram]
---

# Pipeline Operational Phases — Multi-Code Utilisation Analysis

## Summary

New file `wall_thickness_phases.py` that composes the existing `wall_thickness.py` analyzer and `wall_thickness_parametric.py` plotting utilities. Adds 5 standard operational phases, runs each through multiple selectable design codes side-by-side, and produces comparative utilisation charts and T-M interaction diagrams.

**No existing files are modified** (only imports from them).

---

## New File: `src/digitalmodel/analysis/wall_thickness_phases.py`

### Data Model

```
PipeDefinition        - OD, WT, grade, SMYS, SMTS (convenience wrapper)
  .to_geometry()      → PipeGeometry
  .to_material()      → PipeMaterial

PipelinePhase (frozen) - name, Pi, Pe, M, T, description
  .to_design_loads()  → DesignLoads

PhaseResult           - phase_name, code, WallThicknessResult, loads
PhaseComparisonResult - pipe, phases, codes, results[]
  .to_dataframe()     → flat DataFrame (phase × code × check → utilisation)
  .summary_dataframe() → one row per (phase, code)
```

### Phase Factory

```python
compute_hydrostatic_pressure(water_depth, rho=1025, g=9.80665) → Pa
create_standard_phases(water_depth=500, design_pressure=10e6, ...) → List[PipelinePhase]
```

5 phases with computed Pe = ρ·g·depth:

| Phase | Pi | Pe | M | T | Governs |
|-------|----|----|---|---|---------|
| Installation (empty) | 0 | hydrostatic | install M | install T | Collapse + combined |
| Installation (water-filled) | ≈Pe | hydrostatic | install M | install T | Combined loading |
| Hydrotest | 1.25×MAOP | hydrostatic | 0 | 0 | Burst |
| Operation | MAOP | hydrostatic | oper M | oper T | Burst + combined |
| Shutdown (depressurised) | 0 | hydrostatic | 0 | 0 | Collapse |

### Multi-Code Runner

```python
PhaseAnalysisRunner(pipe, phases, codes, safety_class=MEDIUM)
  .run() → PhaseComparisonResult
```

Loops `phases × codes`, creates `WallThicknessAnalyzer` per combination, collects results.

### Visualisation Functions

1. **`plot_phase_utilisation_comparison(comparison)`** — Grouped bar chart
   - Subplots: 1 row × N codes (columns)
   - X-axis: phase names, Y-axis: utilisation
   - Bars grouped by check type, red dashed line at 1.0

2. **`plot_phase_tm_interaction(pipe, phases, codes)`** — Contour subplot grid
   - Rows: phases (5), Columns: codes (2-3)
   - Each cell: filled contour of utilisation + bold unity boundary
   - Red star at operating point (phase T, phase M)
   - Reuses `_von_mises_utilisation_grid` for API cells
   - New `_dnv_combined_utilisation_grid` helper for DNV cells

3. **`generate_phase_summary_table(comparison)`** — DataFrame

4. **`generate_phase_report(comparison, output_path)`** — HTML report with all above

---

## Implementation Steps (TDD)

### Step 1: Data models + hydrostatic pressure
- [ ] Tests: `PipelinePhase`, `PipeDefinition`, `compute_hydrostatic_pressure`
- [ ] Implement constants, dataclasses, hydrostatic function

### Step 2: Phase factory
- [ ] Tests: `create_standard_phases` returns 5 phases with correct pressures
- [ ] Implement factory function

### Step 3: PhaseAnalysisRunner
- [ ] Tests: runner returns correct count of results, correct codes/phases, DataFrame shape
- [ ] Implement runner + `PhaseComparisonResult.to_dataframe()` / `.summary_dataframe()`

### Step 4: Visualisation + report
- [ ] Tests: figures have correct subplots, traces, HTML contains expected sections
- [ ] Implement all 4 viz functions
- [ ] Generate HTML report and open in browser

---

## Files

| File | Action | Description |
|------|--------|-------------|
| `src/digitalmodel/analysis/wall_thickness_phases.py` | CREATE | Phases, runner, visualisation |
| `tests/test_wall_thickness_phases.py` | CREATE | ~35 TDD tests |
| `src/digitalmodel/analysis/wall_thickness.py` | UNCHANGED | Imported only |
| `src/digitalmodel/analysis/wall_thickness_parametric.py` | UNCHANGED | `_von_mises_utilisation_grid` imported |

## Verification

1. `uv run pytest tests/test_wall_thickness_phases.py -v --no-cov` — all pass
2. HTML report renders with bar chart + T-M grid + summary table
3. Both DNV and API codes shown side-by-side
4. Operating point markers visible on T-M interaction diagrams
