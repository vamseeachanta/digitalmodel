# Plan: Phase Convention Normalization & Benchmark Input Comparison

## Context

The barge benchmark comparison (AQWA vs OrcaWave) shows **negative phase correlations** across all 6 DOFs (e.g., heave: -0.999, surge: -0.789). Root cause investigation confirmed:

1. **RAO Phase sign flip**: AQWA uses ISO 6954 (phase lead: `A×cos(ωt+φ)`), OrcaWave uses Orcina (phase lag: `A×cos(ωt-φ)`). Fix: `φ_Orcina = -φ_ISO`. Already documented in `docs/modules/standards/RAO_PHASE_CONVENTIONS.md` but NOT applied in extraction pipeline.

2. **M24 (Sway-Roll) coupling sign flip**: Added mass and damping sway-roll coupling terms have opposite signs between solvers (AQWA negative, OrcaWave positive), consistent across all frequencies. M15 (Surge-Pitch) and M35 (Heave-Pitch) agree in sign. Root cause unconfirmed — needs AQWA theory manual verification. Empirically: negate AQWA's row/column 4 (Roll) off-diagonal coupling terms.

3. **Unit system difference**: AQWA uses SI (kg, m, s), OrcaWave uses OrcaFlex units (te, m, s). Factor ~1000×. Does not affect correlation-based comparison (Pearson r is scale-invariant) but should be tracked in metadata.

4. **Heading/axis conventions**: Confirmed IDENTICAL (0°=head seas, right-hand rule, same DOF directions). AQWA uses -180 to +180 range, OrcaWave uses 0 to 180.

**User decisions**: Normalize at extraction time, Orcina (phase lag) as canonical convention, fix pipeline + regenerate reports.

## Files to Modify

| File | Change |
|------|--------|
| `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` | Add `phase_convention` and `unit_system` fields to `DiffractionResults` |
| `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | Negate RAO phases; negate M24-family coupling terms; set convention metadata |
| `scripts/benchmark/run_3way_benchmark.py` | Set convention metadata in `_extract_from_owr()` and `_extract_from_aqwa_lis()` |
| `src/digitalmodel/hydrodynamics/diffraction/benchmark_plotter.py` | Add input comparison section to HTML reports |
| `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | Pass solver metadata to plotter for input comparison |

## Changes

### 1. Add convention metadata to `output_schemas.py`

Add two string fields to `DiffractionResults`:

```python
phase_convention: str = "unknown"    # "orcina_lag" or "iso_lead"
unit_system: str = "SI"              # "SI" (kg,m,s) or "orcaflex" (te,m,s)
```

No value conversion for units — just metadata tracking. Phase normalization happens at extraction.

### 2. Normalize AQWA phases in `aqwa_converter.py`

In `_build_rao_set()` (called from `convert_to_unified_schema()`), after building each `RAOComponent`:

```python
# Normalize from ISO 6954 (phase lead) to Orcina (phase lag)
component.phase = -component.phase
```

Apply to all 6 DOFs. Set `phase_convention="orcina_lag"` on the returned `DiffractionResults`.

### 3. Fix M24 coupling sign in `aqwa_converter.py`

In `_extract_added_mass_data()` and `_extract_damping_data()`, after building each 6×6 matrix, negate the sway-roll off-diagonal coupling:

```python
# Negate Sway-Roll coupling (M24/M42) to match OrcaWave convention
matrix[1, 3] = -matrix[1, 3]  # M24
matrix[3, 1] = -matrix[3, 1]  # M42
```

**Scope**: Only M24/M42 for now — this is the only coupling term with sufficient magnitude to confirm the sign flip. Other roll-coupling terms (M14, M34, M46) are near-zero for this barge geometry and cannot be verified. Add a code comment documenting this as empirically determined and noting the AQWA theory manual should be checked for definitive confirmation.

### 4. Set convention metadata in benchmark script

In `scripts/benchmark/run_3way_benchmark.py`:
- `_extract_from_owr()`: set `phase_convention="orcina_lag"`, `unit_system="orcaflex"` on returned `DiffractionResults`
- `_extract_from_aqwa_lis()`: set `phase_convention="orcina_lag"` (after applying negation), `unit_system="SI"`

### 5. Add input comparison to benchmark HTML reports

In `benchmark_plotter.py`, add a method `_build_input_comparison_html()` that renders a comparison table of solver inputs. Accept an optional `solver_metadata: Dict[str, Dict]` parameter.

**Input comparison table content** (where available):

| Parameter | AQWA | OrcaWave |
|-----------|------|----------|
| Panel count | from .LIS | from .yml body config |
| Water depth (m) | from spec | from spec |
| Frequency range (rad/s) | min-max, count | min-max, count |
| Heading range (deg) | min-max, count | min-max, count |
| Calculation method | from .LIS header | from .yml SolveType |
| Body dimensions (L×B×T) | from spec/mesh | from spec/mesh |
| Mesh file | filename | filename |
| Phase convention (raw) | ISO 6954 (lead) | Orcina (lag) |
| Unit system | SI (kg) | OrcaFlex (te) |

Render as HTML table at the top of the `benchmark_report.html` page, before the existing plots. Extract metadata from `DiffractionResults` fields and from the solver config files where accessible.

### 6. Regenerate barge_final reports

Re-run the benchmark extraction and plotting on `benchmark_output/barge_final/` using the corrected converters. This will produce updated:
- `benchmark_amplitude.html` — phases now aligned
- `benchmark_phase.html` — phases now aligned
- `benchmark_report.json` — phase correlations should become strongly positive
- `benchmark_report.html` — with input comparison section

## Verification

1. **Unit test**: `uv run pytest tests/hydrodynamics/diffraction/ -v -k benchmark` — all existing tests pass
2. **Phase correlation check**: After regeneration, verify all 6 DOF phase correlations are **positive** (>0.7) in `benchmark_report.json`
3. **Long-period spot check**: At T=20.9s, heading=0°:
   - Surge phase: both solvers should show ~-90° (Orcina convention)
   - Heave phase: both ~0°
   - Pitch phase: both ~+90°
4. **M24 check**: Added mass sway-roll correlation should be **positive** (>0.9) after sign correction
5. **Visual**: Open regenerated HTML reports and confirm phase overlay curves now track together instead of mirroring
6. **Input comparison**: Verify input comparison table appears in `benchmark_report.html` with correct metadata from both solvers

## Open Items / Risks

- **M24 root cause**: The sway-roll coupling sign fix is empirically determined from barge data. Should be validated against AQWA Theory Manual Section 4.3 and/or tested with a simple symmetric geometry (sphere/cylinder) where coupling terms are theoretically zero.
- **Other roll couplings**: M14, M34, M46 corrections may also be needed but cannot be verified with current data (values too small). The fix is scoped to M24/M42 only.
- **Unit conversion**: Not converting values (kg vs te) — just tracking in metadata. If absolute value comparison is needed later, a separate conversion step can be added.
