# Legacy OrcFxAPI Script Inventory

> Generated: 2026-02-23 | Scanned: `docs/domains/orcaflex/` | WRK-316

Total Python files scanned: **42**  
Files importing OrcFxAPI: **32**  
Other Python files (no OrcFxAPI): **10**

## OrcFxAPI Script Triage Table

| File | Lines | Type | Domain | Migration | Phase4 Gap |
|------|-------|------|--------|-----------|------------|
| `mooring/semi/rao-check/RAO Study_heave.py` | 60 | chart | mooring | review | matplotlib renderer |
| `mooring/semi/rao-check/RAO Study_heave_MP1.py` | 58 | chart | mooring | review | matplotlib renderer |
| `pipeline/installation/floating/24in_pipeline/monolithic/postproc/generate_env_case_report.py` | 687 | html_report | pipeline | migrating (WRK-315) | RangeGraph CSV export (added WRK-315) |
| `pipeline/installation/floating/24in_pipeline/monolithic/postproc/generate_html_report.py` | 795 | html_report | pipeline | migrating (WRK-315) | RangeGraph CSV export (added WRK-315) |
| `pipeline/installation/floating/24in_pipeline/monolithic/postproc/test_postproc.py` | 101 | postproc | pipeline | migrating (WRK-315) | — |
| `risers/drilling/wd9500ft/PostProcessing Code.py` | 62 | chart | risers | archive | matplotlib renderer |
| `risers/drilling/wd9500ft/postprocessing.py` | 27 | chart | risers | archive | matplotlib renderer |
| `risers/drilling/wd9500ft/Preprocessing code.py` | 42 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/12 Point Line 1- Tension.py` | 65 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/12 Point Line 8 - Tension.py` | 65 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/12point mooring - Vessel Position.py` | 80 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/8 Point Line 1 - Tension.py` | 72 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/8 Point Line 6 - Tension.py` | 72 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/8 point Mooring - -Vessel Position.py` | 77 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Realization.py` | 296 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Riser Stroke.py` | 88 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Stroke-Slack.py` | 76 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Stroke-Stretch.py` | 75 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Tension_Stress_Strain.py` | 92 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/charts/Tension_Stress_Strain_Stretch.py` | 88 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/12 Point Line 1- Tension.py` | 53 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/12 Point Line 8 - Tension.py` | 53 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/12point mooring - Vessel Position.py` | 60 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/8 Point Line 1 - Tension.py` | 60 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/8 Point Line 6 - Tension.py` | 54 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/8 point Mooring - -Vessel Position.py` | 61 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Realization.py` | 288 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Riser Stroke.py` | 79 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Stroke-Slack.py` | 61 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Stroke-Stretch.py` | 60 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Tension_Stress_Strain.py` | 92 | chart | risers | archive | matplotlib renderer |
| `risers/production/compression-study/code/csv/Tension_Stress_Strain_Stretch.py` | 88 | chart | risers | archive | matplotlib renderer |

## Non-OrcFxAPI Python Files

| File | Lines | Type | Domain |
|------|-------|------|--------|
| `examples/conversion_examples.py` | 441 | misc | examples |
| `examples/gom_scr_design_study/analyze_results.py` | 222 | csv_export | examples |
| `examples/gom_scr_design_study/design_study.py` | 211 | misc | examples |
| `examples/gom_scr_design_study/generate_configs.py` | 134 | misc | examples |
| `examples/gom_scr_design_study/generate_html_report.py` | 528 | html_report | examples |
| `examples/model_generator_examples.py` | 390 | misc | examples |
| `examples/raw/K01/GenPower.py` | 13 | postproc | examples |
| `examples/raw/K01/PythonController.py` | 330 | misc | examples |
| `examples/raw/K02/BladedControllerWrapper.py` | 501 | postproc | examples |
| `modular_input_validation_example.py` | 188 | misc | modular_input_validation_example.py |

## Systemic Phase 4 Gaps

Features present in **>= 2 legacy scripts** that Phase 4 does not yet support:

| Gap | Script Count | Recommended Action |
|-----|-------------|-------------------|
| matplotlib renderer | 29 | Add matplotlib-based chart renderer to Phase 4 (new WRK item) |
| RangeGraph CSV export (added WRK-315) | 2 | Completed via WRK-315 (export_rangegraph_csvs) |

## Migration Decision Key

| Decision | Meaning |
|----------|---------|
| `archive` | No active use; superseded by Phase 4. Move to `_archive/`. |
| `review` | Active or complex workflow -- needs manual evaluation before archiving. |
| `migrating (WRK-315)` | Pipeline installation scripts being migrated in WRK-315. |
| `out-of-scope` | Vendor/example code -- keep as reference, do not migrate. |

## Top Migration Candidates

| Priority | Script(s) | Effort | Rationale |
|----------|-----------|--------|-----------|
| 1 | `pipeline/installation/...` (3 scripts) | Medium | WRK-315 -- actively migrating |
| 2 | `risers/production/compression-study/code/charts/` (10 scripts) | High | Matplotlib -- need Phase 4 matplotlib renderer |
| 3 | `mooring/semi/rao-check/` (2 scripts) | Medium | RAO comparison -- new Phase 4 module needed |

## Notes

- K01/K02 raw examples are Orcina vendor tutorial scripts and are **out of scope** for migration.
- Production riser chart scripts all use `matplotlib` -- a systemic Phase 4 gap.
- GOM SCR design study uses parametric model generation -- outside Phase 4 scope.
- Drilling preprocessing scripts are one-time setup scripts -- archive without migration.
