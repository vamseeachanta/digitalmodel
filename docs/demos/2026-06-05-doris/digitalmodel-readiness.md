# Digitalmodel Demo Readiness For Doris Group

Assessment date: 2026-05-28. Scope: offshore/subsea-first slice for the 2026-06-05 Doris Group demo. Smoke checks were intentionally short; AMBER means a domain has useful material but needs setup, data, license, cleanup, or a tighter demo script.

## Readiness Matrix

| Domain | Status | Demo artifact | Fit for Doris | Command |
|---|---|---|---|---|
| hydrodynamics | GREEN | `src/digitalmodel/hydrodynamics/wave_spectra.py`; `src/digitalmodel/hydrodynamics/rao_analysis/` | Strong offshore fit: wave spectra and RAO analysis are directly relevant to floating systems and subsea installation response | `PYTHONPATH=src .venv/bin/python -m pytest tests/unit/hydrodynamics/test_wave_spectra_extended.py -q` and `PYTHONPATH=src .venv/bin/python -m pytest tests/unit/hydrodynamics/test_rao_analysis.py -q` |
| naval_architecture | GREEN | `src/digitalmodel/naval_architecture/seakeeping.py`; packaged vessel YAML under `src/digitalmodel/naval_architecture/data/` | Strong fit for vessel response, seakeeping, maneuvering, and floating-platform discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/naval_architecture/test_seakeeping.py -q` |
| structural | GREEN | `src/digitalmodel/structural/pipe_capacity/`; `examples/structural/pipe_capacity/basic_usage.py` | Strong fit for subsea pipe/member capacity discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/structural/pipe_capacity/test_pipe_capacity_common.py -q` |
| fatigue | GREEN | `tests/test_fatigue_basic.py`; `examples/structural/fatigue/basic_usage.py` | Strong fit for fatigue screening and design-code workflow discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/test_fatigue_basic.py -q` |
| field_development | GREEN | `src/digitalmodel/field_development/workflow.py`; schematic modules under `src/digitalmodel/field_development/schematics/` | Strong executive/demo fit: concept selection, tieback/FPSO/platform workflows, cost/schedule framing | `PYTHONPATH=src .venv/bin/python -m pytest tests/field_development/test_workflow.py -q` |
| cathodic_protection | GREEN | `src/digitalmodel/cathodic_protection/`; tests for anode sizing and DNV/ISO/API methods | Direct Doris fit for subsea asset integrity and corrosion protection | `PYTHONPATH=src .venv/bin/python -m pytest tests/cathodic_protection/test_anode_sizing.py -q` |
| drilling_riser | GREEN | `src/digitalmodel/drilling_riser/stackup.py`; test vectors under `tests/fixtures/test_vectors/drilling/` | Good offshore fit for riser stack-up and operability discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/drilling_riser/test_stackup_doc_verified.py -q` |
| solvers/openfoam | GREEN | `src/digitalmodel/solvers/openfoam/case_builder.py`; `marine_solvers.py` | Good technical fit as a case-generation framework for marine current/wave CFD, but not yet a turnkey Doris demo scene | `PYTHONPATH=src .venv/bin/python -m pytest tests/solvers/openfoam/test_case_builder.py -q` and `PYTHONPATH=src .venv/bin/python -m pytest tests/solvers/openfoam/test_marine_solvers.py -q` |
| orcaflex | GREEN | `src/digitalmodel/orcaflex/model_builder.py`; `docs/domains/orcaflex/library/` | High Doris fit for risers, moorings, installation, and model-library workflows; live OrcaFlex solve requires license | `PYTHONPATH=src .venv/bin/python -m pytest tests/orcaflex/test_model_builder.py -q` |
| asset_integrity | GREEN | `src/digitalmodel/asset_integrity/`; RSF/API 579 tests | Strong fit for integrity, FFS, corrosion, and defect assessment | `PYTHONPATH=src .venv/bin/python -m pytest tests/asset_integrity/test_rsf_calculations.py -q` |
| subsea | AMBER | `docs/subsea/cross_sections/offshore_cross_section_report.html`; `src/digitalmodel/subsea/` | Very strong Doris fit, but quick smoke found broken/misaligned subsea tests; use cross-section/report artifact or a known-good slice instead of broad subsea VIV/pipeline tests | `PYTHONPATH=src .venv/bin/python -m pytest tests/subsea/viv_analysis/test_viv_analysis_unit.py -q` failed; `PYTHONPATH=src .venv/bin/python -m pytest tests/subsea/pipeline/test_on_bottom_stability.py -q` failed at import |
| marine_ops | AMBER | `tests/marine_ops/marine_engineering/integration/charts/`; `examples/marine_ops/marine_engineering/basic_usage.py` | Good fit for marine engineering/mooring/RAO visuals, but the quick wave-spectra smoke has benchmark failures | `PYTHONPATH=src .venv/bin/python -m pytest tests/marine_ops/marine_engineering/test_wave_spectra.py -q` |
| orcawave | AMBER | `src/digitalmodel/workflows/mcp_server/orcawave/`; `examples/solvers/orcawave/basic_usage.py`; `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` | Relevant for diffraction/radiation workflows, but local smoke failed on stale import path and likely needs Windows/OrcaWave setup for live demo | `PYTHONPATH=src .venv/bin/python -m pytest tests/workflows/orcawave/test_integration.py -q` |
| geotechnical | GREEN | `src/digitalmodel/geotechnical/piles.py`; `src/digitalmodel/geotechnical/scour.py` | Useful supporting slice for foundation/pile/scour discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/test_geotechnical_piles.py -q` |
| gis | GREEN | `src/digitalmodel/specialized/gis/`; sample GeoJSON/KML fixtures under `tests/specialized/gis/test_data/` | Useful map/spatial layer slice for offshore assets and wells | `PYTHONPATH=src .venv/bin/python -m pytest tests/specialized/gis/test_geometry.py -q` |
| power | GREEN | `src/digitalmodel/power/analysis/load_flow.py`; microgrid/protection modules | Secondary fit: electrical/load-flow/protection could support offshore facilities but is not the lead story | `PYTHONPATH=src .venv/bin/python -m pytest tests/power/analysis/test_load_flow.py -q` |
| production_engineering | GREEN | `src/digitalmodel/production_engineering/nodal_solver.py`; IPR/VLP modules | Secondary fit for wells/production systems; useful if Doris audience includes flow assurance/production | `PYTHONPATH=src .venv/bin/python -m pytest tests/production_engineering/test_nodal_solver.py -q` |
| signal_processing | GREEN | `src/digitalmodel/signal_processing/time_series/`; OrcaFlex signal-analysis modules | Useful support slice for time histories, FFT, fatigue inputs, and post-processing | `PYTHONPATH=src .venv/bin/python -m pytest tests/signal_processing/time_series/test_sample_fft.py -q` |
| well | GREEN | `src/digitalmodel/well/drilling/hydraulics.py`; tubular design modules | Secondary fit for drilling/well engineering discussion | `PYTHONPATH=src .venv/bin/python -m pytest tests/well/drilling/test_hydraulics.py -q` |
| ansys | GREEN | `src/digitalmodel/ansys/apdl_reader.py`; APDL/WBJN/reporting parsers | Secondary fit as simulation-file automation; live ANSYS execution is not required for parser demo | `PYTHONPATH=src .venv/bin/python -m pytest tests/ansys/test_apdl_reader.py -q` |
| nde | AMBER | `docs/examples/acoustic_nde_validation.py`; `src/digitalmodel/nde/well_acoustic/` | Interesting subsea inspection story, but optional `the_well` data dependency is not installed locally | `PYTHONPATH=src .venv/bin/python docs/examples/acoustic_nde_validation.py` |
| reservoir | AMBER | `src/digitalmodel/reservoir/stratigraphic.py`; `examples/domains/input_files/reservoir_analysis/field_example_basic.yml` | Secondary fit only; quick smoke has a pandas warning promoted to failure | `PYTHONPATH=src .venv/bin/python -m pytest tests/reservoir/test_stratigraphic.py -q` |

## Suggested Demo Order

1. **Field development**: start with concept/workflow framing because it is easiest for Doris stakeholders to place in a project lifecycle.
2. **Hydrodynamics + naval architecture**: show wave spectra, RAO/seakeeping, and vessel-response capability.
3. **OrcaFlex model library**: show generated/model-library assets and explain live execution depends on licensed OrcaFlex environment.
4. **Structural + fatigue + cathodic protection + asset integrity**: present as the integrity/design-code engineering layer.
5. **Drilling riser + OpenFOAM**: use as deeper technical slices; OpenFOAM should be positioned as marine CFD case generation rather than a complete solver demo.
6. **GIS / geotechnical / signal-processing support**: use only if time remains or the audience asks about spatial data, foundations, or post-processing.

## Smoke Evidence

Green checks run successfully:

- `tests/unit/hydrodynamics/test_wave_spectra_extended.py`: 59 passed.
- `tests/unit/hydrodynamics/test_rao_analysis.py`: 69 passed.
- `tests/naval_architecture/test_seakeeping.py`: 15 passed.
- `tests/structural/pipe_capacity/test_pipe_capacity_common.py`: 103 passed.
- `tests/test_fatigue_basic.py`: 5 passed.
- `tests/field_development/test_workflow.py`: 30 passed.
- `tests/cathodic_protection/test_anode_sizing.py`: 15 passed.
- `tests/drilling_riser/test_stackup_doc_verified.py`: 15 passed.
- `tests/solvers/openfoam/test_case_builder.py`: 26 passed.
- `tests/solvers/openfoam/test_marine_solvers.py`: 25 passed.
- `tests/orcaflex/test_model_builder.py`: 14 passed.
- `tests/asset_integrity/test_rsf_calculations.py`: 28 passed.
- `tests/test_geotechnical_piles.py`: 9 passed.
- `tests/specialized/gis/test_geometry.py`: 39 passed.
- `tests/power/analysis/test_load_flow.py`: 32 passed.
- `tests/production_engineering/test_nodal_solver.py`: 8 passed.
- `tests/signal_processing/time_series/test_sample_fft.py`: 1 passed.
- `tests/well/drilling/test_hydraulics.py`: 34 passed.
- `tests/ansys/test_apdl_reader.py`: 23 passed.

Amber checks:

- `tests/subsea/viv_analysis/test_viv_analysis_unit.py`: 15 passed, 6 failed, 20 errors; failures include `FluidProperties.__init__()` fixture/API mismatch and material-unit expectation mismatches.
- `tests/subsea/pipeline/test_on_bottom_stability.py`: collection failed because `digitalmodel.subsea.pipeline.on_bottom_stability` is missing.
- `tests/marine_ops/marine_engineering/test_wave_spectra.py`: 18 passed, 2 failed; benchmark expectations for JONSWAP peak spectral density and moment ordering need review.
- `tests/workflows/orcawave/test_integration.py`: collection failed because it imports `src.mcp.orcawave...`; local stdout also reports OrcaFlex license unavailable.
- `docs/examples/acoustic_nde_validation.py`: exits cleanly but reports optional `the_well` dependency missing, so it is not presentable without setup.
- `tests/reservoir/test_stratigraphic.py`: 7 passed, 1 failed due to a pandas `FutureWarning` treated as failure.

## Demo Positioning

Lead with the GREEN domains. Avoid broad `subsea` and `marine_ops` live test runs in the Doris demo until their failing smoke paths are cleaned up. For OrcaFlex and OrcaWave, show the model library/generator/workflow artifacts and be explicit that live commercial-solver execution requires a licensed workstation.
