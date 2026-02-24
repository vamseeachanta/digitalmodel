# Module: hydrodynamics

**Path**: `src/digitalmodel/hydrodynamics/`
**Status**: active
**Last updated**: 2026-02-24

## Purpose

Hydrodynamic analysis for offshore structures and vessels — wave loading,
frequency-domain diffraction, RAO computation, OCIMF environmental loading,
and planing hull dynamics. Integrates with ANSYS AQWA, OrcaWave, and
BEMRosetta as external solver backends.

## Key Packages

| Package | Responsibility | Key Classes/Functions |
|---------|---------------|-----------------------|
| `aqwa/` | ANSYS AQWA pre/post-processing | `AqwaPreProcess`, `AqwaPostProcess`, `AqwaReader`, `AqwaRouter`, `AqwaAnalysisRAOs`, `AqwaBatchRunner`, `ViscousDampingDetermination`; `.LIS`/`.AH1` parsers; `ef_server/`, `aqwa_validation/` |
| `bemrosetta/` | BEMRosetta diffraction workflow | `MeshPipeline`, `GmshMeshBuilder`, `MultiSolverComparator`, `WamitReferenceLoader`, `RaoPlotter`, `ReportGenerator`, `OrcaWaveRunner`, `OrcaWaveBatchRunner`, `PolarsExporter` |
| `diffraction/` | Solver-agnostic diffraction orchestration | `DiffractionCli`, `DiffractionUnits`, `GeometryQuality`, `InputSchemas`, `OutputSchemas`, `OutputValidator`, `SpecConverter` |
| `rao_analysis/` | RAO dataset post-processing | `RaoAnalysis` (`rao_analysis.py`); `legacy/` shim |
| `hull_library/` | Parametric hull definitions | `catalog.py`, `lookup.py`, `geometry.py`, `coarsen_mesh.py`, `decimation.py`; mesh utilities (`line_generator/`) |
| `passing_ship/` | Passing vessel hydrodynamic interaction | `calculator.py`, `configuration.py`, `formulations.py`, `force_time_history.py`, `exporters.py`, `input_schemas.py`; `CONVENTIONS.md` |
| `planing_hull/` | 2D+t strip theory planing hull dynamics | `solver.py`, `strip_model.py`, `geometry.py` (WRK-394 in progress) |
| `wave_spectra.py` | Spectral density generation | `WaveSpectra`; JONSWAP, PM, Bretschneider, ISSC, Ochi-Hubble |
| `coefficient_database.py` | Frequency-dependent coefficient storage | `CoefficientDatabase` |
| `interpolator.py` | Coefficient interpolation | `CoefficientsInterpolator` (cubic spline / linear) |
| `models.py` | Core data models | `HydrodynamicMatrix`, `VesselProperties`, `WaveParameters`, `EnvironmentalConditions`, `RAOData` |
| `ocimf_loading.py` | Wind/current loads per OCIMF MEG4 | `OCIMFLoading` |

## Inputs / Outputs

- **Inputs**: AQWA `.lis`/`.ah1` files, OrcaWave `.owd`/`.sim` files,
  BEMRosetta `.bem`/`.hst` files, WAMIT benchmark datasets, YAML/CSV sea
  state scatter diagrams, project YAML config overrides
- **Outputs**: frequency-domain coefficient arrays (added mass, damping, RAOs),
  OCIMF wind/current load tables, diffraction comparison reports (HTML/CSV),
  OrcaFlex `.dat` export files, polars DataFrames for downstream analysis

## External Dependencies

- `infrastructure.base_solvers` (`BaseSolver`, `SolverStatus`)
- `infrastructure.config` (`ConfigRegistry`, `Settings`)
- ANSYS AQWA — external licensed solver (Windows, `acma-ansys05`)
- OrcaWave / OrcaFlex — external licensed solver
- BEMRosetta — open-source BEM solver
- WAMIT — external reference benchmark tool
- `gmsh` — mesh generation for BEMRosetta pipeline
- `polars`, `numpy`, `scipy` — data handling and interpolation

## Known Gaps / Open Work

- `planing_hull/` (WRK-394) is in early development — `solver.py` and
  `strip_model.py` exist but the 2D+t heave/pitch model is not yet complete;
  no tests present
- `rao_analysis/legacy/` is a stub directory with only `__init__.py`; no
  legacy migration has been done
- No unit tests for `wave_spectra.py`, `ocimf_loading.py`, or
  `coefficient_database.py` — integration test coverage only via diffraction
  workflow tests
- `passing_ship/` lacks a CLI entry point and has no dedicated test suite

## Related WRK Items

- WRK-372: AQWA batch-execution skill documented (SKILL.md v1.1.0)
- WRK-394: Planing hull 2D+t strip theory heave/pitch model (in progress)
- WRK-416: Architecture spec authoring (this file)
