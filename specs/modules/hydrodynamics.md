# Module: hydrodynamics

## Purpose
Provides hydrodynamic coefficient management, wave spectrum generation, and
environmental loading calculations for offshore vessel response analysis,
covering added mass, damping matrices, RAOs, and OCIMF wind/current loads.

## Key Classes / Functions
- `HydrodynamicMatrix`: 6×6 frequency-dependent added mass or damping matrix
  following DNV-RP-C205 convention; validates symmetry and positive-definiteness
- `VesselProperties`: Vessel geometric and mass properties (LOA, beam, draft,
  displacement, wind area projections) used for OCIMF and general analysis
- `WaveParameters`: Sea state definition supporting JONSWAP, Pierson-Moskowitz,
  Bretschneider, ISSC, and Ochi-Hubble bimodal spectra
- `EnvironmentalConditions`: Combined wave, wind, and current parameters for
  loading analysis
- `RAOData`: Response Amplitude Operator amplitude and phase arrays over
  frequency and heading; supports nearest-neighbour interpolation per DOF
- `WaveSpectra`: Generates wave spectral density arrays from `WaveParameters`
- `CoefficientDatabase`: Stores and retrieves frequency-dependent
  hydrodynamic coefficient sets by vessel ID
- `CoefficientsInterpolator`: Interpolates coefficients to requested
  frequency arrays using cubic spline or linear methods
- `OCIMFLoading`: Computes wind and current loads per OCIMF MOORING
  EQUIPMENT GUIDELINES (MEG4)
- `get_vessel_type()`: Returns standard `VesselProperties` for fpso,
  semisubmersible, or tanker archetypes

## Sub-packages
- `aqwa/` — ANSYS AQWA pre/post-processing: input deck generation, LIS/AH1
  result parsing, batch runners, result extraction, OrcaFlex export
- `bemrosetta/` — BEMRosetta diffraction solver integration: mesh pipeline,
  GMSH mesh builder, multi-solver comparison, WAMIT reference loading,
  RAO plotting and report generation
- `diffraction/` — Solver-agnostic diffraction workflow: OrcaWave backend,
  unit conversion, geometry quality checks, input/output schema validation
- `rao_analysis/` — Post-processing of RAO datasets from any backend
- `hull_library/` — Parametric hull definitions for standard vessel types
- `passing_ship/` — Hydrodynamic interaction forces during vessel passing

## Data Sources
- ANSYS AQWA: `.lis`, `.ah1` output files; local solver run
- OrcaWave / OrcaFlex: `.owd`, `.sim` output files; local solver run
- BEMRosetta: `.bem`, `.hst` mesh and result files; local solver run
- WAMIT: reference benchmark datasets; local files
- OCIMF MEG4: tabulated wind and current coefficients (embedded constants)
- Wave scatter diagrams: site-specific hindcast CSV/YAML inputs

## Integration Points
- **Depends on**: `infrastructure.base_solvers` (BaseSolver), `infrastructure.config`
- **Used by**: `subsea.mooring_analysis` (vessel RAOs for coupled analysis),
  `subsea.viv_analysis` (wave loading on tubulars),
  `marine_ops` (operability and limiting sea states),
  `workflows` (orchestrated multi-domain runs)

## Status
Active
