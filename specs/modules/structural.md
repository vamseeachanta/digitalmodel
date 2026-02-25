# Module: structural

**Path**: `src/digitalmodel/structural/`
**Status**: active
**Last updated**: 2026-02-24

## Purpose

Offshore structural strength, fatigue, and pipe capacity analysis covering
jacket/topside joint and member checks, S-N curve fatigue damage accumulation,
wall-thickness pressure sizing, pipe cross-section stress, and resilience
lifecycle planning for offshore facilities.

## Key Packages

| Package | Responsibility | Key Classes/Functions |
|---------|---------------|-----------------------|
| `fatigue/` | Spectral and time-domain fatigue | `FatigueAnalysis`, `DamageAccumulation`, `RainflowCounter`, `FrequencyDomainFatigue`, `ParametricSweep`, `DesignCodeReport`, `SNComparisonReport`; `skill.py` for orchestration |
| `fatigue_apps/` | Applied fatigue workflow tooling | `FatigueDamageCalculator`, `IntegratedProcessor`, `FileNamer`; `INPUT_FILE_STRUCTURE.md` |
| `analysis/` | Wall-thickness pressure design | `WallThickness` (DNV-ST-F101, ISO 13623, ASME B31.8); `WallThicknessComparison`, `WallThicknessParametric`, multi-table report generator; `cli.py` |
| `pipe_capacity/` | Combined pipe loading capacity | `PipeCapacity`, `PipeSizing`; `common/` and `custom/` sub-dirs for code-specific extensions |
| `pipe_cross_section/` | Pipe cross-section properties | (referenced from `infrastructure/common/pipe_properties.py`; stub in structural) |
| `stress/` | Stress state and nonlinear material | `VMStress`, `StressStrain`, `NonlinearAnalysis`; `README.md` |
| `jacket_topside/` | Jacket and topside checks | `JointChecks` (API RP 2A K/T/Y/X joints), `MemberChecks` (axial/bending/shear UC) |
| `offshore_resilience/` | Facility lifecycle and resilience | `StructuralHealth`, `LifecyclePlanning`, `MinimumFacility`, `InstallationChecklist`, `_SensorTemplates` |
| `structural_analysis/` | General FEM capacity | `capacity.py`, `buckling.py`, `StressCalculator`; `cli.py`, `models.py`; `legacy/` shim |
| `parametric_coordinator.py` | Multi-domain parametric run orchestration | `ParametricCoordinator` |
| `parametric_report.py` | Report aggregation for parametric studies | `ParametricReport` |

## Inputs / Outputs

- **Inputs**: load case YAML/CSV files, stress time-series CSV, material
  property YAML (from `infrastructure/base_configs/`), API/DNV code constants
  (embedded), project-specific config overrides
- **Outputs**: unity-check tables (CSV/HTML), fatigue life summaries, wall
  thickness selection reports, parametric sensitivity plots, S-N comparison
  reports

## External Dependencies

- `infrastructure.base_solvers` (`BaseSolver`, `SolverStatus`)
- `infrastructure.config` (`ConfigRegistry`, `Settings`)
- `infrastructure.calculations` (math utilities, interpolation)
- `numpy`, `scipy` — numerical solvers and rainflow counting
- `pandas` / `polars` — tabular result handling
- API RP 2A (LRFD/WSD) — embedded interaction equations
- DNV-GL RP-C203 / DNV-ST-F101 — S-N curves and wall thickness tables
- ISO 13623 / ASME B31.8 — pipeline pressure design tables
- BS 7608 — fatigue S-N curves (weld classification)

## Known Gaps / Open Work

- `plate_capacity/` lives in `infrastructure/domains/platecapacity/` as legacy
  scripts (`PlateBuckling_212.py`, `plateBucklingCal_*.py`) — target move to
  `structural/` in WRK-415 migration
- `calculations/plate_buckling.py` in `infrastructure/` duplicates
  `infrastructure/common/plate_buckling.py`; both should consolidate here
  under `structural/analysis/`
- `structural_analysis/legacy/` is a stub directory — no files beyond
  `__init__.py`; migration from `infrastructure/common/` pending
- `pipe_cross_section/` is referenced from `infrastructure/common/` but the
  structural sub-package is a thin stub; full implementation lives in
  `infrastructure/common/pipe_properties.py`
- `fatigue_apps/` has no dedicated unit tests — only validated through
  integration tests in `fatigue_validation/` sub-dir

## Related WRK Items

- WRK-415: Infrastructure refactor — `plate_capacity` migration to structural
  is a Phase 2 deliverable
- WRK-416: Architecture spec authoring (this file)
