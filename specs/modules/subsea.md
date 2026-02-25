# Module: subsea

**Path**: `src/digitalmodel/subsea/`
**Status**: active
**Last updated**: 2026-02-24

## Purpose

Subsea engineering analysis covering catenary geometry, flexible and vertical
risers, mooring system design, pipeline pressure containment and buckling, and
VIV (vortex-induced vibration) fatigue screening. Integrates with OrcaFlex for
full nonlinear time-domain analysis.

## Key Packages

| Package | Responsibility | Key Classes/Functions |
|---------|---------------|-----------------------|
| `catenary/` | Catenary equation solver and OrcaFlex export | `CatenaryDesigner`, `CatenaryModel`, `OrcaFlexGenerator`; `simple_catenary.py`, `lazy_wave.py`, `effective_weight.py`; `cli.py`; `legacy/` shim; `README.md` |
| `catenary_riser/` | Flexible riser combined analysis | `CatenaryRiser`; integrates catenary geometry with fatigue from `structural.fatigue` |
| `mooring_analysis/` | Quasi-static mooring system design | `MooringAnalysis`; line pretension, offset limits, load-case checks; `cli.py`, `models.py`, `fatigue.py`, `frequency_calculator.py`, `screening.py`; `README.md` |
| `pipeline/` | Pipeline pressure, buckling, hydraulics | `PipelinePressureDNV`, `LateralBuckling`, `PressureLoss`, `PipeSizing`; `buckling_common.py`, `pipeline_pressure_workflow.py`; `pipeline.py` (entry), `cli.py` |
| `viv_analysis/` | VIV response and fatigue screening | `VIVAnalysis`, `VIVScreening`, `viv_analysis_components.py`; `models.py`, `fatigue.py`, `frequency_calculator.py`; `viv_analysis_legacy.py`; `README.md` |
| `vertical_riser/` | Top-tensioned riser stress and fatigue | `VerticalRiser` (`vertical_riser.py`), `vertical_riser_components.py`; `legacy/` shim |

## Inputs / Outputs

- **Inputs**: metocean current profiles (CSV/YAML), pipeline route geometry
  (YAML/CSV), soil data (YAML/CSV for on-bottom stability), OrcaFlex `.dat`
  templates, mooring line material specs (YAML), project config overrides
- **Outputs**: catenary tension/angle/sag tables, OrcaFlex `.dat` input files,
  pipeline wall-thickness and buckling assessment reports, VIV onset screening
  tables, fatigue life summaries (CSV/HTML), mooring offset envelopes

## External Dependencies

- `structural.pipe_capacity` (`PipeCapacity`, `PipeSizing`)
- `structural.fatigue` (`FatigueAnalysis`, `DamageAccumulation`)
- `hydrodynamics` (wave loading spectra for riser fatigue)
- `infrastructure.base_solvers` (`BaseSolver`, `SolverStatus`)
- `infrastructure.config` (`ConfigRegistry`, `Settings`)
- OrcaFlex — external licensed time-domain solver
- DNV-ST-F101 — pressure containment tables (embedded)
- DNV-RP-F110 — lateral/upheaval buckling method (embedded)
- DNV-RP-F105 — VIV response amplitude and fatigue (embedded)
- `numpy`, `scipy` — structural and hydrodynamic calculations

## Known Gaps / Open Work

- `vertical_riser/legacy/` is a stub directory containing only `__init__.py`;
  legacy content has not been migrated
- `catenary_riser/` has no dedicated test suite; coverage comes only via
  integration path through catenary and structural fatigue tests
- `mooring_analysis/` fatigue and frequency-calculator modules (`fatigue.py`,
  `frequency_calculator.py`) lack unit tests
- Upheaval buckling and thermal buckling sub-modules referenced in the old spec
  (`pipeline/upheaval_buckling.py`, `thermal_buckling.py`) were not found on
  disk — either not yet implemented or merged into `lateral_buckling.py`
- No VIV time-domain (SHEAR7 / VIVA) integration; `infrastructure/common/`
  `shear7_model_components.py` is a candidate for migration here (WRK-415)

## Related WRK Items

- WRK-415: Infrastructure refactor — `shear7_model_components.py` and
  `viv_analysis_components.py` from `infrastructure/common/` are candidates
  for consolidation into `subsea/viv_analysis/`
- WRK-416: Architecture spec authoring (this file)
