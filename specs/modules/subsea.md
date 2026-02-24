# Module: subsea

## Purpose
Provides analysis for subsea pipeline systems, flexible risers, mooring systems,
and VIV (vortex-induced vibration) screening, covering catenary geometry,
lateral/upheaval buckling, pressure design, and fatigue of tubular members.

## Key Classes / Functions
- `CatenaryDesigner` (`catenary/designer.py`): Catenary equation solver for
  mooring lines and flexible risers; computes tension, angle, and sag
- `CatenaryModel` (`catenary/models.py`): Dataclass for catenary geometry and
  material properties with validation
- `OrcaFlexGenerator` (`catenary/orcaflex_generator.py`): Exports catenary
  configurations to OrcaFlex `.dat` input files
- `MooringAnalysis` (`mooring_analysis/`): Mooring system design checks
  including line pretension, offset limits, and quasi-static load cases
- `PipelinePressureDNV` (`pipeline/pipeline_pressure_dnv.py`): Pressure
  containment and stability checks per DNV-ST-F101
- `LateralBuckling` (`pipeline/lateral_buckling.py`): Lateral buckling
  assessment following DNV-RP-F110 effective axial force method
- `UpheavalBuckling` (`pipeline/upheaval_buckling.py`): Upheaval buckling
  resistance checks with cover/trench burial scenarios
- `ThermalBuckling` (`pipeline/thermal_buckling.py`): Temperature-driven axial
  expansion and buckling interaction
- `PressureLoss` (`pipeline/pressure_loss.py`): Steady-state hydraulic
  pressure drop using Darcy-Weisbach and Colebrook-White
- `VIVAnalysis` (`viv_analysis/viv_analysis.py`): Vortex-induced vibration
  response amplitude and fatigue life per DNV-RP-F105
- `VIVScreening` (`viv_analysis/screening.py`): Onset velocity and reduced
  velocity screening for current-dominated regimes
- `VortexShedding` (`viv_analysis/vortex_shedding.py`): Strouhal number and
  lock-in frequency calculations
- `CatenaryRiser` (`catenary_riser/`): Full flexible riser analysis combining
  catenary geometry with fatigue damage accumulation
- `VerticalRiser` (`vertical_riser/`): Top-tensioned riser stress and fatigue

## Data Sources
- DNV-ST-F101 / DNV-RP-F110 / DNV-RP-F105: embedded code tables
- OrcaFlex: `.dat`, `.sim` files (external solver, local run)
- Metocean current profiles: CSV or YAML inputs
- Soil data: YAML/CSV for pipeline on-bottom stability

## Integration Points
- **Depends on**: `structural.pipe_capacity`, `structural.fatigue`,
  `hydrodynamics` (wave loading), `infrastructure.base_solvers`,
  `infrastructure.config`
- **Used by**: `field_development` (pipeline routing and FEED sizing),
  `asset_integrity` (riser inspection planning),
  `workflows` (coupled riser-mooring analysis)

## Status
Active
