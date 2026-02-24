# Module: structural

## Purpose
Provides offshore structural strength, fatigue, and pipe capacity analysis
covering jacket/topside joint and member checks, S-N curve fatigue damage
accumulation, wall thickness sizing, and pipe cross-section stress calculations.

## Key Classes / Functions
- `FatigueAnalysis` (`fatigue/analysis.py`): Spectral and time-domain fatigue
  damage accumulation using S-N curves and Miner's rule
- `DamageAccumulation` (`fatigue/damage_accumulation.py`): Rainflow cycle
  counting and partial damage summation per load case
- `RainflowCounter` (`fatigue/rainflow.py`): ASTM E1049 rainflow counting
  algorithm for stress time-series
- `SNcurves` (`fatigue/sn_curves.py`): DNV-GL and BS7608 S-N curve definitions
  with environment (air/seawater with CP) and weld class selection
- `ParametricSweep` (`fatigue/parametric_sweep.py`): Multi-variable parametric
  fatigue sensitivity studies
- `JointChecks` (`jacket_topside/joint_checks.py`): API RP 2A tubular joint
  unity-check calculations (K, T, Y, X joints)
- `MemberChecks` (`jacket_topside/member_checks.py`): Combined axial, bending,
  and shear member UC calculations per API RP 2A LRFD
- `WallThickness` (`analysis/wall_thickness.py`): Pressure-containment wall
  thickness per DNV-ST-F101, ISO 13623, and ASME B31.8
- `PipeCapacity` (`pipe_capacity/PipeCapacity.py`): Combined loading capacity
  (burst, collapse, axial, bending) for subsea pipes
- `PipeSizing` (`pipe_capacity/PipeSizing.py`): Automated pipe size selection
  given operating loads and code requirements
- `VMStress` (`stress/vm_stress.py`): Von Mises equivalent stress from
  six-component stress tensors
- `StressStrain` (`stress/stress_strain.py`): Stress-strain curve generation
  and material property lookups
- `NonlinearAnalysis` (`stress/nonlinear.py`): Iterative plastic hinge and
  nonlinear material response

## Data Sources
- API RP 2A (LRFD/WSD): embedded code constants and interaction equations
- DNV-GL RP-C203 / DNV-ST-F101: S-N curves and wall thickness tables
- ISO 13623 / ASME B31.8: pipeline design pressure tables
- Material databases: YAML config files at `infrastructure/base_configs/`

## Integration Points
- **Depends on**: `infrastructure.base_solvers` (BaseSolver, SolverStatus),
  `infrastructure.config`, `infrastructure.calculations`
- **Used by**: `subsea.pipeline` (pipe capacity checks),
  `subsea.catenary_riser` (riser fatigue),
  `field_development` (jacket design),
  `asset_integrity` (in-service fatigue tracking),
  `workflows` (multi-domain orchestration)

## Status
Active
