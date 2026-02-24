# Module: well

## Purpose
Provides well engineering analysis covering drilling mechanics, rate-of-penetration
prediction, wellbore hydraulics, casing and tubular design, and real-time drilling
dysfunction detection.

## Key Classes / Functions
- `BourgoineYoungROP`: Eight-parameter Bourgoyne-Young rate-of-penetration
  model; fits empirical coefficients to offset well data and predicts ROP
  from WOB, RPM, mud weight, bit size, and formation strength
- `WarrenROP`: Warren power-law ROP model as a simpler two-parameter
  alternative for parametric sensitivity studies
- `RopPrediction`: Dataclass holding predicted ROP with confidence bounds
  and contributing factor breakdown
- `WellboreHydraulics`: Computes equivalent circulating density (ECD),
  annular velocity, standpipe pressure, cuttings transport ratio (CTR),
  and pressure drop through bit nozzles; supports API and SI unit modes
- `DysfunctionDetector`: Real-time detection of drilling dysfunctions
  (stick-slip, bit bounce, whirl, washout) from surface measurements;
  emits `DysfunctionEvent` objects with severity and recommended action
- `DysfunctionEvent`: Dataclass recording dysfunction type, timestamp,
  severity, depth, and suggested mitigation
- `DysfunctionType`: Enum of detectable dysfunction classes:
  STICK_SLIP, BIT_BOUNCE, WHIRL, WASHOUT, PACK_OFF, LOST_CIRCULATION
- `DesignEnvelope` (`tubulars/design_envelope.py`): Tubular design envelope
  combining burst, collapse, and triaxial von Mises checks per API 5C3 and
  ISO 10400 for casing and tubing strings

## Data Sources
- API 5C3 / ISO 10400: tubular performance property tables (embedded)
- Offset well logs: LAS format or CSV; supplied per project
- Mud property reports: YAML/CSV inputs with rheology parameters
- Drilling parameter feeds: real-time CSV or WITSML streams

## Integration Points
- **Depends on**: `infrastructure.base_solvers` (BaseSolver),
  `infrastructure.config`, `structural.stress` (von Mises for triaxial check)
- **Used by**: `field_development` (well program design),
  `production_engineering` (completion sizing),
  `workflows` (automated well performance workflows)

## Status
Active — drilling mechanics and hydraulics complete; tubulars module partial
(design_envelope implemented, full string design planned)
