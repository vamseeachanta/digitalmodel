# Module: well

**Path**: `src/digitalmodel/well/`
**Status**: in-development
**Last updated**: 2026-02-24

## Purpose

Well engineering analysis covering drilling mechanics, rate-of-penetration
prediction, wellbore hydraulics, casing and tubular design, and real-time
drilling dysfunction detection. Drilling mechanics and hydraulics are
complete; tubulars module is partially implemented.

## Key Packages

| Package | Responsibility | Key Classes/Functions |
|---------|---------------|-----------------------|
| `drilling/` | ROP models, hydraulics, dysfunction detection | `BourgoineYoungROP` (8-param empirical), `WarrenROP` (power-law + `fit_from_data()`), `WellboreHydraulics` (ECD, annular velocity, standpipe pressure, CTR, bit pressure drop), `DrillstringDysfunctionDetector`, `DysfunctionEvent`, `DysfunctionType` |
| `tubulars/` | Casing and tubing design envelope | `DesignEnvelope` (burst/collapse/triaxial von Mises per API 5C3 / ISO 10400) |

## Inputs / Outputs

- **Inputs**: offset well LAS / CSV logs (formation strength, weight on bit, RPM,
  mud weight), mud rheology YAML/CSV, real-time drilling parameter feeds (CSV or
  WITSML), project config YAML
- **Outputs**: predicted ROP with confidence bounds and factor breakdown,
  ECD/standpipe pressure / CTR tables, dysfunction event stream with severity and
  mitigation recommendations, tubular design-envelope burst/collapse plots

## External Dependencies

- `infrastructure.base_solvers` (`BaseSolver`, `SolverStatus`)
- `infrastructure.config` (`ConfigRegistry`, `Settings`)
- `structural.stress` (`VMStress` for triaxial von Mises in tubular checks)
- API 5C3 / ISO 10400 — tubular performance tables (embedded)
- `numpy`, `scipy` — regression fitting (`fit_from_data()`) and numerical solvers

## Known Gaps / Open Work

- `tubulars/` is partial — `DesignEnvelope` is implemented, but full string
  design (weight selection, connection rating, load case matrix) is not yet
  present
- No CLI entry point for the `well` module yet; drilling hydraulics and ROP are
  Python-API only
- WITSML real-time streaming is listed as a data source but not yet implemented;
  current feeds are batch CSV
- `wellpath3D.py` (3-D well trajectory geometry) lives in
  `infrastructure/common/` — candidate for migration to `well/` in WRK-415
- WRK-379 (drilling dysfunction detector integration) depends on WRK-377 (ROP
  models, done) and WRK-378 (hydraulics); status of WRK-378 should be confirmed
  before closing WRK-379

## Tests

| Test file | Coverage |
|-----------|----------|
| `tests/well/drilling/test_rop_models.py` | `BourgoineYoungROP`, `WarrenROP` — unit + `fit_from_data()` |
| `tests/well/drilling/test_hydraulics.py` | `WellboreHydraulics` — ECD, CTR, standpipe pressure |
| `tests/well/drilling/test_dysfunction_detector.py` | `DrillstringDysfunctionDetector` — stick-slip, bit bounce, whirl, washout |
| `tests/well/tubulars/test_design_envelope.py` | `DesignEnvelope` — burst/collapse/triaxial |

31 tests total; all pass as of WRK-377 completion.

## Related WRK Items

- WRK-377: `BourgoineYoungROP` + `WarrenROP` implementation (done)
- WRK-379: Drilling dysfunction detector integration (pending — depends on WRK-378)
- WRK-415: Infrastructure refactor — `wellpath3D.py` migration from
  `infrastructure/common/` to `well/` is a candidate Phase 2 item
- WRK-416: Architecture spec authoring (this file)
