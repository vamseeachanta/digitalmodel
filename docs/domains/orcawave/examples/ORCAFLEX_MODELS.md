# OrcaFlex Model Locations

The OrcaFlex model files (`.yml`) for these OrcaWave examples are maintained in the canonical OrcaFlex examples directory to avoid duplication.

## File Locations

| OrcaWave Example | OrcaFlex Model Location |
|------------------|-------------------------|
| L01_default_vessel | `docs/domains/orcaflex/examples/raw/L01/L01 Default vessel.yml` |
| L02 OC4 Semi-sub | `docs/domains/orcaflex/examples/raw/L02/L02 OC4 Semi-sub.yml` |
| L03 Semi-sub multibody analysis | `docs/domains/orcaflex/examples/raw/L03/L03 Semi-sub multibody analysis.yml` |
| L04 Sectional bodies | `docs/domains/orcaflex/examples/raw/L04/L04 Sectional bodies.yml` |
| L05 Panel pressures | `docs/domains/orcaflex/examples/raw/L05/L05 Panel pressures.yml` |

## Directory Structure

```
docs/domains/
├── orcawave/examples/
│   ├── L01_default_vessel/        # OrcaWave diffraction files (.gdf, .owr, .yml)
│   ├── L02 OC4 Semi-sub/          # OrcaWave diffraction files
│   ├── L03 Semi-sub multibody/    # OrcaWave diffraction files
│   ├── L04 Sectional bodies/      # OrcaWave diffraction files
│   └── L05 Panel pressures/       # OrcaWave diffraction files
│
└── orcaflex/examples/raw/
    ├── L01/                       # OrcaFlex model (.yml, .dat)
    ├── L02/                       # OrcaFlex model
    ├── L03/                       # OrcaFlex model
    ├── L04/                       # OrcaFlex model
    └── L05/                       # OrcaFlex model
```

## Notes

- OrcaWave `.yml` files contain diffraction analysis settings
- OrcaFlex `.yml` files contain the full vessel model with hydrodynamic database
- The OrcaFlex models reference the OrcaWave hydrodynamic results
