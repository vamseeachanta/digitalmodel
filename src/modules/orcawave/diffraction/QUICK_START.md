# OrcaWave Diffraction Module - Quick Start Guide

## Geometry Files Location

The geometry files are now stored in the specifications directory:
```
specs/modules/orcawave/sea-cypress-diffraction-analysis/inputs/geometry/
```

## Available Commands

### 1. List Available Vessels
```bash
python src/modules/orcawave/diffraction/orchestrator.py --list-vessels
```

### 2. Validate Geometry Files
```bash
# Validate Sea Cypress geometry
python src/modules/orcawave/diffraction/scripts/validate_geometry.py \
    --path specs/modules/orcawave/sea-cypress-diffraction-analysis/inputs/geometry \
    --vessel sea_cypress
```

### 3. Run Complete Analysis
```bash
# Run full workflow for Sea Cypress
python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress

# Dry run (validation only)
python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --dry-run
```

### 4. Run Specific Phase
```bash
# Setup and validation only
python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --phase setup

# Available phases: setup, execute, process, qa, package
```

## Directory Structure

```
src/modules/orcawave/diffraction/
├── orchestrator.py              # Main workflow controller
├── configs/
│   ├── base_diffraction_config.yml
│   └── vessels/
│       └── sea_cypress.yml     # Vessel configuration
├── scripts/
│   ├── validate_geometry.py    # Geometry validation
│   ├── convert_to_orcaflex.py  # Results converter
│   └── run_diffraction_analysis.bat
└── results/
    └── [vessel_name]/          # Results organized by vessel

specs/modules/orcawave/sea-cypress-diffraction-analysis/
└── inputs/
    └── geometry/               # Geometry files location
        ├── Sea Cypress_0.25 Mesh_Binary.stl
        ├── Sea Cypress_0.25 Mesh_Ascii.stl
        └── Sea Cypress_0.25 Mesh_Binary.obj
```

## Configuration

The Sea Cypress vessel configuration (`configs/vessels/sea_cypress.yml`) is configured to use:
- **Geometry path**: `specs/modules/orcawave/sea-cypress-diffraction-analysis/inputs/geometry`
- **Primary file**: `Sea Cypress_0.25 Mesh_Binary.stl`
- **Panel size**: 0.25 meters
- **Water depth**: 100 meters

## Output Files

Results are saved in `results/[vessel_name]/`:
- `[vessel]_hydrodynamics.xlsx` - Excel with all coefficients
- `[vessel]_vessel_type.yml` - OrcaFlex vessel type
- `validation_report_*.txt` - Geometry validation report
- `final_report_*.txt` - Complete workflow summary

## Prerequisites

1. **OrcaWave Installation**: Ensure OrcaWave is installed with valid license
2. **Python Dependencies**: Install via uv
   ```bash
   uv sync
   ```
3. **Geometry Files**: Located in specs directory (already in place)

## Common Issues

### Geometry Path Not Found
- The module now uses relative paths from repository root
- Geometry files are in: `specs/modules/orcawave/sea-cypress-diffraction-analysis/inputs/geometry/`
- Configuration automatically resolves relative paths

### Missing Dependencies
- Use `uv sync` to install all dependencies
- h5py may need separate installation: `uv add h5py`

### Windows Batch Script
- The batch script assumes Windows environment
- For Linux/Mac, create equivalent shell script

## Next Steps

1. Run geometry validation to ensure files are accessible
2. Execute dry run to validate configuration
3. Run full analysis with OrcaWave license
4. Review results in `results/sea_cypress/` directory