# OrcaWave Diffraction Analysis Module

## Overview
This module provides OrcaWave diffraction analysis capabilities for the Sea Cypress vessel and other marine structures.

## Directory Structure

```
diffraction-analysis/
├── configs/                 # OrcaWave configuration files
│   ├── sea_cypress.owp     # OrcaWave project file
│   └── sea_cypress_diffraction.yml  # Analysis configuration
│
├── inputs/                  # Input files for analysis
│   ├── geometry/           # STL and OBJ mesh files
│   │   ├── Sea Cypress_0.25 Mesh_Binary.stl
│   │   └── Sea Cypress_0.25 Mesh_Binary.obj
│   └── *.yml              # Input configuration files
│
├── outputs/                # Analysis outputs
│   ├── processed/         # Processed results for OrcaFlex
│   └── *.csv/xlsx        # RAOs and hydrodynamic data
│
├── scripts/                # Execution scripts
│   ├── execute_orcawave.py            # Main OrcaWave executor
│   ├── process_orcawave_results.py    # Result processor
│   ├── orcaflex_to_orcawave.py       # Format converter
│   └── run_*.bat                      # Batch execution scripts
│
├── revision-1/            # Previous geometry iterations (archived)
│   ├── geometry-iterations/  # Test geometries and conversions
│   ├── documentation/        # Iteration documentation
│   └── scripts/              # Test scripts
│
└── spec documents         # Specifications and task tracking
```

## Quick Start

### 1. Prerequisites
- OrcaWave license and installation
- Python environment with required packages
- Input geometry in STL or OBJ format

### 2. Running Analysis

#### Using Python scripts:
```bash
# Execute OrcaWave analysis
python scripts/execute_orcawave.py --config configs/sea_cypress_diffraction.yml

# Process results for OrcaFlex
python scripts/process_orcawave_results.py --input outputs/ --output outputs/processed/
```

#### Using batch scripts:
```bash
# Run complete analysis pipeline
scripts/run_sea_cypress_analysis.bat

# Start OrcaWave in background
scripts/start_orcawave_background.bat
```

### 3. Configuration

Main configuration file: `configs/sea_cypress_diffraction.yml`

Key parameters:
- Vessel geometry and properties
- Wave frequency range
- Analysis directions
- Output format preferences

## Key Scripts

### execute_orcawave.py
Main executor for OrcaWave analysis. Handles:
- Project setup
- Geometry import
- Analysis execution
- Result extraction

### process_orcawave_results.py
Processes OrcaWave outputs into formats suitable for:
- OrcaFlex vessel data import
- Engineering analysis
- Visualization

### orcaflex_to_orcawave.py
Converts between OrcaFlex and OrcaWave formats for:
- Geometry definitions
- RAO data
- Hydrodynamic coefficients

## Outputs

### Hydrodynamic Data
- `sea_cypress_RAOs.xlsx` - Response amplitude operators
- `sea_cypress_added_mass.csv` - Added mass coefficients
- `sea_cypress_damping.csv` - Damping coefficients
- `sea_cypress_excitation.csv` - Wave excitation forces
- `sea_cypress_mean_drift.csv` - Mean drift forces
- `sea_cypress_QTF_sum.csv` - Quadratic transfer functions

### Processed for OrcaFlex
- `outputs/processed/sea_cypress_orcaflex.yml` - OrcaFlex-ready vessel data
- `outputs/processed/sea_cypress_orcaflex.json` - JSON format vessel data

## Important Notes

1. **Geometry Requirements**
   - Mesh should be watertight
   - Proper orientation (Z-up)
   - Appropriate mesh density

2. **Analysis Settings**
   - Ensure wave frequencies cover operational range
   - Include all relevant wave headings
   - Verify water depth settings

3. **Previous Iterations**
   - Geometry iteration files moved to `revision-1/` for reference
   - Contains test geometries and validation scripts
   - Useful for troubleshooting geometry issues

## Troubleshooting

See `AI_AGENT_ESCALATION_GUIDE.md` and `SPECIALIZED_KNOWLEDGE_REQUIREMENTS.md` for:
- Domain expertise requirements
- Common issues and solutions
- When to escalate to domain experts

## Next Steps

1. Verify OrcaWave installation and license
2. Review and adjust configuration in `configs/sea_cypress_diffraction.yml`
3. Run analysis using provided scripts
4. Import results into OrcaFlex for coupled analysis

## Support

For OrcaWave-specific issues, refer to:
- OrcaWave documentation
- `RUN_ORCAWAVE_INSTRUCTIONS.md` for detailed execution steps
- Domain expert consultation for complex geometry or analysis issues