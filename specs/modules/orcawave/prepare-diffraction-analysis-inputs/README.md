# OrcaWave Diffraction Analysis Input Preparation

This module provides automated tools for preparing OrcaWave diffraction analysis inputs from various data sources.

## Overview

The workflow automates the complete process of:
1. Extracting vessel data from Excel spreadsheets
2. Converting GMsh geometry files to OrcaWave GDF format
3. Generating OrcaWave input configuration files
4. Running OrcaWave analysis
5. Post-processing results to Excel/CSV formats

## Scripts

### Core Components

- **`extract_excel_data.py`** - Extract vessel properties from Excel files
- **`convert_msh_to_gdf.py`** - Convert GMsh .msh files to OrcaWave GDF format
- **`generate_orcawave_input.py`** - Generate OrcaWave YAML input files from templates
- **`postprocess_results.py`** - Extract and format OrcaWave results
- **`run_orcawave.bat`** - Windows batch script for running OrcaWave

### Orchestration

- **`orchestrate_workflow.py`** - Master script for running complete workflow
- **`test_parallel.py`** - Parallel testing and validation of all components

## Quick Start

### 1. Test All Components
```bash
python scripts/test_parallel.py
```

### 2. Run Complete Workflow
```bash
python scripts/orchestrate_workflow.py complete \
    --excel vessel_data.xlsx \
    --msh "Sea Cypress_0.25 Mesh_Ascii.msh" \
    --template go-by/orcawave_001_ship_raos_rev2.yml \
    --output-dir output/
```

### 3. Run Individual Steps

#### Extract Excel Data
```bash
python scripts/extract_excel_data.py vessel_data.xlsx --sheet "Properties"
```

#### Convert Geometry
```bash
python scripts/convert_msh_to_gdf.py input.msh --output output.gdf --validate
```

#### Generate OrcaWave Input
```bash
python scripts/generate_orcawave_input.py template.yml \
    --output orcawave_input.yml \
    --gdf geometry.gdf \
    --excel vessel_data.xlsx
```

#### Run OrcaWave Analysis
```bash
# GUI Mode
scripts\run_orcawave.bat orcawave_input.yml /GUI

# Batch Mode
scripts\run_orcawave.bat orcawave_input.yml /BATCH /LOG
```

#### Post-process Results
```bash
python scripts/postprocess_results.py results_directory \
    --excel output.xlsx \
    --orcaflex vessel_type.yml
```

## Input Files

### Required Files
1. **Geometry File**: GMsh .msh file
   - Located at: `specs/modules/orcawave/diffraction-analysis/inputs/geometry/`
   - Example: `Sea Cypress_0.25 Mesh_Ascii.msh`

2. **Template Files**: OrcaWave YAML templates
   - Located at: `specs/modules/orcawave/diffraction-analysis/inputs/orcawave/go-by/`
   - Examples: `orcawave_001_ship_raos_rev2.yml`

3. **Excel Data**: Vessel properties spreadsheet
   - Must contain: mass, center of gravity, inertia properties
   - Flexible column naming supported

### Output Files
- `geometry.gdf` - Converted geometry in WAMIT GDF format
- `orcawave_input.yml` - Complete OrcaWave input configuration
- `results.xlsx` - Post-processed results in Excel format
- `vessel_type.yml` - OrcaFlex-compatible vessel definition

## Parallel Processing

All scripts support parallel execution for improved performance:
- Geometry validation runs in parallel threads
- Multiple configurations can be processed simultaneously
- Testing validates all components concurrently
- Target: 3x performance improvement

## Validation

Each script includes comprehensive validation:
- **Geometry**: Watertight check, normal orientation, panel quality
- **Configuration**: Required fields, parameter ranges, file existence
- **Results**: Data completeness, physical validity, format compliance

## Agent Integration

This workflow integrates with specialized AI agents:
- **OrcaWave Agent**: Main orchestration and API operations
- **GMsh Agent**: Geometry processing and mesh operations
- **Testing Agent**: Parallel validation and testing

## Requirements

- Python 3.10+ with uv environment
- Packages: pandas, numpy, pyyaml, openpyxl
- OrcaWave 11.5+ (for execution)
- Windows OS (for batch scripts)

## Testing

Run the parallel test suite to validate all components:
```bash
python scripts/test_parallel.py
```

Expected output:
```
============================================================
PARALLEL TEST REPORT
============================================================
Total Tests: 5
Passed: 5 ✅
Failed: 0 ❌
```

## Troubleshooting

### Common Issues

1. **Excel extraction fails**: Check column naming matches expected patterns
2. **Geometry conversion errors**: Validate .msh file format (ASCII required)
3. **OrcaWave execution fails**: Verify OrcaWave installation path in batch script
4. **Post-processing errors**: Ensure OrcaWave completed successfully

### Debug Mode

Enable verbose logging for detailed diagnostics:
```bash
python scripts/orchestrate_workflow.py complete --verbose ...
```

## Next Steps

After successful input preparation:
1. Review generated files for correctness
2. Run OrcaWave analysis (GUI or batch mode)
3. Post-process results for downstream use
4. Import to OrcaFlex if needed

## Support

For issues or questions:
- Check existing scripts in `specs/modules/orcawave/diffraction-analysis/scripts/`
- Refer to OrcaWave Agent documentation: `agents/orcawave/README.md`
- Review GMsh Agent capabilities: `agents/gmsh/README.md`