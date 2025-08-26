# OrcaWave Generic Diffraction Analysis Module

A flexible, vessel-agnostic module for performing hydrodynamic diffraction analysis using OrcaWave.

## Features

- **Multi-vessel support**: Configure and analyze any vessel geometry
- **Template-based configuration**: Base template with vessel-specific overrides
- **Complete workflow automation**: From geometry validation to OrcaFlex export
- **Parallel processing**: 3x speedup for geometry validation
- **Multiple output formats**: Excel, CSV, HDF5, OrcaFlex YAML

## Directory Structure

```
diffraction/
├── configs/
│   ├── base_diffraction_config.yml    # Base template
│   ├── vessels/                       # Vessel-specific configs
│   │   └── sea_cypress.yml           # Example vessel config
│   └── [generated configs]            # Auto-generated from templates
├── scripts/
│   ├── orchestrator.py               # Main workflow controller
│   ├── validate_geometry.py          # Geometry validation
│   ├── convert_to_orcaflex.py       # Results converter
│   └── run_diffraction_analysis.bat  # Batch execution
└── results/
    └── [vessel_name]/                # Vessel-specific results
        ├── csv_outputs/
        ├── orcaflex/
        ├── validation/
        └── logs/
```

## Usage

### List Available Vessels
```bash
uv run python src/modules/orcawave/diffraction/orchestrator.py --list-vessels
```

### Run Analysis for Specific Vessel
```bash
# Using predefined vessel configuration
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress

# Dry run to validate configuration
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --dry-run

# Run specific phase only
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --phase setup
```

### Use Custom Configuration
```bash
uv run python src/modules/orcawave/diffraction/orchestrator.py --config path/to/custom_config.yml
```

## Adding New Vessels

### Method 1: Create Vessel Configuration

1. Create a new YAML file in `configs/vessels/`:

```yaml
# configs/vessels/my_vessel.yml
vessel:
  name: "My Vessel"
  type: "FPSO"
  length_overall: 250.0
  beam: 45.0
  draft: 15.0
  
geometry:
  primary_file: "my_vessel_mesh.stl"
  path: "path/to/geometry/files"
  
environment:
  water_depth: 200.0  # Site-specific
  
frequency_range:
  start: 0.01
  end: 2.5
  steps: 80
  
project:
  name: "My Vessel Analysis"
  client: "Client Name"
```

2. Run analysis:
```bash
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel my_vessel
```

### Method 2: Direct Configuration

Create a complete configuration file based on `base_diffraction_config.yml` and run:
```bash
uv run python src/modules/orcawave/diffraction/orchestrator.py --config my_config.yml
```

## Workflow Phases

### Phase 1: Setup and Validation
- Verify OrcaWave installation
- Validate geometry files (parallel processing)
- Create output directories

### Phase 2: OrcaWave Execution
- Run diffraction analysis
- Monitor convergence
- Handle errors

### Phase 3: Results Processing
- Convert to OrcaFlex format
- Generate visualizations
- Export to multiple formats

### Phase 4: Quality Assurance
- Check reciprocity relations
- Verify energy conservation
- Validate asymptotic behavior

### Phase 5: Results Packaging
- Create deliverable package
- Generate documentation
- Archive results

## Output Files

### OrcaFlex Integration
- `[vessel]_vessel_type.yml` - Complete vessel type definition
- `compact_[vessel]_vessel_type.yml` - Simplified format

### Hydrodynamic Data
- `[vessel]_hydrodynamics.xlsx` - Spreadsheet with all coefficients
- `[vessel].h5` - HDF5 database
- `csv_outputs/` - Individual CSV files

### Reports
- `validation_report_*.txt` - Geometry validation
- `conversion_report.txt` - OrcaFlex conversion
- `final_report_*.txt` - Complete workflow summary

## Configuration Variables

The base template supports these variables:
- `${VESSEL_NAME}` - Vessel name
- `${GEOMETRY_FILE}` - Primary geometry file
- `${GEOMETRY_PATH}` - Path to geometry files
- `${PROJECT_NAME}` - Project identifier
- `${DATE}` - Analysis date

## Requirements

- OrcaWave v11.0+ with valid license
- Python 3.8+ with packages:
  - numpy
  - pandas
  - pyyaml
  - h5py
  - scipy
- Windows OS (for batch scripts)

## Parallel Processing

Geometry validation uses parallel processing:
- Up to 3 files validated simultaneously
- ~3x speedup for multiple geometries
- Automatic result aggregation

## Error Handling

- Comprehensive error logging
- Workflow state preservation
- Error recovery mechanisms
- Detailed error reports in `results/[vessel]/error_*.json`

## Examples

### Sea Cypress Analysis
```bash
# Full workflow
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress

# Validation only
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --phase setup

# Results processing only (after manual OrcaWave run)
uv run python src/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --phase process
```

### Batch Processing Multiple Vessels
```python
import subprocess

vessels = ['sea_cypress', 'fpso_1', 'semi_sub']
for vessel in vessels:
    subprocess.run([
        'python', 
        'orchestrator.py', 
        '--vessel', vessel
    ])
```

## Integration with OrcaFlex

The module generates OrcaFlex-ready vessel types that can be:
1. Imported directly via UI
2. Used in Python API scripts
3. Referenced in batch simulations

## Support

For issues or questions:
- Check logs in `results/[vessel]/logs/`
- Review validation reports
- Consult OrcaWave documentation