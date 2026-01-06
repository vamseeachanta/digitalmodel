# AQWA Analysis Module - Quick Start Guide

**ABOUTME**: Comprehensive quick-start guide for running AQWA diffraction analysis workflows including RAOs, damping, and external force server integration.

---

## Overview

The AQWA module provides automated workflows for:
- **RAO Analysis**: Response Amplitude Operators extraction
- **Damping Analysis**: Hydrodynamic damping computations
- **External Force Server**: EF server integration for coupled analysis

All workflows support both AQWA Workbench (.wbpj) and DAT file (.dat) execution modes.

---

## Quick Start

### 1. Basic RAO Analysis

```bash
# Run RAO extraction from existing AQWA results
python -c "
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis

cfg = {
    'Analysis': {
        'analysis_root_folder': 'path/to/analysis',
        'file_name': 'vessel_model'
    },
    'analysis_settings': {
        'method': 'raos'
    }
}

analyzer = AqwaAnalysis()
results = analyzer.analysis_router(cfg)
print(f'Status: {results[\"results\"][\"status\"]}')
"
```

### 2. Damping Analysis

```bash
python -c "
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis

cfg = {
    'Analysis': {
        'analysis_root_folder': 'path/to/analysis'
    },
    'analysis_settings': {
        'method': 'damping'
    }
}

analyzer = AqwaAnalysis()
results = analyzer.analysis_router(cfg)
"
```

### 3. External Force Server

```bash
python -c "
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis

cfg = {
    'Analysis': {
        'analysis_root_folder': 'path/to/analysis'
    },
    'analysis_settings': {
        'method': 'ef_server'
    }
}

analyzer = AqwaAnalysis()
results = analyzer.analysis_router(cfg)
"
```

---

## Configuration Structure

### Complete Configuration Example

```yaml
# aqwa_analysis_config.yml
Analysis:
  analysis_root_folder: "path/to/analysis/folder"
  file_name: "vessel_name"

analysis_settings:
  method: "raos"  # Options: raos, damping, ef_server

# RAO-specific settings
rao_settings:
  frequency_range: [0.1, 2.0]
  heading_range: [0, 360]

# Damping-specific settings
damping_settings:
  compute_viscous: true

# EF server settings
ef_server_settings:
  port: 5000
  host: "localhost"
```

---

## Supported Analysis Methods

### 1. RAO Analysis (`raos` or `rao`)

Extracts Response Amplitude Operators from AQWA analysis results.

**Input Requirements:**
- AQWA .LIS file with hydrodynamic analysis results
- Analysis root folder path
- Vessel/structure name

**Outputs:**
- RAO matrices for all 6 DOFs
- Frequency and heading coverage
- Phase information

**Usage:**
```python
from digitalmodel.modules.aqwa.aqwa_analysis_raos import AqwaRAOs

cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/MyVessel',
        'file_name': 'vessel_analysis'
    }
}

rao_analyzer = AqwaRAOs()
results = rao_analyzer.rao_router(cfg)
```

### 2. Damping Analysis (`damping` or `viscous_damping`)

Computes hydrodynamic damping matrices.

**Input Requirements:**
- AQWA analysis results
- Vessel geometry

**Outputs:**
- 6x6 damping matrices
- Frequency-dependent coefficients

**Usage:**
```python
from digitalmodel.modules.aqwa.aqwa_analysis_damping import AqwaDamping

cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/MyVessel'
    }
}

damping_analyzer = AqwaDamping()
damping_analyzer.router(cfg)
```

### 3. External Force Server (`ef_server`, `external_forces`)

Launches AQWA External Force Server for coupled analysis.

**Input Requirements:**
- AQWA model configuration
- Communication port settings

**Usage:**
```python
from digitalmodel.modules.aqwa.aqwa_analysis_ef_server import AqwaEFServer

cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/MyVessel'
    },
    'ef_server_settings': {
        'port': 5000,
        'host': 'localhost'
    }
}

ef_server = AqwaEFServer()
ef_server.ef_server_router(cfg)
```

---

## Directory Structure

```
digitalmodel/modules/aqwa/
├── aqwa_analysis.py              # Main entry point with router
├── aqwa_analysis_raos.py         # RAO extraction
├── aqwa_analysis_damping.py      # Damping computation
├── aqwa_analysis_ef_server.py    # External force server
├── aqwa_router.py                # Legacy router
├── aqwa_reader.py                # File parsers
├── aqwa_post_process.py          # Post-processing utilities
├── aqwa_pre_process.py           # Pre-processing utilities
└── aqwa_utilities.py             # Common utilities

docs/modules/aqwa/
├── QUICK_START.md                # This file
├── README.md                     # Module overview
├── examples/                     # Example projects
│   ├── 001_ship_raos.md
│   ├── 002_ship_with_piers.md
│   ├── 003_FPSO_Turret.md
│   └── 102_restart/
├── getting-started/              # Tutorial materials
├── scripts/                      # Batch execution scripts
└── workflows/                    # Workflow documentation
```

---

## Prerequisites

### Software Requirements
1. **ANSYS AQWA** - Licensed installation required for analysis execution
2. **Python 3.8+** - Python environment
3. **UV Package Manager** - For dependency management

### Installation

```bash
# Install dependencies using uv
uv sync

# Verify AQWA installation (Windows)
where aqwa

# Or check environment variable
echo %ANSYS_ROOT%
```

### Python Dependencies
- pandas - Data manipulation
- numpy - Numerical computations
- pyyaml - Configuration parsing

---

## Common Workflows

### Workflow 1: Extract RAOs from Completed AQWA Analysis

```python
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis
from pathlib import Path

# Setup paths
analysis_root = Path("C:/AQWA/Projects/FPSO_Analysis")
model_name = "FPSO_Turret"

# Configure analysis
cfg = {
    'Analysis': {
        'analysis_root_folder': str(analysis_root),
        'file_name': model_name
    },
    'analysis_settings': {
        'method': 'raos'
    },
    'rao_settings': {
        'frequency_range': [0.05, 0.5],  # Hz
        'heading_range': [0, 180]  # degrees
    }
}

# Run analysis
analyzer = AqwaAnalysis()
results = analyzer.analysis_router(cfg)

# Check results
if results['results']['status'] == 'completed':
    print(f"RAO extraction complete: {results['results']['method']}")
else:
    print("Analysis failed")
```

### Workflow 2: Compute Damping Coefficients

```python
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis

cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/Spar',
        'file_name': 'spar_model'
    },
    'analysis_settings': {
        'method': 'viscous_damping'
    },
    'damping_settings': {
        'compute_viscous': True,
        'reference_velocity': 1.0  # m/s
    }
}

analyzer = AqwaAnalysis()
results = analyzer.analysis_router(cfg)
```

### Workflow 3: Launch External Force Server for OrcaFlex Coupling

```python
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis

cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/FPSO',
        'file_name': 'fpso_coupled'
    },
    'analysis_settings': {
        'method': 'external_forces'
    },
    'ef_server_settings': {
        'port': 5555,
        'host': '127.0.0.1',
        'timeout': 3600  # seconds
    }
}

analyzer = AqwaAnalysis()
# This starts the server and waits for connections
analyzer.analysis_router(cfg)
```

---

## Integration with OrcaFlex

### Export RAOs to OrcaFlex Format

```python
from digitalmodel.modules.aqwa.aqwa_analysis_raos import AqwaRAOs
from pathlib import Path

# Extract RAOs
cfg = {
    'Analysis': {
        'analysis_root_folder': 'C:/AQWA/Projects/Vessel',
        'file_name': 'vessel_diffraction'
    }
}

rao_analyzer = AqwaRAOs()
results = rao_analyzer.rao_router(cfg)

# RAOs are automatically formatted for OrcaFlex import
# Output files:
# - vessel_diffraction_raos.csv
# - vessel_diffraction_vessel_type.yml (OrcaFlex format)
```

---

## Examples and Tutorials

### Available Examples

1. **Ship RAOs** - `examples/001_ship_raos.md`
   - Basic ship RAO extraction
   - Multiple heading analysis

2. **Ship with Piers** - `examples/002_ship_with_piers.md`
   - Multi-body interaction
   - Pier fender systems

3. **FPSO Turret** - `examples/003_FPSO_Turret.md`
   - Turret mooring system
   - Wave frequency analysis

4. **Restart Analysis** - `examples/102_restart/`
   - Checkpoint and restart capabilities
   - Long-duration simulations

### Tutorial Materials

Located in `docs/modules/aqwa/examples/training-materials/`:
- AQWA Workbench tutorials
- DAT file examples
- Validation studies

---

## Troubleshooting

### Issue: "Module not found"

**Solution:**
```bash
# Ensure you're in the repository root
cd D:/workspace-hub/digitalmodel

# Activate uv environment
uv sync

# Run with explicit Python path
uv run python -c "from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis"
```

### Issue: "AQWA not found"

**Solution:**
- Verify AQWA installation: `where aqwa` (Windows)
- Check ANSYS environment variables
- Add AQWA to system PATH

### Issue: "Analysis files not found"

**Solution:**
- Verify `analysis_root_folder` path is correct
- Use absolute paths for reliability
- Check file permissions

### Issue: "Invalid method"

**Solution:**
Valid methods are:
- `raos`, `rao` - RAO extraction
- `damping`, `viscous_damping` - Damping analysis
- `ef_server`, `external_forces`, `external-force`, `external_forces_server` - EF server

---

## Advanced Topics

### Batch Processing Multiple Vessels

```python
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis
from pathlib import Path

vessels = [
    {'name': 'FPSO_A', 'folder': 'C:/AQWA/FPSO_A'},
    {'name': 'FPSO_B', 'folder': 'C:/AQWA/FPSO_B'},
    {'name': 'Shuttle', 'folder': 'C:/AQWA/Shuttle'}
]

analyzer = AqwaAnalysis()

for vessel in vessels:
    cfg = {
        'Analysis': {
            'analysis_root_folder': vessel['folder'],
            'file_name': vessel['name']
        },
        'analysis_settings': {
            'method': 'raos'
        }
    }

    results = analyzer.analysis_router(cfg)
    print(f"{vessel['name']}: {results['results']['status']}")
```

### Custom RAO Processing

```python
from digitalmodel.modules.aqwa.aqwa_analysis_raos import AqwaRAOs
import pandas as pd

# Extract RAOs
cfg = {...}
rao_analyzer = AqwaRAOs()
results = rao_analyzer.rao_router(cfg)

# Access raw RAO data for custom processing
# RAO data is available in the analysis output
# Implement custom filtering, interpolation, or analysis
```

---

## API Reference

### AqwaAnalysis Class

**Methods:**
- `analysis_router(cfg)` - Main routing method for all analysis types

**Configuration Keys:**
- `Analysis.analysis_root_folder` (required) - Analysis directory path
- `Analysis.file_name` (optional) - Model/file name
- `analysis_settings.method` (required) - Analysis method selection

**Returns:**
- Dictionary with `results` key containing:
  - `status`: 'completed' or 'failed'
  - `method`: Analysis method used
  - Additional method-specific results

---

## Next Steps

1. **Try Examples**: Start with `examples/001_ship_raos.md`
2. **Explore Workflows**: Review `workflows/` for specific use cases
3. **Integration**: Connect to OrcaFlex using exported vessel types
4. **Automation**: Set up batch processing for multiple vessels/conditions

For detailed technical reference, see:
- Module README: `docs/modules/aqwa/README.md`
- API Documentation: `reference-manuals/`
- AQWA User Guide: ANSYS AQWA documentation

---

**Last Updated**: 2026-01-03
**Version**: Phase 1 - Workflow Hardening
