# OrcaFlex Folder Structure Standardization - Usage Guide

## Table of Contents
1. [Creating a New Module](#creating-a-new-module)
2. [Migrating an Existing Module](#migrating-an-existing-module)
3. [Configuration Examples](#configuration-examples)
4. [Common Scenarios](#common-scenarios)
5. [Troubleshooting](#troubleshooting)

## Creating a New Module

### Step 1: Create Standard Structure
```bash
# Navigate to OrcaFlex modules directory
cd specs/modules/orcaflex/

# Create new module with standard structure
mkdir -p my-new-module/{config,input/{models,data,parameters},output/{analysis,visual,reports,logs},scripts,tests,docs}

# Create essential files
touch my-new-module/{README.md,USAGE.md,spec.md,tasks.md,prompt.md}
```

### Step 2: Configure Module
Create `my-new-module/config/analysis.yml`:
```yaml
meta:
  library: digitalmodel
  module: orcaflex
  submodule: my-new-module

file_management:
  input_directory: specs/modules/orcaflex/my-new-module/input/models
  output_directory: specs/modules/orcaflex/my-new-module/output/analysis
  plot_directory: specs/modules/orcaflex/my-new-module/output/visual
  
  filename:
    pattern: "*.sim"
    extension: [sim, yml]

visualization:
  enabled: true
  output_directory: specs/modules/orcaflex/my-new-module/output/visual
```

## Migrating an Existing Module

### Step 1: Backup Current Module
```bash
# Create backup
cp -r mooring-tension-iteration mooring-tension-iteration.backup
```

### Step 2: Reorganize Folders
```bash
# Example for mooring-tension-iteration module
cd mooring-tension-iteration

# Move files to standard locations
mkdir -p input/models input/parameters
mv go-by/.sim/* input/models/
mv go-by/scripts/*_target_*.csv input/parameters/

# Reorganize outputs
mkdir -p output/{analysis,visual,reports,logs}
mv go-by/output/.csv/* output/analysis/
mv go-by/output/visual/* output/visual/
mv go-by/scripts/logs/* output/logs/
```

### Step 3: Update Configurations
```python
# Python script to update paths
import yaml
import os

def update_config(config_file):
    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)
    
    # Update to relative paths
    module_name = os.path.basename(os.path.dirname(config_file))
    base_path = f"specs/modules/orcaflex/{module_name}"
    
    config['file_management']['input_directory'] = f"{base_path}/input/models"
    config['file_management']['output_directory'] = f"{base_path}/output/analysis"
    config['file_management']['plot_directory'] = f"{base_path}/output/visual"
    
    with open(config_file, 'w') as f:
        yaml.dump(config, f, default_flow_style=False)

# Usage
update_config('config/analysis.yml')
```

## Configuration Examples

### Mooring Analysis Configuration
```yaml
# config/mooring_analysis.yml
meta:
  library: digitalmodel
  module: orcaflex
  submodule: mooring-tension

file_management:
  input_directory: specs/modules/orcaflex/mooring-tension/input/models
  output_directory: specs/modules/orcaflex/mooring-tension/output/analysis
  plot_directory: specs/modules/orcaflex/mooring-tension/output/visual
  
  filename:
    pattern: "*_vessel_statics_6dof"
    extension: [sim]

orcaflex_analysis:
  mooring:
    groups:
    - label: development
      target_pretension:
        filename: ../input/parameters/target_pretension.csv

visualization:
  enabled: true
  output_directory: specs/modules/orcaflex/mooring-tension/output/visual
  save_plots: true
  formats: [png, pdf]
```

### Post-Processing Configuration
```yaml
# config/postprocess.yml
meta:
  library: digitalmodel
  module: orcaflex
  submodule: postprocess

file_management:
  input_directory: specs/modules/orcaflex/postprocess/input/models
  output_directory: specs/modules/orcaflex/postprocess/output/analysis
  
orcaflex:
  postprocess:
    visualization:
      flag: true
    time_series:
      flag: true
      histogram: true

visualization_settings:
  output_directory: specs/modules/orcaflex/postprocess/output/visual
  viewparams:
    elevation:
      ViewSize: 150
    plan:
      ViewSize: 150
```

## Common Scenarios

### Running Analysis with Standard Paths
```bash
# From repository root
cd /d/github/digitalmodel

# Run mooring analysis
python -m digitalmodel specs/modules/orcaflex/mooring-tension/config/analysis.yml

# Run visualization
python -m digitalmodel specs/modules/orcaflex/mooring-tension/config/visualization.yml
```

### Batch Processing Multiple Models
```yaml
# config/batch_analysis.yml
file_management:
  input_directory: specs/modules/orcaflex/my-module/input/models
  output_directory: specs/modules/orcaflex/my-module/output/analysis
  
  filename:
    pattern: "fsts_*"  # Process all files matching pattern
    extension: [sim]

parallel_processing:
  enabled: true
  max_workers: 10
```

### Generating Reports
```python
# scripts/generate_report.py
import os
from pathlib import Path

# Standard paths
module_path = Path("specs/modules/orcaflex/my-module")
analysis_dir = module_path / "output/analysis"
visual_dir = module_path / "output/visual"
report_dir = module_path / "output/reports"

# Generate report using standard locations
def generate_report():
    # Load analysis results
    csv_files = list(analysis_dir.glob("*.csv"))
    
    # Load visualizations
    plot_files = list(visual_dir.glob("*.png"))
    
    # Create report
    report_path = report_dir / "analysis_report.pdf"
    # ... report generation logic
```

## Troubleshooting

### Issue: "Directory not found" Error
**Solution**: Ensure using relative paths from repository root
```yaml
# Wrong
input_directory: ../input/models  # Relative to config file

# Correct
input_directory: specs/modules/orcaflex/my-module/input/models  # From repo root
```

### Issue: Outputs in Wrong Location
**Solution**: Check configuration paths
```python
# Verify paths
import yaml

with open('config/analysis.yml') as f:
    config = yaml.safe_load(f)
    
print("Input:", config['file_management']['input_directory'])
print("Output:", config['file_management']['output_directory'])
print("Plots:", config['file_management'].get('plot_directory'))
```

### Issue: Migration Breaking Existing Workflows
**Solution**: Create compatibility wrapper
```python
# scripts/compatibility.py
import os
import shutil

def create_legacy_links():
    """Create symbolic links for backward compatibility"""
    # Link old locations to new
    os.symlink('output/analysis', 'go-by/output/.csv')
    os.symlink('output/visual', 'go-by/output/visual')
```

## Best Practices

### 1. Always Use Module Root as Reference
```python
from pathlib import Path

# Get module root
module_root = Path("specs/modules/orcaflex/my-module")

# Build paths from module root
input_dir = module_root / "input/models"
output_dir = module_root / "output/analysis"
```

### 2. Validate Paths Before Processing
```python
def validate_paths(config):
    """Validate all paths exist and are relative"""
    paths = [
        config['file_management']['input_directory'],
        config['file_management']['output_directory']
    ]
    
    for path in paths:
        if os.path.isabs(path):
            raise ValueError(f"Absolute path not allowed: {path}")
        if not os.path.exists(path):
            os.makedirs(path, exist_ok=True)
```

### 3. Use Configuration Templates
```bash
# Copy standard template
cp templates/standard_config.yml my-module/config/analysis.yml

# Update module-specific values
sed -i 's/<module>/my-module/g' my-module/config/analysis.yml
```

## Command Reference

### Essential Commands
```bash
# Validate structure
python tools/validate_structure.py specs/modules/orcaflex/my-module

# Check paths
python tools/check_paths.py config/analysis.yml

# Run analysis
python -m digitalmodel config/analysis.yml

# Generate report
python scripts/generate_report.py
```

### Utility Scripts
```bash
# Create new module
./tools/create_module.sh my-module

# Migrate existing module
./tools/migrate_module.sh old-module

# Batch update configs
./tools/update_configs.sh specs/modules/orcaflex/*/config/*.yml
```

## Examples Repository

Find complete examples at:
- `specs/modules/orcaflex/mooring-tension-iteration/` - Fully migrated module
- `specs/modules/orcaflex/templates/` - Configuration templates
- `tools/examples/` - Script examples

---

*For additional help, see [README.md](README.md) or open an issue.*