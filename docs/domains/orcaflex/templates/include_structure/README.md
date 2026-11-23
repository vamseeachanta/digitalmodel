# OrcaFlex Include-Based Model Structure

## Overview

This directory contains templates for the include-based OrcaFlex model format (Format B).
The include-based format splits a monolithic YAML file into modular components for:

- **Reusability**: Share common components across models
- **Parametric analysis**: Easy parameter modification
- **Version control**: Track changes to specific sections
- **Maintainability**: Smaller, focused files

## Directory Structure

```
model_name/
├── master.yml              # Main file with includes + input parameters
├── includes/
│   ├── 01_general.yml      # General settings, units, stages
│   ├── 02_var_data.yml     # VariableData lookup tables
│   ├── 03_environment.yml  # Wave, current, wind, seabed
│   ├── 04_vessel_types.yml # VesselTypes definitions
│   ├── 05_line_types.yml   # LineTypes definitions
│   ├── 06_vessels.yml      # Vessel instances
│   ├── 07_lines.yml        # Line instances
│   ├── 08_buoys.yml        # Buoy instances (6DBuoys)
│   └── 09_shapes.yml       # Shape definitions (if applicable)
└── inputs/
    └── parameters.yml      # Extracted input parameters
```

## Master File Format

```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5
---
# Input Parameters (for parametric analysis)
_inputs:
  water_depth: 100        # m
  hs: 3.5                 # m, significant wave height
  tp: 12.0                # s, peak period
  current_speed: 1.2      # m/s

# Include files
- includefile: includes/01_general.yml
- includefile: includes/02_var_data.yml
- includefile: includes/03_environment.yml
- includefile: includes/04_vessel_types.yml
- includefile: includes/05_line_types.yml
- includefile: includes/06_vessels.yml
- includefile: includes/07_lines.yml
```

## Include File Numbering

Files are numbered to ensure consistent loading order:

| Number | Section | Description |
|--------|---------|-------------|
| 01 | General | Analysis settings, stages, units |
| 02 | VariableData | Lookup tables (drag coefficients) |
| 03 | Environment | Water depth, waves, current, wind |
| 04 | VesselTypes | Vessel type definitions with RAOs |
| 05 | LineTypes | Line type definitions |
| 06 | Vessels | Vessel instances |
| 07 | Lines | Line instances |
| 08 | Buoys | Buoy instances (6DBuoys, Buoys) |
| 08a | BuoyTypes | Buoy type definitions |
| 09 | Shapes | Shape definitions |
| 10 | Constraints | Constraint definitions |
| 11 | Links | Link definitions |
| 12 | Winches | Winch definitions |
| 13 | Supports | Support definitions |

## Conversion Tool

Use the conversion script to convert flat YAML files:

```bash
# Single file
python scripts/conversion/yaml_to_include.py source.yml -o output_dir/

# Batch conversion
python scripts/conversion/yaml_to_include.py source_dir/ -b -o output_dir/
```

## OrcaFlex Include File Syntax

OrcaFlex supports the `includefile` directive:

```yaml
- includefile: path/to/include.yml
```

The include file must contain valid OrcaFlex YAML sections.
