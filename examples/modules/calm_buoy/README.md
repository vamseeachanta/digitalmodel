# CALM Buoy Module Examples

## Overview

Examples for CALM (Catenary Anchor Leg Mooring) buoy offshore mooring systems.

## Files

### Configuration Examples

**`north_sea_calm_project.yml`** - Complete North Sea CALM buoy project configuration
- Water depth: 120m
- 6 radial mooring lines with R4 studless chain
- VLCC offloading capability
- Multiple load cases (operating + extreme)
- Ready-to-use example for project generation

## Usage

### Generate OrcaFlex Model from Configuration

```bash
# From repository root
python scripts/generate_calm_buoy_project.py \
  --config examples/modules/calm_buoy/north_sea_calm_project.yml \
  --fidelity preliminary \
  --validate
```

### Customize for Your Project

```bash
# Copy to projects directory
cp examples/modules/calm_buoy/north_sea_calm_project.yml projects/my_calm_project.yml

# Edit parameters
vim projects/my_calm_project.yml

# Generate
python scripts/generate_calm_buoy_project.py \
  --config projects/my_calm_project.yml \
  --validate
```

## Documentation

- **Quick Start:** `docs/CALM_BUOY_QUICK_START.md`
- **Complete Guide:** `docs/CALM_BUOY_PROJECT_WORKFLOW.md`
- **Template Reference:** `templates/calm_buoy/project_template.yml`

## Related Modules

- `../orcaflex/` - OrcaFlex model templates and validation
- `../mooring/` - Mooring line design examples
- `../fpso/` - FPSO mooring analysis

## Key Parameters

```yaml
human_input:
  site:
    water_depth: 120              # meters

  buoy:
    outer_diameter: 12.0          # meters
    draft: 10.0                   # meters

  mooring:
    number_of_lines: 6
    line_segments:
      - nominal_diameter: 76      # mm (3 inch chain)
        length: 150               # meters
```

## Standards Referenced

- ISO 19901-7:2013 (Station-keeping systems)
- API RP 2SK:2005 (Mooring design)
- DNVGL-OS-E403:2021 (Mooring chain)
- OCIMF SMOG (SPM operations)
