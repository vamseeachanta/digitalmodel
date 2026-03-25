# CALM Buoy Project Template

## Overview

This directory contains the project-level YAML template system for CALM (Catenary Anchor Leg Mooring) buoy offshore mooring projects.

## Quick Links

📘 **Documentation:**
- [Quick Start Guide](../../docs/CALM_BUOY_QUICK_START.md) - Get started in 5 minutes
- [Complete Workflow](../../docs/CALM_BUOY_PROJECT_WORKFLOW.md) - Full documentation (40+ pages)
- [System Summary](../../docs/PROJECT_YAML_SYSTEM_SUMMARY.md) - Implementation details

📁 **Files:**
- [project_template.yml](project_template.yml) - Master YAML template (507 lines)
- [Example Project](../../examples/north_sea_calm_project.yml) - Complete North Sea example

🔧 **Tools:**
- [Generation Script](../../scripts/generate_calm_buoy_project.py) - Python project generator (575 lines)
- [Validation Script](../../run_validation.py) - Validation framework

## What This Template Does

### Input: Project YAML Configuration
```yaml
human_input:
  project:
    name: "My CALM Buoy Project"
    code: "CALM_001"

  site:
    water_depth: 120
    metocean_override:
      hs_max: 2.5

  buoy:
    outer_diameter: 12.0
    draft: 10.0

  mooring:
    number_of_lines: 6
    line_segments:
      - nominal_diameter: 76
        length: 150
```

### Output: Complete Project Structure
```
projects/CALM_001/
├── project_config.yml              # Updated with AI validation
├── README.md                       # Project documentation
├── orcaflex/                       # OrcaFlex model files
│   ├── CALM_001_calm_buoy.yml     # Main file (loadable in OrcaFlex)
│   └── modules/                    # 16-17 module files
├── reports/validation/             # Validation reports
│   ├── validation_*.html          # Interactive dashboard
│   ├── validation_*.md            # Markdown report
│   └── validation_*.csv           # CSV data
├── freecad/                        # FreeCAD files (future)
├── blender/                        # Blender files (future)
└── data/                           # Project-specific data
```

## Features

✅ **200+ Configurable Parameters:**
- Project metadata (name, client, location, standards)
- Site conditions (water depth, seabed, metocean)
- Buoy specifications (geometry, mass, hydrodynamics)
- Mooring system (lines, chains, anchors, safety factors)
- Offloading system (tanker, hoses, hawser)
- Analysis settings (load cases, simulation parameters)

✅ **AI Validation & Recommendations:**
- Validate parameters against engineering limits
- Calculate derived properties
- Generate warnings and suggestions
- Track validation history

✅ **Multi-Fidelity Analysis:**
- Preliminary: Quick screening (coarse mesh)
- Detailed: Full analysis (discretised buoy)
- Sensitivity: Parametric variations

✅ **Comprehensive Validation:**
- Level 1: YAML syntax validation
- Level 2: OrcaFlex API validation (optional)
- Level 3: Physical consistency validation

✅ **Multiple Report Formats:**
- Console (colored output)
- CSV (structured data)
- Markdown (human-readable)
- HTML (interactive dashboard)

## Usage

### 1. Quick Start (5 minutes)

```bash
# Copy example
cp ../../examples/north_sea_calm_project.yml ../../projects/my_project.yml

# Edit configuration
vim ../../projects/my_project.yml

# Generate project
python ../../scripts/generate_calm_buoy_project.py \
  --config ../../projects/my_project.yml \
  --validate

# Review reports
firefox ../../projects/CALM_001/reports/validation/validation_*.html
```

### 2. Detailed Workflow

See [Complete Workflow Guide](../../docs/CALM_BUOY_PROJECT_WORKFLOW.md) for:
- Configuration reference
- Parameter selection guidelines
- Validation procedures
- Best practices
- Troubleshooting

## Template Structure

### Human Input Section (Editable)
```yaml
human_input:
  project:           # Project metadata
  standards:         # Design codes & standards
  site:              # Site conditions & metocean
  buoy:              # Buoy specifications
  mooring:           # Mooring system
  offloading:        # Offloading system
  analysis:          # Analysis settings
```

### AI Generated Section (Auto-populated)
```yaml
ai_generated:
  validation:        # Validation status & confidence
  derived:           # Calculated parameters
  recommendations:   # Warnings & suggestions
  data_sources:      # Reference data links
```

### Generation Configuration
```yaml
generation:
  orcaflex:          # Template selection & overrides
  output:            # Directory structure
  validation:        # Validation settings
```

### Multi-Fidelity Support
```yaml
multi_fidelity:
  enabled: true
  analyses:          # Multiple analysis configurations
    - name: "preliminary_screening"
    - name: "detailed_design"
    - name: "sensitivity_study"
```

## Key Design Decisions

### 1. Directory Structure: Nested by Discipline
```
project/
├── orcaflex/       # OrcaFlex models
├── freecad/        # CAD geometry
├── blender/        # Visualization
├── reports/        # Validation & analysis
└── data/           # Project data
```

### 2. Override Strategy: Hybrid with Dot Notation
```yaml
overrides:
  environment:
    template: "jonswap_default"
    customize:
      wave_hs: "human_input.site.metocean_override.operating_conditions.hs_max"
```

### 3. Human vs AI: Separate Sections (Same File)
- Clear boundaries
- Single source of truth
- Validation flags track history

### 4. Multi-Fidelity: Explicit Configuration
- Declarative approach
- Easy to add/remove analyses
- Supports parametric studies

## Reference Data Integration

The system validates against three data tiers:

**Tier 1 - Generic Ranges:**
```
data/raw/calm_buoy/generic_range/
├── hull_geometry_ranges.csv
├── metocean_design_ranges.csv
└── mooring_capacity_ranges.csv
```

**Tier 2 - Mature Design:**
```
data/processed/calm_buoy/mature_design/
├── hydrodynamic_coefficients.csv
├── structural_components.csv
└── operations_matrix.csv
```

**Tier 3 - Project-Specific:**
```
data/results/calm_buoy/project_specific/
├── environmental_conditions.csv
├── mooring_line_properties.csv
└── offloading_configuration.csv
```

## Standards & Codes

The template references industry standards:
- **ISO 19901-7:2013** - Station-keeping systems
- **API RP 2SK:2005** - Mooring design
- **DNVGL-OS-E403:2021** - Mooring chain
- **OCIMF SMOG** - SPM operations guide
- **OCIMF MEG4** - Mooring equipment

## Future Enhancements

### Phase 2: FreeCAD Integration (Q2 2025)
- Auto-generate 3D buoy geometry
- Create mooring layout drawings
- Export DXF for fabrication

### Phase 3: Blender Visualization (Q3 2025)
- Render high-quality visualizations
- Animate mooring system response
- Generate marketing materials

### Phase 4: Advanced Features (Q4 2025)
- Batch processing
- Parametric optimization
- CI/CD integration
- Web-based UI

## Support

**Issues & Questions:**
- Review [Troubleshooting](../../docs/CALM_BUOY_PROJECT_WORKFLOW.md#troubleshooting) section
- Check [Quick Start FAQ](../../docs/CALM_BUOY_QUICK_START.md#troubleshooting)
- Examine [System Summary](../../docs/PROJECT_YAML_SYSTEM_SUMMARY.md)

**Example Files:**
- Template: `project_template.yml` (this directory)
- Example: `../../examples/north_sea_calm_project.yml`

**Scripts:**
- Generation: `../../scripts/generate_calm_buoy_project.py`
- Validation: `../../run_validation.py`

---

**Version:** 1.0
**Status:** ✅ Complete
**Date:** 2025-01-15
**Author:** Digital Model Team
