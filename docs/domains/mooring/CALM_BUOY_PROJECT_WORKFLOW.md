# CALM Buoy Project Workflow Guide

## Overview

This guide describes the complete workflow for creating, configuring, and analyzing CALM (Catenary Anchor Leg Mooring) buoy offshore mooring systems using the Digital Model framework.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Workflow Diagram](#workflow-diagram)
3. [Project Structure](#project-structure)
4. [Configuration File Format](#configuration-file-format)
5. [Generation Process](#generation-process)
6. [Validation Framework](#validation-framework)
7. [Multi-Fidelity Analysis](#multi-fidelity-analysis)
8. [Best Practices](#best-practices)
9. [Troubleshooting](#troubleshooting)

---

## Quick Start

### 1. Create New Project from Template

```bash
# Copy template to your project directory
cp templates/calm_buoy/project_template.yml projects/my_calm_project.yml

# Or use the example
cp examples/north_sea_calm_project.yml projects/my_calm_project.yml
```

### 2. Edit Configuration

Open `projects/my_calm_project.yml` and fill in the `human_input` section:

- Project metadata (name, client, location)
- Site conditions (water depth, metocean)
- Buoy specifications (geometry, mass, hydrodynamics)
- Mooring system (lines, chain properties, anchors)
- Analysis settings (load cases, simulation parameters)

### 3. Generate Project

```bash
# Generate OrcaFlex model with validation
python scripts/generate_calm_buoy_project.py \
  --config projects/my_calm_project.yml \
  --fidelity preliminary \
  --validate

# Or skip validation for faster generation
python scripts/generate_calm_buoy_project.py \
  --config projects/my_calm_project.yml \
  --skip-validation
```

### 4. Review Generated Files

```
projects/NSE_CALM_001/
├── project_config.yml          # Updated with AI-generated sections
├── README.md                   # Project documentation
├── orcaflex/
│   ├── NSE_CALM_001_calm_buoy.yml
│   └── modules/                # 16 module files
├── reports/validation/
│   ├── validation_*.html       # Validation dashboard
│   ├── validation_*.md         # Markdown report
│   └── validation_*.csv        # CSV results
└── data/                       # Project-specific data
```

### 5. Run OrcaFlex Analysis

```bash
# Open in OrcaFlex
OrcaFlex projects/NSE_CALM_001/orcaflex/NSE_CALM_001_calm_buoy.yml

# Or run via Python API (if available)
python scripts/run_orcaflex_analysis.py \
  --model projects/NSE_CALM_001/orcaflex/NSE_CALM_001_calm_buoy.yml
```

---

## Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. HUMAN INPUT                                                  │
│    - Project metadata                                           │
│    - Site conditions                                            │
│    - Buoy specifications                                        │
│    - Mooring system                                             │
│    - Analysis settings                                          │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. VALIDATION                                                   │
│    ✓ Geometry within ranges                                    │
│    ✓ Metocean conditions realistic                             │
│    ✓ Mooring capacity adequate                                 │
│    ✓ Safety factors met                                        │
│    ⚠ Generate warnings & recommendations                       │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. AI PARAMETER DERIVATION                                     │
│    ◉ Calculate mooring footprint                               │
│    ◉ Estimate natural periods                                  │
│    ◉ Compute total system mass                                 │
│    ◉ Predict watch circle radius                               │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. ORCAFLEX MODULE GENERATION                                  │
│    📄 Base file (calm_buoy_base.yml)                           │
│    📦 16-17 module files:                                       │
│       - Units & analysis settings                              │
│       - Environment (waves, current, wind)                     │
│       - Vessel types (buoy hydrodynamics)                      │
│       - Line types (chain properties)                          │
│       - Vessels (buoy instances)                               │
│       - Lines (mooring configuration)                          │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. LEVEL 1-3 VALIDATION                                        │
│    Level 1: YAML syntax ✓                                      │
│    Level 2: OrcaFlex API (optional)                            │
│    Level 3: Physical consistency ✓                             │
│    📊 Generate HTML/CSV/Markdown reports                        │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 6. ORCAFLEX ANALYSIS                                           │
│    🔹 Load model in OrcaFlex                                   │
│    🔹 Run statics                                              │
│    🔹 Run dynamics simulation                                  │
│    🔹 Post-process results                                     │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│ 7. FUTURE INTEGRATIONS (WIP)                                   │
│    🔧 FreeCAD geometry generation                              │
│    🎨 Blender visualization                                    │
│    📐 Structural drawings                                      │
│    📑 Design documentation                                      │
└─────────────────────────────────────────────────────────────────┘
```

---

## Project Structure

### Template-Based Organization

```
digitalmodel/
├── templates/calm_buoy/
│   ├── project_template.yml           # Master template
│   └── orcaflex_templates/            # OrcaFlex module templates
│
├── examples/
│   └── north_sea_calm_project.yml     # Complete example
│
├── projects/
│   └── <PROJECT_CODE>/                # Generated project
│       ├── project_config.yml         # Customized configuration
│       ├── README.md                  # Project documentation
│       ├── orcaflex/                  # OrcaFlex model files
│       ├── freecad/                   # FreeCAD geometry (future)
│       ├── blender/                   # Blender visualization (future)
│       ├── reports/                   # Validation & analysis reports
│       └── data/                      # Project-specific data
│
├── specs/modules/orcaflex/modular-input-file/output/
│   ├── calm_buoy_base.yml            # Standard template (16 modules)
│   ├── discretised_calm_buoy_base.yml # Detailed template (17 modules)
│   └── _*.yml                         # Individual module files
│
└── data/
    ├── raw/calm_buoy/generic_range/   # Generic parameter ranges
    ├── processed/calm_buoy/mature_design/  # Hydrodynamic database
    └── results/calm_buoy/project_specific/ # Project-specific data
```

---

## Configuration File Format

### Section 1: Human Input

```yaml
human_input:
  project:              # Project metadata
    name: "Project Name"
    code: "PRJ_001"
    client: "Client Name"
    # ...

  standards:            # Design standards & codes
    primary_code: "ISO 19901-7"
    mooring_standard: "API RP 2SK"
    # ...

  site:                 # Site conditions
    water_depth: 120
    seabed_type: "soft clay"
    metocean_override:
      operating_conditions:
        hs_max: 2.5
        tp_max: 8.0
        # ...

  buoy:                 # Buoy specifications
    type: "turret_calm"
    outer_diameter: 12.0
    draft: 10.0
    mass_operating: 9200
    # ...

  mooring:              # Mooring system
    pattern: "radial_symmetric"
    number_of_lines: 6
    line_segments:
      - name: "top_chain"
        nominal_diameter: 76
        length: 150
        mbl: 7300
        # ...

  offloading:           # Offloading system
    tanker_type: "VLCC"
    hose_diameter: 16
    # ...

  analysis:             # Analysis settings
    run_preliminary: true
    load_cases:
      - name: "operating_0deg"
        wave_direction: 0
        # ...
```

### Section 2: AI Generated

```yaml
ai_generated:
  validation:           # Validation status
    validated_by_ai: true
    validation_date: "2025-01-15T14:30:00"
    confidence_score: 0.95
    checks:
      geometry_within_ranges: "Pass"
      # ...

  derived:              # Calculated parameters
    mooring_footprint_radius: 285.5
    total_mooring_mass: 3180.0
    watch_circle_radius: 28.5
    # ...

  recommendations:      # AI suggestions
    warnings:
      - "Water depth exceeds typical range"
    suggestions:
      - "Consider increasing chain diameter"
```

### Section 3: Generation Configuration

```yaml
generation:
  orcaflex:
    templates:
      preliminary:
        base: "calm_buoy_base.yml"
      detailed:
        base: "discretised_calm_buoy_base.yml"

    overrides:          # Parameter customization
      environment:
        template: "jonswap_default"
        customize:
          wave_hs: "human_input.site.metocean_override.operating_conditions.hs_max"
          # ...

  validation:
    run_after_generation: true
    levels: [1, 3]      # Skip level 2 if no OrcaFlex
```

---

## Generation Process

### Parameter Override Strategy (Hybrid Approach)

The generation script uses a **hybrid override strategy**:

1. **Template Selection:**
   - Choose base template (preliminary or detailed)
   - Load all module files from template

2. **Parameter Extraction:**
   - Parse human_input using dot notation
   - Extract values like `human_input.site.water_depth`
   - Support array indexing: `mooring.line_segments[0].length`

3. **Module Customization:**
   - Identify customizable modules (waves, buoy, mooring)
   - Apply overrides using string replacement
   - Update file paths to reference local modules

4. **Validation:**
   - Run Level 1 (YAML syntax) validation
   - Run Level 3 (physical consistency) validation
   - Generate reports in multiple formats

### Example: Wave Module Customization

```python
# Template: _03c_waves_jonswap.yml
WaveHs: 2                    # Default value
WaveTz: 6

# After customization with hs_max=2.5, tp_max=8.0
WaveHs: 2.5                  # From human_input
WaveTz: 6.4                  # Calculated from Tp (Tz = 0.8 * Tp)
```

---

## Validation Framework

### Three-Level Validation

#### Level 1: YAML Syntax ✅
- File exists and readable
- Valid YAML structure
- All includefiles resolved
- No syntax errors

#### Level 2: OrcaFlex API (Optional)
- OrcaFlex can load file
- Static analysis converges
- No OrcaFlex errors/warnings

#### Level 3: Physical Consistency ✅
- Buoy geometry within ranges
- Metocean conditions realistic
- Mooring capacity adequate
- Safety factors met

### Validation Reports

Generated in 4 formats:

1. **Console:** Real-time colored output
2. **CSV:** Structured data for spreadsheet analysis
3. **Markdown:** Human-readable text format
4. **HTML:** Interactive dashboard with collapsible sections

---

## Multi-Fidelity Analysis

### Analysis Levels

```yaml
multi_fidelity:
  enabled: true

  analyses:
    # Quick screening (5-10 minutes)
    - name: "preliminary_screening"
      fidelity: "low"
      orcaflex_template: "calm_buoy_base.yml"
      simulation_duration: 1800
      mesh_density: "coarse"

    # Detailed design (30-60 minutes)
    - name: "detailed_design"
      fidelity: "high"
      orcaflex_template: "discretised_calm_buoy_base.yml"
      simulation_duration: 3600
      mesh_density: "fine"

    # Sensitivity study (2-4 hours)
    - name: "sensitivity_study"
      parameter_variations:
        - parameter: "wave_hs"
          values: [1.5, 2.0, 2.5, 3.0]
```

---

## Best Practices

### 1. Project Setup

✅ **DO:**
- Copy template to projects/ directory
- Use meaningful project codes (e.g., NSE_CALM_001)
- Fill in all required fields
- Review AI-generated recommendations

❌ **DON'T:**
- Modify template files directly
- Use spaces in project codes
- Skip validation steps

### 2. Parameter Selection

✅ **DO:**
- Reference industry standards (API RP 2SK, DNVGL, ISO)
- Use conservative design values
- Include safety margins
- Document assumptions

❌ **DON'T:**
- Use unrealistic metocean conditions
- Underestimate safety factors
- Ignore validation warnings

### 3. Mooring Design

✅ **DO:**
- Maintain safety factor > 1.67 (intact)
- Design for 100-year return period
- Consider marine growth
- Account for line degradation

❌ **DON'T:**
- Specify SF < 1.25 (damaged condition)
- Mix chain grades without justification
- Ignore seabed soil conditions

### 4. Validation

✅ **DO:**
- Review all validation reports
- Address critical issues before analysis
- Document deviation from standards
- Keep validation history

❌ **DON'T:**
- Proceed with failed validation
- Ignore physical consistency warnings
- Skip documentation

---

## Troubleshooting

### Common Issues

#### 1. Validation Fails: "Water depth exceeds typical range"

**Cause:** Water depth > 150m is outside typical CALM buoy range

**Solution:**
- Verify water depth is correct
- Consider alternative mooring (e.g., turret mooring)
- Add justification in project description
- Proceed with caution

#### 2. Validation Fails: "Safety factor below minimum"

**Cause:** Safety factor < 1.67 (intact) or < 1.25 (damaged)

**Solution:**
- Increase chain diameter
- Add more mooring lines
- Reduce pretension
- Review design loads

#### 3. Generation Error: "Module file not found"

**Cause:** Missing template files in `specs/modules/orcaflex/modular-input-file/output/`

**Solution:**
```bash
# Verify template files exist
ls specs/modules/orcaflex/modular-input-file/output/_*.yml

# If missing, restore from repository
git checkout specs/modules/orcaflex/modular-input-file/output/
```

#### 4. OrcaFlex Load Error: "Includefile not found"

**Cause:** Relative paths not set correctly in base file

**Solution:**
- Ensure base file is in `orcaflex/` directory
- Module files must be in `orcaflex/modules/` directory
- Check includefile paths: `includefile: modules/_01a_units_analysis.yml`

#### 5. Validation Warning: "Parameter outside generic range"

**Cause:** Project-specific parameter differs from industry average

**Solution:**
- Review warning context
- Verify parameter is appropriate for project
- Document justification
- Proceed if technically sound

---

## Advanced Topics

### Custom Templates

Create custom templates for specific applications:

```bash
# Create custom template
cp templates/calm_buoy/project_template.yml \
   templates/calm_buoy/arctic_calm_template.yml

# Add Arctic-specific parameters
# - Ice loads
# - Cold temperature effects
# - Extended marine growth
```

### Batch Processing

Generate multiple projects from parameter matrix:

```python
# batch_generate.py
import yaml
from pathlib import Path

# Define parameter variations
water_depths = [80, 100, 120, 140]
chain_sizes = [70, 76, 84]

for depth in water_depths:
    for chain in chain_sizes:
        # Load template
        config = yaml.safe_load(open('template.yml'))

        # Customize
        config['human_input']['site']['water_depth'] = depth
        config['human_input']['mooring']['line_segments'][0]['nominal_diameter'] = chain

        # Generate
        # ... (call generation script)
```

### CI/CD Integration

Automate validation in continuous integration:

```yaml
# .github/workflows/validate-calm-projects.yml
name: Validate CALM Projects

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Validate all projects
        run: |
          python scripts/run_validation.py --projects projects/**/project_config.yml
```

---

## Future Enhancements (Work in Progress)

### FreeCAD Integration
- Auto-generate 3D buoy geometry
- Create mooring layout drawings
- Export DXF for fabrication

### Blender Visualization
- Render high-quality visualizations
- Animate mooring system response
- Generate marketing materials

### Structural Analysis
- Integrate with FEA tools
- Perform fatigue analysis
- Generate structural drawings

### Design Documentation
- Auto-generate design reports
- Create calculation packages
- Export to PDF/Word formats

---

## References

### Standards & Guidelines

- **ISO 19901-7:2013** - Petroleum and natural gas industries - Station-keeping systems for floating offshore structures
- **API RP 2SK:2005** - Design and Analysis of Stationkeeping Systems for Floating Structures
- **DNVGL-OS-E403:2021** - Offshore mooring chain and accessories
- **OCIMF SMOG** - Single Point Mooring Operations Guide (4th Edition)
- **OCIMF MEG4** - Mooring Equipment Guidelines

### Software

- **OrcaFlex** - Orcina Ltd. (https://www.orcina.com)
- **FreeCAD** - Open-source CAD (https://www.freecadweb.org)
- **Blender** - Open-source 3D creation suite (https://www.blender.org)

---

**Document Version:** 1.0
**Last Updated:** 2025-01-15
**Author:** Digital Model Team
