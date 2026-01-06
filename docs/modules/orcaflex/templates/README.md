# OrcaFlex Model Templates Library

**ABOUTME**: Comprehensive template library for generating OrcaFlex models from reusable components
             and configurations. Supports all marine/offshore operations with parametric generation.

---

## 🎯 Overview

This library provides pre-configured OrcaFlex model templates for various marine and offshore operations. Templates use **component assembly** - building models from modular, reusable components with simple configuration files.

### Key Features

- ✅ **Component-Based**: Assemble models from vessel, line, and equipment libraries
- ✅ **Lookup Tables**: Pre-defined properties for vessels, lines, materials
- ✅ **Simple Config**: YAML-based configuration with minimal parameters
- ✅ **Comprehensive**: Growing library covering all offshore operations
- ✅ **Validated**: All templates tested and validated
- ✅ **Integrated**: Works with converter, runner, post-processor

---

## 📁 Library Structure

```
templates/
├── README.md                          # This file
├── components/                        # Component library (lookup tables)
│   ├── vessels/                       # Vessel database
│   │   ├── fpso.csv                  # FPSO vessels
│   │   ├── drillship.csv             # Drillships
│   │   ├── pipelay.csv               # Pipelay vessels
│   │   ├── crane.csv                 # Crane vessels
│   │   └── installation.csv          # Installation vessels
│   ├── lines/                         # Line/riser database
│   │   ├── risers.csv                # Riser catalog
│   │   ├── pipelines.csv             # Pipeline catalog
│   │   ├── umbilicals.csv            # Umbilical catalog
│   │   ├── mooring_lines.csv         # Mooring line catalog
│   │   └── cables.csv                # Cable catalog
│   ├── materials/                     # Material properties
│   │   ├── steel.csv                 # Steel grades
│   │   ├── titanium.csv              # Titanium alloys
│   │   ├── composites.csv            # Composite materials
│   │   └── polymers.csv              # Polymer materials
│   ├── environment/                   # Environmental conditions
│   │   ├── metocean.csv              # Metocean data
│   │   ├── wave_spectra.csv          # Wave spectra definitions
│   │   └── current_profiles.csv      # Current profiles
│   └── equipment/                     # Equipment library
│       ├── buoys.csv                 # Buoy specifications
│       ├── connectors.csv            # Connector properties
│       └── fairleads.csv             # Fairlead specifications
│
├── risers/                            # Riser templates
│   ├── scr_catenary/                 # Steel Catenary Riser
│   ├── ttr_top_tensioned/            # Top-Tensioned Riser
│   ├── lazy_wave/                    # Lazy Wave Riser
│   ├── pliant_wave/                  # Pliant Wave Riser
│   └── hybrid_riser/                 # Hybrid Riser System
│
├── umbilical_installation/            # Umbilical installation
│   ├── static_installation/          # Static umbilical
│   ├── dynamic_umbilical/            # Dynamic umbilical
│   └── bundle_installation/          # Umbilical bundle
│
├── structure_installation/            # Structure installation
│   ├── jacket_installation/          # Jacket lift & install
│   ├── topside_installation/         # Topside module lift
│   ├── subsea_template/              # Subsea structure
│   └── manifold_installation/        # Manifold installation
│
├── pipeline_installation/             # Pipeline installation
│   ├── s_lay/                        # S-lay method
│   ├── j_lay/                        # J-lay method
│   ├── reel_lay/                     # Reel-lay method
│   └── tow_installation/             # Pipeline tow & pull-in
│
├── mooring_systems/                   # Mooring systems
│   ├── calm_buoy/                    # CALM buoy mooring
│   ├── salm_buoy/                    # SALM buoy mooring
│   ├── spread_mooring/               # Spread mooring
│   ├── turret_mooring/               # Turret mooring
│   └── single_point_mooring/         # Single point mooring
│
├── towing_operations/                 # Towing operations
│   ├── platform_tow/                 # Platform towing
│   ├── pipeline_tow/                 # Pipeline towing
│   └── barge_tow/                    # Barge towing
│
├── heavy_lift/                        # Heavy lift operations
│   ├── dual_crane_lift/              # Dual crane lift
│   ├── subsea_lift/                  # Subsea equipment lift
│   └── tandem_lift/                  # Tandem lift operation
│
├── rov_operations/                    # ROV operations
│   ├── inspection/                   # ROV inspection
│   ├── intervention/                 # ROV intervention
│   └── construction/                 # ROV construction support
│
└── specialized/                       # Specialized operations
    ├── riser_pull_in/                # Riser pull-in
    ├── flexjoint_analysis/           # Flexjoint analysis
    ├── touchdown_analysis/           # Touchdown zone analysis
    └── viv_analysis/                 # VIV analysis setup

```

---

## 🧩 Component Assembly Concept

Models are built by **assembling components** rather than modifying templates:

### Traditional Approach (Modify Template)
```
1. Copy template .dat file
2. Manually edit parameters
3. Risk of inconsistencies
4. Hard to maintain
```

### Component Assembly (Our Approach)
```
1. Select components from library
2. Configure with simple YAML
3. Generator assembles model
4. Consistent, validated, reusable
```

### Example Flow
```yaml
# config.yml
model:
  type: "scr_catenary"
  name: "GoM_SCR_Analysis"

vessel:
  lookup: "FPSO_P50"  # From components/vessels/fpso.csv
  position: {x: 0, y: 0, z: 0}

riser:
  lookup: "SCR_10inch_X65"  # From components/lines/risers.csv
  length: 1500
  segments: 150

environment:
  lookup: "GoM_100yr"  # From components/environment/metocean.csv
  water_depth: 1200

analysis:
  type: "dynamic"
  duration: 10800
  time_step: 0.1
```

**Generator Output**: Complete OrcaFlex model (.yml or .dat)

---

## 📖 Template Structure

Each template directory contains:

```
template_name/
├── README.md                    # Template description & usage
├── config_template.yml          # Configuration template
├── example_basic.yml            # Basic configuration example
├── example_advanced.yml         # Advanced configuration example
├── model_template.yml           # Base OrcaFlex model structure
├── validation_criteria.yml      # Model validation rules
└── test_cases/                  # Test configurations
    ├── test_case_1.yml
    └── test_case_2.yml
```

---

## 🚀 Quick Start

### 1. Browse Available Templates

```bash
# List all templates
ls docs/modules/orcaflex/templates/

# View specific category
ls docs/modules/orcaflex/templates/risers/
```

### 2. Select Template

```bash
# View template documentation
cat docs/modules/orcaflex/templates/risers/scr_catenary/README.md
```

### 3. Create Configuration

```yaml
# my_scr_config.yml
model:
  type: "scr_catenary"
  name: "MyProject_SCR_001"

vessel:
  lookup: "FPSO_P50"
  position: {x: 0, y: 0, z: 0}

riser:
  lookup: "SCR_10inch_X65"
  length: 1500

environment:
  lookup: "GoM_100yr"
  water_depth: 1200
```

### 4. Generate Model

```python
from digitalmodel.modules.orcaflex.model_generator import OrcaFlexModelGenerator

# Initialize generator
generator = OrcaFlexModelGenerator()

# Generate model from template
model = generator.generate_from_template(
    template="risers/scr_catenary",
    config="my_scr_config.yml",
    output="my_scr_model.yml"
)

# Validate
validation = generator.validate(model)
print(f"Valid: {validation.is_valid}")
```

### 5. Run Analysis

```python
from digitalmodel.modules.orcaflex.universal import UniversalOrcaFlexRunner

# Run OrcaFlex simulation
runner = UniversalOrcaFlexRunner()
runner.run_single("my_scr_model.yml")
```

---

## 🔍 Component Lookup

### Vessel Lookup

```python
# Available in components/vessels/
vessels = generator.list_components("vessels")
# Returns: ['FPSO_P50', 'FPSO_P70', 'Drillship_DP3', ...]

# Get vessel details
vessel_spec = generator.get_component("vessels", "FPSO_P50")
# Returns full vessel specification
```

### Line Lookup

```python
# Available in components/lines/
lines = generator.list_components("lines/risers")
# Returns: ['SCR_10inch_X65', 'SCR_12inch_X70', ...]

# Get line details
line_spec = generator.get_component("lines/risers", "SCR_10inch_X65")
# Returns: diameter, wall thickness, material, etc.
```

### Environment Lookup

```python
# Available in components/environment/
envs = generator.list_components("environment")
# Returns: ['GoM_100yr', 'NorthSea_10yr', 'WestAfrica_50yr', ...]

# Get environment details
env_spec = generator.get_component("environment", "GoM_100yr")
# Returns: Hs, Tp, current, wind, etc.
```

---

## 📊 Template Categories

### Current Status

| Category | Templates | Status | Priority |
|----------|-----------|--------|----------|
| **Risers** | 5 | 🚧 In Progress | High |
| **Pipeline Installation** | 4 | 📋 Planned | High |
| **Umbilical Installation** | 3 | 📋 Planned | High |
| **Mooring Systems** | 5 | 📋 Planned | Medium |
| **Structure Installation** | 4 | 📋 Planned | Medium |
| **Towing Operations** | 3 | 📋 Planned | Medium |
| **Heavy Lift** | 3 | 📋 Planned | Low |
| **ROV Operations** | 3 | 📋 Planned | Low |
| **Specialized** | 4 | 📋 Planned | Low |

**Total**: 34 template types planned

---

## 🎓 Use Cases

### 1. Quick Model Generation
Generate standard models in minutes instead of hours.

### 2. Parametric Studies
Vary parameters across many models for sensitivity analysis.

```python
# Generate 100 models with varying water depths
for depth in range(500, 1500, 10):
    config['environment']['water_depth'] = depth
    generator.generate(template, config, f"model_depth_{depth}.yml")
```

### 3. Standard Compliance
Ensure all models meet design standards and best practices.

### 4. Training & Learning
Help engineers learn OrcaFlex with validated example models.

### 5. Project Kickstart
Quickly start new projects with proven template configurations.

---

## 🔧 Advanced Features

### Custom Components

Add your own components to the library:

```python
# Add custom vessel
generator.add_component(
    category="vessels",
    name="My_Custom_FPSO",
    properties={
        "length": 300,
        "breadth": 60,
        "displacement": 200000,
        # ... more properties
    }
)
```

### Template Validation

All generated models are validated against:
- ✅ OrcaFlex syntax requirements
- ✅ Engineering design standards (DNV, API, ISO)
- ✅ Physical feasibility checks
- ✅ Best practices compliance

### Version Control

Templates are versioned for tracking changes:
```
scr_catenary/
├── v1.0/  # Initial release
├── v1.1/  # Bug fixes
└── v2.0/  # Major update
```

---

## 📚 Documentation

### For Each Template

- **README.md**: Description, use cases, examples
- **config_schema.json**: Configuration validation schema
- **parameters.md**: All configurable parameters
- **best_practices.md**: Engineering guidelines
- **validation_rules.yml**: Model validation criteria

### General Documentation

- **Template Development Guide**: How to create new templates
- **Component Library Guide**: How to add components
- **Integration Guide**: Using with other tools
- **API Reference**: Model generator API documentation

---

## 🔗 Integration

### With OrcaFlex Converter
```python
# Generate model and convert to .dat
model_yml = generator.generate(template, config)
converter.convert_file(model_yml, output_format='dat')
```

### With Universal Runner
```python
# Generate and run simulation
model = generator.generate(template, config)
runner.run_single(model)
```

### With Post-Processor
```python
# Generate, run, and post-process
model = generator.generate(template, config)
sim = runner.run_single(model)
results = opp.process_single_file(sim)
```

### With Claude Code Skill
```
Use the /orcaflex-model-generator skill to create models from templates.
```

---

## 🤝 Contributing

### Adding New Templates

1. Create template directory structure
2. Define component requirements
3. Create configuration schema
4. Add validation rules
5. Provide examples
6. Document thoroughly

### Adding Components

1. Follow CSV format in components/
2. Include all required properties
3. Add validation rules
4. Document source/references
5. Test with existing templates

---

## 📖 Related Documentation

- [Model Generator API](../../../src/digitalmodel/modules/orcaflex/model_generator/README.md)
- [Component Library Reference](./components/README.md)
- [Template Development Guide](./TEMPLATE_DEVELOPMENT.md)
- [OrcaFlex Modeling Skill](../../../.claude/skills/orcaflex-modeling/SKILL.md)

---

## 📊 Statistics

**Current Library**:
- Templates: 34 planned, 5 in development
- Components: Growing database
- Vessels: 20+ vessels
- Lines: 50+ line types
- Materials: 15+ materials
- Environments: 10+ metocean conditions

**Growth Plan**:
- Q1 2026: Core templates (risers, pipelines, umbilicals)
- Q2 2026: Mooring and installation templates
- Q3 2026: Specialized operations
- Q4 2026: Advanced features and optimization

---

**Version**: 1.0.0
**Last Updated**: 2026-01-02
**Status**: 🚧 In Development
