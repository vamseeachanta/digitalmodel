# Steel Catenary Riser (SCR) Template

**ABOUTME**: Template for generating Steel Catenary Riser (SCR) OrcaFlex models with configurable
             vessel, riser, and environment components from lookup tables.

---

## Overview

This template generates OrcaFlex models for Steel Catenary Riser (SCR) analysis using the component assembly approach. Configure your model by selecting pre-validated components from lookup tables.

### Key Features

- ✅ **Component-Based**: Select vessel, riser, and environment from libraries
- ✅ **Validated**: All components meet engineering standards (DNV, API)
- ✅ **Flexible**: Override any parameter as needed
- ✅ **Quick**: Generate complete models in seconds
- ✅ **Integrated**: Works with converter, runner, post-processor

---

## Quick Start

### 1. Basic Configuration

```yaml
# my_scr_config.yml
model:
  type: "scr_catenary"
  name: "GoM_SCR_Analysis_001"

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

### 2. Generate Model

```python
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator

generator = OrcaFlexModelGenerator()

model = generator.generate_from_template(
    template="risers/scr_catenary",
    config="my_scr_config.yml",
    output="my_scr_model.yml"
)
```

### 3. Run Analysis

```python
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

runner = UniversalOrcaFlexRunner()
runner.run_single("my_scr_model.yml")
```

---

## Available Components

### Vessels (components/vessels/)

| Vessel ID | Type | LOA (m) | Displacement (t) | Description |
|-----------|------|---------|------------------|-------------|
| `FPSO_P50` | FPSO | 300 | 200,000 | Standard FPSO deepwater |
| `FPSO_P70` | FPSO | 320 | 250,000 | Large FPSO ultra-deepwater |
| `FPSO_P30` | FPSO | 270 | 150,000 | Compact FPSO shallow water |
| `FPSO_SPREAD` | FPSO | 310 | 220,000 | FPSO with spread mooring |
| `FPSO_TURRET` | FPSO | 305 | 210,000 | FPSO with turret mooring |

### Risers (components/lines/risers.csv)

| Riser ID | OD (m) | Material | Max Tension (kN) | Description |
|----------|--------|----------|------------------|-------------|
| `SCR_10inch_X65` | 0.2731 | X65 Steel | 2,500 | Standard 10-inch SCR |
| `SCR_12inch_X65` | 0.3239 | X65 Steel | 3,500 | 12-inch SCR |
| `SCR_10inch_X70` | 0.2731 | X70 Steel | 2,700 | High strength 10-inch |

### Environment (components/environment/)

| Environment ID | Hs (m) | Tp (s) | Current (m/s) | Return Period | Description |
|----------------|--------|--------|---------------|---------------|-------------|
| `GoM_100yr` | 14.5 | 15.0 | 1.5 | 100-year | GoM hurricane extreme |
| `GoM_10yr` | 10.2 | 12.5 | 1.2 | 10-year | GoM 10-year storm |
| `GoM_1yr` | 6.5 | 9.5 | 0.8 | 1-year | GoM operating condition |

---

## Configuration Parameters

### Model Section

```yaml
model:
  type: "scr_catenary"              # Template type
  name: "Project_SCR_001"           # Model name
  description: "Optional description"
```

### Vessel Section

```yaml
vessel:
  lookup: "FPSO_P50"                # Vessel ID from lookup table
  position: {x: 0, y: 0, z: 0}     # Vessel position
  heading: 180                      # Vessel heading (degrees)
  # Any vessel property can be overridden:
  LOA: 305                          # Override length
  Displacement: 210000              # Override displacement
```

### Riser Section

```yaml
riser:
  lookup: "SCR_10inch_X65"          # Riser ID from lookup table
  length: 1500                      # Total riser length (m)
  segments: 150                     # Number of segments
  top_tension: 500                  # Top tension (kN)
  # Override any riser property:
  OD: 0.275                         # Override outer diameter
  WallThickness: 0.014              # Override wall thickness
```

### Environment Section

```yaml
environment:
  lookup: "GoM_100yr"               # Environment ID from lookup table
  water_depth: 1200                 # Water depth (m)
  # Override any environment property:
  Hs: 15.0                          # Override significant wave height
  Tp: 16.0                          # Override peak period
  SurfaceCurrent: 1.6               # Override current
```

### Analysis Section

```yaml
analysis:
  type: "dynamic"                   # "static" or "dynamic"
  duration: 10800                   # Duration (seconds) for dynamic
  time_step: 0.1                    # Time step (seconds)
  target_log_sample_interval: 1.0   # Logging interval
```

---

## Examples

### Example 1: Basic SCR Analysis

```yaml
model:
  name: "Basic_SCR"

vessel:
  lookup: "FPSO_P50"

riser:
  lookup: "SCR_10inch_X65"
  length: 1200

environment:
  lookup: "GoM_1yr"
  water_depth: 1000

analysis:
  type: "static"
```

### Example 2: Extreme Condition Analysis

```yaml
model:
  name: "Extreme_SCR_100yr"

vessel:
  lookup: "FPSO_P70"
  heading: 180

riser:
  lookup: "SCR_12inch_X65"
  length: 1800
  segments: 180

environment:
  lookup: "GoM_100yr"
  water_depth: 1500

analysis:
  type: "dynamic"
  duration: 10800
  time_step: 0.1
```

### Example 3: Parametric Study

```python
# Generate multiple models with varying water depths
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator

generator = OrcaFlexModelGenerator()

base_config = {
    'model': {'name': 'SCR_Parametric'},
    'vessel': {'lookup': 'FPSO_P50'},
    'riser': {'lookup': 'SCR_10inch_X65', 'length': 1500},
    'environment': {'lookup': 'GoM_10yr'},
    'analysis': {'type': 'static'}
}

for depth in range(800, 1600, 100):
    config = base_config.copy()
    config['environment']['water_depth'] = depth
    config['model']['name'] = f"SCR_Depth_{depth}m"

    generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=f"models/scr_depth_{depth}.yml"
    )
```

---

## Validation

All generated models are validated for:

### Structural Checks
- ✅ Required vessel properties present
- ✅ Required riser properties present
- ✅ Required environment properties present

### Engineering Checks
- ✅ Wall thickness meets minimum requirements
- ✅ Tension within material limits
- ✅ Bend radius within allowable limits
- ✅ Water depth appropriate for vessel type

### Standard Compliance
- ✅ DNV-OS-F201 (Dynamic Risers)
- ✅ API RP 2RD (Design of Risers)
- ✅ API RP 2SK (Design and Analysis of Stationkeeping Systems)

---

## Integration

### With OrcaFlex Converter

```python
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator
from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced

# Generate model
generator = OrcaFlexModelGenerator()
model = generator.generate_from_template(
    template="risers/scr_catenary",
    config="config.yml",
    output="model.yml"
)

# Convert to .dat for OrcaFlex
converter = OrcaFlexConverterEnhanced(output_format='dat')
converter.convert_file("model.yml", "model.dat")
```

### With Universal Runner

```python
from digitalmodel.orcaflex.model_generator import generate_model
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

# Generate and run
model_file = generate_model("risers/scr_catenary", "config.yml", "model.yml")

runner = UniversalOrcaFlexRunner()
results = runner.run_single(model_file)
```

### With Post-Processor

```python
from digitalmodel.orcaflex.model_generator import generate_model
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner
from digitalmodel.orcaflex.opp import OPP

# Full workflow
model = generate_model("risers/scr_catenary", "config.yml", "model.yml")
sim_file = UniversalOrcaFlexRunner().run_single(model)
results = OPP().process_single_file(sim_file)
print(f"Results saved to: {results}")
```

---

## Use Cases

### 1. Quick Feasibility Study
Generate standard SCR model in minutes for initial assessment.

### 2. Design Optimization
Run parametric studies varying vessel, riser, or environment parameters.

### 3. Extreme Condition Analysis
Quickly test multiple 100-year scenarios.

### 4. Training and Learning
Use validated templates to learn OrcaFlex modeling best practices.

### 5. Project Kickstart
Start new projects with proven configurations.

---

## Troubleshooting

### Component Not Found

```
ComponentNotFoundError: Component 'FPSO_XYZ' not found in category 'vessels'
```

**Solution**: Check available components:
```python
generator = OrcaFlexModelGenerator()
print(generator.list_components("vessels"))
```

### Validation Warnings

Validation may return warnings for:
- Missing recommended properties
- Unusual parameter values
- Non-standard configurations

Warnings don't prevent model generation but should be reviewed.

### Custom Components

Add your own components to the library:

```python
generator.add_component(
    category="vessels",
    component_id="My_Custom_FPSO",
    properties={
        "VesselName": "Custom FPSO",
        "LOA": 315.0,
        "Breadth": 63.0,
        "Displacement": 225000.0,
        # ... all required properties
    }
)
```

---

## Related Documentation

- [Model Generator API](../../../src/digitalmodel/modules/orcaflex/model_generator/README.md)
- [Component Library Reference](../../components/README.md)
- [OrcaFlex Converter](../../../.claude/skills/orcaflex-file-conversion/SKILL.md)
- [Universal Runner](../../../.claude/skills/orcaflex-modeling/SKILL.md)

---

**Template Version**: 1.0.0
**Last Updated**: 2026-01-03
**Status**: ✅ Production Ready
