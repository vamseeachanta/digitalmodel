# OrcaFlex Model Generator - Quick Start Guide

**ABOUTME**: Get started with OrcaFlex model generation using component assembly in 5 minutes.

---

## What is Model Generator?

Instead of manually creating OrcaFlex model files, the Model Generator lets you:

1. **Select components** from pre-validated libraries (vessels, risers, materials, environments)
2. **Configure with YAML** - simple, readable configuration files
3. **Generate models** automatically - complete, validated OrcaFlex models

### Why Use It?

- ⚡ **Fast**: Generate models in seconds, not hours
- ✅ **Reliable**: All components validated against engineering standards
- 🔁 **Reusable**: Build once, use many times
- 📊 **Parametric**: Easy to generate 100s of models for studies
- 🎯 **Simple**: YAML config instead of complex Python code

---

## Installation

```bash
# Model generator is included in digitalmodel package
cd /path/to/digitalmodel
pip install -e .
```

Or with uv:
```bash
uv pip install -e .
```

---

## 5-Minute Tutorial

### Step 1: List Available Components (30 seconds)

```python
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator

generator = OrcaFlexModelGenerator()

# What vessels are available?
print(generator.list_components("vessels"))
# ['FPSO_P50', 'FPSO_P70', 'FPSO_P30', ...]

# What risers?
print(generator.list_components("lines/risers"))
# ['SCR_10inch_X65', 'SCR_12inch_X65', ...]

# What environments?
print(generator.list_components("environment"))
# ['GoM_100yr', 'GoM_10yr', 'NorthSea_100yr', ...]
```

### Step 2: Create Configuration (1 minute)

Save as `my_first_model.yml`:

```yaml
model:
  name: "My_First_SCR"

vessel:
  lookup: "FPSO_P50"           # Select FPSO from library

riser:
  lookup: "SCR_10inch_X65"     # Select 10-inch SCR
  length: 1200                  # 1200m long
  segments: 120                 # 120 segments

environment:
  lookup: "GoM_1yr"            # Gulf of Mexico 1-year condition
  water_depth: 1000             # 1000m water depth

analysis:
  type: "static"                # Static analysis
```

### Step 3: Generate Model (1 minute)

```python
model = generator.generate_from_template(
    template="risers/scr_catenary",
    config="my_first_model.yml",
    output="my_scr_model.yml"
)

print("✅ Model generated: my_scr_model.yml")
```

### Step 4: Validate (30 seconds)

```python
validation = generator.validate(model)

if validation['is_valid']:
    print("✅ Model is valid and ready to use!")
else:
    print("❌ Errors:", validation['errors'])
```

### Step 5: Run Analysis (2 minutes)

```python
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

runner = UniversalOrcaFlexRunner()
runner.run_single("my_scr_model.yml")

print("✅ Analysis complete!")
```

---

## Common Tasks

### View Component Details

```python
# Get FPSO specifications
vessel = generator.get_component("vessels", "FPSO_P50")
print(f"Length: {vessel['LOA']}m")
print(f"Displacement: {vessel['Displacement']} tonnes")

# Get riser specifications
riser = generator.get_component("lines/risers", "SCR_10inch_X65")
print(f"OD: {riser['OD']}m")
print(f"Material: {riser['Material']}")
```

### Override Component Properties

```yaml
# In configuration file
riser:
  lookup: "SCR_10inch_X65"      # Base component
  length: 1500                   # Custom length
  WallThickness: 0.016           # Override thickness (thicker wall)
```

### Generate Multiple Models

```python
# Parametric study - 10 models with different water depths
for depth in range(800, 1800, 100):
    config = {
        'model': {'name': f'SCR_Depth_{depth}m'},
        'vessel': {'lookup': 'FPSO_P50'},
        'riser': {'lookup': 'SCR_10inch_X65', 'length': depth + 300},
        'environment': {'lookup': 'GoM_10yr', 'water_depth': depth},
        'analysis': {'type': 'static'}
    }

    generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=f"models/scr_depth_{depth}.yml"
    )

print("Generated 10 parametric models!")
```

---

## Available Templates

### Currently Available

- ✅ **risers/scr_catenary** - Steel Catenary Riser

### Coming Soon

- 📋 risers/ttr_top_tensioned - Top-Tensioned Riser
- 📋 risers/lazy_wave - Lazy Wave Riser
- 📋 pipeline_installation/s_lay - S-Lay Pipeline Installation
- 📋 umbilical_installation/dynamic_umbilical - Dynamic Umbilical
- ... and 29 more templates

---

## Available Components

### Vessels (9 total)

| ID | Description | LOA | Displacement |
|----|-------------|-----|--------------|
| FPSO_P50 | Standard FPSO | 300m | 200,000t |
| FPSO_P70 | Large FPSO | 320m | 250,000t |
| DS_DP3_7GEN | 7th Gen Drillship | 228m | 90,000t |
| PLV_SLAY_LARGE | S-Lay Vessel | 185m | 45,000t |

### Risers (8 total)

| ID | OD | Material | Max Tension |
|----|-----|----------|-------------|
| SCR_10inch_X65 | 0.273m | X65 Steel | 2,500 kN |
| SCR_12inch_X65 | 0.324m | X65 Steel | 3,500 kN |
| LWR_12inch | 0.324m | X65 Steel | 3,500 kN |

### Environments (10 total)

| ID | Location | Hs | Tp | Return Period |
|----|----------|-----|-----|---------------|
| GoM_1yr | Gulf of Mexico | 6.5m | 9.5s | 1-year |
| GoM_10yr | Gulf of Mexico | 10.2m | 12.5s | 10-year |
| GoM_100yr | Gulf of Mexico | 14.5m | 15.0s | 100-year |
| NorthSea_100yr | North Sea | 16.8m | 16.5s | 100-year |

---

## Full Workflow Example

Complete workflow from generation to results:

```python
from digitalmodel.orcaflex.model_generator import generate_model
from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

# 1. Generate model from template
print("1. Generating model...")
model = generate_model(
    template="risers/scr_catenary",
    config={
        'model': {'name': 'Complete_Workflow'},
        'vessel': {'lookup': 'FPSO_P50'},
        'riser': {'lookup': 'SCR_10inch_X65', 'length': 1200},
        'environment': {'lookup': 'GoM_1yr', 'water_depth': 1000},
        'analysis': {'type': 'static'}
    },
    output="workflow_model.yml"
)

# 2. Convert to .dat format (if needed)
print("2. Converting to .dat format...")
converter = OrcaFlexConverterEnhanced(output_format='dat')
success, dat_file, error = converter.convert_file("workflow_model.yml")

# 3. Run OrcaFlex analysis
print("3. Running OrcaFlex analysis...")
runner = UniversalOrcaFlexRunner()
sim_file = runner.run_single(dat_file)

# 4. Post-process results
print("4. Post-processing results...")
from digitalmodel.orcaflex.opp import OPP
opp = OPP()
results = opp.process_single_file(sim_file)

print(f"✅ Complete! Results in: {results}")
```

---

## Troubleshooting

### Q: Component not found error?

**A:** List available components to see what's in the library:
```python
print(generator.list_components("vessels"))
```

### Q: Want to add my own components?

**A:** Add custom components to the library:
```python
generator.add_component(
    category="vessels",
    component_id="My_Custom_FPSO",
    properties={
        "VesselID": "My_Custom_FPSO",
        "LOA": 315.0,
        "Breadth": 63.0,
        "Displacement": 225000.0,
        # ... all required properties
    }
)
```

### Q: Model has validation warnings?

**A:** Warnings are informational - model can still be used. Review them:
```python
validation = generator.validate(model)
for warning in validation['warnings']:
    print(warning)
```

---

## Next Steps

1. **Try the examples**: Run `docs/modules/orcaflex/examples/model_generator_examples.py`
2. **Read the docs**: See `src/digitalmodel/modules/orcaflex/model_generator/README.md`
3. **Use the skill**: Try `/orcaflex-model-generator` skill in Claude Code
4. **Explore templates**: Browse `docs/modules/orcaflex/templates/`

---

## Getting Help

- **Documentation**: `docs/modules/orcaflex/templates/README.md`
- **API Reference**: `src/digitalmodel/modules/orcaflex/model_generator/README.md`
- **Examples**: `docs/modules/orcaflex/examples/model_generator_examples.py`
- **Claude Skill**: `.claude/skills/orcaflex-model-generator/SKILL.md`

---

**Version**: 1.0.0
**Status**: ✅ Ready to Use
**Last Updated**: 2026-01-03
