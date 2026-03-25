# OrcaFlex Hybrid Template System - Usage Guide

**Version**: 2.0.0 | **Status**: Production Ready | **Last Updated**: 2026-01-19

This guide provides comprehensive documentation for using the OrcaFlex hybrid template system, which enables modular, reusable, and parametric OrcaFlex model management.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Quick Start](#2-quick-start)
3. [Template Structure](#3-template-structure)
4. [Using Templates](#4-using-templates)
5. [Available Templates](#5-available-templates)
6. [Library Components](#6-library-components)
7. [Creating Custom Templates](#7-creating-custom-templates)
8. [Best Practices](#8-best-practices)
9. [Troubleshooting](#9-troubleshooting)

---

## 1. Overview

### What Are Hybrid Templates?

Hybrid templates are a structured approach to OrcaFlex model management that combines two powerful OrcaFlex features:

1. **Object-level IncludeFile**: Include reusable library components (LineTypes, BuoyTypes) directly into model definitions
2. **BaseFile + IncludeFile**: Combine base models with variation files for parametric studies

This approach separates:
- **Equipment definitions** (line types, buoy types) into reusable library files
- **Model geometry** (vessels, lines, connections) into base model files
- **Parameter variations** (water depth, pipe sizes) into dedicated variation files

### Benefits of the Template Approach

| Benefit | Description |
|---------|-------------|
| **Reusability** | Library components shared across multiple models |
| **Consistency** | Standard equipment properties used everywhere |
| **Parametric Studies** | Easy water depth, size, and configuration variations |
| **Version Control** | Track changes to specific components |
| **Maintainability** | Smaller, focused files easier to review |
| **Validation** | All templates tested with automated pytest suite |

### Directory Structure Explanation

```
docs/domains/orcaflex/
├── templates/                        # All hybrid templates
│   ├── README.md                     # Template overview
│   ├── USAGE_GUIDE.md               # This file
│   ├── mooring_systems/             # Mooring templates
│   │   ├── calm_buoy_hybrid/
│   │   ├── spread_mooring_hybrid/
│   │   ├── turret_mooring_hybrid/
│   │   └── salm_hybrid/
│   ├── risers/                      # Riser templates
│   │   ├── scr_hybrid/
│   │   ├── lazy_wave_hybrid/
│   │   ├── pliant_wave_hybrid/
│   │   ├── steep_wave_hybrid/
│   │   ├── ttr_hybrid/
│   │   └── flexible_riser_hybrid/
│   ├── pipelines/                   # Pipeline templates
│   │   └── pipeline_hybrid/
│   ├── umbilicals/                  # Umbilical templates
│   │   └── umbilical_hybrid/
│   └── subsea/                      # Subsea structure templates
│       └── jumper_hybrid/
└── library/                         # Reusable components
    ├── index.yml                    # Component index
    ├── line_types/                  # LineType definitions
    └── buoy_types/                  # BuoyType definitions
```

---

## 2. Quick Start

### How to Use a Template for the First Time

**Step 1**: Load the base model directly

```python
import OrcFxAPI

# Load a base model
model = OrcFxAPI.Model('docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid/base/calm_buoy_base.yml')

# Run static analysis
model.CalculateStatics()

# Check results
buoy = model['CALM_Buoy']
print(f"Buoy position: {buoy.TimeHistory('X', OrcFxAPI.oeStaticState)}")
```

**Step 2**: Use a pre-built case file for variations

```python
import OrcFxAPI

# Load a case (base + variation combined)
model = OrcFxAPI.Model('docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid/cases/case_deep_water.yml')

# Run analysis
model.CalculateStatics()
```

### Step-by-Step Example with CALM Buoy Template

**Example 1: Basic Static Analysis**

```python
import OrcFxAPI

# 1. Load the base CALM buoy model (100m water depth)
model = OrcFxAPI.Model('docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid/base/calm_buoy_base.yml')

# 2. Run statics
model.CalculateStatics()

# 3. Extract mooring line tensions
for i in range(1, 7):
    line = model[f'Mooring_{i}']
    tension = line.StaticResult('Effective tension', OrcFxAPI.oeEndA)
    print(f'Mooring_{i} tension: {tension:.1f} kN')

# 4. Save results
model.SaveSimulation('calm_buoy_100m_static.sim')
```

**Example 2: Deep Water Variation (200m)**

```python
import OrcFxAPI

# Load the deep water case (combines base + variation)
model = OrcFxAPI.Model('docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid/cases/case_deep_water.yml')

# Run analysis
model.CalculateStatics()

# Compare tensions to shallow water case
for i in range(1, 7):
    line = model[f'Mooring_{i}']
    tension = line.StaticResult('Effective tension', OrcFxAPI.oeEndA)
    print(f'Mooring_{i} tension (200m): {tension:.1f} kN')
```

**Example 3: Custom Modification**

```python
import OrcFxAPI

# Load base model
model = OrcFxAPI.Model('docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid/base/calm_buoy_base.yml')

# Modify parameters programmatically
env = model.environment
env.WaterDepth = 150  # Custom water depth

# Adjust line lengths accordingly
for i in range(1, 7):
    line = model[f'Mooring_{i}']
    line.Length[0] = 500  # Adjust chain length

# Run analysis
model.CalculateStatics()
```

---

## 3. Template Structure

Each hybrid template follows a consistent directory structure:

```
template_name_hybrid/
├── README.md                  # Template documentation
├── base/
│   └── *_base.yml            # Full model with library includes
├── variations/
│   └── *.yml                 # Override files (water depth, type, etc.)
└── cases/
    └── case_*.yml            # BaseFile + IncludeFile combinations
```

### base/ - Base Model Files

Base models are complete OrcaFlex models that:
- Include all required objects (Environment, LineTypes, Lines, Buoys, etc.)
- Reference library components using object-level IncludeFile
- Serve as the foundation for parametric variations

**Example: Base Model Structure**

```yaml
%YAML 1.1
# CALM Buoy Base Model - Hybrid Template
---
General:
  UnitsSystem: SI
  StageDuration: [10, 100]
  StaticsMinDamping: 1.5

Environment:
  WaterDepth: 100
  WaterSurfaceZ: 0
  Density: 1.025
  # ... wave, current, wind settings

LineTypes:
  - Name: Chain_84mm
    Category: General
    IncludeFile: ../../../../library/line_types/chain_84mm_r4.yml

6DBuoys:
  - Name: CALM_Buoy
    IncludeFile: ../../../../library/buoy_types/calm_12m_100m.yml
    InitialPosition: [0, 0, -0.8]

Lines:
  - Name: Mooring_1
    EndAConnection: CALM_Buoy
    EndBConnection: Anchored
    LineType, Length, TargetSegmentLength:
      - [Chain_84mm, 350, 10]
```

### variations/ - Variation Files

Variation files contain **complete object definitions** that override base model objects:

**Critical Rule**: OrcaFlex **replaces** objects by name - it does NOT merge properties. Variation files must include ALL properties for any object being modified.

**Example: Deep Water Variation**

```yaml
# Variation: Deep Water 200m
# IMPORTANT: Must include ALL Line properties when overriding
Environment:
  WaterDepth: 200
  CurrentDepth, CurrentFactor, CurrentRotation:
    - [0, 1.0, 0]
    - [100, 0.5, 0]
    - [200, 0.2, 0]

Lines:
  # Must repeat ALL properties, not just changed ones
  - Name: Mooring_1
    EndAConnection: CALM_Buoy
    EndAX: 6.0
    EndAY: 0
    EndAZ: -2.25
    EndBConnection: Anchored
    EndBX: 600           # Changed
    EndBY: 0
    EndBZ: -200          # Changed
    IncludedInStatics: Yes
    LineType, Length, TargetSegmentLength:
      - [Chain_84mm, 700, 10]  # Changed
```

### cases/ - Pre-built Combinations

Case files combine base models with variations using BaseFile + IncludeFile:

**Example: Case File**

```yaml
%YAML 1.1
# Case: CALM Buoy in Deep Water (200m)
---
BaseFile: ../base/calm_buoy_base.yml
IncludeFile: ../variations/deep_water_200m.yml
```

### Library Components

Library files contain **properties only** (no section headers):

**Example: Library LineType**

```yaml
# Chain 84mm R4 Studless - Library Component
# NOTE: No section header - properties only for IncludeFile
OD: 0.084
ID: 0
MassPerUnitLength: 0.088
CompressionIsLimited: Yes
EA: 612000
EI: [0, ~]
GJ: 0
PoissonRatio: 0.5
Cd: [1.0, ~, 0.4]
Ca: [1, ~, 0.07]
```

---

## 4. Using Templates

### Loading a Base Model in OrcaFlex

**Method 1: Python API**

```python
import OrcFxAPI

# Absolute path
model = OrcFxAPI.Model('D:/workspace/templates/calm_buoy_hybrid/base/calm_buoy_base.yml')

# Relative path (from current working directory)
model = OrcFxAPI.Model('templates/calm_buoy_hybrid/base/calm_buoy_base.yml')
```

**Method 2: OrcaFlex GUI**

1. File > Open Data File
2. Navigate to template `base/` directory
3. Select the `*_base.yml` file
4. OrcaFlex resolves IncludeFile paths relative to the YAML file

### Applying Variations Using IncludeFile

**Option A: Use Pre-built Case Files**

```python
# Load case directly - simplest approach
model = OrcFxAPI.Model('cases/case_deep_water.yml')
```

**Option B: Load Base and Apply Variation Programmatically**

```python
import OrcFxAPI

# Load base model
model = OrcFxAPI.Model('base/calm_buoy_base.yml')

# Apply variation (loads and merges)
# Note: This requires manual property updates
env = model.environment
env.WaterDepth = 200

# Update lines for new depth
for line in model.Objects(OrcFxAPI.otLine):
    # Adjust as needed
    pass
```

### Creating Custom Case Files

Create a new YAML file in the `cases/` directory:

```yaml
%YAML 1.1
# Case: Custom Configuration
# Description: 150m water depth with modified wave height
---
BaseFile: ../base/calm_buoy_base.yml
IncludeFile: ../variations/custom_150m.yml
```

Then create the corresponding variation file:

```yaml
# variations/custom_150m.yml
Environment:
  WaterDepth: 150
  WaveTrains:
    - Name: Wave1
      WaveType: Dean stream
      WaveDirection: 180
      WaveHeight: 5.0  # Increased from 3.5m
      WavePeriod: 14.0  # Increased from 12s

# Include complete Line definitions for all modified lines...
```

### Using Library Components

**In Base Models**

```yaml
LineTypes:
  - Name: MyChain
    Category: General
    IncludeFile: ../../../../library/line_types/chain_84mm_r4.yml

  - Name: MyRiser
    Category: General
    IncludeFile: ../../../../library/line_types/scr_10inch_x65.yml

6DBuoys:
  - Name: MyBuoy
    IncludeFile: ../../../../library/buoy_types/calm_12m_100m.yml
    InitialPosition: [0, 0, -1.0]
```

**Important**: The `Category: General` line must appear BEFORE `IncludeFile` for LineTypes.

---

## 5. Available Templates

### Mooring Systems (4 templates)

| Template | Location | Water Depth | Description | Variations |
|----------|----------|-------------|-------------|------------|
| **CALM Buoy** | `mooring_systems/calm_buoy_hybrid/` | 100m | 6-leg CALM buoy mooring | 200m deep water |
| **Spread Mooring** | `mooring_systems/spread_mooring_hybrid/` | 200m | 8-leg spread mooring for FPSO | 500m deep, 12-leg |
| **Turret Mooring** | `mooring_systems/turret_mooring_hybrid/` | 300m | Internal turret with weathervaning | 600m deep, external turret |
| **SALM** | `mooring_systems/salm_hybrid/` | 50m | Single Anchor Leg Mooring | 100m deep, wire rope |

### Risers (6 templates)

| Template | Location | Water Depth | Description | Variations |
|----------|----------|-------------|-------------|------------|
| **SCR** | `risers/scr_hybrid/` | 1200m | Steel Catenary Riser, 10" X65 | 1500m deep, 12" pipe |
| **Lazy Wave** | `risers/lazy_wave_hybrid/` | 1000m | Lazy Wave with buoyancy section | 1500m deep, extended buoyancy |
| **Pliant Wave** | `risers/pliant_wave_hybrid/` | 800m | Pliant Wave configuration | 1200m deep water |
| **Steep Wave** | `risers/steep_wave_hybrid/` | 1000m | Steep wave, short footprint | 1500m deep water |
| **TTR** | `risers/ttr_hybrid/` | 1500m | Top Tensioned Riser | 2000m deep, 12" pipe |
| **Flexible Riser** | `risers/flexible_riser_hybrid/` | 1000m | 8" flexible lazy wave | 1500m deep, 12" large bore |

### Pipelines (1 template)

| Template | Location | Water Depth | Description | Variations |
|----------|----------|-------------|-------------|------------|
| **Pipeline** | `pipelines/pipeline_hybrid/` | 500m | Seabed pipeline, 16" X65 | 1000m deep, 20" trunk, 12" flowline |

### Umbilicals (1 template)

| Template | Location | Water Depth | Description | Variations |
|----------|----------|-------------|-------------|------------|
| **Umbilical** | `umbilicals/umbilical_hybrid/` | 800m | Dynamic flexible umbilical | 1200m deep, steel tube |

### Subsea Structures (1 template)

| Template | Location | Water Depth | Description | Variations |
|----------|----------|-------------|-------------|------------|
| **Jumper** | `subsea/jumper_hybrid/` | 500m | Subsea jumper/flying lead | 1500m deep, rigid jumper |

---

## 6. Library Components

### Line Types (43 components)

Located in `library/line_types/`:

**Chains (10 types)**

| File | Description |
|------|-------------|
| `chain_76mm_r4.yml` | 76mm R4 studless chain |
| `chain_84mm_r3.yml` | 84mm R3 studless chain |
| `chain_84mm_r4.yml` | 84mm R4 studless chain |
| `chain_84mm_r5.yml` | 84mm R5 studless chain |
| `chain_96mm_r4.yml` | 96mm R4 studless chain |
| `chain_120mm_r4.yml` | 120mm R4 studless chain |
| `chain_76mm_r4_stud.yml` | 76mm R4 stud-link chain |
| `chain_84mm_r4_stud.yml` | 84mm R4 stud-link chain |
| `chain_96mm_r4_stud.yml` | 96mm R4 stud-link chain |
| `chain_120mm_r4_stud.yml` | 120mm R4 stud-link chain |

**Wire Ropes (3 types)**

| File | Description |
|------|-------------|
| `wire_rope_64mm.yml` | 64mm wire rope (6x36 IWRC) |
| `wire_rope_76mm.yml` | 76mm wire rope (6x36 IWRC) |
| `wire_rope_84mm.yml` | 84mm wire rope (6x36 IWRC) |

**Polyester Ropes (3 types)**

| File | Description |
|------|-------------|
| `polyester_rope_120mm.yml` | 120mm polyester tether |
| `polyester_rope_140mm.yml` | 140mm polyester tether |
| `polyester_rope_160mm.yml` | 160mm polyester tether |

**Steel Risers (6 types)**

| File | Description |
|------|-------------|
| `scr_10inch_x65.yml` | 10" X65 steel catenary riser |
| `scr_10inch_x70.yml` | 10" X70 steel catenary riser |
| `scr_12inch_x65.yml` | 12" X65 steel catenary riser |
| `ttr_9inch_x80.yml` | 9" X80 top tensioned riser |
| `lwr_12inch.yml` | 12" X65 lazy wave riser |
| `lwr_12inch_buoyancy.yml` | 12" lazy wave with buoyancy |
| `pwr_10inch.yml` | 10" pliant wave riser |
| `hybrid_10inch.yml` | 10" hybrid riser |

**Flexible Pipes (5 types)**

| File | Description |
|------|-------------|
| `flex_6inch.yml` | 6" flexible pipe |
| `flex_8inch.yml` | 8" flexible pipe |
| `flex_8inch_buoyancy.yml` | 8" flexible with buoyancy |
| `flex_12inch.yml` | 12" flexible pipe |
| `flex_12inch_buoyancy.yml` | 12" flexible with buoyancy |

**Pipelines (5 types)**

| File | Description |
|------|-------------|
| `pipe_12inch_x65.yml` | 12" X65 flowline |
| `pipe_16inch_x65.yml` | 16" X65 export pipeline |
| `pipe_20inch_x70.yml` | 20" X70 trunk line |
| `pipe_24inch_x70.yml` | 24" X70 trunk line |
| `pipe_10inch_bundle.yml` | 10" pipe-in-pipe bundle |

**Umbilicals (6 types)**

| File | Description |
|------|-------------|
| `umb_flex_dynamic.yml` | Dynamic flexible umbilical |
| `umb_steel_tube.yml` | Steel tube umbilical |
| `umb_static.yml` | Static umbilical |
| `umb_bundle_large.yml` | Large bundle umbilical |
| `umb_hybrid.yml` | Hybrid umbilical |
| `umb_fiber_only.yml` | Fiber optic only umbilical |

**Other (1 type)**

| File | Description |
|------|-------------|
| `hawser_hmpe_80mm.yml` | 80mm HMPE hawser |

### Buoy Types (12 components)

Located in `library/buoy_types/`:

**CALM Buoys (4 types)**

| File | Description |
|------|-------------|
| `calm_10m_100m.yml` | 10m diameter CALM for 100m depth |
| `calm_12m_100m.yml` | 12m diameter CALM for 100m depth |
| `calm_15m_200m.yml` | 15m diameter CALM for 200m depth |
| `calm_18m_300m.yml` | 18m diameter CALM for 300m depth |

**SPM Buoys (2 types)**

| File | Description |
|------|-------------|
| `spm_8m.yml` | 8m single point mooring buoy |
| `spm_10m.yml` | 10m single point mooring buoy |

**Metocean Buoys (2 types)**

| File | Description |
|------|-------------|
| `metocean_3m.yml` | 3m metocean data buoy |
| `metocean_5m.yml` | 5m metocean data buoy |

**Spar Buoys (2 types)**

| File | Description |
|------|-------------|
| `spar_6m.yml` | 6m spar buoy |
| `spar_10m.yml` | 10m spar buoy |

**Utility Buoys (2 types)**

| File | Description |
|------|-------------|
| `navigation_2m.yml` | 2m navigation buoy |
| `pick_up_1m.yml` | 1m pick-up buoy |

---

## 7. Creating Custom Templates

### How to Create a New Base Model

**Step 1**: Create the directory structure

```
my_template_hybrid/
├── README.md
├── base/
│   └── my_model_base.yml
├── variations/
│   └── variation_1.yml
└── cases/
    └── case_variation_1.yml
```

**Step 2**: Create the base model

```yaml
%YAML 1.1
# My Custom Model - Hybrid Template
# Description: [What this model represents]
---
General:
  UnitsSystem: SI
  StageDuration: [10, 100]
  StaticsMinDamping: 1.5

Environment:
  WaterDepth: 500
  # ... other environment settings

LineTypes:
  - Name: MyLineType
    Category: General
    IncludeFile: ../../../../library/line_types/appropriate_type.yml

# Include all other required objects...
```

**Step 3**: Test the base model

```python
import OrcFxAPI

model = OrcFxAPI.Model('base/my_model_base.yml')
model.CalculateStatics()
print("Base model converged successfully")
```

### How to Create Variations

**Step 1**: Identify what parameters will change

Common variation types:
- Water depth changes
- Line type/size changes
- Configuration changes (number of legs, etc.)
- Environmental condition changes

**Step 2**: Create variation file with COMPLETE object definitions

```yaml
# variations/deep_water.yml
# IMPORTANT: Include ALL properties for objects being modified

Environment:
  WaterDepth: 1000
  CurrentDepth, CurrentFactor, CurrentRotation:
    - [0, 1.0, 0]
    - [500, 0.5, 0]
    - [1000, 0.2, 0]

Lines:
  - Name: Line_1
    # ALL properties must be included
    EndAConnection: Vessel
    EndAX: 0
    EndAY: 0
    EndAZ: -20
    EndBConnection: Anchored
    EndBX: 1500
    EndBY: 0
    EndBZ: -1000
    IncludedInStatics: Yes
    LineType, Length, TargetSegmentLength:
      - [MyLineType, 2000, 10]
```

**Step 3**: Create case file

```yaml
%YAML 1.1
# Case: Deep Water Configuration
---
BaseFile: ../base/my_model_base.yml
IncludeFile: ../variations/deep_water.yml
```

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Template directory | `<type>_hybrid` | `scr_hybrid`, `calm_buoy_hybrid` |
| Base model file | `<short_name>_base.yml` | `scr_base.yml`, `calm_buoy_base.yml` |
| Variation file | `<descriptor>.yml` | `deep_water_1500m.yml`, `12_leg.yml` |
| Case file | `case_<descriptor>.yml` | `case_deep_water.yml`, `case_12_leg.yml` |
| Library line type | `<type>_<size>_<grade>.yml` | `chain_84mm_r4.yml`, `scr_10inch_x65.yml` |
| Library buoy type | `<type>_<size>_<depth>.yml` | `calm_12m_100m.yml` |

### Testing Requirements

All new templates must pass:

1. **Base model loading test**
   ```python
   model = OrcFxAPI.Model('base/model_base.yml')
   assert model is not None
   ```

2. **Static convergence test**
   ```python
   model.CalculateStatics()
   # Should complete without errors
   ```

3. **Case file tests** (for each case)
   ```python
   model = OrcFxAPI.Model('cases/case_variation.yml')
   model.CalculateStatics()
   ```

Run the full test suite:
```bash
uv run pytest tests/domains/orcaflex/test_hybrid_templates.py -v
```

---

## 8. Best Practices

### Version Control

1. **Track all template files in Git**
   ```bash
   git add templates/my_template_hybrid/
   git commit -m "feat: Add my_template hybrid template"
   ```

2. **Use meaningful commit messages**
   - `feat: Add 12-leg spread mooring variation`
   - `fix: Correct chain length in deep water case`
   - `docs: Update CALM buoy README with design notes`

3. **Tag validated versions**
   ```bash
   git tag -a v2.0.0 -m "Validated 11 templates with 58 tests"
   ```

### Documentation

Each template README should include:

1. **Overview**: What the template models
2. **Configuration table**: Key parameters and values
3. **Usage examples**: Python code snippets
4. **Design considerations**: Engineering notes
5. **Library components used**: Referenced library files

### Validation

1. **Always run static analysis** before committing new templates
2. **Test all variations and cases** individually
3. **Check library component paths** are correct
4. **Verify line type names** match between base and variations

### Common Patterns

**Pattern 1: Water Depth Scaling**

When scaling for deeper water:
- Increase line lengths proportionally
- Extend anchor radius
- Add current profile depth points
- Consider changing buoy size

**Pattern 2: LineType Changes**

Changing line types requires:
1. New LineType definition (or new library include)
2. Complete Line object redefinition with new type reference
3. Often requires standalone model (cannot use BaseFile approach)

**Pattern 3: Configuration Changes**

Adding/removing lines (e.g., 8-leg to 12-leg):
- Include ALL lines in variation file
- Lines not in variation will use base model values
- Consider creating standalone case for major reconfigurations

---

## 9. Troubleshooting

### Common Issues with IncludeFile

**Issue 1: "File not found" error**

```
Error: Cannot find file 'library/line_types/chain_84mm_r4.yml'
```

**Cause**: Relative path is incorrect.

**Solution**: IncludeFile paths are relative to the YAML file containing them. Count directory levels carefully:

```yaml
# If your model is in: templates/mooring/calm_buoy_hybrid/base/
# And library is in:    library/line_types/
# Path should be:       ../../../../library/line_types/chain_84mm_r4.yml
```

**Issue 2: "Invalid YAML" error in library file**

**Cause**: Library file has section headers.

**Solution**: Library files must contain properties only:

```yaml
# WRONG - has section header
LineTypes:
  - OD: 0.084

# CORRECT - properties only
OD: 0.084
ID: 0
MassPerUnitLength: 0.088
```

**Issue 3: "Object not found" when using variation**

**Cause**: Variation references an object that does not exist in base.

**Solution**: Ensure variation objects have the same Name as base model objects:

```yaml
# Base has: Name: Mooring_1
# Variation must also have: Name: Mooring_1
```

### Static Convergence Problems

**Issue 1: "Static analysis failed to converge"**

**Possible causes and solutions**:

1. **Line too short**: Increase line length
2. **Insufficient catenary**: Increase anchor radius
3. **Conflicting connections**: Check EndA/EndB positions
4. **Low damping**: Increase `StaticsMinDamping`

```yaml
General:
  StaticsMinDamping: 2.0  # Increase from default 1.5
```

5. **Wrong initial position**: Set realistic starting point

**Issue 2: "Line has zero length" warning**

**Cause**: `LineType, Length, TargetSegmentLength` format issue.

**Solution**: Use multi-column format:

```yaml
# CORRECT
LineType, Length, TargetSegmentLength:
  - [Chain_84mm, 350, 10]

# WRONG - indexed format
LineType: Chain_84mm
Length[1]: 350
```

**Issue 3: "Negative tension" warnings**

**Cause**: Line is in compression (common with flexible pipes).

**Solution**: Ensure `CompressionIsLimited: Yes` for flexible lines, and check geometry.

### LineType Replacement Limitations

**Issue**: Cannot change LineType name in variation

**Explanation**: When you change a LineType name (e.g., from 8-inch to 12-inch), the Line object still references the old name. OrcaFlex replaces entire objects, not individual properties.

**Solution**: Use standalone models for LineType name changes:

```yaml
# Instead of BaseFile + IncludeFile, create a complete model
# cases/case_12inch.yml - Complete standalone model
General:
  # ... all settings

LineTypes:
  - Name: Flex_12inch  # New name
    Category: General
    IncludeFile: ../../../../library/line_types/flex_12inch.yml

Lines:
  - Name: Riser_1
    LineType, Length, TargetSegmentLength:
      - [Flex_12inch, 400, 10]  # References new name
```

### Path Resolution Tips

1. **Always use forward slashes** (`/`) in paths, even on Windows
2. **Use relative paths** from the YAML file location
3. **Test paths** by loading in OrcaFlex GUI first
4. **Keep library at consistent depth** relative to templates

### Getting Help

1. Check the template README for specific guidance
2. Review similar working templates for patterns
3. Run the test suite to identify issues:
   ```bash
   uv run pytest tests/domains/orcaflex/test_hybrid_templates.py -v --tb=short
   ```
4. Check OrcaFlex documentation for YAML format details

---

## Related Documentation

- [Template Overview](README.md) - Quick reference for all templates
- [Library Index](../library/index.yml) - All library components
- [OrcaFlex YAML Reference](https://www.orcina.com/resources/documentation/) - Official OrcaFlex docs
- [Test Suite](../../../../tests/domains/orcaflex/test_hybrid_templates.py) - Automated validation tests

---

**Version**: 2.0.0
**Last Updated**: 2026-01-19
**Validated Templates**: 13
**Library Components**: 55 (43 line types + 12 buoy types)
**Automated Tests**: 58
