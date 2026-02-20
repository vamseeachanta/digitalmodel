# Human-Friendly YAML Format for CALM Buoy Projects

## Overview

The CALM Buoy project generator now supports **human-friendly YAML configuration files** with intuitive, readable keys that engineers can understand without programming knowledge.

---

## ✅ Option A Implementation Complete

We've implemented **Option A: Full Human-Friendly** format with flexible parsing that supports:

1. ✅ Natural language keys ("Project Information", "Buoy Specifications")
2. ✅ Automatic conversion between formats
3. ✅ Backwards compatibility with technical keys
4. ✅ Flexible key matching (case-insensitive, flexible spacing)

---

## Key Format Comparison

### ❌ Old Format (Technical Keys)
```yaml
human_input:
  project:
    name: "North Sea CALM Buoy"
    code: "NSE_CALM_001"
  buoy:
    outer_diameter: 12.0
    mass_operating: 9200
  mooring:
    line_segments:
      - nominal_diameter: 76
        mbl: 7300
  offloading:
    tanker_dwt: 300000
    hawser_mbl: 1800
```

**Issues:**
- Hard to read for non-programmers
- No clear indication of units
- Requires knowledge of technical abbreviations
- Not self-documenting

### ✅ New Format (Human-Friendly Keys)
```yaml
Human Input:
  Project Information:
    Name: "North Sea CALM Buoy"
    Code: "NSE_CALM_001"

  Buoy Specifications:
    Outer Diameter: 12.0  # m
    Operating Mass: 9200  # tonnes

  Mooring System:
    Mooring Lines:
      - Diameter: 76  # mm (3 inch R4 chain)
        Minimum Breaking Load: 7300  # kN

  Offloading System:
    Tanker Specifications:
      Deadweight Tonnage: 300000  # tonnes
    Hawser:
      Hawser Minimum Breaking Load: 1800  # kN
```

**Benefits:**
- ✅ Immediately understandable by engineers
- ✅ Units clearly documented in comments
- ✅ Professional terminology
- ✅ Self-documenting structure

---

## Component Sections

The human-friendly format organizes parameters by CALM Buoy components:

### 1. Project Information
```yaml
Project Information:
  Name: "North Sea CALM Buoy - Example Project"
  Code: "NSE_CALM_001"
  Client: "North Sea Energy Partners"
  Engineer: "Dr. Sarah Johnson"
  Version: "1.0"
```

### 2. Standards & Codes
```yaml
Standards & Codes:
  Primary Code: "ISO 19901-7:2013"
  Mooring Standard: "API RP 2SK 2005"
  Buoy Standard: "DNVGL-OS-E403 2021"
  Classification Society: "ABS"
```

### 3. Site Conditions
```yaml
Site Conditions:
  Water Depth: 120  # m
  Seabed Type: "soft clay"

  Metocean Conditions:
    Operating:
      Significant Wave Height (Hs): 2.5  # m
      Peak Period (Tp): 8.0  # s
      Wind Speed: 15  # m/s

    Extreme (100-year):
      Significant Wave Height (Hs): 8.5  # m
      Peak Period (Tp): 14.0  # s
```

### 4. Buoy Specifications
```yaml
Buoy Specifications:
  Type: "turret_calm"

  Geometry:
    Outer Diameter: 12.0  # m
    Draft: 10.0  # m
    Freeboard: 3.0  # m

  Mass Properties:
    Operating Mass: 9200  # tonnes
    Centre of Gravity: [0.0, 0.0, -2.0]  # m

  Material:
    Steel Grade: "S355"
    Design Life: 25  # years
```

### 5. Mooring System
```yaml
Mooring System:
  Configuration:
    Number of Lines: 6
    Azimuth Spacing: 60  # degrees

  Mooring Lines:
    - Segment Name: "Top Chain"
      Type: "studless_chain"
      Grade: "R4"
      Diameter: 76  # mm (3 inch)
      Length: 150  # m
      Minimum Breaking Load: 7300  # kN

  Design Criteria:
    Safety Factor (Intact): 1.8
    Safety Factor (Damaged): 1.3
```

### 6. Offloading System
```yaml
Offloading System:
  Tanker Specifications:
    Type: "VLCC"
    Deadweight Tonnage: 300000  # tonnes
    Length Overall: 330  # m

  Hose System:
    Hose Diameter: 16  # inches
    Hose Length: 120  # m

  Hawser:
    Hawser Type: "nylon_rope"
    Hawser Diameter: 76  # mm
    Hawser Minimum Breaking Load: 1800  # kN
```

### 7. Analysis Settings
```yaml
Analysis Settings:
  Fidelity Levels:
    Run Preliminary: Yes
    Run Detailed: No

  Load Cases:
    - Name: "Operating - Head Seas"
      Condition: "operating"
      Wave Direction: 0  # degrees

  Simulation:
    Simulation Duration: 3600  # seconds
    Time Step: 0.1  # seconds
```

---

## Using Human-Friendly Format

### Creating New Projects

**Option 1: Start from Template**
```bash
# Copy human-friendly template
cp templates/calm_buoy/project_template_human_friendly.yml my_project.yml

# Edit with any text editor
vim my_project.yml

# Generate project
python scripts/generate_calm_buoy_project.py \
  --config my_project.yml \
  --validate
```

**Option 2: Convert Existing File**
```bash
# Convert technical to human-friendly
python scripts/convert_yaml_format.py \
  my_technical_config.yml \
  my_human_config.yml \
  --to human

# Edit human-friendly version
vim my_human_config.yml

# Generate project (works with either format)
python scripts/generate_calm_buoy_project.py \
  --config my_human_config.yml \
  --validate
```

### Editing Configuration

Engineers can edit human-friendly YAML files in any text editor:

```yaml
# Easy to understand and modify
Buoy Specifications:
  Geometry:
    Outer Diameter: 14.0  # m - Changed from 12.0m
    Draft: 11.0  # m - Increased draft

Mooring System:
  Configuration:
    Number of Lines: 8  # Changed from 6 to 8 lines
```

**No programming knowledge required!**

---

## Flexible Key Matching

The system is **very flexible** and accepts multiple formats:

### All These Work:
```yaml
# Exact match
Project Information:
  Name: "Test"

# Case insensitive
project information:
  name: "Test"

# All lowercase, no spaces
projectinformation:
  name: "Test"

# Technical key (backwards compatible)
project_information:
  name: "Test"

# Even short form
project:
  name: "Test"
```

The mapper intelligently converts all formats to internal technical keys.

---

## Key Mapping System

### How It Works

1. **Load YAML**: Read human-friendly or technical format
2. **Auto-Detect**: System detects key format automatically
3. **Normalize**: Convert all keys to internal technical format
4. **Process**: Use normalized keys for calculations
5. **Output**: Generate OrcaFlex files and reports

### Bidirectional Conversion

```python
from yaml_key_mapper import YAMLKeyMapper

mapper = YAMLKeyMapper()

# Human → Technical
tech_key = mapper.to_technical("Project Information")
# Returns: "project"

# Technical → Human
human_key = mapper.to_human("project")
# Returns: "Project Information"

# Full dictionary conversion
human_dict = mapper.convert_dict_to_human(technical_config)
tech_dict = mapper.convert_dict_to_technical(human_config)
```

---

## Available Files

### Templates
- **`templates/calm_buoy/project_template_human_friendly.yml`**
  - Complete template with all parameters
  - Human-friendly keys throughout
  - Extensive inline comments with units
  - Ready to copy and customize

### Examples
- **`examples/modules/calm_buoy/north_sea_calm_project_human.yml`**
  - Real-world North Sea example
  - Human-friendly format
  - Tested and validated

### Tools
- **`scripts/yaml_key_mapper.py`**
  - Flexible key mapping system
  - 200+ key mappings defined
  - Automatic normalization

- **`scripts/convert_yaml_format.py`**
  - Convert between formats
  - Bidirectional conversion
  - Preserves all data

---

## Key Mappings Reference

### Top-Level Sections
| Human-Friendly | Technical |
|----------------|-----------|
| Human Input | `human_input` |
| AI Generated | `ai_generated` |
| Generation Settings | `generation` |
| Multi-Fidelity Analysis | `multi_fidelity` |

### Subsections
| Human-Friendly | Technical |
|----------------|-----------|
| Project Information | `project` |
| Standards & Codes | `standards` |
| Site Conditions | `site` |
| Buoy Specifications | `buoy` |
| Mooring System | `mooring` |
| Offloading System | `offloading` |
| Analysis Settings | `analysis` |

### Common Parameters
| Human-Friendly | Technical | Units |
|----------------|-----------|-------|
| Outer Diameter | `outer_diameter` | m |
| Operating Mass | `mass_operating` | tonnes |
| Significant Wave Height (Hs) | `hs_max` | m |
| Peak Period (Tp) | `tp_max` | s |
| Minimum Breaking Load | `mbl` | kN |
| Safety Factor (Intact) | `safety_factor_intact` | - |
| Deadweight Tonnage | `tanker_dwt` | tonnes |

**See `scripts/yaml_key_mapper.py` for complete mapping list (200+ keys)**

---

## Benefits Summary

### For Engineers
- ✅ **Intuitive**: No programming knowledge needed
- ✅ **Self-Documenting**: Clear parameter names with units
- ✅ **Professional**: Industry-standard terminology
- ✅ **Safe**: Type-safe with validation

### For Projects
- ✅ **Maintainable**: Easy to review and update
- ✅ **Collaborative**: Multiple engineers can edit
- ✅ **Version Control**: Git-friendly, clear diffs
- ✅ **Documentation**: Can be included in reports

### For System
- ✅ **Backwards Compatible**: Old files still work
- ✅ **Flexible**: Multiple key formats accepted
- ✅ **Validated**: Full validation framework
- ✅ **Tested**: Proven with North Sea example

---

## Validation

Both formats produce **identical results**:

```bash
# Generate with technical format
python scripts/generate_calm_buoy_project.py \
  --config north_sea_calm_project.yml \
  --output projects/TEST_TECH

# Generate with human-friendly format
python scripts/generate_calm_buoy_project.py \
  --config north_sea_calm_project_human.yml \
  --output projects/TEST_HUMAN

# Results are identical!
diff -r projects/TEST_TECH/orcaflex projects/TEST_HUMAN/orcaflex
# No differences found
```

---

## Migration Guide

### Converting Existing Projects

```bash
# Step 1: Convert to human-friendly format
python scripts/convert_yaml_format.py \
  my_old_config.yml \
  my_new_config_human.yml \
  --to human

# Step 2: Review and edit
vim my_new_config_human.yml

# Step 3: Generate (works exactly as before)
python scripts/generate_calm_buoy_project.py \
  --config my_new_config_human.yml \
  --validate
```

### No Code Changes Required

Existing scripts and workflows continue to work:
- ✅ Generation script unchanged (just added mapper)
- ✅ Validation framework unchanged
- ✅ Output format unchanged
- ✅ All tools compatible

---

## Examples

### Before (Technical)
```yaml
human_input:
  mooring:
    line_segments:
      - nominal_diameter: 76
        length: 150
        mbl: 7300
        mass_per_meter: 88.3
```

### After (Human-Friendly)
```yaml
Human Input:
  Mooring System:
    Mooring Lines:
      - Diameter: 76  # mm (3 inch R4 chain)
        Length: 150  # m
        Minimum Breaking Load: 7300  # kN
        Mass per Meter: 88.3  # kg/m
```

**Same data, much clearer!**

---

## Support

- **Template**: `templates/calm_buoy/project_template_human_friendly.yml`
- **Example**: `examples/modules/calm_buoy/north_sea_calm_project_human.yml`
- **Converter**: `scripts/convert_yaml_format.py`
- **Mapper**: `scripts/yaml_key_mapper.py`
- **Generator**: `scripts/generate_calm_buoy_project.py` (supports both formats)

---

**Created:** 2025-11-05
**Status:** ✅ Fully implemented and tested
**Compatibility:** Backwards compatible with all existing technical YAML files
