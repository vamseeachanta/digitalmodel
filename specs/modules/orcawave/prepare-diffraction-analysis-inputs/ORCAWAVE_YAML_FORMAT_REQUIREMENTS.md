# OrcaWave YAML Format Requirements

## ✅ CONFIRMED WORKING SOLUTION

This document captures the **exact formatting requirements** that allow OrcaWave to successfully load YAML configuration files. These requirements were discovered through trial and error and have been confirmed to work with OrcaWave 11.5e.

## Critical Formatting Rules

### 1. File Encoding
- **MUST use UTF-8 with BOM** (Byte Order Mark: `\xEF\xBB\xBF`)
- Regular UTF-8 without BOM will cause loading issues
- Python: Use `encoding='utf-8-sig'` when writing files

### 2. YAML Header
```yaml
%YAML 1.1
# Type: Diffraction
# Program: OrcaWave 11.5e
# File: <full path>
# Created: <time> on <date>
# User: <username>
# Configuration: <config name>
---
```

### 3. Boolean Values
- **MUST use unquoted `Yes` and `No`**
- ❌ WRONG: `'Yes'`, `"Yes"`, `true`, `True`, `TRUE`
- ✅ CORRECT: `Yes`, `No`
- Example:
  ```yaml
  QuadraticLoadPressureIntegration: Yes
  QuadraticLoadMomentumConservation: No
  BodyIncludedInAnalysis: Yes
  ```

### 4. Null/Empty Values
- **Tilde (`~`) for null values**:
  ```yaml
  BodyMeshDipolePanels: ~
  BodyOrcaFlexImportLength: ~
  ```
- **Empty string (`''`) for empty filenames**:
  ```yaml
  FreeSurfacePanelledZoneMeshFileName: ''
  ```
- **NEVER leave fields completely blank**

### 5. Special Values
- **Infinity**: Use unquoted `Infinity` (not `inf`, `INF`, or quoted)
  ```yaml
  QTFMaxPeriodOrFrequency: Infinity
  ```

### 6. Lists/Arrays
- Use hyphen-space format:
  ```yaml
  PeriodOrFrequency:
  - 4
  - 5
  - 6
  ```

### 7. Matrices
- Complex matrix field names must be exact:
  ```yaml
  BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz:
  - - 2094917.78
    - 0
    - 0
  - - 0
    - 9098465.41
    - 0
  - - 0
    - 0
    - 10218270.06
  ```

### 8. Vessel Draft Positioning
- **CRITICAL**: Draft must be reflected in `BodyMeshPosition` Z coordinate
- **Formula**: `BodyMeshPosition[2] = -draft` (negative because Z is positive upward)
- This positions the mesh so the waterline is at Z=0
- Example for 1.981m draft:
  ```yaml
  BodyMeshPosition:
  - 0      # X position
  - 0      # Y position
  - -1.981 # Z position (negative draft)
  ```

### 9. Units for Mass and Inertia
- **CRITICAL**: OrcaWave requires specific units for mass properties
- **Mass**: Must be in **tonnes (Te)**, NOT kilograms
  - Conversion: `mass_Te = mass_kg / 1000`
  - Example: 300,912.45 kg → 300.912 Te
- **Inertia**: Must be in **Te.m²**, NOT kg.m²
  - Conversion: `inertia_Te.m² = inertia_kg.m² / 1000`
  - Example: 2,094,917.78 kg.m² → 2,094.918 Te.m²
- **Lengths**: Meters (m) - no conversion needed from SI
- **Center of Gravity**: Meters (m) - no conversion needed

## Custom Parser Implementation

A custom OrcaWave YAML parser (`orcawave_yaml_parser.py`) was created to handle these requirements:

### Key Components:

1. **OrcaWaveYamlParser class**:
   - Maintains lists of fields requiring specific formatting
   - `boolean_mappings['yes_no_fields']` - Fields using Yes/No
   - `tilde_fields` - Fields using ~ for null
   - `empty_string_fields` - Fields requiring empty string

2. **Custom YAML Dumper**:
   ```python
   class OrcaWaveYamlDumper(yaml.SafeDumper):
       # Custom representers for OrcaWave format
   ```

3. **Value Formatting**:
   - Boolean conversion: `True` → `Yes`, `False` → `No`
   - Null handling: `None` → `~` or `''` based on field
   - Special values: `float('inf')` → `Infinity`

### Usage Example:

```python
from orcawave_yaml_parser import OrcaWaveYamlParser

parser = OrcaWaveYamlParser()
# Format data according to OrcaWave requirements
formatted_data = parser.format_data(raw_data)
# Write with proper encoding and formatting
parser.write_orcawave_yaml(formatted_data, output_path, config_name)
```

## Files Created for This Solution

1. **`scripts/orcawave_yaml_parser.py`** - Custom parser implementation
2. **`scripts/merge_templates.py`** - Template merger using custom parser
3. **`scripts/verify_format.py`** - Format verification tool
4. **This documentation** - For future reference

## Common Pitfalls to Avoid

1. **Using standard YAML libraries** - They don't preserve OrcaWave-specific formatting
2. **Quoted booleans** - OrcaWave requires unquoted Yes/No
3. **true/false instead of Yes/No** - OrcaWave doesn't recognize these
4. **Missing BOM** - File won't load properly
5. **Wrong null representation** - Using `null`, `None`, or blank instead of `~`
6. **Using ~ for filename fields** - Some fields need empty string `''` instead

## Testing Procedure

1. Generate configuration with custom parser
2. Check for UTF-8 BOM: First 3 bytes should be `\xEF\xBB\xBF`
3. Verify Yes/No are unquoted
4. Confirm null values use `~` or `''` as appropriate
5. Load in OrcaWave to confirm

## Success Confirmation

✅ **2025-08-26**: All 4 vessel configurations successfully loaded in OrcaWave 11.5e:
- `orcawave_incident_draft_fuel_centered.yml`
- `orcawave_incident_draft_fuel_centered_adjusted.yml`
- `orcawave_fo_to_port.yml`
- `orcawave_fo_to_port_with_ingress.yml`

## Future Applications

This custom parser solution should be used for:
- Any OrcaWave input file generation
- Converting between OrcaWave and other formats
- Troubleshooting OrcaWave loading issues
- Template-based configuration generation

## Important Note

OrcaWave is **extremely particular** about YAML formatting. Even minor deviations from these requirements can cause loading failures. Always use the custom parser to ensure compatibility.