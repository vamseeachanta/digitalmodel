# Getting Started with Diffraction Analysis

**Duration**: 10 minutes
**Difficulty**: Beginner

## Overview

This tutorial introduces the diffraction analysis module and shows you how to convert your first AQWA or OrcaWave results to OrcaFlex format.

## Prerequisites

- Python 3.10+
- digitalmodel package installed
- AQWA .LIS file or OrcaFlex model with diffraction results

## Installation

```bash
# Navigate to project
cd digitalmodel

# Install package
uv pip install -e .

# Verify installation
diffraction --version
```

## Your First Conversion

### Option A: Using CLI (Recommended)

**Convert AQWA Results:**

```bash
# Basic conversion
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 -o ./output

# This creates 6 files:
# - FPSO_A_vessel_type.yml    (OrcaFlex vessel type)
# - FPSO_A_RAOs.csv           (RAO data)
# - FPSO_A_AddedMass.csv      (Added mass matrices)
# - FPSO_A_Damping.csv        (Damping matrices)
# - FPSO_A_Results.xlsx       (Complete Excel workbook)
# - FPSO_A_validation.json    (Validation report)
```

**Check the results:**

```bash
# View validation report
cat output/FPSO_A_validation.json

# Look for "overall_status": "PASS" or "WARNING"
```

### Option B: Using Python API

```python
from pathlib import Path
from digitalmodel.diffraction import (
    AQWAConverter,
    OrcaFlexExporter,
    validate_results
)

# Step 1: Convert AQWA results
converter = AQWAConverter(
    analysis_folder=Path("./aqwa_analysis"),
    vessel_name="FPSO_A"
)

results = converter.convert_to_unified_schema(water_depth=1200.0)

print(f"Converted vessel: {results.vessel_name}")
print(f"Source: {results.analysis_tool}")

# Step 2: Validate results
validation = validate_results(results)

print(f"Validation: {validation['overall_status']}")

# Step 3: Export to OrcaFlex
exporter = OrcaFlexExporter(results, Path("./output"))
files = exporter.export_all()

print(f"Exported {len(files)} files")
for fmt, path in files.items():
    print(f"  - {fmt}: {path.name}")
```

## Understanding the Output

### 1. Vessel Type YAML

```yaml
VesselType:
  Name: FPSO_A
  WaterDepth: 1200.0
  DiffractionSource: AQWA

  # References to data files
  RAODataFile: FPSO_A_RAOs.csv
  AddedMassDataFile: FPSO_A_AddedMass.csv
  DampingDataFile: FPSO_A_Damping.csv
```

This is the main file to load in OrcaFlex.

### 2. RAO CSV

Contains response amplitude operators for all 6 degrees of freedom:
- Surge (X translation)
- Sway (Y translation)
- Heave (Z translation)
- Roll (rotation about X)
- Pitch (rotation about Y)
- Yaw (rotation about Z)

Format: One row per frequency, columns for each DOF at each heading.

### 3. Added Mass / Damping CSV

Contains 6×6 hydrodynamic coefficient matrices at each frequency.

Format: Long-form with columns:
- Frequency
- Period
- DOF_i (row index)
- DOF_j (column index)
- Value
- Unit

### 4. Excel Workbook

Multi-sheet workbook with:
- **Summary**: Metadata and overview
- **RAOs**: All RAO data
- **AddedMass**: Added mass matrices
- **Damping**: Damping matrices
- **Discretization**: Frequency/heading info

### 5. Validation Report

JSON file with validation results:

```json
{
  "overall_status": "PASS",
  "issues_found": 0,
  "categories": {
    "completeness": "PASS",
    "physical_validity": "PASS",
    "range_checks": "PASS"
  }
}
```

## Validation Status Meanings

- **PASS**: All checks passed ✅
- **WARNING**: Minor issues detected (often acceptable) ⚠️
- **FAIL**: Critical issues found (data may be unusable) ❌

## Common Warnings

**"Extremely large coefficient"**
- Large vessels naturally have large coefficients
- Usually acceptable if vessel is very large/heavy

**"Only N frequencies"**
- Analysis may have limited frequency range
- Consider if coverage is sufficient for your needs

**"Heading gap > 45 degrees"**
- Analysis may not cover all directions uniformly
- Check if gap affects your analysis directions

## Next Steps

1. **Load in OrcaFlex** - See Tutorial 02
2. **Compare AQWA vs OrcaWave** - See Tutorial 03
3. **Batch process multiple vessels** - See Tutorial 04

## Troubleshooting

### Problem: "No .LIS file found"

**Solution:**
```bash
# Check folder contents
ls -la ./aqwa_analysis/

# Look for files ending in .LIS
# If filename doesn't match vessel name, the converter will use the first .LIS file found
```

### Problem: "Validation FAIL"

**Solution:**
1. Check validation report for specific issues
2. Review AQWA analysis log files for errors
3. Verify geometry and mesh quality
4. Re-run analysis if needed

### Problem: "OrcFxAPI not available" (for OrcaWave)

**Solution:**
```bash
# OrcaWave conversion requires OrcFxAPI
# Install OrcaFlex with Python API support
# Or use AQWA conversion instead
```

## Help & Support

```bash
# Get help for any command
diffraction --help
diffraction convert-aqwa --help

# Enable verbose output for debugging
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 --verbose
```

## Quick Reference

```bash
# Convert AQWA
diffraction convert-aqwa <folder> <name> -d <depth> -o <output>

# Convert OrcaWave
diffraction convert-orcawave <model.sim> -d <depth> -o <output>

# Compare both
diffraction compare <aqwa_folder> <orcawave_file> <name> -d <depth>

# Batch process
diffraction batch <config.json>
```

---

**Next Tutorial**: [Loading Results in OrcaFlex](02_loading_in_orcaflex.md)
