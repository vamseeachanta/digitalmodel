# Diffraction Analysis CLI Guide

**Version**: 3.0.0 (Phase 3 Complete)
**Command**: `diffraction`

## Overview

The Diffraction Analysis CLI provides command-line tools for converting, comparing, and batch processing diffraction analysis results from AQWA and OrcaWave to OrcaFlex format.

## Installation

The CLI is automatically installed with the digitalmodel package:

```bash
# Install package
uv pip install -e .

# Verify installation
diffraction --help
```

## Commands

### 1. convert-aqwa

Convert AQWA diffraction results to OrcaFlex format.

**Usage**:
```bash
diffraction convert-aqwa ANALYSIS_FOLDER VESSEL_NAME -d WATER_DEPTH [OPTIONS]
```

**Arguments**:
- `ANALYSIS_FOLDER`: Path to AQWA analysis folder containing .LIS files
- `VESSEL_NAME`: Name of vessel

**Required Options**:
- `-d, --water-depth FLOAT`: Water depth in meters

**Optional**:
- `-o, --output PATH`: Output directory (default: output)
- `--validate/--no-validate`: Run validation checks (default: True)
- `-f, --formats CHOICE`: Export formats (can specify multiple)
  - Choices: `all`, `vessel_type`, `rao_csv`, `added_mass_csv`, `damping_csv`, `excel`, `summary`
  - Default: `all`

**Example**:
```bash
# Convert AQWA results with all formats
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 -o ./output

# Convert with specific formats only
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 \
  -f vessel_type -f rao_csv -f excel

# Skip validation
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 --no-validate
```

**Output Files**:
- `FPSO_A.yml` - OrcaFlex vessel type YAML
- `FPSO_A_RAOs.csv` - RAO data in OrcaFlex CSV format
- `FPSO_A_AddedMass.csv` - Added mass matrices
- `FPSO_A_Damping.csv` - Damping matrices
- `FPSO_A_Results.xlsx` - Complete Excel workbook with all data
- `FPSO_A_Summary.json` - Summary statistics
- `FPSO_A_validation.json` - Validation report

---

### 2. convert-orcawave

Convert OrcaWave diffraction results to OrcaFlex format.

**Usage**:
```bash
diffraction convert-orcawave MODEL_FILE -d WATER_DEPTH [OPTIONS]
```

**Arguments**:
- `MODEL_FILE`: Path to OrcaFlex model file (.sim or .dat)

**Required Options**:
- `-d, --water-depth FLOAT`: Water depth in meters

**Optional**:
- `-v, --vessel-name TEXT`: Vessel name (auto-detect if not specified)
- `-o, --output PATH`: Output directory (default: output)
- `--validate/--no-validate`: Run validation checks (default: True)
- `-f, --formats CHOICE`: Export formats (same as convert-aqwa)

**Example**:
```bash
# Convert with auto-detected vessel name
diffraction convert-orcawave model.sim -d 1200 -o ./output

# Specify vessel name explicitly
diffraction convert-orcawave model.sim -v FPSO_A -d 1200

# Convert specific vessel with selected formats
diffraction convert-orcawave model.dat -v TLP_B -d 800 \
  -f vessel_type -f excel -f summary
```

**Requirements**:
- OrcFxAPI must be installed for OrcaWave conversion
- Model file must contain diffraction analysis results

---

### 3. compare

Compare AQWA and OrcaWave diffraction results.

**Usage**:
```bash
diffraction compare AQWA_FOLDER ORCAWAVE_FILE VESSEL_NAME -d WATER_DEPTH [OPTIONS]
```

**Arguments**:
- `AQWA_FOLDER`: Path to AQWA analysis folder
- `ORCAWAVE_FILE`: Path to OrcaFlex model file
- `VESSEL_NAME`: Name of vessel

**Required Options**:
- `-d, --water-depth FLOAT`: Water depth in meters

**Optional**:
- `-o, --output PATH`: Output directory (default: comparison)
- `-t, --tolerance FLOAT`: Comparison tolerance (default: 0.05 = 5%)

**Example**:
```bash
# Compare with default 5% tolerance
diffraction compare ./aqwa_analysis model.sim FPSO_A -d 1200

# Compare with stricter tolerance
diffraction compare ./aqwa_analysis model.sim FPSO_A -d 1200 -t 0.01

# Compare with custom output directory
diffraction compare ./aqwa_analysis model.sim FPSO_A -d 1200 \
  -o ./comparison_results
```

**Output Files**:
- `FPSO_A_comparison.json` - Detailed comparison report
- `FPSO_A_comparison_plots/` - Comparison visualization plots
- Validation reports for both AQWA and OrcaWave results

**Comparison Report Contents**:
- Overall agreement percentage
- Component-by-component comparison (RAOs, added mass, damping)
- Statistical metrics (mean error, max error, RMSE)
- Frequency and heading coverage comparison
- Physical validity checks

---

### 4. batch

Batch process multiple vessels from configuration file.

**Usage**:
```bash
diffraction batch CONFIG_FILE
```

**Arguments**:
- `CONFIG_FILE`: Path to JSON configuration file

**Example**:
```bash
diffraction batch batch_config.json
```

**Configuration File Format**:
```json
{
  "configurations": [
    {
      "vessel_name": "FPSO_A",
      "source_type": "aqwa",
      "source_path": "./aqwa_analysis_A",
      "water_depth": 1200.0,
      "output_dir": "./output/FPSO_A",
      "validate": true,
      "formats": ["all"]
    },
    {
      "vessel_name": "TLP_B",
      "source_type": "orcawave",
      "source_path": "./model_B.sim",
      "water_depth": 800.0,
      "output_dir": "./output/TLP_B",
      "validate": true,
      "formats": ["vessel_type", "excel", "summary"]
    }
  ],
  "parallel": true,
  "max_workers": 4
}
```

**Configuration Options**:
- `vessel_name`: Vessel identifier
- `source_type`: Either `"aqwa"` or `"orcawave"`
- `source_path`: Path to analysis folder (AQWA) or model file (OrcaWave)
- `water_depth`: Water depth in meters
- `output_dir`: Output directory for this vessel
- `validate`: Whether to run validation (default: true)
- `formats`: List of export formats (default: ["all"])

**Global Options**:
- `parallel`: Enable parallel processing (default: true)
- `max_workers`: Maximum parallel workers (default: 4)

**Output**:
```
================================================================================
Batch Processing Summary
================================================================================
Total configurations: 5
Successful: 4
Warnings: 1
Failed: 0
Total duration: 45.23 seconds
```

---

## Validation Reports

All commands (except batch) generate validation reports that check:

### Completeness
- All required DOFs present (6 DOFs: surge, sway, heave, roll, pitch, yaw)
- Expected frequency range coverage
- Expected heading coverage (0° to 360°)

### Physical Validity
- RAO magnitudes reasonable (< 10.0 m/m for translations, < 5.0 rad/m for rotations)
- Added mass non-negative
- Damping non-negative
- Symmetric matrices (added mass and damping)

### Data Quality
- No NaN or infinite values
- Sufficient data points for interpolation
- Consistent units

**Validation Status**:
- `PASS`: All checks passed
- `WARNING`: Minor issues detected (non-critical)
- `FAIL`: Critical issues found (data unusable)

---

## Export Formats

### vessel_type (YAML)
OrcaFlex vessel type definition with:
- Displacement and mass properties
- RAO tables
- Added mass and damping matrices
- QTF data (if available)

### rao_csv
RAO data in OrcaFlex CSV format:
- Columns: Period, Direction, DOF, Magnitude, Phase
- Compatible with OrcaFlex RAO import

### added_mass_csv / damping_csv
Hydrodynamic matrices in CSV format:
- One file per frequency
- 6x6 symmetric matrices
- OrcaFlex-compatible format

### excel
Complete Excel workbook with multiple sheets:
- Summary (metadata and statistics)
- RAOs (all components)
- Added Mass (all frequencies)
- Damping (all frequencies)
- Validation Results

### summary (JSON)
Summary statistics including:
- Metadata (vessel, water depth, source)
- RAO statistics (min/max/mean by DOF)
- Matrix statistics
- Frequency and heading ranges

---

## Error Handling

All commands provide colored output for status:
- **Green**: Success messages
- **Yellow**: Warnings
- **Red**: Errors

Add `--verbose` flag for detailed error tracebacks:
```bash
diffraction convert-aqwa ./analysis FPSO_A -d 1200 --verbose
```

---

## Examples

### Typical Workflow

**1. Convert AQWA results**:
```bash
diffraction convert-aqwa ./aqwa_fpso FPSO_A -d 1200 -o ./fpso_results
```

**2. Convert OrcaWave results**:
```bash
diffraction convert-orcawave ./orcawave_fpso.sim -d 1200 -o ./fpso_results
```

**3. Compare both**:
```bash
diffraction compare ./aqwa_fpso ./orcawave_fpso.sim FPSO_A -d 1200 \
  -o ./fpso_comparison
```

**4. Batch process fleet**:
```bash
# Create batch config with all vessels
diffraction batch fleet_config.json
```

---

## Programmatic Usage

You can also use the converters programmatically:

```python
from pathlib import Path
from digitalmodel.diffraction import (
    AQWAConverter,
    OrcaWaveConverter,
    OrcaFlexExporter,
    validate_results,
    compare_diffraction_results
)

# Convert AQWA
aqwa = AQWAConverter(Path("./aqwa_analysis"), "FPSO_A")
aqwa_results = aqwa.convert_to_unified_schema(water_depth=1200.0)

# Validate
validation = validate_results(aqwa_results, output_file=Path("validation.json"))
print(f"Validation status: {validation['overall_status']}")

# Export to OrcaFlex
exporter = OrcaFlexExporter(aqwa_results, Path("./output"))
output_files = exporter.export_all()

# Compare with OrcaWave (if available)
orcawave = OrcaWaveConverter(Path("./model.sim"), "FPSO_A")
orcawave_results = orcawave.convert_to_unified_schema(water_depth=1200.0)

comparison = compare_diffraction_results(
    aqwa_results,
    orcawave_results,
    Path("./comparison"),
    tolerance=0.05
)
print(f"Overall agreement: {comparison.overall_agreement}")
```

---

## Troubleshooting

### OrcFxAPI Not Available
**Error**: `[ERROR] OrcFxAPI not available`
**Solution**: Install OrcaFlex with Python API support

### Invalid .LIS File
**Error**: `ValueError: No AQWA results found`
**Solution**: Ensure analysis folder contains valid .LIS output files

### Unicode Errors (Windows)
**Error**: `UnicodeEncodeError: 'charmap' codec can't encode character`
**Solution**: Set console encoding: `set PYTHONIOENCODING=utf-8`

### Missing Data
**Error**: `[FAIL] Validation: Missing required DOFs`
**Solution**: Check that diffraction analysis includes all 6 DOFs

---

## Version History

- **3.0.0** - Phase 3 complete: Full AQWA/OrcaWave conversion, CLI tools, batch processing
- **2.0.0** - Phase 2: OrcaFlex export with validation
- **1.0.0** - Phase 1: Unified schemas and AQWA converter foundation

---

## Support

For issues or questions:
- Check validation reports for detailed diagnostics
- Use `--verbose` flag for detailed error information
- Refer to module documentation in `docs/domains/diffraction/`
