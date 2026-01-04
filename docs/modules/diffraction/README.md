# Unified Diffraction Output Module

**ABOUTME**: Standardized schemas and converters for AQWA and OrcaWave diffraction analysis results with OrcaFlex export capability.

**Version**: 3.0.0
**Status**: Phase 3 - Automation + QA (In Progress)

---

## Overview

This module provides a unified framework for handling diffraction analysis outputs from both AQWA and OrcaWave, enabling:

- **Standardized data structures** for RAO, added mass, and damping coefficients
- **Automatic conversion** to OrcaFlex-compatible formats
- **Comprehensive validation** of results completeness and physical validity
- **Multi-format export** (YAML, CSV, Excel)

---

## Module Structure

```
src/digitalmodel/modules/diffraction/
├── __init__.py                    # Module exports
├── output_schemas.py              # Unified data schemas
├── orcaflex_exporter.py           # OrcaFlex format export
├── aqwa_converter.py              # AQWA results converter
├── orcawave_converter.py          # OrcaWave results converter (NEW Phase 3)
└── output_validator.py            # Results validation

docs/modules/diffraction/
├── README.md                         # This file
├── ORCAWAVE_CONVERTER_GUIDE.md      # OrcaWave converter guide (NEW Phase 3)
├── PHASE_2_COMPLETION.md            # Phase 2 completion report
└── examples/
    ├── README.md                     # Examples documentation
    ├── complete_conversion_example.py # Complete workflow example
    └── outputs/                      # Generated outputs (gitignored)
```

---

## Quick Start

### Convert AQWA Results to OrcaFlex Format

```python
from digitalmodel.modules.diffraction import convert_aqwa_results

# Convert and export in one step
output_dir = convert_aqwa_results(
    analysis_folder="C:/AQWA/Projects/MyVessel",
    vessel_name="FPSO_A",
    water_depth=1200.0,
    output_folder="C:/OrcaFlex/Vessels/FPSO_A"
)

# Generated files:
# - FPSO_A_vessel_type.yml        (OrcaFlex vessel type)
# - FPSO_A_raos.csv               (RAO data)
# - FPSO_A_added_mass.csv         (Added mass matrices)
# - FPSO_A_damping.csv            (Damping matrices)
# - FPSO_A_hydrodynamics.xlsx     (Complete workbook)
# - FPSO_A_summary.txt            (Summary report)
```

### Manual Workflow with Validation

```python
from digitalmodel.modules.diffraction import (
    AQWAConverter,
    OrcaFlexExporter,
    validate_results
)
from pathlib import Path

# Step 1: Convert AQWA results to unified schema
converter = AQWAConverter(
    analysis_folder=Path("C:/AQWA/Projects/MyVessel"),
    vessel_name="FPSO_A"
)

results = converter.convert_to_unified_schema(water_depth=1200.0)

# Step 2: Validate results
validation_report = validate_results(
    results,
    output_file=Path("validation_report.json")
)

print(f"Validation status: {validation_report['overall_status']}")

# Step 3: Export to OrcaFlex format
exporter = OrcaFlexExporter(
    results,
    output_dir=Path("C:/OrcaFlex/Vessels/FPSO_A")
)

output_files = exporter.export_all()

for output_type, file_path in output_files.items():
    print(f"{output_type}: {file_path}")
```

### Convert OrcaWave Results to OrcaFlex Format (Phase 3)

**NEW in Version 3.0.0** - OrcaWave converter for extracting diffraction data from OrcaFlex models:

```python
from digitalmodel.modules.diffraction import (
    ORCAWAVE_AVAILABLE,
    convert_orcawave_results
)

# Check if OrcFxAPI is available
if not ORCAWAVE_AVAILABLE:
    print("OrcFxAPI required - install OrcaFlex with Python API")
else:
    # Convert and export in one step
    output_dir = convert_orcawave_results(
        model_file="path/to/model.sim",
        water_depth=1200.0,
        output_folder="output/",
        vessel_name="MyVessel"  # Optional - auto-detects if None
    )

    # Same output files as AQWA converter:
    # - vessel_type.yml, raos.csv, added_mass.csv, damping.csv, etc.
```

**Manual OrcaWave Workflow**:

```python
from digitalmodel.modules.diffraction import (
    OrcaWaveConverter,
    OrcaFlexExporter,
    validate_results
)

# Step 1: Convert OrcaWave results
converter = OrcaWaveConverter(
    model_file="analysis/fpso.sim",
    vessel_name="FPSO_A"  # Optional
)

results = converter.convert_to_unified_schema(water_depth=1200.0)

# Step 2: Validate (same as AQWA)
validation_report = validate_results(results)

# Step 3: Export (same as AQWA)
exporter = OrcaFlexExporter(results, output_dir="output/")
output_files = exporter.export_all()
```

**See**: `docs/modules/diffraction/ORCAWAVE_CONVERTER_GUIDE.md` for complete documentation

---

## Examples

### Complete Conversion Example

A comprehensive end-to-end example demonstrating the Phase 2 workflow is available:

**Location**: `docs/modules/diffraction/examples/complete_conversion_example.py`

**What it demonstrates**:
1. Creating realistic mock AQWA diffraction data for an FPSO
2. Converting to unified `DiffractionResults` schema
3. Running comprehensive validation (6 categories)
4. Exporting to all 6 OrcaFlex formats

**How to run**:
```bash
cd D:/workspace-hub/digitalmodel
uv run python docs/modules/diffraction/examples/complete_conversion_example.py
```

**Generates 7 output files**:
- Vessel type YAML
- RAO CSV (wide format)
- Added mass CSV (long format)
- Damping CSV (long format)
- Excel workbook (5 sheets)
- Summary text report
- Validation report JSON

**Mock Data**: 17 frequencies, 12 headings, 6 DOFs for a generic FPSO at 1200m water depth

See `docs/modules/diffraction/examples/README.md` for complete documentation.

---

## Data Schemas

### DiffractionResults

Top-level container for complete diffraction analysis:

```python
@dataclass
class DiffractionResults:
    vessel_name: str
    analysis_tool: str              # "AQWA" or "OrcaWave"
    water_depth: float              # meters

    # Main coefficient sets
    raos: RAOSet
    added_mass: AddedMassSet
    damping: DampingSet

    # Metadata
    created_date: str
    analysis_date: Optional[str]
    source_files: Optional[List[str]]
    notes: Optional[str]
```

### RAOSet

Response Amplitude Operators for all 6 DOFs:

```python
@dataclass
class RAOSet:
    vessel_name: str
    analysis_tool: str
    water_depth: float

    # RAO components (magnitude + phase)
    surge: RAOComponent
    sway: RAOComponent
    heave: RAOComponent
    roll: RAOComponent
    pitch: RAOComponent
    yaw: RAOComponent

    # Metadata
    created_date: str
    source_file: Optional[str]
```

### AddedMassSet / DampingSet

Frequency-dependent 6×6 hydrodynamic matrices:

```python
@dataclass
class AddedMassSet:
    vessel_name: str
    analysis_tool: str
    water_depth: float

    matrices: List[HydrodynamicMatrix]  # One per frequency
    frequencies: FrequencyData

    created_date: str
    source_file: Optional[str]
```

---

## Validation

### Automatic Validation

```python
from digitalmodel.modules.diffraction import validate_results

# Run complete validation suite
report = validate_results(results)

# Check overall status
if report['overall_status'] == 'PASS':
    print("✓ All validations passed")
elif report['overall_status'] == 'WARNING':
    print("⚠ Validation completed with warnings")
else:
    print("✗ Validation failed - check report")

# Export detailed report
with open('validation_report.json', 'w') as f:
    json.dump(report, f, indent=2)
```

### Validation Checks

The validator performs:

1. **Schema Validation**
   - All DOFs present with correct dimensions
   - No NaN or Inf values
   - Frequency and heading consistency

2. **Physical Validity**
   - RAO magnitudes within reasonable ranges
   - Added mass diagonal terms positive
   - Damping diagonal terms non-negative

3. **Range Checks**
   - Phase angles within bounds (-180 to 360 deg)
   - Coefficient magnitudes reasonable
   - No extreme values

4. **Frequency Coverage**
   - Adequate low-frequency coverage (< 0.1 rad/s)
   - Sufficient high-frequency range (> 1.5 rad/s)
   - Uniform discretization
   - Minimum 20 frequencies recommended

5. **Heading Coverage**
   - Full 360-degree coverage recommended
   - Maximum gap < 45 degrees
   - Minimum 8 headings recommended

6. **Symmetry Checks**
   - Added mass matrices symmetric
   - Damping matrices symmetric
   - Symmetry error < 1% of maximum value

---

## Export Formats

### OrcaFlex Vessel Type (YAML)

```yaml
VesselType:
  Name: FPSO_A
  WaterDepth: 1200.0
  DiffractionSource: AQWA
  CreatedDate: '2026-01-03 14:30:00'
  RAODataFile: FPSO_A_raos.csv
  AddedMassDataFile: FPSO_A_added_mass.csv
  DampingDataFile: FPSO_A_damping.csv
  Notes: Generated from AQWA analysis results
```

### RAO CSV Format

Columns: `Frequency, Period, Surge_Mag_H0, Surge_Phase_H0, ..., Yaw_Phase_H360`

One row per frequency, magnitude and phase for each DOF at each heading.

### Added Mass / Damping CSV Format

Long-form with columns: `Frequency, Period, DOF_i, DOF_j, Value, Unit`

One row per matrix element at each frequency.

### Excel Workbook

Multi-sheet workbook:
- **Summary**: Metadata and analysis overview
- **RAOs**: All RAO data in tabular format
- **AddedMass**: Added mass matrices
- **Damping**: Damping matrices
- **Discretization**: Frequency and heading information

---

## Units

### Standard Units (SI)

| Quantity | Unit | Symbol |
|----------|------|--------|
| Translation RAO | m/m | meters per meter wave amplitude |
| Rotation RAO | deg/m | degrees per meter wave amplitude |
| Added Mass (linear) | kg | kilograms |
| Added Mass (angular) | kg·m or kg·m² | coupling dependent |
| Damping (linear) | N·s/m | Newton-seconds per meter |
| Damping (angular) | N·m·s/rad | Newton-meter-seconds per radian |
| Frequency | rad/s | radians per second |
| Period | s | seconds |
| Heading | deg | degrees |

---

## Integration with OrcaFlex

### Loading Vessel Type in OrcaFlex

```python
import OrcFxAPI

# Create OrcaFlex model
model = OrcFxAPI.Model()

# Create vessel
vessel = model.CreateObject(OrcFxAPI.otVessel, 'FPSO_A')

# Load vessel type from generated file
vessel.VesselType = 'C:/OrcaFlex/Vessels/FPSO_A/FPSO_A_vessel_type.yml'

# The RAO, added mass, and damping data are automatically loaded
# from referenced CSV files

# Run analysis
model.RunSimulation()
```

---

## Advanced Usage

### Custom RAO Processing

```python
from digitalmodel.modules.diffraction import DiffractionResults, RAOSet

# Access specific RAO data
raos = results.raos

# Get surge RAO at specific frequency and heading
freq_idx = 10  # 10th frequency
heading_idx = 0  # First heading (e.g., 0 deg)

surge_mag = raos.surge.magnitude[freq_idx, heading_idx]
surge_phase = raos.surge.phase[freq_idx, heading_idx]

print(f"Surge RAO: {surge_mag:.3f} m/m at {surge_phase:.1f} deg")

# Get frequency value
freq = raos.surge.frequencies.values[freq_idx]
period = raos.surge.frequencies.periods[freq_idx]

print(f"Frequency: {freq:.4f} rad/s (Period: {period:.2f} s)")
```

### Extracting Specific Matrix

```python
# Get added mass matrix at specific frequency
target_freq = 0.5  # rad/s

matrix = results.added_mass.get_matrix_at_frequency(target_freq)

if matrix:
    # Access specific coupling (e.g., surge-surge)
    surge_surge = matrix.get_coupling(DOF.SURGE, DOF.SURGE)
    print(f"Surge-surge added mass: {surge_surge:.2f} kg")

    # Get full 6x6 matrix
    full_matrix = matrix.matrix
    print(full_matrix)
```

### Converting to Dictionary for JSON Export

```python
# Convert to dictionary
results_dict = results.to_dict()

# Export to JSON
import json
with open('results.json', 'w') as f:
    json.dump(results_dict, f, indent=2)
```

---

## Troubleshooting

### Missing Data

If conversion fails with missing data:
1. Check AQWA .LIS file exists and is readable
2. Verify analysis completed successfully
3. Check for corrupt output files
4. Review AQWA log files for errors

### Validation Warnings

Common warnings and resolutions:

**"RAO magnitude exceeds typical range"**
- May indicate numerical issues in analysis
- Review geometry and mesh quality
- Check for extremely light/heavy structures

**"Negative diagonal added mass"**
- Indicates physical invalidity
- Rerun analysis with different settings
- Check geometry and mass properties

**"Insufficient frequency coverage"**
- Extend frequency range in analysis
- Add more frequency points
- Focus on wave energy spectrum range

### Export Errors

**"File permission denied"**
- Close Excel if workbook is open
- Check write permissions on output directory

**"Invalid path"**
- Use absolute paths
- Ensure output directory exists

---

## API Reference

### AQWAConverter

```python
converter = AQWAConverter(analysis_folder, vessel_name)
results = converter.convert_to_unified_schema(water_depth)
```

### OrcaFlexExporter

```python
exporter = OrcaFlexExporter(results, output_dir)
output_files = exporter.export_all()

# Or export individually
vessel_type = exporter.export_vessel_type()
rao_csv = exporter.export_raos_csv()
excel = exporter.export_excel_workbook()
```

### OutputValidator

```python
validator = OutputValidator(results)
report = validator.run_all_validations()
validator.export_report(output_file)
```

---

## Performance

- **Conversion**: < 1 second for typical analysis
- **Validation**: < 2 seconds for complete suite
- **Export**: < 5 seconds for all formats including Excel

---

## Roadmap

### Phase 2 Complete ✅
- Unified output schemas
- AQWA converter (template)
- OrcaFlex exporter (6 formats)
- Comprehensive validation (6 categories)
- Complete conversion example

### Phase 3 - Automation + QA (In Progress) 🔄
- ✅ **OrcaWave converter** - Core implementation complete (Phase 3.1)
  - OrcFxAPI integration framework
  - Unified schema conversion
  - Same export capability as AQWA
- ✅ **Data extraction refinement** - Complete (Phase 3.2)
  - Actual RAO extraction from OrcFxAPI
  - Added mass/damping matrix extraction
  - Frequency/heading parsing
  - Validation and error handling
- 📋 **Batch processing** - Next (Phase 3.3)
  - Multi-vessel/multi-configuration support
  - Parallel execution framework
  - Result aggregation
- 📋 **Benchmark comparison** - Planned
  - AQWA vs OrcaWave statistical comparison
  - Automated cross-tool validation
  - Deviation analysis and reporting

### Phase 4 - Templates + Examples (Future)
- End-to-end example projects
- Reusable vessel templates
- Configuration generators

---

**Last Updated**: 2026-01-04
**Version**: 3.0.0 - Phase 3.2 (OrcaWave Data Extraction Complete)
