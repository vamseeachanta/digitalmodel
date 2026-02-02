# Diffraction Conversion Examples

**ABOUTME**: Complete end-to-end examples demonstrating Phase 2 unified diffraction output standardization workflow.

---

## Overview

This directory contains practical examples showing the complete workflow from AQWA diffraction analysis data through unified schema conversion to OrcaFlex export in multiple formats.

The examples validate that Phase 2 implementation works correctly and provide templates for users to adapt for their own analyses.

---

## Available Examples

### 1. Complete Conversion Example (`complete_conversion_example.py`)

**Purpose**: Demonstrates the entire workflow from mock AQWA data to OrcaFlex export

**What it does**:
1. Creates realistic mock AQWA diffraction data for a generic FPSO
2. Converts to unified `DiffractionResults` schema
3. Runs comprehensive validation (6 categories)
4. Exports to all 6 OrcaFlex formats

**How to run**:
```bash
# Using uv (recommended)
cd D:/workspace-hub/digitalmodel
uv run python docs/modules/diffraction/examples/complete_conversion_example.py

# Or with regular Python (requires package installation)
python -m pip install -e .
python docs/modules/diffraction/examples/complete_conversion_example.py
```

**Generated outputs** (in `outputs/` subdirectory):
- `Example_FPSO_vessel_type.yml` - OrcaFlex vessel type definition
- `Example_FPSO_raos.csv` - RAO data (wide format, all DOFs and headings)
- `Example_FPSO_added_mass.csv` - Added mass matrices (long format)
- `Example_FPSO_damping.csv` - Damping matrices (long format)
- `Example_FPSO_hydrodynamics.xlsx` - Complete Excel workbook (5 sheets)
- `Example_FPSO_summary.txt` - Text summary report
- `validation_report.json` - Detailed validation results

**Mock Data Characteristics**:
- **Frequencies**: 17 points from 0.05 to 2.0 rad/s (periods 3.1s to 125.7s)
- **Headings**: 12 directions from 0° to 330° (30° spacing)
- **DOFs**: All 6 degrees of freedom (Surge, Sway, Heave, Roll, Pitch, Yaw)
- **Vessel**: Generic FPSO with realistic response characteristics
- **Water Depth**: 1200 m (typical Gulf of Mexico)

**RAO Characteristics**:
- **Surge**: Peaks at low frequencies, maximum in head seas
- **Sway**: Peaks in beam seas
- **Heave**: Resonance around 0.4 rad/s
- **Roll**: Beam seas resonance at 0.3 rad/s
- **Pitch**: Head/stern seas resonance at 0.35 rad/s
- **Yaw**: Smaller magnitudes, quartering seas

**Validation Results**: WARNING status (expected)
- Large pitch/yaw added mass coefficients (typical for FPSO)
- All physical validity checks pass
- Symmetry checks pass (<1% error)
- Frequency coverage adequate for typical analysis

---

## Using the Example with Real AQWA Data

To adapt this example for your own AQWA analysis:

1. **Replace mock data generation** with AQWA file parsing:
```python
from digitalmodel.diffraction import AQWAConverter

# Instead of create_mock_aqwa_data()
converter = AQWAConverter(
    analysis_folder="C:/AQWA/Projects/MyVessel",
    vessel_name="My_FPSO"
)

results = converter.convert_to_unified_schema(water_depth=1200.0)
```

2. **Run validation** as shown in example

3. **Export to OrcaFlex** using same export code

---

## Output File Formats

### Vessel Type YAML

OrcaFlex vessel type file with references to data files:

```yaml
VesselType:
  Name: Example_FPSO
  WaterDepth: 1200.0
  DiffractionSource: AQWA
  CreatedDate: '2026-01-03 21:13:08'
  RAODataFile: Example_FPSO_raos.csv
  AddedMassDataFile: Example_FPSO_added_mass.csv
  DampingDataFile: Example_FPSO_damping.csv
  Notes: Complete example conversion demonstrating Phase 2 unified schema
```

### RAO CSV Format

Wide format with columns:
```
Frequency (rad/s), Period (s),
Surge_Mag_H0.0, Surge_Phase_H0.0,
Sway_Mag_H0.0, Sway_Phase_H0.0,
... (all 6 DOFs × 12 headings)
```

One row per frequency, 17 rows total.

### Added Mass / Damping CSV Format

Long format with columns:
```
Frequency (rad/s), Period (s), DOF_i, DOF_j, Value, Unit
```

One row per matrix element per frequency:
- 17 frequencies × 36 matrix elements = 612 rows

### Excel Workbook Structure

Multi-sheet workbook with professional formatting:

1. **Summary Sheet**:
   - Vessel metadata
   - Analysis parameters
   - Frequency and heading coverage
   - File list

2. **RAOs Sheet**:
   - Complete RAO data table
   - Magnitude and phase for each DOF and heading
   - Color-coded by DOF type

3. **AddedMass Sheet**:
   - Added mass matrices in tabular form
   - Frequency-dependent coefficients
   - Proper units for each coupling type

4. **Damping Sheet**:
   - Damping matrices in tabular form
   - Frequency-dependent coefficients
   - Proper units for each coupling type

5. **Discretization Sheet**:
   - Frequency table (values, periods)
   - Heading table (angles in degrees)

---

## Validation Categories

The example demonstrates all 6 validation categories:

1. **Schema Validation**
   - All DOFs present with correct dimensions
   - No NaN or Inf values
   - Frequency/heading consistency

2. **Physical Validity**
   - RAO magnitudes within reasonable ranges
   - Added mass diagonal terms positive
   - Damping diagonal terms non-negative

3. **Range Checks**
   - Phase angles within -180° to 360°
   - Coefficient magnitudes < 1e10
   - No extreme values

4. **Frequency Coverage**
   - Low frequency coverage (< 0.1 rad/s for long waves)
   - High frequency coverage (> 1.5 rad/s for short waves)
   - Minimum 20 frequencies recommended
   - Uniform spacing (ratio < 5.0)

5. **Heading Coverage**
   - Full 360° coverage recommended
   - Maximum gap < 45°
   - Minimum 8 headings recommended

6. **Symmetry Checks**
   - Added mass matrices symmetric (< 1% error)
   - Damping matrices symmetric (< 1% error)

---

## Integration with OrcaFlex

### Loading Generated Vessel Type

```python
import OrcFxAPI

# Create OrcaFlex model
model = OrcFxAPI.Model()

# Create vessel object
vessel = model.CreateObject(OrcFxAPI.otVessel, 'Example_FPSO')

# Load vessel type from generated file
vessel.VesselType = 'docs/modules/diffraction/examples/outputs/Example_FPSO_vessel_type.yml'

# The RAO, added mass, and damping data files are automatically loaded
# from the same directory as the vessel type file

# Run analysis
model.RunSimulation()
```

### Manual Data Loading

You can also load CSV files directly if needed:

```python
import pandas as pd

# Load RAO data
raos_df = pd.read_csv('docs/modules/diffraction/examples/outputs/Example_FPSO_raos.csv')

# Load added mass matrices
am_df = pd.read_csv('docs/modules/diffraction/examples/outputs/Example_FPSO_added_mass.csv')

# Load damping matrices
damp_df = pd.read_csv('docs/modules/diffraction/examples/outputs/Example_FPSO_damping.csv')
```

---

## Troubleshooting

### UnicodeEncodeError on Windows

If you encounter Unicode errors when running the example, this is a Windows console limitation with Unicode characters. The example has been updated to use ASCII-only output.

### ModuleNotFoundError

If you get `ModuleNotFoundError: No module named 'digitalmodel'`:

```bash
# Install package in development mode
cd D:/workspace-hub/digitalmodel
pip install -e .

# Or use uv (recommended)
uv run python docs/modules/diffraction/examples/complete_conversion_example.py
```

### Missing Optional Import

If validation report JSON export fails, ensure `typing.Optional` is imported in `output_validator.py`. This has been fixed in the current version.

---

## File Organization

```
docs/modules/diffraction/examples/
├── README.md                           # This file
├── complete_conversion_example.py      # Complete workflow example (~400 lines)
└── outputs/                            # Generated output files
    ├── Example_FPSO_vessel_type.yml
    ├── Example_FPSO_raos.csv
    ├── Example_FPSO_added_mass.csv
    ├── Example_FPSO_damping.csv
    ├── Example_FPSO_hydrodynamics.xlsx
    ├── Example_FPSO_summary.txt
    └── validation_report.json
```

---

## Next Steps

After running this example:

1. **Review outputs**: Open Excel file to see formatted results
2. **Check validation**: Review `validation_report.json` for quality assurance
3. **Test in OrcaFlex**: Load vessel type YAML in an OrcaFlex model
4. **Adapt for your data**: Replace mock data with your AQWA analysis results

---

## Related Documentation

- **Phase 2 Completion Report**: `docs/modules/diffraction/PHASE_2_COMPLETION.md`
- **Diffraction Module README**: `docs/modules/diffraction/README.md`
- **AQWA Quick Start**: `docs/modules/aqwa/QUICK_START.md`
- **Expansion Plan**: `docs/modules/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md`

---

**Last Updated**: 2026-01-03
**Phase**: 2 - Output Standardization Complete
**Status**: Production Ready
