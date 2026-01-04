# AQWA Conversion - Complete Example

**Duration**: 15 minutes
**Difficulty**: Intermediate

## Overview

This tutorial walks through a complete AQWA conversion workflow using real data, including validation, troubleshooting, and loading in OrcaFlex.

## Scenario

You have completed an AQWA diffraction analysis for an FPSO in 1200m water depth and need to use the results in OrcaFlex for coupled analysis.

## Files You'll Need

- AQWA analysis folder containing `.LIS` file
- Access to OrcaFlex (for final step)

## Step-by-Step Workflow

### Step 1: Locate Your AQWA Results

AQWA outputs several files, but we only need the `.LIS` file:

```
aqwa_analysis/
├── FPSO_A.LIS          ← We need this
├── FPSO_A.DAT
├── FPSO_A.OUT
└── other_files...
```

**Tip**: The `.LIS` file is typically 1-10 MB and contains all hydrodynamic coefficients.

### Step 2: Quick Inspection

Before conversion, quickly check the .LIS file:

```bash
# Check file size
ls -lh aqwa_analysis/FPSO_A.LIS

# Look for key sections (should see these markers)
grep "R.A.O.S-VARIATION" aqwa_analysis/FPSO_A.LIS
grep "ADDED MASS-VARIATION" aqwa_analysis/FPSO_A.LIS
grep "DAMPING-VARIATION" aqwa_analysis/FPSO_A.LIS
```

If all three markers are found, the file should convert successfully.

### Step 3: Run Conversion

```bash
# Convert with all formats
diffraction convert-aqwa \
    ./aqwa_analysis \
    FPSO_A \
    -d 1200 \
    -o ./orcaflex_data \
    --validate

# Expected output:
# ================================================================================
# AQWA to OrcaFlex Conversion
# ================================================================================
# Analysis folder: ./aqwa_analysis
# Vessel: FPSO_A
# Water depth: 1200.0 m
# Output: ./orcaflex_data
#
# Converting AQWA results...
# [OK] Conversion successful
#
# Validating results...
# [OK] Validation: PASS
#
# Exporting to OrcaFlex formats...
# [OK] Exported 6 files:
#   - vessel_type: FPSO_A_vessel_type.yml
#   - rao_csv: FPSO_A_RAOs.csv
#   - added_mass_csv: FPSO_A_AddedMass.csv
#   - damping_csv: FPSO_A_Damping.csv
#   - excel: FPSO_A_Results.xlsx
#   - summary: FPSO_A_Summary.json
```

### Step 4: Review Validation Report

The validation report is saved as `FPSO_A_validation.json`:

```json
{
  "overall_status": "WARNING",
  "issues_found": 3,
  "categories": {
    "completeness": {
      "status": "PASS",
      "checks": {
        "all_dofs_present": "PASS",
        "no_nan_values": "PASS",
        "sufficient_data": "PASS"
      }
    },
    "physical_validity": {
      "status": "PASS"
    },
    "range_checks": {
      "status": "WARNING",
      "issues": [
        "Extremely large coefficient (2.19e+11) at frequency 0.1000"
      ]
    },
    "frequency_coverage": {
      "status": "WARNING",
      "issues": [
        "Only 12 frequencies - may be insufficient"
      ]
    }
  }
}
```

**Interpretation:**
- ✅ Data is complete and physically valid
- ⚠️ Large coefficients are expected for massive FPSOs
- ⚠️ Consider adding more frequencies if needed for your analysis

### Step 5: Inspect Excel Workbook

Open `FPSO_A_Results.xlsx` to visually inspect the data:

**Summary Sheet:**
- Vessel name, water depth, source tool
- Frequency range (min/max)
- Heading coverage
- Number of data points

**RAOs Sheet:**
- One row per frequency
- Columns for each DOF at each heading
- Both magnitude and phase

**AddedMass/Damping Sheets:**
- Frequency-dependent matrices
- Check diagonal terms (should be positive for added mass)

### Step 6: Review Frequency Coverage

Check if frequency range covers your wave spectrum:

```python
from pathlib import Path
import pandas as pd

# Load RAO data
raos = pd.read_csv("orcaflex_data/FPSO_A_RAOs.csv")

# Check frequency range
print(f"Frequency range: {raos['Frequency'].min():.3f} to {raos['Frequency'].max():.3f} rad/s")
print(f"Period range: {raos['Period'].max():.1f} to {raos['Period'].min():.1f} seconds")
print(f"Number of frequencies: {len(raos)}")

# Typical wave periods for ocean environments: 6-20 seconds
# Ensure your analysis covers this range
```

### Step 7: Load in OrcaFlex

**Using OrcaFlex GUI:**

1. Open OrcaFlex
2. Create new model
3. Add Vessel object
4. In Vessel Data:
   - Go to "Vessel Type" tab
   - Click "..." next to Vessel Type
   - Browse to `FPSO_A_vessel_type.yml`
   - Click OK

The vessel type will load automatically with all RAO, added mass, and damping data.

**Using OrcFxAPI (Python):**

```python
import OrcFxAPI

# Create model
model = OrcFxAPI.Model()

# Add vessel
vessel = model.CreateObject(OrcFxAPI.otVessel, "FPSO_A")

# Load vessel type
vessel.VesselType = str(Path("orcaflex_data/FPSO_A_vessel_type.yml").absolute())

# Verify data loaded
print(f"Vessel type loaded: {vessel.VesselType}")
print(f"Has RAO data: {vessel.IncludedInStatics == 'Yes'}")

# Save model
model.SaveData("fpso_coupled_analysis.dat")
```

### Step 8: Verify in OrcaFlex

After loading, verify the data in OrcaFlex:

1. **Check RAO plots:**
   - Right-click vessel → View → RAO Response
   - Should see curves for all 6 DOFs
   - Check magnitudes are reasonable

2. **Check Added Mass/Damping:**
   - Vessel Data → Hydrodynamics tab
   - Verify matrices loaded at multiple frequencies
   - Diagonal terms should be positive

3. **Run Static Analysis:**
   - Calculate Statics
   - Check vessel displacement and stability
   - Verify no errors or warnings

## Advanced: Custom Export Formats

If you only need specific formats:

```bash
# Export only vessel type and Excel workbook
diffraction convert-aqwa \
    ./aqwa_analysis \
    FPSO_A \
    -d 1200 \
    -o ./output \
    -f vessel_type \
    -f excel
```

## Programmatic Workflow

For integration into scripts or automation:

```python
from pathlib import Path
from digitalmodel.modules.diffraction import (
    AQWAConverter,
    OrcaFlexExporter,
    validate_results
)

def convert_aqwa_to_orcaflex(
    analysis_folder: Path,
    vessel_name: str,
    water_depth: float,
    output_dir: Path
) -> dict:
    """Convert AQWA results to OrcaFlex format with validation."""

    # Step 1: Convert
    print(f"Converting {vessel_name}...")
    converter = AQWAConverter(analysis_folder, vessel_name)
    results = converter.convert_to_unified_schema(water_depth)

    # Step 2: Validate
    print("Validating results...")
    validation = validate_results(
        results,
        output_file=output_dir / f"{vessel_name}_validation.json"
    )

    status = validation['overall_status']
    print(f"Validation: {status}")

    if status == 'FAIL':
        raise ValueError(f"Validation failed for {vessel_name}")

    # Step 3: Export
    print("Exporting to OrcaFlex formats...")
    exporter = OrcaFlexExporter(results, output_dir)
    files = exporter.export_all()

    print(f"Exported {len(files)} files")

    return {
        'validation_status': status,
        'output_files': files
    }

# Use it
result = convert_aqwa_to_orcaflex(
    analysis_folder=Path("aqwa_analysis"),
    vessel_name="FPSO_A",
    water_depth=1200.0,
    output_dir=Path("orcaflex_data")
)
```

## Troubleshooting

### Issue: Validation shows "FAIL"

**Check for:**
1. NaN values in data
2. Missing DOFs
3. Negative diagonal added mass

**Solution:**
```bash
# Run with verbose output to see details
diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 --verbose
```

Review AQWA log files for analysis errors.

### Issue: Very large coefficients warning

**This is often normal for:**
- Very large vessels (FPSOs, semis)
- Low frequencies
- Rotational DOFs (moment of inertia effects)

**Verify by checking:**
- Vessel displacement and dimensions
- Coefficients scale appropriately with mass/inertia

### Issue: Missing some frequencies

**Possible causes:**
1. AQWA analysis didn't converge at those frequencies
2. .LIS file incomplete

**Solution:**
- Check AQWA output log
- Re-run AQWA analysis if needed
- Adjust frequency discretization

## Best Practices

1. **Always validate** before using in OrcaFlex
2. **Review frequency coverage** for your wave spectrum
3. **Check heading coverage** for your environmental conditions
4. **Inspect Excel workbook** visually for anomalies
5. **Keep validation report** for documentation

## Next Steps

- **Tutorial 03**: [Comparing AQWA vs OrcaWave Results](03_comparison_example.md)
- **Tutorial 04**: [Batch Processing Multiple Vessels](04_batch_processing.md)

---

**Previous**: [Getting Started](01_getting_started.md)
**Next**: [Comparison Example](03_comparison_example.md)
