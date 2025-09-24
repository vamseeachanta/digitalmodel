# Tension to Stress Transformation - Step-by-Step Validation Guide

## Table of Contents
1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [Validation Steps](#validation-steps)
4. [Detailed Validation Procedures](#detailed-validation-procedures)
5. [Automated Testing](#automated-testing)
6. [Manual Verification](#manual-verification)
7. [Troubleshooting](#troubleshooting)
8. [Validation Checklist](#validation-checklist)

## Overview

This document provides comprehensive validation procedures for the tension-to-stress transformation module following repository standard practices. The validation ensures accurate conversion of tension ranges (kN) to stress ranges (MPa) using lookup table interpolation.

## Simple Example: Like Converting Toy Cars to Real Cars! 🚗

Imagine you have a collection of toy cars (tension values) and you want to know how big they would be as real cars (stress values). We use a magic conversion book (lookup table) to help us!

## Complete Worked Example - Step by Step! 📚

### The Story: Converting Push Force to Material Stress

Let's follow a simple example where we convert a pushing force of 100 kN (like pushing a heavy box) into stress on a metal beam!

### Step 1: What We Start With (Input Data)

**Our Input File:** `fsts_l015_125km3_l100_pb_FC001_Strut1_rainflow.csv`

```
Range (kN)    | Cycles_Annual  | What it means
------------- | -------------- | --------------------------------
100           | 14,132,800     | Small push happens 14 million times
300           | 0              | Medium push never happens  
500           | 0              | Big push never happens
700           | 0              | Huge push never happens
```

**Think of it like:** Counting how many times you push a swing - mostly gentle pushes, rarely hard pushes!

### Step 2: Our Magic Conversion Book (Lookup Table)

**File:** `tension_range_to_stress_range_function.csv`

For Location 2 (like a specific spot on the beam):
```
Tension (kN) | Stress (MPa) | Think of it as...
------------ | ------------ | -----------------
0            | 0.00         | No push = No stress
100          | 1.03         | Small push = Small stress
200          | 2.05         | Double push = Double stress
300          | 3.08         | Triple push = Triple stress
```

**It's like a recipe:** If 1 cup of flour makes 2 cookies, then 2 cups make 4 cookies!

### Step 3: The Conversion Process (What Actually Happens)

```python
# Our computer reads the input
input_tension = 100  # kN (our pushing force)

# Looks in the conversion book for Location 2
# Finds: 100 kN → 1.03 MPa

# Writes the answer
output_stress = 1.03  # MPa (the stress on the metal)
```

**Visual Example:**
```
INPUT FILE                 LOOKUP TABLE              OUTPUT FILE
[100 kN] ----look up----→ [100→1.03] ----write----→ [1.03 MPa]
```

### Step 4: What We Get (Output Files)

For our input file, we create 7 different output files (one for each location on the beam):

```
Original: fsts_l015_125km3_l100_pb_FC001_Strut1_rainflow.csv
          ↓
Creates 7 files:
├── ...FC001_Strut1_loc02_stress_rainflow.csv (Location 2)
├── ...FC001_Strut1_loc03_stress_rainflow.csv (Location 3)
├── ...FC001_Strut1_loc05_stress_rainflow.csv (Location 5)
├── ...FC001_Strut1_loc06_stress_rainflow.csv (Location 6)
├── ...FC001_Strut1_loc07_stress_rainflow.csv (Location 7)
├── ...FC001_Strut1_loc09_stress_rainflow.csv (Location 9)
└── ...FC001_Strut1_loc10_stress_rainflow.csv (Location 10)
```

### Step 5: Let's Check One Output File!

**File:** `fsts_l015_125km3_l100_pb_FC001_Strut1_loc02_stress_rainflow.csv`

```
stress range (Mpa) | Cycles_Annual  | What happened
------------------ | -------------- | -------------
1.03              | 14,132,800     | 100 kN became 1.03 MPa!
3.08              | 0              | 300 kN became 3.08 MPa
5.13              | 0              | 500 kN became 5.13 MPa
```

### The Complete Journey - Visual Map 🗺️

```
1. START: We have force data (how hard we push)
          ↓
2. LOOK UP: Find the matching stress for each force
          ↓
3. CALCULATE: If exact match not found, calculate between values
          ↓
4. WRITE: Save the stress values to new files
          ↓
5. DONE: We now know the stress on the metal!
```

### Real Example with Numbers

Let's trace one value through the entire process:

**Step-by-Step Calculation:**
```
1. Read input:    100 kN with 14,132,800 cycles
2. Find location: Location 2
3. Look up:       100 kN → ? MPa
4. Find in table: 100 kN = 1.025811034 MPa (exact match!)
5. Write output:  1.025811034 MPa with 14,132,800 cycles
```

**What if the value is between table entries?**

Example: We need 150 kN but table only has 100 kN and 200 kN:
```
100 kN → 1.03 MPa
150 kN → ? (we need to calculate!)
200 kN → 2.05 MPa

Math (like finding the middle):
150 is halfway between 100 and 200
So stress is halfway between 1.03 and 2.05
Answer: 1.54 MPa!
```

### Validation Check - Making Sure It Works! ✅

We check our work like checking math homework:

```python
# Test Case: Convert 100 kN at Location 2
input_value = 100  # kN
expected_output = 1.03  # MPa (from our table)

# Run the conversion
actual_output = transform(input_value, location=2)

# Check if it matches
if actual_output == expected_output:
    print("✅ PASS! The conversion works!")
else:
    print("❌ FAIL! Something went wrong!")
```

### Try It Yourself! 🎮

Run this command to see it work:
```bash
# Step 1: Check what files we have
dir specs\modules\data-transformation\lookup-table\data\

# Step 2: Run the transformation
python specs\modules\data-transformation\lookup-table\test_process_transformation.py

# Step 3: Check the output
dir specs\modules\data-transformation\lookup-table\output\

# Step 4: Look at one result
type specs\modules\data-transformation\lookup-table\output\*loc02*.csv | head -5
```

### Common Questions (Like an 8-Year-Old Would Ask!)

**Q: Why do we need 7 different files?**
A: Imagine a seesaw - the stress is different at different spots. Each location (2, 3, 5, 6, 7, 9, 10) is like a different spot on the seesaw!

**Q: What if our number isn't in the table?**
A: We use math to guess! If we know 10→20 and 20→40, then 15 would be 30 (right in the middle).

**Q: Why do some values have 0 cycles?**
A: It means that push strength never happens - like you never push the swing SUPER hard!

**Q: How do we know it worked?**
A: We check that:
- All files were created ✅
- Numbers are positive ✅  
- Cycles stay the same ✅
- Stress increases when force increases ✅

## Quick Start

### Interactive Validation (Recommended)
```bash
# Run full interactive validation with user confirmations
python specs/modules/data-transformation/lookup-table/interactive_validation.py

# Run automated validation (no prompts)
python specs/modules/data-transformation/lookup-table/interactive_validation.py --auto

# Run specific validation step only
python specs/modules/data-transformation/lookup-table/interactive_validation.py --step 3
```

### Quick Test
```bash
# Run transformation test with validation
python specs/modules/data-transformation/lookup-table/run_test.py

# Dry run to preview without creating files
python specs/modules/data-transformation/lookup-table/dry_run_test.py
```

## Validation Steps

### Step 1: Input File Validation
Validates the structure and integrity of input data files.

### Step 2: Lookup Table Validation
Verifies the lookup table format, data ranges, and monotonicity.

### Step 3: Transformation Logic Validation
Tests interpolation accuracy and config mapping.

### Step 4: Output Generation Validation
Confirms output directory setup and file naming patterns.

### Step 5: End-to-End Test
Performs complete transformation with result verification.

## Detailed Validation Procedures

### Step 1: Input File Validation

#### 1.1 Configuration File Structure
**File:** `inputs/test_transformation_config.yaml`

**Verify:**
- [ ] Configuration file exists and is valid YAML/JSON
- [ ] Contains required sections: `raw_data`, `transformation`, `output`
- [ ] All paths are correctly specified

**Manual Check:**
```python
import yaml
with open('inputs/test_transformation_config.yaml', 'r') as f:
    config = yaml.safe_load(f)
    assert 'raw_data' in config
    assert 'transformation' in config
    assert 'output' in config
```

#### 1.2 Input Data Files
**Directory:** `data/`

**Verify:**
- [ ] Input CSV files exist matching pattern `*_FC*_Strut*_rainflow.csv`
- [ ] Required columns present: `Range (kN)`, `Cycles_Annual`
- [ ] Data types are numeric and valid
- [ ] No negative tension values

**Example Validation:**
```python
import pandas as pd
import glob

# Check input files
input_files = glob.glob('data/*_FC*_Strut*_rainflow.csv')
assert len(input_files) > 0, "No input files found"

# Validate structure
for file in input_files:
    df = pd.read_csv(file)
    assert 'Range (kN)' in df.columns
    assert 'Cycles_Annual' in df.columns
    assert df['Range (kN)'].min() >= 0
```

### Step 2: Lookup Table Validation

#### 2.1 Table Structure
**File:** `inputs/tension_range_to_stress_range_function.csv`

**Verify:**
- [ ] File exists and is readable
- [ ] Contains columns: `location ID`, `config`, `tension range (kN)`, `stress range (Mpa)`
- [ ] Location IDs: [2, 3, 5, 6, 7, 9, 10]
- [ ] Configs include variations with/without `_mwl` suffix

**Example Check:**
```python
lookup_df = pd.read_csv('inputs/tension_range_to_stress_range_function.csv')
lookup_df.columns = lookup_df.columns.str.strip()
lookup_df['config'] = lookup_df['config'].str.strip()

# Verify location IDs
expected_locations = [2, 3, 5, 6, 7, 9, 10]
actual_locations = sorted(lookup_df['location ID'].unique())
assert actual_locations == expected_locations
```

#### 2.2 Data Integrity
**Verify:**
- [ ] Tension ranges are non-negative and increasing
- [ ] Stress values are non-negative
- [ ] Monotonic relationship (stress increases with tension)

**Monotonicity Test:**
```python
for loc_id in lookup_df['location ID'].unique():
    for config in lookup_df['config'].unique():
        subset = lookup_df[
            (lookup_df['location ID'] == loc_id) & 
            (lookup_df['config'] == config)
        ].sort_values('tension range (kN)')
        
        if len(subset) > 1:
            assert subset['stress range (Mpa)'].is_monotonic_increasing, \
                f"Non-monotonic for location {loc_id}, config {config}"
```

### Step 3: Transformation Logic Validation

#### 3.1 Interpolation Accuracy
**Test Linear Interpolation:**

```python
from scipy import interpolate
import numpy as np

# Select test data
test_location = 2
test_config = 'fsts_l015_125km3_l100_pb_mwl'

subset = lookup_df[
    (lookup_df['location ID'] == test_location) & 
    (lookup_df['config'] == test_config)
]

# Create interpolator
x = subset['tension range (kN)'].values
y = subset['stress range (Mpa)'].values
f = interpolate.interp1d(x, y, kind='linear', fill_value='extrapolate')

# Test exact points
for xi, yi in zip(x[:5], y[:5]):
    yi_calc = f(xi)
    assert abs(yi_calc - yi) < 1e-6, f"Interpolation error at {xi}"

# Test midpoint
x_mid = (x[0] + x[1]) / 2
y_mid_expected = (y[0] + y[1]) / 2
y_mid_calc = f(x_mid)
assert abs(y_mid_calc - y_mid_expected) < 1e-6
```

#### 3.2 Config Mapping
**Verify Mappings:**
```python
config_mapping = {
    "fsts_l015_125km3_l100_pb": "fsts_l015_125km3_l100_pb_mwl",
    "fsts_l095_125km3_l000_pb": "fsts_l095_125km3_l000_pb_mwl",
    "fsts_l015": "fsts_l015_mwl",
    "fsts_l095": "fsts_l095_mwl"
}

# Test each mapping
for input_config, expected_output in config_mapping.items():
    # Verify mapping exists in lookup table
    assert expected_output in lookup_df['config'].values
```

### Step 4: Output Generation Validation

#### 4.1 Output Directory
**Verify:**
- [ ] Output directory exists or can be created
- [ ] Write permissions available
- [ ] Sufficient disk space

```python
from pathlib import Path
import os

output_dir = Path('specs/modules/data-transformation/lookup-table/output')
output_dir.mkdir(parents=True, exist_ok=True)
assert output_dir.exists()
assert os.access(output_dir, os.W_OK)
```

#### 4.2 File Naming Pattern
**Expected Pattern:** `{config}_FC{fc_number}_Strut{strut_number}_loc{location_id:02d}_stress_rainflow.csv`

**Test Examples:**
```python
test_cases = [
    ("fsts_l015_125km3_l100_pb", "001", "1", 2, 
     "fsts_l015_125km3_l100_pb_FC001_Strut1_loc02_stress_rainflow.csv"),
    ("fsts_l095", "010", "6", 10,
     "fsts_l095_FC010_Strut6_loc10_stress_rainflow.csv")
]

pattern = "{config}_FC{fc_number}_Strut{strut_number}_loc{location_id:02d}_stress_rainflow.csv"

for config, fc, strut, loc, expected in test_cases:
    actual = pattern.format(
        config=config, 
        fc_number=fc, 
        strut_number=strut, 
        location_id=loc
    )
    assert actual == expected
```

### Step 5: End-to-End Test

#### 5.1 Sample Transformation
**Process one file completely:**

```python
# Run transformation on first input file
from test_process_transformation import TensionToStressTransformer

transformer = TensionToStressTransformer('inputs/test_transformation_config.yaml')
transformer.run()

# Verify outputs
output_files = list(Path('output').glob('*_loc*_stress_rainflow.csv'))
assert len(output_files) == 21  # 3 input files × 7 locations
```

#### 5.2 Output Data Validation
**Verify each output file:**

```python
for output_file in output_files:
    df = pd.read_csv(output_file)
    
    # Check structure
    assert 'stress range (Mpa)' in df.columns
    assert 'Cycles_Annual' in df.columns
    
    # Check data validity
    assert df['stress range (Mpa)'].min() >= 0
    assert not df['stress range (Mpa)'].isnull().any()
    
    # Verify cycles preserved
    input_pattern = output_file.name.replace('_loc', '_FC').replace('_stress_rainflow.csv', '_rainflow.csv')
    # Compare with corresponding input file cycles
```

## Automated Testing

### Run Complete Test Suite
```bash
# Full automated validation
python interactive_validation.py --auto

# Generates validation_report.json with results
```

### Continuous Integration
```yaml
# .github/workflows/validation.yml
name: Validation Tests
on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Python
        uses: actions/setup-python@v2
      - name: Install dependencies
        run: pip install -r requirements.txt
      - name: Run validation
        run: python interactive_validation.py --auto
```

## Manual Verification

### Visual Inspection Checklist

1. **Input Files**
   - [ ] CSV files properly formatted
   - [ ] No corrupted data
   - [ ] Consistent column headers

2. **Lookup Table**
   - [ ] Smooth progression of values
   - [ ] No sudden jumps or discontinuities
   - [ ] Reasonable stress-tension relationships

3. **Output Files**
   - [ ] Correct number of files generated
   - [ ] File sizes reasonable
   - [ ] No empty or corrupted files

4. **Data Consistency**
   - [ ] Total cycles preserved
   - [ ] Stress values in expected range
   - [ ] Location-specific variations logical

### Spot Check Calculations

Select random data points and manually verify:

```python
# Manual calculation example
tension_input = 1000  # kN
location_id = 2
config = 'fsts_l015_125km3_l100_pb_mwl'

# Find bracketing points in lookup table
lower_point = lookup_df[...].iloc[i]
upper_point = lookup_df[...].iloc[i+1]

# Linear interpolation
ratio = (tension_input - lower_point['tension']) / (upper_point['tension'] - lower_point['tension'])
stress_expected = lower_point['stress'] + ratio * (upper_point['stress'] - lower_point['stress'])

# Compare with actual output
```

## Troubleshooting

### Common Issues and Solutions

#### Issue 1: Config Not Found in Lookup Table
**Symptom:** "No interpolator for location X, config Y"

**Solution:**
1. Check config mapping in configuration file
2. Verify lookup table configs (may have spaces)
3. Update config_mapping section

```python
# Debug config issues
lookup_df['config'].str.strip().unique()  # Check actual configs
```

#### Issue 2: Output Files Not Created
**Symptom:** No output files after running transformation

**Solution:**
1. Check output directory permissions
2. Verify input file pattern matches
3. Check log file for errors

```bash
# Check permissions
ls -la specs/modules/data-transformation/lookup-table/output/
# Check log
cat transformation_log.txt
```

#### Issue 3: Interpolation Errors
**Symptom:** ValueError in interpolation

**Solution:**
1. Ensure lookup table is sorted by tension range
2. Check for duplicate tension values
3. Verify extrapolation settings

```python
# Fix sorting issues
subset = subset.sort_values('tension range (kN)').drop_duplicates('tension range (kN)')
```

## Validation Checklist

### Pre-Validation
- [ ] All required files present
- [ ] Python environment configured
- [ ] Dependencies installed (`scipy`, `pandas`, `numpy`, `pyyaml`)

### During Validation
- [ ] Step 1: Input files validated
- [ ] Step 2: Lookup table validated
- [ ] Step 3: Transformation logic validated
- [ ] Step 4: Output setup validated
- [ ] Step 5: End-to-end test passed

### Post-Validation
- [ ] Validation report generated
- [ ] All checkpoints passed
- [ ] Output files verified
- [ ] Documentation updated

### Sign-off
- [ ] Technical review completed
- [ ] Validation report archived
- [ ] Ready for production use

## Appendix: Command Reference

```bash
# Interactive validation
python interactive_validation.py

# Automated validation
python interactive_validation.py --auto

# Specific step validation
python interactive_validation.py --step 1  # Input validation
python interactive_validation.py --step 2  # Lookup table
python interactive_validation.py --step 3  # Transformation logic
python interactive_validation.py --step 4  # Output generation
python interactive_validation.py --step 5  # End-to-end test

# Test runners
python run_test.py                # Full test with validation
python dry_run_test.py            # Preview without file generation
python test_process_transformation.py  # Direct transformation

# With custom config
python interactive_validation.py --config path/to/config.yaml
```

---

## Validation Report Format

The validation generates a JSON report with the following structure:

```json
{
  "timestamp": "2025-09-24T10:30:00",
  "config_file": "inputs/test_transformation_config.yaml",
  "interactive": false,
  "summary": {
    "passed": 15,
    "failed": 0,
    "total": 15
  },
  "checkpoints": {
    "Config section 'raw_data'": {
      "status": "PASS",
      "message": "Configuration contains 'raw_data' section",
      "timestamp": "2025-09-24T10:30:01"
    },
    ...
  }
}
```

This report provides complete traceability for validation results.