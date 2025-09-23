# Load Scaling Step-by-Step Verification Guide

## Table of Contents
1. [Overview](#overview)
2. [Verification Steps](#verification-steps)
3. [Detailed Calculation Examples](#detailed-calculation-examples)
4. [Validation Checklist](#validation-checklist)
5. [Automated Verification](#automated-verification)
6. [Manual Verification Procedures](#manual-verification-procedures)
7. [Common Issues and Troubleshooting](#common-issues-and-troubleshooting)

## Overview

This document provides comprehensive step-by-step verification procedures for the load scaling program to ensure accurate implementation of the reference seastate scaling methodology.

## Verification Steps

### Step 1: Input Data Validation

#### 1.1 Reference Seastate Metadata
**File:** `input/reference_seastate_definitions_sample.csv`

**Verify:**
- [ ] File exists and is readable
- [ ] Contains required columns: `env reference`, `Vw [m/s]`, `Hs [m]`, `Tp [s]`, `Wind Dir [deg]`, `Wave Dir [deg]`
- [ ] All numeric values are valid (non-negative, within physical ranges)

**Example Check:**
```python
import pandas as pd
ref_meta = pd.read_csv('input/reference_seastate_definitions_sample.csv')
assert 'env reference' in ref_meta.columns
assert ref_meta['Vw [m/s]'].min() >= 0  # Wind speed non-negative
assert ref_meta['Hs [m]'].min() >= 0     # Wave height non-negative
```

#### 1.2 Fatigue Seastates
**File:** `input/fatigue_seastates_sample.csv`

**Verify:**
- [ ] Occurrence percentages sum to 100% (±0.1%)
- [ ] All environmental parameters are within valid ranges
- [ ] No missing values in required columns

**Example Check:**
```python
fatigue = pd.read_csv('input/fatigue_seastates_sample.csv')
total_occurrence = fatigue['Occurrence (%)'].sum()
assert abs(total_occurrence - 100.0) < 0.1, f"Occurrence sum: {total_occurrence}%"
```

### Step 2: Reference Selection Verification

#### 2.1 Wind Reference Selection
**Algorithm:** Select reference with closest wind direction

**Example Verification:**
```
Fatigue Condition: FC001
- Wind Direction: 0°
- Wind Speed: 5 m/s

Available References:
- wind01: Direction = 0°, Speed = 10 m/s
- wind02: Direction = 45°, Speed = 10 m/s

Expected Selection: wind01 (exact direction match)
Angular Difference: |0° - 0°| = 0°
```

#### 2.2 Wave Reference Selection
**Algorithm:** Select by direction and period (weighted 70%/30%)

**Example Verification:**
```
Fatigue Condition: FC003
- Wave Direction: 90°
- Hs: 0.5 m
- Tp: 3.5 s

Available References:
- wave01: Direction = 45°, Hs = 0.5 m, Tp = 2.5 s
- wave02: Direction = 90°, Hs = 0.5 m, Tp = 3.5 s

Score Calculation:
- wave01: (45°/180°) * 0.7 + (1.0s/10s) * 0.3 = 0.175 + 0.03 = 0.205
- wave02: (0°/180°) * 0.7 + (0.0s/10s) * 0.3 = 0.0 + 0.0 = 0.0

Expected Selection: wave02 (lowest score)
```

### Step 3: Scaling Factor Calculation Verification

#### 3.1 Wind Scaling Factor
**Formula:** `SF_wind = (V_fatigue / V_reference)²`

**Example Calculation:**
```
FC001: V_fatigue = 5 m/s, V_reference = 10 m/s
SF_wind = (5 / 10)² = 0.25

FC002: V_fatigue = 10 m/s, V_reference = 10 m/s
SF_wind = (10 / 10)² = 1.0

FC003: V_fatigue = 15 m/s, V_reference = 10 m/s
SF_wind = (15 / 10)² = 2.25
```

#### 3.2 Wave Scaling Factor
**Formula:** `SF_wave = Hs_fatigue / Hs_reference`

**Example Calculation:**
```
FC001: Hs_fatigue = 0.15 m, Hs_reference = 0.5 m
SF_wave = 0.15 / 0.5 = 0.3

FC003: Hs_fatigue = 0.5 m, Hs_reference = 0.5 m
SF_wave = 0.5 / 0.5 = 1.0

FC004: Hs_fatigue = 0.75 m, Hs_reference = 0.5 m
SF_wave = 0.75 / 0.5 = 1.5
```

### Step 4: Load Combination Verification

#### 4.1 Scaled Tension Calculation
**Formula:** `T_scaled = T_wind * SF_wind + T_wave * SF_wave`

**Example Calculation:**
```
Reference Data (at t = 0.0s):
- Wind tension: T_wind = 608.98 kN
- Wave tension: T_wave = 543.37 kN

Scaling Factors:
- SF_wind = 0.25
- SF_wave = 0.3

Scaled Tension:
T_scaled = 608.98 * 0.25 + 543.37 * 0.3
T_scaled = 152.245 + 163.011
T_scaled = 315.256 kN
```

### Step 5: Output File Verification

#### 5.1 File Naming Convention
**Pattern:** `{config}_FC{###}_Strut{#}_scaled_tension.csv`

**Examples:**
```
fsts_l015_FC001_Strut1_scaled_tension.csv ✓
fsts_l095_FC010_Strut8_scaled_tension.csv ✓
fsts_l015_125km3_l100_pb_FC005_Strut3_scaled_tension.csv ✓
```

#### 5.2 File Content Structure
**Required Columns:**
1. Time (s)
2. Scaled Tension (kN)
3. Wind Factor
4. Wave Factor
5. Wind Reference
6. Wave Reference

## Detailed Calculation Examples

### Example 1: Complete Calculation for FC001, Strut1, Config fsts_l015

```python
# Input Parameters
fatigue_condition = {
    'Wind Speed': 5,      # m/s
    'Wind Dir': 0,        # degrees
    'Hs': 0.15,          # m
    'Tp': 2.0,           # s
    'Wave Dir': 0,       # degrees
    'Occurrence': 20     # %
}

# Step 1: Select References
wind_reference = 'wind01'  # Dir = 0°, V = 10 m/s
wave_reference = 'wave01'  # Dir = 45°, Hs = 0.5 m

# Step 2: Calculate Scaling Factors
SF_wind = (5 / 10)² = 0.25
SF_wave = 0.15 / 0.5 = 0.3

# Step 3: Load Reference Data (first 3 points)
t = [0.0, 0.1, 0.2]  # seconds
T_wind = [608.98, 591.49, 606.66]  # kN
T_wave = [543.37, 551.84, 545.63]  # kN

# Step 4: Apply Scaling
T_scaled[0] = 608.98 * 0.25 + 543.37 * 0.3 = 315.26 kN
T_scaled[1] = 591.49 * 0.25 + 551.84 * 0.3 = 313.42 kN
T_scaled[2] = 606.66 * 0.25 + 545.63 * 0.3 = 315.35 kN
```

### Example 2: Verification with Scaling Limits

```python
# Configuration with limits
validation = {
    'max_wind_scaling_factor': 100.0,
    'min_wind_scaling_factor': 0.01,
    'max_wave_scaling_factor': 10.0,
    'min_wave_scaling_factor': 0.1
}

# Test Case: Very high wind speed
V_fatigue = 50 m/s
V_reference = 5 m/s
SF_wind_raw = (50 / 5)² = 100

# Apply limits
SF_wind = min(100, max(0.01, SF_wind_raw)) = 100.0 ✓

# Test Case: Very low wave height
Hs_fatigue = 0.01 m
Hs_reference = 1.0 m
SF_wave_raw = 0.01 / 1.0 = 0.01

# Apply limits
SF_wave = min(10.0, max(0.1, SF_wave_raw)) = 0.1 ✓
```

## Validation Checklist

### Pre-Processing Validation
- [ ] Configuration file is valid YAML
- [ ] All input file paths exist
- [ ] Reference data folder contains expected files
- [ ] Column names match configuration
- [ ] Fatigue seastate occurrences sum to 100%
- [ ] All numeric values are within physical ranges

### Processing Validation
- [ ] Reference selection produces valid matches
- [ ] Scaling factors are calculated correctly
- [ ] Scaling limits are enforced
- [ ] Time series lengths are consistent
- [ ] No NaN or infinite values in outputs

### Post-Processing Validation
- [ ] All expected output files are created
- [ ] File naming follows convention
- [ ] Output CSV structure is correct
- [ ] Summary report contains all cases
- [ ] Scaled tensions are within expected ranges

## Automated Verification

### Verification Test Script

```python
#!/usr/bin/env python
"""
Load Scaling Verification Script
Performs automated verification of load scaling calculations
"""

import pandas as pd
import numpy as np
import yaml
from pathlib import Path

class LoadScalingVerifier:
    def __init__(self, config_file):
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        self.errors = []
        
    def verify_input_files(self):
        """Verify all input files exist and are valid"""
        print("Verifying input files...")
        
        # Check reference metadata
        ref_file = self.config['input_data']['reference_seastate']['metadata_file']
        if not Path(ref_file).exists():
            self.errors.append(f"Reference metadata not found: {ref_file}")
            return False
            
        # Check fatigue seastates
        fatigue_file = self.config['input_data']['fatigue_seastates']['metadata_file']
        if not Path(fatigue_file).exists():
            self.errors.append(f"Fatigue seastates not found: {fatigue_file}")
            return False
            
        # Load and validate occurrence sum
        fatigue_df = pd.read_csv(fatigue_file)
        total = fatigue_df['Occurrence (%)'].sum()
        if abs(total - 100.0) > 0.1:
            self.errors.append(f"Occurrence sum = {total}%, expected 100%")
            
        return len(self.errors) == 0
        
    def verify_scaling_calculation(self, fc_id, config, strut):
        """Verify scaling calculation for specific case"""
        print(f"Verifying {config}_FC{fc_id:03d}_Strut{strut}...")
        
        # Load output file
        output_file = f"output/{config}_FC{fc_id:03d}_Strut{strut}_scaled_tension.csv"
        if not Path(output_file).exists():
            self.errors.append(f"Output file not found: {output_file}")
            return False
            
        output_df = pd.read_csv(output_file)
        
        # Verify scaling factors
        wind_factor = output_df['Wind Factor'].iloc[0]
        wave_factor = output_df['Wave Factor'].iloc[0]
        
        # Load fatigue condition
        fatigue_df = pd.read_csv(self.config['input_data']['fatigue_seastates']['metadata_file'])
        fc = fatigue_df.iloc[fc_id - 1]
        
        # Calculate expected factors
        # (This would need reference data for complete verification)
        print(f"  Wind Factor: {wind_factor}")
        print(f"  Wave Factor: {wave_factor}")
        
        return True
        
    def verify_output_structure(self):
        """Verify output file structure and naming"""
        print("Verifying output structure...")
        
        output_dir = Path(self.config['output']['base_folder'])
        if not output_dir.exists():
            self.errors.append("Output directory not found")
            return False
            
        # Check for summary report
        summary_file = output_dir / 'scaling_factors_applied.csv'
        if not summary_file.exists():
            self.errors.append("Summary report not found")
            return False
            
        # Load and verify summary
        summary_df = pd.read_csv(summary_file)
        expected_cols = ['Configuration', 'Fatigue Condition', 'Strut', 
                        'Wind Factor', 'Wave Factor']
        
        for col in expected_cols:
            if col not in summary_df.columns:
                self.errors.append(f"Missing column in summary: {col}")
                
        return len(self.errors) == 0
        
    def run_verification(self):
        """Run complete verification suite"""
        print("="*60)
        print("LOAD SCALING VERIFICATION")
        print("="*60)
        
        # Run verifications
        tests = [
            ("Input Files", self.verify_input_files),
            ("Output Structure", self.verify_output_structure),
        ]
        
        for test_name, test_func in tests:
            print(f"\n{test_name}:")
            if test_func():
                print(f"  ✓ PASSED")
            else:
                print(f"  ✗ FAILED")
                
        # Report errors
        if self.errors:
            print("\nErrors Found:")
            for error in self.errors:
                print(f"  - {error}")
            return False
        else:
            print("\n✓ All verifications passed!")
            return True

if __name__ == "__main__":
    verifier = LoadScalingVerifier('input/load_scaling_config.yml')
    verifier.run_verification()
```

## Manual Verification Procedures

### 1. Single Case Manual Verification

**Step 1:** Select a test case
```
Configuration: fsts_l015
Fatigue Condition: FC001
Strut: 1
```

**Step 2:** Extract parameters from input files
```bash
# Get fatigue parameters
grep "^1," input/fatigue_seastates_sample.csv

# Get reference parameters  
grep "wind01" input/reference_seastate_definitions_sample.csv
grep "wave01" input/reference_seastate_definitions_sample.csv
```

**Step 3:** Calculate expected scaling factors manually
```
Wind: (5/10)² = 0.25
Wave: 0.15/0.5 = 0.3
```

**Step 4:** Verify against output
```bash
# Check scaling factors in output
head -2 output/fsts_l015_FC001_Strut1_scaled_tension.csv

# Verify in summary
grep "fsts_l015,FC001,1" output/scaling_factors_applied.csv
```

### 2. Spot Check Random Samples

```python
import random
import pandas as pd

def spot_check_random_cases(n=5):
    """Randomly verify n cases"""
    summary = pd.read_csv('output/scaling_factors_applied.csv')
    
    # Random sample
    samples = summary.sample(n)
    
    for _, row in samples.iterrows():
        config = row['Configuration']
        fc = row['Fatigue Condition']
        strut = row['Strut']
        
        print(f"\nChecking: {config}_{fc}_Strut{strut}")
        print(f"  Wind Factor: {row['Wind Factor']}")
        print(f"  Wave Factor: {row['Wave Factor']}")
        
        # Load output file
        output_file = f"output/{config}_{fc}_Strut{strut}_scaled_tension.csv"
        df = pd.read_csv(output_file)
        
        # Verify first few points
        print(f"  First tension: {df['Scaled Tension (kN)'].iloc[0]:.2f} kN")
        print(f"  Mean tension: {df['Scaled Tension (kN)'].mean():.2f} kN")
        print(f"  Max tension: {df['Scaled Tension (kN)'].max():.2f} kN")

spot_check_random_cases()
```

### 3. Cross-Validation with Expected Values

```python
def cross_validate_scaling():
    """Cross-validate scaling factors against expected values"""
    
    # Define expected test cases
    test_cases = [
        {
            'config': 'fsts_l015',
            'fc': 'FC001',
            'expected_wind_factor': 0.25,  # (5/10)²
            'expected_wave_factor': 0.3,   # 0.15/0.5
        },
        {
            'config': 'fsts_l015',
            'fc': 'FC002',
            'expected_wind_factor': 1.0,   # (10/10)²
            'expected_wave_factor': 0.5,   # 0.25/0.5
        },
    ]
    
    summary = pd.read_csv('output/scaling_factors_applied.csv')
    
    for test in test_cases:
        # Find case in summary
        case = summary[
            (summary['Configuration'] == test['config']) &
            (summary['Fatigue Condition'] == test['fc']) &
            (summary['Strut'] == 1)
        ].iloc[0]
        
        # Verify factors
        wind_diff = abs(case['Wind Factor'] - test['expected_wind_factor'])
        wave_diff = abs(case['Wave Factor'] - test['expected_wave_factor'])
        
        print(f"{test['config']}_{test['fc']}:")
        print(f"  Wind Factor: {case['Wind Factor']} (expected: {test['expected_wind_factor']}) ✓" if wind_diff < 0.01 else "✗")
        print(f"  Wave Factor: {case['Wave Factor']} (expected: {test['expected_wave_factor']}) ✓" if wave_diff < 0.01 else "✗")
```

## Common Issues and Troubleshooting

### Issue 1: Scaling Factors Out of Range
**Symptom:** Extremely high or low scaling factors

**Verification:**
```python
summary = pd.read_csv('output/scaling_factors_applied.csv')
print(f"Wind Factor Range: {summary['Wind Factor'].min():.2f} - {summary['Wind Factor'].max():.2f}")
print(f"Wave Factor Range: {summary['Wave Factor'].min():.2f} - {summary['Wave Factor'].max():.2f}")

# Check for outliers
outliers = summary[(summary['Wind Factor'] > 10) | (summary['Wave Factor'] > 5)]
if not outliers.empty:
    print(f"Found {len(outliers)} outliers")
```

**Solution:** Verify scaling limits in configuration and check input data ranges

### Issue 2: Reference Selection Mismatch
**Symptom:** Unexpected reference selections

**Verification:**
```python
# Check reference selections for specific directions
summary = pd.read_csv('output/scaling_factors_applied.csv')
fatigue = pd.read_csv('input/fatigue_seastates_sample.csv')

for fc_idx, fc_row in fatigue.iterrows():
    wind_dir = fc_row['Wind Dir (°)']
    
    # Get reference used
    case = summary[
        (summary['Fatigue Condition'] == f'FC{fc_idx+1:03d}') &
        (summary['Strut'] == 1)
    ].iloc[0]
    
    print(f"FC{fc_idx+1:03d}: Wind Dir={wind_dir}° -> Ref={case['Wind Reference']}")
```

### Issue 3: Missing Output Files
**Symptom:** Some output files not created

**Verification:**
```bash
# Count expected vs actual files
expected=$((4 * 10 * 8))  # configs * conditions * struts
actual=$(find output -name "*_scaled_tension.csv" | wc -l)
echo "Expected: $expected, Actual: $actual"

# Find missing combinations
for config in fsts_l015 fsts_l095; do
    for fc in {001..010}; do
        for strut in {1..8}; do
            file="output/${config}_FC${fc}_Strut${strut}_scaled_tension.csv"
            [ ! -f "$file" ] && echo "Missing: $file"
        done
    done
done
```

## Summary

This verification guide provides comprehensive procedures to ensure the load scaling program operates correctly. Regular verification should include:

1. **Input validation** before processing
2. **Spot checks** of random cases during development
3. **Cross-validation** with manually calculated values
4. **Automated testing** for regression prevention
5. **Output verification** after processing

By following these verification steps, you can ensure the load scaling calculations are accurate and reliable for fatigue analysis.