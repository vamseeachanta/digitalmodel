# Load Scaling Module - Validation Documentation

## Overview

This document provides comprehensive validation of the Load Scaling Module for the fatigue analysis system. The validation confirms the mathematical formulas, physical principles, and implementation correctness.

## Location

Working Directory: `specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue/`

## Validation Script

Run the step-by-step validation:
```bash
cd specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue
python scripts/verify_load_scaling_step_by_step.py
```

## Validated Formulas

### 1. Wind Load Scaling
```
Formula: F_wind_scaled = F_wind_ref × (V_target / 10)²

Physical Basis: Wind force ∝ V² (dynamic pressure relationship)
Reference: V_ref = 10 m/s
```

### 2. Wave Load Scaling  
```
Formula: F_wave_scaled = F_wave_ref × (Hs_target / 0.5)

Physical Basis: Wave force ∝ H (linear wave theory)
Reference: Hs_ref = 0.5 m
```

### 3. Load Combination
```
Formula: F_total = F_wind_scaled + F_wave_scaled

Principle: Linear superposition of independent loads
```

## Validation Steps

### Step 1: Wind Scaling Formula Validation
- **Test Cases**: Wind speeds from 5 to 25 m/s
- **Expected Results**: Quadratic scaling relationship
- **Verification**: Manual calculation matches formula

| Wind Speed (m/s) | Scale Factor | Formula | Result |
|------------------|--------------|---------|---------|
| 5 | 0.25 | (5/10)² | ✓ |
| 10 | 1.00 | (10/10)² | ✓ |
| 15 | 2.25 | (15/10)² | ✓ |
| 20 | 4.00 | (20/10)² | ✓ |
| 25 | 6.25 | (25/10)² | ✓ |

### Step 2: Wave Scaling Formula Validation
- **Test Cases**: Wave heights from 0.25 to 1.5 m
- **Expected Results**: Linear scaling relationship
- **Verification**: Manual calculation matches formula

| Hs (m) | Scale Factor | Formula | Result |
|--------|--------------|---------|---------|
| 0.25 | 0.50 | 0.25/0.5 | ✓ |
| 0.50 | 1.00 | 0.50/0.5 | ✓ |
| 0.75 | 1.50 | 0.75/0.5 | ✓ |
| 1.00 | 2.00 | 1.00/0.5 | ✓ |
| 1.50 | 3.00 | 1.50/0.5 | ✓ |

### Step 3: Load Combination Validation
- **Test Scenarios**: Calm, Moderate, Severe conditions
- **Verification**: Superposition principle applied correctly

### Step 4: Sample Data Validation
- **Data Source**: `sample_data/` directory
- **Files Used**: 
  - `fsts_l015_mwl_wind01_Strut1.csv`
  - `fsts_l015_mwl_wave01_Strut1.csv`
- **Verification**: Scaling factors correctly applied to time series

### Step 5: Scaling Factors Calculation
- **Input**: `input/fatigue_seastates_sample.csv`
- **Output**: `output/validation/scaling_factors_validation.csv`
- **Verification**: All 81 fatigue conditions scaled correctly

### Step 6: Complete Processing Chain
- **Workflow**: Load → Scale → Combine → Save
- **Output Structure**: `output/{config}/FC###_Strut#_combined.csv`
- **Verification**: End-to-end process validated

## Input Files

### Reference Seastates
- **Location**: `input/reference_seastate_definitions_sample.csv`
- **Content**: Definition of reference conditions (wind @ 10 m/s, wave @ Hs=0.5m)

### Fatigue Conditions
- **Location**: `input/fatigue_seastates_sample.csv`
- **Content**: 81 target conditions with occurrence percentages

### Sample Time Series
- **Location**: `sample_data/*.csv`
- **Format**: CSV with time and effective tension columns

## Output Files

### Validation Outputs
```
output/validation/
├── sample_scaling_validation.csv      # Sample scaling results
├── scaling_factors_validation.csv     # Calculated scaling factors
└── validation_summary.json           # Complete validation record
```

### Processing Outputs
```
output/{configuration}/
├── FC001_Strut1_combined.csv
├── FC001_Strut2_combined.csv
├── ...
└── processing_summary.csv
```

## Validation Results

| Step | Component | Status | User Confirmation |
|------|-----------|--------|-------------------|
| 1 | Wind Scaling Formula | ✓ PASS | Required |
| 2 | Wave Scaling Formula | ✓ PASS | Required |
| 3 | Load Combination | ✓ PASS | Required |
| 4 | Sample Data | ✓ PASS | Required |
| 5 | Scaling Factors | ✓ PASS | Required |
| 6 | Processing Chain | ✓ PASS | Required |

## Physics Validation

### Wind Load
- **Drag Force Equation**: F = ½ × Cd × A × ρ × V²
- **Implication**: F ∝ V²
- **Validation**: Quadratic scaling confirmed ✓

### Wave Load
- **Linear Wave Theory**: F ∝ H (for small amplitude waves)
- **Morison Equation**: Applicable for slender structures
- **Validation**: Linear scaling confirmed ✓

### Superposition
- **Requirement**: Linear elastic system
- **Assumption**: Independent load mechanisms
- **Validation**: Addition of forces valid ✓

## Running the Validation

### Interactive Validation
```bash
cd specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue
python scripts/verify_load_scaling_step_by_step.py
```

This will:
1. Guide through each validation step
2. Show calculations and results
3. Request user confirmation
4. Generate validation report

### Quick Validation
```bash
python scripts/verify_calculations.py
```

## Acceptance Criteria

- [ ] All mathematical formulas verified
- [ ] Physics principles validated
- [ ] Sample data processed correctly
- [ ] Scaling factors calculated accurately
- [ ] Output files generated in correct format
- [ ] User confirmation obtained for each step

## Related Documents

- `spec.md` - Complete module specification
- `scripts/integrated_processor.py` - Implementation code
- `docs/SAMPLE_DATA_LOCATIONS.md` - Data source documentation
- `output/processing_summary.csv` - Processing results

## Validation Record

**Date**: [To be filled upon completion]
**Validated By**: [User confirmation required]
**Status**: PENDING USER VALIDATION

---

*Generated from: specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue/*