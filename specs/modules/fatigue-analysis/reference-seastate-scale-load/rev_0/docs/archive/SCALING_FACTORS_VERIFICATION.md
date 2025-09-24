# Fatigue Analysis Scaling Factors Verification

## Overview
This document provides pre-calculated scaling factors for all 81 fatigue conditions to facilitate verification of the fatigue analysis system.

## Reference Conditions
- **Wind Reference**: 10 m/s (from reference seastate simulations)
- **Wave Reference**: Hs = 0.5 m (from reference seastate simulations)

## Scaling Formulas

### Wind Load Scaling
Wind loads scale with the square of wind speed ratio:
```
Wind Scale Factor = (Target Wind Speed / Reference Wind Speed)²
Wind Scale Factor = (V_wind / 10)²
```

### Wave Load Scaling
Wave loads scale linearly with significant wave height ratio:
```
Wave Scale Factor = Target Hs / Reference Hs
Wave Scale Factor = Hs / 0.5
```

## Pre-calculated Scaling Factors
The file `fatigue_scaling_factors.csv` contains all 81 fatigue conditions with:
- **Row**: Fatigue condition number (1-81)
- **Wind Speed (m/s)**: Target wind speed from fatigue_seastates.csv
- **Hs (m)**: Target significant wave height from fatigue_seastates.csv
- **Wind Scale Factor**: Pre-calculated wind scaling factor
- **Wave Scale Factor**: Pre-calculated wave scaling factor
- **Wind Scale Formula**: Formula showing calculation
- **Wave Scale Formula**: Formula showing calculation
- **Occurrence (%)**: Annual occurrence percentage for weighting

## Key Observations

### Minimum and Maximum Scaling Factors

#### Wind Scaling
- **Minimum**: 0.015625 (for wind speed = 1.25 m/s)
- **Maximum**: 9.0 (for wind speed = 30 m/s)

#### Wave Scaling
- **Minimum**: 0.02 (for Hs = 0.01 m)
- **Maximum**: 2.86 (for Hs = 1.43 m)

### High Impact Conditions
Conditions with both high scaling factors and significant occurrence:

| Row | Wind Speed | Hs | Wind SF | Wave SF | Occurrence % |
|-----|------------|-----|---------|---------|--------------|
| 8   | 14 m/s     | 0.6m| 1.96    | 1.2     | 4.713        |
| 10  | 11 m/s     | 0.45m| 1.21   | 0.9     | 3.641        |
| 23  | 18 m/s     | 0.8m| 3.24    | 1.6     | 1.365        |

### Low Impact Conditions
Conditions with minimal scaling (potential for reduced loads):

| Row | Wind Speed | Hs | Wind SF | Wave SF | Occurrence % |
|-----|------------|-----|---------|---------|--------------|
| 5   | 1.25 m/s   | 0.01m| 0.016  | 0.02    | 5.171        |
| 11  | 1.25 m/s   | 0.02m| 0.016  | 0.04    | 3.639        |

## Usage in Fatigue Analysis

### Load Scaling Process
1. **Identify Reference Time Trace**: Select appropriate reference (wind or wave) based on dominant loading
2. **Apply Scaling Factor**: Multiply entire time trace by appropriate scaling factor
3. **Combine Scaled Loads**: For combined wind-wave conditions, process both and combine results

### Example Calculation
For Row 8 (Wind = 14 m/s, Hs = 0.6 m):
- If using wind reference: Scale by 1.96
- If using wave reference: Scale by 1.2
- Apply occurrence weight: 4.713%

### Verification Steps
1. Load reference time trace (e.g., 1000 kN peak load)
2. Apply scaling factor (e.g., × 1.96 for wind)
3. Verify scaled peak (should be ~1960 kN)
4. Apply rainflow counting to scaled trace
5. Weight results by occurrence percentage

## Quality Checks

### Scaling Factor Validation
- ✓ All wind scale factors follow (V/10)² formula
- ✓ All wave scale factors follow Hs/0.5 formula
- ✓ Sum of occurrence percentages = 100%
- ✓ Scaling factors cover realistic range (0.016 to 9.0)

### Critical Conditions
The following conditions have extreme scaling and should be carefully validated:

1. **Row 64**: Wind = 26 m/s (SF = 6.76)
2. **Row 73**: Wind = 26 m/s (SF = 6.76)
3. **Row 81**: Wind = 30 m/s (SF = 9.0) - Maximum wind scaling

These conditions represent severe weather events with low occurrence but high load amplification.

## Notes
- Scaling factors are applied to the entire time trace, preserving the load pattern shape
- The choice between wind or wave reference depends on the dominant loading mechanism
- Combined effects may require processing both wind and wave scaled traces separately