# Effective Tension Calculation Methodology

## Overview
This document explains how effective tension time traces are calculated by combining scaled wind and wave loads for fatigue analysis.

## Core Concept
The effective tension represents the total load on a strut from combined environmental effects:
```
Effective Tension = Scaled Wind Load + Scaled Wave Load
```

## Process Flow

### 1. Reference Seastate Selection
For each of the 81 fatigue conditions, we select:
- **Wind Reference**: One of 16 wind-only cases (WD01-WD16) at 10 m/s
- **Wave Reference**: One of 18 wave-only cases (W01-W18) at Hs = 0.5m

Selection is based on direction matching:
- Wind reference selected to match target wind direction
- Wave reference selected to match target wave direction

### 2. Scaling Factor Calculation
Scaling factors transform reference loads to target conditions:

#### Wind Scaling
```
Wind Scale Factor = (Target Wind Speed / 10 m/s)²
```
Example: For 14 m/s wind: (14/10)² = 1.96

#### Wave Scaling
```
Wave Scale Factor = Target Hs / 0.5 m
```
Example: For Hs = 0.8m: 0.8/0.5 = 1.6

### 3. Time Trace Scaling
Each reference time trace is multiplied by its scaling factor:
```python
scaled_wind_trace = wind_reference_trace * wind_scale_factor
scaled_wave_trace = wave_reference_trace * wave_scale_factor
```

### 4. Effective Tension Combination
The scaled traces are summed point-by-point:
```python
effective_tension = scaled_wind_trace + scaled_wave_trace
```

## Example Calculation

### Fatigue Condition 23 (Row 23)
- Wind: 18 m/s at 180°
- Wave: Hs = 0.8m at 150°
- Occurrence: 1.365%

#### Step 1: Select References
- Wind reference: WD05 (10 m/s at 180°)
- Wave reference: W08 (0.5m at 150°)

#### Step 2: Calculate Scaling
- Wind scale: (18/10)² = 3.24
- Wave scale: 0.8/0.5 = 1.6

#### Step 3: Load Reference Time Traces
```python
wind_trace = load_time_trace('WD05_S1.csv')  # Example: [100, 120, 110, ...] kN
wave_trace = load_time_trace('W08_S1.csv')   # Example: [50, 60, 55, ...] kN
```

#### Step 4: Apply Scaling
```python
scaled_wind = wind_trace * 3.24  # [324, 388.8, 356.4, ...] kN
scaled_wave = wave_trace * 1.6   # [80, 96, 88, ...] kN
```

#### Step 5: Combine
```python
effective_tension = scaled_wind + scaled_wave  # [404, 484.8, 444.4, ...] kN
```

## Output Format

Each effective tension file contains:
```csv
# Fatigue Condition: FC023
# Strut: S1
# Wind Speed: 18 m/s
# Significant Wave Height: 0.8 m
# Wind Scale Factor: 3.2400
# Wave Scale Factor: 1.6000
# Annual Occurrence: 1.365%
# Wind Reference: WD05
# Wave Reference: W08
time_s,effective_tension_kN
0.0,404.0
0.1,484.8
0.2,444.4
...
```

## Quality Assurance

### Verification Points
1. **Scaling Factor Check**: Verify factors match formulas
2. **Direction Matching**: Confirm appropriate references selected
3. **Summation**: Validate point-by-point addition
4. **Statistics**: Check reasonable tension ranges
5. **Occurrence Sum**: Ensure all 81 conditions sum to 100%

### Expected Ranges
- **Wind Scale Factors**: 0.016 to 9.0
- **Wave Scale Factors**: 0.02 to 2.86
- **Effective Tensions**: Typically 10-5000 kN

## Benefits of This Approach

1. **Physical Accuracy**: Represents actual combined loading
2. **Traceability**: Clear audit trail via scaling log
3. **Flexibility**: Easy to update if conditions change
4. **Validation**: Can verify against individual components

## Implementation Notes

- Time traces must have same length and time step
- Missing data handled via interpolation
- Negative tensions preserved (compression)
- Phase relationships maintained from original simulations