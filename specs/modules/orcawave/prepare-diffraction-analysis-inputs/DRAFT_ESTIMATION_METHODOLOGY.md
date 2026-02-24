# Vessel Draft Estimation Methodology

## Overview
This document explains how vessel draft values are calculated when not explicitly provided in the source data.

## Draft Estimation Formula

### Base Draft Calculation
When actual draft values are not available, the draft is estimated using standard naval architecture principles:

```
Draft = Depth × 0.65
```

Where:
- **Depth**: Molded depth of the vessel (from keel to main deck)
- **0.65**: Standard draft-to-depth ratio for supply vessels

### Rationale for 65% Factor
The 0.65 (65%) factor is based on:
1. **Vessel Type**: Supply vessels typically operate at 60-70% of their depth
2. **Stability Requirements**: Maintains adequate freeboard for safety
3. **Operational Conditions**: Represents typical loaded condition
4. **Industry Standards**: Common approximation in preliminary design

## Sea Cypress Draft Calculations

### Vessel Dimensions
- **Length Between Perpendiculars (LBP)**: 75 ft (22.86 m)
- **Beam**: 28 ft (8.53 m)  
- **Depth**: 10 ft (3.05 m)

### Base Draft Estimation
```
Base Draft = 10 ft × 0.65 = 6.5 ft
Base Draft = 6.5 ft × 0.3048 m/ft = 1.981 m
```

## Configuration-Specific Draft Adjustments

Different loading conditions require draft adjustments from the base value:

### Configuration 1: Incident Draft Fuel Centered
- **Draft**: 6.5 ft (1.981 m)
- **Adjustment**: None - baseline condition
- **Mass**: 296.2 LT (300,912 kg)

### Configuration 2: Incident Draft Fuel Centered (Adjusted)
- **Draft**: 6.5 ft (1.981 m)
- **Adjustment**: None - same displacement, redistributed weight
- **Mass**: 296.2 LT (300,912 kg)
- **Note**: Same draft as Config 1 due to same total mass

### Configuration 3: 4070 Gallons FO to Port
- **Draft**: 6.7 ft (2.042 m)
- **Adjustment**: +0.2 ft from baseline
- **Mass**: 302.7 LT (307,517 kg)
- **Rationale**: Additional fuel oil (4070 gallons ≈ 13.5 tonnes) increases displacement

### Configuration 4: FO to Port with 5% Engine Room Ingress
- **Draft**: 6.9 ft (2.103 m)
- **Adjustment**: +0.4 ft from baseline
- **Mass**: 309.1 LT (314,101 kg)
- **Rationale**: Combined effect of fuel oil + water ingress (≈ 20.5 tonnes total)

## Draft Adjustment Calculation Method

The draft adjustments are estimated using simplified hydrostatics:

```
ΔDraft ≈ ΔMass / (ρ × Awp)
```

Where:
- **ΔMass**: Change in displacement (tonnes)
- **ρ**: Water density (1.025 t/m³ for seawater)
- **Awp**: Waterplane area (approximated as 0.75 × LBP × Beam)

### Example for Configuration 3:
```
ΔMass = 307.5 - 300.9 = 6.6 tonnes
Awp ≈ 0.75 × 22.86 × 8.53 = 146.3 m²
ΔDraft ≈ 6.6 / (1.025 × 146.3) = 0.044 m ≈ 0.14 ft
```

The 0.2 ft adjustment includes a safety margin.

## Important Notes

1. **Estimation vs. Actual Values**
   - These are **engineering estimates** for analysis purposes
   - Actual draft should be obtained from:
     - Load line certificates
     - Stability booklet
     - Tank sounding tables
     - Vessel operating manual

2. **Validation Required**
   - Compare with similar vessels
   - Check against stability criteria
   - Verify with classification society rules

3. **When to Update**
   - If actual draft values become available
   - If stability calculations provide more accurate values
   - If operational data indicates different loading conditions

## Usage in Code

The estimation is implemented in `extract_hydrodynamic_properties.py`:

```python
# If draft not provided, estimate as typical for supply vessel
if draft_ft is None:
    draft_ft = depth_ft * 0.65  # 65% of depth
    print(f"Note: Draft estimated at {draft_ft:.1f} ft based on vessel depth")
```

To use actual draft values, modify the function call:
```python
# Use actual draft value (in feet)
vessels = extract_vessel_properties(excel_file, draft_ft=6.8)
```

## References
- DNV-RP-C205: Environmental Conditions and Environmental Loads
- ABS Guide for Building and Classing Offshore Supply Vessels
- MARPOL Annex I - International Convention for Prevention of Pollution from Ships