# Folder Naming Standardization Proposal

## Current Issues
1. **Inconsistent format**: `wind_000deg` vs `wave_000deg_Hs050cm_Tp270cs`
2. **Units unclear**: `050cm` (should be meters), `270cs` (centiseconds?)
3. **Not matching input file**: Input uses m/s, m, s but folders use different units

## Proposed Naming Convention

### Option 1: Simplified and Clear
```
wind_010ms_000deg/      # Wind: 10 m/s at 0°
wave_Hs050m_Tp270s_000deg/  # Wave: Hs=0.5m, Tp=2.7s at 0°
```

### Option 2: Even Simpler (Recommended)
```
wind_reference/          # Wind baseline (10 m/s, 0°)
wave_reference/          # Wave baseline (Hs=0.5m, Tp=2.7s, 0°)
```

### Option 3: Match Input File Format
```
wind_10ms_0deg/         # Matches input file units
wave_0.5m_2.7s_0deg/    # Matches input file units
```

## Recommended Solution: Option 2

Use simple, clear names since these are REFERENCE conditions:

### Current → Proposed
```
wind_000deg/                        → wind_reference/
wave_000deg_Hs050cm_Tp270cs/       → wave_reference/
```

### Benefits:
1. **Clear purpose**: Obviously reference/baseline conditions
2. **Simple**: No confusion about units or values
3. **Documented**: Actual values (10 m/s, 0.5m, 2.7s) in README
4. **Consistent**: Both folders follow same pattern

## Implementation

### Step 1: Rename folders
```bash
# For each configuration
mv wind_000deg wind_reference
mv wave_000deg_Hs050cm_Tp270cs wave_reference
```

### Step 2: Create README in each config folder
```markdown
# Reference Conditions

## Wind Reference
- Baseline: 10 m/s at 0°
- Folder: `wind_reference/`
- Scaling: (V_actual / 10)²

## Wave Reference  
- Baseline: Hs = 0.5 m, Tp = 2.7 s at 0°
- Folder: `wave_reference/`
- Scaling: Hs_actual / 0.5
```

### Step 3: Update code references
Update any code that references the old folder names.

## Alternative: Full Descriptive Names

If we want to keep values in folder names for clarity:

```
wind_10.0ms_000deg/         # Clear units, decimal point
wave_Hs0.50m_Tp2.70s_000deg/   # Clear units, decimal points
```

This makes the baseline conditions explicit in the folder name.

## Decision Needed

Which option would you prefer?
1. **Simple**: `wind_reference/` and `wave_reference/`
2. **Descriptive**: `wind_10.0ms_000deg/` and `wave_Hs0.50m_Tp2.70s_000deg/`
3. **Current**: Keep as is but add documentation

The simple option is recommended for clarity and ease of use.