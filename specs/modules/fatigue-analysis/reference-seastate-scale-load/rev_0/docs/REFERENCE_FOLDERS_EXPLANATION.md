# Understanding the Two Reference Subfolders

## Overview
Each configuration (e.g., `fsts_l015`) contains TWO reference subfolders representing SEPARATE environmental loading conditions:

1. **`wind_000deg/`** - Wind-only reference loading
2. **`wave_000deg_Hs050cm_Tp270cs/`** - Wave-only reference loading

## Purpose: Reference Seastate Scaling Methodology

The methodology uses **baseline reference conditions** that get **scaled** to match actual environmental conditions:

### 1. Wind Reference Folder: `wind_000deg/`
- **Purpose**: Contains tension response from WIND loading only
- **Baseline Condition**: 10 m/s wind speed at 0° direction
- **Data Characteristics**:
  - Higher mean tension (~500 kN)
  - Lower frequency oscillations
  - Represents aerodynamic loads on the structure
- **Files**: Strut1.csv through Strut8.csv

### 2. Wave Reference Folder: `wave_000deg_Hs050cm_Tp270cs/`
- **Purpose**: Contains tension response from WAVE loading only
- **Baseline Condition**: 
  - Hs = 0.5 m (significant wave height)
  - Tp = 2.7 s (peak period)
  - Direction = 0°
- **Data Characteristics**:
  - Lower mean tension (~200 kN)
  - Higher frequency oscillations
  - Represents hydrodynamic loads on the structure
- **Files**: Strut1.csv through Strut8.csv

## Why Separate Folders?

The separation allows **independent scaling** of wind and wave loads based on actual conditions:

### Scaling Formulas:
- **Wind Scaling Factor** = (Actual_Wind_Speed / 10)²
- **Wave Scaling Factor** = Actual_Hs / 0.5

### Example Calculation:

For Fatigue Condition with Wind = 15 m/s, Hs = 0.3 m:

1. **Load Wind Reference**
   - Read: `wind_000deg/Strut1.csv`
   - Baseline tension: 609 kN

2. **Scale Wind Component**
   - Scale factor = (15/10)² = 2.25
   - Scaled wind tension = 609 × 2.25 = 1,370 kN

3. **Load Wave Reference**
   - Read: `wave_000deg_Hs050cm_Tp270cs/Strut1.csv`
   - Baseline tension: 266 kN

4. **Scale Wave Component**
   - Scale factor = 0.3/0.5 = 0.60
   - Scaled wave tension = 266 × 0.60 = 160 kN

5. **Combine**
   - Effective tension = 1,370 + 160 = 1,530 kN

## Data Comparison

| Aspect | Wind Reference | Wave Reference |
|--------|---------------|----------------|
| **Folder Name** | `wind_000deg/` | `wave_000deg_Hs050cm_Tp270cs/` |
| **Loading Type** | Aerodynamic | Hydrodynamic |
| **Baseline** | 10 m/s wind | Hs=0.5m, Tp=2.7s |
| **Mean Tension** | ~500 kN | ~200 kN |
| **Std Deviation** | ~87 kN | ~46 kN |
| **Frequency** | Low | High |
| **Physical Source** | Wind on topsides | Waves on hull/columns |

## Directory Structure Pattern

```
sample_data/
└── fsts_l015/                          # Configuration
    ├── wind_000deg/                    # Wind reference
    │   ├── Strut1.csv                  # Wind-only tension
    │   ├── Strut2.csv
    │   └── ... (8 files)
    └── wave_000deg_Hs050cm_Tp270cs/    # Wave reference
        ├── Strut1.csv                  # Wave-only tension
        ├── Strut2.csv
        └── ... (8 files)
```

## Key Points

1. **Separation Enables Proper Scaling**: Wind and waves scale differently with environmental conditions
2. **Physical Basis**: Wind loads scale with velocity squared, wave loads scale linearly with height
3. **Simple Combination**: After scaling, tensions are simply added (superposition principle)
4. **Direction Handling**: The `000deg` indicates the reference direction (0°)
5. **Multiple References Possible**: Production data may have multiple directions (e.g., `wind_090deg/`)

## Usage in Code

```python
# Load references
wind_data = load_csv("wind_000deg/Strut1.csv")
wave_data = load_csv("wave_000deg_Hs050cm_Tp270cs/Strut1.csv")

# Scale based on actual conditions
wind_scaled = wind_data * (actual_wind_speed / 10)**2
wave_scaled = wave_data * (actual_hs / 0.5)

# Combine
effective_tension = wind_scaled + wave_scaled
```

---
*This separation is fundamental to the reference seastate scaling methodology*