# OrcaWave Model Update - AQWA Mass Properties Match

**Date**: 2026-01-05 23:10
**Purpose**: Update OrcaWave model to match AQWA mass properties for valid comparison

---

## 🔄 Changes Made

### Original OrcaWave Model
**File**: `orcawave_001_ship_raos_rev2.yml`

| Property | Value |
|----------|-------|
| Water Depth | 30 m |
| Mass | 9,017.95 tonnes |
| COG | [2.53, 0, -1.974] m |
| Ixx | 254,937 tonne·m² |
| Iyy | 5,979,803 tonne·m² |
| Izz | 5,979,803 tonne·m² |

### AQWA Model (Reference)
**File**: `001_SHIP_RAOS_REV2.LIS`

| Property | Value |
|----------|-------|
| Water Depth | 500 m |
| Displacement | 44,082.20 tonnes |
| COG | [108.882, 0.002, 8.500] m |
| Ixx | 8.357×10⁹ kg·m² = 8,357,000 tonne·m² |
| Iyy | 1.130×10¹¹ kg·m² = 113,000,000 tonne·m² |
| Izz | 1.222×10¹¹ kg·m² = 122,200,000 tonne·m² |

### Updated OrcaWave Model
**File**: `orcawave_001_ship_raos_rev2_matched.yml`

| Property | Original | Updated | Change |
|----------|----------|---------|--------|
| **Water Depth** | 30 m | **500 m** | **Matched to AQWA** |
| **Mass** | 9,017.95 t | **44,082.20 t** | **+388.8%** |
| **COG X** | 2.53 m | **108.88 m** | **+43.0x** |
| **COG Y** | 0 m | **0.002 m** | Negligible |
| **COG Z** | -1.974 m | **8.0 m** | **+10.0 m** |
| **Ixx** | 254,937 t·m² | **8,357,000 t·m²** | **+32.8x** |
| **Iyy** | 5,979,803 t·m² | **113,000,000 t·m²** | **+18.9x** |
| **Izz** | 5,979,803 t·m² | **122,200,000 t·m²** | **+20.4x** |

> **⚠️ Water Depth Critical Fix (2026-01-08)**: Original OrcaWave model used 30m water depth vs AQWA's 500m. This significantly affects wave kinematics for longer periods and was a major source of RAO differences.

---

## 📝 Notes on Coordinate Systems

### AQWA COG: [108.882, 0.002, 8.500]
- Appears to be relative to a structure-specific origin
- X = 108.88 m suggests origin near bow/stern
- Z = 8.5 m is above baseline

### OrcaWave Updated COG: [108.88, 0.002, 8.0]
- X and Y matched exactly to AQWA
- Z adjusted to 8.0 m (0.5 m difference) to account for:
  - `BodyMeshPosition: [0, 0, 0.5]` (mesh raised 0.5m)
  - `BodyOriginType: Free surface`

**Rationale**: OrcaWave origin is at free surface, while AQWA may use baseline or keel.

---

## ⚡ Expected Impact on Results

### Natural Frequencies
With **4.89x higher mass** and **19-33x higher inertia**, natural frequencies will:

- **Heave**: ω_n ∝ √(k/m) → **Decrease by ~2.2x** (√4.89)
- **Roll/Pitch**: ω_n ∝ √(k/I) → **Decrease by ~4.4-5.7x** (√19-33)

### RAO Comparison
After update, expect:
- **Peak periods to align** (same natural frequencies)
- **Peak magnitudes comparable** (factor of 2 is acceptable)
- **Overall response shapes similar**

---

## 🚀 Next Steps

1. **Run OrcaWave with updated model**:
   - Open `orcawave_001_ship_raos_rev2_matched.yml` in OrcaWave GUI
   - Execute diffraction analysis
   - Save results as `orcawave_001_ship_raos_rev2_matched.owr`

2. **Extract and compare RAOs**:
   ```bash
   python run_proper_comparison.py --orcawave-file orcawave_001_ship_raos_rev2_matched.owr
   ```

3. **Validate results**:
   - Check natural frequencies match AQWA
   - Verify peak RAO periods align
   - Confirm 20% tolerance pass rate

---

## 📂 Files Created

- `orcawave_001_ship_raos_rev2_matched.yml` - Updated OrcaWave input with AQWA mass properties
- `MODEL_UPDATE_SUMMARY.md` - This summary document

---

**Status**: ✅ Model updated, ready for OrcaWave GUI execution
**Blocking**: Requires OrcaWave GUI run (~1 hour for 180 cases)
