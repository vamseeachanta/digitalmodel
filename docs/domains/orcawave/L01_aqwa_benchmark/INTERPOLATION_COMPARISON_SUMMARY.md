# AQWA vs OrcaWave Comparison with Linear Interpolation

**Date**: 2026-01-05 22:26:15
**Status**: ⚠️ UNIT MISMATCH IDENTIFIED
**Method**: 2D Linear Interpolation (scipy.interpolate.RegularGridInterpolator)

---

## ✅ Comparison Completed Successfully

### Interpolation Details
- **Source Grid (OrcaWave)**: 20 frequencies × 9 headings = 180 points
- **Target Grid (AQWA)**: 13 frequencies × 111 headings = 1,443 points
- **Method**: Complex RAOs interpolated separately (real + imaginary components)
- **Interpolation**: 2D linear interpolation with zero-fill for out-of-bounds

### Files Generated
- **HTML Report**: `comparison_results/interpolated_comparison_20260105_222615.html`
- **Script**: `run_comparison_with_interpolation.py`

---

## ❌ Critical Issue: Unit/Convention Mismatch

### Magnitude Comparison

| DOF | AQWA Peak | OrcaWave Peak | Ratio (OW/AQWA) |
|-----|-----------|---------------|-----------------|
| **SURGE** | 0.8926 | 2,042.97 | ~2,289x |
| **SWAY** | 0.0000 | 5,471.92 | ∞ |
| **HEAVE** | 0.9224 | 22,771.39 | ~24,690x |
| **ROLL** | 0.0002 | 12,504.12 | ~62,521,000x |
| **PITCH** | 0.4577 | 230,924.84 | ~504,533x |
| **YAW** | 0.0000 | 582,063.26 | ∞ |

**Observation**: OrcaWave magnitudes are **orders of magnitude larger** than AQWA.

---

## 🔍 Possible Root Causes

### 1. **Unit Difference** (Most Likely)

**AQWA RAOs** (from .LIS file):
- Likely **displacement RAOs** in `m/m` (dimensionless)
- Wave amplitude normalization: response per meter of wave
- Values typically 0.1 to 1.0 for translation, radians for rotation

**OrcaWave RAOs** (from .owr file via OrcFxAPI):
- Might be **force/moment RAOs** in `N/m` or `N·m/m`
- API property: `loadRAOsHaskind` suggests **load** (force/moment)
- Values in thousands/millions suggest forces, not displacements

### 2. **Different RAO Definitions**

- **AQWA**: Motion RAOs (displacement/rotation per wave amplitude)
- **OrcaWave API**: Load RAOs (force/moment per wave amplitude)
- **Conversion needed**: Motion RAO = Load RAO / (Hydrostatic Stiffness)

### 3. **Wave Amplitude Normalization**

- **AQWA**: Normalized to 1m wave amplitude (standard)
- **OrcaWave**: Might use different normalization
- **API documentation**: Check `loadRAOsHaskind` vs `motionRAOsHaskind`

### 4. **Coordinate System Difference**

- AQWA and OrcaWave might use different sign conventions
- Heading definitions might differ (0° = head seas vs beam seas)
- Less likely to cause magnitude differences, but worth checking

---

## 🎯 Next Steps (REQUIRED)

### Immediate Action: Investigate API Properties

**Check OrcFxAPI documentation for:**

1. **Alternative RAO properties**:
   ```python
   diffraction.loadRAOsHaskind    # Force/moment RAOs ← Currently using
   diffraction.motionRAOsHaskind  # Motion RAOs ← Should try this
   diffraction.loadRAOs           # Alternative load RAOs
   diffraction.motionRAOs         # Alternative motion RAOs
   ```

2. **Extract both types and compare**:
   - Load RAOs (force/moment)
   - Motion RAOs (displacement/rotation)
   - Check which matches AQWA conventions

3. **Verify units in OrcaWave documentation**:
   - Check OrcFxAPI manual for `loadRAOsHaskind` units
   - Confirm expected magnitude range
   - Look for normalization conventions

### Alternative Approaches

#### Option A: Use Motion RAOs from OrcaWave
```python
# In extract_orcawave_raos.py
motion_raos = diffraction.motionRAOsHaskind  # Try this instead
```

#### Option B: Convert Load RAOs to Motion RAOs
```python
# Need hydrostatic stiffness matrix and mass matrix
motion_rao = load_rao / (K - ω² * M)
# Where K = hydrostatic stiffness, M = mass matrix
```

#### Option C: Extract AQWA Load RAOs
- Check if AQWA .LIS file contains force/moment RAOs
- Compare load-to-load instead of motion-to-load

---

## 📊 Preliminary Results (Before Unit Fix)

### Pass Rates (INVALID due to unit mismatch)

| DOF | Significant Pass Rate | Overall Pass Rate | Status |
|-----|----------------------|------------------|--------|
| SURGE | 100.0% (2/2) | 96.1% | ❌ FAIL* |
| SWAY | 85.1% (1228/1443) | 85.1% | ❌ FAIL |
| HEAVE | 100.0% (2/2) | 96.1% | ❌ FAIL* |
| ROLL | 0.0% (0/2) | 84.7% | ❌ FAIL |
| PITCH | 0.0% (0/2) | 84.7% | ❌ FAIL |
| YAW | 84.8% (1223/1443) | 84.8% | ❌ FAIL |

**Note**: These results are **MEANINGLESS** until unit mismatch is resolved.

*SURGE and HEAVE show 100% pass on "significant values" but the peak magnitudes differ by 2000-25000x, indicating the comparison is invalid.

---

## 🛠️ Recommended Workflow

### Step 1: Check OrcaWave API Properties
```python
# Add to extract_orcawave_raos.py
print(f"Properties available:")
print(f"  - loadRAOsHaskind: {diffraction.loadRAOsHaskind.shape}")
print(f"  - motionRAOsHaskind: {diffraction.motionRAOsHaskind.shape}")  # Try this
print(f"  - loadRAOs: {diffraction.loadRAOs.shape}")  # If available
```

### Step 2: Extract Motion RAOs
```python
# Use motion RAOs instead of load RAOs
motion_raos = diffraction.motionRAOsHaskind
# Compare magnitudes to AQWA
```

### Step 3: Re-run Comparison
```bash
python run_comparison_with_interpolation.py
```

### Step 4: Validate Results
- Check if magnitude ranges now match
- AQWA motion RAOs typically: 0.1-1.0 for translation
- If OrcaWave motion RAOs are similar → SUCCESS
- If still mismatched → investigate further

---

## 📝 Documentation Updates Needed

After resolving unit mismatch:

1. **Update extraction script** with correct RAO type
2. **Document unit conventions** in both systems
3. **Add validation checks** for magnitude ranges
4. **Re-generate HTML report** with correct comparison
5. **Update SESSION_SUMMARY.md** with final results

---

## 🔗 References

### OrcaWave API Documentation
- Check: OrcFxAPI manual section on `Diffraction` class
- Properties: `loadRAOsHaskind`, `motionRAOsHaskind`
- Units: Force/moment vs displacement/rotation RAOs

### AQWA Documentation
- .LIS file RAO format (confirmed: motion RAOs in m/m, rad/m)
- Normalization: 1m wave amplitude (standard practice)

### RAO Theory
- DNV-RP-C205: Environmental Conditions and Environmental Loads
- Motion RAO vs Force RAO relationship
- Hydrostatic restoring and frequency-dependent added mass/damping

---

**Generated**: 2026-01-05 22:30:00
**Status**: ⚠️ Awaiting unit investigation
**Next Action**: Check `motionRAOsHaskind` property in OrcaWave API
**Blocking Issue**: Cannot validate 5% tolerance until units match
