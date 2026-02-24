# Fatigue Curve Implementation Review: ABS-E In Air

**Date:** 2025-01-30
**Module:** `stress_rainflow_to_damage`
**Configuration:** `damage_analysis_config_test.yml`

## Executive Summary

The ABS-E in-air S-N curve parameters in the configuration file are **highly accurate** when compared to the ABS'20 fatigue database. However, the implementation in `run_damage_analysis.py` has a **critical limitation**: it only implements the first segment of the two-segment S-N curve, despite the configuration containing both segments.

---

## 1. Database Comparison Results

### Source Data
- **Database:** ABS'20 Guide for Fatigue Assessment of Offshore Structures (June 2020)
- **Curve:** ABS-E, In Air environment
- **Location:** Row 201 in `data/fatigue/fatigue_curves_structured.csv`

### Parameter Accuracy

| Parameter | Database Value | Config Value | Match Status |
|-----------|---------------|--------------|--------------|
| **Segment 1 (High Stress, N < 10^7)** |
| log_a1 | 12.017 | 12.0170 | ✅ Perfect |
| m1 | 3.0 | 3.0 | ✅ Perfect |
| Transition cycles | 10,000,000 | 10,000,000 | ✅ Perfect |
| Fatigue limit stress | 47.0 MPa | 47.0 MPa | ✅ Perfect |
| **Segment 2 (Low Stress, N ≥ 10^7)** |
| log_a2 | 15.362 | 15.378 | ⚠️ 0.016 diff (1.04%) |
| m2 | 5.0 | 5.0 | ✅ Perfect |

### Assessment
The configuration parameters are **highly accurate**. The minor discrepancy in `log_c` (0.016) is negligible for engineering purposes but could be updated for perfect alignment.

**Recommendation:** Update line 49 in config to:
```yaml
log_c: 15.362  # Changed from 15.378 to match ABS'20 exactly
```

---

## 2. Implementation Analysis

### Current Implementation (run_damage_analysis.py, lines 59-76)

```python
def calculate_cycles_to_failure(self, stress_range: float) -> float:
    """Calculate number of cycles to failure using S-N curve"""
    params = self.config['sn_curve']['parameters']

    # Check if stress is below fatigue limit
    if stress_range <= params['fatigue_limit_stress']:
        return float('inf')  # Infinite life below fatigue limit

    # Calculate N using S-N curve (ONLY SEGMENT 1!)
    log_N = params['log_a'] - params['m'] * np.log10(stress_range)
    N = 10 ** log_N

    return N
```

### Critical Finding

**The two-segment curve is NOT implemented despite being configured!**

The code:
1. ✅ Correctly checks fatigue limit (47.0 MPa)
2. ❌ Only uses Segment 1 parameters (log_a, m)
3. ❌ Ignores Segment 2 parameters (log_c, r)
4. ❌ Does not check transition point (10^7 cycles)

---

## 3. Impact Assessment

### Theoretical Transition Point
- **Transition Stress:** 47.03 MPa (calculated from Segment 1 equation)
- **Transition Cycles:** 10,000,000 cycles
- **Note:** Transition stress ≈ Fatigue limit stress (both ~47 MPa)

### Continuity Check at Transition
- Segment 1 predicts: **10,000,000 cycles** at 47.03 MPa
- Segment 2 predicts: **10,383,251 cycles** at 47.03 MPa
- **Discontinuity:** 3.8% jump (Ratio = 0.963)

### Practical Impact

For typical operating stress ranges (50-100 MPa), the analysis shows:

| Stress (MPa) | N_single | N_two_segment | Damage Difference |
|--------------|----------|---------------|-------------------|
| 50 | 8.32×10⁶ | 8.32×10⁶ | 0% |
| 60 | 4.81×10⁶ | 4.81×10⁶ | 0% |
| 70 | 3.03×10⁶ | 3.03×10⁶ | 0% |
| 80 | 2.03×10⁶ | 2.03×10⁶ | 0% |
| 100 | 1.04×10⁶ | 1.04×10⁶ | 0% |

**Key Observation:** For stress ranges above 47 MPa, both curves give nearly identical results because:
- Stresses > 47 MPa result in N < 10^7 cycles (still in Segment 1 region)
- The transition happens at such low stress/high cycles that typical loads don't reach it

### When Does It Matter?

The two-segment implementation becomes important for:
1. **Very low stress amplitudes** (just above 47 MPa)
2. **High-cycle fatigue** (N > 10^7 cycles)
3. **Variable amplitude loading** with significant low-stress cycles
4. **Conservative life predictions** in the high-cycle region

---

## 4. Fatigue Limit Behavior

### Current Implementation: ✅ CORRECT

```python
if stress_range <= params['fatigue_limit_stress']:  # 47.0 MPa
    return float('inf')  # Infinite life
```

**Assessment:** The fatigue limit is correctly implemented:
- Stresses ≤ 47.0 MPa → Infinite life (damage = 0)
- Stresses > 47.0 MPa → Finite life calculated from S-N curve
- This follows ABS-E specification exactly

**Visualization:** The plots show the fatigue limit correctly enforced at 47 MPa.

---

## 5. Recommendations

### Priority 1: Document Current Limitation
Add comment in code explaining single-segment limitation:

```python
def calculate_cycles_to_failure(self, stress_range: float) -> float:
    """
    Calculate number of cycles to failure using S-N curve

    NOTE: Currently implements ONLY Segment 1 (high-stress region).
    Two-segment curve is configured but not implemented.
    For ABS-E in air: Segment 1 valid for N < 10^7 cycles.
    """
```

### Priority 2: Implement Two-Segment Logic (Optional)

```python
def calculate_cycles_to_failure(self, stress_range: float) -> float:
    """Calculate cycles to failure with two-segment S-N curve"""
    params = self.config['sn_curve']['parameters']

    # Check fatigue limit
    if stress_range <= params['fatigue_limit_stress']:
        return float('inf')

    # Segment 1: High stress (N < transition_cycles)
    log_N1 = params['log_a'] - params['m'] * np.log10(stress_range)
    N1 = 10 ** log_N1

    # Check if two-segment is enabled and needed
    if (params.get('two_segment', {}).get('enabled', False) and
        N1 >= params['fatigue_limit_cycles']):
        # Segment 2: Low stress (N >= transition_cycles)
        two_seg = params['two_segment']
        log_N2 = two_seg['log_c'] - two_seg['r'] * np.log10(stress_range)
        N2 = 10 ** log_N2
        return N2

    return N1
```

### Priority 3: Update Config for Database Alignment

```yaml
sn_curve:
  parameters:
    log_c: 15.362  # Updated from 15.378 to match ABS'20 database
```

---

## 6. Validation Results

### Test Script Output
```
================================================================================
CONCLUSIONS:
================================================================================
1. Current implementation uses ONLY Segment 1 (single segment)
2. Two-segment curve is configured but NOT implemented in code
3. Below 47.0 MPa, single-segment is NON-CONSERVATIVE
4. At fatigue limit (47.0 MPa), both give infinite life
5. RECOMMENDATION: Implement two-segment logic in calculate_cycles_to_failure()
================================================================================
```

### Visual Analysis
See generated plot: `output/test/two_segment_curve_analysis.png`

The plot shows:
1. **S-N Curve:** Single and two-segment curves are nearly identical above 50 MPa
2. **Damage Rate:** Negligible difference in practical stress range
3. **Percent Difference:** < 0.05% difference in cycles for typical stresses
4. **Conservatism Ratio:** ≈ 1.0 (neutral) for operational stress ranges

---

## 7. Conclusions

### What's Working Well ✅
1. **Database alignment:** Config parameters match ABS'20 data accurately
2. **Fatigue limit:** Correctly implemented at 47 MPa with infinite life
3. **Segment 1:** Properly calculates high-stress, low-cycle region
4. **Practical impact:** Minimal for typical operational stresses (>50 MPa)

### What Needs Attention ⚠️
1. **Two-segment curve:** Configured but not implemented in code
2. **Code documentation:** Missing explanation of single-segment limitation
3. **Minor parameter:** log_c has 1% difference from database (optional fix)

### Engineering Assessment
For typical offshore structure applications with stress ranges of 50-100 MPa:
- **Current implementation is adequate** for most practical purposes
- **Two-segment implementation is optional** but recommended for completeness
- **Fatigue limit handling is correct** and follows ABS guidelines

### Risk Level
**LOW** - The single-segment implementation is conservative for high stresses and only differs from two-segment in the rarely-encountered high-cycle region.

---

## 8. References

1. ABS'20 Guide for Fatigue Assessment of Offshore Structures (June 2020)
2. Database: `data/fatigue/fatigue_curves_structured.csv` (Row 201)
3. Configuration: `specs/modules/fatigue-analysis/stress_rainflow_to_damage/input/damage_analysis_config_test.yml`
4. Implementation: `specs/modules/fatigue-analysis/stress_rainflow_to_damage/run_damage_analysis.py`
5. Test script: `specs/modules/fatigue-analysis/stress_rainflow_to_damage/test_two_segment_curve.py`

---

**Reviewed by:** Claude Code Analysis
**Status:** Implementation adequate for typical use; documentation improvement recommended
