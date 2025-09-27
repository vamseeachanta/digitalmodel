# ABS S-N Curve E - Detailed Review and Corrections

## Official ABS Data (from Table 1)

### Curve Class E Parameters
From the ABS table for **Curve E (highlighted in yellow)**:

| Parameter | MPa Units | ksi Units | Description |
|-----------|-----------|-----------|-------------|
| A | 1.04×10¹² | 3.18×10¹⁰ | Intercept coefficient for N < N₀ |
| m | 3.0 | 3.0 | Slope for N < N₀ |
| C | 2.39×10¹⁵ | 1.48×10¹¹ | Intercept coefficient for N > N₀ |
| r | 5.0 | 5.0 | Slope for N > N₀ |
| N₀ | 10⁷ | 10⁷ | Transition point (fatigue limit cycles) |
| S₀ | 47.0 MPa | 6.83 ksi | Fatigue limit stress |

## S-N Curve Equations

### For N ≤ 10⁷ cycles (main fatigue region):
```
N = A × S^(-m)
N = 1.04×10¹² × S^(-3)
```

In logarithmic form:
```
log₁₀(N) = log₁₀(A) - m × log₁₀(S)
log₁₀(N) = log₁₀(1.04×10¹²) - 3.0 × log₁₀(S)
log₁₀(N) = 12.0170 - 3.0 × log₁₀(S)
```

### For N > 10⁷ cycles (below fatigue limit):
```
N = C × S^(-r)
N = 2.39×10¹⁵ × S^(-5)
```

In logarithmic form:
```
log₁₀(N) = log₁₀(2.39×10¹⁵) - 5.0 × log₁₀(S)
log₁₀(N) = 15.378 - 5.0 × log₁₀(S)
```

## Verification Calculations

### At Fatigue Limit (S = 47.0 MPa, N = 10⁷):

Using first segment equation:
```
log₁₀(N) = 12.0170 - 3.0 × log₁₀(47)
log₁₀(N) = 12.0170 - 3.0 × 1.6721
log₁₀(N) = 12.0170 - 5.0163
log₁₀(N) = 7.0007 ≈ 7.0 ✓
N = 10⁷ ✓
```

### Test Point at S = 100 MPa:
```
log₁₀(N) = 12.0170 - 3.0 × log₁₀(100)
log₁₀(N) = 12.0170 - 3.0 × 2.0
log₁₀(N) = 12.0170 - 6.0
log₁₀(N) = 6.0170
N = 10^6.017 = 1.04×10⁶ cycles ✓
```

## Comparison with Current Implementation

| Parameter | Current (Wrong) | Correct (ABS) | Error |
|-----------|----------------|---------------|-------|
| log₁₀(A) | 12.164 | 12.0170 | +0.147 |
| m | 3.0 | 3.0 | ✓ Correct |
| S₀ (fatigue limit) | 52.64 MPa | 47.0 MPa | +5.64 MPa (12%) |
| N₀ | 10⁷ | 10⁷ | ✓ Correct |

## Impact of Error

### Example: Stress = 50 MPa

**Current (Wrong) Implementation:**
- 50 MPa < 52.64 MPa → Infinite life (no damage)

**Correct Implementation:**
- 50 MPa > 47.0 MPa → Finite life
- N = 1.04×10¹² × 50^(-3) = 8.32×10⁶ cycles
- Not infinite! Will accumulate damage

### Example: Stress = 48 MPa

**Current (Wrong):**
- Infinite life

**Correct:**
- N = 1.04×10¹² × 48^(-3) = 9.375×10⁶ cycles
- Close to but below 10⁷, still accumulates damage

## Required Code Corrections

### 1. Configuration Files (all 3 YAMLs)
```yaml
sn_curve:
  curve_type: "ABS-E"
  environment: "in-air"
  parameters:
    # CORRECTED ABS E curve parameters
    log_a: 12.0170             # log₁₀(1.04×10¹²)
    m: 3.0                     # Slope (already correct)
    fatigue_limit_cycles: 1.0e7     # Already correct
    fatigue_limit_stress: 47.0      # CORRECTED from 52.64
    
    # Two-segment S-N curve for completeness
    two_segment:
      enabled: true            # Should be enabled for accuracy
      transition_cycles: 1.0e7 # Transition at N₀
      log_c: 15.378           # log₁₀(2.39×10¹⁵)
      r: 5.0                  # Slope after transition
```

### 2. Python Implementation
```python
def calculate_cycles_to_failure(self, stress_range: float) -> float:
    """
    Calculate cycles to failure using ABS E curve
    Two-segment curve with transition at 10^7 cycles
    """
    params = self.config['sn_curve']['parameters']
    
    # First, check which segment to use
    # Calculate N using first segment equation
    log_n_segment1 = 12.0170 - 3.0 * np.log10(stress_range)
    n_segment1 = 10 ** log_n_segment1
    
    if n_segment1 <= 1.0e7:
        # Use first segment (normal fatigue)
        return n_segment1
    else:
        # Use second segment (near/below fatigue limit)
        # Could return infinity for simplified analysis
        # Or use second segment for more accuracy:
        if self.config['sn_curve']['two_segment']['enabled']:
            log_n_segment2 = 15.378 - 5.0 * np.log10(stress_range)
            return 10 ** log_n_segment2
        else:
            return float('inf')  # Simplified: infinite life below S₀
```

## Summary of Corrections Needed

1. **Update log_a**: 12.164 → 12.0170
2. **Update fatigue limit**: 52.64 MPa → 47.0 MPa
3. **Consider two-segment**: Add second segment parameters for accuracy
4. **Re-run analysis**: Results will be more conservative

## Expected Impact on Results

- Stresses between 47-52.64 MPa will now show finite life
- Overall damage rates will increase
- Some locations previously "infinite life" may now show large but finite life
- More conservative and safe assessment

---
*Review Date: 2025-01-25*
*Reference: ABS Guide for Fatigue Assessment of Offshore Structures*
*Table 1 - Curve Class E*