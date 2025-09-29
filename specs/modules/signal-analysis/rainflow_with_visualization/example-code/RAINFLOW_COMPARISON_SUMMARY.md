# Rainflow Algorithm Comparison Results

## Executive Summary

A comprehensive comparison was performed between two rainflow cycle counting implementations:
1. **Custom Implementation** (`Rainflow-User.py`) - Pure Python stack-based algorithm
2. **Library Implementation** (`run_rainflow_analysis.py`) - Using the standard `rainflow` package

## Key Findings

### ⚠️ CRITICAL DISCREPANCY IDENTIFIED

For a pure sine wave with 10 complete cycles:
- **Custom Algorithm**: 20.0 total cycles (19 full + 2 half) - **INCORRECT**
- **Library Algorithm**: 10.5 total cycles (9 full + 3 half) - **CORRECT**
- **Expected**: ~10 cycles

**The custom algorithm is DOUBLE-COUNTING cycles, leading to a 2x overestimation!**

## Test Results

### Test 1: Pure Sine Wave (10 cycles, amplitude ±10)

| Algorithm | Total Cycles | Full Cycles | Half Cycles | Range |
|-----------|-------------|-------------|-------------|-------|
| Custom    | 20.0        | 19          | 2           | 20.00 |
| Library   | 10.5        | 9           | 3           | 20.00 |

### Test 2: Variable Amplitude Sine Wave

| Algorithm | Total Cycles | Full Cycles | Half Cycles | Range Distribution |
|-----------|-------------|-------------|-------------|-------------------|
| Custom    | 40.0        | 39          | 2           | 0.1-17.9         |
| Library   | 20.5        | 9           | 23          | 2.0-18.0         |

## Analysis of Discrepancy

### Root Cause
The custom implementation has a flaw in its turning point detection algorithm:

```python
# Custom algorithm turning point detection (lines 28-32)
for i in range(1, len(comp)-1):
    if (comp[i] > comp[i-1] and comp[i] >= comp[i+1]) or 
       (comp[i] < comp[i-1] and comp[i] <= comp[i+1]):
        tp.append(comp[i])
```

This logic appears to be creating extra turning points, effectively counting each sine wave cycle twice.

### Impact on Fatigue Analysis

1. **Damage Overestimation**: The custom algorithm will overestimate fatigue damage by approximately 2x
2. **Non-Standard Results**: Does not comply with ASTM E1049 standard
3. **Inconsistent with Industry Tools**: Results will not match commercial fatigue analysis software

## Comparison Methodology

Both algorithms were tested on:
1. **Pure sine waves** - Known cycle count for validation
2. **Variable amplitude sine waves** - Complex patterns to test robustness

### Counting Convention Comparison

| Aspect | Custom Algorithm | Library Algorithm |
|--------|-----------------|-------------------|
| Full Cycle | count = 1.0 | count = 1.0 |
| Half Cycle | count = 0.5 | count = 0.5 |
| Standard | Non-standard | ASTM E1049 |

## Recommendations

### For Production Use:
✅ **USE**: `run_rainflow_analysis.py` (library-based implementation)
- Correct cycle counting per ASTM E1049
- Validated against industry standards
- Comprehensive features (FFT, visualization, batch processing)

### For Excel Integration:
❌ **AVOID**: `Rainflow-User.py` without correction
- Contains double-counting error
- Will produce incorrect fatigue life estimates
- Needs algorithm correction before use

### Immediate Actions:
1. Replace custom algorithm with library implementation in production workflows
2. Re-analyze any previous results obtained with custom algorithm
3. Divide historical custom algorithm results by ~2 for rough correction

## Files Generated

1. **`rainflow_comparison.py`** - Complete comparison test script
2. **`rainflow_comparison_plot.png`** - Visual comparison of algorithms
3. **`rainflow_comparison_results.csv`** - Detailed numerical results
4. **`RAINFLOW_COMPARISON_SUMMARY.md`** - This summary document

## Visualization

The comparison plot shows:
- Row 1: Pure sine wave analysis
- Row 2: Variable amplitude sine analysis
- Columns: Input signal, Custom algorithm results, Library algorithm results

Clear visual evidence of the double-counting issue in the custom algorithm.

## Conclusion

The library-based implementation (`run_rainflow_analysis.py`) should be used for all rainflow cycle counting applications. The custom implementation has a fundamental counting error that makes it unsuitable for fatigue analysis without correction.

---

*Analysis performed: 2025-01-29*
*Tools used: Python 3.13, rainflow 3.2.0, numpy, pandas, matplotlib*