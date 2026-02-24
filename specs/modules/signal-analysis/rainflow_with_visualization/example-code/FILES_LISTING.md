# Rainflow Comparison Files Listing

## Directory: `specs\modules\signal-analysis\rainflow_with_visualization\example-code\`

This directory contains all files related to the rainflow algorithm comparison analysis performed on 2025-01-29.

## Files Overview

### 1. Original Implementations
- **`Rainflow-User.py`** - Custom pure Python rainflow implementation (has double-counting issue)
- **`run_rainflow_analysis.py`** - Production-ready implementation using standard rainflow library (correct)

### 2. Comparison Scripts
- **`rainflow_comparison.py`** - Simplified comparison script with pure sine wave tests
- **`rainflow_comparison_full.py`** - Comprehensive comparison with multiple test signals

### 3. Results and Outputs
- **`rainflow_comparison_plot.png`** - Visual comparison showing algorithm differences
- **`rainflow_comparison_pure_sine.png`** - Additional pure sine wave visualization
- **`rainflow_comparison_results.csv`** - Numerical comparison results

### 4. Documentation
- **`RAINFLOW_COMPARISON_SUMMARY.md`** - Comprehensive analysis document with findings
- **`FILES_LISTING.md`** - This file listing

## Key Findings Summary

The comparison revealed a critical discrepancy:
- **Custom Algorithm**: Counts 20 cycles for a 10-cycle sine wave (INCORRECT - double counting)
- **Library Algorithm**: Counts 10.5 cycles for a 10-cycle sine wave (CORRECT per ASTM E1049)

## File Purposes

| File | Purpose | Status |
|------|---------|--------|
| `Rainflow-User.py` | Excel-integrated custom implementation | ⚠️ Has counting error |
| `run_rainflow_analysis.py` | Production implementation | ✅ Correct |
| `rainflow_comparison.py` | Testing and validation | ✅ Working |
| `rainflow_comparison_plot.png` | Visual proof of discrepancy | ✅ Generated |
| `RAINFLOW_COMPARISON_SUMMARY.md` | Technical documentation | ✅ Complete |

## Usage Instructions

### To Run Comparison:
```bash
python rainflow_comparison.py
```

### To Use Production Implementation:
```bash
python run_rainflow_analysis.py config.yml
```

## Version Control Status

All files in this directory are untracked (new) as of the analysis date.
These files represent new comparison work not previously in the repository.

## Recommendations

1. **DO NOT USE** `Rainflow-User.py` for production fatigue analysis without fixing the double-counting issue
2. **USE** `run_rainflow_analysis.py` for all production rainflow counting needs
3. **REFERENCE** the comparison results when validating future implementations

---

*Generated: 2025-01-29*
*Location: specs/modules/signal-analysis/rainflow_with_visualization/example-code/*