
## Rainflow Counting Visualization

### Understanding Rainflow Counting

Rainflow counting is a method used in fatigue analysis to reduce a complex stress-time history into a set of simple stress reversals that can be used with S-N curves to assess fatigue damage.

### How It Works:

1. **Identify Peaks and Valleys**: The algorithm first identifies all local maxima (peaks) and minima (valleys) in the tension time series.

2. **Count Cycles**: The algorithm then "flows" like rain down the pagoda roof of the time series, counting:
   - **Full cycles**: Complete tension reversals (peak-valley-peak)
   - **Half cycles**: Incomplete reversals

3. **Extract Ranges**: For each cycle, extract:
   - **Range**: The tension difference (amplitude x 2)
   - **Mean**: The average tension level
   - **Count**: Number of occurrences (1.0 for full, 0.5 for half)

### Visual Example:

![Rainflow Visualization](output/verification/step_by_step/figures/rainflow_visualization.png)

The figure above shows:
- **Top panel**: Original tension time series with peaks (red triangles) and valleys (green triangles) marked
- **Middle panels**: Distribution of cycle ranges and binned counts
- **Bottom panels**: Rainflow matrix and damage contribution

### Key Observations:

1. **Cycle Distribution**: Most cycles are small amplitude (< 40 kN range)
2. **Large Cycles**: Few large cycles (> 80 kN) contribute most to fatigue damage
3. **Damage Scaling**: Damage scales with range^3, so large cycles dominate

### Verification Points:

- **Reasonable cycle count**: ~15-20 cycles in 20 seconds of data
- **Range distribution**: Follows expected pattern with many small, few large cycles
- **Peak/valley detection**: Correctly identifies tension reversals
- **Damage concentration**: Large cycles contribute disproportionately to fatigue

This rainflow counting forms the basis for subsequent fatigue damage calculations using S-N curves.
