# Plot Manipulation Details - OrcaFlex Fatigue Analysis

## Overview
This document describes the progressive refinement of the visualization approach for OrcaFlex tension data analysis, documenting each iteration and the specific improvements made.

## Initial Plot Design (Version 1)
- **Layout**: 2x2 subplot grid
- **Separate plots**: Vessel End and Jacket End analyzed separately
- **Issues**: 
  - Difficult to compare vessel vs jacket behavior
  - Redundant use of screen space
  - No direct visual comparison

## Version 2 - FFT Smoothing Enhancement
### Problem
- FFT spectrum was spiky and difficult to interpret
- Insufficient averaging leading to noisy frequency domain representation

### Solution
```python
# Original configuration (spiky)
config = {
    'fft': {
        'window_size': 4096,  # Large window
        'overlap': 0.5,       # 50% overlap
    }
}

# Improved configuration (smooth)
config = {
    'fft': {
        'window_size': 1024,  # Reduced window size
        'overlap': 0.75,      # Increased overlap to 75%
    }
}
```

### Result
- Number of windows increased from ~17 to 137
- Smooth spectrum achieved through better averaging
- Clear identification of dominant frequencies

## Version 3 - Combined Plots
### Problem
- Vessel and Jacket end data on separate plots made comparison difficult
- Y-axis ranges not optimized for data visibility

### Solution
```python
# Combined plotting approach
def plot_combined(vessel_data, jacket_data):
    # Plot both on same axes
    ax.plot(vessel_data, label='Vessel End')
    ax.plot(jacket_data, label='Jacket End')
    
    # Percentile-based y-axis scaling
    all_data = np.concatenate([vessel_data, jacket_data])
    y_min = np.percentile(all_data, 5)
    y_max = np.percentile(all_data, 99.5)
    ax.set_ylim(y_min, y_max)
```

### Result
- Direct visual comparison between vessel and jacket behavior
- Optimized y-axis range focusing on 5th-99.5th percentile
- Better peak visibility

## Version 4 - Line Style Differentiation
### Problem
- Overlapping lines difficult to distinguish
- Both datasets using solid lines caused visual confusion

### Solution
```python
# Line style mapping
vessel_style = {
    'linestyle': '-',      # Solid line
    'linewidth': 1.5,
    'alpha': 0.8
}

jacket_style = {
    'time_series': '--',   # Dashed line
    'fft': ':',           # Dotted line
    'linewidth': 1.5,
    'alpha': 0.8
}
```

### Visual Hierarchy
1. **Time Series Plot**
   - Vessel: Solid line (─────)
   - Jacket: Dashed line (- - - -)

2. **FFT Plot**
   - Vessel: Solid line (─────)
   - Jacket: Dotted line (·····)

3. **Welch PSD Plot**
   - Vessel: Solid line (─────)
   - Jacket: Dotted line (·····)

## Version 5 - Color Consistency
### Problem
- Colors changing between plots confused interpretation
- No consistent visual language across subplots

### Solution
```python
# Consistent color scheme
colors = {
    'vessel': 'navy',      # #000080
    'jacket': 'darkred'    # #8B0000
}

# Applied consistently across all plots
ax.plot(vessel_data, color=colors['vessel'], label='Vessel End')
ax.plot(jacket_data, color=colors['jacket'], label='Jacket End')
```

### Color Psychology
- **Navy Blue (Vessel)**: Represents connection to vessel/floating structure
- **Dark Red (Jacket)**: Represents connection to fixed structure/foundation

## Version 6 (Final) - Annotations and Polish
### Additions
1. **Data completeness note**
   ```python
   ax.text(0.02, 0.98, 'Note: Analysis uses ALL data points',
           transform=ax.transAxes, fontsize=8, fontweight='bold')
   ax.text(0.02, 0.94, 'Time series sample shown for visualization',
           transform=ax.transAxes, fontsize=7)
   ```

2. **Enhanced labels**
   - Clear axis labels with units
   - Descriptive titles for each subplot
   - Legend positioning optimized

3. **Grid and formatting**
   - Alpha-blended grid (alpha=0.3)
   - Consistent font sizes
   - Professional appearance

## Final Plot Configuration Summary

### Layout
- **Figure Size**: 15 x 10 inches
- **Grid**: 2x2 subplots
- **DPI**: 100 (publication quality)

### Subplot Organization
1. **Top Left**: Time Series Sample (first 2000 points)
2. **Top Right**: Rainflow Histogram (combined)
3. **Bottom Left**: FFT Spectrum (0-2 Hz)
4. **Bottom Right**: Welch Power Spectral Density

### Visual Elements
| Element | Vessel End | Jacket End |
|---------|------------|------------|
| Color | Navy (#000080) | Dark Red (#8B0000) |
| Time Series | Solid line | Dashed line |
| FFT/Welch | Solid line | Dotted line |
| Line Width | 1.5 | 1.5 |
| Alpha | 0.8 | 0.8 |

### Key Features
- **Y-axis optimization**: 5th-99.5th percentile scaling
- **Frequency limit**: 0-2 Hz (relevant range for mooring dynamics)
- **Window averaging**: 137 windows with 75% overlap
- **Grid**: Light gray with 30% transparency
- **Legend**: Upper right position for all plots

## Processing Statistics Display
```python
# Information box on plot
stats_text = f"""
STATISTICS COMPARISON
===================
         Vessel End    Jacket End
Count:   {vessel_count}    {jacket_count}
Mean:    {vessel_mean:.2f}    {jacket_mean:.2f}
Std Dev: {vessel_std:.2f}    {jacket_std:.2f}
Min:     {vessel_min:.2f}    {jacket_min:.2f}
Max:     {vessel_max:.2f}    {jacket_max:.2f}
"""
```

## File Naming Convention
- Input: `fat001_fsts_l015_mwl_wave01_Strut1.csv`
- Rainflow: `fat001_fsts_l015_mwl_wave01_Strut1_Tension_Vessel_End_rainflow.csv`
- FFT: `fat001_fsts_l015_mwl_wave01_Strut1_Tension_Vessel_End_fft.csv`
- Plot: `fat001_fsts_l015_mwl_wave01_Strut1_analysis_plot.png`

## Technical Implementation Notes

### Memory Efficiency
- Process files individually to avoid memory issues
- Clear matplotlib figures after saving
- Use generators where possible

### Performance Optimization
- Vectorized operations with NumPy
- Efficient FFT using scipy.fft
- Parallel processing capability (not used in final version for simplicity)

### Error Handling
- Skip corrupted files with logging
- Handle missing columns gracefully
- Validate data types before processing