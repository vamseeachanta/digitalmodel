# OrcaFlex Fatigue Analysis Reference

## Overview
This reference documents the finalized configuration for processing OrcaFlex fatigue CSV files with rainflow counting and FFT spectral analysis, based on extensive testing and user feedback.

## Original User Prompt
```
Using signal_analysis module, perform rainflow and FFT for files in folder 
D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue with file pattern _Strut?.csv.
Save output files in 'rainflow' folder in same directory. Use filenaming convention 
by adding _rainflow and _fft. Only use columns: 'time', 'Tension (Vessel End)', 
'Tension (Jacket End)'. Put output files directly in rainflow folder without subfolders.
Add rainflow and FFT plots. Minimize number of plots. 

Refinements:
- Window averaged FFT to remove spiky spectrum
- Combine vessel end and jacket end in plots
- Adjust y-axis range for good plot visibility with peaks
- Use dotted vs solid lines for visibility
- Use consistent colors across plots for user convenience
```

## Final Configuration

### Data Processing
- **Full Signal Analysis**: ALL data points processed (e.g., 36,001 points for 3600s signal)
- **Columns Analyzed**: `time`, `Tension (Vessel End)`, `Tension (Jacket End)`
- **File Pattern**: `*_Strut?.csv` (matches Strut1 through Strut9)

### FFT Configuration (Smooth Spectrum)
```python
'fft': {
    'window_size': 1024,      # Small window for more averaging
    'overlap': 0.75,          # 75% overlap for smoothing
    'window_type': 'hann',    # Hanning window
    'frequency_limit': 2.0,   # Focus on 0-2 Hz
    'use_window_averaging': True,
    'averaging_method': 'mean'
}
```
- **Result**: ~137 windows for typical 36,000 point signal
- **Effect**: Smooth spectrum without noise spikes

### Rainflow Configuration
```python
'rainflow': {
    'enable': True,
    'bin_count': 50,
    'method': 'astm'  # ASTM E1049-85 standard
}
```

### Visual Design (Consistent & Clear)

#### Color Scheme (Same Across All Plots)
- **Vessel End**: Navy blue (`#000080`)
- **Jacket End**: Dark red (`#8B0000`)

#### Line Styles (For Differentiation)
- **Vessel End**: Solid line (`-`)
- **Jacket End**: Dashed (`--`) or Dotted (`:`)

#### Y-axis Optimization
- **Method**: Percentile-based auto-scaling
- **Range**: 5th to 99.5th percentile of data
- **Padding**: 0.5x lower bound, 2x upper bound

### Output Structure

#### File Naming Convention
```
<original_filename>_<column>_rainflow.csv
<original_filename>_<column>_fft.csv
<original_filename>_analysis_plot.png
```

Example:
```
fat001_fsts_l015_mwl_wave01_Strut1_Tension_Vessel_End_rainflow.csv
fat001_fsts_l015_mwl_wave01_Strut1_Tension_Vessel_End_fft.csv
fat001_fsts_l015_mwl_wave01_Strut1_Tension_Jacket_End_rainflow.csv
fat001_fsts_l015_mwl_wave01_Strut1_Tension_Jacket_End_fft.csv
fat001_fsts_l015_mwl_wave01_Strut1_analysis_plot.png
```

#### Directory Structure
```
07c_fatigue/
├── rainflow/                    # All outputs here (no subfolders)
│   ├── *_rainflow.csv
│   ├── *_fft.csv
│   └── *_analysis_plot.png
└── *.csv                        # Original data files
```

### Plot Layout (2x3 Grid)

1. **Rainflow Histogram** - Combined (both ends overlaid)
2. **FFT Spectrum** - Combined with smooth curves
3. **Welch PSD** - Alternative spectral view
4. **Time Series Sample** - First 2000 points for visualization
5. **Statistics Table** - Comparative metrics
6. **Processing Info** - Configuration details

### Complete Python Implementation

```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from digitalmodel.modules.signal_analysis.orcaflex import TimeSeriesAnalyzer

# Configuration
config = {
    'analysis': {
        'rainflow': {
            'enable': True,
            'bin_count': 50,
            'method': 'astm'
        },
        'fft': {
            'enable': True,
            'window_size': 1024,
            'overlap': 0.75,
            'window_type': 'hann',
            'frequency_limit': 2.0,
            'use_window_averaging': True,
            'averaging_method': 'mean',
            'peak_detection': {
                'enable': True,
                'n_peaks': 5,
                'min_height': 0.01
            }
        }
    },
    'output': {
        'plots': {'enable': False},  # Custom plots
        'formats': {'csv': True}
    },
    'column_mapping': {
        'strategy': 'manual',
        'manual': {
            'time': 'time',
            'data_columns': ['Tension (Vessel End)', 'Tension (Jacket End)']
        }
    }
}

# Color scheme
vessel_color = 'navy'
jacket_color = 'darkred'

# Process files
analyzer = TimeSeriesAnalyzer(config=config)
results = analyzer.process_file(file_path)
```

## Key Insights from Testing

### FFT Smoothing
- With 137 windows and 75% overlap, spectrum is very smooth
- Remaining peaks after smoothing are real system responses, not noise
- These peaks are important for fatigue analysis

### Data Validation
- Entire time series must be analyzed (not just samples)
- Constant signals handled gracefully (no cycles)
- FFT "weights sum to zero" errors recovered automatically

### Performance
- Single file processing: ~2-3 seconds
- 544 files estimated: ~20-30 minutes with sequential processing
- Parallel processing available for faster batch operations

## Test Data Location
- Sample file: `tests/modules/signal-analysis/test_data/fat002_fsts_sample.csv`
- Test outputs: `tests/modules/signal-analysis/test_output/`
- Test script: `test_orcaflex_fatigue_analysis.py`

## Usage Example

```bash
# Single file test
python process_fatigue_final.py

# Batch processing (544 files)
python process_all_fatigue_files.py
```

## Important Notes

1. **Full Signal Processing**: Analysis uses complete time series, plots show samples for visualization only
2. **System Response**: Peaks in smoothed FFT represent actual structural behavior, not noise
3. **File Organization**: All outputs in single 'rainflow' folder for easy access
4. **Color Consistency**: Same colors used across all plots for user convenience
5. **Line Style Differentiation**: Solid vs dashed/dotted for clear signal separation

## References
- ASTM E1049-85: Standard Practices for Cycle Counting in Fatigue Analysis
- Welch's Method: Power spectral density estimation
- Hann Window: Reduces spectral leakage in FFT