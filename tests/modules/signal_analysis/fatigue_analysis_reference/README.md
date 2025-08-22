# OrcaFlex Fatigue Analysis Reference

This directory contains reference materials for the OrcaFlex fatigue analysis workflow implemented for processing tension data from mooring struts.

## Directory Structure

```
fatigue_analysis_reference/
├── README.md              # This file
├── raw_data/             # Sample input data files
│   └── fat002_fsts_sample.csv  # Sample OrcaFlex CSV export
└── sample_outputs/       # Example output files
    ├── *_rainflow.csv    # Rainflow counting results
    ├── *_fft.csv         # FFT analysis results
    └── *_plot.png        # Combined analysis plots
```

## Sample Data

The `raw_data/` directory contains sample CSV files exported from OrcaFlex with columns:
- `time` - Time series in seconds
- `Tension (Vessel End)` - Tension at vessel connection point (kN)
- `Tension (Jacket End)` - Tension at jacket connection point (kN)

## Sample Outputs

The `sample_outputs/` directory contains example results from processing:
- **Rainflow CSV files**: Cycle counts with amplitude ranges and mean values
- **FFT CSV files**: Frequency spectrum data with Hz and Power columns
- **Combined plots**: Visual representation of time series, rainflow histogram, FFT, and Welch PSD

## Configuration Used

```python
config = {
    'analysis': {
        'fft': {
            'window_size': 1024,     # Small window for more averaging
            'overlap': 0.75,         # 75% overlap for smooth spectrum
            'window_type': 'hann',   # Hann window to reduce leakage
            'frequency_limit': 2.0   # Focus on 0-2 Hz range
        }
    }
}
```

## Visual Design

- **Colors**: Navy blue (vessel end), Dark red (jacket end)
- **Line styles**: Solid (vessel), Dashed/dotted (jacket)
- **Y-axis scaling**: Percentile-based (5th to 99.5th) for optimal visibility

## Usage

For Python users, see the test file:
`tests/modules/signal_analysis/test_orcaflex_fatigue_analysis.py`

For complete documentation, see:
`tests/modules/signal_analysis/FATIGUE_ANALYSIS_REFERENCE.md`