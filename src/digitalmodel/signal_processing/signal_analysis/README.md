# Signal Analysis Module

A comprehensive, unified signal analysis module for Digital Model that consolidates all rainflow counting, FFT/spectral analysis, and fatigue damage calculation capabilities into a single, well-organized package.

## Features

### Core Capabilities
- **Rainflow Counting**: ASTM E1049-85 compliant implementation without external dependencies
- **Spectral Analysis**: Multiple FFT methods including standard FFT, Welch's method, and window-averaged FFT
- **Fatigue Analysis**: Complete fatigue damage calculations with standard S-N curves (DNV, API, BS)
- **Signal Filtering**: Advanced frequency domain filtering (Butterworth, Chebyshev, Bessel, Elliptic)
- **Time Series Processing**: Detrending, smoothing, outlier removal, and resampling
- **Peak Detection**: Automatic identification of dominant frequencies in spectral data
- **Backward Compatibility**: Adapters for legacy TimeSeriesComponents code

### Key Advantages
- **No OrcaFlex Dependency**: Pure Python implementation using only NumPy and SciPy
- **Industry Standards**: Compliant with ASTM E1049-85, DNV, API, and BS standards
- **Modular Design**: Clean separation of concerns with dedicated submodules
- **Full Test Coverage**: Comprehensive unit and integration tests
- **Legacy Support**: Seamless migration path from existing code

## Installation

The module is part of the Digital Model package and requires:
```python
numpy>=1.20.0
scipy>=1.7.0
pandas>=1.3.0
```

## Module Structure

```
signal_analysis/
├── core/
│   ├── rainflow.py         # ASTM E1049-85 rainflow counting
│   ├── spectral.py         # FFT and spectral analysis
│   └── timeseries.py       # Time series preprocessing
├── fatigue/
│   ├── damage.py           # Miner's rule damage calculation
│   └── curves.py           # S-N curve implementations
├── filters/
│   └── frequency.py        # Frequency domain filtering
├── adapters/
│   └── legacy.py          # Backward compatibility adapters
└── __init__.py            # Module exports
```

## Quick Start

### Basic Rainflow Counting
```python
from digitalmodel.signal_processing.signal_analysis import RainflowCounter

# Initialize counter
counter = RainflowCounter(method='astm')

# Count cycles from stress signal
cycles = counter.count_cycles(stress_signal, extract_info=True)

# Get statistics
stats = counter.get_statistics(cycles)
print(f"Total cycles: {stats['total_cycles']:.1f}")
print(f"Max range: {stats['max_range']:.1f} MPa")
```

### Spectral Analysis
```python
from digitalmodel.signal_processing.signal_analysis import SpectralAnalyzer

# Initialize analyzer
analyzer = SpectralAnalyzer(sampling_rate=1000, method='welch')

# Compute spectrum
spectrum = analyzer.compute_spectrum(signal)

# Find peaks
peaks = analyzer.find_peaks(spectrum, n_peaks=5)
```

### Fatigue Damage Calculation
```python
from digitalmodel.signal_processing.signal_analysis.fatigue import (
    FatigueDamageCalculator, SNCurve
)

# Select S-N curve (DNV F curve for welded joints)
sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'F'})

# Calculate damage
damage_calc = FatigueDamageCalculator(method='miners')
damage = damage_calc.calculate_damage(
    cycles,
    sn_curve.get_curve_parameters(),
    mean_stress_correction='goodman'
)

print(f"Total damage: {damage['total_damage']:.2e}")
```

### Signal Filtering
```python
from digitalmodel.signal_processing.signal_analysis.filters import FrequencyFilter

# Initialize filter
freq_filter = FrequencyFilter(filter_type='butterworth', order=4)

# Apply filters
filtered_lp = freq_filter.lowpass_filter(signal, cutoff=30, fs=1000)
filtered_bp = freq_filter.bandpass_filter(signal, 5, 15, fs=1000)
filtered_notch = freq_filter.bandstop_filter(signal, 48, 52, fs=1000)
```

## API Reference

### RainflowCounter
Main class for rainflow cycle counting.

**Methods:**
- `count_cycles(signal, extract_info=True)`: Extract cycles from time series
- `get_histogram(cycles_df, bins=None)`: Generate cycle histogram
- `get_statistics(cycles_df)`: Calculate cycle statistics

### SpectralAnalyzer
Comprehensive spectral analysis tools.

**Methods:**
- `compute_spectrum(signal, **kwargs)`: Compute frequency spectrum
- `window_averaged_fft(signal, window_size, overlap)`: Window-averaged FFT
- `find_peaks(spectrum, threshold=None, n_peaks=None)`: Detect spectral peaks
- `filter_spectrum(spectrum, filter_type, **kwargs)`: Apply frequency filtering

### TimeSeriesProcessor
Time series preprocessing utilities.

**Methods:**
- `detrend(signal, method='linear')`: Remove trends
- `smooth(signal, method='savgol', **kwargs)`: Apply smoothing
- `remove_outliers(signal, method='zscore', threshold=3)`: Remove outliers
- `resample(signal, original_rate, new_rate, method='linear')`: Resample signal
- `calculate_statistics(signal)`: Compute signal statistics

### FatigueDamageCalculator
Fatigue damage assessment using Miner's rule.

**Methods:**
- `calculate_damage(cycles_df, sn_params, mean_stress_correction=None)`: Calculate cumulative damage
- `calculate_life(cycles_df, sn_params, target_damage=1.0)`: Estimate fatigue life

### SNCurve
S-N curve implementations for fatigue analysis.

**Supported Standards:**
- DNV (Classes B, C, D, E, F, F1, F3, G, W)
- API (X, X', Seawater cathodic protection)
- BS (Classes B, C, D, E, F, F2, G, W)

## Migration from Legacy Code

For existing code using TimeSeriesComponents:

```python
from digitalmodel.signal_processing.signal_analysis.adapters import TimeSeriesComponentsAdapter

# Use existing configuration
adapter = TimeSeriesComponentsAdapter(legacy_config)

# Legacy methods still work (with deprecation warnings)
cycles_df, cycles_dict = adapter.get_rainflow_count_from_time_series(signal)
fft_result = adapter.window_average_fft(config, signal, time_step)
```

## Examples

Complete working examples are available in:
- `examples/signal_analysis_usage_example.py` - Comprehensive feature demonstration
- `tests/modules/signal_analysis/test_integration.py` - Integration test scenarios

## Testing

Run tests with:
```bash
# Unit tests
python -m pytest tests/modules/signal_analysis/

# Integration tests
python tests/modules/signal_analysis/test_integration.py

# Usage examples
python examples/signal_analysis_usage_example.py
```

## Performance

- **Rainflow Counting**: ~10,000 samples/second
- **FFT Analysis**: ~100,000 samples/second
- **Filtering**: ~50,000 samples/second
- **Memory Efficient**: Streaming capabilities for large datasets

## Contributing

When adding new features:
1. Follow the existing module structure
2. Add comprehensive docstrings
3. Include unit tests
4. Update integration tests
5. Document in this README

## License

Part of Digital Model - Proprietary Software

## References

- ASTM E1049-85: Standard Practices for Cycle Counting in Fatigue Analysis
- DNV-RP-C203: Fatigue Design of Offshore Steel Structures
- API RP 2A-WSD: Planning, Designing, and Constructing Fixed Offshore Platforms
- BS 7910: Guide to Methods for Assessing the Acceptability of Flaws in Metallic Structures