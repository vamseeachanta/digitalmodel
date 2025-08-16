# Rainflow Counting and FFT/Spectral Analysis Code Consolidation

## Executive Summary
Consolidate multiple implementations of rainflow counting and FFT/spectral analysis into a unified, comprehensive signal analysis module that operates independently of proprietary packages like OrcaFlex, while maintaining all existing functionality and improving code reusability.

## Problem Statement

### Current Issues
1. **Fragmented Implementations**: 13+ files contain rainflow counting implementations scattered across the codebase
2. **Dependency Issues**: Some implementations require OrcaFlex API license which limits usage
3. **Code Duplication**: Multiple similar implementations with slight variations
4. **Lack of Documentation**: No central documentation for signal analysis capabilities
5. **Inconsistent APIs**: Different interfaces for similar functionality across modules

### Identified Implementations

#### Rainflow Counting Locations
1. **Primary Implementation**: `src/digitalmodel/modules/time_series/time_series_components.py`
   - Uses `rainflow` Python package
   - ASTM E1049-85 compliant
   - Returns DataFrame with range, mean, count, i_start, i_end

2. **Fatigue Analysis Wrappers**:
   - `src/digitalmodel/common/fatigue_analysis.py`
   - `src/digitalmodel/common/ship_fatigue_analysis.py`
   - `src/digitalmodel/modules/catenary/getFatigueLoading.py`

3. **OrcaFlex Dependent**:
   - `docs/modules/orcaflex/scripts/orcfxapi_goby/27 - RainflowHalfCycles.py`
   - `src/digitalmodel/modules/orcaflex/opp_time_series.py`

#### FFT/Spectral Analysis Locations
1. **Primary Implementation**: `src/digitalmodel/modules/time_series/time_series_components.py`
   - Window averaging FFT
   - Band-pass filtering
   - Peak detection
   - Power spectral density
   - Moving average smoothing

2. **Supporting Implementations**:
   - `src/digitalmodel/time_series.py`
   - `src/digitalmodel/modules/pyintegrity/common/math_solvers.py`
   - `src/digitalmodel/modules/catenary/catenaryMethods.py`

## Requirements

### Functional Requirements

#### Core Signal Analysis Module
1. **Rainflow Counting**
   - ASTM E1049-85 compliant algorithm
   - Support for half-cycle and full-cycle counting
   - Configurable counting parameters
   - Output formats: DataFrame, dictionary, numpy array
   - Statistical summaries (mean stress, range distribution)

2. **FFT/Spectral Analysis**
   - Standard FFT with configurable parameters
   - Window-averaged FFT for long time series
   - Welch's method for power spectral density
   - Configurable window functions (Hanning, Hamming, Blackman)
   - Band-pass and band-stop filtering
   - Peak detection with configurable thresholds
   - Phase information preservation
   - RAO (Response Amplitude Operator) calculation

3. **Time Series Processing**
   - Resampling and interpolation
   - Detrending (linear, polynomial)
   - Signal smoothing (moving average, Savitzky-Golay)
   - Outlier detection and removal
   - Signal statistics (RMS, variance, kurtosis, skewness)

### Non-Functional Requirements
1. **Independence**: No dependency on proprietary software (OrcaFlex, ANSYS)
2. **Performance**: Process 1M+ data points in under 10 seconds
3. **Memory Efficiency**: Streaming capability for large datasets
4. **Compatibility**: Python 3.8+ support
5. **Testing**: 95%+ code coverage
6. **Documentation**: Comprehensive API documentation with examples

## Technical Design

### Architecture

```
signal_analysis/
├── __init__.py
├── core/
│   ├── __init__.py
│   ├── rainflow.py          # Rainflow counting algorithms
│   ├── spectral.py          # FFT and spectral analysis
│   └── timeseries.py        # Time series utilities
├── fatigue/
│   ├── __init__.py
│   ├── damage.py            # Fatigue damage calculations
│   └── curves.py            # S-N curve implementations
├── filters/
│   ├── __init__.py
│   ├── frequency.py         # Frequency domain filters
│   └── time.py              # Time domain filters
├── visualization/
│   ├── __init__.py
│   └── plots.py             # Signal visualization utilities
└── utils/
    ├── __init__.py
    ├── validation.py        # Input validation
    └── io.py               # File I/O utilities
```

### Core Components

#### 1. Rainflow Counter Class
```python
class RainflowCounter:
    def __init__(self, method='astm', bin_count=None):
        """
        Initialize rainflow counter
        
        Parameters:
        - method: 'astm', 'simplified', 'half_cycle', 'full_cycle'
        - bin_count: Number of bins for histogram (optional)
        """
        
    def count_cycles(self, signal, extract_info=True):
        """
        Count rainflow cycles in signal
        
        Returns:
        - DataFrame with cycle information
        """
        
    def get_histogram(self, cycles_df, bins=None):
        """Generate cycle histogram"""
        
    def get_statistics(self, cycles_df):
        """Calculate cycle statistics"""
```

#### 2. Spectral Analyzer Class
```python
class SpectralAnalyzer:
    def __init__(self, sampling_rate=None, method='fft'):
        """
        Initialize spectral analyzer
        
        Parameters:
        - sampling_rate: Sample rate in Hz
        - method: 'fft', 'welch', 'periodogram'
        """
        
    def compute_spectrum(self, signal, **kwargs):
        """Compute frequency spectrum"""
        
    def window_averaged_fft(self, signal, window_size, overlap=0.5):
        """Compute window-averaged FFT"""
        
    def find_peaks(self, spectrum, threshold=None, n_peaks=None):
        """Identify spectral peaks"""
        
    def filter_spectrum(self, spectrum, filter_type, **params):
        """Apply frequency domain filtering"""
```

#### 3. Time Series Processor Class
```python
class TimeSeriesProcessor:
    def __init__(self):
        """Initialize time series processor"""
        
    def detrend(self, signal, method='linear'):
        """Remove trend from signal"""
        
    def smooth(self, signal, method='moving_average', **params):
        """Smooth signal"""
        
    def resample(self, signal, original_rate, target_rate):
        """Resample signal to different rate"""
        
    def remove_outliers(self, signal, method='zscore', threshold=3):
        """Remove outliers from signal"""
```

### Integration Points

1. **Existing Fatigue Analysis**: Maintain backward compatibility with existing fatigue modules
2. **OrcaFlex Migration Path**: Provide adapter classes for OrcaFlex-dependent code
3. **Data Sources**: Support for CSV, HDF5, MAT files, and numpy arrays
4. **Visualization**: Integration with matplotlib and plotly

## Implementation Plan

### Phase 1: Core Module Setup (Week 1-2)
1. Create module structure
2. Implement basic rainflow counting
3. Implement basic FFT analysis
4. Set up testing framework

### Phase 2: Feature Consolidation (Week 3-4)
1. Migrate existing rainflow implementations
2. Migrate FFT/spectral implementations
3. Standardize APIs
4. Create compatibility layers

### Phase 3: Enhancement & Optimization (Week 5-6)
1. Add advanced features (window functions, filters)
2. Optimize performance for large datasets
3. Implement streaming capabilities
4. Add parallel processing support

### Phase 4: Documentation & Testing (Week 7-8)
1. Write comprehensive documentation
2. Create example notebooks
3. Achieve 95% test coverage
4. Performance benchmarking

## Success Criteria

1. **Consolidation**: All rainflow and FFT code consolidated into single module
2. **Independence**: No OrcaFlex or proprietary dependencies in core module
3. **Performance**: 10x faster than current implementations for large datasets
4. **Coverage**: 95%+ test coverage
5. **Documentation**: Complete API documentation with 10+ examples
6. **Backward Compatibility**: All existing code continues to work with adapters

## Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing code | High | Create comprehensive compatibility layer |
| Performance regression | Medium | Benchmark against current implementations |
| Missing functionality | Medium | Thorough inventory of all current features |
| User adoption | Low | Provide migration guide and examples |

## Dependencies

### Required Python Packages
- numpy >= 1.20.0
- scipy >= 1.7.0
- pandas >= 1.3.0
- rainflow >= 3.0.0 (for validation only)

### Optional Packages
- numba (for JIT compilation)
- matplotlib (for visualization)
- plotly (for interactive plots)

## Appendix: Current Implementation Analysis

### Most Comprehensive Non-OrcaFlex Implementation
**File**: `src/digitalmodel/modules/time_series/time_series_components.py`

**Key Features**:
- Window-averaged FFT with configurable window size
- Moving average smoothing
- Band-pass filtering
- Peak detection
- Power spectral density calculation
- Integration with pandas DataFrames
- Rainflow counting using standard rainflow package

**Usage Example**:
```python
from digitalmodel.modules.time_series import TimeSeriesComponents

tsc = TimeSeriesComponents(cfg)

# Rainflow counting
df, cycles_dict = tsc.get_rainflow_count_from_time_series(signal)

# FFT analysis
fft_df = tsc.fft_window_analysis(cfg, signal, time_step)

# Filtered FFT
filtered_fft = tsc.get_filtered_fft(cfg, fft_df)
```

This implementation should serve as the foundation for the consolidated module.