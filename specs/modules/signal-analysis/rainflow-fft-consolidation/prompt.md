# Prompt Documentation: Rainflow Counting and FFT/Spectral Analysis Consolidation

## Original User Request

The user requested consolidation of rainflow counting and FFT/spectral analysis code with the following requirements:
1. Review and document all existing rainflow codes in the repository
2. Identify the most comprehensive implementation that runs without proprietary dependencies (no OrcaFlex)
3. Document the usage of the identified implementation
4. Identify FFT/spectral analysis implementations that include window averaging and peak detection

## Analysis Performed

### Discovery Process
1. **Code Search**: Searched for rainflow and FFT/spectral analysis implementations across the codebase
2. **Implementation Review**: Examined 13 files containing rainflow code and 18 files with FFT/spectral analysis
3. **Dependency Analysis**: Identified which implementations require proprietary software licenses
4. **Feature Inventory**: Cataloged features across all implementations

### Key Findings

#### Rainflow Implementations Found
- **13 files** contain rainflow-related code
- Primary implementation in `src/digitalmodel/modules/time_series/time_series_components.py`
- Uses open-source `rainflow` Python package (ASTM E1049-85 compliant)
- Several wrapper implementations for fatigue analysis
- OrcaFlex-dependent implementations that require licensing

#### FFT/Spectral Analysis Implementations Found
- **18 files** contain FFT/spectral analysis code
- Most comprehensive in `src/digitalmodel/modules/time_series/time_series_components.py`
- Features include:
  - Window-averaged FFT for long time series
  - Band-pass and band-stop filtering
  - Peak detection with configurable thresholds
  - Power spectral density calculation
  - Moving average smoothing
  - RAO (Response Amplitude Operator) calculation

### Most Comprehensive Non-Proprietary Implementation

**File**: `src/digitalmodel/modules/time_series/time_series_components.py`

**Class**: `TimeSeriesComponents`

**Key Features**:
1. **Rainflow Counting**:
   - Method: `get_rainflow_count_from_time_series()`
   - ASTM E1049-85 compliant via rainflow package
   - Returns DataFrame with: range, mean, count, i_start, i_end

2. **FFT/Spectral Analysis**:
   - `window_average_fft()`: Window-based FFT with averaging
   - `fft_window_analysis()`: Configurable window size and overlap
   - `get_filtered_fft()`: Frequency domain filtering
   - `get_filtered_signal()`: Band-pass filtering
   - Peak detection with `find_peaks_in_fft()`
   - Power spectral density calculation

## Specification Created

### Deliverables
1. **spec.md**: Comprehensive specification for code consolidation
2. **tasks.md**: Detailed task breakdown with effort estimates
3. **prompt.md**: This documentation file

### Design Highlights
1. **Modular Architecture**: Separated into core, fatigue, filters, and visualization modules
2. **No Proprietary Dependencies**: Core module operates independently
3. **Backward Compatibility**: Adapter classes for existing code
4. **Performance Focus**: Optimizations for large datasets
5. **Comprehensive Testing**: 95% coverage target

## Usage Examples

### Current Implementation Usage

```python
from digitalmodel.modules.time_series import TimeSeriesComponents

# Initialize with configuration
cfg = {
    "fft": {
        "window": {"size": 1024},
        "filter": {
            "flag": True,
            "min_frequency": 0.1,
            "max_frequency": 10.0
        },
        "peaks": {
            "flag": True,
            "min_peak_distance": 10
        }
    }
}

tsc = TimeSeriesComponents(cfg)

# Rainflow counting
signal = [...]  # Your time series data
cycles_df, cycles_dict = tsc.get_rainflow_count_from_time_series(signal)

# Window-averaged FFT
time_step = 0.1  # seconds
fft_df = tsc.fft_window_analysis(cfg, signal, time_step)

# Get filtered FFT
filtered_fft = tsc.get_filtered_fft(cfg, fft_df)
```

### Proposed Consolidated Module Usage

```python
from signal_analysis import RainflowCounter, SpectralAnalyzer

# Rainflow counting
counter = RainflowCounter(method='astm')
cycles = counter.count_cycles(signal)
histogram = counter.get_histogram(cycles, bins=50)

# Spectral analysis
analyzer = SpectralAnalyzer(sampling_rate=10)  # 10 Hz
spectrum = analyzer.window_averaged_fft(signal, window_size=1024)
peaks = analyzer.find_peaks(spectrum, threshold=0.1)
```

## Reuse Prompt for Similar Tasks

For future signal processing consolidation tasks, use this template:

```
Create a specification for consolidating [SIGNAL_PROCESSING_TYPE] implementations in the repository.

Requirements:
1. Review all existing [SIGNAL_PROCESSING_TYPE] implementations
2. Identify implementations with external dependencies
3. Find the most comprehensive implementation without proprietary dependencies
4. Document current usage patterns
5. Design a consolidated module with:
   - Modular architecture
   - No proprietary dependencies in core
   - Backward compatibility
   - Performance optimizations
   - Comprehensive testing

Deliverables:
- spec.md: Technical specification
- tasks.md: Task breakdown with estimates
- prompt.md: Documentation of analysis and decisions

Focus on:
- Code reusability
- Performance for large datasets
- Clear API design
- Migration path from existing code
```

## Lessons Learned

1. **Multiple Implementations**: Large codebases often have duplicate functionality that evolved independently
2. **Dependency Management**: Proprietary dependencies limit code reusability
3. **Feature Preservation**: Consolidation must maintain all existing features
4. **Migration Strategy**: Backward compatibility is crucial for adoption
5. **Documentation Value**: Comprehensive documentation enables successful consolidation

## Next Steps

1. Review and approve the specification
2. Prioritize features for initial implementation
3. Set up module structure and testing framework
4. Begin incremental migration of existing code
5. Create migration guide for users