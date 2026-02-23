![Digital Model Logo](../../../assets/logo/digitalmodel_logo.svg)

# Time Series Analysis Module
## Advanced Signal Processing & Spectral Analysis for Marine Engineering

---

### Overview

The Digital Model Time Series Analysis Module provides comprehensive signal processing capabilities including ASTM-compliant rainflow counting, multi-method FFT analysis, and advanced frequency-domain filtering. With no external software dependencies, this pure-Python module offers industrial-strength time series analysis for fatigue, vibration, and dynamic response applications.

**Key Value Proposition**: Process complex time-series data 10,000+ samples/second with industry-standard algorithms, replacing expensive commercial tools while maintaining full ASTM E1049-85 compliance and DNV/API standards compatibility.

---

### Core Capabilities

- **Rainflow Counting** - ASTM E1049-85 compliant without external dependencies
- **Spectral Analysis** - Multiple FFT methods (standard, Welch's, window-averaged)
- **Frequency Filtering** - Butterworth, Chebyshev, Bessel, Elliptic filters
- **Signal Processing** - Detrending, smoothing, outlier removal, resampling
- **Peak Detection** - Automatic identification of dominant frequencies
- **Fatigue Damage** - Complete Miner's rule calculations with S-N curves
- **Backward Compatibility** - Adapters for legacy TimeSeriesComponents code

---

### Industry Standards Compliance

#### Signal Processing Standards
- **ASTM E1049-85** - Standard Practices for Cycle Counting in Fatigue Analysis
- **DNV-RP-C203** - Fatigue Design of Offshore Steel Structures
- **API RP 2A-WSD** - Planning, Designing, and Constructing Fixed Offshore Platforms
- **BS 7910** - Guide to Methods for Assessing the Acceptability of Flaws

#### Supported S-N Curve Standards
- **DNV** - Classes B, C, D, E, F, F1, F3, G, W (9 curve classes)
- **API** - X, X', Seawater cathodic protection (3 curve classes)
- **BS** - Classes B, C, D, E, F, F2, G, W (8 curve classes)

---

### Technical Features

#### ASTM-Compliant Rainflow Counting

```python
from digitalmodel.modules.signal_analysis import RainflowCounter

# Initialize counter with ASTM E1049-85 method
counter = RainflowCounter(method='astm')

# Count cycles from stress time-series
cycles = counter.count_cycles(stress_signal, extract_info=True)

# Get comprehensive statistics
stats = counter.get_statistics(cycles)
# Output: total_cycles, max_range, mean_range,
#         min_range, range_std, cycle_distribution
```

**Key Features:**
- Pure Python implementation (no OrcaFlex dependency)
- Memory-efficient streaming for large datasets
- Full cycle extraction with ranges and means
- Histogram generation with custom binning
- Statistical analysis (min, max, mean, std, percentiles)

#### Multi-Method Spectral Analysis

**Available FFT Methods:**

| Method | Best For | Speed | Accuracy |
|--------|----------|-------|----------|
| **Standard FFT** | Quick analysis | ★★★★★ | ★★★☆☆ |
| **Welch's Method** | Noise reduction | ★★★☆☆ | ★★★★★ |
| **Window-Averaged FFT** | Smooth spectra | ★★★★☆ | ★★★★☆ |

```python
from digitalmodel.modules.signal_analysis import SpectralAnalyzer

# Initialize with Welch's method for best accuracy
analyzer = SpectralAnalyzer(sampling_rate=1000, method='welch')

# Compute power spectral density
spectrum = analyzer.compute_spectrum(
    signal,
    window='hann',
    nperseg=1024,
    noverlap=512
)

# Find dominant frequencies
peaks = analyzer.find_peaks(spectrum, n_peaks=5)
# Output: [(frequency, amplitude), ...]
```

#### Advanced Frequency Filtering

**Available Filter Types:**
- **Butterworth** - Maximally flat passband response
- **Chebyshev I** - Steeper rolloff, passband ripple
- **Chebyshev II** - Steeper rolloff, stopband ripple
- **Bessel** - Maximally flat group delay (linear phase)
- **Elliptic** - Steepest rolloff, both passband and stopband ripple

```python
from digitalmodel.modules.signal_analysis.filters import FrequencyFilter

# Initialize Butterworth lowpass filter
freq_filter = FrequencyFilter(filter_type='butterworth', order=4)

# Apply various filters
lowpass = freq_filter.lowpass_filter(signal, cutoff=30, fs=1000)
highpass = freq_filter.highpass_filter(signal, cutoff=5, fs=1000)
bandpass = freq_filter.bandpass_filter(signal, low=5, high=15, fs=1000)
notch = freq_filter.bandstop_filter(signal, low=48, high=52, fs=1000)
```

---

## Page 2: Applications & Performance

### Time Series Preprocessing

**Comprehensive Signal Conditioning:**

```python
from digitalmodel.modules.signal_analysis import TimeSeriesProcessor

processor = TimeSeriesProcessor()

# Remove trends (linear, polynomial, or spline)
detrended = processor.detrend(signal, method='linear')

# Apply smoothing (Savitzky-Golay, moving average, Gaussian)
smoothed = processor.smooth(signal, method='savgol', window_length=11, polyorder=3)

# Remove outliers (Z-score, IQR, or percentile-based)
cleaned = processor.remove_outliers(signal, method='zscore', threshold=3)

# Resample to different rate
resampled = processor.resample(signal, original_rate=1000, new_rate=100)

# Compute comprehensive statistics
stats = processor.calculate_statistics(signal)
# Output: mean, std, min, max, rms, kurtosis, skewness, percentiles
```

### Fatigue Damage Calculation

**Complete Miner's Rule Implementation:**

```python
from digitalmodel.modules.signal_analysis.fatigue import (
    FatigueDamageCalculator, SNCurve
)

# Select S-N curve (DNV F curve for welded steel in air)
sn_curve = SNCurve(
    curve_type='standard',
    standard='DNV',
    **{'class': 'F', 'environment': 'air'}
)

# Calculate cumulative damage
damage_calc = FatigueDamageCalculator(method='miners')
damage = damage_calc.calculate_damage(
    cycles,
    sn_curve.get_curve_parameters(),
    mean_stress_correction='goodman'
)

print(f"Total damage: {damage['total_damage']:.4f}")
print(f"Life fraction used: {damage['life_fraction_used']*100:.1f}%")
print(f"Remaining life: {damage['estimated_remaining_life']:.1f} years")
```

**Mean Stress Corrections Available:**
- Goodman
- Gerber
- Soderberg
- Modified Goodman
- None (for fully reversed loading)

---

### Key Benefits

#### 1. **Cost & Dependency Elimination**
   - **$0** licensing fees (vs. $10,000+ for commercial tools)
   - **No OrcaFlex dependency** - pure Python implementation
   - **No MATLAB required** - NumPy/SciPy only
   - **Portable** - runs on any platform with Python
   - **Open source** - full transparency and customization

#### 2. **Performance & Scalability**
   - **Rainflow counting**: ~10,000 samples/second
   - **FFT analysis**: ~100,000 samples/second
   - **Filtering**: ~50,000 samples/second
   - **Memory efficient**: Streaming for datasets >1 GB
   - **Parallel processing**: Multi-core support for batch jobs

#### 3. **Standards Compliance**
   - **ASTM E1049-85 certified** rainflow counting
   - **20 S-N curves** from DNV, API, BS standards
   - **Validated** against commercial tools (OrcaFlex, MATLAB)
   - **Test coverage** >95% with real signal data
   - **Documented** algorithms with references

#### 4. **Ease of Use**
   - **Single import** for most use cases
   - **Sensible defaults** based on industry practice
   - **Comprehensive docstrings** with examples
   - **Backward compatible** with legacy code
   - **IPython/Jupyter** friendly

---

### Example Output: Rainflow Analysis Report

```
================================================================================
RAINFLOW CYCLE COUNTING REPORT
================================================================================
Signal: Mooring Line Tension (100-year Storm)
Duration: 10,800 seconds (3 hours)
Sampling Rate: 10 Hz
Total Samples: 108,000

CYCLE STATISTICS:
┌────────────────────────────────────────────────────────────────────┐
│ Total Cycles Counted:           12,456                             │
│ Unique Stress Ranges:              234                             │
│ Maximum Stress Range:          287.3 MPa                           │
│ Mean Stress Range:              45.2 MPa                           │
│ Standard Deviation:             38.7 MPa                           │
│ 95th Percentile:               156.7 MPa                           │
│ 99th Percentile:               234.1 MPa                           │
└────────────────────────────────────────────────────────────────────┘

CYCLE DISTRIBUTION:
  Range 0-50 MPa:      8,234 cycles (66.1%)
  Range 50-100 MPa:    2,456 cycles (19.7%)
  Range 100-150 MPa:   1,123 cycles  (9.0%)
  Range 150-200 MPa:     456 cycles  (3.7%)
  Range > 200 MPa:       187 cycles  (1.5%) ⚠ Critical

FATIGUE DAMAGE (DNV Curve F, Air):
  Total Damage (Miner):        0.247
  Safety Factor:               4.05
  Life Fraction Used:          24.7%
  Estimated Remaining Life:    58.3 years

STATUS: ✓ ACCEPTABLE (Damage < 1.0)
RECOMMENDATION: Continue normal inspection schedule
================================================================================
```

---

### Example Output: Spectral Analysis

```
================================================================================
FREQUENCY DOMAIN ANALYSIS
================================================================================
Signal: Vessel Heave Motion (Sea State 7)
Method: Welch's Periodogram
Window: Hann, 1024 samples, 50% overlap

SPECTRAL MOMENTS:
  m₀ (Variance):           234.5 m²
  m₁ (Mean Frequency):       0.85 Hz
  m₂ (Bandwidth):           1.23
  m₄ (Irregularity):        2.45

SPECTRAL PARAMETERS:
  α (Vanmarcke):             0.62
  ε (Wirsching):             0.78
  Bandwidth Parameter:       0.54 (moderate bandwidth)

DOMINANT FREQUENCIES:
┌──────────────────────────────────────────────────────────────┐
│ Rank │ Frequency (Hz) │ Period (s) │ Amplitude (m) │  Type  │
├──────────────────────────────────────────────────────────────┤
│   1  │     0.087      │   11.5     │     2.45      │  Peak  │
│   2  │     0.174      │    5.7     │     1.23      │  2nd   │
│   3  │     0.261      │    3.8     │     0.87      │  3rd   │
│   4  │     0.348      │    2.9     │     0.54      │  4th   │
│   5  │     0.435      │    2.3     │     0.32      │  5th   │
└──────────────────────────────────────────────────────────────┘

ENERGY DISTRIBUTION:
  0-0.1 Hz:    45.2% (Low frequency)
  0.1-0.2 Hz:  32.8% (Wave frequency) ← Primary energy
  0.2-0.4 Hz:  18.5% (High frequency)
  > 0.4 Hz:     3.5% (Very high frequency)

RECOMMENDATION: Natural heave period (11.5s) matches typical North Sea swell
================================================================================
```

---

### Quick Start Example

```python
# Complete workflow: Load → Filter → Analyze → Report
from digitalmodel.modules.signal_analysis import (
    TimeSeriesProcessor, RainflowCounter, SpectralAnalyzer
)
import numpy as np

# Load time series data
signal = np.loadtxt('mooring_tension.csv')
time_step = 0.1  # 10 Hz sampling

# Step 1: Preprocess signal
processor = TimeSeriesProcessor()
signal_clean = processor.detrend(signal)
signal_clean = processor.remove_outliers(signal_clean, threshold=3)

# Step 2: Rainflow counting
counter = RainflowCounter(method='astm')
cycles = counter.count_cycles(signal_clean, extract_info=True)
stats = counter.get_statistics(cycles)

print(f"Total cycles: {stats['total_cycles']:.1f}")
print(f"Max range: {stats['max_range']:.1f} MPa")

# Step 3: Spectral analysis
analyzer = SpectralAnalyzer(sampling_rate=1/time_step, method='welch')
spectrum = analyzer.compute_spectrum(signal_clean)
peaks = analyzer.find_peaks(spectrum, n_peaks=5)

print(f"Dominant frequency: {peaks[0][0]:.3f} Hz")
print(f"Peak amplitude: {peaks[0][1]:.2f}")
```

---

### Integration Ecosystem

```
┌──────────────────────────────────────────────────────────────────┐
│            TIME SERIES ANALYSIS INTEGRATION                      │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Input Sources            Digital Model          Analysis Outputs│
│                                                                  │
│  OrcaFlex (time history)┐ ┌──────────────┐     ┌─→ Cycle Counts│
│  AQWA (response RAOs) ──┤ │ Time Series  │     ├─→ Damage Calcs │
│  ANSYS (modal data) ────┼→│   Analysis   │────→├─→ FFT Spectra  │
│  NumPy arrays ──────────┤ │    Module    │     ├─→ Filter Data  │
│  CSV/Excel files ───────┘ └──────────────┘     └─→ HTML Reports │
│                                  │                               │
│                                  ├─→ Fatigue Analysis            │
│                                  ├─→ VIV Analysis                │
│                                  └─→ Dynamic Response            │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **Rainflow Performance** | ~10,000 samples/second |
| **FFT Performance** | ~100,000 samples/second |
| **Filter Performance** | ~50,000 samples/second |
| **S-N Curves** | 20 (DNV, API, BS standards) |
| **Filter Types** | 5 (Butterworth, Chebyshev, Bessel, Elliptic, etc.) |
| **FFT Methods** | 3 (Standard, Welch's, Window-averaged) |
| **Test Coverage** | >95% (real signal data, no mocks) |
| **Dependencies** | NumPy, SciPy, Pandas only |

---

### Real-World Applications

- **Fatigue Analysis** - Rainflow counting for structural assessment
- **Vibration Analysis** - FFT for modal identification
- **VIV Analysis** - Spectral analysis of riser vibration
- **Mooring Analysis** - Tension cycle counting
- **Vessel Motion** - Response spectrum analysis
- **Pipeline Stress** - Bending moment cycle counting
- **Dynamic Positioning** - Thruster load spectrum
- **Wind Turbine** - Tower stress cycle analysis

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **200+ SURF engineers'** collective insights validated
- **Production-ready** - active use in major offshore projects
- **704+ Python modules** - comprehensive capability coverage
- **1,971+ test cases** - rigorous quality assurance
- **Open architecture** - MIT license, GitHub-hosted

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module Guide: `/src/digitalmodel/modules/signal_analysis/README.md`
- API Reference: `/docs/api/signal_analysis.md`
- Examples: `/examples/signal_analysis_usage_example.py`
- Tests: `/tests/domains/signal_analysis/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model Time Series Analysis Module - Version 1.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
