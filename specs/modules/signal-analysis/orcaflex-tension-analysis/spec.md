# OrcaFlex Tension Data Rainflow and FFT Analysis

## Executive Summary
Implement comprehensive rainflow counting and FFT spectral analysis for OrcaFlex tension data from structural components, enabling fatigue assessment and frequency domain analysis of offshore structures under wave loading.

## Problem Statement

### Current Situation
- OrcaFlex simulations generate large CSV files with tension time series data
- Manual analysis of tension data for fatigue assessment is time-consuming
- No standardized pipeline for processing OrcaFlex tension outputs
- Need for automated rainflow counting and spectral analysis

### Business Need
- Automated fatigue analysis workflow for offshore structures
- Standardized processing of tension time series from OrcaFlex
- Quick identification of dominant frequencies in structural response
- Exportable results for further analysis and reporting

## Requirements

### Functional Requirements

#### Input Processing
1. **Flexible File Discovery**
   - Support single file, file patterns (wildcards), and directory scanning
   - Process multiple files in batch mode
   - Recursive directory traversal option
   - Filter by file size, date, or name pattern
   - Support formats: CSV, Excel, HDF5

2. **Dynamic Column Mapping**
   - Auto-detect time column (common names: 'time', 'Time', 't', 'Time(s)')
   - Pattern-based column discovery (e.g., all columns containing 'Tension')
   - Configuration profiles for different file types
   - Column aliasing and unit conversion
   - Support for multi-index columns

3. **Data Validation**
   - Verify required columns exist or use alternatives
   - Check for data continuity and gaps
   - Validate units and ranges
   - Handle missing or corrupted data
   - Auto-detect sampling rate from time column

#### Rainflow Analysis
1. **Cycle Counting**
   - ASTM E1049-85 compliant rainflow counting
   - Process both tension endpoints independently
   - Generate cycle statistics (range, mean, count)
   - Support half-cycle and full-cycle methods

2. **Output Generation**
   - Export rainflow results to CSV format
   - Include columns: range, mean, count, cumulative damage
   - Generate rainflow histogram plots
   - Create mean-range scatter plots

#### FFT Analysis
1. **Spectral Analysis**
   - Window-averaged FFT for long time series
   - Configurable window size and overlap
   - Power spectral density calculation
   - Frequency resolution optimization

2. **Output Generation**
   - Export FFT results to CSV format
   - Include columns: frequency, amplitude, power, phase
   - Generate frequency spectrum plots
   - Create spectrogram visualizations

### Non-Functional Requirements

1. **Performance**
   - Process 100,000+ data points in under 10 seconds
   - Memory-efficient for files up to 100 MB
   - Support parallel processing for multiple files

2. **Usability**
   - YAML-based configuration
   - Clear error messages and logging
   - Progress indicators for long operations
   - Batch processing capability

3. **Integration**
   - Compatible with existing signal_analysis module
   - Follows repository coding standards
   - Self-contained tests with sample data
   - CI/CD pipeline ready

## Technical Design

### Architecture

```
orcaflex_tension_analysis/
├── config/
│   ├── default_config.yml
│   └── analysis_settings.yml
├── processors/
│   ├── csv_reader.py
│   ├── tension_analyzer.py
│   └── batch_processor.py
├── visualization/
│   ├── rainflow_plots.py
│   └── fft_plots.py
└── tests/
    ├── test_data/
    │   └── fat002_fsts_sample.csv
    └── test_tension_analysis.py
```

### Core Components

#### 1. GenericTimeSeriesReader
```python
class GenericTimeSeriesReader:
    def __init__(self, config: dict):
        """Initialize with configuration"""
        
    def discover_files(self, pattern: str, directory: Path = None) -> List[Path]:
        """Find files matching pattern"""
        
    def auto_detect_columns(self, df: pd.DataFrame) -> dict:
        """Automatically detect time and data columns"""
        
    def read_file(self, filepath: Path, column_mapping: dict = None) -> pd.DataFrame:
        """Read file with flexible column mapping"""
        
    def validate_data(self, df: pd.DataFrame) -> bool:
        """Validate data integrity and required columns"""
        
    def preprocess(self, df: pd.DataFrame) -> pd.DataFrame:
        """Clean and prepare data for analysis"""
```

#### 2. TensionAnalyzer
```python
class TensionAnalyzer:
    def __init__(self, signal_analyzer: SignalAnalyzer):
        """Initialize with signal analysis components"""
        
    def analyze_tension(self, data: pd.DataFrame, column: str) -> dict:
        """Perform complete analysis on tension column"""
        
    def rainflow_analysis(self, signal: np.ndarray) -> pd.DataFrame:
        """Perform rainflow counting"""
        
    def fft_analysis(self, signal: np.ndarray, fs: float) -> pd.DataFrame:
        """Perform windowed FFT analysis"""
        
    def generate_reports(self, results: dict, output_dir: Path):
        """Generate CSV and plot outputs"""
```

#### 3. Configuration Schema
```yaml
# analysis_config.yml
input:
  # File discovery options
  mode: "pattern"  # single, pattern, directory, list
  
  # Single file mode
  file_path: "path/to/file.csv"
  
  # Pattern mode
  pattern: "fat*.csv"
  directory: "D:/1522/ctr7/orcaflex/rev_a08/output/csv"
  recursive: true
  
  # List mode
  file_list:
    - "file1.csv"
    - "file2.csv"
  
  # Column mapping strategies
  column_mapping:
    strategy: "auto"  # auto, manual, profile
    
    # Auto-detection settings
    auto_detect:
      time_patterns: ["time", "Time", "t", "Time(s)", "Time (s)"]
      data_patterns: ["Tension", "Force", "Stress", "Load"]
      exclude_patterns: ["_flag", "_quality", "_comment"]
    
    # Manual mapping (fallback or override)
    manual:
      time: "time"
      data_columns:
        - name: "Tension (Jacket End)"
          alias: "jacket_tension"
          units: "kN"
        - name: "Tension (Vessel End)"
          alias: "vessel_tension"
          units: "kN"
    
    # Profile-based mapping
    profiles:
      orcaflex_standard:
        time: "Time (s)"
        data_pattern: "Tension*"
      seasam_output:
        time: "TIME"
        data_pattern: "FORCE_*"
  
analysis:
  rainflow:
    method: "astm"
    bin_count: 50
    extract_info: true
    
  fft:
    window_size: 4096
    overlap: 0.5
    window_function: "hanning"
    frequency_limit: 10.0  # Hz
    
output:
  directory: "output/tension_analysis"
  formats:
    - csv
    - png
    - json
  
  rainflow_outputs:
    cycles: true
    histogram: true
    matrix: true
    
  fft_outputs:
    spectrum: true
    psd: true
    spectrogram: true
```

### Data Flow

1. **Input Stage**
   - Read OrcaFlex CSV file
   - Extract time and tension columns
   - Validate data quality

2. **Processing Stage**
   - Perform rainflow counting on each tension signal
   - Calculate window-averaged FFT
   - Generate statistics and metrics

3. **Output Stage**
   - Export rainflow cycles to CSV
   - Export FFT spectrum to CSV
   - Generate visualization plots
   - Save analysis summary

## Implementation Plan

### Phase 1: Core Implementation (2 days)
1. Create TensionDataReader class
2. Implement CSV parsing and validation
3. Set up configuration system
4. Create test data structure

### Phase 2: Analysis Integration (2 days)
1. Integrate with signal_analysis module
2. Implement rainflow analysis pipeline
3. Implement FFT analysis pipeline
4. Add batch processing support

### Phase 3: Output Generation (1 day)
1. Create CSV export functions
2. Implement plotting utilities
3. Generate analysis reports
4. Add progress indicators

### Phase 4: Testing & Documentation (1 day)
1. Write comprehensive tests
2. Create usage examples
3. Document API
4. Performance optimization

## Success Criteria

1. **Accuracy**
   - Rainflow results match manual calculations
   - FFT peaks identify correct frequencies
   - Results reproducible across runs

2. **Performance**
   - Process 8MB file in under 5 seconds
   - Memory usage under 500MB
   - Support batch processing of 10+ files

3. **Usability**
   - Single command execution
   - Clear output organization
   - Comprehensive error handling

## Example Usage

### Command Line
```bash
# Single file analysis
python -m digitalmodel.signal_analysis.timeseries_analyzer \
    --config analysis_config.yml \
    --input fat002_fsts_l015_mwl_wave02_Strut1.csv

# Pattern-based processing
python -m digitalmodel.signal_analysis.timeseries_analyzer \
    --pattern "fat*.csv" \
    --directory "D:/1522/ctr7/orcaflex/rev_a08/output/csv" \
    --auto-columns

# Directory processing with profile
python -m digitalmodel.signal_analysis.timeseries_analyzer \
    --directory "/path/to/data" \
    --recursive \
    --profile orcaflex_standard \
    --parallel 4

# Multiple specific files
python -m digitalmodel.signal_analysis.timeseries_analyzer \
    --files file1.csv file2.csv file3.csv \
    --time-column "Time (s)" \
    --data-pattern "Tension*"
```

### Python API
```python
from digitalmodel.signal_analysis import TimeSeriesAnalyzer

# Initialize with auto-detection
analyzer = TimeSeriesAnalyzer(auto_detect_columns=True)

# Process files matching pattern
results = analyzer.process_pattern(
    pattern="fat*.csv",
    directory="D:/1522/ctr7/orcaflex/rev_a08/output/csv",
    output_dir="output/analysis"
)

# Process with custom column mapping
column_map = {
    'time': 'Time (s)',
    'data_columns': ['Effective Tension', 'Bend Moment']
}
results = analyzer.process_file(
    "simulation_output.csv",
    column_mapping=column_map
)

# Batch process with profile
analyzer = TimeSeriesAnalyzer(profile='orcaflex_standard')
results = analyzer.batch_process(
    files=['file1.csv', 'file2.csv'],
    parallel=True
)
```

## Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Large file memory issues | High | Implement chunked reading and streaming |
| Incorrect fatigue calculations | High | Validate against known test cases |
| Performance bottlenecks | Medium | Profile code and optimize critical paths |
| Missing data handling | Medium | Robust error handling and data validation |

## Dependencies

### Required Packages
- pandas >= 1.3.0
- numpy >= 1.20.0
- scipy >= 1.7.0
- matplotlib >= 3.5.0

### Internal Modules
- digitalmodel.signal_analysis
- digitalmodel.signal_analysis.core.rainflow
- digitalmodel.signal_analysis.core.spectral

## Deliverables

1. **Code**
   - TensionDataReader implementation
   - TensionAnalyzer implementation
   - Batch processing utilities
   - Visualization functions

2. **Tests**
   - Unit tests with coverage > 90%
   - Integration tests with sample data
   - Performance benchmarks

3. **Documentation**
   - API documentation
   - Usage examples
   - Configuration guide

4. **Outputs**
   - Rainflow CSV files
   - FFT spectrum CSV files
   - Analysis plots (PNG)
   - Summary reports (JSON)