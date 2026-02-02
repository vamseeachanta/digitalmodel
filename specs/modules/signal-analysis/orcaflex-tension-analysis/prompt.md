# Prompt Documentation: OrcaFlex Tension Analysis Specification

## Original User Request

The user requested to create a specification as part of the signal-analysis module to perform rainflow and FFT analysis with the following requirements:

- Input data obtained from: `D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue\fat002_fsts_l015_mwl_wave02_Strut1.csv`
- Extract columns: time, Tension (Jacket End), Tension (Vessel End)
- Output requirements:
  - Rainflow count as CSV data and plot
  - Averaged windowed FFT as CSV data and plot
- Generate input data that complies with repository guidelines
- Make a copy of all relevant input files for self-contained testing

## Context Gathering

### Identified Context
1. **Existing Module**: Signal-analysis module with rainflow and FFT capabilities already implemented
2. **Input File**: OrcaFlex simulation output CSV file (8.4 MB) containing tension time series
3. **Repository Pattern**: Module-based organization following `specs/modules/<module>/` structure
4. **Testing Requirements**: Self-contained tests with sample data included

### Key Decisions Made
1. **Integration Approach**: Build on existing signal_analysis module rather than creating standalone
2. **Data Management**: Copy sample subset (1000 lines) for testing to keep repository lightweight
3. **Configuration**: YAML-based configuration following repository standards
4. **Output Format**: CSV for data, PNG for plots, JSON for metadata

## Implementation Strategy

### Technical Approach
1. **Leverage Existing Components**:
   - Use `RainflowCounter` from signal_analysis.core.rainflow
   - Use `SpectralAnalyzer` from signal_analysis.core.spectral
   - Extend with OrcaFlex-specific readers and processors

2. **Module Structure**:
   ```
   signal_analysis/
   └── orcaflex/
       ├── tension_analyzer.py
       ├── csv_reader.py
       └── visualization.py
   ```

3. **Processing Pipeline**:
   - Read CSV → Validate → Analyze → Export → Visualize

### Design Considerations
1. **Performance**: Handle large files (8+ MB) efficiently
2. **Flexibility**: Support both single file and batch processing
3. **Maintainability**: Clear separation of concerns
4. **Testability**: Comprehensive test coverage with sample data

## Specification Highlights

### Core Features
1. **TensionDataReader**: Specialized CSV reader for OrcaFlex outputs
2. **TensionAnalyzer**: Orchestrates rainflow and FFT analysis
3. **Batch Processing**: Process multiple files in parallel
4. **Visualization**: Comprehensive plotting capabilities

### Key Outputs
1. **Rainflow Analysis**:
   - Cycle count CSV with range, mean, count columns
   - Histogram plot
   - Mean-range scatter plot

2. **FFT Analysis**:
   - Frequency spectrum CSV
   - Power spectral density plot
   - Spectrogram visualization

## Curated Reuse Prompt

For future similar specifications, use this prompt:

```
Create a specification for analyzing [DATA_TYPE] from [SOURCE_SYSTEM] with the following analysis methods:

Input:
- File location: [FILE_PATH]
- Required columns: [COLUMN_LIST]
- File format: [FORMAT]

Analysis Requirements:
- [ANALYSIS_METHOD_1]: [SPECIFIC_REQUIREMENTS]
- [ANALYSIS_METHOD_2]: [SPECIFIC_REQUIREMENTS]

Output Requirements:
- Data exports: [FORMAT_LIST]
- Visualizations: [PLOT_LIST]
- Reports: [REPORT_TYPE]

Constraints:
- Performance: Process [SIZE] in [TIME]
- Integration: Use existing [MODULE_NAME] module
- Testing: Include self-contained test data
- Configuration: YAML-based settings

Additional Requirements:
- Batch processing support
- Error handling and validation
- Progress indicators
- Documentation and examples
```

## Lessons Learned

### What Worked Well
1. **Reusing Existing Modules**: Leveraging signal_analysis module avoided duplication
2. **Sample Data Strategy**: Using subset of real data for testing
3. **Comprehensive Planning**: Detailed task breakdown with time estimates

### Potential Improvements
1. **Streaming Processing**: Could add for very large files
2. **Real-time Analysis**: Support for live data streams
3. **Web Interface**: Dashboard for results visualization

## Implementation Notes

### Quick Start Implementation
```python
# Minimal implementation example
from digitalmodel.signal_analysis import RainflowCounter, SpectralAnalyzer
import pandas as pd

# Read data
df = pd.read_csv("fat002_fsts_sample.csv")

# Rainflow analysis
counter = RainflowCounter()
cycles = counter.count_cycles(df["Tension (Jacket End)"])

# FFT analysis
analyzer = SpectralAnalyzer(sampling_rate=10)  # 10 Hz typical
spectrum = analyzer.compute_spectrum(df["Tension (Jacket End)"])
```

### Configuration Template
```yaml
input:
  file_path: "tests/modules/signal-analysis/test_data/fat002_fsts_sample.csv"
  columns:
    time: "time"
    tensions: ["Tension (Jacket End)", "Tension (Vessel End)"]

analysis:
  rainflow:
    method: "astm"
  fft:
    window_size: 4096
    overlap: 0.5

output:
  directory: "output/tension_analysis"
  formats: ["csv", "png", "json"]
```

## Next Steps

1. **Immediate**: Implement TensionDataReader class
2. **Short-term**: Create integration tests with sample data
3. **Medium-term**: Add batch processing capabilities
4. **Long-term**: Build web dashboard for results

## References

- Signal Analysis Module: `src/digitalmodel/modules/signal_analysis/`
- OrcaFlex Documentation: Internal reference for CSV format
- ASTM E1049-85: Standard for rainflow counting
- Repository Standards: `CLAUDE.md` for coding guidelines