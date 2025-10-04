# Marine Analysis Module

**Version:** 2.1.0
**Status:** Production Ready
**Last Updated:** October 3, 2025

## Overview

The Marine Analysis module provides comprehensive tools for marine engineering analysis including hydrodynamic response analysis, performance profiling, data extraction, validation, and visualization.

## Features

- **RAO Processing**: Unified reading and processing of Response Amplitude Operators from AQWA and OrcaFlex
- **Performance Profiling**: Comprehensive profiling and optimization analysis tools
- **Data Extraction**: Extract data from OCIMF databases, hydrodynamic models, and mooring components
- **Validation**: Extensive validation tools for marine engineering computations
- **Visualization**: Specialized visualization tools for marine engineering data
- **Analysis**: General analysis utilities for marine engineering datasets

## Module Structure

```
marine_analysis/
├── profiling/          # Performance profiling and optimization
├── extraction/         # Data extraction from various sources
├── validation/         # Validation and verification tools
├── visualization/      # Visualization and charting tools
├── analysis/          # General analysis utilities
├── models/            # Data models
└── parsers/           # File format parsers
```

## Installation

This module is part of the digitalmodel package. Ensure you have the required dependencies:

```bash
# Core dependencies
pip install numpy pandas matplotlib seaborn

# Optional dependencies for full functionality
pip install memory_profiler psutil openpyxl xlrd
```

## Quick Start

### Using the CLI

```bash
# Profile performance
python -m digitalmodel.modules.marine_analysis profile --module wave_spectra

# Extract OCIMF data
python -m digitalmodel.modules.marine_analysis extract --type ocimf --input database.xlsx

# Run Phase 2 validation
python -m digitalmodel.modules.marine_analysis validate --phase 2

# Generate integration charts
python -m digitalmodel.modules.marine_analysis visualize --type integration --input data/

# Analyze Excel data
python -m digitalmodel.modules.marine_analysis analyze --input vessel_data.xlsx
```

### Using as a Python Module

```python
from digitalmodel.modules.marine_analysis import profiling, extraction, validation

# Profile a module
from digitalmodel.modules.marine_analysis.profiling import profile_modules
profiler = profile_modules.ProfilerTool()
results = profiler.profile('wave_spectra')
print(results.summary())

# Extract OCIMF data
from digitalmodel.modules.marine_analysis.extraction import extract_ocimf
extractor = extract_ocimf.OCIMFExtractor()
data = extractor.extract_vessel('tanker_vlcc')

# Run validation
from digitalmodel.modules.marine_analysis.validation import validate_phase2
validator = validate_phase2.Phase2Validator()
report = validator.validate_all()
print(report.generate_report())
```

## Submodules

### Profiling

Performance profiling and optimization analysis for marine engineering computations.

**Modules:**
- `profile_modules`: Core profiling functionality
- `performance_charts`: Visualization of performance metrics
- `optimization_report`: Detailed optimization recommendations
- `run_analysis`: Convenience wrapper for running analysis

**Example:**
```python
from digitalmodel.modules.marine_analysis.profiling import profile_modules

profiler = profile_modules.ProfilerTool()
results = profiler.profile('wave_spectra')
results.save_report('tests/outputs/profiling/')
```

### Extraction

Data extraction from various marine engineering sources.

**Modules:**
- `extract_ocimf`: OCIMF database extraction
- `extract_hydro`: Hydrodynamic coefficient extraction
- `extract_mooring`: Mooring component extraction
- `run_extraction`: Batch extraction utilities

**Example:**
```python
from digitalmodel.modules.marine_analysis.extraction import extract_hydro

extractor = extract_hydro.HydroExtractor()
coefficients = extractor.extract_from_aqwa('output.lis')
coefficients.save('tests/outputs/extraction/hydro_coeffs.csv')
```

### Validation

Validation and verification tools for marine engineering computations.

**Modules:**
- `validate_phase2`: Complete Phase 2 validation suite
- `validate_catenary`: Catenary mooring solver validation

**Example:**
```python
from digitalmodel.modules.marine_analysis.validation import validate_phase2

validator = validate_phase2.Phase2Validator()
results = validator.validate_all()
results.generate_html_report('tests/outputs/validation/')
```

### Visualization

Specialized visualization tools for marine engineering data.

**Modules:**
- `integration_charts`: System integration visualizations
- `ocimf_charts`: OCIMF coefficient visualizations
- `RAOPlotter`: RAO plotting utilities

**Example:**
```python
from digitalmodel.modules.marine_analysis.visualization import ocimf_charts

plotter = ocimf_charts.OCIMFPlotter(vessel_data)
plotter.plot_wind_coefficients()
plotter.plot_polar_diagrams()
plotter.save_all('tests/outputs/visualization/')
```

### Analysis

General analysis utilities for marine engineering datasets.

**Modules:**
- `excel_analyzer`: Excel-based data analysis
- `hydro_usage_example`: Example usage patterns

**Example:**
```python
from digitalmodel.modules.marine_analysis.analysis import excel_analyzer

analyzer = excel_analyzer.MarineExcelAnalyzer('vessel_data.xlsx')
summary = analyzer.analyze_stability()
summary.save('tests/outputs/analysis/')
```

## CLI Commands

### Profile Command

Run performance profiling on marine engineering modules.

```bash
python -m digitalmodel.modules.marine_analysis profile [OPTIONS]

Options:
  --module TEXT   Module to profile (default: all)
  --output PATH   Output directory (default: tests/outputs/profiling)

Examples:
  python -m digitalmodel.modules.marine_analysis profile --module wave_spectra
  python -m digitalmodel.modules.marine_analysis profile --module all --output results/
```

### Extract Command

Extract data from various marine engineering sources.

```bash
python -m digitalmodel.modules.marine_analysis extract [OPTIONS]

Options:
  --type TEXT     Type of extraction [ocimf|hydro|mooring] (required)
  --input PATH    Input file or database (required)
  --output PATH   Output directory (default: tests/outputs/extraction)

Examples:
  python -m digitalmodel.modules.marine_analysis extract --type ocimf --input database.xlsx
  python -m digitalmodel.modules.marine_analysis extract --type hydro --input aqwa_output.lis
```

### Validate Command

Run validation tests on marine engineering computations.

```bash
python -m digitalmodel.modules.marine_analysis validate [OPTIONS]

Options:
  --phase INT     Phase to validate [1|2|3] (default: 2)
  --output PATH   Output directory (default: tests/outputs/validation)

Examples:
  python -m digitalmodel.modules.marine_analysis validate --phase 2
  python -m digitalmodel.modules.marine_analysis validate --phase 2 --output results/
```

### Visualize Command

Generate visualizations for marine engineering data.

```bash
python -m digitalmodel.modules.marine_analysis visualize [OPTIONS]

Options:
  --type TEXT     Type of visualization [integration|ocimf|performance] (required)
  --input PATH    Input data file or directory (required)
  --output PATH   Output directory (default: tests/outputs/visualization)

Examples:
  python -m digitalmodel.modules.marine_analysis visualize --type integration --input data/
  python -m digitalmodel.modules.marine_analysis visualize --type ocimf --input ocimf_data.csv
```

### Analyze Command

Run general analysis on marine engineering datasets.

```bash
python -m digitalmodel.modules.marine_analysis analyze [OPTIONS]

Options:
  --input PATH    Input file to analyze (required)
  --output PATH   Output directory (default: tests/outputs/analysis)

Examples:
  python -m digitalmodel.modules.marine_analysis analyze --input vessel_data.xlsx
```

## Configuration

The module uses sensible defaults but can be configured through:

1. **Environment variables** (for paths and common settings)
2. **Configuration files** (for detailed module configuration)
3. **Command-line arguments** (for CLI usage)
4. **Python API** (for programmatic usage)

## Output Directories

All outputs are organized in the `tests/outputs/` directory:

```
tests/outputs/
├── phase2/
│   ├── ocimf/          # OCIMF coefficient outputs
│   ├── hydro/          # Hydrodynamic analysis outputs
│   └── validation/     # Validation test outputs
└── phase3/
    ├── integration/    # System integration outputs
    └── performance/    # Performance analysis outputs
```

## Testing

Run the comprehensive test suite:

```bash
# Run all reorganization tests
pytest tests/test_module_reorganization.py -v

# Run specific test class
pytest tests/test_module_reorganization.py::TestModuleStructure -v

# Run with coverage
pytest tests/test_module_reorganization.py --cov=digitalmodel.modules.marine_analysis
```

## Migration from Old Structure

If you're migrating from the old `scripts/` based structure, see the [Migration Guide](../../../../docs/MIGRATION_GUIDE.md) for detailed instructions.

### Quick Migration Reference

**Old (Deprecated):**
```python
# Old script execution
python scripts/profile_marine_modules.py --module wave_spectra

# Old imports
import sys
sys.path.append('scripts')
from profile_marine_modules import ProfilerTool
```

**New (Current):**
```python
# New CLI
python -m digitalmodel.modules.marine_analysis profile --module wave_spectra

# New imports
from digitalmodel.modules.marine_analysis.profiling import profile_modules
profiler = profile_modules.ProfilerTool()
```

## Documentation

- **Migration Guide**: [docs/MIGRATION_GUIDE.md](../../../../docs/MIGRATION_GUIDE.md)
- **Reorganization Summary**: [docs/REORGANIZATION_SUMMARY.md](../../../../docs/REORGANIZATION_SUMMARY.md)
- **Directory Structure**: [docs/DIRECTORY_STRUCTURE.txt](../../../../docs/DIRECTORY_STRUCTURE.txt)
- **API Documentation**: See module docstrings
- **Examples**: See `analysis/hydro_usage_example.py`

## Version History

### v2.1.0 (October 2025)
- Module reorganization with proper package structure
- Added submodules: profiling, extraction, validation, visualization, analysis
- Created unified CLI interface
- Comprehensive documentation and migration guide
- Test suite for verification

### v2.0.0 (August 2025)
- Unified RAO reader implementation
- Support for AQWA and OrcaFlex formats
- Data models and parsers
- RAO plotting utilities

## Support

For issues, questions, or contributions:

1. Check the [Migration Guide](../../../../docs/MIGRATION_GUIDE.md)
2. Review the [Reorganization Summary](../../../../docs/REORGANIZATION_SUMMARY.md)
3. Run the test suite: `pytest tests/test_module_reorganization.py`
4. Contact the development team

## License

Part of the digitalmodel package. See main repository for license information.

## Contributing

Contributions are welcome! Please:

1. Follow the established package structure
2. Add tests for new functionality
3. Update documentation
4. Follow PEP 8 style guidelines
5. Add examples for new features

## Related Modules

- `digitalmodel.fatigue`: Fatigue analysis tools
- `digitalmodel.modules.catenary`: Catenary mooring analysis
- `digitalmodel.modules.aqwa`: AQWA interface utilities
- `digitalmodel.modules.orcaflex`: OrcaFlex interface utilities

---

**Maintained by:** Digital Model Development Team
**Last Updated:** October 3, 2025
**Version:** 2.1.0
