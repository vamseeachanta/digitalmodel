# OrcaFlex Comparative Analysis Module

A comprehensive analysis module for comparing OrcaFlex mooring system configurations, generating reports and visualizations.

## Features

- **Pretension Analysis**: Compare mooring line pretensions across configurations
- **Stiffness Analysis**: Analyze 3D mooring stiffness matrices and natural periods
- **Line Groups Analysis**: Statistical analysis by line groups and types
- **Fender Forces**: Analyze fender compression forces
- **Visualization**: Generate comparative plots and charts
- **Reports**: Create detailed markdown reports with findings and recommendations

## Installation

The module is part of the digitalmodel package. Ensure you have the package installed:

```bash
# Using uv (recommended)
uv add pandas matplotlib pyyaml

# Or using pip
pip install pandas matplotlib pyyaml
```

## Usage

### Command Line Interface

The module can be run directly from the command line:

```bash
# Basic usage with input directory
python -m digitalmodel.solvers.orcaflex.analysis --input-directory ./output/.csv

# With output directory specified
python -m digitalmodel.solvers.orcaflex.analysis \
    --input-directory ./output/.csv \
    --output-directory ./results

# Using configuration file
python -m digitalmodel.solvers.orcaflex.analysis --config analysis_config.yml

# With specific report and visualization paths
python -m digitalmodel.solvers.orcaflex.analysis \
    --input-directory ./output/.csv \
    --report ./report/analysis.md \
    --visualizations ./plots
```

### Python API

```python
from digitalmodel.solvers.orcaflex.analysis import MooringComparativeAnalysis

# Initialize analyzer
analyzer = MooringComparativeAnalysis('./output/.csv')

# Perform analysis
pretension_df = analyzer.analyze_pretension()
stiffness_df = analyzer.analyze_stiffness()

# Generate visualizations
vis_dir = analyzer.create_visualizations('./plots')

# Generate report
report = analyzer.generate_report('./report/analysis.md')
```

### Configuration Files

The module supports YAML configuration files. See `configs/` directory for templates:

- `basic_analysis.yml` - Standard analysis configuration
- `mooring_analysis.yml` - OrcaFlex mooring module compatible format
- `quick_analysis.yml` - Minimal configuration for quick analysis

Example configuration:

```yaml
# File management
file_management:
  input_directory: ./output/.csv
  output_directory: ./output

# Analysis settings
pattern: "*.csv"

# Output configuration
output:
  report: ./output/report/analysis.md
  visualizations: ./output/comparative_analysis
```

## Command Line Options

- `--input-directory, -d`: Directory containing CSV analysis files
- `--output-directory, -o`: Base output directory for results
- `--report, -r`: Path for markdown report output
- `--visualizations, -v`: Directory for visualization outputs
- `--config, -c`: Configuration file path (YAML)
- `--pattern`: File pattern for CSV files (default: *.csv)
- `--no-visualizations`: Skip visualization generation
- `--no-report`: Skip report generation
- `--verbose`: Enable verbose output

## Output Files

### Report
The module generates a comprehensive markdown report including:
- Executive summary
- Pretension analysis with convergence metrics
- Stiffness analysis with natural periods
- Line group statistics
- Fender force analysis
- Key findings and recommendations
- Embedded visualizations

### Visualizations
Generated plots include:
- Pretension comparison bar charts
- Stiffness matrix heatmaps
- Natural period comparisons
- Line tension distributions
- Fender force charts

## Integration with OrcaFlex Mooring Module

This analysis module is designed to work seamlessly with the OrcaFlex mooring module output files:

```bash
# Run OrcaFlex mooring analysis
python -m digitalmodel.solvers.orcaflex.mooring --config mooring_config.yml

# Run comparative analysis on results
python -m digitalmodel.solvers.orcaflex.analysis \
    --config mooring_analysis.yml
```

## Specification

For detailed technical specification and implementation plan, see:
- `specs/modules/orcaflex/mooring-analysis/spec.md` - Technical specification
- `specs/modules/orcaflex/mooring-analysis/tasks.md` - Implementation tasks

## Examples

### Example 1: Basic Analysis

```bash
python -m digitalmodel.solvers.orcaflex.analysis \
    --input-directory tests/modules/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output/.csv \
    --output-directory tests/modules/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output
```

### Example 2: Using Configuration

```yaml
# analysis_config.yml
file_management:
  input_directory: ../digitalmodel/tests/modules/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output/.csv
  output_directory: ../digitalmodel/tests/modules/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output

output:
  report: ./report/analysis_report.md
  visualizations: ./comparative_analysis
```

```bash
python -m digitalmodel.solvers.orcaflex.analysis --config analysis_config.yml
```

### Example 3: Python Script

```python
from digitalmodel.solvers.orcaflex.analysis import run_comparative_analysis

# Run analysis with configuration
run_comparative_analysis(['--config', 'analysis_config.yml'])
```

## Development

To extend the module:

1. Add new analysis methods to `comparative.py`
2. Update report generation in `report_generator.py`
3. Add CLI options in `cli.py`
4. Create new configuration templates in `configs/`

## License

Part of the DigitalModel project.