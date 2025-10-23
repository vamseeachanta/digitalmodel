# OrcaFlex Modular Input Validation

Comprehensive validation system for OrcaFlex modular YAML input files with three-level validation architecture.

## Features

### Level 1: YAML Syntax and Structure
- ✅ YAML syntax validation
- ✅ File existence checking
- ✅ Includefile reference resolution
- ✅ Dependency graph analysis
- ✅ Circular dependency detection

### Level 2: OrcaFlex API Validation
- ✅ Software availability detection
- ✅ Version detection
- ✅ File loading via OrcFxAPI
- ✅ Static analysis and warnings
- ✅ Graceful degradation when software unavailable

### Level 3: Physical Consistency
- ✅ Hull geometry parameter validation
- ✅ Metocean design parameter validation
- ✅ Mooring component capacity validation
- ✅ Project-specific data comparison
- ✅ Configurable tolerance (default ±10%)

## Reporting

Generates reports in multiple formats:
- **Console**: Color-coded output with loguru (🟢 INFO, 🟡 WARNING, 🔴 CRITICAL)
- **CSV**: Timestamped validation logs
- **Markdown**: Human-readable reports with tables
- **HTML**: Interactive reports with Plotly visualizations

## Installation

```bash
# Core dependencies
pip install pyyaml pandas loguru

# Optional: OrcaFlex API
# Requires OrcaFlex installation and license

# Optional: HTML reports with Plotly
pip install plotly jinja2
```

## Quick Start

### Python API

```python
from digitalmodel.modules.orcaflex.modular_input_validation import ModularInputValidator

# Create validator
validator = ModularInputValidator()

# Validate a single file
result = validator.validate_file("path/to/file.yml")

# Validate directory
results = validator.validate_directory("path/to/files/")

# Generate reports
validator.generate_reports(results)
```

### Command Line

```bash
# Validate a single file
python -m digitalmodel.modules.orcaflex.modular_input_validation.cli file.yml

# Validate directory
python -m digitalmodel.modules.orcaflex.modular_input_validation.cli path/to/files/

# Custom options
python -m digitalmodel.modules.orcaflex.modular_input_validation.cli file.yml \
  --tolerance 15 \
  --formats console csv markdown \
  --skip-level 2
```

## Configuration

```python
from digitalmodel.modules.orcaflex.modular_input_validation import ValidationConfig

config = ValidationConfig(
    tolerance_percent=10.0,           # ±10% tolerance for physical validation
    enable_orcaflex=True,              # Enable OrcaFlex API validation
    calm_buoy_data_dir="data/",        # Reference data directory
    reports_dir="reports/validation/", # Output directory for reports
    skip_levels=[],                    # Levels to skip (1, 2, or 3)
    generate_reports=True,             # Auto-generate reports
    report_formats=['console', 'csv', 'markdown', 'html'],
    enable_color=True                  # Colored console output
)
```

## File Structure

```
modular_input_validation/
├── __init__.py           # Public API
├── models.py             # Data models (15+ classes)
├── config.py             # Configuration management
├── utils.py              # Helper functions
├── data_loader.py        # CALM buoy reference data loader
├── level_1_yaml.py       # Level 1 validator
├── level_2_orcaflex.py   # Level 2 validator
├── level_3_physical.py   # Level 3 validator
├── validator.py          # Main orchestrator
├── cli.py                # Command-line interface
└── reporters/            # Report generators
    ├── __init__.py
    ├── console.py        # Console reporter (loguru)
    ├── csv_reporter.py   # CSV reporter
    ├── markdown_reporter.py  # Markdown reporter
    └── html_reporter.py  # HTML reporter (Plotly)
```

## Reference Data

The validator uses CSV reference data from `data/` directory:

### Generic Ranges (Bounds Checking)
- `data/raw/calm_buoy/generic_range/hull_geometry_ranges.csv`
- `data/raw/calm_buoy/generic_range/metocean_design_ranges.csv`
- `data/raw/calm_buoy/generic_range/mooring_capacity_ranges.csv`

### Project-Specific Data (Comparison)
- `data/results/calm_buoy/project_specific/environmental_conditions.csv`
- `data/results/calm_buoy/project_specific/mooring_line_properties.csv`

## Validation Workflow

```
Input YAML File
     ↓
┌─────────────────────────┐
│  Level 1: YAML Syntax   │
│  - Parse YAML           │
│  - Check includefiles   │
│  - Build dependency graph│
└─────────────────────────┘
     ↓ (if PASS)
┌─────────────────────────┐
│  Level 2: OrcaFlex API  │
│  - Check software       │
│  - Load via API         │
│  - Static analysis      │
└─────────────────────────┘
     ↓ (if available)
┌─────────────────────────┐
│  Level 3: Physical      │
│  - Load reference data  │
│  - Validate parameters  │
│  - Compare to project   │
└─────────────────────────┘
     ↓
┌─────────────────────────┐
│  Generate Reports       │
│  - Console              │
│  - CSV                  │
│  - Markdown             │
│  - HTML                 │
└─────────────────────────┘
```

## Examples

See `docs/modules/orcaflex/modular_input_validation_example.py` for comprehensive usage examples.

## CI/CD Integration

```yaml
# Example GitHub Actions workflow
- name: Validate OrcaFlex YAML Files
  run: |
    python -m digitalmodel.modules.orcaflex.modular_input_validation.cli \
      specs/modules/orcaflex/modular-input-file/output/ \
      --no-orcaflex \
      --formats console csv markdown
```

## Status Codes

- **PASS**: All checks passed ✅
- **WARN**: Passed with warnings ⚠️
- **FAIL**: Critical issues found ❌
- **SKIPPED**: Level skipped (e.g., OrcaFlex unavailable) ⏭️
- **UNKNOWN**: Unable to determine status ❔

## Extending the Framework

This validation framework serves as a reference implementation. See `docs/VALIDATION_FRAMEWORK_BEST_PRACTICES.md` for guidance on adapting this pattern to other asset categories:

- AQWA hydrodynamic analysis
- Fatigue analysis
- Mooring components
- FPSO systems
- Pipelines and risers

## Version

1.0.0

## License

Internal use only
