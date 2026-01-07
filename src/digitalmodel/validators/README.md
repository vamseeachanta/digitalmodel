# Data Validators Module

## Overview

This module provides data validation utilities with quality scoring and interactive reporting capabilities.

**Installed from**: workspace-hub skill `data-validation-reporter`
**Installation Date**: 2026-01-07
**Source Skill**: `skills/workspace-hub/data-validation-reporter/`

## Components

### DataValidator Class

Located in `data_validator.py`, this class provides:

- **Quality Scoring** (0-100 scale)
- **Missing Data Analysis** (per-column percentages)
- **Type Validation** (automatic detection)
- **Duplicate Detection**
- **Interactive Plotly Reports** (4-panel dashboards)
- **YAML Configuration Support**

## Quick Start

### Basic Usage

```python
from src.digitalmodel.validators import DataValidator
import pandas as pd
from pathlib import Path

# Initialize validator
validator = DataValidator(config_path=Path("config/validation/validation_config.yaml"))

# Load your data
df = pd.read_csv("data/your_data.csv")

# Validate
results = validator.validate_dataframe(
    df=df,
    required_fields=["id", "name", "value"],
    unique_field="id"
)

# Check results
if results['valid']:
    print(f"✅ PASS - Quality Score: {results['quality_score']:.1f}/100")
else:
    print(f"❌ FAIL - {len(results['issues'])} issues found")

# Generate interactive report
validator.generate_interactive_report(
    results,
    Path("reports/validation/my_report.html")
)
```

### With Configuration File

Edit `config/validation/validation_config.yaml`:

```yaml
validation:
  required_fields:
    - vessel_id
    - vessel_name
    - vessel_type

  unique_fields:
    - vessel_id

  numeric_fields:
    - year_built
    - length_m
    - beam_m
    - displacement_tonnes

  thresholds:
    max_missing_pct: 0.2
    min_quality_score: 60
```

Then use:

```python
validator = DataValidator(config_path="config/validation/validation_config.yaml")
results = validator.validate_dataframe(df)
```

## Examples

### Run Demo

```bash
cd D:/workspace-hub/digitalmodel
python examples/validation_integration_demo.py
```

This will:
- Validate sample vessel data
- Show integration with existing validators
- Demonstrate batch validation
- Generate interactive HTML reports

### More Examples

```bash
python examples/validation_examples.py
```

This includes:
- Basic validation
- Custom validation rules
- Quality trend analysis
- Batch file processing

## Features

### Quality Scoring

**Algorithm** (0-100 scale):
- Base: 100 points
- Missing required fields: -20
- High missing data (>50%): -30
- Moderate missing (>20%): -15
- Duplicates: -2 each (max -20)
- Type issues: -5 each (max -15)

**Passing threshold**: ≥60

### Interactive Reports

4-panel Plotly dashboard includes:

1. **Quality Score Gauge** - Color-coded (green/yellow/red)
2. **Missing Data Chart** - Bar chart with percentages
3. **Type Issues Chart** - Validation error counts
4. **Summary Table** - Key metrics overview

Reports are:
- Interactive (hover, zoom, pan)
- Responsive (mobile/desktop)
- Exportable (PNG, SVG)
- Standalone HTML (no dependencies)

### Validation Checks

✅ Empty DataFrame detection
✅ Required field verification
✅ Missing data analysis
✅ Duplicate record detection
✅ Data type validation
✅ Numeric field verification

## Integration

### With Data Procurement

The new validator complements the existing `src/data_procurement/validators/data_validator.py`:

```python
# Use both validators together
from src.data_procurement.validators.data_validator import DataValidator as ProcurementValidator
from src.digitalmodel.validators import DataValidator

# Domain-specific validation
procurement_validator = ProcurementValidator()
procurement_results = procurement_validator.validate_dataframe(df, ...)

# Quality scoring and reporting
quality_validator = DataValidator()
quality_results = quality_validator.validate_dataframe(df, ...)
quality_validator.generate_interactive_report(quality_results, Path("report.html"))
```

### In Data Pipelines

```python
def validate_and_process(input_file, output_file):
    # Load data
    df = pd.read_csv(input_file)

    # Validate
    validator = DataValidator(config_path="config/validation/validation_config.yaml")
    results = validator.validate_dataframe(df, required_fields=["id"])

    # Quality gate
    if not results['valid']:
        validator.generate_interactive_report(
            results,
            Path("reports/validation/failed.html")
        )
        raise ValueError(f"Validation failed: {results['issues']}")

    # Success report
    validator.generate_interactive_report(
        results,
        Path("reports/validation/success.html")
    )

    # Continue processing
    processed = process_data(df)
    processed.to_csv(output_file, index=False)
```

## Configuration

### Location

`config/validation/validation_config.yaml`

### Customize

Edit the YAML file to:
- Add/remove required fields
- Define numeric field types
- Set quality thresholds
- Configure report appearance
- Adjust logging levels

See comments in the config file for all options.

## Dependencies

Already included in `pyproject.toml`:
- `pandas>=1.5.0`
- `plotly==5.17.0`
- `pyyaml==6.0.1`

## Performance

**Benchmarks** (100,000 rows):
- Validation: ~2.5 seconds
- Report generation: ~1.2 seconds
- Memory: ~150MB

## Support

- **Skill Documentation**: `D:/workspace-hub/skills/workspace-hub/data-validation-reporter/SKILL.md`
- **Examples**: `examples/validation_examples.py`
- **Integration Demo**: `examples/validation_integration_demo.py`

## Version

**v1.0.0** - Initial installation from workspace-hub skill library

## Related

- Existing validator: `src/data_procurement/validators/data_validator.py`
- Report templates: `modules/reporting/templates/`
- HTML standards: `docs/HTML_REPORTING_STANDARDS.md`
