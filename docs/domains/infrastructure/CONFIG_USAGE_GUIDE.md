# Configuration System Usage Guide

> Centralized configuration management for digitalmodel
>
> Version: 1.0.0

## Overview

The digitalmodel configuration system provides a centralized, type-safe way to manage all application settings using Pydantic's `BaseSettings`. All settings can be configured via:
1. **Default values** (in code)
2. **Environment variables** (prefixed with `DM_`)
3. **.env file** (for local development)
4. **Runtime overrides** (for testing)

## Quick Start

### Basic Usage

```python
from digitalmodel.config import get_settings

# Get global settings instance
settings = get_settings()

# Access settings
data_path = settings.data_dir / "my_data.csv"
safety_factor = settings.safety_factor
workers = settings.orcaflex_workers

print(f"Using safety factor: {safety_factor}")
print(f"OrcaFlex workers: {workers}")
```

### Environment Variables

Create a `.env` file in your project root:

```bash
# Copy template
cp .env.template .env

# Edit with your values
DM_SAFETY_FACTOR=2.0
DM_ORCAFLEX_WORKERS=50
DM_LOG_LEVEL=DEBUG
DM_DATA_DIR=/custom/data
```

Settings automatically load from `.env` on first access.

### Command-Line Environment Variables

Override settings temporarily:

```bash
# Linux/Mac
DM_SAFETY_FACTOR=2.5 DM_LOG_LEVEL=DEBUG python your_script.py

# Windows (PowerShell)
$env:DM_SAFETY_FACTOR="2.5"
$env:DM_LOG_LEVEL="DEBUG"
python your_script.py

# Windows (CMD)
set DM_SAFETY_FACTOR=2.5
set DM_LOG_LEVEL=DEBUG
python your_script.py
```

## Available Settings

### Paths

| Setting | Default | Environment Variable | Description |
|---------|---------|---------------------|-------------|
| `data_dir` | `./data` | `DM_DATA_DIR` | Root directory for data files |
| `output_dir` | `./reports` | `DM_OUTPUT_DIR` | Root directory for generated reports |
| `log_dir` | `./logs` | `DM_LOG_DIR` | Root directory for log files |
| `cache_dir` | `./cache` | `DM_CACHE_DIR` | Directory for cached results |

### Engineering Parameters

| Setting | Default | Range | Environment Variable | Description |
|---------|---------|-------|---------------------|-------------|
| `safety_factor` | 1.5 | 1.0-5.0 | `DM_SAFETY_FACTOR` | Global safety factor |
| `material_database` | None | - | `DM_MATERIAL_DATABASE` | Path to custom material DB |
| `sn_curve_database` | None | - | `DM_SN_CURVE_DATABASE` | Path to custom S-N curve DB |

### Analysis Settings

| Setting | Default | Range/Options | Environment Variable | Description |
|---------|---------|---------------|---------------------|-------------|
| `default_analysis_mode` | deterministic | deterministic, probabilistic | `DM_ANALYSIS_MODE` | Default analysis mode |
| `max_iterations` | 1000 | 10-100000 | `DM_MAX_ITERATIONS` | Maximum solver iterations |
| `convergence_tolerance` | 1e-6 | > 0 | `DM_CONVERGENCE_TOLERANCE` | Convergence tolerance |

### OrcaFlex Settings

| Setting | Default | Range | Environment Variable | Description |
|---------|---------|-------|---------------------|-------------|
| `orcaflex_workers` | 30 | 1-100 | `DM_ORCAFLEX_WORKERS` | Parallel workers |
| `orcaflex_license_timeout` | 300 | 10-3600 | `DM_ORCAFLEX_LICENSE_TIMEOUT` | License timeout (seconds) |

### Reporting Settings

| Setting | Default | Options | Environment Variable | Description |
|---------|---------|---------|---------------------|-------------|
| `report_format` | html | html, json, csv, all | `DM_REPORT_FORMAT` | Default output format |
| `interactive_plots` | True | true, false | `DM_INTERACTIVE_PLOTS` | Use interactive Plotly plots |
| `plot_theme` | plotly_white | plotly, plotly_white, plotly_dark, ggplot2, seaborn | `DM_PLOT_THEME` | Plotly theme |

### Performance Settings

| Setting | Default | Range | Environment Variable | Description |
|---------|---------|-------|---------------------|-------------|
| `enable_caching` | True | true, false | `DM_ENABLE_CACHING` | Enable result caching |
| `max_memory_mb` | 4096 | 512-65536 | `DM_MAX_MEMORY_MB` | Maximum memory (MB) |

### Logging Settings

| Setting | Default | Options | Environment Variable | Description |
|---------|---------|---------|---------------------|-------------|
| `log_level` | INFO | DEBUG, INFO, WARNING, ERROR, CRITICAL | `DM_LOG_LEVEL` | Logging level |
| `log_format` | standard | standard, detailed, json | `DM_LOG_FORMAT` | Log message format |

### Development Settings

| Setting | Default | Options | Environment Variable | Description |
|---------|---------|---------|---------------------|-------------|
| `environment` | production | development, testing, production | `DM_ENVIRONMENT` | Runtime environment |
| `debug_mode` | False | true, false | `DM_DEBUG` | Enable debug mode |

## Usage Patterns

### Pattern 1: Module Initialization

```python
"""
Module that uses global configuration
"""
from digitalmodel.config import get_settings
import logging

# Get settings at module level
settings = get_settings()

# Configure logging using settings
logging.basicConfig(
    level=getattr(logging, settings.log_level),
    format='%(asctime)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)


def analyze_structure(input_file):
    """Analyze structure using configured safety factor"""
    logger.info(f"Using safety factor: {settings.safety_factor}")

    # Use settings
    data_path = settings.data_dir / input_file
    output_path = settings.output_dir / "analysis_results.html"

    # Perform analysis
    # ...
```

### Pattern 2: CLI Integration

```python
"""
CLI command that uses configuration
"""
import click
from digitalmodel.config import get_settings


@click.command()
@click.option('--safety-factor', type=float, help='Override safety factor')
@click.option('--workers', type=int, help='Override OrcaFlex workers')
def run_analysis(safety_factor, workers):
    """Run analysis with configuration"""
    settings = get_settings()

    # Use CLI args to override if provided
    sf = safety_factor if safety_factor is not None else settings.safety_factor
    w = workers if workers is not None else settings.orcaflex_workers

    click.echo(f"Running with safety factor: {sf}")
    click.echo(f"Using {w} OrcaFlex workers")

    # Run analysis...
```

### Pattern 3: Test Configuration

```python
"""
Test with custom configuration
"""
import pytest
from digitalmodel.config import override_settings, reset_settings


@pytest.fixture
def test_config(tmp_path):
    """Create test configuration"""
    config = override_settings(
        data_dir=tmp_path / "data",
        output_dir=tmp_path / "output",
        environment="testing",
        debug_mode=True,
        enable_caching=False
    )

    yield config

    # Reset after test
    reset_settings()


def test_with_custom_config(test_config):
    """Test using custom configuration"""
    assert test_config.environment == "testing"
    assert test_config.debug_mode is True

    # Use test config
    # ...
```

### Pattern 4: Parametric Analysis

```python
"""
Parametric study with configuration
"""
from digitalmodel.config import get_settings, override_settings
import pandas as pd


def parametric_study():
    """Run parametric study with different safety factors"""
    base_settings = get_settings()

    results = []

    # Try different safety factors
    for sf in [1.2, 1.5, 2.0, 2.5, 3.0]:
        # Override for this iteration
        settings = override_settings(
            safety_factor=sf,
            output_dir=base_settings.output_dir / f"sf_{sf}"
        )

        # Run analysis
        result = run_analysis(settings)
        results.append({
            'safety_factor': sf,
            'max_stress': result.max_stress,
            'capacity_ratio': result.capacity_ratio
        })

    # Create results DataFrame
    df = pd.DataFrame(results)
    return df
```

## Environment-Specific Configurations

### Development Environment

Create `.env.dev`:

```bash
DM_ENVIRONMENT=development
DM_DEBUG=true
DM_LOG_LEVEL=DEBUG
DM_ENABLE_CACHING=false
DM_ORCAFLEX_WORKERS=2
DM_DATA_DIR=./dev_data
DM_OUTPUT_DIR=./dev_output
```

Use it:
```bash
# Linux/Mac
export $(cat .env.dev | xargs)
python your_script.py

# Windows
Get-Content .env.dev | ForEach-Object { $_ -replace '^', '$env:' | Invoke-Expression }
python your_script.py
```

### Production Environment

Create `.env.prod`:

```bash
DM_ENVIRONMENT=production
DM_LOG_LEVEL=WARNING
DM_ENABLE_CACHING=true
DM_MAX_MEMORY_MB=8192
DM_ORCAFLEX_WORKERS=50
DM_DATA_DIR=/mnt/shared/digitalmodel/data
DM_OUTPUT_DIR=/mnt/shared/digitalmodel/reports
```

### Testing Environment

Create `.env.test`:

```bash
DM_ENVIRONMENT=testing
DM_DATA_DIR=./tests/fixtures
DM_OUTPUT_DIR=./tests/outputs
DM_ORCAFLEX_WORKERS=1
DM_ENABLE_CACHING=false
DM_LOG_LEVEL=DEBUG
```

## Validation and Type Safety

All settings are type-checked and validated using Pydantic:

```python
from digitalmodel.config import GlobalSettings
from pydantic import ValidationError

# ✅ Valid configuration
config = GlobalSettings(safety_factor=2.0)

# ❌ Invalid - safety factor out of range
try:
    config = GlobalSettings(safety_factor=6.0)  # Max is 5.0
except ValidationError as e:
    print(e)
# ValidationError: safety_factor must be <= 5.0

# ❌ Invalid - wrong type
try:
    config = GlobalSettings(orcaflex_workers="many")  # Must be int
except ValidationError as e:
    print(e)
# ValidationError: value is not a valid integer

# ❌ Invalid - wrong literal value
try:
    config = GlobalSettings(environment="staging")  # Must be development/testing/production
except ValidationError as e:
    print(e)
# ValidationError: unexpected value; permitted: 'development', 'testing', 'production'
```

## Best Practices

### 1. Use Singleton Pattern

Always use `get_settings()` instead of creating new instances:

```python
# ✅ GOOD: Reuse singleton
from digitalmodel.config import get_settings

settings = get_settings()

# ❌ BAD: Create new instance
from digitalmodel.config import GlobalSettings

settings = GlobalSettings()  # Don't do this unless you have a good reason
```

### 2. Override Only for Testing

Use `override_settings()` sparingly:

```python
# ✅ GOOD: Override for tests
@pytest.fixture
def test_settings():
    settings = override_settings(environment="testing")
    yield settings
    reset_settings()

# ❌ BAD: Override in production code
def my_function():
    settings = override_settings(safety_factor=2.0)  # Use .env instead!
```

### 3. Document Configuration Requirements

Add configuration requirements to module docstrings:

```python
"""
OrcaFlex Analysis Module

Configuration Requirements:
- DM_ORCAFLEX_WORKERS: Number of parallel workers (recommend 30-50)
- DM_ORCAFLEX_LICENSE_TIMEOUT: License acquisition timeout (default: 300s)
- DM_DATA_DIR: Must contain OrcaFlex .dat files

Example .env:
    DM_ORCAFLEX_WORKERS=50
    DM_DATA_DIR=/path/to/orcaflex/models
"""
```

### 4. Validate Configuration Early

Check configuration at startup:

```python
from digitalmodel.config import get_settings
import sys


def validate_configuration():
    """Validate configuration before running"""
    settings = get_settings()

    errors = []

    # Check required directories exist
    if not settings.data_dir.exists():
        errors.append(f"Data directory does not exist: {settings.data_dir}")

    # Check OrcaFlex workers reasonable
    if settings.orcaflex_workers > 100:
        errors.append(f"OrcaFlex workers too high: {settings.orcaflex_workers} (max recommended: 100)")

    # Check safety factor reasonable
    if settings.safety_factor < 1.2:
        errors.append(f"Safety factor too low: {settings.safety_factor} (min recommended: 1.2)")

    if errors:
        print("Configuration errors:")
        for error in errors:
            print(f"  - {error}")
        sys.exit(1)

    print("✓ Configuration valid")


if __name__ == '__main__':
    validate_configuration()
    # Run application...
```

## Migration from Hardcoded Values

### Before (Hardcoded):

```python
# old_module.py
DATA_DIR = Path("./data")
SAFETY_FACTOR = 1.5
ORCAFLEX_WORKERS = 30


def analyze():
    df = pd.read_csv(DATA_DIR / "input.csv")
    # Use SAFETY_FACTOR...
```

### After (Using Configuration):

```python
# new_module.py
from digitalmodel.config import get_settings

settings = get_settings()


def analyze():
    df = pd.read_csv(settings.data_dir / "input.csv")
    # Use settings.safety_factor...
```

**Benefits:**
- ✅ Configurable via environment variables
- ✅ Type-safe and validated
- ✅ Easy to override for testing
- ✅ Centralized configuration
- ✅ No code changes needed for different environments

## Troubleshooting

### Settings Not Loading from .env

**Problem:** Environment variables in `.env` file are not being loaded.

**Solution:**
1. Ensure `.env` file is in the project root (same directory as `pyproject.toml`)
2. Check file encoding is UTF-8
3. Verify no syntax errors in `.env` file
4. Try explicit loading:
   ```python
   from dotenv import load_dotenv
   load_dotenv()
   ```

### Type Validation Errors

**Problem:** Getting `ValidationError` when setting configuration.

**Solution:**
Check the error message and fix the value:
```python
try:
    settings = GlobalSettings(safety_factor="2.0")  # Wrong: string not float
except ValidationError as e:
    print(e.json())  # See detailed error
```

Correct:
```python
settings = GlobalSettings(safety_factor=2.0)  # Correct: float
```

### Path Not Found Errors

**Problem:** Settings paths don't exist.

**Solution:**
Paths are automatically created when using `get_settings()`:
```python
settings = get_settings()  # Creates data_dir, output_dir, log_dir, cache_dir
```

If using `GlobalSettings()` directly, create paths manually:
```python
settings = GlobalSettings()
settings.data_dir.mkdir(parents=True, exist_ok=True)
```

## Additional Resources

- **Pydantic Documentation**: https://docs.pydantic.dev/
- **Environment Variables Best Practices**: https://12factor.net/config
- **Testing with Configuration**: See `tests/test_config.py`

---

**Need help?** Check the `.env.template` file for all available configuration options with examples.
