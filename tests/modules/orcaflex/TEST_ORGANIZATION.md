# OrcaFlex Test Organization

## Test Structure

### Core Tests (`/core/`)
- Configuration and framework tests
- Basic OrcaFlex API functionality

### Analysis Tests (`/analysis/`)
- Direct OrcaFlex model runs
- Simulation verification
- Parallel processing tests

### Mooring Tension Iteration (`/mooring_tension_iteration/`)
- **batch_processing/** - Batch runner tests, sim file creation
- **moorings/** - Mooring-specific analysis tests

### File Preparation (`/file_preparation/`)
- AQWA to OrcaFlex conversion
- YAML preprocessing tests

### Post Processing (`/post_processing/`)
- Results extraction and analysis
- Visualization tests
- Summary generation

### Browser Interface (`/browser_interface/`)
- Web interface integration tests

## Running Tests

```bash
# Run all tests
python tests/modules/orcaflex/run_tests.py

# Run specific category
pytest tests/modules/orcaflex/mooring_tension_iteration/batch_processing/

# Run with OrcaFlex license
ORCAFLEX_LICENSE_AVAILABLE=true pytest tests/modules/orcaflex/
```
