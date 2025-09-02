# Command Comparison: run_models_to_sim.py vs Universal Runner

## Overview
Both commands can process OrcaFlex models with the standardized folder structure. Choose based on your needs:

## Option 1: Local run_models_to_sim.py
**Best for**: Quick local processing, simple workflows

```bash
# Using local Python environment
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/" \
    output_directory=".sim/"

# Or with more options
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/original" \
    output_directory=".sim/baseline" \
    pattern="fsts_*" \
    threads=10
```

### Supported Arguments:
- `dat=true` - Process .dat files only
- `include_dat=true` - Process both .yml and .dat files
- `input_directory="path"` - Where to find model files
- `output_directory="path"` - Where to save .sim files
- `pattern="*.dat"` - File pattern matching
- `threads=N` - Number of parallel threads
- `mock=true` - Test without OrcaFlex license

## Option 2: Universal Runner (digitalmodel.modules.orcaflex.universal)
**Best for**: Repository-wide processing, advanced features, integration with digitalmodel

```bash
# Using module entry point
python -m digitalmodel.modules.orcaflex.universal \
    pattern="*.dat" \
    input_directory=".dat/" \
    output_directory=".sim/"

# Or with CLI command (if installed)
orcaflex-universal \
    pattern="*.dat" \
    input_directory=".dat/original" \
    output_directory=".sim/baseline"
```

### Additional Features:
- Recursive directory search (`recursive=true`)
- Analysis type selection (`analysis_type="static"`)
- Better progress reporting
- Integration with digitalmodel ecosystem
- Automatic worker scaling
- Real-time status updates

## Quick Examples

### Process all .dat files from standard structure
```bash
# Local script
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/original" \
    output_directory=".sim/baseline"

# Universal runner
python -m digitalmodel.modules.orcaflex.universal \
    pattern="*.dat" \
    input_directory=".dat/original" \
    output_directory=".sim/baseline"
```

### Process specific pattern with mock mode
```bash
# Local script
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/" \
    output_directory=".sim/" \
    pattern="fsts_*" \
    mock=true

# Universal runner
python -m digitalmodel.modules.orcaflex.universal \
    pattern="fsts_*.dat" \
    input_directory=".dat/" \
    output_directory=".sim/" \
    mock=true
```

### Process with parallel workers
```bash
# Local script (threads)
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/" \
    output_directory=".sim/" \
    threads=5

# Universal runner (max_workers)
python -m digitalmodel.modules.orcaflex.universal \
    pattern="*.dat" \
    input_directory=".dat/" \
    output_directory=".sim/" \
    max_workers=5
```

## Standardized Folder Structure Support

Both commands fully support the standardized folder structure:

```
analysis_directory/
├── .dat/
│   ├── original/      # input_directory=".dat/original"
│   ├── modified/      # input_directory=".dat/modified"
│   └── archive/
│
├── .sim/
│   ├── baseline/      # output_directory=".sim/baseline"
│   ├── iterations/    # output_directory=".sim/iterations"
│   └── final/         # output_directory=".sim/final"
```

## Recommendation

- **Use run_models_to_sim.py** for:
  - Quick local testing
  - Simple batch processing
  - When you're already in the go-by directory
  - Lightweight operations

- **Use universal runner** for:
  - Production workflows
  - Repository-wide operations
  - Advanced features (recursive search, etc.)
  - Integration with other digitalmodel modules
  - Better progress tracking

## Migration from Old Commands

| Old Command | New with Folders |
|------------|------------------|
| `python run_models_to_sim.py all=true dat=true` | `python run_models_to_sim.py dat=true input_directory=".dat/" output_directory=".sim/"` |
| `python run_models_to_sim.py dat=true pattern="fsts_*"` | `python run_models_to_sim.py dat=true pattern="fsts_*" input_directory=".dat/original" output_directory=".sim/baseline"` |

## Testing the Commands

```bash
# Test with mock mode first
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/" \
    output_directory=".sim/" \
    mock=true

# If successful, run without mock
/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py \
    dat=true \
    input_directory=".dat/" \
    output_directory=".sim/"
```