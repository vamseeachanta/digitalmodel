# run_models_to_sim.py Usage Guide

## Keyword Arguments Format (Recommended)

The script now supports keyword arguments for easier mapping and consistency:

### Basic Usage

```bash
# Process all .dat files
python run_models_to_sim.py all=true dat=true

# Process all .yml files (default)
python run_models_to_sim.py all=true

# Process both .yml and .dat files
python run_models_to_sim.py all=true include_dat=true

# Mock mode (no license required)
python run_models_to_sim.py all=true dat=true mock=true

# Custom pattern
python run_models_to_sim.py all=true dat=true pattern="*vessel_statics*"

# Specify output directory
python run_models_to_sim.py all=true dat=true output="./sim_outputs"

# Specific models
python run_models_to_sim.py models="file1.dat file2.dat"

# Custom thread count
python run_models_to_sim.py all=true dat=true threads=10
```

## Traditional Flag Format (Still Supported)

```bash
# Process all .dat files
python run_models_to_sim.py --all --dat

# Process all .yml files (default)
python run_models_to_sim.py --all

# Process both .yml and .dat files
python run_models_to_sim.py --all --include-dat

# Mock mode
python run_models_to_sim.py --all --dat --mock

# Custom pattern
python run_models_to_sim.py --all --dat --pattern "*vessel_statics*"

# Specific models
python run_models_to_sim.py --models file1.dat file2.dat
```

## Argument Reference

| Keyword Argument | Flag Format | Description | Default |
|-----------------|-------------|-------------|---------|
| `all=true` | `--all` | Process all matching files in directory | false |
| `dat=true` | `--dat` | Process only .dat files (no .yml) | false |
| `include_dat=true` | `--include-dat` | Include .dat files along with .yml | false |
| `pattern="..."` | `--pattern ...` | File pattern to match | "fsts_*" |
| `output="..."` | `--output ...` | Output directory for .sim files | Same as model |
| `mock=true` | `--mock` | Run in mock mode (no license) | false |
| `threads=N` | `--threads N` | Number of parallel threads | 30 |
| `models="..."` | `--models ...` | Specific model files to run | None |

## Examples for Mooring Tension Iteration

### Process the existing .dat file
```bash
# Keyword format (recommended)
python run_models_to_sim.py all=true dat=true

# Or specify the exact file
python run_models_to_sim.py models=fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
```

### Test without license
```bash
python run_models_to_sim.py all=true dat=true mock=true
```

### Process with custom settings
```bash
python run_models_to_sim.py all=true dat=true threads=5 output="./results/sim_files"
```

## Boolean Values

For boolean arguments, the following values are accepted:
- True: `true`, `1`, `yes` (case-insensitive)
- False: `false`, `0`, `no` (case-insensitive)

## Notes

1. **Backwards Compatibility**: Default behavior (no flags) processes only .yml files
2. **Mixed Formats**: You can mix keyword and flag formats in the same command
3. **File Discovery**: The script will log which file types it's searching for
4. **Parallel Processing**: Uses ThreadPoolExecutor for efficient batch processing
5. **Error Handling**: Failed models are logged but don't stop other models from processing