# run_models_to_sim.py Updates

## Summary
Enhanced the `run_models_to_sim.py` script to support OrcaFlex `.dat` files in addition to `.yml` files.

## New Features

### 1. Support for .dat Files
The script now can process both `.yml` and `.dat` OrcaFlex model files.

### 2. New Command-Line Arguments

| Argument | Description | Example |
|----------|-------------|---------|
| `--include-dat` | Include .dat files in addition to .yml files | `--all --include-dat` |
| `--dat-only` | Process only .dat files (no .yml files) | `--all --dat-only` |
| `--pattern` | Custom file pattern to match (default: fsts_*) | `--pattern "model_*"` |

### 3. Usage Examples

```bash
# Process only .yml files (default behavior - backwards compatible)
python run_models_to_sim.py --all

# Process both .yml and .dat files
python run_models_to_sim.py --all --include-dat

# Process only .dat files
python run_models_to_sim.py --all --dat-only

# Process specific .dat file
python run_models_to_sim.py --models fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat

# Custom pattern with .dat files
python run_models_to_sim.py --all --dat-only --pattern "*vessel_statics*"

# Mock mode (no license required) with .dat files
python run_models_to_sim.py --all --include-dat --mock

# Specify output directory for .sim files
python run_models_to_sim.py --all --dat-only --output ./sim_outputs
```

## Implementation Details

### Updated Functions

1. **`find_model_files()`**
   - Added `include_dat` parameter to include .dat files
   - Added `dat_only` parameter to process only .dat files
   - Added `pattern` parameter for custom file matching

2. **`run_single_model()`**
   - Updated documentation to mention .dat file support
   - No code changes needed - OrcaFlex API handles both formats

3. **`main()`**
   - Added argument parsing for new options
   - Updated file type logging to show what's being searched

### Backwards Compatibility
- Default behavior unchanged (only .yml files)
- Existing scripts continue to work without modification
- New features are opt-in via command-line arguments

## Testing

### Test Commands
```bash
# Test with the go-by .dat file in mock mode
cd specs/modules/orcaflex/mooring-tension-iteration/go-by
python run_models_to_sim.py --all --dat-only --mock

# Expected output:
# - Finds 1 model file: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
# - Mock processes it successfully
# - Would create: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim
```

### Verification
✅ Script finds .dat files when `--include-dat` or `--dat-only` is used
✅ Maintains backwards compatibility (default is .yml only)
✅ Pattern matching works with both file types
✅ Mock mode works with .dat files
✅ Help text updated with new options

## Use Case for Mooring Tension Iteration

This update enables the mooring tension iteration workflow to process the existing `.dat` file directly:

```bash
# In the go-by directory
python run_models_to_sim.py --all --dat-only

# This will:
# 1. Find fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
# 2. Load it in OrcaFlex
# 3. Run static analysis
# 4. Save as fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim
```

This resolves the issue where the tension calculation wasn't generating .yml files, allowing the workflow to proceed with the existing .dat file.