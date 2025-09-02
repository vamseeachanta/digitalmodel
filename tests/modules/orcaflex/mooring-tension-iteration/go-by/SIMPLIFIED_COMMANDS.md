# Simplified Command Reference for run_models_to_sim.py

## Core Principle: Pattern drives what gets processed
- **No pattern or pattern="*"** = Process all files
- **Specific pattern** = Process only matching files
- **No need for `all=true`** - it's redundant!

## Quick Reference

| What to Process | Command |
|----------------|---------|
| All .yml files | `python run_models_to_sim.py` |
| All .dat files | `python run_models_to_sim.py dat=true` |
| Both .yml and .dat | `python run_models_to_sim.py include_dat=true` |
| Specific pattern | `python run_models_to_sim.py pattern="fsts_*"` |
| Specific files | `python run_models_to_sim.py models="file1.dat file2.yml"` |

## 1. Processing All Files (Default Behavior)

```bash
# All .yml files (default)
python run_models_to_sim.py

# All .dat files
python run_models_to_sim.py dat=true

# All .yml AND .dat files
python run_models_to_sim.py include_dat=true
```

## 2. Pattern-Based Processing

```bash
# Only files starting with "fsts_"
python run_models_to_sim.py pattern="fsts_*"
python run_models_to_sim.py dat=true pattern="fsts_*"

# Only files containing "vessel_statics"
python run_models_to_sim.py dat=true pattern="*vessel_statics*"

# Only files with "125km3" in the name
python run_models_to_sim.py include_dat=true pattern="*125km3*"

# Complex patterns
python run_models_to_sim.py dat=true pattern="fsts_l015_*_pb_*"
```

## 3. Specific File Processing

```bash
# Single file
python run_models_to_sim.py models=fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat

# Multiple files
python run_models_to_sim.py models="file1.dat file2.yml file3.dat"
```

## 4. Common Workflows

### Mooring Tension Iteration
```bash
# Process the vessel statics .dat file
python run_models_to_sim.py dat=true pattern="*vessel_statics*"

# Or if it's the only .dat file, simply:
python run_models_to_sim.py dat=true
```

### Batch Processing
```bash
# All 125km3 scenario files
python run_models_to_sim.py include_dat=true pattern="*125km3*"

# All line 015 files
python run_models_to_sim.py include_dat=true pattern="*l015*"
```

### Testing
```bash
# Test without license
python run_models_to_sim.py dat=true mock=true

# Test specific pattern
python run_models_to_sim.py pattern="*test*" mock=true
```

## 5. Additional Options

```bash
# Custom output directory
python run_models_to_sim.py dat=true output="./sim_files"

# Limit threads
python run_models_to_sim.py dat=true threads=5

# Combine options
python run_models_to_sim.py dat=true pattern="*final*" output="./results" threads=10
```

## 6. The Logic Flow

1. **If `models` specified**: Run those specific files
2. **Otherwise**: Use pattern to find files
   - Default pattern is "*" (all files)
   - `dat=true`: Only .dat files
   - `include_dat=true`: Both .yml and .dat files
   - No flags: Only .yml files

## 7. Why This is Better

### Before (redundant):
```bash
python run_models_to_sim.py all=true dat=true
python run_models_to_sim.py all=true pattern="fsts_*"
```

### After (cleaner):
```bash
python run_models_to_sim.py dat=true
python run_models_to_sim.py pattern="fsts_*"
```

## 8. Migration from Old Commands

| Old Command | New Command |
|------------|-------------|
| `--all` | (nothing needed - it's default) |
| `--all --dat` | `dat=true` |
| `--all --include-dat` | `include_dat=true` |
| `--all --pattern "fsts_*"` | `pattern="fsts_*"` |

## 9. Examples Showing the Simplification

```bash
# Process everything - no flags needed!
python run_models_to_sim.py include_dat=true

# Process all .dat files - just one flag!
python run_models_to_sim.py dat=true

# Process specific pattern - just the pattern!
python run_models_to_sim.py pattern="*mooring*"

# Combine type and pattern - still simple!
python run_models_to_sim.py dat=true pattern="*vessel*"
```

## Summary

The simplified approach:
- **Pattern is primary**: Controls what files to process
- **Type flags**: Control which extensions to consider
- **No `all=true`**: It's redundant - pattern="*" is the default
- **Cleaner commands**: Fewer parameters, same functionality
- **Backwards compatible**: Old commands still work