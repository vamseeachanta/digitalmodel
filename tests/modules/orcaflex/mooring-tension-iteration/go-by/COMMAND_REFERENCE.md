# Complete Command Reference for run_models_to_sim.py

## Quick Reference Table

| What to Process | Keyword Format (Recommended) | Flag Format |
|----------------|------------------------------|-------------|
| All .yml files only | `all=true` | `--all` |
| All .dat files only | `all=true dat=true` | `--all --dat` |
| Both .yml and .dat files | `all=true include_dat=true` | `--all --include-dat` |
| Specific files | `models="file1.dat file2.yml"` | `--models file1.dat file2.yml` |
| Pattern matching | `all=true pattern="*mooring*"` | `--all --pattern "*mooring*"` |

## 1. Processing .yml Files Only (Default Behavior)

```bash
# Process all .yml files (default behavior)
python run_models_to_sim.py all=true

# Process .yml files with specific pattern
python run_models_to_sim.py all=true pattern="fsts_*.yml"
python run_models_to_sim.py all=true pattern="*mooring*"
python run_models_to_sim.py all=true pattern="*125km3*"

# Process specific .yml files
python run_models_to_sim.py models="model1.yml model2.yml model3.yml"
```

## 2. Processing .dat Files Only

```bash
# Process all .dat files
python run_models_to_sim.py all=true dat=true

# Process .dat files with specific pattern
python run_models_to_sim.py all=true dat=true pattern="fsts_*"
python run_models_to_sim.py all=true dat=true pattern="*vessel_statics*"
python run_models_to_sim.py all=true dat=true pattern="*l015*"

# Process specific .dat file
python run_models_to_sim.py models=fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat

# Multiple specific .dat files
python run_models_to_sim.py models="file1.dat file2.dat file3.dat"
```

## 3. Processing Both .yml and .dat Files

```bash
# Process all .yml AND .dat files
python run_models_to_sim.py all=true include_dat=true

# Both types with pattern
python run_models_to_sim.py all=true include_dat=true pattern="fsts_*"
python run_models_to_sim.py all=true include_dat=true pattern="*test*"

# Mix specific files of both types
python run_models_to_sim.py models="model1.yml model2.dat model3.yml"
```

## 4. Pattern Matching Examples

```bash
# All files starting with "fsts_"
python run_models_to_sim.py all=true dat=true pattern="fsts_*"

# All files containing "mooring"
python run_models_to_sim.py all=true include_dat=true pattern="*mooring*"

# All files ending with "vessel_statics_6dof"
python run_models_to_sim.py all=true dat=true pattern="*vessel_statics_6dof"

# Complex patterns
python run_models_to_sim.py all=true dat=true pattern="fsts_l015_*_pb_*"
python run_models_to_sim.py all=true include_dat=true pattern="*125km3*pb*"

# Case-sensitive patterns (exact match)
python run_models_to_sim.py all=true pattern="FSTS_*"  # Won't match "fsts_*"
```

## 5. Output Directory Options

```bash
# Save .sim files in same directory as models (default)
python run_models_to_sim.py all=true dat=true

# Save all .sim files to specific directory
python run_models_to_sim.py all=true dat=true output="./sim_outputs"
python run_models_to_sim.py all=true dat=true output="../results/simulations"
python run_models_to_sim.py all=true dat=true output="D:/simulations/2024"

# Organize by type
python run_models_to_sim.py all=true include_dat=true output="./sim/mixed"
```

## 6. Performance and Threading Options

```bash
# Default (30 threads)
python run_models_to_sim.py all=true dat=true

# Single-threaded (sequential processing)
python run_models_to_sim.py all=true dat=true threads=1

# Limited threads for resource management
python run_models_to_sim.py all=true dat=true threads=5

# Maximum parallel processing
python run_models_to_sim.py all=true dat=true threads=50
```

## 7. Mock Mode (Testing Without License)

```bash
# Test .dat file processing without OrcaFlex license
python run_models_to_sim.py all=true dat=true mock=true

# Test pattern matching
python run_models_to_sim.py all=true include_dat=true pattern="*test*" mock=true

# Test specific files
python run_models_to_sim.py models="file1.dat file2.yml" mock=true

# Test with output directory
python run_models_to_sim.py all=true dat=true mock=true output="./test_outputs"
```

## 8. Real-World Workflow Examples

### Mooring Tension Iteration Workflow
```bash
# Step 1: Process the vessel statics .dat file
python run_models_to_sim.py all=true dat=true pattern="*vessel_statics*"

# Step 2: Process any generated .yml files
python run_models_to_sim.py all=true pattern="fsts_*"

# Step 3: Process everything in one go
python run_models_to_sim.py all=true include_dat=true
```

### Batch Processing Multiple Cases
```bash
# Process all cases for 125km3 scenario
python run_models_to_sim.py all=true include_dat=true pattern="*125km3*"

# Process all cases for line 015
python run_models_to_sim.py all=true include_dat=true pattern="*l015*"

# Process all pb (permanent ballast) cases
python run_models_to_sim.py all=true include_dat=true pattern="*pb*"
```

### Development and Testing
```bash
# Quick test with first 3 files (default when no --all flag)
python run_models_to_sim.py dat=true

# Test file discovery
python run_models_to_sim.py all=true dat=true mock=true

# Verify pattern matching
python run_models_to_sim.py all=true include_dat=true pattern="test_*" mock=true
```

## 9. Combining Multiple Options

```bash
# Full production run
python run_models_to_sim.py all=true include_dat=true threads=10 output="./production/sim"

# Selective processing with organization
python run_models_to_sim.py all=true dat=true pattern="*final*" output="./final_results" threads=5

# Test run with all options
python run_models_to_sim.py all=true include_dat=true pattern="*test*" output="./test" mock=true threads=1
```

## 10. Special Cases and Tips

### Processing Order
```bash
# Process in alphabetical order (default)
python run_models_to_sim.py all=true include_dat=true

# Note: Files are sorted before processing, so:
# - file1.dat
# - file1.yml  
# - file2.dat
# - file2.yml
```

### Avoiding Duplicates
```bash
# If you have both .yml and .dat versions of same model
# Process only the .dat files
python run_models_to_sim.py all=true dat=true

# Or process only .yml files
python run_models_to_sim.py all=true
```

### Error Handling
```bash
# Continue processing even if some files fail
python run_models_to_sim.py all=true include_dat=true  # Default behavior

# Files that fail are logged but don't stop the batch
# Check the output for lines with [✗ FAIL] markers
```

## 11. Troubleshooting Commands

```bash
# Check what files would be processed (mock mode)
python run_models_to_sim.py all=true dat=true mock=true

# Process with minimal threads to debug
python run_models_to_sim.py all=true dat=true threads=1

# Test specific problematic file
python run_models_to_sim.py models="problematic_file.dat" threads=1

# Verify file patterns
python run_models_to_sim.py all=true pattern="*" mock=true  # Shows all potential files
```

## 12. Advanced Pattern Examples

```bash
# Multiple conditions in pattern (using wildcards)
python run_models_to_sim.py all=true dat=true pattern="fsts_*_125km3_*_pb_*"

# Year-based patterns
python run_models_to_sim.py all=true include_dat=true pattern="*2024*"

# Version-based patterns
python run_models_to_sim.py all=true pattern="*_v1_*"
python run_models_to_sim.py all=true pattern="*_rev3_*"

# Exclude patterns (manual workaround - process specific patterns)
# Instead of excluding, be specific about what to include
python run_models_to_sim.py all=true dat=true pattern="fsts_l015_*"  # Only l015 files
```

## Future Enhancement Ideas

Based on these usage patterns, potential improvements could include:

1. **Exclude patterns**: `exclude_pattern="*temp*"`
2. **Recursive directory search**: `recursive=true`
3. **Dry run mode**: `dry_run=true` (show what would be processed)
4. **Continue from last**: `resume=true` (skip already processed .sim files)
5. **File size filtering**: `min_size="10MB"` 
6. **Date filtering**: `modified_after="2024-01-01"`
7. **Config file support**: `config="batch_config.yml"`
8. **Progress saving**: `save_progress=true`
9. **Email notification**: `notify="email@example.com"`
10. **Validation mode**: `validate_only=true` (check files without processing)

## Summary

The script is designed to be flexible:
- **Default**: Process .yml files only (backwards compatible)
- **dat=true**: Process .dat files only
- **include_dat=true**: Process both types
- **pattern**: Fine-grained control over which files
- **mock=true**: Test without OrcaFlex license
- **threads**: Control parallel processing
- **output**: Organize output files

The keyword argument format (`key=value`) is recommended for clarity and consistency with other digitalmodel modules.