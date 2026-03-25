# Path Updates for New Folder Structure

**Date**: 2025-09-02
**Purpose**: Document all path updates made to scripts and configs for the new folder organization

## Folder Structure Changes

### Old Structure (with dot-prefixed folders)
```
fsts-l015-test-cases/
├── .dat/          # Hidden folder
├── .sim/          # Hidden folder
├── output/
│   └── .csv/      # Hidden folder
└── scripts/
    ├── *.yml      # Config files mixed with scripts
    └── *.sh, *.py # Scripts
```

### New Structure (clean and organized)
```
fsts-l015-test-cases/
├── run_files/
│   ├── dat/       # Input .dat files
│   └── sim/       # Output .sim files
├── output/
│   ├── collate/
│   │   ├── csv/   # CSV analysis results
│   │   └── *.xlsx # Excel summaries
│   ├── plots/     # Generated plots
│   ├── report/    # Analysis reports
│   └── visual/    # Visualizations
└── scripts/
    ├── config/    # All YAML configs
    │   ├── *.yml
    │   └── *.csv  # Target data files
    └── *.sh, *.py # Executable scripts
```

## Updated Files

### 1. Shell Scripts
**dm_iterator.sh**
- Config paths: `dm_*.yml` → `config/dm_*.yml`
- DAT directory: `../.dat/` → `../run_files/dat/`
- SIM directory: `../.sim/` → `../run_files/sim/`

**dm_pretension_iteration.sh**
- Config paths: `dm_*.yml` → `config/dm_*.yml`

### 2. Python Scripts
**generate_mooring_plots.py**
- CSV directory: `output/.csv` → `output/collate/csv`

**run_models_to_sim.py**
- No changes needed (paths passed as arguments)

### 3. Configuration Files

#### dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
- Input: `../.sim` → `../../run_files/sim`
- Output: `../output/.csv` → `../../output/collate/csv`
- Visual: `../output/visual` → `../../output/visual`

#### dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml
- Input: `../.sim` → `../../run_files/sim`
- Output: `../output/.csv` → `../../output/collate/csv`

#### dm_ofx_post_fsts_lngc.yml
- Input: `../.sim` → `../../run_files/sim`
- Output: `../output/.csv` → `../../output/collate/csv`

#### viz.yml
- Input: `../.sim` → `../../run_files/sim`
- Output: `../output/visual` → `../../output/visual`

#### au_collate.yml
- Template target: `../output/collate/` → `../../output/collate/`
- CSV inputs: `../output/.csv/` → `../../output/collate/csv/`

## New Helper Scripts Created

### run_test.sh (Bash)
- Comprehensive test runner for Unix/Linux/Mac
- Uses updated paths throughout
- Includes all processing steps

### run_test.py (Python)
- Cross-platform test runner
- Automatic path resolution
- Error handling and progress reporting

## Path Resolution Notes

### Relative Path Context
All paths in config files are relative to the **config file location**:
- Config files are in: `scripts/config/`
- To reach `run_files/`: Use `../../run_files/`
- To reach `output/`: Use `../../output/`

### Script Execution Context
Scripts executed from `scripts/` directory:
- Config files: `config/*.yml`
- Parent directories: `../run_files/`, `../output/`

## Testing the Updates

Run the complete test suite with:
```bash
# From scripts directory
python run_test.py

# Or using shell script (Unix/Linux/Mac)
./run_test.sh
```

## Benefits of New Structure

1. **No Hidden Folders**: All directories are visible
2. **Clear Organization**: Separate folders for different file types
3. **Cross-Platform**: Works consistently on Windows/Linux/Mac
4. **Maintainable**: Clear separation of configs, scripts, and data
5. **Git-Friendly**: No issues with hidden folders in version control