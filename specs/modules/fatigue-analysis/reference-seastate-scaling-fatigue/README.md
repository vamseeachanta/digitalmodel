# Reference Seastate Scaling for Fatigue Analysis

## Overview
This module implements a load scaling methodology for fatigue analysis, allowing scaling of reference seastate loads to fatigue seastate conditions. The implementation follows DNV-ST-0437 standards for marine structure fatigue assessment.

## 🎯 Purpose
- Scale reference seastate time series data to fatigue seastate conditions
- Apply appropriate scaling factors for wave and wind loads
- Perform fatigue damage calculations using rainflow counting and S-N curves
- Validate calculations against reference implementations

## 📁 Directory Structure

### Core Implementation
```
reference-seastate-scaling-fatigue/
│
├── 📄 Core Files (Root Level)
│   ├── run_load_scaling.py          # Main execution script for load scaling
│   ├── verify_load_scaling.py       # Verification script with detailed checks
│   ├── LOAD_SCALING_PROGRAM_DOCUMENTATION.md  # Program documentation
│   ├── LOAD_SCALING_VERIFICATION.md           # Verification results
│   ├── VERIFICATION_SUMMARY.md                # Summary of verification
│   └── user_spec_load_scaling.md              # User specifications
│
├── 📂 input/                        # Input configuration files
│   ├── master_input_config_sample.yml      # Sample data configuration
│   ├── master_input_config_production.yml  # Production configuration
│   ├── load_scaling_config.yml             # Load scaling parameters
│   ├── fatigue_seastates_sample.csv        # Sample fatigue seastates
│   ├── fatigue_seastates_production.csv    # Production fatigue seastates
│   ├── reference_seastate_definitions_sample.csv     # Sample reference definitions
│   ├── reference_seastate_definitions_production.csv # Production reference definitions
│   └── reference_seastates.xlsx            # Original reference data
│
├── 📂 core/                         # Core processing scripts
│   ├── load_master_config.py               # Configuration loader
│   ├── detailed_calculation_fc001.py       # Detailed calculation example
│   ├── rainflow_visualization.py           # Rainflow counting visualization
│   ├── verify_step_*.py                    # Step-by-step verification scripts
│   ├── sample_data/                        # Sample time series data (CSV files)
│   └── output/                              # Verification outputs
│
├── 📂 docs/                         # Documentation
│   ├── ARCHITECTURE_SPECIFICATION.md       # System architecture
│   └── INPUT_SPECIFICATION.md              # Input file specifications
│
├── 📂 reference_data/               # Reference data for validation
│   └── (reference files for comparison)
│
├── 📂 output/                       # Output directory for results
│   └── (generated output files)
│
└── 📂 rev_0/                        # [ARCHIVED - Previous Implementation]
    └── (complete previous version - can be deleted)
```

## 🚀 Quick Start

### Using Repository UV Environment (Recommended)

### 1. Run Load Scaling
```bash
# Using UV environment (recommended)
uv run python run_load_scaling.py

# Or if UV environment is already activated
python run_load_scaling.py
```
This will:
- Load configuration from `input/master_input_config_sample.yml`
- Process reference seastate data
- Apply scaling factors
- Generate output in `output/` directory

### 2. Verify Results
```bash
# Using UV environment (recommended)
uv run python verify_load_scaling.py

# Or if UV environment is already activated
python verify_load_scaling.py
```
This performs:
- Step-by-step verification of calculations
- Comparison with expected results
- Generation of verification report

## 📊 Load Scaling Methodology

### Scaling Factors
The implementation uses two types of scaling factors:
1. **Wave Scaling Factor (α_wave)**: Scales wave-induced loads
2. **Wind Scaling Factor (α_wind)**: Scales wind-induced loads

### Formula
```
Scaled_Load = α_wave × Wave_Component + α_wind × Wind_Component
```

### Key Parameters
- **Reference Seastate**: Hs = 5.0m, Tp = 10.3s
- **Fatigue Seastates**: Various conditions with different Hs and Tp
- **Scaling Application**: Applied to effective tension time series

## 📋 Input Files Required

### Configuration Files (YAML)
- `master_input_config_*.yml`: Main configuration file
- `load_scaling_config.yml`: Scaling parameters

### Data Files (CSV)
- `fatigue_seastates_*.csv`: Fatigue seastate definitions
- `reference_seastate_definitions_*.csv`: Reference seastate parameters
- Time series data files (in `core/sample_data/`)

## 📈 Output Files

### Generated Outputs
- Scaled time series data
- Fatigue damage calculations
- Verification reports
- Comparison charts (if visualization enabled)

## 🧪 Verification

The verification process includes:
1. **Input Validation**: Check all required files exist
2. **Scaling Factor Calculation**: Verify correct factor computation
3. **Load Scaling**: Validate scaled time series
4. **Rainflow Counting**: Check cycle extraction
5. **Damage Calculation**: Verify fatigue damage results

## 🗑️ Cleanup Recommendations

### Can Be Deleted
- `/rev_0/` - Complete previous implementation (archived)
- `/core/sample_data/` - If duplicates exist in rev_0
- Old verification outputs not needed for current work

### Must Keep
- All files in `/input/` - Configuration and definitions
- Root level Python scripts - Core implementation
- `/docs/` - Documentation
- `/reference_data/` - Validation data

## 📚 Related Documentation

- [LOAD_SCALING_PROGRAM_DOCUMENTATION.md](LOAD_SCALING_PROGRAM_DOCUMENTATION.md) - Detailed program documentation
- [LOAD_SCALING_VERIFICATION.md](LOAD_SCALING_VERIFICATION.md) - Verification results and methodology
- [docs/ARCHITECTURE_SPECIFICATION.md](docs/ARCHITECTURE_SPECIFICATION.md) - System architecture
- [docs/INPUT_SPECIFICATION.md](docs/INPUT_SPECIFICATION.md) - Input file specifications

## 🔧 Dependencies & Environment

### Using Repository UV Environment
The repository uses `uv` for Python dependency management. All dependencies are defined in `pyproject.toml` and locked in `uv.lock`.

**To use the repository environment:**
```bash
# Install dependencies if not already installed
uv sync

# Run scripts with uv
uv run python script_name.py
```

### Required Dependencies
- Python 3.8+
- NumPy
- Pandas
- PyYAML
- Matplotlib (for visualization)
- SciPy (for interpolation)

All dependencies are automatically managed through the repository's `uv` environment.

## 📝 Notes

- This implementation focuses on load scaling methodology
- Previous fatigue analysis implementation archived in `/rev_0/`
- Sample data provided for testing and verification
- Production configurations available for real-world applications

## 🤝 Contributing

When contributing:
1. Keep load scaling files separate from archived content
2. Update verification scripts when modifying calculations
3. Document any changes to scaling methodology
4. Ensure all tests pass before committing

## 📞 Support

For questions about:
- Load scaling methodology: See `LOAD_SCALING_PROGRAM_DOCUMENTATION.md`
- Input formats: See `docs/INPUT_SPECIFICATION.md`
- Verification process: See `LOAD_SCALING_VERIFICATION.md`