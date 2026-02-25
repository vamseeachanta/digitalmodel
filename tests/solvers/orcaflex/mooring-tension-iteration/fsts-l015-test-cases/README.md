# FSTS L015 Mooring Analysis Test Cases

This directory contains test cases and example configurations for the FSTS L015 vessel mooring analysis, including both port (PB) and starboard (SB) configurations at 125km3 location.

## Test Data Structure

```
fsts-l015-test-cases/
├── scripts/              # Test configuration files and scripts
│   ├── *.yml            # OrcaFlex analysis configurations
│   ├── *.csv            # Target pretension values
│   └── *.py             # Helper scripts
├── output/              # Test output files
│   ├── report/          # Analysis reports
│   ├── visual/          # Visualization outputs
│   └── plots/           # Generated plots
└── results/             # Processing results
```

## Test Configurations

### Vessel Configurations
- **FSTS L015 HWL 125km3 L100 PB**: Port side configuration
- **FSTS L015 HWL 125km3 L100 SB**: Starboard side configuration

### Analysis Types
1. **Pretension Analysis**: Mooring line pretension convergence
2. **Stiffness Analysis**: 3D mooring stiffness matrices
3. **Fender Force Analysis**: Fender compression forces
4. **Comparative Analysis**: Cross-configuration comparisons

## Running Tests

### Using the Analysis Module
```bash
# Run comparative analysis on test data
python -m digitalmodel.orcaflex.analysis \
    --input-directory tests/domains/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output/.csv \
    --output-directory tests/domains/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/output
```

### Using Configuration Files
```bash
# Run with specific test configuration
python -m digitalmodel.orcaflex.mooring \
    --config tests/domains/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
```

## Test Scripts Documentation

### run_models_to_sim.py
Converts OrcaFlex model files (.dat/.yml) to simulation files (.sim).

**Usage Examples:**
```bash
# Process all .yml files
python run_models_to_sim.py all=true

# Process .yml files with pattern
python run_models_to_sim.py all=true pattern="fsts_*.yml"

# Process both .yml and .dat files
python run_models_to_sim.py all=true include_dat=true

# Process specific files
python run_models_to_sim.py models="model1.yml model2.yml"
```

### generate_mooring_plots.py
Generates visualization plots from analysis results.

## Test Data Files

### Input Configurations
- `dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`: Port side analysis config
- `dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml`: Starboard side analysis config
- `fsts_l015_125km3_pb_target_mooring_pretension.csv`: Port side target tensions
- `fsts_l015_125km3_sb_target_mooring_pretension.csv`: Starboard side target tensions

### Output Files
- `*_pretension_analysis.csv`: Pretension convergence results
- `*_mooring_stiffness_analysis.csv`: Detailed stiffness calculations
- `*_mooring_stiffness_summary.csv`: Stiffness matrix summary
- `*_fender_force_analysis.csv`: Fender force results

## Verification Reports

Test verification is documented in:
- `task_0.1_verification_report.md`: Initial verification results
- `output/report/analysis_report.md`: Comparative analysis report

## Related Documentation

- Main specification: `specs/modules/orcaflex/mooring-analysis/`
- Analysis module: `src/digitalmodel/modules/orcaflex/analysis/`
- Core mooring module: `src/digitalmodel/modules/orcaflex/mooring.py`

## Notes

These test cases were originally developed as "go-by" examples and have been reorganized for better structure and maintainability. They serve as reference implementations and validation data for the mooring analysis module.