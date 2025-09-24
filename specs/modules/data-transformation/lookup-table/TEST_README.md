# Tension to Stress Range Transformation Test

## Overview
This test suite validates the data transformation module that converts tension ranges (kN) to stress ranges (MPa) using a lookup table with linear interpolation.

## Test Files

### Configuration Files
- `inputs/test_transformation_config.yaml` - Main configuration file (YAML format)
- `inputs/test_transformation_config.json` - Alternative configuration (JSON format)
- `inputs/tension_range_to_stress_range_function.csv` - Lookup table for conversion

### Scripts
- `test_process_transformation.py` - Main transformation processor
- `run_test.py` - Test runner and validator

### Test Data
- `data/fsts_l015_125km3_l100_pb_FC001_Strut*.csv` - Sample rainflow data files

## How to Run the Test

### Basic Execution
```bash
# Run with default test configuration
python specs/modules/data-transformation/lookup-table/test_process_transformation.py

# Or run the full test suite with validation
python specs/modules/data-transformation/lookup-table/run_test.py
```

### Custom Configuration
```bash
# Specify a different configuration file
python specs/modules/data-transformation/lookup-table/test_process_transformation.py path/to/config.yaml
```

## Test Process

The test performs the following steps:

1. **Input Validation**
   - Checks that all required input files exist
   - Validates configuration format

2. **Transformation Execution**
   - Loads lookup table with 7 location IDs (2, 3, 5, 6, 7, 9, 10)
   - Creates linear interpolators for each location ID and config combination
   - Processes all CSV files matching the pattern
   - Applies tension-to-stress conversion using interpolation
   - Generates separate output files for each location ID

3. **Output Validation**
   - Verifies output files are created with correct naming pattern
   - Checks that required columns exist (stress range, Cycles_Annual)
   - Validates data integrity and ranges

4. **Report Generation**
   - Creates `test_report.json` with test summary
   - Documents configuration used and files processed

## Expected Output

For each input file (e.g., `fsts_l015_125km3_l100_pb_FC001_Strut1_rainflow.csv`), the test generates 7 output files:

```
fsts_l015_125km3_l100_pb_FC001_Strut1_loc02_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc03_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc05_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc06_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc07_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc09_stress_rainflow.csv
fsts_l015_125km3_l100_pb_FC001_Strut1_loc10_stress_rainflow.csv
```

Each output file contains:
- `stress range (Mpa)` - Converted stress values
- `Cycles_Annual` - Annual cycle counts from input

## Configuration Details

### Key Settings
- **Interpolation Method**: Linear
- **Extrapolation**: Enabled (allows values beyond lookup table range)
- **Skip Empty Ranges**: Yes (filters out zero values)
- **Location IDs**: Extracted from lookup table (2, 3, 5, 6, 7, 9, 10)

### Config Mapping
The test handles config name variations:
- `fsts_l015_125km3_l100_pb` → `fsts_l015_125km3_l100_pb_mwl`
- `fsts_l095_125km3_l000_pb` → `fsts_l095_125km3_l000_pb_mwl`
- `fsts_l015` → `fsts_l015_mwl`
- `fsts_l095` → `fsts_l095_mwl`

## Test Results

A successful test run will show:
```
[SUCCESS] ALL TESTS PASSED SUCCESSFULLY!
```

The test validates:
- All 7 location ID files are generated per input
- Stress ranges are correctly calculated
- Annual cycles are preserved from input
- File naming follows the pattern with zero-padded location IDs

## Troubleshooting

### Common Issues

1. **Missing Files Error**
   - Ensure all input files exist in the correct directories
   - Check file paths in configuration

2. **No Interpolator Warning**
   - Verify config mapping is correct
   - Check that lookup table contains data for the config/location combination

3. **Output Directory Not Found**
   - The output directory is created automatically
   - Check write permissions for the output path

### Logging
The transformation process creates `transformation_log.txt` with detailed execution information. Check this file for debugging if issues occur.

## Next Steps

After successful test execution:
1. Review generated output files in the output directory
2. Check `test_report.json` for test summary
3. Proceed with production data processing using the same configuration structure