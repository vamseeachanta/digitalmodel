# OrcaFlex Batch Processing Tests

This directory contains test files and configurations for the OrcaFlex batch processing system.

## Directory Structure

### test_configs/
Contains YAML configuration files for testing:
- `batch_all_fsts_vessel_statics_6dof.yml` - 6DOF vessel statics batch configuration
- `batch_network_fsts.yml` - Network FSTS batch configuration  
- `fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.yml` - Specific FSTS configuration
- `test_empty.yml` - Empty test configuration
- `test_batch_config.yml` - General test batch configuration

### test_scripts/
Contains Python test scripts:
- `test_existing_orcaflex_models.py` - Tests with actual OrcaFlex models
- `test_failure_recovery_direct.py` - Direct failure recovery testing
- `test_orcaflex_sim_generation.py` - .sim file generation tests
- `generate_batch_config_6dof.py` - Generates 6DOF batch configurations
- `launch_batch_background.bat` - Windows batch launcher
- `launch_batch_background.ps1` - PowerShell launcher
- `run_batch_background.py` - Background batch runner

### Core Test Files
- `test_with_actual_models.py` - Main test suite with failure recovery
- `test_batch_runner.py` - Unit tests for batch runner
- `run_tests.py` - Test runner script

## Running Tests

### Basic Test
```bash
python test_batch_runner.py
```

### Failure Recovery Test
```bash
python test_scripts/test_failure_recovery_direct.py
```

### Full Test Suite
```bash
python run_tests.py
```

## Features Tested

1. **Batch Processing** - Multiple model processing
2. **Failure Recovery** - Automatic failure summary and rerun configs
3. **Parallel Processing** - Concurrent model execution
4. **Mock Mode** - Testing without OrcaFlex license
5. **File Type Detection** - Automatic model format identification