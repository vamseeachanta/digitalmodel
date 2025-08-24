# Mooring Tension Iteration Orchestrator - Usage Guide

## Quick Start

### Windows
```batch
# Run with default settings (from this directory)
run_orchestrator.bat

# Run with verbose output
run_verbose.bat

# Test with 2 iterations only
run_test.bat
```

### Unix/Linux/Git Bash
```bash
# Make script executable (first time only)
chmod +x run_orchestrator.sh

# Run with default settings
./run_orchestrator.sh

# Run with verbose output
./run_orchestrator.sh --verbose

# Test with 2 iterations
./run_orchestrator.sh --max-iterations 2
```

## Direct Python Execution

### From go-by directory
```bash
cd go-by
python ../orchestrator.py
```

### With DigitalModel virtual environment
```bash
cd go-by
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py
```

## Command-Line Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--working-dir` | `-d` | Directory with batch files | `go-by` |
| `--max-iterations` | `-m` | Maximum iterations to run | `10` |
| `--verbose` | `-v` | Show detailed output | `false` |
| `--dry-run` | | Preview without execution | `false` |
| `--help` | `-h` | Show help message | |

## Examples

### Basic Execution
```bash
python orchestrator.py
```
Runs up to 10 iterations from go-by folder until convergence.

### Verbose Mode
```bash
python orchestrator.py --verbose
```
Shows detailed command output and debugging information.

### Limited Iterations
```bash
python orchestrator.py --max-iterations 5
```
Runs maximum 5 iterations instead of default 10.

### Different Working Directory
```bash
python orchestrator.py --working-dir ../another-folder
```
Uses a different folder containing the batch files.

### Dry Run
```bash
python orchestrator.py --dry-run
```
Shows what commands would be executed without running them.

## Output Interpretation

### Convergence Status
```
Convergence Status:
----------------------------------------------------------------------
  Line01: 79.5 kN (target: 80.0 kN, diff: 0.6%, tol: 50%) ✓
  Line02: 80.2 kN (target: 80.0 kN, diff: 0.3%, tol: 50%) ✓
  ...
  Line16: 59.8 kN (target: 60.0 kN, diff: 0.3%, tol: 1%) ✓
----------------------------------------------------------------------
Converged: 16/16 lines
```

- `✓` = Line is within tolerance (converged)
- `✗` = Line is outside tolerance (not converged)
- `diff` = Percentage difference from target
- `tol` = Allowed tolerance percentage

### Convergence Trend
```
Convergence Trend:
--------------------------------------------------
  Iteration 1: 12/16 lines converged (75.0%)
  Iteration 2: 14/16 lines converged (87.5%)
  Iteration 3: 15/16 lines converged (93.8%)
  Iteration 4: 16/16 lines converged (100.0%) CONVERGED
```

Shows progression across iterations until convergence or max iterations.

## Required Files

The orchestrator expects these files in the working directory:

### Input Files
- `fsts_l015_125km3_pb_target_mooring_pretension.csv` - Target tensions and tolerances
- `dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Tension calculation config
- `dm_ofx_post_fsts_lngc.yml` - Post-processing config
- OrcaFlex model files (`*.yml`, `*.dat`)

### Output Files
- `results/*_pretension_analysis.csv` - Current tension values (updated each iteration)

## Troubleshooting

### "Target CSV not found"
Ensure `fsts_l015_125km3_pb_target_mooring_pretension.csv` exists in working directory.

### "No tension data found"
Check that the batch commands are generating result CSV files in the expected location.

### Commands fail with non-zero exit code
- Ensure DigitalModel framework is installed
- Verify OrcaFlex license is available
- Check that all required YAML config files exist

### Not converging after 10 iterations
- Review tolerance values in target CSV
- Some lines may have very tight tolerances (1%)
- Consider increasing max iterations or adjusting tolerances

## Exit Codes

- `0` = Successfully converged
- `1` = Did not converge or error occurred

## Performance

Typical execution times per iteration:
- Tension calculation: 1-2 minutes
- OrcaFlex analysis: 5-10 minutes
- Post-processing: 2-3 minutes

Total: ~10-15 minutes per iteration

## Support

For issues or questions:
1. Check this usage guide
2. Review the specification in `spec.md`
3. Examine the logs with `--verbose` flag
4. Contact the engineering team