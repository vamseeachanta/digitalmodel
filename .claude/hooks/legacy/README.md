# Verification Hooks System

## 🚨 MANDATORY: Use These Hooks for ALL Repository Operations

This directory contains comprehensive verification hooks that **MUST** be used for all commands, tasks, and operations in the digitalmodel repository.

## Files

- **`verification_hooks.sh`** - Bash verification functions
- **`verification_hooks.py`** - Python verification classes
- **`example_usage.sh`** - Complete example implementation
- **`README.md`** - This documentation

## Core Principles

### Every Command Must Verify:
1. **Pre-execution**: Required files exist, environment is ready
2. **Execution**: Return codes, output generation
3. **Post-execution**: Output files created, no errors in logs
4. **Summary**: Clear pass/fail status

## Bash Usage

### Source the hooks:
```bash
source .agent-os/hooks/verification_hooks.sh
```

### Basic verification:
```bash
# Check return code
verify_return_code "python script.py" "Running Python script"

# Check file exists
verify_file_exists "config.yml" "Configuration file"

# Check file was created
verify_file_created "output.csv" 5 "Output file"

# Check output contains text
verify_output_contains "python --version" "Python 3" "Python version check"
```

### Complete workflow:
```bash
# Pre-execution checks
pre_execution_check "/path/to/working/dir" "file1.yml" "file2.csv" "file3.dat"

# Run command with verification
cmd="python -m module config.yml"
eval "$cmd"
ret=$?

# Post-execution checks
post_execution_check $ret "output1.csv" "output2.json"

# Generate summary
verification_summary $checks_passed $checks_failed $start_time $end_time
```

## Python Usage

### Import and initialize:
```python
from .agent_os.hooks.verification_hooks import VerificationHooks, OrcaFlexVerification

hooks = VerificationHooks(verbose=True)
```

### Basic verification:
```python
# Check file exists
hooks.verify_file_exists("config.yml", "Config file")

# Check return code
hooks.verify_return_code("python script.py", "Run script")

# Check CSV structure
success, data = hooks.verify_csv_structure(
    "data.csv",
    expected_columns=["id", "value", "timestamp"],
    min_rows=10
)

# Check JSON structure
success, data = hooks.verify_json_structure(
    "config.json",
    required_keys=["version", "settings", "data"]
)
```

### Complete workflow:
```python
# Pre-execution
hooks.pre_execution_check(
    working_dir="./go-by",
    required_files=["config.yml", "input.dat"]
)

# Execute command
import subprocess
result = subprocess.run(["python", "script.py"], capture_output=True)

# Post-execution
hooks.post_execution_check(
    return_code=result.returncode,
    output_files=["output.csv", "results.json"]
)

# Summary
success = hooks.verification_summary()
```

## OrcaFlex Specific

### Bash:
```bash
# Check license
verify_orcaflex_license

# Check outputs
verify_orcaflex_output "./results" "*.sim"
```

### Python:
```python
ofx_hooks = OrcaFlexVerification()

# Check license
ofx_hooks.verify_orcaflex_license()

# Check outputs
ofx_hooks.verify_orcaflex_output("./results", "*.sim")
```

## Color-Coded Output

All verification hooks use color coding for clarity:

- 🔵 **[CHECK]** - Verification in progress
- ✅ **[✓ PASS]** - Check passed
- ⚠️ **[⚠ WARN]** - Warning, may continue
- ❌ **[✗ FAIL]** - Check failed

## Integration Examples

### In Python Scripts:
```python
#!/usr/bin/env python
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent.parent))
from .agent_os.hooks.verification_hooks import VerificationHooks

def main():
    hooks = VerificationHooks()
    
    # Verify prerequisites
    if not hooks.verify_file_exists("input.dat"):
        return 1
    
    # Run process
    # ... your code ...
    
    # Verify outputs
    if not hooks.verify_file_created("output.csv"):
        return 1
    
    hooks.verification_summary()
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

### In Bash Scripts:
```bash
#!/bin/bash
source "$(dirname "$0")/.agent-os/hooks/verification_hooks.sh"

# Your script with verification at each step
verify_return_code "command1" "First step"
verify_file_created "output1.txt" 5 "First output"

verify_return_code "command2" "Second step"
verify_file_created "output2.txt" 5 "Second output"

# Summary at the end
verification_summary $checks_passed $checks_failed $start_time $end_time
```

### In Task Execution:
```python
# For /execute-tasks slash command
from .agent_os.hooks.verification_hooks import VerificationHooks

class TaskExecutor:
    def __init__(self):
        self.hooks = VerificationHooks()
    
    def execute_task(self, task_id):
        # Pre-execution verification
        self.hooks.pre_execution_check(...)
        
        # Execute task
        result = self.run_task(task_id)
        
        # Post-execution verification
        self.hooks.post_execution_check(...)
        
        # Report
        return self.hooks.verification_summary()
```

## Mandatory Rules

1. **NEVER** run commands without verification
2. **ALWAYS** check return codes
3. **ALWAYS** verify expected outputs were created
4. **ALWAYS** provide clear pass/fail status
5. **ALWAYS** use the repository's venv environment

## Quick Reference

### Essential Checks for Every Command:

```bash
# 1. Check working directory
verify_dir_exists "$WORKING_DIR"

# 2. Check required input files
verify_file_exists "$INPUT_FILE"

# 3. Run command and check return code
python -m module config.yml
ret=$?
[[ $ret -eq 0 ]] && echo "✓ Success" || echo "✗ Failed"

# 4. Verify output was created
verify_file_created "$OUTPUT_FILE" 10

# 5. Check for errors in logs
verify_log_no_errors "process.log"

# 6. Generate summary
verification_summary $checks_passed $checks_failed
```

## Environment Variables

Set these for consistent behavior:

```bash
export VERIFICATION_VERBOSE=1  # Enable verbose output
export VERIFICATION_STRICT=1   # Fail on first error
export VERIFICATION_LOGFILE="verification.log"  # Log to file
```

## Contributing

When adding new verification hooks:

1. Add to both `.sh` and `.py` versions
2. Use consistent naming: `verify_<what>_<aspect>`
3. Always return boolean success/failure
4. Always increment checks_passed/checks_failed counters
5. Always use color coding for output
6. Document with examples

## Support

For questions or improvements to the verification system, create an issue or PR with tag `verification-hooks`.