# OrcaFlex Modular YML Validation Guide

## Quick Start

Run the validation script to verify all modular files:

```bash
# From the module directory
./scripts/run_validation.sh

# Or directly with Python
python scripts/validate_modules.py --report
```

This will:
- ✅ Validate YAML syntax for all files
- ✅ Verify all `includefile` references exist
- ✅ Check module dependencies
- ✅ Generate a detailed status report (`output/VALIDATION_STATUS.md`)

## Validation Script Features

### 1. YAML Syntax Validation
- Ensures all files are valid YAML 1.1
- Detects syntax errors and formatting issues
- Reports line numbers for errors

### 2. Include File Verification
- Checks that all referenced modules exist
- Validates file paths are correct
- Detects missing or broken references

### 3. Dependency Analysis
- Maps which modules each configuration uses
- Identifies shared vs. configuration-specific modules
- Analyzes reusability across configurations

### 4. Static Analysis
- File size and line count statistics
- Content validation (non-empty files)
- Module usage patterns

## Command-Line Options

```bash
# Basic validation (console output only)
python scripts/validate_modules.py

# With markdown report generation
python scripts/validate_modules.py --report

# Validate specific configuration files
python scripts/validate_modules.py --config calm_buoy_base.yml

# Custom base directory
python scripts/validate_modules.py --base-dir /path/to/project

# Custom report output location
python scripts/validate_modules.py --report --report-file /path/to/report.md
```

## Output Files

### Console Output
Real-time validation progress with summary:
```
======================================================================
OrcaFlex Modular YML Validation Suite
======================================================================

Validating Base Configuration Files...
  - calm_buoy_base.yml... [PASS]
  - discretised_calm_buoy_base.yml... [PASS]

Validating 20 Module Files...

======================================================================
VALIDATION SUMMARY
======================================================================
Overall Status: PASS

Base Files: 2/2 passed
Modules:    20/20 passed
Errors:     0
Warnings:   0
======================================================================
```

### Markdown Report (`output/VALIDATION_STATUS.md`)
Comprehensive validation report including:
- **Summary Statistics** - Pass/fail counts, error/warning tallies
- **Base Configuration Details** - Sections, modules, file sizes
- **Module Usage Analysis** - Shared vs. specific modules
- **Module File Details** - Complete file inventory with metadata
- **Errors & Warnings** - Detailed issue listings (if any)
- **Recommendations** - Action items for resolution

## Status Indicators

| Icon | Status | Meaning |
|------|--------|---------|
| ✅ | PASS | File validated successfully |
| ⚠️ | WARN | File valid but has warnings |
| ❌ | FAIL | File has errors that must be fixed |

## Validation Checks

### For Base Configuration Files
- [x] File exists and is readable
- [x] Valid YAML 1.1 syntax
- [x] All referenced `includefile` modules exist
- [x] Contains expected OrcaFlex sections
- [x] File size is reasonable

### For Module Files
- [x] File exists and is readable
- [x] Valid YAML 1.1 syntax
- [x] Contains actual content (not empty)
- [x] No circular dependencies
- [x] File size matches expected content

## Common Issues and Fixes

### Issue: "YAML syntax error: expected '<document start>'"

**Cause:** Module file contains section keys (e.g., `General:`, `VesselTypes:`)

**Fix:** Remove section keys from module files - they should only contain the data

**Example:**
```yaml
# ❌ WRONG - Module file with section key
VesselTypes:
  - Name: Vessel Type1
    Length: 103

# ✅ CORRECT - Module file without section key
- Name: Vessel Type1
  Length: 103
```

### Issue: "Missing module 'filename.yml'"

**Cause:** Base file references a module that doesn't exist

**Fix:**
1. Check the filename spelling in the base file
2. Verify the module file exists in the `output/` directory
3. Create the missing module if needed

### Issue: "File is empty or has no content"

**Cause:** Module file exists but contains no data

**Fix:** Populate the module with appropriate YAML content from the source file

## Integration with OrcaFlex

### Before Loading in OrcaFlex
Always run validation:
```bash
./scripts/run_validation.sh
```

Check the status:
- **PASS** → Safe to load in OrcaFlex
- **WARN** → Review warnings, may still load
- **FAIL** → Fix errors before loading

### After Modifying Modules
Re-validate to ensure integrity:
```bash
./scripts/run_validation.sh
```

### Continuous Validation
Run validation as part of your workflow:
```bash
# Example: Validate before committing to git
git add .
./scripts/run_validation.sh && git commit -m "Update modules"
```

## Interpreting the Validation Report

### Summary Statistics Section
Quick overview of validation results:
- **Base Configuration Files:** Should be 2/2 for both configs
- **Module Files:** Should match total unique modules (typically 20)
- **Errors:** Must be 0 for PASS status
- **Warnings:** Acceptable but should be investigated

### Module Usage Analysis Section
Understand module reusability:
- **Shared Modules:** Used by multiple configurations (maximize reuse)
- **Configuration-Specific:** Unique to one configuration (variants)

### Module File Details Section
Complete inventory with:
- File sizes (helps identify large modules for potential subdivision)
- Line counts (indicates complexity)
- Status (immediate health check)

## Best Practices

### 1. Validate Frequently
- After creating new modules
- After modifying existing modules
- Before loading in OrcaFlex
- Before committing to version control

### 2. Keep Reports for History
```bash
# Save timestamped reports
python scripts/validate_modules.py --report-file reports/validation_$(date +%Y%m%d_%H%M%S).md
```

### 3. Automate Validation
Add to scripts or CI/CD pipelines:
```bash
#!/bin/bash
# validate_before_commit.sh
./scripts/run_validation.sh
if [ $? -ne 0 ]; then
    echo "Validation failed! Fix errors before committing."
    exit 1
fi
```

### 4. Monitor File Sizes
Large modules (>100KB) may benefit from subdivision:
- Current largest: `_04_vessel_types.yml` (115KB, 1450 lines)
- Consider splitting for vessel sensitivity studies

## Troubleshooting

### Script Won't Run

**Python not found:**
```bash
# Check Python installation
python --version
# Or try
python3 --version
```

**Missing PyYAML:**
```bash
pip install pyyaml
```

### Windows Encoding Issues

If you see Unicode errors on Windows console:
- The script automatically uses ASCII-safe characters
- Or redirect output to file:
  ```bash
  python scripts/validate_modules.py --report > validation.log 2>&1
  ```

### Permission Errors

Ensure write permissions for report file:
```bash
chmod +w output/VALIDATION_STATUS.md
```

Make bash script executable:
```bash
chmod +x scripts/run_validation.sh
```

## Exit Codes

The script returns standard exit codes for automation:

| Code | Meaning | Description |
|------|---------|-------------|
| 0 | Success | All validations passed |
| 1 | Failure | Errors found that must be fixed |

Use in scripts:
```bash
if ./scripts/run_validation.sh; then
    echo "Validation passed - safe to proceed"
else
    echo "Validation failed - review errors"
    exit 1
fi
```

## Advanced Usage

### Validating Custom Configurations

Create your own base file, then validate:
```bash
# Create custom_config.yml with includefiles
python scripts/validate_modules.py --config custom_config.yml --report
```

### Batch Validation

Validate multiple projects:
```bash
for dir in project1 project2 project3; do
    cd $dir
    ./scripts/run_validation.sh
    cd ..
done
```

### Integration with Git Hooks

Add to `.git/hooks/pre-commit`:
```bash
#!/bin/bash
cd specs/modules/orcaflex/modular-input-file
./scripts/run_validation.sh
if [ $? -ne 0 ]; then
    echo "YML validation failed. Fix errors before committing."
    exit 1
fi
```

## Support

For issues or questions:
1. Check `output/VALIDATION_STATUS.md` for detailed error messages
2. Review this guide for common solutions
3. Refer to `output/README.md` for modular file structure documentation

---

**Generated for:** OrcaFlex 11.5e Modular YML Files
**Validation Script:** `scripts/validate_modules.py`
**Bash Wrapper:** `scripts/run_validation.sh`
**Report Output:** `output/VALIDATION_STATUS.md`
