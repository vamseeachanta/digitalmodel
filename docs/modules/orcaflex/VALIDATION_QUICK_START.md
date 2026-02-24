# CALM Buoy Validation - Quick Start Guide

Quick reference for validating `calm_buoy_base.yml` and `discretised_calm_buoy_base.yml`.

---

## 🚀 Quickest Start (Python Script)

```bash
# Run the ready-made validation script
python scripts/validate_calm_buoy_files.py
```

This validates both files and generates all 4 report types.

---

## 📋 CLI Commands

### Validate Both Files

```bash
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml
```

### Validate Entire Directory

```bash
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/
```

### Skip OrcaFlex API (Level 2)

If you don't have OrcaFlex installed:

```bash
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml \
  --skip-level 2
```

### Custom Tolerance

```bash
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  --tolerance 15  # Use ±15% instead of default ±10%
```

### Select Report Formats

```bash
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  --formats console csv markdown  # Skip HTML report
```

### Run Only Specific Level

```bash
# Only Level 1 (YAML syntax)
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  --level-1-only

# Only Level 3 (Physical consistency)
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  --level-3-only
```

---

## 🐍 Python API

### Simple Validation

```python
from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation import ModularInputValidator

# Create validator
validator = ModularInputValidator()

# Validate single file
result = validator.validate_file(
    "specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"
)

print(f"Status: {result.overall_status.value}")
print(f"Issues: {len(result.issues)}")
```

### Validate Multiple Files

```python
from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation import ModularInputValidator

files = [
    Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"),
    Path("specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml")
]

validator = ModularInputValidator()
results = validator.validate_files(files)
validator.generate_reports(results)
```

### Custom Configuration

```python
from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)

config = ValidationConfig(
    tolerance_percent=12.0,      # ±12% tolerance
    enable_orcaflex=False,       # Skip OrcaFlex if not installed
    skip_levels=[],              # Run all available levels
    report_formats=['console', 'csv', 'markdown']  # Skip HTML
)

validator = ModularInputValidator(config)
results = validator.run_validation(
    "specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"
)
```

---

## 📊 Output Locations

After validation, reports are generated in:

```
reports/validation/calm_buoy/
├── validation_YYYYMMDD_HHMMSS.csv       # CSV log
├── validation_YYYYMMDD_HHMMSS.md        # Markdown report
└── validation_YYYYMMDD_HHMMSS.html      # Interactive HTML report
```

Console output is displayed immediately during validation.

---

## 🔍 Understanding the Output

### Console Output

```
🟢 PASS    - All checks passed
🟡 WARN    - Passed with warnings
🔴 FAIL    - Critical issues found
⏭️ SKIPPED - Level skipped (e.g., OrcaFlex unavailable)
```

### Validation Levels

- **Level 1 (YAML)**: Syntax, includefiles, structure
- **Level 2 (OrcaFlex)**: API loading, static analysis (requires OrcaFlex)
- **Level 3 (Physical)**: Parameter ranges, project comparison

### Status Codes

- `PASS`: All validations passed ✅
- `WARN`: Passed with warnings (outside typical ranges) ⚠️
- `FAIL`: Critical issues found (safety factors, syntax errors) ❌
- `SKIPPED`: Level not run (software unavailable) ⏭️

---

## 🛠️ Common Issues

### Issue: "OrcaFlex not available"

**Solution**: Skip Level 2 validation:
```bash
--skip-level 2
```

Or in Python:
```python
config = ValidationConfig(enable_orcaflex=False)
```

### Issue: "File not found: data/..."

**Solution**: Ensure you're running from the repository root:
```bash
cd D:/workspace-hub/digitalmodel
python -m src.digitalmodel.orcaflex.modular_input_validation.cli ...
```

### Issue: "Missing include file"

**Solution**: This is reported as a WARNING in Level 1. Check that all files referenced in `includefile` fields exist relative to the base YAML file.

---

## 📝 Full Example Session

```bash
# Navigate to repository root
cd D:/workspace-hub/digitalmodel

# Run validation on both files
python -m src.digitalmodel.orcaflex.modular_input_validation.cli \
  specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml \
  specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml \
  --tolerance 10 \
  --formats console csv markdown html

# Check the generated reports
ls -la reports/validation/calm_buoy/

# View the HTML report (most comprehensive)
# Open: reports/validation/calm_buoy/validation_YYYYMMDD_HHMMSS.html
```

---

## 💡 Tips

1. **Start with Level 1 only** to check YAML syntax before running full validation:
   ```bash
   --level-1-only
   ```

2. **Use the Python script** for quick testing:
   ```bash
   python scripts/validate_calm_buoy_files.py
   ```

3. **Check the HTML report** for interactive visualizations and detailed analysis

4. **Adjust tolerance** if your design is outside typical ranges but validated:
   ```bash
   --tolerance 15  # Increase to ±15%
   ```

5. **Run in CI/CD** without OrcaFlex:
   ```bash
   --no-orcaflex --formats console csv
   ```

---

## 📚 More Information

- Full Documentation: `src/digitalmodel/modules/orcaflex/modular_input_validation/README.md`
- Usage Examples: `docs/modules/orcaflex/modular_input_validation_example.py`
- Specification: `specs/modules/orcaflex/modular-input-file/ENHANCED_VALIDATION_SPEC.md`
