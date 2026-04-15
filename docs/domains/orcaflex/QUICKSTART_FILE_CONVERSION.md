# OrcaFlex File Conversion - Quick Start Guide

**ABOUTME**: Quick start guide for converting OrcaFlex files between .dat, .yml, and .sim formats using the enhanced converter.

---

## 🚀 Quick Start

### Installation

The converter is included in the `digitalmodel` package. No additional installation required.

### Basic Usage

#### Convert Single File (.dat → .yml)

```python
from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced

# Create converter
converter = OrcaFlexConverterEnhanced(output_format='yml')

# Convert file
success, output_file, error = converter.convert_file("model.dat")

if success:
    print(f"✓ Converted to: {output_file}")
```

#### Convert Single File (.yml → .dat)

```python
converter = OrcaFlexConverterEnhanced(output_format='dat')
success, output_file, error = converter.convert_file("model.yml")
```

#### Batch Convert Directory

```python
from pathlib import Path

converter = OrcaFlexConverterEnhanced(
    input_dir=Path("models/"),
    output_dir=Path("models_yml/"),
    output_format='yml',
    parallel=True  # Enable parallel processing
)

results = converter.convert_batch()
print(f"Converted {results['statistics']['successful']} files")
```

---

## 📋 Command Line Interface

### Reverse extraction boundary (important)

`modular2spec` and `single2spec` are best-effort reverse-extraction helpers.
They are useful for migration scaffolding, inventory, and partial recovery of
Environment / General intent, but they are not a lossless round-trip path from
OrcaFlex strict YAML back into canonical `spec.yml`.

Current expected reverse-extraction limits include object-rich OrcaFlex sections
such as `VesselTypes`, `Vessels`, and `Groups`. Those are out of scope for the
simplified `spec.yml` abstraction and should not be treated as converter bugs.
Sections reported as actionable gaps are different: they indicate places where
additional reverse extraction may still be feasible.

Treat extracted specs as review-required artifacts, not canonical proof that a
strict YAML model can be reconstructed without semantic loss.

### Single File Conversion

```bash
# Convert .dat to .yml
python -m digitalmodel.orcaflex.convert_cli model.dat

# Convert .yml to .dat
python -m digitalmodel.orcaflex.convert_cli model.yml --format dat
```

### Batch Conversion

```bash
# Convert all .dat files in directory
python -m digitalmodel.orcaflex.convert_cli --batch models/ models_yml/

# Convert with pattern
python -m digitalmodel.orcaflex.convert_cli \
    --batch models/ output/ \
    --pattern "*.dat"

# Parallel processing (4 workers)
python -m digitalmodel.orcaflex.convert_cli \
    --batch models/ output/ \
    --parallel --workers 4
```

### Mock Mode (No License)

```bash
# Test without OrcaFlex license
python -m digitalmodel.orcaflex.convert_cli \
    --batch models/ output/ \
    --mock
```

---

## 🎯 Common Workflows

### Workflow 1: Prepare for Version Control

Convert binary .dat files to YAML for Git:

```bash
python -m digitalmodel.orcaflex.convert_cli \
    --batch models/ models_version_control/ \
    --pattern "*.dat"

git add models_version_control/*.yml
git commit -m "Add OrcaFlex models in YAML format"
```

### Workflow 2: Edit Models in YAML, Run in .dat

```python
from pathlib import Path
from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced

# Step 1: Convert to YAML for editing
to_yml = OrcaFlexConverterEnhanced(
    input_dir=Path("models/dat/"),
    output_dir=Path("models/yml/"),
    output_format='yml'
)
to_yml.convert_batch()

# Step 2: Edit YAML files manually
# (Use your favorite editor)

# Step 3: Convert back to .dat for OrcaFlex
to_dat = OrcaFlexConverterEnhanced(
    input_dir=Path("models/yml/"),
    output_dir=Path("models/executable/"),
    output_format='dat'
)
to_dat.convert_batch()

# Step 4: Run OrcaFlex simulations
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

runner = UniversalOrcaFlexRunner(
    input_directory="models/executable/",
    output_directory="results/.sim/"
)
runner.run_batch(pattern="*.dat")
```

### Workflow 3: Extract Models from Simulations

```python
# Convert .sim files to both .dat and .yml
converter = OrcaFlexConverterEnhanced(
    output_dir=Path("extracted_models/"),
    output_format='yml'  # or 'dat'
)

sim_file = Path("results/.sim/simulation_001.sim")
success, yml_file, _ = converter.convert_file(sim_file)
```

---

## 📊 Output & Reports

After batch conversion, find reports in the output directory:

- **`conversion_report.md`** - Human-readable summary
- **`conversion_report.json`** - Detailed JSON data

Example Markdown report:

```markdown
# OrcaFlex Batch Conversion Report

Generated: 2026-01-02 10:30:45

## Summary

- **Total Files**: 180
- **Successful**: 178
- **Failed**: 2
- **Skipped**: 0
- **Processing Time**: 145.32s
- **Output Format**: .yml
- **Mode**: Real (OrcFxAPI)

## Success Rate: 98.9%
```

---

## 🧪 Testing

### Run Tests

```bash
# Run all converter tests
pytest tests/domains/orcaflex/test_orcaflex_converter_enhanced.py -v

# Run specific test
pytest tests/domains/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_single_dat_to_yml -v

# Run with coverage
pytest tests/domains/orcaflex/test_orcaflex_converter_enhanced.py --cov=digitalmodel.orcaflex
```

### Round-Trip Validation

```python
from pathlib import Path
from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced

original = Path("model.dat")

# .dat → .yml
c1 = OrcaFlexConverterEnhanced(output_dir=Path("temp"), output_format='yml')
_, yml_file, _ = c1.convert_file(original)

# .yml → .dat
c2 = OrcaFlexConverterEnhanced(output_dir=Path("temp"), output_format='dat')
_, new_dat, _ = c2.convert_file(yml_file)

# Validate
original_size = original.stat().st_size
new_size = new_dat.stat().st_size
diff_pct = abs(new_size - original_size) / original_size * 100

if diff_pct < 5:
    print("✓ Round-trip validated!")
else:
    print(f"⚠ Size difference: {diff_pct:.2f}%")
```

---

## 🔧 Configuration Options

### Converter Parameters

```python
OrcaFlexConverterEnhanced(
    input_dir=Path,              # Source directory (batch mode)
    output_dir=Path,             # Destination directory
    output_format='yml',         # 'yml', 'dat', or 'both'
    use_mock=False,              # Mock mode (no OrcFxAPI)
    validate=True,               # Validate converted files
    max_retries=2,               # Retry failed conversions
    parallel=False,              # Enable parallel processing
    max_workers=4                # Number of parallel workers
)
```

### CLI Arguments

```bash
Options:
  --batch            # Batch mode (directories)
  --pattern TEXT     # File pattern (e.g., "*.dat")
  --format {yml,dat} # Output format
  --mock             # Mock mode (no license)
  --parallel         # Enable parallel processing
  --workers INT      # Number of workers (default: 4)
  --help             # Show help
```

---

## 📚 Examples

Detailed examples in:
- **Python**: `docs/domains/orcaflex/examples/conversion_examples.py`
- **Tests**: `tests/domains/orcaflex/test_orcaflex_converter_enhanced.py`

Run examples:

```bash
python docs/domains/orcaflex/examples/conversion_examples.py
```

---

## Three-Way Format Converter: Reverse Extraction Limits

The `format_converter` module supports a three-way conversion between
`spec.yml` (simplified), modular OrcaFlex YAML, and single OrcaFlex YAML.
The **forward** path (`spec → modular/single`) is lossless by design.
The **reverse** path (`modular/single → spec`) is **best-effort only**.

### What "best-effort" means

`modular2spec` and `single2spec` extract only the subset of OrcaFlex data
that has a direct equivalent in `spec.yml` (Environment + General fields).
The output file header states this explicitly:

```
# Auto-extracted from OrcaFlex modular format (best-effort)
# NOT a lossless round-trip — only Environment/General fields are mapped
```

### Unmapped section categories

When sections cannot be mapped, the report classifies them:

| Category | Meaning | Examples |
|---|---|---|
| **Expected gaps** | Out of spec scope — no equivalent concept in `spec.yml` | `VesselTypes`, `Vessels`, `Groups`, `Buoys`, `Shapes` |
| **Actionable gaps** | Partial extraction exists or is feasible; higher-value sections | `Lines`, `LineTypes` |

CLI output distinguishes the two:

```
[OK] modular -> spec | best-effort | confidence=0.72 | unmapped=5
  Expected gaps (out of spec scope): Groups, VesselTypes, Vessels
  Actionable gaps (potential improvement): LineTypes, Lines
```

### Confidence score

The confidence score (`0.0 – 1.0`) reflects how many of the 14
Environment/General fields were successfully extracted. A score of `1.0`
means all 14 fields were found, not that the full model is recoverable.

### When to use reverse extraction

- **Bootstrapping a spec from an existing model** — gives you a starting
  point that you must review and complete manually.
- **Checking environment/simulation parameters** — high confidence for
  water depth, wave trains, time step.

Do **not** treat the extracted `spec.yml` as a canonical model definition
without reviewing the actionable gaps and filling in missing data.

---

## 🐛 Troubleshooting

### OrcFxAPI Not Available

**Error**: `ImportError: OrcFxAPI not available`

**Solution**: Install OrcaFlex Python API or use mock mode:

```bash
# Install OrcFxAPI
pip install <OrcaFlex_install_dir>/OrcFxAPI/Python

# Or use mock mode
python -m digitalmodel.orcaflex.convert_cli --batch models/ output/ --mock
```

### Conversion Failed

**Error**: `Conversion failed: [error message]`

**Solutions**:
1. Check file is valid OrcaFlex model
2. Increase `max_retries`
3. Check OrcaFlex license availability
4. Review error in `conversion_report.json`

### File Already Exists

Files are automatically skipped if output exists. To force reconversion, delete existing output files first.

---

## 📖 Related Documentation

- **Skill**: `.claude/skills/orcaflex-file-conversion/SKILL.md`
- **Source Code**: `src/digitalmodel/modules/orcaflex/orcaflex_converter_enhanced.py`
- **CLI**: `src/digitalmodel/modules/orcaflex/convert_cli.py`
- **Tests**: `tests/domains/orcaflex/test_orcaflex_converter_enhanced.py`
- **Examples**: `docs/domains/orcaflex/examples/conversion_examples.py`

---

## 💡 Tips & Best Practices

1. **Version Control**: Always commit YAML files, not .dat files (YAML is diff-friendly)
2. **Batch Processing**: Use parallel mode for >10 files
3. **Validation**: Enable validation for production conversions
4. **Mock Mode**: Test workflows without OrcaFlex license
5. **Reports**: Check conversion reports for failures
6. **Round-Trip**: Validate critical models with round-trip conversion

---

## 🚦 Next Steps

1. Try single file conversion
2. Run batch conversion on example files
3. Test round-trip validation
4. Integrate with your OrcaFlex workflow
5. Explore advanced features in full skill documentation

---

**Questions?** See full skill documentation: `.claude/skills/orcaflex-file-conversion/SKILL.md`
