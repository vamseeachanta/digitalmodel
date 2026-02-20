# Marine Engineering Module Reorganization - Migration Guide

**Version:** 2.1.0
**Date:** October 3, 2025
**Status:** Complete

## Overview

The marine engineering project has been reorganized to follow proper Python package structure with clear separation of concerns:

- **Production code** → `src/digitalmodel/modules/marine_analysis/`
- **Test outputs** → `tests/outputs/`
- **Spec documentation** → `specs/outputs/marine-engineering/`

## What Changed

### 1. Production Scripts Moved to Module Structure

All production scripts from `scripts/` have been reorganized into proper Python modules under `src/digitalmodel/modules/marine_analysis/`:

#### Profiling Module (`profiling/`)
| Old Location | New Location |
|-------------|-------------|
| `scripts/profile_marine_modules.py` | `src/digitalmodel/modules/marine_analysis/profiling/profile_modules.py` |
| `scripts/generate_performance_charts.py` | `src/digitalmodel/modules/marine_analysis/profiling/performance_charts.py` |
| `scripts/generate_optimization_report.py` | `src/digitalmodel/modules/marine_analysis/profiling/optimization_report.py` |
| `scripts/run_performance_analysis.py` | `src/digitalmodel/modules/marine_analysis/profiling/run_analysis.py` |

#### Extraction Module (`extraction/`)
| Old Location | New Location |
|-------------|-------------|
| `scripts/extract_ocimf_database.py` | `src/digitalmodel/modules/marine_analysis/extraction/extract_ocimf.py` |
| `scripts/extract_hydro_coefficients.py` | `src/digitalmodel/modules/marine_analysis/extraction/extract_hydro.py` |
| `scripts/extract_mooring_components.py` | `src/digitalmodel/modules/marine_analysis/extraction/extract_mooring.py` |
| `scripts/run_hydro_extraction.py` | `src/digitalmodel/modules/marine_analysis/extraction/run_extraction.py` |

#### Validation Module (`validation/`)
| Old Location | New Location |
|-------------|-------------|
| `scripts/validate_phase2.py` | `src/digitalmodel/modules/marine_analysis/validation/validate_phase2.py` |
| `scripts/validate_catenary_solver.py` | `src/digitalmodel/modules/marine_analysis/validation/validate_catenary.py` |

#### Visualization Module (`visualization/`)
| Old Location | New Location |
|-------------|-------------|
| `scripts/generate_integration_charts.py` | `src/digitalmodel/modules/marine_analysis/visualization/integration_charts.py` |
| `scripts/visualize_ocimf_data.py` | `src/digitalmodel/modules/marine_analysis/visualization/ocimf_charts.py` |

#### Analysis Module (`analysis/`)
| Old Location | New Location |
|-------------|-------------|
| `scripts/analyze_marine_excel.py` | `src/digitalmodel/modules/marine_analysis/analysis/excel_analyzer.py` |
| `scripts/example_hydro_usage.py` | `src/digitalmodel/modules/marine_analysis/analysis/hydro_usage_example.py` |

### 2. Test Outputs Reorganized

All test output files have been moved from `docs/charts/` to `tests/outputs/`:

| Old Location | New Location |
|-------------|-------------|
| `docs/charts/phase2/ocimf/` | `tests/outputs/phase2/ocimf/` |
| `docs/charts/phase2/validation/` | `tests/outputs/phase2/validation/` |
| `docs/charts/phase2/*.png` | `tests/outputs/phase2/hydro/` |
| `docs/charts/phase3/integration/` | `tests/outputs/phase3/integration/` |
| `docs/charts/phase3/performance/` | `tests/outputs/phase3/performance/` |

### 3. New Directory Structure Created

```
src/digitalmodel/modules/marine_analysis/
├── __init__.py           # Updated with new submodule exports
├── __main__.py          # NEW: CLI entry point
├── profiling/           # NEW: Performance profiling
│   ├── __init__.py
│   ├── profile_modules.py
│   ├── performance_charts.py
│   ├── optimization_report.py
│   └── run_analysis.py
├── extraction/          # NEW: Data extraction
│   ├── __init__.py
│   ├── extract_ocimf.py
│   ├── extract_hydro.py
│   ├── extract_mooring.py
│   └── run_extraction.py
├── validation/          # NEW: Validation tools
│   ├── __init__.py
│   ├── validate_phase2.py
│   └── validate_catenary.py
├── visualization/       # NEW: Visualization tools
│   ├── __init__.py
│   ├── integration_charts.py
│   └── ocimf_charts.py
└── analysis/           # NEW: Analysis utilities
    ├── __init__.py
    ├── excel_analyzer.py
    └── hydro_usage_example.py

tests/outputs/           # NEW: All test outputs
├── phase2/
│   ├── ocimf/
│   ├── hydro/
│   └── validation/
└── phase3/
    ├── integration/
    └── performance/

specs/outputs/           # NEW: Specification documentation
└── marine-engineering/
    ├── analysis-reports/
    ├── extraction-reports/
    └── validation-dashboards/
```

## Migration Instructions

### For Users

#### Old Way (Scripts)
```python
# Old script execution
python scripts/profile_marine_modules.py --module wave_spectra
python scripts/extract_ocimf_database.py --vessel tanker
python scripts/validate_phase2.py --output results/
```

#### New Way (Module CLI)
```python
# New module-based CLI
python -m digitalmodel.marine_ops.marine_analysis profile --module wave_spectra
python -m digitalmodel.marine_ops.marine_analysis extract --type ocimf --input database.xlsx
python -m digitalmodel.marine_ops.marine_analysis validate --phase 2 --output tests/outputs/
```

#### Old Way (Direct Imports)
```python
# Old imports (NO LONGER WORK)
import sys
sys.path.append('scripts')
from profile_marine_modules import ProfilerTool
```

#### New Way (Package Imports)
```python
# New imports (RECOMMENDED)
from digitalmodel.marine_ops.marine_analysis.profiling import profile_modules
from digitalmodel.marine_ops.marine_analysis.extraction import extract_ocimf
from digitalmodel.marine_ops.marine_analysis.validation import validate_phase2

# Or use the unified interface
from digitalmodel.marine_ops.marine_analysis import profiling, extraction, validation

# Run profiling
profiler = profiling.profile_modules.ProfilerTool()
results = profiler.profile('wave_spectra')

# Extract OCIMF data
extractor = extraction.extract_ocimf.OCIMFExtractor()
data = extractor.extract_vessel('tanker_vlcc')

# Run validation
validator = validation.validate_phase2.Phase2Validator()
report = validator.validate_all()
```

### For Developers

#### Update Import Statements

If you have code that imports from the old locations, update as follows:

```python
# OLD (DEPRECATED)
from scripts.profile_marine_modules import ProfilerTool
from scripts.extract_ocimf_database import OCIMFExtractor
from scripts.validate_phase2 import Phase2Validator

# NEW (CORRECT)
from digitalmodel.marine_ops.marine_analysis.profiling.profile_modules import ProfilerTool
from digitalmodel.marine_ops.marine_analysis.extraction.extract_ocimf import OCIMFExtractor
from digitalmodel.marine_ops.marine_analysis.validation.validate_phase2 import Phase2Validator
```

#### Update Output Paths

Update any hardcoded paths to output directories:

```python
# OLD (DEPRECATED)
output_dir = "docs/charts/phase2/ocimf/"
results_dir = "docs/charts/phase3/performance/"

# NEW (CORRECT)
output_dir = "tests/outputs/phase2/ocimf/"
results_dir = "tests/outputs/phase3/performance/"
```

#### Update Test References

Update test files that reference old locations:

```python
# OLD
test_data_path = Path("docs/charts/phase2/validation/")

# NEW
test_data_path = Path("tests/outputs/phase2/validation/")
```

## CLI Commands Reference

The new module provides a unified CLI interface:

### Profile Command
```bash
# Profile all modules
python -m digitalmodel.marine_ops.marine_analysis profile --module all

# Profile specific module
python -m digitalmodel.marine_ops.marine_analysis profile --module wave_spectra --output tests/outputs/profiling/
```

### Extract Command
```bash
# Extract OCIMF data
python -m digitalmodel.marine_ops.marine_analysis extract --type ocimf --input database.xlsx --output tests/outputs/extraction/

# Extract hydrodynamic coefficients
python -m digitalmodel.marine_ops.marine_analysis extract --type hydro --input aqwa_output.lis --output tests/outputs/extraction/

# Extract mooring components
python -m digitalmodel.marine_ops.marine_analysis extract --type mooring --input mooring_db.xlsx --output tests/outputs/extraction/
```

### Validate Command
```bash
# Run Phase 2 validation
python -m digitalmodel.marine_ops.marine_analysis validate --phase 2 --output tests/outputs/validation/
```

### Visualize Command
```bash
# Generate integration charts
python -m digitalmodel.marine_ops.marine_analysis visualize --type integration --input data/ --output tests/outputs/visualization/

# Generate OCIMF charts
python -m digitalmodel.marine_ops.marine_analysis visualize --type ocimf --input ocimf_data.csv --output tests/outputs/visualization/

# Generate performance charts
python -m digitalmodel.marine_ops.marine_analysis visualize --type performance --input profiling_results/ --output tests/outputs/visualization/
```

### Analyze Command
```bash
# Analyze Excel file
python -m digitalmodel.marine_ops.marine_analysis analyze --input vessel_data.xlsx --output tests/outputs/analysis/
```

## Breaking Changes

### 1. Import Paths Changed
All imports must be updated to use the new package structure. The old `scripts/` directory imports will no longer work.

### 2. Output Directories Changed
Default output directories have changed:
- Charts: `docs/charts/` → `tests/outputs/`
- Reports: `outputs/` → `specs/outputs/marine-engineering/`

### 3. CLI Interface Changed
Direct script execution is deprecated. Use the module-based CLI instead.

## Benefits of New Structure

1. **Clear Separation of Concerns**
   - Production code in `src/`
   - Test outputs in `tests/`
   - Documentation in `specs/`

2. **Proper Python Package Structure**
   - All modules are importable
   - Follows PEP 8 guidelines
   - Better IDE support and autocomplete

3. **Unified CLI Interface**
   - Single entry point for all commands
   - Consistent argument structure
   - Better help documentation

4. **Improved Maintainability**
   - Clear module boundaries
   - Easier to test
   - Better code organization

5. **Better Version Control**
   - Cleaner git history
   - Easier to track changes
   - Better merge conflict resolution

## Compatibility Notes

### Backward Compatibility
The original scripts in `scripts/` directory have been **copied** (not moved) to the new locations, so existing workflows will continue to work temporarily. However, they are now **deprecated** and should be migrated to use the new package structure.

### Deprecation Timeline
- **October 2025**: New structure introduced, old scripts deprecated
- **November 2025**: Migration guide distributed
- **December 2025**: Old scripts removed from repository

## Support and Questions

If you encounter issues during migration:

1. Check this migration guide for updated import paths
2. Review the CLI reference for new command syntax
3. Examine the example code in the module docstrings
4. Contact the development team for assistance

## Verification Checklist

After migrating your code, verify:

- [ ] All imports updated to new package structure
- [ ] Output paths updated to `tests/outputs/` or `specs/outputs/`
- [ ] CLI commands updated to use module interface
- [ ] Tests pass with new import structure
- [ ] Documentation updated with new paths
- [ ] No references to deprecated `scripts/` directory

## Additional Resources

- Module API Documentation: `src/digitalmodel/modules/marine_analysis/README.md`
- CLI Help: `python -m digitalmodel.marine_ops.marine_analysis --help`
- Examples: `src/digitalmodel/modules/marine_analysis/analysis/hydro_usage_example.py`

---

**Last Updated:** October 3, 2025
**Version:** 2.1.0
