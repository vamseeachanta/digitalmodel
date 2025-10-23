# Enhanced Modular Input File Validation System

**Version:** 1.0.0
**Date:** 2025-01-23
**Location:** `src/digitalmodel/modules/orcaflex/modular_input_validation/`

## Overview

This document specifies the enhanced validation system for OrcaFlex modular YAML input files with three validation levels, multiple output formats, and physical consistency checks.

## Architecture

### Module Structure

```
src/digitalmodel/modules/orcaflex/modular_input_validation/
├── __init__.py                    # Public API
├── models.py                      # Data models (✅ CREATED)
├── config.py                      # Configuration management
├── validator.py                   # Main validator orchestrator
├── level_1_yaml.py               # Level 1: YAML syntax validation
├── level_2_orcaflex.py           # Level 2: OrcaFlex API validation
├── level_3_physical.py           # Level 3: Physical consistency
├── data_loader.py                # CALM buoy data CSV loader
├── reporters/
│   ├── __init__.py
│   ├── console.py                # Loguru color-coded console output
│   ├── csv_reporter.py           # CSV summary export
│   ├── markdown_reporter.py      # Markdown report generation
│   └── html_reporter.py          # Interactive HTML report with plots
├── cli.py                        # Command-line interface
└── utils.py                      # Utility functions
```

## Validation Levels

### Level 1: YAML Syntax Validation

**Purpose:** Validate YAML structure and include file references

**Checks:**
- YAML syntax validity (yaml.safe_load)
- File existence
- Includefile reference resolution
- Section structure validation
- Module dependency graph

**Status Output:** PASS/WARN/FAIL

**Migrated from:** `specs/modules/orcaflex/modular-input-file/scripts/validate_modules.py`

### Level 2: OrcaFlex API Validation

**Purpose:** Load files via OrcaFlex API and run static analysis

**Checks:**
- OrcaFlex software availability detection
- OrcaFlex version detection
- File loading via OrcFxAPI
- Static analysis execution
- Convergence verification
- Warning/error collection

**Status Output:**
- `orcaflex_available`: YES/NO/UNKNOWN
- `orcaflex_version`: e.g., "11.5e"
- `orcaflex_loadable`: True/False
- `static_converged`: True/False
- `availability_comment`: "Software available", "License unavailable", etc.

**Graceful Degradation:**
- If OrcaFlex not available: Status = SKIPPED, comment explains why
- If license unavailable: Status = UNKNOWN, comment explains
- Continue to Level 3 regardless

### Level 3: Physical Consistency Validation

**Purpose:** Validate parameters against CALM buoy data ranges

**Data Sources:**

1. **Generic Ranges** (`data/raw/calm_buoy/generic_range/`)
   - `hull_geometry_ranges.csv` - Buoy dimensions
   - `metocean_design_ranges.csv` - Environmental conditions
   - `mooring_capacity_ranges.csv` - Line capacities

2. **Project Specific** (`data/results/calm_buoy/project_specific/`)
   - `environmental_conditions.csv` - Target env conditions
   - `mooring_line_properties.csv` - Target line properties

**Validation Rules:**

1. **Range Validation** - Warn if outside generic_range bounds
2. **Project Validation** - Flag if differs from project_specific values by >10%
3. **Tolerance:** ±10%, document actual difference percentage

**Severity Levels:**
- `INFO`: Within all ranges and tolerances
- `WARNING`: Outside generic range OR differs from project by 5-10%
- `CRITICAL`: Differs from project by >10%

**Parameters to Validate:**

From YAML → Data Mappings:

| YAML Parameter | Data Source | CSV Field |
|----------------|-------------|-----------|
| VesselTypes → Draught | hull_geometry_ranges.csv | hull_draft |
| Environment → WaterDepth | metocean_design_ranges.csv | (derived) |
| Environment → WaveHs | metocean_design_ranges.csv | hs_min/hs_max |
| Environment → WaveTz | metocean_design_ranges.csv | tp_min/tp_max |
| LineTypes → MBL | mooring_capacity_ranges.csv | *_capacity |
| Lines → Length | mooring_line_properties.csv | length |

## Directory Structure

### Output Organization

```
reports/validation/calm_buoy/           # Human-readable summaries
├── validation_summary_YYYYMMDD.csv
├── validation_report_YYYYMMDD.md
└── validation_report_YYYYMMDD.html

results/validation/calm_buoy/           # Detailed analysis data
├── detailed_issues_YYYYMMDD.json
├── parameter_comparison_YYYYMMDD.csv
├── orcaflex_output_YYYYMMDD.txt
└── validation_metrics_YYYYMMDD.json
```

## Output Formats

### 1. Console Output (Loguru)

**Color Coding:**
- 🟢 GREEN: PASS, INFO
- 🟡 YELLOW: WARN, WARNING
- 🔴 RED: FAIL, CRITICAL
- ⚪ WHITE: UNKNOWN, SKIPPED

**Format:**
```
[2025-01-23 14:30:15] INFO  | Level 1 YAML Syntax ✓ PASS
[2025-01-23 14:30:16] WARN  | Level 2 OrcaFlex API ⚠ SKIPPED - Software not available
[2025-01-23 14:30:18] INFO  | Level 3 Physical Consistency ✓ PASS with 3 warnings

VALIDATION SUMMARY
==================
File: calm_buoy_base.yml
Overall Status: ⚠ PASS WITH WARNINGS

Issues:
  🟡 WARNING | Wave height 13.78m exceeds generic range max 11.0m (+25.3%)
  🟡 WARNING | Line L1 length 320m differs from project spec 330m (-3.0%)
```

### 2. CSV Summary

**Location:** `reports/validation/calm_buoy/`

**Filename:** `validation_summary_YYYYMMDD_HHMMSS.csv`

**Schema:**
```csv
timestamp,file_name,validation_level,status,yaml_valid,orcaflex_available,orcaflex_version,orcaflex_loadable,static_converged,physical_checks,physical_warnings,critical_issues,error_messages,notes
```

**Example:**
```csv
2025-01-23T14:30:15,calm_buoy_base.yml,all,pass_with_warnings,true,no,unknown,false,false,12,3,0,"OrcaFlex not installed","Wave Hs exceeds range; Line lengths within tolerance"
```

### 3. Markdown Report

**Location:** `reports/validation/calm_buoy/`

**Filename:** `validation_report_YYYYMMDD_HHMMSS.md`

**Structure:**
```markdown
# Validation Report - calm_buoy_base.yml

**Date:** 2025-01-23 14:30:15
**Overall Status:** ⚠ PASS WITH WARNINGS

## Level 1: YAML Syntax ✓ PASS
- Valid YAML: ✓
- All includes resolved: ✓
- Total modules: 24

## Level 2: OrcaFlex API ⚠ SKIPPED
- Software available: NO
- Reason: OrcaFlex not installed on system

## Level 3: Physical Consistency ⚠ PASS (3 warnings)

### Parameter Validation

| Parameter | Actual | Range | Project | Status | Diff% |
|-----------|--------|-------|---------|--------|-------|
| Wave Hs | 13.78m | 6.0-9.5m | 9.0m | ⚠ WARNING | +53% |
| Line L1 Length | 320m | - | 330m | 🟡 INFO | -3.0% |
```

### 4. HTML Interactive Report

**Location:** `reports/validation/calm_buoy/`

**Filename:** `validation_report_YYYYMMDD_HHMMSS.html`

### 5. Detailed Results (for analysis/debugging)

**Location:** `results/validation/calm_buoy/`

**Files Generated:**
- `detailed_issues_YYYYMMDD_HHMMSS.json` - Full issue data with metadata
- `parameter_comparison_YYYYMMDD_HHMMSS.csv` - All parameter comparisons
- `orcaflex_output_YYYYMMDD_HHMMSS.txt` - Raw OrcaFlex analysis output (if available)
- `validation_metrics_YYYYMMDD_HHMMSS.json` - Performance and statistics

**Features:**
- Interactive Plotly charts:
  - Parameter range compliance bar chart
  - Severity distribution pie chart
  - Validation level status dashboard
- Collapsible sections for each level
- Filterable issue table
- Export functionality (PDF, PNG)
- Responsive design

**Technology:**
- Plotly for interactive visualizations
- Bootstrap for responsive layout
- DataTables for issue filtering/sorting

## Implementation Phases

### Phase 1: Core Structure (Current Session)
- [x] Create module structure
- [x] Define data models
- [ ] Implement configuration system
- [ ] Implement Level 1 validator (migrate existing)
- [ ] Implement console reporter with loguru

### Phase 2: OrcaFlex Integration (Next Session)
- [ ] Implement Level 2 validator
- [ ] Add OrcaFlex graceful fallback
- [ ] Add version detection
- [ ] NOTE: Static analysis output will be defined later

### Phase 3: Physical Validation (Next Session)
- [ ] Implement data loader for CALM buoy CSVs
- [ ] Implement Level 3 validator
- [ ] Add range checking logic
- [ ] Add project comparison logic
- [ ] Implement severity determination

### Phase 4: Reporting (Next Session)
- [ ] Implement CSV reporter
- [ ] Implement Markdown reporter
- [ ] Implement HTML reporter with Plotly

### Phase 5: CLI & Testing (Next Session)
- [ ] Implement CLI with argparse
- [ ] Add comprehensive tests
- [ ] Integration testing
- [ ] Documentation

## Dependencies

**Required:**
- `pyyaml` - YAML parsing
- `loguru` - Color-coded logging
- `pandas` - CSV data handling
- `plotly` - Interactive visualizations
- `jinja2` - HTML template rendering

**Optional:**
- `OrcFxAPI` - OrcaFlex integration (graceful fallback if missing)

## Usage Examples

### Basic Validation
```bash
python -m digitalmodel.modules.orcaflex.modular_input_validation \
    specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml
```

### With All Reports
```bash
python -m digitalmodel.modules.orcaflex.modular_input_validation \
    --report-all \
    --reports-dir reports/validation/calm_buoy/ \
    --results-dir results/validation/calm_buoy/ \
    calm_buoy_base.yml
```

### Programmatic Usage
```python
from digitalmodel.modules.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)

config = ValidationConfig(
    tolerance_percent=10.0,
    enable_orcaflex=True,
    calm_buoy_data_dir='data/'
)

validator = ModularInputValidator(config)
result = validator.validate_all('path/to/file.yml')

if result.overall_status == ValidationStatus.PASS:
    print("✓ Validation passed!")
```

## Future Enhancements (Later Sessions)

1. **Static Analysis Output Processing**
   - Line tensions extraction
   - Vessel equilibrium verification
   - Post-processing metrics

2. **Batch Validation**
   - Validate multiple files in parallel
   - Comparative reports across configurations

3. **CI/CD Integration**
   - GitHub Actions workflow
   - Pre-commit hooks
   - Automated PR validation

4. **Advanced Reporting**
   - Trend analysis across validation runs
   - Regression detection
   - Performance dashboards

## Questions Resolved

✅ All clarifications from user have been incorporated:
- Graceful OrcaFlex handling
- Loguru color-coded output
- CSV/MD/HTML reports
- Physical consistency with CALM buoy data
- ±10% tolerance with actual difference tracking
- Severity grading (INFO/WARNING/CRITICAL)
- Location in `src/digitalmodel/modules/orcaflex/modular_input_validation/`
- Integration with existing OrcaFlex modules

## Next Steps

**For Current Session:**
1. Get approval on this specification
2. Continue implementation: config.py, validator.py, level_1_yaml.py
3. Implement basic console output with loguru

**For Future Sessions:**
4. Complete Level 2 & 3 implementation
5. Implement all reporters
6. Add CLI and tests

---

**Status:** 🟡 In Progress - Awaiting approval to continue implementation
