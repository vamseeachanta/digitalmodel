# Validation Framework Best Practices

**Purpose:** Reusable validation patterns for modular input files across asset categories
**Version:** 1.0.0
**Date:** 2025-01-23
**Reference Implementation:** OrcaFlex CALM Buoy Modular Input Validation

---

## Overview

This document defines standardized validation practices for modular input file systems across various asset categories (CALM buoys, FPSOs, pipelines, risers, etc.). The framework provides a consistent three-level validation approach with multiple output formats and physical consistency checks.

## Core Validation Architecture

### Three-Level Validation Pattern

All asset categories should implement validation at three progressive levels:

```
┌─────────────────────────────────────────────────────────────┐
│ Level 1: Syntax & Structure Validation                     │
│ - File format validation (YAML, JSON, etc.)                │
│ - Include/reference resolution                             │
│ - Schema compliance                                         │
│ - Dependency graph validation                              │
│ Status: PASS/WARN/FAIL                                      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Level 2: Software API Validation                           │
│ - Software availability detection (graceful)                │
│ - Version compatibility check                               │
│ - File loading via API                                      │
│ - Analysis execution (static/dynamic)                       │
│ - Convergence verification                                  │
│ Status: PASS/WARN/FAIL/SKIPPED/UNKNOWN                     │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Level 3: Physical Consistency Validation                   │
│ - Parameter range checking (generic bounds)                │
│ - Project-specific comparison                               │
│ - Engineering standards compliance                          │
│ - Physical plausibility checks                              │
│ Status: PASS/WARN/FAIL with severity grading               │
└─────────────────────────────────────────────────────────────┘
```

### Key Principles

1. **Progressive Validation**: Each level builds on the previous
2. **Graceful Degradation**: Levels can be skipped if dependencies unavailable
3. **All Levels Run**: Don't stop on first failure - collect all issues
4. **Severity Grading**: INFO → WARNING → CRITICAL
5. **Multiple Outputs**: Console, CSV, Markdown, HTML

## Module Structure Pattern

### Standard Directory Layout

```
src/digitalmodel/modules/<software>/<asset_category>_validation/
├── __init__.py                    # Public API
├── models.py                      # Data models (enums, dataclasses)
├── config.py                      # Configuration management
├── validator.py                   # Main orchestrator
├── level_1_syntax.py             # Level 1 implementation
├── level_2_api.py                # Level 2 implementation
├── level_3_physical.py           # Level 3 implementation
├── data_loader.py                # Reference data CSV loader
├── reporters/
│   ├── __init__.py
│   ├── console.py                # Loguru color-coded output
│   ├── csv_reporter.py           # CSV summary
│   ├── markdown_reporter.py      # Markdown report
│   └── html_reporter.py          # Interactive HTML
├── cli.py                        # Command-line interface
└── utils.py                      # Utilities
```

### Integration with Existing Modules

**Pattern:** Validation modules should integrate with existing analysis modules, not replace them.

```python
# Use existing model interfaces
from digitalmodel.modules.orcaflex.core.model_interface import ModelInterface
from digitalmodel.modules.orcaflex.core.logging_config import OrcaFlexLogger

# Leverage existing API abstraction
from digitalmodel.modules.orcaflex.core.exceptions import (
    OrcaFlexError, ModelError, LicenseError
)
```

**Benefits:**
- Consistent error handling
- Reuse logging infrastructure
- Maintain existing graceful fallback patterns
- Leverage proven abstractions

## Data Model Standards

### Required Enums

```python
class ValidationLevel(Enum):
    """Progressive validation levels."""
    LEVEL_1_SYNTAX = "level_1_syntax"
    LEVEL_2_API = "level_2_<software>_api"
    LEVEL_3_PHYSICAL = "level_3_physical_consistency"

class ValidationStatus(Enum):
    """Overall validation status."""
    PASS = "pass"
    WARN = "warn"
    FAIL = "fail"
    UNKNOWN = "unknown"
    SKIPPED = "skipped"

class Severity(Enum):
    """Issue severity levels."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"

class SoftwareAvailability(Enum):
    """Software detection result."""
    YES = "yes"
    NO = "no"
    UNKNOWN = "unknown"
```

### Standard Result Structure

```python
@dataclass
class ValidationResult:
    """Complete validation results."""
    file_path: Path
    timestamp: datetime

    # Level results
    level_1: Optional[Level1Result] = None
    level_2: Optional[Level2Result] = None
    level_3: Optional[Level3Result] = None

    # Overall
    overall_status: ValidationStatus = ValidationStatus.UNKNOWN
    issues: List[ValidationIssue] = field(default_factory=list)
    validation_time_seconds: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary for reports."""
        ...
```

## Physical Consistency Validation

### Directory Structure

### Reference Data Organization

**Pattern:** Organize reference data by scope:

```
data/
├── raw/<asset_category>/generic_range/
│   ├── geometry_ranges.csv
│   ├── environmental_ranges.csv
│   └── capacity_ranges.csv
├── processed/<asset_category>/mature_design/
│   ├── coefficients.csv
│   └── components.csv
└── results/<asset_category>/project_specific/
    ├── conditions.csv
    └── properties.csv
```

### Reports and Results Organization

**Pattern:** Separate human-readable reports from detailed results:

```
reports/
└── validation/<asset_category>/
    ├── validation_summary_YYYYMMDD_HHMMSS.csv
    ├── validation_report_YYYYMMDD_HHMMSS.md
    └── validation_report_YYYYMMDD_HHMMSS.html

results/
└── validation/<asset_category>/
    ├── detailed_issues_YYYYMMDD_HHMMSS.json
    ├── parameter_comparison_YYYYMMDD_HHMMSS.csv
    ├── orcaflex_output_YYYYMMDD_HHMMSS.txt
    └── validation_metrics_YYYYMMDD_HHMMSS.json
```

**Distinction:**
- **reports/**: Human-readable summaries for stakeholders (CSV summaries, Markdown, HTML)
- **results/**: Detailed validation data for analysis/debugging (JSON, raw outputs, full parameter lists)

### CSV Reference Data Format

**Standard Schema:**

```csv
parameter,min_value,max_value,reference_basis,notes
hull_diameter,8.0,20.0,"OCIMF Guidelines","Global installed systems envelope"
wave_hs,1.5,11.0,"ISO 19901-1","Operational to survival range"
```

**Key Fields:**
- `parameter`: Canonical name matching software input
- `min_value`/`max_value`: Generic acceptable range
- `reference_basis`: Engineering standard or data source
- `notes`: Context and assumptions

### Validation Rules

```python
def validate_parameter(actual, param_name, reference_data, project_data):
    """
    Standard validation logic pattern.

    Returns:
        PhysicalConsistencyCheck with:
        - severity: INFO/WARNING/CRITICAL
        - within_range: bool (generic range check)
        - matches_project: bool (project comparison)
        - difference_percent: float
    """
    # 1. Range check against generic bounds
    if actual < min_value or actual > max_value:
        severity = Severity.WARNING
        within_range = False

    # 2. Project comparison with tolerance
    if project_value:
        diff_pct = abs(actual - project_value) / project_value * 100

        if diff_pct > 10.0:
            severity = Severity.CRITICAL
            matches_project = False
        elif diff_pct > 5.0:
            severity = Severity.WARNING

    # 3. Document actual difference
    return PhysicalConsistencyCheck(
        parameter=param_name,
        actual_value=actual,
        difference_percent=diff_pct,
        severity=severity,
        ...
    )
```

**Tolerance Guidelines:**
- Default: ±10%
- Document actual difference percentage
- Allow configuration per parameter type

## Output Format Standards

### 1. Console Output (Loguru)

**Color Coding Standard:**

```python
from loguru import logger

# Configure colors
logger.level("INFO", color="<green>")
logger.level("WARNING", color="<yellow>")
logger.level("CRITICAL", color="<red>")

# Emoji indicators
STATUS_EMOJI = {
    ValidationStatus.PASS: "✓",
    ValidationStatus.WARN: "⚠",
    ValidationStatus.FAIL: "✗",
    ValidationStatus.UNKNOWN: "?",
    ValidationStatus.SKIPPED: "⊘"
}

SEVERITY_EMOJI = {
    Severity.INFO: "🟢",
    Severity.WARNING: "🟡",
    Severity.CRITICAL: "🔴"
}
```

**Output Format:**

```
[2025-01-23 14:30:15] INFO  | Level 1 Syntax Validation ✓ PASS
[2025-01-23 14:30:16] WARN  | Level 2 Software API ⚠ SKIPPED - Software not installed
[2025-01-23 14:30:18] INFO  | Level 3 Physical Consistency ✓ PASS with 3 warnings

VALIDATION SUMMARY
==================
File: <asset>_configuration.yml
Overall Status: ⚠ PASS WITH WARNINGS

Issues by Severity:
  🔴 CRITICAL: 0
  🟡 WARNING:  3
  🟢 INFO:     12

Top Issues:
  🟡 WARNING | Parameter 'wave_hs' exceeds generic range (+25%)
  🟡 WARNING | Line length differs from project spec (-3%)
```

### 2. CSV Summary Format

**Standard Schema:**

```csv
timestamp,file_name,asset_category,validation_level,status,
syntax_valid,software_available,software_version,software_loadable,
analysis_converged,physical_checks,physical_warnings,critical_issues,
validation_time_sec,error_messages,notes
```

**Location Pattern:** `reports/validation/<asset_category>/validation_summary_YYYYMMDD_HHMMSS.csv`

### 3. Markdown Report Format

**Standard Structure:**

```markdown
# Validation Report - <Asset Category> - <File>

**Date:** YYYY-MM-DD HH:MM:SS
**Overall Status:** [EMOJI] STATUS

## Validation Summary

| Level | Status | Details |
|-------|--------|---------|
| Level 1: Syntax | ✓ PASS | Valid structure, all includes resolved |
| Level 2: Software API | ⊘ SKIPPED | Software not available |
| Level 3: Physical | ⚠ PASS | 3 warnings |

## Level 1: Syntax & Structure [STATUS]

- File format: ✓ Valid
- Includes resolved: ✓ All found
- Total modules: X

## Level 2: Software API [STATUS]

- Software: [YES/NO/UNKNOWN]
- Version: X.X
- Loadable: [YES/NO]
- Analysis: [CONVERGED/FAILED/SKIPPED]

## Level 3: Physical Consistency [STATUS]

### Parameter Validation

| Parameter | Actual | Generic Range | Project Value | Diff% | Status |
|-----------|--------|---------------|---------------|-------|--------|
| param_1   | X.XX   | min-max       | X.XX          | ±X%   | [EMOJI] |

### Issues

[Severity] - [Message]
  - Parameter: X
  - Expected: Y
  - Actual: Z
  - Difference: ±X%
  - Suggestion: ...
```

### 4. HTML Interactive Report

**Required Features:**
- Bootstrap responsive layout
- Plotly interactive charts:
  - Parameter compliance bar chart
  - Severity distribution pie chart
  - Validation timeline
- DataTables for filterable issue table
- Collapsible sections per level
- Export buttons (PDF, PNG, CSV)

**Technology Stack:**
```python
dependencies = [
    "plotly>=5.0.0",      # Interactive charts
    "jinja2>=3.0.0",      # Template rendering
    "pandas>=1.3.0"       # Data manipulation
]
```

## Asset Category Adaptation Guide

### Step-by-Step Implementation

**1. Identify Software API**

```python
# Example: OrcaFlex
try:
    import OrcFxAPI
    SOFTWARE_AVAILABLE = True
except ImportError:
    OrcFxAPI = None
    SOFTWARE_AVAILABLE = False
```

**2. Define Parameter Mappings**

Create mapping between software input and reference data:

```python
PARAMETER_MAPPINGS = {
    # Software path → Reference CSV field
    'Environment.WaveHs': ('metocean_ranges.csv', 'hs_max'),
    'VesselTypes.Draught': ('geometry_ranges.csv', 'hull_draft'),
    'LineTypes.MBL': ('capacity_ranges.csv', 'mbl_max'),
}
```

**3. Collect Reference Data**

For each asset category, compile:

```
data/raw/<asset_category>/generic_range/
  - Industry standard ranges
  - Regulatory limits
  - Physical bounds

data/processed/<asset_category>/mature_design/
  - Proven configurations
  - Typical values
  - Design envelopes

data/results/<asset_category>/project_specific/
  - Current project targets
  - Site-specific conditions
  - Client requirements
```

**4. Implement Level Validators**

Follow the pattern:

```python
class Level1SyntaxValidator:
    """Asset-category-agnostic syntax validation."""
    def validate(self, file_path: Path) -> Level1Result:
        # YAML/JSON parsing
        # Include resolution
        # Schema validation
        ...

class Level2APIValidator:
    """Software-specific API validation."""
    def validate(self, file_path: Path) -> Level2Result:
        # Detect software
        # Load via API
        # Run analysis
        # Collect warnings/errors
        ...

class Level3PhysicalValidator:
    """Asset-category-specific physical validation."""
    def __init__(self, reference_data_dir: Path):
        self.data = load_reference_data(reference_data_dir)

    def validate(self, parsed_input: Dict) -> Level3Result:
        # Extract parameters
        # Compare to ranges
        # Compare to project data
        # Grade severity
        ...
```

**5. Configure CLI**

```python
@click.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--asset-category', required=True,
              help='Asset category (calm_buoy, fpso, pipeline, etc.)')
@click.option('--reference-data-dir', type=click.Path(),
              help='Directory containing reference data CSVs')
@click.option('--report-all/--no-reports', default=True)
@click.option('--output-dir', type=click.Path())
def validate(input_file, asset_category, reference_data_dir, report_all, output_dir):
    """Validate modular input file for asset category."""
    ...
```

## Asset Category Examples

### CALM Buoy (Reference Implementation)

**Software:** OrcaFlex
**Reference Data:**
- `hull_geometry_ranges.csv` - Diameter, draft, freeboard
- `metocean_design_ranges.csv` - Wave, current, wind
- `mooring_capacity_ranges.csv` - Line capacities, safety factors

**Key Parameters:**
- Hull geometry (diameter, draft)
- Environmental conditions (Hs, Tp, current)
- Mooring line properties (length, MBL, pretension)

### FPSO (Future Implementation)

**Software:** OrcaFlex, AQWA, Moses
**Reference Data:**
- `hull_dimensions_ranges.csv` - Length, beam, depth
- `turret_mooring_ranges.csv` - Turret offset, swivel capacity
- `process_facilities_ranges.csv` - Topside weights, CoG

**Key Parameters:**
- Hull principal dimensions
- Mooring system (lines, anchors, turret)
- Topside configuration (weight, CoG, windage)

### Subsea Pipeline (Future Implementation)

**Software:** ORCAFLEX, OFFPIPE
**Reference Data:**
- `pipe_dimensions_ranges.csv` - OD, WT, material
- `installation_ranges.csv` - Lay tension, curvature
- `operating_conditions_ranges.csv` - Pressure, temperature

**Key Parameters:**
- Pipe properties (OD, WT, material, coating)
- Installation parameters (lay tension, touchdown)
- Operating conditions (pressure, temperature, flow)

### Riser System (Future Implementation)

**Software:** OrcaFlex, Flexcom
**Reference Data:**
- `riser_config_ranges.csv` - Riser type, length, diameter
- `viv_suppression_ranges.csv` - Strake coverage, helical pitch
- `fatigue_criteria_ranges.csv` - SCF, DFF, target life

**Key Parameters:**
- Riser configuration (type, diameter, length)
- VIV suppression (strakes, fairings)
- Fatigue design (SCF, stress ranges, life target)

## CI/CD Integration Pattern

### Pre-Commit Hook

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: validate-orcaflex-input
        name: Validate OrcaFlex Modular Input Files
        entry: python -m digitalmodel.modules.orcaflex.modular_input_validation
        language: python
        files: '\.(yml|yaml)$'
        pass_filenames: true
```

### GitHub Actions Workflow

```yaml
# .github/workflows/validate-input-files.yml
name: Validate Modular Input Files

on:
  pull_request:
    paths:
      - 'specs/modules/**/*.yml'
      - 'docs/modules/**/*.yml'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: |
          pip install -e .[validation]

      - name: Run validation
        run: |
          python -m digitalmodel.modules.orcaflex.modular_input_validation \
            --report-all \
            --output-dir validation_results/ \
            specs/modules/orcaflex/modular-input-file/output/*.yml

      - name: Upload reports
        uses: actions/upload-artifact@v3
        with:
          name: validation-reports
          path: validation_results/
```

## Performance Considerations

### Large File Handling

```python
# Stream large YAML files
def validate_large_file(file_path: Path, chunk_size: int = 1000):
    """Validate large files in chunks."""
    with open(file_path, 'r') as f:
        for chunk in yaml.load_all(f):  # Multi-document YAML
            validate_chunk(chunk)
```

### Parallel Validation

```python
from concurrent.futures import ProcessPoolExecutor

def validate_batch(file_paths: List[Path], max_workers: int = 4):
    """Validate multiple files in parallel."""
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        results = executor.map(validate_single_file, file_paths)
    return list(results)
```

### Caching Reference Data

```python
from functools import lru_cache

@lru_cache(maxsize=10)
def load_reference_data(data_dir: Path) -> Dict[str, pd.DataFrame]:
    """Cache reference data to avoid repeated loading."""
    return {
        'geometry': pd.read_csv(data_dir / 'geometry_ranges.csv'),
        'metocean': pd.read_csv(data_dir / 'metocean_ranges.csv'),
        ...
    }
```

## Testing Standards

### Unit Tests

```python
def test_level1_valid_yaml():
    """Test Level 1 with valid YAML file."""
    validator = Level1SyntaxValidator()
    result = validator.validate(VALID_YAML_PATH)

    assert result.yaml_valid is True
    assert result.status == ValidationStatus.PASS

def test_level2_software_unavailable():
    """Test Level 2 gracefully skips when software missing."""
    with mock.patch('OrcFxAPI', None):
        validator = Level2APIValidator()
        result = validator.validate(TEST_FILE)

        assert result.orcaflex_available == OrcaFlexAvailability.NO
        assert result.status == ValidationStatus.SKIPPED

def test_level3_parameter_out_of_range():
    """Test Level 3 detects out-of-range parameter."""
    validator = Level3PhysicalValidator(TEST_DATA_DIR)
    result = validator.validate({'wave_hs': 15.0})  # Exceeds max

    assert result.warnings > 0
    assert any(c.severity == Severity.WARNING for c in result.consistency_checks)
```

### Integration Tests

```python
def test_full_validation_pipeline():
    """Test complete validation from file to reports."""
    validator = ModularInputValidator(
        config=ValidationConfig(
            reference_data_dir=TEST_DATA_DIR
        )
    )

    result = validator.validate_all(TEST_INPUT_FILE)

    # All levels should run
    assert result.level_1 is not None
    assert result.level_2 is not None
    assert result.level_3 is not None

    # Reports should generate
    csv_path = validator.generate_csv_report(result)
    md_path = validator.generate_markdown_report(result)
    html_path = validator.generate_html_report(result)

    assert csv_path.exists()
    assert md_path.exists()
    assert html_path.exists()
```

## Maintenance & Updates

### Version Control for Reference Data

```bash
# Tag reference data releases
git tag -a calm-buoy-ref-data-v1.0 -m "Initial CALM buoy reference data"

# Track data provenance
data/
└── <asset_category>/
    ├── CHANGELOG.md          # Document data updates
    ├── SOURCES.md            # Reference sources and dates
    └── VERSION               # Semantic version
```

### Reference Data Update Process

1. **Review:** Engineering standards/regulations updated
2. **Update:** Modify CSV reference data files
3. **Document:** Update CHANGELOG.md and SOURCES.md
4. **Validate:** Run validation on historical files to detect impacts
5. **Version:** Increment VERSION and tag in git
6. **Notify:** Update dependent projects

## Troubleshooting Guide

### Common Issues

**Issue:** Level 2 always shows SKIPPED
```
Cause: Software not installed or not in PATH
Solution:
  - Install software (e.g., OrcaFlex)
  - Add to system PATH
  - Or accept SKIPPED status (validation continues)
```

**Issue:** All parameters show WARNING
```
Cause: Reference data CSVs not found or incorrectly formatted
Solution:
  - Verify reference_data_dir path
  - Check CSV schema matches expected format
  - Review data_loader.py error messages
```

**Issue:** HTML report doesn't display charts
```
Cause: Plotly not installed or version mismatch
Solution:
  - pip install plotly>=5.0.0
  - Verify jinja2 templates render correctly
```

## Summary

This validation framework provides:

✅ **Consistency** - Same pattern across all asset categories
✅ **Completeness** - Three levels cover syntax, software, physics
✅ **Flexibility** - Graceful degradation when dependencies missing
✅ **Clarity** - Multiple output formats for different audiences
✅ **Maintainability** - Modular architecture, reusable components
✅ **Extensibility** - Easy to add new asset categories

**Next Steps for Each Asset Category:**

1. Identify software API and establish graceful fallback
2. Compile reference data CSVs (generic, mature, project-specific)
3. Create parameter mappings between software input and reference data
4. Implement three level validators following patterns
5. Configure reporters (console, CSV, MD, HTML)
6. Add CLI interface
7. Write comprehensive tests
8. Integrate with CI/CD

---

**Reference Implementation:**
`src/digitalmodel/modules/orcaflex/modular_input_validation/` (CALM Buoy)

**Specification:**
`specs/modules/orcaflex/modular-input-file/ENHANCED_VALIDATION_SPEC.md`

**Status:** 🟢 Active - Ready for replication across asset categories
