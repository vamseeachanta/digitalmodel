# digitalmodel - Implementation Checklist

> **Actionable tasks with commands and scripts ready to execute**

---

## 🔥 PRIORITY 0: Critical (Do First - 4 hours)

### ✅ Task 1.1: Fix File Naming Issues

**Why**: 20+ files with spaces/commas break Unix tools, CI/CD pipelines, and imports.

**Affected Files**:
```
PlateBuckling_Plots/plateBucklingCal_Bi-axial Plot.py
legacy/apirp2rd/COD/Burst,collapse code/
legacy/nl_stress/Stress-Strain Curves Calculation.py
```

**Action**: Create and run automated renaming script

**Script**: Create `scripts/fix_filenames.sh`
```bash
#!/bin/bash
# Fix filenames with spaces and commas

set -e

echo "🔧 Fixing filenames with spaces and commas..."

# Count files before
FILES_BEFORE=$(find src -name "* *" -o -name "*,*" | wc -l)
echo "Files to rename: $FILES_BEFORE"

# Create backup
echo "Creating backup..."
cp -r src src_backup_$(date +%Y%m%d_%H%M%S)

# Rename files with spaces
find src -depth -name "* *" -type f | while read -r file; do
    newfile="${file// /_}"
    echo "  Renaming: $file → $newfile"
    mv "$file" "$newfile"
done

# Rename directories with spaces
find src -depth -name "* *" -type d | while read -r dir; do
    newdir="${dir// /_}"
    echo "  Renaming dir: $dir → $newdir"
    mv "$dir" "$newdir"
done

# Rename files with commas
find src -depth -name "*,*" -type f | while read -r file; do
    newfile="${file//,/_}"
    echo "  Renaming: $file → $newfile"
    mv "$file" "$newfile"
done

# Rename directories with commas
find src -depth -name "*,*" -type d | while read -r dir; do
    newdir="${dir//,/_}"
    echo "  Renaming dir: $dir → $newdir"
    mv "$dir" "$newdir"
done

# Count files after
FILES_AFTER=$(find src -name "* *" -o -name "*,*" | wc -l)
echo "✅ Complete! Remaining problematic files: $FILES_AFTER"

# TODO: Update imports (use IDE or run separate script)
echo "⚠️  NEXT: Update imports in affected files"
```

**Execute**:
```bash
cd digitalmodel
chmod +x scripts/fix_filenames.sh
./scripts/fix_filenames.sh
```

**Verify**:
```bash
# Should return 0
find src -name "* *" -o -name "*,*" | wc -l
```

**Update Imports**: Use IDE (VS Code, PyCharm) to:
1. Find all references to renamed files
2. Update import statements
3. Run tests to verify

**Estimated Time**: 2 hours (1hr script, 1hr import updates)

---

### ✅ Task 1.2: Create Technical Debt Tracking

**Why**: 107 TODO/FIXME markers need visibility and prioritization.

**Action**: Create `TECHNICAL_DEBT.md`

**Script**: Create `scripts/extract_technical_debt.sh`
```bash
#!/bin/bash
# Extract all technical debt markers

echo "# Technical Debt Inventory" > TECHNICAL_DEBT.md
echo "" >> TECHNICAL_DEBT.md
echo "Generated: $(date)" >> TECHNICAL_DEBT.md
echo "Total Markers: $(grep -r "TODO\|FIXME\|XXX\|HACK" src --include="*.py" | wc -l)" >> TECHNICAL_DEBT.md
echo "" >> TECHNICAL_DEBT.md

echo "## By Priority" >> TECHNICAL_DEBT.md
echo "" >> TECHNICAL_DEBT.md

echo "### CRITICAL (FIXME)" >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md
grep -rn "FIXME" src --include="*.py" | head -20 >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md
echo "" >> TECHNICAL_DEBT.md

echo "### HIGH (XXX, HACK)" >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md
grep -rn "XXX\|HACK" src --include="*.py" | head -20 >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md
echo "" >> TECHNICAL_DEBT.md

echo "### MEDIUM (TODO)" >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md
grep -rn "TODO" src --include="*.py" | head -30 >> TECHNICAL_DEBT.md
echo "\`\`\`" >> TECHNICAL_DEBT.md

echo "✅ Created TECHNICAL_DEBT.md"
```

**Execute**:
```bash
cd digitalmodel
chmod +x scripts/extract_technical_debt.sh
./scripts/extract_technical_debt.sh
```

**Review**: Open `TECHNICAL_DEBT.md` and categorize by module

**Estimated Time**: 2 hours (1hr extraction, 1hr categorization)

---

## ⚡ PRIORITY 1: Foundation (Week 1-2)

### ✅ Task 2.1: Centralized Configuration System

**Why**: Enables parametric analysis, consistent settings across modules.

**Action**: Create `src/digitalmodel/config/` package

**Files to Create**:

**1. `src/digitalmodel/config/__init__.py`**:
```python
"""
Centralized configuration management for digitalmodel.

Usage:
    from digitalmodel.config import get_settings

    settings = get_settings()
    print(settings.data_dir)
"""

from .settings import GlobalSettings, get_settings, override_settings

__all__ = ['GlobalSettings', 'get_settings', 'override_settings']
```

**2. `src/digitalmodel/config/settings.py`**:
```python
"""Global configuration settings using Pydantic"""

from pydantic import BaseSettings, Field, validator
from pathlib import Path
from typing import Optional
import os


class GlobalSettings(BaseSettings):
    """
    Global digitalmodel configuration.

    Environment variables override defaults (prefix: DM_)
    Example: DM_DATA_DIR=/path/to/data
    """

    # Paths
    data_dir: Path = Field(
        default=Path("./data"),
        env="DM_DATA_DIR",
        description="Root directory for data files"
    )
    output_dir: Path = Field(
        default=Path("./outputs"),
        env="DM_OUTPUT_DIR",
        description="Root directory for output files"
    )
    report_dir: Path = Field(
        default=Path("./reports"),
        env="DM_REPORT_DIR",
        description="Root directory for reports"
    )

    # Analysis Defaults
    default_units: str = Field(
        default="SI",
        env="DM_UNITS",
        description="Default unit system (SI, Imperial)"
    )
    safety_factor: float = Field(
        default=1.5,
        env="DM_SAFETY_FACTOR",
        description="Default safety factor"
    )

    # OrcaFlex Settings
    orcaflex_license: Optional[str] = Field(
        default=None,
        env="ORCAFLEX_LICENSE",
        description="OrcaFlex license server"
    )
    orcaflex_workers: int = Field(
        default=30,
        env="ORCAFLEX_WORKERS",
        description="Max parallel OrcaFlex workers"
    )
    orcaflex_timeout: int = Field(
        default=3600,
        env="ORCAFLEX_TIMEOUT",
        description="Simulation timeout (seconds)"
    )

    # Reporting Settings
    report_format: str = Field(
        default="html",
        env="DM_REPORT_FORMAT",
        description="Default report format (html, json, csv)"
    )
    interactive_plots: bool = Field(
        default=True,
        env="DM_INTERACTIVE_PLOTS",
        description="Use interactive Plotly plots"
    )
    plot_theme: str = Field(
        default="plotly_white",
        env="DM_PLOT_THEME",
        description="Default Plotly theme"
    )

    # Logging
    log_level: str = Field(
        default="INFO",
        env="DM_LOG_LEVEL",
        description="Logging level"
    )
    log_file: Optional[Path] = Field(
        default=None,
        env="DM_LOG_FILE",
        description="Log file path"
    )

    @validator('default_units')
    def validate_units(cls, v):
        valid = ['SI', 'Imperial']
        if v not in valid:
            raise ValueError(f"Units must be one of {valid}")
        return v

    @validator('report_format')
    def validate_report_format(cls, v):
        valid = ['html', 'json', 'csv', 'markdown']
        if v not in valid:
            raise ValueError(f"Report format must be one of {valid}")
        return v

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        case_sensitive = False


# Singleton instance
_settings: Optional[GlobalSettings] = None


def get_settings() -> GlobalSettings:
    """Get or create singleton settings instance"""
    global _settings
    if _settings is None:
        _settings = GlobalSettings()
    return _settings


def override_settings(**kwargs) -> GlobalSettings:
    """
    Override settings for parametric analysis or testing.

    Example:
        override_settings(safety_factor=2.0, report_format='json')
    """
    global _settings
    current = get_settings().dict() if _settings else {}
    current.update(kwargs)
    _settings = GlobalSettings(**current)
    return _settings


def reset_settings():
    """Reset settings to defaults (useful for tests)"""
    global _settings
    _settings = None
```

**3. `.env.template`** (in repo root):
```bash
# digitalmodel Configuration
# Copy this to .env and customize

# Paths
DM_DATA_DIR=./data
DM_OUTPUT_DIR=./outputs
DM_REPORT_DIR=./reports

# Analysis
DM_UNITS=SI
DM_SAFETY_FACTOR=1.5

# OrcaFlex
# ORCAFLEX_LICENSE=server:port
ORCAFLEX_WORKERS=30
ORCAFLEX_TIMEOUT=3600

# Reporting
DM_REPORT_FORMAT=html
DM_INTERACTIVE_PLOTS=true
DM_PLOT_THEME=plotly_white

# Logging
DM_LOG_LEVEL=INFO
# DM_LOG_FILE=./logs/digitalmodel.log
```

**4. Test file `tests/test_config.py`**:
```python
"""Test configuration system"""

import pytest
from digitalmodel.config import get_settings, override_settings, reset_settings
from pathlib import Path


def test_default_settings():
    """Test default settings load correctly"""
    reset_settings()
    settings = get_settings()

    assert settings.default_units == "SI"
    assert settings.safety_factor == 1.5
    assert settings.orcaflex_workers == 30
    assert settings.report_format == "html"


def test_override_settings():
    """Test settings override for parametric analysis"""
    reset_settings()

    # Override settings
    settings = override_settings(
        safety_factor=2.0,
        report_format='json'
    )

    assert settings.safety_factor == 2.0
    assert settings.report_format == 'json'
    assert settings.default_units == "SI"  # Unchanged


def test_environment_variable_override(monkeypatch):
    """Test environment variables override defaults"""
    reset_settings()

    monkeypatch.setenv("DM_SAFETY_FACTOR", "3.0")
    monkeypatch.setenv("DM_REPORT_FORMAT", "csv")

    settings = get_settings()

    assert settings.safety_factor == 3.0
    assert settings.report_format == "csv"


def test_validation():
    """Test setting validation"""
    reset_settings()

    with pytest.raises(ValueError, match="Units must be"):
        override_settings(default_units="Invalid")

    with pytest.raises(ValueError, match="Report format must be"):
        override_settings(report_format="invalid")
```

**Execute**:
```bash
# Create the package
mkdir -p src/digitalmodel/config
touch src/digitalmodel/config/__init__.py

# Create the files (copy content from above)
# ... create settings.py, .env.template, tests/test_config.py

# Install pydantic if not already installed
uv pip install pydantic python-dotenv

# Run tests
pytest tests/test_config.py -v
```

**Estimated Time**: 1 day (4hrs setup, 4hrs testing)

---

### ✅ Task 2.2: StandardReport Framework

**Why**: Consistent output format enables parametric analysis and result comparison.

**Action**: Create `src/digitalmodel/reporting/` package

**Files to Create**:

**1. `src/digitalmodel/reporting/__init__.py`**:
```python
"""
Standardized reporting framework for digitalmodel.

All modules should use StandardReport for consistent output.
"""

from .models import (
    StandardReport,
    ParameterSet,
    AnalysisResult,
    ReportFormat,
)
from .parametric import ParametricStudy
from .exporters import HTMLExporter, JSONExporter, CSVExporter

__all__ = [
    'StandardReport',
    'ParameterSet',
    'AnalysisResult',
    'ReportFormat',
    'ParametricStudy',
    'HTMLExporter',
    'JSONExporter',
    'CSVExporter',
]
```

**2. `src/digitalmodel/reporting/models.py`**:
```python
"""Standard report data models"""

from pydantic import BaseModel, Field
from typing import Dict, List, Any, Optional
from datetime import datetime
from enum import Enum
import uuid


class ReportFormat(str, Enum):
    """Supported report output formats"""
    HTML = "html"
    JSON = "json"
    CSV = "csv"
    MARKDOWN = "markdown"


class ParameterSet(BaseModel):
    """A parameter used in the analysis"""
    name: str
    value: Any
    unit: Optional[str] = None
    description: Optional[str] = None


class AnalysisResult(BaseModel):
    """A single analysis result"""
    metric_name: str
    value: float
    unit: str
    status: str  # "PASS", "FAIL", "WARNING"
    safety_factor: Optional[float] = None
    standard: Optional[str] = None
    notes: Optional[str] = None


class StandardReport(BaseModel):
    """
    Universal report format for all digitalmodel analyses.

    Example:
        report = StandardReport(
            report_id="STRUCT-001",
            module="structural_analysis",
            analysis_type="stress_calculation",
            parameters=[
                ParameterSet(name="sigma_x", value=100, unit="MPa"),
            ],
            results=[
                AnalysisResult(
                    metric_name="von_mises_stress",
                    value=150.5,
                    unit="MPa",
                    status="PASS",
                    safety_factor=2.5,
                )
            ],
            summary={"total_stress": 150.5},
            execution_time_seconds=0.023,
            status="SUCCESS",
        )
    """

    # Metadata
    report_id: str = Field(default_factory=lambda: str(uuid.uuid4())[:8])
    module: str
    analysis_type: str
    timestamp: datetime = Field(default_factory=datetime.now)
    version: str = "1.0.0"

    # Input Parameters
    parameters: List[ParameterSet] = []

    # Results
    results: List[AnalysisResult]
    summary: Dict[str, Any] = {}

    # Files
    input_files: List[str] = []
    output_files: List[str] = []
    plot_files: List[str] = []

    # Execution
    execution_time_seconds: float
    status: str  # "SUCCESS", "PARTIAL", "FAILED"
    warnings: List[str] = []
    errors: List[str] = []

    def to_json(self) -> str:
        """Export as JSON"""
        return self.json(indent=2)

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return self.dict()

    def get_parameter(self, name: str) -> Optional[ParameterSet]:
        """Get parameter by name"""
        for param in self.parameters:
            if param.name == name:
                return param
        return None

    def get_result(self, metric_name: str) -> Optional[AnalysisResult]:
        """Get result by metric name"""
        for result in self.results:
            if result.metric_name == metric_name:
                return result
        return None
```

**3. `src/digitalmodel/reporting/parametric.py`**:
```python
"""Parametric study management"""

from pydantic import BaseModel
from typing import List, Dict, Any
import pandas as pd
from .models import StandardReport, ParameterSet


class ParametricStudy(BaseModel):
    """
    Track parametric analysis with multiple runs.

    Example:
        study = ParametricStudy(
            study_name="Stress_Sensitivity",
            base_parameters=[
                ParameterSet(name="material", value="S355"),
            ],
            varied_parameters=["sigma_x", "sigma_y"]
        )

        # Add runs
        for report in run_multiple_analyses():
            study.add_run(report)

        # Generate comparison
        df = study.generate_comparison_table()
        study.export_aggregated_results("results.csv", format="csv")
    """

    study_name: str
    base_parameters: List[ParameterSet] = []
    varied_parameters: List[str] = []
    runs: List[StandardReport] = []

    def add_run(self, report: StandardReport):
        """Add a report to the study"""
        self.runs.append(report)

    def generate_comparison_table(self) -> pd.DataFrame:
        """
        Create comparison table across all runs.

        Returns DataFrame with:
        - Columns for each varied parameter
        - Columns for each result metric
        - One row per run
        """
        data = []

        for run in self.runs:
            row = {}

            # Add parameters
            for param in run.parameters:
                row[f"param_{param.name}"] = param.value

            # Add results
            for result in run.results:
                row[f"result_{result.metric_name}"] = result.value
                row[f"status_{result.metric_name}"] = result.status
                if result.safety_factor:
                    row[f"sf_{result.metric_name}"] = result.safety_factor

            # Add metadata
            row['report_id'] = run.report_id
            row['status'] = run.status
            row['execution_time'] = run.execution_time_seconds

            data.append(row)

        return pd.DataFrame(data)

    def export_aggregated_results(self, filepath: str, format: str = "csv"):
        """Export all results in consistent format"""
        df = self.generate_comparison_table()

        if format == "csv":
            df.to_csv(filepath, index=False)
        elif format == "excel":
            df.to_excel(filepath, index=False)
        elif format == "json":
            df.to_json(filepath, orient='records', indent=2)
        else:
            raise ValueError(f"Unsupported format: {format}")
```

**4. Test file `tests/test_reporting.py`**:
```python
"""Test reporting framework"""

import pytest
from digitalmodel.reporting import (
    StandardReport,
    ParameterSet,
    AnalysisResult,
    ParametricStudy,
)


def test_standard_report_creation():
    """Test creating a standard report"""
    report = StandardReport(
        module="structural_analysis",
        analysis_type="stress_calculation",
        parameters=[
            ParameterSet(name="sigma_x", value=100, unit="MPa"),
            ParameterSet(name="material", value="S355"),
        ],
        results=[
            AnalysisResult(
                metric_name="von_mises_stress",
                value=150.5,
                unit="MPa",
                status="PASS",
                safety_factor=2.5,
                standard="Eurocode 3",
            )
        ],
        summary={"total_stress": 150.5},
        execution_time_seconds=0.023,
        status="SUCCESS",
    )

    assert report.module == "structural_analysis"
    assert len(report.parameters) == 2
    assert len(report.results) == 1
    assert report.status == "SUCCESS"
    assert report.report_id is not None


def test_parameter_lookup():
    """Test getting parameters by name"""
    report = StandardReport(
        module="test",
        analysis_type="test",
        parameters=[
            ParameterSet(name="sigma_x", value=100, unit="MPa"),
            ParameterSet(name="material", value="S355"),
        ],
        results=[],
        execution_time_seconds=0.01,
        status="SUCCESS",
    )

    param = report.get_parameter("sigma_x")
    assert param is not None
    assert param.value == 100

    param = report.get_parameter("nonexistent")
    assert param is None


def test_parametric_study():
    """Test parametric study management"""
    study = ParametricStudy(
        study_name="Stress_Sensitivity",
        base_parameters=[
            ParameterSet(name="material", value="S355"),
        ],
        varied_parameters=["sigma_x", "sigma_y"]
    )

    # Add multiple runs
    for sx in [50, 100, 150]:
        for sy in [40, 80, 120]:
            report = StandardReport(
                module="structural_analysis",
                analysis_type="stress_calc",
                parameters=[
                    ParameterSet(name="sigma_x", value=sx, unit="MPa"),
                    ParameterSet(name="sigma_y", value=sy, unit="MPa"),
                    ParameterSet(name="material", value="S355"),
                ],
                results=[
                    AnalysisResult(
                        metric_name="von_mises_stress",
                        value=sx * 1.2 + sy * 0.8,  # Dummy calculation
                        unit="MPa",
                        status="PASS",
                    )
                ],
                execution_time_seconds=0.01,
                status="SUCCESS",
            )
            study.add_run(report)

    assert len(study.runs) == 9  # 3x3 parameter grid

    # Generate comparison table
    df = study.generate_comparison_table()
    assert len(df) == 9
    assert 'param_sigma_x' in df.columns
    assert 'param_sigma_y' in df.columns
    assert 'result_von_mises_stress' in df.columns
```

**Execute**:
```bash
# Create the package
mkdir -p src/digitalmodel/reporting
touch src/digitalmodel/reporting/__init__.py

# Create files (copy content from above)

# Install dependencies
uv pip install pandas openpyxl

# Run tests
pytest tests/test_reporting.py -v
```

**Estimated Time**: 2 days (6hrs setup, 6hrs testing, 4hrs documentation)

---

### ✅ Task 2.3: Add CLI Integration Tests

**Why**: Prevents regressions in CLI interfaces, ensures consistency.

**Action**: Create `tests/cli/` directory with test files

**Example Test**: `tests/cli/test_structural_cli.py`
```python
"""Integration tests for structural analysis CLI"""

import pytest
from click.testing import CliRunner
from digitalmodel.modules.structural_analysis.cli import cli
import json


@pytest.fixture
def runner():
    """Create CLI test runner"""
    return CliRunner()


def test_stress_calculation_basic(runner):
    """Test basic stress calculation via CLI"""
    result = runner.invoke(cli, [
        'stress',
        '--sigma-x', '100',
        '--sigma-y', '80',
        '--material', 'S355'
    ])

    assert result.exit_code == 0
    assert 'von_mises_stress' in result.output


def test_stress_calculation_json_output(runner, tmp_path):
    """Test JSON output from stress calculation"""
    output_file = tmp_path / "results.json"

    result = runner.invoke(cli, [
        'stress',
        '--sigma-x', '100',
        '--sigma-y', '80',
        '--sigma-z', '60',
        '--material', 'S355',
        '--output', str(output_file)
    ])

    assert result.exit_code == 0
    assert output_file.exists()

    # Verify JSON content
    with open(output_file) as f:
        data = json.load(f)

    assert 'von_mises_stress' in data
    assert 'safety_factor' in data
    assert data['status'] in ['PASS', 'FAIL']


def test_stress_calculation_invalid_material(runner):
    """Test error handling for invalid material"""
    result = runner.invoke(cli, [
        'stress',
        '--sigma-x', '100',
        '--material', 'INVALID'
    ])

    assert result.exit_code != 0
    assert 'Invalid value' in result.output or 'Error' in result.output


def test_stress_calculation_all_stresses(runner):
    """Test with all stress components"""
    result = runner.invoke(cli, [
        'stress',
        '--sigma-x', '100',
        '--sigma-y', '80',
        '--sigma-z', '60',
        '--tau-xy', '20',
        '--tau-xz', '15',
        '--tau-yz', '10',
        '--material', 'S420'
    ])

    assert result.exit_code == 0
    assert 'von_mises_stress' in result.output
    assert 'principal_stresses' in result.output
```

**Create Tests for All CLIs**:
```bash
tests/cli/
├── test_structural_cli.py
├── test_mooring_cli.py
├── test_fatigue_cli.py
├── test_catenary_cli.py
├── test_orcaflex_cli.py
└── test_signal_analysis_cli.py
```

**Execute**:
```bash
# Create test directory
mkdir -p tests/cli

# Create test files (copy examples from above)

# Run all CLI tests
pytest tests/cli/ -v

# Run with coverage
pytest tests/cli/ --cov=src/digitalmodel/modules --cov-report=html
```

**Estimated Time**: 1 day (1hr per CLI, ~6-8 CLIs)

---

## 📋 PRIORITY 2: Refactoring (Week 3-4)

### ✅ Task 3.1: Resolve Module Overlap

**Affected Modules**:
1. `modules/catenary/` vs `modules/catenary_riser/`
2. `modules/mooring/` vs `modules/mooring_analysis/`
3. `modules/orcaflex_post_process/` vs `modules/orcaflex/post_processing/`

**Action for each**:
1. Compare functionality
2. Identify duplicates
3. Merge into single unified module
4. Update imports across codebase
5. Update tests
6. Archive old modules

**Example**: Merge catenary modules
```bash
# 1. Review both modules
ls -la src/digitalmodel/modules/catenary/
ls -la src/digitalmodel/modules/catenary_riser/

# 2. Create unified module
mkdir -p src/digitalmodel/modules/riser_analysis
mkdir -p src/digitalmodel/modules/riser_analysis/catenary
mkdir -p src/digitalmodel/modules/riser_analysis/lazy_wave

# 3. Move files
# (Use IDE refactoring or manual moves with import updates)

# 4. Run tests
pytest tests/ -v

# 5. Archive old modules (after verification)
git mv src/digitalmodel/modules/catenary src/digitalmodel/archive/catenary
git mv src/digitalmodel/modules/catenary_riser src/digitalmodel/archive/catenary_riser
```

**Estimated Time**: 1 week (2-3 days per merge)

---

### ✅ Task 3.2: Refactor common/ Module

**Current**: 23 files, mixed responsibilities
**Target**: Organized into specific packages

**New Structure**:
```
src/digitalmodel/
├── analysis/              # Analysis utilities
│   ├── fatigue.py
│   ├── stress.py
│   └── buckling.py
├── io/                    # Input/output utilities
│   ├── file_handlers.py
│   └── csv_utils.py
├── plotting/              # Visualization utilities
│   ├── themes.py
│   └── utils.py
├── engineering/           # Engineering calculations
│   ├── hydrodynamics.py
│   └── cathodic_protection.py
└── utils/                 # General utilities
    └── parallel_processing.py
```

**Migration Script**: `scripts/refactor_common.sh`
```bash
#!/bin/bash
# Refactor common/ into organized packages

# Create new structure
mkdir -p src/digitalmodel/analysis
mkdir -p src/digitalmodel/io
mkdir -p src/digitalmodel/plotting
mkdir -p src/digitalmodel/engineering
mkdir -p src/digitalmodel/utils

# Move files (examples)
# Analysis
git mv src/digitalmodel/common/fatigue_analysis.py src/digitalmodel/analysis/fatigue.py
git mv src/digitalmodel/common/plate_buckling.py src/digitalmodel/analysis/buckling.py

# Engineering
git mv src/digitalmodel/common/cathodic_protection.py src/digitalmodel/engineering/cathodic_protection.py
git mv src/digitalmodel/common/code_dnvrph103_hydrodynamics_circular.py src/digitalmodel/engineering/hydrodynamics_circular.py

# Plotting
git mv src/digitalmodel/common/visualizations.py src/digitalmodel/plotting/visualizations.py
git mv src/digitalmodel/common/visualization_components.py src/digitalmodel/plotting/components.py

# IO
git mv src/digitalmodel/common/ETL_components.py src/digitalmodel/io/etl.py

# Utils
git mv src/digitalmodel/common/parallel_processing.py src/digitalmodel/utils/parallel.py

echo "✅ Files moved. Update imports next."
```

**Update Imports**: Use IDE or script
```python
# scripts/update_imports_after_refactor.py
import ast
import os
from pathlib import Path

# Map of old → new imports
IMPORT_MAP = {
    'digitalmodel.common.fatigue_analysis': 'digitalmodel.analysis.fatigue',
    'digitalmodel.common.plate_buckling': 'digitalmodel.analysis.buckling',
    'digitalmodel.common.cathodic_protection': 'digitalmodel.engineering.cathodic_protection',
    # ... add all mappings
}

def update_imports_in_file(filepath):
    # Read file, parse AST, update imports, write back
    # (Implementation details)
    pass

# Run on all Python files
for py_file in Path('src').rglob('*.py'):
    update_imports_in_file(py_file)
```

**Estimated Time**: 1 week (3 days migration, 2 days testing, 2 days cleanup)

---

## 🎯 SUCCESS CRITERIA

### After Week 1:
- [ ] All files renamed (no spaces/commas)
- [ ] Technical debt documented
- [ ] CLI tests added for 3+ modules

### After Week 2:
- [ ] Config system implemented and tested
- [ ] StandardReport framework functional
- [ ] 5+ modules using new reporting

### After Week 4:
- [ ] Module overlap resolved
- [ ] common/ refactored
- [ ] Test coverage increased

---

## 📞 HELP & RESOURCES

**Full Audit Report**: `reports/code_audit/COMPREHENSIVE_CODE_AUDIT_2026.md`
**Quick Reference**: `reports/code_audit/QUICK_REFERENCE.md`

**Questions?**
- Check full audit report for detailed explanations
- See code examples in audit report Section 5
- Review module analysis in Appendix

---

**Next Step**: Start with P0 tasks (file naming + technical debt tracking)

