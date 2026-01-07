"""
ABOUTME: Tests for quality gate validation system.
Tests each gate type: tests, coverage, quality, security, documentation.
"""

import json
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

import pytest

from digitalmodel.modules.automation.quality_gates import (
    GateStatus,
    QualityGateValidator,
)


@pytest.fixture
def mock_config():
    """Mock quality gates configuration."""
    return {
        "gates": {
            "tests": {
                "enabled": True,
                "order": 1,
                "command": "pytest --maxfail=1 -x",
                "failure_action": "block",
            },
            "coverage": {
                "enabled": True,
                "order": 2,
                "depends_on": ["tests"],
                "thresholds": {"failure": 60.0, "warning": 80.0},
                "output_file": "coverage.json",
            },
            "quality": {
                "enabled": True,
                "order": 3,
                "depends_on": ["tests", "coverage"],
                "tools": {
                    "ruff": {
                        "enabled": True,
                        "complexity_threshold": {"warning": 10, "failure": 15},
                    }
                },
            },
            "security": {
                "enabled": True,
                "order": 4,
                "depends_on": ["tests", "coverage", "quality"],
                "tools": {
                    "bandit": {
                        "enabled": True,
                        "severity_threshold": {
                            "high": "failure",
                            "medium": "warning",
                            "low": "report",
                        },
                    }
                },
            },
            "documentation": {
                "enabled": True,
                "order": 5,
                "threshold": 75.0,
                "scan_paths": ["src/digitalmodel"],
                "exclude_paths": ["*/tests/*"],
            },
        },
        "settings": {
            "execution_mode": "linear",
            "cli": {"strict_mode": False, "verbose": True, "show_details": True},
            "pre_commit": {"enabled": True, "strict_mode": True, "fail_fast": True},
            "ci_cd": {
                "enabled": True,
                "export_results": True,
                "result_file": "reports/quality_gates_results.json",
            },
            "reporting": {"console": {"enabled": True}, "json": {"enabled": True}},
            "paths": {
                "source_root": "src",
                "test_root": "tests",
                "reports_dir": "reports",
            },
        },
    }


@pytest.fixture
def validator(tmp_path, mock_config):
    """Create validator with mock config."""
    config_file = tmp_path / ".claude" / "quality-gates.yaml"
    config_file.parent.mkdir(parents=True)

    import yaml

    with open(config_file, "w") as f:
        yaml.dump(mock_config, f)

    with patch("digitalmodel.modules.automation.quality_gates.Path.cwd", return_value=tmp_path):
        validator = QualityGateValidator(config_path=config_file)
        return validator


class TestQualityGateValidator:
    """Test QualityGateValidator class."""

    def test_init(self, validator):
        """Test validator initialization."""
        assert validator is not None
        assert validator.config is not None
        assert validator.strict_mode is False

    def test_init_strict_mode(self, tmp_path, mock_config):
        """Test validator initialization with strict mode."""
        config_file = tmp_path / ".claude" / "quality-gates.yaml"
        config_file.parent.mkdir(parents=True)

        import yaml

        with open(config_file, "w") as f:
            yaml.dump(mock_config, f)

        with patch("digitalmodel.modules.automation.quality_gates.Path.cwd", return_value=tmp_path):
            validator = QualityGateValidator(config_path=config_file, strict_mode=True)
            assert validator.strict_mode is True

    def test_get_ordered_gates(self, validator):
        """Test gates are returned in correct order."""
        gates = validator._get_ordered_gates()
        gate_names = [name for name, _ in gates]

        assert gate_names == ["tests", "coverage", "quality", "security", "documentation"]

    def test_check_dependencies_no_deps(self, validator):
        """Test dependency check with no dependencies."""
        assert validator._check_dependencies([], []) is True

    def test_check_dependencies_met(self, validator):
        """Test dependency check when all dependencies met."""
        from digitalmodel.modules.automation.quality_gates import GateResult

        results = [
            GateResult("tests", GateStatus.PASS, "All tests passed"),
            GateResult("coverage", GateStatus.PASS, "Coverage good"),
        ]

        assert validator._check_dependencies(["tests", "coverage"], results) is True

    def test_check_dependencies_not_met(self, validator):
        """Test dependency check when dependency failed."""
        from digitalmodel.modules.automation.quality_gates import GateResult

        results = [
            GateResult("tests", GateStatus.FAILURE, "Tests failed"),
        ]

        assert validator._check_dependencies(["tests"], results) is False

    @patch("subprocess.run")
    def test_execute_tests_gate_pass(self, mock_run, validator):
        """Test tests gate execution - passing tests."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        result = validator._execute_tests_gate({"command": "pytest --maxfail=1 -x"})

        assert result.gate_name == "tests"
        assert result.status == GateStatus.PASS
        assert result.metrics["exit_code"] == 0

    @patch("subprocess.run")
    def test_execute_tests_gate_failure(self, mock_run, validator):
        """Test tests gate execution - failing tests."""
        mock_run.return_value = MagicMock(returncode=1, stdout="", stderr="Test failed")

        result = validator._execute_tests_gate({"command": "pytest --maxfail=1 -x"})

        assert result.gate_name == "tests"
        assert result.status == GateStatus.FAILURE
        assert result.metrics["exit_code"] == 1

    def test_execute_coverage_gate_pass(self, validator, tmp_path):
        """Test coverage gate execution - passing coverage."""
        coverage_data = {
            "totals": {
                "percent_covered": 85.5,
                "covered_lines": 855,
                "num_statements": 1000,
            }
        }

        coverage_file = tmp_path / "coverage.json"
        with open(coverage_file, "w") as f:
            json.dump(coverage_data, f)

        with patch.object(validator, "project_root", tmp_path):
            result = validator._execute_coverage_gate({"output_file": "coverage.json", "thresholds": {"failure": 60.0, "warning": 80.0}})

        assert result.gate_name == "coverage"
        assert result.status == GateStatus.PASS
        assert result.metrics["coverage_percent"] == 85.5

    def test_execute_coverage_gate_warning(self, validator, tmp_path):
        """Test coverage gate execution - warning threshold."""
        coverage_data = {
            "totals": {
                "percent_covered": 70.0,
                "covered_lines": 700,
                "num_statements": 1000,
            }
        }

        coverage_file = tmp_path / "coverage.json"
        with open(coverage_file, "w") as f:
            json.dump(coverage_data, f)

        with patch.object(validator, "project_root", tmp_path):
            result = validator._execute_coverage_gate({"output_file": "coverage.json", "thresholds": {"failure": 60.0, "warning": 80.0}})

        assert result.gate_name == "coverage"
        assert result.status == GateStatus.WARNING
        assert result.metrics["coverage_percent"] == 70.0

    def test_execute_coverage_gate_failure(self, validator, tmp_path):
        """Test coverage gate execution - failure threshold."""
        coverage_data = {
            "totals": {
                "percent_covered": 50.0,
                "covered_lines": 500,
                "num_statements": 1000,
            }
        }

        coverage_file = tmp_path / "coverage.json"
        with open(coverage_file, "w") as f:
            json.dump(coverage_data, f)

        with patch.object(validator, "project_root", tmp_path):
            result = validator._execute_coverage_gate({"output_file": "coverage.json", "thresholds": {"failure": 60.0, "warning": 80.0}})

        assert result.gate_name == "coverage"
        assert result.status == GateStatus.FAILURE
        assert result.metrics["coverage_percent"] == 50.0

    @patch("subprocess.run")
    def test_execute_quality_gate_pass(self, mock_run, validator):
        """Test quality gate execution - no issues."""
        mock_run.return_value = MagicMock(returncode=0, stdout="[]", stderr="")

        result = validator._execute_quality_gate(
            {
                "tools": {
                    "ruff": {
                        "enabled": True,
                        "complexity_threshold": {"warning": 10, "failure": 15},
                    }
                }
            }
        )

        assert result.gate_name == "quality"
        assert result.status == GateStatus.PASS
        assert result.metrics["total_issues"] == 0

    @patch("subprocess.run")
    def test_execute_security_gate_pass(self, mock_run, validator, tmp_path):
        """Test security gate execution - no issues."""
        bandit_data = {"results": []}

        output_file = tmp_path / "reports" / "bandit_report.json"
        output_file.parent.mkdir(parents=True, exist_ok=True)

        # Mock subprocess to create the file
        def create_bandit_report(*args, **kwargs):
            with open(output_file, "w") as f:
                json.dump(bandit_data, f)
            return MagicMock(returncode=0, stdout="", stderr="")

        mock_run.side_effect = create_bandit_report

        with patch.object(validator, "reports_dir", tmp_path / "reports"):
            result = validator._execute_security_gate(
                {
                    "tools": {
                        "bandit": {
                            "enabled": True,
                            "severity_threshold": {
                                "high": "failure",
                                "medium": "warning",
                            },
                        }
                    }
                }
            )

        assert result.gate_name == "security"
        assert result.status == GateStatus.PASS
        assert result.metrics["total_issues"] == 0

    @patch("subprocess.run")
    def test_execute_security_gate_high_severity(self, mock_run, validator, tmp_path):
        """Test security gate execution - high severity issues."""
        bandit_data = {
            "results": [
                {"issue_severity": "HIGH", "issue_text": "SQL injection risk"},
                {"issue_severity": "MEDIUM", "issue_text": "Weak crypto"},
            ]
        }

        output_file = tmp_path / "reports" / "bandit_report.json"
        output_file.parent.mkdir(parents=True, exist_ok=True)

        def create_bandit_report(*args, **kwargs):
            with open(output_file, "w") as f:
                json.dump(bandit_data, f)
            return MagicMock(returncode=1, stdout="", stderr="")

        mock_run.side_effect = create_bandit_report

        with patch.object(validator, "reports_dir", tmp_path / "reports"):
            result = validator._execute_security_gate(
                {
                    "tools": {
                        "bandit": {
                            "enabled": True,
                            "severity_threshold": {
                                "high": "failure",
                                "medium": "warning",
                            },
                        }
                    }
                }
            )

        assert result.gate_name == "security"
        assert result.status == GateStatus.FAILURE
        assert result.metrics["high_severity"] == 1
        assert result.metrics["medium_severity"] == 1

    def test_execute_documentation_gate_pass(self, validator, tmp_path):
        """Test documentation gate execution - sufficient coverage."""
        # Create test Python file with docstrings
        test_file = tmp_path / "src" / "digitalmodel" / "test.py"
        test_file.parent.mkdir(parents=True)

        code = '''
def documented_func():
    """This function has a docstring."""
    pass

def another_documented():
    """Another docstring."""
    return True

def undocumented():
    pass
'''
        test_file.write_text(code)

        with patch.object(validator, "project_root", tmp_path):
            result = validator._execute_documentation_gate(
                {
                    "threshold": 60.0,  # 2/3 = 66.7%, should pass
                    "scan_paths": ["src/digitalmodel"],
                    "exclude_paths": ["*/tests/*"],
                }
            )

        assert result.gate_name == "documentation"
        assert result.status == GateStatus.PASS
        assert result.metrics["total_functions"] == 3
        assert result.metrics["documented_functions"] == 2

    def test_count_docstrings(self, validator, tmp_path):
        """Test docstring counting function."""
        test_file = tmp_path / "test.py"
        code = '''
def func1():
    """Docstring."""
    pass

def func2():
    pass

async def async_func():
    """Async docstring."""
    return None

class MyClass:
    def method(self):
        """Method docstring."""
        pass
'''
        test_file.write_text(code)

        total, documented = validator._count_docstrings(test_file)

        assert total == 4  # func1, func2, async_func, method
        assert documented == 3  # func1, async_func, method

    def test_build_report(self, validator):
        """Test report building from results."""
        from digitalmodel.modules.automation.quality_gates import GateResult

        results = [
            GateResult("tests", GateStatus.PASS, "Tests passed"),
            GateResult("coverage", GateStatus.WARNING, "Coverage low"),
            GateResult("quality", GateStatus.FAILURE, "Quality issues"),
        ]

        report = validator._build_report(results, 10.5)

        assert report.overall_status == GateStatus.FAILURE
        assert report.gates_executed == 3
        assert report.gates_passed == 1
        assert report.gates_warned == 1
        assert report.gates_failed == 1
        assert report.execution_time == 10.5

    def test_build_report_strict_mode(self, validator):
        """Test report building with strict mode (warnings become failures)."""
        from digitalmodel.modules.automation.quality_gates import GateResult

        validator.strict_mode = True

        results = [
            GateResult("tests", GateStatus.PASS, "Tests passed"),
            GateResult("coverage", GateStatus.WARNING, "Coverage low"),
        ]

        report = validator._build_report(results, 5.0)

        assert report.overall_status == GateStatus.FAILURE  # Warning → Failure in strict mode

    def test_export_results(self, validator, tmp_path):
        """Test JSON export of results."""
        from digitalmodel.modules.automation.quality_gates import GateResult, QualityGateReport

        results = [
            GateResult("tests", GateStatus.PASS, "Tests passed", metrics={"exit_code": 0}),
        ]

        report = QualityGateReport(
            overall_status=GateStatus.PASS,
            gates_executed=1,
            gates_passed=1,
            gates_warned=0,
            gates_failed=0,
            gates_skipped=0,
            results=results,
            execution_time=5.0,
        )

        reports_dir = tmp_path / "reports"
        reports_dir.mkdir(exist_ok=True)

        with patch.object(validator, "reports_dir", reports_dir):
            validator._export_results(report)

        output_file = reports_dir / "quality_gates_results.json"
        assert output_file.exists()

        with open(output_file) as f:
            data = json.load(f)

        assert data["overall_status"] == "pass"
        assert data["gates_executed"] == 1
        assert len(data["results"]) == 1


class TestGateIntegration:
    """Integration tests for complete gate execution."""

    @patch("subprocess.run")
    def test_linear_execution_order(self, mock_run, validator, tmp_path):
        """Test gates execute in linear order with dependencies."""
        # Mock all subprocess calls to succeed
        mock_run.return_value = MagicMock(returncode=0, stdout="[]", stderr="")

        # Create mock coverage file
        coverage_file = tmp_path / "coverage.json"
        coverage_data = {"totals": {"percent_covered": 85.0, "covered_lines": 850, "num_statements": 1000}}
        with open(coverage_file, "w") as f:
            json.dump(coverage_data, f)

        # Create mock bandit report
        bandit_file = tmp_path / "reports" / "bandit_report.json"
        bandit_file.parent.mkdir(parents=True, exist_ok=True)

        def create_reports(*args, **kwargs):
            if "bandit" in str(args):
                with open(bandit_file, "w") as f:
                    json.dump({"results": []}, f)
            return MagicMock(returncode=0, stdout="[]", stderr="")

        mock_run.side_effect = create_reports

        with patch.object(validator, "project_root", tmp_path):
            with patch.object(validator, "reports_dir", tmp_path / "reports"):
                report = validator.execute_all_gates()

        # Check execution order and dependencies
        gate_names = [r.gate_name for r in report.results if r.status != GateStatus.SKIPPED]

        # Should execute in order: tests, coverage, quality, security
        # (documentation may be skipped if no files found)
        assert "tests" in gate_names
        assert "coverage" in gate_names
        assert gate_names.index("tests") < gate_names.index("coverage")
