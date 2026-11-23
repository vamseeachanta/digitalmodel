"""
ABOUTME: Tests for quality gate validation system.
Tests each gate type: tests, coverage, quality, security, documentation.
"""

import json
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.workflows.automation.quality_gates import (
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

    with patch("digitalmodel.workflows.automation.quality_gates.Path.cwd", return_value=tmp_path):
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

        with patch("digitalmodel.workflows.automation.quality_gates.Path.cwd", return_value=tmp_path):
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
        from digitalmodel.workflows.automation.quality_gates import GateResult

        results = [
            GateResult("tests", GateStatus.PASS, "All tests passed"),
            GateResult("coverage", GateStatus.PASS, "Coverage good"),
        ]

        assert validator._check_dependencies(["tests", "coverage"], results) is True

    def test_check_dependencies_not_met(self, validator):
        """Test dependency check when dependency failed."""
        from digitalmodel.workflows.automation.quality_gates import GateResult

        results = [
            GateResult("tests", GateStatus.FAILURE, "Tests failed"),
        ]

        assert validator._check_dependencies(["tests"], results) is False

    def test_check_dependencies_not_met_when_dependency_skipped_or_error(self, validator):
        """Skipped/error dependencies should not unlock dependent gates."""
        from digitalmodel.workflows.automation.quality_gates import GateResult

        skipped_results = [
            GateResult("tests-citations", GateStatus.SKIPPED, "Handled elsewhere"),
        ]
        error_results = [
            GateResult("tests-citations", GateStatus.ERROR, "Unknown gate type"),
        ]

        assert validator._check_dependencies(["tests-citations"], skipped_results) is False
        assert validator._check_dependencies(["tests-citations"], error_results) is False

    def test_execute_domain_test_gate_is_skipped_by_aggregate_runner(self, validator):
        """Domain test gates are owned by the domain-sharded CI workflow."""
        result = validator._execute_gate(
            "tests-citations",
            {
                "domain": "citations",
                "command": "python -m pytest tests/citations",
            },
        )

        assert result.gate_name == "tests-citations"
        assert result.status == GateStatus.SKIPPED
        assert "domain-sharded" in result.message

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

    @patch("subprocess.run")
    def test_domain_sharded_gate_skips_in_github_actions(self, mock_run, validator, monkeypatch):
        """Aggregate CI must not serially execute domain-sharded test gates."""
        monkeypatch.setenv("GITHUB_ACTIONS", "true")

        result = validator._execute_gate(
            "tests-cathodic-protection",
            {
                "domain": "cathodic-protection",
                "command": "pytest tests/cathodic_protection",
            },
        )

        assert result.gate_name == "tests-cathodic-protection"
        assert result.status == GateStatus.SKIPPED
        assert "quality-gates-by-domain" in result.message
        mock_run.assert_not_called()

    def test_execute_all_skips_domain_gates_and_dependents_in_github_actions(
        self, tmp_path, monkeypatch
    ):
        """Aggregate CI delegates domain gates and skips gates needing their artifacts."""
        import yaml

        monkeypatch.setenv("GITHUB_ACTIONS", "true")
        config = {
            "gates": {
                "tests-citations": {
                    "enabled": True,
                    "order": 1,
                    "domain": "citations",
                    "command": "pytest tests/citations",
                },
                "tests-all": {
                    "enabled": True,
                    "order": 2,
                    "aggregate": True,
                    "depends_on": ["tests-citations"],
                    "command": "python -c 'print(\"domain test gates complete\")'",
                },
                "coverage": {
                    "enabled": True,
                    "order": 3,
                    "depends_on": ["tests-all"],
                    "output_file": "coverage.json",
                },
                "documentation": {
                    "enabled": True,
                    "order": 4,
                    "threshold": 75.0,
                    "scan_paths": ["src/digitalmodel"],
                    "exclude_paths": [],
                },
            },
            "settings": {
                "execution_mode": "dag",
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
        config_file = tmp_path / ".claude" / "quality-gates.yaml"
        config_file.parent.mkdir(parents=True)
        config_file.write_text(yaml.dump(config), encoding="utf-8")

        with patch("digitalmodel.workflows.automation.quality_gates.Path.cwd", return_value=tmp_path):
            validator = QualityGateValidator(config_path=config_file)
            report = validator.execute_all_gates()

        statuses = {result.gate_name: result.status for result in report.results}
        assert statuses["tests-citations"] == GateStatus.SKIPPED
        assert statuses["tests-all"] == GateStatus.SKIPPED
        assert statuses["coverage"] == GateStatus.SKIPPED
        assert statuses["documentation"] == GateStatus.PASS
        assert report.overall_status == GateStatus.PASS

    @patch("subprocess.run")
    def test_execute_tests_gate_writes_full_log(self, mock_run, validator, tmp_path):
        """Tests gate must write reports/quality-gates-pytest-full.log on every run (#546)."""
        fake_output = ("FAILED tests/x.py::test_a - AssertionError\n" * 1200)  # ~50 KB
        mock_run.return_value = MagicMock(returncode=1, stdout=fake_output, stderr="")
        validator.reports_dir = tmp_path  # redirect writes

        result = validator._execute_tests_gate({"command": "pytest --maxfail=20"})

        log_path = tmp_path / "quality-gates-pytest-full.log"
        assert log_path.exists(), "full pytest log must be written"
        contents = log_path.read_text(encoding="utf-8")
        assert "FAILED tests/x.py::test_a" in contents
        assert len(contents) >= 50_000, "full log must contain entire pytest output, not truncated tail"
        # Existing JSON metric stays intact
        assert "output_tail" in result.metrics

    def test_execute_tests_gate_writes_full_log_live(self, validator, tmp_path):
        """Live (un-mocked) subprocess invocation: tiny shell command stands in for pytest (#546).

        Note: _execute_tests_gate uses command.split() (whitespace tokenization),
        so we use a command whose arguments contain no spaces.
        """
        validator.reports_dir = tmp_path
        # 'yes' would be unbounded; use 'seq 1 100' which prints 1\n2\n...\n100\n (100 newlines).
        cmd = "seq 1 100"

        validator._execute_tests_gate({"command": cmd})

        log_path = tmp_path / "quality-gates-pytest-full.log"
        assert log_path.exists()
        assert log_path.read_text().count("\n") >= 100

    @patch("subprocess.run")
    def test_execute_tests_gate_writes_full_log_on_timeout(self, mock_run, validator, tmp_path):
        """Even on TimeoutExpired, partial pytest stdout must be written to the full log (#546)."""
        from subprocess import TimeoutExpired
        mock_run.side_effect = TimeoutExpired(cmd="pytest", timeout=600, output="partial output before kill\n")
        validator.reports_dir = tmp_path

        result = validator._execute_tests_gate({"command": "pytest --maxfail=20"})

        log_path = tmp_path / "quality-gates-pytest-full.log"
        assert log_path.exists()
        assert "partial output before kill" in log_path.read_text()
        assert result.status == GateStatus.ERROR

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
        from digitalmodel.workflows.automation.quality_gates import GateResult

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
        from digitalmodel.workflows.automation.quality_gates import GateResult

        validator.strict_mode = True

        results = [
            GateResult("tests", GateStatus.PASS, "Tests passed"),
            GateResult("coverage", GateStatus.WARNING, "Coverage low"),
        ]

        report = validator._build_report(results, 5.0)

        assert report.overall_status == GateStatus.FAILURE  # Warning → Failure in strict mode

    def test_export_results(self, validator, tmp_path):
        """Test JSON export of results."""
        from digitalmodel.workflows.automation.quality_gates import GateResult, QualityGateReport

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
