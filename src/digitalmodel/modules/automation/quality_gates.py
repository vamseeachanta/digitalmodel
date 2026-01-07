"""
ABOUTME: Quality Gate Validator - Executes quality checks with configurable thresholds.
Implements linear gate execution (tests → coverage → quality → security → documentation).
"""

import ast
import json
import subprocess
import sys
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import yaml
from loguru import logger


class GateStatus(Enum):
    """Gate execution status."""

    PASS = "pass"
    WARNING = "warning"
    FAILURE = "failure"
    SKIPPED = "skipped"
    ERROR = "error"


class GateAction(Enum):
    """Action to take on gate result."""

    BLOCK = "block"  # Hard failure
    WARN = "warn"  # Warning only
    REPORT = "report"  # Info only


@dataclass
class GateResult:
    """Result from a single gate execution."""

    gate_name: str
    status: GateStatus
    message: str
    details: Dict[str, Any] = field(default_factory=dict)
    metrics: Dict[str, float] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)


@dataclass
class QualityGateReport:
    """Aggregated quality gate report."""

    overall_status: GateStatus
    gates_executed: int
    gates_passed: int
    gates_warned: int
    gates_failed: int
    gates_skipped: int
    results: List[GateResult] = field(default_factory=list)
    execution_time: float = 0.0


class QualityGateValidator:
    """
    Validates code quality through configurable gates.

    Executes gates in linear order:
    1. Tests (must pass first)
    2. Coverage (depends on tests)
    3. Quality (complexity + linting)
    4. Security (vulnerability scanning)
    5. Documentation (docstring coverage)
    """

    def __init__(self, config_path: Optional[Path] = None, strict_mode: bool = False):
        """
        Initialize quality gate validator.

        Args:
            config_path: Path to quality-gates.yaml config file
            strict_mode: If True, warnings become failures
        """
        self.config_path = config_path or Path(".claude/quality-gates.yaml")
        self.strict_mode = strict_mode
        self.config = self._load_config()
        self.project_root = Path.cwd()
        self.reports_dir = self.project_root / self.config["settings"]["paths"]["reports_dir"]
        self.reports_dir.mkdir(parents=True, exist_ok=True)

    def _load_config(self) -> Dict[str, Any]:
        """Load and validate configuration."""
        if not self.config_path.exists():
            raise FileNotFoundError(f"Config file not found: {self.config_path}")

        with open(self.config_path) as f:
            config = yaml.safe_load(f)

        logger.info(f"Loaded quality gates config from {self.config_path}")
        return config

    def execute_all_gates(self) -> QualityGateReport:
        """
        Execute all enabled gates in linear order.

        Returns:
            QualityGateReport with aggregated results
        """
        import time

        start_time = time.time()
        results = []

        # Get gates sorted by execution order
        gates = self._get_ordered_gates()

        logger.info(f"Executing {len(gates)} quality gates in linear order")

        for gate_name, gate_config in gates:
            if not gate_config.get("enabled", True):
                logger.info(f"Gate '{gate_name}' is disabled, skipping")
                results.append(
                    GateResult(
                        gate_name=gate_name,
                        status=GateStatus.SKIPPED,
                        message="Gate is disabled in configuration",
                    )
                )
                continue

            # Check dependencies
            dependencies = gate_config.get("depends_on", [])
            if not self._check_dependencies(dependencies, results):
                logger.warning(f"Gate '{gate_name}' dependencies not met, skipping")
                results.append(
                    GateResult(
                        gate_name=gate_name,
                        status=GateStatus.SKIPPED,
                        message="Dependencies not satisfied",
                    )
                )
                continue

            # Execute gate
            logger.info(f"Executing gate: {gate_name}")
            result = self._execute_gate(gate_name, gate_config)
            results.append(result)

            # Check if we should fail fast
            if self._should_fail_fast(result):
                logger.error(f"Gate '{gate_name}' failed, stopping execution (fail-fast enabled)")
                break

        execution_time = time.time() - start_time

        # Build report
        report = self._build_report(results, execution_time)

        # Export results
        self._export_results(report)

        return report

    def _get_ordered_gates(self) -> List[Tuple[str, Dict[str, Any]]]:
        """Get gates sorted by execution order."""
        gates = self.config.get("gates", {})
        return sorted(gates.items(), key=lambda x: x[1].get("order", 999))

    def _check_dependencies(self, dependencies: List[str], results: List[GateResult]) -> bool:
        """Check if all dependencies passed."""
        if not dependencies:
            return True

        completed_gates = {r.gate_name: r.status for r in results}

        for dep in dependencies:
            if dep not in completed_gates:
                return False
            if completed_gates[dep] == GateStatus.FAILURE:
                return False

        return True

    def _execute_gate(self, gate_name: str, gate_config: Dict[str, Any]) -> GateResult:
        """Execute a single quality gate."""
        try:
            if gate_name == "tests":
                return self._execute_tests_gate(gate_config)
            elif gate_name == "coverage":
                return self._execute_coverage_gate(gate_config)
            elif gate_name == "quality":
                return self._execute_quality_gate(gate_config)
            elif gate_name == "security":
                return self._execute_security_gate(gate_config)
            elif gate_name == "documentation":
                return self._execute_documentation_gate(gate_config)
            else:
                return GateResult(
                    gate_name=gate_name,
                    status=GateStatus.ERROR,
                    message=f"Unknown gate type: {gate_name}",
                )
        except Exception as e:
            logger.exception(f"Error executing gate '{gate_name}'")
            return GateResult(
                gate_name=gate_name,
                status=GateStatus.ERROR,
                message=f"Execution error: {str(e)}",
                errors=[str(e)],
            )

    def _execute_tests_gate(self, config: Dict[str, Any]) -> GateResult:
        """Execute tests gate."""
        command = config.get("command", "pytest --maxfail=1 -x")

        try:
            result = subprocess.run(
                command.split(),
                capture_output=True,
                text=True,
                timeout=300,
            )

            if result.returncode == 0:
                return GateResult(
                    gate_name="tests",
                    status=GateStatus.PASS,
                    message="All tests passed",
                    metrics={"exit_code": 0},
                )
            else:
                return GateResult(
                    gate_name="tests",
                    status=GateStatus.FAILURE,
                    message="Some tests failed",
                    metrics={"exit_code": result.returncode},
                    errors=[result.stderr] if result.stderr else [],
                )
        except subprocess.TimeoutExpired:
            return GateResult(
                gate_name="tests",
                status=GateStatus.ERROR,
                message="Test execution timed out",
                errors=["Timeout after 300 seconds"],
            )
        except Exception as e:
            return GateResult(
                gate_name="tests",
                status=GateStatus.ERROR,
                message=f"Test execution failed: {str(e)}",
                errors=[str(e)],
            )

    def _execute_coverage_gate(self, config: Dict[str, Any]) -> GateResult:
        """Execute coverage gate."""
        coverage_file = self.project_root / config.get("output_file", "coverage.json")

        if not coverage_file.exists():
            return GateResult(
                gate_name="coverage",
                status=GateStatus.ERROR,
                message=f"Coverage file not found: {coverage_file}",
                errors=[f"Expected coverage report at {coverage_file}"],
            )

        try:
            with open(coverage_file) as f:
                coverage_data = json.load(f)

            # Extract total coverage percentage
            total_coverage = coverage_data.get("totals", {}).get("percent_covered", 0.0)

            thresholds = config.get("thresholds", {})
            failure_threshold = thresholds.get("failure", 60.0)
            warning_threshold = thresholds.get("warning", 80.0)

            # Determine status
            if total_coverage < failure_threshold:
                status = GateStatus.FAILURE
                message = f"Coverage {total_coverage:.2f}% is below failure threshold ({failure_threshold}%)"
            elif total_coverage < warning_threshold:
                status = GateStatus.WARNING
                message = f"Coverage {total_coverage:.2f}% is below warning threshold ({warning_threshold}%)"
            else:
                status = GateStatus.PASS
                message = f"Coverage {total_coverage:.2f}% meets requirements"

            return GateResult(
                gate_name="coverage",
                status=status,
                message=message,
                metrics={
                    "coverage_percent": total_coverage,
                    "lines_covered": coverage_data.get("totals", {}).get("covered_lines", 0),
                    "lines_total": coverage_data.get("totals", {}).get("num_statements", 0),
                },
            )

        except Exception as e:
            return GateResult(
                gate_name="coverage",
                status=GateStatus.ERROR,
                message=f"Failed to read coverage data: {str(e)}",
                errors=[str(e)],
            )

    def _execute_quality_gate(self, config: Dict[str, Any]) -> GateResult:
        """Execute code quality gate (Ruff linting + complexity)."""
        tools = config.get("tools", {})
        ruff_config = tools.get("ruff", {})

        if not ruff_config.get("enabled", True):
            return GateResult(
                gate_name="quality",
                status=GateStatus.SKIPPED,
                message="Ruff checking is disabled",
            )

        try:
            # Run ruff check
            result = subprocess.run(
                ["ruff", "check", "src/", "--output-format=json"],
                capture_output=True,
                text=True,
                timeout=120,
            )

            issues = []
            if result.stdout:
                try:
                    issues = json.loads(result.stdout)
                except json.JSONDecodeError:
                    pass

            # Analyze complexity issues
            complexity_threshold = ruff_config.get("complexity_threshold", {})
            warning_complexity = complexity_threshold.get("warning", 10)
            failure_complexity = complexity_threshold.get("failure", 15)

            high_complexity_issues = [
                issue
                for issue in issues
                if "C901" in issue.get("code", "")  # McCabe complexity code
            ]

            max_complexity = 0
            for issue in high_complexity_issues:
                # Extract complexity from message (e.g., "C901 'function' is too complex (15)")
                msg = issue.get("message", "")
                if "(" in msg and ")" in msg:
                    try:
                        complexity = int(msg.split("(")[-1].split(")")[0])
                        max_complexity = max(max_complexity, complexity)
                    except ValueError:
                        pass

            # Determine status
            total_issues = len(issues)

            if max_complexity >= failure_complexity:
                status = GateStatus.FAILURE
                message = f"Code complexity {max_complexity} exceeds failure threshold ({failure_complexity})"
            elif max_complexity >= warning_complexity:
                status = GateStatus.WARNING
                message = f"Code complexity {max_complexity} exceeds warning threshold ({warning_complexity})"
            elif total_issues > 0:
                status = GateStatus.WARNING
                message = f"Found {total_issues} linting issues"
            else:
                status = GateStatus.PASS
                message = "No code quality issues found"

            return GateResult(
                gate_name="quality",
                status=status,
                message=message,
                metrics={
                    "total_issues": total_issues,
                    "max_complexity": max_complexity,
                    "complexity_issues": len(high_complexity_issues),
                },
                details={"issues": issues[:10]},  # First 10 issues only
            )

        except subprocess.TimeoutExpired:
            return GateResult(
                gate_name="quality",
                status=GateStatus.ERROR,
                message="Ruff check timed out",
                errors=["Timeout after 120 seconds"],
            )
        except Exception as e:
            return GateResult(
                gate_name="quality",
                status=GateStatus.ERROR,
                message=f"Quality check failed: {str(e)}",
                errors=[str(e)],
            )

    def _execute_security_gate(self, config: Dict[str, Any]) -> GateResult:
        """Execute security gate (Bandit scanning)."""
        tools = config.get("tools", {})
        bandit_config = tools.get("bandit", {})

        if not bandit_config.get("enabled", True):
            return GateResult(
                gate_name="security",
                status=GateStatus.SKIPPED,
                message="Bandit scanning is disabled",
            )

        try:
            output_file = self.reports_dir / "bandit_report.json"

            # Run bandit
            result = subprocess.run(
                [
                    "bandit",
                    "-r",
                    "src/",
                    "-f",
                    "json",
                    "-o",
                    str(output_file),
                ],
                capture_output=True,
                text=True,
                timeout=120,
            )

            # Bandit returns non-zero if issues found, but that's expected
            if not output_file.exists():
                return GateResult(
                    gate_name="security",
                    status=GateStatus.ERROR,
                    message="Bandit report not generated",
                    errors=["Output file not created"],
                )

            with open(output_file) as f:
                bandit_data = json.load(f)

            # Count issues by severity
            results = bandit_data.get("results", [])
            high_severity = sum(1 for r in results if r.get("issue_severity") == "HIGH")
            medium_severity = sum(1 for r in results if r.get("issue_severity") == "MEDIUM")
            low_severity = sum(1 for r in results if r.get("issue_severity") == "LOW")

            # Determine status based on thresholds
            severity_threshold = bandit_config.get("severity_threshold", {})

            if high_severity > 0 and severity_threshold.get("high") == "failure":
                status = GateStatus.FAILURE
                message = f"Found {high_severity} high severity security issues"
            elif medium_severity > 0 and severity_threshold.get("medium") == "warning":
                status = GateStatus.WARNING
                message = f"Found {medium_severity} medium severity security issues"
            elif low_severity > 0:
                status = GateStatus.PASS
                message = f"Only {low_severity} low severity issues found"
            else:
                status = GateStatus.PASS
                message = "No security issues detected"

            return GateResult(
                gate_name="security",
                status=status,
                message=message,
                metrics={
                    "high_severity": high_severity,
                    "medium_severity": medium_severity,
                    "low_severity": low_severity,
                    "total_issues": len(results),
                },
                details={"report_file": str(output_file)},
            )

        except subprocess.TimeoutExpired:
            return GateResult(
                gate_name="security",
                status=GateStatus.ERROR,
                message="Bandit scan timed out",
                errors=["Timeout after 120 seconds"],
            )
        except Exception as e:
            return GateResult(
                gate_name="security",
                status=GateStatus.ERROR,
                message=f"Security scan failed: {str(e)}",
                errors=[str(e)],
            )

    def _execute_documentation_gate(self, config: Dict[str, Any]) -> GateResult:
        """Execute documentation coverage gate."""
        threshold = config.get("threshold", 75.0)
        scan_paths = config.get("scan_paths", ["src/digitalmodel"])
        exclude_paths = config.get("exclude_paths", [])

        try:
            total_functions = 0
            documented_functions = 0

            for scan_path in scan_paths:
                path = self.project_root / scan_path
                if not path.exists():
                    continue

                for py_file in path.rglob("*.py"):
                    # Check exclusions
                    if any(py_file.match(pattern) for pattern in exclude_paths):
                        continue

                    try:
                        funcs, docs = self._count_docstrings(py_file)
                        total_functions += funcs
                        documented_functions += docs
                    except Exception as e:
                        logger.debug(f"Failed to parse {py_file}: {e}")
                        continue

            # Calculate coverage
            if total_functions == 0:
                return GateResult(
                    gate_name="documentation",
                    status=GateStatus.PASS,
                    message="No functions found to document",
                    metrics={"total_functions": 0, "documented_functions": 0, "coverage_percent": 100.0},
                )

            coverage_percent = (documented_functions / total_functions) * 100

            # Determine status
            if coverage_percent < threshold:
                status = GateStatus.WARNING  # Documentation failures are warnings only
                message = f"Documentation coverage {coverage_percent:.1f}% is below threshold ({threshold}%)"
            else:
                status = GateStatus.PASS
                message = f"Documentation coverage {coverage_percent:.1f}% meets requirements"

            return GateResult(
                gate_name="documentation",
                status=status,
                message=message,
                metrics={
                    "total_functions": total_functions,
                    "documented_functions": documented_functions,
                    "coverage_percent": coverage_percent,
                },
            )

        except Exception as e:
            return GateResult(
                gate_name="documentation",
                status=GateStatus.ERROR,
                message=f"Documentation check failed: {str(e)}",
                errors=[str(e)],
            )

    def _count_docstrings(self, file_path: Path) -> Tuple[int, int]:
        """
        Count functions and documented functions in a Python file.

        Returns:
            Tuple of (total_functions, documented_functions)
        """
        with open(file_path, encoding="utf-8") as f:
            try:
                tree = ast.parse(f.read())
            except SyntaxError:
                return 0, 0

        total_functions = 0
        documented_functions = 0

        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                total_functions += 1

                # Check if function has docstring
                if (
                    node.body
                    and isinstance(node.body[0], ast.Expr)
                    and isinstance(node.body[0].value, ast.Constant)
                    and isinstance(node.body[0].value.value, str)
                ):
                    documented_functions += 1

        return total_functions, documented_functions

    def _should_fail_fast(self, result: GateResult) -> bool:
        """Check if we should stop execution after this result."""
        fail_fast = self.config["settings"].get("pre_commit", {}).get("fail_fast", False)

        if not fail_fast:
            return False

        # In strict mode, warnings also cause fail-fast
        if self.strict_mode and result.status == GateStatus.WARNING:
            return True

        return result.status == GateStatus.FAILURE

    def _build_report(self, results: List[GateResult], execution_time: float) -> QualityGateReport:
        """Build aggregated report from gate results."""
        status_counts = {
            GateStatus.PASS: 0,
            GateStatus.WARNING: 0,
            GateStatus.FAILURE: 0,
            GateStatus.SKIPPED: 0,
            GateStatus.ERROR: 0,
        }

        for result in results:
            status_counts[result.status] += 1

        # Determine overall status
        if status_counts[GateStatus.FAILURE] > 0 or status_counts[GateStatus.ERROR] > 0:
            overall_status = GateStatus.FAILURE
        elif status_counts[GateStatus.WARNING] > 0:
            # In strict mode, warnings become failures
            overall_status = GateStatus.FAILURE if self.strict_mode else GateStatus.WARNING
        else:
            overall_status = GateStatus.PASS

        return QualityGateReport(
            overall_status=overall_status,
            gates_executed=len(results),
            gates_passed=status_counts[GateStatus.PASS],
            gates_warned=status_counts[GateStatus.WARNING],
            gates_failed=status_counts[GateStatus.FAILURE],
            gates_skipped=status_counts[GateStatus.SKIPPED],
            results=results,
            execution_time=execution_time,
        )

    def _export_results(self, report: QualityGateReport) -> None:
        """Export results to JSON file."""
        if not self.config["settings"]["ci_cd"].get("export_results", True):
            return

        result_file = self.config["settings"]["ci_cd"]["result_file"]
        # Handle both absolute and relative paths
        if "/" in result_file or "\\" in result_file:
            # Relative path like "reports/quality_gates_results.json"
            output_file = self.project_root / result_file
        else:
            # Just filename
            output_file = self.reports_dir / result_file

        # Convert to serializable dict
        data = {
            "overall_status": report.overall_status.value,
            "gates_executed": report.gates_executed,
            "gates_passed": report.gates_passed,
            "gates_warned": report.gates_warned,
            "gates_failed": report.gates_failed,
            "gates_skipped": report.gates_skipped,
            "execution_time": report.execution_time,
            "results": [
                {
                    "gate_name": r.gate_name,
                    "status": r.status.value,
                    "message": r.message,
                    "metrics": r.metrics,
                    "details": r.details,
                    "errors": r.errors,
                }
                for r in report.results
            ],
        }

        with open(output_file, "w") as f:
            json.dump(data, f, indent=2)

        logger.info(f"Quality gate results exported to {output_file}")

    def print_report(self, report: QualityGateReport) -> None:
        """Print formatted console report."""
        # Color codes
        RESET = "\033[0m"
        RED = "\033[91m"
        YELLOW = "\033[93m"
        GREEN = "\033[92m"
        BLUE = "\033[94m"
        BOLD = "\033[1m"

        print("\n" + "=" * 80)
        print(f"{BOLD}Quality Gates Report{RESET}")
        print("=" * 80)

        # Overall status
        status_color = {
            GateStatus.PASS: GREEN,
            GateStatus.WARNING: YELLOW,
            GateStatus.FAILURE: RED,
        }[report.overall_status]

        print(f"\n{BOLD}Overall Status:{RESET} {status_color}{report.overall_status.value.upper()}{RESET}")
        print(f"{BOLD}Execution Time:{RESET} {report.execution_time:.2f}s")
        print(f"\n{BOLD}Summary:{RESET}")
        print(f"  ✓ Passed:  {GREEN}{report.gates_passed}{RESET}")
        print(f"  ⚠ Warned:  {YELLOW}{report.gates_warned}{RESET}")
        print(f"  ✗ Failed:  {RED}{report.gates_failed}{RESET}")
        print(f"  ○ Skipped: {BLUE}{report.gates_skipped}{RESET}")

        print(f"\n{BOLD}Gate Results:{RESET}")
        print("-" * 80)

        for result in report.results:
            # Status icon and color
            if result.status == GateStatus.PASS:
                icon = "✓"
                color = GREEN
            elif result.status == GateStatus.WARNING:
                icon = "⚠"
                color = YELLOW
            elif result.status == GateStatus.FAILURE:
                icon = "✗"
                color = RED
            elif result.status == GateStatus.SKIPPED:
                icon = "○"
                color = BLUE
            else:
                icon = "!"
                color = RED

            print(f"\n{color}{icon} {result.gate_name.upper()}{RESET}")
            print(f"  {result.message}")

            # Print metrics if available
            if result.metrics and self.config["settings"]["cli"].get("show_details", True):
                print(f"  Metrics:")
                for key, value in result.metrics.items():
                    if isinstance(value, float):
                        print(f"    • {key}: {value:.2f}")
                    else:
                        print(f"    • {key}: {value}")

            # Print errors if any
            if result.errors:
                print(f"  {RED}Errors:{RESET}")
                for error in result.errors[:3]:  # Max 3 errors
                    print(f"    • {error}")

        print("\n" + "=" * 80 + "\n")
