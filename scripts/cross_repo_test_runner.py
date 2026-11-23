#!/usr/bin/env python3
# ABOUTME: Cross-repository test runner with parallel execution and HTML reporting
# Discovers and tests all repositories in workspace-hub with aggregated results

"""
Cross-Repository Test Runner

Automated testing infrastructure for all 26 repositories in workspace-hub:
- Auto-discovers repositories with pyproject.toml and tests/
- Runs pytest in isolated uv environments (parallel execution)
- Aggregates test results and compliance checks
- Generates interactive HTML dashboard with Plotly
- Exports results to CSV and JSON

Usage:
    python scripts/cross_repo_test_runner.py
    python scripts/cross_repo_test_runner.py --sequential
    python scripts/cross_repo_test_runner.py --workers 8
"""

import argparse
import json
import subprocess
import sys
import time
from concurrent.futures import ProcessPoolExecutor, TimeoutError, as_completed
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd
import plotly.graph_objects as go
import yaml
from plotly.subplots import make_subplots


class RepositoryDiscovery:
    """Discover repositories in workspace-hub."""

    def __init__(self, scan_path: Path, config: Dict):
        """
        Initialize repository discovery.

        Args:
            scan_path: Root path to scan for repositories
            config: Configuration dictionary
        """
        self.scan_path = Path(scan_path)
        self.config = config
        self.exclude = set(config.get("repositories", {}).get("exclude", []))

    def discover(self) -> List[Path]:
        """
        Discover all repositories with pyproject.toml AND tests/ directory.

        Returns:
            List of repository paths
        """
        repositories = []

        if not self.scan_path.exists():
            print(f"Warning: Scan path does not exist: {self.scan_path}")
            return repositories

        for item in self.scan_path.iterdir():
            if not item.is_dir():
                continue

            if item.name in self.exclude or item.name.startswith("."):
                continue

            # Check for pyproject.toml AND tests/ directory
            has_pyproject = (item / "pyproject.toml").exists()
            has_tests = (item / "tests").exists() and (item / "tests").is_dir()

            if has_pyproject and has_tests:
                repositories.append(item)

        return sorted(repositories)


class RepositoryTester:
    """Run tests for a single repository."""

    def __init__(self, repo_path: Path, config: Dict, timeout: int = 300):
        """
        Initialize repository tester.

        Args:
            repo_path: Path to repository
            config: Configuration dictionary
            timeout: Timeout in seconds
        """
        self.repo_path = repo_path
        self.config = config
        self.timeout = timeout

    def run_tests(self) -> Dict:
        """
        Run pytest in isolated uv environment.

        Returns:
            Test results dictionary
        """
        result = {
            "repo_name": self.repo_path.name,
            "repo_path": str(self.repo_path),
            "timestamp": datetime.now().isoformat(),
            "status": "unknown",
            "tests_passed": 0,
            "tests_failed": 0,
            "tests_skipped": 0,
            "tests_total": 0,
            "duration_seconds": 0.0,
            "coverage_percent": None,
            "error_message": None,
            "warnings": []
        }

        # Check for required files
        if not (self.repo_path / "pyproject.toml").exists():
            result["status"] = "skipped"
            result["warnings"].append("No pyproject.toml found")
            return result

        if not (self.repo_path / "tests").exists():
            result["status"] = "skipped"
            result["warnings"].append("No tests/ directory found")
            return result

        start_time = time.time()

        try:
            # Run pytest via uv in the repository directory
            cmd = [
                "uv",
                "run",
                "pytest",
                "--tb=short",
                "--quiet",
                "--json-report",
                "--json-report-file=.pytest-report.json"
            ]

            # Add coverage if pytest-cov is available
            if self._has_pytest_cov():
                cmd.extend(["--cov", "--cov-report=json"])

            process = subprocess.run(
                cmd,
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                timeout=self.timeout
            )

            duration = time.time() - start_time
            result["duration_seconds"] = round(duration, 2)

            # Parse pytest JSON report
            report_path = self.repo_path / ".pytest-report.json"
            if report_path.exists():
                with open(report_path, "r") as f:
                    pytest_report = json.load(f)

                summary = pytest_report.get("summary", {})
                result["tests_passed"] = summary.get("passed", 0)
                result["tests_failed"] = summary.get("failed", 0)
                result["tests_skipped"] = summary.get("skipped", 0)
                result["tests_total"] = summary.get("total", 0)

                # Clean up report file
                report_path.unlink()

            # Parse coverage report if available
            coverage_path = self.repo_path / "coverage.json"
            if coverage_path.exists():
                with open(coverage_path, "r") as f:
                    coverage_report = json.load(f)
                result["coverage_percent"] = round(
                    coverage_report.get("totals", {}).get("percent_covered", 0), 2
                )
                coverage_path.unlink()

            # Determine status
            if process.returncode == 0:
                result["status"] = "passed"
            elif process.returncode == 5:  # No tests collected
                result["status"] = "no_tests"
                result["warnings"].append("No tests collected")
            else:
                result["status"] = "failed"
                result["error_message"] = process.stderr[:500] if process.stderr else None

        except subprocess.TimeoutExpired:
            result["status"] = "timeout"
            result["duration_seconds"] = self.timeout
            result["error_message"] = f"Tests exceeded {self.timeout}s timeout"

        except Exception as e:
            result["status"] = "error"
            result["duration_seconds"] = time.time() - start_time
            result["error_message"] = str(e)[:500]

        return result

    def _has_pytest_cov(self) -> bool:
        """Check if pytest-cov is available in the repository."""
        try:
            result = subprocess.run(
                ["uv", "run", "python", "-c", "import pytest_cov"],
                cwd=self.repo_path,
                capture_output=True,
                timeout=5
            )
            return result.returncode == 0
        except Exception:
            return False


class ComplianceTester:
    """Run compliance tests for repositories."""

    def __init__(self, config: Dict):
        """
        Initialize compliance tester.

        Args:
            config: Configuration dictionary
        """
        self.config = config

    def run_compliance_checks(self, repo_path: Path) -> Dict:
        """
        Run compliance checks for a repository.

        Args:
            repo_path: Path to repository

        Returns:
            Compliance check results
        """
        # Import the compliance checker
        sys.path.insert(0, str(Path(__file__).parent.parent / "tests" / "cross_repo"))
        from test_standards_compliance import test_repository_compliance

        return test_repository_compliance(repo_path, self.config)


def run_single_repository(args: Tuple[Path, Dict, int]) -> Tuple[Dict, Dict]:
    """
    Run tests and compliance checks for a single repository.

    Args:
        args: Tuple of (repo_path, config, timeout)

    Returns:
        Tuple of (test_results, compliance_results)
    """
    repo_path, config, timeout = args

    # Run tests
    tester = RepositoryTester(repo_path, config, timeout)
    test_results = tester.run_tests()

    # Run compliance checks
    compliance_tester = ComplianceTester(config)
    compliance_results = compliance_tester.run_compliance_checks(repo_path)

    return test_results, compliance_results


class HTMLReportGenerator:
    """Generate interactive HTML dashboard with Plotly."""

    def __init__(self, output_dir: Path):
        """
        Initialize report generator.

        Args:
            output_dir: Directory to save reports
        """
        self.output_dir = output_dir
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def generate(
        self,
        test_results: List[Dict],
        compliance_results: List[Dict],
        timestamp: str
    ) -> Path:
        """
        Generate HTML dashboard.

        Args:
            test_results: List of test result dictionaries
            compliance_results: List of compliance result dictionaries
            timestamp: Timestamp string

        Returns:
            Path to generated HTML file
        """
        # Create subplots
        fig = make_subplots(
            rows=3,
            cols=2,
            subplot_titles=(
                "Test Results by Repository",
                "Test Execution Time",
                "Coverage Distribution",
                "Compliance Scores",
                "Test Status Summary",
                "Top Issues by Repository"
            ),
            specs=[
                [{"type": "bar"}, {"type": "bar"}],
                [{"type": "box"}, {"type": "bar"}],
                [{"type": "pie"}, {"type": "bar"}]
            ],
            vertical_spacing=0.12,
            horizontal_spacing=0.15
        )

        # Prepare data
        df_tests = pd.DataFrame(test_results)
        df_compliance = pd.DataFrame(compliance_results)

        # 1. Test Results by Repository (Stacked Bar)
        repo_names = df_tests["repo_name"].tolist()
        passed = df_tests["tests_passed"].tolist()
        failed = df_tests["tests_failed"].tolist()
        skipped = df_tests["tests_skipped"].tolist()

        fig.add_trace(
            go.Bar(name="Passed", x=repo_names, y=passed, marker_color="green"),
            row=1, col=1
        )
        fig.add_trace(
            go.Bar(name="Failed", x=repo_names, y=failed, marker_color="red"),
            row=1, col=1
        )
        fig.add_trace(
            go.Bar(name="Skipped", x=repo_names, y=skipped, marker_color="orange"),
            row=1, col=1
        )

        # 2. Test Execution Time
        fig.add_trace(
            go.Bar(
                x=df_tests["repo_name"],
                y=df_tests["duration_seconds"],
                marker_color="steelblue",
                name="Duration (s)"
            ),
            row=1, col=2
        )

        # 3. Coverage Distribution (Box Plot)
        coverage_data = df_tests[df_tests["coverage_percent"].notna()]["coverage_percent"]
        if not coverage_data.empty:
            fig.add_trace(
                go.Box(
                    y=coverage_data,
                    name="Coverage %",
                    marker_color="lightblue",
                    boxmean="sd"
                ),
                row=2, col=1
            )

        # 4. Compliance Scores
        fig.add_trace(
            go.Bar(
                x=df_compliance["repo_name"],
                y=df_compliance["overall_score"],
                marker_color="teal",
                name="Compliance %"
            ),
            row=2, col=2
        )

        # 5. Test Status Summary (Pie Chart)
        status_counts = df_tests["status"].value_counts()
        fig.add_trace(
            go.Pie(
                labels=status_counts.index,
                values=status_counts.values,
                name="Status"
            ),
            row=3, col=1
        )

        # 6. Top Issues by Repository
        issue_counts = []
        for _, row in df_compliance.iterrows():
            issue_count = len(row.get("suggestions", []))
            issue_counts.append(issue_count)

        df_compliance["issue_count"] = issue_counts
        top_issues = df_compliance.nlargest(10, "issue_count")

        fig.add_trace(
            go.Bar(
                x=top_issues["repo_name"],
                y=top_issues["issue_count"],
                marker_color="coral",
                name="Issues"
            ),
            row=3, col=2
        )

        # Update layout
        fig.update_layout(
            title_text=f"Cross-Repository Test Results - {timestamp}",
            showlegend=True,
            height=1200,
            barmode="stack"
        )

        fig.update_xaxes(tickangle=-45)

        # Save HTML
        html_path = self.output_dir / f"dashboard-{timestamp}.html"
        fig.write_html(str(html_path))

        return html_path


class ResultsExporter:
    """Export results to CSV and JSON."""

    def __init__(self, output_dir: Path):
        """
        Initialize exporter.

        Args:
            output_dir: Directory to save exports
        """
        self.output_dir = output_dir
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def export_csv(
        self,
        test_results: List[Dict],
        compliance_results: List[Dict],
        timestamp: str
    ) -> Path:
        """
        Export results to CSV.

        Args:
            test_results: List of test result dictionaries
            compliance_results: List of compliance result dictionaries
            timestamp: Timestamp string

        Returns:
            Path to CSV file
        """
        # Merge test and compliance results
        df_tests = pd.DataFrame(test_results)
        df_compliance = pd.DataFrame(compliance_results)

        # Extract compliance scores
        compliance_scores = []
        for _, row in df_compliance.iterrows():
            checks = row.get("checks", {})
            compliance_scores.append({
                "repo_name": row["repo_name"],
                "compliance_score": row["overall_score"],
                "module_structure_ok": checks.get("module_structure", {}).get("passed", False),
                "file_organization_ok": checks.get("file_organization", {}).get("passed", False),
                "claude_md_ok": checks.get("claude_md", {}).get("passed", False),
                "agent_registry_ok": checks.get("agent_registry", {}).get("passed", False),
                "suggestion_count": len(row.get("suggestions", []))
            })

        df_compliance_flat = pd.DataFrame(compliance_scores)

        # Merge dataframes
        df_merged = pd.merge(
            df_tests,
            df_compliance_flat,
            on="repo_name",
            how="outer"
        )

        # Save to CSV
        csv_path = self.output_dir / f"results-{timestamp}.csv"
        df_merged.to_csv(csv_path, index=False)

        return csv_path

    def export_json(
        self,
        test_results: List[Dict],
        compliance_results: List[Dict],
        timestamp: str
    ) -> Path:
        """
        Export results to JSON.

        Args:
            test_results: List of test result dictionaries
            compliance_results: List of compliance result dictionaries
            timestamp: Timestamp string

        Returns:
            Path to JSON file
        """
        combined_results = {
            "timestamp": timestamp,
            "test_results": test_results,
            "compliance_results": compliance_results,
            "summary": {
                "total_repositories": len(test_results),
                "tests_passed_repos": sum(1 for r in test_results if r["status"] == "passed"),
                "tests_failed_repos": sum(1 for r in test_results if r["status"] == "failed"),
                "average_compliance_score": sum(
                    r["overall_score"] for r in compliance_results
                ) / len(compliance_results) if compliance_results else 0.0,
                "total_tests_run": sum(r["tests_total"] for r in test_results),
                "total_tests_passed": sum(r["tests_passed"] for r in test_results),
                "total_tests_failed": sum(r["tests_failed"] for r in test_results)
            }
        }

        json_path = self.output_dir / f"test-results-{timestamp}.json"
        with open(json_path, "w", encoding="utf-8") as f:
            json.dump(combined_results, indent=2, fp=f)

        return json_path


def load_config() -> Dict:
    """Load configuration from YAML file."""
    config_path = Path(__file__).parent.parent / "config" / "cross-repo-test-config.yaml"

    if not config_path.exists():
        raise FileNotFoundError(f"Configuration file not found: {config_path}")

    with open(config_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Cross-repository test runner for workspace-hub"
    )
    parser.add_argument(
        "--sequential",
        action="store_true",
        help="Run tests sequentially instead of parallel"
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=4,
        help="Number of parallel workers (default: 4)"
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=300,
        help="Timeout per repository in seconds (default: 300)"
    )

    args = parser.parse_args()

    # Load configuration
    print("Loading configuration...")
    config = load_config()

    # Discover repositories
    print("Discovering repositories...")
    scan_path = Path(config["repositories"]["scan_path"])
    discovery = RepositoryDiscovery(scan_path, config)
    repositories = discovery.discover()

    print(f"Found {len(repositories)} repositories with tests")

    if not repositories:
        print("No repositories found. Exiting.")
        return 1

    # Run tests
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    test_results = []
    compliance_results = []

    if args.sequential or args.workers == 1:
        print("Running tests sequentially...")
        for i, repo in enumerate(repositories, 1):
            print(f"[{i}/{len(repositories)}] Testing {repo.name}...")
            test_result, compliance_result = run_single_repository(
                (repo, config, args.timeout)
            )
            test_results.append(test_result)
            compliance_results.append(compliance_result)
    else:
        print(f"Running tests in parallel with {args.workers} workers...")
        with ProcessPoolExecutor(max_workers=args.workers) as executor:
            futures = {
                executor.submit(
                    run_single_repository,
                    (repo, config, args.timeout)
                ): repo
                for repo in repositories
            }

            for i, future in enumerate(as_completed(futures), 1):
                repo = futures[future]
                try:
                    test_result, compliance_result = future.result()
                    test_results.append(test_result)
                    compliance_results.append(compliance_result)
                    print(f"[{i}/{len(repositories)}] Completed {repo.name} - {test_result['status']}")
                except Exception as e:
                    print(f"[{i}/{len(repositories)}] Error in {repo.name}: {str(e)}")

    # Generate reports
    print("\nGenerating reports...")
    output_dir = Path(__file__).parent.parent / config["reporting"]["output_dir"]

    # HTML Dashboard
    if config["reporting"]["html_dashboard"]:
        report_gen = HTMLReportGenerator(output_dir)
        html_path = report_gen.generate(test_results, compliance_results, timestamp)
        print(f"HTML dashboard: {html_path}")

    # CSV Export
    if config["reporting"]["csv_export"]:
        exporter = ResultsExporter(output_dir)
        csv_path = exporter.export_csv(test_results, compliance_results, timestamp)
        print(f"CSV results: {csv_path}")

    # JSON Export
    if config["reporting"]["json_results"]:
        exporter = ResultsExporter(output_dir)
        json_path = exporter.export_json(test_results, compliance_results, timestamp)
        print(f"JSON results: {json_path}")

    # Print summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Total repositories tested: {len(test_results)}")
    print(f"Passed: {sum(1 for r in test_results if r['status'] == 'passed')}")
    print(f"Failed: {sum(1 for r in test_results if r['status'] == 'failed')}")
    print(f"Skipped: {sum(1 for r in test_results if r['status'] == 'skipped')}")
    print(f"Errors: {sum(1 for r in test_results if r['status'] == 'error')}")
    print(f"Average compliance score: {sum(r['overall_score'] for r in compliance_results) / len(compliance_results):.1f}%")

    return 0


if __name__ == "__main__":
    sys.exit(main())
