#!/usr/bin/env python3
"""
Test Quality Metrics Dashboard Generator

This script generates comprehensive metrics about the testing infrastructure
and creates reports for monitoring test quality over time.
"""

import json
import argparse
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional
import subprocess
import xml.etree.ElementTree as ET


class TestMetricsCollector:
    """Collect and analyze test metrics."""

    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.reports_dir = project_root / "reports"
        self.metrics = {}

    def collect_coverage_metrics(self) -> Dict[str, Any]:
        """Collect code coverage metrics."""
        coverage_xml = self.reports_dir / "coverage.xml"
        if not coverage_xml.exists():
            return {"error": "Coverage report not found"}

        try:
            tree = ET.parse(coverage_xml)
            root = tree.getroot()

            # Extract overall coverage
            coverage_attr = root.attrib
            line_rate = float(coverage_attr.get('line-rate', 0))
            branch_rate = float(coverage_attr.get('branch-rate', 0))

            # Extract package-level coverage
            packages = []
            for package in root.findall('.//package'):
                pkg_name = package.get('name', 'unknown')
                pkg_line_rate = float(package.get('line-rate', 0))
                pkg_branch_rate = float(package.get('branch-rate', 0))

                packages.append({
                    'name': pkg_name,
                    'line_coverage': pkg_line_rate * 100,
                    'branch_coverage': pkg_branch_rate * 100
                })

            return {
                'overall_line_coverage': line_rate * 100,
                'overall_branch_coverage': branch_rate * 100,
                'packages': packages,
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            return {"error": f"Failed to parse coverage report: {e}"}

    def collect_test_results(self) -> Dict[str, Any]:
        """Collect test execution results."""
        test_report = self.reports_dir / "test_report.json"
        if not test_report.exists():
            return {"error": "Test report not found"}

        try:
            with open(test_report, 'r') as f:
                data = json.load(f)

            summary = data.get('summary', {})

            return {
                'total_tests': summary.get('total', 0),
                'passed': summary.get('passed', 0),
                'failed': summary.get('failed', 0),
                'skipped': summary.get('skipped', 0),
                'errors': summary.get('error', 0),
                'duration': data.get('duration', 0),
                'success_rate': (summary.get('passed', 0) / max(summary.get('total', 1), 1)) * 100,
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            return {"error": f"Failed to parse test report: {e}"}

    def collect_benchmark_metrics(self) -> Dict[str, Any]:
        """Collect performance benchmark metrics."""
        benchmark_report = self.reports_dir / "benchmarks.json"
        if not benchmark_report.exists():
            return {"error": "Benchmark report not found"}

        try:
            with open(benchmark_report, 'r') as f:
                data = json.load(f)

            benchmarks = data.get('benchmarks', [])
            if not benchmarks:
                return {"error": "No benchmark data found"}

            # Analyze benchmark performance
            total_benchmarks = len(benchmarks)
            avg_time = sum(b.get('stats', {}).get('mean', 0) for b in benchmarks) / total_benchmarks
            slowest = max(benchmarks, key=lambda b: b.get('stats', {}).get('mean', 0))
            fastest = min(benchmarks, key=lambda b: b.get('stats', {}).get('mean', 0))

            return {
                'total_benchmarks': total_benchmarks,
                'average_time': avg_time,
                'slowest_test': {
                    'name': slowest.get('name', 'unknown'),
                    'time': slowest.get('stats', {}).get('mean', 0)
                },
                'fastest_test': {
                    'name': fastest.get('name', 'unknown'),
                    'time': fastest.get('stats', {}).get('mean', 0)
                },
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            return {"error": f"Failed to parse benchmark report: {e}"}

    def collect_mutation_metrics(self) -> Dict[str, Any]:
        """Collect mutation testing metrics."""
        mutation_report = self.reports_dir / "mutation-report.json"
        if not mutation_report.exists():
            return {"error": "Mutation report not found"}

        try:
            with open(mutation_report, 'r') as f:
                data = json.load(f)

            return {
                'total_mutants': data.get('total', 0),
                'killed_mutants': data.get('killed', 0),
                'survived_mutants': data.get('survived', 0),
                'timeout_mutants': data.get('timeout', 0),
                'mutation_score': data.get('mutation_score', 0),
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            return {"error": f"Failed to parse mutation report: {e}"}

    def collect_test_file_metrics(self) -> Dict[str, Any]:
        """Collect metrics about test files themselves."""
        tests_dir = self.project_root / "tests"

        if not tests_dir.exists():
            return {"error": "Tests directory not found"}

        # Count test files by type
        test_files = list(tests_dir.rglob("test_*.py"))
        total_test_files = len(test_files)

        # Categorize tests
        categories = {
            'unit': len(list(tests_dir.rglob("**/test_*.py"))),
            'integration': len(list((tests_dir / "integration").rglob("*.py"))) if (tests_dir / "integration").exists() else 0,
            'benchmarks': len(list((tests_dir / "benchmarks").rglob("*.py"))) if (tests_dir / "benchmarks").exists() else 0,
            'property': len(list((tests_dir / "property").rglob("*.py"))) if (tests_dir / "property").exists() else 0,
            'security': len(list((tests_dir / "security").rglob("*.py"))) if (tests_dir / "security").exists() else 0,
        }

        # Calculate lines of test code
        total_lines = 0
        for test_file in test_files:
            try:
                total_lines += len(test_file.read_text().splitlines())
            except Exception:
                continue

        return {
            'total_test_files': total_test_files,
            'total_test_lines': total_lines,
            'test_categories': categories,
            'avg_lines_per_file': total_lines / max(total_test_files, 1),
            'timestamp': datetime.now().isoformat()
        }

    def run_quick_test_analysis(self) -> Dict[str, Any]:
        """Run a quick test to get immediate metrics."""
        try:
            result = subprocess.run(
                [sys.executable, "-m", "pytest", "--collect-only", "-q"],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=30
            )

            output_lines = result.stdout.strip().split('\n')
            total_tests = 0

            for line in output_lines:
                if "test session starts" in line:
                    continue
                elif " error" in line.lower():
                    continue
                elif line.strip():
                    total_tests += 1

            return {
                'collected_tests': total_tests,
                'collection_time': 'quick',
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            return {"error": f"Failed to collect tests: {e}"}

    def collect_all_metrics(self) -> Dict[str, Any]:
        """Collect all available metrics."""
        self.reports_dir.mkdir(exist_ok=True)

        metrics = {
            'collection_timestamp': datetime.now().isoformat(),
            'project_root': str(self.project_root),
            'coverage': self.collect_coverage_metrics(),
            'test_results': self.collect_test_results(),
            'benchmarks': self.collect_benchmark_metrics(),
            'mutation_testing': self.collect_mutation_metrics(),
            'test_files': self.collect_test_file_metrics(),
            'quick_analysis': self.run_quick_test_analysis()
        }

        return metrics


class QualityDashboard:
    """Generate quality dashboard reports."""

    def __init__(self, metrics: Dict[str, Any]):
        self.metrics = metrics

    def generate_markdown_report(self) -> str:
        """Generate a markdown quality report."""
        report = []
        report.append("# 📊 Test Quality Dashboard")
        report.append("")
        report.append(f"**Generated**: {self.metrics['collection_timestamp']}")
        report.append("")

        # Test Overview
        quick_analysis = self.metrics.get('quick_analysis', {})
        test_files = self.metrics.get('test_files', {})

        if 'error' not in quick_analysis and 'error' not in test_files:
            report.append("## 🧪 Test Overview")
            report.append(f"- **Total Test Files**: {test_files.get('total_test_files', 'N/A')}")
            report.append(f"- **Collected Tests**: {quick_analysis.get('collected_tests', 'N/A')}")
            report.append(f"- **Lines of Test Code**: {test_files.get('total_test_lines', 'N/A')}")
            report.append("")

            # Test Distribution
            categories = test_files.get('test_categories', {})
            if categories:
                report.append("### Test Distribution")
                for category, count in categories.items():
                    if count > 0:
                        report.append(f"- **{category.title()}**: {count} files")
                report.append("")

        # Coverage Analysis
        coverage = self.metrics.get('coverage', {})
        if 'error' not in coverage:
            report.append("## 📈 Coverage Analysis")
            report.append(f"- **Line Coverage**: {coverage.get('overall_line_coverage', 0):.1f}%")
            report.append(f"- **Branch Coverage**: {coverage.get('overall_branch_coverage', 0):.1f}%")

            # Coverage status
            line_cov = coverage.get('overall_line_coverage', 0)
            if line_cov >= 90:
                status = "🟢 Excellent"
            elif line_cov >= 80:
                status = "🟡 Good"
            elif line_cov >= 70:
                status = "🟠 Needs Improvement"
            else:
                status = "🔴 Poor"

            report.append(f"- **Status**: {status}")
            report.append("")

        # Test Results
        test_results = self.metrics.get('test_results', {})
        if 'error' not in test_results:
            report.append("## ✅ Test Results")
            total = test_results.get('total_tests', 0)
            passed = test_results.get('passed', 0)
            failed = test_results.get('failed', 0)
            success_rate = test_results.get('success_rate', 0)

            report.append(f"- **Total Tests**: {total}")
            report.append(f"- **Passed**: {passed}")
            report.append(f"- **Failed**: {failed}")
            report.append(f"- **Success Rate**: {success_rate:.1f}%")
            report.append(f"- **Duration**: {test_results.get('duration', 0):.2f}s")
            report.append("")

        # Performance Analysis
        benchmarks = self.metrics.get('benchmarks', {})
        if 'error' not in benchmarks:
            report.append("## ⚡ Performance Analysis")
            report.append(f"- **Total Benchmarks**: {benchmarks.get('total_benchmarks', 0)}")
            report.append(f"- **Average Time**: {benchmarks.get('average_time', 0):.6f}s")

            slowest = benchmarks.get('slowest_test', {})
            fastest = benchmarks.get('fastest_test', {})

            if slowest:
                report.append(f"- **Slowest Test**: {slowest.get('name', 'N/A')} ({slowest.get('time', 0):.6f}s)")
            if fastest:
                report.append(f"- **Fastest Test**: {fastest.get('name', 'N/A')} ({fastest.get('time', 0):.6f}s)")
            report.append("")

        # Mutation Testing
        mutation = self.metrics.get('mutation_testing', {})
        if 'error' not in mutation:
            report.append("## 🧬 Mutation Testing")
            report.append(f"- **Total Mutants**: {mutation.get('total_mutants', 0)}")
            report.append(f"- **Killed**: {mutation.get('killed_mutants', 0)}")
            report.append(f"- **Survived**: {mutation.get('survived_mutants', 0)}")
            report.append(f"- **Mutation Score**: {mutation.get('mutation_score', 0):.1f}%")
            report.append("")

        # Quality Assessment
        report.append("## 🎯 Quality Assessment")

        quality_score = self._calculate_quality_score()
        if quality_score >= 90:
            status = "🏆 Gold Standard"
        elif quality_score >= 80:
            status = "🥉 Good Quality"
        elif quality_score >= 70:
            status = "⚠️ Needs Improvement"
        else:
            status = "🔴 Poor Quality"

        report.append(f"- **Overall Quality Score**: {quality_score:.1f}/100")
        report.append(f"- **Status**: {status}")
        report.append("")

        # Recommendations
        recommendations = self._generate_recommendations()
        if recommendations:
            report.append("## 💡 Recommendations")
            for rec in recommendations:
                report.append(f"- {rec}")
            report.append("")

        return '\n'.join(report)

    def _calculate_quality_score(self) -> float:
        """Calculate an overall quality score."""
        score = 0.0
        factors = 0

        # Coverage factor (40% weight)
        coverage = self.metrics.get('coverage', {})
        if 'error' not in coverage:
            line_cov = coverage.get('overall_line_coverage', 0)
            score += min(line_cov, 100) * 0.4
            factors += 0.4

        # Test success rate (30% weight)
        test_results = self.metrics.get('test_results', {})
        if 'error' not in test_results:
            success_rate = test_results.get('success_rate', 0)
            score += success_rate * 0.3
            factors += 0.3

        # Mutation score (20% weight)
        mutation = self.metrics.get('mutation_testing', {})
        if 'error' not in mutation:
            mutation_score = mutation.get('mutation_score', 0)
            score += mutation_score * 0.2
            factors += 0.2

        # Test organization (10% weight)
        test_files = self.metrics.get('test_files', {})
        if 'error' not in test_files:
            categories = test_files.get('test_categories', {})
            category_score = min(len([c for c in categories.values() if c > 0]) * 20, 100)
            score += category_score * 0.1
            factors += 0.1

        return score / max(factors, 1) if factors > 0 else 0

    def _generate_recommendations(self) -> List[str]:
        """Generate improvement recommendations."""
        recommendations = []

        # Coverage recommendations
        coverage = self.metrics.get('coverage', {})
        if 'error' not in coverage:
            line_cov = coverage.get('overall_line_coverage', 0)
            if line_cov < 80:
                recommendations.append(f"Increase test coverage from {line_cov:.1f}% to at least 80%")

        # Test organization
        test_files = self.metrics.get('test_files', {})
        if 'error' not in test_files:
            categories = test_files.get('test_categories', {})
            if categories.get('integration', 0) == 0:
                recommendations.append("Add integration tests to verify module interactions")
            if categories.get('benchmarks', 0) == 0:
                recommendations.append("Add performance benchmark tests")
            if categories.get('security', 0) == 0:
                recommendations.append("Add security validation tests")

        # Performance recommendations
        benchmarks = self.metrics.get('benchmarks', {})
        if 'error' not in benchmarks:
            avg_time = benchmarks.get('average_time', 0)
            if avg_time > 0.1:  # 100ms threshold
                recommendations.append(f"Optimize test performance (average: {avg_time:.3f}s)")

        return recommendations

    def save_report(self, output_path: Path):
        """Save the report to a file."""
        report_content = self.generate_markdown_report()
        output_path.write_text(report_content)

    def save_json_metrics(self, output_path: Path):
        """Save raw metrics as JSON."""
        with open(output_path, 'w') as f:
            json.dump(self.metrics, f, indent=2)


def main():
    """Main function for running the metrics collector."""
    parser = argparse.ArgumentParser(description="Generate test quality metrics dashboard")
    parser.add_argument("--project-root", type=Path, default=Path.cwd(),
                        help="Project root directory")
    parser.add_argument("--output-dir", type=Path, default=Path("reports"),
                        help="Output directory for reports")
    parser.add_argument("--format", choices=["markdown", "json", "both"], default="both",
                        help="Output format")

    args = parser.parse_args()

    # Ensure output directory exists
    args.output_dir.mkdir(exist_ok=True)

    # Collect metrics
    print("Collecting test quality metrics...")
    collector = TestMetricsCollector(args.project_root)
    metrics = collector.collect_all_metrics()

    # Generate dashboard
    dashboard = QualityDashboard(metrics)

    # Save reports
    if args.format in ["markdown", "both"]:
        markdown_path = args.output_dir / "test-quality-dashboard.md"
        dashboard.save_report(markdown_path)
        print(f"Markdown report saved to: {markdown_path}")

    if args.format in ["json", "both"]:
        json_path = args.output_dir / "test-metrics.json"
        dashboard.save_json_metrics(json_path)
        print(f"JSON metrics saved to: {json_path}")

    # Print summary
    print("\n" + "="*50)
    print("QUALITY SUMMARY")
    print("="*50)

    quality_score = dashboard._calculate_quality_score()
    print(f"Overall Quality Score: {quality_score:.1f}/100")

    if quality_score >= 90:
        print("Status: 🏆 GOLD STANDARD - Excellent testing infrastructure!")
    elif quality_score >= 80:
        print("Status: 🥉 GOOD QUALITY - Well-tested codebase")
    elif quality_score >= 70:
        print("Status: ⚠️ NEEDS IMPROVEMENT")
    else:
        print("Status: 🔴 POOR QUALITY - Significant testing gaps")

    recommendations = dashboard._generate_recommendations()
    if recommendations:
        print("\nKey Recommendations:")
        for i, rec in enumerate(recommendations[:3], 1):
            print(f"{i}. {rec}")

    print(f"\nFull report available at: {args.output_dir}")


if __name__ == "__main__":
    main()