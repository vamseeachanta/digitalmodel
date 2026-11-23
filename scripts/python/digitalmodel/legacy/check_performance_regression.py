#!/usr/bin/env python3
"""
Performance regression detection script for digitalmodel CI/CD pipeline.

This script analyzes benchmark results and detects performance regressions
by comparing current performance metrics with historical baselines.
"""

import json
import sys
import argparse
import statistics
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime


@dataclass
class BenchmarkResult:
    """Represents a single benchmark result."""
    name: str
    mean_time: float
    std_dev: float
    min_time: float
    max_time: float
    iterations: int
    timestamp: str = ""


@dataclass
class RegressionReport:
    """Represents a performance regression analysis report."""
    total_benchmarks: int
    regressions_found: int
    improvements_found: int
    threshold_percent: float
    critical_regressions: List[Dict[str, Any]]
    minor_regressions: List[Dict[str, Any]]
    improvements: List[Dict[str, Any]]


class PerformanceAnalyzer:
    """Analyzes performance benchmarks for regressions."""

    def __init__(self, threshold_percent: float = 20.0):
        """
        Initialize the performance analyzer.

        Args:
            threshold_percent: Percentage threshold for regression detection
        """
        self.threshold_percent = threshold_percent
        self.critical_threshold = threshold_percent * 1.5  # 30% for critical

    def load_benchmark_results(self, file_path: Path) -> List[BenchmarkResult]:
        """Load benchmark results from JSON file."""
        try:
            with open(file_path, 'r') as f:
                data = json.load(f)

            results = []
            if 'benchmarks' in data:
                for benchmark in data['benchmarks']:
                    result = BenchmarkResult(
                        name=benchmark.get('name', 'unknown'),
                        mean_time=benchmark.get('stats', {}).get('mean', 0.0),
                        std_dev=benchmark.get('stats', {}).get('stddev', 0.0),
                        min_time=benchmark.get('stats', {}).get('min', 0.0),
                        max_time=benchmark.get('stats', {}).get('max', 0.0),
                        iterations=benchmark.get('stats', {}).get('rounds', 0),
                        timestamp=data.get('datetime', datetime.now().isoformat())
                    )
                    results.append(result)

            return results

        except (FileNotFoundError, json.JSONDecodeError, KeyError) as e:
            print(f"Error loading benchmark results from {file_path}: {e}")
            return []

    def compare_benchmarks(
        self,
        current_results: List[BenchmarkResult],
        baseline_results: List[BenchmarkResult]
    ) -> RegressionReport:
        """
        Compare current benchmark results with baseline.

        Args:
            current_results: Current benchmark results
            baseline_results: Baseline benchmark results

        Returns:
            RegressionReport with analysis results
        """
        # Create lookup for baseline results
        baseline_lookup = {result.name: result for result in baseline_results}

        regressions = []
        improvements = []
        critical_regressions = []

        for current in current_results:
            if current.name not in baseline_lookup:
                print(f"Warning: No baseline found for benchmark '{current.name}'")
                continue

            baseline = baseline_lookup[current.name]

            # Calculate percentage change
            if baseline.mean_time > 0:
                change_percent = (
                    (current.mean_time - baseline.mean_time) / baseline.mean_time
                ) * 100
            else:
                continue

            change_info = {
                'name': current.name,
                'baseline_time': baseline.mean_time,
                'current_time': current.mean_time,
                'change_percent': change_percent,
                'change_absolute': current.mean_time - baseline.mean_time,
                'baseline_std': baseline.std_dev,
                'current_std': current.std_dev,
                'statistical_significance': self._is_statistically_significant(
                    current, baseline
                )
            }

            # Classify the change
            if change_percent > self.critical_threshold:
                critical_regressions.append(change_info)
            elif change_percent > self.threshold_percent:
                regressions.append(change_info)
            elif change_percent < -self.threshold_percent:
                improvements.append(change_info)

        return RegressionReport(
            total_benchmarks=len(current_results),
            regressions_found=len(regressions),
            improvements_found=len(improvements),
            threshold_percent=self.threshold_percent,
            critical_regressions=critical_regressions,
            minor_regressions=regressions,
            improvements=improvements
        )

    def _is_statistically_significant(
        self,
        current: BenchmarkResult,
        baseline: BenchmarkResult,
        confidence_level: float = 0.95
    ) -> bool:
        """
        Determine if the performance change is statistically significant.

        This is a simplified statistical test. In production, you might want
        to use more sophisticated statistical methods.
        """
        # Simple heuristic: change is significant if it's more than 2 standard deviations
        if baseline.std_dev == 0:
            return True

        change_in_stddevs = abs(current.mean_time - baseline.mean_time) / baseline.std_dev
        return change_in_stddevs > 2.0

    def generate_report(self, report: RegressionReport) -> str:
        """Generate a human-readable report."""
        lines = [
            "# Performance Regression Analysis Report",
            "",
            f"**Analysis Date:** {datetime.now().isoformat()}",
            f"**Regression Threshold:** {report.threshold_percent}%",
            f"**Total Benchmarks:** {report.total_benchmarks}",
            "",
            "## Summary",
            "",
            f"- 🔴 Critical Regressions: {len(report.critical_regressions)}",
            f"- 🟡 Minor Regressions: {report.regressions_found}",
            f"- 🟢 Improvements: {report.improvements_found}",
            ""
        ]

        # Critical regressions
        if report.critical_regressions:
            lines.extend([
                "## 🚨 Critical Performance Regressions",
                "",
                "| Benchmark | Baseline (s) | Current (s) | Change | Significant |",
                "|-----------|--------------|-------------|--------|-------------|"
            ])

            for reg in report.critical_regressions:
                sig_marker = "✓" if reg['statistical_significance'] else "?"
                lines.append(
                    f"| {reg['name']} | {reg['baseline_time']:.6f} | "
                    f"{reg['current_time']:.6f} | +{reg['change_percent']:.1f}% | {sig_marker} |"
                )
            lines.append("")

        # Minor regressions
        if report.minor_regressions:
            lines.extend([
                "## ⚠️ Minor Performance Regressions",
                "",
                "| Benchmark | Baseline (s) | Current (s) | Change | Significant |",
                "|-----------|--------------|-------------|--------|-------------|"
            ])

            for reg in report.minor_regressions:
                sig_marker = "✓" if reg['statistical_significance'] else "?"
                lines.append(
                    f"| {reg['name']} | {reg['baseline_time']:.6f} | "
                    f"{reg['current_time']:.6f} | +{reg['change_percent']:.1f}% | {sig_marker} |"
                )
            lines.append("")

        # Improvements
        if report.improvements:
            lines.extend([
                "## 🎉 Performance Improvements",
                "",
                "| Benchmark | Baseline (s) | Current (s) | Improvement | Significant |",
                "|-----------|--------------|-------------|-------------|-------------|"
            ])

            for imp in report.improvements:
                sig_marker = "✓" if imp['statistical_significance'] else "?"
                lines.append(
                    f"| {imp['name']} | {imp['baseline_time']:.6f} | "
                    f"{imp['current_time']:.6f} | {abs(imp['change_percent']):.1f}% | {sig_marker} |"
                )
            lines.append("")

        # Recommendations
        lines.extend([
            "## 📋 Recommendations",
            ""
        ])

        if report.critical_regressions:
            lines.append("- 🚨 **Immediate Action Required**: Critical performance regressions detected")
            lines.append("- Investigate changes that may have caused the regressions")
            lines.append("- Consider reverting recent changes or optimizing affected code paths")
        elif report.minor_regressions:
            lines.append("- ⚠️ **Review Recommended**: Minor performance regressions detected")
            lines.append("- Monitor these regressions in future runs")
            lines.append("- Consider optimization if regressions persist")
        else:
            lines.append("- ✅ **No Action Required**: No significant performance regressions detected")

        if report.improvements:
            lines.append("- 🎉 **Great Work**: Performance improvements detected!")

        return "\n".join(lines)

    def save_baseline(self, results: List[BenchmarkResult], baseline_file: Path):
        """Save current results as baseline for future comparisons."""
        baseline_data = {
            'timestamp': datetime.now().isoformat(),
            'benchmarks': [
                {
                    'name': result.name,
                    'stats': {
                        'mean': result.mean_time,
                        'stddev': result.std_dev,
                        'min': result.min_time,
                        'max': result.max_time,
                        'rounds': result.iterations
                    }
                }
                for result in results
            ]
        }

        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(baseline_file, 'w') as f:
            json.dump(baseline_data, f, indent=2)

        print(f"Baseline saved to {baseline_file}")


def main():
    """Main entry point for the performance regression checker."""
    parser = argparse.ArgumentParser(
        description="Detect performance regressions in benchmark results"
    )
    parser.add_argument(
        '--current',
        type=Path,
        help='Path to current benchmark results JSON file'
    )
    parser.add_argument(
        '--baseline',
        type=Path,
        help='Path to baseline benchmark results JSON file'
    )
    parser.add_argument(
        '--threshold',
        type=float,
        default=20.0,
        help='Regression threshold percentage (default: 20.0)'
    )
    parser.add_argument(
        '--output',
        type=Path,
        help='Output file for the regression report'
    )
    parser.add_argument(
        '--save-baseline',
        action='store_true',
        help='Save current results as new baseline'
    )
    parser.add_argument(
        '--auto-detect',
        action='store_true',
        help='Auto-detect benchmark files in current directory'
    )

    args = parser.parse_args()

    analyzer = PerformanceAnalyzer(threshold_percent=args.threshold)

    # Auto-detect benchmark files if requested
    if args.auto_detect:
        current_dir = Path('.')
        benchmark_files = list(current_dir.glob('*benchmark*.json'))

        if not benchmark_files:
            print("No benchmark files found in current directory")
            sys.exit(1)

        args.current = benchmark_files[0]
        print(f"Auto-detected current benchmark file: {args.current}")

        # Look for baseline in .benchmarks directory
        baseline_dir = Path('.benchmarks')
        if baseline_dir.exists():
            baseline_files = list(baseline_dir.glob('*baseline*.json'))
            if baseline_files:
                args.baseline = baseline_files[0]
                print(f"Auto-detected baseline file: {args.baseline}")

    # Load current results
    if not args.current or not args.current.exists():
        print("Current benchmark results file not found")
        sys.exit(1)

    current_results = analyzer.load_benchmark_results(args.current)
    if not current_results:
        print("No valid benchmark results found in current file")
        sys.exit(1)

    print(f"Loaded {len(current_results)} current benchmark results")

    # Handle baseline operations
    if args.save_baseline:
        baseline_file = args.baseline or Path('.benchmarks/baseline.json')
        analyzer.save_baseline(current_results, baseline_file)
        return

    # Load baseline for comparison
    if not args.baseline or not args.baseline.exists():
        print("Warning: No baseline file found. Creating initial baseline.")
        baseline_file = Path('.benchmarks/baseline.json')
        analyzer.save_baseline(current_results, baseline_file)
        print("Initial baseline created. Run again after making changes to detect regressions.")
        return

    baseline_results = analyzer.load_benchmark_results(args.baseline)
    if not baseline_results:
        print("No valid baseline results found")
        sys.exit(1)

    print(f"Loaded {len(baseline_results)} baseline benchmark results")

    # Perform regression analysis
    report = analyzer.compare_benchmarks(current_results, baseline_results)

    # Generate and output report
    report_text = analyzer.generate_report(report)

    if args.output:
        with open(args.output, 'w') as f:
            f.write(report_text)
        print(f"Report saved to {args.output}")
    else:
        print(report_text)

    # Exit with error code if critical regressions found
    if report.critical_regressions:
        print(f"\n❌ CRITICAL: {len(report.critical_regressions)} critical performance regressions detected!")
        sys.exit(1)
    elif report.minor_regressions:
        print(f"\n⚠️ WARNING: {report.regressions_found} minor performance regressions detected")
        # Don't fail CI for minor regressions, just warn
        sys.exit(0)
    else:
        print("\n✅ No significant performance regressions detected")
        sys.exit(0)


if __name__ == '__main__':
    main()