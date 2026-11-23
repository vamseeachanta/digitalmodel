#!/usr/bin/env python3
"""
Test Performance Analysis Script
Analyzes test execution performance, identifies bottlenecks, and generates recommendations.
"""

import os
import time
import json
import statistics
from pathlib import Path
from typing import Dict, List, Any
import subprocess
import sys


class TestPerformanceAnalyzer:
    """Analyzes test performance and identifies bottlenecks."""

    def __init__(self, test_dir: Path):
        self.test_dir = test_dir
        self.results = {}

    def analyze_test_structure(self) -> Dict[str, Any]:
        """Analyze test file structure and categorization."""
        results = {
            'total_files': 0,
            'file_sizes': [],
            'modules': [],
            'test_types': {
                'unit': 0,
                'integration': 0,
                'performance': 0,
                'property': 0,
                'benchmark': 0,
                'security': 0,
                'automation': 0
            },
            'large_files': [],
            'complex_modules': []
        }

        for py_file in self.test_dir.rglob('*.py'):
            if py_file.name.startswith('test_') or py_file.name.endswith('_test.py') or py_file.name == 'conftest.py':
                results['total_files'] += 1
                size = py_file.stat().st_size
                results['file_sizes'].append(size)

                # Count lines of code
                try:
                    with open(py_file, 'r', encoding='utf-8') as f:
                        lines = len(f.readlines())

                    if lines > 500:  # Large files
                        results['large_files'].append({
                            'file': str(py_file.relative_to(self.test_dir)),
                            'lines': lines,
                            'size_kb': size / 1024
                        })

                    if lines > 300:  # Complex modules
                        results['complex_modules'].append({
                            'file': str(py_file.relative_to(self.test_dir)),
                            'lines': lines
                        })

                except Exception:
                    pass

                # Categorize by path and name
                path_str = str(py_file.relative_to(self.test_dir)).lower()
                if 'performance' in path_str or 'benchmark' in path_str:
                    results['test_types']['performance'] += 1
                elif 'integration' in path_str:
                    results['test_types']['integration'] += 1
                elif 'property' in path_str:
                    results['test_types']['property'] += 1
                elif 'security' in path_str:
                    results['test_types']['security'] += 1
                elif 'automation' in path_str:
                    results['test_types']['automation'] += 1
                else:
                    results['test_types']['unit'] += 1

                results['modules'].append(path_str)

        return results

    def analyze_pytest_configuration(self, config_file: Path) -> Dict[str, Any]:
        """Analyze pytest configuration for performance implications."""
        config_analysis = {
            'parallel_enabled': False,
            'coverage_enabled': False,
            'reporting_overhead': [],
            'timeout_configured': False,
            'performance_markers': False,
            'optimization_potential': []
        }

        if not config_file.exists():
            return config_analysis

        try:
            with open(config_file, 'r') as f:
                content = f.read()

            # Check for parallel execution
            if 'pytest-xdist' in content or '-n auto' in content:
                config_analysis['parallel_enabled'] = True
            else:
                config_analysis['optimization_potential'].append(
                    'Enable parallel execution with pytest-xdist (-n auto)'
                )

            # Check for coverage
            if '--cov' in content:
                config_analysis['coverage_enabled'] = True
                config_analysis['reporting_overhead'].append('coverage')

            # Check for HTML reports
            if '--html' in content:
                config_analysis['reporting_overhead'].append('html_reports')

            # Check for JSON reports
            if '--json' in content:
                config_analysis['reporting_overhead'].append('json_reports')

            # Check for timeout
            if '--timeout' in content:
                config_analysis['timeout_configured'] = True

            # Check for performance markers
            if 'performance' in content or 'benchmark' in content:
                config_analysis['performance_markers'] = True

        except Exception as e:
            config_analysis['error'] = str(e)

        return config_analysis

    def estimate_execution_times(self, structure_analysis: Dict) -> Dict[str, Any]:
        """Estimate test execution times based on file analysis."""
        estimates = {
            'total_estimated_time': 0,
            'by_category': {},
            'bottleneck_files': [],
            'parallel_potential': 0
        }

        # Base time estimates per test type (seconds per file)
        base_times = {
            'unit': 2.0,
            'integration': 8.0,
            'performance': 15.0,
            'benchmark': 20.0,
            'property': 12.0,
            'security': 10.0,
            'automation': 25.0
        }

        for test_type, count in structure_analysis['test_types'].items():
            estimated_time = count * base_times.get(test_type, 5.0)
            estimates['by_category'][test_type] = {
                'files': count,
                'estimated_seconds': estimated_time,
                'estimated_minutes': estimated_time / 60
            }
            estimates['total_estimated_time'] += estimated_time

        # Identify potential bottlenecks from large files
        for large_file in structure_analysis['large_files']:
            # Estimate execution time based on file size/complexity
            complexity_factor = large_file['lines'] / 100  # Base complexity
            estimated_file_time = complexity_factor * 2  # 2 seconds per 100 lines

            if estimated_file_time > 30:  # Files that might take >30s
                estimates['bottleneck_files'].append({
                    'file': large_file['file'],
                    'estimated_time_seconds': estimated_file_time,
                    'lines': large_file['lines'],
                    'reason': 'Large file with high complexity'
                })

        # Calculate parallel execution potential
        if estimates['total_estimated_time'] > 60:  # Worth parallelizing if >1 minute
            # Assume 4 cores, 70% efficiency
            estimates['parallel_potential'] = estimates['total_estimated_time'] * 0.3
            estimates['parallel_estimated_time'] = estimates['total_estimated_time'] * 0.3

        return estimates

    def analyze_dependencies(self) -> Dict[str, Any]:
        """Analyze test dependencies that might affect performance."""
        dep_analysis = {
            'heavy_imports': [],
            'external_services': [],
            'file_operations': 0,
            'mock_usage': 0,
            'database_tests': 0
        }

        heavy_modules = [
            'pandas', 'numpy', 'scipy', 'matplotlib', 'plotly',
            'tensorflow', 'torch', 'sklearn', 'cv2',
            'selenium', 'requests', 'scrapy'
        ]

        external_indicators = [
            'requests.', 'httpx.', 'urllib', 'api', 'service',
            'database', 'sql', 'mongo', 'redis'
        ]

        for py_file in self.test_dir.rglob('*.py'):
            if py_file.name.startswith('test_') or py_file.name.endswith('_test.py'):
                try:
                    with open(py_file, 'r', encoding='utf-8') as f:
                        content = f.read()

                    # Check for heavy imports
                    for module in heavy_modules:
                        if f'import {module}' in content or f'from {module}' in content:
                            dep_analysis['heavy_imports'].append({
                                'file': str(py_file.relative_to(self.test_dir)),
                                'module': module
                            })

                    # Check for external service usage
                    for indicator in external_indicators:
                        if indicator in content:
                            dep_analysis['external_services'].append({
                                'file': str(py_file.relative_to(self.test_dir)),
                                'type': indicator
                            })

                    # Count file operations
                    if 'open(' in content or 'pathlib' in content or 'os.path' in content:
                        dep_analysis['file_operations'] += 1

                    # Count mock usage
                    if 'mock' in content.lower() or 'Mock' in content:
                        dep_analysis['mock_usage'] += 1

                    # Count database tests
                    if any(db in content.lower() for db in ['sql', 'database', 'db', 'sqlite', 'postgres']):
                        dep_analysis['database_tests'] += 1

                except Exception:
                    continue

        return dep_analysis

    def generate_recommendations(self, analysis_results: Dict) -> List[str]:
        """Generate performance optimization recommendations."""
        recommendations = []

        structure = analysis_results.get('structure', {})
        config = analysis_results.get('config', {})
        estimates = analysis_results.get('estimates', {})
        dependencies = analysis_results.get('dependencies', {})

        # Parallel execution recommendations
        if not config.get('parallel_enabled', False) and estimates.get('total_estimated_time', 0) > 60:
            recommendations.append({
                'priority': 'HIGH',
                'category': 'Parallel Execution',
                'recommendation': 'Enable pytest-xdist for parallel test execution',
                'implementation': 'Add -n auto to pytest command or install pytest-xdist',
                'estimated_improvement': f"{estimates.get('parallel_potential', 0):.0f} seconds saved"
            })

        # Large file recommendations
        large_files = structure.get('large_files', [])
        if len(large_files) > 5:
            recommendations.append({
                'priority': 'MEDIUM',
                'category': 'Test Organization',
                'recommendation': f'Split {len(large_files)} large test files (>500 lines)',
                'implementation': 'Break down complex test files into smaller, focused modules',
                'estimated_improvement': '20-30% faster test discovery and execution'
            })

        # Bottleneck file recommendations
        bottlenecks = estimates.get('bottleneck_files', [])
        if bottlenecks:
            recommendations.append({
                'priority': 'HIGH',
                'category': 'Performance Bottlenecks',
                'recommendation': f'Optimize {len(bottlenecks)} high-complexity test files',
                'implementation': 'Review and refactor slow test files, consider mocking heavy operations',
                'estimated_improvement': f"Up to {sum(b['estimated_time_seconds'] for b in bottlenecks) * 0.5:.0f} seconds saved"
            })

        # Heavy import recommendations
        heavy_imports = len(dependencies.get('heavy_imports', []))
        if heavy_imports > 10:
            recommendations.append({
                'priority': 'MEDIUM',
                'category': 'Import Optimization',
                'recommendation': f'Optimize {heavy_imports} heavy module imports',
                'implementation': 'Use lazy imports, mock heavy dependencies, or conditional imports',
                'estimated_improvement': '10-15% faster test startup time'
            })

        # Coverage overhead recommendations
        if config.get('coverage_enabled', False) and estimates.get('total_estimated_time', 0) > 300:
            recommendations.append({
                'priority': 'LOW',
                'category': 'Reporting Overhead',
                'recommendation': 'Consider disabling coverage for development runs',
                'implementation': 'Use separate make targets: one with coverage (CI), one without (dev)',
                'estimated_improvement': '15-25% faster execution without coverage'
            })

        # Mock usage recommendations
        external_services = len(dependencies.get('external_services', []))
        if external_services > 5 and dependencies.get('mock_usage', 0) < external_services * 0.7:
            recommendations.append({
                'priority': 'HIGH',
                'category': 'External Dependencies',
                'recommendation': 'Increase mocking of external services',
                'implementation': 'Mock API calls, database connections, and file operations',
                'estimated_improvement': 'Eliminate network delays and external service dependencies'
            })

        return recommendations

    def run_full_analysis(self) -> Dict[str, Any]:
        """Run complete test performance analysis."""
        print("Starting comprehensive test performance analysis...")
        start_time = time.time()

        # Structure analysis
        print("1. Analyzing test structure...")
        structure_analysis = self.analyze_test_structure()

        # Configuration analysis
        print("2. Analyzing pytest configuration...")
        config_file = self.test_dir.parent / "pyproject.toml"
        config_analysis = self.analyze_pytest_configuration(config_file)

        # Execution time estimates
        print("3. Estimating execution times...")
        time_estimates = self.estimate_execution_times(structure_analysis)

        # Dependency analysis
        print("4. Analyzing dependencies...")
        dependency_analysis = self.analyze_dependencies()

        # Generate recommendations
        print("5. Generating recommendations...")

        full_results = {
            'structure': structure_analysis,
            'config': config_analysis,
            'estimates': time_estimates,
            'dependencies': dependency_analysis,
            'analysis_time': time.time() - start_time
        }

        recommendations = self.generate_recommendations(full_results)
        full_results['recommendations'] = recommendations

        return full_results


def main():
    """Main execution function."""
    test_dir = Path("/mnt/github/github/digitalmodel/tests")

    if not test_dir.exists():
        print(f"Test directory not found: {test_dir}")
        return

    analyzer = TestPerformanceAnalyzer(test_dir)
    results = analyzer.run_full_analysis()

    # Print summary
    print("\n" + "="*80)
    print("TEST PERFORMANCE ANALYSIS SUMMARY")
    print("="*80)

    structure = results['structure']
    print(f"Total test files: {structure['total_files']}")
    print(f"Average file size: {sum(structure['file_sizes'])/len(structure['file_sizes'])/1024:.1f}KB")
    print(f"Largest file: {max(structure['file_sizes'])/1024:.1f}KB")

    print(f"\nTest distribution:")
    for test_type, count in structure['test_types'].items():
        print(f"  {test_type}: {count} files")

    estimates = results['estimates']
    print(f"\nEstimated execution time: {estimates['total_estimated_time']/60:.1f} minutes")

    if estimates.get('parallel_estimated_time'):
        print(f"With parallel execution: {estimates['parallel_estimated_time']/60:.1f} minutes")

    print(f"\nLarge files (>500 lines): {len(structure['large_files'])}")
    print(f"Potential bottlenecks: {len(estimates.get('bottleneck_files', []))}")

    print(f"\nDependency analysis:")
    deps = results['dependencies']
    print(f"  Heavy imports: {len(deps['heavy_imports'])}")
    print(f"  External services: {len(deps['external_services'])}")
    print(f"  Files with file operations: {deps['file_operations']}")
    print(f"  Files using mocks: {deps['mock_usage']}")

    # Print recommendations
    recommendations = results['recommendations']
    if recommendations:
        print(f"\n" + "="*80)
        print("OPTIMIZATION RECOMMENDATIONS")
        print("="*80)

        for i, rec in enumerate(recommendations, 1):
            print(f"\n{i}. [{rec['priority']}] {rec['category']}")
            print(f"   Recommendation: {rec['recommendation']}")
            print(f"   Implementation: {rec['implementation']}")
            print(f"   Estimated improvement: {rec['estimated_improvement']}")

    # Save results
    output_file = Path("/mnt/github/github/digitalmodel/docs/test_performance_analysis.json")
    output_file.parent.mkdir(parents=True, exist_ok=True)

    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)

    print(f"\nDetailed results saved to: {output_file}")
    print(f"Analysis completed in {results['analysis_time']:.2f} seconds")


if __name__ == "__main__":
    main()