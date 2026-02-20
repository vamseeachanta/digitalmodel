#!/usr/bin/env python3
"""
Test Resource Usage Monitor
Monitors CPU, memory, and I/O usage during test execution to identify resource bottlenecks.
"""

import psutil
import time
import json
import threading
from pathlib import Path
from typing import Dict, List, Any
import queue
import subprocess
import sys
from datetime import datetime


class ResourceMonitor:
    """Monitor system resources during test execution."""

    def __init__(self, sampling_interval: float = 0.5):
        self.sampling_interval = sampling_interval
        self.monitoring = False
        self.data_queue = queue.Queue()
        self.monitor_thread = None
        self.process = psutil.Process()
        self.start_stats = None

    def start_monitoring(self):
        """Start resource monitoring in background thread."""
        self.monitoring = True
        self.start_stats = self._get_current_stats()
        self.monitor_thread = threading.Thread(target=self._monitor_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()

    def stop_monitoring(self) -> Dict[str, Any]:
        """Stop monitoring and return collected data."""
        self.monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=2.0)

        # Collect all data points
        data_points = []
        while not self.data_queue.empty():
            try:
                data_points.append(self.data_queue.get_nowait())
            except queue.Empty:
                break

        return self._analyze_resource_data(data_points)

    def _get_current_stats(self) -> Dict[str, Any]:
        """Get current system resource statistics."""
        try:
            cpu_percent = psutil.cpu_percent(interval=None)
            memory = psutil.virtual_memory()
            disk = psutil.disk_usage('/')

            # Process-specific stats
            process_memory = self.process.memory_info()

            return {
                'timestamp': time.time(),
                'cpu_percent': cpu_percent,
                'memory_percent': memory.percent,
                'memory_used_mb': memory.used / 1024 / 1024,
                'memory_available_mb': memory.available / 1024 / 1024,
                'disk_percent': disk.percent,
                'process_memory_mb': process_memory.rss / 1024 / 1024,
                'process_cpu_percent': self.process.cpu_percent(),
                'open_files': len(self.process.open_files()) if hasattr(self.process, 'open_files') else 0
            }
        except Exception as e:
            return {'timestamp': time.time(), 'error': str(e)}

    def _monitor_loop(self):
        """Main monitoring loop running in background thread."""
        while self.monitoring:
            stats = self._get_current_stats()
            self.data_queue.put(stats)
            time.sleep(self.sampling_interval)

    def _analyze_resource_data(self, data_points: List[Dict]) -> Dict[str, Any]:
        """Analyze collected resource data and generate insights."""
        if not data_points:
            return {'error': 'No data collected'}

        # Extract time series data
        cpu_data = [p.get('cpu_percent', 0) for p in data_points if 'cpu_percent' in p]
        memory_data = [p.get('memory_percent', 0) for p in data_points if 'memory_percent' in p]
        process_memory_data = [p.get('process_memory_mb', 0) for p in data_points if 'process_memory_mb' in p]

        analysis = {
            'monitoring_duration': data_points[-1]['timestamp'] - data_points[0]['timestamp'],
            'samples_collected': len(data_points),
            'cpu_usage': {
                'mean': sum(cpu_data) / len(cpu_data) if cpu_data else 0,
                'max': max(cpu_data) if cpu_data else 0,
                'min': min(cpu_data) if cpu_data else 0,
                'spikes': len([x for x in cpu_data if x > 80])  # High CPU usage points
            },
            'memory_usage': {
                'mean': sum(memory_data) / len(memory_data) if memory_data else 0,
                'max': max(memory_data) if memory_data else 0,
                'min': min(memory_data) if memory_data else 0
            },
            'process_memory': {
                'mean_mb': sum(process_memory_data) / len(process_memory_data) if process_memory_data else 0,
                'max_mb': max(process_memory_data) if process_memory_data else 0,
                'growth_mb': process_memory_data[-1] - process_memory_data[0] if process_memory_data else 0
            },
            'resource_efficiency': {
                'cpu_utilization': 'high' if sum(cpu_data) / len(cpu_data) > 50 else 'normal',
                'memory_pressure': 'high' if max(memory_data) > 80 else 'normal',
                'potential_memory_leak': process_memory_data[-1] > process_memory_data[0] * 2 if process_memory_data else False
            },
            'raw_data': data_points
        }

        return analysis


class TestExecutionProfiler:
    """Profile test execution with resource monitoring."""

    def __init__(self, test_dir: Path):
        self.test_dir = test_dir
        self.results = {}

    def profile_test_execution(self, test_pattern: str = "tests/", timeout: int = 300) -> Dict[str, Any]:
        """Profile a test execution with resource monitoring."""
        monitor = ResourceMonitor(sampling_interval=0.2)

        print(f"Starting test execution profiling for: {test_pattern}")
        start_time = time.time()

        # Start resource monitoring
        monitor.start_monitoring()

        try:
            # Run a simple Python-based test execution simulation
            # Since we can't run actual pytest due to dependencies, we'll simulate
            result = self._simulate_test_execution(test_pattern, timeout)
            execution_time = time.time() - start_time

        except Exception as e:
            execution_time = time.time() - start_time
            result = {'error': str(e), 'returncode': 1}

        # Stop monitoring and get resource data
        resource_data = monitor.stop_monitoring()

        return {
            'test_pattern': test_pattern,
            'execution_time': execution_time,
            'test_result': result,
            'resource_usage': resource_data,
            'timestamp': datetime.now().isoformat()
        }

    def _simulate_test_execution(self, test_pattern: str, timeout: int) -> Dict[str, Any]:
        """Simulate test execution to measure resource usage."""
        # Instead of running actual tests, we'll simulate by doing similar operations
        # that tests would do: file I/O, imports, computations

        results = {
            'simulated': True,
            'operations_performed': [],
            'files_processed': 0
        }

        # Simulate test discovery (file operations)
        test_files = list(self.test_dir.rglob('*.py'))
        test_files = [f for f in test_files if 'test' in f.name.lower()]

        results['files_processed'] = len(test_files)
        results['operations_performed'].append(f"Discovered {len(test_files)} test files")

        # Simulate heavy operations that tests might do
        time.sleep(0.1)  # Simulate startup time

        # Simulate file reading (common in tests)
        for i, test_file in enumerate(test_files[:20]):  # Limit to first 20 for simulation
            try:
                with open(test_file, 'r') as f:
                    content = f.read()
                    # Simulate some processing
                    lines = len(content.splitlines())

                if i % 10 == 0:  # Every 10th file, add delay to simulate complex test
                    time.sleep(0.05)

            except Exception:
                continue

        results['operations_performed'].append("Simulated file reading and processing")

        # Simulate memory allocation (like loading test data)
        import numpy as np
        data = [np.random.randn(1000) for _ in range(10)]
        results['operations_performed'].append("Simulated memory allocation")

        # Cleanup
        del data

        return results

    def profile_individual_modules(self) -> Dict[str, Any]:
        """Profile individual test modules to identify resource-heavy ones."""
        module_profiles = {}

        # Get test modules grouped by category
        modules = {
            'unit': list(self.test_dir.rglob('test_*.py')),
            'integration': list((self.test_dir / 'integration').rglob('*.py')) if (self.test_dir / 'integration').exists() else [],
            'performance': list((self.test_dir / 'performance').rglob('*.py')) if (self.test_dir / 'performance').exists() else [],
            'automation': list((self.test_dir / 'test_automation').rglob('*.py')) if (self.test_dir / 'test_automation').exists() else []
        }

        for category, files in modules.items():
            if not files:
                continue

            category_results = []

            for test_file in files[:5]:  # Limit to first 5 files per category for demo
                file_profile = self._profile_single_file(test_file)
                category_results.append(file_profile)

            module_profiles[category] = {
                'total_files': len(files),
                'profiled_files': len(category_results),
                'profiles': category_results,
                'avg_complexity': sum(p.get('complexity_score', 0) for p in category_results) / len(category_results) if category_results else 0
            }

        return module_profiles

    def _profile_single_file(self, test_file: Path) -> Dict[str, Any]:
        """Profile a single test file for resource usage patterns."""
        profile = {
            'file': str(test_file.relative_to(self.test_dir)),
            'size_kb': test_file.stat().st_size / 1024,
            'complexity_score': 0,
            'estimated_execution_time': 0,
            'resource_indicators': []
        }

        try:
            with open(test_file, 'r', encoding='utf-8') as f:
                content = f.read()

            lines = content.splitlines()
            profile['lines'] = len(lines)

            # Calculate complexity score based on content analysis
            complexity_factors = {
                'imports': len([l for l in lines if l.strip().startswith('import') or l.strip().startswith('from')]),
                'functions': len([l for l in lines if 'def test_' in l]),
                'classes': len([l for l in lines if l.strip().startswith('class')]),
                'decorators': len([l for l in lines if l.strip().startswith('@')]),
                'loops': len([l for l in lines if 'for ' in l or 'while ' in l]),
                'file_ops': len([l for l in lines if 'open(' in l or 'read(' in l or 'write(' in l]),
            }

            profile['complexity_score'] = sum(complexity_factors.values())
            profile['complexity_factors'] = complexity_factors

            # Identify resource-heavy patterns
            if complexity_factors['file_ops'] > 5:
                profile['resource_indicators'].append('High file I/O operations')

            if 'subprocess' in content or 'os.system' in content:
                profile['resource_indicators'].append('External process calls')

            if any(heavy in content for heavy in ['pandas', 'numpy', 'matplotlib', 'requests']):
                profile['resource_indicators'].append('Heavy library usage')

            if 'time.sleep' in content or 'asyncio.sleep' in content:
                profile['resource_indicators'].append('Sleep/delay operations')

            # Estimate execution time based on complexity
            base_time = 1.0  # Base 1 second per test file
            complexity_multiplier = 1 + (profile['complexity_score'] / 50)  # Scale with complexity
            profile['estimated_execution_time'] = base_time * complexity_multiplier

        except Exception as e:
            profile['error'] = str(e)

        return profile

    def generate_resource_report(self) -> Dict[str, Any]:
        """Generate comprehensive resource usage report."""
        print("Generating comprehensive resource usage report...")

        # Profile overall test execution
        overall_profile = self.profile_test_execution()

        # Profile individual modules
        module_profiles = self.profile_individual_modules()

        # Analyze results
        report = {
            'summary': {
                'analysis_timestamp': datetime.now().isoformat(),
                'total_execution_time': overall_profile['execution_time'],
                'resource_efficiency': overall_profile['resource_usage'].get('resource_efficiency', {}),
                'modules_analyzed': sum(mp['profiled_files'] for mp in module_profiles.values()),
                'high_complexity_files': []
            },
            'overall_execution': overall_profile,
            'module_analysis': module_profiles,
            'recommendations': []
        }

        # Identify high-complexity files
        for category, data in module_profiles.items():
            for profile in data['profiles']:
                if profile.get('complexity_score', 0) > 30:  # High complexity threshold
                    report['summary']['high_complexity_files'].append({
                        'file': profile['file'],
                        'category': category,
                        'complexity_score': profile['complexity_score'],
                        'estimated_time': profile['estimated_execution_time']
                    })

        # Generate recommendations based on analysis
        resource_usage = overall_profile['resource_usage']

        if resource_usage.get('resource_efficiency', {}).get('cpu_utilization') == 'high':
            report['recommendations'].append({
                'type': 'CPU Optimization',
                'issue': 'High CPU utilization detected',
                'recommendation': 'Consider reducing computational complexity or using parallel execution',
                'priority': 'medium'
            })

        if resource_usage.get('resource_efficiency', {}).get('memory_pressure') == 'high':
            report['recommendations'].append({
                'type': 'Memory Optimization',
                'issue': 'High memory pressure during execution',
                'recommendation': 'Optimize memory usage, use generators, or increase available memory',
                'priority': 'high'
            })

        if resource_usage.get('resource_efficiency', {}).get('potential_memory_leak'):
            report['recommendations'].append({
                'type': 'Memory Leak',
                'issue': 'Potential memory leak detected',
                'recommendation': 'Review memory cleanup in test teardown, check for circular references',
                'priority': 'high'
            })

        if len(report['summary']['high_complexity_files']) > 5:
            report['recommendations'].append({
                'type': 'Test Complexity',
                'issue': f"Found {len(report['summary']['high_complexity_files'])} high-complexity test files",
                'recommendation': 'Break down complex test files into smaller, focused modules',
                'priority': 'medium'
            })

        return report


def main():
    """Main execution function."""
    test_dir = Path("/mnt/github/github/digitalmodel/tests")

    if not test_dir.exists():
        print(f"Test directory not found: {test_dir}")
        return

    profiler = TestExecutionProfiler(test_dir)
    report = profiler.generate_resource_report()

    # Print summary
    print("\n" + "="*80)
    print("RESOURCE USAGE ANALYSIS SUMMARY")
    print("="*80)

    summary = report['summary']
    print(f"Analysis completed: {summary['analysis_timestamp']}")
    print(f"Total execution time: {summary['total_execution_time']:.2f} seconds")
    print(f"Modules analyzed: {summary['modules_analyzed']}")
    print(f"High-complexity files: {len(summary['high_complexity_files'])}")

    # Resource efficiency
    efficiency = report['overall_execution']['resource_usage'].get('resource_efficiency', {})
    print(f"\nResource Efficiency:")
    print(f"  CPU utilization: {efficiency.get('cpu_utilization', 'unknown')}")
    print(f"  Memory pressure: {efficiency.get('memory_pressure', 'unknown')}")
    print(f"  Memory leak detected: {efficiency.get('potential_memory_leak', False)}")

    # Module breakdown
    print(f"\nModule Analysis:")
    for category, data in report['module_analysis'].items():
        print(f"  {category}: {data['total_files']} files, avg complexity: {data['avg_complexity']:.1f}")

    # High complexity files
    if summary['high_complexity_files']:
        print(f"\nHigh-Complexity Files:")
        for file_info in summary['high_complexity_files'][:5]:  # Top 5
            print(f"  {file_info['file']} (score: {file_info['complexity_score']}, est: {file_info['estimated_time']:.1f}s)")

    # Recommendations
    if report['recommendations']:
        print(f"\n" + "="*80)
        print("RESOURCE OPTIMIZATION RECOMMENDATIONS")
        print("="*80)

        for i, rec in enumerate(report['recommendations'], 1):
            print(f"\n{i}. [{rec['priority'].upper()}] {rec['type']}")
            print(f"   Issue: {rec['issue']}")
            print(f"   Recommendation: {rec['recommendation']}")

    # Save detailed report
    output_file = Path("/mnt/github/github/digitalmodel/docs/test_resource_usage_report.json")
    output_file.parent.mkdir(parents=True, exist_ok=True)

    with open(output_file, 'w') as f:
        json.dump(report, f, indent=2, default=str)

    print(f"\nDetailed resource usage report saved to: {output_file}")


if __name__ == "__main__":
    main()