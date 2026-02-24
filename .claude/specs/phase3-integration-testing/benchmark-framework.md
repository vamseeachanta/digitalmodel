# Phase 3: Benchmark Framework

> Performance benchmarking infrastructure for DigitalModel
> Version: 1.0.0
> Last Updated: 2025-01-09

## Overview

The Benchmark Framework provides automated performance measurement, baseline establishment, regression detection, and trend analysis for the DigitalModel platform. This infrastructure ensures system performance meets requirements and detects regressions early.

---

## Benchmarking Infrastructure

### Core Components

```python
# src/benchmarking/framework.py
"""Benchmarking framework for DigitalModel"""

import time
import psutil
import json
from pathlib import Path
from typing import Dict, Any, Callable
from dataclasses import dataclass, asdict
import statistics

@dataclass
class BenchmarkResult:
    """Result of a benchmark run"""
    name: str
    timestamp: float
    elapsed_seconds: float
    iterations: int
    ops_per_second: float
    memory_mb_delta: float
    memory_mb_peak: float
    cpu_percent_avg: float
    passed: bool
    threshold_met: bool
    error: str = None

    def to_dict(self) -> Dict:
        return asdict(self)

    def to_json(self) -> str:
        return json.dumps(self.to_dict(), indent=2)


class BenchmarkRunner:
    """Execute and track benchmarks"""

    def __init__(self, results_dir: Path = Path(".benchmarks")):
        self.results_dir = Path(results_dir)
        self.results_dir.mkdir(exist_ok=True)
        self.process = psutil.Process()
        self.results = []

    def run_benchmark(
        self,
        name: str,
        func: Callable,
        iterations: int = 1,
        threshold_seconds: float = None,
        *args,
        **kwargs
    ) -> BenchmarkResult:
        """Execute a benchmark"""
        memory_samples = []
        cpu_samples = []
        times = []

        # Warmup
        func(*args, **kwargs)

        # Measure
        for i in range(iterations):
            gc.collect()  # Clear garbage before measurement

            memory_before = self.process.memory_info().rss
            start_time = time.perf_counter()
            start_cpu = self.process.cpu_percent(interval=0.01)

            try:
                result = func(*args, **kwargs)
            except Exception as e:
                return BenchmarkResult(
                    name=name,
                    timestamp=time.time(),
                    elapsed_seconds=0,
                    iterations=0,
                    ops_per_second=0,
                    memory_mb_delta=0,
                    memory_mb_peak=0,
                    cpu_percent_avg=0,
                    passed=False,
                    threshold_met=False,
                    error=str(e)
                )

            elapsed = time.perf_counter() - start_time
            memory_after = self.process.memory_info().rss
            cpu_usage = self.process.cpu_percent(interval=0.01)

            times.append(elapsed)
            memory_samples.append((memory_after - memory_before) / 1_000_000)
            cpu_samples.append(cpu_usage)

        # Calculate statistics
        avg_time = statistics.mean(times)
        memory_delta = statistics.mean(memory_samples)
        memory_peak = max(memory_samples) if memory_samples else 0
        cpu_avg = statistics.mean(cpu_samples)
        ops_per_sec = iterations / sum(times)

        # Check threshold
        threshold_met = True
        if threshold_seconds is not None:
            threshold_met = avg_time <= threshold_seconds

        benchmark_result = BenchmarkResult(
            name=name,
            timestamp=time.time(),
            elapsed_seconds=avg_time,
            iterations=iterations,
            ops_per_second=ops_per_sec,
            memory_mb_delta=memory_delta,
            memory_mb_peak=memory_peak,
            cpu_percent_avg=cpu_avg,
            passed=True,
            threshold_met=threshold_met,
        )

        self.results.append(benchmark_result)
        return benchmark_result

    def save_results(self, filename: str = None):
        """Save benchmark results"""
        if filename is None:
            filename = f"benchmarks_{int(time.time())}.json"

        filepath = self.results_dir / filename
        with open(filepath, 'w') as f:
            json.dump(
                [r.to_dict() for r in self.results],
                f,
                indent=2
            )

        return filepath

    def print_summary(self):
        """Print summary of all benchmarks"""
        print("\n" + "="*70)
        print("BENCHMARK SUMMARY")
        print("="*70)

        for result in self.results:
            status = "✓ PASS" if result.passed else "✗ FAIL"
            threshold = "✓" if result.threshold_met else "⚠"

            print(f"\n{result.name:<40} {status}")
            print(f"  Time:      {result.elapsed_seconds:.4f}s")
            print(f"  Ops/sec:   {result.ops_per_second:.2f}")
            print(f"  Memory:    {result.memory_mb_delta:+.2f}MB (peak: {result.memory_mb_peak:.2f}MB)")
            print(f"  CPU:       {result.cpu_percent_avg:.1f}%")
            print(f"  Threshold: {threshold}")

        print("\n" + "="*70)
```

### Pytest Benchmark Plugin Integration

```python
# tests/conftest.py
"""Pytest configuration with benchmark support"""

import pytest
from src.benchmarking.framework import BenchmarkRunner

@pytest.fixture(scope="session")
def benchmark_runner():
    """Benchmark runner for session"""
    runner = BenchmarkRunner()
    yield runner
    runner.print_summary()
    runner.save_results()

@pytest.fixture
def benchmark(benchmark_runner):
    """Simple benchmark fixture for tests"""
    class SimpleBenchmark:
        def __init__(self, runner):
            self.runner = runner

        def measure(self, name, func, iterations=1, threshold=None, *args, **kwargs):
            return self.runner.run_benchmark(
                name, func, iterations, threshold, *args, **kwargs
            )

    return SimpleBenchmark(benchmark_runner)
```

---

## Performance Profiling

### CPU Profiling

```python
# src/benchmarking/profilers.py
"""Performance profilers"""

import cProfile
import pstats
from io import StringIO
from functools import wraps

def profile_cpu(func):
    """Decorator to profile CPU usage"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        profiler = cProfile.Profile()
        profiler.enable()

        result = func(*args, **kwargs)

        profiler.disable()
        stats = pstats.Stats(profiler, stream=StringIO())
        stats.sort_stats('cumulative')
        stats.print_stats(10)  # Top 10 functions

        return result
    return wrapper

def get_cpu_profile(func, *args, **kwargs):
    """Get CPU profile for function"""
    profiler = cProfile.Profile()
    profiler.enable()
    func(*args, **kwargs)
    profiler.disable()

    stats = pstats.Stats(profiler)
    stats.sort_stats('cumulative')
    return stats
```

### Memory Profiling

```python
# src/benchmarking/memory_profiler.py
"""Memory profiling utilities"""

import tracemalloc
import sys
from typing import Callable

class MemoryProfiler:
    """Track memory usage"""

    def __init__(self):
        self.snapshots = []

    def start(self):
        """Start memory tracking"""
        tracemalloc.start()

    def take_snapshot(self, label: str = None):
        """Take memory snapshot"""
        snapshot = tracemalloc.take_snapshot()
        self.snapshots.append({
            'label': label,
            'snapshot': snapshot,
            'timestamp': time.time()
        })

    def get_top_allocations(self, count: int = 10):
        """Get top memory allocations"""
        if not self.snapshots:
            return None

        latest = self.snapshots[-1]['snapshot']
        top_stats = latest.statistics('lineno')

        return [
            {
                'line': str(stat),
                'size_mb': stat.size / 1_000_000,
                'count': stat.count
            }
            for stat in top_stats[:count]
        ]

    def profile_function(self, func: Callable, *args, **kwargs):
        """Profile memory for function execution"""
        self.start()

        result = func(*args, **kwargs)

        self.take_snapshot('after_execution')
        return result, self.get_top_allocations()
```

---

## Baseline Establishment

### Creating Performance Baselines

```python
# tests/performance/test_baseline_establishment.py
"""Establish performance baselines"""

import pytest
import json
from pathlib import Path
from src.benchmarking.framework import BenchmarkRunner

class TestBaselineEstablishment:
    """Establish performance baselines"""

    @pytest.mark.baseline
    def test_establish_baseline_config_loading(self, benchmark):
        """Establish baseline for config loading"""
        def load_config():
            from src.config.manager import ConfigManager
            config = ConfigManager("tests/fixtures/configs/orcaflex_config.yaml")
            return config

        # Run multiple times to establish baseline
        result = benchmark.measure(
            "config_loading",
            load_config,
            iterations=100
        )

        # Store baseline
        baseline_data = {
            "operation": "config_loading",
            "avg_time_ms": result.elapsed_seconds * 1000,
            "ops_per_sec": result.ops_per_second,
            "memory_delta_mb": result.memory_mb_delta,
        }

        baseline_file = Path(".benchmarks/baselines/config_loading.json")
        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(baseline_file, 'w') as f:
            json.dump(baseline_data, f, indent=2)

        print(f"Baseline established: {baseline_data}")

    @pytest.mark.baseline
    def test_establish_baseline_solver_execution(self, benchmark):
        """Establish baseline for solver execution"""
        def execute_solver():
            from src.solvers.factory import SolverFactory
            solver = SolverFactory.create("orcaflex", mock_mode=True)
            results = solver.analyze(
                files=["tests/fixtures/data/orcaflex_sample.txt"],
                parameters={}
            )
            return results

        result = benchmark.measure(
            "solver_execution",
            execute_solver,
            iterations=50
        )

        baseline_data = {
            "operation": "solver_execution",
            "avg_time_ms": result.elapsed_seconds * 1000,
            "ops_per_sec": result.ops_per_second,
            "memory_delta_mb": result.memory_mb_delta,
        }

        baseline_file = Path(".benchmarks/baselines/solver_execution.json")
        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(baseline_file, 'w') as f:
            json.dump(baseline_data, f, indent=2)

    @pytest.mark.baseline
    def test_establish_baseline_data_pipeline(self, benchmark):
        """Establish baseline for data processing pipeline"""
        import pandas as pd

        def process_data():
            data = pd.DataFrame({
                "Time": [i * 0.1 for i in range(1000)],
                "Tension": [100 + i * 0.1 for i in range(1000)],
            })

            from src.processing.pipeline import DataPipeline
            pipeline = DataPipeline({})
            processed = pipeline.process(data)
            stats = pipeline.calculate_statistics(processed)
            return stats

        result = benchmark.measure(
            "data_pipeline_1000_points",
            process_data,
            iterations=50
        )

        baseline_data = {
            "operation": "data_pipeline_1000_points",
            "avg_time_ms": result.elapsed_seconds * 1000,
            "ops_per_sec": result.ops_per_second,
            "memory_delta_mb": result.memory_mb_delta,
        }

        baseline_file = Path(".benchmarks/baselines/data_pipeline.json")
        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(baseline_file, 'w') as f:
            json.dump(baseline_data, f, indent=2)

    @pytest.mark.baseline
    def test_establish_baseline_output_generation(self, benchmark):
        """Establish baseline for output generation"""
        def generate_output():
            import pandas as pd
            from src.output.generator import OutputGenerator

            results = {
                "data": pd.DataFrame({
                    "Time": [i * 0.1 for i in range(100)],
                    "Tension": [100 + i for i in range(100)],
                }),
                "statistics": {
                    "mean": 150,
                    "max": 200,
                    "min": 100,
                }
            }

            generator = OutputGenerator(results)
            return generator

        result = benchmark.measure(
            "output_generation",
            generate_output,
            iterations=50
        )

        baseline_data = {
            "operation": "output_generation",
            "avg_time_ms": result.elapsed_seconds * 1000,
            "ops_per_sec": result.ops_per_second,
            "memory_delta_mb": result.memory_mb_delta,
        }

        baseline_file = Path(".benchmarks/baselines/output_generation.json")
        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(baseline_file, 'w') as f:
            json.dump(baseline_data, f, indent=2)

# Run: pytest tests/performance/test_baseline_establishment.py -m baseline -v
```

### Baseline Data Format

```json
{
  ".benchmarks/baselines/config_loading.json": {
    "operation": "config_loading",
    "avg_time_ms": 15.2,
    "ops_per_sec": 65.8,
    "memory_delta_mb": 2.3
  },
  ".benchmarks/baselines/solver_execution.json": {
    "operation": "solver_execution",
    "avg_time_ms": 45.8,
    "ops_per_sec": 21.8,
    "memory_delta_mb": 12.5
  },
  ".benchmarks/baselines/data_pipeline.json": {
    "operation": "data_pipeline_1000_points",
    "avg_time_ms": 8.2,
    "ops_per_sec": 121.9,
    "memory_delta_mb": 1.2
  }
}
```

---

## Regression Detection

### Automated Regression Testing

```python
# tests/performance/test_regression_detection.py
"""Detect performance regressions"""

import pytest
import json
from pathlib import Path

class TestRegressionDetection:
    """Detect performance regressions"""

    @pytest.fixture
    def load_baseline(self):
        """Load baseline performance data"""
        baseline_dir = Path(".benchmarks/baselines")
        baselines = {}

        for baseline_file in baseline_dir.glob("*.json"):
            with open(baseline_file) as f:
                baselines[baseline_file.stem] = json.load(f)

        return baselines

    def test_config_loading_no_regression(self, benchmark, load_baseline):
        """Verify config loading within threshold"""
        baseline = load_baseline["config_loading"]
        threshold = baseline["avg_time_ms"] * 1.1  # 10% tolerance

        def load_config():
            from src.config.manager import ConfigManager
            config = ConfigManager("tests/fixtures/configs/orcaflex_config.yaml")
            return config

        result = benchmark.measure(
            "config_loading_regression",
            load_config,
            iterations=100,
            threshold_seconds=threshold / 1000
        )

        assert result.threshold_met, (
            f"Config loading regressed: "
            f"{result.elapsed_seconds*1000:.2f}ms > {threshold:.2f}ms baseline"
        )

    def test_solver_execution_no_regression(self, benchmark, load_baseline):
        """Verify solver execution within threshold"""
        baseline = load_baseline["solver_execution"]
        threshold = baseline["avg_time_ms"] * 1.1  # 10% tolerance

        def execute_solver():
            from src.solvers.factory import SolverFactory
            solver = SolverFactory.create("orcaflex", mock_mode=True)
            results = solver.analyze(
                files=["tests/fixtures/data/orcaflex_sample.txt"],
                parameters={}
            )
            return results

        result = benchmark.measure(
            "solver_execution_regression",
            execute_solver,
            iterations=50,
            threshold_seconds=threshold / 1000
        )

        assert result.threshold_met, (
            f"Solver execution regressed: "
            f"{result.elapsed_seconds*1000:.2f}ms > {threshold:.2f}ms baseline"
        )

    def test_data_pipeline_no_regression(self, benchmark, load_baseline):
        """Verify data pipeline within threshold"""
        baseline = load_baseline["data_pipeline"]
        threshold = baseline["avg_time_ms"] * 1.1

        import pandas as pd

        def process_data():
            data = pd.DataFrame({
                "Time": [i * 0.1 for i in range(1000)],
                "Tension": [100 + i * 0.1 for i in range(1000)],
            })

            from src.processing.pipeline import DataPipeline
            pipeline = DataPipeline({})
            processed = pipeline.process(data)
            return processed

        result = benchmark.measure(
            "data_pipeline_regression",
            process_data,
            iterations=50,
            threshold_seconds=threshold / 1000
        )

        assert result.threshold_met, (
            f"Data pipeline regressed: "
            f"{result.elapsed_seconds*1000:.2f}ms > {threshold:.2f}ms baseline"
        )

# Run: pytest tests/performance/test_regression_detection.py -v
```

---

## Trend Tracking

### Performance Trend Analysis

```python
# src/benchmarking/trend_analyzer.py
"""Analyze performance trends over time"""

import json
from pathlib import Path
from typing import List, Dict
import statistics
from datetime import datetime

class TrendAnalyzer:
    """Analyze performance trends"""

    def __init__(self, results_dir: Path = Path(".benchmarks")):
        self.results_dir = results_dir
        self.history_dir = results_dir / "history"
        self.history_dir.mkdir(parents=True, exist_ok=True)

    def archive_results(self, results):
        """Archive benchmark results"""
        timestamp = datetime.now().isoformat()
        archive_file = self.history_dir / f"results_{timestamp}.json"

        with open(archive_file, 'w') as f:
            json.dump(results, f, indent=2)

        return archive_file

    def analyze_trend(self, operation: str, limit: int = None):
        """Analyze trend for operation"""
        history_files = sorted(self.history_dir.glob("results_*.json"))
        if limit:
            history_files = history_files[-limit:]

        data = []
        for history_file in history_files:
            with open(history_file) as f:
                results = json.load(f)

            # Find operation in results
            for result in results:
                if result.get("name") == operation:
                    data.append({
                        "timestamp": history_file.name,
                        "elapsed_seconds": result["elapsed_seconds"],
                        "ops_per_second": result["ops_per_second"],
                        "memory_mb_delta": result["memory_mb_delta"],
                    })

        if not data:
            return None

        # Calculate statistics
        times = [d["elapsed_seconds"] for d in data]
        ops_per_sec = [d["ops_per_second"] for d in data]

        trend = {
            "operation": operation,
            "samples": len(data),
            "time_avg_ms": statistics.mean(times) * 1000,
            "time_min_ms": min(times) * 1000,
            "time_max_ms": max(times) * 1000,
            "time_trend": self._calculate_trend(times),
            "ops_per_sec_avg": statistics.mean(ops_per_sec),
            "ops_per_sec_trend": self._calculate_trend(ops_per_sec),
            "memory_delta_avg_mb": statistics.mean(
                [d["memory_mb_delta"] for d in data]
            ),
        }

        return trend

    @staticmethod
    def _calculate_trend(values: List[float]) -> str:
        """Calculate trend direction (improving, degrading, stable)"""
        if len(values) < 2:
            return "unknown"

        first_half = values[:len(values)//2]
        second_half = values[len(values)//2:]

        avg_first = statistics.mean(first_half)
        avg_second = statistics.mean(second_half)

        # For time: lower is better (improving)
        # For ops/sec: higher is better (improving)
        change_percent = ((avg_second - avg_first) / avg_first) * 100

        if change_percent < -2:
            return "improving"
        elif change_percent > 2:
            return "degrading"
        else:
            return "stable"

    def print_trends(self, operations: List[str] = None, limit: int = 10):
        """Print trend analysis"""
        print("\n" + "="*70)
        print("PERFORMANCE TRENDS")
        print("="*70)

        if operations is None:
            operations = ["config_loading", "solver_execution", "data_pipeline_1000_points"]

        for operation in operations:
            trend = self.analyze_trend(operation, limit)
            if trend:
                print(f"\n{operation}")
                print(f"  Avg Time:      {trend['time_avg_ms']:.2f}ms")
                print(f"  Min:           {trend['time_min_ms']:.2f}ms")
                print(f"  Max:           {trend['time_max_ms']:.2f}ms")
                print(f"  Trend:         {trend['time_trend']} ({trend['time_trend']})")
                print(f"  Samples:       {trend['samples']}")
```

---

## Reporting and Visualization

### Performance Report Generation

```python
# src/benchmarking/report_generator.py
"""Generate performance reports"""

import json
from pathlib import Path
from typing import List, Dict

class PerformanceReportGenerator:
    """Generate HTML performance reports"""

    def generate_html_report(self, results, output_file: Path):
        """Generate HTML performance report"""
        html = """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Performance Benchmark Report</title>
            <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
            <style>
                body { font-family: Arial, sans-serif; margin: 20px; }
                .metric { margin: 20px 0; padding: 10px; background: #f5f5f5; }
                .pass { color: green; font-weight: bold; }
                .fail { color: red; font-weight: bold; }
                table { border-collapse: collapse; width: 100%; }
                th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }
                th { background-color: #4CAF50; color: white; }
            </style>
        </head>
        <body>
            <h1>Performance Benchmark Report</h1>
            <div id="summary"></div>
            <div id="details"></div>
            <div id="charts"></div>
        </body>
        </html>
        """

        # Generate summary table
        summary_html = self._generate_summary_table(results)

        # Generate detail charts
        chart_html = self._generate_charts(results)

        # Combine and write
        html = html.replace(
            '<div id="summary"></div>',
            f'<div id="summary">{summary_html}</div>'
        )
        html = html.replace(
            '<div id="charts"></div>',
            f'<div id="charts">{chart_html}</div>'
        )

        with open(output_file, 'w') as f:
            f.write(html)

    @staticmethod
    def _generate_summary_table(results):
        """Generate summary table HTML"""
        rows = ""
        for result in results:
            status = '<span class="pass">✓ PASS</span>' if result['passed'] else '<span class="fail">✗ FAIL</span>'
            rows += f"""
            <tr>
                <td>{result['name']}</td>
                <td>{result['elapsed_seconds']:.4f}s</td>
                <td>{result['ops_per_second']:.2f}</td>
                <td>{result['memory_mb_delta']:+.2f}MB</td>
                <td>{status}</td>
            </tr>
            """

        return f"""
        <table>
            <tr>
                <th>Test Name</th>
                <th>Time (s)</th>
                <th>Ops/sec</th>
                <th>Memory Delta</th>
                <th>Status</th>
            </tr>
            {rows}
        </table>
        """

    @staticmethod
    def _generate_charts(results):
        """Generate performance charts"""
        return "<p>Charts would be generated with Plotly.js</p>"
```

### CI/CD Integration Report

```bash
#!/bin/bash
# scripts/benchmarks/compare_with_baseline.sh

set -e

echo "Comparing benchmark results with baseline..."

# Run benchmarks
pytest tests/performance/test_regression_detection.py -v

# Archive results
python -c "
from src.benchmarking.trend_analyzer import TrendAnalyzer
import json

analyzer = TrendAnalyzer()

# Load latest results
with open('.benchmarks/benchmarks_*.json') as f:
    results = json.load(f)

analyzer.archive_results(results)

# Analyze trends
analyzer.print_trends()
"

echo "Benchmark comparison complete!"
```

---

## CI/CD Integration

### GitHub Actions Benchmark Workflow

```yaml
# .github/workflows/benchmarks.yml
name: Performance Benchmarks

on:
  push:
    branches: [main, develop]
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: |
          pip install uv
          uv pip install -e ".[dev]"

      - name: Establish baselines (first run)
        run: |
          pytest tests/performance/test_baseline_establishment.py \
            -m baseline -v || true

      - name: Run regression tests
        run: |
          pytest tests/performance/test_regression_detection.py -v

      - name: Generate report
        run: |
          python scripts/benchmarks/generate_report.py

      - name: Upload benchmark results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: .benchmarks/

      - name: Comment on PR (if PR)
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('.benchmarks/latest.json'));

            let comment = '## Performance Benchmarks\n\n';
            results.forEach(r => {
              const status = r.passed ? '✓' : '✗';
              comment += `${status} ${r.name}: ${(r.elapsed_seconds*1000).toFixed(2)}ms\n`;
            });

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

---

## Performance Targets

Performance targets by operation (see performance-targets.md for detailed specs):

| Operation | Target | Acceptance |
|-----------|--------|-----------|
| Config loading (single) | <20ms | <25ms |
| Solver execution (mock) | <50ms | <75ms |
| Data pipeline (1k points) | <10ms | <15ms |
| Output generation | <100ms | <150ms |
| 100 file batch | <2s | <3s |
| 1000 file batch | <30s | <60s |
| Concurrent (5 ops) | Linear scaling | <5% degradation |

---

## Checklist

- [ ] Benchmarking framework set up
- [ ] Baselines established for all operations
- [ ] Regression tests automated
- [ ] Trend tracking implemented
- [ ] CI/CD integration working
- [ ] Performance reports generated
- [ ] Thresholds validated
- [ ] Documentation complete

---

**End of Benchmark Framework**
