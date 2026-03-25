"""
ABOUTME: Central benchmark orchestration and result management
ABOUTME: Coordinates benchmark execution, aggregates results, validates thresholds
"""

import logging
import time
import psutil
import os
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Any, Optional, Tuple
from enum import Enum
from pathlib import Path
from datetime import datetime
import json
import csv

logger = logging.getLogger(__name__)


class BenchmarkCategory(Enum):
    """Categories for benchmark classification."""
    SOLVER_EXECUTION = "solver_execution"
    CONFIG_LOADING = "config_loading"
    CONFIG_VALIDATION = "config_validation"
    MEMORY_USAGE = "memory_usage"
    SCALABILITY = "scalability"
    INTEGRATION = "integration"


@dataclass
class PerformanceMetrics:
    """Container for performance metrics from a benchmark run."""
    execution_time_ms: float
    memory_peak_mb: float
    memory_avg_mb: float
    cpu_percent: float
    throughput: Optional[float] = None  # ops/sec for scalability tests
    iterations: int = 1
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    def to_dict(self) -> Dict[str, Any]:
        """Convert metrics to dictionary."""
        return asdict(self)


@dataclass
class BenchmarkResult:
    """Complete benchmark result with metrics and metadata."""
    name: str
    category: BenchmarkCategory
    metrics: PerformanceMetrics
    description: str = ""
    passed: bool = True
    threshold: Optional[Dict[str, float]] = None
    error: Optional[str] = None
    tags: List[str] = field(default_factory=list)
    baseline: Optional[PerformanceMetrics] = None

    def meets_threshold(self) -> Tuple[bool, List[str]]:
        """
        Check if metrics meet performance thresholds.

        Returns:
            Tuple of (meets_threshold, violation_messages)
        """
        if not self.threshold:
            return True, []

        violations = []

        if "max_time_ms" in self.threshold:
            if self.metrics.execution_time_ms > self.threshold["max_time_ms"]:
                violations.append(
                    f"Execution time {self.metrics.execution_time_ms:.2f}ms "
                    f"exceeds threshold {self.threshold['max_time_ms']:.2f}ms"
                )

        if "max_memory_mb" in self.threshold:
            if self.metrics.memory_peak_mb > self.threshold["max_memory_mb"]:
                violations.append(
                    f"Peak memory {self.metrics.memory_peak_mb:.2f}MB "
                    f"exceeds threshold {self.threshold['max_memory_mb']:.2f}MB"
                )

        if "max_cpu_percent" in self.threshold:
            if self.metrics.cpu_percent > self.threshold["max_cpu_percent"]:
                violations.append(
                    f"CPU usage {self.metrics.cpu_percent:.1f}% "
                    f"exceeds threshold {self.threshold['max_cpu_percent']:.1f}%"
                )

        if "min_throughput" in self.threshold and self.metrics.throughput:
            if self.metrics.throughput < self.threshold["min_throughput"]:
                violations.append(
                    f"Throughput {self.metrics.throughput:.2f} ops/sec "
                    f"below threshold {self.threshold['min_throughput']:.2f} ops/sec"
                )

        return len(violations) == 0, violations

    def get_comparison_to_baseline(self) -> Dict[str, float]:
        """
        Get performance comparison to baseline as percentage deltas.

        Returns:
            Dictionary with percentage changes
        """
        if not self.baseline:
            return {}

        deltas = {}
        deltas["time_delta_percent"] = (
            (self.metrics.execution_time_ms - self.baseline.execution_time_ms)
            / self.baseline.execution_time_ms
            * 100
        )
        deltas["memory_delta_percent"] = (
            (self.metrics.memory_peak_mb - self.baseline.memory_peak_mb)
            / self.baseline.memory_peak_mb
            * 100
        )

        if self.metrics.throughput and self.baseline.throughput:
            deltas["throughput_delta_percent"] = (
                (self.metrics.throughput - self.baseline.throughput)
                / self.baseline.throughput
                * 100
            )

        return deltas

    def to_dict(self) -> Dict[str, Any]:
        """Convert result to dictionary for serialization."""
        result_dict = {
            "name": self.name,
            "category": self.category.value,
            "description": self.description,
            "passed": self.passed,
            "error": self.error,
            "tags": self.tags,
            "metrics": self.metrics.to_dict(),
        }

        if self.threshold:
            result_dict["threshold"] = self.threshold

        if self.baseline:
            result_dict["baseline"] = self.baseline.to_dict()
            result_dict["comparison_to_baseline"] = self.get_comparison_to_baseline()

        return result_dict


@dataclass
class BenchmarkConfig:
    """Configuration for benchmark execution."""
    name: str
    max_time_ms: float = 1000.0
    max_memory_mb: float = 512.0
    max_cpu_percent: float = 95.0
    min_throughput: Optional[float] = None
    iterations: int = 1
    warmup_iterations: int = 1
    timeout_sec: int = 30
    collect_memory_samples: bool = True
    memory_sample_interval_sec: float = 0.01
    tags: List[str] = field(default_factory=list)

    def get_thresholds(self) -> Dict[str, float]:
        """Get threshold dictionary from config."""
        thresholds = {
            "max_time_ms": self.max_time_ms,
            "max_memory_mb": self.max_memory_mb,
            "max_cpu_percent": self.max_cpu_percent,
        }

        if self.min_throughput is not None:
            thresholds["min_throughput"] = self.min_throughput

        return thresholds


class BenchmarkExecutor:
    """Executes individual benchmarks with metric collection."""

    def __init__(self, config: BenchmarkConfig):
        """
        Initialize benchmark executor.

        Args:
            config: Benchmark configuration
        """
        self.config = config
        self.process = psutil.Process(os.getpid())

    def execute(
        self,
        func,
        *args,
        category: BenchmarkCategory = BenchmarkCategory.SOLVER_EXECUTION,
        description: str = "",
        **kwargs
    ) -> BenchmarkResult:
        """
        Execute benchmark function and collect metrics.

        Args:
            func: Function to benchmark
            *args: Positional arguments for function
            category: Benchmark category
            description: Benchmark description
            **kwargs: Keyword arguments for function

        Returns:
            BenchmarkResult with collected metrics
        """
        logger.debug(f"Executing benchmark: {self.config.name}")

        # Warmup iterations
        for _ in range(self.config.warmup_iterations):
            try:
                func(*args, **kwargs)
            except Exception as e:
                logger.warning(f"Warmup iteration failed: {e}")

        # Actual benchmark iterations
        metrics_list = []
        memory_samples = []

        for iteration in range(self.config.iterations):
            try:
                metrics = self._execute_single(func, args, kwargs, memory_samples)
                metrics_list.append(metrics)
            except Exception as e:
                logger.error(f"Benchmark iteration {iteration} failed: {e}")
                return self._create_error_result(category, description, str(e))

        # Aggregate metrics
        aggregated = self._aggregate_metrics(metrics_list)
        aggregated.iterations = self.config.iterations

        # Check thresholds
        result = BenchmarkResult(
            name=self.config.name,
            category=category,
            metrics=aggregated,
            description=description,
            threshold=self.config.get_thresholds(),
            tags=self.config.tags,
        )

        passed, violations = result.meets_threshold()
        result.passed = passed

        if not passed:
            logger.warning(f"Benchmark {self.config.name} failed thresholds: {violations}")
            result.error = "; ".join(violations)

        return result

    def _execute_single(
        self, func, args: Tuple, kwargs: Dict, memory_samples: List[float]
    ) -> PerformanceMetrics:
        """Execute single benchmark iteration and collect metrics."""
        # Force garbage collection before measurement
        import gc
        gc.collect()

        # Measure memory before
        mem_before = self.process.memory_info().rss / 1024 / 1024  # MB

        # Execute with timing
        start_time = time.perf_counter()
        cpu_percent_start = self.process.cpu_percent()

        func(*args, **kwargs)

        cpu_percent_end = self.process.cpu_percent()
        end_time = time.perf_counter()

        # Measure memory after
        mem_after = self.process.memory_info().rss / 1024 / 1024  # MB

        execution_time_ms = (end_time - start_time) * 1000
        memory_peak_mb = max(mem_before, mem_after)
        memory_avg_mb = (mem_before + mem_after) / 2
        cpu_percent = (cpu_percent_start + cpu_percent_end) / 2

        memory_samples.append(memory_peak_mb)

        return PerformanceMetrics(
            execution_time_ms=execution_time_ms,
            memory_peak_mb=memory_peak_mb,
            memory_avg_mb=memory_avg_mb,
            cpu_percent=cpu_percent,
        )

    def _aggregate_metrics(self, metrics_list: List[PerformanceMetrics]) -> PerformanceMetrics:
        """Aggregate metrics from multiple iterations."""
        if not metrics_list:
            raise ValueError("No metrics to aggregate")

        execution_times = [m.execution_time_ms for m in metrics_list]
        memory_peaks = [m.memory_peak_mb for m in metrics_list]
        memory_avgs = [m.memory_avg_mb for m in metrics_list]
        cpu_percents = [m.cpu_percent for m in metrics_list]

        # Use median for stability
        execution_times.sort()
        memory_peaks.sort()
        memory_avgs.sort()
        cpu_percents.sort()

        median_idx = len(execution_times) // 2

        return PerformanceMetrics(
            execution_time_ms=execution_times[median_idx],
            memory_peak_mb=max(memory_peaks),
            memory_avg_mb=sum(memory_avgs) / len(memory_avgs),
            cpu_percent=cpu_percents[median_idx],
            iterations=len(metrics_list),
        )

    def _create_error_result(
        self, category: BenchmarkCategory, description: str, error_msg: str
    ) -> BenchmarkResult:
        """Create error result."""
        return BenchmarkResult(
            name=self.config.name,
            category=category,
            metrics=PerformanceMetrics(0, 0, 0, 0),
            description=description,
            passed=False,
            error=error_msg,
        )


class BenchmarkAggregator:
    """Aggregates and analyzes benchmark results."""

    def __init__(self):
        """Initialize aggregator."""
        self.results: List[BenchmarkResult] = []
        self.baselines: Dict[str, PerformanceMetrics] = {}

    def add_result(self, result: BenchmarkResult) -> None:
        """
        Add benchmark result to collection.

        Args:
            result: BenchmarkResult to add
        """
        self.results.append(result)
        logger.debug(f"Added result: {result.name}")

    def add_results(self, results: List[BenchmarkResult]) -> None:
        """
        Add multiple results.

        Args:
            results: List of BenchmarkResults
        """
        self.results.extend(results)
        logger.debug(f"Added {len(results)} results")

    def set_baseline(self, name: str, metrics: PerformanceMetrics) -> None:
        """
        Set baseline metrics for comparison.

        Args:
            name: Benchmark name
            metrics: Baseline metrics
        """
        self.baselines[name] = metrics
        logger.debug(f"Set baseline for {name}")

    def get_summary(self) -> Dict[str, Any]:
        """
        Get summary statistics of all results.

        Returns:
            Dictionary with summary metrics
        """
        if not self.results:
            return {"total": 0, "passed": 0, "failed": 0}

        total = len(self.results)
        passed = sum(1 for r in self.results if r.passed)
        failed = total - passed

        execution_times = [r.metrics.execution_time_ms for r in self.results if r.passed]
        memory_peaks = [r.metrics.memory_peak_mb for r in self.results if r.passed]

        return {
            "total": total,
            "passed": passed,
            "failed": failed,
            "pass_rate": (passed / total * 100) if total > 0 else 0,
            "avg_execution_time_ms": sum(execution_times) / len(execution_times)
            if execution_times else 0,
            "avg_memory_peak_mb": sum(memory_peaks) / len(memory_peaks)
            if memory_peaks else 0,
            "max_execution_time_ms": max(execution_times) if execution_times else 0,
            "max_memory_peak_mb": max(memory_peaks) if memory_peaks else 0,
        }

    def get_by_category(self, category: BenchmarkCategory) -> List[BenchmarkResult]:
        """
        Get results by category.

        Args:
            category: BenchmarkCategory to filter

        Returns:
            List of results in category
        """
        return [r for r in self.results if r.category == category]

    def get_failures(self) -> List[BenchmarkResult]:
        """
        Get all failed benchmarks.

        Returns:
            List of failed BenchmarkResults
        """
        return [r for r in self.results if not r.passed]

    def export_to_json(self, filepath: Path) -> None:
        """
        Export results to JSON file.

        Args:
            filepath: Output file path
        """
        data = {
            "timestamp": datetime.now().isoformat(),
            "summary": self.get_summary(),
            "results": [r.to_dict() for r in self.results],
        }

        filepath.parent.mkdir(parents=True, exist_ok=True)

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

        logger.info(f"Exported results to {filepath}")

    def export_to_csv(self, filepath: Path) -> None:
        """
        Export results to CSV file.

        Args:
            filepath: Output file path
        """
        filepath.parent.mkdir(parents=True, exist_ok=True)

        with open(filepath, "w", newline="") as f:
            fieldnames = [
                "name",
                "category",
                "description",
                "passed",
                "execution_time_ms",
                "memory_peak_mb",
                "memory_avg_mb",
                "cpu_percent",
                "iterations",
                "tags",
            ]

            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()

            for result in self.results:
                row = {
                    "name": result.name,
                    "category": result.category.value,
                    "description": result.description,
                    "passed": result.passed,
                    "execution_time_ms": f"{result.metrics.execution_time_ms:.2f}",
                    "memory_peak_mb": f"{result.metrics.memory_peak_mb:.2f}",
                    "memory_avg_mb": f"{result.metrics.memory_avg_mb:.2f}",
                    "cpu_percent": f"{result.metrics.cpu_percent:.1f}",
                    "iterations": result.metrics.iterations,
                    "tags": ",".join(result.tags),
                }
                writer.writerow(row)

        logger.info(f"Exported results to {filepath}")


class BenchmarkSuite:
    """Main benchmark suite for coordinating all benchmarks."""

    def __init__(self, name: str, results_dir: Optional[Path] = None):
        """
        Initialize benchmark suite.

        Args:
            name: Suite name
            results_dir: Directory for saving results (default: ./benchmarks/results)
        """
        self.name = name
        self.results_dir = results_dir or Path("./benchmarks/results")
        self.results_dir.mkdir(parents=True, exist_ok=True)

        self.aggregator = BenchmarkAggregator()
        self.benchmarks: List[BenchmarkConfig] = []

        logger.debug(f"Initialized benchmark suite: {name}")

    def register_benchmark(self, config: BenchmarkConfig) -> None:
        """
        Register benchmark configuration.

        Args:
            config: BenchmarkConfig to register
        """
        self.benchmarks.append(config)
        logger.debug(f"Registered benchmark: {config.name}")

    def register_benchmarks(self, configs: List[BenchmarkConfig]) -> None:
        """
        Register multiple benchmark configurations.

        Args:
            configs: List of BenchmarkConfigs
        """
        self.benchmarks.extend(configs)
        logger.debug(f"Registered {len(configs)} benchmarks")

    def run_benchmark(
        self,
        config: BenchmarkConfig,
        func,
        *args,
        category: BenchmarkCategory = BenchmarkCategory.SOLVER_EXECUTION,
        description: str = "",
        **kwargs
    ) -> BenchmarkResult:
        """
        Execute a single benchmark.

        Args:
            config: BenchmarkConfig for execution
            func: Function to benchmark
            *args: Positional arguments
            category: Benchmark category
            description: Benchmark description
            **kwargs: Keyword arguments

        Returns:
            BenchmarkResult from execution
        """
        executor = BenchmarkExecutor(config)
        result = executor.execute(func, *args, category=category, description=description, **kwargs)
        self.aggregator.add_result(result)
        return result

    def run_all(self) -> Dict[str, Any]:
        """
        Run all registered benchmarks.

        Returns:
            Summary dictionary
        """
        logger.info(f"Running benchmark suite: {self.name} ({len(self.benchmarks)} benchmarks)")

        for config in self.benchmarks:
            logger.info(f"Executing benchmark: {config.name}")
            try:
                # Note: This is a placeholder for actual benchmark execution
                # Subclasses will override with actual implementations
                pass
            except Exception as e:
                logger.error(f"Benchmark {config.name} failed: {e}")

        summary = self.aggregator.get_summary()
        logger.info(f"Suite complete. Results: {summary}")

        return summary

    def save_results(self) -> Dict[str, Path]:
        """
        Save benchmark results to files.

        Returns:
            Dictionary with output file paths
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        json_file = self.results_dir / f"benchmark_results_{timestamp}.json"
        csv_file = self.results_dir / f"benchmark_results_{timestamp}.csv"

        self.aggregator.export_to_json(json_file)
        self.aggregator.export_to_csv(csv_file)

        logger.info(f"Saved results: JSON={json_file}, CSV={csv_file}")

        return {"json": json_file, "csv": csv_file}

    def get_report_data(self) -> Dict[str, Any]:
        """
        Get data for report generation.

        Returns:
            Dictionary suitable for report generation
        """
        return {
            "suite_name": self.name,
            "timestamp": datetime.now().isoformat(),
            "summary": self.aggregator.get_summary(),
            "results": [r.to_dict() for r in self.aggregator.results],
            "categories": {
                category.value: [
                    r.to_dict()
                    for r in self.aggregator.get_by_category(category)
                ]
                for category in BenchmarkCategory
            },
            "failures": [r.to_dict() for r in self.aggregator.get_failures()],
        }
