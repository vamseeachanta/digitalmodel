"""
ABOUTME: Configuration manager performance benchmarks
ABOUTME: Tests for YAML loading, validation, and access patterns
"""

import logging
import yaml
import tempfile
from typing import Dict, List, Any
from pathlib import Path

from digitalmodel.base_configs import ConfigManager, ConfigLoader, SchemaValidator
from .benchmark_suite import (
    BenchmarkConfig,
    BenchmarkResult,
    BenchmarkCategory,
    BenchmarkExecutor,
    BenchmarkSuite,
)

logger = logging.getLogger(__name__)


class ConfigurationBenchmarks:
    """Benchmark suite for configuration manager performance testing."""

    def __init__(self, results_dir: Path = None):
        """
        Initialize configuration benchmarks.

        Args:
            results_dir: Directory for saving results
        """
        self.suite = BenchmarkSuite("Configuration Benchmarks", results_dir)
        self._register_config_benchmarks()
        self._create_test_configs()

    def _register_config_benchmarks(self) -> None:
        """Register all configuration benchmark configurations."""
        # YAML loading benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="config_yaml_load_small",
                max_time_ms=50.0,
                iterations=10,
                tags=["yaml_loading"],
            ),
            BenchmarkConfig(
                name="config_yaml_load_medium",
                max_time_ms=100.0,
                iterations=5,
                tags=["yaml_loading"],
            ),
            BenchmarkConfig(
                name="config_yaml_load_large",
                max_time_ms=500.0,
                iterations=2,
                tags=["yaml_loading"],
            ),
        ])

        # Validation benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="config_validation_simple",
                max_time_ms=50.0,
                iterations=10,
                tags=["validation"],
            ),
            BenchmarkConfig(
                name="config_validation_complex",
                max_time_ms=200.0,
                iterations=5,
                tags=["validation"],
            ),
        ])

        # ConfigManager benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="config_manager_initialization",
                max_time_ms=50.0,
                iterations=10,
                tags=["initialization"],
            ),
            BenchmarkConfig(
                name="config_manager_get_operation",
                max_time_ms=10.0,
                iterations=100,
                tags=["access"],
            ),
            BenchmarkConfig(
                name="config_manager_set_operation",
                max_time_ms=20.0,
                iterations=50,
                tags=["access"],
            ),
            BenchmarkConfig(
                name="config_manager_nested_access",
                max_time_ms=10.0,
                iterations=100,
                tags=["access"],
            ),
        ])

    def _create_test_configs(self) -> None:
        """Create test YAML configuration files."""
        self.temp_dir = tempfile.TemporaryDirectory()
        self.temp_path = Path(self.temp_dir.name)

        # Small config
        small_config = {
            "app": {
                "name": "test",
                "version": "1.0.0",
            }
        }

        # Medium config
        medium_config = {
            "app": {
                "name": "test",
                "version": "1.0.0",
                "debug": True,
            },
            "database": {
                "host": "localhost",
                "port": 5432,
                "user": "test",
                "password": "test",
            },
            "solver": {
                "type": "structural",
                "tolerance": 1e-6,
                "max_iterations": 1000,
                "parameters": {
                    "param1": 1.5,
                    "param2": 2.5,
                    "param3": 3.5,
                }
            }
        }

        # Large config with nested structures
        large_config = {f"section_{i}": medium_config for i in range(10)}

        # Save configs
        self.small_config_file = self.temp_path / "config_small.yaml"
        self.medium_config_file = self.temp_path / "config_medium.yaml"
        self.large_config_file = self.temp_path / "config_large.yaml"

        with open(self.small_config_file, "w") as f:
            yaml.dump(small_config, f)

        with open(self.medium_config_file, "w") as f:
            yaml.dump(medium_config, f)

        with open(self.large_config_file, "w") as f:
            yaml.dump(large_config, f)

    def benchmark_yaml_loading(self, config_file: Path) -> BenchmarkResult:
        """
        Benchmark YAML file loading.

        Args:
            config_file: Path to YAML file

        Returns:
            BenchmarkResult
        """
        config_size = "unknown"
        if "small" in config_file.name:
            config_size = "small"
        elif "medium" in config_file.name:
            config_size = "medium"
        elif "large" in config_file.name:
            config_size = "large"

        config = BenchmarkConfig(
            name=f"yaml_loading_{config_size}",
            max_time_ms=500.0,
            iterations=5,
            tags=["yaml_loading", config_size],
        )

        def load_yaml():
            with open(config_file, "r") as f:
                yaml.safe_load(f)

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            load_yaml,
            category=BenchmarkCategory.CONFIG_LOADING,
            description=f"Loading {config_size} YAML configuration",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_loader(self, config_file: Path) -> BenchmarkResult:
        """
        Benchmark ConfigLoader performance.

        Args:
            config_file: Path to YAML file

        Returns:
            BenchmarkResult
        """
        config_size = "unknown"
        if "small" in config_file.name:
            config_size = "small"
        elif "medium" in config_file.name:
            config_size = "medium"
        elif "large" in config_file.name:
            config_size = "large"

        config = BenchmarkConfig(
            name=f"config_loader_{config_size}",
            max_time_ms=500.0,
            iterations=5,
            tags=["config_loading", config_size],
        )

        loader = ConfigLoader()

        def load_config():
            loader.load_from_file(config_file)

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            load_config,
            category=BenchmarkCategory.CONFIG_LOADING,
            description=f"ConfigLoader with {config_size} file",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_schema_validation(self, config_data: Dict) -> BenchmarkResult:
        """
        Benchmark schema validation performance.

        Args:
            config_data: Configuration data to validate

        Returns:
            BenchmarkResult
        """
        config_size = "complex" if len(str(config_data)) > 1000 else "simple"

        config = BenchmarkConfig(
            name=f"schema_validation_{config_size}",
            max_time_ms=200.0,
            iterations=5,
            tags=["validation", config_size],
        )

        validator = SchemaValidator()

        def validate_schema():
            # This would use the actual schema from config_data
            validator.validate(config_data, {})

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            validate_schema,
            category=BenchmarkCategory.CONFIG_VALIDATION,
            description=f"Schema validation with {config_size} data",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_manager_initialization(self) -> BenchmarkResult:
        """
        Benchmark ConfigManager initialization.

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name="config_manager_init",
            max_time_ms=50.0,
            iterations=10,
            tags=["initialization"],
        )

        def init_manager():
            ConfigManager()

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            init_manager,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="ConfigManager initialization time",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_get_operations(self) -> BenchmarkResult:
        """
        Benchmark configuration get operations.

        Returns:
            BenchmarkResult
        """
        manager = ConfigManager()
        manager.set("test.nested.value", 42)
        manager.set("test.string", "hello")
        manager.set("test.list", [1, 2, 3])

        config = BenchmarkConfig(
            name="config_get_operations",
            max_time_ms=10.0,
            iterations=100,
            tags=["access"],
        )

        def get_values():
            manager.get("test.nested.value")
            manager.get("test.string")
            manager.get("test.list")

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            get_values,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="Configuration get operation performance",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_set_operations(self) -> BenchmarkResult:
        """
        Benchmark configuration set operations.

        Returns:
            BenchmarkResult
        """
        manager = ConfigManager()

        config = BenchmarkConfig(
            name="config_set_operations",
            max_time_ms=20.0,
            iterations=50,
            tags=["access"],
        )

        counter = [0]

        def set_values():
            manager.set(f"test.key_{counter[0]}", counter[0])
            counter[0] += 1

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            set_values,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="Configuration set operation performance",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_nested_config_access(self) -> BenchmarkResult:
        """
        Benchmark nested configuration access patterns.

        Returns:
            BenchmarkResult
        """
        manager = ConfigManager()

        # Create deeply nested config
        nested = {"level1": {"level2": {"level3": {"level4": {"value": 42}}}}}
        manager.set("deep.structure", nested)

        config = BenchmarkConfig(
            name="nested_config_access",
            max_time_ms=10.0,
            iterations=100,
            tags=["access"],
        )

        def access_nested():
            manager.get("deep.structure.level1.level2.level3.level4.value", None)

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            access_nested,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="Deeply nested configuration access",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_update_performance(self) -> BenchmarkResult:
        """
        Benchmark configuration update performance.

        Returns:
            BenchmarkResult
        """
        manager = ConfigManager()
        initial_config = {
            "param1": 1.0,
            "param2": 2.0,
            "nested": {
                "a": 1,
                "b": 2,
                "c": 3,
            }
        }

        for key, value in initial_config.items():
            if isinstance(value, dict):
                for subkey, subvalue in value.items():
                    manager.set(f"config.{key}.{subkey}", subvalue)
            else:
                manager.set(f"config.{key}", value)

        config = BenchmarkConfig(
            name="config_update_performance",
            max_time_ms=50.0,
            iterations=10,
            tags=["access"],
        )

        counter = [0]

        def update_config():
            manager.set("config.param1", 1.0 + counter[0] * 0.1)
            manager.set("config.param2", 2.0 + counter[0] * 0.1)
            counter[0] += 1

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            update_config,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="Configuration update performance",
        )
        self.suite.aggregator.add_result(result)
        return result

    def benchmark_config_copy_performance(self) -> BenchmarkResult:
        """
        Benchmark configuration copying and deep copies.

        Returns:
            BenchmarkResult
        """
        manager = ConfigManager()

        # Create large config
        for i in range(100):
            manager.set(f"key_{i}", {"value": i, "nested": {"a": i, "b": i * 2}})

        config = BenchmarkConfig(
            name="config_copy_performance",
            max_time_ms=100.0,
            iterations=10,
            tags=["access"],
        )

        def copy_config():
            import copy
            config_copy = copy.deepcopy(manager.config)

        executor = BenchmarkExecutor(config)
        result = executor.execute(
            copy_config,
            category=BenchmarkCategory.CONFIG_LOADING,
            description="Configuration deep copy performance",
        )
        self.suite.aggregator.add_result(result)
        return result

    def run_all_benchmarks(self) -> Dict[str, Any]:
        """
        Run all configuration benchmarks.

        Returns:
            Summary of benchmark results
        """
        logger.info("Running all configuration benchmarks")

        # Benchmark YAML loading
        self.suite.aggregator.add_result(self.benchmark_yaml_loading(self.small_config_file))
        self.suite.aggregator.add_result(self.benchmark_yaml_loading(self.medium_config_file))
        self.suite.aggregator.add_result(self.benchmark_yaml_loading(self.large_config_file))

        # Benchmark ConfigLoader
        self.suite.aggregator.add_result(self.benchmark_config_loader(self.small_config_file))
        self.suite.aggregator.add_result(self.benchmark_config_loader(self.medium_config_file))
        self.suite.aggregator.add_result(self.benchmark_config_loader(self.large_config_file))

        # Benchmark ConfigManager
        self.suite.aggregator.add_result(self.benchmark_config_manager_initialization())
        self.suite.aggregator.add_result(self.benchmark_config_get_operations())
        self.suite.aggregator.add_result(self.benchmark_config_set_operations())
        self.suite.aggregator.add_result(self.benchmark_nested_config_access())
        self.suite.aggregator.add_result(self.benchmark_config_update_performance())
        self.suite.aggregator.add_result(self.benchmark_config_copy_performance())

        summary = self.suite.aggregator.get_summary()
        results = self.suite.save_results()
        logger.info(f"Configuration benchmarks complete. Files: {results}")

        return summary

    def get_results_summary(self) -> Dict[str, Any]:
        """
        Get summary of all benchmark results.

        Returns:
            Summary dictionary
        """
        return self.suite.aggregator.get_summary()

    def cleanup(self) -> None:
        """Clean up temporary files."""
        if hasattr(self, 'temp_dir'):
            self.temp_dir.cleanup()
