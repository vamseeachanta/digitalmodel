"""
Test utility functions and helpers for digitalmodel testing.

This module provides common utilities used across all test files.
"""

import json
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Union
from unittest.mock import Mock, MagicMock
import pytest


class TestDataFactory:
    """Factory for creating test data objects."""

    @staticmethod
    def create_sample_file_structure(base_path: Path) -> Dict[str, Path]:
        """Create a sample file structure for testing."""
        structure = {}

        # Create directories
        (base_path / "src").mkdir()
        (base_path / "tests").mkdir()
        (base_path / "docs").mkdir()
        (base_path / "config").mkdir()

        # Create files
        files = {
            "readme": base_path / "README.md",
            "main": base_path / "src" / "main.py",
            "test": base_path / "tests" / "test_main.py",
            "config": base_path / "config" / "settings.yaml",
            "docs": base_path / "docs" / "guide.md"
        }

        for name, path in files.items():
            path.write_text(f"# {name.title()} file\nContent for {name}")
            structure[name] = path

        return structure

    @staticmethod
    def create_test_data_sets() -> Dict[str, Any]:
        """Create various test data sets."""
        return {
            "empty": [],
            "single_item": [1],
            "small_list": [1, 2, 3, 4, 5],
            "large_list": list(range(1000)),
            "mixed_types": [1, "two", 3.0, {"four": 4}],
            "nested_dict": {
                "level1": {
                    "level2": {
                        "data": [1, 2, 3]
                    }
                }
            },
            "string_data": {
                "short": "hello",
                "long": "a" * 1000,
                "unicode": "Hello 世界 🌍",
                "special_chars": "!@#$%^&*()_+{}[]|\\:;\"'<>?,./`~"
            }
        }

    @staticmethod
    def create_mock_api_responses() -> Dict[str, Any]:
        """Create mock API response data."""
        return {
            "success": {
                "status": 200,
                "data": {"message": "success", "items": [1, 2, 3]},
                "headers": {"Content-Type": "application/json"}
            },
            "error_404": {
                "status": 404,
                "data": {"error": "Not Found"},
                "headers": {"Content-Type": "application/json"}
            },
            "error_500": {
                "status": 500,
                "data": {"error": "Internal Server Error"},
                "headers": {"Content-Type": "application/json"}
            },
            "large_response": {
                "status": 200,
                "data": {"items": list(range(10000))},
                "headers": {"Content-Type": "application/json"}
            }
        }


class PerformanceAssertion:
    """Helper class for performance assertions."""

    def __init__(self, max_time: float = 1.0):
        self.max_time = max_time
        self.start_time = None

    def __enter__(self):
        self.start_time = time.perf_counter()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.start_time is not None:
            duration = time.perf_counter() - self.start_time
            assert duration < self.max_time, f"Operation took {duration:.3f}s, expected < {self.max_time}s"

    @staticmethod
    def assert_performance(func, *args, max_time: float = 1.0, **kwargs):
        """Assert that a function completes within the specified time."""
        start = time.perf_counter()
        result = func(*args, **kwargs)
        duration = time.perf_counter() - start

        assert duration < max_time, f"Function took {duration:.3f}s, expected < {max_time}s"
        return result

    @staticmethod
    def measure_performance(func, *args, **kwargs) -> tuple[Any, float]:
        """Measure function performance and return result and duration."""
        start = time.perf_counter()
        result = func(*args, **kwargs)
        duration = time.perf_counter() - start
        return result, duration


class MockManager:
    """Enhanced mock management for testing."""

    def __init__(self):
        self.mocks = {}
        self.patches = []

    def create_mock(self, name: str, **kwargs) -> Mock:
        """Create a named mock object."""
        mock = Mock(**kwargs)
        self.mocks[name] = mock
        return mock

    def create_mock_service(self, name: str) -> Mock:
        """Create a mock service with common methods."""
        mock = Mock()
        mock.connect.return_value = True
        mock.disconnect.return_value = True
        mock.get_data.return_value = {"status": "success", "data": []}
        mock.post_data.return_value = {"status": "created", "id": "123"}
        mock.is_healthy.return_value = True

        self.mocks[name] = mock
        return mock

    def get_mock(self, name: str) -> Optional[Mock]:
        """Get a named mock object."""
        return self.mocks.get(name)

    def reset_all_mocks(self):
        """Reset all managed mocks."""
        for mock in self.mocks.values():
            mock.reset_mock()

    def assert_mock_called(self, name: str, method: str, *args, **kwargs):
        """Assert that a mock method was called with specific arguments."""
        mock = self.get_mock(name)
        assert mock is not None, f"Mock '{name}' not found"

        method_mock = getattr(mock, method, None)
        assert method_mock is not None, f"Method '{method}' not found on mock '{name}'"

        if args or kwargs:
            method_mock.assert_called_with(*args, **kwargs)
        else:
            method_mock.assert_called()


class TestFileManager:
    """Manage test files and temporary directories."""

    def __init__(self):
        self.temp_dirs = []
        self.temp_files = []

    def create_temp_dir(self) -> Path:
        """Create a temporary directory."""
        temp_dir = Path(tempfile.mkdtemp())
        self.temp_dirs.append(temp_dir)
        return temp_dir

    def create_temp_file(self, content: str = "", suffix: str = ".txt") -> Path:
        """Create a temporary file with content."""
        temp_file = Path(tempfile.mktemp(suffix=suffix))
        temp_file.write_text(content)
        self.temp_files.append(temp_file)
        return temp_file

    def create_json_file(self, data: Any, suffix: str = ".json") -> Path:
        """Create a temporary JSON file."""
        temp_file = Path(tempfile.mktemp(suffix=suffix))
        temp_file.write_text(json.dumps(data, indent=2))
        self.temp_files.append(temp_file)
        return temp_file

    def cleanup(self):
        """Clean up all temporary files and directories."""
        import shutil

        for temp_file in self.temp_files:
            if temp_file.exists():
                temp_file.unlink()

        for temp_dir in self.temp_dirs:
            if temp_dir.exists():
                shutil.rmtree(temp_dir)

        self.temp_files.clear()
        self.temp_dirs.clear()


class TestValidator:
    """Common validation functions for tests."""

    @staticmethod
    def validate_dict_structure(data: Dict, required_keys: List[str]) -> bool:
        """Validate that a dictionary has required keys."""
        return all(key in data for key in required_keys)

    @staticmethod
    def validate_list_types(data: List, expected_type: type) -> bool:
        """Validate that all items in a list are of expected type."""
        return all(isinstance(item, expected_type) for item in data)

    @staticmethod
    def validate_range(value: Union[int, float], min_val: float, max_val: float) -> bool:
        """Validate that a value is within expected range."""
        return min_val <= value <= max_val

    @staticmethod
    def validate_file_content(file_path: Path, expected_patterns: List[str]) -> bool:
        """Validate that a file contains expected patterns."""
        if not file_path.exists():
            return False

        content = file_path.read_text()
        return all(pattern in content for pattern in expected_patterns)

    @staticmethod
    def validate_json_schema(data: Any, schema: Dict) -> bool:
        """Basic JSON schema validation."""
        try:
            if isinstance(schema, dict) and isinstance(data, dict):
                for key, expected_type in schema.items():
                    if key not in data:
                        return False
                    if not isinstance(data[key], expected_type):
                        return False
                return True
            return isinstance(data, schema)
        except Exception:
            return False


class TestMetrics:
    """Collect and analyze test metrics."""

    def __init__(self):
        self.metrics = {}
        self.timings = {}

    def record_metric(self, name: str, value: Any):
        """Record a test metric."""
        self.metrics[name] = value

    def start_timer(self, name: str):
        """Start a timer for measuring duration."""
        self.timings[name] = {"start": time.perf_counter()}

    def stop_timer(self, name: str) -> float:
        """Stop a timer and return duration."""
        if name in self.timings and "start" in self.timings[name]:
            duration = time.perf_counter() - self.timings[name]["start"]
            self.timings[name]["duration"] = duration
            return duration
        return 0.0

    def get_metric(self, name: str) -> Any:
        """Get a recorded metric."""
        return self.metrics.get(name)

    def get_duration(self, name: str) -> float:
        """Get duration for a named timer."""
        return self.timings.get(name, {}).get("duration", 0.0)

    def get_all_metrics(self) -> Dict[str, Any]:
        """Get all recorded metrics."""
        return {
            "metrics": self.metrics.copy(),
            "timings": {k: v.get("duration", 0.0) for k, v in self.timings.items()}
        }

    def assert_performance_target(self, timer_name: str, max_duration: float):
        """Assert that a timer meets performance target."""
        duration = self.get_duration(timer_name)
        assert duration < max_duration, f"Timer '{timer_name}' took {duration:.3f}s, expected < {max_duration}s"


# Utility functions for common test operations
def assert_approximately_equal(actual: float, expected: float, tolerance: float = 1e-6):
    """Assert that two floating point numbers are approximately equal."""
    assert abs(actual - expected) < tolerance, f"Expected {expected}, got {actual} (tolerance: {tolerance})"


def assert_contains_all(container: Union[List, Dict, str], items: List[Any]):
    """Assert that container contains all specified items."""
    for item in items:
        assert item in container, f"Item '{item}' not found in container"


def assert_has_attributes(obj: Any, attributes: List[str]):
    """Assert that an object has all specified attributes."""
    for attr in attributes:
        assert hasattr(obj, attr), f"Object missing attribute '{attr}'"


def create_test_fixture(fixture_type: str, **kwargs) -> Any:
    """Factory function for creating test fixtures."""
    fixtures = {
        "mock_service": lambda: MockManager().create_mock_service("test_service"),
        "test_data": lambda: TestDataFactory.create_test_data_sets(),
        "temp_dir": lambda: TestFileManager().create_temp_dir(),
        "performance_tracker": lambda: TestMetrics(),
    }

    if fixture_type in fixtures:
        return fixtures[fixture_type]()
    else:
        raise ValueError(f"Unknown fixture type: {fixture_type}")


# Decorators for common test patterns
def with_performance_tracking(max_time: float = 1.0):
    """Decorator to add performance tracking to test functions."""
    def decorator(func):
        def wrapper(*args, **kwargs):
            with PerformanceAssertion(max_time):
                return func(*args, **kwargs)
        return wrapper
    return decorator


def with_cleanup(cleanup_func):
    """Decorator to ensure cleanup after test execution."""
    def decorator(func):
        def wrapper(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            finally:
                cleanup_func()
        return wrapper
    return decorator


# Context managers for test scenarios
class MockEnvironment:
    """Context manager for mocking environment variables."""

    def __init__(self, env_vars: Dict[str, str]):
        self.env_vars = env_vars
        self.original_values = {}

    def __enter__(self):
        import os
        for key, value in self.env_vars.items():
            self.original_values[key] = os.environ.get(key)
            os.environ[key] = value
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        import os
        for key, original_value in self.original_values.items():
            if original_value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = original_value


class TemporaryTestEnvironment:
    """Context manager for creating a complete temporary test environment."""

    def __init__(self):
        self.file_manager = TestFileManager()
        self.mock_manager = MockManager()
        self.metrics = TestMetrics()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file_manager.cleanup()
        self.mock_manager.reset_all_mocks()

    def create_file(self, content: str = "", suffix: str = ".txt") -> Path:
        return self.file_manager.create_temp_file(content, suffix)

    def create_mock(self, name: str, **kwargs) -> Mock:
        return self.mock_manager.create_mock(name, **kwargs)

    def record_metric(self, name: str, value: Any):
        self.metrics.record_metric(name, value)

    def get_metrics(self) -> Dict[str, Any]:
        return self.metrics.get_all_metrics()