# ABOUTME: Pytest fixtures for benchmark tests.
# ABOUTME: Provides datasets of varying sizes for performance testing.

import pytest


@pytest.fixture
def benchmark_datasets():
    """
    Provide datasets of different sizes for performance benchmarking.

    Returns a dictionary with 'small', 'medium', and 'large' datasets
    containing numeric data suitable for performance testing operations
    such as filtering, statistics computation, and list processing.
    """
    return {
        "small": list(range(100)),
        "medium": list(range(1000)),
        "large": list(range(10000)),
    }
