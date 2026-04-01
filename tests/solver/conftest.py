"""Solver test configuration.

All tests in this directory require OrcFxAPI and must run on licensed-win-1.
They are automatically marked with @pytest.mark.solver.
"""
import pytest


def pytest_collection_modifyitems(items):
    """Auto-mark all tests in tests/solver/ with @pytest.mark.solver."""
    for item in items:
        if "solver" in str(item.fspath):
            item.add_marker(pytest.mark.solver)
