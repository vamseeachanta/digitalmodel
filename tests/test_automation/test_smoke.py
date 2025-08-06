"""
Smoke tests for the test automation system itself.
These tests validate that our test runner infrastructure works correctly.
"""

def test_basic_math():
    """Basic test that should always pass."""
    assert 1 + 1 == 2

def test_string_operations():
    """Test string operations."""
    assert "hello".upper() == "HELLO"
    assert "world" in "hello world"

def test_list_operations():
    """Test list operations."""
    test_list = [1, 2, 3]
    assert len(test_list) == 3
    assert 2 in test_list

def test_imports_work():
    """Test that basic imports work."""
    import os
    import sys
    assert os.path.exists(".")
    assert len(sys.path) > 0

def test_test_automation_discovery():
    """Test that our discovery engine works."""
    from test_automation.core.discovery import TestDiscoveryEngine
    engine = TestDiscoveryEngine()
    modules = engine.discover_modules()
    assert len(modules) > 0
    assert isinstance(modules, dict)

def test_test_automation_config():
    """Test that configuration loading works."""
    from test_automation.config import config
    assert hasattr(config, 'execution')
    assert hasattr(config, 'paths')
    assert config.execution.max_workers > 0