"""
Smoke test to validate that the test runner infrastructure works.
"""

def test_basic_math():
    """Basic test that should always pass."""
    assert 1 + 1 == 2

def test_runner_infrastructure():
    """Test that basic Python functionality works in test context."""
    # Test basic operations
    test_dict = {'key': 'value'}
    assert test_dict['key'] == 'value'
    
    # Test list comprehension
    squares = [x*x for x in range(3)]
    assert squares == [0, 1, 4]

def test_imports_basic():
    """Test that basic imports work without digitalmodel dependencies."""
    import os
    import json
    import sys
    
    assert os.getcwd() is not None
    assert json.dumps({'test': True}) == '{"test": true}'
    assert len(sys.argv) >= 1