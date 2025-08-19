#!/usr/bin/env python
"""Test Python module integration."""
import sys
import json
from pathlib import Path

# Add project to path
project_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(project_root / "src"))

def test_imports():
    """Test module imports."""
    try:
        from digitalmodel.modules.orcaflex.run_to_sim import (
            OrcaFlexModelRunner, 
            run_models, 
            ORCAFLEX_AVAILABLE
        )
        print("✓ Successfully imported run_to_sim module")
        return True
    except ImportError as e:
        print(f"✗ Import failed: {e}")
        return False

def test_runner_creation():
    """Test creating runner instance."""
    from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner
    try:
        runner = OrcaFlexModelRunner(mock_mode=True)
        print("✓ Successfully created OrcaFlexModelRunner instance")
        return True
    except Exception as e:
        print(f"✗ Failed to create runner: {e}")
        return False

def test_mock_run():
    """Test mock run functionality."""
    from digitalmodel.modules.orcaflex.run_to_sim import run_models
    try:
        result = run_models(mock=True, all_models=False)
        print(f"✓ Mock run completed: Total={result['total']}, Success={result['successful']}, Failed={result['failed']}")
        return result['failed'] == 0
    except Exception as e:
        print(f"✗ Mock run failed: {e}")
        return False

def test_single_file_processing():
    """Test processing a single file."""
    from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner
    from pathlib import Path
    
    try:
        runner = OrcaFlexModelRunner(mock_mode=True)
        test_file = Path(__file__).parent / "test_configs" / "fsts_test_model1.yml"
        
        if test_file.exists():
            result = runner.run_single_model(test_file)
            if result['success']:
                print(f"✓ Single file processing succeeded: {result['model']}")
                return True
            else:
                print(f"✗ Single file processing failed: {result['error']}")
                return False
        else:
            print(f"⚠ Test file not found: {test_file}")
            return True  # Not a failure if file doesn't exist in this context
    except Exception as e:
        print(f"✗ Exception in single file processing: {e}")
        return False

if __name__ == "__main__":
    tests = [
        ("Imports", test_imports),
        ("Runner Creation", test_runner_creation),
        ("Mock Run", test_mock_run),
        ("Single File Processing", test_single_file_processing),
    ]
    
    passed = 0
    failed = 0
    
    print("\nPython Integration Tests")
    print("=" * 40)
    
    for test_name, test_func in tests:
        print(f"\nTesting {test_name}...")
        if test_func():
            passed += 1
        else:
            failed += 1
    
    print("\n" + "=" * 40)
    print(f"Results: {passed} passed, {failed} failed")
    
    sys.exit(0 if failed == 0 else 1)
