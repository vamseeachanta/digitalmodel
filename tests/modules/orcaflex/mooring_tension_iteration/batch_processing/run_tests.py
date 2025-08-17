#!/usr/bin/env python
"""
Test Runner for OrcaFlex Batch Processing Tests
===============================================
Run this to verify the batch runner functionality.
"""

import sys
import unittest
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / 'src'))

# Import test module
from test_batch_runner import suite

if __name__ == '__main__':
    print("=" * 80)
    print("OrcaFlex Batch Runner Test Suite")
    print("=" * 80)
    print("\nThis test suite verifies:")
    print("  • Batch configuration loading")
    print("  • Model file validation")
    print("  • Length[2] modification handling")
    print("  • Includefile processing")
    print("  • Target tension CSV handling")
    print("  • .sim file generation (2 models → 2 .sim files)")
    print("  • Report generation")
    print("  • Mock mode operation")
    print("\n" + "=" * 80 + "\n")
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite())
    
    # Print summary
    print("\n" + "=" * 80)
    print("Test Summary")
    print("=" * 80)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    
    if result.wasSuccessful():
        print("\n✓ All tests passed!")
    else:
        print("\n✗ Some tests failed. See details above.")
    
    print("=" * 80)
    
    sys.exit(0 if result.wasSuccessful() else 1)