#!/usr/bin/env python
"""
OrcaFlex Module Test Runner

This script runs the OrcaFlex module tests with appropriate mode selection
based on license availability and user preferences.
"""

import sys
import os
import argparse
import subprocess
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.digitalmodel.modules.orcaflex.core.model_interface import check_orcaflex_available


def print_banner(title):
    """Print a formatted banner."""
    width = 60
    print("=" * width)
    print(f" {title:^{width-2}} ")
    print("=" * width)


def detect_environment():
    """Detect OrcaFlex environment."""
    status = check_orcaflex_available()
    
    print_banner("OrcaFlex Environment Detection")
    print(f"OrcFxAPI Module: {'[OK] Available' if status['has_module'] else '[X] Not Available'}")
    print(f"OrcaFlex License: {'[OK] Valid' if status['has_license'] else '[X] Invalid/Missing'}")
    print()
    
    return status


def run_tests(mode='auto', verbose=False, coverage=False, specific_test=None):
    """
    Run tests in specified mode.
    
    Args:
        mode: 'auto', 'mock', 'real', or 'both'
        verbose: Enable verbose output
        coverage: Generate coverage report
        specific_test: Run specific test file or pattern
    """
    status = detect_environment()
    
    # Determine test mode
    if mode == 'auto':
        if status['has_license']:
            mode = 'both'
            print("[>] AUTO MODE: License detected - will test BOTH real and mock")
        else:
            mode = 'mock'
            print("[>] AUTO MODE: No license - will test MOCK only")
    
    # Set environment variables based on mode
    env = os.environ.copy()
    
    if mode == 'mock':
        env['ORCAFLEX_FORCE_MOCK'] = '1'
        print_banner("Running MOCK Mode Tests")
        print("[i]  All tests will use mock implementation")
        print("[i]  This is suitable for CI/CD environments")
    elif mode == 'real':
        if not status['has_license']:
            print("[X] ERROR: Cannot run real mode without OrcaFlex license")
            return 1
        env['ORCAFLEX_SKIP_MOCK'] = '1'
        print_banner("Running REAL Mode Tests")
        print("[!]  All tests will use real OrcaFlex")
        print("[!]  This requires valid license")
    else:  # both
        if not status['has_license']:
            print("[!]  WARNING: 'both' mode requested but no license available")
            print("[i]  Falling back to mock-only mode")
            mode = 'mock'
            env['ORCAFLEX_FORCE_MOCK'] = '1'
        else:
            print_banner("Running DUAL Mode Tests")
            print("[OK] Tests will run in BOTH real and mock modes")
            print("[OK] This provides maximum coverage")
    
    # Build pytest command
    cmd = ['python', '-m', 'pytest']
    
    # Add test path
    if specific_test:
        cmd.append(specific_test)
    else:
        cmd.append('tests/modules/orcaflex')
    
    # Add options
    if verbose:
        cmd.extend(['-v', '-s'])  # Verbose with output
    
    if coverage:
        cmd.extend([
            '--cov=src/digitalmodel/modules/orcaflex',
            '--cov-report=html',
            '--cov-report=term'
        ])
    
    # Add marker selection based on mode
    if mode == 'mock':
        cmd.extend(['-m', 'not requires_orcaflex'])
    elif mode == 'real':
        cmd.extend(['-m', 'not requires_no_orcaflex'])
    
    print(f"\n[*] Running command: {' '.join(cmd)}\n")
    
    # Run tests
    result = subprocess.run(cmd, env=env)
    
    # Print summary
    print()
    if result.returncode == 0:
        print_banner("[OK] Tests PASSED")
        if mode == 'both':
            print("[OK] Both real and mock implementations tested successfully")
        elif mode == 'mock':
            print("[OK] Mock implementation tested successfully")
        else:
            print("[OK] Real OrcaFlex implementation tested successfully")
    else:
        print_banner("[X] Tests FAILED")
        print(f"Exit code: {result.returncode}")
    
    return result.returncode


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="OrcaFlex Module Test Runner",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                    # Auto-detect mode based on license
  %(prog)s --mock            # Force mock mode (for CI/CD)
  %(prog)s --real            # Force real mode (requires license)
  %(prog)s --both            # Test both modes (requires license)
  %(prog)s --coverage        # Generate coverage report
  %(prog)s test_config.py    # Run specific test file
        """
    )
    
    # Mode selection (mutually exclusive)
    mode_group = parser.add_mutually_exclusive_group()
    mode_group.add_argument(
        '--mock',
        action='store_const',
        const='mock',
        dest='mode',
        help='Force mock mode testing only'
    )
    mode_group.add_argument(
        '--real',
        action='store_const',
        const='real',
        dest='mode',
        help='Force real OrcaFlex testing only (requires license)'
    )
    mode_group.add_argument(
        '--both',
        action='store_const',
        const='both',
        dest='mode',
        help='Test both real and mock modes (requires license for real)'
    )
    
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose test output'
    )
    
    parser.add_argument(
        '-c', '--coverage',
        action='store_true',
        help='Generate coverage report'
    )
    
    parser.add_argument(
        'test',
        nargs='?',
        help='Specific test file or pattern to run'
    )
    
    parser.add_argument(
        '--check',
        action='store_true',
        help='Only check environment without running tests'
    )
    
    # Set default mode
    parser.set_defaults(mode='auto')
    
    args = parser.parse_args()
    
    if args.check:
        # Just check environment
        detect_environment()
        print("\n[OK] Environment check complete")
        return 0
    
    # Run tests
    return run_tests(
        mode=args.mode,
        verbose=args.verbose,
        coverage=args.coverage,
        specific_test=args.test
    )


if __name__ == '__main__':
    sys.exit(main())