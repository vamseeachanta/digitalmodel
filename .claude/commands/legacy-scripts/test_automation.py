#!/usr/bin/env python3
"""
Cross-repository test automation slash command.

This command provides a unified interface to comprehensive test automation
capabilities including discovery, execution, failure analysis, auto-fix,
and reporting. Designed for cross-repository reusability.

Usage Examples:
    /test-automation run-all --parallel --coverage
    /test-automation run-module aqwa --verbose
    /test-automation status --detailed
    /test-automation fix-failing --fix-auto
    /test-automation report --format html
    /test-automation before specs/modules/my-feature/
    /test-automation after specs/modules/my-feature/ --notes "Added new API endpoints"
"""

import sys
import os
import subprocess
import argparse
from pathlib import Path
from typing import List, Optional, Dict, Any
import json
import time
from datetime import datetime

# Add current directory to Python path for module imports
current_dir = Path(__file__).parent.parent.parent
if str(current_dir) not in sys.path:
    sys.path.insert(0, str(current_dir))

class TestAutomationCommand:
    """
    Cross-repository test automation command interface.
    
    Provides a unified interface to comprehensive test automation capabilities
    that can be used across different repository structures and configurations.
    """
    
    def __init__(self):
        self.name = "test-automation"
        self.description = "Comprehensive test automation with cross-repo reusability"
        self.version = "1.0.0"
        
    def create_parser(self) -> argparse.ArgumentParser:
        """Create and configure the argument parser."""
        
        parser = argparse.ArgumentParser(
            prog='test-automation',
            description='Cross-repository test automation with comprehensive capabilities',
            formatter_class=argparse.RawDescriptionHelpFormatter,
            epilog='''
Examples:
  # Execute all tests with parallel execution and coverage
  /test-automation run-all --parallel --coverage --fix-auto
  
  # Run specific module with detailed output
  /test-automation run-module aqwa --verbose --coverage
  
  # Get comprehensive test suite status
  /test-automation status --detailed
  
  # Analyze and auto-fix failing tests
  /test-automation fix-failing --fix-auto --mark-manual
  
  # Generate comprehensive reports
  /test-automation report --format html --include-trends
  
  # Before/after implementation tracking
  /test-automation before specs/modules/my-feature/
  /test-automation after specs/modules/my-feature/ --notes "Implementation notes"
  
  # Clean up test artifacts
  /test-automation clean --all
            '''
        )
        
        parser.add_argument(
            '--version', 
            action='version', 
            version=f'%(prog)s {self.version}'
        )
        
        parser.add_argument(
            '--help-full',
            action='store_true',
            help='Show comprehensive help and documentation'
        )
        
        # Subcommands
        subparsers = parser.add_subparsers(dest='command', help='Available commands')
        
        # run-all command
        run_all_parser = subparsers.add_parser(
            'run-all',
            help='Execute all tests by module with comprehensive analysis'
        )
        run_all_parser.add_argument(
            '--parallel',
            action='store_true',
            default=True,
            help='Run tests in parallel (default: true)'
        )
        run_all_parser.add_argument(
            '--verbose',
            action='store_true',
            help='Show detailed test output'
        )
        run_all_parser.add_argument(
            '--coverage',
            action='store_true',
            help='Generate comprehensive coverage reports'
        )
        run_all_parser.add_argument(
            '--coverage-threshold',
            type=float,
            default=80.0,
            help='Minimum coverage threshold percentage (default: 80)'
        )
        run_all_parser.add_argument(
            '--coverage-report-format',
            choices=['html', 'json', 'lcov', 'xml'],
            default='html',
            help='Coverage report format (default: html)'
        )
        run_all_parser.add_argument(
            '--fix-auto',
            action='store_true',
            help='Automatically fix resolvable issues'
        )
        run_all_parser.add_argument(
            '--mark-manual',
            action='store_true',
            help='Mark unfixable tests for manual review'
        )
        
        # run-module command
        run_module_parser = subparsers.add_parser(
            'run-module',
            help='Execute tests for specific module'
        )
        run_module_parser.add_argument(
            'module_name',
            help='Name of the module to test'
        )
        run_module_parser.add_argument(
            '--verbose',
            action='store_true',
            help='Show detailed test output'
        )
        run_module_parser.add_argument(
            '--coverage',
            action='store_true',
            help='Generate coverage reports for module'
        )
        run_module_parser.add_argument(
            '--fix-auto',
            action='store_true',
            help='Automatically fix resolvable issues'
        )
        
        # status command
        status_parser = subparsers.add_parser(
            'status',
            help='Show comprehensive test suite status'
        )
        status_parser.add_argument(
            '--detailed',
            action='store_true',
            help='Show detailed status information'
        )
        status_parser.add_argument(
            '--json',
            action='store_true',
            help='Output status in JSON format'
        )
        
        # fix-failing command
        fix_parser = subparsers.add_parser(
            'fix-failing',
            help='Analyze and auto-fix failing tests'
        )
        fix_parser.add_argument(
            '--fix-auto',
            action='store_true',
            help='Automatically fix resolvable issues'
        )
        fix_parser.add_argument(
            '--mark-manual',
            action='store_true',
            help='Mark unfixable tests for manual review'
        )
        fix_parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show what would be fixed without making changes'
        )
        fix_parser.add_argument(
            '--coverage-improvements',
            action='store_true',
            help='Include coverage-focused fixes'
        )
        
        # report command
        report_parser = subparsers.add_parser(
            'report',
            help='Generate comprehensive test reports'
        )
        report_parser.add_argument(
            '--format',
            choices=['html', 'json', 'csv'],
            default='html',
            help='Report format (default: html)'
        )
        report_parser.add_argument(
            '--output',
            help='Output file path'
        )
        report_parser.add_argument(
            '--include-trends',
            action='store_true',
            help='Include historical trend analysis'
        )
        report_parser.add_argument(
            '--include-coverage-gaps',
            action='store_true',
            help='Include coverage gap analysis'
        )
        
        # clean command
        clean_parser = subparsers.add_parser(
            'clean',
            help='Clean up test artifacts and temporary files'
        )
        clean_parser.add_argument(
            '--cache',
            action='store_true',
            help='Clean test execution cache'
        )
        clean_parser.add_argument(
            '--reports',
            action='store_true',
            help='Clean old report files'
        )
        clean_parser.add_argument(
            '--logs',
            action='store_true',
            help='Clean old log files'
        )
        clean_parser.add_argument(
            '--all',
            action='store_true',
            help='Clean all temporary files'
        )
        
        # before command
        before_parser = subparsers.add_parser(
            'before',
            help='Capture baseline before implementation'
        )
        before_parser.add_argument(
            'spec_path',
            help='Path to spec folder or file'
        )
        before_parser.add_argument(
            '--description',
            help='Description of the implementation'
        )
        
        # after command  
        after_parser = subparsers.add_parser(
            'after',
            help='Capture baseline after implementation and generate comparison'
        )
        after_parser.add_argument(
            'spec_path',
            help='Path to spec folder or file'
        )
        after_parser.add_argument(
            '--notes',
            help='Implementation notes for the summary'
        )
        
        # compare command
        compare_parser = subparsers.add_parser(
            'compare',
            help='Compare two specific baselines'
        )
        compare_parser.add_argument(
            'before_label',
            help='Label of the before baseline'
        )
        compare_parser.add_argument(
            'after_label', 
            help='Label of the after baseline'
        )
        compare_parser.add_argument(
            '--spec-path',
            help='Spec path for generating summary'
        )
        
        return parser

    def check_repository_compatibility(self) -> Dict[str, Any]:
        """
        Check if current repository is compatible with test automation.
        
        Returns:
            Dict containing compatibility status and recommendations
        """
        compatibility = {
            'compatible': False,
            'test_framework': None,
            'test_directory': None,
            'recommendations': []
        }
        
        current_path = Path.cwd()
        
        # Check for Python test frameworks
        if (current_path / 'pyproject.toml').exists():
            compatibility['compatible'] = True
            compatibility['test_framework'] = 'pytest'
        elif (current_path / 'setup.py').exists():
            compatibility['compatible'] = True
            compatibility['test_framework'] = 'pytest/unittest'
        elif (current_path / 'requirements.txt').exists():
            compatibility['compatible'] = True
            compatibility['test_framework'] = 'pytest'
        
        # Check for test directories
        test_dirs = ['tests', 'test', 'testing']
        for test_dir in test_dirs:
            if (current_path / test_dir).exists():
                compatibility['test_directory'] = test_dir
                break
        
        # Generate recommendations if not fully compatible
        if not compatibility['compatible']:
            compatibility['recommendations'].append(
                "Install pytest: pip install pytest pytest-cov"
            )
        
        if not compatibility['test_directory']:
            compatibility['recommendations'].append(
                "Create tests/ directory for test files"
            )
            
        return compatibility

    def execute_native_command(self, args: List[str]) -> int:
        """
        Execute using native test automation system if available.
        
        Args:
            args: Command line arguments
            
        Returns:
            Exit code
        """
        try:
            # Check if we have the native test automation system
            test_automation_path = Path.cwd() / 'src' / 'test_automation'
            
            if test_automation_path.exists():
                # Use the native implementation
                cmd = [sys.executable, '-m', 'test_automation'] + args
                result = subprocess.run(cmd, cwd=Path.cwd())
                return result.returncode
            else:
                print("❌ Native test automation system not found")
                print("   This repository doesn't have the comprehensive test automation system.")
                print("   Falling back to basic pytest execution...")
                return self.execute_fallback_command(args)
                
        except Exception as e:
            print(f"❌ Error executing native command: {e}")
            return 1

    def execute_fallback_command(self, args: List[str]) -> int:
        """
        Execute fallback commands for repositories without native system.
        
        Args:
            args: Command line arguments
            
        Returns:
            Exit code
        """
        parser = self.create_parser()
        parsed_args = parser.parse_args(args)
        
        try:
            compatibility = self.check_repository_compatibility()
            
            if not compatibility['compatible']:
                print("❌ Repository not compatible with test automation")
                print("   Recommendations:")
                for rec in compatibility['recommendations']:
                    print(f"   • {rec}")
                return 1
            
            # Basic fallback implementations
            if parsed_args.command == 'run-all':
                return self._fallback_run_all(parsed_args, compatibility)
            elif parsed_args.command == 'run-module':
                return self._fallback_run_module(parsed_args, compatibility)
            elif parsed_args.command == 'status':
                return self._fallback_status(parsed_args, compatibility)
            elif parsed_args.command == 'fix-failing':
                print("❌ Auto-fix requires the comprehensive test automation system")
                print("   Consider implementing the full system for advanced features.")
                return 1
            elif parsed_args.command == 'report':
                return self._fallback_report(parsed_args, compatibility)
            elif parsed_args.command == 'clean':
                return self._fallback_clean(parsed_args)
            else:
                print(f"❌ Command '{parsed_args.command}' requires the comprehensive system")
                return 1
                
        except Exception as e:
            print(f"❌ Error in fallback execution: {e}")
            return 1

    def _fallback_run_all(self, args, compatibility: Dict[str, Any]) -> int:
        """Fallback implementation for run-all command."""
        test_dir = compatibility['test_directory'] or 'tests'
        
        print(f"🧪 Running all tests using fallback method...")
        print(f"   Test directory: {test_dir}")
        
        # Build pytest command
        cmd = ['python', '-m', 'pytest', test_dir, '-v']
        
        if args.coverage:
            cmd.extend(['--cov', '.', '--cov-report', 'html', '--cov-report', 'term'])
        
        if args.parallel:
            cmd.extend(['-n', 'auto'])  # Requires pytest-xdist
        
        try:
            result = subprocess.run(cmd, cwd=Path.cwd())
            
            if result.returncode == 0:
                print("✅ All tests completed successfully!")
            else:
                print("❌ Some tests failed!")
                
            return result.returncode
            
        except FileNotFoundError:
            print("❌ pytest not found. Install with: pip install pytest")
            if args.coverage:
                print("   For coverage: pip install pytest-cov")
            if args.parallel:
                print("   For parallel execution: pip install pytest-xdist")
            return 1
        except Exception as e:
            print(f"❌ Error running tests: {e}")
            return 1

    def _fallback_run_module(self, args, compatibility: Dict[str, Any]) -> int:
        """Fallback implementation for run-module command."""
        test_dir = compatibility['test_directory'] or 'tests'
        module_path = Path(test_dir) / args.module_name
        
        if not module_path.exists():
            print(f"❌ Module test path not found: {module_path}")
            return 1
        
        print(f"🧪 Running tests for module: {args.module_name}")
        
        cmd = ['python', '-m', 'pytest', str(module_path), '-v']
        
        if args.coverage:
            cmd.extend(['--cov', '.', '--cov-report', 'term'])
        
        try:
            result = subprocess.run(cmd, cwd=Path.cwd())
            return result.returncode
        except Exception as e:
            print(f"❌ Error running module tests: {e}")
            return 1

    def _fallback_status(self, args, compatibility: Dict[str, Any]) -> int:
        """Fallback implementation for status command."""
        test_dir = Path(compatibility['test_directory'] or 'tests')
        
        print("📊 Test Suite Status (Fallback Mode)")
        print("=" * 50)
        
        if not test_dir.exists():
            print(f"❌ Test directory not found: {test_dir}")
            return 1
        
        # Count test files
        test_files = list(test_dir.rglob("test_*.py")) + list(test_dir.rglob("*_test.py"))
        
        print(f"Test directory: {test_dir}")
        print(f"Test files found: {len(test_files)}")
        print(f"Framework: {compatibility['test_framework']}")
        
        if args.detailed:
            print("\nTest Files:")
            for test_file in sorted(test_files):
                rel_path = test_file.relative_to(test_dir)
                print(f"  • {rel_path}")
        
        return 0

    def _fallback_report(self, args, compatibility: Dict[str, Any]) -> int:
        """Fallback implementation for report command."""
        print("📋 Generating basic test report...")
        
        # Run tests with coverage
        test_dir = compatibility['test_directory'] or 'tests'
        cmd = ['python', '-m', 'pytest', test_dir, '--cov', '.', '--cov-report', args.format]
        
        try:
            result = subprocess.run(cmd, cwd=Path.cwd())
            
            if args.format == 'html':
                print("📄 HTML coverage report generated in htmlcov/")
            
            return result.returncode
            
        except Exception as e:
            print(f"❌ Error generating report: {e}")
            return 1

    def _fallback_clean(self, args) -> int:
        """Fallback implementation for clean command."""
        print("🧹 Cleaning test artifacts...")
        
        cleanup_paths = []
        
        if args.cache or args.all:
            cleanup_paths.extend([
                '.pytest_cache',
                '__pycache__',
                '*.pyc'
            ])
        
        if args.reports or args.all:
            cleanup_paths.extend([
                'htmlcov',
                '*.coverage',
                'coverage.xml',
                'test_results_*.json'
            ])
        
        if args.logs or args.all:
            cleanup_paths.extend([
                '*.log',
                'logs/'
            ])
        
        for path_pattern in cleanup_paths:
            if '*' in path_pattern:
                import glob
                for path in glob.glob(path_pattern):
                    try:
                        if os.path.isfile(path):
                            os.remove(path)
                            print(f"  Removed file: {path}")
                        elif os.path.isdir(path):
                            import shutil
                            shutil.rmtree(path)
                            print(f"  Removed directory: {path}")
                    except Exception as e:
                        print(f"  Warning: Could not remove {path}: {e}")
            else:
                path = Path(path_pattern)
                if path.exists():
                    try:
                        if path.is_file():
                            path.unlink()
                            print(f"  Removed file: {path}")
                        elif path.is_dir():
                            import shutil
                            shutil.rmtree(path)
                            print(f"  Removed directory: {path}")
                    except Exception as e:
                        print(f"  Warning: Could not remove {path}: {e}")
        
        print("✅ Cleanup completed")
        return 0

    def show_comprehensive_help(self):
        """Show comprehensive help documentation."""
        help_text = f"""
{self.name} v{self.version} - Comprehensive Test Automation

OVERVIEW:
  Cross-repository test automation system with comprehensive capabilities
  including test discovery, parallel execution, failure analysis, auto-fix,
  coverage tracking, and advanced reporting.

CORE FEATURES:
  • Test Discovery: Automatic identification of test modules and files
  • Parallel Execution: High-performance parallel test execution
  • Failure Analysis: AI-powered failure pattern recognition
  • Auto-Fix Engine: Automatic resolution of common test issues
  • Coverage Tracking: Multi-dimensional coverage analysis and reporting
  • Trend Analysis: Historical test and coverage trend tracking
  • Before/After Tracking: Implementation impact analysis
  • CI/CD Integration: Seamless pipeline integration

REPOSITORY COMPATIBILITY:
  • Native Mode: Full feature set in repositories with comprehensive system
  • Fallback Mode: Basic functionality in any Python repository with pytest
  • Cross-Repository: Reusable across different project structures

COMMAND CATEGORIES:

1. TEST EXECUTION:
   run-all        Execute all tests with comprehensive analysis
   run-module     Execute tests for specific module

2. ANALYSIS & FIXING:
   status         Show comprehensive test suite status
   fix-failing    Analyze and auto-fix failing tests

3. REPORTING:
   report         Generate comprehensive test reports
   clean          Clean up test artifacts

4. IMPLEMENTATION TRACKING:
   before         Capture baseline before implementation
   after          Capture baseline after implementation  
   compare        Compare specific baselines

USAGE PATTERNS:

Development Workflow:
  /test-automation run-all --coverage --fix-auto
  /test-automation run-module my_module --verbose
  /test-automation fix-failing --fix-auto

Implementation Tracking:
  /test-automation before specs/modules/feature/
  # ... implement feature ...
  /test-automation after specs/modules/feature/ --notes "Added new API"

Reporting & Analysis:
  /test-automation report --format html --include-trends
  /test-automation status --detailed

CI/CD Integration:
  /test-automation run-all --parallel --coverage --fix-auto

CROSS-REPOSITORY DEPLOYMENT:
  This command is designed for reuse across repositories. Simply copy
  this file to .agent-os/commands/ in any repository, and it will adapt
  to the available test infrastructure.

For comprehensive documentation, see the specs at:
  specs/modules/test-suite-automation/
        """
        print(help_text)

    def run(self, args: Optional[List[str]] = None) -> int:
        """
        Main entry point for the test automation command.
        
        Args:
            args: Command line arguments (for testing)
            
        Returns:
            Exit code (0 for success, non-zero for error)
        """
        if args is None:
            args = sys.argv[1:]
        
        # Handle comprehensive help
        if '--help-full' in args:
            self.show_comprehensive_help()
            return 0
        
        # Handle no arguments
        if not args:
            parser = self.create_parser()
            parser.print_help()
            return 0
        
        try:
            # Try native implementation first
            return self.execute_native_command(args)
            
        except KeyboardInterrupt:
            print("\n❌ Operation cancelled by user")
            return 130
        except Exception as e:
            print(f"❌ Unexpected error: {e}")
            return 1


def main():
    """Entry point when run as a script."""
    command = TestAutomationCommand()
    return command.run()


if __name__ == '__main__':
    sys.exit(main())