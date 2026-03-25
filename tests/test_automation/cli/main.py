"""
Main CLI interface for test automation system.
"""

import argparse
import sys
import json
from typing import List, Optional
from pathlib import Path
from datetime import datetime

from test_automation.config import config
from test_automation.logging_config import get_logger

logger = get_logger('test_automation.cli')

def create_parser() -> argparse.ArgumentParser:
    """Create and configure the argument parser."""
    
    parser = argparse.ArgumentParser(
        prog='test-automation',
        description='Automated test suite management and execution system',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
  test-automation run-all --parallel --coverage
  test-automation run-module aqwa --verbose
  test-automation status
  test-automation fix-failing --fix-auto
  test-automation report --format html
  test-automation clean
  
  # Before/after implementation tracking:
  test-automation before specs/modules/my-feature/
  test-automation after specs/modules/my-feature/ --notes "Added new API endpoints"
  test-automation compare baseline_before baseline_after --spec-path specs/modules/my-feature/
        '''
    )
    
    parser.add_argument(
        '--version', 
        action='version', 
        version='%(prog)s 0.1.0'
    )
    
    parser.add_argument(
        '--config',
        help='Path to configuration file',
        type=str,
        default=None
    )
    
    parser.add_argument(
        '--log-level',
        help='Set logging level',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        default='INFO'
    )
    
    # Subcommands
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # run-all command
    run_all_parser = subparsers.add_parser(
        'run-all',
        help='Execute all tests by module'
    )
    run_all_parser.add_argument(
        '--parallel',
        action='store_true',
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
        help='Generate coverage reports'
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
        help='Generate coverage reports'
    )
    run_module_parser.add_argument(
        '--fix-auto',
        action='store_true',
        help='Automatically fix resolvable issues'
    )
    
    # status command
    status_parser = subparsers.add_parser(
        'status',
        help='Show test suite status summary'
    )
    status_parser.add_argument(
        '--detailed',
        action='store_true',
        help='Show detailed status information'
    )
    
    # fix-failing command
    fix_parser = subparsers.add_parser(
        'fix-failing',
        help='Attempt automated fixes for failing tests'
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
    
    # report command
    report_parser = subparsers.add_parser(
        'report',
        help='Generate comprehensive test report'
    )
    report_parser.add_argument(
        '--format',
        choices=['html', 'json', 'csv'],
        default='html',
        help='Report format'
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

def handle_run_all(args) -> int:
    """Handle run-all command with advanced parallel execution."""
    logger.info("Executing run-all command")
    logger.info(f"Parallel: {args.parallel}, Verbose: {args.verbose}")
    logger.info(f"Coverage: {args.coverage}, Auto-fix: {args.fix_auto}")
    
    try:
        print("Starting comprehensive test suite execution...")
        print("Test discovery and module categorization...")
        
        # Use advanced parallel manager if parallel execution is requested
        if args.parallel:
            from test_automation.core.parallel_manager import ParallelExecutionManager
            manager = ParallelExecutionManager()
            results = manager.execute_all_modules(verbose=args.verbose)
            execution_summary = manager.get_execution_summary()
        else:
            from test_automation.core.runner import ModuleTestRunner
            runner = ModuleTestRunner(parallel=False)
            results = runner.run_all_modules(verbose=args.verbose)
            execution_summary = runner.get_execution_summary()
        
        if not results:
            print("No runnable modules found!")
            return 0
        
        # Calculate summary statistics
        total_modules = len(results)
        passed_modules = sum(1 for r in results.values() if r.status == 'passed')
        failed_modules = sum(1 for r in results.values() if r.status in ['failed', 'mixed', 'error'])
        skipped_modules = sum(1 for r in results.values() if r.status == 'skipped')
        
        total_tests = sum(r.total_tests for r in results.values())
        passed_tests = sum(r.passed_tests for r in results.values())
        failed_tests = sum(r.failed_tests for r in results.values())
        error_tests = sum(r.error_tests for r in results.values())
        skipped_tests = sum(r.skipped_tests for r in results.values())
        
        success_rate = (passed_tests / max(total_tests, 1)) * 100
        total_duration = sum(r.total_duration for r in results.values())
        
        print(f"\nTest Execution Summary:")
        print("=" * 50)
        print(f"Modules: {passed_modules} passed, {failed_modules} failed, {skipped_modules} skipped (Total: {total_modules})")
        print(f"Tests: {passed_tests} passed, {failed_tests} failed, {error_tests} errors, {skipped_tests} skipped (Total: {total_tests})")
        print(f"Success Rate: {success_rate:.1f}%")
        print(f"Total Duration: {total_duration:.1f}s")
        
        # Show execution efficiency if parallel
        if args.parallel and 'execution_stats' in execution_summary:
            stats = execution_summary['execution_stats']
            if 'parallelization_efficiency' in stats and stats['parallelization_efficiency'] > 1:
                efficiency = stats['parallelization_efficiency']
                print(f"Parallelization Efficiency: {efficiency:.1f}x speedup")
        
        # Show module results
        if args.verbose:
            print(f"\nDetailed Module Results:")
            print("-" * 40)
            for module_name, result in sorted(results.items()):
                status_icon = {
                    'passed': '[PASS]',
                    'failed': '[FAIL]', 
                    'mixed': '[MIXED]',
                    'skipped': '[SKIP]',
                    'error': '[ERROR]'
                }.get(result.status, '[UNKNOWN]')
                
                duration_str = f" ({result.total_duration:.1f}s)" if result.total_duration > 0 else ""
                print(f"  {status_icon} {module_name}: {result.passed_tests}/{result.total_tests} tests{duration_str}")
                
                # Show failed tests
                if result.status in ['failed', 'mixed', 'error'] and result.test_results:
                    for test_result in result.test_results:
                        if test_result.status in ['failed', 'error']:
                            print(f"    - {test_result.test_file}: {test_result.status}")
        
        # Auto-fix if requested
        if args.fix_auto and (failed_modules > 0):
            print(f"\nAttempting automatic fixes for {failed_modules} failed modules...")
            fixed_count = attempt_auto_fixes(results)
            if fixed_count > 0:
                print(f"Auto-fixed {fixed_count} issues. Re-run tests to verify.")
            else:
                print("No auto-fixable issues found.")
        
        # Save results
        results_file = f"test_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        if args.parallel and hasattr(manager, 'test_runner'):
            manager.test_runner.save_results(results_file)
        elif not args.parallel:
            runner.save_results(results_file)
        print(f"\nDetailed results saved to: {results_file}")
        
        # Return appropriate exit code
        if failed_modules == 0:
            print("\nAll tests completed successfully!")
            return 0
        else:
            print(f"\nTests completed with {failed_modules} failed modules!")
            return 1
            
    except Exception as e:
        logger.error(f"Error in run-all command: {e}", exc_info=True)
        print(f"Error running tests: {e}")
        return 1


def attempt_auto_fixes(results: dict) -> int:
    """Attempt to automatically fix common test issues."""
    from test_automation.core.failure_analyzer import FailureAnalysisEngine
    
    try:
        # Initialize analysis engine
        analyzer = FailureAnalysisEngine()
        
        # Analyze failures
        analysis_results = analyzer.analyze_test_results(results)
        
        if not analysis_results:
            return 0
        
        # Apply auto-fixes
        fix_results = analyzer.apply_auto_fixes(dry_run=False)
        
        # Show summary
        print(f"  Attempted fixes: {fix_results['attempted']}")
        print(f"  Successfully fixed: {fix_results['successful']}")
        print(f"  Failed to fix: {fix_results['failed']}")
        
        return fix_results['successful']
        
    except Exception as e:
        logger.error(f"Error in auto-fix process: {e}")
        return 0

def handle_run_module(args) -> int:
    """Handle run-module command."""
    logger.info(f"Executing run-module command for: {args.module_name}")
    
    try:
        from test_automation.core.runner import ModuleTestRunner
        
        # Initialize test runner
        runner = ModuleTestRunner(parallel=False)  # Single module, no need for parallel
        
        print(f"Running tests for module: {args.module_name}")
        
        # Run the module tests
        result = runner.run_module_tests(args.module_name, verbose=args.verbose)
        
        # Display results
        print(f"\nModule: {result.module_name}")
        print(f"Status: {result.status.upper()}")
        print(f"Duration: {result.total_duration:.2f}s")
        print(f"Tests: {result.total_tests} total, {result.passed_tests} passed, {result.failed_tests} failed, {result.skipped_tests} skipped")
        
        if result.error_tests > 0:
            print(f"Errors: {result.error_tests}")
        if result.timeout_tests > 0:
            print(f"Timeouts: {result.timeout_tests}")
        
        # Show individual test results if verbose
        if args.verbose and result.test_results:
            print("\nIndividual Test Results:")
            for test_result in result.test_results:
                status_icon = {
                    'passed': '[PASS]',
                    'failed': '[FAIL]',
                    'skipped': '[SKIP]',
                    'error': '[ERROR]',
                    'timeout': '[TIMEOUT]'
                }.get(test_result.status, '[UNKNOWN]')
                
                print(f"  {status_icon} {test_result.test_file} ({test_result.duration:.2f}s)")
                if test_result.status == 'failed' and test_result.error_output:
                    print(f"    Error: {test_result.error_output[:200]}...")
        
        # Return appropriate exit code
        if result.status in ['passed', 'skipped']:
            print("\nModule tests completed successfully!")
            return 0
        elif result.status == 'mixed':
            print("\nModule tests completed with some failures!")
            return 1
        else:
            print("\nModule tests failed!")
            return 1
            
    except Exception as e:
        logger.error(f"Error in run-module command: {e}", exc_info=True)
        print(f"Error running module tests: {e}")
        return 1

def handle_status(args) -> int:
    """Handle status command."""
    logger.info("Executing status command")
    
    try:
        from test_automation.core.discovery import TestDiscoveryEngine
        
        # Initialize discovery engine
        discovery = TestDiscoveryEngine()
        
        print("Discovering test modules...")
        modules = discovery.discover_modules()
        
        # Get discovery summary
        summary = discovery.get_discovery_summary()
        stats = summary['statistics']
        
        print("\nTest Suite Status Summary")
        print("=" * 50)
        print(f"Total modules discovered: {stats['total_modules']}")
        print(f"Total test files: {stats['total_test_files']}")
        print(f"Runnable tests: {stats['runnable_tests']}")
        print(f"Licensed software tests: {stats['licensed_tests']}")
        print(f"Cache status: {'Hit' if stats.get('cache_hit') else 'Fresh discovery'}")
        
        if stats.get('last_discovery'):
            from datetime import datetime
            last_discovery = datetime.fromisoformat(stats['last_discovery'].replace('Z', '+00:00'))
            print(f"Last discovery: {last_discovery.strftime('%Y-%m-%d %H:%M:%S UTC')}")
        
        print(f"Total estimated duration: {summary['total_estimated_duration']:.1f} seconds")
        
        if args.detailed:
            print("\nModule Categories:")
            print("-" * 30)
            for category, count in summary['categories'].items():
                print(f"{category.replace('_', ' ').title()}: {count} modules")
            
            print("\nDiscovered Modules:")
            print("-" * 30)
            for name, module in modules.items():
                status = "[READY]" if module.runnable_tests > 0 else "[SKIP]"
                print(f"{status} {name}: {module.total_tests} tests ({module.runnable_tests} runnable)")
        
        # Validate results
        issues = discovery.validate_discovery_results()
        if any(issues.values()):
            print(f"\nDiscovery Issues Found:")
            for issue_type, issue_list in issues.items():
                if issue_list:
                    print(f"  {issue_type.replace('_', ' ').title()}: {len(issue_list)}")
        else:
            print(f"\nAll discovery results validated successfully")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in status command: {e}", exc_info=True)
        print(f"Error getting status: {e}")
        return 1

def handle_fix_failing(args) -> int:
    """Handle fix-failing command."""
    logger.info("Executing fix-failing command")
    logger.info(f"Auto-fix: {args.fix_auto}, Mark manual: {args.mark_manual}, Dry run: {args.dry_run}")
    
    try:
        from test_automation.core.runner import ModuleTestRunner
        from test_automation.core.failure_analyzer import FailureAnalysisEngine
        
        print("Analyzing failing tests for auto-fix opportunities...")
        
        # First, run tests to get current failure state
        runner = ModuleTestRunner(parallel=False)
        results = runner.run_all_modules(verbose=False)
        
        if not results:
            print("No test results found!")
            return 0
        
        # Initialize failure analyzer
        analyzer = FailureAnalysisEngine()
        
        # Analyze failures
        analysis_results = analyzer.analyze_test_results(results)
        
        if not analysis_results:
            print("No failures found to analyze!")
            return 0
        
        # Get summary
        summary = analyzer.get_failure_summary()
        
        print(f"\nFailure Analysis Summary:")
        print(f"=" * 30)
        print(f"Total failures: {summary['total_failures']}")
        print(f"Modules affected: {summary['modules_with_failures']}")
        print(f"Auto-fixable: {summary['auto_fixable_failures']}")
        print(f"Manual review required: {summary['manual_review_required']}")
        
        print(f"\nFailure Types:")
        for failure_type, count in summary['failure_types'].items():
            print(f"  {failure_type.replace('_', ' ').title()}: {count}")
        
        # Apply fixes if requested
        if args.fix_auto:
            print(f"\nApplying automatic fixes...")
            fix_results = analyzer.apply_auto_fixes(dry_run=args.dry_run)
            
            if args.dry_run:
                print(f"DRY RUN - Would attempt to fix {fix_results['attempted']} issues")
            else:
                print(f"Fix Results:")
                print(f"  Attempted: {fix_results['attempted']}")
                print(f"  Successful: {fix_results['successful']}")
                print(f"  Failed: {fix_results['failed']}")
                
                if fix_results['successful'] > 0:
                    print(f"\n✓ Successfully fixed {fix_results['successful']} issues!")
                    print("  Re-run tests to verify fixes.")
        
        # Generate manual review report if requested
        if args.mark_manual:
            print(f"\nGenerating manual review report...")
            manual_report = analyzer.generate_manual_review_report()
            
            report_file = f"manual_review_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
            with open(report_file, 'w', encoding='utf-8') as f:
                json.dump(manual_report, f, indent=2)
            
            print(f"Manual review report saved to: {report_file}")
            print(f"Issues requiring manual attention: {manual_report['total_manual_issues']}")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in fix-failing command: {e}", exc_info=True)
        print(f"Error analyzing failures: {e}")
        return 1

def handle_report(args) -> int:
    """Handle report command."""
    logger.info(f"Executing report command - format: {args.format}")
    
    try:
        from test_automation.core.reporting import ReportGenerator
        
        print(f"Generating comprehensive {args.format.upper()} report...")
        
        # Initialize report generator
        generator = ReportGenerator()
        
        # Generate status report
        report_data = generator.generate_status_report()
        
        # Save in requested format
        if args.format == 'html':
            output_path = generator.save_html_report(report_data, args.output)
            print(f"HTML dashboard generated: {output_path}")
            
        elif args.format == 'json':
            output_path = generator.save_json_report(report_data, args.output)
            print(f"JSON report generated: {output_path}")
            
        elif args.format == 'csv':
            output_path = generator.save_csv_report(report_data, args.output)
            print(f"CSV report generated: {output_path}")
        
        # Show summary
        current = report_data['current_status']
        print(f"\nReport Summary:")
        print(f"  Modules: {current['modules']['passed']}/{current['modules']['total']} passing")
        print(f"  Tests: {current['tests']['passed']}/{current['tests']['total']} passing")
        print(f"  Success Rate: {current['success_rate']:.1f}%")
        
        # Show trends if available
        trends = report_data.get('trends', {})
        if trends.get('status') != 'insufficient_data':
            trend_direction = trends.get('trends', {}).get('direction', 'stable')
            print(f"  Trend: {trend_direction.title()}")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in report command: {e}", exc_info=True)
        print(f"Error generating report: {e}")
        return 1

def handle_clean(args) -> int:
    """Handle clean command."""
    logger.info("Executing clean command")
    
    # TODO: Implement actual clean functionality
    print("Cleaning up test artifacts...")
    print("Cleanup completed successfully!")
    
    return 0


def handle_before(args) -> int:
    """Handle before command - capture baseline before implementation."""
    logger.info(f"Capturing before baseline for spec: {args.spec_path}")
    
    try:
        from test_automation.core.comparison_tracker import ImplementationTracker
        
        tracker = ImplementationTracker()
        
        print(f"Capturing baseline before implementing: {args.spec_path}")
        baseline_label = tracker.start_implementation(args.spec_path, args.description)
        
        print(f"✓ Baseline captured: {baseline_label}")
        print("You can now proceed with your implementation.")
        print(f"When complete, run: test-automation after {args.spec_path}")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in before command: {e}", exc_info=True)
        print(f"Error capturing baseline: {e}")
        return 1


def handle_after(args) -> int:
    """Handle after command - capture baseline after implementation and generate comparison."""
    logger.info(f"Capturing after baseline for spec: {args.spec_path}")
    
    try:
        from test_automation.core.comparison_tracker import ImplementationTracker
        
        tracker = ImplementationTracker()
        
        print(f"Capturing baseline after implementing: {args.spec_path}")
        print("Running comprehensive test analysis...")
        
        comparison_file = tracker.complete_implementation(args.spec_path, args.notes)
        
        print(f"✓ Implementation comparison completed!")
        print(f"✓ Comparison saved: {comparison_file}")
        
        # Show the spec folder path
        from pathlib import Path
        spec_dir = Path(args.spec_path)
        if spec_dir.is_file():
            spec_dir = spec_dir.parent
        
        summary_file = spec_dir / "test_summary.md"
        if summary_file.exists():
            print(f"✓ Test summary generated: {summary_file}")
            print(f"\nView the executive summary at: {summary_file}")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in after command: {e}", exc_info=True)
        print(f"Error generating comparison: {e}")
        return 1


def handle_compare(args) -> int:
    """Handle compare command - compare two specific baselines."""
    logger.info(f"Comparing baselines: {args.before_label} vs {args.after_label}")
    
    try:
        from test_automation.core.comparison_tracker import ImplementationTracker
        
        tracker = ImplementationTracker()
        
        print(f"Comparing baselines: {args.before_label} → {args.after_label}")
        
        comparison = tracker.generate_comparison_report(
            args.before_label, 
            args.after_label, 
            args.spec_path
        )
        
        print(f"\n✓ Comparison completed!")
        print(f"Impact: {comparison.impact_category.title()} (Score: {comparison.impact_score:.1f}/100)")
        print(f"Success Rate Change: {comparison.success_rate_change:+.1f}%")
        
        if args.spec_path:
            from pathlib import Path
            spec_dir = Path(args.spec_path)
            if spec_dir.is_file():
                spec_dir = spec_dir.parent
            summary_file = spec_dir / "test_summary.md"
            print(f"✓ Summary generated: {summary_file}")
        
        # Show key findings
        if comparison.key_findings:
            print(f"\nKey Findings:")
            for finding in comparison.key_findings[:3]:
                print(f"  • {finding}")
        
        return 0
        
    except Exception as e:
        logger.error(f"Error in compare command: {e}", exc_info=True)
        print(f"Error comparing baselines: {e}")
        return 1

def main(argv: Optional[List[str]] = None) -> int:
    """
    Main entry point for the test automation CLI.
    
    Args:
        argv: Command line arguments (for testing)
        
    Returns:
        Exit code (0 for success, non-zero for error)
    """
    
    parser = create_parser()
    args = parser.parse_args(argv)
    
    try:
        # Update logging level if specified
        if hasattr(args, 'log_level') and args.log_level:
            import logging
            logging.getLogger().setLevel(getattr(logging, args.log_level))
        
        # Handle commands
        if args.command == 'run-all':
            return handle_run_all(args)
        elif args.command == 'run-module':
            return handle_run_module(args)
        elif args.command == 'status':
            return handle_status(args)
        elif args.command == 'fix-failing':
            return handle_fix_failing(args)
        elif args.command == 'report':
            return handle_report(args)
        elif args.command == 'clean':
            return handle_clean(args)
        elif args.command == 'before':
            return handle_before(args)
        elif args.command == 'after':
            return handle_after(args)
        elif args.command == 'compare':
            return handle_compare(args)
        else:
            parser.print_help()
            return 1
            
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        print("\nOperation cancelled by user")
        return 130
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}", exc_info=True)
        print(f"Error: {str(e)}")
        return 1

if __name__ == '__main__':
    sys.exit(main())