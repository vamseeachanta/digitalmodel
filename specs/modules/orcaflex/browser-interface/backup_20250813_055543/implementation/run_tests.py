"""
Test Runner for OrcaFlex Browser Interface
Executes Task 1.7 validation tests with comprehensive reporting
"""

import sys
import os
from pathlib import Path
import unittest
import json
from datetime import datetime

# Add directories to path
test_dir = Path(__file__).parent.parent.parent.parent.parent.parent / 'tests' / 'modules' / 'orcaflex' / 'browser-interface'
src_dir = Path(__file__).parent / 'src' / 'backend'

sys.path.insert(0, str(test_dir))
sys.path.insert(0, str(src_dir))

# Import the test module
import test_browser_integration


class TestResultsFormatter:
    """Format test results for clear reporting"""
    
    @staticmethod
    def format_results(result):
        """Format test results into a comprehensive report"""
        report = []
        report.append("\n" + "="*70)
        report.append("ORCAFLEX BROWSER INTERFACE - TASK 1.7 TEST RESULTS")
        report.append("="*70)
        report.append(f"Execution Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("")
        
        # Summary statistics
        total = result.testsRun
        failures = len(result.failures)
        errors = len(result.errors)
        success = total - failures - errors
        
        report.append("TEST SUMMARY:")
        report.append(f"  Total Tests: {total}")
        report.append(f"  [PASS] Passed: {success}")
        report.append(f"  [FAIL] Failed: {failures}")
        report.append(f"  [ERROR] Errors: {errors}")
        report.append(f"  Success Rate: {(success/total*100):.1f}%")
        report.append("")
        
        # Task breakdown
        report.append("TASK BREAKDOWN:")
        report.append("  [DONE] Task 1.7.1: Test suite development - COMPLETED")
        report.append("  [TEST] Task 1.7.2: Folder selection - TESTING IN PROGRESS")
        report.append("  [TODO] Task 1.7.3: Auto-max mode implementation - PENDING TEST")
        report.append("  [TODO] Task 1.7.4: Auto-max verification - PENDING TEST")
        report.append("  [TODO] Task 1.7.5: Manual mode switch - PENDING TEST")
        report.append("  [TODO] Task 1.7.6: Manual mode verification - PENDING TEST")
        report.append("  [TODO] Task 1.7.7: Additional tests - PENDING TEST")
        report.append("")
        
        # Detailed results
        if failures:
            report.append("FAILED TESTS:")
            for test, traceback in result.failures:
                report.append(f"  [FAIL] {test}")
                report.append(f"     {traceback[:200]}...")
            report.append("")
        
        if errors:
            report.append("TEST ERRORS:")
            for test, traceback in result.errors:
                report.append(f"  [ERROR] {test}")
                report.append(f"     {traceback[:200]}...")
            report.append("")
        
        # Overall status
        report.append("="*70)
        if result.wasSuccessful():
            report.append("[SUCCESS] ALL TESTS PASSED - Browser functionality validated!")
            report.append("   Ready for user acceptance testing")
        else:
            report.append("[FAILURE] TESTS INCOMPLETE - Review failures above")
            report.append("   Fix issues before proceeding to user testing")
        report.append("="*70)
        
        return "\n".join(report)


def run_specific_task_tests(task_number):
    """Run tests for a specific task"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Map task numbers to test classes
    task_mapping = {
        '1.7.2': test_browser_integration.TestTask1_7_2_FolderSelection,
        '1.7.3': test_browser_integration.TestTask1_7_3_AutoMaxMode,
        '1.7.4': test_browser_integration.TestTask1_7_4_VerifyAutoMaxMode,
        '1.7.5': test_browser_integration.TestTask1_7_5_ManualModeSwitch,
        '1.7.6': test_browser_integration.TestTask1_7_6_VerifyManualMode,
        '1.7.7': test_browser_integration.TestTask1_7_7_AdditionalTests,
    }
    
    if task_number in task_mapping:
        suite.addTests(loader.loadTestsFromTestCase(task_mapping[task_number]))
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        return result
    else:
        print(f"Task {task_number} not found")
        return None


def run_all_tests():
    """Run all validation tests"""
    print("\n" + "="*70)
    print("STARTING ORCAFLEX BROWSER INTERFACE VALIDATION")
    print("Task 1.7: Manual Testing and Validation")
    print("="*70)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add all test classes
    test_classes = [
        test_browser_integration.TestTask1_7_2_FolderSelection,
        test_browser_integration.TestTask1_7_3_AutoMaxMode,
        test_browser_integration.TestTask1_7_4_VerifyAutoMaxMode,
        test_browser_integration.TestTask1_7_5_ManualModeSwitch,
        test_browser_integration.TestTask1_7_6_VerifyManualMode,
        test_browser_integration.TestTask1_7_7_AdditionalTests,
        test_browser_integration.TestIntegrationScenarios,
    ]
    
    for test_class in test_classes:
        suite.addTests(loader.loadTestsFromTestCase(test_class))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Format and print results
    formatter = TestResultsFormatter()
    print(formatter.format_results(result))
    
    # Save results to file
    results_file = Path(__file__).parent / 'test_results.json'
    results_data = {
        'timestamp': datetime.now().isoformat(),
        'total_tests': result.testsRun,
        'failures': len(result.failures),
        'errors': len(result.errors),
        'success_rate': ((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100),
        'task_status': {
            '1.7.1': 'completed',
            '1.7.2': 'testing',
            '1.7.3': 'testing',
            '1.7.4': 'testing',
            '1.7.5': 'testing',
            '1.7.6': 'testing',
            '1.7.7': 'testing'
        }
    }
    
    with open(results_file, 'w') as f:
        json.dump(results_data, f, indent=2)
    
    print(f"\nResults saved to: {results_file}")
    
    return result.wasSuccessful()


def main():
    """Main entry point"""
    if len(sys.argv) > 1:
        # Run specific task test
        task = sys.argv[1]
        print(f"\nRunning tests for Task {task}...")
        result = run_specific_task_tests(task)
        if result:
            print(f"\nTask {task} Results: {'PASSED' if result.wasSuccessful() else 'FAILED'}")
    else:
        # Run all tests
        success = run_all_tests()
        sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()