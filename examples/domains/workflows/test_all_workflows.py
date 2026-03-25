#!/usr/bin/env python3
"""
Test All Workflow Examples

Description: Automated testing of all 10 workflow examples
Author: Marine Engineering Team
Date: 2025-10-03
"""

import sys
import subprocess
from pathlib import Path
import logging
from typing import List, Dict
import time

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class WorkflowTester:
    """Test runner for workflow examples."""

    def __init__(self, workflows_dir: Path):
        self.workflows_dir = workflows_dir
        self.results: List[Dict] = []

    def get_workflow_files(self) -> List[Path]:
        """Get all workflow Python files."""
        workflows = []
        for i in range(1, 11):
            pattern = f"{i:02d}_*.py"
            files = list(self.workflows_dir.glob(pattern))
            if files:
                workflows.extend(files)
        return sorted(workflows)

    def run_workflow(self, workflow_file: Path) -> Dict:
        """
        Run a single workflow and capture results.

        Parameters
        ----------
        workflow_file : Path
            Path to workflow file

        Returns
        -------
        result : Dict
            Test result
        """
        logger.info(f"Testing: {workflow_file.name}")

        start_time = time.time()

        try:
            # Run the workflow
            result = subprocess.run(
                [sys.executable, str(workflow_file)],
                capture_output=True,
                text=True,
                timeout=300  # 5 minute timeout
            )

            duration = time.time() - start_time

            success = result.returncode == 0

            return {
                'workflow': workflow_file.name,
                'success': success,
                'duration': duration,
                'returncode': result.returncode,
                'stdout': result.stdout if not success else '',
                'stderr': result.stderr if not success else '',
                'error': None
            }

        except subprocess.TimeoutExpired:
            duration = time.time() - start_time
            logger.error(f"Timeout: {workflow_file.name}")
            return {
                'workflow': workflow_file.name,
                'success': False,
                'duration': duration,
                'returncode': -1,
                'stdout': '',
                'stderr': '',
                'error': 'Timeout after 5 minutes'
            }

        except Exception as e:
            duration = time.time() - start_time
            logger.error(f"Error in {workflow_file.name}: {e}")
            return {
                'workflow': workflow_file.name,
                'success': False,
                'duration': duration,
                'returncode': -1,
                'stdout': '',
                'stderr': '',
                'error': str(e)
            }

    def run_all_tests(self) -> None:
        """Run all workflow tests."""
        logger.info("=" * 80)
        logger.info("WORKFLOW TESTING SUITE")
        logger.info("=" * 80)

        workflows = self.get_workflow_files()
        logger.info(f"Found {len(workflows)} workflow files to test")

        for workflow in workflows:
            result = self.run_workflow(workflow)
            self.results.append(result)

            if result['success']:
                logger.info(f"✓ PASS: {workflow.name} ({result['duration']:.1f}s)")
            else:
                logger.error(f"✗ FAIL: {workflow.name}")
                if result['error']:
                    logger.error(f"  Error: {result['error']}")
                if result['stderr']:
                    logger.error(f"  Stderr: {result['stderr'][:200]}")

        self.print_summary()

    def print_summary(self) -> None:
        """Print test summary."""
        logger.info("\n" + "=" * 80)
        logger.info("TEST SUMMARY")
        logger.info("=" * 80)

        total = len(self.results)
        passed = sum(1 for r in self.results if r['success'])
        failed = total - passed
        total_time = sum(r['duration'] for r in self.results)

        logger.info(f"Total Tests: {total}")
        logger.info(f"Passed: {passed}")
        logger.info(f"Failed: {failed}")
        logger.info(f"Success Rate: {passed/total*100:.1f}%")
        logger.info(f"Total Time: {total_time:.1f}s")

        if failed > 0:
            logger.info("\nFailed Tests:")
            for result in self.results:
                if not result['success']:
                    logger.info(f"  - {result['workflow']}")
                    if result['error']:
                        logger.info(f"    Error: {result['error']}")

        logger.info("=" * 80)

    def generate_report(self, output_file: Path) -> None:
        """Generate test report."""
        import json

        output_file.parent.mkdir(parents=True, exist_ok=True)

        report = {
            'test_date': time.strftime('%Y-%m-%d %H:%M:%S'),
            'total_tests': len(self.results),
            'passed': sum(1 for r in self.results if r['success']),
            'failed': sum(1 for r in self.results if not r['success']),
            'total_duration': sum(r['duration'] for r in self.results),
            'results': self.results
        }

        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)

        logger.info(f"\nTest report saved: {output_file}")


def main():
    """Main execution."""
    workflows_dir = Path(__file__).parent
    output_dir = workflows_dir / "outputs" / "test_results"

    tester = WorkflowTester(workflows_dir)
    tester.run_all_tests()

    # Generate report
    report_file = output_dir / f"test_report_{time.strftime('%Y%m%d_%H%M%S')}.json"
    tester.generate_report(report_file)

    # Exit with appropriate code
    failed_count = sum(1 for r in tester.results if not r['success'])
    sys.exit(0 if failed_count == 0 else 1)


if __name__ == "__main__":
    main()
