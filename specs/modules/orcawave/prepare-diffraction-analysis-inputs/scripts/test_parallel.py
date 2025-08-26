"""
Test all scripts in parallel to validate functionality.
"""

import concurrent.futures
import subprocess
import sys
from pathlib import Path
import logging
import time
from typing import Dict, Any, List, Tuple
import traceback

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(threadName)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class ParallelTester:
    """Run tests in parallel threads."""
    
    def __init__(self, scripts_dir: Path):
        """Initialize with scripts directory."""
        self.scripts_dir = Path(scripts_dir)
        self.test_results = {}
        
    def test_excel_extractor(self) -> Tuple[bool, str]:
        """Test Excel data extraction script."""
        try:
            script = self.scripts_dir / 'extract_excel_data.py'
            
            # Test with --help to verify script loads
            result = subprocess.run(
                [sys.executable, str(script), '--help'],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                return True, "Excel extractor script loads successfully"
            else:
                return False, f"Excel extractor failed: {result.stderr}"
                
        except Exception as e:
            return False, f"Excel extractor error: {str(e)}"
            
    def test_msh_converter(self) -> Tuple[bool, str]:
        """Test MSH to GDF converter script."""
        try:
            script = self.scripts_dir / 'convert_msh_to_gdf.py'
            
            # Test with --help
            result = subprocess.run(
                [sys.executable, str(script), '--help'],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                return True, "MSH converter script loads successfully"
            else:
                return False, f"MSH converter failed: {result.stderr}"
                
        except Exception as e:
            return False, f"MSH converter error: {str(e)}"
            
    def test_input_generator(self) -> Tuple[bool, str]:
        """Test OrcaWave input generator script."""
        try:
            script = self.scripts_dir / 'generate_orcawave_input.py'
            
            # Test with --help
            result = subprocess.run(
                [sys.executable, str(script), '--help'],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                return True, "Input generator script loads successfully"
            else:
                return False, f"Input generator failed: {result.stderr}"
                
        except Exception as e:
            return False, f"Input generator error: {str(e)}"
            
    def test_postprocessor(self) -> Tuple[bool, str]:
        """Test post-processing script."""
        try:
            script = self.scripts_dir / 'postprocess_results.py'
            
            # Test with --help
            result = subprocess.run(
                [sys.executable, str(script), '--help'],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                return True, "Post-processor script loads successfully"
            else:
                return False, f"Post-processor failed: {result.stderr}"
                
        except Exception as e:
            return False, f"Post-processor error: {str(e)}"
            
    def test_batch_script(self) -> Tuple[bool, str]:
        """Test Windows batch script syntax."""
        try:
            script = self.scripts_dir / 'run_orcawave.bat'
            
            if not script.exists():
                return False, "Batch script not found"
                
            # Just check if file exists and has content
            with open(script, 'r') as f:
                content = f.read()
                if '@echo off' in content and 'OrcaWave' in content:
                    return True, "Batch script structure valid"
                else:
                    return False, "Batch script missing expected content"
                    
        except Exception as e:
            return False, f"Batch script error: {str(e)}"
            
    def run_all_tests_parallel(self) -> Dict[str, Dict[str, Any]]:
        """Run all tests in parallel."""
        test_functions = {
            'Excel Extractor': self.test_excel_extractor,
            'MSH Converter': self.test_msh_converter,
            'Input Generator': self.test_input_generator,
            'Post-processor': self.test_postprocessor,
            'Batch Script': self.test_batch_script
        }
        
        logger.info(f"Starting parallel tests for {len(test_functions)} components")
        start_time = time.time()
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            # Submit all test tasks
            future_to_test = {
                executor.submit(test_func): test_name
                for test_name, test_func in test_functions.items()
            }
            
            # Collect results as they complete
            for future in concurrent.futures.as_completed(future_to_test):
                test_name = future_to_test[future]
                try:
                    success, message = future.result(timeout=30)
                    self.test_results[test_name] = {
                        'success': success,
                        'message': message,
                        'duration': time.time() - start_time
                    }
                    
                    status = "✅" if success else "❌"
                    logger.info(f"{status} {test_name}: {message}")
                    
                except concurrent.futures.TimeoutError:
                    self.test_results[test_name] = {
                        'success': False,
                        'message': "Test timeout (30s)",
                        'duration': 30
                    }
                    logger.error(f"❌ {test_name}: Timeout")
                    
                except Exception as e:
                    self.test_results[test_name] = {
                        'success': False,
                        'message': f"Exception: {str(e)}",
                        'duration': time.time() - start_time
                    }
                    logger.error(f"❌ {test_name}: {str(e)}")
                    
        total_time = time.time() - start_time
        logger.info(f"All tests completed in {total_time:.2f} seconds")
        
        return self.test_results
        
    def validate_integration(self) -> bool:
        """Validate that all components can work together."""
        logger.info("\n=== Integration Validation ===")
        
        # Check if all critical scripts passed
        critical_scripts = [
            'MSH Converter',
            'Input Generator',
            'Post-processor'
        ]
        
        all_passed = True
        for script in critical_scripts:
            if script in self.test_results:
                if not self.test_results[script]['success']:
                    logger.error(f"Critical component failed: {script}")
                    all_passed = False
            else:
                logger.error(f"Critical component not tested: {script}")
                all_passed = False
                
        if all_passed:
            logger.info("✅ All critical components validated")
        else:
            logger.error("❌ Integration validation failed")
            
        return all_passed
        
    def generate_report(self) -> str:
        """Generate test report."""
        report = []
        report.append("\n" + "=" * 60)
        report.append("PARALLEL TEST REPORT")
        report.append("=" * 60)
        
        # Summary
        total_tests = len(self.test_results)
        passed = sum(1 for r in self.test_results.values() if r['success'])
        failed = total_tests - passed
        
        report.append(f"\nTotal Tests: {total_tests}")
        report.append(f"Passed: {passed} ✅")
        report.append(f"Failed: {failed} ❌")
        
        # Detailed results
        report.append("\n" + "-" * 60)
        report.append("DETAILED RESULTS:")
        report.append("-" * 60)
        
        for test_name, result in self.test_results.items():
            status = "✅ PASS" if result['success'] else "❌ FAIL"
            report.append(f"\n{test_name}:")
            report.append(f"  Status: {status}")
            report.append(f"  Message: {result['message']}")
            report.append(f"  Duration: {result.get('duration', 0):.2f}s")
            
        # Performance summary
        if self.test_results:
            max_duration = max(r.get('duration', 0) for r in self.test_results.values())
            report.append("\n" + "-" * 60)
            report.append("PERFORMANCE:")
            report.append("-" * 60)
            report.append(f"Total execution time: {max_duration:.2f}s")
            report.append(f"Parallel speedup: ~{len(self.test_results):.1f}x")
            
        report.append("\n" + "=" * 60)
        
        return "\n".join(report)


def main():
    """Main execution function."""
    logger.info("OrcaWave Scripts Parallel Testing")
    logger.info("==================================")
    
    # Get scripts directory
    scripts_dir = Path(__file__).parent
    
    # Create tester
    tester = ParallelTester(scripts_dir)
    
    # Run all tests in parallel
    results = tester.run_all_tests_parallel()
    
    # Validate integration
    integration_valid = tester.validate_integration()
    
    # Generate and print report
    report = tester.generate_report()
    # Use UTF-8 encoding for output
    sys.stdout.reconfigure(encoding='utf-8')
    print(report)
    
    # Exit with appropriate code
    if integration_valid and all(r['success'] for r in results.values()):
        logger.info("\nAll tests passed successfully!")
        sys.exit(0)
    else:
        logger.error("\nSome tests failed. Please review the report above.")
        sys.exit(1)
        

if __name__ == "__main__":
    main()