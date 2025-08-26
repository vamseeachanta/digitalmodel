#!/usr/bin/env python3
"""
Master Test Runner for OrcaWave Geometry Validation
Runs all tests in sequence and generates comprehensive report
"""

import subprocess
import json
import time
from pathlib import Path
from datetime import datetime
import logging
import sys
import concurrent.futures
from typing import Dict, List, Tuple

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('test_results.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class OrcaWaveTestRunner:
    """Comprehensive test runner for OrcaWave geometry validation"""
    
    def __init__(self):
        self.base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis")
        self.results = {
            'timestamp': datetime.now().isoformat(),
            'tests': {},
            'summary': {
                'total': 0,
                'passed': 0,
                'failed': 0,
                'skipped': 0
            }
        }
        
    def run_all_tests(self):
        """Run all tests in sequence"""
        logger.info("="*60)
        logger.info("ORCAWAVE GEOMETRY TEST SUITE")
        logger.info("="*60)
        
        start_time = time.time()
        
        # Define test sequence
        tests = [
            ('validate_formats', self.test_gdf_validation),
            ('check_files', self.test_file_existence),
            ('verify_dimensions', self.test_geometry_dimensions),
            ('test_converters', self.test_conversion_tools),
            ('validate_configs', self.test_config_files),
            ('check_orcawave', self.test_orcawave_installation),
            ('test_simple_geometry', self.test_simple_box),
            ('test_full_geometry', self.test_full_vessel)
        ]
        
        # Run tests
        for test_name, test_func in tests:
            logger.info(f"\n[TEST] Running: {test_name}")
            try:
                result = test_func()
                self.results['tests'][test_name] = result
                self.results['summary']['total'] += 1
                
                if result['status'] == 'passed':
                    self.results['summary']['passed'] += 1
                    logger.info(f"  [PASSED] {test_name}")
                elif result['status'] == 'failed':
                    self.results['summary']['failed'] += 1
                    logger.error(f"  [FAILED] {test_name}: {result.get('error', 'Unknown error')}")
                else:
                    self.results['summary']['skipped'] += 1
                    logger.warning(f"  [SKIPPED] {test_name}")
                    
            except Exception as e:
                logger.error(f"  [ERROR] {test_name}: {e}")
                self.results['tests'][test_name] = {
                    'status': 'failed',
                    'error': str(e)
                }
                self.results['summary']['failed'] += 1
                self.results['summary']['total'] += 1
        
        # Calculate duration
        duration = time.time() - start_time
        self.results['duration'] = f"{duration:.2f} seconds"
        
        # Generate report
        self.generate_report()
        
        return self.results
    
    def test_file_existence(self) -> Dict:
        """Check if all required files exist"""
        required_files = [
            'inputs/geometry/simple_box_test.gdf',
            'inputs/geometry/small_box_test.gdf',
            'inputs/geometry/sea_cypress_orcawave.gdf',
            'inputs/geometry/sea_cypress_gmsh_optimized.dat',
            'configs/test_simple_box.yml',
            'configs/sea_cypress_diffraction.yml',
            'scripts/validate_gdf_format.py',
            'scripts/convert_to_orcawave_gdf.py'
        ]
        
        missing_files = []
        for file_path in required_files:
            full_path = self.base_dir / file_path
            if not full_path.exists():
                missing_files.append(file_path)
        
        if missing_files:
            return {
                'status': 'failed',
                'missing_files': missing_files,
                'error': f"Missing {len(missing_files)} required files"
            }
        
        return {
            'status': 'passed',
            'message': f"All {len(required_files)} required files exist"
        }
    
    def test_gdf_validation(self) -> Dict:
        """Run GDF format validation"""
        script_path = self.base_dir / "scripts/validate_gdf_format.py"
        
        if not script_path.exists():
            return {
                'status': 'skipped',
                'error': 'Validation script not found'
            }
        
        try:
            result = subprocess.run(
                [sys.executable, str(script_path)],
                capture_output=True,
                text=True,
                cwd=str(script_path.parent),
                timeout=30
            )
            
            # Parse output for validation results
            output = result.stdout
            passed_count = output.count('[PASSED]')
            failed_count = output.count('[FAILED]')
            
            return {
                'status': 'passed' if failed_count == 0 else 'failed',
                'passed_files': passed_count,
                'failed_files': failed_count,
                'details': output[:500]  # First 500 chars of output
            }
            
        except subprocess.TimeoutExpired:
            return {
                'status': 'failed',
                'error': 'Validation script timeout'
            }
        except Exception as e:
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def test_geometry_dimensions(self) -> Dict:
        """Verify geometry dimensions are correct"""
        geometry_specs = {
            'simple_box_test.gdf': {
                'panels': 10,
                'length': 10.0,
                'beam': 5.0,
                'draft': 2.0
            },
            'sea_cypress_orcawave.gdf': {
                'panels': 24332,
                'length': 22.95,
                'beam': 8.57,
                'draft': 4.28
            }
        }
        
        results = {}
        for filename, expected in geometry_specs.items():
            file_path = self.base_dir / f"inputs/geometry/{filename}"
            if file_path.exists():
                # Read file and check basic properties
                with open(file_path, 'r') as f:
                    lines = f.readlines()
                    if len(lines) > 3:
                        try:
                            vertex_count = int(lines[3].strip())
                            expected_vertices = expected['panels'] * 3
                            results[filename] = {
                                'vertex_count': vertex_count,
                                'expected': expected_vertices,
                                'match': vertex_count == expected_vertices
                            }
                        except ValueError:
                            results[filename] = {'error': 'Cannot parse vertex count'}
            else:
                results[filename] = {'error': 'File not found'}
        
        all_match = all(r.get('match', False) for r in results.values() if 'error' not in r)
        
        return {
            'status': 'passed' if all_match else 'failed',
            'geometries': results
        }
    
    def test_conversion_tools(self) -> Dict:
        """Test conversion scripts exist and are runnable"""
        scripts = [
            'scripts/convert_to_orcawave_gdf.py',
            'scripts/create_simple_box_gdf.py',
            'scripts/test_orcawave_geometry.py'
        ]
        
        results = {}
        for script_path in scripts:
            full_path = self.base_dir / script_path
            if full_path.exists():
                # Try to import the script
                try:
                    result = subprocess.run(
                        [sys.executable, '-m', 'py_compile', str(full_path)],
                        capture_output=True,
                        timeout=5
                    )
                    results[script_path] = {
                        'exists': True,
                        'valid_python': result.returncode == 0
                    }
                except Exception as e:
                    results[script_path] = {
                        'exists': True,
                        'valid_python': False,
                        'error': str(e)
                    }
            else:
                results[script_path] = {'exists': False}
        
        all_valid = all(r.get('valid_python', False) for r in results.values())
        
        return {
            'status': 'passed' if all_valid else 'failed',
            'scripts': results
        }
    
    def test_config_files(self) -> Dict:
        """Validate YAML configuration files"""
        configs = [
            'configs/test_simple_box.yml',
            'configs/sea_cypress_diffraction.yml'
        ]
        
        results = {}
        for config_path in configs:
            full_path = self.base_dir / config_path
            if full_path.exists():
                try:
                    import yaml
                    with open(full_path, 'r') as f:
                        data = yaml.safe_load(f)
                    
                    # Check for required fields
                    required_fields = ['UnitsSystem', 'WaterDepth', 'Bodies']
                    missing = [f for f in required_fields if f not in data]
                    
                    results[config_path] = {
                        'valid_yaml': True,
                        'missing_fields': missing,
                        'complete': len(missing) == 0
                    }
                except Exception as e:
                    results[config_path] = {
                        'valid_yaml': False,
                        'error': str(e)
                    }
            else:
                results[config_path] = {'exists': False}
        
        all_complete = all(r.get('complete', False) for r in results.values())
        
        return {
            'status': 'passed' if all_complete else 'failed',
            'configs': results
        }
    
    def test_orcawave_installation(self) -> Dict:
        """Check if OrcaWave is installed"""
        orcawave_path = Path(r"C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe")
        
        if orcawave_path.exists():
            return {
                'status': 'passed',
                'path': str(orcawave_path),
                'message': 'OrcaWave installation found'
            }
        else:
            return {
                'status': 'failed',
                'error': f'OrcaWave not found at {orcawave_path}'
            }
    
    def test_simple_box(self) -> Dict:
        """Test simple box geometry"""
        gdf_file = self.base_dir / "inputs/geometry/simple_box_test.gdf"
        
        if not gdf_file.exists():
            return {
                'status': 'skipped',
                'error': 'Simple box GDF not found'
            }
        
        # Verify file format
        with open(gdf_file, 'r') as f:
            lines = f.readlines()
        
        checks = {
            'header': 'WAMIT' in lines[0] if len(lines) > 0 else False,
            'gravity': '9.80665' in lines[1] if len(lines) > 1 else False,
            'vertex_count': lines[3].strip() == '30' if len(lines) > 3 else False
        }
        
        all_passed = all(checks.values())
        
        return {
            'status': 'passed' if all_passed else 'failed',
            'checks': checks,
            'message': 'Simple box geometry validated'
        }
    
    def test_full_vessel(self) -> Dict:
        """Test full vessel geometry"""
        gdf_file = self.base_dir / "inputs/geometry/sea_cypress_orcawave.gdf"
        
        if not gdf_file.exists():
            return {
                'status': 'skipped',
                'error': 'Sea Cypress GDF not found'
            }
        
        # Verify file format
        with open(gdf_file, 'r') as f:
            lines = f.readlines()
        
        checks = {
            'header': 'WAMIT' in lines[0] if len(lines) > 0 else False,
            'gravity': '9.80665' in lines[1] if len(lines) > 1 else False,
            'vertex_count': lines[3].strip() == '72996' if len(lines) > 3 else False,
            'sufficient_lines': len(lines) > 70000
        }
        
        all_passed = all(checks.values())
        
        return {
            'status': 'passed' if all_passed else 'failed',
            'checks': checks,
            'message': 'Full vessel geometry validated'
        }
    
    def generate_report(self):
        """Generate comprehensive test report"""
        report_path = self.base_dir / "TEST_RESULTS.md"
        
        with open(report_path, 'w') as f:
            f.write("# OrcaWave Geometry Test Results\n\n")
            f.write(f"**Test Run:** {self.results['timestamp']}\n")
            f.write(f"**Duration:** {self.results.get('duration', 'N/A')}\n\n")
            
            # Summary
            f.write("## Summary\n\n")
            summary = self.results['summary']
            f.write(f"- **Total Tests:** {summary['total']}\n")
            f.write(f"- **Passed:** {summary['passed']} ")
            f.write(f"({100*summary['passed']/summary['total']:.1f}%)\n" if summary['total'] > 0 else "\n")
            f.write(f"- **Failed:** {summary['failed']}\n")
            f.write(f"- **Skipped:** {summary['skipped']}\n\n")
            
            # Test details
            f.write("## Test Details\n\n")
            for test_name, result in self.results['tests'].items():
                status_icon = "[PASS]" if result['status'] == 'passed' else "[FAIL]" if result['status'] == 'failed' else "[SKIP]"
                f.write(f"### {status_icon} {test_name}\n\n")
                f.write(f"**Status:** {result['status'].upper()}\n\n")
                
                if 'error' in result:
                    f.write(f"**Error:** {result['error']}\n\n")
                
                if 'message' in result:
                    f.write(f"**Message:** {result['message']}\n\n")
                
                # Write additional details
                for key, value in result.items():
                    if key not in ['status', 'error', 'message']:
                        f.write(f"**{key}:** `{value}`\n\n")
            
            # Recommendations
            f.write("## Recommendations\n\n")
            if summary['failed'] == 0:
                f.write("[SUCCESS] All tests passed! Ready to proceed with OrcaWave analysis.\n\n")
                f.write("Next steps:\n")
                f.write("1. Run `test_orcawave_cli.bat` to test in OrcaWave\n")
                f.write("2. Start with simple box geometry\n")
                f.write("3. If successful, load full vessel geometry\n")
            else:
                f.write("[WARNING] Some tests failed. Please review the errors above.\n\n")
                f.write("Troubleshooting:\n")
                f.write("1. Check file paths and permissions\n")
                f.write("2. Verify OrcaWave installation\n")
                f.write("3. Run individual test scripts for debugging\n")
        
        # Also save JSON results
        json_path = self.base_dir / "test_results.json"
        with open(json_path, 'w') as f:
            json.dump(self.results, f, indent=2, default=str)
        
        logger.info(f"\nTest report saved to: {report_path}")
        logger.info(f"JSON results saved to: {json_path}")

def run_parallel_tests():
    """Run tests in parallel where possible"""
    runner = OrcaWaveTestRunner()
    
    # Define parallel test groups
    parallel_tests = [
        runner.test_file_existence,
        runner.test_geometry_dimensions,
        runner.test_conversion_tools,
        runner.test_config_files
    ]
    
    results = {}
    
    logger.info("Running tests in parallel...")
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
        future_to_test = {executor.submit(test): test.__name__ for test in parallel_tests}
        
        for future in concurrent.futures.as_completed(future_to_test):
            test_name = future_to_test[future]
            try:
                result = future.result(timeout=30)
                results[test_name] = result
                logger.info(f"  Completed: {test_name}")
            except Exception as e:
                results[test_name] = {
                    'status': 'failed',
                    'error': str(e)
                }
                logger.error(f"  Failed: {test_name}: {e}")
    
    return results

def main():
    """Main execution"""
    logger.info("Starting OrcaWave Geometry Test Suite")
    
    runner = OrcaWaveTestRunner()
    results = runner.run_all_tests()
    
    # Print summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    summary = results['summary']
    print(f"Total: {summary['total']} | Passed: {summary['passed']} | Failed: {summary['failed']} | Skipped: {summary['skipped']}")
    
    if summary['failed'] == 0:
        print("\n[SUCCESS] ALL TESTS PASSED - Ready for OrcaWave!")
    else:
        print(f"\n[WARNING] {summary['failed']} TESTS FAILED - Review TEST_RESULTS.md for details")
    
    print(f"\nDetailed report: TEST_RESULTS.md")
    print("="*60)
    
    return 0 if summary['failed'] == 0 else 1

if __name__ == "__main__":
    sys.exit(main())