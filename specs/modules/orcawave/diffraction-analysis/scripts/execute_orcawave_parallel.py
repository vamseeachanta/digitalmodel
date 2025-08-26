#!/usr/bin/env python
"""
Execute OrcaWave analysis with parallel validation
Tests the configuration in parallel before proposing to user
"""

import os
import sys
import yaml
import json
import argparse
import logging
import subprocess
import threading
import time
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, List, Optional, Tuple
from concurrent.futures import ThreadPoolExecutor, as_completed

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent.parent))

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class OrcaWaveExecutor:
    """Execute OrcaWave analysis with parallel validation"""
    
    def __init__(self, config_file: str, orcawave_exe: str = None):
        """
        Initialize OrcaWave executor
        
        Args:
            config_file: Path to OrcaWave YAML configuration
            orcawave_exe: Path to OrcaWave executable (auto-detect if None)
        """
        self.config_file = Path(config_file)
        self.orcawave_exe = orcawave_exe or self._find_orcawave_exe()
        self.validation_results = {}
        self.execution_log = []
        
        # Output directories
        self.base_dir = self.config_file.parent.parent
        self.output_dir = self.base_dir / 'outputs'
        self.log_dir = self.base_dir / 'logs'
        self.validation_dir = self.base_dir / 'validation'
        
        # Create directories
        for dir_path in [self.output_dir, self.log_dir, self.validation_dir]:
            dir_path.mkdir(parents=True, exist_ok=True)
            
    def _find_orcawave_exe(self) -> Optional[str]:
        """
        Auto-detect OrcaWave executable
        
        Returns:
            Path to OrcaWave executable or None
        """
        possible_paths = [
            r"C:\Program Files\Orcina\OrcaWave\OrcaWave.exe",
            r"C:\Program Files (x86)\Orcina\OrcaWave\OrcaWave.exe",
            r"D:\OrcaWave\OrcaWave.exe",
        ]
        
        for path in possible_paths:
            if Path(path).exists():
                logger.info(f"Found OrcaWave at: {path}")
                return path
                
        logger.warning("OrcaWave executable not found. Will run in validation mode only.")
        return None
        
    def validate_config(self) -> Dict[str, Any]:
        """
        Validate OrcaWave configuration file
        
        Returns:
            Validation results dictionary
        """
        logger.info(f"Validating configuration: {self.config_file}")
        
        validation = {
            'status': 'valid',
            'errors': [],
            'warnings': [],
            'info': []
        }
        
        # Check file exists
        if not self.config_file.exists():
            validation['status'] = 'invalid'
            validation['errors'].append(f"Configuration file not found: {self.config_file}")
            return validation
            
        # Load and parse YAML
        try:
            with open(self.config_file, 'r') as f:
                config = yaml.safe_load(f)
        except Exception as e:
            validation['status'] = 'invalid'
            validation['errors'].append(f"Failed to parse YAML: {e}")
            return validation
            
        # Required fields
        required_fields = [
            'UnitsSystem', 'SolveType', 'WaterDepth', 
            'WaterDensity', 'PeriodOrFrequency', 'WaveHeading', 'Bodies'
        ]
        
        for field in required_fields:
            if field not in config:
                validation['status'] = 'invalid'
                validation['errors'].append(f"Missing required field: {field}")
                
        # Validate bodies
        if 'Bodies' in config:
            for i, body in enumerate(config.get('Bodies', [])):
                if 'BodyMeshFileName' not in body:
                    validation['errors'].append(f"Body {i}: Missing mesh file")
                else:
                    mesh_file = self.config_file.parent / body['BodyMeshFileName']
                    if not mesh_file.exists():
                        validation['warnings'].append(f"Body {i}: Mesh file not found: {body['BodyMeshFileName']}")
                        
                if 'BodyMass' not in body:
                    validation['warnings'].append(f"Body {i}: Missing mass definition")
                    
        # Check wave parameters
        if 'PeriodOrFrequency' in config:
            periods = config['PeriodOrFrequency']
            if len(periods) < 3:
                validation['warnings'].append("Very few wave periods defined")
            validation['info'].append(f"Wave periods: {len(periods)} values from {min(periods)} to {max(periods)}")
            
        if 'WaveHeading' in config:
            headings = config['WaveHeading']
            validation['info'].append(f"Wave headings: {len(headings)} values from {min(headings)} to {max(headings)}")
            
        # Log validation results
        if validation['errors']:
            logger.error(f"Validation errors: {validation['errors']}")
        if validation['warnings']:
            logger.warning(f"Validation warnings: {validation['warnings']}")
            
        return validation
        
    def parallel_validation(self) -> Dict[str, Any]:
        """
        Run parallel validation tests
        
        Returns:
            Validation test results
        """
        logger.info("Running parallel validation tests...")
        
        test_results = {
            'timestamp': datetime.now().isoformat(),
            'config_file': str(self.config_file),
            'tests': {}
        }
        
        # Define validation tests
        validation_tests = [
            ('config_validation', self.validate_config),
            ('mesh_validation', self._validate_mesh_files),
            ('numerical_stability', self._check_numerical_stability),
            ('memory_estimate', self._estimate_memory_usage)
        ]
        
        # Run tests in parallel
        with ThreadPoolExecutor(max_workers=4) as executor:
            future_to_test = {
                executor.submit(test_func): test_name 
                for test_name, test_func in validation_tests
            }
            
            for future in as_completed(future_to_test):
                test_name = future_to_test[future]
                try:
                    result = future.result(timeout=30)
                    test_results['tests'][test_name] = result
                    logger.info(f"Test '{test_name}' completed: {result.get('status', 'unknown')}")
                except Exception as e:
                    test_results['tests'][test_name] = {
                        'status': 'error',
                        'error': str(e)
                    }
                    logger.error(f"Test '{test_name}' failed: {e}")
                    
        # Overall validation status
        all_valid = all(
            test.get('status') in ['valid', 'warning', 'info'] 
            for test in test_results['tests'].values()
        )
        test_results['overall_status'] = 'ready' if all_valid else 'needs_attention'
        
        # Save validation results
        validation_file = self.validation_dir / f"validation_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(validation_file, 'w') as f:
            json.dump(test_results, f, indent=2)
            
        logger.info(f"Validation results saved to: {validation_file}")
        return test_results
        
    def _validate_mesh_files(self) -> Dict[str, Any]:
        """Validate mesh files referenced in configuration"""
        result = {'status': 'valid', 'details': []}
        
        try:
            with open(self.config_file, 'r') as f:
                config = yaml.safe_load(f)
                
            for body in config.get('Bodies', []):
                if 'BodyMeshFileName' in body:
                    mesh_path = self.config_file.parent / body['BodyMeshFileName']
                    if mesh_path.exists():
                        size_mb = mesh_path.stat().st_size / (1024 * 1024)
                        result['details'].append(f"Mesh found: {body['BodyMeshFileName']} ({size_mb:.2f} MB)")
                    else:
                        result['status'] = 'warning'
                        result['details'].append(f"Mesh missing: {body['BodyMeshFileName']}")
                        
        except Exception as e:
            result['status'] = 'error'
            result['error'] = str(e)
            
        return result
        
    def _check_numerical_stability(self) -> Dict[str, Any]:
        """Check numerical stability parameters"""
        result = {'status': 'valid', 'parameters': {}}
        
        try:
            with open(self.config_file, 'r') as f:
                config = yaml.safe_load(f)
                
            # Check tolerances
            result['parameters']['length_tolerance'] = config.get('LengthTolerance', 'not set')
            result['parameters']['waterline_tolerance'] = config.get('WaterlineZTolerance', 'not set')
            
            # Check solver
            result['parameters']['solver'] = config.get('LinearSolverMethod', 'not set')
            
            # Check panel warnings
            aspect_ratio = config.get('PanelAspectRatioWarningLevel', 25)
            if aspect_ratio > 50:
                result['status'] = 'warning'
                result['parameters']['high_aspect_ratio'] = aspect_ratio
                
        except Exception as e:
            result['status'] = 'error'
            result['error'] = str(e)
            
        return result
        
    def _estimate_memory_usage(self) -> Dict[str, Any]:
        """Estimate memory usage for analysis"""
        result = {'status': 'info', 'estimate': {}}
        
        try:
            with open(self.config_file, 'r') as f:
                config = yaml.safe_load(f)
                
            # Estimate based on problem size
            n_periods = len(config.get('PeriodOrFrequency', []))
            n_headings = len(config.get('WaveHeading', []))
            n_bodies = len(config.get('Bodies', []))
            
            # Rough estimation (MB)
            base_memory = 500  # Base OrcaWave memory
            per_frequency = 50  # Memory per frequency
            per_heading = 30  # Memory per heading
            
            estimated_mb = base_memory + (n_periods * per_frequency) + (n_headings * per_heading)
            
            result['estimate'] = {
                'periods': n_periods,
                'headings': n_headings,
                'bodies': n_bodies,
                'memory_mb': estimated_mb,
                'computation_time_min': n_periods * n_headings * 0.5  # Rough estimate
            }
            
            if estimated_mb > 4000:
                result['status'] = 'warning'
                result['message'] = 'High memory usage expected'
                
        except Exception as e:
            result['status'] = 'error'
            result['error'] = str(e)
            
        return result
        
    def execute_analysis(self, dry_run: bool = False) -> Dict[str, Any]:
        """
        Execute OrcaWave analysis
        
        Args:
            dry_run: If True, validate only without execution
            
        Returns:
            Execution results
        """
        # Run validation first
        validation = self.parallel_validation()
        
        if validation['overall_status'] != 'ready':
            logger.warning("Validation found issues. Review before proceeding.")
            if not dry_run:
                response = input("Continue with execution anyway? (y/n): ")
                if response.lower() != 'y':
                    return validation
                    
        if dry_run:
            logger.info("Dry run complete. Validation results available.")
            return validation
            
        # Execute OrcaWave
        if not self.orcawave_exe:
            logger.error("OrcaWave executable not found. Cannot execute analysis.")
            validation['execution'] = {'status': 'skipped', 'reason': 'OrcaWave not found'}
            return validation
            
        logger.info("Executing OrcaWave analysis...")
        
        # Create batch file for execution
        batch_file = self._create_batch_file()
        
        # Execute
        try:
            start_time = time.time()
            result = subprocess.run(
                [str(batch_file)],
                capture_output=True,
                text=True,
                timeout=3600,  # 1 hour timeout
                cwd=str(self.config_file.parent)
            )
            
            execution_time = time.time() - start_time
            
            validation['execution'] = {
                'status': 'completed' if result.returncode == 0 else 'failed',
                'return_code': result.returncode,
                'execution_time_sec': execution_time,
                'stdout': result.stdout[-5000:] if result.stdout else None,  # Last 5000 chars
                'stderr': result.stderr[-5000:] if result.stderr else None
            }
            
            # Save execution log
            log_file = self.log_dir / f"execution_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
            with open(log_file, 'w') as f:
                f.write(f"Execution started: {datetime.now().isoformat()}\n")
                f.write(f"Configuration: {self.config_file}\n")
                f.write(f"Execution time: {execution_time:.2f} seconds\n\n")
                f.write("=== STDOUT ===\n")
                f.write(result.stdout or "No output")
                f.write("\n\n=== STDERR ===\n")
                f.write(result.stderr or "No errors")
                
            logger.info(f"Execution log saved to: {log_file}")
            
        except subprocess.TimeoutExpired:
            validation['execution'] = {
                'status': 'timeout',
                'message': 'Analysis exceeded time limit'
            }
        except Exception as e:
            validation['execution'] = {
                'status': 'error',
                'error': str(e)
            }
            
        return validation
        
    def _create_batch_file(self) -> Path:
        """Create batch file for OrcaWave execution"""
        batch_content = f"""@echo off
echo Starting OrcaWave analysis...
echo Configuration: {self.config_file}
echo.

"{self.orcawave_exe}" "{self.config_file}"

echo.
echo Analysis complete!
pause
"""
        
        batch_file = self.config_file.parent / f"run_orcawave_{datetime.now().strftime('%Y%m%d_%H%M%S')}.bat"
        with open(batch_file, 'w') as f:
            f.write(batch_content)
            
        return batch_file
        
    def generate_report(self, results: Dict[str, Any]) -> str:
        """
        Generate execution report
        
        Args:
            results: Execution results dictionary
            
        Returns:
            Path to report file
        """
        report_file = self.validation_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
        
        with open(report_file, 'w') as f:
            f.write("# OrcaWave Execution Report\n\n")
            f.write(f"**Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"**Configuration:** {self.config_file}\n\n")
            
            f.write("## Validation Results\n\n")
            f.write(f"**Overall Status:** {results.get('overall_status', 'unknown')}\n\n")
            
            for test_name, test_result in results.get('tests', {}).items():
                f.write(f"### {test_name.replace('_', ' ').title()}\n")
                f.write(f"- Status: {test_result.get('status', 'unknown')}\n")
                
                if 'details' in test_result:
                    f.write("- Details:\n")
                    for detail in test_result['details']:
                        f.write(f"  - {detail}\n")
                        
                if 'parameters' in test_result:
                    f.write("- Parameters:\n")
                    for key, value in test_result['parameters'].items():
                        f.write(f"  - {key}: {value}\n")
                        
                if 'estimate' in test_result:
                    f.write("- Estimates:\n")
                    for key, value in test_result['estimate'].items():
                        f.write(f"  - {key}: {value}\n")
                        
                f.write("\n")
                
            if 'execution' in results:
                f.write("## Execution Results\n\n")
                exec_result = results['execution']
                f.write(f"- Status: {exec_result.get('status', 'unknown')}\n")
                f.write(f"- Return Code: {exec_result.get('return_code', 'N/A')}\n")
                f.write(f"- Execution Time: {exec_result.get('execution_time_sec', 0):.2f} seconds\n")
                
        logger.info(f"Report generated: {report_file}")
        return str(report_file)


def main():
    """Main execution function"""
    parser = argparse.ArgumentParser(
        description='Execute OrcaWave analysis with parallel validation'
    )
    parser.add_argument(
        '--config',
        required=True,
        help='Path to OrcaWave YAML configuration file'
    )
    parser.add_argument(
        '--orcawave-exe',
        help='Path to OrcaWave executable (auto-detect if not provided)'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Run validation only without execution'
    )
    parser.add_argument(
        '--parallel-test',
        action='store_true',
        default=True,
        help='Run validation tests in parallel (default: True)'
    )
    
    args = parser.parse_args()
    
    # Create executor
    executor = OrcaWaveExecutor(args.config, args.orcawave_exe)
    
    # Execute analysis
    results = executor.execute_analysis(dry_run=args.dry_run)
    
    # Generate report
    report = executor.generate_report(results)
    
    # Print summary
    print("\n" + "="*60)
    print("ORCAWAVE EXECUTION SUMMARY")
    print("="*60)
    print(f"Overall Status: {results.get('overall_status', 'unknown')}")
    print(f"Report: {report}")
    
    if 'execution' in results:
        print(f"Execution Status: {results['execution'].get('status', 'unknown')}")
        
    print("\nNext Steps:")
    if results.get('overall_status') == 'ready':
        print("1. Review the validation report")
        print("2. Run the analysis using the generated batch file")
        print("3. Post-process results using process_orcawave_results.py")
    else:
        print("1. Review the validation report for issues")
        print("2. Fix any errors or warnings identified")
        print("3. Re-run validation")
        
    print("="*60)


if __name__ == "__main__":
    main()