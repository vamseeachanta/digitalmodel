#!/usr/bin/env python3
"""
OrcaWave Python Wrapper with COM API Integration

This script provides a Python interface for running OrcaWave analyses
with progress monitoring and result extraction capabilities.

Author: OrcaWave Automation Module
Created: 2025-08-26
"""

import os
import sys
import time
import logging
import subprocess
import argparse
from pathlib import Path
from typing import Optional, List, Dict, Any
from datetime import datetime
import yaml
import json

# Try to import COM support (Windows only)
try:
    import win32com.client
    import pythoncom
    COM_AVAILABLE = True
except ImportError:
    COM_AVAILABLE = False
    print("Warning: win32com not available. COM API features disabled.")

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaWaveRunner:
    """OrcaWave execution wrapper with COM API support."""
    
    def __init__(self, orcawave_path: str = None):
        """Initialize OrcaWave runner.
        
        Args:
            orcawave_path: Path to OrcaWave executable
        """
        self.orcawave_path = orcawave_path or self._find_orcawave()
        self.com_object = None
        self.results_dir = Path(__file__).parent.parent / "outputs" / "results"
        self.logs_dir = Path(__file__).parent.parent / "outputs" / "logs"
        
        # Create directories
        self.results_dir.mkdir(parents=True, exist_ok=True)
        self.logs_dir.mkdir(parents=True, exist_ok=True)
        
        logger.info(f"OrcaWave path: {self.orcawave_path}")
        
    def _find_orcawave(self) -> str:
        """Find OrcaWave installation.
        
        Returns:
            Path to OrcaWave executable
        """
        # Common installation paths
        common_paths = [
            r"C:\Program Files\Orcina\OrcaWave\11.5\OrcaWave.exe",
            r"C:\Program Files\Orcina\OrcaWave\11.4\OrcaWave.exe",
            r"C:\Program Files (x86)\Orcina\OrcaWave\11.5\OrcaWave.exe",
            r"C:\Program Files (x86)\Orcina\OrcaWave\11.4\OrcaWave.exe",
        ]
        
        for path in common_paths:
            if os.path.exists(path):
                return path
                
        # Try to find from registry or environment
        orcawave_env = os.environ.get('ORCAWAVE_PATH')
        if orcawave_env and os.path.exists(orcawave_env):
            return orcawave_env
            
        raise FileNotFoundError(
            "OrcaWave not found. Please install OrcaWave or set ORCAWAVE_PATH environment variable."
        )
        
    def init_com_api(self) -> bool:
        """Initialize COM API connection.
        
        Returns:
            True if successful, False otherwise
        """
        if not COM_AVAILABLE:
            logger.warning("COM API not available on this system")
            return False
            
        try:
            pythoncom.CoInitialize()
            self.com_object = win32com.client.Dispatch("OrcaWave.Application")
            logger.info("COM API initialized successfully")
            return True
        except Exception as e:
            logger.error(f"Failed to initialize COM API: {e}")
            return False
            
    def run_batch(self, config_file: Path, output_dir: Optional[Path] = None,
                  monitor_progress: bool = True) -> Dict[str, Any]:
        """Run OrcaWave in batch mode.
        
        Args:
            config_file: Path to configuration YAML
            output_dir: Output directory for results
            monitor_progress: Enable progress monitoring
            
        Returns:
            Dictionary with execution results
        """
        config_file = Path(config_file)
        if not config_file.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_file}")
            
        # Prepare output directory
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        config_name = config_file.stem
        
        if output_dir is None:
            output_dir = self.results_dir / f"{config_name}_{timestamp}"
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Prepare log file
        log_file = self.logs_dir / f"{config_name}_{timestamp}.log"
        
        logger.info(f"Running batch analysis: {config_name}")
        logger.info(f"Configuration: {config_file}")
        logger.info(f"Output directory: {output_dir}")
        
        # Build command
        cmd = [
            self.orcawave_path,
            "/batch",
            str(config_file),
            "/output", str(output_dir)
        ]
        
        # Execute with progress monitoring
        start_time = time.time()
        result = {
            'config': config_name,
            'start_time': datetime.now().isoformat(),
            'status': 'running',
            'output_dir': str(output_dir),
            'log_file': str(log_file)
        }
        
        try:
            if monitor_progress:
                process = subprocess.Popen(
                    cmd,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                    bufsize=1
                )
                
                # Monitor output
                with open(log_file, 'w') as log:
                    for line in process.stdout:
                        log.write(line)
                        log.flush()
                        
                        # Parse progress indicators
                        if "Progress:" in line or "%" in line:
                            progress = self._extract_progress(line)
                            if progress:
                                self._show_progress(progress, config_name)
                                
                process.wait()
                return_code = process.returncode
                
            else:
                # Simple execution without monitoring
                with open(log_file, 'w') as log:
                    return_code = subprocess.call(cmd, stdout=log, stderr=subprocess.STDOUT)
                    
            # Check result
            elapsed_time = time.time() - start_time
            
            if return_code == 0:
                result['status'] = 'completed'
                result['message'] = 'Analysis completed successfully'
                logger.info(f"✅ Analysis completed in {elapsed_time:.1f} seconds")
                
                # Check for output files
                output_files = self._check_outputs(output_dir)
                result['output_files'] = output_files
                
            else:
                result['status'] = 'failed'
                result['message'] = f'Analysis failed with code {return_code}'
                logger.error(f"❌ Analysis failed with code {return_code}")
                
        except Exception as e:
            result['status'] = 'error'
            result['message'] = str(e)
            logger.error(f"Error running analysis: {e}")
            
        result['end_time'] = datetime.now().isoformat()
        result['elapsed_time'] = time.time() - start_time
        
        return result
        
    def run_com_api(self, config_file: Path, output_dir: Optional[Path] = None) -> Dict[str, Any]:
        """Run OrcaWave using COM API for advanced control.
        
        Args:
            config_file: Path to configuration YAML
            output_dir: Output directory for results
            
        Returns:
            Dictionary with execution results
        """
        if not self.com_object:
            if not self.init_com_api():
                logger.warning("COM API not available, falling back to batch mode")
                return self.run_batch(config_file, output_dir)
                
        config_file = Path(config_file)
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        config_name = config_file.stem
        
        if output_dir is None:
            output_dir = self.results_dir / f"{config_name}_{timestamp}"
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        logger.info(f"Running via COM API: {config_name}")
        
        result = {
            'config': config_name,
            'start_time': datetime.now().isoformat(),
            'status': 'running',
            'output_dir': str(output_dir),
            'method': 'COM API'
        }
        
        try:
            # Load model
            self.com_object.LoadModel(str(config_file))
            
            # Set output directory
            self.com_object.OutputDirectory = str(output_dir)
            
            # Configure analysis options
            self.com_object.CalculateRAOs = True
            self.com_object.CalculateQTFs = True
            self.com_object.OutputPanelPressures = False
            
            # Run calculation
            logger.info("Starting calculation...")
            self.com_object.Calculate()
            
            # Monitor progress
            while self.com_object.CalculationInProgress:
                progress = self.com_object.CalculationProgress
                self._show_progress(progress, config_name)
                time.sleep(1)
                
            # Check completion
            if self.com_object.CalculationCompleted:
                result['status'] = 'completed'
                result['message'] = 'Analysis completed successfully'
                logger.info("✅ Analysis completed successfully")
                
                # Extract results
                result['rao_data'] = self._extract_raos_com()
                result['qtf_data'] = self._extract_qtfs_com()
                
            else:
                result['status'] = 'failed'
                result['message'] = self.com_object.LastError
                logger.error(f"❌ Analysis failed: {result['message']}")
                
        except Exception as e:
            result['status'] = 'error'
            result['message'] = str(e)
            logger.error(f"COM API error: {e}")
            
        result['end_time'] = datetime.now().isoformat()
        
        return result
        
    def run_parallel(self, config_files: List[Path], max_workers: int = 4) -> List[Dict[str, Any]]:
        """Run multiple configurations in parallel.
        
        Args:
            config_files: List of configuration files
            max_workers: Maximum parallel workers
            
        Returns:
            List of results for each configuration
        """
        from concurrent.futures import ProcessPoolExecutor, as_completed
        
        logger.info(f"Running {len(config_files)} configurations in parallel (max workers: {max_workers})")
        
        results = []
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            # Submit all jobs
            future_to_config = {
                executor.submit(self.run_batch, config, monitor_progress=False): config
                for config in config_files
            }
            
            # Process completed jobs
            for future in as_completed(future_to_config):
                config = future_to_config[future]
                try:
                    result = future.result()
                    results.append(result)
                    logger.info(f"Completed: {config.stem} - {result['status']}")
                except Exception as e:
                    logger.error(f"Failed: {config.stem} - {e}")
                    results.append({
                        'config': config.stem,
                        'status': 'error',
                        'message': str(e)
                    })
                    
        return results
        
    def _extract_progress(self, line: str) -> Optional[float]:
        """Extract progress percentage from output line.
        
        Args:
            line: Output line from OrcaWave
            
        Returns:
            Progress percentage (0-100) or None
        """
        import re
        
        # Look for percentage patterns
        patterns = [
            r'(\d+(?:\.\d+)?)\s*%',
            r'Progress:\s*(\d+(?:\.\d+)?)',
            r'\[(\d+)/(\d+)\]'
        ]
        
        for pattern in patterns:
            match = re.search(pattern, line)
            if match:
                if len(match.groups()) == 2:
                    # Format: [current/total]
                    current, total = float(match.group(1)), float(match.group(2))
                    return (current / total) * 100
                else:
                    # Direct percentage
                    return float(match.group(1))
                    
        return None
        
    def _show_progress(self, progress: float, task_name: str):
        """Display progress bar.
        
        Args:
            progress: Progress percentage (0-100)
            task_name: Name of current task
        """
        bar_length = 40
        filled = int(bar_length * progress / 100)
        bar = '█' * filled + '░' * (bar_length - filled)
        
        sys.stdout.write(f'\r{task_name}: [{bar}] {progress:.1f}%')
        sys.stdout.flush()
        
        if progress >= 100:
            sys.stdout.write('\n')
            
    def _check_outputs(self, output_dir: Path) -> List[str]:
        """Check for output files in directory.
        
        Args:
            output_dir: Directory to check
            
        Returns:
            List of output file names
        """
        output_files = []
        
        # Expected output file patterns
        patterns = ['*.out', '*.csv', '*.dat', '*.txt', '*.yml']
        
        for pattern in patterns:
            files = list(output_dir.glob(pattern))
            output_files.extend([f.name for f in files])
            
        if output_files:
            logger.info(f"Found {len(output_files)} output files")
        else:
            logger.warning("No output files found")
            
        return output_files
        
    def _extract_raos_com(self) -> Optional[Dict[str, Any]]:
        """Extract RAO data using COM API.
        
        Returns:
            Dictionary of RAO data or None
        """
        if not self.com_object:
            return None
            
        try:
            rao_data = {
                'periods': list(self.com_object.Periods),
                'headings': list(self.com_object.Headings),
                'raos': {}
            }
            
            # Extract RAOs for each DOF
            dofs = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
            for dof in dofs:
                rao_data['raos'][dof] = self.com_object.GetRAO(dof)
                
            return rao_data
            
        except Exception as e:
            logger.error(f"Failed to extract RAOs: {e}")
            return None
            
    def _extract_qtfs_com(self) -> Optional[Dict[str, Any]]:
        """Extract QTF data using COM API.
        
        Returns:
            Dictionary of QTF data or None
        """
        if not self.com_object:
            return None
            
        try:
            qtf_data = {
                'frequencies': list(self.com_object.QTFFrequencies),
                'qtf_matrix': self.com_object.GetQTFMatrix()
            }
            return qtf_data
            
        except Exception as e:
            logger.error(f"Failed to extract QTFs: {e}")
            return None


def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(
        description='OrcaWave Python Wrapper with COM API Integration'
    )
    
    parser.add_argument(
        'config',
        nargs='?',
        help='Configuration file or "all" for batch processing'
    )
    
    parser.add_argument(
        '--mode',
        choices=['batch', 'com', 'parallel'],
        default='batch',
        help='Execution mode (default: batch)'
    )
    
    parser.add_argument(
        '--output-dir',
        type=Path,
        help='Output directory for results'
    )
    
    parser.add_argument(
        '--orcawave-path',
        help='Path to OrcaWave executable'
    )
    
    parser.add_argument(
        '--max-workers',
        type=int,
        default=4,
        help='Maximum parallel workers (default: 4)'
    )
    
    parser.add_argument(
        '--no-progress',
        action='store_true',
        help='Disable progress monitoring'
    )
    
    parser.add_argument(
        '--save-results',
        help='Save results to JSON file'
    )
    
    args = parser.parse_args()
    
    # Initialize runner
    runner = OrcaWaveRunner(args.orcawave_path)
    
    # Handle different execution modes
    config_dir = Path(__file__).parent.parent / "outputs" / "orcawave_configs" / "merged"
    
    if not args.config:
        print("Available configurations:")
        for config in config_dir.glob("*.yml"):
            print(f"  - {config.stem}")
        return
        
    if args.config == "all":
        # Process all configurations
        config_files = list(config_dir.glob("*.yml"))
        
        if args.mode == 'parallel':
            results = runner.run_parallel(config_files, args.max_workers)
        else:
            results = []
            for config in config_files:
                if args.mode == 'com':
                    result = runner.run_com_api(config, args.output_dir)
                else:
                    result = runner.run_batch(
                        config,
                        args.output_dir,
                        monitor_progress=not args.no_progress
                    )
                results.append(result)
                
        # Display summary
        print("\n" + "="*80)
        print("EXECUTION SUMMARY")
        print("="*80)
        
        for result in results:
            status_icon = "✅" if result['status'] == 'completed' else "❌"
            print(f"{status_icon} {result['config']}: {result['status']}")
            if result.get('elapsed_time'):
                print(f"   Time: {result['elapsed_time']:.1f} seconds")
                
    else:
        # Process single configuration
        config_file = Path(args.config)
        
        if not config_file.exists():
            config_file = config_dir / args.config
            if not config_file.suffix:
                config_file = config_file.with_suffix('.yml')
                
        if not config_file.exists():
            print(f"Error: Configuration file not found: {config_file}")
            return 1
            
        if args.mode == 'com':
            result = runner.run_com_api(config_file, args.output_dir)
        else:
            result = runner.run_batch(
                config_file,
                args.output_dir,
                monitor_progress=not args.no_progress
            )
            
        # Display result
        print("\n" + "="*80)
        print(f"Result: {result['status'].upper()}")
        if result.get('message'):
            print(f"Message: {result['message']}")
        if result.get('output_files'):
            print(f"Output files: {len(result['output_files'])}")
            
        results = [result]
        
    # Save results if requested
    if args.save_results:
        with open(args.save_results, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        print(f"\nResults saved to: {args.save_results}")
        
    # Return appropriate exit code
    all_successful = all(r['status'] == 'completed' for r in results)
    return 0 if all_successful else 1


if __name__ == "__main__":
    sys.exit(main())