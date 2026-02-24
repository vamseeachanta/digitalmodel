#!/usr/bin/env python
"""
OrcaWave Batch Execution Script for Sea Cypress Diffraction Analysis
Automates the execution of OrcaWave analysis with error handling and logging.
"""

import os
import sys
import subprocess
import time
import logging
from pathlib import Path
from datetime import datetime
import json
import shutil

# Setup logging
log_dir = Path("../logs")
log_dir.mkdir(exist_ok=True)
log_file = log_dir / f"orcawave_run_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# Configuration
CONFIG_FILE = Path("../configs/sea_cypress_diffraction.yml")
OUTPUT_DIR = Path("../outputs")
ORCAWAVE_EXECUTABLE = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe"

class OrcaWaveRunner:
    """Manages OrcaWave analysis execution."""
    
    def __init__(self, config_file: Path, output_dir: Path):
        self.config_file = config_file
        self.output_dir = output_dir
        self.start_time = None
        self.end_time = None
        
    def validate_inputs(self) -> bool:
        """Validate all required inputs exist."""
        logger.info("Validating inputs...")
        
        # Check config file
        if not self.config_file.exists():
            logger.error(f"Configuration file not found: {self.config_file}")
            return False
        
        # Check GDF file referenced in config
        gdf_file = Path("../inputs/geometry/sea_cypress_trimesh.gdf")
        if not gdf_file.exists():
            logger.error(f"GDF geometry file not found: {gdf_file}")
            return False
        
        # Create output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        logger.info("OK: All inputs validated successfully")
        return True
    
    def check_license(self) -> bool:
        """Check if OrcaWave license is available."""
        logger.info("Checking OrcaWave license...")
        
        # Try to run OrcaWave in check mode
        try:
            result = subprocess.run(
                [ORCAWAVE_EXECUTABLE, "--check-license"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                logger.info("OK: OrcaWave license available")
                return True
            else:
                logger.warning("WARNING: OrcaWave license not available")
                return False
                
        except FileNotFoundError:
            logger.error(f"OrcaWave executable not found: {ORCAWAVE_EXECUTABLE}")
            return False
        except subprocess.TimeoutExpired:
            logger.error("License check timed out")
            return False
        except Exception as e:
            logger.error(f"License check failed: {e}")
            return False
    
    def estimate_runtime(self) -> dict:
        """Estimate runtime based on mesh size and analysis parameters."""
        # Read mesh statistics
        gdf_file = Path("../inputs/geometry/sea_cypress_trimesh.gdf")
        
        with open(gdf_file, 'r') as f:
            lines = f.readlines()
            n_vertices = int(lines[2].strip())
            n_panels = int(lines[3 + n_vertices].strip())
        
        # Count analysis points
        n_frequencies = 18  # From config
        n_headings = 9      # From config
        
        # Estimate (rough approximation)
        base_time_per_calc = 2  # seconds per frequency/heading combination
        total_calcs = n_frequencies * n_headings
        estimated_seconds = total_calcs * base_time_per_calc * (n_panels / 1000)
        
        return {
            'n_panels': n_panels,
            'n_vertices': n_vertices,
            'n_frequencies': n_frequencies,
            'n_headings': n_headings,
            'total_calculations': total_calcs,
            'estimated_minutes': estimated_seconds / 60,
            'estimated_hours': estimated_seconds / 3600
        }
    
    def run_analysis(self, dry_run: bool = False) -> bool:
        """Execute OrcaWave analysis."""
        if dry_run:
            logger.info("DRY RUN MODE - Not executing actual analysis")
            estimate = self.estimate_runtime()
            logger.info(f"Analysis would process:")
            logger.info(f"  - {estimate['n_panels']} panels")
            logger.info(f"  - {estimate['n_frequencies']} frequencies")
            logger.info(f"  - {estimate['n_headings']} wave headings")
            logger.info(f"  - {estimate['total_calculations']} total calculations")
            logger.info(f"  - Estimated runtime: {estimate['estimated_hours']:.1f} hours")
            return True
        
        logger.info("Starting OrcaWave analysis...")
        self.start_time = time.time()
        
        # Prepare command
        cmd = [
            ORCAWAVE_EXECUTABLE,
            "-i", str(self.config_file),
            "-o", str(self.output_dir),
            "-v"  # Verbose output
        ]
        
        try:
            # Run OrcaWave
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1
            )
            
            # Stream output
            for line in process.stdout:
                logger.info(f"OrcaWave: {line.strip()}")
            
            # Wait for completion
            return_code = process.wait()
            
            self.end_time = time.time()
            runtime = self.end_time - self.start_time
            
            if return_code == 0:
                logger.info(f"OK: Analysis completed successfully in {runtime/60:.1f} minutes")
                return True
            else:
                logger.error(f"ERROR: Analysis failed with return code {return_code}")
                return False
                
        except Exception as e:
            logger.error(f"Analysis execution failed: {e}")
            return False
    
    def validate_outputs(self) -> bool:
        """Validate that expected output files were created."""
        logger.info("Validating outputs...")
        
        expected_files = [
            "sea_cypress_RAOs.xlsx",
            "sea_cypress_added_mass.csv",
            "sea_cypress_damping.csv",
            "sea_cypress_excitation.csv"
        ]
        
        missing_files = []
        for file_name in expected_files:
            file_path = self.output_dir / file_name
            if not file_path.exists():
                missing_files.append(file_name)
            else:
                size_mb = file_path.stat().st_size / (1024 * 1024)
                logger.info(f"  OK: {file_name} ({size_mb:.2f} MB)")
        
        if missing_files:
            logger.warning(f"Missing expected files: {missing_files}")
            return False
        
        logger.info("OK: All expected outputs generated")
        return True
    
    def generate_report(self):
        """Generate analysis summary report."""
        report = {
            'timestamp': datetime.now().isoformat(),
            'config_file': str(self.config_file),
            'runtime_seconds': self.end_time - self.start_time if self.end_time else None,
            'status': 'completed' if self.validate_outputs() else 'incomplete',
            'log_file': str(log_file)
        }
        
        report_file = self.output_dir / "analysis_report.json"
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        logger.info(f"Report saved to: {report_file}")


def main():
    """Main execution function."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Run OrcaWave analysis for Sea Cypress")
    parser.add_argument('--dry-run', action='store_true', help='Validate inputs without running')
    parser.add_argument('--skip-license-check', action='store_true', help='Skip license validation')
    args = parser.parse_args()
    
    logger.info("="*60)
    logger.info("OrcaWave Sea Cypress Diffraction Analysis")
    logger.info("="*60)
    
    # Initialize runner
    runner = OrcaWaveRunner(CONFIG_FILE, OUTPUT_DIR)
    
    # Validation steps
    if not runner.validate_inputs():
        logger.error("Input validation failed")
        sys.exit(1)
    
    if not args.skip_license_check and not args.dry_run:
        if not runner.check_license():
            logger.error("License check failed")
            logger.info("Run with --dry-run to validate setup without license")
            sys.exit(1)
    
    # Show runtime estimate
    estimate = runner.estimate_runtime()
    logger.info(f"Estimated runtime: {estimate['estimated_hours']:.1f} hours")
    
    if not args.dry_run:
        response = input("Continue with analysis? [y/N]: ")
        if response.lower() != 'y':
            logger.info("Analysis cancelled by user")
            sys.exit(0)
    
    # Run analysis
    success = runner.run_analysis(dry_run=args.dry_run)
    
    if success and not args.dry_run:
        runner.validate_outputs()
        runner.generate_report()
    
    logger.info("="*60)
    logger.info("Analysis complete" if success else "Analysis failed")
    logger.info("="*60)
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()