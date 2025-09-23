#!/usr/bin/env python
"""
Command Line Interface for Load Scaling Module
==============================================

Standalone CLI runner for the load scaling module that can be executed
directly from the command line with various options.

Usage Examples:
    # Run with YAML configuration
    python cli_load_scaling.py config.yml
    
    # Run with verbose output
    python cli_load_scaling.py config.yml --verbose
    
    # Validate configuration only
    python cli_load_scaling.py config.yml --validate-only
    
    # Generate template configuration
    python cli_load_scaling.py --generate-template > my_config.yml
"""

import argparse
import sys
import os
from pathlib import Path
import pandas as pd
import numpy as np
import logging
from datetime import datetime
import json
import yaml
from typing import List, Dict, Optional
from concurrent.futures import ProcessPoolExecutor, as_completed
import time

# Add module path
MODULE_DIR = Path(__file__).parent
sys.path.insert(0, str(MODULE_DIR.parent.parent))

from digitalmodel.modules.fatigue_analysis.load_scaling import LoadScalingProcessor

# Configure logging
def setup_logging(verbose: bool = False):
    """Setup logging configuration"""
    level = logging.DEBUG if verbose else logging.INFO
    format_str = '%(asctime)s - %(levelname)s - %(message)s' if verbose else '%(levelname)s: %(message)s'
    
    logging.basicConfig(
        level=level,
        format=format_str,
        handlers=[
            logging.StreamHandler(sys.stdout),
            logging.FileHandler('load_scaling.log')
        ]
    )
    return logging.getLogger(__name__)


class LoadScalingCLI:
    """Command line interface for load scaling module"""
    
    def __init__(self, args):
        """Initialize CLI with parsed arguments"""
        self.args = args
        self.logger = setup_logging(args.verbose)
        self.start_time = time.time()
        
        # Setup paths
        self.setup_paths()
        
    def setup_paths(self):
        """Setup input and output paths"""
        # Default to sample data if not specified
        if not self.args.data_dir:
            spec_dir = Path("D:/github/digitalmodel/specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue")
            if spec_dir.exists():
                self.args.data_dir = str(spec_dir / "sample_data")
                self.logger.info(f"Using sample data from: {self.args.data_dir}")
        
        # Create output directory
        self.output_dir = Path(self.args.output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def process_single_configuration(self, config: str) -> Dict:
        """Process a single configuration"""
        self.logger.info(f"\nProcessing configuration: {config}")
        self.logger.info("=" * 60)
        
        try:
            # Initialize load scaler
            scaler = LoadScaler(
                reference_seastates_path=self.args.reference_seastates,
                fatigue_conditions_path=self.args.fatigue_conditions,
                output_dir=str(self.output_dir / config)
            )
            
            # Process all conditions
            summary = scaler.process_all_conditions(save_outputs=not self.args.no_save)
            
            # Calculate statistics
            stats = {
                'config': config,
                'conditions_processed': len(summary),
                'max_load': summary['max_load'].max(),
                'mean_load': summary['mean_load'].mean(),
                'processing_time': time.time() - self.start_time
            }
            
            self.logger.info(f"✓ Completed {config}: {len(summary)} conditions processed")
            
            return stats
            
        except Exception as e:
            self.logger.error(f"✗ Failed to process {config}: {str(e)}")
            return {'config': config, 'error': str(e)}
    
    def process_batch(self):
        """Process multiple configurations in batch mode"""
        configs = self.args.config if isinstance(self.args.config, list) else [self.args.config]
        
        if self.args.parallel > 1:
            self.logger.info(f"Processing {len(configs)} configurations in parallel (workers: {self.args.parallel})")
            results = self.process_parallel(configs)
        else:
            self.logger.info(f"Processing {len(configs)} configurations sequentially")
            results = [self.process_single_configuration(c) for c in configs]
        
        return results
    
    def process_parallel(self, configs: List[str]) -> List[Dict]:
        """Process configurations in parallel"""
        results = []
        
        with ProcessPoolExecutor(max_workers=self.args.parallel) as executor:
            future_to_config = {
                executor.submit(self.process_single_configuration, config): config
                for config in configs
            }
            
            for future in as_completed(future_to_config):
                config = future_to_config[future]
                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    self.logger.error(f"Failed to process {config}: {e}")
                    results.append({'config': config, 'error': str(e)})
        
        return results
    
    def validate_inputs(self) -> bool:
        """Validate input files and parameters"""
        self.logger.info("Validating inputs...")
        
        # Check reference seastates file
        if not Path(self.args.reference_seastates).exists():
            self.logger.error(f"Reference seastates file not found: {self.args.reference_seastates}")
            return False
        
        # Check fatigue conditions file
        if not Path(self.args.fatigue_conditions).exists():
            self.logger.error(f"Fatigue conditions file not found: {self.args.fatigue_conditions}")
            return False
        
        # Validate data if checking
        if self.args.validate:
            try:
                # Load and check reference seastates
                ref_df = pd.read_csv(self.args.reference_seastates)
                self.logger.info(f"  Reference seastates: {len(ref_df)} entries")
                
                # Load and check fatigue conditions
                fatigue_df = pd.read_csv(self.args.fatigue_conditions)
                self.logger.info(f"  Fatigue conditions: {len(fatigue_df)} entries")
                
                # Check occurrence sum
                occurrence_sum = fatigue_df['Occurrence (%)'].sum() if 'Occurrence (%)' in fatigue_df.columns else 0
                if abs(occurrence_sum - 100.0) > 0.1:
                    self.logger.warning(f"  Occurrence sum: {occurrence_sum:.1f}% (should be 100%)")
                
            except Exception as e:
                self.logger.error(f"Validation failed: {e}")
                return False
        
        self.logger.info("✓ Input validation passed")
        return True
    
    def generate_report(self, results: List[Dict]):
        """Generate processing report"""
        report_path = self.output_dir / "cli_processing_report.json"
        
        # Prepare report data
        report = {
            'timestamp': datetime.now().isoformat(),
            'command': ' '.join(sys.argv),
            'parameters': vars(self.args),
            'results': results,
            'summary': {
                'total_configs': len(results),
                'successful': len([r for r in results if 'error' not in r]),
                'failed': len([r for r in results if 'error' in r]),
                'total_time': time.time() - self.start_time
            }
        }
        
        # Save JSON report
        with open(report_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        self.logger.info(f"Report saved to: {report_path}")
        
        # Print summary
        print("\n" + "=" * 80)
        print("PROCESSING SUMMARY")
        print("=" * 80)
        print(f"Configurations processed: {report['summary']['total_configs']}")
        print(f"Successful: {report['summary']['successful']}")
        print(f"Failed: {report['summary']['failed']}")
        print(f"Total time: {report['summary']['total_time']:.2f} seconds")
    
    def run(self):
        """Main execution method"""
        print("=" * 80)
        print("LOAD SCALING MODULE - CLI")
        print("=" * 80)
        print(f"Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        # Validate inputs
        if not self.validate_inputs():
            return 1
        
        # Process based on mode
        if self.args.test:
            self.run_test_mode()
        elif self.args.batch:
            results = self.process_batch()
            self.generate_report(results)
        else:
            result = self.process_single_configuration(self.args.config)
            self.generate_report([result])
        
        # Print completion
        elapsed = time.time() - self.start_time
        print(f"\n✓ Completed in {elapsed:.2f} seconds")
        print(f"Output directory: {self.output_dir}")
        
        return 0
    
    def run_test_mode(self):
        """Run in test mode with sample data"""
        self.logger.info("Running in TEST mode with sample data")
        
        # Create small test dataset
        test_conditions = pd.DataFrame([
            {'Row': 1, 'Wind Speed (m/s)': 5, 'Wind Dir (°)': 0, 
             'Hs (m)': 0.25, 'Tp (s)': 2.0, 'Wave Dir (°)': 0, 'Occurrence (%)': 50},
            {'Row': 2, 'Wind Speed (m/s)': 10, 'Wind Dir (°)': 180,
             'Hs (m)': 0.5, 'Tp (s)': 3.0, 'Wave Dir (°)': 180, 'Occurrence (%)': 50}
        ])
        
        test_path = self.output_dir / "test_conditions.csv"
        test_conditions.to_csv(test_path, index=False)
        
        # Process test data
        self.args.fatigue_conditions = str(test_path)
        result = self.process_single_configuration('test_config')
        
        print("\nTest Results:")
        print(f"  Conditions processed: {result.get('conditions_processed', 0)}")
        print(f"  Max load: {result.get('max_load', 0):.2f}")
        print(f"  Mean load: {result.get('mean_load', 0):.2f}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='Load Scaling Module - Scales reference seastate loads to fatigue conditions',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                                    # Run with defaults
  %(prog)s --config fsts_l015                # Process specific configuration  
  %(prog)s --batch --parallel 4              # Batch process with 4 workers
  %(prog)s --test                            # Run test mode
  %(prog)s --validate --verbose              # Validate inputs with verbose output
        """
    )
    
    # Input files
    parser.add_argument('--reference-seastates', 
                       default='input/reference_seastates.csv',
                       help='Path to reference seastates CSV file')
    parser.add_argument('--fatigue-conditions',
                       default='input/fatigue_conditions.csv',
                       help='Path to fatigue conditions CSV file')
    parser.add_argument('--data-dir',
                       help='Directory containing time series data')
    
    # Processing options
    parser.add_argument('--config', default='fsts_l015',
                       help='Configuration to process (e.g., fsts_l015)')
    parser.add_argument('--struts', type=str,
                       help='Comma-separated list of struts to process (e.g., 1,2,3,4)')
    parser.add_argument('--conditions', type=str,
                       help='Comma-separated list of condition IDs to process')
    
    # Output options
    parser.add_argument('--output-dir', default='output/load_scaling',
                       help='Output directory for results')
    parser.add_argument('--no-save', action='store_true',
                       help='Do not save individual output files')
    
    # Execution modes
    parser.add_argument('--batch', action='store_true',
                       help='Process all configurations in batch mode')
    parser.add_argument('--parallel', type=int, default=1,
                       help='Number of parallel workers for batch processing')
    parser.add_argument('--test', action='store_true',
                       help='Run in test mode with sample data')
    
    # Validation and debugging
    parser.add_argument('--validate', action='store_true',
                       help='Validate input files before processing')
    parser.add_argument('--verbose', action='store_true',
                       help='Enable verbose output')
    parser.add_argument('--dry-run', action='store_true',
                       help='Show what would be done without executing')
    
    args = parser.parse_args()
    
    # Handle special arguments
    if args.struts:
        args.struts = [int(s) for s in args.struts.split(',')]
    if args.conditions:
        args.conditions = [int(c) for c in args.conditions.split(',')]
    
    # Dry run mode
    if args.dry_run:
        print("DRY RUN MODE - No processing will occur")
        print("\nConfiguration:")
        for key, value in vars(args).items():
            print(f"  {key}: {value}")
        return 0
    
    # Create and run CLI
    cli = LoadScalingCLI(args)
    return cli.run()


if __name__ == "__main__":
    sys.exit(main())