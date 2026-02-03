#!/usr/bin/env python3
"""
Reference Seastate Scaling Fatigue Analysis CLI

This module provides command-line interface for fatigue analysis of strut foundations
using reference seastate scaling methodology with rainflow counting and S-N curves.

Usage:
    python -m digitalmodel.structural.fatigue_analysis [options]
    
Examples:
    # Run with default production data
    python -m digitalmodel.structural.fatigue_analysis
    
    # Run with specific input directory
    python -m digitalmodel.structural.fatigue_analysis --input-directory ./data
    
    # Run with sample data for testing
    python -m digitalmodel.structural.fatigue_analysis --sample --timesteps 1000
    
    # Run specific configurations
    python -m digitalmodel.structural.fatigue_analysis --configs fsts_l015,fsts_l095
"""

import argparse
import sys
import logging
from pathlib import Path
from typing import List, Optional
import json

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.structural.fatigue_analysis.reference_seastate_processor import (
    ReferenceSeaStateProcessor,
    FatigueCondition,
    Configuration
)

def setup_logging(verbose: bool = False):
    """Setup logging configuration"""
    level = logging.DEBUG if verbose else logging.INFO
    format_str = '%(asctime)s - %(name)s - %(levelname)s - %(message)s' if verbose else '%(levelname)s: %(message)s'
    
    logging.basicConfig(
        level=level,
        format=format_str,
        handlers=[
            logging.StreamHandler(sys.stdout),
        ]
    )
    
    # Suppress some verbose libraries
    if not verbose:
        logging.getLogger('matplotlib').setLevel(logging.WARNING)
        logging.getLogger('PIL').setLevel(logging.WARNING)

def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description='Reference Seastate Scaling Fatigue Analysis for Strut Foundations',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                              # Run with default production data
  %(prog)s --input-directory ./data    # Use specific input directory
  %(prog)s --sample --timesteps 1000   # Run with sample data
  %(prog)s --configs fsts_l015         # Process specific configuration
  %(prog)s --struts 1,2,3,4           # Process specific struts
  %(prog)s --dry-run                   # Preview without processing
        """
    )
    
    # Input/Output arguments
    parser.add_argument(
        '--input-directory', '--directory', '-d',
        type=str,
        dest='input_directory',
        help='Input directory containing CSV data files'
    )
    
    parser.add_argument(
        '--output-directory', '--output', '-o',
        type=str,
        dest='output_directory',
        default='output',
        help='Output directory for results (default: output)'
    )
    
    # Processing options
    parser.add_argument(
        '--configs',
        type=str,
        help='Comma-separated list of configurations to process (default: all)'
    )
    
    parser.add_argument(
        '--struts',
        type=str,
        default='1,2,3,4,5,6,7,8',
        help='Comma-separated list of strut numbers to process (default: 1-8)'
    )
    
    parser.add_argument(
        '--conditions',
        type=str,
        help='Path to fatigue conditions CSV file'
    )
    
    parser.add_argument(
        '--weights',
        type=str,
        help='Path to configuration weights CSV file'
    )
    
    parser.add_argument(
        '--stress-table',
        type=str,
        help='Path to tension-to-stress conversion table'
    )
    
    # Sample data options
    parser.add_argument(
        '--sample',
        action='store_true',
        help='Use sample data for testing'
    )
    
    parser.add_argument(
        '--timesteps',
        type=int,
        default=1000,
        help='Number of timesteps to use from each file (default: 1000)'
    )
    
    # S-N Curve options
    parser.add_argument(
        '--sn-curve',
        type=str,
        default='ABS_E_AIR',
        choices=['ABS_E_AIR', 'ABS_F_AIR', 'DNV_D_AIR', 'DNV_C_SEAWATER'],
        help='S-N curve to use for fatigue analysis (default: ABS_E_AIR)'
    )
    
    parser.add_argument(
        '--scf',
        type=float,
        default=1.0,
        help='Stress Concentration Factor (default: 1.0)'
    )
    
    parser.add_argument(
        '--design-life',
        type=float,
        default=20.0,
        help='Design life in years (default: 20.0)'
    )
    
    # Processing control
    parser.add_argument(
        '--parallel',
        type=int,
        default=1,
        help='Number of parallel workers (default: 1, 0=auto)'
    )
    
    parser.add_argument(
        '--adaptive',
        action='store_true',
        help='Use adaptive parallel processing based on system load'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Preview operations without processing'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )
    
    parser.add_argument(
        '--export-intermediate',
        action='store_true',
        help='Export intermediate results (tensions, cycles, damage)'
    )
    
    return parser.parse_args()

def validate_inputs(args):
    """Validate input arguments and paths"""
    errors = []
    
    # Check input directory
    if args.input_directory:
        input_path = Path(args.input_directory)
        if not input_path.exists():
            errors.append(f"Input directory does not exist: {input_path}")
    elif not args.sample:
        # Check for default production path
        prod_path = Path("D:/1522/ctr9/fatigue_wsp_method/07c_fatigue/csv")
        if not prod_path.exists():
            errors.append("Production data path not found. Use --input-directory or --sample")
    
    # Check optional file paths
    if args.conditions and not Path(args.conditions).exists():
        errors.append(f"Conditions file not found: {args.conditions}")
    
    if args.weights and not Path(args.weights).exists():
        errors.append(f"Weights file not found: {args.weights}")
    
    if args.stress_table and not Path(args.stress_table).exists():
        errors.append(f"Stress table file not found: {args.stress_table}")
    
    # Validate struts
    try:
        strut_list = [int(s.strip()) for s in args.struts.split(',')]
        if not all(1 <= s <= 8 for s in strut_list):
            errors.append("Strut numbers must be between 1 and 8")
    except ValueError:
        errors.append("Invalid strut numbers format. Use comma-separated integers.")
    
    if errors:
        for error in errors:
            logging.error(error)
        return False
    
    return True

def run_analysis(args):
    """Run the fatigue analysis with given arguments"""
    
    # Determine data source
    if args.sample:
        data_path = "sample_data"
        logging.info("Using sample data for analysis")
    elif args.input_directory:
        data_path = args.input_directory
        logging.info(f"Using input directory: {data_path}")
    else:
        data_path = "D:/1522/ctr9/fatigue_wsp_method/07c_fatigue/csv"
        logging.info(f"Using production data: {data_path}")
    
    # Parse configurations
    config_list = None
    if args.configs:
        config_list = [c.strip() for c in args.configs.split(',')]
        logging.info(f"Processing configurations: {', '.join(config_list)}")
    
    # Parse struts
    strut_list = [int(s.strip()) for s in args.struts.split(',')]
    logging.info(f"Processing struts: {strut_list}")
    
    # Initialize processor
    try:
        from .integrated_processor import (
            IntegratedFatigueProcessor,
            ProductionDataHandler
        )
        
        # Create data handler
        data_handler = ProductionDataHandler(
            base_path=data_path,
            sample_timesteps=args.timesteps
        )
        
        # Filter configurations if specified
        if config_list:
            available_configs = {k: v for k, v in data_handler.configurations.items() 
                               if k in config_list}
            if not available_configs:
                logging.error(f"No matching configurations found. Available: {list(data_handler.configurations.keys())}")
                return False
            data_handler.configurations = available_configs
        
        # Create processor
        processor = IntegratedFatigueProcessor(data_handler)
        
        # Override S-N curve if specified
        if args.sn_curve != 'ABS_E_AIR':
            from .fatigue_damage_calculator import FatigueDamageCalculator
            processor.fatigue_calculator = FatigueDamageCalculator(
                sn_curve=FatigueDamageCalculator.CURVES[args.sn_curve],
                scf=args.scf,
                design_life_years=args.design_life
            )
            logging.info(f"Using S-N curve: {args.sn_curve}, SCF: {args.scf}")
        
        # Load custom conditions if provided
        if args.conditions:
            import pandas as pd
            df = pd.read_csv(args.conditions)
            # Convert to FatigueCondition objects
            conditions = []
            for _, row in df.iterrows():
                from .integrated_processor import FatigueCondition
                conditions.append(FatigueCondition(
                    id=int(row.get('Row', len(conditions) + 1)),
                    wind_speed=float(row.get('Wind Speed (m/s)', 5.0)),
                    wind_dir=float(row.get('Wind Dir (°)', 0.0)),
                    hs=float(row.get('Hs (m)', 0.15)),
                    tp=float(row.get('Tp (s)', 2.0)),
                    wave_dir=float(row.get('Wave Dir (°)', 0.0)),
                    occurrence=float(row.get('Occurrence (%)', 1.0))
                ))
            processor.fatigue_conditions = conditions
            logging.info(f"Loaded {len(conditions)} fatigue conditions from {args.conditions}")
        
        # Create output directory
        output_path = Path(args.output_directory)
        output_path.mkdir(parents=True, exist_ok=True)
        
        if args.dry_run:
            # Preview mode
            logging.info("\n" + "="*60)
            logging.info("DRY RUN - Preview Mode")
            logging.info("="*60)
            
            logging.info(f"\nConfigurations to process:")
            for config_name, config in data_handler.configurations.items():
                logging.info(f"  - {config_name}: {config.description} (Weight: {config.weight}%)")
            
            logging.info(f"\nStruts to process: {strut_list}")
            logging.info(f"Fatigue conditions: {len(processor.fatigue_conditions)}")
            logging.info(f"Output directory: {output_path.absolute()}")
            
            # Count files
            total_files = 0
            for config_name in data_handler.configurations:
                config_path = Path(data_path) / config_name
                if config_path.exists():
                    ref_dirs = len(list(config_path.iterdir()))
                    total_files += ref_dirs * len(strut_list)
            
            logging.info(f"\nEstimated files to process: {total_files}")
            logging.info("\nUse without --dry-run to execute analysis")
            
            return True
        
        # Run the analysis
        logging.info("\n" + "="*60)
        logging.info("STARTING FATIGUE ANALYSIS")
        logging.info("="*60)
        
        # Check if parallel processing is requested
        if args.parallel and args.parallel > 1:
            # Use parallel processor
            from .parallel_processor import (
                ParallelFatigueProcessor,
                AdaptiveParallelProcessor
            )
            
            # Create base processor for parallel processing
            from .reference_seastate_processor import (
                ReferenceSeaStateProcessor
            )
            
            base_processor = ReferenceSeaStateProcessor(
                data_path=data_path,
                output_path=args.output_directory,
                scf=args.scf,
                design_life_years=args.design_life
            )
            
            # Create appropriate parallel processor
            if args.adaptive:
                parallel_processor = AdaptiveParallelProcessor(
                    base_processor=base_processor,
                    num_workers=args.parallel if args.parallel > 0 else None,
                    show_progress=not args.verbose
                )
                logging.info("Using adaptive parallel processing")
            else:
                parallel_processor = ParallelFatigueProcessor(
                    base_processor=base_processor,
                    num_workers=args.parallel,
                    show_progress=not args.verbose
                )
                logging.info(f"Using parallel processing with {args.parallel} workers")
            
            # Get configurations to process
            if config_list:
                configs_to_process = config_list
            else:
                configs_to_process = list(data_handler.configurations.keys())
            
            # Load fatigue conditions
            conditions = processor.fatigue_conditions
            
            # Create tasks and process in parallel
            tasks = parallel_processor.create_tasks(
                config_names=configs_to_process,
                strut_nums=strut_list,
                conditions=conditions
            )
            
            results = parallel_processor.process_batch_parallel(tasks)
            
            # Get performance metrics
            metrics = parallel_processor.get_performance_metrics()
            
            # Convert results to format expected by rest of code
            results_data = []
            for result in results:
                results_data.append({
                    'config_name': result.config_name,
                    'strut_num': result.strut_num,
                    'condition_id': result.condition_id,
                    'annual_damage': result.annual_damage,
                    'fatigue_life_years': result.fatigue_life_years,
                    'max_stress_range': result.max_stress_range,
                    'total_cycles': result.total_cycles
                })
            
            # Create summary
            from collections import defaultdict
            config_summaries = defaultdict(lambda: {
                'weight_pct': 0,
                'damages': [],
                'fatigue_lives': []
            })
            
            for result in results:
                config = result.config_name
                config_summaries[config]['damages'].append(result.annual_damage)
                config_summaries[config]['fatigue_lives'].append(result.fatigue_life_years)
                if config in data_handler.configurations:
                    config_summaries[config]['weight_pct'] = data_handler.configurations[config].weight
            
            summary = {}
            for config, data in config_summaries.items():
                summary[config] = {
                    'weight_pct': data['weight_pct'],
                    'critical_strut': min(enumerate(data['fatigue_lives'], 1), key=lambda x: x[1])[0] if data['fatigue_lives'] else 1,
                    'min_fatigue_life': min(data['fatigue_lives']) if data['fatigue_lives'] else 0,
                    'max_annual_damage': max(data['damages']) if data['damages'] else 0,
                    'mean_fatigue_life': np.mean(data['fatigue_lives']) if data['fatigue_lives'] else 0
                }
            
            # Save performance report
            if args.parallel > 1:
                parallel_processor.save_performance_report()
                logging.info(f"Parallel processing metrics:")
                logging.info(f"  Total tasks: {metrics['total_tasks']}")
                logging.info(f"  Successful: {metrics['successful_tasks']}")
                logging.info(f"  Average time per task: {metrics['average_time']:.2f}s")
                logging.info(f"  Parallelization efficiency: {metrics['parallelization_efficiency']:.1f}%")
            
            results = results_data
        else:
            # Use sequential processing (original method)
            from .integrated_processor import main
            results, summary = main(data_path)
        
        # Save results
        if results:
            # Export detailed results
            import pandas as pd
            results_df = pd.DataFrame(results)
            results_file = output_path / "fatigue_analysis_results.csv"
            results_df.to_csv(results_file, index=False)
            logging.info(f"Results saved to: {results_file}")
            
            # Export summary
            summary_file = output_path / "configuration_summary.json"
            with open(summary_file, 'w') as f:
                json.dump(summary, f, indent=2, default=str)
            logging.info(f"Summary saved to: {summary_file}")
            
            # Print summary
            logging.info("\n" + "="*60)
            logging.info("ANALYSIS COMPLETE")
            logging.info("="*60)
            
            for config_name, config_summary in summary.items():
                logging.info(f"\n{config_name}:")
                logging.info(f"  Weight: {config_summary['weight_pct']:.2f}%")
                logging.info(f"  Critical Strut: {config_summary['critical_strut']}")
                logging.info(f"  Min Fatigue Life: {config_summary['min_fatigue_life']:.1f} years")
            
            return True
        else:
            logging.error("No results generated")
            return False
            
    except Exception as e:
        logging.error(f"Analysis failed: {str(e)}", exc_info=args.verbose)
        return False

def main():
    """Main entry point"""
    args = parse_arguments()
    
    # Setup logging
    setup_logging(args.verbose)
    
    # Validate inputs
    if not validate_inputs(args):
        sys.exit(1)
    
    # Run analysis
    success = run_analysis(args)
    
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()