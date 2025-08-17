#!/usr/bin/env python
"""
Command-Line Interface for Mooring Tension Iteration System
Provides easy access to run iterations with various configurations
"""

import argparse
import sys
import logging
from pathlib import Path
import yaml
from datetime import datetime
import json

from main_orchestrator import MooringIterationOrchestrator


def load_config(config_path: Path) -> dict:
    """Load configuration from YAML file"""
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    return config


def setup_logging(config: dict):
    """Set up logging based on configuration"""
    log_config = config.get('logging', {})
    
    # Create formatters
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Get root logger
    logger = logging.getLogger()
    logger.setLevel(getattr(logging, log_config.get('level', 'INFO')))
    
    # Clear existing handlers
    logger.handlers = []
    
    # Console handler
    if log_config.get('console_logging', True):
        console_handler = logging.StreamHandler()
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)
    
    # File handler
    if log_config.get('file_logging', True):
        log_folder = Path(config['paths']['base_path']) / log_config.get('log_folder', 'logs')
        log_folder.mkdir(parents=True, exist_ok=True)
        
        log_file = log_folder / f"mooring_iteration_{datetime.now():%Y%m%d_%H%M%S}.log"
        file_handler = logging.FileHandler(log_file)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)
    
    return logger


def validate_paths(config: dict) -> bool:
    """Validate that required paths exist"""
    base_path = Path(config['paths']['base_path'])
    
    if not base_path.exists():
        print(f"ERROR: Base path does not exist: {base_path}")
        return False
    
    # Check for target CSV
    target_csv = base_path / config['paths']['pretension_folder'] / config['target_files']['default']
    if not target_csv.exists():
        print(f"ERROR: Target CSV not found: {target_csv}")
        return False
    
    return True


def run_iteration(args):
    """Run the mooring tension iteration process"""
    # Load configuration
    config_path = Path(args.config)
    if not config_path.exists():
        print(f"ERROR: Configuration file not found: {config_path}")
        return 1
    
    config = load_config(config_path)
    
    # Override config with command-line arguments
    if args.max_iterations:
        config['iteration']['max_iterations'] = args.max_iterations
    if args.tolerance:
        config['iteration']['convergence_tolerance'] = args.tolerance
    if args.damping:
        config['iteration']['damping_factor'] = args.damping
    if args.target_csv:
        config['target_files']['default'] = args.target_csv
    if args.model:
        config['models']['static_6dof'] = args.model
    
    # Set up logging
    logger = setup_logging(config)
    logger.info("Starting Mooring Tension Iteration CLI")
    
    # Validate paths
    if not validate_paths(config):
        return 1
    
    # Build paths
    base_path = Path(config['paths']['base_path'])
    target_csv = base_path / config['paths']['pretension_folder'] / config['target_files']['default']
    model_file = config['models']['static_6dof']
    
    # Prepare orchestrator config
    orch_config = {
        'max_iterations': config['iteration']['max_iterations'],
        'convergence_tolerance': config['iteration']['convergence_tolerance'],
        'damping_factor': config['iteration']['damping_factor'],
        'extraction_config': config['models']['extraction_config'],
        'output_dir': base_path / config['paths']['output_folder'],
        'save_history': config['output']['save_history'],
        'plot_convergence': config['output']['plot_convergence']
    }
    
    # Create orchestrator
    try:
        orchestrator = MooringIterationOrchestrator(
            base_path=base_path,
            target_csv=target_csv,
            model_file=model_file,
            config=orch_config
        )
    except Exception as e:
        logger.error(f"Failed to initialize orchestrator: {e}")
        return 1
    
    # Run iteration process
    try:
        converged, results = orchestrator.run_iteration_process()
        
        if converged:
            logger.info("Process completed successfully - CONVERGED")
            return 0
        else:
            logger.warning("Process completed - NOT CONVERGED")
            return 2
            
    except KeyboardInterrupt:
        logger.warning("Process interrupted by user")
        return 130
    except Exception as e:
        logger.error(f"Process failed with error: {e}", exc_info=True)
        return 1


def check_status(args):
    """Check the status of a previous run"""
    output_dir = Path(args.output_dir)
    
    if not output_dir.exists():
        print(f"Output directory not found: {output_dir}")
        return 1
    
    # Find most recent history file
    history_files = list(output_dir.glob("iteration_history_*.json"))
    
    if not history_files:
        print("No iteration history files found")
        return 1
    
    # Get most recent
    latest_history = max(history_files, key=lambda p: p.stat().st_mtime)
    
    # Load and display
    with open(latest_history, 'r') as f:
        history = json.load(f)
    
    print(f"\nIteration History from: {latest_history.name}")
    print("="*60)
    
    if history:
        last_entry = history[-1]
        print(f"Total iterations: {last_entry['iteration']}")
        print(f"Final max error: {last_entry['max_error']:.2f}%")
        print(f"Timestamp: {last_entry['timestamp']}")
        
        # Show convergence progress
        print("\nConvergence Progress:")
        print(f"{'Iteration':<10} {'Max Error (%)':<15} {'Analysis Time (s)':<15}")
        print("-"*40)
        for entry in history:
            print(f"{entry['iteration']:<10} {entry['max_error']:<15.2f} {entry.get('analysis_time', 0):<15.1f}")
    
    return 0


def validate_config(args):
    """Validate a configuration file"""
    config_path = Path(args.config)
    
    if not config_path.exists():
        print(f"Configuration file not found: {config_path}")
        return 1
    
    try:
        config = load_config(config_path)
        print(f"Configuration file is valid: {config_path}")
        
        # Check required sections
        required_sections = ['paths', 'target_files', 'models', 'iteration', 'convergence']
        missing = [s for s in required_sections if s not in config]
        
        if missing:
            print(f"Warning: Missing sections: {missing}")
        
        # Validate paths
        if validate_paths(config):
            print("All required paths exist")
        else:
            print("Some required paths are missing")
            return 1
        
        return 0
        
    except Exception as e:
        print(f"Configuration file is invalid: {e}")
        return 1


def main():
    """Main CLI entry point"""
    parser = argparse.ArgumentParser(
        description='Mooring Tension Iteration System - Automated OrcaFlex Analysis',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run iteration with default config
  %(prog)s run --config config.yaml
  
  # Run with custom parameters
  %(prog)s run --config config.yaml --max-iterations 20 --tolerance 0.005
  
  # Check status of previous run
  %(prog)s status --output-dir iteration_output
  
  # Validate configuration
  %(prog)s validate --config config.yaml
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    
    # Run command
    run_parser = subparsers.add_parser('run', help='Run mooring tension iteration')
    run_parser.add_argument('--config', '-c', required=True, help='Configuration file path')
    run_parser.add_argument('--target-csv', '-t', help='Override target CSV file name')
    run_parser.add_argument('--model', '-m', help='Override OrcaFlex model file')
    run_parser.add_argument('--max-iterations', '-i', type=int, help='Override maximum iterations')
    run_parser.add_argument('--tolerance', '-tol', type=float, help='Override convergence tolerance')
    run_parser.add_argument('--damping', '-d', type=float, help='Override damping factor')
    run_parser.set_defaults(func=run_iteration)
    
    # Status command
    status_parser = subparsers.add_parser('status', help='Check status of previous run')
    status_parser.add_argument('--output-dir', '-o', required=True, help='Output directory path')
    status_parser.set_defaults(func=check_status)
    
    # Validate command
    validate_parser = subparsers.add_parser('validate', help='Validate configuration file')
    validate_parser.add_argument('--config', '-c', required=True, help='Configuration file to validate')
    validate_parser.set_defaults(func=validate_config)
    
    # Parse arguments
    args = parser.parse_args()
    
    if args.command is None:
        parser.print_help()
        return 1
    
    # Execute command
    return args.func(args)


if __name__ == '__main__':
    sys.exit(main())