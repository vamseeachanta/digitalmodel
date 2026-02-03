"""
Command-line interface for Create Go-By Folder Tool
"""

import argparse
import sys
from pathlib import Path
from typing import Optional
import logging

from .orchestrator import CreateGoBy
from .validators import validate_arguments

# Set up logging
logger = logging.getLogger(__name__)


def create_parser() -> argparse.ArgumentParser:
    """Create and configure argument parser."""
    parser = argparse.ArgumentParser(
        prog='create-go-by',
        description='Create representative go-by folder from existing folder',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Basic usage
  create-go-by -s ./source -t ./target
  
  # With options
  create-go-by -s ./source -t ./target --overwrite --max-file-size 5KB
  
  # Resume from checkpoint
  create-go-by --resume -t ./target
  
  # Analysis mode for simulation folders
  create-go-by -s ./simulations -t ./template --analysis-mode
        """
    )
    
    # Required arguments
    parser.add_argument(
        '-s', '--source-folder',
        type=Path,
        help='Path to existing folder to convert'
    )
    parser.add_argument(
        '-t', '--target-folder',
        type=Path,
        help='Path for go-by folder output (required unless --show-learning-report)'
    )
    
    # Optional flags
    parser.add_argument(
        '--overwrite',
        action='store_true',
        help='Overwrite existing target folder (warns user first)'
    )
    parser.add_argument(
        '--max-file-size',
        type=str,
        default='10KB',
        help='Maximum size for representative files (e.g., 10KB, 1MB)'
    )
    parser.add_argument(
        '--variation-coverage',
        choices=['low', 'medium', 'high'],
        default='medium',
        help='How many variation representatives to keep'
    )
    parser.add_argument(
        '--analysis-mode',
        action='store_true',
        help='Enable analysis-specific features (parameter detection, workflow mapping)'
    )
    parser.add_argument(
        '--preserve-variations',
        action='store_true',
        default=True,
        help='Keep first/middle/last of sequences'
    )
    parser.add_argument(
        '--capture-workflow',
        action='store_true',
        help='Emit workflow structure metadata'
    )
    parser.add_argument(
        '--parameter-detection',
        choices=['auto', 'off'],
        default='auto',
        help='Detect naming/parameter patterns'
    )
    parser.add_argument(
        '--time-series-sampling',
        choices=['linear', 'logarithmic'],
        default='linear',
        help='Downsample long time series'
    )
    parser.add_argument(
        '--map-dependencies',
        action='store_true',
        help='Attempt dependency mapping between files'
    )
    parser.add_argument(
        '--generate-variation-script',
        action='store_true',
        help='Emit _generators/generate_variations.py'
    )
    parser.add_argument(
        '--exclude-patterns',
        type=str,
        help='Comma-separated glob patterns to exclude (e.g., "node_modules,*.bin")'
    )
    parser.add_argument(
        '--include-patterns',
        type=str,
        help='Comma-separated glob patterns to include (e.g., "*.yaml,*.config")'
    )
    parser.add_argument(
        '--preserve-recent',
        type=str,
        help='Prefer keeping newer examples (e.g., 30d)'
    )
    parser.add_argument(
        '--log-level',
        choices=['DEBUG', 'INFO', 'WARN', 'ERROR'],
        default='INFO',
        help='Logging level'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Report actions without writing files'
    )
    parser.add_argument(
        '--resume',
        action='store_true',
        help='Resume from checkpoint'
    )
    parser.add_argument(
        '--config',
        type=Path,
        help='YAML configuration file with parameters'
    )
    parser.add_argument(
        '--show-learning-report',
        action='store_true',
        help='Show learning system report and exit'
    )
    parser.add_argument(
        '--preserve-structure',
        action='store_true',
        default=True,
        help='Preserve exact folder structure (default: True)'
    )
    parser.add_argument(
        '--version',
        action='version',
        version='%(prog)s 0.1.0'
    )
    
    # Advanced features
    parser.add_argument(
        '--parallel',
        type=int,
        metavar='N',
        help='Enable parallel processing with N workers (0 for auto)'
    )
    
    parser.add_argument(
        '--checkpoint-interval',
        type=int,
        default=100,
        help='Auto-save checkpoint every N files (default: 100)'
    )
    
    parser.add_argument(
        '--no-checkpoint',
        action='store_true',
        help='Disable checkpoint/resume capability'
    )
    
    parser.add_argument(
        '--optimize-workers',
        action='store_true',
        help='Auto-optimize number of parallel workers'
    )
    
    parser.add_argument(
        '--yes-to-all', '-y',
        action='store_true',
        help='Automatically answer yes to all prompts'
    )
    
    parser.add_argument(
        '--no-interaction',
        action='store_true',
        help='Run in non-interactive mode (no prompts)'
    )
    
    parser.add_argument(
        '--no-progress-bar',
        action='store_true',
        help='Disable progress bar display'
    )
    
    parser.add_argument(
        '--monitor-memory',
        action='store_true',
        default=True,
        help='Monitor memory usage during processing'
    )
    
    return parser


def parse_size(size_str: str) -> int:
    """Parse human-readable size string to bytes."""
    size_str = str(size_str).upper().strip()
    
    multipliers = {
        'GB': 1024 * 1024 * 1024,
        'MB': 1024 * 1024,
        'KB': 1024,
        'B': 1
    }
    
    for suffix, multiplier in multipliers.items():
        if size_str.endswith(suffix):
            number_str = size_str[:-len(suffix)].strip()
            if number_str:  # Only process if there's a number
                try:
                    return int(float(number_str) * multiplier)
                except ValueError:
                    raise ValueError(f"Invalid size format: {size_str}")
    
    # If no suffix, assume bytes
    try:
        return int(float(size_str))
    except ValueError:
        # Default to 10KB if parsing fails
        logger.warning(f"Could not parse size '{size_str}', using default 10KB")
        return 10240


def load_config_file(config_path: Path) -> dict:
    """Load configuration from YAML file."""
    try:
        import yaml
        with open(config_path, 'r') as f:
            return yaml.safe_load(f) or {}
    except ImportError:
        logger.error("PyYAML not installed. Cannot load config file.")
        return {}
    except Exception as e:
        logger.error(f"Error loading config file: {e}")
        return {}


def main(argv: Optional[list] = None) -> int:
    """Main CLI entry point."""
    parser = create_parser()
    args = parser.parse_args(argv)
    
    # Configure logging
    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Show learning report if requested (before other validations)
    if getattr(args, 'show_learning_report', False):
        from .learning import GoByLearningSystem
        learning = GoByLearningSystem()
        print("\n" + learning.get_summary_report())
        return 0
    
    # Check required arguments
    if not args.target_folder:
        logger.error("Target folder is required")
        parser.print_help()
        return 1
    
    # Load config file if provided
    config = {}
    if args.config:
        config = load_config_file(args.config)
        logger.info(f"Loaded configuration from {args.config}")
    
    # Override config with command-line arguments
    for key, value in vars(args).items():
        if value is not None:
            config[key] = value
    
    # Handle resume mode
    if args.resume:
        if not args.target_folder.exists():
            logger.error("Cannot resume: target folder does not exist")
            return 1
        checkpoint_file = args.target_folder / '.go_by_checkpoint' / 'checkpoint.json'
        if not checkpoint_file.exists():
            logger.error("Cannot resume: no checkpoint found")
            return 1
        logger.info(f"Resuming from checkpoint at {checkpoint_file}")
    elif not args.source_folder:
        logger.error("Source folder is required unless resuming")
        parser.print_help()
        return 1
    
    # Parse exclude/include patterns
    if config.get('exclude_patterns'):
        config['exclude_patterns'] = [
            p.strip() for p in config['exclude_patterns'].split(',')
        ]
    if config.get('include_patterns'):
        config['include_patterns'] = [
            p.strip() for p in config['include_patterns'].split(',')
        ]
    
    # Parse max file size
    if 'max_file_size' in config:
        try:
            config['max_file_size'] = parse_size(config['max_file_size'])
        except ValueError as e:
            logger.error(str(e))
            return 1
    
    # Validate arguments
    try:
        validate_arguments(config)
    except ValueError as e:
        logger.error(f"Validation error: {e}")
        return 1
    
    # Dry run notification
    if config.get('dry_run'):
        logger.info("DRY RUN MODE: No files will be written")
    
    # Create and execute
    try:
        goby = CreateGoBy(
            source=config.get('source_folder'),
            target=config['target_folder'],
            config=config
        )
        
        success = goby.execute()
        return 0 if success else 1
        
    except KeyboardInterrupt:
        logger.info("\nOperation cancelled by user")
        return 130
    except Exception as e:
        logger.error(f"Unexpected error: {e}", exc_info=True)
        return 1


if __name__ == "__main__":
    sys.exit(main())