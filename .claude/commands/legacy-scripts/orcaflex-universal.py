#!/usr/bin/env python
"""
Slash Command: /orcaflex-universal
Universal OrcaFlex simulation runner accessible from anywhere.

Usage:
    /orcaflex-universal --all                    # Process all models
    /orcaflex-universal --pattern "*.yml"        # Pattern matching
    /orcaflex-universal --mock --test            # Test mode
    /orcaflex-universal --config batch.yml       # Use config file
"""

import os
import sys
import subprocess
from pathlib import Path
import argparse
import json

def find_digitalmodel_root():
    """Find the digitalmodel repository root."""
    # Try multiple strategies
    current = Path.cwd()
    
    # Strategy 1: Search upward
    search_path = current
    while search_path != search_path.parent:
        if (search_path / "src" / "digitalmodel").exists():
            return search_path
        search_path = search_path.parent
    
    # Strategy 2: Common locations
    common_paths = [
        Path("D:/github/digitalmodel"),
        Path.home() / "github" / "digitalmodel",
        Path("/mnt/github/digitalmodel"),
    ]
    
    for path in common_paths:
        if path.exists() and (path / "src" / "digitalmodel").exists():
            return path
    
    # Strategy 3: Environment variable
    if "DIGITALMODEL_ROOT" in os.environ:
        return Path(os.environ["DIGITALMODEL_ROOT"])
    
    raise RuntimeError("Could not find digitalmodel repository")


def main():
    """Main entry point for slash command."""
    parser = argparse.ArgumentParser(
        prog='/orcaflex-universal',
        description='Universal OrcaFlex simulation runner',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  /orcaflex-universal --all                         # Process all models
  /orcaflex-universal --pattern "fsts_*.yml" --all  # Pattern matching
  /orcaflex-universal --models file1.yml file2.yml  # Specific files
  /orcaflex-universal --mock --test                 # Test mode (3 files, no license)
  /orcaflex-universal --config batch.yml            # Use configuration file
  /orcaflex-universal --create-config               # Create config template
        """
    )
    
    # Arguments
    parser.add_argument('--pattern', '-p', default='*.yml',
                       help='File pattern to match')
    parser.add_argument('--input-dir', '-i',
                       help='Input directory')
    parser.add_argument('--output-dir', '-o',
                       help='Output directory')
    parser.add_argument('--models', '-m', nargs='+',
                       help='Specific model files')
    parser.add_argument('--config', '-c',
                       help='Configuration file')
    parser.add_argument('--all', action='store_true',
                       help='Process all matching files')
    parser.add_argument('--test', action='store_true',
                       help='Test mode (first 3 files)')
    parser.add_argument('--mock', action='store_true',
                       help='Mock mode (no license)')
    parser.add_argument('--recursive', '-r', action='store_true',
                       help='Recursive search')
    parser.add_argument('--workers', '-w', type=int, default=30,
                       help='Max parallel workers')
    parser.add_argument('--exclude', '-e', action='append',
                       help='Exclude patterns')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')
    parser.add_argument('--report', help='Save JSON report')
    parser.add_argument('--create-config', action='store_true',
                       help='Create config template')
    
    args = parser.parse_args()
    
    try:
        # Find repository root
        repo_root = find_digitalmodel_root()
        cli_script = repo_root / "src" / "digitalmodel" / "modules" / "orcaflex" / "universal_cli.py"
        
        if not cli_script.exists():
            print(f"Error: CLI script not found at {cli_script}")
            return 1
        
        # Build command
        cmd = [sys.executable, str(cli_script)]
        
        # Handle create-config separately
        if args.create_config:
            cmd.extend(['create-config'])
            if args.output_dir:
                cmd.extend(['--output', args.output_dir])
        else:
            # Add arguments
            if args.pattern:
                cmd.extend(['--pattern', args.pattern])
            if args.input_dir:
                cmd.extend(['--input-dir', args.input_dir])
            if args.output_dir:
                cmd.extend(['--output-dir', args.output_dir])
            if args.models:
                for model in args.models:
                    cmd.extend(['--models', model])
            if args.config:
                cmd.extend(['--config', args.config])
            if args.all:
                cmd.append('--all')
            if args.test:
                cmd.append('--test')
            if args.mock:
                cmd.append('--mock')
            if args.recursive:
                cmd.append('--recursive')
            if args.workers != 30:
                cmd.extend(['--workers', str(args.workers)])
            if args.exclude:
                for pattern in args.exclude:
                    cmd.extend(['--exclude', pattern])
            if args.verbose:
                cmd.append('--verbose')
            if args.report:
                cmd.extend(['--report', args.report])
        
        # Show command being run
        print(f"Repository: {repo_root}")
        print(f"Running: {' '.join(cmd)}\n")
        
        # Execute
        result = subprocess.run(cmd, cwd=Path.cwd())
        return result.returncode
        
    except Exception as e:
        print(f"Error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())