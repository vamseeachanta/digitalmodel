#!/usr/bin/env python
"""
Slash command: /orcaflex-sim
Universal command for OrcaFlex model processing and simulation generation.

This command provides a simple interface to run OrcaFlex models and generate
simulation files (.sim) from YAML configurations.

Usage Examples:
    /orcaflex-sim                           # Show help and examples
    /orcaflex-sim --all                     # Run all models in current directory
    /orcaflex-sim --models model1.yml       # Run specific model
    /orcaflex-sim --pattern "fsts_*.yml"    # Run models matching pattern
    /orcaflex-sim --mock --all              # Test without OrcaFlex license
    /orcaflex-sim --batch config.yml        # Run batch configuration
"""

import os
import sys
import subprocess
import argparse
import json
import yaml
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Optional, Any
import concurrent.futures
import shutil

# Add src to path for imports
repo_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(repo_root / "src"))


class OrcaFlexSimCommand:
    """Enhanced OrcaFlex simulation command with additional features."""
    
    def __init__(self):
        """Initialize the command."""
        self.repo_root = repo_root
        self.module_path = self.repo_root / "src" / "digitalmodel" / "modules" / "orcaflex"
        self.cli_script = self.module_path / "run_to_sim_cli.py"
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
    def show_examples(self):
        """Show usage examples."""
        examples = """
==================================================================
                  OrcaFlex Simulation Command                     
==================================================================

COMMON USAGE PATTERNS:

1. Run all models in current directory:
   /orcaflex-sim --all

2. Run specific models:
   /orcaflex-sim --models model1.yml model2.yml

3. Run models matching a pattern:
   /orcaflex-sim --pattern "fsts_*.yml" --all

4. Run with custom output directory:
   /orcaflex-sim --all --output ./simulations

5. Test without OrcaFlex license (mock mode):
   /orcaflex-sim --mock --all

6. Run with specific thread count:
   /orcaflex-sim --all --threads 10

7. Run from batch configuration:
   /orcaflex-sim --batch batch_config.yml

8. Quick test (first 3 models only):
   /orcaflex-sim --test

BATCH CONFIGURATION EXAMPLE:
Create a batch_config.yml file:
```yaml
models:
  directory: ./models
  pattern: "fsts_*.yml"
  output: ./simulations
settings:
  threads: 20
  mock: false
  verbose: true
```

Then run: /orcaflex-sim --batch batch_config.yml

ENVIRONMENT SETUP:
- Set DIGITALMODEL_ROOT to repository path if running from outside
- Ensure Python environment has required packages
- OrcaFlex license needed for real runs (use --mock for testing)
        """
        print(examples)
        
    def run_batch_config(self, config_file: Path) -> int:
        """Run models based on batch configuration file."""
        try:
            with open(config_file, 'r') as f:
                config = yaml.safe_load(f)
            
            # Extract configuration
            models_config = config.get('models', {})
            settings = config.get('settings', {})
            
            # Build command arguments
            args = []
            
            if models_config.get('directory'):
                args.extend(['--directory', models_config['directory']])
            
            if models_config.get('pattern'):
                args.extend(['--pattern', models_config['pattern']])
            
            if models_config.get('output'):
                args.extend(['--output', models_config['output']])
            
            if models_config.get('files'):
                args.extend(['--models'] + models_config['files'])
            else:
                args.append('--all')
            
            if settings.get('threads'):
                args.extend(['--threads', str(settings['threads'])])
            
            if settings.get('mock'):
                args.append('--mock')
            
            if settings.get('verbose'):
                args.append('--verbose')
            
            return self.run_cli(args)
            
        except Exception as e:
            print(f"Error reading batch configuration: {e}")
            return 1
    
    def run_cli(self, args: List[str]) -> int:
        """Run the CLI script with given arguments."""
        if not self.cli_script.exists():
            print(f"Error: CLI script not found at {self.cli_script}")
            return 1
        
        cmd = [sys.executable, str(self.cli_script)] + args
        
        print(f"Running: {' '.join(cmd)}")
        print(f"Working directory: {Path.cwd()}")
        
        try:
            result = subprocess.run(cmd, capture_output=False, text=True)
            return result.returncode
        except Exception as e:
            print(f"Error running command: {e}")
            return 1
    
    def create_batch_template(self, output_file: str = "batch_config.yml"):
        """Create a template batch configuration file."""
        template = {
            'models': {
                'directory': './models',
                'pattern': '*.yml',
                'output': './simulations',
                'files': [
                    '# Uncomment and list specific files if not using pattern',
                    '# - model1.yml',
                    '# - model2.yml'
                ]
            },
            'settings': {
                'threads': 30,
                'mock': False,
                'verbose': True
            },
            'description': 'OrcaFlex batch simulation configuration'
        }
        
        with open(output_file, 'w') as f:
            yaml.dump(template, f, default_flow_style=False, sort_keys=False)
        
        print(f"Created batch configuration template: {output_file}")
        print("Edit this file and run: /orcaflex-sim --batch batch_config.yml")
        
    def run(self, argv: List[str] = None) -> int:
        """Main command execution."""
        parser = argparse.ArgumentParser(
            prog='/orcaflex-sim',
            description='Universal OrcaFlex simulation command',
            add_help=False  # We'll handle help ourselves
        )
        
        # Custom options for this wrapper
        parser.add_argument('--help', '-h', action='store_true', help='Show help and examples')
        parser.add_argument('--batch', help='Run from batch configuration file')
        parser.add_argument('--create-template', action='store_true', 
                          help='Create batch configuration template')
        parser.add_argument('--test', action='store_true',
                          help='Quick test mode (first 3 models)')
        
        # Pass-through options for CLI
        parser.add_argument('--models', nargs='+', help='Specific model files')
        parser.add_argument('--all', action='store_true', help='Run all models')
        parser.add_argument('--pattern', help='File pattern to match')
        parser.add_argument('--directory', help='Directory to search')
        parser.add_argument('--output', help='Output directory')
        parser.add_argument('--threads', type=int, help='Number of threads')
        parser.add_argument('--mock', action='store_true', help='Mock mode (no license)')
        parser.add_argument('--verbose', action='store_true', help='Verbose output')
        
        # Parse arguments
        args, unknown = parser.parse_known_args(argv)
        
        # Handle special cases
        if args.help or (not any(vars(args).values()) and not unknown):
            self.show_examples()
            return 0
        
        if args.create_template:
            self.create_batch_template()
            return 0
        
        if args.batch:
            return self.run_batch_config(Path(args.batch))
        
        # Build CLI arguments
        cli_args = []
        
        if args.test:
            # Quick test mode - don't add --all
            pass
        elif args.all:
            cli_args.append('--all')
        
        if args.models:
            cli_args.extend(['--models'] + args.models)
        
        if args.pattern:
            cli_args.extend(['--pattern', args.pattern])
        
        if args.directory:
            cli_args.extend(['--directory', args.directory])
        
        if args.output:
            cli_args.extend(['--output', args.output])
        
        if args.threads:
            cli_args.extend(['--threads', str(args.threads)])
        
        if args.mock:
            cli_args.append('--mock')
        
        if args.verbose:
            cli_args.append('--verbose')
        
        # Add any unknown arguments
        cli_args.extend(unknown)
        
        return self.run_cli(cli_args)


def main(argv: List[str] = None) -> int:
    """Main entry point."""
    command = OrcaFlexSimCommand()
    return command.run(argv)


if __name__ == "__main__":
    sys.exit(main())