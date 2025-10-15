#!/usr/bin/env python
"""Add configuration files and helper scripts to OrcaFlex test folders."""

import os
import shutil
from pathlib import Path
import yaml
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def create_batch_config(folder: Path, test_name: str):
    """Create a batch configuration file for the test folder."""
    scripts_dir = folder / 'scripts'
    scripts_dir.mkdir(exist_ok=True)
    
    config = {
        'test_name': test_name,
        'description': f'Batch configuration for {test_name} tests',
        'model_files': {
            'directory': '../',
            'pattern': '*.dat',
            'recursive': False
        },
        'output_settings': {
            'base_directory': '../output',
            'csv_directory': '../output/.csv',
            'sim_directory': '../.sim',
            'save_sim': True,
            'save_csv': True
        },
        'analysis_settings': {
            'analysis_type': 'static',
            'parallel_workers': 4,
            'timeout': 3600,
            'retry_on_failure': True,
            'max_retries': 2
        },
        'logging': {
            'level': 'INFO',
            'log_directory': 'logs',
            'log_file': f'{test_name}_analysis.log'
        }
    }
    
    config_path = scripts_dir / f'{test_name}_config.yml'
    if not config_path.exists():
        with open(config_path, 'w', encoding='utf-8') as f:
            yaml.dump(config, f, default_flow_style=False, sort_keys=False)
        logger.info(f"Created config: {config_path}")
    return config_path

def create_run_script(folder: Path, test_name: str):
    """Create a run script for the test folder."""
    scripts_dir = folder / 'scripts'
    scripts_dir.mkdir(exist_ok=True)
    
    script_content = f'''#!/usr/bin/env python
"""Run {test_name} OrcaFlex analysis."""

import sys
import os
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parents[5]
sys.path.insert(0, str(project_root))

def run_analysis():
    """Run the OrcaFlex analysis for {test_name}."""
    from digitalmodel.modules.orcaflex.universal import UniversalOrcaFlexRunner
    
    current_dir = Path(__file__).parent.parent
    config_file = Path(__file__).parent / '{test_name}_config.yml'
    
    runner = UniversalOrcaFlexRunner()
    result = runner.run(
        input_directory=str(current_dir),
        config=str(config_file) if config_file.exists() else None,
        pattern='*.dat',
        parallel=4
    )
    
    return 0 if result else 1

if __name__ == '__main__':
    sys.exit(run_analysis())
'''
    
    script_path = scripts_dir / f'run_{test_name}.py'
    if not script_path.exists():
        with open(script_path, 'w', encoding='utf-8') as f:
            f.write(script_content)
        logger.info(f"Created run script: {script_path}")
    return script_path

def create_batch_script(folder: Path, test_name: str):
    """Create a batch/shell script for the test folder."""
    scripts_dir = folder / 'scripts'
    scripts_dir.mkdir(exist_ok=True)
    
    # Windows batch script
    batch_content = f'''@echo off
REM Run {test_name} OrcaFlex analysis

echo Running {test_name} analysis...
cd /d "%~dp0\\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/{test_name}_config.yml ^
    --parallel 4

echo Analysis complete!
pause
'''
    
    batch_path = scripts_dir / f'run_{test_name}.bat'
    if not batch_path.exists():
        with open(batch_path, 'w', encoding='utf-8') as f:
            f.write(batch_content)
        logger.info(f"Created batch script: {batch_path}")
    
    # Unix shell script
    shell_content = f'''#!/bin/bash
# Run {test_name} OrcaFlex analysis

echo "Running {test_name} analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \\
    --input-directory . \\
    --config scripts/{test_name}_config.yml \\
    --parallel 4

echo "Analysis complete!"
'''
    
    shell_path = scripts_dir / f'run_{test_name}.sh'
    if not shell_path.exists():
        with open(shell_path, 'w', encoding='utf-8') as f:
            f.write(shell_content)
        # Make executable
        os.chmod(shell_path, 0o755)
        logger.info(f"Created shell script: {shell_path}")

def process_test_folder(folder: Path):
    """Process a single test folder."""
    # Extract test name from folder
    test_name = folder.name.replace('-', '_').replace(' ', '_')
    
    logger.info(f"Processing: {folder.name}")
    
    # Create configurations and scripts
    create_batch_config(folder, test_name)
    create_run_script(folder, test_name)
    create_batch_script(folder, test_name)
    
    # Create logs directory
    logs_dir = folder / 'scripts' / 'logs'
    logs_dir.mkdir(exist_ok=True)
    
    # Create results directory
    results_dir = folder / 'scripts' / 'results'
    results_dir.mkdir(exist_ok=True)

def main():
    """Main execution."""
    base_path = Path('D:/github/digitalmodel')
    orcaflex_tests = base_path / 'tests' / 'modules' / 'orcaflex'
    
    # List of test folders to process
    test_folders = [
        'analysis',
        'batch_processing',
        'browser_interface',
        'core',
        'file_preparation',
        'mooring_tension_iteration',
        'orcaflex_analysis',
        'orcaflex_file_preparation',
        'orcaflex_post_process',
        'post_processing',
        'universal',
        'unresolved'
    ]
    
    for folder_name in test_folders:
        folder = orcaflex_tests / folder_name
        if folder.exists() and folder.is_dir():
            try:
                process_test_folder(folder)
                logger.info(f"✓ Processed {folder_name}")
            except Exception as e:
                logger.error(f"✗ Error processing {folder_name}: {e}")
    
    logger.info("\nConfiguration files added successfully!")

if __name__ == '__main__':
    main()