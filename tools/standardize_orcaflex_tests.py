#!/usr/bin/env python
"""
Standardize OrcaFlex test folder structure based on fsts-l015-test-cases example.

Standard structure:
test-folder/
├── scripts/          # Configuration files and scripts
│   ├── *.yml        # OrcaFlex analysis configurations
│   ├── *.csv        # Input data files
│   ├── *.py         # Helper scripts
│   └── logs/        # Execution logs
├── output/          # Analysis output files
│   ├── .csv/        # CSV output files
│   ├── collate/     # Collated results
│   ├── plots/       # Generated plots
│   ├── report/      # Analysis reports
│   └── visual/      # Visualization outputs
├── .dat/            # OrcaFlex data files
├── .sim/            # OrcaFlex simulation files
└── README.md        # Documentation
"""

import os
import shutil
from pathlib import Path
import yaml
import logging
from typing import List, Dict, Any

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class OrcaFlexTestStandardizer:
    """Standardize OrcaFlex test folder structure."""
    
    # Standard folder structure
    STANDARD_DIRS = {
        'scripts': ['logs', 'results'],
        'output': ['.csv', 'collate', 'plots', 'report', 'visual'],
        '.dat': [],
        '.sim': []
    }
    
    # File extension to folder mapping
    FILE_MAPPING = {
        '.yml': 'scripts',
        '.yaml': 'scripts',
        '.csv': 'scripts',  # Input CSV files go to scripts
        '.sh': 'scripts',
        '.bat': 'scripts',
        '.ps1': 'scripts',
        '.dat': '.dat',
        '.sim': '.sim',
        '.log': 'scripts/logs',
        '.xlsx': 'output/collate',
        '.png': 'output/plots',
        '.jpg': 'output/visual',
        '.jpeg': 'output/visual',
        '.md': '.'  # Keep markdown in root
    }
    
    def __init__(self, base_path: Path):
        """Initialize standardizer with base path."""
        self.base_path = Path(base_path)
        self.test_folders = self._find_test_folders()
        
    def _find_test_folders(self) -> List[Path]:
        """Find all OrcaFlex test folders that need standardization."""
        test_folders = []
        orcaflex_path = self.base_path / 'tests' / 'modules' / 'orcaflex'
        
        # Skip the standard example folder
        skip_folders = ['fsts-l015-test-cases', '__pycache__', '.pytest_cache']
        
        for folder in orcaflex_path.iterdir():
            if folder.is_dir() and folder.name not in skip_folders:
                test_folders.append(folder)
                # Also check for subdirectories that might be test cases
                for subfolder in folder.iterdir():
                    if subfolder.is_dir() and subfolder.name not in skip_folders:
                        # Check if it contains test data
                        if any(subfolder.glob('*.yml')) or any(subfolder.glob('*.dat')) or any(subfolder.glob('*.sim')):
                            test_folders.append(subfolder)
        
        return sorted(test_folders)
    
    def create_standard_structure(self, folder: Path):
        """Create standard directory structure in the given folder."""
        logger.info(f"Creating standard structure in: {folder}")
        
        for main_dir, subdirs in self.STANDARD_DIRS.items():
            main_path = folder / main_dir
            main_path.mkdir(exist_ok=True)
            
            for subdir in subdirs:
                sub_path = main_path / subdir
                sub_path.mkdir(exist_ok=True)
    
    def organize_files(self, folder: Path):
        """Organize existing files into standard structure."""
        logger.info(f"Organizing files in: {folder}")
        
        # Get all files in the folder (not in subdirectories yet)
        files_to_move = []
        for item in folder.iterdir():
            if item.is_file():
                # Skip test files - they should stay in the root
                if item.name.startswith('test_') or item.name.endswith('_test.py'):
                    continue
                files_to_move.append(item)
        
        # Move files to appropriate directories
        for file_path in files_to_move:
            ext = file_path.suffix.lower()
            
            # Special handling for output CSV files
            if ext == '.csv' and any(keyword in file_path.name.lower() for keyword in ['output', 'result', '_var_data']):
                target_dir = folder / 'output' / '.csv'
            else:
                target_dir = folder / self.FILE_MAPPING.get(ext, '.')
            
            if target_dir != folder / '.':
                target_dir = Path(str(target_dir).replace('/.', ''))
                target_dir.mkdir(parents=True, exist_ok=True)
                
                target_path = target_dir / file_path.name
                if not target_path.exists():
                    logger.info(f"Moving {file_path.name} to {target_dir}")
                    shutil.move(str(file_path), str(target_path))
    
    def create_readme(self, folder: Path):
        """Create or update README.md for the test folder."""
        readme_path = folder / 'README.md'
        
        if not readme_path.exists():
            folder_name = folder.name
            parent_name = folder.parent.name if folder.parent.name != 'orcaflex' else ''
            
            content = f"""# {folder_name.replace('-', ' ').replace('_', ' ').title()} Test Cases

This directory contains test cases for {parent_name + ' ' if parent_name else ''}{folder_name}.

## Directory Structure

```
{folder_name}/
├── scripts/              # Test configuration files and scripts
│   ├── *.yml            # OrcaFlex analysis configurations
│   ├── *.csv            # Input data files
│   ├── *.py             # Helper scripts
│   └── logs/            # Execution logs
├── output/              # Test output files
│   ├── .csv/            # CSV output files
│   ├── collate/         # Collated results
│   ├── plots/           # Generated plots
│   ├── report/          # Analysis reports
│   └── visual/          # Visualization outputs
├── .dat/                # OrcaFlex data files
├── .sim/                # OrcaFlex simulation files
└── README.md            # This file
```

## Running Tests

### Using the Universal OrcaFlex Runner
```bash
# Run all models in this directory
python -m digitalmodel.modules.orcaflex.universal --input-directory {folder.relative_to(self.base_path)}

# Run with specific pattern
python -m digitalmodel.modules.orcaflex.universal --pattern "*.yml" --input-directory {folder.relative_to(self.base_path)}
```

### Using Configuration Files
```bash
# Run with specific configuration
python -m digitalmodel.modules.orcaflex.mooring --config {folder.relative_to(self.base_path)}/scripts/<config_file>.yml
```

## Test Files

### Configuration Files
- Located in `scripts/` directory
- YAML format for OrcaFlex analysis configurations
- CSV files for input data (pretension values, target forces, etc.)

### Output Files
- Results stored in `output/` directory structure
- CSV outputs in `output/.csv/`
- Visualizations in `output/visual/` and `output/plots/`
- Reports in `output/report/`

## Notes

This test directory follows the standardized OrcaFlex test structure.
"""
            
            with open(readme_path, 'w', encoding='utf-8') as f:
                f.write(content)
            logger.info(f"Created README.md for {folder_name}")
    
    def create_standard_config(self, folder: Path):
        """Create a standard configuration file template if none exists."""
        scripts_dir = folder / 'scripts'
        scripts_dir.mkdir(exist_ok=True)
        
        # Check if any config files exist
        config_files = list(scripts_dir.glob('*.yml')) + list(scripts_dir.glob('*.yaml'))
        
        if not config_files:
            # Create a template config
            config_template = {
                'model_type': 'orcaflex',
                'analysis_type': 'static',
                'input_files': {
                    'pattern': '*.dat',
                    'directory': '../.dat'
                },
                'output_settings': {
                    'directory': '../output/.csv',
                    'save_sim': True,
                    'sim_directory': '../.sim'
                },
                'analysis_settings': {
                    'parallel_workers': 4,
                    'timeout': 3600,
                    'retry_failed': True
                },
                'logging': {
                    'level': 'INFO',
                    'log_file': 'logs/analysis.log'
                }
            }
            
            config_path = scripts_dir / 'analysis_config.yml'
            with open(config_path, 'w', encoding='utf-8') as f:
                yaml.dump(config_template, f, default_flow_style=False, sort_keys=False)
            logger.info(f"Created template config: {config_path}")
    
    def create_helper_scripts(self, folder: Path):
        """Create standard helper scripts."""
        scripts_dir = folder / 'scripts'
        scripts_dir.mkdir(exist_ok=True)
        
        # Create run_analysis.py
        run_script = scripts_dir / 'run_analysis.py'
        if not run_script.exists():
            content = '''#!/usr/bin/env python
"""Run OrcaFlex analysis for this test folder."""

import sys
from pathlib import Path
import subprocess

def main():
    """Run the analysis using the universal OrcaFlex runner."""
    current_dir = Path(__file__).parent.parent
    
    # Run universal runner with this directory
    cmd = [
        sys.executable, '-m', 'digitalmodel.modules.orcaflex.universal',
        '--input-directory', str(current_dir),
        '--pattern', '*.yml',
        '--parallel', '4'
    ]
    
    print(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd)
    return result.returncode

if __name__ == '__main__':
    sys.exit(main())
'''
            with open(run_script, 'w', encoding='utf-8') as f:
                f.write(content)
            logger.info(f"Created run_analysis.py in {folder.name}")
    
    def clean_folder(self, folder: Path):
        """Remove unnecessary files and empty directories."""
        logger.info(f"Cleaning up {folder}")
        
        # Remove backup files
        for pattern in ['*.backup', '*.bak', '*.tmp', '*~', '*.swp']:
            for file in folder.rglob(pattern):
                logger.info(f"Removing backup file: {file}")
                file.unlink()
        
        # Remove empty directories
        for dirpath, dirnames, filenames in os.walk(folder, topdown=False):
            if not dirnames and not filenames:
                Path(dirpath).rmdir()
                logger.info(f"Removed empty directory: {dirpath}")
    
    def standardize_all(self):
        """Standardize all identified test folders."""
        logger.info(f"Found {len(self.test_folders)} test folders to standardize")
        
        for folder in self.test_folders:
            logger.info(f"\n{'='*60}")
            logger.info(f"Standardizing: {folder.relative_to(self.base_path)}")
            logger.info(f"{'='*60}")
            
            try:
                # Create standard structure
                self.create_standard_structure(folder)
                
                # Organize existing files
                self.organize_files(folder)
                
                # Create/update README
                self.create_readme(folder)
                
                # Create standard config if needed
                self.create_standard_config(folder)
                
                # Create helper scripts
                self.create_helper_scripts(folder)
                
                # Clean up
                self.clean_folder(folder)
                
                logger.info(f"Successfully standardized {folder.name}")
                
            except Exception as e:
                logger.error(f"Error standardizing {folder.name}: {e}")
    
    def generate_report(self):
        """Generate a standardization report."""
        report_path = self.base_path / 'tests' / 'modules' / 'orcaflex' / 'STANDARDIZATION_REPORT.md'
        
        content = """# OrcaFlex Test Standardization Report

## Summary

Standardized folder structure for all OrcaFlex test directories based on the fsts-l015-test-cases example.

## Standard Structure

```
test-folder/
├── scripts/          # Configuration files and scripts
│   ├── *.yml        # OrcaFlex analysis configurations
│   ├── *.csv        # Input data files
│   ├── *.py         # Helper scripts
│   └── logs/        # Execution logs
├── output/          # Analysis output files
│   ├── .csv/        # CSV output files
│   ├── collate/     # Collated results
│   ├── plots/       # Generated plots
│   ├── report/      # Analysis reports
│   └── visual/      # Visualization outputs
├── .dat/            # OrcaFlex data files
├── .sim/            # OrcaFlex simulation files
└── README.md        # Documentation
```

## Standardized Folders

"""
        
        for folder in self.test_folders:
            rel_path = folder.relative_to(self.base_path)
            content += f"- `{rel_path}`\n"
        
        content += f"""

## Total Folders Standardized: {len(self.test_folders)}

## Next Steps

1. Review the standardized structure in each folder
2. Run tests to ensure functionality is preserved
3. Update any broken references in test files
4. Commit the standardized structure
"""
        
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        logger.info(f"Generated report: {report_path}")


def main():
    """Main execution function."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Standardize OrcaFlex test folder structure')
    parser.add_argument('--base-path', default='D:/github/digitalmodel', help='Base path of the repository')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done without making changes')
    
    args = parser.parse_args()
    
    standardizer = OrcaFlexTestStandardizer(Path(args.base_path))
    
    if args.dry_run:
        logger.info("DRY RUN MODE - No changes will be made")
        logger.info(f"Would standardize {len(standardizer.test_folders)} folders:")
        for folder in standardizer.test_folders:
            logger.info(f"  - {folder.relative_to(standardizer.base_path)}")
    else:
        standardizer.standardize_all()
        standardizer.generate_report()
        logger.info("\nStandardization complete!")


if __name__ == '__main__':
    main()