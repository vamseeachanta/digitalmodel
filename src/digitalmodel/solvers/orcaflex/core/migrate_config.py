"""
Configuration Migration Utility for OrcaFlex Module

This script helps migrate legacy configuration files to the new unified format.
It can process single files or entire directories of configuration files.
"""

import argparse
import json
import yaml
from pathlib import Path
from typing import Dict, Any, List, Optional
import logging
from datetime import datetime

from .configuration import ConfigurationManager, OrcaFlexConfig
from .exceptions import ConfigurationError


class ConfigurationMigrator:
    """
    Utility class for migrating legacy OrcaFlex configurations.
    
    This class provides methods to:
    - Detect legacy configuration formats
    - Convert legacy formats to the new unified schema
    - Validate converted configurations
    - Generate migration reports
    """
    
    def __init__(self, verbose: bool = False):
        """
        Initialize the configuration migrator.
        
        Args:
            verbose: Enable verbose logging
        """
        self.manager = ConfigurationManager()
        self.verbose = verbose
        self.migration_report = []
        
        # Set up logging
        logging.basicConfig(
            level=logging.DEBUG if verbose else logging.INFO,
            format='[%(levelname)s] %(message)s'
        )
        self.logger = logging.getLogger(__name__)
    
    def migrate_file(self, 
                     input_path: Path,
                     output_path: Optional[Path] = None,
                     backup: bool = True) -> bool:
        """
        Migrate a single configuration file.
        
        Args:
            input_path: Path to legacy configuration file
            output_path: Path for migrated configuration (optional)
            backup: Create backup of original file
            
        Returns:
            True if migration successful, False otherwise
        """
        input_path = Path(input_path)
        
        if not input_path.exists():
            self.logger.error(f"File not found: {input_path}")
            return False
        
        self.logger.info(f"Migrating: {input_path}")
        
        try:
            # Load legacy configuration
            legacy_config = self._load_legacy_file(input_path)
            
            # Check if already in new format
            if not self.manager._is_legacy_format(legacy_config):
                self.logger.info(f"Already in new format: {input_path}")
                self.migration_report.append({
                    'file': str(input_path),
                    'status': 'skipped',
                    'reason': 'Already in new format'
                })
                return True
            
            # Create backup if requested
            if backup:
                backup_path = input_path.with_suffix(f'.backup_{datetime.now().strftime("%Y%m%d_%H%M%S")}')
                input_path.rename(backup_path)
                self.logger.info(f"Created backup: {backup_path}")
                
                # Copy backup back to original for conversion
                with open(backup_path, 'r') as f:
                    legacy_config = yaml.safe_load(f) if backup_path.suffix in ['.yaml', '.yml'] else json.load(f)
            
            # Convert to new format
            new_config = self.manager.load(config_dict=legacy_config)
            
            # Validate converted configuration
            warnings = new_config.validate_compatibility()
            if warnings:
                self.logger.warning(f"Validation warnings: {warnings}")
            
            # Determine output path
            if output_path is None:
                output_path = input_path
            else:
                output_path = Path(output_path)
            
            # Save migrated configuration
            new_config.to_yaml(output_path)
            self.logger.info(f"Saved migrated configuration: {output_path}")
            
            # Add to report
            self.migration_report.append({
                'file': str(input_path),
                'status': 'success',
                'output': str(output_path),
                'warnings': warnings
            })
            
            return True
            
        except Exception as e:
            self.logger.error(f"Migration failed: {e}")
            self.migration_report.append({
                'file': str(input_path),
                'status': 'failed',
                'error': str(e)
            })
            return False
    
    def migrate_directory(self,
                         directory: Path,
                         pattern: str = "*.yaml",
                         recursive: bool = True,
                         backup: bool = True) -> int:
        """
        Migrate all configuration files in a directory.
        
        Args:
            directory: Directory containing configuration files
            pattern: File pattern to match
            recursive: Search recursively
            backup: Create backups of original files
            
        Returns:
            Number of successfully migrated files
        """
        directory = Path(directory)
        
        if not directory.exists():
            self.logger.error(f"Directory not found: {directory}")
            return 0
        
        # Find configuration files
        if recursive:
            files = list(directory.rglob(pattern))
        else:
            files = list(directory.glob(pattern))
        
        # Also check for .yml files
        if pattern == "*.yaml":
            if recursive:
                files.extend(list(directory.rglob("*.yml")))
            else:
                files.extend(list(directory.glob("*.yml")))
        
        self.logger.info(f"Found {len(files)} configuration files")
        
        success_count = 0
        for file_path in files:
            if self.migrate_file(file_path, backup=backup):
                success_count += 1
        
        return success_count
    
    def _load_legacy_file(self, file_path: Path) -> Dict[str, Any]:
        """
        Load a legacy configuration file.
        
        Args:
            file_path: Path to configuration file
            
        Returns:
            Configuration dictionary
        """
        with open(file_path, 'r') as f:
            if file_path.suffix in ['.yaml', '.yml']:
                return yaml.safe_load(f)
            elif file_path.suffix == '.json':
                return json.load(f)
            else:
                # Try YAML first, then JSON
                content = f.read()
                try:
                    return yaml.safe_load(content)
                except:
                    return json.loads(content)
    
    def generate_report(self, output_path: Optional[Path] = None) -> str:
        """
        Generate a migration report.
        
        Args:
            output_path: Optional path to save report
            
        Returns:
            Report content as string
        """
        report_lines = [
            "=" * 60,
            "OrcaFlex Configuration Migration Report",
            f"Generated: {datetime.now().isoformat()}",
            "=" * 60,
            ""
        ]
        
        # Summary statistics
        total = len(self.migration_report)
        success = sum(1 for r in self.migration_report if r['status'] == 'success')
        skipped = sum(1 for r in self.migration_report if r['status'] == 'skipped')
        failed = sum(1 for r in self.migration_report if r['status'] == 'failed')
        
        report_lines.extend([
            "Summary:",
            f"  Total files processed: {total}",
            f"  Successfully migrated: {success}",
            f"  Skipped (already migrated): {skipped}",
            f"  Failed: {failed}",
            ""
        ])
        
        # Detailed results
        if self.migration_report:
            report_lines.append("Detailed Results:")
            report_lines.append("-" * 40)
            
            for item in self.migration_report:
                report_lines.append(f"\nFile: {item['file']}")
                report_lines.append(f"Status: {item['status'].upper()}")
                
                if item['status'] == 'success':
                    report_lines.append(f"Output: {item['output']}")
                    if item.get('warnings'):
                        report_lines.append("Warnings:")
                        for warning in item['warnings']:
                            report_lines.append(f"  - {warning}")
                elif item['status'] == 'failed':
                    report_lines.append(f"Error: {item['error']}")
                elif item['status'] == 'skipped':
                    report_lines.append(f"Reason: {item['reason']}")
        
        report_content = "\n".join(report_lines)
        
        # Save report if path provided
        if output_path:
            output_path = Path(output_path)
            with open(output_path, 'w') as f:
                f.write(report_content)
            self.logger.info(f"Report saved to: {output_path}")
        
        return report_content
    
    def validate_config(self, config_path: Path) -> bool:
        """
        Validate a configuration file.
        
        Args:
            config_path: Path to configuration file
            
        Returns:
            True if valid, False otherwise
        """
        try:
            config = OrcaFlexConfig.from_yaml(config_path)
            warnings = config.validate_compatibility()
            
            if warnings:
                self.logger.warning("Validation warnings:")
                for warning in warnings:
                    self.logger.warning(f"  - {warning}")
            
            self.logger.info(f"Configuration valid: {config_path}")
            return True
            
        except Exception as e:
            self.logger.error(f"Validation failed: {e}")
            return False


def main():
    """Command-line interface for configuration migration."""
    parser = argparse.ArgumentParser(
        description="Migrate legacy OrcaFlex configuration files to the new unified format"
    )
    
    parser.add_argument(
        'input',
        help='Input file or directory path'
    )
    
    parser.add_argument(
        '-o', '--output',
        help='Output file path (for single file migration)'
    )
    
    parser.add_argument(
        '-d', '--directory',
        action='store_true',
        help='Migrate all configuration files in directory'
    )
    
    parser.add_argument(
        '-p', '--pattern',
        default='*.yaml',
        help='File pattern for directory migration (default: *.yaml)'
    )
    
    parser.add_argument(
        '-r', '--recursive',
        action='store_true',
        help='Search directories recursively'
    )
    
    parser.add_argument(
        '--no-backup',
        action='store_true',
        help='Do not create backup files'
    )
    
    parser.add_argument(
        '--validate-only',
        action='store_true',
        help='Only validate configuration without migration'
    )
    
    parser.add_argument(
        '--report',
        help='Generate migration report at specified path'
    )
    
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose output'
    )
    
    args = parser.parse_args()
    
    # Create migrator
    migrator = ConfigurationMigrator(verbose=args.verbose)
    
    # Handle validation only
    if args.validate_only:
        input_path = Path(args.input)
        if input_path.is_file():
            success = migrator.validate_config(input_path)
            return 0 if success else 1
        else:
            print("Validation only works with single files")
            return 1
    
    # Handle migration
    input_path = Path(args.input)
    
    if args.directory or input_path.is_dir():
        # Directory migration
        count = migrator.migrate_directory(
            input_path,
            pattern=args.pattern,
            recursive=args.recursive,
            backup=not args.no_backup
        )
        print(f"\nMigrated {count} files")
    else:
        # Single file migration
        success = migrator.migrate_file(
            input_path,
            output_path=Path(args.output) if args.output else None,
            backup=not args.no_backup
        )
        if not success:
            return 1
    
    # Generate report
    if args.report or args.directory:
        report_path = Path(args.report) if args.report else None
        report = migrator.generate_report(report_path)
        if not args.report:
            print("\n" + report)
    
    return 0


if __name__ == "__main__":
    exit(main())