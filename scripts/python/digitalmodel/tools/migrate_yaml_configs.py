#!/usr/bin/env python
"""
YAML Configuration Migration Script

Migrates old time_series_components configurations to new signal_analysis format.
"""

import yaml
import argparse
from pathlib import Path
from typing import Dict, Any
import shutil
from datetime import datetime


class YAMLConfigMigrator:
    """Migrate YAML configurations to new signal_analysis format"""
    
    def __init__(self, backup: bool = True):
        self.backup = backup
        self.migration_log = []
        
    def migrate_config(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Migrate old configuration to new format.
        
        Args:
            config: Old configuration dictionary
            
        Returns:
            Migrated configuration
        """
        new_config = {}
        
        # Add metadata
        new_config['meta'] = {
            'library': 'digitalmodel',
            'basename': 'signal_analysis',
            'version': '2.0.0',
            'description': 'Migrated configuration',
            'migration_date': datetime.now().isoformat()
        }
        
        # Preserve original basename if exists
        if 'basename' in config:
            new_config['meta']['original_basename'] = config['basename']
        
        # Main signal_analysis configuration
        new_config['signal_analysis'] = {}
        
        # Migrate data source
        if 'data' in config:
            new_config['signal_analysis']['data_source'] = self._migrate_data_source(config['data'])
        
        # Migrate column configuration
        if 'master_settings' in config and 'groups' in config['master_settings']:
            new_config['signal_analysis'].update(
                self._migrate_columns(config['master_settings']['groups'])
            )
        
        # Migrate analysis settings
        if 'analysis' in config:
            new_config['signal_analysis'].update(
                self._migrate_analysis(config['analysis'])
            )
        
        # Migrate FFT settings
        if 'fft' in config:
            new_config['signal_analysis']['spectral_analysis'] = self._migrate_fft(config['fft'])
        
        # Migrate time_series_components settings
        if 'time_series_components' in config:
            new_config['signal_analysis'].update(
                self._migrate_time_series_components(config['time_series_components'])
            )
        
        # Add performance settings
        new_config['signal_analysis']['performance'] = {
            'parallel_processing': True,
            'n_workers': 4,
            'chunk_size': None
        }
        
        # Migrate output settings
        if 'default' in config and 'config' in config['default']:
            new_config['signal_analysis']['output'] = self._migrate_output(config['default']['config'])
        
        # Preserve Analysis section for legacy compatibility
        if 'Analysis' in config:
            new_config['Analysis'] = config['Analysis']
        
        # Preserve default settings
        if 'default' in config:
            new_config['default'] = config['default']
        
        return new_config
    
    def _migrate_data_source(self, data_config: Dict) -> Dict:
        """Migrate data source configuration"""
        source = {
            'type': data_config.get('type', 'csv')
        }
        
        if 'groups' in data_config and len(data_config['groups']) > 0:
            group = data_config['groups'][0]
            if 'file_name' in group:
                source['file_path'] = group['file_name']
            if 'label' in group:
                source['label'] = group['label']
        
        return source
    
    def _migrate_columns(self, groups_config: Dict) -> Dict:
        """Migrate column configuration"""
        result = {}
        
        if 'columns' in groups_config:
            cols = groups_config['columns']
            if 'x' in cols and len(cols['x']) > 0:
                result['columns'] = {'time_column': cols['x'][0]}
            if 'y' in cols:
                result['columns'] = result.get('columns', {})
                result['columns']['signal_columns'] = cols['y']
        
        if 'transform' in groups_config:
            preprocessing = {'scale_factors': {}}
            for transform in groups_config['transform']:
                if 'column' in transform and 'scale' in transform:
                    preprocessing['scale_factors'][transform['column']] = transform['scale']
            result['preprocessing'] = preprocessing
        
        return result
    
    def _migrate_analysis(self, analysis_config: Dict) -> Dict:
        """Migrate analysis settings"""
        result = {}
        
        if 'basic' in analysis_config:
            basic = analysis_config['basic']
            
            # Rainflow analysis
            if basic.get('rainflow', False):
                result['rainflow_analysis'] = {
                    'enabled': True,
                    'method': 'astm',
                    'extract_info': True
                }
            
            # FFT settings
            if basic.get('sample_window_average_fft', False):
                if 'spectral_analysis' not in result:
                    result['spectral_analysis'] = {}
                result['spectral_analysis']['method'] = 'window_averaged'
                result['spectral_analysis']['enabled'] = True
            elif basic.get('fft', False) or basic.get('sample_fft', False):
                if 'spectral_analysis' not in result:
                    result['spectral_analysis'] = {}
                result['spectral_analysis']['method'] = 'fft'
                result['spectral_analysis']['enabled'] = True
            
            # Statistics
            if basic.get('statistics', False):
                result['statistics'] = {
                    'enabled': True,
                    'compute': ['mean', 'std', 'rms', 'peak_to_peak', 'zero_crossings']
                }
        
        return result
    
    def _migrate_fft(self, fft_config: Dict) -> Dict:
        """Migrate FFT configuration"""
        spectral = {
            'enabled': True,
            'method': 'fft'  # Default, may be overridden
        }
        
        # Window settings
        if 'window' in fft_config:
            window = fft_config['window']
            if 'size' in window:
                spectral['window_size'] = window['size']
                spectral['nperseg'] = window['size']  # For Welch method
            
            if 'moving_average' in window:
                ma = window['moving_average']
                spectral['post_processing'] = {
                    'moving_average': {
                        'enabled': ma.get('flag', False),
                        'window_size': ma.get('window_size', 4)
                    }
                }
        
        # Filter settings
        if 'filter' in fft_config:
            filter_cfg = fft_config['filter']
            filtering = {'enabled': filter_cfg.get('flag', False)}
            
            if filter_cfg.get('low_pass', {}).get('flag', False):
                filtering['type'] = 'lowpass'
                filtering['cutoff'] = filter_cfg['low_pass'].get('frequency_maximum')
            elif filter_cfg.get('high_pass', {}).get('flag', False):
                filtering['type'] = 'highpass'
                filtering['cutoff'] = filter_cfg['high_pass'].get('frequency_minimum')
            elif filter_cfg.get('band_pass', {}).get('flag', False):
                filtering['type'] = 'bandpass'
                filtering['low_cutoff'] = filter_cfg['band_pass'].get('frequency_minimum')
                filtering['high_cutoff'] = filter_cfg['band_pass'].get('frequency_maximum')
            else:
                filtering['type'] = 'none'
            
            spectral['filtering'] = filtering
        
        # Peak detection
        if 'peaks' in fft_config:
            peaks = fft_config['peaks']
            spectral['peak_detection'] = {
                'enabled': peaks.get('flag', False),
                'method': peaks.get('solver', 'find_peaks'),
                'min_height_percentage': peaks.get('min_height_percentage', 60),
                'min_distance_percentage': peaks.get('min_distance_index_percentage', 50)
            }
        
        return spectral
    
    def _migrate_time_series_components(self, tsc_config: Dict) -> Dict:
        """Migrate time_series_components settings"""
        result = {}
        
        if 'analysis' in tsc_config:
            if 'fft' in tsc_config['analysis']:
                result['spectral_analysis'] = self._migrate_fft(tsc_config['analysis']['fft'])
            
            if 'rainflow' in tsc_config['analysis']:
                rf = tsc_config['analysis']['rainflow']
                result['rainflow_analysis'] = {
                    'enabled': True,
                    'method': 'astm',
                    'bins': rf.get('bins', 20),
                    'range': rf.get('range', [0, 100])
                }
        
        return result
    
    def _migrate_output(self, config: Dict) -> Dict:
        """Migrate output configuration"""
        return {
            'directory': 'results/',
            'save_intermediate': True,
            'generate_report': True,
            'overwrite': config.get('overwrite', {}).get('output', True),
            'formats': ['csv', 'xlsx']
        }
    
    def migrate_file(self, input_path: Path, output_path: Path = None) -> bool:
        """
        Migrate a YAML configuration file.
        
        Args:
            input_path: Path to input YAML file
            output_path: Path to output file (optional)
            
        Returns:
            Success status
        """
        try:
            # Read original configuration
            with open(input_path, 'r') as f:
                old_config = yaml.safe_load(f)
            
            # Backup if requested
            if self.backup:
                backup_path = input_path.with_suffix('.yml.bak')
                shutil.copy2(input_path, backup_path)
                self.migration_log.append(f"Backed up to {backup_path}")
            
            # Migrate configuration
            new_config = self.migrate_config(old_config)
            
            # Determine output path
            if output_path is None:
                output_path = input_path.parent / f"{input_path.stem}_migrated.yml"
            
            # Write migrated configuration
            with open(output_path, 'w') as f:
                yaml.dump(new_config, f, default_flow_style=False, sort_keys=False)
            
            self.migration_log.append(f"Migrated {input_path} -> {output_path}")
            return True
            
        except Exception as e:
            self.migration_log.append(f"Error migrating {input_path}: {str(e)}")
            return False
    
    def migrate_directory(self, directory: Path, pattern: str = "*.yml") -> Dict[str, int]:
        """
        Migrate all YAML files in a directory.
        
        Args:
            directory: Directory to process
            pattern: File pattern to match
            
        Returns:
            Statistics dictionary
        """
        stats = {'total': 0, 'success': 0, 'failed': 0, 'skipped': 0}
        
        for yaml_file in directory.glob(pattern):
            # Skip already migrated files
            if '_migrated' in yaml_file.stem or yaml_file.suffix == '.bak':
                stats['skipped'] += 1
                continue
            
            stats['total'] += 1
            
            # Check if file needs migration
            with open(yaml_file, 'r') as f:
                content = f.read()
                if 'signal_analysis' in content and 'time_series_components' not in content:
                    self.migration_log.append(f"Skipping {yaml_file} - already migrated")
                    stats['skipped'] += 1
                    continue
            
            # Migrate file
            if self.migrate_file(yaml_file):
                stats['success'] += 1
            else:
                stats['failed'] += 1
        
        return stats
    
    def print_summary(self):
        """Print migration summary"""
        print("\n" + "="*60)
        print("YAML Configuration Migration Summary")
        print("="*60)
        for log_entry in self.migration_log:
            print(f"  {log_entry}")
        print("="*60)


def main():
    parser = argparse.ArgumentParser(
        description='Migrate YAML configurations to signal_analysis format'
    )
    parser.add_argument(
        'input',
        type=str,
        help='Input file or directory'
    )
    parser.add_argument(
        '-o', '--output',
        type=str,
        help='Output file (for single file migration)'
    )
    parser.add_argument(
        '--no-backup',
        action='store_true',
        help='Do not create backup files'
    )
    parser.add_argument(
        '-r', '--recursive',
        action='store_true',
        help='Process directory recursively'
    )
    
    args = parser.parse_args()
    
    migrator = YAMLConfigMigrator(backup=not args.no_backup)
    input_path = Path(args.input)
    
    if input_path.is_file():
        # Single file migration
        output_path = Path(args.output) if args.output else None
        success = migrator.migrate_file(input_path, output_path)
        
        if success:
            print(f"Successfully migrated {input_path}")
        else:
            print(f"Failed to migrate {input_path}")
    
    elif input_path.is_dir():
        # Directory migration
        pattern = "**/*.yml" if args.recursive else "*.yml"
        stats = migrator.migrate_directory(input_path, pattern)
        
        print(f"\nProcessed {stats['total']} files:")
        print(f"  Success: {stats['success']}")
        print(f"  Failed: {stats['failed']}")
        print(f"  Skipped: {stats['skipped']}")
    
    else:
        print(f"Error: {input_path} is not a valid file or directory")
        return 1
    
    migrator.print_summary()
    return 0


if __name__ == "__main__":
    exit(main())