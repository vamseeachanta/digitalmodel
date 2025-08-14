"""
WORKING OrcaFlex Browser Interface - Connects to REAL FILES
This actually reads and processes real OrcaFlex CSV files from the specified directory
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import json
import re
from datetime import datetime
import glob

# Configuration - ACTUAL OrcaFlex data path
ORCAFLEX_BASE_PATH = Path("D:/1522/ctr7/orcaflex/rev_a08")
OUTPUT_PATH = ORCAFLEX_BASE_PATH / "output" / "csv"

class RealOrcaFlexBrowser:
    """Browser that actually reads real OrcaFlex files"""
    
    def __init__(self, base_path: Path = ORCAFLEX_BASE_PATH):
        self.base_path = base_path
        self.output_path = base_path / "output" / "csv"
        self.current_config = {
            'vessel_type': 'fsts',
            'loading_condition': 'l015',
            'tide_level': 'hwl',
            'return_period': '0100yr',
            'wave_direction': '000deg',
            'analysis_type': '03c',
            'auto_max': True
        }
        self.max_force_config = None
        self.available_files = []
        
        print(f"Initializing OrcaFlex Browser with path: {self.base_path}")
        self._check_directory()
    
    def _check_directory(self):
        """Check if the OrcaFlex directory exists and has files"""
        if not self.base_path.exists():
            print(f"⚠️ WARNING: Base path does not exist: {self.base_path}")
            print("Creating mock directory structure for demo...")
            self._create_mock_structure()
        else:
            print(f"✅ Found OrcaFlex directory: {self.base_path}")
            self._scan_available_files()
    
    def _scan_available_files(self):
        """Scan for actual CSV files in the output directory"""
        print("\nScanning for CSV files...")
        
        # Look for CSV files in various possible locations
        search_paths = [
            self.output_path,
            self.output_path / "combined",
            self.output_path / "individual",
            self.base_path / "output",
            self.base_path
        ]
        
        all_csv_files = []
        for search_path in search_paths:
            if search_path.exists():
                csv_files = list(search_path.glob("*.csv"))
                if csv_files:
                    print(f"  Found {len(csv_files)} CSV files in {search_path}")
                    all_csv_files.extend(csv_files)
        
        self.available_files = all_csv_files
        
        if not all_csv_files:
            print("  No CSV files found. Will create sample data...")
            self._create_sample_data()
        else:
            print(f"\n📊 Total CSV files found: {len(all_csv_files)}")
            self._categorize_files()
    
    def _categorize_files(self):
        """Categorize the found files"""
        categories = {
            'summary': [],
            'strut': [],
            'jacket': [],
            'mooring': [],
            'fst': [],
            'other': []
        }
        
        for file_path in self.available_files[:20]:  # Show first 20
            filename = file_path.name.lower()
            
            if filename.startswith('dm'):
                categories['summary'].append(file_path)
            elif 'strut' in filename:
                categories['strut'].append(file_path)
            elif 'jacket' in filename:
                categories['jacket'].append(file_path)
            elif 'mooring' in filename or 'line' in filename:
                categories['mooring'].append(file_path)
            elif 'fst' in filename and '6dof' in filename:
                categories['fst'].append(file_path)
            else:
                categories['other'].append(file_path)
        
        print("\n📁 File Categories:")
        for category, files in categories.items():
            if files:
                print(f"  {category}: {len(files)} files")
                for f in files[:3]:  # Show first 3 of each
                    print(f"    - {f.name}")
    
    def find_maximum_force_configuration(self):
        """Find the configuration with maximum strut forces from REAL summary files"""
        print("\n🔍 Searching for maximum force configuration...")
        
        # Look for summary files
        summary_patterns = [
            "dm*strut_dyn.csv",
            "dm*strut*.csv",
            "*summary*.csv",
            "dm*.csv"
        ]
        
        summary_files = []
        for pattern in summary_patterns:
            if self.output_path.exists():
                matches = list(self.output_path.glob(pattern))
                summary_files.extend(matches)
                if matches:
                    print(f"  Found {len(matches)} files matching {pattern}")
                    break
        
        if not summary_files:
            print("  No summary files found. Using default configuration.")
            return self._get_default_max_config()
        
        # Process summary files to find maximum
        max_force = -float('inf')
        max_config = None
        max_file = None
        
        for file_path in summary_files[:10]:  # Process first 10
            try:
                print(f"  Reading {file_path.name}...")
                df = pd.read_csv(file_path, nrows=100)  # Read first 100 rows
                
                # Look for force columns
                force_columns = [col for col in df.columns if 'max' in col.lower() and 
                               ('strut' in col.lower() or 'force' in col.lower() or 'tension' in col.lower())]
                
                if force_columns:
                    # Find maximum value
                    for col in force_columns:
                        try:
                            col_max = pd.to_numeric(df[col], errors='coerce').max()
                            if not np.isnan(col_max) and col_max > max_force:
                                max_force = col_max
                                max_file = file_path
                                # Extract configuration from filename
                                max_config = self._parse_filename_config(file_path.name)
                                print(f"    New max found: {col_max:.2f} in {col}")
                        except:
                            pass
            except Exception as e:
                print(f"    Error reading {file_path.name}: {e}")
        
        if max_config:
            self.max_force_config = max_config
            print(f"\n✅ Maximum force configuration found:")
            print(f"  File: {max_file.name}")
            print(f"  Force: {max_force:.2f}")
            print(f"  Config: {json.dumps(max_config, indent=2)}")
        else:
            print("  Using default configuration")
            self.max_force_config = self._get_default_max_config()
        
        return self.max_force_config
    
    def _parse_filename_config(self, filename: str) -> Dict:
        """Parse configuration from filename"""
        config = {
            'vessel_type': 'fsts',
            'loading_condition': 'l015',
            'tide_level': 'hwl',
            'return_period': '0100yr',
            'wave_direction': '000deg',
            'analysis_type': '03c'
        }
        
        filename_lower = filename.lower()
        
        # Extract vessel type
        for vessel in ['fsts', 'flng', 'lngc']:
            if vessel in filename_lower:
                config['vessel_type'] = vessel
                break
        
        # Extract loading condition
        match = re.search(r'(l\d{3})', filename_lower)
        if match:
            config['loading_condition'] = match.group(1)
        
        # Extract tide level
        for tide in ['hwl', 'mwl', 'lwl']:
            if tide in filename_lower:
                config['tide_level'] = tide
                break
        
        # Extract return period
        match = re.search(r'(\d{4}yr)', filename_lower)
        if match:
            config['return_period'] = match.group(1)
        
        # Extract wave direction
        match = re.search(r'(\d{3}deg)', filename_lower)
        if match:
            config['wave_direction'] = match.group(1)
        
        # Extract analysis type
        match = re.search(r'(\d{2}[a-z])', filename_lower)
        if match:
            config['analysis_type'] = match.group(1)
        
        return config
    
    def search_files_with_pattern(self, config: Dict) -> List[Path]:
        """Search for files matching the configuration pattern"""
        # Generate search pattern
        pattern_parts = []
        if not config.get('auto_max'):
            pattern_parts = [
                config.get('vessel_type', '*'),
                config.get('analysis_type', '*'),
                config.get('return_period', '*'),
                config.get('loading_condition', '*'),
                config.get('tide_level', '*')
            ]
        else:
            # Use max configuration if in auto-max mode
            if self.max_force_config:
                pattern_parts = [
                    self.max_force_config.get('vessel_type', '*'),
                    self.max_force_config.get('analysis_type', '*'),
                    self.max_force_config.get('return_period', '*'),
                    self.max_force_config.get('loading_condition', '*'),
                    self.max_force_config.get('tide_level', '*')
                ]
        
        pattern = '_'.join(filter(lambda x: x != '*', pattern_parts))
        print(f"\n🔎 Searching for pattern: {pattern}*")
        
        matching_files = []
        for file_path in self.available_files:
            if pattern.lower() in file_path.name.lower():
                matching_files.append(file_path)
        
        print(f"  Found {len(matching_files)} matching files")
        
        # Show sample of matching files
        for f in matching_files[:5]:
            print(f"    - {f.name}")
        
        return matching_files
    
    def _get_default_max_config(self) -> Dict:
        """Get default max configuration"""
        return {
            'vessel_type': 'fsts',
            'loading_condition': 'l095',  # 95% loading typically has max forces
            'tide_level': 'hwl',  # High water level
            'return_period': '0100yr',  # 100 year storm
            'wave_direction': '000deg',
            'analysis_type': '03c'
        }
    
    def _create_sample_data(self):
        """Create sample OrcaFlex CSV files for demonstration"""
        print("\n📝 Creating sample OrcaFlex data files...")
        
        # Create output directory
        sample_dir = Path("sample_orcaflex_data/output/csv")
        sample_dir.mkdir(parents=True, exist_ok=True)
        
        # Create a summary file with maximum forces
        summary_data = {
            'fe_filename': [
                'fsts_03c_0100yr_l095_hwl.sim',
                'fsts_03c_0100yr_l050_mwl.sim',
                'fsts_03c_0100yr_l015_lwl.sim'
            ],
            'Strut1_Body_eff_tension_max': [1500.0, 1200.0, 900.0],
            'Strut2_Body_eff_tension_max': [1450.0, 1150.0, 850.0],
            'Jacket1_force_max': [2000.0, 1800.0, 1600.0],
            'Mooring1_tension_max': [1800.0, 1600.0, 1400.0]
        }
        df = pd.DataFrame(summary_data)
        summary_file = sample_dir / "dm_fsts_03c_0100yr_strut_dyn.csv"
        df.to_csv(summary_file, index=False)
        print(f"  Created summary file: {summary_file.name}")
        
        # Create time series files
        configs = [
            ('fsts_03c_0100yr_l095_hwl', 1500),  # Max force config
            ('fsts_03c_0100yr_l050_mwl', 1200),
            ('fsts_03c_0100yr_l015_lwl', 900)
        ]
        
        for base_name, base_force in configs:
            for component in ['Strut1', 'Strut2', 'Jacket1', 'Mooring1']:
                # Create realistic time series data
                time = np.linspace(0, 3600, 361)  # 1 hour, 10-second intervals
                force = base_force + 100 * np.sin(2 * np.pi * time / 300) + \
                       50 * np.random.randn(len(time))
                
                ts_data = {
                    'Time': time,
                    'Force': force,
                    'Moment_X': force * 0.5,
                    'Moment_Y': force * 0.3
                }
                df = pd.DataFrame(ts_data)
                
                filename = f"{base_name}_{component}.csv"
                df.to_csv(sample_dir / filename, index=False)
            
            print(f"  Created time series files for {base_name}")
        
        # Update paths to use sample data
        self.base_path = Path("sample_orcaflex_data")
        self.output_path = sample_dir
        self._scan_available_files()
        
        print(f"\n✅ Sample data created in: {sample_dir}")
        return sample_dir
    
    def run_interactive_demo(self):
        """Run an interactive demonstration"""
        print("\n" + "="*60)
        print("ORCAFLEX BROWSER INTERFACE - WORKING DEMO")
        print("="*60)
        
        # Find maximum configuration
        self.find_maximum_force_configuration()
        
        while True:
            print("\n" + "-"*40)
            print("Current Configuration:")
            print(f"  Mode: {'AUTO-MAX' if self.current_config['auto_max'] else 'MANUAL'}")
            if not self.current_config['auto_max']:
                print(f"  Vessel: {self.current_config['vessel_type']}")
                print(f"  Loading: {self.current_config['loading_condition']}")
                print(f"  Tide: {self.current_config['tide_level']}")
                print(f"  Return Period: {self.current_config['return_period']}")
            
            print("\nOptions:")
            print("  1. Toggle Auto-Max/Manual Mode")
            print("  2. Change Parameters (Manual mode)")
            print("  3. Search Files")
            print("  4. Show File Statistics")
            print("  5. Export Configuration")
            print("  0. Exit")
            
            choice = input("\nSelect option: ").strip()
            
            if choice == '0':
                break
            elif choice == '1':
                self.current_config['auto_max'] = not self.current_config['auto_max']
                print(f"Switched to {'AUTO-MAX' if self.current_config['auto_max'] else 'MANUAL'} mode")
            elif choice == '2':
                if self.current_config['auto_max']:
                    print("Switch to manual mode first!")
                else:
                    self._change_parameters()
            elif choice == '3':
                self.search_files_with_pattern(self.current_config)
            elif choice == '4':
                self._show_statistics()
            elif choice == '5':
                self._export_configuration()
    
    def _change_parameters(self):
        """Interactive parameter change"""
        print("\nChange Parameters:")
        print("1. Vessel Type (fsts/flng/lngc)")
        print("2. Loading (l015/l050/l095)")
        print("3. Tide Level (hwl/mwl/lwl)")
        print("4. Return Period (0001yr/0010yr/0100yr)")
        
        param_choice = input("Select parameter to change: ").strip()
        
        if param_choice == '1':
            vessel = input("Enter vessel type (fsts/flng/lngc): ").strip().lower()
            if vessel in ['fsts', 'flng', 'lngc']:
                self.current_config['vessel_type'] = vessel
        elif param_choice == '2':
            loading = input("Enter loading (l015/l050/l095): ").strip().lower()
            if loading in ['l015', 'l050', 'l095']:
                self.current_config['loading_condition'] = loading
        elif param_choice == '3':
            tide = input("Enter tide level (hwl/mwl/lwl): ").strip().lower()
            if tide in ['hwl', 'mwl', 'lwl']:
                self.current_config['tide_level'] = tide
        elif param_choice == '4':
            period = input("Enter return period (0001yr/0010yr/0100yr): ").strip()
            if period in ['0001yr', '0010yr', '0100yr']:
                self.current_config['return_period'] = period
    
    def _show_statistics(self):
        """Show file statistics"""
        print(f"\n📊 File Statistics:")
        print(f"  Total CSV files: {len(self.available_files)}")
        
        if self.available_files:
            total_size = sum(f.stat().st_size for f in self.available_files if f.exists())
            print(f"  Total size: {total_size / (1024*1024):.2f} MB")
            
            # Show newest and oldest files
            files_with_time = [(f, f.stat().st_mtime) for f in self.available_files if f.exists()]
            if files_with_time:
                files_with_time.sort(key=lambda x: x[1])
                oldest = files_with_time[0][0]
                newest = files_with_time[-1][0]
                print(f"  Oldest file: {oldest.name}")
                print(f"  Newest file: {newest.name}")
    
    def _export_configuration(self):
        """Export current configuration"""
        filename = f"orcaflex_config_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(filename, 'w') as f:
            json.dump(self.current_config, f, indent=2)
        print(f"Configuration exported to: {filename}")


def main():
    """Main entry point"""
    print("Starting OrcaFlex Browser Interface...")
    print(f"Python version: {sys.version}")
    print(f"Working directory: {os.getcwd()}")
    
    browser = RealOrcaFlexBrowser()
    
    # Check if running with command line arguments
    if len(sys.argv) > 1:
        if sys.argv[1] == '--test':
            # Run automated test
            print("\nRunning automated test...")
            browser.find_maximum_force_configuration()
            browser.search_files_with_pattern(browser.current_config)
        elif sys.argv[1] == '--create-sample':
            # Create sample data
            browser._create_sample_data()
    else:
        # Run interactive demo
        browser.run_interactive_demo()


if __name__ == "__main__":
    main()