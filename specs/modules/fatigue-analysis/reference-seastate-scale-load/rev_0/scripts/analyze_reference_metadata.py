#!/usr/bin/env python
"""
Analyze reference metadata from sample files
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np

# Sample files to analyze
sample_files = {
    'fsts_l015': {
        'wind01': 'fsts_l015_mwl_wind01_Strut1.csv',
        'wave01': 'fsts_l015_mwl_wave01_Strut1.csv'
    },
    'fsts_l095': {
        'wind01': 'fsts_l095_mwl_wind01_Strut1.csv',
        'wave01': 'fsts_l095_mwl_wave01_Strut1.csv'
    },
    'fsts_l015_125km3_l100_pb': {
        'wind01': 'fsts_l015_125km3_l100_pb_mwl_wind01_Strut1.csv',
        'wave01': 'fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv'
    },
    'fsts_l095_125km3_l000_pb': {
        'wind01': 'fsts_l095_125km3_l000_pb_mwl_wind01_Strut1.csv',
        'wave01': 'fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv'
    }
}

# Reference load specifications
reference_specs = {
    'wind01': {
        'type': 'Wind',
        'magnitude': '10 m/s',
        'period': 'N/A',
        'direction': '0°'
    },
    'wave01': {
        'type': 'Wave',
        'magnitude': 'Hs=0.5m',
        'period': 'Tp=2.7s',
        'direction': '0°'
    }
}

def analyze_files():
    """Analyze all sample files and create detailed table"""
    
    sample_path = Path(__file__).parent / "sample_data"
    
    print("\n### Enhanced Data Validation Table with Reference Metadata:\n")
    
    # Print table header with grouped columns
    print("| | | | | Reference Load |||||")
    print("|Configuration|Reference|Time Range|Tension Range|Samples|Type|Magnitude|Period|Direction|")
    print("|-------------|---------|----------|-------------|-------|-----|---------|------|---------|")
    
    for config, refs in sample_files.items():
        for ref_name, filename in refs.items():
            filepath = sample_path / filename
            
            if filepath.exists():
                df = pd.read_csv(filepath)
                
                # Get data ranges
                time_col = df.columns[0]
                tension_col = df.columns[1]
                
                time_range = f"{df[time_col].min():.1f}-{df[time_col].max():.1f}s"
                tension_min = df[tension_col].min()
                tension_max = df[tension_col].max()
                tension_range = f"{tension_min:.1f}-{tension_max:.1f} kN"
                samples = len(df)
                
                # Get reference metadata
                ref_meta = reference_specs[ref_name]
                
                # Format configuration name for display
                config_display = config
                if config == 'fsts_l015':
                    config_display = 'FSTs Light (15%)'
                elif config == 'fsts_l095':
                    config_display = 'FSTs Full (95%)'
                elif config == 'fsts_l015_125km3_l100_pb':
                    config_display = 'FSTs Light + LNGC Full'
                elif config == 'fsts_l095_125km3_l000_pb':
                    config_display = 'FSTs Full + LNGC Light'
                
                # Print row
                print(f"|{config_display}|{ref_name}|{time_range}|{tension_range}|{samples}|"
                      f"{ref_meta['type']}|{ref_meta['magnitude']}|{ref_meta['period']}|{ref_meta['direction']}|")

if __name__ == "__main__":
    analyze_files()