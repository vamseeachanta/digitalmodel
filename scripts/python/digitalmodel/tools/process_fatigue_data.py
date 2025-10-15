#!/usr/bin/env python
"""
Process OrcaFlex fatigue data with custom naming convention
Adds _rainflow and _fft to output filenames
"""

import os
import sys
from pathlib import Path
import pandas as pd
import logging

# Add the project root to path
sys.path.insert(0, r'D:\github\digitalmodel\src')

from digitalmodel.modules.signal_analysis.orcaflex import TimeSeriesAnalyzer

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

def process_fatigue_files():
    """Process fatigue CSV files with custom naming"""
    
    # Configuration
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    pattern = "*_Strut?.csv"
    
    # Columns to analyze
    time_column = "time"
    data_columns = ["Tension (Vessel End)", "Tension (Jacket End)"]
    
    # Create configuration
    config = {
        'analysis': {
            'rainflow': {'enable': True, 'bin_count': 50},
            'fft': {'enable': True, 'window_size': 4096}
        },
        'output': {
            'plots': {'enable': False},
            'formats': {'csv': True}
        },
        'column_mapping': {
            'strategy': 'manual',
            'manual': {
                'time': time_column,
                'data_columns': data_columns
            }
        }
    }
    
    # Create analyzer
    analyzer = TimeSeriesAnalyzer(config=config)
    
    # Discover files
    files = list(input_dir.glob(pattern))
    logger.info(f"Found {len(files)} files to process")
    
    # Process each file
    for i, file_path in enumerate(files, 1):
        try:
            logger.info(f"Processing [{i}/{len(files)}]: {file_path.name}")
            
            # Read the file with specific columns
            df = pd.read_csv(file_path)
            
            # Check if required columns exist
            required_cols = [time_column] + data_columns
            missing_cols = [col for col in required_cols if col not in df.columns]
            if missing_cols:
                logger.warning(f"Missing columns in {file_path.name}: {missing_cols}")
                continue
            
            # Process the file
            results = analyzer.process_file(file_path)
            
            # Create custom output files
            base_name = file_path.stem  # e.g., "fat001_fsts_l015_mwl_wave01_Strut1"
            file_output_dir = output_dir / base_name
            file_output_dir.mkdir(parents=True, exist_ok=True)
            
            # Export results with custom naming
            for col_name in data_columns:
                safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
                
                if col_name in results.get('columns', {}):
                    col_results = results['columns'][col_name]
                    
                    # Export rainflow with custom name
                    if 'cycles' in col_results.get('rainflow', {}):
                        cycles = col_results['rainflow']['cycles']
                        if isinstance(cycles, pd.DataFrame):
                            # Custom filename: original_name_rainflow.csv
                            rainflow_filename = f"{base_name}_{safe_col_name}_rainflow.csv"
                            rainflow_path = file_output_dir / rainflow_filename
                            cycles.to_csv(rainflow_path, index=False)
                            logger.info(f"  Saved: {rainflow_filename}")
                    
                    # Export FFT with custom name
                    if 'window_fft' in col_results.get('spectral', {}):
                        spectrum = col_results['spectral']['window_fft']
                        if isinstance(spectrum, pd.DataFrame):
                            # Custom filename: original_name_fft.csv
                            fft_filename = f"{base_name}_{safe_col_name}_fft.csv"
                            fft_path = file_output_dir / fft_filename
                            spectrum.to_csv(fft_path, index=False)
                            logger.info(f"  Saved: {fft_filename}")
            
        except Exception as e:
            logger.error(f"Error processing {file_path.name}: {e}")
            continue
    
    logger.info(f"Processing complete. Results saved to {output_dir}")

if __name__ == "__main__":
    process_fatigue_files()