#!/usr/bin/env python
"""
Test processing for a single OrcaFlex fatigue file with custom naming and plots
"""

import os
import sys
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import logging

# Add the project root to path
sys.path.insert(0, r'D:\github\digitalmodel\src')

from digitalmodel.signal_analysis.orcaflex import TimeSeriesAnalyzer

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

def process_single_test_file():
    """Process a single fatigue CSV file for testing"""
    
    # Configuration
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Get first file matching pattern for testing
    pattern = "*_Strut?.csv"
    files = list(input_dir.glob(pattern))
    
    if not files:
        logger.error("No files found matching pattern")
        return
    
    # Use first file for testing
    test_file = files[0]
    logger.info(f"Testing with file: {test_file.name}")
    
    # Columns to analyze
    time_column = "time"
    data_columns = ["Tension (Vessel End)", "Tension (Jacket End)"]
    
    # Create configuration
    config = {
        'analysis': {
            'rainflow': {'enable': True, 'bin_count': 50},
            'fft': {'enable': True, 'window_size': 4096, 'frequency_limit': 2.0}
        },
        'output': {
            'plots': {'enable': False},  # We'll create custom plots
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
    
    try:
        # Read the file
        df = pd.read_csv(test_file)
        logger.info(f"File shape: {df.shape}")
        logger.info(f"Columns: {list(df.columns)[:10]}...")  # Show first 10 columns
        
        # Check if required columns exist
        required_cols = [time_column] + data_columns
        missing_cols = [col for col in required_cols if col not in df.columns]
        if missing_cols:
            logger.error(f"Missing columns: {missing_cols}")
            return
        
        # Process the file
        logger.info("Processing file...")
        results = analyzer.process_file(test_file)
        
        # Create base filename
        base_name = test_file.stem  # e.g., "fat001_fsts_l015_mwl_wave01_Strut1"
        
        # Create combined plot with subplots
        fig = plt.figure(figsize=(16, 10))
        fig.suptitle(f'Analysis Results: {base_name}', fontsize=14, fontweight='bold')
        
        # Process results and save CSV files
        plot_row = 0
        for col_idx, col_name in enumerate(data_columns):
            if col_name not in results.get('columns', {}):
                continue
                
            col_results = results['columns'][col_name]
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            
            # Save rainflow CSV
            if 'cycles' in col_results.get('rainflow', {}):
                cycles = col_results['rainflow']['cycles']
                if isinstance(cycles, pd.DataFrame) and len(cycles) > 0:
                    rainflow_filename = f"{base_name}_{safe_col_name}_rainflow.csv"
                    rainflow_path = output_dir / rainflow_filename
                    cycles.to_csv(rainflow_path, index=False)
                    logger.info(f"Saved: {rainflow_filename}")
                    
                    # Plot rainflow histogram
                    ax1 = plt.subplot(3, 2, plot_row * 2 + 1)
                    if 'histogram' in col_results.get('rainflow', {}):
                        histogram = col_results['rainflow']['histogram']
                        if isinstance(histogram, pd.DataFrame) and len(histogram) > 0:
                            centers = (histogram['range_min'] + histogram['range_max']) / 2
                            ax1.bar(centers, histogram['count'], 
                                   width=(histogram['range_max'] - histogram['range_min']).mean(),
                                   alpha=0.7, edgecolor='black')
                    ax1.set_xlabel('Range')
                    ax1.set_ylabel('Count')
                    ax1.set_title(f'Rainflow Histogram - {col_name}')
                    ax1.grid(True, alpha=0.3)
            
            # Save FFT CSV
            if 'window_fft' in col_results.get('spectral', {}):
                spectrum = col_results['spectral']['window_fft']
                if isinstance(spectrum, pd.DataFrame) and len(spectrum) > 0:
                    fft_filename = f"{base_name}_{safe_col_name}_fft.csv"
                    fft_path = output_dir / fft_filename
                    spectrum.to_csv(fft_path, index=False)
                    logger.info(f"Saved: {fft_filename}")
                    
                    # Plot FFT spectrum
                    ax2 = plt.subplot(3, 2, plot_row * 2 + 2)
                    # Limit frequency range for better visualization
                    freq_limit = 2.0  # Hz
                    mask = spectrum['frequency'] <= freq_limit
                    ax2.semilogy(spectrum.loc[mask, 'frequency'], 
                               spectrum.loc[mask, 'power'],
                               alpha=0.7)
                    ax2.set_xlabel('Frequency (Hz)')
                    ax2.set_ylabel('Power')
                    ax2.set_title(f'FFT Spectrum - {col_name}')
                    ax2.grid(True, alpha=0.3)
                    ax2.set_xlim([0, freq_limit])
            
            plot_row += 1
        
        # Add summary statistics in the last subplot
        ax3 = plt.subplot(3, 2, 5)
        ax3.axis('off')
        
        summary_text = f"File: {test_file.name}\n"
        summary_text += f"Time range: {df[time_column].min():.1f} - {df[time_column].max():.1f} s\n"
        summary_text += f"Sampling rate: {1/(df[time_column].iloc[1] - df[time_column].iloc[0]):.2f} Hz\n\n"
        
        for col_name in data_columns:
            if col_name in results.get('columns', {}):
                col_results = results['columns'][col_name]
                stats = col_results.get('statistics', {})
                rainflow_stats = col_results.get('rainflow', {}).get('statistics', {})
                
                summary_text += f"{col_name}:\n"
                summary_text += f"  Mean: {stats.get('mean', 0):.2f}\n"
                summary_text += f"  Std: {stats.get('std', 0):.2f}\n"
                summary_text += f"  Range: [{stats.get('min', 0):.2f}, {stats.get('max', 0):.2f}]\n"
                summary_text += f"  Rainflow cycles: {rainflow_stats.get('total_cycles', 0)}\n"
                summary_text += f"  Max range: {rainflow_stats.get('max_range', 0):.2f}\n\n"
        
        ax3.text(0.1, 0.9, summary_text, transform=ax3.transAxes,
                fontsize=10, verticalalignment='top', fontfamily='monospace')
        
        # Add time series plot in the last position
        ax4 = plt.subplot(3, 2, 6)
        time = df[time_column].values[:1000]  # First 1000 points for visualization
        for col_name in data_columns:
            if col_name in df.columns:
                ax4.plot(time, df[col_name].values[:1000], label=col_name, alpha=0.7)
        ax4.set_xlabel('Time (s)')
        ax4.set_ylabel('Tension')
        ax4.set_title('Time Series (First 1000 points)')
        ax4.legend(loc='best', fontsize=8)
        ax4.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        # Save plot
        plot_filename = f"{base_name}_analysis_plot.png"
        plot_path = output_dir / plot_filename
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        logger.info(f"Saved plot: {plot_filename}")
        
        # Show plot (optional)
        # plt.show()
        plt.close()
        
        # Summary
        logger.info("\n" + "="*60)
        logger.info("TEST SUMMARY")
        logger.info("="*60)
        logger.info(f"Input file: {test_file.name}")
        logger.info(f"Output directory: {output_dir}")
        logger.info("\nGenerated files:")
        
        for col_name in data_columns:
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            logger.info(f"  - {base_name}_{safe_col_name}_rainflow.csv")
            logger.info(f"  - {base_name}_{safe_col_name}_fft.csv")
        logger.info(f"  - {plot_filename}")
        
        logger.info("\nFile naming convention:")
        logger.info("  Rainflow: <original_filename>_<column>_rainflow.csv")
        logger.info("  FFT: <original_filename>_<column>_fft.csv")
        logger.info("  Plot: <original_filename>_analysis_plot.png")
        
        logger.info("\nAll files saved directly in: " + str(output_dir))
        logger.info("\nIf this looks good, we can process all 544 files with the same format.")
        
    except Exception as e:
        logger.error(f"Error processing file: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    process_single_test_file()