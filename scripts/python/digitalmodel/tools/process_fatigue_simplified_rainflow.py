"""
Simplified OrcaFlex Fatigue Analysis - Rainflow with Range and Cycle Count only
Processes tension data from OrcaFlex CSV files
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import glob
import logging
from scipy import signal
from collections import Counter

# Import from signal_analysis module
from src.digitalmodel.signal_analysis.core.rainflow import RainflowCounter
from src.digitalmodel.signal_analysis.core.spectral import SpectralAnalyzer

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

def process_fatigue_file(file_path, output_dir):
    """Process a single fatigue file with simplified rainflow output"""
    
    try:
        # Read CSV file
        df = pd.read_csv(file_path)
        
        # Define columns to analyze
        time_column = 'time'
        data_columns = ['Tension (Vessel End)', 'Tension (Jacket End)']
        
        # Check if required columns exist
        if time_column not in df.columns:
            logger.error(f"Time column '{time_column}' not found in {file_path}")
            return
        
        missing_cols = [col for col in data_columns if col not in df.columns]
        if missing_cols:
            logger.warning(f"Missing columns in {file_path}: {missing_cols}")
            data_columns = [col for col in data_columns if col in df.columns]
        
        if not data_columns:
            logger.error(f"No data columns to process in {file_path}")
            return
        
        # Get base filename
        base_name = Path(file_path).stem
        
        # Configuration for analysis
        config = {
            'analysis': {
                'fft': {
                    'window_size': 1024,
                    'overlap': 0.75,
                    'window_type': 'hann',
                    'frequency_limit': 2.0
                }
            }
        }
        
        # Colors for plotting
        vessel_color = 'navy'
        jacket_color = 'darkred'
        
        # Process each column
        rainflow_data = {}
        fft_data = {}
        psd_data = {}
        
        for col_name in data_columns:
            if col_name in df.columns:
                signal_data = df[col_name].values
                
                # SIMPLIFIED RAINFLOW ANALYSIS
                analyzer = RainflowCounter()
                cycles_df = analyzer.count_cycles(signal_data)
                
                # Group cycles by range and sum the counts
                # Round range to reasonable precision (0.1 kN)
                cycles_df['Range'] = cycles_df['range'].round(1)
                
                # Group by Range and sum counts
                rainflow_df = cycles_df.groupby('Range')['count'].sum().reset_index()
                rainflow_df.columns = ['Range', 'Count']
                rainflow_df = rainflow_df.sort_values('Range')
                
                rainflow_data[col_name] = rainflow_df
                
                # FFT ANALYSIS (keep as before)
                spectral = SpectralAnalyzer(config['analysis']['fft'])
                freq, power = spectral.compute_fft(signal_data, sample_rate=1.0)
                
                # Window-averaged FFT for smooth spectrum
                window_size = config['analysis']['fft']['window_size']
                overlap = config['analysis']['fft']['overlap']
                
                freq_avg, power_avg = spectral.compute_windowed_fft(
                    signal_data, 
                    sample_rate=1.0,
                    window_size=window_size,
                    overlap=overlap
                )
                
                fft_df = pd.DataFrame({
                    'frequency': freq_avg,
                    'power': power_avg
                })
                fft_data[col_name] = fft_df
                
                # Welch PSD
                frequencies, psd = signal.welch(
                    signal_data,
                    fs=1.0,
                    window='hann',
                    nperseg=window_size,
                    noverlap=int(window_size * overlap)
                )
                
                psd_df = pd.DataFrame({
                    'frequency': frequencies,
                    'power': psd
                })
                psd_data[col_name] = psd_df
        
        # Save simplified rainflow results
        for col_name, rainflow_df in rainflow_data.items():
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            output_file = output_dir / f"{base_name}_{safe_col_name}_rainflow_simplified.csv"
            rainflow_df.to_csv(output_file, index=False)
            logger.info(f"Saved simplified rainflow: {output_file.name}")
        
        # Save FFT results
        for col_name, fft_df in fft_data.items():
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            output_file = output_dir / f"{base_name}_{safe_col_name}_fft.csv"
            fft_df.to_csv(output_file, index=False)
            logger.info(f"Saved FFT: {output_file.name}")
        
        # Generate combined plot
        fig = plt.figure(figsize=(15, 10))
        
        # 1. Time Series Sample (top left)
        ax1 = plt.subplot(2, 2, 1)
        sample_points = min(2000, len(df))
        time_plot = df[time_column].values[:sample_points]
        
        for col_name in data_columns:
            if col_name in df.columns:
                if 'Vessel' in col_name:
                    color = vessel_color
                    linestyle = '-'
                    label = 'Vessel End'
                else:
                    color = jacket_color
                    linestyle = '--'
                    label = 'Jacket End'
                
                ax1.plot(time_plot, df[col_name].values[:sample_points],
                        color=color, linestyle=linestyle, linewidth=1.5,
                        alpha=0.8, label=label)
        
        ax1.set_xlabel('Time (s)')
        ax1.set_ylabel('Tension (kN)')
        ax1.set_title(f'Time Series Sample - {base_name}')
        ax1.legend(loc='upper right')
        ax1.grid(True, alpha=0.3)
        ax1.text(0.02, 0.98, 'Note: Analysis uses ALL data points',
                transform=ax1.transAxes, fontsize=8, fontweight='bold',
                verticalalignment='top')
        ax1.text(0.02, 0.94, 'Time series sample shown for visualization',
                transform=ax1.transAxes, fontsize=7,
                verticalalignment='top')
        
        # 2. SIMPLIFIED Rainflow Histogram (top right)
        ax2 = plt.subplot(2, 2, 2)
        
        bar_width = 0.35
        for i, (col_name, rainflow_df) in enumerate(rainflow_data.items()):
            if isinstance(rainflow_df, pd.DataFrame) and not rainflow_df.empty:
                if 'Vessel' in col_name:
                    color = vessel_color
                    label = 'Vessel End'
                    offset = -bar_width/2
                else:
                    color = jacket_color
                    label = 'Jacket End'
                    offset = bar_width/2
                
                # Plot top 20 most significant ranges
                top_ranges = rainflow_df.nlargest(20, 'Count')
                x_pos = np.arange(len(top_ranges))
                ax2.bar(x_pos + offset, top_ranges['Count'].values,
                       bar_width, color=color, alpha=0.7, label=label)
        
        ax2.set_xlabel('Range Bin Index')
        ax2.set_ylabel('Cycle Count')
        ax2.set_title('Simplified Rainflow - Top 20 Ranges')
        ax2.legend(loc='upper right')
        ax2.grid(True, alpha=0.3)
        
        # 3. FFT Spectrum (bottom left)
        ax3 = plt.subplot(2, 2, 3)
        freq_limit = config['analysis']['fft']['frequency_limit']
        
        for col_name, fft_df in fft_data.items():
            if isinstance(fft_df, pd.DataFrame):
                mask = fft_df['frequency'] <= freq_limit
                
                if 'Vessel' in col_name:
                    color = vessel_color
                    linestyle = '-'
                    label = 'Vessel End'
                else:
                    color = jacket_color
                    linestyle = ':'
                    label = 'Jacket End'
                
                ax3.semilogy(fft_df.loc[mask, 'frequency'],
                           fft_df.loc[mask, 'power'],
                           color=color, linestyle=linestyle, linewidth=1.5,
                           alpha=0.8, label=label)
        
        ax3.set_xlabel('Frequency (Hz)')
        ax3.set_ylabel('Power')
        ax3.set_title('FFT Spectrum (Window-Averaged)')
        ax3.legend(loc='upper right')
        ax3.grid(True, alpha=0.3, which='both')
        ax3.set_xlim([0, freq_limit])
        
        # 4. Welch PSD (bottom right)
        ax4 = plt.subplot(2, 2, 4)
        
        for col_name, psd_df in psd_data.items():
            if isinstance(psd_df, pd.DataFrame):
                mask = psd_df['frequency'] <= freq_limit
                
                if 'Vessel' in col_name:
                    color = vessel_color
                    linestyle = '-'
                    label = 'Vessel End'
                else:
                    color = jacket_color
                    linestyle = ':'
                    label = 'Jacket End'
                
                ax4.semilogy(psd_df.loc[mask, 'frequency'],
                           psd_df.loc[mask, 'power'],
                           color=color, linestyle=linestyle, linewidth=1.5,
                           alpha=0.8, label=label)
        
        ax4.set_xlabel('Frequency (Hz)')
        ax4.set_ylabel('PSD')
        ax4.set_title('Welch Power Spectral Density')
        ax4.legend(loc='upper right')
        ax4.grid(True, alpha=0.3, which='both')
        ax4.set_xlim([0, freq_limit])
        
        plt.suptitle(f'Fatigue Analysis - {base_name}', fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save plot
        plot_filename = output_dir / f"{base_name}_analysis_plot.png"
        plt.savefig(plot_filename, dpi=100, bbox_inches='tight')
        plt.close()
        logger.info(f"Saved plot: {plot_filename.name}")
        
        return True
        
    except Exception as e:
        logger.error(f"Error processing {file_path}: {e}")
        return False

def process_sample_file():
    """Process a single sample file to test the simplified output"""
    
    # Test with one file first
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    output_dir.mkdir(exist_ok=True)
    
    # Find a sample file
    sample_files = list(input_dir.glob("*_Strut1.csv"))
    if sample_files:
        sample_file = sample_files[0]
        logger.info(f"\n{'='*60}")
        logger.info(f"TESTING SIMPLIFIED RAINFLOW WITH SAMPLE FILE")
        logger.info(f"{'='*60}")
        logger.info(f"Sample file: {sample_file.name}")
        
        success = process_fatigue_file(sample_file, output_dir)
        
        if success:
            logger.info(f"\n✅ Sample processing successful!")
            logger.info(f"✅ Simplified rainflow: Range and Count only")
            logger.info(f"✅ Ready for batch processing")
            return True
    
    return False

def batch_process_all_files():
    """Process all Strut files in batch"""
    
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    output_dir.mkdir(exist_ok=True)
    
    # Find all Strut files
    pattern = "*_Strut?.csv"
    all_files = list(input_dir.glob(pattern))
    
    logger.info(f"\n{'='*60}")
    logger.info(f"BATCH PROCESSING ALL FILES")
    logger.info(f"{'='*60}")
    logger.info(f"Found {len(all_files)} files matching pattern: {pattern}")
    
    successful = 0
    failed = 0
    
    for i, file_path in enumerate(all_files, 1):
        logger.info(f"\n[{i}/{len(all_files)}] Processing: {file_path.name}")
        
        if process_fatigue_file(file_path, output_dir):
            successful += 1
        else:
            failed += 1
        
        # Progress update every 10 files
        if i % 10 == 0:
            logger.info(f"\nProgress: {i}/{len(all_files)} files processed")
            logger.info(f"Successful: {successful}, Failed: {failed}")
    
    # Final summary
    logger.info(f"\n{'='*60}")
    logger.info(f"BATCH PROCESSING COMPLETE")
    logger.info(f"{'='*60}")
    logger.info(f"Total files: {len(all_files)}")
    logger.info(f"✅ Successful: {successful}")
    logger.info(f"❌ Failed: {failed}")
    logger.info(f"Output directory: {output_dir}")
    
    return successful, failed

if __name__ == "__main__":
    # First test with sample
    if process_sample_file():
        # If sample successful, proceed with batch
        logger.info("\nProceeding with full batch processing...")
        batch_process_all_files()