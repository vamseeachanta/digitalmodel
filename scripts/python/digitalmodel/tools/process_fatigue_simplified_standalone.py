"""
Simplified Standalone OrcaFlex Fatigue Analysis
Rainflow with Range and Cycle Count only
No dependencies on signal_analysis module
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import logging
from scipy import signal
from collections import defaultdict

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

def rainflow_counting(data):
    """Simple rainflow counting implementation"""
    def find_extrema(signal):
        """Find peaks and valleys in signal"""
        extrema = []
        for i in range(1, len(signal) - 1):
            if signal[i] > signal[i-1] and signal[i] > signal[i+1]:
                extrema.append(('peak', i, signal[i]))
            elif signal[i] < signal[i-1] and signal[i] < signal[i+1]:
                extrema.append(('valley', i, signal[i]))
        return extrema
    
    def simple_rainflow(extrema):
        """Simplified rainflow counting"""
        cycles = []
        for i in range(len(extrema) - 1):
            if extrema[i][0] != extrema[i+1][0]:  # Peak to valley or valley to peak
                range_val = abs(extrema[i][2] - extrema[i+1][2])
                mean_val = (extrema[i][2] + extrema[i+1][2]) / 2
                cycles.append({'range': range_val, 'mean': mean_val, 'count': 0.5})
        return cycles
    
    extrema = find_extrema(data)
    if len(extrema) < 2:
        return pd.DataFrame(columns=['Range', 'Count'])
    
    cycles = simple_rainflow(extrema)
    
    # Group by range and sum counts
    range_counts = defaultdict(float)
    for cycle in cycles:
        range_rounded = round(cycle['range'], 1)  # Round to 0.1 kN
        range_counts[range_rounded] += cycle['count']
    
    # Convert to DataFrame
    df = pd.DataFrame([
        {'Range': r, 'Count': c} 
        for r, c in sorted(range_counts.items())
    ])
    
    return df

def process_fatigue_file(file_path, output_dir):
    """Process a single fatigue file with simplified output"""
    
    try:
        # Read CSV file
        df = pd.read_csv(file_path)
        
        # Define columns
        time_column = 'time'
        data_columns = ['Tension (Vessel End)', 'Tension (Jacket End)']
        
        # Validate columns
        if time_column not in df.columns:
            logger.error(f"Time column not found in {file_path}")
            return False
        
        available_columns = [col for col in data_columns if col in df.columns]
        if not available_columns:
            logger.error(f"No data columns found in {file_path}")
            return False
        
        # Get base filename
        base_name = Path(file_path).stem
        
        # Process each column
        results = {}
        
        for col_name in available_columns:
            signal_data = df[col_name].values
            
            # Simplified Rainflow
            rainflow_df = rainflow_counting(signal_data)
            results[f'{col_name}_rainflow'] = rainflow_df
            
            # FFT Analysis
            freqs = np.fft.fftfreq(len(signal_data), d=1.0)
            fft_vals = np.fft.fft(signal_data)
            power = np.abs(fft_vals) ** 2
            
            # Keep only positive frequencies
            pos_mask = freqs > 0
            fft_df = pd.DataFrame({
                'frequency': freqs[pos_mask],
                'power': power[pos_mask]
            })
            fft_df = fft_df[fft_df['frequency'] <= 2.0]  # Limit to 2 Hz
            results[f'{col_name}_fft'] = fft_df
            
            # Welch PSD
            f_welch, psd = signal.welch(signal_data, fs=1.0, nperseg=1024, noverlap=768)
            psd_df = pd.DataFrame({
                'frequency': f_welch[f_welch <= 2.0],
                'power': psd[f_welch <= 2.0]
            })
            results[f'{col_name}_psd'] = psd_df
        
        # Save simplified rainflow results
        for col_name in available_columns:
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            
            # Save rainflow (simplified with just Range and Count)
            rainflow_df = results[f'{col_name}_rainflow']
            rainflow_file = output_dir / f"{base_name}_{safe_col_name}_rainflow_simplified.csv"
            rainflow_df.to_csv(rainflow_file, index=False)
            logger.info(f"  Saved: {rainflow_file.name} ({len(rainflow_df)} range bins)")
            
            # Save FFT
            fft_df = results[f'{col_name}_fft']
            fft_file = output_dir / f"{base_name}_{safe_col_name}_fft.csv"
            fft_df.to_csv(fft_file, index=False)
        
        # Generate plot
        vessel_color = 'navy'
        jacket_color = 'darkred'
        
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))
        
        # 1. Time Series Sample
        ax1 = axes[0, 0]
        sample_size = min(2000, len(df))
        time_sample = df[time_column].values[:sample_size]
        
        for col_name in available_columns:
            color = vessel_color if 'Vessel' in col_name else jacket_color
            linestyle = '-' if 'Vessel' in col_name else '--'
            label = 'Vessel End' if 'Vessel' in col_name else 'Jacket End'
            
            ax1.plot(time_sample, df[col_name].values[:sample_size],
                    color=color, linestyle=linestyle, label=label, alpha=0.8)
        
        ax1.set_xlabel('Time (s)')
        ax1.set_ylabel('Tension (kN)')
        ax1.set_title('Time Series Sample')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        ax1.text(0.02, 0.98, 'Analysis uses ALL data',
                transform=ax1.transAxes, fontsize=8, fontweight='bold',
                verticalalignment='top')
        
        # 2. Simplified Rainflow
        ax2 = axes[0, 1]
        bar_width = 0.35
        
        for i, col_name in enumerate(available_columns):
            rainflow_df = results[f'{col_name}_rainflow']
            if not rainflow_df.empty:
                # Show top 20 ranges
                top_ranges = rainflow_df.nlargest(20, 'Count')
                
                color = vessel_color if 'Vessel' in col_name else jacket_color
                label = 'Vessel End' if 'Vessel' in col_name else 'Jacket End'
                offset = -bar_width/2 if 'Vessel' in col_name else bar_width/2
                
                x_pos = np.arange(len(top_ranges))
                ax2.bar(x_pos + offset, top_ranges['Count'].values,
                       bar_width, color=color, alpha=0.7, label=label)
        
        ax2.set_xlabel('Range Index (Top 20)')
        ax2.set_ylabel('Cycle Count')
        ax2.set_title('Simplified Rainflow - Range vs Count')
        ax2.legend()
        ax2.grid(True, alpha=0.3)
        
        # 3. FFT Spectrum
        ax3 = axes[1, 0]
        for col_name in available_columns:
            fft_df = results[f'{col_name}_fft']
            if not fft_df.empty:
                color = vessel_color if 'Vessel' in col_name else jacket_color
                linestyle = '-' if 'Vessel' in col_name else ':'
                label = 'Vessel End' if 'Vessel' in col_name else 'Jacket End'
                
                ax3.semilogy(fft_df['frequency'], fft_df['power'],
                           color=color, linestyle=linestyle, label=label, alpha=0.8)
        
        ax3.set_xlabel('Frequency (Hz)')
        ax3.set_ylabel('Power')
        ax3.set_title('FFT Spectrum')
        ax3.legend()
        ax3.grid(True, alpha=0.3, which='both')
        ax3.set_xlim([0, 2.0])
        
        # 4. Welch PSD
        ax4 = axes[1, 1]
        for col_name in available_columns:
            psd_df = results[f'{col_name}_psd']
            if not psd_df.empty:
                color = vessel_color if 'Vessel' in col_name else jacket_color
                linestyle = '-' if 'Vessel' in col_name else ':'
                label = 'Vessel End' if 'Vessel' in col_name else 'Jacket End'
                
                ax4.semilogy(psd_df['frequency'], psd_df['power'],
                           color=color, linestyle=linestyle, label=label, alpha=0.8)
        
        ax4.set_xlabel('Frequency (Hz)')
        ax4.set_ylabel('PSD')
        ax4.set_title('Welch Power Spectral Density')
        ax4.legend()
        ax4.grid(True, alpha=0.3, which='both')
        ax4.set_xlim([0, 2.0])
        
        plt.suptitle(f'Fatigue Analysis - {base_name}', fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save plot
        plot_file = output_dir / f"{base_name}_analysis_plot.png"
        plt.savefig(plot_file, dpi=100, bbox_inches='tight')
        plt.close()
        
        logger.info(f"  Saved plot: {plot_file.name}")
        
        return True
        
    except Exception as e:
        logger.error(f"Error processing {file_path}: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_sample():
    """Test with a sample file"""
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    output_dir.mkdir(exist_ok=True)
    
    # Find sample file
    sample_files = list(input_dir.glob("*_Strut1.csv"))
    if not sample_files:
        logger.error("No sample files found")
        return False
    
    sample_file = sample_files[0]
    logger.info(f"\n{'='*60}")
    logger.info(f"TESTING SIMPLIFIED RAINFLOW")
    logger.info(f"{'='*60}")
    logger.info(f"Sample file: {sample_file.name}")
    logger.info(f"Output: Range and Count columns only")
    
    success = process_fatigue_file(sample_file, output_dir)
    
    if success:
        logger.info(f"\n✅ Sample processing successful!")
        
        # Show sample output
        sample_output = output_dir / f"{sample_file.stem}_Tension_Vessel_End_rainflow_simplified.csv"
        if sample_output.exists():
            df = pd.read_csv(sample_output)
            logger.info(f"\nSample rainflow output (first 5 rows):")
            logger.info(f"\n{df.head()}")
            logger.info(f"\nTotal range bins: {len(df)}")
            logger.info(f"Total cycles: {df['Count'].sum():.1f}")
    
    return success

def batch_process():
    """Process all files in batch"""
    input_dir = Path(r"D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue")
    output_dir = input_dir / "rainflow"
    output_dir.mkdir(exist_ok=True)
    
    # Find all Strut files
    all_files = list(input_dir.glob("*_Strut?.csv"))
    
    logger.info(f"\n{'='*60}")
    logger.info(f"BATCH PROCESSING ALL FILES")
    logger.info(f"{'='*60}")
    logger.info(f"Found {len(all_files)} files to process")
    logger.info(f"Output format: Simplified rainflow (Range, Count)")
    
    successful = 0
    failed = 0
    
    for i, file_path in enumerate(all_files, 1):
        logger.info(f"\n[{i}/{len(all_files)}] Processing: {file_path.name}")
        
        if process_fatigue_file(file_path, output_dir):
            successful += 1
        else:
            failed += 1
        
        # Progress update
        if i % 10 == 0:
            logger.info(f"\nProgress: {i}/{len(all_files)} ({i*100/len(all_files):.1f}%)")
            logger.info(f"Success: {successful}, Failed: {failed}")
    
    # Final summary
    logger.info(f"\n{'='*60}")
    logger.info(f"BATCH PROCESSING COMPLETE")
    logger.info(f"{'='*60}")
    logger.info(f"Total files: {len(all_files)}")
    logger.info(f"✅ Successful: {successful}")
    if failed > 0:
        logger.info(f"❌ Failed: {failed}")
    logger.info(f"Output directory: {output_dir}")
    
    return successful, failed

if __name__ == "__main__":
    # Test with sample first
    if test_sample():
        # Proceed with batch
        logger.info("\nProceeding with full batch processing...")
        batch_process()