#!/usr/bin/env python
"""
Test processing with increased window averaging for smoother FFT
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

from digitalmodel.modules.signal_analysis.orcaflex import TimeSeriesAnalyzer

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

def process_single_test_file():
    """Process a single fatigue CSV file with enhanced smoothing"""
    
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
    
    # Create configuration with MORE aggressive smoothing
    config = {
        'analysis': {
            'rainflow': {
                'enable': True, 
                'bin_count': 50
            },
            'fft': {
                'enable': True, 
                'window_size': 1024,  # SMALLER window = MORE windows for averaging
                'overlap': 0.75,  # HIGHER overlap (75%) for more smoothing
                'window_type': 'hann',  # Hanning window
                'frequency_limit': 2.0,  # Limit frequency range to 2 Hz
                'use_window_averaging': True,  # Enable window averaging
                'averaging_method': 'mean',  # Use mean averaging
                'peak_detection': {
                    'enable': True,
                    'n_peaks': 5,
                    'min_height': 0.01
                }
            }
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
    
    try:
        # Read the file
        df = pd.read_csv(test_file)
        total_points = len(df)
        logger.info(f"File shape: {df.shape}")
        
        # Calculate sampling info
        time_data = df[time_column].values
        dt = time_data[1] - time_data[0]
        sampling_rate = 1.0 / dt
        signal_duration = time_data[-1]
        
        # Calculate number of windows
        window_size = config['analysis']['fft']['window_size']
        overlap = config['analysis']['fft']['overlap']
        step_size = int(window_size * (1 - overlap))
        num_windows = (total_points - window_size) // step_size + 1
        
        logger.info(f"Sampling rate: {sampling_rate:.2f} Hz")
        logger.info(f"Signal duration: {signal_duration:.1f} seconds")
        logger.info(f"Total data points: {total_points:,}")
        logger.info(f"FFT window size: {window_size} samples")
        logger.info(f"Overlap: {overlap*100:.0f}%")
        logger.info(f"Number of windows for averaging: {num_windows}")
        
        # Process the file
        logger.info(f"Processing with {num_windows} windows for smoother FFT...")
        results = analyzer.process_file(test_file)
        
        # Create base filename
        base_name = test_file.stem
        
        # Create figure with multiple FFT comparisons
        fig = plt.figure(figsize=(18, 14))
        
        # Add main title
        fig.suptitle(f'FFT Smoothing Analysis: {base_name}\n⚠️ ANALYSIS USES ALL {total_points:,} DATA POINTS', 
                    fontsize=14, fontweight='bold')
        
        # We'll create comparison plots for each column
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
                    ax1 = plt.subplot(4, 3, col_idx * 3 + 1)
                    if 'histogram' in col_results.get('rainflow', {}):
                        histogram = col_results['rainflow']['histogram']
                        if isinstance(histogram, pd.DataFrame) and len(histogram) > 0:
                            centers = (histogram['range_min'] + histogram['range_max']) / 2
                            ax1.bar(centers, histogram['count'], 
                                   width=(histogram['range_max'] - histogram['range_min']).mean(),
                                   alpha=0.7, edgecolor='black', color='steelblue')
                    ax1.set_xlabel('Range')
                    ax1.set_ylabel('Count')
                    ax1.set_title(f'Rainflow - {col_name}')
                    ax1.grid(True, alpha=0.3)
            
            # Plot different FFT results for comparison
            freq_limit = 2.0
            
            # 1. Standard FFT (if available)
            ax2 = plt.subplot(4, 3, col_idx * 3 + 2)
            if 'spectrum' in col_results.get('spectral', {}):
                spectrum = col_results['spectral']['spectrum']
                if isinstance(spectrum, pd.DataFrame) and len(spectrum) > 0:
                    mask = spectrum['frequency'] <= freq_limit
                    ax2.semilogy(spectrum.loc[mask, 'frequency'], 
                               spectrum.loc[mask, 'power'],
                               alpha=0.6, color='red', linewidth=0.8, label='Standard FFT')
            
            # 2. Window-averaged FFT (should be smoother)
            if 'window_fft' in col_results.get('spectral', {}):
                window_fft = col_results['spectral']['window_fft']
                if isinstance(window_fft, pd.DataFrame) and len(window_fft) > 0:
                    # Save the smooth FFT
                    fft_filename = f"{base_name}_{safe_col_name}_fft.csv"
                    fft_path = output_dir / fft_filename
                    window_fft.to_csv(fft_path, index=False)
                    logger.info(f"Saved: {fft_filename} (smoothed with {num_windows} windows)")
                    
                    mask = window_fft['frequency'] <= freq_limit
                    ax2.semilogy(window_fft.loc[mask, 'frequency'], 
                               window_fft.loc[mask, 'power'],
                               alpha=0.9, color='darkgreen', linewidth=1.5, 
                               label=f'Window Avg ({num_windows} windows)')
            
            ax2.set_xlabel('Frequency (Hz)')
            ax2.set_ylabel('Power')
            ax2.set_title(f'FFT Comparison - {col_name}')
            ax2.legend(loc='upper right', fontsize=8)
            ax2.grid(True, alpha=0.3, which='both')
            ax2.set_xlim([0, freq_limit])
            
            # Add smoothing info
            ax2.text(0.02, 0.98, f'Window: {window_size}\nOverlap: {overlap*100:.0f}%\nWindows: {num_windows}', 
                    transform=ax2.transAxes, fontsize=8,
                    verticalalignment='top', 
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
            
            # 3. Welch PSD (alternative smoothing method)
            ax3 = plt.subplot(4, 3, col_idx * 3 + 3)
            if 'psd' in col_results.get('spectral', {}):
                psd = col_results['spectral']['psd']
                if isinstance(psd, pd.DataFrame) and len(psd) > 0:
                    psd_mask = psd['frequency'] <= freq_limit
                    ax3.semilogy(psd.loc[psd_mask, 'frequency'],
                               psd.loc[psd_mask, 'power'],
                               alpha=0.8, color='darkblue', linewidth=1.5)
                    ax3.set_xlabel('Frequency (Hz)')
                    ax3.set_ylabel('PSD')
                    ax3.set_title(f'Welch PSD - {col_name}')
                    ax3.grid(True, alpha=0.3, which='both')
                    ax3.set_xlim([0, freq_limit])
        
        # Add summary info
        ax_summary = plt.subplot(4, 3, 7)
        ax_summary.axis('off')
        
        summary_text = f"DATA ANALYSIS SUMMARY\n"
        summary_text += f"{'='*25}\n"
        summary_text += f"File: {test_file.name}\n"
        summary_text += f"Duration: {signal_duration:.1f} s\n"
        summary_text += f"Points: {total_points:,}\n"
        summary_text += f"Sampling: {sampling_rate:.2f} Hz\n\n"
        summary_text += f"FFT SMOOTHING:\n"
        summary_text += f"Window size: {window_size}\n"
        summary_text += f"Overlap: {overlap*100:.0f}%\n"
        summary_text += f"Windows: {num_windows}\n"
        
        ax_summary.text(0.1, 0.9, summary_text, transform=ax_summary.transAxes,
                       fontsize=10, verticalalignment='top', fontfamily='monospace',
                       bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.3))
        
        # Add time series sample
        ax_time = plt.subplot(4, 3, 8)
        sample_points = 2000
        time_plot = df[time_column].values[:sample_points]
        for col_name in data_columns:
            if col_name in df.columns:
                ax_time.plot(time_plot, df[col_name].values[:sample_points], 
                           label=col_name, alpha=0.7, linewidth=1)
        ax_time.set_xlabel('Time (s)')
        ax_time.set_ylabel('Tension')
        ax_time.set_title(f'TIME SERIES SAMPLE\n(Showing {sample_points} of {total_points:,} points)')
        ax_time.legend(loc='best', fontsize=8)
        ax_time.grid(True, alpha=0.3)
        
        # Add analysis note
        ax_note = plt.subplot(4, 3, 9)
        ax_note.axis('off')
        note_text = "SMOOTHING ANALYSIS:\n" + "="*25 + "\n\n"
        
        if num_windows > 50:
            note_text += f"✓ Using {num_windows} windows\n"
            note_text += "✓ High overlap (75%)\n"
            note_text += "✓ Maximum smoothing applied\n\n"
        else:
            note_text += f"⚠️ Only {num_windows} windows\n"
            note_text += "⚠️ Limited smoothing possible\n\n"
        
        note_text += "If spectrum still spiky:\n"
        note_text += "• May be actual system\n"
        note_text += "  resonances/response\n"
        note_text += "• Not noise - real features\n"
        note_text += "• Important for fatigue\n"
        note_text += "  analysis\n\n"
        note_text += "Alternative: Use Welch PSD\n"
        note_text += "for different smoothing"
        
        ax_note.text(0.1, 0.9, note_text, transform=ax_note.transAxes,
                    fontsize=9, verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='lightyellow', alpha=0.5))
        
        # Try even more aggressive smoothing as a test
        ax_test = plt.subplot(4, 3, 10)
        ax_test.set_title('Extra Smoothing Test')
        
        # Get first column data for testing
        if data_columns and data_columns[0] in results.get('columns', {}):
            col_results = results['columns'][data_columns[0]]
            
            # Apply additional smoothing to the window FFT
            if 'window_fft' in col_results.get('spectral', {}):
                window_fft = col_results['spectral']['window_fft']
                if isinstance(window_fft, pd.DataFrame) and len(window_fft) > 0:
                    mask = window_fft['frequency'] <= freq_limit
                    freqs = window_fft.loc[mask, 'frequency'].values
                    powers = window_fft.loc[mask, 'power'].values
                    
                    # Apply moving average for extra smoothing
                    from scipy.ndimage import uniform_filter1d
                    
                    # Different smoothing levels
                    smooth_5 = uniform_filter1d(powers, size=5)
                    smooth_10 = uniform_filter1d(powers, size=10)
                    smooth_20 = uniform_filter1d(powers, size=20)
                    
                    ax_test.semilogy(freqs, powers, alpha=0.3, color='gray', 
                                    linewidth=0.5, label='Original')
                    ax_test.semilogy(freqs, smooth_5, alpha=0.6, color='blue', 
                                    linewidth=1, label='5-pt smooth')
                    ax_test.semilogy(freqs, smooth_10, alpha=0.7, color='green', 
                                    linewidth=1.2, label='10-pt smooth')
                    ax_test.semilogy(freqs, smooth_20, alpha=0.8, color='red', 
                                    linewidth=1.5, label='20-pt smooth')
                    
                    ax_test.set_xlabel('Frequency (Hz)')
                    ax_test.set_ylabel('Power')
                    ax_test.legend(loc='upper right', fontsize=7)
                    ax_test.grid(True, alpha=0.3, which='both')
                    ax_test.set_xlim([0, freq_limit])
        
        plt.tight_layout()
        
        # Save plot
        plot_filename = f"{base_name}_analysis_plot.png"
        plot_path = output_dir / plot_filename
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        logger.info(f"Saved plot: {plot_filename}")
        plt.close()
        
        # Summary
        logger.info("\n" + "="*60)
        logger.info("ENHANCED SMOOTHING TEST RESULTS")
        logger.info("="*60)
        logger.info(f"FFT Smoothing Parameters:")
        logger.info(f"  - Window size: {window_size} samples (smaller = more windows)")
        logger.info(f"  - Overlap: {overlap*100:.0f}% (higher = more smoothing)")
        logger.info(f"  - Number of windows: {num_windows}")
        logger.info(f"  - Averaging method: Mean")
        
        if num_windows > 50:
            logger.info(f"\n✅ Good smoothing with {num_windows} windows")
            logger.info("If still spiky, peaks are likely real system response")
        else:
            logger.info(f"\n⚠️ Limited smoothing with only {num_windows} windows")
            logger.info("Consider using even smaller window size")
        
        logger.info("\nGenerated files:")
        for col_name in data_columns:
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            logger.info(f"  - {base_name}_{safe_col_name}_rainflow.csv")
            logger.info(f"  - {base_name}_{safe_col_name}_fft.csv (max smoothing)")
        logger.info(f"  - {plot_filename}")
        
        logger.info("\nNOTE: If spectrum remains spiky after smoothing,")
        logger.info("      these are likely real system resonances/responses")
        logger.info("      important for fatigue analysis, not noise.")
        
    except Exception as e:
        logger.error(f"Error processing file: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    process_single_test_file()