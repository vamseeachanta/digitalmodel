#!/usr/bin/env python
"""
Test processing with combined vessel/jacket plots using different line styles for visibility
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
    """Process a single fatigue CSV file with combined plots and distinct line styles"""
    
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
    
    # Create configuration with aggressive smoothing
    config = {
        'analysis': {
            'rainflow': {
                'enable': True, 
                'bin_count': 50
            },
            'fft': {
                'enable': True, 
                'window_size': 1024,  # Small window = more windows for averaging
                'overlap': 0.75,  # High overlap for smoothing
                'window_type': 'hann',
                'frequency_limit': 2.0,
                'use_window_averaging': True,
                'averaging_method': 'mean',
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
        
        logger.info(f"Total data points: {total_points:,}")
        logger.info(f"Number of FFT windows: {num_windows}")
        
        # Process the file
        logger.info(f"Processing with {num_windows} windows for smoother FFT...")
        results = analyzer.process_file(test_file)
        
        # Create base filename
        base_name = test_file.stem
        
        # Create figure with COMBINED plots
        fig = plt.figure(figsize=(16, 10))
        
        # Add main title
        fig.suptitle(f'Combined Analysis: {base_name}\n⚠️ ANALYSIS USES ALL {total_points:,} DATA POINTS', 
                    fontsize=14, fontweight='bold')
        
        # Define line styles for better visibility
        vessel_style = {'linestyle': '-', 'linewidth': 2.0, 'alpha': 0.8}  # Solid line
        jacket_style = {'linestyle': '--', 'linewidth': 2.0, 'alpha': 0.8}  # Dashed line
        
        # Store data for combined plots
        rainflow_data = {}
        fft_data = {}
        psd_data = {}
        stats_data = {}
        
        # Process and save CSV files
        for col_name in data_columns:
            if col_name not in results.get('columns', {}):
                continue
                
            col_results = results['columns'][col_name]
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            
            # Store statistics
            stats_data[col_name] = {
                'stats': col_results.get('statistics', {}),
                'rainflow_stats': col_results.get('rainflow', {}).get('statistics', {})
            }
            
            # Save and store rainflow data
            if 'cycles' in col_results.get('rainflow', {}):
                cycles = col_results['rainflow']['cycles']
                if isinstance(cycles, pd.DataFrame) and len(cycles) > 0:
                    rainflow_filename = f"{base_name}_{safe_col_name}_rainflow.csv"
                    rainflow_path = output_dir / rainflow_filename
                    cycles.to_csv(rainflow_path, index=False)
                    logger.info(f"Saved: {rainflow_filename}")
                    
                    if 'histogram' in col_results.get('rainflow', {}):
                        rainflow_data[col_name] = col_results['rainflow']['histogram']
            
            # Save and store FFT data
            if 'window_fft' in col_results.get('spectral', {}):
                window_fft = col_results['spectral']['window_fft']
                if isinstance(window_fft, pd.DataFrame) and len(window_fft) > 0:
                    fft_filename = f"{base_name}_{safe_col_name}_fft.csv"
                    fft_path = output_dir / fft_filename
                    window_fft.to_csv(fft_path, index=False)
                    logger.info(f"Saved: {fft_filename}")
                    fft_data[col_name] = window_fft
            
            # Store PSD data
            if 'psd' in col_results.get('spectral', {}):
                psd_data[col_name] = col_results['spectral']['psd']
        
        # 1. COMBINED Rainflow Histogram
        ax1 = plt.subplot(2, 3, 1)
        colors = ['steelblue', 'coral']
        patterns = ['/', '\\']  # Different hatch patterns for bars
        for idx, (col_name, histogram) in enumerate(rainflow_data.items()):
            if isinstance(histogram, pd.DataFrame) and len(histogram) > 0:
                centers = (histogram['range_min'] + histogram['range_max']) / 2
                label = 'Vessel End' if 'Vessel' in col_name else 'Jacket End'
                ax1.bar(centers, histogram['count'], 
                       width=(histogram['range_max'] - histogram['range_min']).mean(),
                       alpha=0.6, edgecolor='black', color=colors[idx],
                       hatch=patterns[idx], label=label)
        ax1.set_xlabel('Range')
        ax1.set_ylabel('Count')
        ax1.set_title('Rainflow Histogram - Combined')
        ax1.legend(loc='upper right')
        ax1.grid(True, alpha=0.3)
        
        # 2. COMBINED FFT Spectrum with different line styles
        ax2 = plt.subplot(2, 3, 2)
        freq_limit = 2.0
        
        # First pass to find y-axis limits
        all_powers = []
        for col_name, spectrum in fft_data.items():
            if isinstance(spectrum, pd.DataFrame):
                mask = spectrum['frequency'] <= freq_limit
                powers = spectrum.loc[mask, 'power'].values
                all_powers.extend(powers[powers > 0])
        
        if all_powers:
            y_min = np.percentile(all_powers, 5) * 0.5
            y_max = np.percentile(all_powers, 99.5) * 2
        
        # Plot FFT data with different line styles
        for idx, (col_name, spectrum) in enumerate(fft_data.items()):
            if isinstance(spectrum, pd.DataFrame):
                mask = spectrum['frequency'] <= freq_limit
                label = 'Vessel End (solid)' if 'Vessel' in col_name else 'Jacket End (dashed)'
                style = vessel_style if 'Vessel' in col_name else jacket_style
                color = 'darkgreen' if 'Vessel' in col_name else 'darkred'
                
                ax2.semilogy(spectrum.loc[mask, 'frequency'], 
                           spectrum.loc[mask, 'power'],
                           color=color, label=label, **style)
        
        ax2.set_xlabel('Frequency (Hz)')
        ax2.set_ylabel('Power')
        ax2.set_title(f'FFT Spectrum - Combined\n({num_windows} windows, 75% overlap)')
        ax2.legend(loc='upper right')
        ax2.grid(True, alpha=0.3, which='both')
        ax2.set_xlim([0, freq_limit])
        if all_powers:
            ax2.set_ylim([y_min, y_max])
        
        # Add smoothing info
        ax2.text(0.02, 0.98, f'Window: {window_size}\nOverlap: 75%\nWindows: {num_windows}', 
                transform=ax2.transAxes, fontsize=8,
                verticalalignment='top', 
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        # 3. COMBINED Welch PSD with different line styles
        ax3 = plt.subplot(2, 3, 3)
        
        # Find PSD y-axis limits
        all_psd_powers = []
        for col_name, psd in psd_data.items():
            if isinstance(psd, pd.DataFrame):
                mask = psd['frequency'] <= freq_limit
                powers = psd.loc[mask, 'power'].values
                all_psd_powers.extend(powers[powers > 0])
        
        if all_psd_powers:
            psd_y_min = np.percentile(all_psd_powers, 5) * 0.5
            psd_y_max = np.percentile(all_psd_powers, 99.5) * 2
        
        for idx, (col_name, psd) in enumerate(psd_data.items()):
            if isinstance(psd, pd.DataFrame):
                mask = psd['frequency'] <= freq_limit
                label = 'Vessel End (solid)' if 'Vessel' in col_name else 'Jacket End (dotted)'
                if 'Vessel' in col_name:
                    style = {'linestyle': '-', 'linewidth': 2.0, 'alpha': 0.8}
                    color = 'darkblue'
                else:
                    style = {'linestyle': ':', 'linewidth': 2.5, 'alpha': 0.8}  # Dotted line
                    color = 'darkorange'
                
                ax3.semilogy(psd.loc[mask, 'frequency'],
                           psd.loc[mask, 'power'],
                           color=color, label=label, **style)
        
        ax3.set_xlabel('Frequency (Hz)')
        ax3.set_ylabel('PSD')
        ax3.set_title('Welch Power Spectral Density - Combined')
        ax3.legend(loc='upper right')
        ax3.grid(True, alpha=0.3, which='both')
        ax3.set_xlim([0, freq_limit])
        if all_psd_powers:
            ax3.set_ylim([psd_y_min, psd_y_max])
        
        # 4. COMBINED Time Series Sample with different line styles
        ax4 = plt.subplot(2, 3, 4)
        sample_points = 2000
        time_plot = df[time_column].values[:sample_points]
        
        for idx, col_name in enumerate(data_columns):
            if col_name in df.columns:
                if 'Vessel' in col_name:
                    label = 'Vessel End (solid)'
                    style = {'linestyle': '-', 'linewidth': 1.5, 'alpha': 0.8}
                    color = 'green'
                else:
                    label = 'Jacket End (dashed)'
                    style = {'linestyle': '--', 'linewidth': 1.5, 'alpha': 0.8}
                    color = 'red'
                
                ax4.plot(time_plot, df[col_name].values[:sample_points], 
                        label=label, color=color, **style)
        
        ax4.set_xlabel('Time (s)')
        ax4.set_ylabel('Tension')
        ax4.set_title(f'Time Series Sample (First {sample_points} of {total_points:,} points)')
        ax4.legend(loc='best')
        ax4.grid(True, alpha=0.3)
        
        # Add note about full analysis
        ax4.text(0.5, 0.02, f'⚠️ Analysis uses ALL {total_points:,} points', 
                transform=ax4.transAxes, fontsize=9, fontweight='bold',
                horizontalalignment='center',
                bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.7))
        
        # 5. Statistics Comparison Table
        ax5 = plt.subplot(2, 3, 5)
        ax5.axis('off')
        
        # Create comparison table with line style legend
        table_text = "STATISTICS COMPARISON\n" + "="*30 + "\n\n"
        table_text += "LINE STYLES:\n"
        table_text += "  Vessel End: —— (solid)\n"
        table_text += "  Jacket End: - - (dashed/dotted)\n\n"
        table_text += f"{'Parameter':<20} {'Vessel End':>12} {'Jacket End':>12}\n"
        table_text += "-"*44 + "\n"
        
        if stats_data:
            vessel_stats = stats_data.get('Tension (Vessel End)', {})
            jacket_stats = stats_data.get('Tension (Jacket End)', {})
            
            # Basic statistics
            v_stats = vessel_stats.get('stats', {})
            j_stats = jacket_stats.get('stats', {})
            
            table_text += f"{'Mean':.<20} {v_stats.get('mean', 0):>12.2f} {j_stats.get('mean', 0):>12.2f}\n"
            table_text += f"{'Std Dev':.<20} {v_stats.get('std', 0):>12.2f} {j_stats.get('std', 0):>12.2f}\n"
            table_text += f"{'Maximum':.<20} {v_stats.get('max', 0):>12.2f} {j_stats.get('max', 0):>12.2f}\n"
            table_text += f"{'Minimum':.<20} {v_stats.get('min', 0):>12.2f} {j_stats.get('min', 0):>12.2f}\n"
            
            # Rainflow statistics
            v_rf = vessel_stats.get('rainflow_stats', {})
            j_rf = jacket_stats.get('rainflow_stats', {})
            
            table_text += "\nRAINFLOW ANALYSIS\n"
            table_text += f"{'Total Cycles':.<20} {v_rf.get('total_cycles', 0):>12.0f} {j_rf.get('total_cycles', 0):>12.0f}\n"
            table_text += f"{'Max Range':.<20} {v_rf.get('max_range', 0):>12.2f} {j_rf.get('max_range', 0):>12.2f}\n"
            table_text += f"{'Mean Range':.<20} {v_rf.get('mean_range', 0):>12.2f} {j_rf.get('mean_range', 0):>12.2f}\n"
        
        ax5.text(0.1, 0.9, table_text, transform=ax5.transAxes,
                fontsize=9, verticalalignment='top', fontfamily='monospace',
                bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.3))
        
        # 6. Processing Information
        ax6 = plt.subplot(2, 3, 6)
        ax6.axis('off')
        
        info_text = "PROCESSING INFORMATION\n" + "="*30 + "\n\n"
        info_text += f"File: {test_file.name}\n"
        info_text += f"Duration: {signal_duration:.1f} seconds\n"
        info_text += f"Points: {total_points:,}\n"
        info_text += f"Sampling: {sampling_rate:.2f} Hz\n\n"
        
        info_text += "FFT SMOOTHING:\n"
        info_text += f"• Window size: {window_size} samples\n"
        info_text += f"• Overlap: 75%\n"
        info_text += f"• Windows averaged: {num_windows}\n"
        info_text += f"• Result: Smooth spectrum\n\n"
        
        info_text += "VISUALIZATION:\n"
        info_text += "• Vessel End: Solid lines\n"
        info_text += "• Jacket End: Dashed/dotted\n"
        info_text += "• Optimized y-axis scaling\n\n"
        
        info_text += "NOTE: Peaks represent actual\n"
        info_text += "system response characteristics"
        
        ax6.text(0.1, 0.9, info_text, transform=ax6.transAxes,
                fontsize=9, verticalalignment='top',
                bbox=dict(boxstyle='round', facecolor='lightyellow', alpha=0.5))
        
        plt.tight_layout()
        
        # Save plot
        plot_filename = f"{base_name}_analysis_plot.png"
        plot_path = output_dir / plot_filename
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        logger.info(f"Saved plot: {plot_filename}")
        plt.close()
        
        # Summary
        logger.info("\n" + "="*60)
        logger.info("COMBINED ANALYSIS WITH DISTINCT LINE STYLES")
        logger.info("="*60)
        logger.info(f"Analysis Configuration:")
        logger.info(f"  - Data points: ALL {total_points:,} points")
        logger.info(f"  - FFT windows: {num_windows} (with 75% overlap)")
        logger.info(f"  - Y-axis: Auto-scaled for optimal visibility")
        logger.info(f"  - Line styles:")
        logger.info(f"    • Vessel End: Solid lines (—)")
        logger.info(f"    • Jacket End: Dashed (--) / Dotted (··)")
        
        logger.info("\nGenerated files:")
        for col_name in data_columns:
            safe_col_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            logger.info(f"  - {base_name}_{safe_col_name}_rainflow.csv")
            logger.info(f"  - {base_name}_{safe_col_name}_fft.csv")
        logger.info(f"  - {plot_filename} (combined plots with distinct line styles)")
        
        logger.info("\n✅ Vessel End and Jacket End combined with distinct line styles")
        logger.info("✅ Better visibility with solid vs dashed/dotted lines")
        logger.info("✅ Y-axis optimized for peak visibility")
        logger.info("✅ Ready to process all 544 files")
        
    except Exception as e:
        logger.error(f"Error processing file: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    process_single_test_file()