#!/usr/bin/env python
"""
Rainflow Counting and FFT Analysis Tool
Version: 1.0.0
Date: 2025-01-23

This script performs rainflow cycle counting and FFT analysis on time series data,
generating visualizations and CSV output files.
"""

import sys
import os
from pathlib import Path
import pandas as pd
import numpy as np
import yaml
import logging
from typing import Dict, List, Tuple, Optional, Union
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D
from scipy import signal
from scipy.signal import find_peaks, welch
import warnings
warnings.filterwarnings('ignore')

# Try different rainflow import methods
try:
    from rainflow import extract_cycles, count_cycles
    RAINFLOW_AVAILABLE = True
except ImportError:
    try:
        import rainflow
        extract_cycles = rainflow.extract_cycles if hasattr(rainflow, 'extract_cycles') else None
        count_cycles = rainflow.count_cycles if hasattr(rainflow, 'count_cycles') else None
        RAINFLOW_AVAILABLE = True
    except ImportError:
        RAINFLOW_AVAILABLE = False
        print("Warning: rainflow package not installed. Install with: pip install rainflow")


class RainflowAnalyzer:
    """Main class for rainflow counting and FFT analysis"""
    
    def __init__(self, config_path: str):
        """Initialize the analyzer with configuration file"""
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self.setup_logging()
        self.results = {}
        
    def _load_config(self) -> dict:
        """Load configuration from YAML file"""
        with open(self.config_path, 'r') as f:
            return yaml.safe_load(f)
    
    def setup_logging(self):
        """Setup logging configuration"""
        log_level = self.config.get('logging', {}).get('level', 'INFO')
        logging.basicConfig(
            level=getattr(logging, log_level),
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
        
    def run(self):
        """Main execution method"""
        self.logger.info(f"Starting Rainflow Analysis - {self.config['project']['name']}")
        
        # Get input files
        input_files = self._get_input_files()
        self.logger.info(f"Found {len(input_files)} input files")
        
        # Process each file
        for file_path in input_files:
            try:
                self._process_file(file_path)
            except Exception as e:
                self.logger.error(f"Error processing {file_path}: {str(e)}")
                if not self.config.get('processing', {}).get('batch', {}).get('continue_on_error', True):
                    raise
        
        # Generate aggregate summary
        self._generate_summary()
        
        self.logger.info("Analysis complete!")
        
    def _get_input_files(self) -> List[Path]:
        """Get list of input files based on configuration"""
        data_folder_str = self.config['input_data']['timetrace']['data_folder']
        
        # Check if it's an absolute path
        if Path(data_folder_str).is_absolute():
            data_folder = Path(data_folder_str)
        else:
            # Relative path - use relative to config file location
            script_dir = Path(self.config_path).parent.parent
            data_folder = script_dir / data_folder_str
            
        pattern = self.config['input_data']['timetrace'].get('filter_pattern', '*.csv')
        
        if not data_folder.exists():
            self.logger.warning(f"Data folder does not exist: {data_folder}")
            return []
        
        files = list(data_folder.glob(pattern))
        self.logger.info(f"Searching in: {data_folder}")
        self.logger.info(f"Pattern: {pattern}")
        self.logger.info(f"Found {len(files)} files")
        if len(files) <= 10:
            self.logger.info(f"Files: {[f.name for f in files]}")
        
        return files
    
    def _process_file(self, file_path: Path):
        """Process a single input file"""
        self.logger.info(f"Processing: {file_path.name}")
        
        # Parse filename for metadata
        file_info = self._parse_filename(file_path)
        
        # Load data
        data = self._load_data(file_path)
        
        # Perform rainflow counting
        rainflow_results = self._perform_rainflow(data, file_info)
        
        # Perform FFT analysis
        fft_results = self._perform_fft(data, file_info)
        
        # Generate visualizations
        self._generate_visualizations(data, rainflow_results, fft_results, file_info)
        
        # Save results
        self._save_results(rainflow_results, fft_results, file_info)
        
    def _parse_filename(self, file_path: Path) -> dict:
        """Parse filename to extract metadata"""
        name = file_path.stem
        parts = name.split('_')
        
        # Try to extract FC number and strut number
        fc_num = None
        strut_num = None
        config_name = parts[0] if parts else 'unknown'
        
        for part in parts:
            if part.startswith('FC'):
                fc_num = part[2:]
            elif part.startswith('Strut'):
                strut_num = part[5:]
        
        return {
            'filename': name,
            'config': config_name,
            'fc_number': fc_num or '000',
            'strut_number': strut_num or '0',
            'full_path': file_path
        }
    
    def _load_data(self, file_path: Path) -> pd.DataFrame:
        """Load time series data from CSV file"""
        data = pd.read_csv(file_path)
        
        # Get column names from config
        data_col = self.config['input_data']['timetrace']['data_column']
        time_col = self.config['input_data']['timetrace'].get('time_column', 'time')
        
        # Check if columns exist
        if data_col not in data.columns:
            # Try to find a similar column
            for col in data.columns:
                if 'tension' in col.lower() or 'stress' in col.lower():
                    self.logger.warning(f"Column '{data_col}' not found, using '{col}'")
                    data_col = col
                    break
        
        # Ensure we have the required columns
        if data_col not in data.columns:
            raise ValueError(f"Data column '{data_col}' not found in {file_path}")
        
        # Create time column if not present
        if time_col not in data.columns:
            dt = self.config['input_data']['timetrace']['sampling']['dt']
            data[time_col] = np.arange(len(data)) * dt
            self.logger.info(f"Created time column with dt={dt}s")
        
        return data[[time_col, data_col]].rename(columns={
            time_col: 'time',
            data_col: 'tension'
        })
    
    def _perform_rainflow(self, data: pd.DataFrame, file_info: dict) -> dict:
        """Perform rainflow cycle counting"""
        if not RAINFLOW_AVAILABLE:
            self.logger.warning("Rainflow package not available, skipping")
            return {}
        
        tension = data['tension'].values
        
        # Remove mean if configured
        if self.config['input_data']['preprocessing']['remove_mean']:
            tension = tension - np.mean(tension)
        
        # Perform rainflow counting
        binning_config = self.config['rainflow_counting']['binning']
        
        # Try different methods based on what's available
        cycles = []
        try:
            if extract_cycles:
                # extract_cycles returns: (range, mean, count, i_start, i_end)
                cycles_raw = list(extract_cycles(tension))
                cycles = [(abs(rng), mean, count, i_start, i_end) 
                         for rng, mean, count, i_start, i_end in cycles_raw]
            elif count_cycles:
                # count_cycles returns: (range, count)
                cycles_raw = list(count_cycles(tension))
                cycles = [(abs(rng), 0, count, 0, 0) for rng, count in cycles_raw]
        except Exception as e:
            self.logger.error(f"Error in rainflow counting: {str(e)}")
            # Try alternative approach
            try:
                import rainflow
                cycles_raw = list(rainflow.count_cycles(tension))
                cycles = [(abs(rng), 0, count, 0, 0) for rng, count in cycles_raw]
            except:
                return {}
        
        if not cycles:
            self.logger.warning("No cycles found")
            return {}
        
        # Extract ranges and means
        ranges = np.array([c[0] for c in cycles])
        means = np.array([c[1] for c in cycles])
        counts = np.array([c[2] for c in cycles])
        
        # Bin the results
        num_bins = binning_config['num_bins']
        min_range = binning_config['min_range']
        max_range = binning_config['max_range']
        
        bin_edges = np.linspace(min_range, max_range, num_bins + 1)
        bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2
        
        # Count cycles in each bin
        binned_counts = np.zeros(num_bins)
        binned_means = np.zeros(num_bins)
        
        for i, (rng, mean, count) in enumerate(zip(ranges, means, counts)):
            if rng >= min_range and rng <= max_range:
                bin_idx = np.digitize(rng, bin_edges) - 1
                if 0 <= bin_idx < num_bins:
                    binned_counts[bin_idx] += count
                    binned_means[bin_idx] += mean * count
        
        # Calculate mean for each bin
        for i in range(num_bins):
            if binned_counts[i] > 0:
                binned_means[i] /= binned_counts[i]
        
        # Calculate annual scaling
        time_duration = data['time'].iloc[-1] - data['time'].iloc[0]
        hours_in_trace = time_duration / 3600.0
        annual_hours = self.config['rainflow_counting']['cycle_scaling']['annual_hours']
        scale_factor = annual_hours / hours_in_trace if hours_in_trace > 0 else 1.0
        
        annual_counts = binned_counts * scale_factor
        
        return {
            'ranges': ranges,
            'means': means,
            'counts': counts,
            'bin_centers': bin_centers,
            'bin_edges': bin_edges,
            'binned_counts': binned_counts,
            'binned_means': binned_means,
            'annual_counts': annual_counts,
            'scale_factor': scale_factor,
            'time_duration': time_duration,
            'total_cycles': np.sum(counts),
            'max_range': np.max(ranges) if len(ranges) > 0 else 0,
            'mean_range': np.mean(ranges) if len(ranges) > 0 else 0
        }
    
    def _perform_fft(self, data: pd.DataFrame, file_info: dict) -> dict:
        """Perform FFT analysis using Welch's method"""
        tension = data['tension'].values
        time = data['time'].values
        
        # Calculate sampling frequency
        dt = np.mean(np.diff(time))
        fs = 1.0 / dt
        
        # Get FFT configuration
        fft_config = self.config['fft_analysis']
        window_config = fft_config['window']
        
        # Window parameters
        window_type = window_config['type']
        # Fix window type for scipy
        if window_type.lower() == 'hanning':
            window_type = 'hann'
            
        window_length_sec = window_config['length_seconds']
        overlap_percent = window_config['overlap_percent'] / 100.0
        
        # Adjust window length if it exceeds data length
        data_length_sec = time[-1] - time[0]
        if window_length_sec > data_length_sec:
            window_length_sec = data_length_sec * 0.5
            self.logger.warning(f"Window length adjusted to {window_length_sec:.1f}s (50% of data)")
        
        # Convert to samples
        nperseg = int(window_length_sec * fs)
        noverlap = int(nperseg * overlap_percent)
        
        # Perform Welch's method for PSD
        frequencies, psd = welch(
            tension,
            fs=fs,
            window=window_type,
            nperseg=nperseg,
            noverlap=noverlap,
            scaling='density',
            detrend=fft_config['spectral']['detrend']
        )
        
        # Calculate amplitude spectrum
        amplitude = np.sqrt(psd)
        
        # Find dominant frequency
        dominant_idx = np.argmax(psd[1:]) + 1  # Skip DC
        dominant_freq = frequencies[dominant_idx]
        dominant_period = 1.0 / dominant_freq if dominant_freq > 0 else np.inf
        
        # Calculate periods for all frequencies
        periods = np.where(frequencies > 0, 1.0 / frequencies, np.inf)
        
        return {
            'frequencies': frequencies,
            'periods': periods,
            'psd': psd,
            'amplitude': amplitude,
            'dominant_frequency': dominant_freq,
            'dominant_period': dominant_period,
            'fs': fs,
            'window_type': window_type,
            'nperseg': nperseg
        }
    
    def _generate_visualizations(self, data: pd.DataFrame, rainflow_results: dict, 
                                fft_results: dict, file_info: dict):
        """Generate all visualization plots"""
        if not self.config['visualization']['timeseries_plot']['enabled']:
            return
            
        # Handle absolute or relative output paths
        base_folder = self.config['output']['folders'].get('base', '')
        if base_folder and Path(base_folder).is_absolute():
            base_path = Path(base_folder)
        else:
            base_path = Path(self.config_path).parent.parent
            if base_folder:
                base_path = base_path / base_folder
                
        viz_folder = base_path / self.config['output']['folders']['visualizations']
        viz_folder.mkdir(parents=True, exist_ok=True)
        
        # Generate base filename
        pattern = self.config['output']['file_naming']['pattern']
        # Convert to proper types for formatting
        try:
            fc_num = int(file_info['fc_number']) if file_info['fc_number'].isdigit() else file_info['fc_number']
        except:
            fc_num = file_info['fc_number']
        
        base_name = pattern.format(
            config=file_info['config'],
            fc_number=fc_num,
            strut_number=file_info['strut_number'],
            type=''
        ).rstrip('_')
        
        # 1. Time trace plot
        if self.config['visualization']['timeseries_plot']['enabled']:
            self._plot_time_trace(data, viz_folder / f"{base_name}_time_trace.png")
        
        # 2. Rainflow matrix plot
        if rainflow_results and self.config['visualization']['rainflow_plot']['enabled']:
            self._plot_rainflow_matrix(rainflow_results, viz_folder / f"{base_name}_rainflow_matrix.png")
        
        # 3. Cycle histogram
        if rainflow_results:
            self._plot_cycle_histogram(rainflow_results, viz_folder / f"{base_name}_cycle_histogram.png")
        
        # 4. FFT spectrum with period domain
        if fft_results and self.config['visualization']['fft_plot']['enabled']:
            self._plot_fft_spectrum(fft_results, viz_folder / f"{base_name}_fft_spectrum.png")
    
    def _plot_time_trace(self, data: pd.DataFrame, output_path: Path):
        """Plot time trace with peaks and valleys"""
        fig, ax = plt.subplots(figsize=(14, 6))
        
        time = data['time'].values
        tension = data['tension'].values
        
        ax.plot(time, tension, 'b-', linewidth=0.5, alpha=0.7)
        
        # Find and mark peaks and valleys
        peaks, _ = find_peaks(tension)
        valleys, _ = find_peaks(-tension)
        
        if len(peaks) > 0:
            ax.plot(time[peaks], tension[peaks], 'r^', markersize=4, label='Peaks')
        if len(valleys) > 0:
            ax.plot(time[valleys], tension[valleys], 'gv', markersize=4, label='Valleys')
        
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Tension (kN)')
        ax.set_title('Time Trace with Identified Peaks and Valleys')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        plt.tight_layout()
        plt.savefig(output_path, dpi=150, bbox_inches='tight')
        plt.close()
    
    def _plot_rainflow_matrix(self, results: dict, output_path: Path):
        """Plot rainflow matrix as 3D scatter plot with dots"""
        if len(results.get('ranges', [])) == 0:
            return
            
        # Create figure with 3D scatter plot and 2D projection
        fig = plt.figure(figsize=(16, 7))
        
        # Get data
        ranges = results['ranges']
        means = results['means']
        counts = results['counts']
        
        # Filter out zero counts for cleaner visualization
        mask = counts > 0
        ranges_filtered = ranges[mask]
        means_filtered = means[mask]
        counts_filtered = counts[mask]
        
        # Plot 1: 3D scatter plot
        ax1 = fig.add_subplot(121, projection='3d')
        
        if len(counts_filtered) > 0:
            # Normalize sizes for visualization (larger dots for higher counts)
            sizes = 50 * (counts_filtered / np.max(counts_filtered)) ** 0.5  # Square root scaling
            colors = counts_filtered
            
            # 3D scatter plot with dots
            scatter = ax1.scatter(ranges_filtered, means_filtered, counts_filtered,
                                s=sizes, c=colors, 
                                cmap='viridis', alpha=0.7, edgecolors='black', linewidth=0.3,
                                marker='o')  # Circular dots
            
            ax1.set_xlabel('Stress Range (kN)', fontsize=10, labelpad=10)
            ax1.set_ylabel('Mean Stress (kN)', fontsize=10, labelpad=10)
            ax1.set_zlabel('Cycle Count', fontsize=10, labelpad=10)
            ax1.set_title('Rainflow Matrix - 3D Scatter Plot', fontsize=12)
            
            # Set viewing angle for better visibility
            ax1.view_init(elev=20, azim=45)
            
            # Add grid
            ax1.grid(True, alpha=0.3)
            
            # Add colorbar
            cbar1 = plt.colorbar(scatter, ax=ax1, pad=0.1, shrink=0.8)
            cbar1.set_label('Cycle Count', fontsize=10)
        
        # Plot 2: 2D projection (top view)
        ax2 = fig.add_subplot(122)
        
        if len(counts_filtered) > 0:
            # 2D scatter with sized dots
            sizes_2d = 100 * (counts_filtered / np.max(counts_filtered)) ** 0.5
            
            scatter2 = ax2.scatter(ranges_filtered, means_filtered,
                                 s=sizes_2d, c=counts_filtered,
                                 cmap='viridis', alpha=0.6, edgecolors='black', linewidth=0.5)
            
            ax2.set_xlabel('Stress Range (kN)', fontsize=11)
            ax2.set_ylabel('Mean Stress (kN)', fontsize=11)
            ax2.set_title('Rainflow Matrix - Top View', fontsize=12)
            ax2.grid(True, alpha=0.3)
            
            # Add colorbar
            cbar2 = plt.colorbar(scatter2, ax=ax2)
            cbar2.set_label('Cycle Count', fontsize=10)
            
            # Add statistics box
            stats_text = (f'Total Cycles: {np.sum(counts):.0f}\n'
                         f'Max Range: {np.max(ranges):.1f} kN\n'
                         f'Mean Range: {np.mean(ranges):.1f} kN\n'
                         f'Max Count: {np.max(counts):.0f}')
            ax2.text(0.02, 0.98, stats_text, transform=ax2.transAxes,
                    verticalalignment='top', fontsize=9,
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        plt.suptitle('Rainflow Counting Results - 3D Visualization', fontsize=14, fontweight='bold')
        plt.tight_layout()
        plt.savefig(output_path, dpi=150, bbox_inches='tight')
        plt.close()
    
    def _plot_cycle_histogram(self, results: dict, output_path: Path):
        """Plot cycle count histogram"""
        if 'bin_centers' not in results:
            return
            
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))
        
        # Plot 1: Time trace cycles
        ax1.bar(results['bin_centers'], results['binned_counts'], 
                width=(results['bin_edges'][1] - results['bin_edges'][0]) * 0.8,
                alpha=0.7, color='blue', edgecolor='black')
        ax1.set_xlabel('Stress Range (kN)')
        ax1.set_ylabel('Cycle Count (Time Trace)')
        ax1.set_title('Cycle Count Distribution - Time Trace Duration')
        ax1.grid(True, alpha=0.3)
        
        # Plot 2: Annual cycles
        ax2.bar(results['bin_centers'], results['annual_counts'],
                width=(results['bin_edges'][1] - results['bin_edges'][0]) * 0.8,
                alpha=0.7, color='green', edgecolor='black')
        ax2.set_xlabel('Stress Range (kN)')
        ax2.set_ylabel('Cycle Count (Annual)')
        ax2.set_title(f'Annual Cycle Count Distribution (Scale Factor: {results["scale_factor"]:.2f})')
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(output_path, dpi=150, bbox_inches='tight')
        plt.close()
    
    def _plot_fft_spectrum(self, fft_results: dict, output_path: Path):
        """Plot FFT spectrum with both frequency and period domains"""
        fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(14, 10))
        
        frequencies = fft_results['frequencies']
        periods = fft_results['periods']
        psd = fft_results['psd']
        amplitude = fft_results['amplitude']
        
        # Filter for plotting range
        freq_range = self.config['fft_analysis']['parameters']['freq_range']
        freq_mask = (frequencies >= freq_range['min']) & (frequencies <= freq_range['max'])
        
        # Plot 1: PSD vs Frequency (linear scale)
        ax1.plot(frequencies[freq_mask], psd[freq_mask], 'b-', linewidth=1)
        ax1.set_xlabel('Frequency (Hz)')
        ax1.set_ylabel('PSD (kN²/Hz)')
        ax1.set_title('Power Spectral Density')
        ax1.grid(True, alpha=0.3)
        ax1.axvline(fft_results['dominant_frequency'], color='r', linestyle='--', 
                   label=f'Dominant: {fft_results["dominant_frequency"]:.3f} Hz')
        ax1.legend()
        
        # Plot 2: PSD vs Period (log-log scale)
        # Only plot for reasonable period range (0.1 to 1000 seconds)
        period_mask = (periods > 0.1) & (periods < 1000) & (frequencies > 0)
        if np.any(period_mask):
            ax2.loglog(periods[period_mask], psd[period_mask], 'g-', linewidth=1)
            ax2.set_xlabel('Period (s)')
            ax2.set_ylabel('PSD (kN²/Hz)')
            ax2.set_title('Power Spectral Density - Period Domain')
            ax2.grid(True, alpha=0.3, which='both')
            ax2.axvline(fft_results['dominant_period'], color='r', linestyle='--',
                       label=f'Dominant: {fft_results["dominant_period"]:.2f} s')
            ax2.legend()
        
        # Plot 3: Amplitude spectrum vs Frequency
        ax3.semilogy(frequencies[freq_mask], amplitude[freq_mask], 'r-', linewidth=1)
        ax3.set_xlabel('Frequency (Hz)')
        ax3.set_ylabel('Amplitude (kN/√Hz)')
        ax3.set_title('Amplitude Spectrum')
        ax3.grid(True, alpha=0.3)
        
        # Plot 4: Cumulative power
        cumulative_power = np.cumsum(psd[freq_mask])
        if len(cumulative_power) > 0:
            cumulative_power = cumulative_power / cumulative_power[-1] * 100
            ax4.plot(frequencies[freq_mask], cumulative_power, 'm-', linewidth=1.5)
            ax4.set_xlabel('Frequency (Hz)')
            ax4.set_ylabel('Cumulative Power (%)')
            ax4.set_title('Cumulative Power Distribution')
            ax4.grid(True, alpha=0.3)
            ax4.axhline(90, color='k', linestyle=':', alpha=0.5, label='90%')
            ax4.legend()
        
        plt.suptitle(f'FFT Analysis - Window: {fft_results["window_type"]}, '
                    f'Length: {fft_results["nperseg"]/fft_results["fs"]:.1f}s', 
                    fontsize=12)
        plt.tight_layout()
        plt.savefig(output_path, dpi=150, bbox_inches='tight')
        plt.close()
    
    def _save_results(self, rainflow_results: dict, fft_results: dict, file_info: dict):
        """Save analysis results to CSV files"""
        # Handle absolute or relative output paths
        base_folder = self.config['output']['folders'].get('base', '')
        if base_folder and Path(base_folder).is_absolute():
            base_path = Path(base_folder)
        else:
            base_path = Path(self.config_path).parent.parent
            if base_folder:
                base_path = base_path / base_folder
                
        output_folder = base_path / self.config['output']['folders']['rainflow_results']
        output_folder.mkdir(parents=True, exist_ok=True)
        
        # Generate filename
        pattern = self.config['output']['file_naming']['pattern']
        # Convert to proper types for formatting
        try:
            fc_num = int(file_info['fc_number']) if file_info['fc_number'].isdigit() else file_info['fc_number']
        except:
            fc_num = file_info['fc_number']
            
        base_name = pattern.format(
            config=file_info['config'],
            fc_number=fc_num,
            strut_number=file_info['strut_number'],
            type='rainflow'
        )
        
        # Save rainflow results
        if rainflow_results and 'bin_centers' in rainflow_results:
            rainflow_df = pd.DataFrame({
                'Range (kN)': rainflow_results['bin_centers'],
                'Mean (kN)': rainflow_results['binned_means'],
                'Cycles_TimeTrace': rainflow_results['binned_counts'],
                'Cycles_Annual': rainflow_results['annual_counts'],
                'Bin_Number': np.arange(len(rainflow_results['bin_centers'])) + 1,
                'Damage_Contribution': np.zeros(len(rainflow_results['bin_centers']))  # Placeholder
            })
            
            rainflow_path = output_folder / f"{base_name}.csv"
            rainflow_df.to_csv(rainflow_path, index=False)
            self.logger.info(f"Saved rainflow results to {rainflow_path.name}")
        
        # Save FFT results as CSV
        if fft_results and 'frequencies' in fft_results:
            # Create FFT output filename  
            fft_name = pattern.format(
                config=file_info['config'],
                fc_number=fc_num,
                strut_number=file_info['strut_number'],
                type='fft'
            )
            
            # Prepare FFT data
            frequencies = fft_results['frequencies']
            periods = fft_results['periods']
            psd = fft_results['psd']
            amplitude = fft_results['amplitude']
            
            # Create DataFrame with FFT results
            fft_df = pd.DataFrame({
                'Frequency (Hz)': frequencies,
                'Period (s)': periods,
                'PSD (kN²/Hz)': psd,
                'Amplitude (kN/√Hz)': amplitude
            })
            
            # Save FFT CSV
            fft_path = output_folder / f"{fft_name}.csv"
            fft_df.to_csv(fft_path, index=False)
            self.logger.info(f"Saved FFT results to {fft_path.name}")
            
            # Also save dominant frequency info
            dominant_info = pd.DataFrame({
                'Metric': ['Dominant Frequency', 'Dominant Period', 'Sampling Rate', 'Window Type'],
                'Value': [
                    f"{fft_results['dominant_frequency']:.4f} Hz",
                    f"{fft_results['dominant_period']:.2f} s",
                    f"{fft_results['fs']:.2f} Hz",
                    fft_results['window_type']
                ]
            })
            
            info_path = output_folder / f"{fft_name}_info.csv"
            dominant_info.to_csv(info_path, index=False)
            self.logger.info(f"Saved FFT info to {info_path.name}")
        
        # Store results for summary
        self.results[file_info['filename']] = {
            'rainflow': rainflow_results,
            'fft': fft_results,
            'file_info': file_info
        }
    
    def _generate_summary(self):
        """Generate aggregate summary report"""
        if not self.results:
            return
            
        # Handle absolute or relative output paths
        base_folder = self.config['output']['folders'].get('base', '')
        if base_folder and Path(base_folder).is_absolute():
            base_path = Path(base_folder)
        else:
            base_path = Path(self.config_path).parent.parent
            if base_folder:
                base_path = base_path / base_folder
                
        output_folder = base_path / self.config['output']['folders']['rainflow_results']
        
        summary_data = []
        for filename, result in self.results.items():
            rainflow = result.get('rainflow', {})
            fft = result.get('fft', {})
            
            summary_data.append({
                'Filename': filename,
                'Total_Cycles': rainflow.get('total_cycles', 0),
                'Max_Range_kN': rainflow.get('max_range', 0),
                'Mean_Range_kN': rainflow.get('mean_range', 0),
                'Scale_Factor': rainflow.get('scale_factor', 1),
                'Dominant_Freq_Hz': fft.get('dominant_frequency', 0),
                'Dominant_Period_s': fft.get('dominant_period', 0)
            })
        
        summary_df = pd.DataFrame(summary_data)
        summary_path = output_folder / self.config['output']['formats']['aggregate_summary']['filename']
        summary_df.to_csv(summary_path, index=False)
        
        self.logger.info(f"Generated summary report: {summary_path.name}")
        print("\n" + "="*60)
        print("ANALYSIS SUMMARY")
        print("="*60)
        print(summary_df.to_string())
        print("="*60)


def main():
    """Main entry point"""
    if len(sys.argv) < 2:
        print("Usage: python run_rainflow_analysis.py <config_file>")
        sys.exit(1)
    
    config_file = sys.argv[1]
    
    if not os.path.exists(config_file):
        print(f"Error: Configuration file not found: {config_file}")
        sys.exit(1)
    
    try:
        analyzer = RainflowAnalyzer(config_file)
        analyzer.run()
    except Exception as e:
        print(f"Error during analysis: {str(e)}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()