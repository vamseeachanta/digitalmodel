#!/usr/bin/env python
"""
Test Implementation for OrcaFlex Tension Data Analysis

This test demonstrates rainflow counting and FFT analysis on OrcaFlex
tension data, following the specification in:
specs/modules/signal-analysis/orcaflex-tension-analysis/

Author: DigitalModel Team
Date: 2024-08-21
"""

import sys
import yaml
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import json
from datetime import datetime

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.signal_analysis import (
    RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
)


class OrcaFlexTensionAnalyzer:
    """Analyzer for OrcaFlex tension time series data"""
    
    def __init__(self, config_file=None):
        """Initialize with configuration"""
        self.config = self.load_config(config_file)
        self.results = {}
        
        # Initialize components
        self.rainflow_counter = RainflowCounter(
            method=self.config['analysis']['rainflow']['method']
        )
        self.spectral_analyzer = None  # Will initialize with sampling rate
        self.processor = TimeSeriesProcessor()
        
    def load_config(self, config_file):
        """Load configuration from YAML file"""
        if config_file is None:
            config_file = Path(__file__).parent / 'test_configs' / 'orcaflex_tension_analysis.yml'
        
        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)
        
        return config
    
    def read_csv_data(self, file_path=None):
        """Read OrcaFlex CSV output file"""
        if file_path is None:
            file_path = self.config['input']['file_path']
        
        file_path = Path(file_path)
        if not file_path.is_absolute():
            # Make path relative to test directory
            file_path = Path(__file__).parent / "test_data" / "fat002_fsts_sample.csv"
        
        print(f"\nReading data from: {file_path}")
        
        # Read CSV
        df = pd.read_csv(file_path)
        
        # Verify required columns
        time_col = self.config['input']['columns']['time']
        tension_configs = self.config['input']['columns']['tensions']
        
        if time_col not in df.columns:
            raise ValueError(f"Time column '{time_col}' not found in CSV")
        
        # Extract tension columns
        tensions = {}
        for tension_cfg in tension_configs:
            col_name = tension_cfg['name']
            if col_name in df.columns:
                tensions[tension_cfg['location']] = {
                    'data': df[col_name].values,
                    'units': tension_cfg['units'],
                    'name': col_name
                }
                print(f"  Found column: {col_name} ({len(df)} samples)")
            else:
                print(f"  Warning: Column '{col_name}' not found")
        
        # Calculate sampling rate
        time = df[time_col].values
        dt = np.mean(np.diff(time))
        sampling_rate = 1.0 / dt
        
        print(f"  Sampling rate: {sampling_rate:.2f} Hz")
        print(f"  Duration: {time[-1]:.2f} seconds")
        
        self.spectral_analyzer = SpectralAnalyzer(sampling_rate=sampling_rate)
        
        return {
            'time': time,
            'tensions': tensions,
            'sampling_rate': sampling_rate,
            'dataframe': df
        }
    
    def analyze_tension(self, data, location_key):
        """Perform complete analysis on a tension signal"""
        signal = data['tensions'][location_key]['data']
        name = data['tensions'][location_key]['name']
        units = data['tensions'][location_key]['units']
        
        print(f"\n{'='*60}")
        print(f"Analyzing: {name}")
        print(f"{'='*60}")
        
        results = {
            'name': name,
            'units': units,
            'location': location_key
        }
        
        # 1. Statistics
        print("\n1. Computing statistics...")
        stats = self.processor.calculate_statistics(signal)
        results['statistics'] = stats
        print(f"   Mean: {stats['mean']:.2f} {units}")
        print(f"   Std Dev: {stats['std']:.2f} {units}")
        print(f"   Max: {stats['max']:.2f} {units}")
        print(f"   Min: {stats['min']:.2f} {units}")
        
        # 2. Rainflow Analysis
        print("\n2. Performing rainflow counting...")
        cycles = self.rainflow_counter.count_cycles(signal)
        results['rainflow_cycles'] = cycles
        
        cycle_stats = self.rainflow_counter.get_statistics(cycles)
        results['cycle_statistics'] = cycle_stats
        print(f"   Total cycles: {cycle_stats['total_cycles']}")
        print(f"   Max range: {cycle_stats['max_range']:.2f} {units}")
        print(f"   Mean range: {cycle_stats['mean_range']:.2f} {units}")
        
        # Generate histogram
        histogram = self.rainflow_counter.get_histogram(cycles, bins=20)
        results['cycle_histogram'] = histogram
        
        # 3. FFT Analysis
        print("\n3. Performing FFT analysis...")
        
        # Window-averaged FFT
        fft_config = self.config['analysis']['fft']
        # Adjust window size if signal is too short
        window_size = min(fft_config['window_size'], len(signal) // 4)
        window_fft = self.spectral_analyzer.window_averaged_fft(
            signal,
            window_size=window_size,
            overlap=fft_config['overlap']
        )
        results['window_fft'] = window_fft
        
        # Find peaks
        peaks = self.spectral_analyzer.find_peaks(
            window_fft,
            n_peaks=fft_config['peak_detection']['n_peaks']
        )
        results['spectral_peaks'] = peaks
        
        if len(peaks) > 0:
            print(f"   Dominant frequencies (Hz): {peaks['frequency'].values[:5]}")
            print(f"   Peak powers: {peaks['power'].values[:5]}")
        
        # Compute PSD
        psd = self.spectral_analyzer.compute_psd(signal, method='welch')
        results['psd'] = psd
        
        return results
    
    def generate_plots(self, data, results, output_dir):
        """Generate visualization plots"""
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        for location_key, result in results.items():
            if location_key == 'metadata':
                continue
                
            print(f"\nGenerating plots for {result['name']}...")
            
            # Create figure
            fig = plt.figure(figsize=(16, 10))
            fig.suptitle(f"Tension Analysis: {result['name']}", fontsize=14, fontweight='bold')
            
            # 1. Time series plot
            ax1 = plt.subplot(2, 3, 1)
            time = data['time']
            signal = data['tensions'][location_key]['data']
            ax1.plot(time[:2000], signal[:2000], 'b-', linewidth=0.5)
            ax1.set_xlabel('Time (s)')
            ax1.set_ylabel(f'Tension ({result["units"]})')
            ax1.set_title('Time Series (First 200 seconds)')
            ax1.grid(True, alpha=0.3)
            
            # 2. Rainflow histogram
            ax2 = plt.subplot(2, 3, 2)
            histogram = result['cycle_histogram']
            if len(histogram) > 0:
                centers = (histogram['range_min'] + histogram['range_max']) / 2
                ax2.bar(centers, histogram['count'],
                       width=(histogram['range_max'] - histogram['range_min']).mean(),
                       edgecolor='black', alpha=0.7)
            ax2.set_xlabel(f'Stress Range ({result["units"]})')
            ax2.set_ylabel('Count')
            ax2.set_title('Rainflow Histogram')
            ax2.grid(True, alpha=0.3)
            
            # 3. Mean-Range scatter
            ax3 = plt.subplot(2, 3, 3)
            cycles = result['rainflow_cycles']
            if len(cycles) > 0:
                scatter = ax3.scatter(cycles['mean'], cycles['range'],
                                    c=cycles['count'], s=10, alpha=0.6, cmap='viridis')
                plt.colorbar(scatter, ax=ax3, label='Count')
            ax3.set_xlabel(f'Mean Stress ({result["units"]})')
            ax3.set_ylabel(f'Stress Range ({result["units"]})')
            ax3.set_title('Rainflow Matrix')
            ax3.grid(True, alpha=0.3)
            
            # 4. FFT Spectrum
            ax4 = plt.subplot(2, 3, 4)
            fft = result['window_fft']
            freq_limit = self.config['analysis']['fft']['frequency_limit']
            if len(fft) > 0:
                mask = fft['frequency'] <= freq_limit
                ax4.semilogy(fft.loc[mask, 'frequency'],
                           fft.loc[mask, 'power'], 'b-')
                
                # Mark peaks
                peaks = result['spectral_peaks']
                if len(peaks) > 0:
                    for _, peak in peaks.head(5).iterrows():
                        if peak['frequency'] <= freq_limit:
                            ax4.axvline(peak['frequency'], color='r', 
                                      linestyle='--', alpha=0.5)
            
            ax4.set_xlabel('Frequency (Hz)')
            ax4.set_ylabel('Power')
            ax4.set_title('Window-Averaged FFT')
            ax4.grid(True, alpha=0.3)
            
            # 5. PSD
            ax5 = plt.subplot(2, 3, 5)
            psd = result['psd']
            if len(psd) > 0 and 'frequency' in psd.columns:
                psd_col = 'psd' if 'psd' in psd.columns else 'power'
                mask = psd['frequency'] <= freq_limit
                ax5.semilogy(psd.loc[mask, 'frequency'],
                           psd.loc[mask, psd_col], 'g-')
            ax5.set_xlabel('Frequency (Hz)')
            ax5.set_ylabel('PSD')
            ax5.set_title('Power Spectral Density')
            ax5.grid(True, alpha=0.3)
            
            # 6. Statistics summary
            ax6 = plt.subplot(2, 3, 6)
            ax6.axis('off')
            
            stats = result['statistics']
            cycle_stats = result['cycle_statistics']
            
            summary_text = f"""Statistics Summary:
            
Signal Statistics:
  Mean: {stats['mean']:.2f} {result['units']}
  Std Dev: {stats['std']:.2f} {result['units']}
  Max: {stats['max']:.2f} {result['units']}
  Min: {stats['min']:.2f} {result['units']}
  RMS: {stats['rms']:.2f} {result['units']}
  
Rainflow Analysis:
  Total Cycles: {cycle_stats['total_cycles']}
  Max Range: {cycle_stats['max_range']:.2f} {result['units']}
  Mean Range: {cycle_stats['mean_range']:.2f} {result['units']}
  
Spectral Analysis:
  Dominant Freq: {result['spectral_peaks']['frequency'].values[0]:.3f} Hz
  Sampling Rate: {data['sampling_rate']:.2f} Hz"""
            
            ax6.text(0.1, 0.9, summary_text, transform=ax6.transAxes,
                    fontsize=9, verticalalignment='top', fontfamily='monospace')
            ax6.set_title('Analysis Summary')
            
            plt.tight_layout()
            
            # Save figure
            plot_file = output_dir / f"tension_analysis_{location_key}.png"
            plt.savefig(plot_file, dpi=150, bbox_inches='tight')
            print(f"  Saved plot: {plot_file}")
            plt.close(fig)
    
    def export_results(self, results, output_dir):
        """Export results to CSV and JSON"""
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        for location_key, result in results.items():
            if location_key == 'metadata':
                continue
            
            print(f"\nExporting results for {result['name']}...")
            
            # Export rainflow cycles to CSV
            cycles = result['rainflow_cycles']
            cycles_file = output_dir / f"rainflow_cycles_{location_key}.csv"
            cycles.to_csv(cycles_file, index=False)
            print(f"  Saved rainflow cycles: {cycles_file}")
            
            # Export FFT spectrum to CSV
            fft = result['window_fft']
            fft_file = output_dir / f"fft_spectrum_{location_key}.csv"
            fft.to_csv(fft_file, index=False)
            print(f"  Saved FFT spectrum: {fft_file}")
            
            # Export summary to JSON
            summary = {
                'name': result['name'],
                'units': result['units'],
                'location': result['location'],
                'statistics': result['statistics'],
                'cycle_statistics': result['cycle_statistics'],
                'dominant_frequencies': result['spectral_peaks']['frequency'].head(5).tolist()
                if len(result['spectral_peaks']) > 0 else []
            }
            
            # Convert numpy types for JSON
            def convert_types(obj):
                if isinstance(obj, (np.int32, np.int64)):
                    return int(obj)
                elif isinstance(obj, (np.float32, np.float64)):
                    return float(obj)
                elif isinstance(obj, dict):
                    return {k: convert_types(v) for k, v in obj.items()}
                elif isinstance(obj, list):
                    return [convert_types(v) for v in obj]
                return obj
            
            summary = convert_types(summary)
            
            json_file = output_dir / f"analysis_summary_{location_key}.json"
            with open(json_file, 'w') as f:
                json.dump(summary, f, indent=2)
            print(f"  Saved summary: {json_file}")
    
    def run_analysis(self):
        """Run complete analysis pipeline"""
        print("\n" + "#"*60)
        print("#  OrcaFlex Tension Data Analysis")
        print("#"*60)
        
        # Read data
        data = self.read_csv_data()
        
        # Analyze each tension location
        results = {'metadata': {
            'timestamp': datetime.now().isoformat(),
            'config': self.config['metadata'],
            'sampling_rate': data['sampling_rate']
        }}
        
        for location_key in data['tensions'].keys():
            result = self.analyze_tension(data, location_key)
            results[location_key] = result
        
        # Generate output directory with timestamp
        if self.config['output']['use_timestamp']:
            timestamp = datetime.now().strftime(self.config['output']['timestamp_format'])
            output_dir = Path(self.config['output']['directory']) / timestamp
        else:
            output_dir = Path(self.config['output']['directory'])
        
        # Generate plots
        self.generate_plots(data, results, output_dir)
        
        # Export results
        self.export_results(results, output_dir)
        
        # Summary
        print("\n" + "="*60)
        print("ANALYSIS COMPLETE")
        print("="*60)
        print(f"\nOutput directory: {output_dir}")
        print(f"Files generated:")
        for file in sorted(output_dir.glob('*')):
            size = file.stat().st_size / 1024
            print(f"  {file.name:40s} {size:8.1f} KB")
        
        return results


def main():
    """Main test function"""
    # Initialize analyzer
    analyzer = OrcaFlexTensionAnalyzer()
    
    # Run analysis
    results = analyzer.run_analysis()
    
    print("\n" + "#"*60)
    print("#  TEST COMPLETED SUCCESSFULLY")
    print("#"*60)
    
    return 0


if __name__ == "__main__":
    sys.exit(main())