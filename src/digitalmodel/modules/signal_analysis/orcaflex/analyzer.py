"""
Time Series Analyzer for comprehensive signal analysis

Orchestrates rainflow counting and FFT analysis on time series data
with support for batch processing and flexible output generation.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
from typing import Dict, List, Optional, Union, Any
import logging
from datetime import datetime

from ..core.rainflow import RainflowCounter
from ..core.spectral import SpectralAnalyzer
from ..core.timeseries import TimeSeriesProcessor
from .reader import GenericTimeSeriesReader

logger = logging.getLogger(__name__)


class TimeSeriesAnalyzer:
    """Main analyzer for time series data processing"""
    
    def __init__(self, 
                 config: Optional[Dict] = None,
                 auto_detect_columns: bool = False,
                 profile: Optional[str] = None):
        """
        Initialize analyzer
        
        Args:
            config: Configuration dictionary
            auto_detect_columns: Whether to auto-detect columns
            profile: Column mapping profile to use
        """
        self.config = config or {}
        self.auto_detect = auto_detect_columns
        self.profile = profile
        
        # Initialize components
        self.reader = GenericTimeSeriesReader(config)
        self.rainflow_counter = RainflowCounter(
            method=self.config.get('analysis', {}).get('rainflow', {}).get('method', 'astm')
        )
        self.processor = TimeSeriesProcessor()
        self.spectral_analyzer = None  # Will be initialized with sampling rate
        
        # Results storage
        self.results = {}
        
    def process_file(self, 
                    filepath: Union[str, Path],
                    column_mapping: Optional[Dict] = None,
                    output_dir: Optional[Union[str, Path]] = None) -> Dict:
        """
        Process a single file
        
        Args:
            filepath: Path to file
            column_mapping: Optional column mapping override
            output_dir: Output directory for results
            
        Returns:
            Dictionary of analysis results
        """
        filepath = Path(filepath)
        logger.info(f"Processing file: {filepath}")
        
        # Read file
        df, mapping = self.reader.read_file(
            filepath, 
            column_mapping=column_mapping,
            auto_detect=self.auto_detect
        )
        
        # Validate data
        validation = self.reader.validate_data(df, mapping)
        if validation['warnings']:
            for warning in validation['warnings']:
                logger.warning(warning)
        
        # Get sampling rate
        sampling_rate = validation['statistics'].get('sampling_rate', 10.0)
        self.spectral_analyzer = SpectralAnalyzer(sampling_rate=sampling_rate)
        
        # Preprocess if configured
        if self.config.get('preprocessing', {}).get('enable', False):
            df = self.reader.preprocess(
                df, mapping,
                fill_gaps=self.config['preprocessing'].get('fill_gaps', True),
                remove_outliers=self.config['preprocessing'].get('remove_outliers', False),
                detrend=self.config['preprocessing'].get('detrend', False)
            )
        
        # Analyze each data column
        results = {
            'file': str(filepath),
            'timestamp': datetime.now().isoformat(),
            'validation': validation,
            'columns': {}
        }
        
        time_col = mapping.get('time')
        time_data = df[time_col].values if time_col else np.arange(len(df))
        
        for col_spec in mapping.get('data_columns', []):
            if isinstance(col_spec, dict):
                col_name = col_spec.get('name')
                col_info = col_spec
            else:
                col_name = col_spec
                col_info = {'name': col_name}
            
            if col_name not in df.columns:
                continue
            
            # Analyze column
            col_results = self._analyze_column(
                df[col_name].values,
                time_data,
                col_info,
                sampling_rate
            )
            
            results['columns'][col_name] = col_results
        
        # Generate outputs if directory specified
        if output_dir:
            self._generate_outputs(results, output_dir)
        
        # Store results
        self.results[str(filepath)] = results
        
        return results
    
    def _analyze_column(self, 
                       signal: np.ndarray,
                       time: np.ndarray,
                       col_info: Dict,
                       sampling_rate: float) -> Dict:
        """
        Analyze a single data column
        
        Args:
            signal: Signal data
            time: Time array
            col_info: Column information
            sampling_rate: Sampling rate in Hz
            
        Returns:
            Analysis results dictionary
        """
        results = {
            'info': col_info,
            'statistics': {},
            'rainflow': {},
            'spectral': {}
        }
        
        # Calculate statistics
        stats = self.processor.calculate_statistics(signal)
        results['statistics'] = stats
        
        # Rainflow analysis
        if self.config.get('analysis', {}).get('rainflow', {}).get('enable', True):
            cycles = self.rainflow_counter.count_cycles(signal)
            results['rainflow']['cycles'] = cycles
            results['rainflow']['statistics'] = self.rainflow_counter.get_statistics(cycles)
            
            # Generate histogram
            bin_count = self.config.get('analysis', {}).get('rainflow', {}).get('bin_count', 50)
            histogram = self.rainflow_counter.get_histogram(cycles, bins=bin_count)
            results['rainflow']['histogram'] = histogram
        
        # Spectral analysis
        if self.config.get('analysis', {}).get('fft', {}).get('enable', True):
            fft_config = self.config.get('analysis', {}).get('fft', {})
            
            # Standard FFT
            spectrum = self.spectral_analyzer.compute_spectrum(signal)
            results['spectral']['spectrum'] = spectrum
            
            # Window-averaged FFT
            window_size = min(fft_config.get('window_size', 1024), len(signal) // 4)
            window_fft = self.spectral_analyzer.window_averaged_fft(
                signal,
                window_size=window_size,
                overlap=fft_config.get('overlap', 0.5)
            )
            results['spectral']['window_fft'] = window_fft
            
            # Find peaks
            n_peaks = fft_config.get('peak_detection', {}).get('n_peaks', 10)
            peaks = self.spectral_analyzer.find_peaks(window_fft, n_peaks=n_peaks)
            results['spectral']['peaks'] = peaks
            
            # PSD
            psd = self.spectral_analyzer.compute_psd(signal, method='welch')
            results['spectral']['psd'] = psd
        
        return results
    
    def process_pattern(self,
                       pattern: str,
                       directory: Union[str, Path],
                       output_dir: Optional[Union[str, Path]] = None,
                       parallel: bool = False) -> Dict:
        """
        Process files matching a pattern
        
        Args:
            pattern: File pattern (e.g., "*.csv")
            directory: Directory to search
            output_dir: Output directory
            parallel: Whether to process in parallel
            
        Returns:
            Dictionary of results keyed by filename
        """
        # Discover files
        files = self.reader.discover_files(pattern=pattern, directory=directory)
        
        if not files:
            logger.warning(f"No files found matching pattern: {pattern}")
            return {}
        
        logger.info(f"Found {len(files)} files to process")
        
        # Process files
        if parallel:
            results = self._process_parallel(files, output_dir)
        else:
            results = {}
            for file in files:
                try:
                    file_results = self.process_file(file, output_dir=output_dir)
                    results[str(file)] = file_results
                except Exception as e:
                    logger.error(f"Failed to process {file}: {e}")
                    if not self.config.get('batch', {}).get('continue_on_error', True):
                        raise
        
        # Generate summary if configured
        if self.config.get('output', {}).get('summary', {}).get('create', True):
            self._generate_summary(results, output_dir)
        
        return results
    
    def _process_parallel(self, files: List[Path], output_dir: Optional[Path]) -> Dict:
        """Process files in parallel"""
        from concurrent.futures import ProcessPoolExecutor, as_completed
        
        max_workers = self.config.get('batch', {}).get('parallel', {}).get('max_workers', 4)
        results = {}
        
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(self.process_file, file, output_dir=output_dir): file
                for file in files
            }
            
            for future in as_completed(futures):
                file = futures[future]
                try:
                    file_results = future.result()
                    results[str(file)] = file_results
                except Exception as e:
                    logger.error(f"Failed to process {file}: {e}")
                    if not self.config.get('batch', {}).get('continue_on_error', True):
                        raise
        
        return results
    
    def batch_process(self,
                     files: List[Union[str, Path]],
                     output_dir: Optional[Union[str, Path]] = None,
                     parallel: bool = True) -> Dict:
        """
        Process a list of files
        
        Args:
            files: List of file paths
            output_dir: Output directory
            parallel: Whether to process in parallel
            
        Returns:
            Dictionary of results
        """
        files = [Path(f) for f in files]
        
        if parallel:
            results = self._process_parallel(files, output_dir)
        else:
            results = {}
            for file in files:
                try:
                    file_results = self.process_file(file, output_dir=output_dir)
                    results[str(file)] = file_results
                except Exception as e:
                    logger.error(f"Failed to process {file}: {e}")
                    if not self.config.get('batch', {}).get('continue_on_error', True):
                        raise
        
        return results
    
    def _generate_outputs(self, results: Dict, output_dir: Union[str, Path]):
        """Generate output files and visualizations"""
        output_dir = Path(output_dir)
        
        # Create output directory structure
        if self.config.get('output', {}).get('organization', {}).get('by_file', True):
            file_name = Path(results['file']).stem
            output_dir = output_dir / file_name
        
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Export data for each column
        for col_name, col_results in results['columns'].items():
            safe_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
            
            # Export rainflow cycles
            if 'cycles' in col_results.get('rainflow', {}):
                cycles = col_results['rainflow']['cycles']
                if isinstance(cycles, pd.DataFrame):
                    csv_path = output_dir / f"rainflow_{safe_name}.csv"
                    cycles.to_csv(csv_path, index=False)
                    logger.info(f"Exported rainflow cycles: {csv_path}")
            
            # Export FFT spectrum
            if 'window_fft' in col_results.get('spectral', {}):
                spectrum = col_results['spectral']['window_fft']
                if isinstance(spectrum, pd.DataFrame):
                    csv_path = output_dir / f"fft_{safe_name}.csv"
                    spectrum.to_csv(csv_path, index=False)
                    logger.info(f"Exported FFT spectrum: {csv_path}")
            
            # Generate plots if configured
            if self.config.get('output', {}).get('plots', {}).get('enable', True):
                self._generate_plots(col_name, col_results, output_dir)
    
    def _generate_plots(self, col_name: str, col_results: Dict, output_dir: Path):
        """Generate visualization plots for a column"""
        safe_name = col_name.replace(' ', '_').replace('(', '').replace(')', '')
        
        # Create figure with subplots
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        fig.suptitle(f"Analysis: {col_name}", fontsize=14)
        
        # Rainflow histogram
        if 'histogram' in col_results.get('rainflow', {}):
            ax = axes[0, 0]
            histogram = col_results['rainflow']['histogram']
            if isinstance(histogram, pd.DataFrame) and len(histogram) > 0:
                centers = (histogram['range_min'] + histogram['range_max']) / 2
                ax.bar(centers, histogram['count'],
                      width=(histogram['range_max'] - histogram['range_min']).mean())
            ax.set_xlabel('Range')
            ax.set_ylabel('Count')
            ax.set_title('Rainflow Histogram')
            ax.grid(True, alpha=0.3)
        
        # FFT spectrum
        if 'window_fft' in col_results.get('spectral', {}):
            ax = axes[0, 1]
            spectrum = col_results['spectral']['window_fft']
            if isinstance(spectrum, pd.DataFrame) and len(spectrum) > 0:
                freq_limit = self.config.get('analysis', {}).get('fft', {}).get('frequency_limit', 10)
                mask = spectrum['frequency'] <= freq_limit
                ax.semilogy(spectrum.loc[mask, 'frequency'],
                          spectrum.loc[mask, 'power'])
            ax.set_xlabel('Frequency (Hz)')
            ax.set_ylabel('Power')
            ax.set_title('FFT Spectrum')
            ax.grid(True, alpha=0.3)
        
        # Rainflow matrix
        if 'cycles' in col_results.get('rainflow', {}):
            ax = axes[1, 0]
            cycles = col_results['rainflow']['cycles']
            if isinstance(cycles, pd.DataFrame) and len(cycles) > 0:
                scatter = ax.scatter(cycles['mean'], cycles['range'],
                                   c=cycles['count'], s=10, alpha=0.6, cmap='viridis')
                plt.colorbar(scatter, ax=ax)
            ax.set_xlabel('Mean')
            ax.set_ylabel('Range')
            ax.set_title('Rainflow Matrix')
            ax.grid(True, alpha=0.3)
        
        # Statistics summary
        ax = axes[1, 1]
        ax.axis('off')
        stats = col_results.get('statistics', {})
        rainflow_stats = col_results.get('rainflow', {}).get('statistics', {})
        
        summary_text = f"""Statistics:
Mean: {stats.get('mean', 0):.2f}
Std: {stats.get('std', 0):.2f}
Max: {stats.get('max', 0):.2f}
Min: {stats.get('min', 0):.2f}

Rainflow:
Cycles: {rainflow_stats.get('total_cycles', 0)}
Max Range: {rainflow_stats.get('max_range', 0):.2f}
Mean Range: {rainflow_stats.get('mean_range', 0):.2f}"""
        
        ax.text(0.1, 0.9, summary_text, transform=ax.transAxes,
               fontsize=10, verticalalignment='top', fontfamily='monospace')
        
        plt.tight_layout()
        
        # Save figure
        plot_path = output_dir / f"analysis_{safe_name}.png"
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        plt.close(fig)
        logger.info(f"Saved plot: {plot_path}")
    
    def _generate_summary(self, results: Dict, output_dir: Optional[Path]):
        """Generate summary report for batch processing"""
        if not output_dir:
            return
            
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create summary DataFrame
        summary_data = []
        
        for file_path, file_results in results.items():
            for col_name, col_results in file_results.get('columns', {}).items():
                row = {
                    'file': Path(file_path).name,
                    'column': col_name,
                    'mean': col_results.get('statistics', {}).get('mean'),
                    'std': col_results.get('statistics', {}).get('std'),
                    'max': col_results.get('statistics', {}).get('max'),
                    'min': col_results.get('statistics', {}).get('min'),
                    'cycles': col_results.get('rainflow', {}).get('statistics', {}).get('total_cycles'),
                    'max_range': col_results.get('rainflow', {}).get('statistics', {}).get('max_range')
                }
                
                # Add dominant frequency
                peaks = col_results.get('spectral', {}).get('peaks')
                if isinstance(peaks, pd.DataFrame) and len(peaks) > 0:
                    row['dominant_freq'] = peaks.iloc[0]['frequency']
                
                summary_data.append(row)
        
        summary_df = pd.DataFrame(summary_data)
        
        # Save summary
        summary_path = output_dir / 'batch_summary.csv'
        summary_df.to_csv(summary_path, index=False)
        logger.info(f"Saved batch summary: {summary_path}")