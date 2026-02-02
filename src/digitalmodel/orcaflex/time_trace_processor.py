"""
OrcaFlex Time Trace Processor

Integrates OrcaFlex simulation outputs with signal analysis module
for automated fatigue and spectral analysis.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Union, Any
from pathlib import Path
import pandas as pd
import numpy as np
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed
from functools import partial

# Configure logging
logger = logging.getLogger(__name__)


@dataclass
class TimeTraceConfig:
    """Configuration for time trace processing"""
    # Source configuration
    source_type: str = 'orcaflex'  # 'orcaflex', 'csv', 'hdf5'
    file_path: Optional[Path] = None
    model: Optional[Any] = None  # OrcaFlex model object
    
    # Time trace selection
    objects: List[str] = field(default_factory=list)
    variables: List[str] = field(default_factory=list)
    time_range: Optional[tuple] = None
    
    # Processing options
    preprocessing: Dict = field(default_factory=dict)
    fatigue_analysis: Dict = field(default_factory=dict)
    spectral_analysis: Dict = field(default_factory=dict)
    
    # Output configuration
    output_dir: Path = field(default=Path('./results'))
    save_intermediate: bool = False
    generate_report: bool = True
    
    # Performance options
    parallel_processing: bool = True
    n_workers: int = 4
    chunk_size: Optional[int] = None  # For memory-efficient processing


class OrcaFlexTimeTraceProcessor:
    """
    Main processor for OrcaFlex time trace analysis
    Bridges OrcaFlex outputs with signal analysis module
    """
    
    def __init__(self, config: TimeTraceConfig):
        self.config = config
        self.results = {}
        self._setup_output_directory()
        
    def _setup_output_directory(self):
        """Create output directory if it doesn't exist"""
        self.config.output_dir.mkdir(parents=True, exist_ok=True)
        
    def process(self) -> Dict:
        """
        Main processing pipeline
        
        Returns:
            Dictionary containing analysis results
        """
        logger.info(f"Starting time trace processing from {self.config.source_type}")
        
        try:
            # Step 1: Load time traces
            time_traces = self._load_time_traces()
            logger.info(f"Loaded {len(time_traces.columns)-1} time traces")
            
            # Step 2: Preprocess signals
            if self.config.preprocessing:
                time_traces = self._preprocess_signals(time_traces)
                logger.info("Preprocessing completed")
            
            # Step 3: Perform analyses
            results = {}
            
            if self.config.fatigue_analysis:
                logger.info("Performing fatigue analysis...")
                results['fatigue'] = self._perform_fatigue_analysis(time_traces)
                
            if self.config.spectral_analysis:
                logger.info("Performing spectral analysis...")
                results['spectral'] = self._perform_spectral_analysis(time_traces)
            
            # Step 4: Save results
            if self.config.save_intermediate:
                self._save_intermediate_results(results)
            
            # Step 5: Generate reports
            if self.config.generate_report:
                self._generate_reports(results)
                
            logger.info("Processing completed successfully")
            return results
            
        except Exception as e:
            logger.error(f"Processing failed: {str(e)}")
            raise
    
    def _load_time_traces(self) -> pd.DataFrame:
        """
        Load time traces from specified source
        
        Returns:
            DataFrame with time traces
        """
        if self.config.source_type == 'orcaflex':
            return self._load_from_orcaflex()
        elif self.config.source_type == 'csv':
            return self._load_from_csv()
        elif self.config.source_type == 'hdf5':
            return self._load_from_hdf5()
        else:
            raise ValueError(f"Unknown source type: {self.config.source_type}")
    
    def _load_from_orcaflex(self) -> pd.DataFrame:
        """Load time traces directly from OrcaFlex model"""
        try:
            import OrcFxAPI
        except ImportError:
            raise ImportError("OrcFxAPI not available. Please install OrcaFlex Python API")
        
        # Load model if file path provided
        if self.config.file_path:
            model = OrcFxAPI.Model(str(self.config.file_path))
        elif self.config.model:
            model = self.config.model
        else:
            raise ValueError("Either file_path or model must be provided")
        
        # Ensure model is simulated
        if model.state != OrcFxAPI.ModelState.InSimulation:
            logger.info("Running simulation...")
            model.RunSimulation()
        
        # Extract time traces
        time_traces = {'Time': model.GetTimeHistory('Time')}
        
        for obj_name in self.config.objects:
            try:
                obj = model[obj_name]
                for var_name in self.config.variables:
                    column_name = f"{obj_name}.{var_name}"
                    time_traces[column_name] = obj.TimeHistory(var_name)
                    logger.debug(f"Loaded {column_name}")
            except Exception as e:
                logger.warning(f"Could not load {obj_name}.{var_name}: {str(e)}")
        
        df = pd.DataFrame(time_traces)
        
        # Apply time range if specified
        if self.config.time_range:
            start, end = self.config.time_range
            df = df[(df['Time'] >= start) & (df['Time'] <= end)]
        
        return df
    
    def _load_from_csv(self) -> pd.DataFrame:
        """Load time traces from CSV file"""
        if not self.config.file_path:
            raise ValueError("file_path must be provided for CSV source")
        
        df = pd.read_csv(self.config.file_path)
        
        # Apply time range if specified
        if self.config.time_range and 'Time' in df.columns:
            start, end = self.config.time_range
            df = df[(df['Time'] >= start) & (df['Time'] <= end)]
        
        return df
    
    def _load_from_hdf5(self) -> pd.DataFrame:
        """Load time traces from HDF5 file"""
        if not self.config.file_path:
            raise ValueError("file_path must be provided for HDF5 source")
        
        with pd.HDFStore(self.config.file_path, 'r') as store:
            df = store['time_traces']
        
        # Apply time range if specified
        if self.config.time_range and 'Time' in df.columns:
            start, end = self.config.time_range
            df = df[(df['Time'] >= start) & (df['Time'] <= end)]
        
        return df
    
    def _preprocess_signals(self, traces: pd.DataFrame) -> pd.DataFrame:
        """
        Apply preprocessing to signals
        
        Args:
            traces: DataFrame with time traces
            
        Returns:
            Preprocessed DataFrame
        """
        from ..signal_analysis import TimeSeriesProcessor
        
        processor = TimeSeriesProcessor()
        processed = traces.copy()
        
        for column in processed.columns:
            if column == 'Time':
                continue
            
            signal = processed[column].values
            
            # Apply preprocessing steps
            if self.config.preprocessing.get('remove_outliers'):
                signal = processor.remove_outliers(
                    signal, 
                    method=self.config.preprocessing.get('outlier_method', 'zscore'),
                    threshold=self.config.preprocessing.get('outlier_threshold', 3)
                )
            
            if self.config.preprocessing.get('detrend'):
                signal = processor.detrend(
                    signal,
                    method=self.config.preprocessing.get('detrend_method', 'linear')
                )
            
            if self.config.preprocessing.get('smooth'):
                signal = processor.smooth(
                    signal,
                    method=self.config.preprocessing.get('smooth_method', 'savgol'),
                    **self.config.preprocessing.get('smooth_params', {})
                )
            
            processed[column] = signal
        
        return processed
    
    def _perform_fatigue_analysis(self, traces: pd.DataFrame) -> Dict:
        """
        Perform fatigue analysis on time traces
        
        Args:
            traces: DataFrame with time traces
            
        Returns:
            Dictionary with fatigue analysis results
        """
        from ..signal_analysis import RainflowCounter
        from ..signal_analysis.fatigue import FatigueDamageCalculator, SNCurve
        
        results = {}
        
        # Get fatigue parameters
        sn_standard = self.config.fatigue_analysis.get('sn_standard', 'DNV')
        sn_class = self.config.fatigue_analysis.get('sn_class', 'F')
        scf = self.config.fatigue_analysis.get('scf', 1.0)
        mean_stress_correction = self.config.fatigue_analysis.get('mean_stress_correction')
        design_life_years = self.config.fatigue_analysis.get('design_life_years', 25)
        
        # Initialize components
        counter = RainflowCounter(method='astm')
        sn_curve = SNCurve(
            curve_type='standard',
            standard=sn_standard,
            **{'class': sn_class}
        )
        damage_calc = FatigueDamageCalculator(method='miners')
        
        # Process each time trace
        if self.config.parallel_processing and len(traces.columns) > 2:
            # Parallel processing for multiple traces
            results = self._parallel_fatigue_analysis(
                traces, counter, sn_curve, damage_calc, 
                scf, mean_stress_correction, design_life_years
            )
        else:
            # Sequential processing
            for column in traces.columns:
                if column == 'Time':
                    continue
                
                logger.debug(f"Analyzing {column}")
                
                # Apply stress concentration factor
                signal = traces[column].values * scf
                
                # Rainflow counting
                cycles = counter.count_cycles(signal, extract_info=True)
                
                # Calculate damage
                damage = damage_calc.calculate_damage(
                    cycles,
                    sn_curve.get_curve_parameters(),
                    mean_stress_correction=mean_stress_correction
                )
                
                # Calculate statistics
                stats = counter.get_statistics(cycles)
                
                # Store results
                results[column] = {
                    'cycles': cycles,
                    'damage_per_hour': damage['total_damage'] / (len(signal) / self._get_sampling_rate(traces)),
                    'damage_design_life': damage['total_damage'] * design_life_years * 365 * 24,
                    'life_years': 1.0 / (damage['total_damage'] * 365 * 24) if damage['total_damage'] > 0 else float('inf'),
                    'statistics': stats,
                    'parameters': {
                        'sn_curve': f"{sn_standard}-{sn_class}",
                        'scf': scf,
                        'mean_stress_correction': mean_stress_correction
                    }
                }
        
        return results
    
    def _perform_spectral_analysis(self, traces: pd.DataFrame) -> Dict:
        """
        Perform spectral analysis on time traces
        
        Args:
            traces: DataFrame with time traces
            
        Returns:
            Dictionary with spectral analysis results
        """
        from ..signal_analysis import SpectralAnalyzer
        
        results = {}
        
        # Get spectral parameters
        method = self.config.spectral_analysis.get('method', 'welch')
        window = self.config.spectral_analysis.get('window', 'hann')
        nperseg = self.config.spectral_analysis.get('nperseg', 256)
        n_peaks = self.config.spectral_analysis.get('n_peaks', 5)
        
        # Calculate sampling rate
        fs = self._get_sampling_rate(traces)
        
        # Initialize analyzer
        analyzer = SpectralAnalyzer(sampling_rate=fs, method=method)
        
        # Process each time trace
        for column in traces.columns:
            if column == 'Time':
                continue
            
            logger.debug(f"Spectral analysis of {column}")
            
            signal = traces[column].values
            
            # Compute spectrum
            if method == 'welch':
                spectrum = analyzer.compute_spectrum(
                    signal,
                    nperseg=nperseg,
                    window=window
                )
            elif method == 'window_averaged':
                spectrum = analyzer.window_averaged_fft(
                    signal,
                    window_size=nperseg,
                    window_function=window
                )
            else:
                spectrum = analyzer.compute_spectrum(signal)
            
            # Find peaks
            peaks = analyzer.find_peaks(spectrum, n_peaks=n_peaks)
            
            # Store results
            results[column] = {
                'spectrum': spectrum,
                'peaks': peaks,
                'dominant_frequency': peaks.iloc[0]['frequency'] if len(peaks) > 0 else None,
                'dominant_period': 1.0 / peaks.iloc[0]['frequency'] if len(peaks) > 0 and peaks.iloc[0]['frequency'] > 0 else None,
                'total_power': spectrum['power'].sum(),
                'parameters': {
                    'method': method,
                    'window': window,
                    'nperseg': nperseg,
                    'sampling_rate': fs
                }
            }
        
        return results
    
    def _get_sampling_rate(self, traces: pd.DataFrame) -> float:
        """Calculate sampling rate from time trace"""
        if 'Time' in traces.columns:
            dt = traces['Time'].iloc[1] - traces['Time'].iloc[0]
            return 1.0 / dt
        else:
            # Assume 1 Hz if no time column
            return 1.0
    
    def _parallel_fatigue_analysis(self, traces, counter, sn_curve, damage_calc, 
                                   scf, mean_stress_correction, design_life_years):
        """Perform fatigue analysis in parallel"""
        from concurrent.futures import ProcessPoolExecutor
        
        results = {}
        columns = [c for c in traces.columns if c != 'Time']
        
        with ProcessPoolExecutor(max_workers=self.config.n_workers) as executor:
            futures = {}
            
            for column in columns:
                future = executor.submit(
                    self._analyze_single_trace,
                    traces[column].values,
                    column, scf, sn_curve.get_curve_parameters(),
                    mean_stress_correction, design_life_years,
                    self._get_sampling_rate(traces)
                )
                futures[future] = column
            
            for future in as_completed(futures):
                column = futures[future]
                try:
                    results[column] = future.result()
                except Exception as e:
                    logger.error(f"Failed to analyze {column}: {str(e)}")
        
        return results
    
    @staticmethod
    def _analyze_single_trace(signal, name, scf, sn_params, 
                             mean_stress_correction, design_life_years, fs):
        """Analyze single trace (for parallel processing)"""
        from ..signal_analysis import RainflowCounter
        from ..signal_analysis.fatigue import FatigueDamageCalculator
        
        counter = RainflowCounter(method='astm')
        damage_calc = FatigueDamageCalculator(method='miners')
        
        # Apply SCF
        signal = signal * scf
        
        # Count cycles
        cycles = counter.count_cycles(signal, extract_info=True)
        
        # Calculate damage
        damage = damage_calc.calculate_damage(
            cycles, sn_params,
            mean_stress_correction=mean_stress_correction
        )
        
        # Calculate statistics
        stats = counter.get_statistics(cycles)
        
        # Return results
        hours = len(signal) / (fs * 3600)
        return {
            'cycles': cycles,
            'damage_per_hour': damage['total_damage'] / hours,
            'damage_design_life': damage['total_damage'] * design_life_years * 365 * 24 / hours,
            'life_years': hours / (damage['total_damage'] * 365 * 24) if damage['total_damage'] > 0 else float('inf'),
            'statistics': stats
        }
    
    def _save_intermediate_results(self, results: Dict):
        """Save intermediate results to files"""
        import json
        
        # Save summary to JSON
        summary_path = self.config.output_dir / 'analysis_summary.json'
        
        # Convert non-serializable objects
        summary = {}
        for analysis_type, analysis_results in results.items():
            summary[analysis_type] = {}
            for trace_name, trace_results in analysis_results.items():
                summary[analysis_type][trace_name] = {
                    k: v for k, v in trace_results.items()
                    if not isinstance(v, (pd.DataFrame, np.ndarray))
                }
        
        with open(summary_path, 'w') as f:
            json.dump(summary, f, indent=2, default=str)
        
        # Save detailed results to HDF5
        if any('cycles' in r for r in results.get('fatigue', {}).values()):
            hdf_path = self.config.output_dir / 'detailed_results.h5'
            with pd.HDFStore(hdf_path, 'w') as store:
                for trace_name, trace_results in results.get('fatigue', {}).items():
                    if 'cycles' in trace_results:
                        store[f'cycles/{trace_name}'] = trace_results['cycles']
        
        logger.info(f"Intermediate results saved to {self.config.output_dir}")
    
    def _generate_reports(self, results: Dict):
        """Generate analysis reports"""
        # Create summary DataFrame
        summary_data = []
        
        for trace_name in results.get('fatigue', {}).keys():
            row = {'trace': trace_name}
            
            # Add fatigue results
            if 'fatigue' in results and trace_name in results['fatigue']:
                fatigue = results['fatigue'][trace_name]
                row['damage_per_year'] = fatigue['damage_per_hour'] * 24 * 365
                row['damage_design_life'] = fatigue['damage_design_life']
                row['life_years'] = fatigue['life_years']
                row['max_range'] = fatigue['statistics']['max_range']
                row['total_cycles'] = fatigue['statistics']['total_cycles']
            
            # Add spectral results
            if 'spectral' in results and trace_name in results['spectral']:
                spectral = results['spectral'][trace_name]
                row['dominant_frequency'] = spectral['dominant_frequency']
                row['dominant_period'] = spectral['dominant_period']
                row['total_power'] = spectral['total_power']
            
            summary_data.append(row)
        
        summary_df = pd.DataFrame(summary_data)
        
        # Save to Excel
        excel_path = self.config.output_dir / 'analysis_report.xlsx'
        with pd.ExcelWriter(excel_path, engine='openpyxl') as writer:
            summary_df.to_excel(writer, sheet_name='Summary', index=False)
            
            # Add detailed sheets if needed
            if 'fatigue' in results:
                fatigue_df = pd.DataFrame([
                    {
                        'trace': k,
                        **v['statistics'],
                        'damage_per_hour': v['damage_per_hour'],
                        'life_years': v['life_years']
                    }
                    for k, v in results['fatigue'].items()
                ])
                fatigue_df.to_excel(writer, sheet_name='Fatigue Details', index=False)
        
        logger.info(f"Report generated: {excel_path}")
        
        return summary_df


def create_default_config(
    file_path: Path,
    objects: List[str],
    variables: List[str],
    output_dir: Path = Path('./results')
) -> TimeTraceConfig:
    """
    Create default configuration for common analysis
    
    Args:
        file_path: Path to OrcaFlex file
        objects: List of object names
        variables: List of variable names
        output_dir: Output directory
        
    Returns:
        TimeTraceConfig with sensible defaults
    """
    return TimeTraceConfig(
        source_type='orcaflex',
        file_path=file_path,
        objects=objects,
        variables=variables,
        preprocessing={
            'remove_outliers': True,
            'outlier_method': 'zscore',
            'outlier_threshold': 4,
            'detrend': True,
            'detrend_method': 'linear'
        },
        fatigue_analysis={
            'sn_standard': 'DNV',
            'sn_class': 'F',
            'scf': 1.0,
            'mean_stress_correction': 'goodman',
            'design_life_years': 25
        },
        spectral_analysis={
            'method': 'welch',
            'window': 'hann',
            'nperseg': 512,
            'n_peaks': 5
        },
        output_dir=output_dir,
        save_intermediate=True,
        generate_report=True,
        parallel_processing=True,
        n_workers=4
    )