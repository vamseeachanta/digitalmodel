"""
OrcaFlex Post-Processing Time Series Module V2

Updated to use the new integrated signal_analysis module for improved performance
and maintainability.
"""

# Standard library imports
import logging
import os
from pathlib import Path
from typing import Dict, List, Optional, Any
from concurrent.futures import ProcessPoolExecutor

# Third party imports
import pandas as pd
import numpy as np

# Digital Model imports
from digitalmodel.common.ETL_components import ETL_components
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.time_trace_processor import (
    OrcaFlexTimeTraceProcessor,
    TimeTraceConfig,
    create_default_config
)

try:
    import OrcFxAPI
except ImportError:
    logging.debug("OrcFxAPI not available")

of_objects = OrcaFlexObjects()


class OPPTimeSeriesV2:
    """
    Modernized OrcaFlex post-processing using integrated signal analysis.
    
    Key improvements:
    - Uses new OrcaFlexTimeTraceProcessor for signal analysis
    - Parallel processing support for multiple traces
    - Cleaner separation of concerns
    - Better error handling and logging
    """
    
    def __init__(self):
        self.results = {}
        self.logger = logging.getLogger(__name__)
        
    def router(self, cfg: Dict) -> Dict:
        """
        Route processing based on configuration.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            Processing results
        """
        results = {}
        
        if "time_series_settings" in cfg:
            settings = cfg["time_series_settings"]
            
            if settings.get("data", False):
                results["time_series"] = self.process_time_series_data(cfg)
                
            if settings.get("fatigue_analysis", False):
                results["fatigue"] = self.process_fatigue_analysis(cfg)
                
            if settings.get("spectral_analysis", False):
                results["spectral"] = self.process_spectral_analysis(cfg)
                
            if settings.get("histogram", False):
                results["histogram"] = self.process_histograms(cfg)
                
        if cfg.get("orcaflex", {}).get("postprocess", {}).get("RAOs", {}).get("flag", False):
            results["RAOs"] = self.process_RAOs(cfg)
            
        return results
    
    def process_time_series_data(self, cfg: Dict) -> Dict:
        """
        Process time series data using new integrated processor.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            Processed time series results
        """
        self.logger.info("Processing time series data with new signal analysis module")
        
        # Extract configuration
        ts_groups = cfg["time_series_settings"]["groups"]
        model_dict = cfg.get("model_dict", {})
        file_name = cfg.get("file_name", "output")
        
        # Create processor configuration
        objects = []
        variables = []
        
        for group in ts_groups:
            for column in group["Columns"]:
                if "ObjectName" in column:
                    objects.append(column["ObjectName"])
                if "Variable" in column:
                    variables.append(column["Variable"])
        
        # Remove duplicates
        objects = list(set(objects))
        variables = list(set(variables))
        
        # Create configuration for new processor
        config = TimeTraceConfig(
            source_type='orcaflex',
            model=model_dict.get("model"),
            objects=objects,
            variables=variables,
            preprocessing=self._get_preprocessing_config(cfg),
            output_dir=Path(cfg.get("Analysis", {}).get("result_folder", "./results")),
            save_intermediate=True,
            generate_report=False
        )
        
        # Process using new module
        processor = OrcaFlexTimeTraceProcessor(config)
        
        # Load time traces
        time_traces = processor._load_time_traces()
        
        # Save by group as in original
        results = {"groups": []}
        for group in ts_groups:
            group_label = group["Label"]
            group_columns = ["Time"] + [col["Label"] for col in group["Columns"]]
            
            # Filter to group columns
            group_df = time_traces[group_columns] if all(c in time_traces.columns for c in group_columns) else pd.DataFrame()
            
            # Save to file
            file_name_stem = Path(file_name).stem
            output_file = os.path.join(
                cfg["Analysis"]["result_folder"],
                f"{file_name_stem}_{group_label}.csv"
            )
            
            csv_decimal = cfg.get("orcaflex", {}).get("postprocess", {}).get("time_series", {}).get("csv_decimal", 6)
            group_df.round(csv_decimal).to_csv(output_file, index=False)
            
            results["groups"].append({
                "label": group_label,
                "data": output_file
            })
        
        return results
    
    def process_fatigue_analysis(self, cfg: Dict) -> Dict:
        """
        Process fatigue analysis using new integrated module.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            Fatigue analysis results
        """
        self.logger.info("Processing fatigue analysis with new module")
        
        # Extract objects and variables for fatigue
        fatigue_cfg = cfg.get("fatigue_analysis", {})
        
        config = TimeTraceConfig(
            source_type='orcaflex',
            model=cfg.get("model_dict", {}).get("model"),
            objects=fatigue_cfg.get("objects", []),
            variables=fatigue_cfg.get("variables", ["Tension"]),
            preprocessing={
                'remove_outliers': True,
                'detrend': True
            },
            fatigue_analysis={
                'sn_standard': fatigue_cfg.get('sn_standard', 'DNV'),
                'sn_class': fatigue_cfg.get('sn_class', 'F'),
                'scf': fatigue_cfg.get('scf', 1.0),
                'mean_stress_correction': fatigue_cfg.get('mean_stress_correction', 'goodman'),
                'design_life_years': fatigue_cfg.get('design_life_years', 25)
            },
            spectral_analysis={},  # Disable spectral for fatigue-only
            output_dir=Path(cfg.get("Analysis", {}).get("result_folder", "./results")),
            generate_report=True,
            parallel_processing=True,
            n_workers=4
        )
        
        processor = OrcaFlexTimeTraceProcessor(config)
        results = processor.process()
        
        return results.get('fatigue', {})
    
    def process_spectral_analysis(self, cfg: Dict) -> Dict:
        """
        Process spectral analysis using new integrated module.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            Spectral analysis results
        """
        self.logger.info("Processing spectral analysis with new module")
        
        spectral_cfg = cfg.get("spectral_analysis", {})
        
        config = TimeTraceConfig(
            source_type='orcaflex',
            model=cfg.get("model_dict", {}).get("model"),
            objects=spectral_cfg.get("objects", []),
            variables=spectral_cfg.get("variables", []),
            preprocessing={
                'detrend': True,
                'smooth': spectral_cfg.get('smooth', False)
            },
            fatigue_analysis={},  # Disable fatigue for spectral-only
            spectral_analysis={
                'method': spectral_cfg.get('method', 'welch'),
                'window': spectral_cfg.get('window', 'hann'),
                'nperseg': spectral_cfg.get('nperseg', 512),
                'n_peaks': spectral_cfg.get('n_peaks', 5)
            },
            output_dir=Path(cfg.get("Analysis", {}).get("result_folder", "./results")),
            generate_report=True
        )
        
        processor = OrcaFlexTimeTraceProcessor(config)
        results = processor.process()
        
        return results.get('spectral', {})
    
    def process_histograms(self, cfg: Dict) -> Dict:
        """
        Process rainflow histograms using new module.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            Histogram results
        """
        self.logger.info("Processing histograms with new rainflow counter")
        
        from digitalmodel.modules.signal_analysis import RainflowCounter
        
        histogram_cfg = cfg.get("histogram_settings", {})
        model = cfg.get("model_dict", {}).get("model")
        
        results = {}
        counter = RainflowCounter(method='astm')
        
        for obj_name in histogram_cfg.get("objects", []):
            for var_name in histogram_cfg.get("variables", []):
                # Get time trace
                time_trace = self._get_time_trace(model, obj_name, var_name, cfg)
                
                if time_trace is not None and len(time_trace) > 0:
                    # Count cycles
                    cycles = counter.count_cycles(time_trace)
                    
                    # Generate histogram
                    bins = histogram_cfg.get("bins", 20)
                    histogram = counter.get_histogram(cycles, bins=bins)
                    
                    # Store result
                    key = f"{obj_name}.{var_name}"
                    results[key] = {
                        'cycles': cycles,
                        'histogram': histogram,
                        'statistics': counter.get_statistics(cycles)
                    }
        
        return results
    
    def process_RAOs(self, cfg: Dict) -> Dict:
        """
        Process Response Amplitude Operators using spectral analysis.
        
        Args:
            cfg: Configuration dictionary
            
        Returns:
            RAO results
        """
        self.logger.info("Processing RAOs with new spectral analyzer")
        
        from digitalmodel.modules.signal_analysis import SpectralAnalyzer
        
        rao_cfg = cfg.get("RAOs", {})
        model = cfg.get("model_dict", {}).get("model")
        
        results = {}
        
        # Get sampling rate
        times = model.SampleTimes() if model else []
        fs = 1.0 / (times[1] - times[0]) if len(times) > 1 else 1.0
        
        analyzer = SpectralAnalyzer(sampling_rate=fs, method='fft')
        
        for rao_set in rao_cfg.get("sets", []):
            # Get reference signal (excitation)
            ref_cfg = rao_set.get("reference", {})
            reference_signal = self._get_time_trace(
                model,
                ref_cfg.get("ObjectName"),
                ref_cfg.get("Variable"),
                cfg
            )
            
            # Get response signals
            response_cfg = rao_set.get("response", {})
            
            for obj_name in response_cfg.get("objects", []):
                for var_name in response_cfg.get("variables", []):
                    response_signal = self._get_time_trace(
                        model, obj_name, var_name, cfg
                    )
                    
                    if reference_signal is not None and response_signal is not None:
                        # Compute transfer function (RAO)
                        ref_spectrum = analyzer.compute_spectrum(reference_signal)
                        resp_spectrum = analyzer.compute_spectrum(response_signal)
                        
                        # Calculate RAO
                        rao = self._calculate_rao(ref_spectrum, resp_spectrum)
                        
                        key = f"{obj_name}.{var_name}"
                        results[key] = {
                            'frequency': rao['frequency'],
                            'amplitude': rao['amplitude'],
                            'phase': rao['phase'],
                            'complex': rao['complex']
                        }
        
        return results
    
    def _get_preprocessing_config(self, cfg: Dict) -> Dict:
        """Extract preprocessing configuration."""
        preproc = cfg.get("preprocessing", {})
        return {
            'remove_outliers': preproc.get('remove_outliers', False),
            'outlier_method': preproc.get('outlier_method', 'zscore'),
            'outlier_threshold': preproc.get('outlier_threshold', 3),
            'detrend': preproc.get('detrend', False),
            'detrend_method': preproc.get('detrend_method', 'linear'),
            'smooth': preproc.get('smooth', False),
            'smooth_method': preproc.get('smooth_method', 'savgol'),
            'smooth_params': preproc.get('smooth_params', {})
        }
    
    def _get_time_trace(self, model: Any, obj_name: str, var_name: str, cfg: Dict) -> Optional[np.ndarray]:
        """Get time trace from OrcaFlex model."""
        try:
            if model and obj_name and var_name:
                obj = model[obj_name]
                time_period = OrcFxAPI.SpecifiedPeriod(0, model.simulationStopTime)
                return obj.TimeHistory(var_name, time_period)
        except Exception as e:
            self.logger.warning(f"Could not get time trace for {obj_name}.{var_name}: {e}")
        return None
    
    def _calculate_rao(self, ref_spectrum: pd.DataFrame, resp_spectrum: pd.DataFrame) -> Dict:
        """Calculate RAO from reference and response spectra."""
        # Ensure same frequency grid
        freq = ref_spectrum['frequency'].values
        ref_power = ref_spectrum['power'].values
        resp_power = resp_spectrum['power'].values
        
        # Avoid division by zero
        eps = 1e-10
        ref_power = np.where(ref_power < eps, eps, ref_power)
        
        # Calculate transfer function
        H = resp_power / ref_power
        amplitude = np.abs(H)
        phase = np.angle(H)
        
        return {
            'frequency': freq,
            'amplitude': amplitude,
            'phase': phase,
            'complex': H
        }


def migrate_configuration(old_cfg: Dict) -> Dict:
    """
    Migrate old configuration to new format.
    
    Args:
        old_cfg: Old configuration dictionary
        
    Returns:
        New configuration compatible with V2
    """
    new_cfg = old_cfg.copy()
    
    # Map old time_series_components to new signal_analysis
    if 'time_series_components' in new_cfg:
        new_cfg['signal_analysis'] = new_cfg.pop('time_series_components')
    
    # Update method names
    if 'analysis' in new_cfg:
        if 'fft' in new_cfg['analysis']:
            fft_cfg = new_cfg['analysis']['fft']
            if 'window_average_fft' in fft_cfg:
                fft_cfg['window_averaged_fft'] = fft_cfg.pop('window_average_fft')
    
    # Add new processing options
    if 'signal_analysis' not in new_cfg:
        new_cfg['signal_analysis'] = {
            'parallel_processing': True,
            'n_workers': 4
        }
    
    return new_cfg