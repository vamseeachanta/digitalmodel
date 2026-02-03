"""
Compatibility adapters for migrating from existing implementations

These adapters provide backward compatibility for code using the old
time_series_components and fatigue_analysis modules.
"""

import numpy as np
import pandas as pd
import warnings
from typing import Union, Tuple, Dict, Any, Optional
import logging

from .core.rainflow import RainflowCounter
from .core.spectral import SpectralAnalyzer
from .core.timeseries import TimeSeriesProcessor

logger = logging.getLogger(__name__)


class TimeSeriesComponentsAdapter:
    """
    Adapter for legacy TimeSeriesComponents code
    
    Provides backward compatibility for existing code using
    the old time_series_components module.
    """
    
    def __init__(self, cfg: Optional[Dict] = None):
        """
        Initialize adapter with legacy configuration
        
        Parameters
        ----------
        cfg : dict, optional
            Legacy configuration dictionary
        """
        self.cfg = cfg or {}
        self.rainflow_counter = RainflowCounter()
        self.spectral_analyzer = SpectralAnalyzer()
        self.timeseries_processor = TimeSeriesProcessor()
        
        warnings.warn(
            "TimeSeriesComponents is deprecated. "
            "Please migrate to signal_analysis module.",
            DeprecationWarning,
            stacklevel=2
        )
    
    def get_rainflow_count_from_time_series(self, time_series: Union[list, np.ndarray]) -> Tuple[pd.DataFrame, Dict]:
        """
        Legacy rainflow counting method
        
        Parameters
        ----------
        time_series : array-like
            Input signal
            
        Returns
        -------
        df : pd.DataFrame
            Cycles DataFrame
        dict : dict
            Cycles dictionary
        """
        logger.info("Using legacy rainflow counting interface")
        
        # Use new implementation
        df = self.rainflow_counter.count_cycles(time_series, extract_info=True)
        dict_result = df.to_dict()
        
        return df, dict_result
    
    def window_average_fft(self, cfg: Dict, signal: Union[list, np.ndarray], time_step: float) -> pd.DataFrame:
        """
        Legacy window-averaged FFT method
        
        Parameters
        ----------
        cfg : dict
            Configuration dictionary
        signal : array-like
            Input signal
        time_step : float
            Time step between samples
            
        Returns
        -------
        pd.DataFrame
            FFT results
        """
        logger.info("Using legacy window-averaged FFT interface")
        
        # Extract parameters from legacy config
        fft_cfg = cfg.get('fft', {})
        window_size = fft_cfg.get('window', {}).get('size', 1024)
        overlap = fft_cfg.get('window', {}).get('overlap', 0.5)
        
        # Calculate sampling rate
        sampling_rate = 1.0 / time_step
        
        # Use new implementation
        self.spectral_analyzer.sampling_rate = sampling_rate
        spectrum = self.spectral_analyzer.window_averaged_fft(
            signal, 
            window_size=window_size,
            overlap=overlap
        )
        
        # Rename columns for compatibility
        spectrum = spectrum.rename(columns={
            'frequency': 'fft_freq',
            'magnitude': 'fft'
        })
        
        return spectrum
    
    def fft_window_analysis(self, cfg: Optional[Dict], signal: Union[list, np.ndarray], 
                           time_step: float, time_window: Optional[int] = None) -> pd.DataFrame:
        """
        Legacy FFT window analysis
        
        Parameters
        ----------
        cfg : dict, optional
            Configuration
        signal : array-like
            Input signal
        time_step : float
            Time step
        time_window : int, optional
            Window size
            
        Returns
        -------
        pd.DataFrame
            FFT results
        """
        if cfg is None:
            cfg = self.cfg
            
        return self.window_average_fft(cfg, signal, time_step)
    
    def get_filtered_fft(self, cfg: Dict, average_fft_df: pd.DataFrame) -> pd.DataFrame:
        """
        Legacy FFT filtering method
        
        Parameters
        ----------
        cfg : dict
            Configuration
        average_fft_df : pd.DataFrame
            FFT DataFrame
            
        Returns
        -------
        pd.DataFrame
            Filtered FFT
        """
        logger.info("Using legacy FFT filtering interface")
        
        # Extract filter parameters
        fft_cfg = cfg.get('fft', {})
        if fft_cfg.get('filter', {}).get('flag', False):
            min_freq = fft_cfg['filter'].get('min_frequency', 0)
            max_freq = fft_cfg['filter'].get('max_frequency', float('inf'))
            
            # Apply filter
            if 'fft_freq' in average_fft_df.columns:
                freq_col = 'fft_freq'
            else:
                freq_col = 'frequency'
                
            mask = (average_fft_df[freq_col] >= min_freq) & (average_fft_df[freq_col] <= max_freq)
            return average_fft_df[mask].copy()
        
        return average_fft_df
    
    def fft_analysis(self, cfg: Dict, signal: Union[list, np.ndarray], 
                     time_step: float) -> Tuple[Dict, Dict]:
        """
        Legacy FFT analysis method
        
        Parameters
        ----------
        cfg : dict
            Configuration
        signal : array-like
            Input signal
        time_step : float
            Time step
            
        Returns
        -------
        signal_dict : dict
            Signal and FFT data
        filtered_signal_dict : dict
            Filtered signal and FFT data
        """
        logger.info("Using legacy FFT analysis interface")
        
        # Calculate sampling rate
        sampling_rate = 1.0 / time_step
        
        # Compute FFT
        self.spectral_analyzer.sampling_rate = sampling_rate
        fft_df = self.spectral_analyzer.compute_spectrum(signal)
        
        # Rename columns for compatibility
        fft_df = fft_df.rename(columns={
            'frequency': 'fft_freq',
            'magnitude': 'fft'
        })
        
        signal_dict = {
            'time_trace': signal,
            'fft': fft_df
        }
        
        # Apply filtering if configured
        filtered_signal_dict = self.get_filtered_signal(cfg.get('fft', {}), signal_dict)
        
        return signal_dict, filtered_signal_dict
    
    def get_filtered_signal(self, cfg_fft: Dict, signal_dict: Dict) -> Dict:
        """
        Legacy signal filtering method
        
        Parameters
        ----------
        cfg_fft : dict
            FFT configuration
        signal_dict : dict
            Signal dictionary
            
        Returns
        -------
        dict
            Filtered signal dictionary
        """
        if not cfg_fft.get('filter', {}).get('band_pass', {}).get('flag', False):
            return signal_dict
            
        # Extract filter parameters
        min_freq = cfg_fft['filter']['band_pass'].get('frequency_minimum', 0)
        max_freq = cfg_fft['filter']['band_pass'].get('frequency_maximum', float('inf'))
        
        # Filter in frequency domain
        fft_df = signal_dict['fft']
        if 'fft_freq' in fft_df.columns:
            freq_col = 'fft_freq'
        else:
            freq_col = 'frequency'
            
        # Create filtered spectrum
        filtered_fft = self.spectral_analyzer.filter_spectrum(
            fft_df,
            filter_type='bandpass',
            low_freq=min_freq,
            high_freq=max_freq
        )
        
        # Reconstruct time domain signal if possible
        if 'fft_complex' in filtered_fft.columns:
            filtered_signal = self.spectral_analyzer.inverse_transform(filtered_fft)
        else:
            filtered_signal = signal_dict['time_trace']
        
        return {
            'time_trace': filtered_signal,
            'fft': filtered_fft
        }


class FatigueAnalysisAdapter:
    """
    Adapter for legacy fatigue analysis code
    
    Provides backward compatibility for existing fatigue analysis implementations.
    """
    
    def __init__(self):
        """Initialize fatigue analysis adapter"""
        self.rainflow_counter = RainflowCounter()
        
        warnings.warn(
            "FatigueAnalysis is deprecated. "
            "Please migrate to signal_analysis.fatigue module.",
            DeprecationWarning,
            stacklevel=2
        )
    
    def get_rainflow_from_timetrace(self, timetrace: Union[list, np.ndarray]) -> Tuple[pd.DataFrame, Dict]:
        """
        Legacy rainflow extraction from time trace
        
        Parameters
        ----------
        timetrace : array-like
            Input time series
            
        Returns
        -------
        rainflow_df : pd.DataFrame
            Rainflow cycles
        rainflow_dict : dict
            Rainflow dictionary
        """
        logger.info("Using legacy fatigue rainflow interface")
        
        # Use new implementation
        rainflow_df = self.rainflow_counter.count_cycles(timetrace, extract_info=True)
        rainflow_dict = rainflow_df.to_dict()
        
        return rainflow_df, rainflow_dict
    
    def damage_from_rainflow_cycles(self, rainflow_df: pd.DataFrame, 
                                   fatigue_curve: Dict) -> float:
        """
        Legacy damage calculation from rainflow cycles
        
        Parameters
        ----------
        rainflow_df : pd.DataFrame
            Rainflow cycles
        fatigue_curve : dict
            S-N curve parameters
            
        Returns
        -------
        float
            Cumulative damage
        """
        logger.info("Using legacy damage calculation interface")
        
        # Miner's rule damage accumulation
        total_damage = 0.0
        
        # Extract S-N curve parameters
        # Assuming format: N = A * S^(-m)
        A = fatigue_curve.get('A', 1e12)
        m = fatigue_curve.get('m', 3.0)
        
        for _, cycle in rainflow_df.iterrows():
            stress_range = cycle['range']
            n_cycles = cycle['count']
            
            # Calculate allowable cycles from S-N curve
            N_allowable = A * (stress_range ** (-m))
            
            # Accumulate damage
            damage = n_cycles / N_allowable
            total_damage += damage
        
        return total_damage


class OrcaFlexAdapter:
    """
    Adapter for OrcaFlex-dependent code
    
    Provides alternative implementations for OrcaFlex-specific functionality.
    """
    
    def __init__(self):
        """Initialize OrcaFlex adapter"""
        self.rainflow_counter = RainflowCounter()
        
        warnings.warn(
            "OrcaFlex rainflow implementation replaced with open-source alternative. "
            "Results may differ slightly from OrcaFlex API.",
            UserWarning,
            stacklevel=2
        )
    
    def RainflowHalfCycles(self, signal: Union[list, np.ndarray], 
                          period: Optional[int] = None) -> list:
        """
        Replacement for OrcaFlex RainflowHalfCycles method
        
        Parameters
        ----------
        signal : array-like
            Input signal
        period : int, optional
            Not used (for compatibility)
            
        Returns
        -------
        list
            Half cycles
        """
        logger.info("Using OrcaFlex adapter for rainflow counting")
        
        # Use half-cycle counting
        counter = RainflowCounter(method='half_cycle')
        cycles_df = counter.count_cycles(signal, extract_info=False)
        
        # Convert to OrcaFlex-like format
        half_cycles = []
        for _, cycle in cycles_df.iterrows():
            if cycle['count'] == 0.5:
                half_cycles.append(cycle['range'])
        
        return half_cycles


def create_migration_guide():
    """
    Create a migration guide document
    
    Returns
    -------
    str
        Migration guide text
    """
    guide = """
    # Migration Guide: Signal Analysis Module
    
    ## Quick Migration
    
    ### TimeSeriesComponents Migration
    
    Old code:
    ```python
    from digitalmodel.signal_processing.time_series.time_series_components import TimeSeriesComponents
    
    tsc = TimeSeriesComponents(cfg)
    cycles_df, cycles_dict = tsc.get_rainflow_count_from_time_series(signal)
    fft_df = tsc.window_average_fft(cfg, signal, time_step)
    ```
    
    New code:
    ```python
    from digitalmodel.signal_analysis import RainflowCounter, SpectralAnalyzer
    
    # Rainflow counting
    counter = RainflowCounter()
    cycles_df = counter.count_cycles(signal)
    
    # FFT analysis
    analyzer = SpectralAnalyzer(sampling_rate=1/time_step)
    fft_df = analyzer.window_averaged_fft(signal, window_size=1024)
    ```
    
    ### Using Compatibility Adapter
    
    For gradual migration:
    ```python
    from digitalmodel.signal_processing.signal_analysis.adapters import TimeSeriesComponentsAdapter
    
    # Drop-in replacement
    tsc = TimeSeriesComponentsAdapter(cfg)
    cycles_df, cycles_dict = tsc.get_rainflow_count_from_time_series(signal)
    ```
    
    ## Feature Mapping
    
    | Old Method | New Method | Module |
    |------------|------------|---------|
    | get_rainflow_count_from_time_series | count_cycles | RainflowCounter |
    | window_average_fft | window_averaged_fft | SpectralAnalyzer |
    | fft_window_analysis | compute_spectrum | SpectralAnalyzer |
    | get_filtered_fft | filter_spectrum | SpectralAnalyzer |
    | get_filtered_signal | filter_spectrum + inverse_transform | SpectralAnalyzer |
    
    ## Configuration Changes
    
    Old configuration format:
    ```yaml
    fft:
      window:
        size: 1024
        overlap: 0.5
      filter:
        flag: true
        min_frequency: 0.1
        max_frequency: 10.0
    ```
    
    New API (programmatic):
    ```python
    analyzer = SpectralAnalyzer(sampling_rate=100)
    spectrum = analyzer.window_averaged_fft(signal, window_size=1024, overlap=0.5)
    filtered = analyzer.filter_spectrum(spectrum, 'bandpass', low_freq=0.1, high_freq=10.0)
    ```
    """
    
    return guide