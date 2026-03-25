"""
Time series processing utilities

Tools for time series manipulation, filtering, and analysis.
"""

import numpy as np
import pandas as pd
import scipy.signal
import scipy.interpolate
import scipy.stats
import scipy.ndimage
from typing import Union, Optional, Tuple, Dict, Any
import logging
import warnings

logger = logging.getLogger(__name__)


class TimeSeriesProcessor:
    """
    Time series processing utilities
    
    Provides tools for detrending, smoothing, resampling, and
    general time series manipulation.
    """
    
    def __init__(self):
        """Initialize time series processor"""
        pass
    
    def detrend(self, signal: Union[list, np.ndarray],
               method: str = 'linear',
               order: int = 1) -> np.ndarray:
        """
        Remove trend from signal
        
        Parameters
        ----------
        signal : array-like
            Input signal
        method : str, default='linear'
            Detrending method: 'none', 'mean', 'linear', 'polynomial'
        order : int, default=1
            Polynomial order (for polynomial method)
            
        Returns
        -------
        np.ndarray
            Detrended signal
        """
        signal = np.asarray(signal)
        
        if method == 'none':
            return signal
        elif method == 'mean':
            return signal - np.mean(signal)
        elif method == 'linear':
            return scipy.signal.detrend(signal, type='linear')
        elif method == 'polynomial':
            # Fit polynomial
            x = np.arange(len(signal))
            coeffs = np.polyfit(x, signal, order)
            trend = np.polyval(coeffs, x)
            return signal - trend
        else:
            raise ValueError(f"Unknown detrend method: {method}")
    
    def smooth(self, signal: Union[list, np.ndarray],
              method: str = 'moving_average',
              window_size: int = 5,
              **kwargs) -> np.ndarray:
        """
        Smooth signal
        
        Parameters
        ----------
        signal : array-like
            Input signal
        method : str, default='moving_average'
            Smoothing method: 'moving_average', 'savgol', 'gaussian', 'median'
        window_size : int, default=5
            Window size for smoothing
        **kwargs
            Additional method-specific parameters
            
        Returns
        -------
        np.ndarray
            Smoothed signal
        """
        signal = np.asarray(signal)
        
        if method == 'moving_average':
            return self._moving_average(signal, window_size)
        elif method == 'savgol':
            polyorder = kwargs.get('polyorder', 2)
            return scipy.signal.savgol_filter(signal, window_size, polyorder)
        elif method == 'gaussian':
            sigma = kwargs.get('sigma', 1.0)
            return scipy.ndimage.gaussian_filter1d(signal, sigma)
        elif method == 'median':
            return scipy.signal.medfilt(signal, kernel_size=window_size)
        else:
            raise ValueError(f"Unknown smoothing method: {method}")
    
    def _moving_average(self, signal: np.ndarray, window_size: int) -> np.ndarray:
        """
        Apply moving average filter
        
        Parameters
        ----------
        signal : np.ndarray
            Input signal
        window_size : int
            Window size
            
        Returns
        -------
        np.ndarray
            Smoothed signal
        """
        # Use convolution for efficiency
        window = np.ones(window_size) / window_size
        
        # Pad signal to handle edges
        pad_width = window_size // 2
        padded = np.pad(signal, pad_width, mode='edge')
        
        # Apply convolution
        smoothed = np.convolve(padded, window, mode='valid')
        
        # Ensure same length as input
        if len(smoothed) > len(signal):
            smoothed = smoothed[:len(signal)]
        elif len(smoothed) < len(signal):
            # This shouldn't happen with proper padding
            smoothed = np.pad(smoothed, (0, len(signal) - len(smoothed)), mode='edge')
        
        return smoothed
    
    def resample(self, signal: Union[list, np.ndarray],
                original_rate: float,
                target_rate: float,
                method: str = 'linear') -> Tuple[np.ndarray, np.ndarray]:
        """
        Resample signal to different sampling rate
        
        Parameters
        ----------
        signal : array-like
            Input signal
        original_rate : float
            Original sampling rate (Hz)
        target_rate : float
            Target sampling rate (Hz)
        method : str, default='linear'
            Interpolation method: 'linear', 'cubic', 'fft'
            
        Returns
        -------
        resampled_signal : np.ndarray
            Resampled signal
        new_time : np.ndarray
            New time array
        """
        signal = np.asarray(signal)
        
        # Calculate resampling ratio
        ratio = target_rate / original_rate
        new_length = int(len(signal) * ratio)
        
        if method == 'fft':
            # Use scipy's resample (FFT-based)
            resampled = scipy.signal.resample(signal, new_length)
        else:
            # Use interpolation
            old_time = np.arange(len(signal)) / original_rate
            new_time = np.arange(new_length) / target_rate
            
            if method == 'linear':
                f = scipy.interpolate.interp1d(old_time, signal, kind='linear',
                                              fill_value='extrapolate')
            elif method == 'cubic':
                f = scipy.interpolate.interp1d(old_time, signal, kind='cubic',
                                              fill_value='extrapolate')
            else:
                raise ValueError(f"Unknown interpolation method: {method}")
            
            resampled = f(new_time)
        
        # Generate new time array
        new_time = np.arange(new_length) / target_rate
        
        logger.info(f"Resampled from {len(signal)} to {new_length} samples")
        
        return resampled, new_time
    
    def remove_outliers(self, signal: Union[list, np.ndarray],
                       method: str = 'zscore',
                       threshold: float = 3.0,
                       replace: str = 'interpolate') -> np.ndarray:
        """
        Remove outliers from signal
        
        Parameters
        ----------
        signal : array-like
            Input signal
        method : str, default='zscore'
            Outlier detection method: 'zscore', 'iqr', 'isolation'
        threshold : float, default=3.0
            Threshold for outlier detection
        replace : str, default='interpolate'
            How to replace outliers: 'interpolate', 'mean', 'median', 'remove'
            
        Returns
        -------
        np.ndarray
            Signal with outliers removed/replaced
        """
        signal = np.asarray(signal).copy()
        
        # Detect outliers
        if method == 'zscore':
            z_scores = np.abs((signal - np.mean(signal)) / np.std(signal))
            outlier_mask = z_scores > threshold
        elif method == 'iqr':
            q1 = np.percentile(signal, 25)
            q3 = np.percentile(signal, 75)
            iqr = q3 - q1
            lower_bound = q1 - threshold * iqr
            upper_bound = q3 + threshold * iqr
            outlier_mask = (signal < lower_bound) | (signal > upper_bound)
        else:
            raise ValueError(f"Unknown outlier method: {method}")
        
        # Replace outliers
        if np.any(outlier_mask):
            if replace == 'interpolate':
                # Linear interpolation
                good_indices = np.where(~outlier_mask)[0]
                bad_indices = np.where(outlier_mask)[0]
                
                if len(good_indices) > 1:
                    signal[outlier_mask] = np.interp(bad_indices, good_indices,
                                                    signal[good_indices])
                else:
                    # Fall back to mean if not enough good points
                    signal[outlier_mask] = np.mean(signal[~outlier_mask])
                    
            elif replace == 'mean':
                signal[outlier_mask] = np.mean(signal[~outlier_mask])
            elif replace == 'median':
                signal[outlier_mask] = np.median(signal[~outlier_mask])
            elif replace == 'remove':
                signal = signal[~outlier_mask]
            else:
                raise ValueError(f"Unknown replace method: {replace}")
        
        logger.info(f"Removed/replaced {np.sum(outlier_mask)} outliers")
        
        return signal
    
    def calculate_statistics(self, signal: Union[list, np.ndarray]) -> Dict[str, float]:
        """
        Calculate comprehensive signal statistics
        
        Parameters
        ----------
        signal : array-like
            Input signal
            
        Returns
        -------
        dict
            Dictionary of statistics
        """
        signal = np.asarray(signal)
        
        stats = {
            # Basic statistics
            'mean': np.mean(signal),
            'std': np.std(signal),
            'var': np.var(signal),
            'min': np.min(signal),
            'max': np.max(signal),
            'range': np.ptp(signal),
            
            # Percentiles
            'median': np.median(signal),
            'q1': np.percentile(signal, 25),
            'q3': np.percentile(signal, 75),
            'iqr': np.percentile(signal, 75) - np.percentile(signal, 25),
            
            # Higher moments
            'skewness': scipy.stats.skew(signal),
            'kurtosis': scipy.stats.kurtosis(signal),
            
            # RMS and energy
            'rms': np.sqrt(np.mean(signal ** 2)),
            'energy': np.sum(signal ** 2),
            
            # Peak values
            'peak_to_peak': np.ptp(signal),
            'crest_factor': np.max(np.abs(signal)) / np.sqrt(np.mean(signal ** 2)),
            
            # Signal properties
            'length': len(signal),
            'zero_crossings': np.sum(np.diff(np.sign(signal)) != 0) // 2
        }
        
        return stats
    
    def find_zero_crossings(self, signal: Union[list, np.ndarray],
                           direction: str = 'both') -> np.ndarray:
        """
        Find zero crossing indices
        
        Parameters
        ----------
        signal : array-like
            Input signal
        direction : str, default='both'
            Crossing direction: 'both', 'positive', 'negative'
            
        Returns
        -------
        np.ndarray
            Indices of zero crossings
        """
        signal = np.asarray(signal)
        
        # Calculate sign changes
        sign = np.sign(signal)
        sign_change = np.diff(sign)
        
        if direction == 'both':
            # Any zero crossing
            crossings = np.where(sign_change != 0)[0]
        elif direction == 'positive':
            # Negative to positive
            crossings = np.where(sign_change > 0)[0]
        elif direction == 'negative':
            # Positive to negative
            crossings = np.where(sign_change < 0)[0]
        else:
            raise ValueError(f"Unknown direction: {direction}")
        
        return crossings
    
    def calculate_envelope(self, signal: Union[list, np.ndarray],
                          method: str = 'hilbert') -> Tuple[np.ndarray, np.ndarray]:
        """
        Calculate signal envelope
        
        Parameters
        ----------
        signal : array-like
            Input signal
        method : str, default='hilbert'
            Envelope method: 'hilbert', 'peak'
            
        Returns
        -------
        upper_envelope : np.ndarray
            Upper envelope
        lower_envelope : np.ndarray
            Lower envelope
        """
        signal = np.asarray(signal)
        
        if method == 'hilbert':
            # Hilbert transform
            analytic_signal = scipy.signal.hilbert(signal)
            amplitude_envelope = np.abs(analytic_signal)
            
            upper_envelope = amplitude_envelope
            lower_envelope = -amplitude_envelope
            
        elif method == 'peak':
            # Find peaks and valleys
            peaks, _ = scipy.signal.find_peaks(signal)
            valleys, _ = scipy.signal.find_peaks(-signal)
            valleys_values = -signal[valleys]
            
            if len(peaks) > 1 and len(valleys) > 1:
                # Interpolate between peaks/valleys
                upper_envelope = np.interp(np.arange(len(signal)), peaks, signal[peaks])
                lower_envelope = np.interp(np.arange(len(signal)), valleys, signal[valleys])
            else:
                # Fall back to constant envelope
                upper_envelope = np.full_like(signal, np.max(signal))
                lower_envelope = np.full_like(signal, np.min(signal))
        else:
            raise ValueError(f"Unknown envelope method: {method}")
        
        return upper_envelope, lower_envelope
    
    def segment_signal(self, signal: Union[list, np.ndarray],
                      segment_length: int,
                      overlap: float = 0.0) -> list:
        """
        Segment signal into overlapping windows
        
        Parameters
        ----------
        signal : array-like
            Input signal
        segment_length : int
            Length of each segment
        overlap : float, default=0.0
            Overlap fraction (0 to 1)
            
        Returns
        -------
        list
            List of signal segments
        """
        signal = np.asarray(signal)
        
        # Calculate step size
        step = int(segment_length * (1 - overlap))
        
        # Extract segments
        segments = []
        for start in range(0, len(signal) - segment_length + 1, step):
            segment = signal[start:start + segment_length]
            segments.append(segment)
        
        logger.info(f"Created {len(segments)} segments of length {segment_length}")
        
        return segments