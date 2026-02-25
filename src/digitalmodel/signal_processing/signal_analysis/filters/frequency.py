"""
Frequency domain filtering implementations

Provides various digital filter designs and frequency domain filtering tools.
"""

import numpy as np
import scipy.signal
from typing import Union, Tuple, Optional, Dict
import logging

logger = logging.getLogger(__name__)


class FrequencyFilter:
    """
    Frequency domain filter implementation
    
    Supports various filter types and designs.
    """
    
    def __init__(self, filter_type: str = 'butterworth', order: int = 4):
        """
        Initialize frequency filter
        
        Parameters
        ----------
        filter_type : str
            Filter type: 'butterworth', 'chebyshev1', 'chebyshev2', 'elliptic', 'bessel'
        order : int
            Filter order
        """
        self.filter_type = filter_type
        self.order = order
        self.last_filter = None
    
    def design_filter(self, cutoff_freq: Union[float, Tuple[float, float]],
                     sampling_rate: float,
                     btype: str = 'lowpass',
                     **kwargs) -> Tuple[np.ndarray, np.ndarray]:
        """
        Design digital filter
        
        Parameters
        ----------
        cutoff_freq : float or tuple
            Cutoff frequency/frequencies (Hz)
        sampling_rate : float
            Sampling rate (Hz)
        btype : str
            Filter band type: 'lowpass', 'highpass', 'bandpass', 'bandstop'
        **kwargs
            Additional filter-specific parameters
            
        Returns
        -------
        b, a : ndarray
            Filter coefficients
        """
        # Normalize frequencies
        nyquist = sampling_rate / 2
        
        if isinstance(cutoff_freq, (list, tuple)):
            Wn = [f / nyquist for f in cutoff_freq]
        else:
            Wn = cutoff_freq / nyquist
        
        # Design filter based on type
        if self.filter_type == 'butterworth':
            b, a = scipy.signal.butter(self.order, Wn, btype=btype)
            
        elif self.filter_type == 'chebyshev1':
            rp = kwargs.get('rp', 1)  # Passband ripple (dB)
            b, a = scipy.signal.cheby1(self.order, rp, Wn, btype=btype)
            
        elif self.filter_type == 'chebyshev2':
            rs = kwargs.get('rs', 40)  # Stopband attenuation (dB)
            b, a = scipy.signal.cheby2(self.order, rs, Wn, btype=btype)
            
        elif self.filter_type == 'elliptic':
            rp = kwargs.get('rp', 1)  # Passband ripple
            rs = kwargs.get('rs', 40)  # Stopband attenuation
            b, a = scipy.signal.ellip(self.order, rp, rs, Wn, btype=btype)
            
        elif self.filter_type == 'bessel':
            b, a = scipy.signal.bessel(self.order, Wn, btype=btype)
            
        else:
            raise ValueError(f"Unknown filter type: {self.filter_type}")
        
        self.last_filter = (b, a)
        logger.info(f"Designed {self.filter_type} {btype} filter, order={self.order}")
        
        return b, a
    
    def apply_filter(self, signal: Union[list, np.ndarray],
                    b: np.ndarray, a: np.ndarray,
                    zero_phase: bool = True) -> np.ndarray:
        """
        Apply filter to signal
        
        Parameters
        ----------
        signal : array-like
            Input signal
        b, a : ndarray
            Filter coefficients
        zero_phase : bool
            Use zero-phase filtering
            
        Returns
        -------
        ndarray
            Filtered signal
        """
        signal = np.asarray(signal)
        
        if zero_phase:
            # Zero-phase filtering (no phase distortion)
            filtered = scipy.signal.filtfilt(b, a, signal)
        else:
            # Standard filtering
            filtered = scipy.signal.lfilter(b, a, signal)
        
        return filtered
    
    def bandpass_filter(self, signal: Union[list, np.ndarray],
                       low_freq: float, high_freq: float,
                       sampling_rate: float,
                       zero_phase: bool = True) -> np.ndarray:
        """
        Apply bandpass filter
        
        Parameters
        ----------
        signal : array-like
            Input signal
        low_freq : float
            Low cutoff frequency (Hz)
        high_freq : float
            High cutoff frequency (Hz)
        sampling_rate : float
            Sampling rate (Hz)
        zero_phase : bool
            Use zero-phase filtering
            
        Returns
        -------
        ndarray
            Filtered signal
        """
        b, a = self.design_filter(
            (low_freq, high_freq),
            sampling_rate,
            btype='bandpass'
        )
        
        return self.apply_filter(signal, b, a, zero_phase)
    
    def bandstop_filter(self, signal: Union[list, np.ndarray],
                       low_freq: float, high_freq: float,
                       sampling_rate: float,
                       zero_phase: bool = True) -> np.ndarray:
        """
        Apply bandstop (notch) filter
        
        Parameters
        ----------
        signal : array-like
            Input signal
        low_freq : float
            Low cutoff frequency (Hz)
        high_freq : float
            High cutoff frequency (Hz)
        sampling_rate : float
            Sampling rate (Hz)
        zero_phase : bool
            Use zero-phase filtering
            
        Returns
        -------
        ndarray
            Filtered signal
        """
        b, a = self.design_filter(
            (low_freq, high_freq),
            sampling_rate,
            btype='bandstop'
        )
        
        return self.apply_filter(signal, b, a, zero_phase)
    
    def lowpass_filter(self, signal: Union[list, np.ndarray],
                      cutoff_freq: float,
                      sampling_rate: float,
                      zero_phase: bool = True) -> np.ndarray:
        """
        Apply lowpass filter
        
        Parameters
        ----------
        signal : array-like
            Input signal
        cutoff_freq : float
            Cutoff frequency (Hz)
        sampling_rate : float
            Sampling rate (Hz)
        zero_phase : bool
            Use zero-phase filtering
            
        Returns
        -------
        ndarray
            Filtered signal
        """
        b, a = self.design_filter(cutoff_freq, sampling_rate, btype='lowpass')
        return self.apply_filter(signal, b, a, zero_phase)
    
    def highpass_filter(self, signal: Union[list, np.ndarray],
                       cutoff_freq: float,
                       sampling_rate: float,
                       zero_phase: bool = True) -> np.ndarray:
        """
        Apply highpass filter
        
        Parameters
        ----------
        signal : array-like
            Input signal
        cutoff_freq : float
            Cutoff frequency (Hz)
        sampling_rate : float
            Sampling rate (Hz)
        zero_phase : bool
            Use zero-phase filtering
            
        Returns
        -------
        ndarray
            Filtered signal
        """
        b, a = self.design_filter(cutoff_freq, sampling_rate, btype='highpass')
        return self.apply_filter(signal, b, a, zero_phase)
    
    def get_frequency_response(self, b: Optional[np.ndarray] = None,
                              a: Optional[np.ndarray] = None,
                              worN: int = 512,
                              fs: Optional[float] = None) -> Tuple[np.ndarray, np.ndarray]:
        """
        Get frequency response of filter
        
        Parameters
        ----------
        b, a : ndarray, optional
            Filter coefficients (uses last filter if not provided)
        worN : int
            Number of frequency points
        fs : float, optional
            Sampling frequency
            
        Returns
        -------
        w : ndarray
            Frequencies
        h : ndarray
            Complex frequency response
        """
        if b is None or a is None:
            if self.last_filter is None:
                raise ValueError("No filter designed yet")
            b, a = self.last_filter
        
        w, h = scipy.signal.freqz(b, a, worN=worN, fs=fs)
        
        return w, h
    
    def adaptive_filter(self, signal: Union[list, np.ndarray],
                       reference: Union[list, np.ndarray],
                       n_taps: int = 32,
                       mu: float = 0.01) -> Tuple[np.ndarray, np.ndarray]:
        """
        Adaptive filtering using LMS algorithm
        
        Parameters
        ----------
        signal : array-like
            Input signal
        reference : array-like
            Reference signal
        n_taps : int
            Number of filter taps
        mu : float
            Step size
            
        Returns
        -------
        filtered : ndarray
            Filtered signal
        weights : ndarray
            Final filter weights
        """
        signal = np.asarray(signal)
        reference = np.asarray(reference)
        
        n = len(signal)
        weights = np.zeros(n_taps)
        filtered = np.zeros(n)
        
        # Pad signal for convolution
        padded_signal = np.pad(signal, (n_taps-1, 0), mode='constant')
        
        for i in range(n):
            # Get input vector
            x = padded_signal[i:i+n_taps][::-1]
            
            # Filter output
            y = np.dot(weights, x)
            filtered[i] = y
            
            # Error
            if i < len(reference):
                error = reference[i] - y
                
                # Update weights (LMS)
                weights += mu * error * x
        
        return filtered, weights