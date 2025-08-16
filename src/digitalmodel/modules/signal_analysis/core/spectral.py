"""
Spectral analysis implementation

FFT and spectral analysis tools for signal processing.
"""

import numpy as np
import pandas as pd
import scipy.signal
import scipy.fftpack
from typing import Union, Tuple, Optional, Dict, Any
import logging
import warnings

logger = logging.getLogger(__name__)


class SpectralAnalyzer:
    """
    Spectral analysis for signal processing
    
    Provides FFT, power spectral density, and frequency domain analysis tools.
    """
    
    def __init__(self, sampling_rate: Optional[float] = None, 
                 method: str = 'fft'):
        """
        Initialize spectral analyzer
        
        Parameters
        ----------
        sampling_rate : float, optional
            Sampling rate in Hz (required for frequency calculations)
        method : str, default='fft'
            Analysis method: 'fft', 'welch', 'periodogram'
        """
        self.sampling_rate = sampling_rate
        self.method = method
        self.last_spectrum = None
        
    def compute_spectrum(self, signal: Union[list, np.ndarray],
                        sampling_rate: Optional[float] = None,
                        **kwargs) -> pd.DataFrame:
        """
        Compute frequency spectrum of signal
        
        Parameters
        ----------
        signal : array-like
            Input signal
        sampling_rate : float, optional
            Sampling rate (uses self.sampling_rate if not provided)
        **kwargs
            Additional parameters for specific methods
            
        Returns
        -------
        pd.DataFrame
            DataFrame with columns: frequency, magnitude, power, phase
        """
        # Get sampling rate
        fs = sampling_rate or self.sampling_rate
        if fs is None:
            raise ValueError("Sampling rate must be provided")
        
        # Convert to numpy array
        signal = np.asarray(signal)
        
        # Compute spectrum based on method
        if self.method == 'fft':
            spectrum = self._compute_fft(signal, fs, **kwargs)
        elif self.method == 'welch':
            spectrum = self._compute_welch(signal, fs, **kwargs)
        elif self.method == 'periodogram':
            spectrum = self._compute_periodogram(signal, fs, **kwargs)
        else:
            raise ValueError(f"Unknown method: {self.method}")
        
        self.last_spectrum = spectrum
        return spectrum
    
    def _compute_fft(self, signal: np.ndarray, fs: float,
                    detrend: str = 'linear') -> pd.DataFrame:
        """
        Compute FFT of signal
        
        Parameters
        ----------
        signal : np.ndarray
            Input signal
        fs : float
            Sampling rate
        detrend : str
            Detrending method: 'none', 'mean', 'linear'
            
        Returns
        -------
        pd.DataFrame
            Spectrum DataFrame
        """
        # Detrend signal
        if detrend == 'mean':
            signal = signal - np.mean(signal)
        elif detrend == 'linear':
            signal = scipy.signal.detrend(signal)
        
        # Compute FFT
        n = len(signal)
        fft = scipy.fftpack.fft(signal)
        freq = scipy.fftpack.fftfreq(n, d=1/fs)
        
        # Calculate magnitude and power
        magnitude = np.abs(fft)
        power = magnitude ** 2
        phase = np.angle(fft)
        
        # Create DataFrame (positive frequencies only)
        positive_freq_idx = freq >= 0
        
        spectrum_df = pd.DataFrame({
            'frequency': freq[positive_freq_idx],
            'magnitude': magnitude[positive_freq_idx],
            'power': power[positive_freq_idx],
            'phase': phase[positive_freq_idx],
            'fft_complex': fft[positive_freq_idx]
        })
        
        # Normalize power (one-sided spectrum)
        spectrum_df['power'] *= 2 / n
        spectrum_df['magnitude'] *= 2 / n
        
        logger.info(f"Computed FFT spectrum with {len(spectrum_df)} frequency bins")
        
        return spectrum_df
    
    def _compute_welch(self, signal: np.ndarray, fs: float,
                      nperseg: Optional[int] = None,
                      noverlap: Optional[int] = None,
                      window: str = 'hann') -> pd.DataFrame:
        """
        Compute Welch's power spectral density estimate
        
        Parameters
        ----------
        signal : np.ndarray
            Input signal
        fs : float
            Sampling rate
        nperseg : int, optional
            Length of each segment
        noverlap : int, optional
            Number of points to overlap
        window : str
            Window function
            
        Returns
        -------
        pd.DataFrame
            Spectrum DataFrame
        """
        # Set defaults
        if nperseg is None:
            nperseg = min(256, len(signal))
        if noverlap is None:
            noverlap = nperseg // 2
        
        # Compute Welch PSD
        freq, psd = scipy.signal.welch(signal, fs=fs, window=window,
                                       nperseg=nperseg, noverlap=noverlap)
        
        # Create DataFrame
        spectrum_df = pd.DataFrame({
            'frequency': freq,
            'power': psd,
            'magnitude': np.sqrt(psd),
            'phase': np.zeros_like(freq)  # Phase not available from Welch
        })
        
        logger.info(f"Computed Welch PSD with {len(spectrum_df)} frequency bins")
        
        return spectrum_df
    
    def _compute_periodogram(self, signal: np.ndarray, fs: float,
                            window: str = 'boxcar') -> pd.DataFrame:
        """
        Compute periodogram power spectral density estimate
        
        Parameters
        ----------
        signal : np.ndarray
            Input signal
        fs : float
            Sampling rate
        window : str
            Window function
            
        Returns
        -------
        pd.DataFrame
            Spectrum DataFrame
        """
        # Compute periodogram
        freq, psd = scipy.signal.periodogram(signal, fs=fs, window=window)
        
        # Create DataFrame
        spectrum_df = pd.DataFrame({
            'frequency': freq,
            'power': psd,
            'magnitude': np.sqrt(psd),
            'phase': np.zeros_like(freq)  # Phase not available
        })
        
        logger.info(f"Computed periodogram with {len(spectrum_df)} frequency bins")
        
        return spectrum_df
    
    def window_averaged_fft(self, signal: Union[list, np.ndarray],
                           window_size: int,
                           overlap: float = 0.5,
                           sampling_rate: Optional[float] = None,
                           window_function: str = 'hann') -> pd.DataFrame:
        """
        Compute window-averaged FFT for long signals
        
        Parameters
        ----------
        signal : array-like
            Input signal
        window_size : int
            Size of each window
        overlap : float, default=0.5
            Overlap fraction (0 to 1)
        sampling_rate : float, optional
            Sampling rate
        window_function : str
            Window function name
            
        Returns
        -------
        pd.DataFrame
            Averaged spectrum DataFrame
        """
        # Get sampling rate
        fs = sampling_rate or self.sampling_rate
        if fs is None:
            raise ValueError("Sampling rate must be provided")
        
        # Convert to numpy array
        signal = np.asarray(signal)
        
        # Calculate step size
        step = int(window_size * (1 - overlap))
        
        # Get window function
        if window_function != 'boxcar':
            window = scipy.signal.get_window(window_function, window_size)
        else:
            window = np.ones(window_size)
        
        # Initialize accumulator
        accumulated_spectrum = None
        num_windows = 0
        
        # Process windows
        for start in range(0, len(signal) - window_size + 1, step):
            # Extract window
            windowed_signal = signal[start:start + window_size] * window
            
            # Compute FFT
            fft = scipy.fftpack.fft(windowed_signal)
            power = np.abs(fft) ** 2
            
            # Accumulate
            if accumulated_spectrum is None:
                accumulated_spectrum = power
            else:
                accumulated_spectrum += power
            
            num_windows += 1
        
        # Average
        if num_windows > 0:
            accumulated_spectrum /= num_windows
        else:
            raise ValueError("Signal too short for specified window size")
        
        # Create frequency array
        freq = scipy.fftpack.fftfreq(window_size, d=1/fs)
        
        # Create DataFrame (positive frequencies only)
        positive_freq_idx = freq >= 0
        
        spectrum_df = pd.DataFrame({
            'frequency': freq[positive_freq_idx],
            'power': accumulated_spectrum[positive_freq_idx],
            'magnitude': np.sqrt(accumulated_spectrum[positive_freq_idx])
        })
        
        # Normalize
        spectrum_df['power'] *= 2 / window_size
        spectrum_df['magnitude'] *= 2 / window_size
        
        logger.info(f"Computed window-averaged FFT with {num_windows} windows")
        
        self.last_spectrum = spectrum_df
        return spectrum_df
    
    def find_peaks(self, spectrum: Optional[pd.DataFrame] = None,
                  threshold: Optional[float] = None,
                  n_peaks: Optional[int] = None,
                  min_distance: Optional[float] = None) -> pd.DataFrame:
        """
        Find peaks in frequency spectrum
        
        Parameters
        ----------
        spectrum : pd.DataFrame, optional
            Spectrum DataFrame (uses last_spectrum if not provided)
        threshold : float, optional
            Minimum peak height threshold
        n_peaks : int, optional
            Number of highest peaks to return
        min_distance : float, optional
            Minimum frequency distance between peaks
            
        Returns
        -------
        pd.DataFrame
            DataFrame with peak information
        """
        if spectrum is None:
            spectrum = self.last_spectrum
            
        if spectrum is None:
            raise ValueError("No spectrum available")
        
        # Get power values
        power = spectrum['power'].values
        freq = spectrum['frequency'].values
        
        # Find peaks using scipy
        peak_indices, peak_properties = scipy.signal.find_peaks(
            power,
            height=threshold,
            distance=min_distance
        )
        
        # Create peaks DataFrame
        peaks_df = pd.DataFrame({
            'frequency': freq[peak_indices],
            'power': power[peak_indices],
            'magnitude': np.sqrt(power[peak_indices])
        })
        
        # Sort by power
        peaks_df = peaks_df.sort_values('power', ascending=False)
        
        # Limit number of peaks
        if n_peaks is not None:
            peaks_df = peaks_df.head(n_peaks)
        
        logger.info(f"Found {len(peaks_df)} peaks in spectrum")
        
        return peaks_df
    
    def filter_spectrum(self, spectrum: Optional[pd.DataFrame] = None,
                       filter_type: str = 'bandpass',
                       low_freq: Optional[float] = None,
                       high_freq: Optional[float] = None) -> pd.DataFrame:
        """
        Apply frequency domain filtering
        
        Parameters
        ----------
        spectrum : pd.DataFrame, optional
            Spectrum DataFrame
        filter_type : str
            Filter type: 'bandpass', 'bandstop', 'lowpass', 'highpass'
        low_freq : float, optional
            Low frequency cutoff
        high_freq : float, optional
            High frequency cutoff
            
        Returns
        -------
        pd.DataFrame
            Filtered spectrum
        """
        if spectrum is None:
            spectrum = self.last_spectrum
            
        if spectrum is None:
            raise ValueError("No spectrum available")
        
        # Copy spectrum
        filtered_spectrum = spectrum.copy()
        freq = filtered_spectrum['frequency'].values
        
        # Create filter mask
        if filter_type == 'bandpass':
            if low_freq is None or high_freq is None:
                raise ValueError("Both low_freq and high_freq required for bandpass")
            mask = (freq >= low_freq) & (freq <= high_freq)
            
        elif filter_type == 'bandstop':
            if low_freq is None or high_freq is None:
                raise ValueError("Both low_freq and high_freq required for bandstop")
            mask = (freq < low_freq) | (freq > high_freq)
            
        elif filter_type == 'lowpass':
            if high_freq is None:
                raise ValueError("high_freq required for lowpass")
            mask = freq <= high_freq
            
        elif filter_type == 'highpass':
            if low_freq is None:
                raise ValueError("low_freq required for highpass")
            mask = freq >= low_freq
            
        else:
            raise ValueError(f"Unknown filter type: {filter_type}")
        
        # Apply filter
        filtered_spectrum.loc[~mask, ['power', 'magnitude']] = 0
        
        if 'fft_complex' in filtered_spectrum.columns:
            filtered_spectrum.loc[~mask, 'fft_complex'] = 0
        
        logger.info(f"Applied {filter_type} filter to spectrum")
        
        return filtered_spectrum
    
    def compute_psd(self, signal: Union[list, np.ndarray],
                   sampling_rate: Optional[float] = None,
                   method: str = 'welch',
                   **kwargs) -> pd.DataFrame:
        """
        Compute power spectral density
        
        Parameters
        ----------
        signal : array-like
            Input signal
        sampling_rate : float, optional
            Sampling rate
        method : str
            PSD method: 'welch', 'periodogram', 'fft'
        **kwargs
            Additional method parameters
            
        Returns
        -------
        pd.DataFrame
            PSD DataFrame
        """
        # Store original method
        original_method = self.method
        
        # Set method for PSD
        self.method = method
        
        # Compute spectrum
        psd = self.compute_spectrum(signal, sampling_rate, **kwargs)
        
        # Restore original method
        self.method = original_method
        
        # Ensure it's labeled as PSD
        if 'power' in psd.columns:
            psd['psd'] = psd['power']
        
        return psd
    
    def inverse_transform(self, spectrum: pd.DataFrame) -> np.ndarray:
        """
        Compute inverse FFT to reconstruct time domain signal
        
        Parameters
        ----------
        spectrum : pd.DataFrame
            Spectrum DataFrame with 'fft_complex' column
            
        Returns
        -------
        np.ndarray
            Reconstructed time domain signal
        """
        if 'fft_complex' not in spectrum.columns:
            raise ValueError("Spectrum must contain 'fft_complex' column for inverse transform")
        
        # Get complex FFT values
        fft_complex = spectrum['fft_complex'].values
        
        # Reconstruct full spectrum (negative frequencies)
        # Assuming we have positive frequencies only
        if len(fft_complex) > 1:
            # Mirror for negative frequencies
            full_fft = np.concatenate([fft_complex, np.conj(fft_complex[-2:0:-1])])
        else:
            full_fft = fft_complex
        
        # Inverse FFT
        signal = np.real(scipy.fftpack.ifft(full_fft))
        
        logger.info(f"Reconstructed signal with {len(signal)} samples")
        
        return signal