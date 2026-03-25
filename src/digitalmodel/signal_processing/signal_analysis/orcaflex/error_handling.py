"""
Enhanced error handling and recovery for time series analysis

Provides graceful degradation and detailed error reporting.
"""

import logging
import traceback
from typing import Dict, Any, Optional, Callable
from functools import wraps
import warnings
import numpy as np

logger = logging.getLogger(__name__)


class AnalysisError(Exception):
    """Base exception for analysis errors"""
    pass


class DataValidationError(AnalysisError):
    """Raised when data validation fails"""
    pass


class ColumnMappingError(AnalysisError):
    """Raised when column mapping fails"""
    pass


class RainflowError(AnalysisError):
    """Raised when rainflow analysis fails"""
    pass


class SpectralAnalysisError(AnalysisError):
    """Raised when spectral analysis fails"""
    pass


class ErrorHandler:
    """Centralized error handling and recovery"""
    
    def __init__(self, continue_on_error: bool = True, 
                 detailed_logging: bool = False):
        """
        Initialize error handler
        
        Args:
            continue_on_error: Whether to continue processing on errors
            detailed_logging: Whether to include stack traces in logs
        """
        self.continue_on_error = continue_on_error
        self.detailed_logging = detailed_logging
        self.error_log = []
        self.warning_log = []
        
    def log_error(self, context: str, error: Exception, 
                  data: Optional[Dict] = None) -> Dict:
        """
        Log error with context
        
        Args:
            context: Description of where error occurred
            error: The exception that was raised
            data: Optional data for debugging
            
        Returns:
            Error report dictionary
        """
        error_report = {
            'context': context,
            'error_type': type(error).__name__,
            'error_message': str(error),
            'data': data or {}
        }
        
        if self.detailed_logging:
            error_report['traceback'] = traceback.format_exc()
        
        self.error_log.append(error_report)
        
        logger.error(f"{context}: {error}")
        if self.detailed_logging:
            logger.debug(f"Stack trace:\n{error_report['traceback']}")
        
        return error_report
    
    def log_warning(self, context: str, message: str, 
                    data: Optional[Dict] = None):
        """Log warning with context"""
        warning_report = {
            'context': context,
            'message': message,
            'data': data or {}
        }
        
        self.warning_log.append(warning_report)
        logger.warning(f"{context}: {message}")
    
    def get_summary(self) -> Dict:
        """Get error and warning summary"""
        return {
            'errors': len(self.error_log),
            'warnings': len(self.warning_log),
            'error_log': self.error_log,
            'warning_log': self.warning_log
        }
    
    def clear_logs(self):
        """Clear error and warning logs"""
        self.error_log = []
        self.warning_log = []


def safe_analysis(fallback_result: Any = None, 
                  error_key: str = 'error',
                  continue_on_error: bool = True):
    """
    Decorator for safe analysis execution with fallback
    
    Args:
        fallback_result: Result to return on error
        error_key: Key to store error in result dict
        continue_on_error: Whether to continue or re-raise
    """
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            except Exception as e:
                logger.error(f"{func.__name__} failed: {e}")
                
                if not continue_on_error:
                    raise
                
                # Return fallback result
                if isinstance(fallback_result, dict):
                    result = fallback_result.copy()
                    result[error_key] = {
                        'function': func.__name__,
                        'error': str(e),
                        'type': type(e).__name__
                    }
                    return result
                else:
                    return fallback_result
        
        return wrapper
    return decorator


class SafeAnalyzer:
    """Analyzer with enhanced error handling"""
    
    def __init__(self, error_handler: Optional[ErrorHandler] = None):
        """Initialize with error handler"""
        self.error_handler = error_handler or ErrorHandler()
    
    def safe_rainflow_analysis(self, signal: np.ndarray, 
                               counter: Any) -> Dict:
        """
        Perform rainflow analysis with error handling
        
        Args:
            signal: Input signal
            counter: RainflowCounter instance
            
        Returns:
            Analysis results or error report
        """
        results = {
            'cycles': None,
            'statistics': {},
            'histogram': None,
            'success': False,
            'error': None
        }
        
        try:
            # Check signal validity
            if len(signal) < 3:
                raise DataValidationError("Signal too short for rainflow analysis")
            
            if np.all(signal == signal[0]):
                raise DataValidationError("Signal is constant - no cycles to count")
            
            # Perform analysis
            cycles = counter.count_cycles(signal)
            results['cycles'] = cycles
            results['statistics'] = counter.get_statistics(cycles)
            
            # Generate histogram with error handling
            try:
                results['histogram'] = counter.get_histogram(cycles, bins=20)
            except Exception as e:
                self.error_handler.log_warning(
                    "Rainflow histogram generation",
                    f"Failed to generate histogram: {e}"
                )
            
            results['success'] = True
            
        except Exception as e:
            error_report = self.error_handler.log_error(
                "Rainflow analysis",
                e,
                {'signal_length': len(signal), 'signal_range': np.ptp(signal)}
            )
            results['error'] = error_report
            
            # Provide partial results if possible
            results['statistics'] = {
                'total_cycles': 0,
                'max_range': np.ptp(signal) if len(signal) > 0 else 0,
                'mean_range': 0,
                'error': str(e)
            }
        
        return results
    
    def safe_spectral_analysis(self, signal: np.ndarray,
                               analyzer: Any,
                               config: Dict) -> Dict:
        """
        Perform spectral analysis with error handling
        
        Args:
            signal: Input signal
            analyzer: SpectralAnalyzer instance
            config: FFT configuration
            
        Returns:
            Analysis results or error report
        """
        results = {
            'spectrum': None,
            'window_fft': None,
            'peaks': None,
            'psd': None,
            'success': False,
            'error': None
        }
        
        try:
            # Check signal validity
            if len(signal) < 16:
                raise DataValidationError("Signal too short for FFT analysis")
            
            # Remove DC component if causing issues
            signal_centered = signal - np.mean(signal)
            
            # Try standard FFT
            try:
                spectrum = analyzer.compute_spectrum(signal_centered)
                results['spectrum'] = spectrum
            except Exception as e:
                self.error_handler.log_warning(
                    "Standard FFT",
                    f"Failed, trying alternative: {e}"
                )
                # Try with zero-padding
                padded_signal = np.pad(signal_centered, 
                                      (0, 2**int(np.ceil(np.log2(len(signal)))) - len(signal)))
                spectrum = analyzer.compute_spectrum(padded_signal)
                results['spectrum'] = spectrum
            
            # Try window-averaged FFT with adaptive window size
            try:
                window_size = min(config.get('window_size', 256), len(signal) // 4)
                
                # Ensure window size is power of 2
                window_size = 2**int(np.log2(window_size))
                
                if window_size >= 16:  # Minimum viable window
                    window_fft = analyzer.window_averaged_fft(
                        signal_centered,
                        window_size=window_size,
                        overlap=config.get('overlap', 0.5)
                    )
                    results['window_fft'] = window_fft
                else:
                    self.error_handler.log_warning(
                        "Window-averaged FFT",
                        f"Window size too small: {window_size}"
                    )
                    
            except ZeroDivisionError as e:
                # Handle "weights sum to zero" error
                self.error_handler.log_warning(
                    "Window-averaged FFT",
                    "Weights sum to zero - using standard FFT instead"
                )
                results['window_fft'] = results['spectrum']
                
            except Exception as e:
                self.error_handler.log_warning(
                    "Window-averaged FFT",
                    f"Failed: {e}"
                )
            
            # Try to find peaks
            if results['window_fft'] is not None:
                try:
                    n_peaks = config.get('peak_detection', {}).get('n_peaks', 5)
                    peaks = analyzer.find_peaks(results['window_fft'], n_peaks=n_peaks)
                    results['peaks'] = peaks
                except Exception as e:
                    self.error_handler.log_warning(
                        "Peak detection",
                        f"Failed: {e}"
                    )
            
            # Try PSD calculation
            try:
                psd = analyzer.compute_psd(signal_centered, method='welch')
                results['psd'] = psd
            except Exception as e:
                self.error_handler.log_warning(
                    "PSD calculation",
                    f"Failed: {e}"
                )
            
            results['success'] = any([
                results['spectrum'] is not None,
                results['window_fft'] is not None,
                results['psd'] is not None
            ])
            
        except Exception as e:
            error_report = self.error_handler.log_error(
                "Spectral analysis",
                e,
                {
                    'signal_length': len(signal),
                    'signal_mean': np.mean(signal) if len(signal) > 0 else 0,
                    'signal_std': np.std(signal) if len(signal) > 0 else 0
                }
            )
            results['error'] = error_report
        
        return results
    
    def validate_signal(self, signal: np.ndarray, 
                       context: str = "Signal") -> Dict:
        """
        Validate signal quality and characteristics
        
        Args:
            signal: Input signal
            context: Description for logging
            
        Returns:
            Validation report
        """
        report = {
            'valid': True,
            'warnings': [],
            'errors': [],
            'characteristics': {}
        }
        
        # Basic checks
        if len(signal) == 0:
            report['valid'] = False
            report['errors'].append("Signal is empty")
            return report
        
        # Calculate characteristics
        report['characteristics'] = {
            'length': len(signal),
            'mean': np.mean(signal),
            'std': np.std(signal),
            'min': np.min(signal),
            'max': np.max(signal),
            'nan_count': np.isnan(signal).sum(),
            'inf_count': np.isinf(signal).sum()
        }
        
        # Check for NaN/Inf
        if report['characteristics']['nan_count'] > 0:
            report['warnings'].append(f"Signal contains {report['characteristics']['nan_count']} NaN values")
            
        if report['characteristics']['inf_count'] > 0:
            report['warnings'].append(f"Signal contains {report['characteristics']['inf_count']} Inf values")
        
        # Check for constant signal
        if report['characteristics']['std'] == 0:
            report['warnings'].append("Signal is constant (zero variance)")
        
        # Check for extreme values
        if report['characteristics']['std'] > 0:
            z_max = abs((report['characteristics']['max'] - report['characteristics']['mean']) / 
                       report['characteristics']['std'])
            if z_max > 10:
                report['warnings'].append(f"Signal contains extreme values (z-score: {z_max:.1f})")
        
        # Log warnings
        for warning in report['warnings']:
            self.error_handler.log_warning(context, warning)
        
        return report


def create_safe_analyzer(config: Dict) -> SafeAnalyzer:
    """
    Create analyzer with error handling
    
    Args:
        config: Configuration dictionary
        
    Returns:
        SafeAnalyzer instance
    """
    error_config = config.get('error_handling', {})
    error_handler = ErrorHandler(
        continue_on_error=error_config.get('continue_on_error', True),
        detailed_logging=error_config.get('detailed_logging', False)
    )
    
    return SafeAnalyzer(error_handler)