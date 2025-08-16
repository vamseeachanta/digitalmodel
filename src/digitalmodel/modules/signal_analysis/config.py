"""
Configuration for signal analysis module

Default parameters and settings for signal processing algorithms.
"""

# Rainflow counting defaults
RAINFLOW_DEFAULTS = {
    "method": "astm",  # ASTM E1049-85 standard
    "extract_info": True,  # Extract cycle start/end indices
    "bins": None,  # Default to automatic binning
}

# FFT/Spectral analysis defaults
SPECTRAL_DEFAULTS = {
    "method": "fft",  # Default to standard FFT
    "window": "hann",  # Default window function
    "overlap": 0.5,  # 50% overlap for window averaging
    "detrend": "linear",  # Default detrending method
    "scaling": "density",  # Return power spectral density
}

# Time series processing defaults
TIMESERIES_DEFAULTS = {
    "interpolation": "linear",  # Default interpolation method
    "outlier_method": "zscore",  # Default outlier detection
    "outlier_threshold": 3.0,  # Z-score threshold
    "smoothing_window": 5,  # Default smoothing window size
}

# Filtering defaults
FILTER_DEFAULTS = {
    "order": 4,  # Default filter order
    "filter_type": "butterworth",  # Default filter type
    "zero_phase": True,  # Use zero-phase filtering by default
}

# Visualization defaults
VISUALIZATION_DEFAULTS = {
    "figure_size": (10, 6),  # Default figure size
    "dpi": 100,  # Default DPI
    "grid": True,  # Show grid by default
    "interactive": False,  # Use matplotlib by default
}