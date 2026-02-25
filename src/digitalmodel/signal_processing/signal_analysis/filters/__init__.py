"""
Signal filtering module

Frequency and time domain filtering implementations:
- Band-pass and band-stop filters
- Low-pass and high-pass filters
- Digital filter design utilities
"""

from .frequency import FrequencyFilter

__all__ = ["FrequencyFilter"]