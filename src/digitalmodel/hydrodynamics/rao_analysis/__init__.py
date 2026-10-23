# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""RAO plotting and solver-free linear response analysis.

The numerical functions use explicit frequency, phase and density conventions.
The existing plotting class is loaded on demand with its plotting dependencies.
"""

from .response import SpectralResponse, regular_wave_response, spectral_response

__all__ = ["RAOAnalysis", "SpectralResponse", "regular_wave_response", "spectral_response"]


def __getattr__(name):
    """Keep the legacy public plotting entry point available on demand."""
    if name == "RAOAnalysis":
        from .rao_analysis import RAOAnalysis

        return RAOAnalysis
    raise AttributeError(name)
