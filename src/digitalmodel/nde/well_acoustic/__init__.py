# ABOUTME: well_acoustic sub-package — The Well acoustic scattering dataset loaders
# ABOUTME: for subsea Non-Destructive Evaluation (NDE) validation.
# ABOUTME: Dataset attribution: The Well — CC BY 4.0, Polymathic AI.

"""
digitalmodel.nde.well_acoustic
==============================

Loaders for The Well acoustic scattering datasets, enabling subsea NDE
validation with physics-accurate acoustic wave simulation data.

Datasets (CC BY 4.0, Polymathic AI — https://github.com/PolymathicAI/the_well):
  acoustic_scattering           — basic variant
  acoustic_scattering_maze      — maze obstacle variant
  acoustic_scattering_inclusions — multi-scatter inclusions variant

Usage::

    from digitalmodel.nde.well_acoustic import AcousticScatteringLoader

    loader = AcousticScatteringLoader(variant="basic")
    sample = loader.stream_sample(n_steps=10)
"""

from digitalmodel.nde.well_acoustic.acoustic_scattering_loader import (
    THE_WELL_ATTRIBUTION,
    WELL_AVAILABLE,
    AcousticScatteringLoader,
)

__all__ = [
    "AcousticScatteringLoader",
    "WELL_AVAILABLE",
    "THE_WELL_ATTRIBUTION",
]
