# ABOUTME: Tubular design sub-package — design envelopes, connection ratings, wear
# ABOUTME: Public API for casing/tubing string design checks (WRK-376)

"""Tubular design tools — triaxial stress envelope, VME checks, API limits."""

from digitalmodel.well.tubulars.design_envelope import (
    AnisotropicVmeEnvelope,
    ApiEllipseEnvelope,
    IsotropicVmeEnvelope,
    TubularGeometry,
    compute_hoop_stress,
    compute_vme_stress,
    design_envelope_points,
)

__all__ = [
    "AnisotropicVmeEnvelope",
    "ApiEllipseEnvelope",
    "IsotropicVmeEnvelope",
    "TubularGeometry",
    "compute_hoop_stress",
    "compute_vme_stress",
    "design_envelope_points",
]
