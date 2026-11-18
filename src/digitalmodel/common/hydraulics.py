"""Shared single-phase pipe friction correlations, Darcy convention."""

import math


def swamee_jain_friction_factor(reynolds: float, relative_roughness: float) -> float:
    """Return Darcy f: 64/Re below 2300, explicit Swamee-Jain otherwise.

    Swamee & Jain (1976), J. Hydraulics Div. 102(5), 657-664,
    doi:10.1061/JYCEAJ.0004542: f=0.25/log10(eps/(3.7D)+5.74/Re**0.9)**2.
    The transition band (2300-5000) is a screening extrapolation, not a
    calibrated transition law. Public implementation reference: INL
    OpenPronghorn, Path-Integrated Incompressible Flow Model, Friction Factor.
    Laminar 64/Re follows Hagen-Poiseuille (DOE-HDBK-1012/3-92, Fluid Flow).
    Both arguments are dimensionless; this helper is also available to CT
    hydraulics without importing completion or plotting modules.
    """
    if not math.isfinite(reynolds) or reynolds <= 0:
        raise ValueError("reynolds must be finite and > 0")
    if not math.isfinite(relative_roughness) or not 0 <= relative_roughness < 1:
        raise ValueError("relative_roughness must be finite and in [0, 1)")
    if reynolds < 2300:
        return 64.0 / reynolds
    return 0.25 / math.log10(relative_roughness / 3.7 + 5.74 / reynolds**0.9) ** 2
