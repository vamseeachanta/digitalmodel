"""Shared Darcy friction factor, moved from field_development.onshore_layout."""

import math

LAMINAR_REYNOLDS_LIMIT = 2300.0


def swamee_jain_friction_factor(reynolds: float, relative_roughness: float) -> float:
    """Darcy friction factor.

    Laminar (Re < 2300): f = 64/Re (exact, Hagen-Poiseuille).
    Turbulent: Swamee-Jain explicit approximation to Colebrook-White:
    f = 0.25 / [log10(eps/(3.7 D) + 5.74 / Re^0.9)]^2.

    Swamee & Jain (1976), J. Hydraulics Division ASCE 102(5), 657-664,
    DOI:10.1061/JYCEAJ.0004542. Validity 5e3 <= Re <= 3e8,
    1e-6 <= eps/D <= 1e-2; transition is a screening approximation.
    The existing field-development numerical behavior is preserved.
    """
    if reynolds <= 0:
        raise ValueError(f"reynolds must be > 0, got {reynolds!r}")
    if reynolds < LAMINAR_REYNOLDS_LIMIT:
        return 64.0 / reynolds
    return 0.25 / math.log10(relative_roughness / 3.7 + 5.74 / reynolds**0.9) ** 2
