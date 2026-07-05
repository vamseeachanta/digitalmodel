# ABOUTME: Analytical tank-sloshing natural periods and class-simplified convective
# ABOUTME: response, with a resonance check against vessel-motion / mooring periods.

"""
Tank sloshing — analytical natural periods and simplified convective response.

This is the fast, class-reviewable first tier of a sloshing assessment: it tells
you the tank's natural sloshing periods and whether any of them coincide with the
vessel's motion or mooring periods (the coupling that external diffraction —
AQWA / OrcaWave — does not capture).  When resonance is flagged, or for high fill
and violent/impact sloshing, the follow-on tier is a VOF free-surface CFD run;
this module deliberately covers only the analytical / simplified tier.

Methods
-------
* Rectangular (prismatic) tank, linear potential-flow theory:

      omega_n^2 = g * (n*pi/L) * tanh(n*pi*h/L),   n = 1, 2, ...

  the odd modes (n = 1, 3, ...) are the antisymmetric sloshing modes excited by
  lateral (sway) motion.

* Upright circular-cylindrical tank, linear potential-flow theory:

      omega_mn^2 = g * (eps_mn/R) * tanh(eps_mn*h/R)

  where ``eps_mn`` is the n-th root of J_m'(x) = 0.  The m = 1 family is the
  lateral (antisymmetric) sloshing family; ``eps_11 = 1.8412`` gives the
  fundamental sloshing mode.

* API 650 Annex E convective (sloshing) period for a flat-bottom cylindrical
  tank — the accepted simplified storage-tank form:

      Tc = 1.8 * Ks * sqrt(D),   Ks = 0.578 / sqrt(tanh(3.68 * H / D))

  (D, H in metres; Tc in seconds).

References
----------
* Faltinsen, O.M. & Timokha, A.N. — Sloshing (Cambridge, 2009), linear modal
  natural frequencies for rectangular and upright cylindrical tanks.
* API STD 650, Annex E (Seismic Design of Storage Tanks) — convective period Tc.
* DNV / ABS / BV / ClassNK simplified sloshing assessment procedures (screening
  tier); GTT methodology for membrane LNG containment.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import List

# Standard gravity (m/s^2)
G_STD = 9.80665

# Roots of J_1'(x) = 0 — the lateral (antisymmetric) sloshing family for an
# upright circular-cylindrical tank.  eps_11 = 1.8412 is the fundamental mode.
_BESSEL_J1_PRIME_ROOTS: List[float] = [1.84118378, 5.33144277, 8.53631637, 11.70600490]


# ---------------------------------------------------------------------------
# Result container
# ---------------------------------------------------------------------------


@dataclass
class SloshingResult:
    """
    Result of an analytical sloshing screen for a single tank.

    Attributes
    ----------
    geometry : str
        "rectangular" or "cylindrical".
    natural_periods : list of float
        Sloshing natural periods (s), fundamental first.
    fundamental_period : float
        First (longest) sloshing natural period (s).
    convective_period : float or None
        API 650 Annex E convective period (s); None when not applicable.
    resonance : bool
        True when a natural period falls within tolerance of an excitation period.
    resonant_periods : list of float
        The natural periods (s) that triggered the resonance flag.
    """

    geometry: str
    natural_periods: List[float]
    fundamental_period: float
    convective_period: float | None = None
    resonance: bool = False
    resonant_periods: List[float] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Natural periods
# ---------------------------------------------------------------------------


def rectangular_tank_periods(
    length: float, fill_depth: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Sloshing natural periods (s) of a rectangular tank, antisymmetric modes.

    Returns the periods of the first ``n_modes`` antisymmetric modes
    (n = 1, 3, 5, ...) excited by lateral motion, longest first.
    """
    if length <= 0 or fill_depth <= 0:
        raise ValueError("length and fill_depth must be positive")
    periods: List[float] = []
    n = 1
    while len(periods) < n_modes:
        k = n * math.pi / length
        omega_sq = g * k * math.tanh(k * fill_depth)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
        n += 2  # antisymmetric modes only
    return periods


def cylindrical_tank_periods(
    diameter: float, fill_depth: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Sloshing natural periods (s) of an upright cylindrical tank, lateral family.

    Uses the m = 1 (antisymmetric) family; returns the first ``n_modes`` periods
    longest first.
    """
    if diameter <= 0 or fill_depth <= 0:
        raise ValueError("diameter and fill_depth must be positive")
    if n_modes > len(_BESSEL_J1_PRIME_ROOTS):
        raise ValueError(f"n_modes must be <= {len(_BESSEL_J1_PRIME_ROOTS)}")
    radius = diameter / 2.0
    periods: List[float] = []
    for eps in _BESSEL_J1_PRIME_ROOTS[:n_modes]:
        omega_sq = g * (eps / radius) * math.tanh(eps * fill_depth / radius)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
    return periods


def api650_convective_period(diameter: float, fill_height: float) -> float:
    """
    API 650 Annex E convective (sloshing) period Tc (s); D, H in metres.

        Tc = 1.8 * Ks * sqrt(D),   Ks = 0.578 / sqrt(tanh(3.68 * H / D))
    """
    if diameter <= 0 or fill_height <= 0:
        raise ValueError("diameter and fill_height must be positive")
    ks = 0.578 / math.sqrt(math.tanh(3.68 * fill_height / diameter))
    return 1.8 * ks * math.sqrt(diameter)


def _ellipse_equivalent_rectangle(width: float, height: float, fill_fraction: float):
    """
    Equivalent-rectangle reduction of an elliptical / obround / horizontal-cylinder
    cross-section at a partial fill.

    Returns ``(a, b_s, h_eq)`` — semi-major (half-width) ``a``, the free-surface
    width ``b_s`` and the equivalent still-liquid depth ``h_eq`` — such that the
    transverse sloshing frequency follows Lamb's rectangular relation with
    L -> b_s and h -> h_eq.  ``fill_fraction`` = liquid depth / tank height (0..1).
    """
    a = width / 2.0
    b = height / 2.0
    f = min(max(fill_fraction, 0.02), 0.98)  # clamp: b_s -> 0 at the extremes
    s = 2.0 * f - 1.0
    disc = math.sqrt(max(1.0 - s * s, 1e-9))
    b_s = 2.0 * a * disc  # free-surface width
    area_frac = 0.5 + (math.asin(s) + s * disc) / math.pi  # A_L / (pi a b)
    h_eq = b * area_frac * (math.pi / 2.0) / disc
    return a, b_s, h_eq


def oval_tank_periods(
    width: float, height: float, fill_fraction: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Transverse sloshing natural periods (s) of an elliptical / obround / horizontal
    cylindrical tank via the equivalent-rectangle method (Lamb, with the free-surface
    width and equivalent depth from the cross-section geometry at ``fill_fraction``).

    ``width`` and ``height`` are the full cross-section dimensions; a horizontal
    circular cylinder is the special case ``width == height == diameter``.

    Validity: the equivalent-rectangle approximation is good over the mid-fill
    range (fill_fraction ~0.15-0.75, within ~10-15% of the exact potential-flow
    value for a circle at half fill).  Near a full tank the free-surface width
    ``b_s`` collapses and the approximation over-predicts the frequency — those
    high-fill / violent cases are the ones a VOF free-surface CFD run resolves.
    """
    if width <= 0 or height <= 0:
        raise ValueError("width and height must be positive")
    if not 0.0 < fill_fraction < 1.0:
        raise ValueError("fill_fraction must be in (0, 1)")
    _, b_s, h_eq = _ellipse_equivalent_rectangle(width, height, fill_fraction)
    periods: List[float] = []
    n = 1
    while len(periods) < n_modes:
        k = n * math.pi / b_s
        omega_sq = g * k * math.tanh(k * h_eq)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
        n += 2  # antisymmetric transverse modes
    return periods


def horizontal_cylinder_periods(
    diameter: float, fill_fraction: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Transverse sloshing natural periods (s) of a HORIZONTAL circular cylinder — the
    road-tanker / rail-tank-car / IMO Type-C shape — at ``fill_fraction`` (0..1).

    Equivalent-rectangle special case of :func:`oval_tank_periods` with
    width = height = diameter.
    """
    return oval_tank_periods(diameter, diameter, fill_fraction, n_modes, g)


def resonance_check(
    natural_periods: List[float], excitation_periods: List[float], tolerance: float = 0.15
) -> List[float]:
    """
    Return the natural periods within ``tolerance`` (fractional) of any excitation
    period — e.g. vessel roll / sway / mooring periods.  Empty list => no resonance.
    """
    hits: List[float] = []
    for tn in natural_periods:
        for te in excitation_periods:
            if te > 0 and abs(tn - te) / te <= tolerance:
                hits.append(tn)
                break
    return hits


# ---------------------------------------------------------------------------
# Registry workflow
# ---------------------------------------------------------------------------


def screen_tank(
    geometry: str,
    fill_depth: float,
    length: float | None = None,
    diameter: float | None = None,
    n_modes: int = 3,
    excitation_periods: List[float] | None = None,
    resonance_tolerance: float = 0.15,
    g: float = G_STD,
) -> SloshingResult:
    """Analytical sloshing screen for one tank; see module docstring for methods."""
    geometry = geometry.lower()
    if geometry == "rectangular":
        if length is None:
            raise ValueError("rectangular geometry requires length")
        periods = rectangular_tank_periods(length, fill_depth, n_modes, g)
        convective = None
    elif geometry == "cylindrical":
        if diameter is None:
            raise ValueError("cylindrical geometry requires diameter")
        periods = cylindrical_tank_periods(diameter, fill_depth, n_modes, g)
        convective = api650_convective_period(diameter, fill_depth)
    else:
        raise ValueError("geometry must be 'rectangular' or 'cylindrical'")

    resonant = resonance_check(periods, excitation_periods or [], resonance_tolerance)
    return SloshingResult(
        geometry=geometry,
        natural_periods=periods,
        fundamental_period=periods[0],
        convective_period=convective,
        resonance=bool(resonant),
        resonant_periods=resonant,
    )


def router(cfg: dict) -> dict:
    """
    Registry entry point.  Expects a ``sloshing`` config block, e.g.::

        {"sloshing": {"geometry": "cylindrical", "diameter": 40.0,
                       "fill_depth": 20.0, "excitation_periods": [12.0]}}

    Returns a plain-dict result for serialisation.
    """
    params = cfg.get("sloshing", cfg)
    result = screen_tank(
        geometry=params["geometry"],
        fill_depth=params["fill_depth"],
        length=params.get("length"),
        diameter=params.get("diameter"),
        n_modes=params.get("n_modes", 3),
        excitation_periods=params.get("excitation_periods"),
        resonance_tolerance=params.get("resonance_tolerance", 0.15),
        g=params.get("g", G_STD),
    )
    return {
        "geometry": result.geometry,
        "natural_periods": result.natural_periods,
        "fundamental_period": result.fundamental_period,
        "convective_period": result.convective_period,
        "resonance": result.resonance,
        "resonant_periods": result.resonant_periods,
    }
