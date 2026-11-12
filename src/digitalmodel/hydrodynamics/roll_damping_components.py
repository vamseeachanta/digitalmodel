"""Component (Ikeda-type) estimate of viscous roll damping for ships and barges.

Potential-flow diffraction gives the wave (radiation) part of roll damping
only. For ships, barges and other long hulls that part is small, and the
response near the roll natural period is governed by viscous contributions
that a diffraction solver does not compute. This module estimates them with
the empirical component method collected by

    S. Chakrabarti, "Empirical calculation of roll damping for ships and
    barges", Ocean Engineering 28 (2001) 915-932,

which restates Ikeda, Himeno and Tanaka (1978), citing Himeno (1981), and
adds a sharp-cornered barge eddy formula (Ikeda 1984 in the paper; Ikeda,
Fujiwara and Katayama 1993 in ITTC 7.5-02-07-04.5). Equation numbers in docstrings are those of that paper. Where
the paper's print departs from its sources, the form in ITTC Recommended
Procedure 7.5-02-07-04.5 (2011 and 2024 revisions) is implemented and the
difference is stated at the function.

The total equivalent linear damping is the sum (eq. 5)

    B_eq = B_F + B_E + B_L + B_W + B_BK

of skin friction, eddy making, lift, wave and bilge keel parts. The split
ignores interaction between the parts, and the coefficients come from model
tests, mostly two-dimensional, at small roll amplitude (about 5 deg, p. 924).

Units: every formula is dimensionally homogeneous, so any consistent set may
be used (SI: kg/m3, m, s, rad, giving N m s/rad). Angles are radians, roll
amplitude included. ``og`` is the vertical distance from the still-water level
down to the roll axis (positive downward), ``og = draft - KG`` for a roll axis
through the centre of gravity.

Damping coefficients returned per unit length carry ``_section`` in the
function name; the rest are whole-hull values.

Edition selection. ``variant`` on the sectional eddy functions changes the
C_R bracket only (``chakrabarti2001``, ``ittc2011``, ``ittc2021`` which prints
the 2011 bracket, ``ittc2024``); every other formula follows the paper. Two
further prints differ and are not selectable: ITTC 2011 eq. 2.20, 2021 eq. 44
and 2024 eq. 44 print ``gamma`` with ``(r_max + 2M/H) sqrt(A**2 + B**2)``,
whereas the paper's eq. 20 is ``r_max + (2M/H) sqrt(A**2 + B**2)``. The paper's
reading is kept: it reproduces the midbody C_R of the paper's Fig. 5 (123.5
against about 120), while the ITTC print gives 76 there and a negative C_R for
a B/d 2.5, sigma 0.95 ship section. ITTC 2011 eq. 2.25 prints the bilge-keel
normal force with ``f`` where the paper and ITTC 2024 have ``f**2``; ``f**2``
is used.

Domain. Inputs outside the formulas' physical domain raise
:class:`RollDampingComponentError`: coefficients above one, explicit bilge
radii larger than the section allows, roll amplitudes of 45 deg or more, and
any input combination that drives a dissipation coefficient to zero or below.
Inputs inside the domain but beyond the paper's evidence are returned with
explicit flags on :class:`RollDampingComponents` (``FLAG_*`` constants). No
universal B/T or amplitude cut-off is established by the sources; the paper
states about 5 deg (p. 924) and its examples use 10 deg.

Provenance. Whole-hull results carry a structured citation sidecar
(:class:`digitalmodel.citations.schema.Citation`, one record per equation
group) in ``RollDampingComponents.citations``. :func:`validate_citations`
resolves them fail-closed against the wiki source page.

Conversion to a percentage of critical uses
:func:`digitalmodel.hydrodynamics.diffraction.roll_damping.damping_to_percent_critical`,
so that the critical-damping basis (structural plus added inertia, stated at a
frequency) is the one the AQWA and OrcaWave conversion already uses.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Literal, Optional, Sequence

from digitalmodel.citations.schema import Citation, validate_citation

from digitalmodel.hydrodynamics.diffraction.roll_damping import (
    damping_to_percent_critical,
)

__all__ = [
    "FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES",
    "FLAG_EQ35_WITH_ROUNDED_BILGE",
    "MAX_AMPLITUDE",
    "PAPER_EXAMPLE_AMPLITUDE",
    "SOURCE_CITATION",
    "HullSection",
    "OutsideEmpiricalDomainError",
    "RollDampingComponentError",
    "RollDampingComponents",
    "barge_eddy_damping",
    "barge_roll_damping",
    "bilge_keel_damping_section",
    "bilge_keel_hull_pressure_section",
    "bilge_keel_lever",
    "bilge_keel_normal_section",
    "bilge_radius_from_area_coefficient",
    "drag_route_eddy_damping",
    "eddy_damping_section",
    "eddy_damping_sections",
    "eddy_forward_speed_factor",
    "eddy_pressure_ratio",
    "equivalent_linear_damping",
    "friction_damping_laminar",
    "friction_damping_turbulent",
    "friction_forward_speed_factor",
    "friction_radius",
    "lewis_coefficients",
    "lift_damping",
    "ship_wetted_area",
    "triangular_section_drag_coefficient",
    "validate_citations",
    "wave_damping_forward_speed",
]

STANDARD_GRAVITY = 9.80665

EddyVariant = Literal[
    "chakrabarti2001", "ittc2011", "ittc2021", "ittc2024", "as_printed"
]

#: Roll amplitude at and above which no formula here is evaluated (rad).
MAX_AMPLITUDE = math.pi / 4.0
#: Largest amplitude the paper's worked examples use (10 deg, pp. 928-929).
PAPER_EXAMPLE_AMPLITUDE = math.radians(10.0)

FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES = "amplitude_above_paper_examples"
FLAG_EQ35_WITH_ROUNDED_BILGE = "eq35_with_rounded_bilge"

#: The source of every formula in this module. The wiki page is the
#: naval-architecture source page for the paper; resolution needs its
#: frontmatter to carry these code_id, publisher and revision values.
SOURCE_CITATION = Citation(
    code_id="chakrabarti-2001-empirical-roll-damping-ships-barges",
    publisher="Elsevier Science Ltd",
    revision="2001",
    section="Ocean Engineering 28 (2001) 915-932",
    wiki_path=(
        "wikis/naval-architecture/wiki/sources/"
        "chakrabarti-2001-empirical-roll-damping-ships-barges.md"
    ),
    note="Equations restated after Ikeda, Himeno and Tanaka (1978); see module docstring",
    source_sibling="generic",
)


class RollDampingComponentError(ValueError):
    """An input outside the range the empirical formulas accept."""


class OutsideEmpiricalDomainError(RollDampingComponentError):
    """Valid inputs for which a fitted formula gives no positive dissipation."""


# --- validation helpers -------------------------------------------------------


def _positive(name: str, value: float) -> float:
    if not isinstance(value, (int, float)) or not math.isfinite(value) or value <= 0.0:
        raise RollDampingComponentError(
            f"{name} must be finite and positive, got {value!r}"
        )
    return float(value)


def _non_negative(name: str, value: float) -> float:
    if not isinstance(value, (int, float)) or not math.isfinite(value) or value < 0.0:
        raise RollDampingComponentError(
            f"{name} must be finite and non-negative, got {value!r}"
        )
    return float(value)


def _finite(name: str, value: float) -> float:
    if not isinstance(value, (int, float)) or not math.isfinite(value):
        raise RollDampingComponentError(f"{name} must be finite, got {value!r}")
    return float(value)


def _area_coefficient(sigma: float) -> float:
    if (
        not isinstance(sigma, (int, float))
        or not math.isfinite(sigma)
        or not 0.0 < sigma <= 1.0
    ):
        raise RollDampingComponentError(
            f"section area coefficient must lie in (0, 1], got {sigma!r}"
        )
    return float(sigma)


def _fraction(name: str, value: float) -> float:
    """A hull form coefficient: finite, positive and not above one."""
    value = _positive(name, value)
    if value > 1.0:
        raise RollDampingComponentError(f"{name} must not exceed 1, got {value!r}")
    return value


def _amplitude(value: float, *, positive: bool = False) -> float:
    value = (
        _positive("amplitude", value) if positive else _non_negative("amplitude", value)
    )
    if value >= MAX_AMPLITUDE:
        raise RollDampingComponentError(
            f"roll amplitude {value!r} rad is at or above {MAX_AMPLITUDE!r} rad (45 deg); "
            "no formula here has a basis there"
        )
    return value


def _section_radius(radius: float, beam: float, draft: float) -> float:
    """An explicit bilge radius: non-negative and within min(d, B/2) (eqs. 29, 59)."""
    radius = _non_negative("bilge_radius", radius)
    limit = min(draft, beam / 2.0)
    if radius > limit * (1.0 + 1e-12):
        raise RollDampingComponentError(
            f"bilge radius {radius!r} exceeds the section limit min(d, B/2) = {limit!r}"
        )
    return radius


def _dissipative(name: str, value: float) -> float:
    if not math.isfinite(value) or value <= 0.0:
        raise OutsideEmpiricalDomainError(
            f"{name} evaluates to {value!r}; the inputs are outside the empirical "
            "domain of the formula (a dissipation coefficient must be positive)"
        )
    return value


def _og_ratio(og: float, draft: float) -> float:
    ratio = _finite("og", og) / draft
    if ratio >= 1.0:
        raise RollDampingComponentError(
            f"the roll axis must lie above the keel (og/draft < 1), got {ratio!r}"
        )
    return ratio


# --- eq. (4): equivalent linearisation ---------------------------------------


def equivalent_linear_damping(
    b1: float, b2: float, b3: float, omega: float, amplitude: float
) -> float:
    """Equivalent linear coefficient of ``B1 v + B2 |v| v + B3 v**3`` (eq. 4).

    Equal energy dissipated per cycle of harmonic roll of ``amplitude`` (rad)
    at ``omega`` (rad/s). Journee and Massie (2001) eq. 6.36 is the same rule.
    """
    _finite("b1", b1)
    _finite("b2", b2)
    _finite("b3", b3)
    omega = _positive("omega", omega)
    amplitude = _non_negative("amplitude", amplitude)
    velocity = omega * amplitude
    return b1 + 8.0 / (3.0 * math.pi) * b2 * velocity + 0.75 * b3 * velocity**2


# --- section 3.1: skin friction -----------------------------------------------


def ship_wetted_area(
    length: float, draft: float, beam: float, block_coefficient: float
) -> float:
    """Approximate wetted area of a ship hull, ``L (1.7 D + C_B B)`` (eq. 9)."""
    return _positive("length", length) * (
        1.7 * _positive("draft", draft)
        + _fraction("block_coefficient", block_coefficient) * _positive("beam", beam)
    )


def friction_radius(
    block_coefficient: float, wetted_area: float, length: float, og: float
) -> float:
    """Effective radius of the wetted surface from the roll axis (eq. 8).

    ``r_f = ((0.887 + 0.145 C_B) S / L - 2 OG) / pi``. The regression is for
    ship forms; ITTC 7.5-02-07-04.5 (2024) eq. 39.1 writes the same expression
    with ``S / L`` expanded as eq. 9.
    """
    cb = _fraction("block_coefficient", block_coefficient)
    s_per_l = _positive("wetted_area", wetted_area) / _positive("length", length)
    radius = ((0.887 + 0.145 * cb) * s_per_l - 2.0 * _finite("og", og)) / math.pi
    if radius <= 0.0:
        raise RollDampingComponentError(
            f"friction radius is not positive ({radius!r}); og is too large for the hull"
        )
    return radius


def friction_damping_laminar(
    rho: float,
    nu: float,
    wetted_area: float,
    radius: float,
    amplitude: float,
    omega: float,
) -> float:
    """Kato laminar skin-friction damping at zero speed (eqs. 6 and 7).

    ``C_f = 1.328 (3.22 r**2 R0**2 omega / (2 pi nu)) ** -0.5``, the form also
    given by ITTC 7.5-02-07-04.5 with the roll period ``T = 2 pi / omega``.
    """
    rho = _positive("rho", rho)
    nu = _positive("nu", nu)
    area = _positive("wetted_area", wetted_area)
    radius = _positive("radius", radius)
    amplitude = _positive("amplitude", amplitude)
    omega = _positive("omega", omega)
    cf = (
        1.328 * (3.22 * radius**2 * amplitude**2 * omega / (2.0 * math.pi * nu)) ** -0.5
    )
    return 4.0 / (3.0 * math.pi) * rho * area * radius**3 * amplitude * omega * cf


def friction_damping_turbulent(
    rho: float,
    nu: float,
    wetted_area: float,
    radius: float,
    amplitude: float,
    omega: float,
) -> float:
    """Skin-friction damping with the turbulent correction, zero speed (eq. 10).

    ``0.787 rho S r**2 sqrt(omega nu) (1 + 0.00814 (r**2 R0**2 omega / nu)**0.386)``.
    The leading term equals :func:`friction_damping_laminar`.
    """
    rho = _positive("rho", rho)
    nu = _positive("nu", nu)
    area = _positive("wetted_area", wetted_area)
    radius = _positive("radius", radius)
    amplitude = _non_negative("amplitude", amplitude)
    omega = _positive("omega", omega)
    reynolds = radius**2 * amplitude**2 * omega / nu
    return (
        0.787
        * rho
        * area
        * radius**2
        * math.sqrt(omega * nu)
        * (1.0 + 0.00814 * reynolds**0.386)
    )


def friction_forward_speed_factor(speed: float, omega: float, length: float) -> float:
    """Schmitke's speed factor ``1 + 4.1 U / (omega L)`` on friction (eq. 12)."""
    speed = _non_negative("speed", speed)
    return 1.0 + 4.1 * speed / (_positive("omega", omega) * _positive("length", length))


# --- section 3.2: eddy making -------------------------------------------------


def lewis_coefficients(
    half_beam_draft_ratio: float, sigma: float
) -> tuple[float, float]:
    """Lewis-form mapping coefficients ``(a1, a3)`` of a section.

    The paper's nomenclature calls ``a1, a2, a3`` "extinction coefficients"
    (p. 923); in Ikeda's method and in ITTC 7.5-02-07-04.5 they are the
    Lewis-form parameters of the section, computed here from the half
    beam-draught ratio ``H0 = B / 2d`` and the area coefficient ``sigma``.

    Raises when no Lewis form has that ``H0`` and ``sigma``.
    """
    h0 = _positive("half_beam_draft_ratio", half_beam_draft_ratio)
    sigma = _area_coefficient(sigma)
    ratio = (h0 - 1.0) / (h0 + 1.0)
    c1 = 3.0 + 4.0 * sigma / math.pi + (1.0 - 4.0 * sigma / math.pi) * ratio**2
    discriminant = 9.0 - 2.0 * c1
    if discriminant < 0.0:
        raise RollDampingComponentError(
            f"no Lewis form has H0={h0!r} and sigma={sigma!r} (discriminant {discriminant!r})"
        )
    a3 = (-c1 + 3.0 + math.sqrt(discriminant)) / c1
    a1 = (1.0 + a3) * ratio
    if 1.0 + a1 + a3 <= 0.0 or 1.0 - a1 + a3 <= 0.0:
        raise RollDampingComponentError(
            f"Lewis mapping degenerate for H0={h0!r}, sigma={sigma!r}"
        )
    # The map z = M (zeta + a1/zeta + a3/zeta**3) gives a non-intersecting
    # contour only if dz/dzeta has no zero outside the unit circle, i.e. both
    # roots t = zeta**2 of t**2 - a1 t - 3 a3 = 0 satisfy |t| <= 1.
    root = complex(a1**2 + 12.0 * a3) ** 0.5
    if max(abs((a1 + root) / 2.0), abs((a1 - root) / 2.0)) > 1.0 + 1e-12:
        raise RollDampingComponentError(
            f"the Lewis form for H0={h0!r}, sigma={sigma!r} intersects itself"
        )
    return a1, a3


def bilge_radius_from_area_coefficient(
    beam: float, draft: float, sigma: float
) -> float:
    """Bilge radius of a wall-sided, flat-bottomed section (eqs. 29 and 59).

    ``R = 2 d sqrt(H0 (sigma - 1) / (pi - 4))`` is the radius whose two
    quarter-circle bilges remove the area ``(1 - sigma) B d``. It is capped at
    the draught when ``H0 >= 1`` and at the half beam when ``H0 < 1``.
    """
    beam = _positive("beam", beam)
    draft = _positive("draft", draft)
    sigma = _area_coefficient(sigma)
    h0 = beam / (2.0 * draft)
    radius = 2.0 * draft * math.sqrt(h0 * (sigma - 1.0) / (math.pi - 4.0))
    cap = draft if h0 >= 1.0 else beam / 2.0
    return min(radius, cap)


def _lewis_radius(a1: float, a3: float, scale: float, psi: float) -> float:
    return scale * math.sqrt(
        ((1.0 + a1) * math.sin(psi) - a3 * math.sin(3.0 * psi)) ** 2
        + ((1.0 - a1) * math.cos(psi) + a3 * math.cos(3.0 * psi)) ** 2
    )


def eddy_pressure_ratio(
    beam: float,
    draft: float,
    sigma: float,
    og: float,
    bilge_radius: Optional[float] = None,
    variant: EddyVariant = "chakrabarti2001",
) -> float:
    """Sectional eddy coefficient ``C_R`` of eq. 25, with ``C_p`` (eqs. 19-34).

    ``C_R = C_p (r_max/d)**2 {(1 - f1 R/d)(1 - k OG/d - f1 R/d) + f2 (H0 - f1 R/d)**2}``

    ``variant`` selects the printed forms that differ:

    * ``"chakrabarti2001"`` (default): ``k = 1``; the paper's eq. 25 with the
      whole-bracket reading of ITTC 7.5-02-07-04.5 (2011 and 2024).
    * ``"ittc2024"``: ``k = 3/2``, as printed in ITTC 7.5-02-07-04.5 rev. 02
      (2024), citing Katayama et al. (2022).
    * ``"ittc2011"``: second factor ``(1 - OG/d)`` without ``f1 R/d``, as
      printed in the 2011 revision; ``"ittc2021"`` is the same bracket, as
      printed in the 2021 revision (eq. 43).
    * ``"as_printed"``: Chakrabarti eq. 25 bracketed as typeset, with
      ``(r_max/d)**2`` on the first product only and ``C_p`` (eq. 18) on the
      whole. It is kept to show that the print does not reproduce the paper's
      own Fig. 5; it is not a method.

    ``gamma`` follows the paper's eq. 20; see the module docstring for the
    ITTC print. A result at or below zero raises
    :class:`RollDampingComponentError` (outside the empirical domain).

    The paper prints the third term of ``A1`` and ``B1`` (eqs. 22-23) with a
    minus sign and ``a2`` for ``a3`` in ``B1``; the signs and indices here are
    those of ITTC 7.5-02-07-04.5. ``psi`` is chosen as the larger of
    ``r_max(0)`` and ``r_max(psi2)`` as in ITTC; the paper gives only
    ``psi2``.
    """
    beam = _positive("beam", beam)
    draft = _positive("draft", draft)
    sigma = _area_coefficient(sigma)
    og_d = _og_ratio(og, draft)
    if bilge_radius is None:
        bilge_radius = bilge_radius_from_area_coefficient(beam, draft, sigma)
    radius_d = _section_radius(bilge_radius, beam, draft) / draft
    h0 = beam / (2.0 * draft)

    a1, a3 = lewis_coefficients(h0, sigma)
    scale = beam / (2.0 * (1.0 + a1 + a3))
    psi1 = 0.0
    if a3 != 0.0 and abs(a1 * (1.0 + a3) / (4.0 * a3)) <= 1.0:
        psi2 = 0.5 * math.acos(a1 * (1.0 + a3) / (4.0 * a3))
    else:
        psi2 = math.atan(h0)
    r1 = _lewis_radius(a1, a3, scale, psi1)
    r2 = _lewis_radius(a1, a3, scale, psi2)
    psi, r_max = (psi1, r1) if r1 >= r2 else (psi2, r2)

    h = (
        1.0
        + a1**2
        + 9.0 * a3**2
        + 2.0 * a1 * (1.0 - 3.0 * a3) * math.cos(2.0 * psi)
        - 6.0 * a3 * math.cos(4.0 * psi)
    )
    a_term = (
        -2.0 * a3 * math.cos(5.0 * psi)
        + a1 * (1.0 - a3) * math.cos(3.0 * psi)
        + ((6.0 - 3.0 * a1) * a3**2 + (a1**2 - 3.0 * a1) * a3 + a1**2) * math.cos(psi)
    )
    b_term = (
        -2.0 * a3 * math.sin(5.0 * psi)
        + a1 * (1.0 - a3) * math.sin(3.0 * psi)
        + ((6.0 + 3.0 * a1) * a3**2 + (3.0 * a1 + a1**2) * a3 + a1**2) * math.sin(psi)
    )

    h0_prime = h0 / (1.0 - og_d)
    sigma_prime = (sigma - og_d) / (1.0 - og_d)
    if sigma_prime <= 0.0:
        raise RollDampingComponentError(
            f"sigma' = (sigma - OG/d)/(1 - OG/d) must be positive, got {sigma_prime!r}"
        )
    f1 = 0.5 * (1.0 + math.tanh(20.0 * (sigma - 0.7)))
    f2 = (
        0.5 * (1.0 - math.cos(math.pi * sigma))
        - 1.5 * (1.0 - math.exp(-5.0 * (1.0 - sigma))) * math.sin(math.pi * sigma) ** 2
    )
    f3 = 1.0 + 4.0 * math.exp(-1.65e5 * (1.0 - sigma) ** 2)
    gamma = (
        math.sqrt(math.pi)
        * f3
        * (r_max + 2.0 * scale / h * math.sqrt(a_term**2 + b_term**2))
        / (2.0 * draft * (1.0 - og_d) * math.sqrt(h0_prime * sigma_prime))
    )
    cp = 0.5 * (0.87 * math.exp(-gamma) - 4.0 * math.exp(-0.187 * gamma) + 3.0)

    rmax_d2 = (r_max / draft) ** 2
    bilge = f1 * radius_d
    second = f2 * (h0 - bilge) ** 2
    if variant == "chakrabarti2001":
        first = (1.0 - bilge) * (1.0 - og_d - bilge)
    elif variant == "ittc2024":
        first = (1.0 - bilge) * (1.0 - 1.5 * og_d - bilge)
    elif variant in ("ittc2011", "ittc2021"):
        first = (1.0 - bilge) * (1.0 - og_d)
    elif variant == "as_printed":
        printed = cp * (rmax_d2 * (1.0 - bilge) * (1.0 - og_d - bilge) + second)
        return _dissipative("C_R (as printed)", printed)
    else:
        raise RollDampingComponentError(f"unknown eddy variant {variant!r}")
    return _dissipative("C_R", cp * rmax_d2 * (first + second))


def eddy_damping_section(
    rho: float,
    beam: float,
    draft: float,
    sigma: float,
    og: float,
    bilge_radius: Optional[float],
    amplitude: float,
    omega: float,
    variant: EddyVariant = "chakrabarti2001",
) -> float:
    """Sectional eddy-making damping at zero speed, per unit length (eq. 18).

    ``B'_E0 = (4 / 3 pi) rho d**4 R0 omega C_R``; see :func:`eddy_pressure_ratio`.
    """
    rho = _positive("rho", rho)
    draft = _positive("draft", draft)
    amplitude = _amplitude(amplitude)
    omega = _positive("omega", omega)
    cr = eddy_pressure_ratio(beam, draft, sigma, og, bilge_radius, variant)
    return 4.0 / (3.0 * math.pi) * rho * draft**4 * amplitude * omega * cr


@dataclass(frozen=True)
class HullSection:
    """One station of a hull for strip integration.

    ``x`` is the longitudinal position; ``bilge_radius`` of ``None`` is
    estimated from ``sigma`` with :func:`bilge_radius_from_area_coefficient`.
    """

    x: float
    beam: float
    draft: float
    sigma: float
    bilge_radius: Optional[float] = None


def eddy_damping_sections(
    sections: Sequence[HullSection],
    rho: float,
    og: float,
    amplitude: float,
    omega: float,
    end_station_factor: float = 1.0,
    h0_cap: Optional[float] = None,
    variant: EddyVariant = "chakrabarti2001",
    exclude_non_positive: bool = False,
) -> float:
    """Eddy-making damping of a hull by trapezoidal integration of eq. 18.

    ``og`` is measured from the waterline and applies to every station.
    ``end_station_factor`` multiplies the first and last stations; Chakrabarti
    (2001, p. 930) used 0.7 to suppress the blow-up at raked barge ends and
    calls the factor strictly empirical. ``h0_cap`` limits ``B / 2d`` at each
    station by reducing the beam used there (the paper's optional ULHO, 4 or
    5); how the paper applies the cap is not printed, so this reading is an
    assumption.

    A station whose C_R is zero or negative raises by default. The C_R fit
    passes through zero for some full, rounded sections (H0 near 1, sigma
    near 0.76); ``exclude_non_positive=True`` lets such stations contribute
    zero instead. It is an explicit choice by the caller, not a method from
    the paper, and it should be reported with the result.
    """
    if len(sections) < 2:
        raise RollDampingComponentError("at least two stations are needed to integrate")
    end_station_factor = _non_negative("end_station_factor", end_station_factor)
    xs = [_finite("x", s.x) for s in sections]
    if any(b <= a for a, b in zip(xs, xs[1:])):
        raise RollDampingComponentError(
            "stations must be sorted by strictly increasing x"
        )
    values = []
    for i, s in enumerate(sections):
        beam = s.beam
        if h0_cap is not None:
            beam = min(beam, 2.0 * _positive("h0_cap", h0_cap) * s.draft)
        try:
            value = eddy_damping_section(
                rho,
                beam,
                s.draft,
                s.sigma,
                og,
                s.bilge_radius,
                amplitude,
                omega,
                variant,
            )
        except OutsideEmpiricalDomainError:
            if not exclude_non_positive:
                raise
            value = 0.0
        if i in (0, len(sections) - 1):
            value *= end_station_factor
        values.append(value)
    return sum(
        0.5 * (values[i] + values[i + 1]) * (xs[i + 1] - xs[i])
        for i in range(len(xs) - 1)
    )


def barge_eddy_damping(
    rho: float,
    length: float,
    beam: float,
    draft: float,
    og: float,
    amplitude: float,
    omega: float,
    og_factor: float = 1.0,
) -> float:
    """Eddy-making damping of a sharp-cornered barge at zero speed (eq. 35).

    ``B_E0 = (2/pi) rho L d**4 (H0**2 + 1 - k OG/d)(H0**2 + (1 - OG/d)**2) R0 omega``

    attributed by the paper (p. 923) to Ikeda (1984) and by ITTC 7.5-02-07-04.5
    to Ikeda, Fujiwara and Katayama (1993), recommended by the paper for a
    rectangular barge (p. 931). ``k = og_factor`` is 1 in the paper and in ITTC
    7.5-02-07-04.5 (2011); the 2024 revision prints 3/2. The two agree for a
    roll axis at the waterline. The formula has no bilge-radius term: the
    corner is taken as sharp. Inputs that make the first bracket zero or
    negative (small B/T with a deep roll axis and the 3/2 factor) raise.
    """
    rho = _positive("rho", rho)
    length = _positive("length", length)
    beam = _positive("beam", beam)
    draft = _positive("draft", draft)
    og_d = _og_ratio(og, draft)
    amplitude = _amplitude(amplitude)
    omega = _positive("omega", omega)
    og_factor = _positive("og_factor", og_factor)
    h0 = beam / (2.0 * draft)
    first = _dissipative("eq. 35 first bracket", h0**2 + 1.0 - og_factor * og_d)
    return (
        2.0
        / math.pi
        * rho
        * length
        * draft**4
        * first
        * (h0**2 + (1.0 - og_d) ** 2)
        * amplitude
        * omega
    )


def eddy_forward_speed_factor(omega: float, length: float, speed: float) -> float:
    """Reduction of eddy damping with forward speed or current (eq. 36).

    ``K**2 / (1 + K**2)`` with ``K = 0.04 omega L / U``; 1 at zero speed.
    """
    omega = _positive("omega", omega)
    length = _positive("length", length)
    speed = _non_negative("speed", speed)
    if speed == 0.0:
        return 1.0
    k = 0.04 * omega * length / speed
    return k**2 / (1.0 + k**2)


def triangular_section_drag_coefficient(beam_over_kg: float) -> float:
    """Drag coefficient of a triangular section, Tanaka route (eq. 17)."""
    x = _positive("beam_over_kg", beam_over_kg)
    return 0.438 - 0.449 * x + 0.236 * x**2


def drag_route_eddy_damping(
    drag_coefficient: float,
    rho: float,
    area: float,
    radius: float,
    amplitude: float,
    omega: float,
) -> float:
    """Equivalent linear damping from the drag-coefficient route (eq. 13).

    The paper gives the force ``F = 1/2 C_e rho S (r omega R0)**2`` but not
    its conversion to a damping coefficient. Taken here as the moment
    ``F r`` of a quadratic damper, linearised by eq. 4:
    ``B = (8 / 3 pi) (1/2 C_e rho S r**3) omega R0``. The drag coefficient
    ``C_e`` (eq. 14) needs the C1 and C2 tables of the paper and is an input.
    """
    cd = _non_negative("drag_coefficient", drag_coefficient)
    rho = _positive("rho", rho)
    area = _positive("area", area)
    radius = _positive("radius", radius)
    amplitude = _amplitude(amplitude)
    quadratic = 0.5 * cd * rho * area * radius**3
    return equivalent_linear_damping(0.0, quadratic, 0.0, omega, amplitude)


# --- section 4: lift ----------------------------------------------------------


def lift_damping(
    rho: float,
    speed: float,
    length: float,
    draft: float,
    beam: float,
    og: float,
    midship_coefficient: float,
) -> float:
    """Hull lift damping at forward speed or in current (eqs. 37 and 38).

    ``B_L = (0.15/2) rho U L d**3 k_N (1 - 2.8 OG/d + 4.667 (OG/d)**2)``,
    ``k_N = 2 pi d / L + kappa (4.1 B / L - 0.045)``, ``kappa`` 0, 0.1 or 0.3
    for ``C_M <= 0.92``, ``<= 0.97`` and above. Identical to ITTC
    7.5-02-07-04.5 eq. 33 with ``l0 = 0.3 d`` and ``l_R = 0.5 d``.
    """
    rho = _positive("rho", rho)
    speed = _non_negative("speed", speed)
    length = _positive("length", length)
    draft = _positive("draft", draft)
    beam = _positive("beam", beam)
    og_d = _finite("og", og) / draft
    cm = _fraction("midship_coefficient", midship_coefficient)
    if cm <= 0.92:
        kappa = 0.0
    elif cm <= 0.97:
        kappa = 0.1
    else:
        kappa = 0.3
    k_n = 2.0 * math.pi * draft / length + kappa * (4.1 * beam / length - 0.045)
    return (
        0.15
        / 2.0
        * rho
        * speed
        * length
        * draft**3
        * k_n
        * (1.0 - 2.8 * og_d + 4.667 * og_d**2)
    )


# --- section 5: wave ----------------------------------------------------------


def wave_damping_forward_speed(
    zero_speed_damping: float,
    omega: float,
    draft: float,
    speed: float,
    gravity: float = STANDARD_GRAVITY,
) -> float:
    """Wave (radiation) roll damping corrected for speed (eqs. 39-43).

    ``zero_speed_damping`` is the potential-flow roll damping from a
    diffraction solver at ``omega``. The correction is Ikeda's doublet model;
    ITTC 7.5-02-07-04.5 (2011 eq. 2.8, 2024 eq. 31) prints the tanh argument
    as ``20 Omega - 0.3``, which does not return the zero-speed value at
    ``U = 0``; the paper's ``20 (Omega - 0.3)`` does, and is used here.
    """
    b_w0 = _non_negative("zero_speed_damping", zero_speed_damping)
    omega = _positive("omega", omega)
    draft = _positive("draft", draft)
    speed = _non_negative("speed", speed)
    gravity = _positive("gravity", gravity)
    if speed == 0.0:
        # At zero speed the fitted factor evaluates to 1 + O(1e-4) rather than
        # exactly 1 (tanh(-6) and exp(-9.4) are not exact limits); the input
        # is by definition the zero-speed value, so it is returned unchanged.
        return b_w0
    xi = omega**2 * draft / gravity
    tau = speed * omega / gravity
    a1 = 1.0 + xi**-1.2 * math.exp(-2.0 * xi)
    a2 = 0.5 + xi**-1.0 * math.exp(-2.0 * xi)
    factor = 0.5 * (
        (a2 + 1.0)
        + (a2 - 1.0) * math.tanh(20.0 * (tau - 0.3))
        + (2.0 * a1 - a2 - 1.0) * math.exp(-150.0 * (tau - 0.25) ** 2)
    )
    return b_w0 * factor


# --- section 6: bilge keel ----------------------------------------------------


def bilge_keel_lever(
    beam: float, draft: float, og: float, bilge_radius: float
) -> float:
    """Distance from the roll axis to the bilge-circle midpoint (eq. 60)."""
    beam = _positive("beam", beam)
    draft = _positive("draft", draft)
    og_d = _og_ratio(og, draft)
    r_d = _section_radius(bilge_radius, beam, draft) / draft
    h0 = beam / (2.0 * draft)
    return draft * math.sqrt((h0 - 0.293 * r_d) ** 2 + (1.0 - og_d - 0.293 * r_d) ** 2)


def _flow_factor(sigma: float) -> float:
    """Velocity increase at the bilge, ``1 + 0.3 exp(-160 (1 - sigma))`` (eq. 49)."""
    return 1.0 + 0.3 * math.exp(-160.0 * (1.0 - sigma))


def bilge_keel_normal_section(
    rho: float,
    beam: float,
    draft: float,
    sigma: float,
    og: float,
    bilge_keel_breadth: float,
    amplitude: float,
    omega: float,
    bilge_radius: Optional[float] = None,
) -> float:
    """Bilge-keel normal-force damping per unit keel length, zero speed (eqs. 47-48).

    Uses ``f**2`` as the paper and ITTC 2024 print; ITTC 2011 eq. 2.25 prints
    ``f``.
    """
    rho = _positive("rho", rho)
    sigma = _area_coefficient(sigma)
    b_bk = _positive("bilge_keel_breadth", bilge_keel_breadth)
    amplitude = _amplitude(amplitude, positive=True)
    omega = _positive("omega", omega)
    if bilge_radius is None:
        bilge_radius = bilge_radius_from_area_coefficient(beam, draft, sigma)
    lever = bilge_keel_lever(beam, draft, og, bilge_radius)
    f = _flow_factor(sigma)
    cd = 22.5 * b_bk / (math.pi * lever * amplitude * f) + 2.4
    return 8.0 / (3.0 * math.pi) * rho * lever**3 * b_bk * omega * amplitude * f**2 * cd


def bilge_keel_hull_pressure_section(
    rho: float,
    beam: float,
    draft: float,
    sigma: float,
    og: float,
    bilge_keel_breadth: float,
    amplitude: float,
    omega: float,
    bilge_radius: Optional[float] = None,
) -> float:
    """Damping from hull pressure induced by bilge keels, per unit length (eqs. 50-58).

    Uses ``B0 = m4**3 / (3 (H0 - 0.215 m1)) + ...`` as printed in the paper and
    in ITTC 7.5-02-07-04.5 (2024); the 2011 revision prints ``m2**2`` for
    ``m4**3``. This part alone may be negative (the hull pressure can oppose
    the keel force); :func:`bilge_keel_damping_section` checks the sum.
    """
    rho = _positive("rho", rho)
    beam = _positive("beam", beam)
    draft = _positive("draft", draft)
    sigma = _area_coefficient(sigma)
    b_bk = _positive("bilge_keel_breadth", bilge_keel_breadth)
    amplitude = _amplitude(amplitude, positive=True)
    omega = _positive("omega", omega)
    if bilge_radius is None:
        bilge_radius = bilge_radius_from_area_coefficient(beam, draft, sigma)
    radius = _section_radius(bilge_radius, beam, draft)
    lever = bilge_keel_lever(beam, draft, og, radius)
    f = _flow_factor(sigma)
    h0 = beam / (2.0 * draft)

    m1 = radius / draft
    m2 = og / draft
    m3 = 1.0 - m1 - m2
    m4 = h0 - m1
    denominator = (h0 - 0.215 * m1) * (1.0 - 0.215 * m1)
    m5 = (0.414 * h0 + 0.0651 * m1**2 - (0.382 * h0 + 0.0106) * m1) / denominator
    m6 = (0.414 * h0 + 0.0651 * m1**2 - (0.382 + 0.0106 * h0) * m1) / denominator
    s0 = 0.3 * math.pi * f * lever * amplitude + 1.95 * b_bk
    quarter = 0.25 * math.pi * radius
    if s0 > quarter:
        m7 = s0 / draft - 0.25 * math.pi * m1
        m8 = m7 + 0.414 * m1
    else:
        m7 = 0.0
        m8 = m7 + math.sqrt(2.0) * (1.0 - math.cos(s0 / radius)) * m1
    a2 = (m3 + m4) * m8 - m7**2
    b2 = (
        m4**3 / (3.0 * (h0 - 0.215 * m1))
        + (1.0 - m1) ** 2 * (2.0 * m3 - m2) / (6.0 * (1.0 - 0.215 * m1))
        + m1 * (m3 * m5 + m4 * m6)
    )
    cp_minus = -22.5 * b_bk / (math.pi * lever * f * amplitude) - 1.2
    cp_plus = 1.2
    return (
        4.0
        / (3.0 * math.pi)
        * rho
        * lever**2
        * draft**2
        * omega
        * amplitude
        * f**2
        * (-cp_minus * a2 + cp_plus * b2)
    )


def bilge_keel_damping_section(
    rho: float,
    beam: float,
    draft: float,
    sigma: float,
    og: float,
    bilge_keel_breadth: float,
    amplitude: float,
    omega: float,
    bilge_radius: Optional[float] = None,
) -> float:
    """Bilge-keel damping per unit keel length at zero speed (eq. 46).

    Normal-force plus hull-pressure parts; the keel's own wave making and the
    speed effect are neglected, as in the paper. A sum at or below zero raises
    (outside the empirical domain, for example at very large amplitude).
    """
    args = (
        rho,
        beam,
        draft,
        sigma,
        og,
        bilge_keel_breadth,
        amplitude,
        omega,
        bilge_radius,
    )
    return _dissipative(
        "bilge-keel damping",
        bilge_keel_normal_section(*args) + bilge_keel_hull_pressure_section(*args),
    )


# --- totals -------------------------------------------------------------------


@dataclass(frozen=True)
class RollDampingComponents:
    """Equivalent linear roll damping by component, in consistent units.

    ``flags`` lists domain conditions that the caller must see (``FLAG_*``
    constants); an empty tuple means none was raised. ``citations`` is the
    structured provenance of the components, one :class:`Citation` per
    equation group; :func:`validate_citations` resolves it.
    """

    friction: float
    eddy: float
    lift: float
    wave: float
    bilge_keel: float
    flags: tuple[str, ...] = ()
    citations: tuple[Citation, ...] = field(default=(), repr=False)

    def __post_init__(self) -> None:
        for name in ("friction", "eddy", "lift", "wave", "bilge_keel"):
            _non_negative(name, getattr(self, name))

    @property
    def total(self) -> float:
        """``B_eq`` of eq. 5."""
        return self.friction + self.eddy + self.lift + self.wave + self.bilge_keel

    def percent_critical(
        self, stiffness: float, inertia: float, added_inertia: float
    ) -> float:
        """Total as a percentage of ``2 sqrt(C (I + A))`` (roll_damping basis)."""
        return damping_to_percent_critical(
            self.total, stiffness, inertia, added_inertia
        )

    def as_dict(self) -> dict[str, float]:
        return {
            "friction": self.friction,
            "eddy": self.eddy,
            "lift": self.lift,
            "wave": self.wave,
            "bilge_keel": self.bilge_keel,
            "total": self.total,
        }


def _cite(*sections: str) -> tuple[Citation, ...]:
    return tuple(replace(SOURCE_CITATION, section=s) for s in sections)


def validate_citations(
    result: RollDampingComponents, wiki_root: Optional[Path] = None
) -> None:
    """Resolve every citation of ``result`` fail-closed against the wiki.

    Raises :class:`digitalmodel.citations.schema.CitationResolutionError` when
    the page is missing or its code_id, publisher or revision differ. With
    ``wiki_root`` of ``None`` the citation resolver's search order is used.
    """
    if not result.citations:
        raise RollDampingComponentError("the result carries no citations to resolve")
    for citation in result.citations:
        validate_citation(citation, repo_root=wiki_root)


def barge_roll_damping(
    rho: float,
    nu: float,
    length: float,
    beam: float,
    draft: float,
    kg: float,
    amplitude: float,
    omega: float,
    wave_damping_zero_speed: float = 0.0,
    speed: float = 0.0,
    block_coefficient: float = 1.0,
    midship_coefficient: float = 0.999,
    bilge_keel_breadth: float = 0.0,
    bilge_keel_length: float = 0.0,
    bilge_radius: float = 0.0,
    og_factor: float = 1.0,
    gravity: float = STANDARD_GRAVITY,
) -> RollDampingComponents:
    """Component roll damping of a rectangular barge, as the paper recommends.

    * friction: eqs. 8-10 with the ship wetted-area regression, times eq. 12;
    * eddy: the sharp-cornered barge formula, eq. 35, times eq. 36;
    * lift: eqs. 37-38 (zero at zero speed);
    * wave: ``wave_damping_zero_speed`` from a diffraction solver at
      ``omega``, corrected by eq. 39;
    * bilge keel: eqs. 46-60 per unit length times ``bilge_keel_length``,
      evaluated with ``bilge_radius`` (a sharp corner when zero) and the
      section area coefficient implied by that radius.

    ``omega`` should be the roll natural frequency when the result is turned
    into a percentage of critical.

    The keel inputs are validated whether or not a keel is fitted; a keel needs
    both a breadth and a length. The result is flagged
    ``FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES`` above 10 deg and
    ``FLAG_EQ35_WITH_ROUNDED_BILGE`` when a non-zero bilge radius is given,
    because eq. 35 assumes a sharp corner.
    """
    draft = _positive("draft", draft)
    beam = _positive("beam", beam)
    og = draft - _positive("kg", kg)
    amplitude = _amplitude(amplitude, positive=True)
    _fraction("midship_coefficient", midship_coefficient)
    b_bk = _non_negative("bilge_keel_breadth", bilge_keel_breadth)
    l_bk = _non_negative("bilge_keel_length", bilge_keel_length)
    radius = _section_radius(bilge_radius, beam, draft)
    if (b_bk > 0.0) != (l_bk > 0.0):
        raise RollDampingComponentError(
            "a bilge keel needs both a breadth and a length; got breadth "
            f"{b_bk!r} and length {l_bk!r}"
        )
    radius_f = friction_radius(
        block_coefficient,
        ship_wetted_area(length, draft, beam, block_coefficient),
        length,
        og,
    )
    area = ship_wetted_area(length, draft, beam, block_coefficient)
    friction = friction_damping_turbulent(rho, nu, area, radius_f, amplitude, omega)
    friction *= friction_forward_speed_factor(speed, omega, length)

    eddy = barge_eddy_damping(rho, length, beam, draft, og, amplitude, omega, og_factor)
    eddy *= eddy_forward_speed_factor(omega, length, speed)

    lift = lift_damping(rho, speed, length, draft, beam, og, midship_coefficient)
    wave = wave_damping_forward_speed(
        wave_damping_zero_speed, omega, draft, speed, gravity
    )

    bilge_keel = 0.0
    if b_bk > 0.0:
        sigma = 1.0 - (4.0 - math.pi) * radius**2 / (2.0 * beam * draft)
        bilge_keel = l_bk * bilge_keel_damping_section(
            rho, beam, draft, sigma, og, b_bk, amplitude, omega, radius
        )

    flags = []
    if amplitude > PAPER_EXAMPLE_AMPLITUDE:
        flags.append(FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES)
    if radius > 0.0:
        flags.append(FLAG_EQ35_WITH_ROUNDED_BILGE)
    citations = _cite(
        "eqs. 4-5, p. 918 (equivalent linearisation and component sum)",
        "eqs. 8-10, 12, pp. 919-920 (skin friction)",
        "eqs. 35-36, p. 924 (sharp-cornered barge eddy making)",
        "eqs. 37-38, p. 924 (lift)",
        "eqs. 39-43, p. 925 (wave damping at speed)",
    )
    if b_bk > 0.0:
        citations += _cite("eqs. 46-60, pp. 926-927 (bilge keel)")
    return RollDampingComponents(
        friction, eddy, lift, wave, bilge_keel, tuple(flags), citations
    )
