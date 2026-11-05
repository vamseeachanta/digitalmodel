# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bottom-up casing seats and a static, vertical-well kick-tolerance screen.

The calculation basis is the equations in issue #3842. Cited textbook editions
and NORSOK requirements have not been independently confirmed. Results are
screening calculations, not an operational casing programme or well-control
approval. No gas expansion, temperature, circulation, riser or deviation model
is included. Input depths share a surface TVD datum; gradients are EMW in ppg.
"""

from __future__ import annotations

from bisect import bisect_right
from dataclasses import dataclass
import math
from typing import Literal

# Rounded oilfield conversions supplied in the issue, not licensed criteria.
PPG_TO_PSI_PER_FT = 0.052
ANNULAR_CAPACITY_DIVISOR = 1029.4


def _nonnegative(value: float, name: str) -> None:
    if not math.isfinite(value) or value < 0:
        raise ValueError(f"{name} must be finite and nonnegative")


def _positive(value: float, name: str) -> None:
    _nonnegative(value, name)
    if value == 0:
        raise ValueError(f"{name} must be positive")


def _basis(value: str, name: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must state a nonblank basis")


@dataclass(frozen=True)
class CasingPressureProfile:
    """Piecewise-linear pore/fracture EMW on common, increasing TVD knots.

    At least two knots and an explicit basis are required. No extrapolation is
    allowed. Checking fracture >= pore at every common knot also checks every
    point between knots, since their difference is linear there.
    """

    depths_ft: tuple[float, ...]
    pore_ppg: tuple[float, ...]
    fracture_ppg: tuple[float, ...]
    basis: str

    def __post_init__(self) -> None:
        """Copy sequences and reject ambiguous or nonphysical profiles."""
        for name in ("depths_ft", "pore_ppg", "fracture_ppg"):
            object.__setattr__(self, name, tuple(getattr(self, name)))
        _basis(self.basis, "profile basis")
        if len(self.depths_ft) < 2 or not (
            len(self.depths_ft) == len(self.pore_ppg) == len(self.fracture_ppg)
        ):
            raise ValueError("profile requires at least two matching knots")
        for depth in self.depths_ft:
            _nonnegative(depth, "depth")
        if any(b <= a for a, b in zip(self.depths_ft, self.depths_ft[1:])):
            raise ValueError("profile depths must be strictly increasing")
        for pore, fracture in zip(self.pore_ppg, self.fracture_ppg):
            _positive(pore, "pore EMW")
            _positive(fracture, "fracture EMW")
            if fracture < pore:
                raise ValueError("fracture EMW must not be below pore EMW")

    def _at(self, depth_ft: float, values: tuple[float, ...]) -> float:
        _nonnegative(depth_ft, "interpolation depth")
        if not self.depths_ft[0] <= depth_ft <= self.depths_ft[-1]:
            raise ValueError("depth is outside the profile; extrapolation refused")
        i = min(bisect_right(self.depths_ft, depth_ft) - 1, len(values) - 2)
        fraction = (depth_ft - self.depths_ft[i]) / (
            self.depths_ft[i + 1] - self.depths_ft[i]
        )
        return values[i] + fraction * (values[i + 1] - values[i])

    def pore_at(self, depth_ft: float) -> float:
        """Interpolate pore EMW, ppg, at a TVD within the profile."""
        return self._at(depth_ft, self.pore_ppg)

    def fracture_at(self, depth_ft: float) -> float:
        """Interpolate fracture EMW, ppg, at a TVD within the profile."""
        return self._at(depth_ft, self.fracture_ppg)


@dataclass(frozen=True)
class CasingSection:
    """One bottom-up interval, with its shoe criterion and bottom mud weight.

    ``profile_top`` marks the terminal interval whose shallow strings remain
    unspecified. Its upper boundary is not a selected casing seat.
    """

    shoe_ft: float
    bottom_ft: float
    mud_weight_ppg: float
    required_fracture_ppg: float
    fracture_at_shoe_ppg: float
    criterion: Literal["fracture_gradient", "profile_top"]


@dataclass(frozen=True)
class SeatSelection:
    """Sections in deepest-first order and the retained design basis."""

    profile: CasingPressureProfile
    td_ft: float
    trip_margin_ppg: float
    kick_margin_ppg: float
    trip_margin_basis: str
    kick_margin_basis: str
    sections: tuple[CasingSection, ...]

    @property
    def seat_depths_ft(self) -> tuple[float, ...]:
        """Selected shoes only, deepest first, excluding the terminal top."""
        return tuple(s.shoe_ft for s in self.sections
                     if s.criterion == "fracture_gradient")


def _shallowest_shoe(
    profile: CasingPressureProfile, bottom: float, required: float,
) -> float:
    # Walk upward over the exact linear segments. The first knot below the
    # threshold bounds the deepest upward crossing, including fracture troughs.
    deeper = bottom
    deeper_fg = profile.fracture_at(bottom)
    if deeper_fg < required:
        raise ValueError("required fracture EMW exceeds fracture EMW at bottom")
    for depth in reversed(profile.depths_ft):
        if depth >= bottom:
            continue
        fracture = profile.fracture_at(depth)
        if fracture < required:
            return depth + (required - fracture) * (deeper - depth) / (
                deeper_fg - fracture
            )
        deeper, deeper_fg = depth, fracture
    return profile.depths_ft[0]


def select_casing_seats(
    profile: CasingPressureProfile,
    *,
    td_ft: float,
    trip_margin_ppg: float,
    kick_margin_ppg: float,
    trip_margin_basis: str,
    kick_margin_basis: str,
    max_sections: int = 100,
) -> SeatSelection:
    """Select the shallowest shoe holding MW + kick margin to each bottom.

    MW is pore(bottom) + trip margin. The full fracture suffix must satisfy the
    threshold, including internal troughs. Selection stops when that suffix
    reaches the profile top. A bottom window that cannot hold the required
    weight, a zero-length interval, or an interval underbalanced at an internal
    pore peak raises ValueError rather than returning a feasible programme.
    The computational limit ``max_sections`` bounds profiles that approach a
    fixed depth without reaching the top; it is not an engineering criterion.
    """
    _positive(td_ft, "TD")
    if not profile.depths_ft[0] < td_ft <= profile.depths_ft[-1]:
        raise ValueError("TD must be deeper than the profile top and within its range")
    _nonnegative(trip_margin_ppg, "trip margin")
    _nonnegative(kick_margin_ppg, "kick margin")
    _basis(trip_margin_basis, "trip margin basis")
    _basis(kick_margin_basis, "kick margin basis")
    if type(max_sections) is not int or max_sections <= 0:
        raise ValueError("max_sections must be a positive integer")
    bottom = td_ft
    sections: list[CasingSection] = []
    while True:
        if len(sections) >= max_sections:
            raise ValueError("selection exceeds max_sections; profile may not converge")
        mud = profile.pore_at(bottom) + trip_margin_ppg
        required = mud + kick_margin_ppg
        shoe = _shallowest_shoe(profile, bottom, required)
        if bottom - shoe <= 1e-8:
            raise ValueError("selection cannot make upward progress")
        if any(shoe <= d <= bottom and profile.pore_at(d) > mud
               for d in profile.depths_ft) or profile.pore_at(shoe) > mud:
            raise ValueError("bottom-based mud weight underbalances a pore peak")
        criterion: Literal["fracture_gradient", "profile_top"] = (
            "profile_top" if shoe == profile.depths_ft[0] else "fracture_gradient"
        )
        sections.append(CasingSection(
            shoe, bottom, mud, required, profile.fracture_at(shoe), criterion,
        ))
        if criterion == "profile_top":
            break
        bottom = shoe
    return SeatSelection(
        profile, td_ft, trip_margin_ppg, kick_margin_ppg,
        trip_margin_basis, kick_margin_basis, tuple(sections),
    )


@dataclass(frozen=True)
class KickTolerance:
    """Static single-bubble capacity and explicit design-volume acceptance."""

    section: CasingSection
    influx_density_ppg: float
    hole_diameter_in: float
    pipe_diameter_in: float
    design_kick_intensity_ppg: float
    minimum_volume_bbl: float
    kick_intensity_basis: str
    minimum_volume_basis: str
    shoe_margin_psi: float
    zero_volume_kick_intensity_ppg: float
    annular_capacity_bbl_ft: float
    influx_height_ft: float
    volume_bbl: float
    limited_by_section_length: bool
    status: Literal["no tolerance", "below minimum volume", "acceptable"]
    passes: bool


def kick_tolerance(
    section: CasingSection,
    *,
    influx_density_ppg: float,
    hole_diameter_in: float,
    pipe_diameter_in: float,
    design_kick_intensity_ppg: float,
    minimum_volume_bbl: float,
    kick_intensity_basis: str,
    minimum_volume_basis: str,
) -> KickTolerance:
    """Check one vertical section with a single bubble at the bit.

    KI is the excess formation EMW over the section MW, not over pore EMW.
    H = (shoe * (FG_shoe - MW) - bottom * KI) / (MW - rho_influx).
    A nonpositive numerator gives no tolerance, even if minimum volume is zero.
    H is capped at bottom - shoe: beyond that the bubble enters the casing and
    the single open-hole geometry no longer applies. The cap is reported.
    """
    _nonnegative(section.shoe_ft, "shoe depth")
    _positive(section.bottom_ft, "bottom depth")
    if section.shoe_ft >= section.bottom_ft:
        raise ValueError("shoe must be shallower than section bottom")
    _positive(section.mud_weight_ppg, "mud weight")
    _positive(section.fracture_at_shoe_ppg, "shoe fracture EMW")
    _nonnegative(influx_density_ppg, "influx density")
    if influx_density_ppg >= section.mud_weight_ppg:
        raise ValueError("influx density must be below mud weight")
    _positive(hole_diameter_in, "hole diameter")
    _positive(pipe_diameter_in, "pipe diameter")
    if pipe_diameter_in >= hole_diameter_in:
        raise ValueError("hole diameter must exceed pipe diameter")
    _nonnegative(design_kick_intensity_ppg, "design kick intensity")
    _nonnegative(minimum_volume_bbl, "minimum acceptable volume")
    _basis(kick_intensity_basis, "kick intensity basis")
    _basis(minimum_volume_basis, "minimum volume basis")
    margin = section.shoe_ft * (
        section.fracture_at_shoe_ppg - section.mud_weight_ppg
    )
    available = margin - section.bottom_ft * design_kick_intensity_ppg
    # Treat cancellation at KI_0 as zero; tolerance is in ft*ppg, not height.
    if math.isclose(available, 0.0, abs_tol=1e-9):
        available = 0.0
    height = max(0.0, available / (section.mud_weight_ppg - influx_density_ppg))
    length = section.bottom_ft - section.shoe_ft
    limited = height > length
    height = min(height, length)
    capacity = (hole_diameter_in ** 2 - pipe_diameter_in ** 2) / (
        ANNULAR_CAPACITY_DIVISOR
    )
    volume = height * capacity
    passes = height > 0.0 and volume >= minimum_volume_bbl
    status: Literal["no tolerance", "below minimum volume", "acceptable"] = (
        "no tolerance" if height == 0.0 else
        "acceptable" if passes else "below minimum volume"
    )
    return KickTolerance(
        section, influx_density_ppg, hole_diameter_in, pipe_diameter_in,
        design_kick_intensity_ppg, minimum_volume_bbl,
        kick_intensity_basis, minimum_volume_basis,
        PPG_TO_PSI_PER_FT * margin, margin / section.bottom_ft,
        capacity, height, volume, limited, status, passes,
    )
