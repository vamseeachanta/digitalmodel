# ABOUTME: Tug fender-height optimization and berthing-energy check across an assisted fleet
# ABOUTME: A fender at the wrong height rides over/under the assisted hull and loses the load path
"""Tug fendering analysis (issue #1196).

A tug serves vessels of widely varying freeboard and hull geometry from one
fixed fender arrangement. If the fender sits too high or too low relative to an
assisted vessel's contact band, the push-point load path is lost and the tug
cannot safely push. This module finds the fender height that maximizes contact
overlap across the assisted-fleet freeboard envelope, and checks the bow fender
energy capacity against PIANC berthing energy.

References:
    - PIANC WG33 / WG145 berthing-energy method (E = 0.5 m v^2 Cm Ce Cs)
    - Maritime Symposium Rotterdam, optimization of tug boat operations
"""

from dataclasses import dataclass


@dataclass
class AssistedVessel:
    """Contact band a tug can push against on an assisted vessel."""

    name: str
    contact_low_m: float  # lower edge of usable push band above waterline, m
    contact_high_m: float  # upper edge of usable push band above waterline, m


@dataclass
class FenderHeightResult:
    """Optimized fender height across an assisted fleet."""

    fender_height_m: float  # centre height of fender above waterline, m
    fender_face_height_m: float  # vertical extent (face height) of the fender, m
    mean_overlap_fraction: float  # mean contact overlap across the fleet (0..1)
    worst_vessel: str  # vessel with the least overlap
    worst_overlap_fraction: float
    all_vessels_contact: bool  # every vessel has some overlap


@dataclass
class BerthingEnergyResult:
    """PIANC berthing-energy check against a fender's rated capacity."""

    berthing_energy_kj: float
    fender_rated_energy_kj: float
    utilisation: float  # required / rated
    adequate: bool


def _overlap_fraction(v: AssistedVessel, low: float, high: float) -> float:
    """Fraction of the fender face that overlaps a vessel's contact band."""
    face = high - low
    if face <= 0:
        return 0.0
    lo = max(v.contact_low_m, low)
    hi = min(v.contact_high_m, high)
    return max(0.0, hi - lo) / face


def optimize_fender_height(
    fleet: list[AssistedVessel],
    fender_face_height_m: float,
    search_step_m: float = 0.05,
) -> FenderHeightResult:
    """Find the fender centre height that maximizes mean contact overlap.

    Scans candidate fender centre heights over the range spanned by the fleet's
    contact bands and picks the one with the greatest mean overlap fraction.

    Args:
        fleet: assisted vessels with their usable contact bands.
        fender_face_height_m: vertical extent of the tug's fender face, m.
        search_step_m: scan resolution, m.

    Returns:
        FenderHeightResult with the optimal centre height and per-fleet overlap.
    """
    if not fleet:
        raise ValueError("fleet must contain at least one vessel")
    if fender_face_height_m <= 0:
        raise ValueError("fender_face_height_m must be positive")

    lo_bound = min(v.contact_low_m for v in fleet)
    hi_bound = max(v.contact_high_m for v in fleet)
    half = fender_face_height_m / 2.0

    best = None
    centre = lo_bound
    # iterate inclusive of the upper bound
    n_steps = int(round((hi_bound - lo_bound) / search_step_m)) + 1
    for i in range(max(n_steps, 1)):
        centre = lo_bound + i * search_step_m
        low = centre - half
        high = centre + half
        overlaps = [_overlap_fraction(v, low, high) for v in fleet]
        mean_overlap = sum(overlaps) / len(overlaps)
        worst_idx = min(range(len(overlaps)), key=lambda k: overlaps[k])
        candidate = FenderHeightResult(
            fender_height_m=centre,
            fender_face_height_m=fender_face_height_m,
            mean_overlap_fraction=mean_overlap,
            worst_vessel=fleet[worst_idx].name,
            worst_overlap_fraction=overlaps[worst_idx],
            all_vessels_contact=all(o > 0 for o in overlaps),
        )
        if best is None or candidate.mean_overlap_fraction > best.mean_overlap_fraction:
            best = candidate

    return best


def berthing_energy(
    displacement_t: float,
    approach_speed_m_s: float,
    fender_rated_energy_kj: float,
    mass_coefficient: float = 1.5,
    eccentricity_coefficient: float = 0.7,
    softness_coefficient: float = 1.0,
) -> BerthingEnergyResult:
    """PIANC berthing energy and fender adequacy.

    E = 0.5 * m * v^2 * Cm * Ce * Cs

    Args:
        displacement_t: berthing mass, tonnes.
        approach_speed_m_s: normal approach velocity, m/s.
        fender_rated_energy_kj: fender energy absorption capacity, kJ.
        mass_coefficient: added-mass coefficient Cm.
        eccentricity_coefficient: eccentricity coefficient Ce.
        softness_coefficient: softness coefficient Cs.

    Returns:
        BerthingEnergyResult with required energy, utilisation and pass/fail.
    """
    if displacement_t <= 0 or approach_speed_m_s < 0:
        raise ValueError("invalid displacement or approach speed")

    mass_kg = displacement_t * 1000.0
    energy_j = (
        0.5
        * mass_kg
        * approach_speed_m_s**2
        * mass_coefficient
        * eccentricity_coefficient
        * softness_coefficient
    )
    energy_kj = energy_j / 1000.0
    utilisation = (
        energy_kj / fender_rated_energy_kj
        if fender_rated_energy_kj > 0
        else float("inf")
    )

    return BerthingEnergyResult(
        berthing_energy_kj=energy_kj,
        fender_rated_energy_kj=fender_rated_energy_kj,
        utilisation=utilisation,
        adequate=utilisation <= 1.0,
    )
