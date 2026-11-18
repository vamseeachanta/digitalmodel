# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""STAWAVE-1 for a single wind sea with stated limits; no swell correction.

Source: ITTC 7.5-04-01-01.1, Preparation, Conduct and Analysis of Speed/Power
Trials (2024, revision 08), Appendix G.1, equation G-1 and section 6.3.
https://ittc.info/media/11972/75-04-01-011.pdf
This is reflection screening, not a complete wave-resistance prediction.
"""

from dataclasses import dataclass
from math import sqrt

from .energy import wave_frequencies
from .models import OperatingPoint, RegularWaves, ResistanceRow, Ship, Status, finite


@dataclass(frozen=True)
class ReflectionEvidence:
    """Required operating evidence, supplied independently of this formula.

    hs_m is the significant height of the single wind sea (no swell).
    bow_acceleration_m_s2 is the assessed vertical bow acceleration magnitude
    for the same speed, heading and sea state. Its derivation and probability
    level belong to the supplied motion assessment. It is not inferred from Hs.
    short_waves_confirmed records an external short-wave applicability check;
    G.1 supplies no numerical wavelength boundary, so none is invented here.
    """

    hs_m: float
    bow_acceleration_m_s2: float
    height_basis: str
    short_waves_confirmed: bool

    def __post_init__(self):
        finite(self.hs_m, "significant wave height")
        finite(self.bow_acceleration_m_s2, "bow acceleration")
        if self.height_basis not in {"measured", "observed"}:
            raise ValueError("height basis must be measured or observed")
        if not isinstance(self.short_waves_confirmed, bool):
            raise ValueError("short-wave confirmation must be boolean")


@dataclass(frozen=True)
class ReflectionResult:
    status: Status
    resistance_n: float | None
    reason: str


def applicability(
    ship: Ship, point: OperatingPoint, evidence: ReflectionEvidence
) -> str:
    """Return all violated method gates, or an empty string if evaluated."""
    reasons = []
    # ITTC 2024 rev. 08, Appendix G.1: bow sector +/-45 degrees, inclusive.
    if abs(point.heading_deg) > 45:
        reasons.append("heading outside the +/-45 degree bow sector")
    # Same clause: bow vertical acceleration strictly less than 0.05*g.
    if evidence.bow_acceleration_m_s2 >= 0.05 * ship.gravity_m_s2:
        reasons.append("bow acceleration is not below 0.05*g")
    # ITTC 2024 rev. 08, section 6.3: height <= factor*sqrt(Lpp/100).
    factor = 2.25 if evidence.height_basis == "measured" else 1.5
    if evidence.hs_m > factor * sqrt(ship.length_pp_m / 100):
        reasons.append("significant height exceeds section 6.3 limit")
    if not evidence.short_waves_confirmed:
        reasons.append("short-wave applicability has not been confirmed")
    return "; ".join(reasons)


def reflection_coefficient(ship: Ship) -> float:
    """C in R=C*Hs², N/m²; ITTC 2024 rev. 08, Appendix G.1, equation G-1."""
    return (
        ship.density_kg_m3
        * ship.gravity_m_s2
        * ship.beam_m
        * sqrt(ship.beam_m / ship.bow_length_m)
        / 16
    )


def stawave1(
    ship: Ship, point: OperatingPoint, evidence: ReflectionEvidence
) -> ReflectionResult:
    """Irregular-sea resistance from G-1; invalid applicability is NOT_EVALUATED."""
    reason = applicability(ship, point, evidence)
    if reason:
        return ReflectionResult(Status.NOT_EVALUATED, None, reason)
    return ReflectionResult(
        Status.EVALUATED,
        reflection_coefficient(ship) * evidence.hs_m**2,
        "Single wind sea; reflection only; externally confirmed short waves and supplied bow acceleration.",
    )


def stawave1_equivalent_regular_table(
    ship: Ship,
    point: OperatingPoint,
    waves: RegularWaves,
    evidence: ReflectionEvidence,
) -> list[ResistanceRow]:
    """Constant equivalent response whose spectral mean reproduces G-1.

    G-1 is an irregular-sea expression, not a wavelength-dependent regular-wave
    method. With Hs=4*sqrt(m0) (repository wave_spectra.py) and mean R=2*K*m0
    (ITTC 7.5-02-07-02.2, 2024 rev. 07, eq. 19), K=8*C. Then R_regular=K*a².
    The factor eight follows algebraically; using Hs=2*a would be incorrect.
    transfer=R_regular/(rho*g*a²*B²/Lpp), as specified for this screening table.
    Every row inherits applicability of the supplied sea-state evidence. An
    EVALUATED row does not establish a frequency-resolved reflection response.
    No interpolation outside the supplied short-wave regime is justified.
    """
    reason = applicability(ship, point, evidence)
    coefficient = 8 * reflection_coefficient(ship)
    scale = ship.density_kg_m3 * ship.gravity_m_s2 * ship.beam_m**2 / ship.length_pp_m
    rows = []
    for wavelength, amplitude in zip(waves.wavelength_m, waves.amplitude_m):
        omega, encounter = wave_frequencies(ship, point, wavelength)
        rows.append(
            ResistanceRow(
                wavelength,
                amplitude,
                omega,
                encounter,
                Status.NOT_EVALUATED if reason else Status.EVALUATED,
                None if reason else coefficient * amplitude**2,
                None if reason else coefficient / scale,
                reason
                or "Constant spectral equivalent of G-1; not a frequency-resolved response.",
                "STAWAVE-1 spectral equivalent",
            )
        )
    return rows
