# ABOUTME: Seakeeping module — 6-DOF frequency-domain ship motion analysis
# ABOUTME: Strip theory, RAO estimation, natural periods, motion criteria
"""
Seakeeping calculations for frequency-domain ship motion analysis.

Covers natural periods (heave, pitch, roll), simplified RAO estimation,
motion sickness incidence, and operability criteria.

References:
- PNA Vol III — Motions in Waves
- USNA EN400 Chapter 8 — Seakeeping
- Journée & Massie — Offshore Hydromechanics
"""

import math
import warnings
from pathlib import Path
from typing import Optional

from digitalmodel.citations.registry import get_en400_reference
from digitalmodel.citations.schema import CitationResolutionError

# One-shot guard so standalone mode warns once, not per call (mirrors the
# DNV-OS-E301 mooring pilot's degradation behavior).
_EN400_STANDALONE_WARNED = False

# Standard gravity and seawater density
G = 9.81  # m/s²
RHO_SW = 1025.0  # kg/m³


def natural_roll_period(
    beam_m: float,
    gm_m: float,
    c_roll: float = 0.80,
) -> float:
    """Natural roll period (seconds).

    T_roll = 2 * pi * k_xx / sqrt(g * GM)
    where k_xx ≈ C * B (roll radius of gyration)
    C ≈ 0.35-0.45 for cargo ships, 0.40-0.45 for tankers

    Args:
        beam_m: ship beam in meters
        gm_m: metacentric height GM in meters
        c_roll: roll gyration coefficient (k_xx/B), default 0.80
            Note: c_roll includes the 2*pi factor for simplicity
    """
    if gm_m <= 0:
        raise ValueError("GM must be positive for stable vessel")
    k_xx = 0.40 * beam_m  # typical gyration radius
    return 2 * math.pi * k_xx / math.sqrt(G * gm_m)


def natural_heave_period(
    displacement_tonnes: float,
    waterplane_area_m2: float,
) -> float:
    """Natural heave period (seconds).

    T_heave = 2 * pi * sqrt(m / (rho * g * Awp))
    """
    mass_kg = displacement_tonnes * 1000.0
    stiffness = RHO_SW * G * waterplane_area_m2
    return 2 * math.pi * math.sqrt(mass_kg / stiffness)


def natural_heave_period_cited(
    displacement_tonnes: float,
    waterplane_area_m2: float,
    *,
    repo_root: Optional[Path] = None,
) -> dict:
    """`natural_heave_period` with an EN400 provenance sidecar.

    Mirrors the DNV-OS-E301 mooring pilot's opt-in-by-name design: the legacy
    `natural_heave_period` stays unchanged; callers opt into citation emission
    by name. Returns ``{"value", "units", "citations"}``.

    Fail-closed: a configured-but-missing/stale wiki page raises
    CitationResolutionError. Standalone mode (resolver unconfigured) degrades
    gracefully with a one-shot RuntimeWarning and an empty ``citations`` list.
    """
    global _EN400_STANDALONE_WARNED
    period_s = natural_heave_period(displacement_tonnes, waterplane_area_m2)
    citations: list = []
    try:
        cited = get_en400_reference(
            "Chapter 8 — Seakeeping (natural heave period)",
            note="natural heave period from waterplane stiffness",
            repo_root=repo_root,
        )
        citations = [cited.citation]
    except CitationResolutionError as exc:
        if exc.reason.startswith("resolver_unconfigured"):
            if not _EN400_STANDALONE_WARNED:
                warnings.warn(
                    "digitalmodel standalone mode: EN400 citation unavailable; "
                    "proceeding without a provenance sidecar.",
                    RuntimeWarning,
                    stacklevel=2,
                )
                _EN400_STANDALONE_WARNED = True
            citations = []
        else:
            raise
    return {"value": period_s, "units": "s", "citations": citations}


def natural_pitch_period(
    lwl_m: float,
    gml_m: float,
    displacement_tonnes: float,
) -> float:
    """Natural pitch period (seconds).

    T_pitch = 2 * pi * k_yy / sqrt(g * GML)
    where k_yy ≈ 0.25 * L (pitch radius of gyration)
    """
    if gml_m <= 0:
        raise ValueError("Longitudinal GM must be positive")
    k_yy = 0.25 * lwl_m
    return 2 * math.pi * k_yy / math.sqrt(G * gml_m)


def encounter_frequency(
    wave_freq_rad: float,
    speed_ms: float,
    heading_deg: float,
) -> float:
    """Encounter frequency for ship in waves.

    omega_e = omega - (omega² / g) * V * cos(mu)

    Args:
        wave_freq_rad: wave circular frequency (rad/s)
        speed_ms: ship speed (m/s)
        heading_deg: heading angle (0=following, 180=head seas)
    """
    mu_rad = math.radians(heading_deg)
    return wave_freq_rad - (wave_freq_rad**2 / G) * speed_ms * math.cos(
        mu_rad
    )


def simple_heave_rao(
    wave_freq_rad: float,
    natural_freq_rad: float,
    damping_ratio: float = 0.05,
) -> float:
    """Simplified heave RAO (transfer function magnitude).

    H(omega) = 1 / sqrt((1 - r²)² + (2*zeta*r)²)
    where r = omega / omega_n
    """
    r = wave_freq_rad / natural_freq_rad
    denom = math.sqrt((1 - r**2) ** 2 + (2 * damping_ratio * r) ** 2)
    return 1.0 / denom if denom > 1e-10 else 100.0


def motion_sickness_incidence(
    vertical_accel_rms_g: float,
    exposure_hours: float = 2.0,
) -> float:
    """Motion Sickness Incidence (MSI) percentage.

    Simplified O'Hanlon & McCauley model:
    MSI ≈ min(100, K * a_rms * sqrt(t))
    where K ≈ 120 for bridge location
    """
    k = 120.0
    msi = k * vertical_accel_rms_g * math.sqrt(exposure_hours)
    return min(100.0, msi)


def significant_motion(
    rao_values: list[float],
    wave_spectrum_values: list[float],
    freq_step_rad: float,
) -> float:
    """Significant motion amplitude from spectral analysis.

    s_1/3 = 2 * sqrt(m0)
    where m0 = integral(RAO² * S(omega) * d_omega)
    """
    if len(rao_values) != len(wave_spectrum_values):
        raise ValueError("RAO and spectrum arrays must have same length")

    m0 = sum(
        rao**2 * s_w * freq_step_rad
        for rao, s_w in zip(rao_values, wave_spectrum_values)
    )
    return 2.0 * math.sqrt(m0)
