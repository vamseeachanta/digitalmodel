# ABOUTME: Engineering fundamentals — unit conversions, density, buoyancy basics
# ABOUTME: Validated against USNA EN400 Chapter 1 worked examples
"""
Naval architecture engineering fundamentals.

Covers unit conversions, water properties, and basic buoyancy calculations
from USNA EN400 Chapter 1.
"""
from __future__ import annotations

import warnings
from pathlib import Path
from typing import Optional

from digitalmodel.citations.registry import get_en400_reference
from digitalmodel.citations.schema import CitationResolutionError

# One-shot guard so standalone mode warns once, not per call (mirrors the
# DNV-OS-E301 mooring pilot's degradation behavior).
_EN400_STANDALONE_WARNED = False

G = 32.17  # ft/s², gravitational acceleration
GAMMA_SW = 64.0  # lb/ft³, specific weight of salt water
GAMMA_FW = 62.4  # lb/ft³, specific weight of fresh water
LT_TO_LB = 2240.0  # 1 long ton = 2240 lb

# Fresh water density table (slug/ft⁴) by temperature (°F)
_FW_DENSITY_TABLE = {
    50: 1.9400,
    55: 1.9393,
    59: 1.9384,
    60: 1.9383,
    61: 1.9381,
    62: 1.9379,
    63: 1.9377,
    64: 1.9375,
    65: 1.9373,
    70: 1.9362,
    75: 1.9349,
    80: 1.9333,
}


def mass_to_weight(mass_slug: float) -> float:
    """Convert mass in slugs to weight in pounds.

    W = m * g
    """
    return mass_slug * G


def mass_to_weight_cited(
    mass_slug: float, *, repo_root: Optional[Path] = None
) -> dict:
    """`mass_to_weight` with an EN400 provenance sidecar.

    Mirrors the DNV-OS-E301 mooring pilot's opt-in-by-name design: the legacy
    `mass_to_weight` stays unchanged; callers opt into citation emission by name.
    Returns ``{"value", "units", "citations"}``.

    Fail-closed: a configured-but-missing/stale wiki page raises
    CitationResolutionError. Standalone mode (resolver unconfigured) degrades
    gracefully with a one-shot RuntimeWarning and an empty ``citations`` list.
    """
    global _EN400_STANDALONE_WARNED
    weight = mass_to_weight(mass_slug)
    citations: list = []
    try:
        cited = get_en400_reference(
            "Chapter 1 — Fundamentals (W = m·g, g = 32.17 ft/s²)",
            note="mass-to-weight unit conversion",
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
    return {"value": weight, "units": "lb", "citations": citations}


def displaced_volume_to_weight_lt(
    volume_ft3: float, water: str = "saltwater"
) -> float:
    """Convert displaced volume to weight in long tons.

    W(LT) = gamma * V / 2240
    """
    gamma = GAMMA_SW if water == "saltwater" else GAMMA_FW
    return gamma * volume_ft3 / LT_TO_LB


def interpolate_water_density(
    temp_f: float, water: str = "freshwater"
) -> float:
    """Interpolate water density (slug/ft⁴) at a given temperature.

    Linear interpolation between table values.
    """
    if water != "freshwater":
        raise NotImplementedError("Only freshwater density table available")

    temps = sorted(_FW_DENSITY_TABLE.keys())
    if temp_f <= temps[0]:
        return _FW_DENSITY_TABLE[temps[0]]
    if temp_f >= temps[-1]:
        return _FW_DENSITY_TABLE[temps[-1]]

    for i in range(len(temps) - 1):
        if temps[i] <= temp_f <= temps[i + 1]:
            t0, t1 = temps[i], temps[i + 1]
            rho0 = _FW_DENSITY_TABLE[t0]
            rho1 = _FW_DENSITY_TABLE[t1]
            return rho0 + (temp_f - t0) / (t1 - t0) * (rho1 - rho0)

    raise ValueError(f"Temperature {temp_f}°F out of table range")
