#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation #1171 — inline wave force on a surface-piercing
vertical circular cylinder, validated against the MacCamy-Fuchs (1954) linear
diffraction closed form (and its small-ka Morison inertia limit).

The first true wave-*loading* validation of the suite (vs the propagation-only
#1170 numerical wave tank): a rigid, bottom-mounted, surface-piercing cylinder
in a regular wave, with the inline horizontal force measured by an OpenFOAM
``forces`` function object and compared to the exact diffraction force.

Reference (frozen, pure-Python)
-------------------------------
MacCamy, R.C. & Fuchs, R.A. (1954), "Wave Forces on Piles: A Diffraction
Theory", U.S. Army Corps of Engineers, Beach Erosion Board, TM-69. The total
inline force amplitude on a cylinder of radius ``a`` in a linear wave of height
``H``, wavenumber ``k`` (from ``omega^2 = g k tanh(kh)``), depth ``h`` is

    F0 = (2 rho g H / k^2) tanh(k h) A(ka),   A(ka) = 1 / |H1'(ka)|
       = 1 / sqrt( J1'(ka)^2 + Y1'(ka)^2 )

with the force leading the incident wave crest / free-surface velocity by

    gamma = 90 deg - delta,   delta = atan2( J1'(ka), Y1'(ka) ).

``delta`` is the diffraction *departure* from the ideal inertial 90 deg lead
(delta -> 0 as ka -> 0); the physically gated quantity is ``gamma`` (the force
lead over the velocity), which -> 90 deg in the long-wave inertia limit. In that
limit the force collapses onto the Morison inertia term with Cm = 2 (added-mass
coefficient Ca = 1 for a circle); the effective inertia coefficient is
``Cm_eff = 4 A(ka) / (pi ka^2)`` -> 2 as ka -> 0.

This module ships the frozen analytical reference (``scipy.special`` derivative
Bessel functions) plus the case config; the OpenFOAM case builder, force
extraction and known-answer gate live alongside (see ``build_maccamy_fuchs_case``
and ``analyze_cylinder_loading``).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any, Dict, Tuple

from .wave_tank import GRAVITY, dispersion_wavenumber

RHO_WATER = 1000.0

# Known-answer gate tolerances (issue #1171).
MACCAMY_FUCHS_FORCE_TOLERANCE = 0.15   # |F_cfd - F_mf| / F_mf, peak inline force
PHASE_TOLERANCE_DEG = 15.0             # force lead over velocity vs gamma
LOADING_PERIOD_TOLERANCE = 0.05        # force period vs wave period
FORCE_DRIFT_TOLERANCE = 0.10           # mean/amplitude drift over the window
MORISON_CM = 2.0                       # inertia coefficient, circle (Ca = 1)

# Diffraction-regime threshold: below this ka the Morison inertia limit is the
# right reference; above it, diffraction (MacCamy-Fuchs) matters. D/L > ~0.2.
DIFFRACTION_KA = 0.5


def _bessel_derivs(ka: float) -> Tuple[float, float]:
    """(J1'(ka), Y1'(ka)) — first-order derivative Bessel functions."""
    from scipy.special import jvp, yvp
    return float(jvp(1, ka)), float(yvp(1, ka))


def maccamy_fuchs_A(ka: float) -> float:
    """Diffraction amplitude factor A(ka) = 1 / sqrt(J1'(ka)^2 + Y1'(ka)^2)."""
    j1p, y1p = _bessel_derivs(ka)
    return 1.0 / math.hypot(j1p, y1p)


def maccamy_fuchs_force(wave_height: float, wave_period: float,
                        depth: float, diameter: float,
                        rho: float = RHO_WATER,
                        g: float = GRAVITY) -> Dict[str, float]:
    """Total inline force amplitude + phase on a surface-piercing cylinder.

    Args:
        wave_height: Regular wave height H (m).
        wave_period: Wave period T (s).
        depth: Still-water depth h (m).
        diameter: Cylinder diameter D (m).

    Returns:
        Dict with ``F_amplitude`` (N, peak inline force), ``phase_delta_deg``
        (diffraction departure from 90 deg), ``phase_lead_velocity_deg``
        (``gamma`` — the gated force lead over velocity), ``Cm_eff``, and the
        wave parameters ``k``, ``ka``, ``wavelength``, ``kd``, ``tanh_kd``.
    """
    k = dispersion_wavenumber(wave_period, depth)
    a = diameter / 2.0
    ka = k * a
    j1p, y1p = _bessel_derivs(ka)
    A = 1.0 / math.hypot(j1p, y1p)
    F0 = (2.0 * rho * g * wave_height / k ** 2) * math.tanh(k * depth) * A
    delta = math.degrees(math.atan2(j1p, y1p))
    return {
        "F_amplitude": F0,
        "phase_delta_deg": delta,
        "phase_lead_velocity_deg": 90.0 - delta,
        "Cm_eff": 4.0 * A / (math.pi * ka ** 2),
        "k": k,
        "ka": ka,
        "wavelength": 2.0 * math.pi / k,
        "kd": k * depth,
        "tanh_kd": math.tanh(k * depth),
    }


def morison_inertia_force(wave_height: float, wave_period: float,
                          depth: float, diameter: float,
                          Cm: float = MORISON_CM,
                          rho: float = RHO_WATER,
                          g: float = GRAVITY) -> float:
    """Small-ka Morison inertia force amplitude (the ka -> 0 anchor).

    F = Cm rho (pi a^2) g (H/2) tanh(k h) — the depth-integrated inertia force
    on a surface-piercing cylinder in a linear wave (the MacCamy-Fuchs force
    converges onto this as ka -> 0).
    """
    k = dispersion_wavenumber(wave_period, depth)
    a = diameter / 2.0
    return Cm * rho * math.pi * a ** 2 * g * (wave_height / 2.0) * math.tanh(k * depth)


# ---------------------------------------------------------------------------
# Case configuration
# ---------------------------------------------------------------------------


@dataclass
class CylinderWaveLoadingConfig:
    """Config for the vertical-cylinder wave-loading case (#1171).

    A surface-piercing rigid cylinder of diameter ``diameter`` at ``cylinder_x``
    in the verified #1170 StokesII wave, on a **3D half-domain** (a symmetry
    plane at y = 0 through the cylinder axis; the far y-wall a slip wall well
    clear of the cylinder). The waterline ``depth`` must land on a background
    cell face (VOF lesson from #1165/#1302), so ``nz`` divides ``depth`` into
    ``tank_height``.

    Headline diffraction point (defaults): D = 0.30 m, T = 0.80 s, H = 0.03 m,
    depth 0.40 m -> ka = 0.955, D/L = 0.30, MacCamy-Fuchs F0 = 14.74 N, which is
    0.72x the Morison inertia force (a 28% diffraction reduction — so a Morison
    prediction genuinely fails the 15% gate and the case discriminates
    diffraction physics).
    """

    wave_height: float = 0.03
    wave_period: float = 0.80
    depth: float = 0.40
    diameter: float = 0.30
    tank_length: float = 10.0
    tank_height: float = 0.60
    y_half_width: float = 1.6           # far slip wall > 5 D from the axis
    cylinder_x: float = 6.0             # established region, clear of inlet ramp
    nx: int = 400
    ny: int = 64
    nz: int = 60                        # depth 0.40 on a face: 0.40 / (0.60/60) = 40
    ramp_time: float = 2.0
    gauges_x: Tuple[float, ...] = (2.0, 3.6, 4.0, 4.4, 4.8, 5.2, 8.0)
    end_time: float = 12.0
    write_interval: float = 0.5
    steady_window: Tuple[float, float] = (8.0, 12.0)
    name: str = "validation_maccamy_fuchs"

    @property
    def wavenumber(self) -> float:
        return dispersion_wavenumber(self.wave_period, self.depth)

    @property
    def wavelength(self) -> float:
        return 2.0 * math.pi / self.wavenumber

    @property
    def radius(self) -> float:
        return self.diameter / 2.0

    @property
    def ka(self) -> float:
        return self.wavenumber * self.radius

    @property
    def diameter_over_wavelength(self) -> float:
        return self.diameter / self.wavelength

    @property
    def reference(self) -> Dict[str, float]:
        """Frozen MacCamy-Fuchs + Morison targets for this config."""
        mf = maccamy_fuchs_force(self.wave_height, self.wave_period,
                                 self.depth, self.diameter)
        mf["morison_force"] = morison_inertia_force(
            self.wave_height, self.wave_period, self.depth, self.diameter)
        mf["mf_over_morison"] = mf["F_amplitude"] / mf["morison_force"]
        return mf


def provenance() -> Dict[str, Any]:
    """Citation + frozen headline targets for the case (report/registry)."""
    cfg = CylinderWaveLoadingConfig()
    ref = cfg.reference
    return {
        "issue": "#1171",
        "reference": "MacCamy, R.C. & Fuchs, R.A. (1954), 'Wave Forces on Piles: "
                     "A Diffraction Theory', U.S. Army Corps of Engineers, Beach "
                     "Erosion Board, Technical Memorandum No. 69",
        "supporting": ["Sarpkaya & Isaacson (1981); Chakrabarti (1987); "
                       "Morison et al. (1950) inertia+drag; Faltinsen (1990)"],
        "wave": {"H": cfg.wave_height, "T": cfg.wave_period, "depth": cfg.depth},
        "cylinder": {"D": cfg.diameter, "x": cfg.cylinder_x},
        "regime": {"ka": ref["ka"], "D_over_L": cfg.diameter_over_wavelength,
                   "diffraction": ref["ka"] > DIFFRACTION_KA},
        "targets": {"F_maccamy_fuchs_N": ref["F_amplitude"],
                    "F_morison_N": ref["morison_force"],
                    "mf_over_morison": ref["mf_over_morison"],
                    "force_lead_velocity_deg": ref["phase_lead_velocity_deg"]},
    }
