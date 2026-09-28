"""Event inputs of the drilling-riser campaign: the drift-off / drive-off vessel trajectory after a DP loss and the
anti-recoil tension schedule after an EDS disconnect (API RP 16Q / ISO 13624-1 event analyses).

**Drift-off / drive-off.** The vessel (fixed heading along global x) moves in surge and sway under the mean
environmental loads, all acting along the environment direction ``heading_deg`` (direction of travel, the OrcaFlex
convention; wind, waves and current collinear, ASSUMED):

* wind - ``0.5 rho_air U^2 C A`` on the frontal (x) and lateral (y) projected areas, weighted by cos / sin of the
  direction;
* current - quadratic drag on the relative velocity, ``0.5 rho_w C |u_r| u_r`` on B x T (x) and L x T (y), which also
  limits the drift speed;
* mean wave drift - the short-wave reflection limit ``rho_w g Hs^2 / 16`` per metre of reflecting length (half the
  beam in surge, the length in sway), reduced for long waves by ``R = 1 / (1 + (lambda_p / 2B)^2)`` (lambda_p the
  deep-water wavelength at Tp). Class-typical and ASSUMED (owner decision W326) until the diffraction coefficients
  arrive;
* thrust - zero after a drift-off; a drive-off thrust ramped linearly to ``thrust_n`` over ``thrust_ramp_s`` along the
  environment direction (the adverse case).

Masses: displacement x (1 + added mass) per direction. The riser's own restoring force on the vessel is neglected
(two orders below the vessel inertia and environment loads over the event). The trajectory is integrated
(semi-implicit Euler) and then prescribed to the OrcaFlex vessel, with the first-order RAO motion superimposed.

**Anti-recoil.** After the LMRP unlatches, the tensioner anti-recoil valves isolate the accumulators: the tension
falls linearly from the pre-disconnect value to the hold tension over the closure time and is then held (the
closure curve as piecewise-constant steps at the step midpoints, one OrcaFlex stage per step). With a failed valve
(``open_fraction`` of the tensioners, D-94) that share keeps the pre-disconnect tension.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any

RHO_AIR = 1.225
RHO_W = 1025.0
G = 9.80665


@dataclass(frozen=True)
class DriftVessel:
    displacement_t: float
    added_mass_surge: float
    added_mass_sway: float
    length_m: float
    beam_m: float
    draft_m: float
    wind_cx: float
    wind_area_x_m2: float
    wind_cy: float
    wind_area_y_m2: float
    current_cx: float
    current_cy: float


@dataclass(frozen=True)
class DriftEnvironment:
    heading_deg: float
    wind_u10_m_s: float = 0.0
    current_m_s: float = 0.0
    hs_m: float = 0.0
    tp_s: float = 0.0


def wave_drift_force_n(v: DriftVessel, *, hs_m: float, tp_s: float, heading_deg: float) -> tuple[float, float]:
    """Mean wave-drift force (x, y), N, in irregular waves (class-typical, ASSUMED)."""
    if hs_m <= 0 or tp_s <= 0:
        return 0.0, 0.0
    lam = G * tp_s ** 2 / (2 * math.pi)
    r = 1.0 / (1.0 + (lam / (2.0 * v.beam_m)) ** 2)
    q = RHO_W * G * hs_m ** 2 / 16.0 * r
    c, s = math.cos(math.radians(heading_deg)), math.sin(math.radians(heading_deg))
    return q * 0.5 * v.beam_m * c, q * v.length_m * s


def drift_trajectory(v: DriftVessel, env: DriftEnvironment, *, duration_s: float, dt_s: float = 0.05,
                     thrust_n: float = 0.0, thrust_ramp_s: float = 15.0, initial_speed_m_s: float = 0.0) -> dict[str, Any]:
    """Vessel position and velocity (global x, y from the start position) after the DP loss at t = 0."""
    c, s = math.cos(math.radians(env.heading_deg)), math.sin(math.radians(env.heading_deg))
    mx = v.displacement_t * 1000.0 * (1.0 + v.added_mass_surge)
    my = v.displacement_t * 1000.0 * (1.0 + v.added_mass_sway)
    q_air = 0.5 * RHO_AIR * env.wind_u10_m_s ** 2
    fwx, fwy = q_air * v.wind_cx * v.wind_area_x_m2 * c, q_air * v.wind_cy * v.wind_area_y_m2 * s
    fdx, fdy = wave_drift_force_n(v, hs_m=env.hs_m, tp_s=env.tp_s, heading_deg=env.heading_deg)
    ax_c, ay_c = 0.5 * RHO_W * v.current_cx * v.beam_m * v.draft_m, 0.5 * RHO_W * v.current_cy * v.length_m * v.draft_m
    ucx, ucy = env.current_m_s * c, env.current_m_s * s
    x = y = 0.0
    vx, vy = initial_speed_m_s * c, initial_speed_m_s * s
    n = int(round(duration_s / dt_s))
    out = {"t": [0.0], "x": [0.0], "y": [0.0], "vx": [vx], "vy": [vy]}
    for i in range(1, n + 1):
        t = (i - 1) * dt_s
        # thrust at the step midpoint (exact for the linear ramp in the closed-form check)
        th = thrust_n * min(1.0, (t + 0.5 * dt_s) / thrust_ramp_s) if thrust_n else 0.0
        rx, ry = ucx - vx, ucy - vy
        fx = fwx + fdx + ax_c * abs(rx) * rx + th * c
        fy = fwy + fdy + ay_c * abs(ry) * ry + th * s
        vx0, vy0 = vx, vy
        vx += fx / mx * dt_s
        vy += fy / my * dt_s
        x += 0.5 * (vx0 + vx) * dt_s
        y += 0.5 * (vy0 + vy) * dt_s
        out["t"].append(round(i * dt_s, 9))
        out["x"].append(x)
        out["y"].append(y)
        out["vx"].append(vx)
        out["vy"].append(vy)
    out["forces_n"] = {"wind": (fwx, fwy), "wave_drift": (fdx, fdy), "thrust_max": thrust_n}
    return out


def anti_recoil_stages(*, t0_n: float, hold_n: float, closure_s: float = 2.5, step_s: float = 0.5,
                       open_fraction: float = 0.0) -> list[dict[str, Any]]:
    """Tensioner total tension per stage after the disconnect: the linear closure from ``t0_n`` to ``hold_n`` over
    ``closure_s`` as steps of ``step_s`` (midpoint values), then held (last stage, open-ended); ``open_fraction`` of
    the tensioners keeps ``t0_n`` (failed anti-recoil valve)."""
    n = max(1, int(round(closure_s / step_s)))
    out = []
    for k in range(n):
        f = (k + 0.5) / n
        closed = t0_n + (hold_n - t0_n) * f
        out.append({"duration_s": closure_s / n, "tension_n": (1 - open_fraction) * closed + open_fraction * t0_n})
    out.append({"duration_s": None, "tension_n": (1 - open_fraction) * hold_n + open_fraction * t0_n})
    return out
