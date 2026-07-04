"""Core: time-domain 6-DOF motion reconstruction (digitalmodel #1358).

For each phased wave component *i* the incident phase is propagated from the
forecast's ``phase_reference_location`` to the asset location using the
dispersion relation, then transferred through the asset RAO::

    theta_i(t) = omega_i*t + phi_i - k_i * (d_hat_i . (x_asset - x_ref))
                 + arg H_dof(omega_i, heading_i)
    x_dof(t)   = sum_i a_i * |H_dof(omega_i, heading_i)| * cos(theta_i(t))

The ``-k*Delta x`` term is the point of a *forecast*: a wave phased at a sensing
point must be propagated to where the asset actually is. Translations come out
in metres, rotations in degrees (``DisplacementRAO`` units).
"""

from __future__ import annotations

from typing import Dict, Optional, Sequence, Tuple

import numpy as np

from .models import DOF_NAMES, GRAVITY, MotionForecast, WaveForecast


def wavenumber(omega: float, water_depth: Optional[float] = None) -> float:
    """Wavenumber k from the linear dispersion relation.

    Deep water (``water_depth is None``): ``k = omega^2 / g``. Finite depth:
    solve ``omega^2 = g k tanh(k h)`` for k.

    The residual ``f(k) = g k tanh(k h) - omega^2`` is strictly increasing in k,
    so this uses **bisection** (guaranteed convergent, unlike the fixed-point map
    ``k <- omega^2/(g tanh(kh))`` which oscillates in shallow water) with a
    relative tolerance, and raises if it fails to converge.
    """
    if omega <= 0:
        return 0.0
    k_deep = omega * omega / GRAVITY
    if water_depth is None or water_depth <= 0 or not np.isfinite(water_depth):
        return k_deep

    def f(k: float) -> float:
        return GRAVITY * k * np.tanh(k * water_depth) - omega * omega

    # bracket: f(0+) < 0; grow hi until f(hi) > 0 (true k >= k_deep for finite h)
    lo, hi = 0.0, max(k_deep, 1.0 / water_depth)
    for _ in range(200):
        if f(hi) > 0:
            break
        hi *= 2.0
    else:  # pragma: no cover - unreachable for physical inputs
        raise RuntimeError(f"could not bracket wavenumber (omega={omega}, h={water_depth})")

    for _ in range(200):
        mid = 0.5 * (lo + hi)
        fm = f(mid)
        if abs(fm) <= 1e-10 * omega * omega or (hi - lo) <= 1e-12 * mid:
            return float(mid)
        if fm > 0:
            hi = mid
        else:
            lo = mid
    raise RuntimeError(
        f"wavenumber bisection did not converge (omega={omega}, h={water_depth})"
    )


def reconstruct_motion(
    forecast: WaveForecast,
    rao,
    *,
    asset_location: Tuple[float, float] = (0.0, 0.0),
    dt: float = 0.2,
    t_grid: Optional[Sequence[float]] = None,
    dofs: Sequence[str] = DOF_NAMES,
) -> MotionForecast:
    """Reconstruct 6-DOF asset motion over the predictable-zone horizon.

    Parameters
    ----------
    forecast:
        Phased incident-wave forecast (with ``phase_reference_location``).
    rao:
        Object exposing ``H(dof, omega, heading) -> complex`` (see
        :mod:`.rao_adapter`).
    asset_location:
        (x, y) of the asset (m); the wave phase is propagated here from the
        forecast reference location.
    dt:
        Time step (s) for the default grid.
    t_grid:
        Explicit times (s). **Fail-closed**: any time outside
        ``[origin_time, origin_time + horizon]`` raises — no extrapolation
        beyond the predictable zone.

    Notes
    -----
    Component headings and the RAO's heading axis must share the *going-to*
    convention (deg from +x). Convert a source RAO's headings at ingest with
    :func:`.conventions.heading_going_to_deg` if it reports "coming-from".
    """
    if not forecast.components:
        raise ValueError("forecast has no wave components")
    t0 = forecast.origin_time
    t1 = forecast.origin_time + forecast.horizon
    if t_grid is None:
        n = max(2, int(round(forecast.horizon / dt)) + 1)
        t = np.linspace(t0, t1, n)
    else:
        t = np.asarray(t_grid, dtype=float)
        eps = 1e-9
        if t.min() < t0 - eps or t.max() > t1 + eps:
            raise ValueError(
                f"t_grid [{t.min():.3f}, {t.max():.3f}] exceeds predictable "
                f"zone [{t0:.3f}, {t1:.3f}] (fail-closed; no extrapolation)"
            )

    xr, yr = forecast.phase_reference_location
    xa, ya = asset_location
    dx, dy = xa - xr, ya - yr

    out: Dict[str, np.ndarray] = {d: np.zeros_like(t) for d in dofs}

    for comp in forecast.components:
        k = wavenumber(comp.omega, forecast.water_depth)
        brad = np.deg2rad(comp.heading)
        # projection of the reference->asset offset onto the propagation dir
        proj = np.cos(brad) * dx + np.sin(brad) * dy
        spatial_phase = -k * proj
        wave_phase = comp.omega * t + comp.phase + spatial_phase
        for d in dofs:
            H = rao.H(d, comp.omega, comp.heading)
            mag = abs(H)
            if mag == 0.0:
                continue
            out[d] += comp.amplitude * mag * np.cos(wave_phase + np.angle(H))

    return MotionForecast(
        t=t, dof=out, origin_time=t0, horizon=forecast.horizon,
        metadata={
            "asset_location": (xa, ya),
            "phase_reference_location": (xr, yr),
            "water_depth": forecast.water_depth,
            "n_components": len(forecast.components),
        },
    )


def vertical_motion_at(
    motion: MotionForecast, offset: Tuple[float, float, float]
) -> np.ndarray:
    """Vertical displacement (m) at a rigid-body point ``offset=(rx,ry,rz)``.

    Small-angle rigid-body kinematics referenced to the motion origin::

        z(r, t) = heave - rx*pitch + ry*roll     (angles in rad)

    Used for structure-interface points (crane tip, gangway tip, deployment
    point). ``rz`` is accepted for API symmetry (no vertical lever term).
    """
    rx, ry, _rz = offset
    heave = motion.dof["heave"]
    pitch = np.deg2rad(motion.dof["pitch"])
    roll = np.deg2rad(motion.dof["roll"])
    return heave - rx * pitch + ry * roll


def time_derivative(t: np.ndarray, x: np.ndarray) -> np.ndarray:
    """Numerical derivative dx/dt on a (possibly non-uniform) grid."""
    return np.gradient(x, t)
