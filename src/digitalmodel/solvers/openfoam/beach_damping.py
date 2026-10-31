# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Limit directional beach rates before density weighting or matrix assembly.

The policy is s_eff = min(s, c*r), with s = f*b in inverse seconds,
r = 1/dt and a required caller-selected cap 0 < c < 1. Source: issue #2046
(2026), proposed coefficient guard; see docs/domains/openfoam/beach_damping.md.
No wave-profile coefficient or empirical acceptance limit is supplied here.
This policy does not certify stability or absorption of a coupled CFD solve.
"""

from __future__ import annotations

import math
from collections.abc import Sequence
from pathlib import Path
from typing import NamedTuple


class BeachLimiterResult(NamedTuple):
    """Effective zone-ordered rates and cell-count diagnostics.

    Empty zones have no clip fraction or maximum. For decomposed meshes,
    sum counts before dividing; never average partition clip fractions.
    """

    effective_rates: tuple[float, ...]
    clipped_cells: int
    zone_cells: int
    clip_fraction: float | None
    max_effective_fraction: float | None
    time_scheme: str


def _finite(value: float | None, name: str) -> float:
    if value is None or not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    return float(value)


def _cap(value: float) -> float:
    value = _finite(value, "max_damping_fraction")
    # Zero and unity are the policy bounds in issue #2046, not fitted values.
    if not 0 < value < 1:
        raise ValueError("max_damping_fraction must be strictly between 0 and 1")
    return value


def limit_beach_rates(
    rates: Sequence[float],
    *,
    time_scheme: str,
    max_damping_fraction: float,
    r_delta_t: Sequence[float] | None = None,
    delta_t: float | None = None,
) -> BeachLimiterResult:
    """Apply the issue #2046 coefficient policy to zone-ordered cells.

    Args:
        rates: Raw f*b values in inverse seconds, already spatially blended.
        time_scheme: Either ``Euler`` or ``localEuler``; other schemes raise.
        max_damping_fraction: Required dimensionless cap strictly inside (0, 1).
        r_delta_t: For localEuler, zone-ordered reciprocal local steps in
            inverse seconds. Missing or invalid data raises, even if delta_t
            was supplied. Source: OpenFOAM v2312 (2023), localEulerDdt.C,
            localRDeltaT, lines 47-55.
        delta_t: For Euler, positive global step in seconds. LocalEuler ignores
            it; Euler rejects r_delta_t to avoid using a stale registered field.

    Returns:
        Effective rates in inverse seconds, counts and dimensionless ratios.

    Raises:
        ValueError: Inputs violate the rate, timestep or scheme contract.
    """
    cap = _cap(max_damping_fraction)
    raw = tuple(_finite(rate, "rates") for rate in rates)
    if any(rate < 0 for rate in raw):
        raise ValueError("rates must be nonnegative")

    if time_scheme == "localEuler":
        if r_delta_t is None:
            raise ValueError("localEuler requires r_delta_t; global fallback is unsafe")
        reciprocal = tuple(_finite(value, "r_delta_t") for value in r_delta_t)
        if len(reciprocal) != len(raw):
            raise ValueError("r_delta_t and rates must have the same length")
    elif time_scheme == "Euler":
        if r_delta_t is not None:
            raise ValueError("Euler requires delta_t without r_delta_t")
        step = _finite(delta_t, "delta_t")
        if step <= 0:
            raise ValueError("delta_t must be positive")
        inverse_step = _finite(1 / step, "reciprocal delta_t")
        reciprocal = (inverse_step,) * len(raw)
    else:
        raise ValueError("time_scheme must be Euler or localEuler")

    if any(value <= 0 for value in reciprocal):
        raise ValueError("r_delta_t must be positive")
    effective = tuple(min(rate, cap * r) for rate, r in zip(raw, reciprocal))
    clipped = sum(new < old for new, old in zip(effective, raw))
    count = len(raw)
    return BeachLimiterResult(
        effective,
        clipped,
        count,
        clipped / count if count else None,
        max((rate / r for rate, r in zip(effective, reciprocal)), default=None),
        time_scheme,
    )


def render_beach_limiter(*, max_damping_fraction: float) -> str:
    """Render a v2312 codeAddSupRho insertion fragment, not an fvOptions file.

    Supply mesh-sized raw ``beachRate`` in inverse seconds and selected
    ``cells_``. Use returned ``beachEffectiveRate`` for BOTH the implicit
    coefficient and explicit tangential correction, before density weighting.
    See the integration contract and NOT_EVALUATED limits in the documentation.
    """
    cap = _cap(max_damping_fraction)
    template = Path(__file__).parent / "templates/beach/limitBeachRates.H"
    return template.read_text(encoding="utf-8").replace(
        "@MAX_DAMPING_FRACTION@", repr(cap)
    )
