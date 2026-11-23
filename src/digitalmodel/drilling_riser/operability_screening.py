"""Drilling-riser operability screening (#1283, epic #1279 child E).

Serves the pre-computed ``drilling_riser_operability`` atlas as a live go/no-go
screen, mirroring :mod:`digitalmodel.mooring_resilience.screening`. The atlas is
an EXACT node-cache; interpolation is used only for the in-range rail, and any
verdict that is *marginal* (the governing utilisation's confidence band straddles
1.0, from the per-interval local interpolation error) ESCALATES to the exact
analytical recompute rather than trusting an interpolated boundary call. An
out-of-range query escalates, never clamps.

> Screening tool only. The exact ``compute_operating_envelope`` run remains the
> document of record; the dynamic tier (drift-off / VIV) is separate.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

BASENAME = "drilling_riser_operability"

OPERABLE = "OPERABLE"
INOPERABLE = "INOPERABLE"
ESCALATE = "ESCALATE"

_DISCLAIMER = (
    "Static analytical operability screen from a pre-computed atlas — not a "
    "certified deliverable. Marginal / out-of-range points escalate to the exact "
    "run. Dynamic amplification, drift-off and VIV are the solver tier."
)


@dataclass(frozen=True)
class OperabilityScreen:
    config: str
    offset_pct: float
    current_speed_mps: float
    governing_utilisation: Optional[float]  # None when out of range
    in_range: bool
    light: str          # OPERABLE | INOPERABLE | ESCALATE
    reason: str
    provenance: dict


def _atlas(atlas_root) -> Atlas:
    root = Path(atlas_root) if atlas_root is not None else DEFAULT_ATLAS_ROOT
    return Atlas.load(root, BASENAME)


def screen_operability(
    config: str,
    offset_pct: float,
    current_speed_mps: float,
    *,
    atlas_root=None,
) -> OperabilityScreen:
    """Screen one live ``(config, offset_pct, current_speed_mps)`` point."""
    atlas = _atlas(atlas_root)
    provenance = {
        "atlas_id": atlas.atlas_id,
        "standards": atlas.provenance.get("standards"),
        "tier": atlas.provenance.get("tier"),
        "disclaimer": _DISCLAIMER,
    }
    point = {"config": config, "offset_pct": offset_pct, "current_speed_mps": current_speed_mps}
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return OperabilityScreen(
            config, offset_pct, current_speed_mps, None, False, ESCALATE,
            prediction.reason, provenance,
        )
    uc = prediction.value
    eps = atlas.local_error(point)
    lo, hi = uc * (1 - eps), uc * (1 + eps)
    if lo <= 1.0 <= hi:
        light, reason = ESCALATE, (
            f"governing utilisation {uc:.3f} band [{lo:.3f}, {hi:.3f}] straddles 1.0 "
            f"— recompute exact"
        )
    elif uc <= 1.0:
        light, reason = OPERABLE, f"governing utilisation {uc:.3f} <= 1.0"
    else:
        light, reason = INOPERABLE, f"governing utilisation {uc:.3f} > 1.0"
    return OperabilityScreen(
        config, offset_pct, current_speed_mps, uc, True, light, reason, provenance,
    )
