"""Atlas generation: sweep a workflow's parameter grid, fit the surrogate,
validate against held-out interior points, and stamp provenance.

Decision 1 of the spec. For the pilot the response is computed by the workflow's
own math (``get_sn_curve`` + ``miner_damage``), so the atlas is identical to a
live ``mooring_fatigue`` run rather than a re-derivation.
"""

from __future__ import annotations

import hashlib
import itertools
import json
from pathlib import Path
from typing import Any, Callable

import pandas as pd

from digitalmodel.parametric.atlas import Atlas, Axis

# response functions keyed by workflow basename -----------------------------


def _mooring_fatigue_damage(point: dict[str, Any], environment: str) -> float:
    """Miner damage for a single (tension_range, n_cycles) cell — exactly the
    per-bin quantity the mooring_fatigue workflow sums."""
    from digitalmodel.fatigue.damage import miner_damage
    from digitalmodel.fatigue.sn_curves import get_sn_curve

    sn_curve = get_sn_curve(str(point["sn_curve"]), environment)
    stress_range = float(point["tension_range_kN"]) * 1000.0 / float(point["area_mm2"])
    df = miner_damage(
        pd.DataFrame([{"stress_range": stress_range, "cycles": float(point["n_cycles"])}]),
        sn_curve,
    )
    return float(df.attrs["total_damage"])


RESPONSE_FUNCS: dict[str, Callable[[dict[str, Any], str], float]] = {
    "mooring_fatigue": _mooring_fatigue_damage,
}


# grid construction ----------------------------------------------------------


def _grid_points(axes: list[Axis]) -> list[dict[str, Any]]:
    names = [ax.name for ax in axes]
    value_lists = [ax.values if ax.is_categorical else ax.grid for ax in axes]
    return [dict(zip(names, combo)) for combo in itertools.product(*value_lists)]


def _holdout_points(axes: list[Axis]) -> list[dict[str, Any]]:
    """Interior test points: geometric/arithmetic midpoints between every pair
    of adjacent knots on each continuous axis, at the first value of the other
    axes. Catches grid-too-coarse error the publish gate enforces against."""
    cont = [ax for ax in axes if not ax.is_categorical]
    cats = [ax for ax in axes if ax.is_categorical]
    base = {ax.name: ax.grid[len(ax.grid) // 2] for ax in cont}
    points: list[dict[str, Any]] = []
    cat_combos = list(
        itertools.product(*[ax.values for ax in cats])
    ) or [()]
    for cat_combo in cat_combos:
        cat_part = {ax.name: v for ax, v in zip(cats, cat_combo)}
        for ax in cont:
            for a, b in zip(ax.grid, ax.grid[1:]):
                mid = (a * b) ** 0.5 if ax.scale == "log" else (a + b) / 2.0
                pt = {**base, **cat_part, ax.name: mid}
                points.append(pt)
    return points


def _atlas_id(spec: dict[str, Any], provenance: dict[str, Any]) -> str:
    blob = json.dumps(
        {"spec": spec, "code_version": provenance.get("code_version"),
         "standards": provenance.get("standards")},
        sort_keys=True,
    )
    return hashlib.sha256(blob.encode()).hexdigest()[:12]


def generate_atlas(
    basename: str,
    physics: str,
    response: str,
    axes: list[Axis],
    *,
    response_kwargs: dict[str, Any] | None = None,
    tolerance: float = 0.10,
    code_version: str = "unknown",
    standards: list[dict[str, str]] | None = None,
    input_template: Path | None = None,
) -> Atlas:
    response_kwargs = response_kwargs or {}
    fn = RESPONSE_FUNCS[basename]

    rows = []
    for point in _grid_points(axes):
        rows.append({**point, response: fn(point, **response_kwargs)})
    grid = pd.DataFrame(rows)

    provenance = {
        "basename": basename,
        "code_version": code_version,
        "standards": standards or [],
        "response_kwargs": response_kwargs,
    }
    if input_template is not None and Path(input_template).exists():
        provenance["input_template_sha256"] = hashlib.sha256(
            Path(input_template).read_bytes()
        ).hexdigest()

    spec = {
        "physics": physics,
        "response": response,
        "axes": [ax.to_dict() for ax in axes],
    }
    atlas_id = _atlas_id(spec, provenance)
    atlas = Atlas(
        basename=basename,
        atlas_id=atlas_id,
        physics=physics,
        response=response,
        axes=axes,
        grid=grid,
        provenance=provenance,
    )

    # held-out validation -------------------------------------------------
    worst = 0.0
    samples = []
    for point in _holdout_points(axes):
        predicted = atlas.predict(point)
        actual = fn(point, **response_kwargs)
        rel = abs(predicted.value - actual) / actual if actual else 0.0
        worst = max(worst, rel)
        samples.append(
            {"point": point, "predicted": predicted.value, "actual": actual,
             "rel_error": rel, "in_range": predicted.in_range}
        )
    worst_samples = sorted(samples, key=lambda s: -s["rel_error"])[:15]
    atlas.validation = {
        "metric": "max_rel_error",
        "max_rel_error": worst,
        "threshold": tolerance,
        "passes": worst <= tolerance,
        "n_holdout": len(samples),
        "worst_samples": worst_samples,
    }
    return atlas
