"""Shared held-out validation for generated and sweep-built atlases."""
import math
from typing import Any


def _finite_evidence(value):
    """Represent non-finite failed evidence as JSON null, retaining failures."""
    if isinstance(value, dict):
        return {key: _finite_evidence(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_finite_evidence(item) for item in value]
    return None if isinstance(value, float) and not math.isfinite(value) else value


def validation_passes(atlas):
    """A pass flag cannot override missing or contradictory numeric evidence."""
    error = atlas.max_rel_error
    threshold = atlas.validation.get("threshold")
    return (atlas.validation.get("passes") is True
            and isinstance(error, (int, float)) and math.isfinite(error)
            and isinstance(threshold, (int, float)) and math.isfinite(threshold)
            and 0 <= error <= threshold < 1)


def validate_atlas(atlas, fn, tolerance):
    from digitalmodel.parametric.generate import _holdout_points

    # held-out validation -------------------------------------------------
    worst = 0.0
    samples = []
    failures = []
    # local_error_map[cat_key][axis] = [rel_error per interval] (#828)
    local_error_map: dict[str, dict[str, list[float]]] = {}
    for entry in _holdout_points(atlas.axes):
        point = entry["point"]
        predicted = atlas.predict(point)
        actual = fn(point)
        valid = predicted.in_range and math.isfinite(predicted.value) and math.isfinite(actual)
        if not valid:
            failures.append(f"invalid holdout at {point}: {predicted.reason or 'non-finite response'}")
            rel = math.inf
        else:
            delta = abs(predicted.value - actual)
            rel = delta / abs(actual) if actual else (0.0 if delta == 0 else math.inf)
            if not math.isfinite(rel):
                failures.append(f"non-finite residual at {point}")
        worst = max(worst, rel)
        samples.append(
            {"point": point, "predicted": predicted.value, "actual": actual,
             "rel_error": rel, "in_range": predicted.in_range}
        )
        # Each sample bounds every interval it crosses, including seeded lines.
        from digitalmodel.parametric.atlas import _interval_index
        for ax in atlas.continuous_axes:
            per_axis = local_error_map.setdefault(entry["cat_key"], {}).setdefault(
                ax.name, [0.0] * (len(ax.grid) - 1))
            if per_axis:
                index = _interval_index(ax.grid, point[ax.name])
                per_axis[index] = max(per_axis[index], rel)
    worst_samples = sorted(samples, key=lambda s: -s["rel_error"])[:15]
    atlas.validation = _finite_evidence({
        "metric": "max_rel_error",
        "max_rel_error": worst,
        "threshold": tolerance,
        "passes": not failures and worst <= tolerance,
        "failures": failures,
        "n_holdout": len(samples),
        "worst_samples": worst_samples,
        "local_error_map": local_error_map,
    })
    return atlas.validation
