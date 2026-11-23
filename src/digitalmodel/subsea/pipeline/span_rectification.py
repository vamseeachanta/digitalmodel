# ABOUTME: Free-span rectification design — intermediate supports for an over-long span.
# ABOUTME: Wraps the DNV-RP-F105 allowable-span calc and sizes the support layout.
"""Pipeline free-span rectification design (DNV-RP-F105).

Builds on the Level-1 allowable free-span length (`SpanAllowableLength`): when an
as-surveyed span exceeds the allowable length, it must be *rectified* — split
into shorter sub-spans by intermediate supports (grout bags, mattresses, rock
berms) so each sub-span is within the allowable limit.

For an actual span L_a and an allowable span L_allow, the minimum number of
equally spaced intermediate supports is

    n_supports = ceil(L_a / L_allow) - 1

which yields n_supports + 1 equal sub-spans of length L_a / (n_supports + 1),
each <= L_allow. No rectification is needed when L_a <= L_allow.

References: DNV-RP-F105 (free spanning pipelines) Sec. 4.2 (allowable span);
the support layout follows the standard equal-sub-span rectification rule.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any


def design_rectification(
    actual_span_m: float, allowable_span_m: float
) -> dict[str, Any]:
    """Minimum equal-spaced intermediate supports to bring sub-spans within allowable."""
    if actual_span_m <= 0.0:
        raise ValueError("actual_span_m must be positive")
    if allowable_span_m <= 0.0:
        raise ValueError("allowable_span_m must be positive")

    if actual_span_m <= allowable_span_m:
        return {
            "rectification_required": False,
            "n_supports": 0,
            "sub_span_count": 1,
            "resulting_sub_span_m": actual_span_m,
            "support_spacing_m": actual_span_m,
            "span_utilization": actual_span_m / allowable_span_m,
        }

    sub_span_count = math.ceil(actual_span_m / allowable_span_m)
    n_supports = sub_span_count - 1
    resulting_sub_span = actual_span_m / sub_span_count
    return {
        "rectification_required": True,
        "n_supports": n_supports,
        "sub_span_count": sub_span_count,
        "resulting_sub_span_m": resulting_sub_span,
        "support_spacing_m": resulting_sub_span,
        "span_utilization": resulting_sub_span / allowable_span_m,
    }


def router(cfg: dict) -> dict:
    from digitalmodel.subsea.pipeline.free_span.models import (
        BoundaryConditionF105,
        EnvironmentType,
        PipeSpanInput,
    )
    from digitalmodel.subsea.pipeline.free_span.span_allowable_length import (
        SpanAllowableLength,
    )

    settings = cfg.get("span_rectification") or {}
    pipe_span = dict(settings["pipe_span"])
    pipe_span["bc"] = BoundaryConditionF105(pipe_span["bc"])
    pipe_span["environment"] = EnvironmentType(pipe_span["environment"])
    inp = PipeSpanInput(**pipe_span)
    submerged_weight = float(settings.get("submerged_weight_N_m", 850.0))

    allowable_span = SpanAllowableLength(
        inp, submerged_weight_N_m=submerged_weight
    ).max_span_m()
    actual_span = float(settings.get("actual_span_length_m", inp.span_length_m))

    rectification = design_rectification(actual_span, allowable_span)
    # An as-built span exceeding the allowable FAILS the screen; the rectification
    # (intermediate supports) is the mitigation that brings it back into limit.
    status = "fail" if rectification["rectification_required"] else "pass"

    result = {
        "standard": "DNV-RP-F105 Sec 4.2 (allowable span) + equal-sub-span rectification",
        "allowable_span_m": allowable_span,
        "actual_span_m": actual_span,
        **rectification,
        "screening_status": status,
    }
    payload = {**settings, "result": result, "screening_status": status}
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["span_rectification"] = payload
    cfg["screening_status"] = status
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/span_rectification"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "span_rectification_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
