"""Thin workflow runners for pipeline integrity registry entries."""
from __future__ import annotations

from dataclasses import asdict


def run_on_bottom_stability(cfg: dict) -> dict:
    """Reject the legacy route: instantaneous loads do not qualify peaks.

    The shipped input has no qualified extreme-cycle load envelope or
    explicit factor-selection basis. Direct peak-check APIs require those
    inputs; an instantaneous sample cannot establish them.
    """
    raise ValueError(
        "on_bottom_stability workflow is unqualified for extreme-cycle peak "
        "checks; supply qualified peak loads and an explicitly selected "
        "safety factor to the direct peak-check APIs"
    )


def run_free_span(cfg: dict) -> dict:
    """Run the existing DNV-RP-F105 free-span VIV fatigue facade."""
    from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
    from digitalmodel.subsea.pipeline.free_span.models import (
        BoundaryConditionF105,
        EnvironmentType,
        PipeSpanInput,
    )

    free_span = cfg["free_span"]
    pipe_span = dict(free_span["pipe_span"])
    pipe_span["bc"] = BoundaryConditionF105(pipe_span["bc"])
    pipe_span["environment"] = EnvironmentType(pipe_span["environment"])
    inp = PipeSpanInput(**pipe_span)
    result = FreespanVIVFatigue(
        inp,
        submerged_weight_N_m=free_span.get("submerged_weight_N_m", 850.0),
        alpha=free_span.get("alpha", 1.0),
        KC=free_span.get("KC", 30.0),
    ).assess()

    free_span["result"] = asdict(result)
    return cfg


def run_lateral_buckling(cfg: dict) -> dict:
    """Route lateral buckling through the existing pipeline router."""
    from digitalmodel.subsea.pipeline.pipeline import Pipeline

    cfg.setdefault("calculation", {})["name"] = "lateral_buckling"
    return Pipeline().router(cfg)


def run_upheaval_buckling(cfg: dict) -> dict:
    """Route upheaval buckling through the existing pipeline router."""
    from digitalmodel.subsea.pipeline.pipeline import Pipeline

    cfg.setdefault("calculation", {})["name"] = "upheaval_buckling"
    return Pipeline().router(cfg)
