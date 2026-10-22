"""Bridge a parametric sweep into the parametric Atlas (parametrics P2, #971).

Turns a completed ``ParametricResultsSummary`` over a full-factorial
``ParametricStudy`` grid into a ``parametric.Atlas`` — the existing atlas /
interpolation artifact (#794). No new atlas engine: the swept parameters become
the atlas axes and the chosen result field becomes the response, so
``Atlas.predict`` serves instant interpolated answers with the built-in
out-of-range rail.
"""

from __future__ import annotations

from typing import Optional

import numpy as np

from digitalmodel.orcaflex.batch_parametric import (
    ParametricResultsSummary,
    ParametricStudy,
)
from digitalmodel.parametric.atlas import Atlas, Axis


def atlas_from_summary(
    study: ParametricStudy,
    summary: ParametricResultsSummary,
    response: str,
    *,
    basename: str,
    atlas_id: str,
    physics: str = "linear",
    scales: Optional[dict[str, str]] = None,
) -> Atlas:
    """Build a ``parametric.Atlas`` from a swept study + its results.

    ``response`` is the ``CaseResult`` field to interpolate (e.g.
    ``max_utilisation``). Requires a complete (full-factorial) grid of completed
    cases — raises if cases are missing/failed so the atlas is never built over a
    hole.
    """
    df = summary.to_dataframe()
    if df.empty:
        raise ValueError("atlas grid is not full-factorial: no completed cases")
    df = df[df["status"] == "completed"]

    axis_names = [p.name for p in study.parameters]
    if not axis_names:
        raise ValueError("study has no parameters to build axes from")
    if len(set(axis_names)) != len(axis_names):
        raise ValueError("atlas axis names must be unique")
    if not set(axis_names).issubset(df.columns):
        raise ValueError("atlas grid is not full-factorial: missing axis columns")
    if response not in df.columns:
        raise ValueError(
            f"response {response!r} not in result columns {sorted(df.columns)}"
        )

    expected = 1
    axes: list[Axis] = []
    scales = scales or {}
    for param in study.parameters:
        values = sorted(param.get_values())
        if not values or not np.isfinite(values).all() or len(set(values)) != len(values):
            raise ValueError(f"axis {param.name!r} must have finite unique nonempty values")
        expected *= len(values)
        axes.append(
            Axis(name=param.name, scale=scales.get(param.name, "linear"), grid=values)
        )

    if len(df) != expected:
        raise ValueError(
            f"atlas grid is not full-factorial: {len(df)} completed cases vs "
            f"{expected} expected ({' x '.join(str(len(a.grid)) for a in axes)})"
        )
    # Full-factorial membership (atlas-spec.md Decision 2; issue 2118):
    # N unique tuples in the declared product of cardinality N cover every node.
    if df.duplicated(axis_names).any() or any(
        not df[axis.name].isin(axis.grid).all() for axis in axes
    ):
        raise ValueError("atlas grid is not full-factorial: duplicate or undeclared node")
    # Membership in the finite declared axes already establishes axis finiteness.
    if not np.isfinite(df[response].to_numpy(dtype=float)).all():
        raise ValueError("atlas response must contain only finite values")

    grid = df[axis_names + [response]].reset_index(drop=True)
    return Atlas(
        basename=basename,
        atlas_id=atlas_id,
        physics=physics,
        response=response,
        axes=axes,
        grid=grid,
        validation={"status": "unavailable", "reason": "no holdout evaluation"},
        provenance={"source": "parametrics_sweep", "study": study.name},
    )
