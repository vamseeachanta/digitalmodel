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
    df = df[df["status"] == "completed"]

    axis_names = [p.name for p in study.parameters]
    if not axis_names:
        raise ValueError("study has no parameters to build axes from")
    if response not in df.columns:
        raise ValueError(
            f"response {response!r} not in result columns {sorted(df.columns)}"
        )

    expected = 1
    axes: list[Axis] = []
    scales = scales or {}
    for param in study.parameters:
        values = sorted(param.get_values())
        expected *= len(values)
        axes.append(
            Axis(name=param.name, scale=scales.get(param.name, "linear"), grid=values)
        )

    if len(df) != expected:
        raise ValueError(
            f"atlas grid is not full-factorial: {len(df)} completed cases vs "
            f"{expected} expected ({' x '.join(str(len(a.grid)) for a in axes)})"
        )
    if df[response].isna().any():
        raise ValueError(
            f"response {response!r} has missing values; cannot build atlas"
        )

    grid = df[axis_names + [response]].reset_index(drop=True)
    return Atlas(
        basename=basename,
        atlas_id=atlas_id,
        physics=physics,
        response=response,
        axes=axes,
        grid=grid,
        validation={"max_rel_error": 0.0},
        provenance={"source": "parametrics_sweep", "study": study.name},
    )
