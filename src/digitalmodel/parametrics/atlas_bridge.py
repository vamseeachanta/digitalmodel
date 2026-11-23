"""Bridge a parametric sweep into the parametric Atlas (parametrics P2, #971).

Turns a completed ``ParametricResultsSummary`` over a full-factorial
``ParametricStudy`` grid into a ``parametric.Atlas`` — the existing atlas /
interpolation artifact (#794). No new atlas engine: the swept parameters become
the atlas axes and the chosen result field becomes the response, so
``Atlas.predict`` serves instant interpolated answers with the built-in
out-of-range rail.
"""

from __future__ import annotations

from itertools import product
from math import prod
from numbers import Real
from typing import Optional

import numpy as np

from digitalmodel.orcaflex.batch_parametric import (
    ParametricResultsSummary,
    ParametricStudy,
)
from digitalmodel.parametric.atlas import Atlas, Axis


def _require_finite_numeric(values: list, label: str) -> None:
    """Reject coercible strings, booleans and complex values before finiteness."""
    if any(
        isinstance(value, (bool, np.bool_)) or not isinstance(value, Real)
        for value in values
    ):
        raise ValueError(f"{label} must contain real, non-boolean numeric values")
    if not np.isfinite(values).all():
        raise ValueError(f"{label} must contain only finite values")


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
    cases with finite numeric coordinates and responses. Duplicate, missing or
    undeclared nodes raise before constructing an atlas.
    """
    df = summary.to_dataframe()
    axis_names = [p.name for p in study.parameters]
    if not axis_names:
        raise ValueError("study has no parameters to build axes from")
    if len(set(axis_names)) != len(axis_names):
        raise ValueError("study parameters must have unique names")
    if response not in df.columns:
        raise ValueError(
            f"response {response!r} not in result columns {sorted(df.columns)}"
        )
    for column in ["status", *axis_names]:
        if column not in df.columns:
            raise ValueError(f"required result column {column!r} is missing")
    df = df[df["status"] == "completed"]

    axes: list[Axis] = []
    scales = scales or {}
    for param in study.parameters:
        values = param.get_values()
        label = f"axis {param.name!r}"
        if not values:
            raise ValueError(f"{label} must not be empty")
        _require_finite_numeric(values, label)
        if len(set(values)) != len(values):
            raise ValueError(f"{label} knots must be unique")
        axes.append(
            Axis(
                name=param.name,
                scale=scales.get(param.name, "linear"),
                grid=sorted(values),
            )
        )

    for column in [*axis_names, response]:
        _require_finite_numeric(df[column].tolist(), f"column {column!r}")
    actual = list(df[axis_names].itertuples(index=False, name=None))
    if (
        len(actual) != prod(len(axis.grid) for axis in axes)
        or len(actual) != len(set(actual))
        or set(actual) != set(product(*(axis.grid for axis in axes)))
    ):
        raise ValueError(
            "atlas grid is not full-factorial: completed cases must cover "
            "every declared Cartesian node exactly once"
        )

    grid = df[axis_names + [response]].sort_values(axis_names).reset_index(drop=True)
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
