#!/usr/bin/env python3
"""Plotly helpers for the standard report engine — CDN-free, byte-stable.

ABOUTME: Renders one Plotly figure to a ``<div>`` with sorted-key JSON so a
re-render is byte-identical, and exposes the inline plotly.js bundle for the
engine to embed once per document. Also builds simple line/bar figure dicts
from plain columns so domain adapters never import plotly themselves.
"""

from __future__ import annotations

import json
from typing import Any, Literal, Mapping, Sequence

from plotly.utils import PlotlyJSONEncoder  # type: ignore[import-untyped]

#: Default figure height used by the engine's print CSS (``.js-plotly-plot``).
FIGURE_HEIGHT_PX = 420


def canonical_figure_dict(fig_or_dict: Any) -> dict[str, Any]:
    """Validate a figure (dict or ``go.Figure``) and return a sorted-key dict.

    Validation goes through :class:`plotly.graph_objects.Figure`, which also
    applies the default template so the rendered look matches ``fig.show()``.
    The round-trip through :class:`PlotlyJSONEncoder` with ``sort_keys`` makes
    key order independent of construction order.
    """
    import plotly.graph_objects as go  # type: ignore[import-untyped]

    figure = fig_or_dict if isinstance(fig_or_dict, go.Figure) else go.Figure(
        fig_or_dict
    )
    payload = json.dumps(figure.to_dict(), cls=PlotlyJSONEncoder, sort_keys=True)
    result: dict[str, Any] = json.loads(payload)
    return result


def plotly_div(fig_or_dict: Any, figure_id: str) -> str:
    """Render a figure as an HTML fragment that expects ``Plotly`` on ``window``.

    Uses ``plotly.io.to_html(full_html=False, include_plotlyjs=False,
    div_id=figure_id)`` on the canonicalised dict, so the output carries no
    random div id and no script src; the engine embeds plotly.js once.
    """
    import plotly.io as pio  # type: ignore[import-untyped]

    fragment: str = pio.to_html(
        canonical_figure_dict(fig_or_dict),
        full_html=False,
        include_plotlyjs=False,
        div_id=figure_id,
        default_height=f"{FIGURE_HEIGHT_PX}px",
        validate=False,
    )
    return fragment


def inline_plotlyjs() -> str:
    """The full plotly.js bundle (~3.6 MB) for a single inline ``<script>``."""
    import plotly.offline  # type: ignore[import-untyped]

    bundle: str = plotly.offline.get_plotlyjs()
    return bundle


def figure_from_columns(
    kind: Literal["line", "bar"],
    x: Sequence[Any],
    series: Mapping[str, Sequence[Any]],
    *,
    title: str = "",
    x_label: str = "",
    y_label: str = "",
) -> dict[str, Any]:
    """Build a plain figure dict (line or bar) from an x column and y series.

    Callers pass columns and labels; no plotly import is needed on their side.
    Every series must have as many values as ``x``.
    """
    if kind not in ("line", "bar"):
        raise ValueError(f"kind must be 'line' or 'bar', got {kind!r}")
    if not series:
        raise ValueError("series must contain at least one entry")
    data: list[dict[str, Any]] = []
    for name, values in series.items():
        if len(values) != len(x):
            raise ValueError(
                f"series {name!r} has {len(values)} values for {len(x)} x values"
            )
        trace: dict[str, Any] = {
            "type": "scatter" if kind == "line" else "bar",
            "name": str(name),
            "x": list(x),
            "y": list(values),
        }
        if kind == "line":
            trace["mode"] = "lines+markers"
        data.append(trace)
    layout: dict[str, Any] = {
        "title": {"text": title},
        "xaxis": {"title": {"text": x_label}},
        "yaxis": {"title": {"text": y_label}},
        "margin": {"l": 60, "r": 20, "t": 50, "b": 50},
        "hovermode": "x unified",
        "autosize": True,
    }
    if kind == "bar":
        layout["barmode"] = "group"
    return {"data": data, "layout": layout}


__all__ = [
    "FIGURE_HEIGHT_PX",
    "canonical_figure_dict",
    "figure_from_columns",
    "inline_plotlyjs",
    "plotly_div",
]
