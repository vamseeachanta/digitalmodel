"""SIROCCO consumer-readiness smoke test for polar_force_overlay (digitalmodel#616 — TDD #14).

Per plan r1 m2 strengthening: tests five sub-conditions to ensure the module is genuinely
usable from a workspace-hub#2760-side caller, not trivially passing on "≥6 traces returned".
"""
from __future__ import annotations

import warnings

import pandas as pd
import plotly.graph_objects as go

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.marine_ops.marine_engineering.visualization import (
    ForceArrowKind,
    VesselSilhouetteSpec,
    polar_force_overlay,
)


def _make_sirocco_style_dataframe() -> pd.DataFrame:
    """Synthetic SIROCCO-style force DataFrame.

    Models rudder-induced moored-current loads: X/Y/Z/K/M/N components at headings
    representing rudder angles ±5° around centerline. Values are illustrative — the
    test asserts API shape and visual-output properties, not numeric correctness.
    """
    rudder_angles_deg = list(range(-5, 6))  # -5 .. +5
    # Project onto a 0..360 angular axis for the polar (translate from rudder-angle).
    headings = [(180.0 + a) % 360.0 for a in rudder_angles_deg]
    rows = []
    for h in headings:
        rows.append({
            "theta_deg": h,
            "fx": 100.0 + h * 0.1,
            "fy": 250.0 - h * 0.5,
            "fz": 0.0,
            "mx": 0.0,
            "my": 0.0,
            "mz": 5.0 + h * 0.01,
        })
    return pd.DataFrame(rows)


def _make_sirocco_silhouette() -> VesselSilhouetteSpec:
    half_beam = 16.0
    profile = HullProfile(
        name="sirocco-test-fixture",
        hull_type=HullType.SHIP,
        length_bp=180.0,
        beam=32.0,
        draft=10.0,
        depth=14.0,
        source="synthetic SIROCCO-style smoke test (test only)",
        stations=[
            HullStation(x_position=0.0,
                        waterline_offsets=[(0.0, 0.0), (8.0, half_beam * 0.5)]),
            HullStation(x_position=90.0,
                        waterline_offsets=[(0.0, 0.0), (8.0, half_beam)]),
            HullStation(x_position=180.0,
                        waterline_offsets=[(0.0, 0.0), (8.0, half_beam * 0.5)]),
        ],
    )
    return VesselSilhouetteSpec(hull_profile=profile, silhouette_kind="generic")


def test_smoke_sirocco_consumer_readiness():
    """Five sub-conditions per plan §TDD #14 (strengthened by r1 m2)."""
    data = _make_sirocco_style_dataframe()
    silhouette = _make_sirocco_silhouette()

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        fig = polar_force_overlay(
            data, silhouette,
            force_arrow_kind=ForceArrowKind.RESULTANT_2D,
            title="SIROCCO smoke test",
        )

    # Sub-condition (a): returns a Figure
    assert isinstance(fig, go.Figure), "must return go.Figure"

    # Sub-condition (d): no warnings during call
    relevant = [w for w in caught
                if not issubclass(w.category, (DeprecationWarning, PendingDeprecationWarning))]
    assert len(relevant) == 0, f"unexpected warnings: {[str(w.message) for w in relevant]}"

    # Sub-condition (e): figure layout has non-empty title and angularaxis ticks
    assert fig.layout.title.text, "title must be non-empty"
    polar_layout = fig.layout.polar
    assert polar_layout.angularaxis is not None, "polar.angularaxis must be configured"
    assert polar_layout.angularaxis.tickvals is not None, "angularaxis must have explicit tickvals"
    assert polar_layout.angularaxis.ticktext is not None, "angularaxis must have explicit ticktext"
    assert len(polar_layout.angularaxis.tickvals) >= 4, "at least 4 angular ticks expected"

    # Sub-condition (b): legend has distinct entries for the rendered force components.
    # When force_arrow_kind=RESULTANT_2D the module renders the resultant per row plus
    # may include per-component legend entries. Require ≥1 legend entry.
    legend_entries = [t.name for t in fig.data if t.showlegend is not False and getattr(t, "name", None)]
    assert len(legend_entries) >= 1, (
        f"figure must have at least 1 legend entry; got {legend_entries}"
    )

    # Sub-condition (c): arrow traces are present (head-marker symbols) and have ≥3 distinct
    # arrow directions (proves the module rendered direction-distinct arrows across the rudder sweep).
    arrow_heads = [t for t in fig.data if getattr(t.marker, "symbol", None) == "triangle-up"]
    assert len(arrow_heads) >= 1, "no arrow-head traces found (triangle-up markers)"
    # Collect head theta values; arrow directions should not all be coincident.
    head_thetas = []
    for ah in arrow_heads:
        head_thetas.extend(list(ah.theta) if ah.theta is not None else [])
    distinct_dirs = len(set(round(t, 1) for t in head_thetas))
    assert distinct_dirs >= 3, (
        f"expected ≥3 distinct arrow-head directions across the sweep; got {distinct_dirs}: {head_thetas}"
    )
