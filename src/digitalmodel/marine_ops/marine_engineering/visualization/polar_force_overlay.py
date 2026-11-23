"""Polar plot with vessel silhouette and on-body force vectors (digitalmodel#616).

Reusable Plotly polar Figure producer for the OCIMF coefficient explorer,
moored-vessel force reviews, and other generic force/moment studies.

Citation gate: arrow-direction values for OCIMF data convention are bound by
OCIMF_CONVENTION_AUTHORITY in _convention.py, which cites OCIMF MEG3 (2008)
Annex A §A1 and MEG4 (2018) Annex A §A2 directly.

Pre-spike: arrow-rendering technique (two-trace Scatterpolar: shaft line +
triangle-up marker with rotation) is documented in
`digitalmodel/docs/spikes/2026-05-20-plotly-polar-arrow-technique/`.
"""
from __future__ import annotations

import math

import pandas as pd
import plotly.graph_objects as go

from ._convention import OCIMF_CONVENTION_AUTHORITY
from .types import (
    ForceArrowKind,
    FrameConvention,
    RadialAxisMode,
    VesselSilhouetteSpec,
)
from .vessel_silhouettes import get_polygon


# Detected schemas
_LONG_FORMAT_REQUIRED = {"theta_deg", "value", "component"}
_WIDE_FORMAT_REQUIRED = {"theta_deg", "fx", "fy", "fz", "mx", "my", "mz"}


# ---------- Pure functions ----------


def _resolve_arrow_direction_in_body_frame(
    theta_incidence_deg: float,
    component_sign: int,
    frame_convention: FrameConvention,
) -> float:
    """Return the data-frame angle (degrees) the arrow should point in.

    For INCIDENCE_HEADING_BODY_FIXED (OCIMF convention): lateral force direction
    is determined purely by the sign of Cy. Independent of θ_incidence because
    lateral force is along the body Y axis by definition. The authoritative
    values come from OCIMF_CONVENTION_AUTHORITY, which cites MEG3 §A1 + MEG4 §A2.

    For FORCE_DIRECTION_INERTIAL: the input theta is already the force-direction
    angle (caller-supplied); pass through unchanged.
    """
    if frame_convention is FrameConvention.INCIDENCE_HEADING_BODY_FIXED:
        if component_sign >= 0:
            return OCIMF_CONVENTION_AUTHORITY.positive_cy_arrow_at_starboard_incidence_deg()
        return OCIMF_CONVENTION_AUTHORITY.negative_cy_arrow_at_starboard_incidence_deg()
    if frame_convention is FrameConvention.FORCE_DIRECTION_INERTIAL:
        return float(theta_incidence_deg)
    raise ValueError(f"unsupported frame_convention: {frame_convention}")


# ---------- Schema validation ----------


def _detect_schema(data: pd.DataFrame) -> str:
    cols = set(data.columns)
    if "theta_deg" not in cols:
        raise ValueError(
            "input DataFrame must contain a 'theta_deg' column; "
            f"got columns: {sorted(cols)}"
        )
    if _WIDE_FORMAT_REQUIRED.issubset(cols):
        return "wide"
    if _LONG_FORMAT_REQUIRED.issubset(cols):
        return "long"
    raise ValueError(
        "input DataFrame schema must be either "
        f"long-format (cols ⊇ {sorted(_LONG_FORMAT_REQUIRED)}) "
        f"or wide-format (cols ⊇ {sorted(_WIDE_FORMAT_REQUIRED)}); "
        f"got cols={sorted(cols)}"
    )


def _validate_theta_range(data: pd.DataFrame) -> None:
    if (data["theta_deg"] < 0).any() or (data["theta_deg"] >= 360).any():
        raise ValueError(
            "theta_deg must be in [0, 360); found out-of-range values"
        )


# ---------- Silhouette rendering ----------


def _add_silhouette_traces(
    fig: go.Figure,
    spec: VesselSilhouetteSpec,
    max_data_r: float,
    silhouette_radial_fraction: float,
) -> None:
    """Add the vessel silhouette as a closed Scatterpolar polygon with transparent fill.

    Per reopen-fix-bug-1: silhouette polygon is RESCALED to fit within
    `silhouette_radial_fraction * max_data_r` so it never dominates the radial
    axis. Without this, silhouette's literal meters (e.g., length_bp/2 = 150m)
    would dwarf dimensionless coefficient data (Cyc 0-3) and compress the
    actual signal to a dot at the origin.
    """
    poly = get_polygon(
        spec.silhouette_kind,
        length_bp_m=spec.hull_profile.length_bp,
        beam_m=spec.hull_profile.beam,
        custom_path=spec.custom_path,
    )
    # Convert (x_body, y_body) -> polar (r, theta_deg) in DATA frame
    # OCIMF convention: +X_body = bow (θ=0), +Y_body = port (θ=90, anti-clockwise positive).
    # For a point at (x, y), r = sqrt(x²+y²), θ = atan2(y, x) in degrees [0, 360).
    raw_rs: list[float] = []
    thetas: list[float] = []
    for x, y in poly:
        r = math.hypot(x, y)
        theta_rad = math.atan2(y, x)
        theta_deg = math.degrees(theta_rad) % 360.0
        raw_rs.append(r)
        thetas.append(theta_deg)

    # Rescale to fit within silhouette_radial_fraction * max_data_r
    raw_max_r = max(raw_rs) if raw_rs else 1.0
    if raw_max_r > 0 and max_data_r > 0:
        target_max_r = silhouette_radial_fraction * max_data_r
        scale = target_max_r / raw_max_r
        rs = [r * scale for r in raw_rs]
    else:
        rs = raw_rs

    fig.add_trace(
        go.Scatterpolar(
            r=rs,
            theta=thetas,
            mode="lines",
            fill="toself",
            fillcolor=f"rgba(136, 136, 136, {spec.opacity})",
            line=dict(color="#888", width=1),
            hoverinfo="skip",
            showlegend=False,
            name=f"silhouette ({spec.silhouette_kind})",
            opacity=spec.opacity,
        )
    )


# ---------- Force-arrow rendering ----------


def _add_force_arrow(
    fig: go.Figure,
    r_origin: float,
    r_tip: float,
    placement_theta_deg: float,
    arrow_direction_deg: float,
    color: str,
) -> None:
    """Add a single force-arrow (two traces: shaft + head).

    Per the pre-spike (docs/spikes/2026-05-20-plotly-polar-arrow-technique/):
    use a Scatterpolar line for the shaft and a Scatterpolar marker with
    triangle-up + angle rotation for the head. Both traces keep native polar
    coords and skip hover/legend.
    """
    # Shaft: line from (r_origin, placement_theta) outward to (r_tip, placement_theta)
    fig.add_trace(
        go.Scatterpolar(
            r=[r_origin, r_tip],
            theta=[placement_theta_deg, placement_theta_deg],
            mode="lines",
            line=dict(color=color, width=2),
            hoverinfo="skip",
            showlegend=False,
        )
    )
    # Head: triangle-up marker at the tip, angle = arrow_direction_deg
    fig.add_trace(
        go.Scatterpolar(
            r=[r_tip],
            theta=[placement_theta_deg],
            mode="markers",
            marker=dict(
                symbol="triangle-up",
                size=12,
                color=color,
                angle=arrow_direction_deg,
            ),
            hoverinfo="skip",
            showlegend=False,
        )
    )


# ---------- Data-trace rendering ----------


def _add_data_traces(
    fig: go.Figure,
    data: pd.DataFrame,
    schema: str,
    radial_axis_mode: RadialAxisMode,
    group_col: str | None,
) -> None:
    """Render the coefficient/force value traces (preserves existing make_polar_overlay shape)."""
    if schema == "long":
        groups = (
            sorted(data[group_col].dropna().unique())
            if group_col and group_col in data.columns
            else [None]
        )
        palette = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#17becf"]
        for i, grp in enumerate(groups):
            sub = data if grp is None else data[data[group_col] == grp]
            sub = sub.sort_values("theta_deg")
            if sub.empty:
                continue
            color = palette[i % len(palette)]
            pos = sub[sub["value"] >= 0]
            neg = sub[sub["value"] < 0]
            label_grp = f"{group_col}={grp}" if grp is not None else "data"
            if not pos.empty:
                fig.add_trace(
                    go.Scatterpolar(
                        r=pos["value"].values,
                        theta=pos["theta_deg"].values,
                        mode="lines+markers",
                        name=f"{label_grp} (+)",
                        line=dict(color=color, width=2),
                        marker=dict(size=6, color=color, symbol="circle"),
                    )
                )
            if not neg.empty:
                fig.add_trace(
                    go.Scatterpolar(
                        r=neg["value"].abs().values,
                        theta=neg["theta_deg"].values,
                        mode="lines+markers",
                        name=f"{label_grp} (−)",
                        line=dict(color=color, width=2, dash="dash"),
                        marker=dict(size=9, color=color, symbol="x-thin",
                                    line=dict(color=color, width=2)),
                    )
                )
    else:  # wide
        # One trace per component (X/Y/Z/K/M/N), magnitude on the radial axis.
        components = [("fx", "X"), ("fy", "Y"), ("fz", "Z"), ("mx", "K"), ("my", "M"), ("mz", "N")]
        palette = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"]
        sub = data.sort_values("theta_deg")
        for i, (col, label) in enumerate(components):
            if (sub[col] == 0).all():
                continue  # skip zero-only components for legend cleanliness
            color = palette[i % len(palette)]
            fig.add_trace(
                go.Scatterpolar(
                    r=sub[col].abs().values,
                    theta=sub["theta_deg"].values,
                    mode="lines+markers",
                    name=label,
                    line=dict(color=color, width=2),
                    marker=dict(size=6, color=color),
                )
            )


# ---------- Public API ----------


def polar_force_overlay(
    data: pd.DataFrame,
    silhouette: VesselSilhouetteSpec,
    frame_convention: FrameConvention = FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    force_arrow_kind: ForceArrowKind = ForceArrowKind.LATERAL_ONLY,
    radial_axis_mode: RadialAxisMode = RadialAxisMode.MAGNITUDE,
    title: str = "",
    group_col: str | None = None,
    arrow_sample_step_deg: float = 30.0,
    silhouette_radial_fraction: float = 0.25,
) -> go.Figure:
    """Produce a Plotly polar Figure with vessel silhouette + force-arrow overlay.

    Args:
        data: DataFrame with either long-format (cols: theta_deg, value, component)
            OR wide-format (cols: theta_deg, fx, fy, fz, mx, my, mz).
        silhouette: VesselSilhouetteSpec composing a HullProfile and silhouette style.
        frame_convention: how to interpret theta_deg (OCIMF default).
        force_arrow_kind: which on-body force-vector to render.
        radial_axis_mode: MAGNITUDE (today's |C|) or SIGNED.
        title: figure title text.
        group_col: long-format only — name of column to group/colour by.
        arrow_sample_step_deg: arrows are drawn every N degrees; bounds clutter.

    Returns:
        Plotly `go.Figure` with silhouette + data traces + arrow traces +
        provenance metadata in `fig.layout.meta`.
    """
    # ---------- 1. Validate input ----------
    schema = _detect_schema(data)
    _validate_theta_range(data)

    # Compute max data radial value for silhouette + arrow scaling
    if schema == "long":
        max_data_r = float(data["value"].abs().max()) if not data.empty else 1.0
    else:
        force_cols = [c for c in ("fx", "fy", "fz", "mx", "my", "mz") if c in data.columns]
        max_data_r = float(data[force_cols].abs().to_numpy().max()) if force_cols else 1.0
    if max_data_r <= 0:
        max_data_r = 1.0

    fig = go.Figure()

    # ---------- 2. Silhouette first (drawn behind data) ----------
    _add_silhouette_traces(fig, silhouette, max_data_r, silhouette_radial_fraction)

    # ---------- 3. Data traces ----------
    _add_data_traces(fig, data, schema, radial_axis_mode, group_col)

    # ---------- 4. Force arrows ----------
    if force_arrow_kind is not ForceArrowKind.NONE:
        _render_force_arrows(
            fig, data, schema, frame_convention, force_arrow_kind, arrow_sample_step_deg,
        )

    # ---------- 5. Layout ----------
    fig.update_layout(
        title=dict(text=title or "Polar force overlay", x=0.02, xanchor="left"),
        polar=dict(
            angularaxis=dict(
                direction="clockwise",
                rotation=90,
                tickmode="array",
                tickvals=[0, 45, 90, 135, 180, 225, 270, 315],
                ticktext=[
                    "0° bow", "45°", "90° stbd*", "135°",
                    "180° stern", "225°", "270° port*", "315°",
                ],
            ),
            radialaxis=dict(gridcolor="#e0e0e0"),
        ),
        height=520,
        margin=dict(l=60, r=200, t=100, b=40),
        paper_bgcolor="white",
        showlegend=True,
        legend=dict(font=dict(size=10)),
        # Provenance metadata for downstream report consumers (per plan r1 m5)
        meta=dict(
            data_source_kind="caller-supplied",
            frame_convention=frame_convention.value,
            convention_citation=OCIMF_CONVENTION_AUTHORITY.citation_text(),
        ),
    )

    return fig


def _render_force_arrows(
    fig: go.Figure,
    data: pd.DataFrame,
    schema: str,
    frame_convention: FrameConvention,
    force_arrow_kind: ForceArrowKind,
    arrow_sample_step_deg: float,
) -> None:
    """Render arrows at sampled headings.

    Samples at every `arrow_sample_step_deg` so arrow count is bounded at
    `360 / step` regardless of data density.
    """
    # Build a (theta, lateral_value) view independent of schema.
    # Per reopen-fix-bug-2: long-format lateral-component detection must recognize
    # OCIMF coefficient names (Cyc, Cyw) in addition to the generic "Y" / "fy"
    # column-name conventions. Cy* coefficients are lateral force components;
    # Cxy* coefficients are yaw moments (excluded here — moments belong to a
    # future ForceArrowKind.YAW_MOMENT path, not LATERAL_ONLY).
    if schema == "long":
        records = data[["theta_deg", "value", "component"]].copy()
        def _is_lateral(comp: str) -> bool:
            if not isinstance(comp, str):
                return False
            if comp in ("Y", "fy"):
                return True
            # OCIMF lateral: starts with "Cy" but NOT "Cxy" (yaw moment).
            # "Cyc" starts with "Cy"; "Cxyc" starts with "Cx" so startswith("Cy") is False — clean discriminator.
            return comp.startswith("Cy")
        records = records[records["component"].map(_is_lateral)]
    else:
        records = data[["theta_deg", "fy"]].rename(columns={"fy": "value"}).copy()
        records["component"] = "fy"

    # Subsample: bin by step, take the first record per bin
    records["bin"] = (records["theta_deg"] // arrow_sample_step_deg).astype(int)
    sampled = records.drop_duplicates(subset=["bin"]).sort_values("theta_deg")

    max_value = float(records["value"].abs().max()) if not records.empty else 1.0
    if max_value <= 0:
        max_value = 1.0

    for _, row in sampled.iterrows():
        if force_arrow_kind is ForceArrowKind.LONGITUDINAL_ONLY:
            # not implemented in v1 for lateral-focused tests; fall back to no-op
            continue
        sign = 1 if row["value"] >= 0 else -1
        arrow_dir = _resolve_arrow_direction_in_body_frame(
            theta_incidence_deg=float(row["theta_deg"]),
            component_sign=sign,
            frame_convention=frame_convention,
        )
        # Arrow PLACEMENT is at the data incidence angle (where the data point sits
        # on the polar); the arrow HEAD'S ROTATION points toward the force direction.
        # This produces multiple distinct head positions across the sweep (one per
        # sampled incidence θ) while every head rotates to indicate force direction.
        incidence_theta = float(row["theta_deg"])
        # Arrow length = |value| / max_value (normalized)
        length = abs(float(row["value"])) / max_value * 0.4  # 40% of radial axis at peak
        color = "#1f77b4" if sign >= 0 else "#d62728"
        _add_force_arrow(
            fig,
            r_origin=0.0,
            r_tip=length,
            placement_theta_deg=incidence_theta,    # placed at data incidence
            arrow_direction_deg=arrow_dir,          # head rotated to show force direction
            color=color,
        )
