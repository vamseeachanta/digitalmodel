"""Benchmark 3D mesh schematic HTML builder.

ABOUTME: build_mesh_schematic_html plus _build_panel_scatter_html and
_build_panel_mesh3d_html. Split from benchmark_input_reports.py (WRK-593).

Lazy import of MeshPipeline inside build_mesh_schematic_html to avoid
requiring OrcFxAPI at import time.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import plotly.graph_objects as go
from loguru import logger

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    _parse_fdf_panels,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


def build_mesh_schematic_html(
    solver_names: List[str],
    solver_results: Dict[str, DiffractionResults],
    solver_metadata: Dict[str, Dict[str, Any]],
    get_solver_style,
) -> str:
    """Build 3D mesh schematic section for the benchmark report."""
    from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

    # --- Primary: OrcFxAPI panelGeometry (symmetry-expanded) ---
    for solver in solver_names:
        meta = solver_metadata.get(solver, {})
        panel_geometry_data = meta.get("panel_geometry")
        if panel_geometry_data:
            n_panels = len(panel_geometry_data)
            has_vertices = bool(
                panel_geometry_data and "vertices" in panel_geometry_data[0]
            )
            fdf_path = meta.get("fdf_path")
            if has_vertices:
                mesh_html = _build_panel_mesh3d_html(
                    panel_geometry_data,
                    fdf_path=fdf_path,
                    title=f"Panel Mesh & Free Surface Zone ({solver})",
                )
                fs_note = (
                    " + free-surface zone from FDF" if fdf_path else ""
                )
                return (
                    f"<h2>Panel Mesh Geometry</h2>\n"
                    f'<p style="color:#555;font-size:0.9em">Source: OrcFxAPI panelGeometry '
                    f'({n_panels} panels, symmetry-expanded{fs_note}). '
                    f"Use the view buttons or drag to rotate.</p>\n"
                    f"{mesh_html}"
                )
            scatter_html = _build_panel_scatter_html(
                panel_geometry_data,
                title=f"Panel Geometry ({solver})",
            )
            return (
                f"<h2>Panel Mesh Geometry</h2>\n"
                f'<p style="color:#555;font-size:0.9em">Source: OrcFxAPI panelGeometry '
                f'({n_panels} panels, symmetry-expanded)</p>\n'
                f"{scatter_html}"
            )

    # --- Fallback: GDF file loading ---
    mesh_entries: List[tuple] = []
    for solver in solver_names:
        meta = solver_metadata.get(solver, {})
        mesh_path = meta.get("mesh_path")
        if mesh_path:
            mesh_entries.append((solver, str(mesh_path)))

    if not mesh_entries:
        return ""

    pipeline = MeshPipeline()
    loaded: List[tuple] = []
    for solver, mesh_path_str in mesh_entries:
        try:
            mesh = pipeline.load(Path(mesh_path_str))
            loaded.append((solver, mesh))
        except Exception as exc:
            logger.warning(
                f"Could not load mesh for solver '{solver}' "
                f"from '{mesh_path_str}': {exc}"
            )

    if not loaded:
        return ""

    fig = go.Figure()

    for idx, (solver, mesh) in enumerate(loaded):
        style = get_solver_style(idx)
        color = style["color_base"]
        verts = mesh.vertices
        panels_data = mesh.panels

        i_list: List[int] = []
        j_list: List[int] = []
        k_list: List[int] = []
        for panel in panels_data:
            valid = [v for v in panel if v >= 0]
            if len(valid) >= 3:
                i_list.append(valid[0])
                j_list.append(valid[1])
                k_list.append(valid[2])
            if len(valid) == 4:
                i_list.append(valid[0])
                j_list.append(valid[2])
                k_list.append(valid[3])

        fig.add_trace(
            go.Mesh3d(
                x=verts[:, 0], y=verts[:, 1], z=verts[:, 2],
                i=i_list, j=j_list, k=k_list,
                opacity=0.6, color=color,
                name=f"{solver} ({mesh.n_panels} panels)",
                showlegend=True,
            )
        )

        edge_x: List[Optional[float]] = []
        edge_y: List[Optional[float]] = []
        edge_z: List[Optional[float]] = []
        for panel in panels_data:
            valid = [v for v in panel if v >= 0]
            if len(valid) < 3:
                continue
            for vi in valid:
                edge_x.append(float(verts[vi, 0]))
                edge_y.append(float(verts[vi, 1]))
                edge_z.append(float(verts[vi, 2]))
            edge_x.append(float(verts[valid[0], 0]))
            edge_y.append(float(verts[valid[0], 1]))
            edge_z.append(float(verts[valid[0], 2]))
            edge_x.append(None)
            edge_y.append(None)
            edge_z.append(None)

        fig.add_trace(
            go.Scatter3d(
                x=edge_x, y=edge_y, z=edge_z,
                mode="lines",
                line=dict(color=color, width=1),
                name=f"{solver} edges",
                showlegend=False, hoverinfo="skip",
            )
        )

    all_verts = np.vstack([m.vertices for _, m in loaded])
    x_min, y_min, _ = np.min(all_verts, axis=0)
    x_max, y_max, _ = np.max(all_verts, axis=0)
    pad_x = (x_max - x_min) * 0.15
    pad_y = (y_max - y_min) * 0.15
    wx = [x_min - pad_x, x_max + pad_x, x_max + pad_x, x_min - pad_x]
    wy = [y_min - pad_y, y_min - pad_y, y_max + pad_y, y_max + pad_y]
    wz = [0.0, 0.0, 0.0, 0.0]
    fig.add_trace(
        go.Mesh3d(
            x=wx, y=wy, z=wz,
            i=[0, 0], j=[1, 2], k=[2, 3],
            opacity=0.15, color="#3498db",
            name="Waterline (z=0)",
            showlegend=True, hoverinfo="skip",
        )
    )

    fig.update_layout(
        title_text="Panel Mesh Geometry",
        template="plotly_white",
        scene=dict(
            aspectmode="data",
            xaxis_title="X (m)", yaxis_title="Y (m)", zaxis_title="Z (m)",
        ),
        margin=dict(l=0, r=0, t=40, b=0),
        height=600,
    )

    plot_div = fig.to_html(
        full_html=False, include_plotlyjs=False, div_id="mesh_schematic",
    )

    table_rows = ""
    for solver, mesh in loaded:
        table_rows += (
            f"<tr>"
            f"<td>{html_mod.escape(solver)}</td>"
            f"<td>{mesh.n_panels}</td>"
            f"<td>{mesh.n_vertices}</td>"
            f"<td>{mesh.total_area:.2f}</td>"
            f"</tr>\n"
        )

    summary_table = (
        "<table style='max-width:500px;margin-top:0.8em;'>"
        "<tr><th>Solver</th><th>Panels</th>"
        "<th>Vertices</th><th>Total Area (m&sup2;)</th></tr>"
        f"{table_rows}"
        "</table>"
    )

    return (
        f"<h2>Panel Mesh Geometry</h2>\n"
        f"{plot_div}\n"
        f"{summary_table}"
    )


def _build_panel_scatter_html(
    panel_geometry_data: list,
    title: str = "Panel Geometry",
    height: int = 400,
) -> str:
    """Build interactive 3D scatter of panel centroids from panelGeometry data."""
    bodies_data: dict = {}
    for p in panel_geometry_data:
        name = p.get("objectName", "Body")
        if name not in bodies_data:
            bodies_data[name] = {"x": [], "y": [], "z": [], "area": []}
        c = p["centroid"]
        bodies_data[name]["x"].append(c[0])
        bodies_data[name]["y"].append(c[1])
        bodies_data[name]["z"].append(c[2])
        bodies_data[name]["area"].append(p["area"])

    colours = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"]
    traces: List[Any] = []
    for i, (name, pts) in enumerate(bodies_data.items()):
        sizes = [max(4.0, 12.0 * (a ** 0.5)) for a in pts["area"]]
        traces.append(go.Scatter3d(
            x=pts["x"], y=pts["y"], z=pts["z"],
            mode="markers",
            marker=dict(size=sizes, color=colours[i % len(colours)], opacity=0.7),
            name=name,
            text=[f"Area: {a:.3f} m\u00b2" for a in pts["area"]],
            hovertemplate="%{text}<extra>%{fullData.name}</extra>",
        ))

    all_x = [v for b in bodies_data.values() for v in b["x"]]
    all_y = [v for b in bodies_data.values() for v in b["y"]]
    if all_x and all_y:
        xr = [min(all_x) * 1.1, max(all_x) * 1.1]
        yr = [min(all_y) * 1.1, max(all_y) * 1.1]
        traces.append(go.Surface(
            x=[[xr[0], xr[1]], [xr[0], xr[1]]],
            y=[[yr[0], yr[0]], [yr[1], yr[1]]],
            z=[[0.0, 0.0], [0.0, 0.0]],
            showscale=False, opacity=0.15,
            colorscale=[[0, "#4fc3f7"], [1, "#4fc3f7"]],
            name="Waterplane", showlegend=False,
        ))

    fig = go.Figure(data=traces)
    fig.update_layout(
        title=title, height=height,
        scene=dict(
            xaxis_title="X (m)", yaxis_title="Y (m)", zaxis_title="Z (m)",
            aspectmode="data",
        ),
        margin=dict(l=0, r=0, t=40, b=0),
    )
    return fig.to_html(full_html=False, include_plotlyjs="cdn")


def _add_fdf_surface_traces(fig: go.Figure, fdf_panels: list) -> None:
    """Add symmetry-mirrored free-surface zone mesh and edges to fig."""
    fvx: List[float] = []
    fvy: List[float] = []
    fvz: List[float] = []
    fti: List[int] = []
    ftj: List[int] = []
    ftk: List[int] = []
    fex: List[Optional[float]] = []
    fey: List[Optional[float]] = []
    fez: List[Optional[float]] = []
    for pverts in fdf_panels:
        n = len(fvx)
        for v in pverts:
            fvx.append(float(v[0])); fvy.append(float(v[1])); fvz.append(float(v[2]))
        fti.append(n); ftj.append(n + 1); ftk.append(n + 2)
        if len(pverts) == 4:
            fti.append(n); ftj.append(n + 2); ftk.append(n + 3)
        for v in pverts:
            fex.append(float(v[0])); fey.append(float(v[1])); fez.append(float(v[2]))
        fex.append(float(pverts[0][0])); fey.append(float(pverts[0][1]))
        fez.append(float(pverts[0][2]))
        fex.append(None); fey.append(None); fez.append(None)
    # Y-mirror the free-surface zone (originally half-plane)
    n0 = len(fvx)
    mirror_i = [i + n0 for i in fti]
    mirror_j = [j + n0 for j in ftj]
    mirror_k = [k + n0 for k in ftk]
    fvx += list(fvx[:n0]); fvy += [-y for y in fvy[:n0]]; fvz += list(fvz[:n0])
    fti += mirror_i; ftj += mirror_j; ftk += mirror_k
    fig.add_trace(go.Mesh3d(
        x=fvx, y=fvy, z=fvz, i=fti, j=ftj, k=ftk,
        color="#00bcd4", opacity=0.25,
        name="Free-surface zone", showlegend=True, hoverinfo="skip",
    ))
    ne = len(fex)
    fex += list(fex[:ne])
    fey += [(-y if y is not None else None) for y in fey[:ne]]
    fez += list(fez[:ne])
    fig.add_trace(go.Scatter3d(
        x=fex, y=fey, z=fez, mode="lines",
        line=dict(color="#0097a7", width=0.8),
        name="FS edges", showlegend=False, hoverinfo="skip",
    ))


def _build_panel_mesh3d_html(
    panel_geometry_data: list,
    fdf_path: Optional[str] = None,
    title: str = "Panel Mesh & Free Surface Zone",
    height: int = 560,
) -> str:
    """Build interactive Mesh3d figure from panelGeometry vertices."""
    vx: List[float] = []
    vy: List[float] = []
    vz: List[float] = []
    tri_i: List[int] = []
    tri_j: List[int] = []
    tri_k: List[int] = []
    ex: List[Optional[float]] = []
    ey: List[Optional[float]] = []
    ez: List[Optional[float]] = []

    for panel in panel_geometry_data:
        verts = panel.get("vertices")
        if not verts or len(verts) < 3:
            continue
        n = len(vx)
        for v in verts:
            vx.append(float(v[0])); vy.append(float(v[1])); vz.append(float(v[2]))
        tri_i.append(n); tri_j.append(n + 1); tri_k.append(n + 2)
        if len(verts) == 4:
            tri_i.append(n); tri_j.append(n + 2); tri_k.append(n + 3)
        for v in verts:
            ex.append(float(v[0])); ey.append(float(v[1])); ez.append(float(v[2]))
        ex.append(float(verts[0][0])); ey.append(float(verts[0][1]))
        ez.append(float(verts[0][2]))
        ex.append(None); ey.append(None); ez.append(None)

    fig = go.Figure()
    if vx:
        fig.add_trace(go.Mesh3d(
            x=vx, y=vy, z=vz, i=tri_i, j=tri_j, k=tri_k,
            color="#e05a5a", opacity=0.45,
            name="Body panels", showlegend=True, hoverinfo="skip",
        ))
        fig.add_trace(go.Scatter3d(
            x=ex, y=ey, z=ez, mode="lines",
            line=dict(color="#c0392b", width=1),
            name="Body edges", showlegend=False, hoverinfo="skip",
        ))

    if fdf_path:
        fdf_panels = _parse_fdf_panels(Path(fdf_path))
        if fdf_panels:
            _add_fdf_surface_traces(fig, fdf_panels)

    cam_persp = dict(eye=dict(x=1.6, y=1.2, z=0.9), up=dict(x=0, y=0, z=1))
    cam_plan = dict(eye=dict(x=0, y=0, z=3.0), up=dict(x=0, y=1, z=0))
    cam_elev = dict(eye=dict(x=3.0, y=0, z=0), up=dict(x=0, y=0, z=1))
    fig.update_layout(
        title=dict(text=title, font=dict(size=13)),
        height=height,
        scene=dict(
            aspectmode="data",
            xaxis_title="X (m)", yaxis_title="Y (m)", zaxis_title="Z (m)",
        ),
        margin=dict(l=0, r=0, t=55, b=0),
        legend=dict(x=0.01, y=0.99, bgcolor="rgba(255,255,255,0.7)"),
        updatemenus=[dict(
            type="buttons", direction="right",
            x=0.0, y=1.10, xanchor="left",
            buttons=[
                dict(label="Perspective", method="relayout",
                     args=[{"scene.camera": cam_persp}]),
                dict(label="Plan (top)", method="relayout",
                     args=[{"scene.camera": cam_plan}]),
                dict(label="Elevation (side)", method="relayout",
                     args=[{"scene.camera": cam_elev}]),
            ],
        )],
    )
    fig.update_layout(scene_camera=cam_persp)
    return fig.to_html(full_html=False, include_plotlyjs=False)
