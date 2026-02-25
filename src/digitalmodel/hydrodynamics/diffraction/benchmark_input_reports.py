"""Benchmark input file reports: comparison tables, file viewers, semantic diffs.

ABOUTME: Standalone functions for building HTML sections that compare solver
input configurations. Extracted from BenchmarkPlotter as part of WRK-592.

These functions accept explicit parameters instead of self.* references.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import plotly.graph_objects as go
from loguru import logger

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    _FILE_DESCRIPTIONS,
    _parse_fdf_panels,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


def build_input_comparison_html(
    solver_names: List[str],
    solver_results: Dict[str, DiffractionResults],
    solver_metadata: Dict[str, Dict[str, Any]],
) -> str:
    """Render an HTML table comparing solver input parameters.

    Builds a structured comparison covering geometry, mass properties,
    environment, mesh, analysis settings, and phase/unit conventions.
    """
    # Ordered parameter definitions: (display_label, source)
    _PARAM_ROWS: List[tuple] = [
        ("_section", "Geometry"),
        ("Length (m)", "length"),
        ("Beam (m)", "beam"),
        ("Draft (m)", "draft"),
        ("Body dimensions (L x B x T)", "body_dimensions"),
        ("_section", "Mass Properties"),
        ("Mass", "mass"),
        ("Centre of gravity (m)", "centre_of_gravity"),
        ("Radii of gyration (m)", "radii_of_gyration"),
        ("_section", "Environment"),
        ("Water depth (m)", "_water_depth"),
        ("Water density (kg/m3)", "water_density"),
        ("Gravity (m/s2)", "gravity"),
        ("_section", "Mesh"),
        ("Mesh file", "mesh_file"),
        ("Mesh format", "mesh_format"),
        ("Panel count", "panel_count"),
        ("Symmetry", "mesh_symmetry"),
        ("_section", "Damping"),
        ("Radiation damping", "radiation_damping"),
        ("Viscous damping", "viscous_damping"),
        ("Damping lid", "damping_lid"),
        ("_section", "Analysis Settings"),
        ("Frequency range (rad/s)", "_freq_range"),
        ("Heading range (deg)", "_head_range"),
        ("Calculation method", "calculation_method"),
        ("Remove irregular freq.", "remove_irregular_frequencies"),
        ("QTF calculation", "qtf_calculation"),
        ("Precision", "precision"),
        ("_section", "Conventions"),
        ("Phase convention (raw)", "_raw_phase"),
        ("Phase convention (normalized)", "_norm_phase"),
        ("Unit system", "_unit_system"),
        ("_section", "OrcaWave Solver Settings"),
        ("Units system", "ow_units_system"),
        ("Solve type", "ow_solve_type"),
        ("Load RAO method", "ow_load_rao_method"),
        ("Linear solver", "ow_linear_solver"),
        ("Divide non-planar panels", "ow_divide_nonplanar"),
        ("Length tolerance", "ow_length_tolerance"),
        ("Waves referred to by", "ow_waves_referred_to_by"),
        ("_section", "OrcaWave Body Details"),
        ("Body count", "ow_body_count"),
        ("Body name", "ow_body_name"),
        ("Inertia specification", "ow_inertia_spec"),
        ("Interior surface panels", "ow_interior_panels"),
        ("Connection parent", "ow_connection_parent"),
        ("Fixed DOFs", "ow_fixed_dofs"),
        ("Roll damping target", "ow_roll_damping_target"),
        ("External damping", "ow_external_damping"),
        ("External stiffness", "ow_external_stiffness"),
        ("_section", "OrcaWave Advanced"),
        ("Damping lid", "ow_damping_lid"),
        ("Damping lid mesh", "ow_damping_lid_mesh"),
        ("Damping factor (epsilon)", "ow_damping_epsilon"),
        ("QTF calc method", "ow_qtf_method"),
        ("QTF crossing angles", "ow_qtf_crossing_angles"),
        ("QTF frequency types", "ow_qtf_freq_types"),
        ("Include mean drift QTFs", "ow_qtf_mean_drift"),
        ("Free surface zone type", "ow_fsz_type"),
        ("Free surface zone mesh", "ow_fsz_mesh"),
        ("Free surface inner radius", "ow_fsz_inner_radius"),
    ]

    merged: Dict[str, Dict[str, str]] = {}
    section_order: List[str] = []

    for label, source in _PARAM_ROWS:
        if label == "_section":
            section_order.append(f"__section__{source}")
            continue

        values_found = False
        if label not in merged:
            merged[label] = {"parameter": label}

        for solver in solver_names:
            dr = solver_results[solver]
            meta = solver_metadata.get(solver, {})
            freq = dr.raos.surge.frequencies
            head = dr.raos.surge.headings

            val: Optional[str] = None

            if source == "_water_depth":
                val = f"{dr.water_depth:.1f}"
            elif source == "_freq_range":
                val = (f"{freq.min_freq:.3f} – {freq.max_freq:.3f}"
                       f" ({freq.count})")
            elif source == "_head_range":
                val = (f"{head.min_heading:.0f} – {head.max_heading:.0f}"
                       f" ({head.count})")
            elif source == "_raw_phase":
                val = meta.get(
                    "raw_phase_convention",
                    "ISO 6954 (lead)" if dr.analysis_tool == "AQWA"
                    else "Orcina (lag)",
                )
            elif source == "_norm_phase":
                val = dr.phase_convention
            elif source == "_unit_system":
                val = dr.unit_system
            elif source in meta:
                val = str(meta[source])

            if val is not None:
                merged[label][solver] = val
                values_found = True

        if values_found:
            section_order.append(label)

    parts: List[str] = [
        "<h2>Input Comparison</h2>",
        '<table class="input-table" style="max-width:800px;">',
        "<thead><tr><th>Parameter</th>",
    ]
    for solver in solver_names:
        parts.append(f"<th>{html_mod.escape(solver)}</th>")
    parts.append("</tr></thead><tbody>")

    n_cols = 1 + len(solver_names)

    sections_with_data: set[str] = set()
    current_section = ""
    for entry in section_order:
        if entry.startswith("__section__"):
            current_section = entry
            continue
        param_data = merged.get(entry)
        if param_data:
            has_real_value = any(
                param_data.get(s, "-") != "-"
                for s in solver_names
            )
            if has_real_value:
                sections_with_data.add(current_section)

    for entry in section_order:
        if entry.startswith("__section__"):
            if entry not in sections_with_data:
                continue
            sec_name = entry.replace("__section__", "")
            parts.append(
                f"<tr class='section-row'>"
                f"<td colspan='{n_cols}'>"
                f"{html_mod.escape(sec_name)}</td></tr>"
            )
            continue

        param_data = merged.get(entry)
        if not param_data:
            continue

        all_dash = all(
            param_data.get(s, "-") == "-"
            for s in solver_names
        )
        if all_dash:
            continue

        parts.append("<tr>")
        parts.append(
            f"<td class='param-label'>"
            f"{html_mod.escape(param_data['parameter'])}</td>"
        )
        for solver in solver_names:
            val = param_data.get(solver, "-")
            parts.append(f"<td>{html_mod.escape(val)}</td>")
        parts.append("</tr>")

    parts.append("</tbody></table>")
    return "\n".join(parts)


def build_semantic_equivalence_html(
    solver_names: List[str],
    solver_metadata: Dict[str, Dict[str, Any]],
) -> str:
    """Render semantic equivalence comparison between solver inputs."""
    sem_data: Optional[Dict[str, Any]] = None
    for solver in solver_names:
        meta = solver_metadata.get(solver, {})
        if "_semantic_equivalence" in meta:
            sem_data = meta["_semantic_equivalence"]
            break

    if sem_data is None:
        return ""

    match_count = sem_data.get("match_count", 0)
    cosmetic_count = sem_data.get("cosmetic_count", 0)
    convention_count = sem_data.get("convention_count", 0)
    sig_count = sem_data.get("significant_count", 0)
    total = match_count + cosmetic_count + convention_count + sig_count
    diffs = sem_data.get("diffs", [])

    if sig_count == 0:
        badge_color = "#27ae60"
        badge_text = "EQUIVALENT"
    elif sig_count <= 5:
        badge_color = "#f39c12"
        badge_text = f"{sig_count} SIGNIFICANT DIFF(S)"
    else:
        badge_color = "#e74c3c"
        badge_text = f"{sig_count} SIGNIFICANT DIFF(S)"

    parts: List[str] = [
        "<h3>Semantic Equivalence</h3>",
        "<p>Key-by-key comparison of the OrcaWave SaveData() YAML "
        "exports from both solver paths. Keys are classified as:</p>"
        "<ul style='font-size:13px;margin:0.3em 0 0.8em;'>"
        "<li><strong>Cosmetic</strong> — GUI display, naming, "
        "output flags, dormant settings (no solver effect)</li>"
        "<li><strong>Convention</strong> — equivalent data in "
        "different representation (e.g. Hz vs rad/s)</li>"
        "<li><strong>Significant</strong> — real solver parameter "
        "difference that may affect results</li></ul>",
        "<table class='stats-table' style='max-width:500px;"
        "margin-bottom:1em;'>",
        "<tr><th>Metric</th><th>Count</th></tr>",
        f"<tr><td>Matching keys</td>"
        f"<td style='color:#27ae60;font-weight:bold'>"
        f"{match_count}</td></tr>",
        f"<tr><td>Cosmetic differences</td>"
        f"<td style='color:#6b7280'>{cosmetic_count}</td></tr>",
        f"<tr><td>Convention differences</td>"
        f"<td style='color:#2563eb'>{convention_count}</td></tr>",
        f"<tr><td>Significant differences</td>"
        f"<td style='color:{'#e74c3c' if sig_count > 0 else '#27ae60'}"
        f";font-weight:bold'>{sig_count}</td></tr>",
        f"<tr><td>Total keys compared</td><td>{total}</td></tr>",
        "</table>",
        f"<div style='display:inline-block;padding:4px 16px;"
        f"border-radius:4px;color:white;font-weight:bold;"
        f"background:{badge_color};margin-bottom:1em;'>"
        f"{badge_text}</div>",
    ]

    if diffs:
        sig_diffs = [d for d in diffs if d["level"] == "significant"]
        conv_diffs = [d for d in diffs if d["level"] == "convention"]
        cos_diffs = [d for d in diffs if d["level"] == "cosmetic"]

        _KEY_COMMENTS: Dict[str, str] = {
            "DivideNonPlanarPanels": (
                "Splits non-planar panels into triangles; "
                "no effect on planar meshes but matters "
                "for curved geometry"
            ),
            "WavesReferredToBy": "Same frequencies, different unit label",
            "PeriodOrFrequency": (
                "Same frequency grid expressed as rad/s vs period (s)"
            ),
            "PreferredQuadraticLoadCalculationMethod": (
                "QTF setting; dormant when QTF disabled"
            ),
            "QTFMinCrossingAngle": "QTF setting; dormant when QTF disabled",
            "QTFMaxCrossingAngle": "QTF setting; dormant when QTF disabled",
            "QuadraticLoadPressureIntegration": (
                "QTF setting; dormant when QTF disabled"
            ),
            "QTFCalculationMethod": "QTF setting; dormant when QTF disabled",
            "QTFFrequencyTypes": "QTF setting; dormant when QTF disabled",
            "IncludeMeanDriftFullQTFs": (
                "QTF setting; dormant when QTF disabled"
            ),
            "OutputPanelPressures": "Output flag; no effect on RAO results",
            "OutputPanelVelocities": "Output flag; no effect on RAO results",
            "FieldPointX, FieldPointY, FieldPointZ": (
                "Pressure monitoring points; not used in RAO calculation"
            ),
            "FreeSurfaceMeshPen": "GUI display color only",
            "InteriorSurfacePanelsPen": "GUI display color only",
            "BodyMeshPen": "GUI display color only",
            "WaterlinePen": "GUI display color only",
            "DampingLidMeshPen": "GUI display color only",
            "BodyName": "Label only; no solver effect",
            "BodyMeshFileName": "Different filename, same mesh geometry",
            "BodyOrcaFlexImportLength": (
                "OrcaFlex GUI import hint; not used by OrcaWave solver"
            ),
            "BodyOrcaFlexImportSymmetry": (
                "OrcaFlex import setting; not a solver parameter"
            ),
            "DampingLidMeshFileName": "Different filename; same lid geometry",
            "ComputationStrategy": (
                "OrcaWave internal default; not configurable via spec"
            ),
            "EnableMultibodyConstraints": (
                "OrcaWave internal default; not configurable via spec"
            ),
            "BodyOriginType": (
                "OrcaWave internal default; not configurable via spec"
            ),
            "BodyVolumeWarningLevel": (
                "OrcaWave internal default; not configurable via spec"
            ),
        }

        _LEVEL_DEFAULTS: Dict[str, str] = {
            "significant": "Solver parameter difference",
            "convention": "Same physics, different representation",
            "cosmetic": "No solver effect",
        }

        def _comment_for(d: Dict[str, Any]) -> str:
            key = d["key"]
            bare = key.split(".")[-1] if "." in key else key
            return _KEY_COMMENTS.get(
                bare, _LEVEL_DEFAULTS.get(d["level"], ""),
            )

        def _render_diff_table(
            items: List[Dict[str, Any]], title: str,
            collapsed: bool = False,
        ) -> None:
            if not items:
                return
            header = (
                "<table style='max-width:1000px;font-size:12px;"
                "margin-top:0.5em;'>"
                "<tr><th style='text-align:left'>Key</th>"
                "<th style='text-align:left'>OrcaWave (.owd)</th>"
                "<th style='text-align:left'>OrcaWave (spec.yml)</th>"
                "<th style='text-align:left'>Comment</th>"
                "</tr>"
            )
            rows = ""
            for d in items:
                comment = html_mod.escape(_comment_for(d))
                rows += (
                    f"<tr>"
                    f"<td><code>{html_mod.escape(d['key'])}</code></td>"
                    f"<td>{html_mod.escape(d['owd'])}</td>"
                    f"<td>{html_mod.escape(d['spec'])}</td>"
                    f"<td style='color:#777;font-style:italic;'>"
                    f"{comment}</td>"
                    f"</tr>"
                )
            if collapsed:
                parts.append(
                    f"<details style='margin-top:0.5em;font-size:12px;'>"
                    f"<summary>{title} ({len(items)})</summary>"
                    f"{header}{rows}</table></details>"
                )
            else:
                parts.append(f"<h4>{title}</h4>{header}{rows}</table>")

        _render_diff_table(sig_diffs, "Significant Differences")
        _render_diff_table(
            conv_diffs, "Convention Differences", collapsed=True,
        )
        _render_diff_table(
            cos_diffs, "Cosmetic Differences", collapsed=True,
        )

    return "\n".join(parts)


def build_input_files_html(
    solver_names: List[str],
    solver_metadata: Dict[str, Dict[str, Any]],
) -> str:
    """Render scrollable input file previews for each solver."""
    max_lines = 2000
    file_entries: List[tuple] = []

    for solver in solver_names:
        meta = solver_metadata.get(solver, {})
        input_file = meta.get("input_file")
        if not input_file:
            continue

        file_path = Path(input_file)
        if not file_path.exists():
            logger.warning(
                f"Input file for solver '{solver}' not found: {input_file}"
            )
            continue

        content: Optional[str] = None
        for encoding in ("utf-8", "latin-1"):
            try:
                content = file_path.read_text(encoding=encoding)
                break
            except (UnicodeDecodeError, OSError):
                continue

        if content is None:
            logger.warning(
                f"Could not read input file for solver '{solver}': "
                f"{input_file}"
            )
            continue

        lines = content.splitlines()
        truncated = len(lines) > max_lines
        if truncated:
            lines = lines[:max_lines]
        content = "\n".join(lines)

        file_entries.append(
            (solver, str(file_path), content, truncated, len(lines))
        )

    if not file_entries:
        return ""

    semantic_html = build_semantic_equivalence_html(
        solver_names, solver_metadata,
    )

    parts: List[str] = ["<h2>Input Files</h2>"]
    if semantic_html:
        parts.append(semantic_html)

    for idx, (solver, path_str, content, truncated, n_lines) in enumerate(
        file_entries
    ):
        safe_solver = html_mod.escape(solver)
        safe_path = html_mod.escape(path_str)
        safe_content = html_mod.escape(content)
        textarea_id = f"file_content_{idx}"

        line_spans: List[str] = []
        for line in content.splitlines():
            line_spans.append(
                f'<span class="line">{html_mod.escape(line)}</span>'
            )
        numbered_content = "\n".join(line_spans)

        truncation_note = ""
        if truncated:
            truncation_note = (
                f'<div style="padding:0.4em 1em;background:#fef9e7;'
                f'border:1px solid #ddd;border-top:none;font-size:0.8em;'
                f'color:#888;font-style:italic;">'
                f"Showing first {n_lines} lines (file truncated)"
                f"</div>"
            )

        description = _FILE_DESCRIPTIONS.get(solver, "")
        desc_html = ""
        if description:
            desc_html = (
                f'<p style="margin:0.3em 0 0.5em;font-size:12px;'
                f'color:#64748b;max-width:700px;">'
                f'{html_mod.escape(description)}</p>'
            )

        parts.append(f"""\
<h3 style="margin-top:1.5em;margin-bottom:0.2em;">{safe_solver}</h3>
{desc_html}
<div class="file-viewer">
  <div class="file-viewer-header">
    <div>
      <span class="solver-label">{safe_solver}</span>
      <span class="file-path">{safe_path}</span>
    </div>
    <button onclick="openFileWindow_{idx}()">Open in New Window</button>
  </div>
  <div class="file-content">
    <pre>{numbered_content}</pre>
  </div>
  {truncation_note}
  <textarea id="{textarea_id}" style="display:none;">{safe_content}</textarea>
  <script>
    function openFileWindow_{idx}() {{
      var ta = document.getElementById('{textarea_id}');
      var w = window.open('', '_blank');
      w.document.write(
        '<html><head><title>{safe_solver} - {safe_path}</title>' +
        '<style>body{{font-family:"SF Mono","Cascadia Code","Consolas",' +
        'monospace;white-space:pre;margin:1em;font-size:13px;' +
        'line-height:1.5;tab-size:4;}}</style></head><body>' +
        ta.value.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;') +
        '</body></html>'
      );
      w.document.close();
    }}
  </script>
</div>""")

    return "\n".join(parts)


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
            vx.append(float(v[0]))
            vy.append(float(v[1]))
            vz.append(float(v[2]))
        tri_i.append(n); tri_j.append(n + 1); tri_k.append(n + 2)
        if len(verts) == 4:
            tri_i.append(n); tri_j.append(n + 2); tri_k.append(n + 3)
        for v in verts:
            ex.append(float(v[0]))
            ey.append(float(v[1]))
            ez.append(float(v[2]))
        ex.append(float(verts[0][0]))
        ey.append(float(verts[0][1]))
        ez.append(float(verts[0][2]))
        ex.append(None); ey.append(None); ez.append(None)

    fig = go.Figure()
    if vx:
        fig.add_trace(go.Mesh3d(
            x=vx, y=vy, z=vz,
            i=tri_i, j=tri_j, k=tri_k,
            color="#e05a5a", opacity=0.45,
            name="Body panels", showlegend=True, hoverinfo="skip",
        ))
        fig.add_trace(go.Scatter3d(
            x=ex, y=ey, z=ez,
            mode="lines", line=dict(color="#c0392b", width=1),
            name="Body edges", showlegend=False, hoverinfo="skip",
        ))

    if fdf_path:
        fdf_panels = _parse_fdf_panels(Path(fdf_path))
        if fdf_panels:
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
                    fvx.append(float(v[0]))
                    fvy.append(float(v[1]))
                    fvz.append(float(v[2]))
                fti.append(n); ftj.append(n + 1); ftk.append(n + 2)
                if len(pverts) == 4:
                    fti.append(n); ftj.append(n + 2); ftk.append(n + 3)
                for v in pverts:
                    fex.append(float(v[0]))
                    fey.append(float(v[1]))
                    fez.append(float(v[2]))
                fex.append(float(pverts[0][0]))
                fey.append(float(pverts[0][1]))
                fez.append(float(pverts[0][2]))
                fex.append(None); fey.append(None); fez.append(None)
            n0 = len(fvx)
            mirror_vx = list(fvx)
            mirror_vy = [-y for y in fvy]
            mirror_vz = list(fvz)
            mirror_i = [i + n0 for i in fti]
            mirror_j = [j + n0 for j in ftj]
            mirror_k = [k + n0 for k in ftk]
            fvx += mirror_vx
            fvy += mirror_vy
            fvz += mirror_vz
            fti += mirror_i
            ftj += mirror_j
            ftk += mirror_k
            fig.add_trace(go.Mesh3d(
                x=fvx, y=fvy, z=fvz,
                i=fti, j=ftj, k=ftk,
                color="#00bcd4", opacity=0.25,
                name="Free-surface zone", showlegend=True, hoverinfo="skip",
            ))
            mirror_ex = list(fex)
            mirror_ey = [(-y if y is not None else None) for y in fey]
            mirror_ez = list(fez)
            fex += mirror_ex
            fey += mirror_ey
            fez += mirror_ez
            fig.add_trace(go.Scatter3d(
                x=fex, y=fey, z=fez,
                mode="lines", line=dict(color="#0097a7", width=0.8),
                name="FS edges", showlegend=False, hoverinfo="skip",
            ))

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
