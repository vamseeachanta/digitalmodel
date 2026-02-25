"""Benchmark solver input parameter comparison and semantic equivalence HTML.

ABOUTME: build_input_comparison_html and build_semantic_equivalence_html
functions. Split from benchmark_input_reports.py (WRK-593 God Object split).

No circular dependencies — imports only html, typing, output_schemas.
"""
from __future__ import annotations

import html as html_mod
from typing import Any, Dict, List, Optional

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
                val = (f"{freq.min_freq:.3f} \u2013 {freq.max_freq:.3f}"
                       f" ({freq.count})")
            elif source == "_head_range":
                val = (f"{head.min_heading:.0f} \u2013 {head.max_heading:.0f}"
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
        "<li><strong>Cosmetic</strong> \u2014 GUI display, naming, "
        "output flags, dormant settings (no solver effect)</li>"
        "<li><strong>Convention</strong> \u2014 equivalent data in "
        "different representation (e.g. Hz vs rad/s)</li>"
        "<li><strong>Significant</strong> \u2014 real solver parameter "
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
