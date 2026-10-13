# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reference-jacket presentation from an attributed three-dimensional schedule."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any
import re


from digitalmodel.cathodic_protection.report_elevation import _svg, validate_geometry
from digitalmodel.reporting.spec import FigureBlock

ATTRIBUTION = (
    "[2] Mroczek, 2023, revision da77a393; Apache License 2.0. "
    "[1] Mroczek, Arwade and Lackner, 2023; CC BY 4.0."
)


def reference_projection(model: Mapping[str, Any]) -> dict[str, Any]:
    """Project every tubular axis onto the vertical y–z plane."""
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    legs: list[dict[str, Any]] = []
    diagonals: list[dict[str, Any]] = []
    for member in model["members"]:
        a, b = (joints[i] for i in member["joint_ids"])
        item = {"name": f"member-{member['id']}", "start": [a[1], a[2]],
                "end": [b[1], b[2]],
                "diameter_m": props[member["property_set_id"]]["diameter_m"],
                "true_length_m": member["true_length_m"], "count": member["count"]}
        (legs if member["section_class"] <= 5 else diagonals).append(item)
    return {
        "basis": "openly_licensed_reference", "datum": "still-water level",
        "waterline_m": 0, "mudline_m": -model["water_depth_m"],
        "zones": [
            {"name": "Submerged and splash, undivided", "calculation_zone": "submerged",
             "lower_m": -model["water_depth_m"], "upper_m": 0},
            {"name": "Splash and atmospheric, undivided", "calculation_zone": "atmospheric",
             "lower_m": 0, "upper_m": max(j[2] for j in joints.values())}],
        "legs": legs, "diagonals": diagonals, "brace_levels": [],
        "citation": "[1]; [2]", "attribution": ATTRIBUTION,
        "licence": "Apache License 2.0; CC BY 4.0",
    }


def _dimensioned_svg(model: Mapping[str, Any], geometry: Any) -> str:
    """Append coordinate-derived principal dimensions to the scaled figure."""
    svg = _svg(geometry)
    match = re.search(r'viewBox="0 0 ([^ ]+) ([^"]+)"', svg)
    if match is None:
        raise ValueError("drawing extent is not established")
    width, height = map(float, match.groups())
    joints = [j["coordinates_m"] for j in model["joints"]]
    top = max(j[2] for j in joints)
    bottom = min(j[2] for j in joints)
    top_span = max(j[1] for j in joints if j[2] == top) - min(j[1] for j in joints if j[2] == top)
    base_span = max(j[1] for j in joints if j[2] == bottom) - min(j[1] for j in joints if j[2] == bottom)
    labels = [f"Axis height above mudline: {top-bottom:.1f} m; water depth: {-bottom:.1f} m",
              f"Projected top span: {top_span:.3f} m; mudline span: {base_span:.3f} m"]
    svg = svg.replace(match.group(), f'viewBox="0 0 {width:g} {height+45:g}"')
    text = "".join(f'<text x="45" y="{height+15+i*14:g}" font-size="10">{label}</text>' for i, label in enumerate(labels))
    return svg.replace("</svg>", text + "</svg>")


def reference_figure(model: Mapping[str, Any]) -> FigureBlock:
    """Draw the complete coordinate-derived jacket at one scale."""
    geometry = validate_geometry(reference_projection(model))
    return FigureBlock(
        title="Reference jacket elevation", figure_id="fig-reference-jacket-elevation",
        svg=_dimensioned_svg(model, geometry),
        caption=(
            "Projection onto the vertical y–z plane of all 63 tubular members; "
            "axis true lengths are computed in three dimensions. The source inventory "
            "contains no horizontal plan-bracing members. Joint offsets and can lengths "
            "are not deducted. Water depth is 60 m; tubular height above mudline is 74.1 m. "
            "The projected top and mudline spans are distinct from triangle side widths; "
            "both measures are compared in Verification of structural dimensions. "
            "Splash separation is not evaluated: water-level and lower splash-limit "
            "evidence are required. Values were transcribed and converted in form; "
            "the figure is drawn from coordinates, with no source figure adapted. "
            + ATTRIBUTION
        ),
    )
