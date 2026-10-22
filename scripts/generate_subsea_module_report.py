# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the subsea report using the documented public catenary example.

Run from the repository with PYTHONPATH=src. No licensed solver is imported.
The report contains its styles, chart arrays and comment controls inline.
"""

from __future__ import annotations

import argparse
import ast
from dataclasses import replace
import hashlib
import json
import math
from pathlib import Path
import re
from typing import Any

from jinja2 import Environment, StrictUndefined
from markupsafe import Markup
import numpy as np

from digitalmodel.subsea.catenary_riser import (
    CatenaryRiserResult, EffectiveWeightCalculator, EffectiveWeightResult,
    PRODUCTION_OIL, RiserConfiguration,
    SEAWATER, SimpleCatenaryAnalyzer, STEEL_API_5L_X65,
)

ROOT = Path(__file__).resolve().parents[1]
PACKAGE = Path("src/digitalmodel/subsea/catenary_riser")
DESTINATION = Path("docs/reports/modules/subsea")
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
README = PACKAGE / "README.md"
TEMPLATE = "scripts/templates/subsea_module_report.html"
REFERENCE_URL = "https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf"
# Catenary Curve, 15 August 2019, p. 11, Table 1 and steps 2–4.
# These are published geometry values, not engineering design defaults.
PUBLIC_GEOMETRY = {
    "parameter_m": 72.288087476,
    "upper_rise_m": 29.522693342,
    "upper_offset_m": 63.291060536,
    "upper_length_m": 71.69286924,
}


def example_inputs() -> dict[str, Any]:
    """Read literals from the README Python API example without executing it."""
    text = (ROOT / README).read_text(encoding="utf-8")
    snippet = text.split("### Python API", 1)[1].split("```python", 1)[1].split("```", 1)[0]
    tree = ast.parse(snippet)
    assignment = next(node for node in tree.body if isinstance(node, ast.Assign)
                      and any(isinstance(t, ast.Name) and t.id == "riser" for t in node.targets))
    if (not isinstance(assignment.value, ast.Call)
            or not isinstance(assignment.value.func, ast.Name)
            or assignment.value.func.id != "RiserConfiguration"):
        raise ValueError("Documented example no longer constructs RiserConfiguration")
    references = {"material": "STEEL_API_5L_X65", "internal_fluid": "PRODUCTION_OIL",
                  "external_fluid": "SEAWATER"}
    literals = {}
    seen = set()
    for keyword in assignment.value.keywords:
        seen.add(keyword.arg)
        if keyword.arg in references:
            if not isinstance(keyword.value, ast.Name) or keyword.value.id != references[keyword.arg]:
                raise ValueError(f"Unsupported documented input: {keyword.arg}")
        elif isinstance(keyword.value, ast.Constant) and keyword.arg is not None:
            literals[keyword.arg] = ast.literal_eval(keyword.value)
        else:
            raise ValueError(f"Unsupported documented input: {keyword.arg}")
    if not references.keys() <= seen:
        raise ValueError("Missing documented input: material or fluid")
    return literals


def calculate_example() -> tuple[dict[str, Any], RiserConfiguration, CatenaryRiserResult, EffectiveWeightResult]:
    """Call the documented weight and geometry entry points on README inputs."""
    inputs = example_inputs()
    riser = RiserConfiguration(**inputs, material=STEEL_API_5L_X65,
                               internal_fluid=PRODUCTION_OIL, external_fluid=SEAWATER)
    weight = EffectiveWeightCalculator().calculate(riser)
    result = SimpleCatenaryAnalyzer().analyze_riser(
        riser, water_depth=inputs["water_depth"], horizontal_offset=inputs["horizontal_offset"])
    if not result.converged:
        raise ValueError("Documented example has not converged within available length")
    return inputs, riser, result, weight


def public_reference() -> dict[str, float]:
    """Map the upper vertex-to-support branch into the module's static model.

    Weight is taken from the repository example to scale force. Geometry is
    independent of that choice when H/w is held at the published parameter.
    No claim is made about the reference cable's actual weight or capacity.
    """
    _, riser, _, _ = calculate_example()
    ref = PUBLIC_GEOMETRY
    result = SimpleCatenaryAnalyzer().analyze_riser(
        replace(riser, water_depth=ref["upper_rise_m"],
                horizontal_offset=ref["upper_offset_m"]),
        horizontal_tension=ref["parameter_m"] * riser.effective_weight_per_length)
    offset_difference = 100 * (result.horizontal_offset / ref["upper_offset_m"] - 1)
    length_difference = 100 * (result.arc_length / ref["upper_length_m"] - 1)
    difference_bound = math.ceil(max(abs(offset_difference), abs(length_difference)) * 10) / 10
    return {"parameter_m": ref["parameter_m"], "upper_rise_m": ref["upper_rise_m"],
            "offset_m": result.horizontal_offset, "length_m": result.arc_length,
            "expected_offset_m": ref["upper_offset_m"],
            "expected_length_m": ref["upper_length_m"],
            "offset_difference_pct": offset_difference,
            "length_difference_pct": length_difference,
            "difference_bound_pct": difference_bound}


def example_numbers(
    inputs: dict[str, Any], riser: RiserConfiguration,
    result: CatenaryRiserResult, weight: EffectiveWeightResult,
) -> dict[str, float]:
    """Collect all report-visible example values from the actual model objects."""
    maximum_parameter = (riser.length ** 2 - result.water_depth ** 2) / (2 * result.water_depth)
    boundary = SimpleCatenaryAnalyzer().analyze_riser(
        riser, horizontal_tension=maximum_parameter * result.effective_weight,
        horizontal_offset=maximum_parameter * math.acosh(1 + result.water_depth / maximum_parameter))
    values = {key: float(value) for key, value in inputs.items() if isinstance(value, (int, float))}
    values.update({key: float(getattr(weight, key)) for key in
                   ("steel_weight", "coating_weight", "contents_weight", "buoyancy", "effective_weight")})
    values.update({key: float(getattr(result, key)) for key in
                   ("arc_length", "grounded_length", "catenary_parameter", "top_angle")})
    values.update(top_tension_kN=result.top_tension / 1000,
                  horizontal_tension_kN=result.horizontal_tension / 1000,
                  minimum_radius_m=1 / result.max_curvature,
                  offset_limit_m=float(boundary.horizontal_offset),
                  density_steel=float(riser.material.density),
                  density_contents=float(riser.internal_fluid.density),
                  density_water=float(riser.external_fluid.density))
    # Read the implementation's gravity literal, never an uncited second value.
    source = ast.parse((ROOT / PACKAGE / "models.py").read_text())
    prop = next(node for node in ast.walk(source) if isinstance(node, ast.FunctionDef)
                and node.name == "steel_weight_per_length")
    values["gravity"] = next(node.value for node in ast.walk(prop)
                              if isinstance(node, ast.Constant) and isinstance(node.value, float))
    return values


def format_number(key: str, value: float) -> str:
    """Apply engineering precision; wall and coating thickness use decimals."""
    decimals = 3 if key in {"wall_thickness", "coating_thickness", "outer_diameter"} else 1
    if key == "gravity":
        decimals = 2
    return f"{value:,.{decimals}f}".replace("-", "−")


def chart_data() -> dict[str, Any]:
    """Precompute curves through public methods; browser controls only display."""
    inputs, riser, result, _ = calculate_example()
    analyzer = SimpleCatenaryAnalyzer()
    arc, tension = analyzer.calculate_tension_along_riser(result, n_points=81)
    a = result.catenary_parameter
    shape_x = a * np.arcsinh(arc / a)
    shape_z = a * (np.sqrt(1 + (arc / a) ** 2) - 1)
    a_limit = (riser.length ** 2 - result.water_depth ** 2) / (2 * result.water_depth)
    x_limit = a_limit * math.acosh(1 + result.water_depth / a_limit)
    offsets = np.linspace(inputs["horizontal_offset"] / 2, x_limit, 61)
    required = [analyzer.analyze_riser(riser, horizontal_offset=float(offset)).arc_length
                for offset in offsets]
    return {"arc_m": arc.tolist(), "tension_kN": (tension / 1000).tolist(),
            "shape_x_m": shape_x.tolist(), "shape_z_m": shape_z.tolist(),
            "offset_m": offsets.tolist(), "required_length_m": required,
            "offset_limit_m": x_limit, "length_limit_m": riser.length}


def evidence() -> dict[str, Any]:
    """Reject stale evidence rather than repeat an obsolete passing claim."""
    record: dict[str, Any] = json.loads((ROOT / DESTINATION / "validation.json").read_text())
    for path, digest in record["source_sha256"].items():
        if hashlib.sha256((ROOT / path).read_bytes()).hexdigest() != digest:
            raise ValueError(f"Validation source changed: {path}")
    if record["failed"] or record["errors"]:
        raise ValueError("Selected validation tests did not pass")
    return record


def module_inventory() -> list[dict[str, Any]]:
    """Enumerate the inspected module and its corresponding test directories."""
    base = ROOT / "src/digitalmodel/subsea"
    rows = []
    for folder in sorted(base.iterdir()):
        if folder.is_dir() and not folder.name.startswith("_"):
            tests = ROOT / "tests/subsea" / folder.name
            rows.append({"name": folder.name.replace("_", " "),
                         "source": folder.relative_to(ROOT).as_posix(),
                         "test_files": len(list(tests.glob("test_*.py"))) if tests.exists() else 0})
    return rows


def build_report(output: Path | None = None) -> Path:
    """Render one offline HTML file using the established stylesheet verbatim."""
    inputs, riser, result, weight = calculate_example()
    values = example_numbers(inputs, riser, result, weight)
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    match = re.search(r"<style>(.*?)</style>", exemplar, re.S)
    if match is None:
        raise ValueError("The report exemplar has no inline stylesheet")
    style = match.group(1)
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    env = Environment(autoescape=True, undefined=StrictUndefined)
    template = env.from_string((ROOT / TEMPLATE).read_text(encoding="utf-8"))

    def number(key: str) -> Markup:
        value = values[key]
        return Markup('<span data-number="{}" data-value="{}">{}</span>').format(
            key, repr(value), format_number(key, value))

    rendered = template.render(
        style=Markup(brand + "\n" + style), number=number, values=values,
        chart=chart_data(), reference=public_reference(), evidence=evidence(),
        inventory=module_inventory(), reference_url=REFERENCE_URL,
        changes=json.loads((ROOT / DESTINATION / "changes.json").read_text()),
    )
    target = Path(output) if output else ROOT / DESTINATION / "subsea-module-report.html"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(rendered.rstrip() + "\n", encoding="utf-8")
    return target


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    build_report(args.output)
