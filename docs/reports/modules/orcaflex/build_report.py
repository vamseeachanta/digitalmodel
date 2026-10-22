"""Build the standalone analytical riser report; no native solver is used.

From the repository root: PYTHONPATH=src python docs/reports/modules/orcaflex/build_report.py
Dependencies: the orcaflex analytical package (NumPy, pandas, SciPy,
Pydantic, PyYAML). The public regression example is read, never executed.
"""
from __future__ import annotations

import ast
import hashlib
import json
import math
import re
from html import escape
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
STYLE_SOURCE = "docs/api/cfd/turbulent-flat-plate-verification.html"
SOURCE = "src/digitalmodel/orcaflex/riser_config.py"
EXAMPLE = "tests/orcaflex/test_riser_config.py"
CHANGES = {"revision": "A", "status": "Draft for client review", "changes": [
    {"section": "Detailed Results", "description":
     "Initial analytical example of submerged weight and still-water catenary geometry; operating capacity is not established."},
    {"section": "Limitations", "description":
     "The minimum-offset input and total-length allowance are distinguished from verified design criteria."}
]}


def example_arguments(name):
    tree = ast.parse((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    method = next(n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef) and n.name == name)
    call = next(n for n in ast.walk(method) if isinstance(n, ast.Call)
                and isinstance(n.func, ast.Name) and n.func.id == "SCRDesignInput")
    return {kw.arg: ast.literal_eval(kw.value) for kw in call.keywords}


def gravity():
    tree = ast.parse((ROOT / SOURCE).read_text(encoding="utf-8"))
    method = next(n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef)
                  and n.name == "submerged_weight_per_m")
    return next(ast.literal_eval(n.value) for n in ast.walk(method)
                if isinstance(n, ast.Assign) and any(isinstance(t, ast.Name)
                and t.id == "g" for t in n.targets))


def reference(model):
    """Independent algebraic arrangement of weight and catenary equilibrium."""
    from digitalmodel.orcaflex.riser_config import PIPE_GRADES
    p = model.pipe
    bore = p.outer_diameter - 2 * p.wall_thickness
    coated = p.outer_diameter + 2 * p.coating_thickness
    steel = PIPE_GRADES[p.grade.value]["density_kg_m3"]
    mass = math.pi / 4 * (steel * (p.outer_diameter**2 - bore**2)
                         + p.coating_density * (coated**2 - p.outer_diameter**2)
                         + p.contents_density * bore**2 - p.seawater_density * coated**2)
    weight = gravity() * mass
    span = model.water_depth + model.hang_off_elevation
    angle = math.radians(model.hang_off_angle_from_vertical)
    a = span * math.sin(angle) / (1 - math.sin(angle))
    horizontal = weight * a
    top = weight * (a + span)
    return {"catenary_parameter_m": a, "horizontal_tension_kN": horizontal / 1000,
            "top_tension_kN": top / 1000, "tdp_tension_kN": horizontal / 1000,
            "suspended_length_m": math.sqrt(span * (span + 2 * a)),
            "horizontal_offset_m": a * math.acosh(1 + span / a)}


def calculate():
    from digitalmodel.orcaflex.riser_config import PIPE_GRADES, SCRDesignInput
    model = SCRDesignInput(**example_arguments("test_scr_geometry_calculation"))
    upper = next(m.le for m in SCRDesignInput.model_fields["hang_off_angle_from_vertical"].metadata
                 if getattr(m, "le", None) is not None)
    second = example_arguments("test_scr_length_exceeds_depth")["hang_off_angle_from_vertical"]
    chart = []
    for angle in sorted({model.hang_off_angle_from_vertical, second, upper}):
        candidate = model.model_copy(update={"hang_off_angle_from_vertical": angle})
        chart.append({"angle": angle, "tension": candidate.calculate_geometry()["top_tension_kN"],
                      "reference": reference(candidate)["top_tension_kN"]})
    source_files = [SOURCE, EXAMPLE, "src/digitalmodel/materials/grades.py"]
    tests = ast.parse((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    return {"inputs": model.model_dump(mode="json"), "geometry": model.calculate_geometry(),
            "weight": model.pipe.submerged_weight_per_m, "reference": reference(model),
            "chart": chart, "angle_limit": upper, "gravity": gravity(),
            "steel_density": PIPE_GRADES[model.pipe.grade.value]["density_kg_m3"],
            "test_count": sum(isinstance(n, ast.FunctionDef) and n.name.startswith("test_")
                              for n in ast.walk(tests)),
            "sources": {p: hashlib.sha256((ROOT / p).read_text(encoding="utf-8").encode()).hexdigest()
                        for p in source_files}}


def numeric_values(data):
    values = {"geometry." + k: v for k, v in data["geometry"].items()}
    for key, value in data["inputs"].items():
        if isinstance(value, dict):
            values.update({"pipe." + k: v for k, v in value.items() if isinstance(v, (int, float))})
        elif isinstance(value, (int, float)):
            values["inputs." + key] = value
    values.update({k: data[k] for k in ("weight", "angle_limit", "gravity", "steel_density", "test_count")})
    return values


def display_value(key, value):
    if key == "test_count":
        return str(value)
    if key == "gravity":
        return f"{value:.5f}"
    if key.startswith("pipe.") and ("thickness" in key or "allowance" in key):
        return f"{value:.3f}"
    if key.startswith("pipe.") and "diameter" in key:
        return f"{value:.4f}"
    return f"{value:.1f}".replace("-", "−")


def numeric_span(key, values):
    return f'<span data-value="{key}">{display_value(key, values[key])}</span>'


def result_table(data):
    labels = {"catenary_parameter_m": ("Catenary parameter", "m"),
              "horizontal_tension_kN": ("Horizontal tension", "kN"),
              "top_tension_kN": ("Top tension", "kN"),
              "tdp_tension_kN": ("Touchdown tension", "kN"),
              "suspended_length_m": ("Suspended length", "m"),
              "horizontal_offset_m": ("Hang-off to touchdown distance", "m"),
              "hang_off_angle_deg": ("Hang-off angle from vertical", "degrees"),
              "estimated_total_length_m": ("Estimated total length; allowance unqualified", "m")}
    values = numeric_values(data)
    rows = "".join(f'<tr><td>{label}</td><td>{numeric_span("geometry." + key, values)}</td>'
                   f'<td>{unit}</td><td><a href="#ref-source">Analytical implementation</a></td></tr>'
                   for key, (label, unit) in labels.items())
    return '<table><thead><tr><th>Output</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>' + rows + '</tbody></table>'


def render(data):
    template = (HERE / "report_template.html").read_text(encoding="utf-8")
    example_style = (ROOT / STYLE_SOURCE).read_text(encoding="utf-8")
    css = re.search(r"<style>(.*?)</style>", example_style, re.S).group(1)
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    values = numeric_values(data)
    template = re.sub(r"\{\{number:([^}]+)\}\}", lambda m: numeric_span(m[1], values), template)
    replacements = {"style": brand + "\n" + css, "results": result_table(data),
                    "grade": escape(data["inputs"]["pipe"]["grade"]),
                    "evidence": json.dumps(data, sort_keys=True, allow_nan=False),
                    "changes": json.dumps(CHANGES, sort_keys=True),
                    "ui": (HERE / "report_ui.js").read_text(encoding="utf-8")}
    for key, value in replacements.items():
        template = template.replace("{{" + key + "}}", value)
    if "{{" in template:
        raise ValueError("Unresolved report placeholder")
    return template


def build(destination=HERE):
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    html = render(calculate())
    (destination / "report.html").write_text(html, encoding="utf-8", newline="\n")
    (destination / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")
    return destination / "report.html"


if __name__ == "__main__":
    build()
