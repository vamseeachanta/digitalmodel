"""Build the static screen report using only the public synthetic example.

Run from the repository root with src on PYTHONPATH. No licensed interface
is imported. Report precision is deliberate; raw library digits are not stored.
"""

import argparse
import html
import json
import re
from pathlib import Path

import yaml

from digitalmodel.riser_configuration_screen.static import scr_offsets
from digitalmodel.riser_configuration_screen.workflow import screen

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs/reports/modules/riserconfigurationscreen"
EXAMPLE = ROOT / "examples/workflows/riser-configuration-screen/scr.yml"


def calculate():
    """Call the documented workflow and a separate analytical check case."""
    settings = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))["riser_configuration_screen"]
    result = screen(settings)
    result.pop("runtime_s")
    anchor = scr_offsets({"water_depth_m": 100.0, "submerged_weight_n_m": 10.0,
                          "grounded_length_m": 50.0, "hangoff_angle_deg": 30.0}, [0.0])[0]
    # Hand anchor: sin(30 degrees)=1/2, a=100 m, H=10*100 N,
    # T_top=10*(100+100) N. These literals do not come from the solver.
    reference = {"depth_m": 100.0, "horizontal_tension_n": 1000.0,
                 "top_tension_n": 2000.0, "touchdown_curvature_per_m": 0.01}
    differences = {key: 100 * (anchor[key] / reference[key] - 1)
                   for key in reference if key != "depth_m"}
    return {"settings": settings, "result": result, "anchor": anchor,
            "reference": reference, "differences_percent": differences}


def display_data(value):
    """Round significant digits to avoid platform-dependent final digits."""
    if isinstance(value, dict):
        return {key: display_data(item) for key, item in value.items()}
    if isinstance(value, list):
        return [display_data(item) for item in value]
    if isinstance(value, float):
        return float(f"{value:.8g}") if abs(value) >= 1e-9 else 0.0
    return value


def number(value, digits):
    if value != 0 and round(value, digits) == 0:
        return f"{value:.2e}".replace("-", "\u2212")
    return f"{value:.{digits}f}".replace("-", "\u2212")


def numeric(data, path, digits=2):
    value = data
    for key in path.split("."):
        value = value[int(key)] if isinstance(value, list) else value[key]
    return f'<span data-number="{path}" data-digits="{digits}">{number(value, digits)}</span>'


def input_table(data):
    labels = {
        "water_depth_m": ("Water depth", "m", 1),
        "submerged_weight_n_m": ("Submerged weight per length", "N/m", 1),
        "grounded_length_m": ("Initial grounded tail", "m", 1),
        "pipe.outer_diameter_m": ("Outside diameter", "m", 3),
        "pipe.wall_thickness_m": ("Wall thickness", "m", 3),
        "pipe.youngs_modulus_pa": ("Young's modulus", "Pa", 0),
        "pipe.yield_strength_pa": ("Yield strength", "Pa", 0),
        "pipe.internal_pressure_pa": ("Internal pressure", "Pa", 0),
        "pipe.external_pressure_pa": ("External pressure", "Pa", 0),
        "constraints.max_top_tension_n": ("Top tension limit", "N", 0),
        "constraints.min_bend_radius_m": ("Minimum bend radius", "m", 1),
        "constraints.min_hog_clearance_m": ("Minimum hog clearance (not applicable)", "m", 1),
        "constraints.max_static_utilisation": ("Static utilisation limit", "dimensionless", 2),
    }
    rows = [f'<tr><td>{label}</td><td>{numeric(data, "settings." + key, digits)}</td>'
            f'<td>{unit}</td><td>Public synthetic example; section 9.2</td></tr>'
            for key, (label, unit, digits) in labels.items()]
    return '<table><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>' + ''.join(rows) + '</tbody></table>'


def candidate_table(data):
    rows = []
    for index, candidate in enumerate(data["result"]["ranked_candidates"]):
        prefix = f"result.ranked_candidates.{index}."
        cells = [numeric(data, prefix + "rank", 0),
                 numeric(data, prefix + "configuration.hangoff_angle_deg", 1),
                 numeric(data, prefix + "curvature_range_per_m", 8),
                 "The supplied screening criteria are met." if candidate["feasible"]
                 else "The supplied screening criteria are not met."]
        rows.append('<tr>' + ''.join(f'<td>{cell}</td>' for cell in cells) + '<td>Module calculation; section 9.2</td></tr>')
    return '<table><thead><tr><th>Rank</th><th>Initial angle (degrees)</th><th>Curvature range (1/m)</th><th>Disposition</th><th>Source</th></tr></thead><tbody>' + ''.join(rows) + '</tbody></table>'


def excursion_table(data):
    rows = []
    for index, row in enumerate(data["result"]["ranked_candidates"][0]["offset_results"]):
        prefix = f"result.ranked_candidates.0.offset_results.{index}."
        offset = row["offset_m"]
        label = ("Cross excursion" if isinstance(offset, dict) else
                 ("Near excursion" if offset < 0 else "Far excursion" if offset > 0 else "Initial position"))
        cells = [label] + [numeric(data, prefix + key, digits) for key, digits in (
            ("top_tension_n", 1), ("grounded_length_m", 2),
            ("touchdown_curvature_per_m", 8), ("static_von_mises_utilisation", 4))]
        rows.append('<tr>' + ''.join(f'<td>{cell}</td>' for cell in cells) + '<td>Module calculation; section 9.2</td></tr>')
    return '<table><thead><tr><th>Position</th><th>Top tension (N)</th><th>Grounded length (m)</th><th>Touchdown curvature (1/m)</th><th>Utilisation (dimensionless)</th><th>Source</th></tr></thead><tbody>' + ''.join(rows) + '</tbody></table>'


def house_style():
    source = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    # Copy existing rules, omit source comments from the distributed document.
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S)
    return '<style>' + brand + '</style>' + re.search(r'<style>.*?</style>', source, re.S).group(0)


def render_report():
    """Rebuild the complete document in memory before writing it."""
    data = calculate()
    template = (FOLDER / "report.template.html").read_text(encoding="utf-8")
    changes = json.loads((FOLDER / "changes.json").read_text(encoding="utf-8"))
    substitutions = {"STYLE": house_style(), "INPUTS": input_table(data),
                     "CANDIDATES": candidate_table(data), "EXCURSIONS": excursion_table(data),
                     "DATA": json.dumps(display_data(data), ensure_ascii=True, allow_nan=False),
                     "CHANGES": html.escape(changes["changes"][0]["description"])}
    for key, value in substitutions.items():
        template = template.replace("{{" + key + "}}", value)
    template = re.sub(r"\[\[([\w.]+):(\d+)\]\]",
                      lambda match: numeric(data, match[1], int(match[2])), template)
    if "{{" in template or re.search(r"\[\[[\w.]+:\d+\]\]", template):
        raise ValueError("Unresolved report field")
    return template


def build_report(target=FOLDER / "report.html"):
    template = render_report()
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(template, encoding="utf-8", newline="\n")
    if target.read_text(encoding="utf-8") != template:
        raise ValueError("Report readback differs")
    return target


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=FOLDER / "report.html")
    build_report(parser.parse_args().output)
