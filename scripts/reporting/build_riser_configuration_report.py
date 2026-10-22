# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the static screen report using only the public synthetic example.

Run from the repository root with src on PYTHONPATH. No licensed interface
is imported. Report precision is deliberate; raw library digits are not stored.
"""

import argparse
import html
import json
import math
import re
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.riser_configuration_screen.static import scr_offsets
from digitalmodel.riser_configuration_screen.workflow import screen

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs/reports/modules/riserconfigurationscreen"
EXAMPLE = ROOT / "examples/workflows/riser-configuration-screen/scr.yml"
ANCHOR_RELATIVE_TOLERANCE = 1e-7


def read_utf8(path: Path) -> str:
    """Decode explicitly and normalize checkout line endings to LF."""
    with path.open(encoding="utf-8", newline=None) as stream:
        return stream.read()


def calculate() -> dict[str, Any]:
    """Call the documented workflow and a separate analytical check case."""
    settings = yaml.safe_load(read_utf8(EXAMPLE))["riser_configuration_screen"]
    result = screen(settings)
    result.pop("runtime_s")
    anchor = scr_offsets({"water_depth_m": 100.0, "submerged_weight_n_m": 10.0,
                          "grounded_length_m": 50.0, "hangoff_angle_deg": 30.0}, [0.0])[0]
    # Hand anchor: sin(30 degrees)=1/2, a=100 m, H=10*100 N,
    # T_top=10*(100+100) N. These literals do not come from the solver.
    reference = {"depth_m": 100.0, "horizontal_tension_n": 1000.0,
                 "top_tension_n": 2000.0, "touchdown_curvature_per_m": 0.01}
    return {"settings": settings, "result": result, "anchor": anchor,
            "reference": reference}


def display_data(data: dict[str, Any]) -> dict[str, Any]:
    """Project displayed/chart fields at fixed precision, excluding diagnostics.

    Forces retain 0.1 N, geometry 0.000001 m, curvature 1e-12 /m and
    utilisation 0.000001. Classification and ranking use unrounded results.
    """
    fields = {"top_tension_n": 1, "horizontal_tension_n": 1,
              "grounded_length_m": 6, "horizontal_span_m": 6,
              "touchdown_curvature_per_m": 12, "static_von_mises_utilisation": 6}

    def fixed(value: float, digits: int) -> float:
        return round(value, digits) or 0.0

    candidates = []
    for index, candidate in enumerate(data["result"]["ranked_candidates"]):
        rows = []
        for row in candidate["offset_results"]:
            keys = fields if index == 0 else {"static_von_mises_utilisation": 6}
            displayed: dict[str, Any] = {key: fixed(row[key], digits)
                                         for key, digits in keys.items()
                                         if key != "horizontal_tension_n"}
            if index == 0:
                offset = row["offset_m"]
                displayed["offset_m"] = ({key: fixed(value, 6) for key, value in offset.items()}
                                         if isinstance(offset, dict) else fixed(offset, 6))
                if displayed["offset_m"] == 0:
                    displayed["shape"] = [[fixed(value, 6) for value in point] for point in row["shape"]]
            rows.append(displayed)
        candidates.append({"rank": candidate["rank"], "feasible": candidate["feasible"],
                           "configuration": {key: fixed(value, 1) for key, value
                                             in candidate["configuration"].items()},
                           "curvature_range_per_m": fixed(candidate["curvature_range_per_m"], 12),
                           "offset_results": rows})
    anchor = {key: fixed(data["anchor"][key], fields[key])
              for key in data["reference"] if key != "depth_m"}
    return {"settings": data["settings"], "reference": data["reference"], "anchor": anchor,
            "result": {"grid_size": data["result"]["grid_size"], "ranked_candidates": candidates}}


def number(value: float, digits: int) -> str:
    """Format a reported value without losing a small nonzero result."""
    if value != 0 and round(value, digits) == 0:
        return f"{value:.2e}".replace("-", "\u2212")
    return f"{value:.{digits}f}".replace("-", "\u2212")


def numeric(data: dict[str, Any], path: str, digits: int = 2) -> str:
    """Bind a displayed number to its source field and precision."""
    value: Any = data
    for key in path.split("."):
        value = value[int(key)] if isinstance(value, list) else value[key]
    return f'<span data-number="{path}" data-digits="{digits}">{number(value, digits)}</span>'


def input_table(data: dict[str, Any]) -> str:
    """Render the assumed input and criterion register."""
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


def candidate_table(data: dict[str, Any]) -> str:
    """Render ranks and objectives without changing the solver order."""
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


def excursion_table(data: dict[str, Any]) -> str:
    """Render the first-ranked configuration at each supplied excursion."""
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


def house_style() -> str:
    """Inline the report layout and inherit canonical brand tokens."""
    source = read_utf8(ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html")
    style = re.search(r'<style>.*?</style>', source, re.S)
    if style is None:
        raise ValueError("House style contains no style element")
    return '<link rel="stylesheet" href="../../../api/_assets/brand.css">' + style.group(0)


def render_report() -> str:
    """Rebuild the complete document in memory before writing it."""
    data = calculate()
    for key, reference in data["reference"].items():
        if key != "depth_m" and not math.isclose(data["anchor"][key], reference,
                                                rel_tol=ANCHOR_RELATIVE_TOLERANCE):
            raise ValueError(f"analytical anchor exceeds relative tolerance: {key}")
    data = display_data(data)
    template = read_utf8(FOLDER / "report.template.html")
    changes = json.loads(read_utf8(FOLDER / "changes.json"))
    history = ''.join('<tr>' + ''.join(f'<td>{html.escape(value)}</td>' for value in
                                     (row["revision"], row["date"],
                                      row.get("history_description", row["description"]))) + '</tr>'
                      for row in changes["changes"])
    substitutions = {"STYLE": house_style(), "INPUTS": input_table(data),
                     "CANDIDATES": candidate_table(data), "EXCURSIONS": excursion_table(data),
                     "DATA": json.dumps(data, ensure_ascii=True, allow_nan=False),
                     "CHANGES": html.escape(changes["changes"][0]["description"]),
                     "HISTORY": history, "REVISION": html.escape(changes["revision"])}
    for key, value in substitutions.items():
        template = template.replace("{{" + key + "}}", value)
    template = re.sub(r"\[\[([\w.]+):(\d+)\]\]",
                      lambda match: numeric(data, match[1], int(match[2])), template)
    if "{{" in template or re.search(r"\[\[[\w.]+:\d+\]\]", template):
        raise ValueError("Unresolved report field")
    return template


def build_report(target: Path = FOLDER / "report.html") -> Path:
    """Write UTF-8 with LF line endings and verify the saved content."""
    template = render_report()
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(template, encoding="utf-8", newline="\n")
    if read_utf8(target) != template:
        raise ValueError("Report readback differs")
    return target


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=FOLDER / "report.html")
    build_report(parser.parse_args().output)
