"""Rebuild the static mooring report from the public synthetic workflow case.

Usage: PYTHONPATH=src python docs/reports/modules/dynmoormooring/generate.py
       --work-dir <disposable-output-directory>
Dependencies: source checkout, numpy and PyYAML (package imports).
"""

import argparse
import ast
import csv
import html
import json
import math
import re
from pathlib import Path

from digitalmodel.dynmoor_mooring.workflow import router
from digitalmodel.subsea.mooring_analysis import dynmoor

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
REPORT_REVISION = "3"
STYLE = "docs/api/cfd/turbulent-flat-plate-verification.html"
SOURCES = [
    "src/digitalmodel/dynmoor_mooring/workflow.py",
    "src/digitalmodel/subsea/mooring_analysis/dynmoor.py",
    "tests/subsea/mooring_analysis/test_dynmoor.py",
    "tests/subsea/mooring_analysis/test_dynmoor_workflow.py",
    "docs/domains/mooring/dynmoor-legacy-mooring.md",
    "docs/reports/modules/dynmoormooring/example.json",
    STYLE,
    "docs/api/_assets/brand.css",
]


def number(value):
    """Engineering display precision without rounding a nonzero value to zero."""
    if isinstance(value, int):
        return str(value)
    if value != 0 and abs(value) < 0.005:
        return f"{value:.2e}".replace("-", "−")
    return f"{value:.2f}".replace("-", "−")


def numeric_values(value, prefix=""):
    result = {}
    if isinstance(value, dict):
        for key, item in value.items():
            result.update(numeric_values(item, f"{prefix}.{key}".strip(".")))
    elif isinstance(value, list):
        for key, item in enumerate(value):
            result.update(numeric_values(item, f"{prefix}.{key}"))
    elif isinstance(value, (int, float)) and not isinstance(value, bool):
        result[prefix] = value
    return result


def value_span(key, value):
    return f'<span data-value="{key}">{number(value)}</span>'


def compute(work_dir):
    cfg = json.loads((HERE / "example.json").read_text(encoding="utf-8"))
    inputs = json.loads(json.dumps(cfg["dynmoor_mooring"]))
    cfg["dynmoor_mooring"]["output_dir"] = str(Path(work_dir).resolve())
    result = router(cfg)["dynmoor_mooring"]
    with Path(result["line_tables_csv"]).open(encoding="utf-8") as stream:
        table = [{key: float(value) if key != "model" else value
                  for key, value in row.items()} for row in csv.DictReader(stream)]
    return {"inputs": inputs, "lines": result["lines"],
            "offsets": result["offsets_evaluated"], "table": table,
            "benchmark": benchmark(table),
            "water_weight_factor": dynmoor.WATER_WEIGHT_FACTORS["chain"],
            "table_rows": dynmoor.N_TABLE}


def benchmark(table):
    """Read public frozen references without importing the test harness."""
    tree = ast.parse((ROOT / SOURCES[2]).read_text(encoding="utf-8"))
    reference = next(ast.literal_eval(node.value) for node in tree.body
                     if isinstance(node, ast.Assign) and any(
                         isinstance(target, ast.Name) and target.id == "CASE_A_CATENARY"
                         for target in node.targets))
    return [{"row": row[0], "reference_tension_kips": row[3],
             "calculated_tension_kips": table[row[0] - 1]["total_tension_kips"],
             "difference_percent": 100 * (
                 table[row[0] - 1]["total_tension_kips"] / row[3] - 1)}
            for row in reference]


def records_table(records, prefix, columns):
    header = "".join(f"<th>{html.escape(label)}</th>" for _, label in columns)
    rows = []
    for index, record in enumerate(records):
        cells = "".join(f'<td>{value_span(f"{prefix}.{index}.{key}", record[key])}</td>'
                        for key, _ in columns)
        rows.append(f"<tr>{cells}</tr>")
    return f"<table><thead><tr>{header}</tr></thead><tbody>{''.join(rows)}</tbody></table>"


def chart(data):
    table = data["table"]
    strength = data["inputs"]["line_types"][0]["segments"][0]["breaking_strength_kips"]
    x0, x1 = table[0]["distance_to_anchor_ft"], table[-1]["distance_to_anchor_ft"]
    ymax = max(row["total_tension_kips"] for row in table) * 1.12
    xy = lambda x, y: (65 + 680 * (x - x0) / (x1 - x0), 300 - 260 * y / ymax)
    dots, points = [], []
    for row in table:
        x, y = xy(row["distance_to_anchor_ft"], row["total_tension_kips"])
        points.append(f"{x:.3f},{y:.3f}")
        label = (f'Case synthetic spread; component chain; distance {number(row["distance_to_anchor_ft"])} ft; '
                 f'demand {number(row["total_tension_kips"])} kip; breaking strength reference '
                 f'{number(strength)} kip; allowable tension NOT_EVALUATED')
        dots.append(f'<circle tabindex="0" cx="{x:.3f}" cy="{y:.3f}" r="4" '
                    f'fill="var(--brand)" aria-label="{label}"><title>{label}</title></circle>')
    _, limit_y = xy(x0, strength)
    return f'''<svg viewBox="0 0 800 355" role="img" aria-label="Total chain tension versus horizontal span">
<path d="M65 35 V300 H745" fill="none" stroke="currentColor"/>
<line x1="65" y1="{limit_y:.3f}" x2="745" y2="{limit_y:.3f}" stroke="var(--nogo)" stroke-dasharray="6 4"/>
<text x="70" y="{limit_y - 8:.3f}">Breaking strength reference: {number(strength)} kip; not allowable</text>
<polyline points="{' '.join(points)}" fill="none" stroke="var(--brand)"/>{''.join(dots)}
<text x="65" y="320">{number(x0)}</text><text x="690" y="320">{number(x1)}</text>
<text x="260" y="347">Horizontal fairlead-to-anchor distance (ft)</text>
<text x="18" y="275" transform="rotate(-90 18 275)">Total fairlead tension (kip)</text>
<text x="40" y="304">0</text></svg>'''


def schematic(data):
    lines = data["lines"]
    items = []
    for i, line in enumerate(lines):
        fairlead = data["inputs"]["lines"][i]
        ax, ay = 400 + line["anchor_x_ft"] / 4, 170 - line["anchor_y_ft"] / 7
        fx, fy = 400 + fairlead["fairlead_x_ft"] / 4, 170 - fairlead["fairlead_y_ft"] / 7
        items.append(f'<path d="M{fx:.2f} {fy:.2f} L{ax:.2f} {ay:.2f}" stroke="var(--brand)"/>'
                     f'<circle cx="{ax:.2f}" cy="{ay:.2f}" r="5"/>'
                     f'<text x="{ax + 8:.2f}" y="{ay:.2f}">Anchor {i + 1}</text>')
    return '<svg viewBox="0 0 800 355" role="img" aria-label="Spread mooring plan and fixed anchors">' + (
        '<rect x="350" y="150" width="100" height="40" fill="none" stroke="currentColor"/>'
        '<text x="360" y="175">Hull</text><text x="290" y="18">Plan: schematic, not to scale</text>'
        '<text x="20" y="335">Fixed anchors; fairleads follow prescribed surge, sway and yaw.</text>'
        + ''.join(items) + '</svg>')


def appendix(data):
    rows = []
    for key, value in numeric_values(data).items():
        source = "Input record" if key.startswith("inputs.") else "Returned static calculation"
        if key in ("water_weight_factor", "table_rows"):
            source = "Legacy arithmetic definition"
        rows.append(f'<tr><td>{html.escape(key)}</td><td>{value_span(key, value)}</td><td>{source}</td></tr>')
    return '<table><tr><th>Field (units in field name)</th><th>Value</th><th>Source</th></tr>' + ''.join(rows) + '</table>'


def build(work_dir):
    data = compute(work_dir)
    exemplar = (ROOT / STYLE).read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group()
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    changes = {"revision": REPORT_REVISION, "changes": [
        "Static spread-mooring calculations and legacy comparison evidence are documented.",
        "Operating limits and dynamic response remain not evaluated.",
        "Equation sources, independent static checks and portable report output are documented.",
        "Report and comment revisions are synchronized, and mismatched comment revisions are flagged."
    ]}
    replacements = {
        "STYLE": f"<style>{brand}</style>{style}",
        "RESULTS": json.dumps(data, allow_nan=False),
        "CHANGES": json.dumps(changes),
        "REVISION": REPORT_REVISION,
        "CHART": chart(data), "SCHEMATIC": schematic(data),
        "APPENDIX": appendix(data),
        "PRETENSION": number(data["lines"][0]["pretension_kips"]),
        "SPAN": number(data["lines"][0]["horizontal_distance_to_anchor_ft"]),
        "SURGE": number(data["offsets"][1]["restoring_surge_kips"]),
        "WEIGHT_FACTOR": number(data["water_weight_factor"]),
        "TABLE_ROWS": str(data["table_rows"]),
        "LINE_TABLE": records_table(data["lines"], "lines", [
            ("line", "Line"), ("pretension_kips", "Pretension (kip)"),
            ("horizontal_distance_to_anchor_ft", "Horizontal span (ft)"),
            ("fairlead_angle_deg", "Fairlead angle (degrees)")]),
        "OFFSET_TABLE": records_table(data["offsets"], "offsets", [
            ("surge_ft", "Surge (ft)"), ("sway_ft", "Sway (ft)"),
            ("yaw_deg", "Yaw (degrees)"), ("restoring_surge_kips", "Surge force (kip)"),
            ("restoring_sway_kips", "Sway force (kip)"),
            ("restoring_yaw_kip_ft", "Yaw moment (kip-ft)")]),
        "BENCHMARK_TABLE": records_table(data["benchmark"], "benchmark", [
            ("row", "Table row"), ("reference_tension_kips", "Legacy reference (kip)"),
            ("calculated_tension_kips", "Calculated tension (kip)"),
            ("difference_percent", "Difference from reference (%)")]),
        "SOURCES": source_table(),
    }
    document = (HERE / "report.template.html").read_text(encoding="utf-8")
    for key, value in replacements.items():
        document = document.replace("{{" + key + "}}", value)
    if re.search(r"\{\{[A-Z_]+\}\}", document):
        raise ValueError("Unresolved report field")
    return document, changes


def source_table():
    rows = []
    for path in SOURCES:
        rows.append(f'<tr><td><a href="../../../../{path}">{path}</a></td></tr>')
    return '<table><tr><th>Repository source</th></tr>' + ''.join(rows) + '</table>'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    document, changes = build(args.work_dir)
    (HERE / "report.html").write_text(document, encoding="utf-8", newline="\n")
    (HERE / "changes.json").write_text(json.dumps(changes, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
