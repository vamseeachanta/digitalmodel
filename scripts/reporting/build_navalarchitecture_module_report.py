# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the self-contained naval architecture report from actual module calls.

Usage: PYTHONPATH=src python scripts/reporting/build_navalarchitecture_module_report.py
Use --check to verify the retained HTML without writing it.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
from importlib import metadata
import platform
import math
import re
import xml.etree.ElementTree as ET
from pathlib import Path
from string import Template

from digitalmodel.naval_architecture.stability import G, LT_TO_LB, free_surface_correction

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/navalarchitecture"
STYLE_EXEMPLAR = ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html"
REVISION = "03"
DATE = "2026-10-22"


# Suite counts are a historical receipt, not a freshness gate for unrelated code.
# Only these text inputs determine the calculation and presentation in this report.
REPORT_INPUTS = (
    "scripts/reporting/build_navalarchitecture_module_report.py",
    "src/digitalmodel/naval_architecture/stability.py",
    "tests/naval_architecture/test_module_report.py",
    "docs/reports/modules/navalarchitecture/example.json",
    "docs/reports/modules/navalarchitecture/report.template.html",
    "docs/api/cfd/turbulent-flat-plate-verification.html",
    "docs/api/_assets/brand.css",
)


def evidence_digest() -> str:
    """Hash explicit POSIX-relative input names and LF-normalized text bytes."""
    digest = hashlib.sha256()
    for relative in sorted(REPORT_INPUTS):
        digest.update(relative.encode("utf-8") + b"\0")
        digest.update((ROOT / relative).read_bytes().replace(b"\r\n", b"\n"))
        digest.update(b"\0")
    return digest.hexdigest()


def record_checks(junit_path: Path, *, command: str) -> None:
    """Retain aggregate outcomes without local directories or personal metadata."""
    cases = list(ET.parse(junit_path).iter("testcase"))
    if not cases or any(not (case.get("classname", "").startswith("tests.naval_architecture.") or
                            (case.find("skipped") is not None and
                             case.get("name", "").startswith("tests.naval_architecture.")))
                        for case in cases):
        raise ValueError("Expected a naval architecture test receipt")
    failed = sum(case.find("failure") is not None or case.find("error") is not None for case in cases)
    skipped = [item for case in cases if (item := case.find("skipped")) is not None]
    expected = sum(item.get("type") == "pytest.xfail" for item in skipped)
    packages = {distribution.metadata["Name"].lower(): distribution.version
                for distribution in metadata.distributions()}
    extras = ["cad"] if {"rhino3dm", "cadquery-ocp"} <= packages.keys() else []
    record = {"date": DATE, "scope": "Historical naval architecture suite outcomes; report input manifest",
              "command": command,
              "environment": {"python": platform.python_version(),
                              "implementation": platform.python_implementation(),
                              "extras": extras, "packages": dict(sorted(packages.items()))},
              "source_files": sorted(REPORT_INPUTS),
              "source_digest": evidence_digest(), "tests": len(cases),
              "passed": len(cases) - failed - len(skipped), "failed": failed,
              "skipped": len(skipped) - expected, "expected_failures": expected,
              "qualification": "Software checks; no operational stability acceptance"}
    target = REPORT / "checks.json"
    target.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    if json.loads(target.read_text(encoding="utf-8")) != record:
        raise OSError("Check-record read-back differs")


def format_number(value: float, key: str) -> str:
    """Apply report precision without rounding a nonzero result to zero."""
    precision = 1 if key.endswith("pct") else 3
    text = f"{value:,.{precision}f}"
    if value and float(text.replace(",", "")) == 0:
        text = f"{value:.2e}"
    return text.replace("-", "−")


def worked_example() -> dict:
    """Call the documented API at the public condition and each sensitivity point."""
    source = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    inputs = source["inputs"]
    for value in inputs.values():
        if not math.isfinite(value) or value <= 0:
            raise ValueError("The public example requires finite positive inputs")
    if inputs["km_ft"] <= inputs["kg_ft"]:
        raise ValueError("The public example requires positive uncorrected metacentric height")
    constants = source["constants"]
    if G != constants["g_ft_s2"]["value"] or LT_TO_LB != constants["lb_per_long_ton"]["value"]:
        raise ValueError("Module conversion constants differ from the cited source")
    correction, gm = free_surface_correction(**inputs)
    _, solid_gm = free_surface_correction(**dict(inputs, tank_breadth_ft=0.0))
    sensitivity = source["sensitivity"]
    count = sensitivity["sample_count"]
    sweep = []
    for index in range(count):
        fraction = sensitivity["breadth_fraction_min"] + index / (count - 1) * (
            sensitivity["breadth_fraction_max"] - sensitivity["breadth_fraction_min"]
        )
        breadth = inputs["tank_breadth_ft"] * fraction
        loss, effective = free_surface_correction(**dict(inputs, tank_breadth_ft=breadth))
        sweep.append({"breadth_ft": breadth, "correction_ft": loss, "effective_gm_ft": effective})
    return {
        "inputs": inputs,
        "results": {"correction_ft": correction, "effective_gm_ft": gm, "solid_gm_ft": solid_gm},
        "derived": {"reduction_pct": 100 * correction / solid_gm,
                    "neutral_gm_ft": constants["neutral_gm_ft"]["value"]},
        "geometry": {key: value for key, value in source["geometry"].items() if key != "source"},
        "constants": {"g_ft_s2": G, "lb_per_long_ton": LT_TO_LB},
        "sweep": sweep,
        "source": source["source"],
    }


def number(value: float, key: str, *, tag: str = "span") -> str:
    """Bind each visible worked-example number to its unrounded value."""
    return (f'<{tag} data-number="{key}" data-value="{value}">'
            f'{format_number(value, key)}</{tag}>')


def table(headers: list[str], rows: list[list[str]], caption: str) -> str:
    """Render real table columns with a caption below the table."""
    head = "".join(f"<th scope=\"col\">{item}</th>" for item in headers)
    body = "".join("<tr>" + "".join(f"<td>{item}</td>" for item in row) + "</tr>" for row in rows)
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody><caption>{caption}</caption></table>"


def chart(data: dict) -> str:
    """Embed an offline SVG chart; hover and keyboard focus use the same records."""
    points = data["sweep"]
    left, right, top, bottom = 70, 810, 30, 285
    x_min, x_max = points[0]["breadth_ft"], points[-1]["breadth_ft"]
    y_max = math.ceil(data["results"]["solid_gm_ft"])
    x = lambda value: left + (value - x_min) / (x_max - x_min) * (right - left)
    y = lambda value: bottom - value / y_max * (bottom - top)
    coords = " ".join(f"{x(p['breadth_ft']):.2f},{y(p['effective_gm_ft']):.2f}" for p in points)
    markers = []
    for index, point in enumerate(points):
        label = (f"Public tank sensitivity; rectangular free surface; breadth {point['breadth_ft']:.3f} ft; "
                 f"effective metacentric height {point['effective_gm_ft']:.3f} ft; "
                 f"neutral boundary {data['derived']['neutral_gm_ft']:.3f} ft; "
                 f"correction {point['correction_ft']:.3f} ft; operational criterion not supplied.")
        markers.append(f'<circle cx="{x(point["breadth_ft"]):.2f}" cy="{y(point["effective_gm_ft"]):.2f}" '
                       f'r="4" tabindex="0" role="img" class="sample" data-index="{index}" '
                       f'aria-label="{html.escape(label)}"><title>{html.escape(label)}</title></circle>')
    ticks = []
    for value in range(0, y_max + 1, 5):
        ticks.append(f'<text x="60" y="{y(value)+4:.2f}" text-anchor="end">{value:.1f}</text>')
    for point in (points[0], points[len(points)//2], points[-1]):
        value = point["breadth_ft"]
        ticks.append(f'<text x="{x(value):.2f}" y="310" text-anchor="middle">{value:.1f}</text>')
    return (f'<svg id="gm-chart" viewBox="0 0 860 360" role="group" aria-labelledby="chart-title chart-desc">'
            '<title id="chart-title">Effective metacentric height against free-surface breadth</title>'
            '<desc id="chart-desc">One loading condition; all other inputs held fixed. '
            'The dashed line is the neutral initial stability boundary, not an operational criterion.</desc>'
            f'<path d="M{left} {top}V{bottom}H{right}" class="axis"/>'
            f'<path d="M{left} {bottom}H{right}" class="boundary"/>'
            f'<polyline points="{coords}" class="curve"/>' + "".join(markers + ticks) +
            '<text x="440" y="345" text-anchor="middle">Free-surface breadth (ft)</text>'
            '<text x="17" y="165" text-anchor="middle" transform="rotate(-90 17 165)">'
            'Effective metacentric height (ft)</text></svg>')


def inline_house_style() -> str:
    """Read the existing stylesheet and fail explicitly if the exemplar changes."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    match = re.search(r"<style>(.*?)</style>", STYLE_EXEMPLAR.read_text(encoding="utf-8"), re.S)
    if match is None:
        raise ValueError("The house-style exemplar has no stylesheet")
    return (f'<style data-source="docs/api/_assets/brand.css">{brand}</style>\n'
            f'<style>{match.group(1)}</style>')


def report_files() -> dict[str, str]:
    """Render the complete document and its embedded change-panel record."""
    data = worked_example()
    source = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    values = {**data["inputs"], **data["results"], **data["derived"],
              **data["geometry"], **data["constants"]}
    replacements = {key: number(value, key) for key, value in values.items()}
    replacements.update({f"svg_{key}": number(values[key], key, tag="tspan")
                         for key in ("tank_breadth_ft", "tank_depth_ft", "tank_length_ft")})
    labels = [("Displacement", "displacement_lt", "long ton"),
              ("Gravity centre above keel", "kg_ft", "ft"),
              ("Transverse metacentre above keel", "km_ft", "ft"),
              ("Tank length", "tank_length_ft", "ft"),
              ("Tank free-surface breadth", "tank_breadth_ft", "ft"),
              ("Fuel mass density", "rho_fluid", "slug/ft³")]
    replacements["input_table"] = table(
        ["Quantity", "Value", "Unit", "Source"],
        [[label, replacements[key], unit, '<a href="#ref-public">Reference A, problem 26</a>']
         for label, key, unit in labels],
        "Table A. Public example inputs; the original units are retained at the API boundary.")
    replacements["register"] = table(
        ["Item", "Status", "Basis"],
        [[html.escape(row[key]) for key in ("item", "status", "basis")]
         for row in source["data_register"]], "Table B. Missing-data and assumption register.")
    checks = json.loads((REPORT / "checks.json").read_text(encoding="utf-8"))
    if checks["source_digest"] != evidence_digest():
        raise ValueError("The retained check record does not match the current source files")
    replacements["check_table"] = table(
        ["Software check outcome", "Count", "Source"],
        [[label, str(checks[key]), "Retained check record"] for label, key in
         [("Passed", "passed"), ("Failed", "failed"), ("Skipped", "skipped"),
          ("Expected failure", "expected_failures"), ("Total", "tests")]],
        "Table C. Historical software checks in the recorded environment; these counts do not denote current suite results or engineering acceptance.")
    replacements["example_data"] = json.dumps(data, sort_keys=True).replace("<", "\\u003c")
    changes = {"document": "navalarchitecture-module-report", "revision": REVISION,
               "date": DATE, "changes": [
                   {"section": "summary", "description": "Revision history preserves earlier issues and records replacement software-check evidence within declared dependency bounds."},
                   {"section": "example", "description": "The calculation and its independent hand checks are unchanged."},
                   {"section": "validation", "description": "The software-check receipt uses declared pandas and Pillow versions; ignore rules preserve earlier module exceptions and keep saved comments untracked."}]}
    replacements["changes_data"] = json.dumps(changes, sort_keys=True)
    replacements["revision"] = REVISION
    replacements["date"] = DATE
    replacements["chart"] = chart(data)
    replacements["house_style"] = inline_house_style()
    text = Template((REPORT / "report.template.html").read_text(encoding="utf-8")).substitute(replacements)
    digest = hashlib.sha256(text.encode()).hexdigest()
    text = text.replace("CONTENT_DIGEST", digest)
    return {"report.html": text, "changes.json": json.dumps(changes, indent=2) + "\n"}


def build_report(output_dir: Path = REPORT, *, check: bool = False) -> None:
    """Write only the two generated deliverables, or check them without writes."""
    files = report_files()
    if not check:
        output_dir.mkdir(parents=True, exist_ok=True)
    for name, text in files.items():
        target = output_dir / name
        if check:
            if not target.is_file() or target.read_text(encoding="utf-8") != text:
                raise ValueError(f"Report requires rebuilding: {name}")
        else:
            target.write_text(text, encoding="utf-8")
            if target.read_text(encoding="utf-8") != text:
                raise OSError(f"Report read-back differs: {name}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=REPORT)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--record-checks", type=Path,
                        help="Retain aggregate outcomes from an actual module-wide JUnit receipt before building")
    parser.add_argument("--test-command", help="Exact test command with portable output-directory variables")
    args = parser.parse_args()
    if args.record_checks:
        if args.check:
            parser.error("--record-checks writes evidence and cannot be combined with --check")
        if not args.test_command:
            parser.error("--record-checks requires --test-command for receipt provenance")
        record_checks(args.record_checks, command=args.test_command)
    build_report(args.output_dir, check=args.check)
