"""Standalone durable JSON/HTML output for the worn-casing evidence-gap workflow."""

from __future__ import annotations

import argparse
from html import escape
import json
from pathlib import Path

from digitalmodel.well.tubulars.casing_wear import assess_casing_wear


def _display(value) -> str:
    if value is None:
        return "Not evaluated"
    if isinstance(value, float):
        return f"{value:.1f}"
    return escape(str(value))


def _table(headers, rows) -> str:
    head = "".join(f"<th>{escape(h)}</th>" for h in headers)
    body = "".join(
        "<tr>" + "".join(f"<td>{_display(v)}</td>" for v in row) + "</tr>"
        for row in rows
    )
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def _wall(value) -> str:
    return "Not evaluated" if value is None else f"{value:.3f}"


def _inspection_section(result: dict) -> str:
    data, summary = result["inputs"], result["inspection_summary"]
    rows = [
        ["Outside diameter (mm)", data["od_mm"]],
        ["Nominal wall (mm)", _wall(data["nominal_wall_mm"])],
        ["Minimum observed wall (mm)", _wall(summary["minimum_observed_wall_mm"])],
        [
            "Absolute measurement uncertainty (mm)",
            _wall(data["measurement_uncertainty_mm"]),
        ],
        ["Lower wall bound (mm)", _wall(summary["lower_wall_bound_mm"])],
        ["Equivalent wall (mm)", summary["equivalent_wall_mm"]],
        ["Log kind", data["log_kind"]],
        ["Damage", data["damage"]],
    ]
    limits = "".join(f"<li>{escape(s)}</li>" for s in summary["limitations"])
    return (
        "<h2>Inspection</h2>"
        f"<p>Source: {escape(data['source_ref'])}. Observed: {escape(data['observed_at'])}.</p>"
        + _table(["Quantity", "Value"], rows)
        + "<p>Table 1. Supplied geometry and inspection bookkeeping at the observation time.</p>"
        + f"<ul>{limits}</ul>"
    )


def _load_section(result: dict) -> str:
    keys = (
        "name",
        "internal_pressure_mpa",
        "external_pressure_mpa",
        "axial_force_kn",
        "temperature_c",
        "burst_design_factor",
        "collapse_design_factor",
    )
    headers = [
        "Case",
        "Internal pressure (MPa)",
        "External pressure (MPa)",
        "Axial force (kN)",
        "Temperature (°C)",
        "Burst factor",
        "Collapse factor",
    ]
    rows = [[c[k] for k in keys] for c in result["inputs"]["load_cases"]]
    return (
        "<h2>Load cases</h2>"
        + _table(headers, rows)
        + "<p>Table 2. Supplied loads and factors; no acceptance limits are inferred.</p>"
    )


def _check_section(result: dict) -> str:
    sections = ["<h2>Checks and missing evidence</h2>"]
    for index, case in enumerate(result["load_cases"], start=3):
        sections.append(f"<h3>{escape(case['name'])}</h3>")
        sections.append(
            _table(
                ["Check", "Status", "Missing evidence"],
                [[c["id"], c["status"], c["missing_evidence"]] for c in case["checks"]],
            )
        )
        sections.append(
            f"<p>Table {index}. Remaining-capacity checks; all results remain unevaluated.</p>"
        )
    return "\n".join(sections)


def _source_section(result: dict) -> str:
    sections = ["<h2>Sources</h2><ul>"]
    for source in result["sources"].values():
        sections.append(
            f"<li>{escape(source['title'])}. {escape(source['publication'])}. "
            f"{escape(source['sections'])}. {escape(source['use'])}"
        )
        if "url" in source:
            sections.append(
                f" <a href='{escape(source['url'], quote=True)}'>Public study</a>."
            )
        sections.append("</li>")
    return "\n".join(sections) + "</ul>"


def render_report(result: dict) -> str:
    """Render escaped assessment values; no capacity result is implied by intake."""
    revision = _table(
        ["Revision", "Date", "Change and reason"],
        [
            [
                "Initial template",
                "2026-10-20",
                "Initial evidence-gap report template; capacity evidence is incomplete. "
                "This is the template revision date, not a report issue date.",
            ]
        ],
    )
    sections = [
        "<!doctype html><html lang='en'><meta charset='utf-8'>",
        "<title>Casing wear remaining-capacity analysis</title>",
        "<style>body{font:16px system-ui;max-width:1100px;margin:2em auto;padding:1em}"
        "table{border-collapse:collapse;width:100%;margin:1em 0}th,td{border:1px solid #aaa;"
        "padding:.5em;text-align:left;vertical-align:top}code{overflow-wrap:anywhere}</style>",
        "<h1>Casing wear remaining-capacity analysis</h1><h2>Summary</h2>",
        "<p><strong>NOT_EVALUATED.</strong> No operating pressure or remaining collapse, burst "
        "or combined-load capacity is established. Inspection arithmetic is descriptive only.</p>",
        "<h2>Revision history</h2>",
        revision,
        _inspection_section(result),
        _load_section(result),
        _check_section(result),
        "<h2>Validation and way forward</h2>",
        f"<p>{escape(result['validation']['basis'])}</p><p>{escape(result['next_action'])}</p>",
        f"<p>Canonical input SHA-256: <code>{escape(result['input_sha256'])}</code>.</p>",
        "<p>The JSON companion retains every supplied sample, load, factor and the measured ovality.</p>",
        _source_section(result),
        "</html>",
    ]
    return "\n".join(sections) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "input", type=Path, help="Inspection JSON; see the worked example"
    )
    parser.add_argument(
        "--output-directory",
        type=Path,
        required=True,
        help="New directory; existing output is never overwritten",
    )
    args = parser.parse_args()
    result = assess_casing_wear(json.loads(args.input.read_text(encoding="utf-8")))
    rendered = render_report(result)
    args.output_directory.mkdir(parents=True, exist_ok=False)
    json_path = args.output_directory / "assessment.json"
    html_path = args.output_directory / "analysis.html"
    json_path.write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    html_path.write_text(rendered, encoding="utf-8")
    if json.loads(json_path.read_text(encoding="utf-8")) != result:
        raise RuntimeError("JSON readback mismatch")
    if html_path.read_text(encoding="utf-8") != rendered:
        raise RuntimeError("HTML readback mismatch")
    print("NOT_EVALUATED: inspection retained; capacity evidence required.")


if __name__ == "__main__":
    main()
