"""Reproducible table exports and an escaped, offline engineering report."""

from __future__ import annotations

import csv
from html import escape
import json
from pathlib import Path
from tempfile import TemporaryDirectory

from digitalmodel.lifting_lug.welded_study import UNITS, validate


def _intro(data):
    rows = data["rows"]
    valid = sum(row["status"] == "CALCULATED_SCREENING" for row in rows)
    evaluated = sum(
        d["status"] == "CALCULATED_SCREENING"
        for row in rows
        for d in row["directions"].values()
    )
    title = "Welded-padeye parametric demand analysis"
    html = [
        f'<!doctype html><html lang="en"><meta charset="utf-8"><title>{title}</title>',
        "<style>body{max-width:1200px;margin:2em auto;font:16px/1.5 sans-serif}",
        "table{border-collapse:collapse}th,td{border:1px solid #999;padding:.3em}",
        "caption{caption-side:bottom}svg{max-width:100%;height:auto}</style>",
        f"<h1>{title}</h1><h2>Summary</h2><p>{len(rows)} cases; {valid} calculated cases; ",
        f"{evaluated} calculated directions. Historical benchmark: UNVERIFIED_SOURCE. ",
        "The results establish nominal elastic demand for an idealized rectangular plate only. ",
        "No capacity, utilization or design acceptance is established.</p>",
        "<h2>Revision history</h2><table><caption>Table 1. Content revision and issue reason.</caption>",
        "<tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>",
        f"<tr><td>{escape(data['revision'])}</td><td>{escape(data['criteria_revision'])}</td>",
        "<td>Initial screening study; explicit inputs retain unresolved historical qualification.</td></tr>",
        "</table>",
        "<h2>Basis and method</h2><p>Positive Y is upward; X is in-plane horizontal; ",
        "positive moment follows the right-hand rule. Angles are from upward vertical. ",
        "The gross load is unfactored. Fy = gross × factor / lug count; Fx = ±Fy tan(angle); ",
        "Mz = rx Fy − ry Fx. Two straight base welds are co-projected in-plane. ",
        "L = 2B; J = B³/6; q = (Fx/L, Fy/L + Mz x/J); ",
        "qmax occurs at a line endpoint. Throat = leg/√2. ",
        "Plate stress = Fy/(Bt) ± 6Mz/(tB²); average shear = Fx/(Bt).</p>",
        "<p>Thickness varies independently. Exterior scaling is about the hole followed by ",
        "base-centroid translation; the hole radius and baseline weld leg remain fixed. ",
        "The multiplier changes only the weld leg. Exact ties select positive direction. ",
        "Zero-angle negative direction is NOT_APPLICABLE. Independent historical response ",
        "verification remains NOT_EVALUATED for every response.</p>",
        "<h3>Required input basis</h3><dl>",
    ]
    return html


def _basis(data):
    html = []
    for group in ("geometry", "load"):
        for key, value in data[group].items():
            html.append(
                f"<dt>{escape(group)}.{escape(key)}</dt><dd>{escape(str(value))}</dd>"
            )
    html.append("</dl><h2>Limitations and way forward</h2><ul>")
    for value in data["limitations"].values():
        html.append(f"<li>{escape(value)}</li>")
    html.append(
        "</ul><p>Obtain qualified historical geometry and weld details, resolve the erratum, ",
    )
    html.append(
        "then compare an independent baseline before assigning benchmark reproduction status. ",
    )
    html.append(
        "Supply material, clearance and governing criteria before any design check.</p>"
    )
    return html


def _chart(data):
    rows = data["rows"]
    html = []
    selected = [
        r for r in rows if r["key"][0] == "1.25" and r["key"][2:] == ["1", "45"]
    ]
    html.append(
        '<h2>Fixed-slice weld demand</h2><svg viewBox="0 0 700 220" role="img" ',
    )
    html.append(
        'aria-label="Maximum weld line demand versus exterior scale; thickness 1.250 in, weld multiplier 1, angle 45 degrees">'
    )
    maximum = max(
        (
            r["envelopes"]["qmax_kip_per_in"]["value"]
            for r in selected
            if r["envelopes"]
        ),
        default=0,
    )
    for i, row in enumerate(selected):
        x = 40 + i * 130  # Display coordinates, not engineering constants.
        value = (
            row["envelopes"]["qmax_kip_per_in"]["value"] if row["envelopes"] else None
        )
        height = 120 * value / maximum if value is not None and maximum else 0
        html.append(
            f'<rect x="{x}" y="{160 - height:.1f}" width="70" height="{height:.1f}" fill="#236578"/>'
        )
        label = f"{value:.1f}" if value is not None else "invalid"
        html.append(
            f'<text x="{x}" y="180">{row["key"][1]}</text><text x="{x}" y="{150 - height:.1f}">{label}</text>'
        )
    html.append(
        "</svg><p>Figure 1. qmax (kip/in) versus exterior scale; thickness 1.250 in, weld multiplier 1.0, angle 45.0°.</p>"
    )
    return html


def _table(data):
    rows = data["rows"]
    html = []
    html.append(
        "<h2>Complete case envelopes</h2><table><caption>Table 2. Signed maximum-absolute envelopes; invalid cases retain null demands in JSON/CSV. Demand display precision is one decimal place; thickness is shown to three decimals.</caption><tr><th>t (in), scale, weld multiplier, angle (°)</th><th>Status</th>"
    )
    for name, unit in UNITS.items():
        html.append(f"<th>{escape(name)} ({escape(unit)})</th>")
    html.append("</tr>")
    for row in rows:
        display_key = [f"{float(row['key'][0]):.3f}", *row["key"][1:]]
        html.append(
            f"<tr><td>{escape(', '.join(display_key))}</td><td>{row['status']}</td>"
        )
        for name in UNITS:
            envelope = row["envelopes"][name] if row["envelopes"] else None
            value = (
                f"{envelope['value']:.1f} ({envelope['direction']})"
                if envelope
                else "NOT_EVALUATED"
            )
            html.append(f"<td>{value}</td>")
        html.append("</tr>")
    html.append("</table><h2>Sources</h2><ul>")
    for source in data["citations"]:
        html.append(
            f'<li><a href="{escape(source["url"], quote=True)}">{escape(source["document"])}</a>, {escape(source["edition"])}, {escape(source["section"])}: {escape(source["use"])}</li>'
        )
    html.append("</ul></html>")
    return html


def render_report(data):
    """Render the complete case table with qualified basis and a fixed-slice chart."""
    validate(data)
    return "".join(_intro(data) + _basis(data) + _chart(data) + _table(data))


def _write_staged(data, directory):
    serialized = json.dumps(data, indent=2, allow_nan=False) + "\n"
    with (directory / "study.json").open("x", encoding="utf-8", newline="\n") as stream:
        stream.write(serialized)
    with (directory / "study.csv").open("x", encoding="utf-8", newline="\n") as stream:
        fields = [
            "revision",
            "thickness_in",
            "scale",
            "weld_multiplier",
            "angle_deg",
            "direction",
            "status",
            "reason",
            *UNITS,
        ]
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for row in data["rows"]:
            for name, direction in row["directions"].items():
                writer.writerow(
                    dict(zip(fields[:5], [data["revision"], *row["key"]]))
                    | {
                        "direction": name,
                        "status": direction["status"],
                        "reason": direction["reason"] or "",
                        **(direction["values"] or {}),
                    }
                )
    with (directory / "report.html").open(
        "x", encoding="utf-8", newline="\n"
    ) as stream:
        stream.write(render_report(data))
    # Readback validates serialized numerical values and the content-addressed revision.
    validate(json.loads((directory / "study.json").read_text(encoding="utf-8")))


def write_outputs(data, directory):
    """Publish all three validated outputs together into a new or empty directory."""
    validate(data)
    directory = Path(directory).resolve()
    if directory.exists() and (not directory.is_dir() or any(directory.iterdir())):
        raise FileExistsError(
            "study outputs already exist; use a new revision directory"
        )
    directory.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(prefix=".welded-study-", dir=directory.parent) as temporary:
        staged = Path(temporary) / "outputs"
        staged.mkdir()
        _write_staged(data, staged)
        if directory.exists():
            directory.rmdir()  # Only an empty directory can be removed.
        staged.rename(directory)
