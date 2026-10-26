# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared SVG/CSV rendering from the same unrounded, auditable data rows.

All pixel dimensions, colours, tick counts and font sizes below are presentation
choices, not engineering parameters. Display labels use one decimal place;
CSV, SVG metadata, mark attributes and manifests retain the exported precision.
"""

from __future__ import annotations

from collections import defaultdict
import csv
import json
from pathlib import Path
from typing import Any
import xml.etree.ElementTree as ET

SVG = "http://www.w3.org/2000/svg"
ET.register_namespace("", SVG)
COLORS = ("#176b9b", "#d05a21", "#328253", "#8155a3", "#a13758", "#756122")


def element(parent: ET.Element, tag: str, **attrs: Any) -> ET.Element:
    """Append a namespaced SVG element."""
    return ET.SubElement(parent, f"{{{SVG}}}{tag}", {k.replace("_", "-"): str(v) for k, v in attrs.items()})


def label(parent: ET.Element, x: float, y: float, text: str, **attrs: Any) -> ET.Element:
    """Append text, escaped by the XML serializer."""
    node = element(parent, "text", x=x, y=y, **attrs)
    node.text = text
    return node


def row(token: str, case: str, series: str, x: float, y: float,
        x_unit: str, y_unit: str, source: str) -> dict[str, Any]:
    """Create one numeric mark and its portable provenance."""
    return dict(token_id=token, case_id=case, series=series, x=x, y=y,
                x_unit=x_unit, y_unit=y_unit, source_file=source)


def write_json(path: Path, value: Any) -> None:
    """Write deterministic JSON, rejecting non-finite values."""
    path.write_text(json.dumps(value, indent=2, allow_nan=False) + "\n", encoding="utf-8")


def render_artifact(output: Path, token: str, rows: list[dict[str, Any]], *,
                    title: str, x_title: str, y_title: str,
                    categorical: bool = False, equal_scale: bool = False,
                    annotations: list[dict[str, Any]] | None = None,
                    note: str = "Exported results; method and allowable recorded in manifest.") -> dict[str, Any]:
    """Write a figure and its exact numeric table, returning its manifest entry.

    Equal scale uses positive depth downwards. Category plots use explicit
    case indices in the table and draw unconnected result marks and ranges.
    """
    if not rows:
        raise ValueError("cannot render an empty figure")
    output.mkdir(parents=True, exist_ok=True)
    csv_name, svg_name = token + ".csv", token + ".svg"
    with (output / csv_name).open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    root = ET.Element(f"{{{SVG}}}svg", {
        "viewBox": "0 0 1240 820", "width": "1240", "height": "820",
        "role": "img", "aria-labelledby": "title description"})
    element(root, "title", id="title").text = title
    element(root, "desc", id="description").text = note
    element(root, "metadata").text = json.dumps(rows, allow_nan=False)
    element(root, "rect", width=1240, height=820, fill="white")
    canvas = element(root, "g", font_family="sans-serif", font_size=13, fill="#26323b")
    label(canvas, 90, 35, title, font_size=20, font_weight="bold")
    left, top, width, height = 95.0, 80.0, 755.0, 580.0
    xs, ys = [r["x"] for r in rows], [r["y"] for r in rows]
    xmin, xmax = min(0, min(xs)), max(xs)
    ymin, ymax = min(0, min(ys)), max(ys)
    if categorical:
        xmin, xmax = min(xs) - 0.5, max(xs) + 0.5
    if xmin == xmax:
        xmax = xmin + 1
    if ymin == ymax:
        ymax = ymin + 1
    if not equal_scale:
        ymax += (ymax - ymin) * 0.08
    sx, sy = width / (xmax - xmin), height / (ymax - ymin)
    if equal_scale:
        sx = sy = min(sx, sy)
    ox = left - xmin * sx
    oy = top - ymin * sy if equal_scale else top + height + ymin * sy
    sy = sy if equal_scale else -sy
    axes = element(canvas, "g", id="axes", data_x_origin=ox, data_y_origin=oy,
                   data_x_scale=sx, data_y_scale=sy)
    element(axes, "rect", x=left, y=top, width=width, height=height,
            fill="none", stroke="#ccd4da")
    for i in range(6):
        value = ymin + (ymax-ymin)*i/5
        y = oy + value*sy
        element(axes, "line", x1=left, x2=left+width, y1=y, y2=y, stroke="#e6ebef")
        label(axes, left-12, y+4, f"{value:.1f}", text_anchor="end")
        if not categorical:
            value = xmin + (xmax-xmin)*i/5
            x = ox + value*sx
            label(axes, x, top+height+24, f"{value:.1f}", text_anchor="middle")
    label(canvas, left+width/2, 775, x_title, text_anchor="middle", font_size=15)
    label(canvas, 24, top+height/2, y_title, text_anchor="middle", font_size=15,
          transform=f"rotate(-90 24 {top+height/2})")
    cases = sorted({r["case_id"] for r in rows if r["case_id"]})
    pairs = sorted({case.rsplit("-", 1)[0] for case in cases})
    groups: dict[tuple[str, str], list[tuple[int, dict[str, Any]]]] = defaultdict(list)
    for index, datum in enumerate(rows):
        groups[(datum["case_id"], datum["series"])].append((index, datum))
    def style(case: str) -> tuple[str, str]:
        if not case:
            return "#555555", "5 4"
        color = COLORS[pairs.index(case.rsplit("-", 1)[0]) % len(COLORS)]
        return color, "8 4" if case.endswith("SLWR") else "none"
    for (case, series), points in groups.items():
        color, dash = style(case)
        if not categorical or series == "limit":
            element(canvas, "polyline", points=" ".join(
                f'{ox+r["x"]*sx:.9f},{oy+r["y"]*sy:.9f}' for _, r in points),
                fill="none", stroke=color, stroke_width=2, stroke_dasharray=dash,
                data_case=case, data_series=series)
    if equal_scale:
        for case in cases:
            buoyed = [r for r in rows if r["case_id"] == case and r.get("buoyed")]
            if buoyed:
                element(canvas, "polyline", points=" ".join(
                    f'{ox+r["x"]*sx:.9f},{oy+r["y"]*sy:.9f}' for r in buoyed),
                    fill="none", stroke=style(case)[0], stroke_width=6,
                    stroke_dasharray=style(case)[1], data_series="buoyancy",
                    data_case=case)
    if categorical:
        for case in cases:
            values = [r for r in rows if r["case_id"] == case and r["series"].startswith("dynamic_")]
            if len(values) == 2:
                element(canvas, "line", x1=ox+values[0]["x"]*sx, x2=ox+values[0]["x"]*sx,
                        y1=oy+values[0]["y"]*sy, y2=oy+values[1]["y"]*sy,
                        stroke=style(case)[0], stroke_width=2, stroke_dasharray=style(case)[1])
    for index, datum in enumerate(rows):
        color, dash = style(datum["case_id"])
        element(canvas, "circle", cx=f'{ox+datum["x"]*sx:.9f}', cy=f'{oy+datum["y"]*sy:.9f}',
                r=(6 if datum["series"] == "static" else 3) if categorical else 2,
                fill="white" if datum["series"] in {"static", "dynamic_min"} else color,
                stroke=color, stroke_width=1.5, stroke_dasharray=dash,
                data_row=index, data_x=datum["x"], data_y=datum["y"],
                data_case=datum["case_id"], data_series=datum["series"])
    for index, case in enumerate(cases):
        color, dash = style(case)
        y = 95 + index*23
        element(canvas, "line", x1=885, x2=925, y1=y, y2=y, stroke=color, stroke_width=2, stroke_dasharray=dash)
        label(canvas, 935, y+4, case, font_size=12)
        if categorical:
            datum = next(r for r in rows if r["case_id"] == case)
            x = ox+datum["x"]*sx
            label(canvas, x, top+height+20, case, font_size=11,
                  transform=f"rotate(40 {x} {top+height+20})")
    if categorical:
        series = {r["series"] for r in rows}
        if "static" in series:
            label(canvas, 885, 420, "Large open circle: static", font_size=12)
            label(canvas, 885, 440, "Small filled circle: dynamic maximum", font_size=12)
        if "dynamic_min" in series:
            label(canvas, 885, 460, "Small open circle: dynamic minimum", font_size=12)
        if "limit" in series:
            label(canvas, 885, 420, "Dashed line: normalised limit = 1.0", font_size=12)
    for index, annotation in enumerate(annotations or []):
        x, y = ox+annotation["x"]*sx, oy+annotation["y"]*sy
        ly = 425 + index*34
        element(canvas, "polyline", points=f"{x},{y} 870,{ly} 885,{ly}",
                fill="none", stroke="#6a7680", stroke_width=0.8)
        element(canvas, "rect", x=883, y=ly-16, width=345, height=24, fill="white")
        label(canvas, 890, ly, annotation["label"], font_size=12)
    label(canvas, 95, 805, note, font_size=11)
    ET.ElementTree(root).write(output / svg_name, encoding="utf-8", xml_declaration=True)
    # Keep hook-compatible terminal newlines for XML as well as JSON/CSV.
    with (output / svg_name).open("ab") as stream:
        stream.write(b"\n")
    return {"id": token, "file": svg_name, "csv_file": csv_name, "rows": rows}
