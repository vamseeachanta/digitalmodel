# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Shared thickness tables, deterministic input provenance and limit figures.

No code constants or acceptance tables are held here. A utilisation limit of
one is the definition required/nominal <= 1, not a material acceptance rule.
Domain adapters supply the equations, quantities, sources and applicability.
"""

from __future__ import annotations

from dataclasses import dataclass
import hashlib
import json
import math

from digitalmodel.reporting.figures import figure_from_columns
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.spec import DocumentMeta, Section, TableBlock


def validate_number(name: str, value: float, *, zero: bool = False) -> None:
    """Reject nonfinite or nonpositive inputs; optionally permit zero."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or (value < 0 if zero else value <= 0):
        raise ValueError(
            f"{name} must be finite and {'nonnegative' if zero else 'positive'}"
        )


@dataclass(frozen=True)
class ThicknessRow:
    label: str
    design_mm: float
    test_mm: float | None
    floor_mm: float | None
    required_mm: float
    nominal_mm: float
    corrosion_mm: float
    governing: str


def thickness_table(
    title: str,
    rows: list[ThicknessRow],
    source: str,
    *,
    extra_columns: tuple[str, ...] = (),
    extra_rows: tuple[tuple, ...] = (),
) -> TableBlock:
    """Format thickness and corrosion to three decimals; ratios to 0.1 %."""
    if extra_columns and len(extra_rows) != len(rows):
        raise ValueError("extra course rows must match thickness rows")
    columns = [
        "Component",
        *extra_columns,
        "Design [mm]",
        "Test [mm]",
        "Floor [mm]",
        "Required [mm]",
        "Nominal [mm]",
        "CA [mm]",
        "Margin [mm]",
        "Utilisation",
        "Governing",
    ]
    body = []
    for index, row in enumerate(rows):
        extras = list(extra_rows[index]) if extra_columns else []
        body.append(
            [
                row.label,
                *extras,
                f"{row.design_mm:.3f}",
                "Not Evaluated" if row.test_mm is None else f"{row.test_mm:.3f}",
                "Not Evaluated" if row.floor_mm is None else f"{row.floor_mm:.3f}",
                f"{row.required_mm:.3f}",
                f"{row.nominal_mm:.3f}",
                f"{row.corrosion_mm:.3f}",
                f"{row.nominal_mm-row.required_mm:.3f}",
                f"{row.required_mm/row.nominal_mm:.1%}",
                row.governing,
            ]
        )
    return TableBlock(title=title, columns=columns, rows=body, source=source)


def limit_figure(
    x,
    series: dict,
    *,
    title: str,
    x_label: str,
    y_label: str,
    limit: float,
    limit_label: str,
    kind: str = "line",
) -> dict:
    figure = figure_from_columns(
        kind, x, series, title=title, x_label=x_label, y_label=y_label
    )
    figure["layout"]["shapes"] = [
        {
            "type": "line",
            "xref": "paper",
            "x0": 0,
            "x1": 1,
            "y0": limit,
            "y1": limit,
            "line": {"dash": "dash", "color": "firebrick"},
        }
    ]
    figure["layout"]["annotations"] = [
        {
            "xref": "paper",
            "x": 1,
            "y": limit,
            "text": limit_label,
            "showarrow": False,
            "yshift": 12,
        }
    ]
    figure["layout"]["yaxis"]["tickformat"] = ".3g"
    return figure


def input_provenance(identifier: str, inputs: dict) -> Provenance:
    canonical = json.dumps(
        inputs, sort_keys=True, separators=(",", ":"), allow_nan=False
    ).encode()
    digest = "sha256:" + hashlib.sha256(canonical).hexdigest()
    return Provenance().add(
        "calculation-inputs",
        identifier,
        digest=digest,
        description="Digest of canonical input echo; not source qualification",
    )


def nominal_limit_hover(figure: dict, limits: dict[str, list[float]]) -> dict:
    """Pair requirement points with nominal walls, including negative margins.

    Nominal minus required is a geometric margin, not a code acceptance rule.
    Repeated course-boundary heights retain their own selected plate values.
    """
    traces = {trace["name"]: trace for trace in figure["data"]}
    for name, nominal in limits.items():
        if name not in traces:
            raise ValueError(f"Missing requirement series: {name}")
        trace = traces[name]
        if len(nominal) != len(trace["y"]):
            raise ValueError("nominal limits must match requirement points")
        for value in nominal:
            validate_number("nominal limit", value)
        trace["customdata"] = [
            [wall, wall - required] for wall, required in zip(nominal, trace["y"])
        ]
        trace["hovertemplate"] = (
            "%{fullData.name}<br>Coordinate: %{x}<br>Required: %{y:.3f} mm"
            "<br>Nominal wall: %{customdata[0]:.3f} mm"
            "<br>Margin: %{customdata[1]:.3f} mm<extra></extra>"
        )
    return figure


def revision_section(document: DocumentMeta) -> Section:
    return Section(
        key="revision-history",
        title="Revision history",
        blocks=[
            TableBlock(
                title="Revision history",
                columns=["Revision", "Date", "Change and reason"],
                rows=[
                    [row.rev, row.date, row.description]
                    for row in document.revision_history
                ],
            )
        ],
    )
