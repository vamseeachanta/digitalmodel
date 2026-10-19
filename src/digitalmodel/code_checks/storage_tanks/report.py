# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Tank report adapter for the shared typed reporting engine; no HTML assembly."""

from dataclasses import asdict
import json

from digitalmodel.reporting.spec import (
    DocumentMeta,
    FigureBlock,
    ReportSpec,
    Section,
    TableBlock,
    TextBlock,
)
from digitalmodel.reporting.thickness import (
    ThicknessRow,
    input_provenance,
    limit_figure,
    revision_section,
    thickness_table,
)

from .one_foot import ShellResult, _number


def _courses(result: ShellResult) -> TableBlock:
    rows = [
        ThicknessRow(
            f"Course {c.course}",
            c.design_mm,
            c.test_mm,
            result.inputs.basis.minimum_nominal_mm,
            c.required_mm,
            c.selected_mm,
            result.inputs.corrosion_allowance_mm,
            c.governing_case,
        )
        for c in result.courses
    ]
    extras = tuple(
        (f"{c.bottom_m:.3f}", f"{c.design_head_m:.3f}", f"{c.test_head_m:.3f}")
        for c in result.courses
    )
    return thickness_table(
        "Shell course thicknesses",
        rows,
        result.inputs.basis.source,
        extra_columns=("Bottom [m]", "Design H [m]", "Test H [m]"),
        extra_rows=extras,
    )


def _pressure(result: ShellResult, unit_weight: float) -> FigureBlock:
    inputs = result.inputs
    # p=gamma*h (hydrostatic equilibrium); gamma is a sourced caller input.
    heights = sorted(
        {
            0,
            inputs.design_level_m,
            inputs.test_level_m,
            *(row.top_m for row in result.courses),
        }
    )
    design = [
        unit_weight * inputs.specific_gravity * max(inputs.design_level_m - z, 0)
        for z in heights
    ]
    test = [unit_weight * max(inputs.test_level_m - z, 0) for z in heights]
    figure = limit_figure(
        heights,
        {"Design liquid": design, "Hydrotest water": test},
        title="Hydrostatic pressure against height",
        x_label="Height above bottom [m]",
        y_label="Pressure [kPa]",
        limit=max(design[0], test[0]),
        limit_label="Maximum applied base pressure (reference, not allowable)",
    )
    return FigureBlock(
        title="Hydrostatic pressure against height",
        figure_id="tank-pressure",
        plotly=figure,
        caption="Separate design and hydrotest levels; gauge pressure.",
    )


def _thickness(result: ShellResult) -> FigureBlock:
    heights = [z for row in result.courses for z in (row.bottom_m, row.top_m)]
    series = {
        name: [getattr(row, field) for row in result.courses for _ in range(2)]
        for name, field in (
            ("Design including CA", "design_mm"),
            ("Hydrotest", "test_mm"),
            ("Selected plate", "selected_mm"),
        )
    }
    if result.inputs.basis.minimum_nominal_mm is not None:
        series["Nominal floor"] = [result.inputs.basis.minimum_nominal_mm] * len(
            heights
        )
    figure = limit_figure(
        heights,
        series,
        title="Thickness against height",
        x_label="Height above bottom [m]",
        y_label="Nominal thickness [mm]",
        limit=0,
        limit_label="Physical lower bound",
    )
    return FigureBlock(
        title="Thickness against height",
        figure_id="tank-thickness",
        plotly=figure,
        caption="Constant demand within each course; selected plate is the sizing limit.",
    )


def _basis(result: ShellResult, unit_weight: float, pressure_source: str) -> Section:
    inputs = result.inputs
    entries = [
        ["Diameter", f"{inputs.diameter_m:.3f}", "m", "Supplied geometry"],
        [
            "Course heights, bottom up",
            ", ".join(f"{v:.3f}" for v in inputs.course_heights_m),
            "m",
            "Supplied geometry",
        ],
        [
            "Design / test level",
            f"{inputs.design_level_m:.3f} / {inputs.test_level_m:.3f}",
            "m",
            "Supplied fill levels",
        ],
        ["Specific gravity", f"{inputs.specific_gravity:.3f}", "1", "Supplied liquid"],
        [
            "Corrosion allowance",
            f"{inputs.corrosion_allowance_mm:.3f}",
            "mm",
            "Supplied allowance",
        ],
        ["Steel density", f"{inputs.density_kg_m3:.1f}", "kg/m3", "Supplied density"],
        ["Water unit weight", f"{unit_weight:.3f}", "kN/m3", pressure_source],
        [
            "Method coefficient",
            f"{inputs.basis.coefficient:.3f}",
            "MPa mm/m2",
            inputs.basis.source,
        ],
        ["Head offset", f"{inputs.basis.head_offset_m:.3f}", "m", inputs.basis.source],
        [
            "Diameter limit",
            "Not Evaluated"
            if inputs.basis.diameter_limit_m is None
            else f"{inputs.basis.diameter_limit_m:.3f}",
            "m",
            inputs.basis.source,
        ],
        [
            "Minimum nominal floor",
            "Not Evaluated"
            if inputs.basis.minimum_nominal_mm is None
            else f"{inputs.basis.minimum_nominal_mm:.3f}",
            "mm",
            inputs.basis.source,
        ],
        [
            "Available plates",
            ", ".join(f"{v:.3f}" for v in inputs.plate_sizes_mm),
            "mm",
            "Supplied plate catalogue",
        ],
    ]
    return Section(
        key="design-basis",
        title="Design basis",
        blocks=[
            TableBlock(
                title="Design basis",
                columns=["Input", "Value", "Unit", "Source"],
                rows=entries,
            ),
            TableBlock(
                title="Allowables",
                columns=["Material", "Design [MPa]", "Test [MPa]", "Source"],
                rows=[
                    [
                        inputs.material.name,
                        f"{inputs.material.design_mpa:.1f}",
                        f"{inputs.material.test_mpa:.1f}",
                        inputs.material.source,
                    ]
                ],
            ),
        ],
    )


def tank_report(
    result: ShellResult,
    document: DocumentMeta,
    *,
    water_unit_weight_kn_m3: float,
    pressure_source: str,
) -> ReportSpec:
    """Expose arithmetic and evidence gaps; never turn missing sources into PASS."""
    _number("water unit weight", water_unit_weight_kn_m3)
    if not pressure_source.strip():
        raise ValueError("pressure source is required")
    inputs = json.loads(json.dumps(asdict(result.inputs), allow_nan=False))
    inputs.update(
        water_unit_weight_kn_m3=water_unit_weight_kn_m3, pressure_source=pressure_source
    )
    worst = max(result.courses, key=lambda row: row.utilisation)
    summary = Section(
        key="summary",
        title="Summary",
        blocks=[
            TextBlock(
                markdown=(
                    f"Shell-equation status: {result.status}. Governing nominal utilisation is "
                    f"{worst.utilisation:.1%} in course {worst.course}, compared with 100.0 %. "
                    f"Approximate nominal shell mass is {result.shell_mass_kg:.1f} kg. "
                    "This scope covers shell hydrostatic sizing only; wind, seismic, roof, "
                    "bottom, stability, fabrication tolerances and full tank compliance are not evaluated.\n\n"
                    + "\n".join(f"- {gap}" for gap in result.not_evaluated)
                )
            )
        ],
    )
    method = Section(
        key="method",
        title="Method and qualification",
        blocks=[
            TextBlock(
                markdown=(
                    "API 650 5.6.3 equation form from the public issue brief (edition unverified): "
                    "td = c D max(H-a,0) G / Sd + CA; tt = c D max(Ht-a,0) / St. "
                    "H and Ht are measured from each course bottom to its liquid surface. "
                    "The maximum of td, tt and the supplied nominal floor is rounded upward "
                    "to a plate in the supplied catalogue. Mass uses pi D sum(height * nominal wall) "
                    "and the supplied steel density; specified diameter approximates the mid-surface. "
                    "Source: " + result.inputs.basis.source
                )
            )
        ],
    )
    figures = Section(
        key="figures",
        title="Interactive figures",
        blocks=[
            _pressure(result, water_unit_weight_kn_m3),
            _thickness(result),
            FigureBlock(
                title="Nominal utilisation per course",
                figure_id="tank-utilisation",
                caption="Governing nominal requirement divided by selected nominal wall.",
                plotly=limit_figure(
                    [f"Course {c.course}" for c in result.courses],
                    {"Utilisation": [c.utilisation for c in result.courses]},
                    title="Nominal utilisation per course",
                    x_label="Course",
                    y_label="Utilisation [1]",
                    limit=1,
                    limit_label="Required / selected = 1.0",
                    kind="bar",
                ),
            ),
        ],
    )
    governing = Section(
        key="results",
        title="Course results",
        blocks=[
            _courses(result),
            TableBlock(
                title="Governing-case summary",
                columns=["Quantity", "Value"],
                rows=[
                    ["Governing course", str(worst.course)],
                    ["Governing case", worst.governing_case],
                    ["Maximum nominal utilisation", f"{worst.utilisation:.1%}"],
                    ["Approximate shell mass [kg]", f"{result.shell_mass_kg:.1f}"],
                    ["Code qualification", result.status],
                ],
            ),
        ],
    )
    return ReportSpec(
        document=document,
        sections=[
            summary,
            revision_section(document),
            _basis(result, water_unit_weight_kn_m3, pressure_source),
            method,
            governing,
            figures,
        ],
        citations=list(result.citations),
        input_echo=inputs,
        provenance=input_provenance("tank-shell-inputs", inputs),
    )
