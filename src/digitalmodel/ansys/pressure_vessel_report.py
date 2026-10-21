# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""ASME shell/head report views over the existing pressure-vessel calculator.

Equation sources: ASMEThicknessCalc docstrings, UG-27(c)(1), UG-32(d)/(f),
UG-99 (standard edition unspecified in the inherited module). This adapter
does not qualify that engineering basis. It never imports or starts a solver.
Hydrotest factor and test allowable are explicit inputs, without code defaults.
"""

from dataclasses import asdict

from digitalmodel.ansys.pressure_vessel import (
    ASMEThicknessCalc,
    DesignConditions,
    VesselGeometry,
)
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
    validate_number,
)


def _validate(geometry, conditions, factor, test_stress, points, source):
    for name in ("inner_diameter_mm", "shell_length_mm", "wall_thickness_mm"):
        validate_number(name, getattr(geometry, name))
    validate_number("corrosion allowance", geometry.corrosion_allowance_mm, zero=True)
    head = geometry.head_thickness_mm
    if head is None:
        head = geometry.wall_thickness_mm
    validate_number("head wall", head)
    if min(head, geometry.wall_thickness_mm) <= geometry.corrosion_allowance_mm:
        raise ValueError("corrosion allowance consumes nominal wall")
    if geometry.head_type not in ("2:1_ellipsoidal", "hemispherical"):
        raise ValueError("unsupported head type; supply an evaluated head model")
    for name in ("design_pressure_mpa", "allowable_stress_mpa", "joint_efficiency"):
        validate_number(name, getattr(conditions, name))
    # Efficiency is a fraction by definition; its code selection is not qualified here.
    if conditions.joint_efficiency > 1:
        raise ValueError("joint efficiency exceeds unity")
    if conditions.hydrotest_pressure_mpa is not None:
        validate_number("supplied hydrotest pressure", conditions.hydrotest_pressure_mpa)
    validate_number("test factor", factor)
    validate_number("test allowable", test_stress)
    if not source.strip() or not points:
        raise ValueError("source and pressure sweep are required")
    for value in points:
        validate_number("pressure sweep", value, zero=True)
    if any(a >= b for a, b in zip(points, points[1:])):
        raise ValueError("pressure sweep must be strictly increasing")
    # Denominator from inherited UG-27(c)(1) equation (edition unspecified).
    if conditions.allowable_stress_mpa * conditions.joint_efficiency <= 0.6 * max(
        *points, conditions.design_pressure_mpa
    ):
        raise ValueError("pressure sweep exceeds the inherited shell equation domain")
    return head


def _thicknesses(geometry, conditions, pressure):
    calc = ASMEThicknessCalc()
    common = dict(
        pressure_mpa=pressure,
        allowable_stress_mpa=conditions.allowable_stress_mpa,
        joint_efficiency=conditions.joint_efficiency,
        corrosion_allowance_mm=geometry.corrosion_allowance_mm,
    )
    shell = calc.ug27_cylindrical_shell(
        inner_radius_mm=geometry.inner_diameter_mm / 2, **common
    )
    if geometry.head_type == "hemispherical":
        head = calc.ug32_hemispherical_head(
            inner_radius_mm=geometry.inner_diameter_mm / 2, **common
        )
    else:
        head = calc.ug32_ellipsoidal_head(
            inner_diameter_mm=geometry.inner_diameter_mm, **common
        )
    return shell["t_with_ca_mm"], head["t_with_ca_mm"]


def _mawp(geometry, conditions, head_nominal):
    shell = ASMEThicknessCalc().calculate_mawp(
        geometry.wall_thickness_mm,
        geometry.inner_diameter_mm / 2,
        conditions.allowable_stress_mpa,
        conditions.joint_efficiency,
        geometry.corrosion_allowance_mm,
    )
    net = head_nominal - geometry.corrosion_allowance_mm
    diameter_or_radius = geometry.inner_diameter_mm
    if geometry.head_type == "hemispherical":
        diameter_or_radius /= 2
    # Algebraic inverse of inherited UG-32(d)/(f), edition unspecified.
    # 2 and 0.2 are from those existing equations, not a new code-table value.
    head = (
        2
        * conditions.allowable_stress_mpa
        * conditions.joint_efficiency
        * net
        / (diameter_or_radius + 0.2 * net)
    )
    return shell, head


def _figures(geometry, conditions, points, rows, mawp):
    pressures = sorted(set((*points, conditions.design_pressure_mpa)))
    values = [_thicknesses(geometry, conditions, pressure) for pressure in pressures]
    figure = limit_figure(
        pressures,
        {
            "Shell required including CA": [row[0] for row in values],
            "Head required including CA": [row[1] for row in values],
            "Shell nominal": [rows[0].nominal_mm] * len(pressures),
            "Head nominal": [rows[1].nominal_mm] * len(pressures),
        },
        title="Required thickness against pressure",
        x_label="Internal pressure [MPa]",
        y_label="Thickness [mm]",
        limit=0,
        limit_label="Physical lower bound",
    )
    figure["layout"]["shapes"].extend(
        [
            {
                "type": "line",
                "x0": pressure,
                "x1": pressure,
                "yref": "paper",
                "y0": 0,
                "y1": 1,
                "line": {"dash": "dot", "color": color},
            }
            for pressure, color in (
                (conditions.design_pressure_mpa, "black"),
                (min(mawp), "firebrick"),
            )
        ]
    )
    figure["data"].append(
        {
            "type": "scatter",
            "mode": "markers",
            "name": "Design point",
            "x": [conditions.design_pressure_mpa],
            "y": [max(row.required_mm for row in rows)],
        }
    )
    figure["layout"]["annotations"].append(
        {
            "x": min(mawp),
            "y": 1,
            "yref": "paper",
            "showarrow": False,
            "text": f"Shell/head governing MAWP: {min(mawp):.1f} MPa",
        }
    )
    return Section(
        key="figures",
        title="Interactive figures",
        blocks=[
            FigureBlock(
                title="Required thickness against pressure",
                figure_id="vessel-thickness",
                plotly=figure,
                caption="Nominal walls are sizing limits; vertical lines mark design pressure and governing shell/head MAWP.",
            ),
            FigureBlock(
                title="Component nominal utilisation",
                figure_id="vessel-utilisation",
                plotly=limit_figure(
                    [row.label for row in rows],
                    {"Utilisation": [row.required_mm / row.nominal_mm for row in rows]},
                    title="Component nominal utilisation",
                    x_label="Component",
                    y_label="Utilisation [1]",
                    limit=1,
                    limit_label="Required / nominal = 1.0",
                    kind="bar",
                ),
            ),
        ],
    )


def _basis(geometry, conditions, factor, test_stress, source):
    return Section(
        key="design-basis",
        title="Design basis",
        blocks=[
            TableBlock(
                title="Design basis",
                columns=["Input", "Value", "Unit", "Source"],
                rows=[
                    [
                        "Inside diameter",
                        f"{geometry.inner_diameter_mm:.3f}",
                        "mm",
                        source,
                    ],
                    ["Shell length", f"{geometry.shell_length_mm:.3f}", "mm", source],
                    ["Head type", geometry.head_type, "n/a", source],
                    [
                        "Design pressure",
                        f"{conditions.design_pressure_mpa:.1f}",
                        "MPa",
                        source,
                    ],
                    [
                        "Design temperature",
                        f"{conditions.design_temperature_c:.1f}",
                        "degC",
                        source,
                    ],
                    [
                        "Joint efficiency",
                        f"{conditions.joint_efficiency:.3f}",
                        "1",
                        source,
                    ],
                    ["Hydrotest factor", f"{factor:.3f}", "1", source],
                ],
            ),
            TableBlock(
                title="Allowables",
                columns=["Material", "Design [MPa]", "Test [MPa]", "Source"],
                rows=[
                    [
                        conditions.material_name,
                        f"{conditions.allowable_stress_mpa:.1f}",
                        f"{test_stress:.1f}",
                        source,
                    ]
                ],
            ),
        ],
    )


def vessel_report(
    *,
    geometry: VesselGeometry,
    conditions: DesignConditions,
    document: DocumentMeta,
    test_factor: float,
    test_allowable_mpa: float,
    pressure_points_mpa: tuple[float, ...],
    source: str,
) -> ReportSpec:
    """Build a uniform internal-pressure report without evaluating code compliance.

    This scope sizes shell and head for the design load. Hydrotest pressure is
    reported, but hydrotest stress/yield acceptance, external pressure, nozzle,
    local loads and code applicability are NOT_EVALUATED. No elevation pressure
    plot is invented for geometry without a vertical fluid-head definition.
    """
    head = _validate(
        geometry,
        conditions,
        test_factor,
        test_allowable_mpa,
        pressure_points_mpa,
        source,
    )
    shell_req, head_req = _thicknesses(
        geometry, conditions, conditions.design_pressure_mpa
    )
    rows = [
        ThicknessRow(
            name,
            required,
            None,
            None,
            required,
            nominal,
            geometry.corrosion_allowance_mm,
            "Design internal pressure",
        )
        for name, required, nominal in (
            ("Shell", shell_req, geometry.wall_thickness_mm),
            ("Head", head_req, head),
        )
    ]
    mawp = _mawp(geometry, conditions, head)
    hydrotest = conditions.hydrotest_pressure_mpa
    if hydrotest is None:
        hydrotest = ASMEThicknessCalc().calculate_hydrotest_pressure(
            conditions.design_pressure_mpa,
            test_factor,
            conditions.allowable_stress_mpa,
            test_allowable_mpa,
        )
    inputs = dict(
        geometry=asdict(geometry),
        conditions=asdict(conditions),
        test_factor=test_factor,
        test_allowable_mpa=test_allowable_mpa,
        pressure_points_mpa=list(pressure_points_mpa),
        source=source,
    )
    summary = Section(
        key="summary",
        title="Summary",
        blocks=[
            TextBlock(
                markdown=(
                    f"Shell/head governing MAWP is {min(mawp):.1f} MPa against design pressure "
                    f"{conditions.design_pressure_mpa:.1f} MPa. Code qualification: NOT_EVALUATED. "
                    "Inherited UG-27, UG-32 and UG-99 equation editions and material-table selection "
                    "are not qualified. The pressure sweep is an arithmetic illustration, not an "
                    "applicability check. Hydrotest stress, external pressure, nozzles, local loads, "
                    "stability and full vessel compliance are NOT_EVALUATED."
                )
            )
        ],
    )
    results = Section(
        key="results",
        title="Shell and head results",
        blocks=[
            thickness_table("Shell and head thicknesses", rows, source),
            TableBlock(
                title="Governing-case summary",
                columns=["Quantity", "Value"],
                rows=[
                    ["Shell MAWP [MPa]", f"{mawp[0]:.1f}"],
                    ["Head MAWP [MPa]", f"{mawp[1]:.1f}"],
                    ["Governing MAWP [MPa]", f"{min(mawp):.1f}"],
                    ["Governing component", "Shell" if mawp[0] <= mawp[1] else "Head"],
                    ["Hydrotest pressure [MPa]", f"{hydrotest:.1f}"],
                    [
                        "Hydrotest pressure basis",
                        "Supplied"
                        if conditions.hydrotest_pressure_mpa is not None
                        else "Supplied factor * design pressure * test/design allowable",
                    ],
                ],
            ),
        ],
    )
    return ReportSpec(
        document=document,
        input_echo=inputs,
        provenance=input_provenance("vessel-thickness-inputs", inputs),
        sections=[
            summary,
            revision_section(document),
            _basis(geometry, conditions, test_factor, test_allowable_mpa, source),
            results,
            _figures(geometry, conditions, pressure_points_mpa, rows, mawp),
        ],
    )
