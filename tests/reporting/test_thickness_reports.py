# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report views retain arithmetic, units, limits and explicit source gaps."""

from dataclasses import replace
import json

import pytest

from digitalmodel.ansys.pressure_vessel import DesignConditions, VesselGeometry
from digitalmodel.ansys.pressure_vessel_report import vessel_report
from digitalmodel.code_checks.storage_tanks import (
    MaterialAllowables,
    OneFootBasis,
    TankDesign,
    design_shell,
)
from digitalmodel.code_checks.storage_tanks.report import tank_report
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.spec import DocumentMeta, FigureBlock, TableBlock


def document(title):
    return DocumentMeta(
        number="T0000-RPT-001-00",
        revision="00",
        title=title,
        project="Generic calculation",
        client="Public illustration",
        date="2026-10-19",
    )


def shell_result():
    """Synthetic inputs from the independent arithmetic in test_one_foot."""
    return design_shell(
        TankDesign(
            30,
            (2.25,) * 8,
            18,
            18,
            1,
            1.5,
            7800,
            (6, 8, 10, 12, 14, 16, 18, 20),
            MaterialAllowables("Illustrative steel", 150, 160, "assumed"),
            OneFootBasis(4.9, 0.3, 60, 6, "Issue 2958; assumed test limits"),
        )
    )


def blocks(spec, kind):
    return [
        b for section in spec.sections for b in section.blocks if isinstance(b, kind)
    ]


def test_tank_tables_and_charts_use_result_values():
    result = shell_result()
    spec = tank_report(
        result,
        document("Tank shell analysis"),
        water_unit_weight_kn_m3=9.8,
        pressure_source="Assumed illustration: 9.8 kN/m3",
    )
    tables = {block.title: block for block in blocks(spec, TableBlock)}
    table = tables["Shell course thicknesses"]
    assert len(table.rows) == 8
    assert "Bottom [m]" in table.columns
    assert "Nominal [mm]" in table.columns
    assert table.rows[0][table.columns.index("Design [mm]")] == "18.846"
    assert table.rows[0][table.columns.index("Nominal [mm]")] == "20.000"
    figures = {block.figure_id: block.plotly for block in blocks(spec, FigureBlock)}
    assert len(figures) == 3
    pressure = figures["tank-pressure"]["data"][0]
    # Independent hydrostatics, synthetic unit weight: 9.8*18*1=176.4 kPa.
    assert pressure["y"][0] == pytest.approx(176.4)
    assert pressure["y"][-1] == 0
    thickness = figures["tank-thickness"]["data"][0]
    assert thickness["x"][:4] == [0, 2.25, 2.25, 4.5]
    assert thickness["y"][:2] == [result.courses[0].design_mm] * 2
    assert figures["tank-utilisation"]["layout"]["shapes"][0]["y0"] == 1
    assert all(fig["layout"].get("shapes") for fig in figures.values())
    assert spec.input_echo["basis"]["source"] == result.inputs.basis.source


def test_tank_test_pressure_profile_uses_separate_fill_level():
    original = shell_result()
    result = design_shell(replace(original.inputs, design_level_m=1, test_level_m=2))
    spec = tank_report(
        result,
        document("Tank shell analysis"),
        water_unit_weight_kn_m3=10,
        pressure_source="Synthetic arithmetic",
    )
    pressure = blocks(spec, FigureBlock)[0].plotly["data"]
    assert pressure[0]["y"][0] == 10
    assert pressure[1]["y"][0] == 20
    # Both liquid surfaces are explicit knots; no diagonal across a dry segment.
    assert 1 in pressure[0]["x"] and 2 in pressure[1]["x"]


def vessel_spec(**overrides):
    """UG-27 hand case: t=10*100/(106-6)+2=12 mm.

    Source: inherited ASMEThicknessCalc docstrings, edition unspecified.
    All inputs are synthetic. Head UG-32 ellipsoid: 10*200/(212-2)+2.
    Hydrotest = 1.5*10*(120/106) = 1800/106 MPa.
    """
    arguments = dict(
        geometry=VesselGeometry(
            inner_diameter_mm=200,
            wall_thickness_mm=12,
            corrosion_allowance_mm=2,
            head_thickness_mm=12,
        ),
        conditions=DesignConditions(
            design_pressure_mpa=10, allowable_stress_mpa=106, joint_efficiency=1
        ),
        document=document("Pressure vessel shell analysis"),
        test_factor=1.5,
        test_allowable_mpa=120,
        pressure_points_mpa=(0, 5, 10),
        source="Synthetic inputs; inherited equations",
    )
    arguments.update(overrides)
    return vessel_report(**arguments)


def test_vessel_table_and_pressure_curve_match_hand_values():
    spec = vessel_spec()
    tables = {block.title: block for block in blocks(spec, TableBlock)}
    table = tables["Shell and head thicknesses"]
    assert table.rows[0][table.columns.index("Required [mm]")] == "12.000"
    assert table.rows[1][table.columns.index("Required [mm]")] == "11.524"
    summary = tables["Governing-case summary"]
    assert ["Shell MAWP [MPa]", "10.0"] in summary.rows
    assert ["Governing MAWP [MPa]", "10.0"] in summary.rows
    assert ["Hydrotest pressure [MPa]", "17.0"] in summary.rows
    figure = blocks(spec, FigureBlock)[0].plotly
    assert figure["data"][0]["y"][-1] == pytest.approx(12)
    assert any(shape.get("x0") == 10 for shape in figure["layout"]["shapes"])
    assert any(trace.get("name") == "Design point" for trace in figure["data"])


def test_head_can_control_reported_mawp():
    geometry = VesselGeometry(
        inner_diameter_mm=200,
        wall_thickness_mm=22,
        corrosion_allowance_mm=2,
        head_thickness_mm=7,
    )
    spec = vessel_spec(geometry=geometry)
    summary = next(
        b for b in blocks(spec, TableBlock) if b.title == "Governing-case summary"
    )
    # Inherited UG-32 inverse, synthetic net head t=5:
    # P=2*106*5/(200+0.2*5)=1060/201=5.2736... MPa.
    assert ["Governing MAWP [MPa]", "5.3"] in summary.rows
    assert ["Governing component", "Head"] in summary.rows


@pytest.mark.parametrize("points", [(), (-1, 10), (10, 5), (0, 200), (0, float("nan"))])
def test_invalid_pressure_sweeps_rejected(points):
    with pytest.raises(ValueError):
        vessel_spec(pressure_points_mpa=points)


def test_unsupported_head_is_not_silently_treated_as_ellipsoidal():
    with pytest.raises(ValueError, match="head"):
        vessel_spec(geometry=VesselGeometry(head_type="torispherical"))


@pytest.mark.parametrize(
    "field,value", [("test_factor", 0), ("test_allowable_mpa", -1), ("source", "")]
)
def test_invalid_vessel_report_basis_rejected(field, value):
    with pytest.raises(ValueError):
        vessel_spec(**{field: value})


def test_both_reports_render_offline_with_input_sources_and_limits():
    tank = tank_report(
        shell_result(),
        document("Tank shell analysis"),
        water_unit_weight_kn_m3=9.8,
        pressure_source="assumed",
    )
    for spec in (tank, vessel_spec()):
        rendered = render_html(spec)
        assert "Plotly.newPlot" in rendered
        assert "<script src=" not in rendered
        assert "NOT_EVALUATED" in rendered
        assert "Revision history" in rendered
        assert "Allowables" in rendered
        assert spec.provenance.sources[0].digest.startswith("sha256:")
        assert json.loads(json.dumps(spec.input_echo)) == spec.input_echo


def test_hemispherical_head_uses_its_own_equation():
    """Inherited UG-32(f): 10*100/(2*106-0.2*10)+2=6.7619... mm."""
    geometry = VesselGeometry(
        inner_diameter_mm=200,
        wall_thickness_mm=12,
        corrosion_allowance_mm=2,
        head_type="hemispherical",
    )
    spec = vessel_spec(geometry=geometry)
    table = next(
        b for b in blocks(spec, TableBlock) if b.title == "Shell and head thicknesses"
    )
    assert table.rows[1][table.columns.index("Required [mm]")] == "6.762"


def test_supplied_hydrotest_pressure_is_preserved():
    conditions = DesignConditions(
        design_pressure_mpa=10,
        allowable_stress_mpa=106,
        joint_efficiency=1,
        hydrotest_pressure_mpa=12,
    )
    spec = vessel_spec(conditions=conditions)
    summary = next(
        b for b in blocks(spec, TableBlock) if b.title == "Governing-case summary"
    )
    assert ["Hydrotest pressure [MPa]", "12.0"] in summary.rows
    assert ["Hydrotest pressure basis", "Supplied"] in summary.rows


def test_failed_wall_is_visible_in_margin_and_utilisation():
    geometry = VesselGeometry(
        inner_diameter_mm=200, wall_thickness_mm=10, corrosion_allowance_mm=2
    )
    spec = vessel_spec(geometry=geometry)
    table = next(
        b for b in blocks(spec, TableBlock) if b.title == "Shell and head thicknesses"
    )
    assert table.rows[0][table.columns.index("Margin [mm]")] == "-2.000"
    assert table.rows[0][table.columns.index("Utilisation")] == "120.0%"
