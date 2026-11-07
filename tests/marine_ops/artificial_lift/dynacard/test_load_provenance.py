"""Regression checks for the disclosed, unqualified Gibbs load approximation."""

import json
from math import pi, sqrt
from pathlib import Path
import runpy

import pytest
import numpy as np
from click.testing import CliRunner

from digitalmodel.marine_ops.artificial_lift.dynacard.cli import cli
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver
from digitalmodel.marine_ops.artificial_lift.dynacard.report_sections import (
    build_diagnostic_report_html,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


@pytest.fixture
def context():
    """Synthetic arithmetic exercise; dimensions are chosen inputs, not field data."""
    return DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(
            position=[0.0, 50.0, 100.0, 50.0],
            load=[1000.0, 2000.0, 3000.0, 2000.0],
        ),
        rod_string=[RodSection(diameter=sqrt(4.0 / pi), length=100.0)],
        pump=PumpProperties(diameter=1.0, depth=100.0),
        spm=1.0,
    )


def solve(context):
    with pytest.warns(UserWarning, match="load not propagated"):
        return DynacardPhysicsSolver(context).solve_wave_equation()


def test_load_basis_records_the_executed_arithmetic(context):
    """Hand calculation of the retained algorithm, not physical validation.

    Source: constants.py, MATERIAL PROPERTIES and LEGACY CALIBRATION, and
    physics.py, buoyant adjustment (repository implementation, 2026).
    Chosen d=sqrt(4/pi) in gives A=1 in^2. W=100*490/144=340.2777778 lbf;
    W_b=W*0.825=280.7291667 lbf. (F_s-W_b)*0.88 gives the four loads below.
    The coefficients are unqualified legacy inputs, not acceptance limits.
    pytest.approx supplies its numerical comparison tolerance, not an
    engineering acceptance criterion.
    """
    result = solve(context)
    basis = result.downhole_load_basis
    assert basis.method == "affine_surface_load"
    assert basis.validation_status == "NOT_EVALUATED"
    assert basis.buoyant_rod_weight_lbf == pytest.approx(280.7291666666667)
    assert basis.attenuation_factor == 0.88  # constants.py: LOAD_ATTENUATION_FACTOR
    assert result.downhole_card.load == pytest.approx(
        [632.9583333333333, 1512.9583333333333, 2392.9583333333333, 1512.9583333333333]
    )
    assert "not propagated" in basis.limitations
    assert "fillage" in basis.limitations


def test_basis_survives_json_roundtrip(context):
    result = solve(context)
    restored = AnalysisResults.model_validate_json(result.model_dump_json())
    assert restored.downhole_load_basis == result.downhole_load_basis


def test_undeclared_basis_is_unknown_in_older_results():
    assert AnalysisResults.model_validate({}).downhole_load_basis is None


def test_full_workflow_keeps_basis(context):
    # Chosen smooth inputs exercise downstream corner detection; the four-point
    # arithmetic card deliberately has no distinct fluid-load corner phases.
    phase = np.linspace(0.0, 2.0 * pi, 100)
    context.surface_card = CardData(
        position=(100.0 * (1.0 - np.cos(phase))).tolist(),
        load=(25000.0 + 5000.0 * np.sin(phase)).tolist(),
    )
    with pytest.warns(UserWarning, match="load not propagated"):
        result = DynacardWorkflow(context, solver_method="gibbs").run_full_analysis()
    assert result.downhole_load_basis.validation_status == "NOT_EVALUATED"
    assert result.model_dump()["downhole_load_basis"]["method"] == "affine_surface_load"


def render(result):
    return build_diagnostic_report_html(
        {}, result, "", {"classification": "NORMAL", "severity": "normal",
                          "screening_status": "pass"},
        {"note": "single card", "setpoints": {}, "alarms": []},
    )


def test_report_discloses_basis_before_diagnostic_verdict(context):
    html = render(solve(context))
    assert "affine_surface_load" in html
    assert "NOT_EVALUATED" in html
    assert "not propagated" in html
    assert html.index("NOT_EVALUATED") < html.index("screening_status")


def test_report_escapes_basis_limitations(context):
    result = solve(context)
    result.downhole_load_basis.limitations = "<script>untrusted</script>"
    html = render(result)
    assert "<script>untrusted</script>" not in html
    assert "&lt;script&gt;untrusted&lt;/script&gt;" in html


def test_cli_discloses_basis_in_summary_and_json(context, monkeypatch, tmp_path):
    result = solve(context)
    module = "digitalmodel.marine_ops.artificial_lift.dynacard.cli"
    monkeypatch.setattr(f"{module}.load_from_json_file", lambda _: context)
    monkeypatch.setattr(f"{module}.DynacardWorkflow.run_full_analysis", lambda _: result)
    source = tmp_path / "input.json"
    source.write_text("{}", encoding="utf-8")
    output = tmp_path / "result.json"
    invocation = CliRunner().invoke(cli, [str(source), "--output", str(output)])
    assert invocation.exit_code == 0, invocation.output
    assert "affine_surface_load" in invocation.output
    assert "NOT_EVALUATED" in invocation.output
    assert "not propagated" in invocation.output
    assert json.loads(output.read_text())["downhole_load_basis"] == (
        result.downhole_load_basis.model_dump()
    )


def test_worked_example_matches_hand_calculation():
    """Display values are rounded from the hand calculation above, to one decimal."""
    example = Path(__file__).resolve().parents[4] / (
        "examples/domains/artificial_lift/dynacard_load_basis.py"
    )
    with pytest.warns(UserWarning, match="load not propagated"):
        output = runpy.run_path(str(example))["worked_example"]()
    assert output["answer_load_lbf"] == [633.0, 1513.0, 2393.0, 1513.0]
    assert output["method"]["validation_status"] == "NOT_EVALUATED"
    assert output["method"]["buoyant_rod_weight_lbf"] == 280.7
