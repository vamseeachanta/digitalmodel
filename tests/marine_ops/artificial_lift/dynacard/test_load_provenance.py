"""Provenance checks for coupled Gibbs propagation; field accuracy is unevaluated."""

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
            position=[0.0] * 5,
            load=[1000.0] * 5,
        ),
        rod_string=[RodSection(
            diameter=sqrt(4.0 / pi), length=100.0, density=144.0,
        )],
        pump=PumpProperties(diameter=1.0, depth=100.0),
        spm=1.0,
        fluid_density=72.0,
    )


def solve(context, **kwargs):
    with pytest.warns(UserWarning, match="comparison only: not field validated"):
        return DynacardPhysicsSolver(context).solve_wave_equation(**kwargs)


def test_load_basis_records_the_executed_arithmetic(context):
    """Independent static equilibrium, not a field validation criterion.

    With constant surface position and force, u_tt=u_t=0 in the damped
    rod equation (Applied Mechanics of Solids, section 4.3.3; SPE-588-PA).
    Archimedes' displaced-fluid weight gives W_b=(rho_rod-rho_fluid)*A*L/144.
    Chosen d=sqrt(4/pi) in gives A=1 in^2; L=100 ft, rho_rod=144 lb/ft^3
    and rho_fluid=72 lb/ft^3 give W_b=(144-72)*1*100/144=50 lbf.
    Static pump force is F_p=F_s-W_b=1000-50=950 lbf, without attenuation.
    Static elastic stretch is constant; the zero-minimum position datum
    gives pump position 0 in at every sample. pytest.approx supplies a
    numerical comparison tolerance only, not an engineering acceptance limit.
    """
    result = solve(context)
    basis = result.downhole_load_basis
    assert basis.method == "coupled_frequency_domain"
    assert basis.validation_status == "NOT_EVALUATED"
    assert basis.buoyant_rod_weight_lbf == pytest.approx(50.0)
    assert basis.attenuation_factor is None
    assert basis.harmonic_count is None
    assert basis.closing_sample_dropped is True
    assert result.downhole_card.load == pytest.approx(
        [950.0] * len(result.downhole_card.load)
    )
    assert result.downhole_card.position == pytest.approx(
        [0.0] * len(result.downhole_card.position)
    )
    assert "not propagated" not in basis.limitations
    assert "load and displacement are propagated" in basis.limitations
    assert "vertical rod string" in basis.limitations
    assert "deviation, Coulomb friction and damping calibration are not modelled" in (
        basis.limitations
    )
    assert "field diagnostic accuracy is not established" in basis.limitations


def test_basis_survives_json_roundtrip(context):
    result = solve(context, harmonic_count=1)
    restored = AnalysisResults.model_validate_json(result.model_dump_json())
    assert restored.downhole_load_basis == result.downhole_load_basis
    assert restored.downhole_load_basis.harmonic_count == 1


def test_legacy_affine_basis_remains_readable():
    saved = {
        "method": "affine_surface_load", "buoyant_rod_weight_lbf": 280.7,
        "attenuation_factor": 0.88,
        "limitations": "Comparison only: load is not propagated through the wave equation.",
    }
    restored = AnalysisResults.model_validate({"downhole_load_basis": saved})
    basis = restored.downhole_load_basis
    assert basis.method == saved["method"]
    assert basis.attenuation_factor == saved["attenuation_factor"]
    assert basis.limitations == saved["limitations"]
    assert basis.harmonic_count is None
    assert basis.closing_sample_dropped is False


@pytest.mark.parametrize("include_method", [False, True])
def test_legacy_affine_defaults_do_not_claim_coupled_propagation(include_method):
    saved = {"buoyant_rod_weight_lbf": 280.7, "attenuation_factor": 0.88}
    if include_method:
        saved["method"] = "affine_surface_load"
    basis = AnalysisResults.model_validate({"downhole_load_basis": saved}).downhole_load_basis
    assert basis.method == "affine_surface_load"
    assert "not propagated" in basis.limitations
    assert "load and displacement are propagated" not in basis.limitations


@pytest.mark.parametrize("harmonic_count", [None, 0, 1, 2])
def test_basis_records_harmonic_selection(context, harmonic_count):
    assert solve(context, harmonic_count=harmonic_count).downhole_load_basis.harmonic_count == (
        harmonic_count
    )


def test_basis_sums_section_weights_including_supplied_coupling_weight(context):
    """A2=2 in², w2=4 lb/ft, L2=50 ft: W2=(4-72*2/144)*50=150 lbf.

    W1=50 lbf from the static hand case, so total W=200 and F_p=1000-200=800 lbf.
    Supplied coupling weight controls W2 rather than the material density.
    """
    context.rod_string.append(RodSection(
        diameter=sqrt(8.0 / pi), length=50.0, density=144.0, weight_per_foot=4.0,
    ))
    result = solve(context)
    assert result.downhole_load_basis.buoyant_rod_weight_lbf == pytest.approx(200.0)
    assert result.downhole_card.load == pytest.approx([800.0] * len(result.downhole_card.load))


def test_undeclared_basis_is_unknown_in_older_results():
    assert AnalysisResults.model_validate({}).downhole_load_basis is None


def test_full_workflow_keeps_basis(context):
    # Chosen smooth inputs exercise downstream corner detection; the static
    # arithmetic card has no distinct fluid-load corner phases.
    phase = np.linspace(0.0, 2.0 * pi, 100)
    context.surface_card = CardData(
        position=(100.0 * (1.0 - np.cos(phase))).tolist(),
        load=(25000.0 + 5000.0 * np.sin(phase)).tolist(),
    )
    with pytest.warns(UserWarning, match="comparison only: not field validated"):
        result = DynacardWorkflow(context, solver_method="gibbs").run_full_analysis()
    assert result.downhole_load_basis.validation_status == "NOT_EVALUATED"
    assert result.model_dump()["downhole_load_basis"]["method"] == "coupled_frequency_domain"


def render(result):
    return build_diagnostic_report_html(
        {}, result, "", {"classification": "NORMAL", "severity": "normal",
                          "screening_status": "pass"},
        {"note": "single card", "setpoints": {}, "alarms": []},
    )


def test_report_discloses_basis_before_diagnostic_verdict(context):
    html = render(solve(context))
    assert "coupled_frequency_domain" in html
    assert "NOT_EVALUATED" in html
    assert "load and displacement are propagated" in html
    assert "not propagated" not in html
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
    assert "coupled_frequency_domain" in invocation.output
    assert "NOT_EVALUATED" in invocation.output
    assert "load and displacement are propagated" in invocation.output
    assert "not propagated" not in invocation.output
    saved = json.loads(output.read_text())
    assert saved["downhole_load_basis"] == result.downhole_load_basis.model_dump(
        exclude_none=True
    )
    assert AnalysisResults.model_validate(saved).downhole_load_basis == result.downhole_load_basis


def test_worked_example_matches_hand_calculation():
    """Display values are rounded from the hand calculation above, to one decimal."""
    example = Path(__file__).resolve().parents[4] / (
        "examples/domains/artificial_lift/dynacard_load_basis.py"
    )
    with pytest.warns(UserWarning, match="comparison only: not field validated"):
        output = runpy.run_path(str(example))["worked_example"]()
    assert output["answer_load_lbf"] == [950.0] * 4
    assert output["answer_position_in"] == [0.0] * 4
    assert output["method"]["method"] == "coupled_frequency_domain"
    assert output["method"]["validation_status"] == "NOT_EVALUATED"
    assert output["method"]["buoyant_rod_weight_lbf"] == 50.0
    assert output["method"]["attenuation_factor"] is None
    assert output["method"]["harmonic_count"] is None
    assert output["method"]["closing_sample_dropped"] is True
