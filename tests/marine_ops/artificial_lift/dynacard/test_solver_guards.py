# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Defect-hunting checks for comparison gates and solver result restrictions."""

import inspect
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import load_from_json_file
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    EverittJenningsContextSolver,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import PhysicsError
from digitalmodel.marine_ops.artificial_lift.dynacard.finite_difference import FiniteDifferenceSolver
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults, CardData, DynacardAnalysisContext, PumpProperties, RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import (
    DynacardWorkflow, perform_well_troubleshooting,
)
from pathlib import Path


@pytest.fixture
def context():
    """Use the existing workflow test's synthetic boundary, without field data."""
    phase = np.linspace(0.0, 2.0 * np.pi, 100)
    return DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(
            position=(100.0 * (1.0 - np.cos(phase))).tolist(),
            load=(25000.0 + 5000.0 * np.sin(phase)).tolist(),
        ),
        rod_string=[RodSection(diameter=1.0, length=5000.0)],
        pump=PumpProperties(diameter=1.75, depth=5000.0),
        spm=10.0,
    )


def _solve(context, method):
    workflow = DynacardWorkflow(context, solver_method=method)
    if method == "gibbs":
        return workflow.solver.solve_wave_equation()
    return workflow.solver.solve()


@pytest.mark.parametrize("method,flag", [
    ("gibbs", "comparison only: load not propagated"),
    ("finite_difference", "comparison only: not validated"),
])
def test_direct_comparison_solver_warns_and_serializes_flag(context, method, flag):
    with pytest.warns(UserWarning, match=flag):
        results = _solve(context, method)
    assert flag in results.model_dump()["solver_flags"]
    assert results.solver_method == method


@pytest.mark.parametrize("method", ["gibbs"])
def test_direct_solver_rejects_excessive_downhole_stroke(context, method):
    context.surface_card.position = [x * 0.001 for x in context.surface_card.position]
    with pytest.raises(PhysicsError, match="downhole stroke exceeds surface stroke"):
        _solve(context, method)


@pytest.mark.parametrize("method", ["gibbs"])
def test_workflow_rejects_invalid_stroke_before_diagnosis(context, method):
    workflow = DynacardWorkflow(context, solver_method=method)
    invalid = AnalysisResults(downhole_card=CardData(
        position=[0.0, 400.0, 0.0], load=[1.0, 2.0, 3.0],
    ))
    workflow.solver = SimpleNamespace(solve=lambda: invalid, solve_wave_equation=lambda: invalid)
    with pytest.raises(PhysicsError, match="downhole stroke exceeds surface stroke"):
        workflow.run_full_analysis()


def _comparison_solvers(monkeypatch, offset=0.0, invalid=False):
    """Controlled cards separate gate behavior from known solver instability."""
    phase = np.linspace(0.0, 2.0 * np.pi, 100, endpoint=False)
    reference = CardData(position=(50.0 * (1.0 - np.cos(phase))).tolist(),
                         load=(5000.0 + 1000.0 * np.sin(phase)).tolist())
    candidate = CardData(position=np.roll(reference.position[::-1], 17).tolist(),
                         load=(np.roll(reference.load[::-1], 17) + offset).tolist())
    if invalid:
        candidate.load[0] = float("nan")
    monkeypatch.setattr(EverittJenningsContextSolver, "solve", lambda _: AnalysisResults(downhole_card=reference))
    monkeypatch.setattr(DynacardPhysicsSolver, "solve_wave_equation", lambda _: AnalysisResults(downhole_card=candidate))
    monkeypatch.setattr(FiniteDifferenceSolver, "solve", lambda _: AnalysisResults(downhole_card=reference))
    return reference


def test_comparison_uses_default_reference_and_ignores_phase(context, monkeypatch):
    reference = _comparison_solvers(monkeypatch)
    report = DynacardWorkflow(context, solver_method="gibbs").compare_solvers()
    assert report["reference_solver"] == "everitt_jennings"
    assert report["reference_results"].downhole_card == reference
    assert set(report["comparisons"]) == {"gibbs", "finite_difference"}
    for comparison in report["comparisons"].values():
        assert comparison["load_nrmse_pct"] < 1e-10
        assert comparison["stroke_error_pct"] == 0.0
    assert report["within_threshold"] is True


def test_comparison_raises_with_metrics_above_envelope(context, monkeypatch):
    _comparison_solvers(monkeypatch, offset=1000.0)
    with pytest.raises(PhysicsError, match="solver disagreement") as caught:
        DynacardWorkflow(context).compare_solvers()
    assert caught.value.details["comparisons"]["gibbs"]["load_nrmse_pct"] == pytest.approx(50.0)


def test_explicit_divergence_opt_in_preserves_failed_gate(context, monkeypatch):
    _comparison_solvers(monkeypatch, offset=1000.0)
    report = DynacardWorkflow(context).compare_solvers(allow_divergence=True)
    assert report["within_threshold"] is False
    assert report["threshold_load_nrmse_pct"] == 4.4
    assert report["comparisons"]["gibbs"]["load_nrmse_pct"] == pytest.approx(50.0)


def test_invalid_candidate_never_becomes_a_passing_comparison(context, monkeypatch):
    _comparison_solvers(monkeypatch, invalid=True)
    with pytest.raises(PhysicsError, match="solver disagreement"):
        DynacardWorkflow(context).compare_solvers()
    report = DynacardWorkflow(context).compare_solvers(allow_divergence=True)
    assert report["within_threshold"] is False
    assert "non-finite" in report["comparisons"]["gibbs"]["error"]
    assert "gibbs" not in report["solver_results"]


def test_divergence_opt_in_requires_a_boolean(context):
    with pytest.raises(ValueError, match="boolean"):
        DynacardWorkflow(context).compare_solvers(allow_divergence="False")


def test_unknown_solver_cannot_silently_select_gibbs(context):
    with pytest.raises(ValueError, match="unknown solver"):
        DynacardWorkflow(context, solver_method="mistyped")


def test_deviated_fixture_reports_vertical_approximation():
    fixture = Path(__file__).parents[1] / "test_data" / "cleansed_well_005.json"
    results = EverittJenningsContextSolver(load_from_json_file(fixture)).solve()
    assert "deviation present, vertical approximation applied" in results.solver_flags
    assert results.model_dump()["max_inclination_deg"] == pytest.approx(39.98)


def test_production_utility_keeps_default_solver(context):
    assert inspect.signature(perform_well_troubleshooting).parameters["solver_method"].default == "everitt_jennings"
    result = perform_well_troubleshooting(context.model_dump())
    assert result.solver_method == "everitt_jennings"
    assert not any("comparison only" in flag for flag in result.solver_flags)


@pytest.mark.parametrize("method", ["everitt_jennings", "finite_difference"])
def test_direct_solver_allows_downhole_overtravel(context, method):
    context.surface_card.position = [x * 0.001 for x in context.surface_card.position]
    results = _solve(context, method)
    assert np.ptp(results.downhole_card.position) > np.ptp(context.surface_card.position)
    assert np.all(np.isfinite(results.downhole_card.load))


@pytest.mark.parametrize("method", ["everitt_jennings", "finite_difference"])
def test_workflow_allows_overtravel_before_diagnosis(context, method):
    context.surface_card.position = [x * 0.001 for x in context.surface_card.position]
    expected = _solve(context, method).downhole_card
    expected_position = np.array(expected.position)
    expected_load = np.array(expected.load)
    result = DynacardWorkflow(context, solver_method=method).run_full_analysis()
    np.testing.assert_allclose(result.downhole_card.position, expected_position)
    np.testing.assert_allclose(result.downhole_card.load, expected_load)
    assert np.ptp(result.downhole_card.position) > np.ptp(context.surface_card.position)


@pytest.mark.parametrize("method", ["everitt_jennings", "gibbs", "finite_difference"])
@pytest.mark.parametrize("position,load,message", [
    ([0.0, float("nan"), 0.0], [1.0, 2.0, 3.0], "non-finite"),
    ([0.0, 1.0, 0.0], [1.0, float("inf"), 3.0], "non-finite"),
    ([0.0, 1.0], [1.0, 2.0], "at least three"),
    ([0.0, 1.0, 0.0], [1.0, 2.0, 3.0, 4.0], "lengths differ"),
])
def test_shared_card_validity_is_independent_of_stroke(method, position, load, message):
    from digitalmodel.marine_ops.artificial_lift.dynacard.solver_guards import validate_stroke

    with pytest.raises(PhysicsError, match=message):
        validate_stroke([0.0, 1.0, 0.0], position, load, method)


@pytest.mark.parametrize("solver,entry", [
    (DynacardPhysicsSolver, "solve_wave_equation"),
    (FiniteDifferenceSolver, "solve"),
])
def test_candidate_programming_error_propagates(context, monkeypatch, solver, entry):
    def broken_candidate(_):
        raise ValueError("candidate programming error")

    monkeypatch.setattr(solver, entry, broken_candidate)
    with pytest.raises(ValueError, match="candidate programming error"):
        DynacardWorkflow(context).compare_solvers(allow_divergence=True)
