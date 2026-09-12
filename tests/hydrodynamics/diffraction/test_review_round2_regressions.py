"""Regression tests for the second adversarial review of issue 1633."""

from __future__ import annotations

import json

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    ComparisonPolicy,
    MultiSolverComparator,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    HydrostaticResults,
)
from tests.hydrodynamics.diffraction.test_abscissa_contract import (
    AQWA_L01_FREQUENCIES,
    ORCAWAVE_L01_FREQUENCIES,
)
from tests.hydrodynamics.diffraction.test_benchmark_runner import (
    _load_ship_benchmark_script,
)
from tests.hydrodynamics.diffraction import test_unit_box_benchmark as unit_box


def _relative_bound_holds(actual: np.ndarray, nominal: np.ndarray) -> bool:
    nonzero = nominal != 0.0
    relative = np.zeros_like(actual, dtype=float)
    relative[nonzero] = np.abs(actual[nonzero] / nominal[nonzero] - 1.0)
    return bool(
        np.all(relative[nonzero] <= unit_box.SOLVER_RELATIVE_UNCERTAINTY)
        and np.array_equal(actual[~nonzero], nominal[~nonzero])
    )


def _nominal_rao_magnitude(dof: DOF) -> np.ndarray:
    if dof == DOF.HEAVE:
        base = unit_box._unit_box_heave_rao(unit_box.FREQUENCIES)
    elif dof in (DOF.SURGE, DOF.SWAY):
        base = unit_box._unit_box_surge_rao(unit_box.FREQUENCIES)
    elif dof in (DOF.PITCH, DOF.ROLL):
        base = unit_box._unit_box_pitch_rao(unit_box.FREQUENCIES)
    else:
        base = np.full(unit_box.N_FREQ, 1e-4)
    headings = np.array([1.0, 0.7, 1.0, 0.7])
    if dof in (DOF.SWAY, DOF.ROLL):
        headings = np.array([0.1, 1.0, 0.1, 1.0])
    return np.outer(base, headings)


@pytest.mark.parametrize("dof", list(DOF))
def test_unit_box_rao_each_sample_respects_declared_relative_uncertainty(
    dof: DOF,
) -> None:
    actual = unit_box._unit_box_rao_component(
        dof,
        unit_box.FREQUENCIES,
        seed=7,
    ).magnitude

    assert _relative_bound_holds(actual, _nominal_rao_magnitude(dof)) is True


def test_unit_box_rao_perturbation_is_per_sample() -> None:
    actual = unit_box._unit_box_rao_component(
        DOF.YAW,
        unit_box.FREQUENCIES,
        seed=7,
    ).magnitude
    ratio = actual / _nominal_rao_magnitude(DOF.YAW)

    assert np.ptp(ratio) > 0.0


def test_unit_box_added_mass_each_sample_respects_declared_uncertainty() -> None:
    actual = unit_box._unit_box_added_mass(
        unit_box.FREQUENCIES,
        seed=7,
    )
    diagonal = np.array([250.0, 250.0, 100.0, 25.0, 25.0, 5.0])
    nominal = []
    for frequency in unit_box.FREQUENCIES:
        matrix = np.diag(diagonal * (1.0 + 0.1 * np.exp(-frequency)))
        matrix[0, 4] = matrix[4, 0] = 5.0
        matrix[1, 3] = matrix[3, 1] = 5.0
        nominal.append(matrix)

    assert all(
        _relative_bound_holds(a.matrix, n)
        for a, n in zip(actual.matrices, nominal)
    ) is True


def test_unit_box_damping_each_sample_respects_declared_uncertainty() -> None:
    actual = unit_box._unit_box_damping(
        unit_box.FREQUENCIES,
        seed=7,
    )
    diagonal = np.array([50.0, 50.0, 30.0, 5.0, 5.0, 1.0])
    nominal = [np.diag(diagonal * frequency) for frequency in unit_box.FREQUENCIES]

    assert all(
        _relative_bound_holds(a.matrix, n)
        for a, n in zip(actual.matrices, nominal)
    ) is True


def _ship_rao_data(frequencies: np.ndarray) -> dict:
    return {
        0.0: {
            dof: {
                "freq": frequencies.tolist(),
                "amp": (frequencies * dof.value).tolist(),
                "phase": np.zeros(frequencies.size).tolist(),
            }
            for dof in DOF
        }
    }


def test_real_l01_gap_produces_refusal_artifact(tmp_path) -> None:
    script = _load_ship_benchmark_script()
    first = script.create_diffraction_results(
        _ship_rao_data(AQWA_L01_FREQUENCIES), "SyntheticShip", "AQWA",
    )
    second = script.create_diffraction_results(
        _ship_rao_data(ORCAWAVE_L01_FREQUENCIES), "SyntheticShip", "OrcaWave",
    )
    config = BenchmarkConfig(
        output_dir=tmp_path,
        dry_run=True,
        solver_relative_uncertainty=0.01,
        response_absolute_resolution=1e-12,
        minimum_explained_variance=0.5,
        comparison_justification=(
            "Test-only policy; abscissa refusal precedes numeric verdict gates."
        ),
    )

    result = BenchmarkRunner(config).run_from_results(
        {"AQWA": first, "OrcaWave": second}
    )
    actual = None
    if result.report_json_path is not None:
        data = json.loads(result.report_json_path.read_text(encoding="utf-8"))
        actual = (
            result.success,
            data["comparison_status"],
            data["overall_consensus"],
            data["refusal_reasons"],
            (
                "first source relative gap 0.756757 over "
                "[0.407000, 0.715000] rad/s"
            ) in result.report_html_path.read_text(encoding="utf-8"),
        )

    assert actual == (
        True,
        "REFUSED",
        None,
        [
            "AbscissaGapError: first source relative gap 0.756757 over "
            "[0.407000, 0.715000] rad/s exceeds maximum 0.100000",
            "UNTRUSTED_SOURCE",
        ],
        True,
    )


def test_runner_propagates_programming_errors(
    monkeypatch: pytest.MonkeyPatch,
    two_identical_results,
) -> None:
    runner = BenchmarkRunner(BenchmarkConfig(dry_run=True))

    def fail(_results):
        raise TypeError("plotting bug")

    monkeypatch.setattr(runner, "_compare", fail)

    with pytest.raises(TypeError, match="^plotting bug$"):
        runner.run_from_results(two_identical_results)


def test_policy_declares_phase_is_diagnostic_only() -> None:
    policy = ComparisonPolicy.from_uncertainties(
        solver_relative_uncertainty=0.01,
        response_absolute_resolution=1e-12,
        minimum_explained_variance=0.5,
        justification="Test-only policy.",
    )

    assert policy.to_dict()["phase_verdict_role"] == "diagnostic_only"


def test_two_solver_budget_accepts_difference_between_one_and_two_shares(
    two_identical_results,
) -> None:
    uncertainty = 0.01
    for dof in DOF:
        component = getattr(
            two_identical_results["SolverB"].raos, dof.name.lower()
        )
        component.magnitude = component.magnitude * (1.0 + 1.5 * uncertainty)
    policy = ComparisonPolicy.from_uncertainties(
        solver_relative_uncertainty=uncertainty,
        response_absolute_resolution=1e-12,
        minimum_explained_variance=0.5,
        justification=(
            "Each of two synthetic solvers contributes one uncertainty share."
        ),
    )

    consensus = MultiSolverComparator(
        two_identical_results, policy=policy,
    ).compute_consensus()

    assert consensus["HEAVE"].consensus_level == "FULL"


def test_hydrostatic_constant_matrices_have_undefined_correlation(
    two_identical_results,
) -> None:
    for index, result in enumerate(two_identical_results.values(), start=1):
        result.hydrostatics = HydrostaticResults(
            vessel_name=result.vessel_name,
            displacement_volume=1.0,
            mass=1.0,
            centre_of_gravity=[0.0, 0.0, 0.0],
            centre_of_buoyancy=[0.0, 0.0, 0.0],
            waterplane_area=1.0,
            stiffness_matrix=np.full((6, 6), index * 1e-9),
        )

    comparison = MultiSolverComparator(
        two_identical_results
    ).compare_hydrostatics()["SolverA-vs-SolverB"]

    assert comparison.stiffness_matrix_correlation is None
