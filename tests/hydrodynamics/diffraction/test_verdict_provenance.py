"""Fail-closed provenance and benchmark-verdict contracts for #1633."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import numpy as np

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    FrequencyData,
    HydrodynamicMatrix,
)


_UNITS = {"translational": "kg", "rotational": "kg.m^2"}


def _matrix(value: float, source: str) -> HydrodynamicMatrix:
    return HydrodynamicMatrix(
        matrix=np.eye(6) * value,
        frequency=1.0,
        matrix_type="added_mass",
        units=_UNITS,
        source=source,
    )


def _derive_status(**kwargs):
    from digitalmodel.hydrodynamics.diffraction.benchmark_verdict import (
        derive_status,
    )

    return derive_status(**kwargs)


def test_placeholder_matrices_cannot_yield_pass():
    matrices = [
        _matrix(1000.0, "placeholder"),
        _matrix(100.0, "placeholder"),
        _matrix(1000.0, "placeholder"),
        _matrix(100.0, "placeholder"),
    ]

    verdict = _derive_status(
        matrices=matrices,
        correlation=1.0,
        quality="COMPARED",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "suspect",
        "matrix provenance must be solver; found placeholder",
    )


def test_identical_quality_yields_suspect():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(1000.0, "solver")],
        correlation=1.0,
        quality="IDENTICAL",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "suspect",
        "comparison inputs are identical",
    )


def test_refused_comparison_is_distinct_from_disagreement():
    incomplete = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=None,
        quality="COMPARED",
        consensus="NO_CONSENSUS",
    )
    disagreement = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=0.5,
        quality="COMPARED",
        consensus="NO_CONSENSUS",
    )

    assert (
        (incomplete.status, incomplete.reason),
        (disagreement.status, disagreement.reason),
    ) == (
        ("incomplete", "comparison refused: correlation unavailable"),
        ("fail", "comparison consensus is NO_CONSENSUS"),
    )


def test_solver_sourced_adequately_sampled_comparison_yields_pass():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=0.995,
        quality="COMPARED",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "pass",
        "solver-sourced comparison has usable statistics and FULL consensus",
    )


def test_solver_sourced_null_response_still_permits_pass():
    verdict = _derive_status(
        matrices=[_matrix(0.0, "solver"), _matrix(0.0, "solver")],
        correlation=1.0,
        quality="NULL_RESPONSE",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "pass",
        "solver-sourced comparison has usable statistics and FULL consensus",
    )


def test_default_unknown_provenance_cannot_yield_pass():
    matrix = HydrodynamicMatrix(
        matrix=np.eye(6),
        frequency=1.0,
        matrix_type="added_mass",
        units=_UNITS,
    )

    verdict = _derive_status(
        matrices=[matrix],
        correlation=0.995,
        quality="COMPARED",
        consensus="FULL",
    )

    assert (matrix.source, verdict.status, verdict.reason) == (
        "unknown",
        "suspect",
        "matrix provenance must be solver; found unknown",
    )


def test_matrix_source_survives_public_dictionary_round_trip():
    frequencies = FrequencyData(
        values=np.array([1.0]),
        periods=None,
        count=0,
        min_freq=0.0,
        max_freq=0.0,
    )
    matrix_set = AddedMassSet(
        vessel_name="provenance-test",
        analysis_tool="test",
        water_depth=100.0,
        matrices=[_matrix(1000.0, "solver")],
        frequencies=frequencies,
        created_date="2026-08-04",
    )

    restored = AddedMassSet.from_dict(matrix_set.to_dict())

    assert restored.matrices[0].source == "solver"


def test_any_refused_comparison_makes_report_incomplete():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=[0.995, None],
        quality=["COMPARED", "INSUFFICIENT_DATA"],
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "incomplete",
        "comparison refused: INSUFFICIENT_DATA",
    )


def test_any_identical_comparison_makes_report_suspect():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=[0.995, 1.0],
        quality=["COMPARED", "IDENTICAL"],
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "suspect",
        "comparison inputs are identical",
    )


def test_non_finite_correlation_is_incomplete():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=float("nan"),
        quality="COMPARED",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "incomplete",
        "comparison refused: correlation must be finite",
    )


def test_out_of_range_correlation_is_incomplete():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "solver")],
        correlation=1.01,
        quality="COMPARED",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "incomplete",
        "comparison refused: correlation must be within [-1, 1]",
    )


def test_single_matrix_input_cannot_yield_pass():
    verdict = _derive_status(
        matrices=[_matrix(1000.0, "solver")],
        correlation=0.995,
        quality="COMPARED",
        consensus="FULL",
    )

    assert (verdict.status, verdict.reason) == (
        "incomplete",
        "comparison requires at least two matrix inputs",
    )


def test_interpolation_cannot_launder_mixed_matrix_provenance():
    script = (
        Path(__file__).resolve().parents[3]
        / "scripts/benchmark/run_3way_benchmark.py"
    )
    spec = importlib.util.spec_from_file_location("_run_3way_benchmark", script)
    module = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise RuntimeError("run_3way_benchmark loader unavailable")
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    source_frequencies = np.array([1.0, 2.0])
    target_frequencies = np.array([1.5])
    source_set = AddedMassSet(
        vessel_name="provenance-test",
        analysis_tool="mixed",
        water_depth=100.0,
        matrices=[_matrix(1000.0, "solver"), _matrix(100.0, "placeholder")],
        frequencies=FrequencyData(
            values=source_frequencies,
            periods=None,
            count=0,
            min_freq=0.0,
            max_freq=0.0,
        ),
        created_date="2026-08-04",
    )
    target_data = FrequencyData(
        values=target_frequencies,
        periods=None,
        count=0,
        min_freq=0.0,
        max_freq=0.0,
    )

    interpolated = module._interpolate_matrix_set(
        source_set,
        source_frequencies,
        target_frequencies,
        target_data,
    )

    assert interpolated.matrices[0].source == "placeholder"
