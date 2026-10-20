# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic examples for the diagnostic evidence contract.

No physical acceptance is inferred. Array sizes and perturbations are test
inputs. Expected values are hand calculations described beside each test.
"""
from copy import deepcopy
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.comparison_framework import DiffractionComparator
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator


def test_legacy_tolerance_is_not_an_acceptance_policy(mock_diffraction_results):
    """A scalar has no uncertainty, resolution or explanation: it must refuse."""
    with pytest.raises(ValueError, match="policy"):
        DiffractionComparator(mock_diffraction_results, mock_diffraction_results, tolerance=0.1)


def test_legacy_without_policy_is_diagnostic(mock_diffraction_results):
    """Equality alone cannot establish engineering acceptance without criteria."""
    report = DiffractionComparator(mock_diffraction_results, mock_diffraction_results).generate_report()
    assert report.overall_agreement is None
    assert report.comparison_status == "REFUSED"
    assert "UNCONFIGURED_POLICY" in report.refusal_reasons


@pytest.mark.parametrize("attribute", ["added_mass", "damping"])
def test_malformed_matrices_export_refusal(mock_diffraction_results, tmp_path, attribute):
    """A ragged sample has no six-DOF coefficient interpretation."""
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    getattr(second, attribute).matrices[0].matrix = [[1], [2, 3]]
    comparator = MultiSolverComparator({"first": first, "second": second})
    target = tmp_path / "refusal.json"
    comparator.export_report_json(target)
    assert "INVALID_ABSCISSA" in target.read_text()


def _hydrostatics():
    return SimpleNamespace(
        displacement_volume=10.0, mass=20.0,
        centre_of_gravity=[0.0, 0.0, 0.0], centre_of_buoyancy=[0.0, 0.0, 0.0],
        waterplane_area=5.0, stiffness_matrix=np.eye(6),
    )


@pytest.mark.parametrize("defect", ["nonfinite", "shape", "overflow", "vector"])
def test_hydrostatic_invalid_evidence_exports_refusal(mock_diffraction_results, tmp_path, defect):
    """Nonfinite, malformed and overflowing differences cannot be measurements."""
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    first.hydrostatics = _hydrostatics()
    second.hydrostatics = _hydrostatics()
    if defect == "nonfinite":
        second.hydrostatics.mass = np.nan
    elif defect == "shape":
        second.hydrostatics.stiffness_matrix = np.ones((3, 3))
    elif defect == "vector":
        second.hydrostatics.centre_of_gravity = [0.0]
    else:
        first.hydrostatics.mass = -np.finfo(float).max
        second.hydrostatics.mass = np.finfo(float).max
    comparator = MultiSolverComparator({"first": first, "second": second})
    report = comparator.generate_report()
    hydro = next(iter(report.pairwise_results.values())).hydrostatic_comparison
    assert hydro.comparison_status == "REFUSED"
    assert report.comparison_status == "REFUSED"
    target = tmp_path / "hydro.json"
    comparator.export_report_json(target)
    assert "INVALID_HYDROSTATICS" in target.read_text()
    assert "NaN" not in target.read_text()
    assert "Infinity" not in target.read_text()


def test_pairwise_exports_actual_sampling_counts(mock_diffraction_results):
    """Identical grids retain their actual row count; headings are not samples."""
    comparator = MultiSolverComparator({"first": mock_diffraction_results, "second": mock_diffraction_results})
    report = comparator.generate_report()
    pair = next(iter(comparator._serialize_pairwise(report.pairwise_results).values()))
    count = mock_diffraction_results.raos.surge.frequencies.count
    assert pair["rao_comparisons"]["surge"]["sample_count"] == count
    assert pair["added_mass_sample_counts"]["1,1"] == count
    assert pair["damping_sample_counts"]["1,1"] == count


def test_coupling_interpolates_signed_coefficients():
    """Hand calculation: y=omega-1.16 on both grids; interpolation gives
    [-0.16,-0.08,0,0.08,0.16], so Pearson r=1 and RMS difference=0.
    Taking magnitudes before interpolation changes the crossing segment.
    """
    from digitalmodel.hydrodynamics.diffraction.benchmark_evidence import matrix_diagnostic
    first_axis = np.array([1.0, 1.08, 1.16, 1.24, 1.32])
    second_axis = np.linspace(1.0, 1.32, 6)
    first = np.broadcast_to((first_axis - 1.16)[:, None, None], (5, 6, 6))
    second = np.broadcast_to((second_axis - 1.16)[:, None, None], (6, 6, 6))
    evidence = matrix_diagnostic(first, second, first_axis, second_axis)
    assert evidence["correlation"][0][0] == pytest.approx(1.0)
    assert evidence["rms_error"][0][0] == pytest.approx(0.0, abs=1e-15)
    assert evidence["sample_count"] == 5


@pytest.mark.parametrize("defect", ["constant", "missing_axis", "nonfinite", "ragged", "short", "disjoint"])
def test_coupling_never_invents_correlation(defect):
    """Pearson denominator is zero for constants; invalid evidence is refused."""
    from digitalmodel.hydrodynamics.diffraction.benchmark_evidence import matrix_diagnostic
    axis = np.linspace(1.0, 1.4, 5)
    values = np.broadcast_to(axis[:, None, None], (5, 6, 6)).copy()
    other = values.copy()
    second_axis = axis.copy()
    if defect == "constant":
        values.fill(1.0)
        other.fill(1.0)
    elif defect == "missing_axis":
        second_axis = None
    elif defect == "nonfinite":
        other[0, 0, 0] = np.nan
    elif defect == "ragged":
        other = [[1], [2, 3]]
    elif defect == "short":
        axis, values = axis[:2], values[:2]
        second_axis, other = second_axis[:2], other[:2]
    else:
        second_axis += 10.0
    evidence = matrix_diagnostic(values, other, axis, second_axis)
    assert evidence["correlation"][0][0] is None
    assert evidence["comparison_status"] == "REFUSED"


def test_verdict_refusal_vocabulary_is_shared():
    """An invalid abscissa must retain its refusal rather than becoming a pass."""
    from digitalmodel.hydrodynamics.diffraction.benchmark_verdict import derive_status
    verdict = derive_status(matrices=[], correlation=1.0, quality="INVALID_ABSCISSA", consensus="FULL")
    assert "INVALID_ABSCISSA" in verdict.reason


def test_legacy_json_preserves_refusal_and_sampling(mock_diffraction_results, tmp_path):
    """A refused report remains strict JSON, including integer array locations."""
    import json
    target = tmp_path / "legacy.json"
    DiffractionComparator(mock_diffraction_results, mock_diffraction_results).export_report(target)
    data = json.loads(target.read_text())
    assert data["comparison_status"] == "REFUSED"
    assert data["rao_comparisons"]["surge"]["sample_count"] == mock_diffraction_results.raos.surge.frequencies.count


def test_canonical_exporters_share_sampling_and_hydrostatic_refusal(mock_diffraction_results, tmp_path):
    """Both JSON producers must describe exactly the same pairwise evidence."""
    from digitalmodel.hydrodynamics.diffraction.benchmark_runner import BenchmarkConfig, BenchmarkRunner
    from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import BenchmarkPlotter
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    first.hydrostatics = _hydrostatics()
    second.hydrostatics = _hydrostatics()
    second.hydrostatics.mass = np.inf
    comparator = MultiSolverComparator({"first": first, "second": second})
    report = comparator.generate_report()
    runner = BenchmarkRunner(BenchmarkConfig(output_dir=tmp_path))
    serialized = runner._report_to_dict(report)
    pair = next(iter(serialized["pairwise_results"].values()))
    assert pair["hydrostatic_comparison"]["mass_diff"] is None
    assert pair["hydrostatic_comparison"]["comparison_status"] == "REFUSED"
    assert pair["added_mass_sample_counts"]["1,1"] == first.added_mass.frequencies.count
    # A refused coupling cell must be visible rather than an unlabelled dash.
    blank = [[None] * 6 for _ in range(6)]
    quality = [["INVALID_ABSCISSA"] * 6 for _ in range(6)]
    page = BenchmarkPlotter.build_coupling_heatmap_html(tmp_path, blank, blank, "first", "second", am_quality=quality, damp_quality=quality)
    assert page.read_text().count('class="cell-refused"') == 72


def test_cli_rejects_scalar_before_reading_solver_files(tmp_path):
    """No converter or solver may run for an unqualified scalar tolerance."""
    from click.testing import CliRunner
    from digitalmodel.hydrodynamics.diffraction.cli import cli
    folder = tmp_path / "inputs"
    folder.mkdir()
    model = folder / "model.sim"
    model.write_text("synthetic fixture; no native result")
    result = CliRunner().invoke(cli, ["compare", str(folder), str(model), "synthetic", "--water-depth", "100", "--tolerance", "0.1"])
    assert result.exit_code != 0
    assert "policy" in result.output


@pytest.mark.parametrize("attribute", ["added_mass", "damping"])
def test_matrix_numeric_domain_refuses_overflow(mock_diffraction_results, tmp_path, attribute):
    """Finite inputs with an unrepresentable difference have no finite RMS."""
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    getattr(first, attribute).matrices[0].matrix.fill(-np.finfo(float).max)
    getattr(second, attribute).matrices[0].matrix.fill(np.finfo(float).max)
    comparator = MultiSolverComparator({"first": first, "second": second})
    comparator.export_report_json(tmp_path / "overflow.json")
    stats = getattr(comparator, "compare_" + attribute)()
    assert next(iter(stats.values()))[(1, 1)].quality == "INSUFFICIENT_DATA"


def test_worked_example():
    """Affine interpolation gives r=1 and RMS=0; constants have no Pearson r."""
    import runpy
    from pathlib import Path
    example = Path(__file__).resolve().parents[3] / "examples" / "diffraction_evidence_contract.py"
    answer = runpy.run_path(str(example))["demonstrate"]()
    assert answer["aligned"]["correlation"] == pytest.approx(1.0)
    assert answer["aligned"]["rms_error"] == pytest.approx(0.0, abs=1e-15)
    assert answer["aligned"]["sample_count"] == 5
    assert answer["constant"] == {"correlation": None, "quality": "INSUFFICIENT_DATA"}
    assert answer["missing_axis"] == {"correlation": None, "quality": "INVALID_ABSCISSA"}


def test_legacy_rao_quality_matches_canonical_null_response(mock_diffraction_results):
    """Zero response is NULL_RESPONSE, not structural coefficient absence."""
    first = deepcopy(mock_diffraction_results)
    for name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        getattr(first.raos, name).magnitude.fill(0.0)
    legacy = DiffractionComparator(first, first).compare_raos()["surge"]
    canonical = MultiSolverComparator({"first": first, "second": first}).compare_raos()
    assert legacy.statistics.quality == next(iter(canonical.values()))["surge"].magnitude_stats.quality


def test_sampling_count_survives_undefined_correlation():
    """Five aligned constant samples exist even though their variance is zero."""
    axis = np.linspace(1.0, 1.32, 5)
    stats = MultiSolverComparator._calculate_deviation_stats(np.ones(5), np.ones(5), axis)
    assert stats.correlation is None
    assert stats.sample_count == 5


def test_hydro_data_preserves_malformed_matrix_refusal(mock_diffraction_results, tmp_path):
    """Raw YAML must retain the refusal, not crash or present a ragged matrix."""
    import yaml
    from digitalmodel.hydrodynamics.diffraction.benchmark_runner import BenchmarkConfig, BenchmarkRunner
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    second.added_mass.matrices[0].matrix = [[1], [2, 3]]
    results = {"first": first, "second": second}
    report = MultiSolverComparator(results).generate_report()
    runner = BenchmarkRunner(BenchmarkConfig(output_dir=tmp_path))
    target = runner._generate_hydro_data_yaml(results, report)
    data = yaml.safe_load(target.read_text())
    assert data["comparison"]["comparison_status"] == "REFUSED"
    assert data["solvers"]["second"]["added_mass"][0] is None
    assert data["solvers"]["second"]["raw_data_refusals"] == ["added_mass:0"]


def test_legacy_policy_controls_agreement(mock_diffraction_results):
    """Hand calculation: b=1.05a has r=1, but symmetric relative RMS is
    0.05/sqrt((1+1.05**2)/2), between 0.002 and 0.2. Synthetic pair
    uncertainty budgets of 2*0.001 and 2*0.1 must give opposite outcomes.
    """
    from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import ComparisonPolicy
    first = deepcopy(mock_diffraction_results)
    second = deepcopy(first)
    for name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        getattr(second.raos, name).magnitude *= 1.05
    results = []
    for uncertainty in (0.001, 0.1):
        policy = ComparisonPolicy(uncertainty, 1e-9, 0.81, "Synthetic budget; no engineering acceptance.")
        results.append(DiffractionComparator(first, second, policy=policy).generate_report().overall_agreement)
    assert results == ["POOR", "EXCELLENT"]
