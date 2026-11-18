# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline step 1 controls; synthetic evidence never qualifies a native run."""

import copy
import hashlib
import json
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[4]
FIXTURE = ROOT / "tests/fixtures/test_vectors/marine/kcs_resistance_efd.yaml"
ROW_ID = "kcs_t2015_case_2_1_free_with_rudder"


def reference():
    data = yaml.safe_load(FIXTURE.read_text())
    return next(row for row in data["reference_rows"] if row["id"] == ROW_ID)


def test_reference_preserves_values_and_exposes_unverified_source_units():
    row = reference()
    series = row["series"]
    assert [p["ct"] for p in series["points"]] == [
        0.003796, 0.003641, 0.003475, 0.003467, 0.003711, 0.004501
    ]
    assert series["published_data_file"] == "vary_Fr_2-1.xls"
    assert series["source_document"] == "Tokyo 2015 workshop, Case 2.1 instructions"
    assert series["source_url"].endswith("/Case_2.1/Case_2-1.html")
    assert series["readback"]["status"] == "pending"
    assert series["readback"]["sheet_name"] is None
    assert series["units_status"] == "inherited_unverified"
    assert series["units"] == {"froude": "1", "ct": "1", "sinkage_cm": "cm", "trim_deg": "deg"}
    assert set(series["page_locators"]) == {"ct", "sinkage_cm", "trim_deg"}
    assert "no licence statement was found" in series["licence_note"].lower()
    assert "not redistributed" in series["licence_note"]
    assert row["reynolds_applies_at_froude"] == 0.26


def test_three_grid_known_quadratic_answer():
    from digitalmodel.solvers.openfoam.validation.kcs_step1 import three_grid_gci

    report = three_grid_gci([1, 2, 4], [1.02, 1.08, 1.32])
    assert report["p"] == pytest.approx(2)
    assert report["f_ext"] == pytest.approx(1)
    assert report["gci_fine_pct"] == pytest.approx(100 * 1.25 * 0.02 / 1.02)
    assert report["asymptotic_ratio"] == pytest.approx(1)
    assert report["method_source"]["section"]


@pytest.mark.parametrize("h,values", [
    ([1, 2], [1, 2]),
    ([1, 2, 3], [1.02, 1.08, 1.18]),
    ([2, 1, 4], [1.02, 1.08, 1.32]),
    ([1, 2, 4], [1, 1, 1]),
    ([1, 2, 4], [1, 2, 1.5]),
    ([1, 2, 4], [1, 3, 4]),
    ([1, 2, 4], [1, 2, float("nan")]),
    ([1, 2, float("inf")], [1, 2, 3]),
    ([1, 2, 4], [0, 1, 5]),
])
def test_unqualified_grid_series_refused(h, values):
    from digitalmodel.solvers.openfoam.validation.kcs_step1 import three_grid_gci

    with pytest.raises(ValueError):
        three_grid_gci(h, values)


def synthetic_case(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    policy = json.loads((ROOT / "config/cfd/kcs_step1_qa.json").read_text())
    row = reference()
    point = row["series"]["points"][4]
    history = tmp_path / "history.csv"
    history.write_text("time_s,ct,cp,cv,sinkage_cm,trim_deg\n" + "".join(
        f"{i / 2},{point['ct']},{point['ct'] * .2},{point['ct'] * .8},"
        f"{point['sinkage_cm']},{point['trim_deg']}\n" for i in range(81)
    ))
    metadata = {
        "schema_version": 1, "evidence_kind": "synthetic", "mesh": "coarse",
        "reference_sha256": qa.digest(row), "criteria_sha256": qa.digest(policy),
        "history_sha256": hashlib.sha256(history.read_bytes()).hexdigest(),
        "conditions": qa.reference_conditions(row, .26),
        "units": {"time_s": "s", "ct": "1", "cp": "1", "cv": "1", "sinkage_cm": "cm", "trim_deg": "deg"},
        "sign_convention": {"sinkage_cm": "positive_up", "trim_deg": "positive_bow_up"},
        "residuals": {"p_rgh": 1e-5, "U": 1e-6},
        "y_plus": {"min": .1, "mean": .8, "max": 1.6},
        "free_surface": {"dx_m": .05, "dz_m": .004},
        "h_m": .05, "mesh_family": "synthetic_family", "numerics": "synthetic_scheme",
        "refinement_axes": "xyz",
    }
    return history, metadata, row, policy


def bind_history(history, metadata):
    """Pair intentionally perturbed synthetic inputs for a specific control."""
    metadata["history_sha256"] = hashlib.sha256(history.read_bytes()).hexdigest()


def test_known_history_comparison_reports_limits_without_native_qualification(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    report = qa.compare_run(history, meta, row, policy)
    assert report["comparison_passed"] is True
    assert report["qualified"] is False
    assert report["evidence_kind"] == "synthetic"
    assert report["force_convergence"]["passed"] is True
    assert report["diagnostics"]["y_plus"] == meta["y_plus"]
    assert report["diagnostics"]["free_surface"] == meta["free_surface"]
    assert report["criteria_sha256"] == qa.digest(policy)
    assert report["history_sha256"]


@pytest.mark.parametrize("column,bad_value", [("ct", .005), ("sinkage_cm", 1.394), ("trim_deg", .169)])
def test_perturbed_quantity_fails_its_explicit_tolerance(tmp_path, column, bad_value):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    lines = history.read_text().splitlines()
    index = lines[0].split(",").index(column)
    rows = [line.split(",") for line in lines[1:]]
    for values in rows:
        values[index] = str(bad_value)
        if column == "ct":
            values[2:4] = [str(.2 * bad_value), str(.8 * bad_value)]
    history.write_text(lines[0] + "\n" + "\n".join(",".join(values) for values in rows) + "\n")
    bind_history(history, meta)
    report = qa.compare_run(history, meta, row, policy)
    assert report["quantities"][column]["passed"] is False
    assert report["comparison_passed"] is False


@pytest.mark.parametrize("field", ["residuals", "y_plus", "free_surface", "units", "conditions"])
def test_missing_observed_diagnostic_or_basis_is_rejected(tmp_path, field):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    del meta[field]
    with pytest.raises(ValueError):
        qa.compare_run(history, meta, row, policy)


@pytest.mark.parametrize("mutation", ["nan", "duplicate", "reverse", "irregular", "short", "unbalanced"])
def test_bad_history_is_rejected(tmp_path, mutation):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    lines = history.read_text().splitlines()
    rows = [line.split(",") for line in lines[1:]]
    if mutation == "nan":
        rows[-1][1] = "nan"
    elif mutation == "duplicate":
        rows[-1][0] = rows[-2][0]
    elif mutation == "reverse":
        rows.reverse()
    elif mutation == "irregular":
        rows[-2][0] = str(float(rows[-2][0]) + .1)
    elif mutation == "short":
        rows = rows[-10:]
    else:
        rows[-1][1] = ".1"
    history.write_text(lines[0] + "\n" + "\n".join(",".join(values) for values in rows) + "\n")
    with pytest.raises(ValueError):
        qa.compare_run(history, meta, row, policy)


def admission_inputs(tmp_path):
    """Fabricated unit-test documents; these are never solver evidence."""
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    row["series"]["units_status"] = "verified"
    row["series"]["readback"] = {
        "status": "verified", "sheet_name": "synthetic_sheet",
        "source_sha256": "a" * 64,
        "table_sha256": qa.digest({k: row["series"][k] for k in ("points", "units", "sign_convention")}),
        "cells": {k: "synthetic_cell" for k in ("ct", "sinkage_cm", "trim_deg")},
    }
    meta["evidence_kind"] = "native"
    meta["reference_sha256"] = qa.digest(row)
    ref_path, policy_path, meta_path = [tmp_path / name for name in ("reference.yaml", "policy.json", "coarse.json")]
    ref_path.write_text(yaml.safe_dump({"reference_rows": [row]}))
    policy_path.write_text(json.dumps(policy))
    meta_path.write_text(json.dumps(meta))
    return ref_path, policy_path, history, meta_path


@pytest.mark.parametrize("next_mesh", ["medium", "fine"])
def test_run_admission_recomputes_passing_coarse_evidence(tmp_path, next_mesh):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    args = admission_inputs(tmp_path)
    result = qa.require_run_admission(next_mesh, *args)
    assert result["admitted"] is True
    assert result["mesh"] == next_mesh
    assert result["coarse_history_sha256"]


@pytest.mark.parametrize("missing", ["reference", "criteria", "smoke"])
def test_each_missing_prerequisite_refuses_medium_and_fine(tmp_path, missing):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    ref, policy, history, meta = admission_inputs(tmp_path)
    if missing == "reference":
        ref = tmp_path / "absent-reference.yaml"
    elif missing == "criteria":
        policy = tmp_path / "absent-criteria.json"
    else:
        history = None
    for mesh in ("medium", "fine"):
        with pytest.raises(ValueError, match=missing):
            qa.require_run_admission(mesh, ref, policy, history, meta)


def test_committed_pending_reference_refuses_run_admission(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    _, policy, history, meta = admission_inputs(tmp_path)
    with pytest.raises(ValueError, match="reference"):
        qa.require_run_admission("medium", FIXTURE, policy, history, meta)


@pytest.mark.parametrize("change", ["synthetic", "stale_criteria", "stale_reference", "froude", "body", "mesh", "units", "sign", "diagnostics", "failed_comparison"])
def test_stale_or_failed_coarse_evidence_refuses_run_admission(tmp_path, change):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    ref, policy, history, meta_path = admission_inputs(tmp_path)
    meta = json.loads(meta_path.read_text())
    if change == "synthetic":
        meta["evidence_kind"] = "synthetic"
    elif change == "stale_criteria":
        meta["criteria_sha256"] = "b" * 64
    elif change == "stale_reference":
        meta["reference_sha256"] = "b" * 64
    elif change == "froude":
        meta["conditions"]["froude"] = .152
    elif change == "body":
        meta["conditions"]["body_condition"] = "fixed_even_keel"
    elif change == "mesh":
        meta["mesh"] = "medium"
    elif change == "units":
        meta["units"]["sinkage_cm"] = "m"
    elif change == "sign":
        meta["sign_convention"]["trim_deg"] = "positive_bow_down"
    elif change == "diagnostics":
        meta["y_plus"]["max"] = 50
    else:
        history.write_text(history.read_text().replace("-0.169", "0.169"))
        bind_history(history, meta)
        meta["passed"] = True  # a caller's verdict must never release a run
    meta_path.write_text(json.dumps(meta))
    with pytest.raises(ValueError):
        qa.require_run_admission("fine", ref, policy, history, meta_path)


@pytest.mark.parametrize("problem", ["drift", "fluctuation"])
def test_unsettled_force_history_fails_without_changing_average_target(tmp_path, problem):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    lines = history.read_text().splitlines()
    for i in range(1, len(lines)):
        values = lines[i].split(",")
        factor = (1.005 if i <= 61 else 1) if problem == "drift" else (1.02 if i % 2 else .98)
        for j in (1, 2, 3):
            values[j] = str(float(values[j]) * factor)
        lines[i] = ",".join(values)
    history.write_text("\n".join(lines) + "\n")
    bind_history(history, meta)
    assert qa.compare_run(history, meta, row, policy)["force_convergence"]["passed"] is False


def test_comparison_uses_unrounded_boundary(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    lines = history.read_text().splitlines()
    for i in range(1, len(lines)):
        values = lines[i].split(",")
        for j in (1, 2, 3):
            values[j] = str(float(values[j]) * 1.030000001)
        lines[i] = ",".join(values)
    history.write_text("\n".join(lines) + "\n")
    bind_history(history, meta)
    assert qa.compare_run(history, meta, row, policy)["quantities"]["ct"]["passed"] is False


def triplet_inputs(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    paths, metas = [], []
    for mesh, factor in (("fine", 1), ("medium", 2), ("coarse", 4)):
        folder = tmp_path / mesh
        folder.mkdir()
        history, meta, row, policy = synthetic_case(folder)
        meta.update(mesh=mesh, h_m=.0125 * factor)
        meta["free_surface"] = {"dx_m": .0125 * factor, "dz_m": .001 * factor}
        lines = history.read_text().splitlines()
        for i in range(1, len(lines)):
            values = lines[i].split(",")
            for j in (1, 2, 3):
                values[j] = str(float(values[j]) * (1 + .001 * factor**2))
            lines[i] = ",".join(values)
        history.write_text("\n".join(lines) + "\n")
        bind_history(history, meta)
        paths.append(history)
        metas.append(meta)
    return paths, metas, row, policy


def test_triplet_readback_reports_gci_but_synthetic_is_not_qualified(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    report = qa.compare_triplet(*triplet_inputs(tmp_path))
    assert report["gci"]["ct"]["p"] == pytest.approx(2)
    assert report["gci"]["ct"]["gci_fine_pct"] == pytest.approx(100 * 1.25 * .001 / 1.001)
    assert report["gci_passed"] is True
    assert report["qualified"] is False
    assert set(report["gci"]) == {"ct", "cp", "cv"}


@pytest.mark.parametrize("problem", ["missing_mesh", "duplicate_mesh", "family", "numerics", "axes", "surface_ratio"])
def test_triplet_requires_observed_systematic_mesh_family(tmp_path, problem):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    paths, metas, row, policy = triplet_inputs(tmp_path)
    if problem == "missing_mesh":
        paths.pop()
        metas.pop()
    elif problem == "duplicate_mesh":
        metas[-1]["mesh"] = "medium"
    elif problem == "family":
        metas[-1]["mesh_family"] = "other_family"
    elif problem == "numerics":
        metas[-1]["numerics"] = "other_scheme"
    elif problem == "axes":
        metas[-1]["refinement_axes"] = "xy"
    else:
        metas[-1]["free_surface"]["dz_m"] *= .9
    with pytest.raises(ValueError):
        qa.compare_triplet(paths, metas, row, policy)


def test_cli_reads_actual_files_and_never_releases_pending_reference(tmp_path):
    import os
    import subprocess
    import sys

    ref, policy, history, meta_path = admission_inputs(tmp_path)
    output = tmp_path / "report.json"
    env = {**os.environ, "PYTHONPATH": str(ROOT / "src")}
    script = ROOT / "scripts/cfd/readback_kcs_step1.py"
    compared = subprocess.run([sys.executable, str(script), "compare", "--reference", str(ref),
                               "--criteria", str(policy), "--history", str(history),
                               "--metadata", str(meta_path), "--output", str(output)],
                              env=env, capture_output=True, text=True)
    assert compared.returncode == 0, compared.stderr
    assert json.loads(output.read_text())["comparison_passed"] is True
    refused = subprocess.run([sys.executable, str(script), "admit", "--reference", str(FIXTURE),
                              "--criteria", str(policy), "--next-mesh", "medium",
                              "--history", str(history), "--metadata", str(meta_path),
                              "--output", str(output)], env=env, capture_output=True, text=True)
    assert refused.returncode == 1
    assert json.loads(output.read_text())["admitted"] is False
    assert "reference" in json.loads(output.read_text())["reason"]


@pytest.mark.parametrize("problem", ["missing_force", "missing_tolerance", "nan_limit", "zero_window", "wrong_units", "extra_quantity", "unsupported_speed", "loosened_limit"])
def test_invalid_predeclared_criteria_refuse_comparison(tmp_path, problem):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    if problem == "missing_force":
        del policy["force_history"]
    elif problem == "missing_tolerance":
        del policy["tolerances"]["trim_deg"]
    elif problem == "nan_limit":
        policy["tolerances"]["ct"]["limit"] = float("nan")
    elif problem == "zero_window":
        policy["force_history"]["window_seconds"] = 0
    elif problem == "wrong_units":
        policy["tolerances"]["sinkage_cm"]["units"] = "m"
    elif problem == "extra_quantity":
        policy["tolerances"]["other"] = copy.deepcopy(policy["tolerances"]["ct"])
    elif problem == "unsupported_speed":
        policy["froude"] = .152
    else:
        policy["tolerances"]["ct"]["limit"] = 1e9
    with pytest.raises(ValueError):
        qa.validate_policy(policy)


def test_grid_uncertainty_refuses_unsettled_histories(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    paths, metas, row, policy = triplet_inputs(tmp_path)
    rows = paths[0].read_text().splitlines()
    for i in range(62, len(rows)):
        values = rows[i].split(",")
        for j in (1, 2, 3):
            values[j] = str(float(values[j]) * 1.01)
        rows[i] = ",".join(values)
    paths[0].write_text("\n".join(rows) + "\n")
    bind_history(paths[0], metas[0])
    with pytest.raises(ValueError, match="convergence"):
        qa.compare_triplet(paths, metas, row, policy)


def test_changed_reference_values_invalidate_source_readback_even_with_new_run_digest(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    ref, policy, history, meta_path = admission_inputs(tmp_path)
    fixture = yaml.safe_load(ref.read_text())
    row = fixture["reference_rows"][0]
    row["series"]["points"][4]["ct"] *= 1.001
    meta = json.loads(meta_path.read_text())
    meta["reference_sha256"] = qa.digest(row)
    ref.write_text(yaml.safe_dump(fixture))
    meta_path.write_text(json.dumps(meta))
    with pytest.raises(ValueError, match="reference"):
        qa.require_run_admission("medium", ref, policy, history, meta_path)


def test_native_coarse_request_still_requires_correct_case_conditions(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    ref, policy, _, _ = admission_inputs(tmp_path)
    fixture = yaml.safe_load(ref.read_text())
    fixture["reference_rows"][0]["appendages"] = "none"
    ref.write_text(yaml.safe_dump(fixture))
    with pytest.raises(ValueError):
        qa.require_run_admission("coarse", ref, policy)


@pytest.mark.parametrize("quantity", ["sinkage_cm", "trim_deg"])
@pytest.mark.parametrize("problem", ["drift", "fluctuation"])
def test_unsettled_motion_cannot_pass_on_a_matching_mean(tmp_path, quantity, problem):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    lines = history.read_text().splitlines()
    j = lines[0].split(",").index(quantity)
    offset = .06 if quantity == "sinkage_cm" else .012
    for i in range(1, len(lines)):
        values = lines[i].split(",")
        delta = (offset if i <= 61 else 0) if problem == "drift" else (offset if i % 2 else -offset)
        values[j] = str(float(values[j]) + delta)
        lines[i] = ",".join(values)
    history.write_text("\n".join(lines) + "\n")
    bind_history(history, meta)
    report = qa.compare_run(history, meta, row, policy)
    assert report["quantities"][quantity]["passed"] is True
    assert report["comparison_passed"] is False


def test_implausible_observed_order_does_not_pass_with_tiny_gci():
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    with pytest.raises(ValueError, match="order"):
        qa.three_grid_gci([1, 2, 4], [1, 1.00001, 2])


@pytest.mark.parametrize("field", ["sign", "reynolds_applicability"])
def test_reference_convention_changes_refuse_comparison(tmp_path, field):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    if field == "sign":
        row["series"]["sign_convention"]["trim_deg"] = "positive_bow_down"
    else:
        row["reynolds_applies_at_froude"] = .152
    meta["reference_sha256"] = qa.digest(row)
    with pytest.raises(ValueError):
        qa.compare_run(history, meta, row, policy)


def test_unpaired_metadata_cannot_validate_a_different_history(tmp_path):
    from digitalmodel.solvers.openfoam.validation import kcs_step1 as qa

    history, meta, row, policy = synthetic_case(tmp_path)
    history.write_text(history.read_text() + "\n")
    with pytest.raises(ValueError, match="history"):
        qa.compare_run(history, meta, row, policy)
