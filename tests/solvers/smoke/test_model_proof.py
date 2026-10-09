"""The parent independently checks native proof completeness and readback data."""
import copy

import pytest

from digitalmodel.solvers.smoke.model_proof import validate_report


@pytest.fixture
def proof():
    return dict(ok=True, phase="solve", stage="complete", version="11.6c",
                settings_verified=True, input_readback_verified=True,
                simulation_complete=True, fidelity_verified=False,
                thread_count_requested=1, thread_count_observed=1,
                manifest_sha256="a" * 64, loaded_data_sha256="b" * 64,
                simulation_sha256="c" * 64, warnings=[], results={
                    "times": [i / 10 for i in range(1001)],
                    "histories": {key: [1.0] * 1001 for key in (
                        "tension_end_a", "tension_end_b", "buoy_x", "buoy_z",
                        "buoy_rotation_2", "sea_surface_z")},
                    "static": {"line_arclength": [0, 620],
                               "line_position": {key: [0, 1] for key in "XYZ"},
                               "buoy_position": {key: 1 for key in "XYZ"},
                               "end_tensions": {key: 1 for key in "AB"}},
                    "units": {"times": "s"}, "frames": {"positions": "global"}})


def test_complete_proof_and_exact_readback(proof):
    validate_report(proof, "solve", "a" * 64)
    reader = {**copy.deepcopy(proof), "phase": "readback", "fidelity_verified": True}
    validate_report(reader, "readback", "a" * 64, proof)
    reader["results"]["histories"]["buoy_x"][0] += 1
    with pytest.raises(ValueError):
        validate_report(reader, "readback", "a" * 64, proof)


@pytest.mark.parametrize("key", ["stage", "version", "settings_verified",
    "input_readback_verified", "simulation_complete", "manifest_sha256",
    "loaded_data_sha256", "results", "warnings"])
def test_incomplete_proof_rejected(proof, key):
    del proof[key]
    with pytest.raises(ValueError):
        validate_report(proof, "solve", "a" * 64)


@pytest.mark.parametrize("mutation", ["missing_history", "short_grid", "nan", "bool", "static"])
def test_incomplete_or_nonfinite_results_rejected(proof, mutation):
    results = proof["results"]
    if mutation == "missing_history":
        del results["histories"]["buoy_z"]
    elif mutation == "short_grid":
        results["times"].pop()
    elif mutation in {"nan", "bool"}:
        results["histories"]["buoy_z"][0] = float("nan") if mutation == "nan" else True
    else:
        del results["static"]["end_tensions"]["A"]
    with pytest.raises(ValueError):
        validate_report(proof, "solve", "a" * 64)
