# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic fixtures test contracts, not solver physics or acceptance limits."""
import copy
import importlib.util
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/benchmark/l01_investigation.py"


@pytest.fixture
def mod():
    spec = importlib.util.spec_from_file_location("l01_investigation", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def config():
    # Arbitrary synthetic test grid/limits, not proposed engineering values.
    return {
        "frequencies_rad_s": [1 + i / 100 for i in range(20)],
        "headings_deg": list(range(9)),
        "threads": [6],
        "budget_minutes": 60,
        "resource_citation": "synthetic allocation only",
        "max_relative_gap": 0.1,
        "sampling_citation": "synthetic sampling only",
        "basis": {key: "synthetic declared basis" for key in (
            "geometry", "mesh", "mass", "inertia", "centre_of_gravity",
            "reference_point", "water_depth", "density", "gravity",
            "units", "axes", "heading_convention", "phase_convention",
            "normalization", "dof_order", "solve_type", "source_rights", "restraints")},
        "criteria": {
            "citation": "synthetic acceptance only", "aqg_band": "synthetic",
            "max_normalized_error": 0.1,
            "rao_scales": [1] * 6, "phase_amplitude_floors": [0.01] * 6,
            "added_mass_scales": [[1] * 6 for _ in range(6)],
            "damping_scales": [[1] * 6 for _ in range(6)],
        },
    }


def export(c, solver):
    return {
        "solver": solver, "source": "solver", "native_sha256": "a" * 64,
        "basis": copy.deepcopy(c["basis"]), "native_readback": True,
        "raos": [
            {"frequency_rad_s": f, "heading_deg": h, "values": [[1, 0]] * 6}
            for f in c["frequencies_rad_s"] for h in c["headings_deg"]
        ],
        "matrices": [
            {"frequency_rad_s": f,
             "added_mass": [[1] * 6 for _ in range(6)],
             "damping": [[1] * 6 for _ in range(6)]}
            for f in c["frequencies_rad_s"]
        ],
    }


def test_stages_are_exact_source_subsets_and_full_comparison_is_distinct(mod):
    c = config()
    request = mod.build_request(c)
    assert sorted({row["cases"] for row in request["scaling_runs"]}) == [16, 90, 180]
    assert len(request["scaling_runs"]) == 6
    for row in request["scaling_runs"]:
        assert set(row["frequencies_rad_s"]) <= set(c["frequencies_rad_s"])
        assert set(row["headings_deg"]) <= set(c["headings_deg"])
        assert row["scientific_comparison"] is False
    assert request["comparison_grid"]["frequencies_rad_s"] == c["frequencies_rad_s"]
    assert request["native_inputs"] == "NOT_EVALUATED"


@pytest.mark.parametrize("field,value,message", [
    ("threads", [True], "unique positive integer thread counts"),
    ("budget_minutes", float("nan"), "finite numeric value"),
    ("resource_citation", "", "nonempty citation or declaration"),
    ("max_relative_gap", 0, "positive value"),
    ("frequencies_rad_s", [1 + i / 100 for i in range(19)] + [1.18],
     "frequencies must be strictly increasing"),
    ("headings_deg", list(range(8)) + [360], "headings must be unique canonical degrees"),
    ("headings_deg", list(range(8)) + [7], "headings must be unique canonical degrees"),
])
def test_invalid_requests_refused(mod, field, value, message):
    c = config()
    c[field] = value
    with pytest.raises(ValueError, match=message):
        mod.build_request(c)


def test_coarse_grid_is_not_a_scientific_grid(mod):
    c = config()
    c["frequencies_rad_s"] = list(range(1, 21))
    with pytest.raises(ValueError, match="sampling"):
        mod.build_request(c)


def test_reordering_is_safe_but_never_engineering_qualification(mod):
    c = config()
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    b["raos"].reverse()
    b["matrices"].reverse()
    result = mod.compare(c, a, b)
    assert result["band_check"] == "WITHIN_BAND"
    assert result["engineering_qualification"] == "NOT_EVALUATED"
    assert result["rao_samples"] == 180


@pytest.mark.parametrize("mutation", ["basis", "placeholder", "missing", "duplicate",
                                      "matrix_shape", "nan", "readback", "digest"])
def test_incomplete_or_untrusted_exports_refused(mod, mutation):
    c = config()
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    if mutation == "basis":
        b["basis"]["gravity"] = "different"
    elif mutation == "placeholder":
        b["source"] = "placeholder"
    elif mutation == "missing":
        b["raos"].pop()
    elif mutation == "duplicate":
        b["raos"].append(b["raos"][0])
    elif mutation == "matrix_shape":
        b["matrices"][0]["added_mass"] = []
    elif mutation == "nan":
        b["raos"][0]["values"] = [[float("nan"), 0]] * 6
    elif mutation == "readback":
        b["native_readback"] = False
    else:
        b["native_sha256"] = ""
    with pytest.raises(ValueError):
        mod.compare(c, a, b)


def test_single_bad_sample_governs_and_phase_wrap_is_circular(mod):
    c = config()
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    a["raos"][0]["values"] = [[-1, 0.001]] * 6
    b["raos"][0]["values"] = [[-1, -0.001]] * 6
    result = mod.compare(c, a, b)
    assert result["max_phase_difference_deg"] < 1
    b["raos"][1]["values"] = [[2, 0]] * 6
    result = mod.compare(c, a, b)
    assert result["band_check"] == "OUTSIDE_BAND"
    assert result["governing"]["quantity"] == "rao"


def test_zero_response_phase_is_not_evaluated(mod):
    c = config()
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    for payload in (a, b):
        for row in payload["raos"]:
            row["values"] = [[0, 0]] * 6
    result = mod.compare(c, a, b)
    assert result["max_phase_difference_deg"] is None
    assert result["phase_not_evaluated"] == 180 * 6


def test_zero_scale_and_missing_basis_are_refused(mod):
    c = config()
    c["criteria"]["rao_scales"][0] = 0
    with pytest.raises(ValueError):
        mod.build_request(c)
    c = config()
    del c["basis"]["reference_point"]
    with pytest.raises(ValueError):
        mod.build_request(c)


def test_duplicate_json_keys_refused(mod, tmp_path):
    path = tmp_path / "duplicate.json"
    path.write_text('{"a": 1, "a": 2}', encoding="utf-8")
    with pytest.raises(ValueError):
        mod.read_json(path)


def test_restraint_basis_is_required(mod):
    c = config()
    del c["basis"]["restraints"]
    with pytest.raises(ValueError, match="basis"):
        mod.build_request(c)


def test_incompatible_restraints_refused(mod):
    c = config()
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    b["basis"]["restraints"] = "different synthetic restraints"
    with pytest.raises(ValueError, match="basis"):
        mod.compare(c, a, b)


def test_extreme_finite_phase_does_not_overflow(mod):
    c = config()
    c["criteria"]["rao_scales"] = [1e300] * 6
    c["criteria"]["phase_amplitude_floors"] = [1e-310] * 6
    a, b = export(c, "AQWA"), export(c, "OrcaWave")
    a["raos"][0]["values"] = [[1e308, 1e308]] * 6
    b["raos"][0]["values"] = [[1e308, 1e308]] * 6
    result = mod.compare(c, a, b)
    assert result["max_phase_difference_deg"] == 0
