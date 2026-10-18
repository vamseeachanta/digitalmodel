# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Geometry admission and fixed arithmetic for the ruled preparation."""

import json

import pytest

from tests.installation.test_shore_pull_in import CAMPAIGN, driver


def ruled_table():
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    table["basis"]["water_depth_m"] = 21
    table["basis"]["pipeline"]["segments"][0]["length"] = 2000
    table["cases"][2]["rise_m"] = 21
    table["public_dataset"]["columns"]["PIPELAY_MIN_WATER_DEPTH_M"] = "10.7"
    return table


@pytest.mark.parametrize("change,error", [
    ("draft-clearance", "clearance"), ("minimum-lay", "clearance"),
    ("equal-length", "length"), ("short-length", "length"),
    ("chord", "chord"), ("negative-rise", "rise"),
    ("excess-rise", "rise"), ("nan-depth", "finite"),
    ("nan-length", "finite"), ("nan-rise", "finite"),
    ("multiple-segments", "one segment"),
])
def test_geometry_rejects_invalid_inputs_before_generation(tmp_path, monkeypatch, change, error):
    table = ruled_table()
    basis = table["basis"]
    if change == "draft-clearance":
        basis["water_depth_m"] = 20.14
    elif change == "minimum-lay":
        table["public_dataset"]["columns"]["PIPELAY_MIN_WATER_DEPTH_M"] = "22"
    elif change in {"equal-length", "short-length", "chord"}:
        basis["pipeline"]["segments"][0]["length"] = {"equal-length": 21, "short-length": 20, "chord": 21.1}[change]
        if change == "chord":
            # Large coated radius isolates chord failure after the span passes.
            basis["pipeline"]["coatings"]["weight"][0]["thickness"] = 100
    elif change in {"negative-rise", "excess-rise", "nan-rise"}:
        table["cases"][1]["rise_m"] = {"negative-rise": -1, "excess-rise": 22, "nan-rise": float("nan")}[change]
    elif change == "nan-depth":
        basis["water_depth_m"] = float("nan")
    elif change == "nan-length":
        basis["pipeline"]["segments"][0]["length"] = float("nan")
    else:
        basis["pipeline"]["segments"].append(dict(basis["pipeline"]["segments"][0]))
    d = driver()
    def forbidden(*args, **kwargs):
        pytest.fail("generation reached before geometry rejection")
    monkeypatch.setattr(d.ModularModelGenerator, "generate", forbidden)
    monkeypatch.setattr(d, "audit_input_sources", lambda table: table["assumed_inputs"])
    (tmp_path / "case-table.json").write_text(json.dumps(table))
    (tmp_path / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    with pytest.raises(ValueError, match=error):
        d.prepare(tmp_path, tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_ruled_geometry_passes_at_clearance_and_rise_boundaries():
    table = ruled_table()
    table["basis"]["water_depth_m"] = 1.3 * 15.5
    table["cases"][2]["rise_m"] = table["basis"]["water_depth_m"]
    driver().audit_geometry(table)


def test_geometry_and_hand_pulls(tmp_path):
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    assert table["revision"] == "5"
    assert table["basis"]["water_depth_m"] == 21
    assert table["basis"]["pipeline"]["segments"][0]["length"] == 2000
    assert [c["rise_m"] for c in table["cases"]] == [0, 0, 21]
    assert table["public_dataset"]["columns"]["PIPELAY_MIN_WATER_DEPTH_M"] == "10.7"
    # Annular steel + corrosion + concrete mass = 1.5314 te/m;
    # water displacement = 1.0142 te/m; (difference)*9.80665 = 5.0726 kN/m.
    # T = w*(1-b)*(mu*sqrt(L^2-rise^2) + rise), zero tail, ideal efficiency.
    d = driver()
    assert d.weight(table["basis"]) == pytest.approx(5.0726, rel=1e-5)
    result = d.prepare(CAMPAIGN, tmp_path / "output")
    for case, expected in zip(result["cases"], [3246.4, 4411.2, 1676.4]):
        assert case["method"]["pipe_head_tension_kn"] == pytest.approx(expected, rel=1e-4)
        assert case["analytical_pull_kn"] == pytest.approx(expected, rel=1e-4)
        assert case["method"]["winch_demand_kn"] == pytest.approx(expected, rel=1e-4)


def test_independent_check_also_rejects_incorrect_winch_demand(tmp_path, monkeypatch):
    d = driver()
    original = d.pull_in
    def wrong_demand(*args, **kwargs):
        result = original(*args, **kwargs)
        result["winch_demand_kn"] += 1
        return result
    monkeypatch.setattr(d, "pull_in", wrong_demand)
    with pytest.raises(ValueError, match="analytical force"):
        d.prepare(CAMPAIGN, tmp_path / "output")
    assert not (tmp_path / "output").exists()
