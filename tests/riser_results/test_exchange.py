# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic exchange fixtures; expected quantities are hand calculations."""

import copy
import importlib
import json
import math
import os
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.riser_results.exporter import build_documents, export_results
from digitalmodel.riser_results.schema import read_document, validate_document
from digitalmodel.structural.rainflow_cycles import count_cycles


@pytest.fixture
def case():
    # Frozen matrix row, restricted to the fields extraction needs.
    return {
        "case_id": "R10-W1-SCR", "outside_diameter_mm": "273.100",
        "wall_thickness_mm": "25.600", "checked_wall_mm": "19.4000",
        "fatigue_stationary_seconds": "3600", "fatigue_seas": "F03;F04;F05",
    }


@pytest.fixture
def settings():
    return {
        "analysis_type": "dynamic", "load_condition": "extreme",
        "smys_MPa": 450, "design_case_factor": 1.2,
        "load_case": {"id": "nominal", "wave": {"toward_deg": 180},
                      "current": {"speed_m_s": 0}, "offset": 0},
        "period_s": [150, 3750], "fibres_deg": [0, 180], "scf": 1.2,
        "sea_state": {"id": "F03", "Hs_m": 2.112264, "Tp_s": 6.59,
                      "direction_deg": 180, "occurrence": 0.015},
    }


@pytest.fixture
def snapshot():
    """Four stations; a 40 MPa signed range and one complete cycle per hour."""
    profiles = {
        "effective_tension": {"static": [2000, 1500, 1000, 500],
                              "min": [1000, 500, -100, 0],
                              "max": [3000, 2500, 2000, 500]},
        "bending_moment": {"static": [0, 1000, 2000, 0],
                           "min": [0, 0, 0, 0], "max": [0, 2000, 4000, 0]},
        # Hand check: 180 MPa / ((2/3) * 450 * 1.2 MPa) = 0.5.
        "von_mises_stress": {"static": [90e6, 100e6, 120e6, 0],
                             "min": [80e6, 90e6, 100e6, 0],
                             "max": [120e6, 150e6, 180e6, 0]},
    }
    histories = [
        {"name": name, "arc_length": arc, "fibre_deg": fibre,
         "stress": [0, 40e6, 0]}
        for name, arc in [("hang_off", 0), ("touchdown", 20)]
        for fibre in [0, 180]
    ]
    return {
        "data_basis": "synthetic", "software": {"name": "synthetic", "version": "fixture"},
        "units": {"length": "m", "force": "N", "moment": "N.m",
                  "stress": "Pa", "angle": "rad"},
        "arc_length": [0, 10, 20, 30], "profiles": profiles,
        "times_s": [150, 1950, 3750], "touchdown_arc_length": [20, 20, 20],
        "earlier_contacts": [[10], [10], [10]],
        "top_angle": {"static": math.pi / 18, "history": [0, math.pi / 9, math.pi / 18]},
        "hang_off_tension": {"static": 2000, "history": [1000, 3000, 2000]},
        "stress_geometry": {"outside_diameter_m": 0.2731, "checked_wall_m": 0.0194},
        "stress_histories": histories,
    }


@pytest.fixture
def run():
    return {"input_sha256": "a" * 64, "result_file": "synthetic.sim",
            "date": "2026-10-26T00:00:00Z"}


def test_hand_checked_strength_and_units(case, settings, snapshot, run):
    strength, fatigue = build_documents(snapshot, case, settings, run)
    validate_document(strength, "strength")
    validate_document(fatigue, "fatigue")
    assert strength["effective_tension_kN"] == [3, 2.5, 2, 0.5]
    assert strength["envelopes"]["effective_tension_kN"]["min"][2] == -0.1
    assert strength["bending_moment_kNm"] == [0, 2, 4, 0]
    assert strength["von_mises_utilisation"][2] == pytest.approx(0.5)
    assert strength["utilisation_method"]["allowable_MPa"] == pytest.approx(360)
    assert strength["max_utilisation"] == {"value": 0.5, "arc_length_m": 20,
                                           "location": "touchdown"}
    assert strength["touchdown"]["stress_range_MPa"] == 40
    assert strength["top_angle_deg"] == pytest.approx(
        {"static": 10, "dynamic_min": 0, "dynamic_max": 20})
    assert strength["hang_off_tension_kN"] == {"static": 2, "dynamic_max": 3}
    assert strength["touchdown"]["earlier_contacts_m"] == [[10], [10], [10]]


def test_hand_checked_benchmark_damage(case, settings, snapshot, run):
    _, fatigue = build_documents(snapshot, case, settings, run)
    # One cycle per 3600 s, 1.5% of 8760 h: 131.4 cycles/year.
    # N(1.2*40 MPa) = 2e6 * (80/48)^3, annual D = 0.0000141912.
    assert fatigue["damage_per_year"] == pytest.approx(0.0000141912)
    assert fatigue["life_years"] == pytest.approx(1 / 0.0000141912)
    assert fatigue["damage_basis"] == "air-equivalent benchmark damage"
    for hot in fatigue["hot_spots"]:
        assert hot["cycles_per_year"] == pytest.approx(131.4)
        assert hot["range_histogram"] == [{"stress_range_MPa": 40, "cycles": 1}]
        assert hot["scf"] == 1.2


@pytest.mark.parametrize("signal, expected", [
    ([0, 10], [(10, 0.5)]),
    ([0, 10, 0], [(10, 0.5), (10, 0.5)]),
    ([0, 10, 5, 15, 0], [(5, 1), (15, 0.5), (15, 0.5)]),
    ([0, 0, 10, 10, 0, 0], [(10, 0.5), (10, 0.5)]),
    ([3, 3, 3], []),
])
def test_rainflow_full_half_cycles(signal, expected):
    assert [(cycle.range_value, cycle.count) for cycle in count_cycles(signal)] == expected


def test_zero_damage_has_no_established_life(case, settings, snapshot, run):
    for history in snapshot["stress_histories"]:
        history["stress"] = [7, 7, 7]
    _, fatigue = build_documents(snapshot, case, settings, run)
    assert fatigue["damage_per_year"] == 0
    assert fatigue["life_years"] is None


@pytest.mark.parametrize("mutation", [
    lambda s: s["arc_length"].__setitem__(1, 0),
    lambda s: s["profiles"]["bending_moment"]["max"].pop(),
    lambda s: s["profiles"]["von_mises_stress"]["max"].__setitem__(0, float("nan")),
    lambda s: s["units"].__setitem__("force", "unknown"),
    lambda s: s["times_s"].__setitem__(-1, 3000),
    lambda s: s["touchdown_arc_length"].__setitem__(0, 10),
    lambda s: s["stress_histories"].pop(),
    lambda s: s["stress_histories"][0].__setitem__("stress", [1, 2]),
    lambda s: s["stress_geometry"].__setitem__("checked_wall_m", 0.0256),
])
def test_reject_bad_extraction(case, settings, snapshot, run, mutation):
    mutation(snapshot)
    with pytest.raises(ValueError):
        build_documents(snapshot, case, settings, run)


@pytest.mark.parametrize("mutation", [
    lambda d: d.pop("touchdown"),
    lambda d: d.pop("utilisation_method"),
    lambda d: d["arc_length_m"].__setitem__(1, 0),
    lambda d: d["effective_tension_kN"].pop(),
    lambda d: d["run"].__setitem__("input_sha256", "bad"),
    lambda d: d["run"].__setitem__("date", "yesterday"),
    lambda d: d["max_utilisation"].__setitem__("value", 8),
    lambda d: d["top_angle_deg"].__setitem__("dynamic_max", -1),
])
def test_shared_validator_rejects_mutations(case, settings, snapshot, run, mutation):
    strength, _ = build_documents(snapshot, case, settings, run)
    mutation(strength)
    with pytest.raises(ValueError):
        validate_document(strength, "strength")


def test_parser_rejects_duplicate_keys_and_nonfinite(tmp_path):
    path = tmp_path / "invalid.json"
    for content in ['{"case_id":"a","case_id":"b"}', '{"damage":NaN}']:
        path.write_text(content)
        with pytest.raises(ValueError):
            read_document(path)


def test_export_files_hashes_and_no_partial_invalid_output(tmp_path, case, settings, snapshot):
    sim = tmp_path / "synthetic.sim"
    sim.write_bytes(b"synthetic simulation")
    inp = tmp_path / "synthetic.dat"
    inp.write_bytes(b"synthetic input")

    class FakeAdapter:
        def read(self, result_file, case_row, options):
            assert result_file == sim
            return copy.deepcopy(snapshot)

    out = tmp_path / "results" / case["case_id"]
    paths = export_results(sim, inp, case, settings, out, FakeAdapter())
    assert {p.name for p in paths} == {"strength.json", "fatigue-F03.json"}
    import hashlib
    strength = read_document(out / "strength.json")
    assert strength["run"]["input_sha256"] == hashlib.sha256(inp.read_bytes()).hexdigest()
    assert strength["run"]["result_file"] == "synthetic.sim"
    assert strength["run"]["result_sha256"] == hashlib.sha256(sim.read_bytes()).hexdigest()
    validate_document(strength, "strength")
    snapshot["arc_length"][1] = 0
    bad_out = tmp_path / "invalid"
    with pytest.raises(ValueError):
        export_results(sim, inp, case, settings, bad_out, FakeAdapter())
    assert not bad_out.exists()


def test_imports_do_not_attempt_licensed_api():
    # Reject import attempts before the finder can load a binding or a mock.
    script = '''
import importlib.abc
import sys
class Deny(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {'OrcFxAPI', 'OrcaWave', 'aqwa'}:
            raise AssertionError('licensed import attempted')
sys.meta_path.insert(0, Deny())
import digitalmodel.riser_results
import digitalmodel.riser_results.exporter
import digitalmodel.riser_results.native
import digitalmodel.riser_results.__main__
'''
    result = subprocess.run(
        [sys.executable, "-c", script], capture_output=True, text=True,
        env={**os.environ, "PYTHONPATH": str(Path(__file__).resolve().parents[2] / "src")})
    assert result.returncode == 0, result.stderr


def test_cli_uses_injected_adapter(tmp_path, case, settings, snapshot, monkeypatch):
    cli = importlib.import_module("digitalmodel.riser_results.__main__")
    sim = tmp_path / "synthetic.sim"
    sim.write_bytes(b"synthetic simulation")
    (tmp_path / "synthetic.dat").write_bytes(b"synthetic input")
    manifest = {"case_row": case, "input_file": "synthetic.dat", "extraction": settings}
    sim.with_suffix(".export.json").write_text(json.dumps(manifest))

    class FakeAdapter:
        def read(self, *args):
            return snapshot

    monkeypatch.setattr(cli, "load_adapter", lambda: FakeAdapter())
    out = tmp_path / "results" / case["case_id"]
    assert cli.main(["export", "--sim", str(sim), "--case", case["case_id"],
                     "--out", str(out)]) == 0
    assert (out / "fatigue-F03.json").is_file()
    assert cli.main(["export", "--sim", str(sim), "--case", "incorrect",
                     "--out", str(out)]) == 2


@pytest.mark.parametrize("force,moment,stress,angle,length", [
    ("kN", "kN.m", "MPa", "deg", "m"),
    ("N", "N.m", "kPa", "rad", "mm"),
])
def test_equivalent_unit_systems(case, settings, snapshot, run, force, moment, stress, angle, length):
    # Independent SI prefix definitions; neither expected values nor factors use the exporter.
    force_factor = {"kN": 0.001, "N": 1}[force]
    moment_factor = {"kN.m": 0.001, "N.m": 1}[moment]
    stress_factor = {"MPa": 1e-6, "kPa": 0.001}[stress]
    angle_factor = {"deg": 180 / math.pi, "rad": 1}[angle]
    length_factor = {"m": 1, "mm": 1000}[length]
    for name, factor in (("effective_tension", force_factor),
                          ("bending_moment", moment_factor),
                          ("von_mises_stress", stress_factor)):
        for bound in ("static", "min", "max"):
            snapshot["profiles"][name][bound] = [x * factor for x in snapshot["profiles"][name][bound]]
    snapshot["arc_length"] = [x * length_factor for x in snapshot["arc_length"]]
    snapshot["touchdown_arc_length"] = [x * length_factor for x in snapshot["touchdown_arc_length"]]
    snapshot["earlier_contacts"] = [[x * length_factor for x in row] for row in snapshot["earlier_contacts"]]
    for row in snapshot["stress_histories"]:
        row["arc_length"] *= length_factor
        row["stress"] = [x * stress_factor for x in row["stress"]]
    for name, factor in (("top_angle", angle_factor), ("hang_off_tension", force_factor)):
        snapshot[name]["static"] *= factor
        snapshot[name]["history"] = [x * factor for x in snapshot[name]["history"]]
    snapshot["units"] = dict(zip(("force", "moment", "stress", "angle", "length"),
                                 (force, moment, stress, angle, length)))
    strength, fatigue = build_documents(snapshot, case, settings, run)
    assert strength["max_utilisation"]["value"] == pytest.approx(0.5)
    assert strength["touchdown"]["stress_range_MPa"] == pytest.approx(40)
    assert strength["top_angle_deg"]["dynamic_max"] == pytest.approx(20)
    assert fatigue["damage_per_year"] == pytest.approx(0.0000141912)


def test_static_result_has_no_fabricated_dynamic_values(case, settings, snapshot, run):
    settings["analysis_type"] = "static"
    settings["load_condition"] = "functional"
    settings["design_case_factor"] = 1
    settings.pop("sea_state")
    snapshot["touchdown_arc_length"] = [20]
    snapshot["earlier_contacts"] = [[10]]
    strength, fatigue = build_documents(snapshot, case, settings, run)
    assert fatigue is None
    assert strength["von_mises_utilisation"][2] == pytest.approx(0.4)  # 120/300 MPa
    assert strength["top_angle_deg"]["dynamic_min"] is None
    assert strength["hang_off_tension_kN"]["dynamic_max"] is None
    assert strength["touchdown"]["stress_range_MPa"] is None
    validate_document(strength, "strength")


def test_does_not_count_a_moving_maximum_as_a_material_history(case, settings, snapshot, run):
    snapshot["touchdown_arc_length"] = [10, 20, 20]
    for fibre in [0, 180]:
        snapshot["stress_histories"].append({
            "name": "touchdown", "arc_length": 10, "fibre_deg": fibre,
            "stress": [100e6, 100e6, 100e6],
        })
    strength, fatigue = build_documents(snapshot, case, settings, run)
    # A moving maximum jumps from 100 to 40 MPa; each real history is independent.
    assert strength["touchdown"]["stress_range_MPa"] == 40
    constant = [h for h in fatigue["hot_spots"] if h["arc_length_m"] == 10]
    assert all(h["damage_per_year"] == 0 for h in constant)
    assert fatigue["damage_per_year"] == pytest.approx(0.0000141912)


@pytest.mark.parametrize("mutation", [
    lambda d: d["sea_state"].__setitem__("occurrence", 1.5),
    lambda d: d["sea_state"].__setitem__("id", "../invalid"),
    lambda d: d["hot_spots"][0].__setitem__("scf", 0),
    lambda d: d["hot_spots"][0]["range_histogram"][0].__setitem__("cycles", -0.5),
    lambda d: d.__setitem__("life_years", 0),
    lambda d: d.__setitem__("damage_per_year", 1),
    lambda d: d["hot_spots"].append(copy.deepcopy(d["hot_spots"][0])),
    lambda d: d.__setitem__("hot_spots", []),
])
def test_fatigue_validator_rejects_mutations(case, settings, snapshot, run, mutation):
    _, fatigue = build_documents(snapshot, case, settings, run)
    mutation(fatigue)
    with pytest.raises(ValueError):
        validate_document(fatigue, "fatigue")


def test_export_resumes_matching_digests_and_refuses_replacement(tmp_path, case, settings, snapshot):
    sim, inp, out = tmp_path / "synthetic.sim", tmp_path / "synthetic.dat", tmp_path / "results"
    sim.write_bytes(b"synthetic")
    inp.write_bytes(b"synthetic input")
    reads = []

    class FakeAdapter:
        def read(self, *args):
            reads.append(True)
            return snapshot

    paths = export_results(sim, inp, case, settings, out, FakeAdapter())
    original = {p.name: p.read_bytes() for p in paths}
    assert export_results(sim, inp, case, settings, out, FakeAdapter()) == paths
    assert len(reads) == 1
    assert {p.name: p.read_bytes() for p in paths} == original
    sim.write_bytes(b"different synthetic result")
    with pytest.raises(FileExistsError):
        export_results(sim, inp, case, settings, out, FakeAdapter())
    assert {p.name: p.read_bytes() for p in paths} == original


def test_digest_change_during_extraction_is_rejected(tmp_path, case, settings, snapshot):
    sim, inp, out = tmp_path / "synthetic.sim", tmp_path / "synthetic.dat", tmp_path / "results"
    sim.write_bytes(b"synthetic")
    inp.write_bytes(b"synthetic input")

    class ChangingAdapter:
        def read(self, *args):
            sim.write_bytes(b"changed synthetic result")
            return snapshot

    with pytest.raises(ValueError, match="changed during extraction"):
        export_results(sim, inp, case, settings, out, ChangingAdapter())
    assert not out.exists()


def test_cli_retry_does_not_reload_binding(tmp_path, case, settings, snapshot, monkeypatch):
    cli = importlib.import_module("digitalmodel.riser_results.__main__")
    sim = tmp_path / "synthetic.sim"
    sim.write_bytes(b"synthetic simulation")
    (tmp_path / "synthetic.dat").write_bytes(b"synthetic input")
    sim.with_suffix(".export.json").write_text(json.dumps({
        "case_row": case, "input_file": "synthetic.dat", "extraction": settings,
    }))

    class FakeAdapter:
        def read(self, *args):
            return snapshot

    monkeypatch.setattr(cli, "load_adapter", lambda: FakeAdapter())
    args = ["export", "--sim", str(sim), "--case", case["case_id"],
            "--out", str(tmp_path / "results")]
    assert cli.main(args) == 0

    def forbidden():
        raise AssertionError("completed export must not load a binding")

    monkeypatch.setattr(cli, "load_adapter", forbidden)
    assert cli.main(args) == 0


@pytest.mark.parametrize("mutation", [
    lambda d: d["envelopes"]["bending_moment_kNm"]["max"].pop(),
    lambda d: d["envelopes"]["bending_moment_kNm"]["max"].__setitem__(1, -1),
])
def test_shared_validator_checks_envelopes(case, settings, snapshot, run, mutation):
    strength, _ = build_documents(snapshot, case, settings, run)
    mutation(strength)
    with pytest.raises(ValueError):
        validate_document(strength, "strength")
