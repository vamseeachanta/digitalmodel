"""CINT / reaction-output parser and guards (a)-(f) (#2157 P0a).

Every guard is evaluated from solved output. Each guard has a committed
negative fixture that fails that guard, and only that guard.
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest

from digitalmodel.ansys import cint_parser

FIXTURES = Path(__file__).resolve().parent / "fixtures" / "cint"
GUARDS = (
    "a_equilibrium",
    "b_mesh_load",
    "c_contour",
    "d_complete",
    "e_sanitised",
    "f_units",
    "g_j_mesh",
)
FILES = ("cint_L0.txt", "cint_L1.txt", "reac_L0.txt", "reac_L1.txt")


def _gates(results: dict) -> dict:
    """The gating guards only (the legacy guard (c) value is a record)."""
    return {n: r for n, r in results.items() if n in GUARDS}


def _load(case: str) -> tuple[dict[int, str], dict[int, str]]:
    """Fixture case = the good set with the case's own files overlaid."""
    texts = {}
    for name in FILES:
        own = FIXTURES / case / name
        src = own if own.is_file() else FIXTURES / "good" / name
        texts[name] = src.read_text(encoding="utf-8")
    cint = {0: texts["cint_L0.txt"], 1: texts["cint_L1.txt"]}
    reac = {0: texts["reac_L0.txt"], 1: texts["reac_L1.txt"]}
    return cint, reac


def _guards(case: str):
    cint, reac = _load(case)
    return cint_parser.evaluate_guards(cint, reac, extra_tokens=())


# --------------------------------------------------------------------------- #
# Parsing
# --------------------------------------------------------------------------- #
def test_parse_cint_table_roundtrip():
    cint, _ = _load("good")
    table = cint_parser.parse_cint_table(cint[1])
    assert table.units["K"] == "MPa*sqrt(mm)"
    assert table.declared_front_nodes == 5
    assert table.declared_contours == 6
    assert len(table.rows) == 30
    row = table.rows[0]
    assert (row.node, row.contour) == (1, 1)
    assert row.k1 == pytest.approx(0.18300000e03)


def test_parse_reaction_file():
    _, reac = _load("good")
    r = cint_parser.parse_reaction_file(reac[0])
    assert r.fz_ligament == pytest.approx(-80000.0, rel=1e-3)
    assert r.stress_mpa == pytest.approx(100.0)
    assert r.loaded_area_mm2 == pytest.approx(800.0)
    assert r.mapdl_rev == "26.1"


def test_front_record_converts_k_once_to_mpa_sqrt_m():
    cint, _ = _load("good")
    table = cint_parser.parse_cint_table(cint[1])
    front = cint_parser.front_records(table, crack_depth_mm=2.0, crack_half_length_mm=4.0)
    raw = table.rows[0].k1
    assert front[0]["contours"][0]["K1"] == pytest.approx(raw / math.sqrt(1000.0), rel=1e-12)


def test_front_record_phi_and_reported_k():
    cint, _ = _load("good")
    table = cint_parser.parse_cint_table(cint[1])
    front = cint_parser.front_records(table, crack_depth_mm=2.0, crack_half_length_mm=4.0)
    phis = [n["phi_deg"] for n in front]
    assert phis == pytest.approx([0.0, 45.0, 90.0, 135.0, 180.0], abs=1e-6)
    deep = front[2]
    k_last3 = [c["K1"] for c in deep["contours"][3:]]
    assert deep["K1_reported"] == pytest.approx(sum(k_last3) / 3.0)


def test_centre_crack_secant_value():
    # parser-sanity comparator (plan TDD list): a = 5, W = 100, sigma = 100
    assert cint_parser.centre_crack_secant_k(5.0, 100.0, 100.0) == pytest.approx(
        12.6110, abs=5e-5
    )


# --------------------------------------------------------------------------- #
# Guards on the good set, and one negative fixture per guard
# --------------------------------------------------------------------------- #
def test_good_fixture_passes_every_guard():
    results = _guards("good")
    assert set(results) == set(GUARDS) | {"c_contour_legacy", "g_end_nodes_record"}
    for name, res in _gates(results).items():
        assert res.status == "pass", f"{name}: {res}"


@pytest.mark.parametrize(
    "case, guard",
    [
        ("neg_a_equilibrium", "a_equilibrium"),
        ("neg_b_stale_mesh", "b_mesh_load"),
        ("neg_c_contour", "c_contour"),
        ("neg_d_missing_field", "d_complete"),
        ("neg_e_host_token", "e_sanitised"),
        ("neg_f_wrong_unit", "f_units"),
    ],
)
def test_negative_fixture_fails_only_its_guard(case, guard):
    results = _gates(_guards(case))
    failed = sorted(n for n, r in results.items() if r.status != "pass")
    assert failed == [guard]


def test_g_open_front_end_node_oscillation_is_recorded_not_gating():
    """Owner G16: on a front ending on a free surface, guard (g) uses interior
    nodes only; the end node's J change (2 % here) is recorded, not gated."""
    res = _guards("g_end_node_oscillates")
    assert {n: r.status for n, r in _gates(res).items()} == dict.fromkeys(GUARDS, "pass")
    rec = res["g_end_nodes_record"]
    assert rec.status == "fail" and rec.value > 0.01
    assert "not gating" in rec.detail


def test_g_open_front_interior_node_failure_fails_g_only():
    res = _gates(_guards("neg_g_interior"))
    assert sorted(n for n, r in res.items() if r.status != "pass") == ["g_j_mesh"]
    assert res["g_j_mesh"].value > 0.01


def test_g_closed_front_unaffected_and_has_no_end_node_record():
    cint, reac = _load_weld("good")
    res = cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
    assert res["g_end_nodes_record"].status == "not_applicable"
    cint, reac = _load_weld("neg_g_j_mesh")
    res = cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
    assert res["g_j_mesh"].status == "fail"
    assert "governing node" in res["g_j_mesh"].detail


def test_guard_limits_are_the_plan_values():
    results = _guards("good")
    assert results["a_equilibrium"].limit == 0.005
    assert results["b_mesh_load"].limit == 0.001
    assert results["c_contour"].limit == 0.03
    assert results["g_j_mesh"].limit == 0.01  # owner G14
    assert results["c_contour_legacy"].limit == 0.03


def test_host_token_scan_uses_runtime_tokens():
    assert cint_parser.find_host_tokens("run on node build-07", extra_tokens=("build-07",))
    assert cint_parser.find_host_tokens("# USER=someone") != []
    assert cint_parser.find_host_tokens("0.123E+03 45.0") == []


def test_parsed_record_has_no_host_fields():
    cint, reac = _load("good")
    record = cint_parser.build_mesh_record(
        level=1,
        cint_text=cint[1],
        reac_text=reac[1],
        crack_depth_mm=2.0,
        crack_half_length_mm=4.0,
    )
    text = json.dumps(record)
    assert cint_parser.find_host_tokens(text) == []
    for key in ("host", "hostname", "user", "username", "cwd", "path"):
        assert key not in record


def test_record_csv_has_no_host_fields():
    cint, reac = _load("good")
    record = cint_parser.build_mesh_record(
        level=1,
        cint_text=cint[1],
        reac_text=reac[1],
        crack_depth_mm=2.0,
        crack_half_length_mm=4.0,
    )
    csv_text = cint_parser.front_csv(record)
    assert csv_text.splitlines()[0].startswith("node,phi_deg,contour")
    assert cint_parser.find_host_tokens(csv_text) == []


# --------------------------------------------------------------------------- #
# Receipt schema
# --------------------------------------------------------------------------- #
def test_schema_rejects_missing_fields():
    problems = cint_parser.validate_receipt_schema({"schema": "x"})
    assert problems
    assert any("meshes" in p for p in problems)


# --------------------------------------------------------------------------- #
# P0b weldolet outputs: closed polar front, generic reaction_sum, uncracked
# --------------------------------------------------------------------------- #
WELD_FIXTURES = Path(__file__).resolve().parent / "fixtures" / "cint_weldolet"
POLAR = {"type": "polar_z"}


def _load_weld(case: str) -> tuple[dict[int, str], dict[int, str]]:
    texts = {}
    for name in FILES:
        own = WELD_FIXTURES / case / name
        src = own if own.is_file() else WELD_FIXTURES / "good" / name
        texts[name] = src.read_text(encoding="utf-8")
    return ({0: texts["cint_L0.txt"], 1: texts["cint_L1.txt"]},
            {0: texts["reac_L0.txt"], 1: texts["reac_L1.txt"]})


def test_weldolet_reaction_sum_parsed_as_the_equilibrium_quantity():
    _, reac = _load_weld("good")
    r = cint_parser.parse_reaction_file(reac[0])
    assert r.fz_ligament is None
    assert r.reaction_sum == pytest.approx(-4.7 * 18645.86008, rel=1e-12)
    assert r.extras["reaction_y_sum"] == pytest.approx(-3.0e-6)
    assert r.extras["n_fixed_nodes"] == 1012.0


def test_weldolet_polar_front_angles():
    cint, _ = _load_weld("good")
    table = cint_parser.parse_cint_table(cint[1])
    front = cint_parser.front_records(table, front_geometry=POLAR)
    assert [n["phi_deg"] for n in front] == [0.0, 90.0, 180.0, 270.0]
    for key in ("K1_reported", "K2_reported", "K3_reported", "J_reported"):
        assert all(n[key] is not None for n in front)


def test_weldolet_good_fixture_passes_every_guard():
    cint, reac = _load_weld("good")
    res = cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
    assert {n: r.status for n, r in _gates(res).items()} == dict.fromkeys(GUARDS, "pass")


@pytest.mark.parametrize(
    "case, guard",
    [
        ("neg_a_equilibrium", "a_equilibrium"),
        ("neg_b_stale_mesh", "b_mesh_load"),
        ("neg_c_k_front", "c_contour"),  # G13 (i): K spread / max |K| on the front
        ("neg_c_j_node", "c_contour"),  # G13 (ii): J spread / mean J at the node
        ("neg_d_missing_field", "d_complete"),
        ("neg_e_host_token", "e_sanitised"),
        ("neg_f_wrong_unit", "f_units"),
        ("neg_g_j_mesh", "g_j_mesh"),  # G14: J at the governing node, mesh pair
    ],
)
def test_weldolet_negative_fixture_fails_only_its_guard(case, guard):
    cint, reac = _load_weld(case)
    res = _gates(
        cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
    )
    assert sorted(n for n, r in res.items() if r.status != "pass") == [guard]


def test_c_part_i_and_ii_fail_for_their_own_reason():
    for case, part in (("neg_c_k_front", "(i)"), ("neg_c_j_node", "(ii)")):
        cint, reac = _load_weld(case)
        res = cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
        detail = res["c_contour"].detail
        assert f"{part} fail" in detail, (case, detail)
        other = "(ii)" if part == "(i)" else "(i)"
        assert f"{other} pass" in detail, (case, detail)


def test_zero_crossing_node_passes_new_c_and_fails_legacy():
    """A K_I zero crossing on the front (tiny absolute spread, tiny mean) failed
    the old per-node relative metric; it passes G13 and the legacy value is kept."""
    cint, reac = _load_weld("zero_crossing")
    res = cint_parser.evaluate_guards(cint, reac, front_geometry=POLAR, extra_tokens=())
    assert res["c_contour"].status == "pass"
    assert res["c_contour_legacy"].status == "fail"
    assert res["c_contour_legacy"].value > 0.03


def test_governing_node_is_max_j_on_the_finer_mesh():
    cint, _ = _load_weld("neg_g_j_mesh")
    fronts = {
        lv: cint_parser.front_records(cint_parser.parse_cint_table(t), front_geometry=POLAR)
        for lv, t in cint.items()
    }
    gov = cint_parser.governing_node(fronts)
    assert gov["level_fine"] == 1 and gov["level_coarse"] == 0
    assert gov["phi_deg"] in (0.0, 180.0)
    assert gov["j_rel_change"] > 0.01


def test_k_gov_from_j():
    # K = sqrt(E' J), E' = E / (1 - nu^2); J in N/mm -> K in MPa*sqrt(m)
    e, nu, j = 182_500.0, 0.3, 9.6e-3
    expected = math.sqrt(e / (1.0 - nu * nu) * j) / math.sqrt(1000.0)
    assert cint_parser.k_from_j(j, e, nu) == pytest.approx(expected, rel=1e-15)
    assert cint_parser.k_from_j(j, e, nu) == pytest.approx(1.3875, abs=5e-5)


def test_uncracked_evaluation_marks_contour_guards_not_applicable():
    _, reac = _load_weld("good")
    res = cint_parser.evaluate_guards({}, reac, cracked=False, extra_tokens=())
    assert res["c_contour"].status == "not_applicable"
    assert res["d_complete"].status == "not_applicable"
    for name in ("a_equilibrium", "b_mesh_load", "e_sanitised", "f_units"):
        assert res[name].status == "pass", name
    _, bad = _load_weld("neg_a_equilibrium")
    res = cint_parser.evaluate_guards({}, bad, cracked=False, extra_tokens=())
    assert res["a_equilibrium"].status == "fail"


def test_uncracked_record_has_empty_front():
    _, reac = _load_weld("good")
    rec = cint_parser.build_mesh_record(level=0, cint_text=None, reac_text=reac[0])
    assert rec["front"] == [] and rec["declared_front_nodes"] == 0
    assert rec["reactions"]["reaction_sum_n"] == pytest.approx(-4.7 * 18645.86008)


def test_schema_v2_requires_per_level_run_and_artifacts():
    mesh = {"level": 0, "deck_sha256": "0" * 64, "declared_front_nodes": 1,
            "declared_contours": 6, "reactions": {}, "front": [{"node": 1}]}
    receipt = {"schema": cint_parser.RECEIPT_SCHEMA_ID, "state": "x", "issue": 2157,
               "kind": "weldolet_crack", "spec": {}, "units": {}, "meshing": {},
               "run": {}, "meshes": [mesh, dict(mesh, level=1)], "primary_level": 1,
               "guards": {}, "crack": {}, "sigma_ref": {}, "front_geometry": POLAR}
    problems = " | ".join(cint_parser.validate_receipt_schema(receipt))
    assert "meshes[0]: missing 'run'" in problems
    assert "meshes[0]: missing 'artifacts'" in problems
    assert "run: missing 'generator_files'" in problems
