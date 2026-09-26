"""Tests for data/crack_fe_database (#2157 P3; owner cards D03 repo only, D04 structural-ffs).

Comparator classes: conservation (the manifest hashes and row counts match the files;
the rebuild from the committed receipts and the coordinator is byte-identical) and
provenance (every row traces to a committed receipt by SHA-256, and its values equal
the receipt's values).
"""

from __future__ import annotations

import csv
import hashlib
import importlib.util
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.ansys.crack_receipt import text_sha256

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data" / "crack_fe_database"
EXAMPLE = ROOT / "examples" / "workflows" / "crack-fe-weldolet"
FE_STATES = EXAMPLE / "fe_states"
INPUT = EXAMPLE / "input.yml"
TABLES = ("crack_fe_states", "crack_results")


def _rows(table: str) -> list[dict]:
    with open(DATA / f"{table}.csv", newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def _manifest() -> dict:
    return yaml.safe_load((DATA / "manifest.yaml").read_text(encoding="utf-8"))


def _receipt(state: str) -> dict:
    return json.loads((FE_STATES / f"{state}.receipt.json").read_text("utf-8"))


def _builder():
    path = ROOT / "scripts" / "crack_fe_database" / "build_tables.py"
    spec = importlib.util.spec_from_file_location("crack_fe_build_tables", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def test_manifest_hashes_and_counts_match_the_files():
    manifest = _manifest()
    assert manifest["route"] == "public"
    assert manifest["issue"] == 2157
    assert manifest["domain"] == "structural-ffs"
    assert "D03" in manifest["rights_decision"] and "D04" in manifest["rights_decision"]
    assert "Hugging Face" in manifest["distribution"]
    for table in TABLES:
        entry = manifest["tables"][table]
        blob = (DATA / entry["file"]).read_bytes()
        assert hashlib.sha256(blob).hexdigest() == entry["sha256"], table
        assert entry["rows"] == len(_rows(table)), table


def test_manifest_inputs_are_the_committed_files():
    inputs = _manifest()["inputs"]
    assert inputs["case_sha256"] == text_sha256(INPUT.read_text("utf-8"))
    declared = json.loads((FE_STATES / "declared_states.json").read_text("utf-8"))
    assert set(inputs["receipts"]) == {s["state"] for s in declared["states"]}
    for state, sha in inputs["receipts"].items():
        assert sha == text_sha256((FE_STATES / f"{state}.receipt.json").read_text("utf-8"))


def test_data_files_are_binary_in_gitattributes():
    text = (ROOT / ".gitattributes").read_text(encoding="utf-8")
    assert "data/crack_fe_database/** -text" in text.splitlines()


@pytest.fixture(scope="module")
def rebuilt(tmp_path_factory) -> Path:
    out = tmp_path_factory.mktemp("crack_fe_database")
    _builder().build(INPUT, out)
    return out


def test_rebuild_is_byte_identical(rebuilt):
    for name in ("crack_fe_states.csv", "crack_results.csv", "manifest.yaml"):
        assert (rebuilt / name).read_bytes() == (DATA / name).read_bytes(), name


def _f(x: str) -> float:
    return float(x)


def test_every_state_row_traces_to_a_committed_receipt():
    rows = _rows("crack_fe_states")
    declared = json.loads((FE_STATES / "declared_states.json").read_text("utf-8"))
    assert {r["state"] for r in rows} == {s["state"] for s in declared["states"]}
    for r in rows:
        receipt = _receipt(r["state"])
        path = FE_STATES / f"{r['state']}.receipt.json"
        assert r["receipt_sha256"] == text_sha256(path.read_text("utf-8"))
        assert r["kind"] == receipt["kind"]
        assert r["mapdl_version"] == receipt["run"]["mapdl_version"]
        assert r["producing_commit"] == receipt["run"]["producing_commit"]
        if receipt["kind"] == "weldolet_crack":
            gov = receipt["governing"]
            assert r["plane"] == receipt["plane"]
            assert _f(r["a_mm"]) == receipt["crack"]["depth_mm"]
            assert _f(r["ligament_mm"]) == receipt["crack"]["remaining_ligament_mm"]
            assert _f(r["k_gov_mpa_sqrt_m"]) == gov["k_gov_max_mpa_sqrt_m"]
            assert _f(r["j_n_per_mm"]) == gov["j_at_governing_n_per_mm"]
            mm = gov["mode_mix_at_governing_mpa_sqrt_m"]
            assert (_f(r["k1_mpa_sqrt_m"]), _f(r["k2_mpa_sqrt_m"]), _f(r["k3_mpa_sqrt_m"])) == (
                mm["K1"], mm["K2"], mm["K3"])
        else:
            assert r["k_gov_mpa_sqrt_m"] == "n/a"
        if receipt["kind"] == "limit_load":
            assert _f(r["p_limit_mpa"]) == receipt["limit_load"]["p_limit_mpa"]
        if r["sigma_m_mpa"] != "n/a":
            unc = _receipt("p0b_uncracked")["sigma_ref"]["paths"]
            path_row = next(p for p in unc if p["path"] == int(r["sigma_ref_path"]))
            pre = "hoop_" if r["plane"] == "crotch" else ""
            assert _f(r["sigma_m_mpa"]) == path_row[f"{pre}sigma_m_mpa"]
            assert _f(r["sigma_b_mpa"]) == path_row[f"{pre}sigma_b_mpa"]


def test_every_result_row_traces_to_a_committed_receipt():
    rows = _rows("crack_results")
    assert {r["case"] for r in rows} >= {"base", "sensitivity_limit_load_lr"}
    for r in rows:
        state = r["governing_state"]
        path = FE_STATES / f"{state}.receipt.json"
        assert r["governing_receipt_sha256"] == text_sha256(path.read_text("utf-8"))
        receipt = _receipt(state)
        if receipt["kind"] == "weldolet_crack":
            assert _f(r["k_gov_mpa_sqrt_m"]) == receipt["governing"]["k_gov_max_mpa_sqrt_m"]
        assert r["evidence_status"] == "INCOMPLETE"
    base = [r for r in rows if r["case"] == "base"]
    assert [_f(r["a_mm"]) for r in base] == [2.35, 2.8, 3.2, 3.6, 4.0]
    established = [r for r in base if r["depth_status"] == "established"]
    assert all(r["governing_plane"] == "crotch" for r in established)
    for r in base:
        if r["depth_status"] != "established":
            assert r["remaining_life_to_last_fe_cycles"] == "n/a"
