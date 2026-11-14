"""Offline checks for the two issue-note transcriptions; no model imports."""

import ast
import copy
import json
import re
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DOC_DIR = ROOT / "docs/domains/hydrodynamics"
STEM = "drive-off-research-note-2026-10"


@pytest.fixture(scope="module")
def note():
    data = yaml.safe_load((DOC_DIR / f"{STEM}.sources.yaml").read_text("utf-8"))
    return data, (DOC_DIR / f"{STEM}.md").read_text("utf-8")


def validate_code_mapping(data):
    """Find declared symbols statically, including module-level assignments."""
    for row in data["code_mapping"]:
        if row["status"] == "absent":
            assert not row.get("symbol") and not row.get("path")
            continue
        assert row["status"] == "present"
        path = ROOT / row["path"]
        assert path.is_file(), f"Missing mapped file: {row['path']}"
        symbols = set()
        for node in ast.parse(path.read_text("utf-8")).body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                symbols.add(node.name)
            elif isinstance(node, (ast.Assign, ast.AnnAssign)):
                targets = node.targets if isinstance(node, ast.Assign) else [node.target]
                symbols.update(t.id for t in targets if isinstance(t, ast.Name))
        assert row["symbol"] in symbols, f"Unknown mapped symbol: {row['symbol']}"


def test_note_structure_and_content(note):
    data, markdown = note
    assert data["schema_version"] == 1 and data["issue"] == 162
    assert data["document"] == f"{STEM}.md"
    assert data["checked"] == ["2026-10-11", "2026-10-12"]
    for comment in ("6115195050", "6116520568"):
        assert f"#issuecomment-{comment}" in markdown
    assert re.findall(r"^## (\d+)\.", markdown, re.MULTILINE) == [
        str(i) for i in range(1, 15)
    ]
    for phrase in ("assessment, not a finding", "differences listed, not resolved",
                   "historical and superseded", "reproduction remains skipped"):
        assert phrase in markdown
    assert data["counts"] == {
        "read": 14, "located_not_read": 17,
        "enumerated_read": 14, "enumerated_located_not_read": 15,
    }
    all_ids = []
    for key, prefix, status, count in (
        ("sources_read", "R", "read", 14),
        ("located_not_read", "L", "located_not_read", 15),
    ):
        entries = data[key]
        assert [e["id"] for e in entries] == [f"{prefix}{i}" for i in range(1, count + 1)]
        for entry in entries:
            assert entry["status"] == status
            assert f"[{entry['id']}] {entry['citation']}" in markdown
            all_ids.append(entry["id"])
    assert len(all_ids) == len(set(all_ids))
    assert set(re.findall(r"\[([RL]\d+)\]", markdown)) == set(all_ids)


def test_values_agree_with_merged_fixture(note):
    data, markdown = note
    fixture = json.loads((ROOT / "tests/drilling_riser/fixtures/drive_off_worked_example_2017.json").read_text("utf-8"))
    values = [entry for entry in data["benchmark_values"] if "fixture_key" in entry]
    assert {entry["fixture_key"] for entry in values} == {
        "inputs.vessel_mass", "reported_results.upper_flexjoint_angle_limit",
        "reported_results.upper_flexjoint_limit_time", "reported_results.stroke_limit",
        "reported_results.wellhead_bending_moment_limit",
    }
    for entry in values:
        group, key = entry["fixture_key"].split(".")
        assert entry["value"] == fixture[group][key]["value"]
        assert entry["unit"] == fixture[group][key]["unit"]
        assert entry["printed"] in markdown


def test_timing_arithmetic_self_check(note):
    data, markdown = note
    timing = data["abs_timing_s"]
    assert timing == {"point_of_disconnect": 257, "red": 187, "disconnect": 70}
    assert timing["point_of_disconnect"] - timing["red"] == timing["disconnect"]
    for printed in ("257 s", "187 s", "70 s"):
        assert printed in markdown


def test_code_mapping_symbols_exist(note):
    data, markdown = note
    assert {r["status"] for r in data["code_mapping"]} == {"present", "absent"}
    validate_code_mapping(data)
    for row in data["code_mapping"]:
        assert row["element"] in markdown
        if row["status"] == "present":
            assert row["path"] in markdown and row["symbol"] in markdown


def test_invented_symbol_is_rejected(note):
    broken = copy.deepcopy(note[0])
    next(r for r in broken["code_mapping"] if r["status"] == "present")["symbol"] = "invented_symbol"
    with pytest.raises(AssertionError, match="Unknown mapped symbol: invented_symbol"):
        validate_code_mapping(broken)
