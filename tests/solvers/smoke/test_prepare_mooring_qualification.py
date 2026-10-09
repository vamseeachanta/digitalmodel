"""Offline qualification packaging preserves inputs and reports reference differences."""
import importlib.util
import json
from pathlib import Path
import sys

import pytest

from digitalmodel.solvers.smoke.model_manifest import verify_manifest

SCRIPT = Path(__file__).resolve().parents[3] / "scripts/prepare_mooring_qualification.py"


def test_packaging_fixture_without_optional_api(monkeypatch):
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    prior_path = sys.path[:]
    assert packaging.__wrapped__(monkeypatch).prepare_bundle
    assert sys.path == prior_path


@pytest.fixture
def packaging(monkeypatch):
    monkeypatch.setenv("PYTHONHASHSEED", "0")

    def prohibited(*args, **kwargs):
        pytest.fail("packaging attempted native construction")

    api = sys.modules.get("OrcFxAPI")
    if api is not None:
        monkeypatch.setattr(api, "Model", prohibited)
    # Exercise the offline route even on workstations with the API installed.
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    spec = importlib.util.spec_from_file_location("qualification_packaging", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    prior_path = sys.path[:]
    try:
        spec.loader.exec_module(module)
    finally:
        sys.path[:] = prior_path
    return module


def test_existing_output_is_rejected_without_changes(packaging, tmp_path):
    marker = tmp_path / "keep.txt"
    marker.write_text("original")
    with pytest.raises((ValueError, FileExistsError)):
        packaging.prepare_bundle(tmp_path)
    assert marker.read_text() == "original"
    assert list(tmp_path.iterdir()) == [marker]


@pytest.mark.parametrize("seed", [None, "1"])
def test_seed_required_before_writing(packaging, tmp_path, monkeypatch, seed):
    monkeypatch.delenv("PYTHONHASHSEED", raising=False)
    if seed is not None:
        monkeypatch.setenv("PYTHONHASHSEED", seed)
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="seed"):
        packaging.prepare_bundle(output)
    assert not output.exists()


def test_complete_bundle_and_reference_differences(packaging, tmp_path):
    original = packaging.SOURCE.read_bytes()
    manifest_path = packaging.prepare_bundle(tmp_path / "output")
    data = verify_manifest(manifest_path)
    root = data["_root"]
    assert (root / "source/spec.yml").read_bytes() == original
    assert packaging.SOURCE.read_bytes() == original
    actual = {p.relative_to(root).as_posix() for p in (root / "bundle").rglob("*") if p.is_file()}
    listed = {item["path"] for item in data["files"]}
    assert actual | {"source/spec.yml"} == listed
    assert [x["path"] for x in data["files"]] == sorted(listed)
    assert data["master"] == "bundle/master.yml"
    differences = json.loads((root / "reference-differences.json").read_text())
    assert differences["reference_sha256"]
    assert differences["difference_count"] == len(differences["differences"])
    assert differences["difference_count"] > 0
    assert differences["reference_compatible"] is False
    assert differences["schema_version"] == 2
    assert differences["missing_member_hash_rule"] == packaging._missing_member_hash_rule()
    absent = packaging._value_hash(None, present=False)
    for item in differences["differences"]:
        if item["kind"] in ("missing_key", "missing_item"):
            assert (item["reference_value_sha256"] == absent) is not item["reference_present"]
            assert (item["candidate_value_sha256"] == absent) is not item["candidate_present"]
    assert all("path" in item for item in differences["differences"])


def test_existing_file_is_rejected(packaging, tmp_path):
    output = tmp_path / "file"
    output.write_text("preserve")
    with pytest.raises(FileExistsError):
        packaging.prepare_bundle(output)
    assert output.read_text() == "preserve"


def test_failed_generation_removes_only_owned_output(packaging, tmp_path, monkeypatch):
    def fail(*args):
        raise ValueError("injected generation failure")

    monkeypatch.setattr(packaging.ModularModelGenerator, "generate", fail)
    neighbor = tmp_path / "neighbor"
    neighbor.write_text("preserve")
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="injected"):
        packaging.prepare_bundle(output)
    assert not output.exists()
    assert neighbor.read_text() == "preserve"


def test_reference_diff_preserves_nested_missing_and_typed_values(packaging):
    before = {"General": {"a": [False, 2], "gone": 3}}
    after = {"General": {"a": [0], "added": 4}}
    differences = list(packaging._differences(before, after))
    assert {x["path"] for x in differences} == {
        "/General/a/0", "/General/a/1", "/General/gone", "/General/added",
    }
    assert all(x["reference_value_sha256"] != x["candidate_value_sha256"]
               for x in differences)


@pytest.mark.parametrize("reference,candidate,reference_present", [
    ({"General": {"ExplicitNull": None}}, {"General": {}}, True),
    ({"General": {}}, {"General": {"ExplicitNull": None}}, False),
])
def test_present_null_and_absent_mapping_member_have_distinct_hashes(
        packaging, reference, candidate, reference_present):
    difference, = list(packaging._differences(reference, candidate))
    null_sha256 = "74234e98afe7498fb5daf1f36ac2d78acc339464f950703b8c019892f982b90b"
    absent_sha256 = "feb0e82e6d4e278cde90a7ae8c85488c89227c452d53bb0384db99b02a2ebbab"
    assert difference["path"] == "/General/ExplicitNull"
    assert difference["kind"] == "missing_key"
    assert difference["reference_present"] is reference_present
    assert difference["candidate_present"] is not reference_present
    present_key = "reference_value_sha256" if reference_present else "candidate_value_sha256"
    absent_key = "candidate_value_sha256" if reference_present else "reference_value_sha256"
    assert difference[present_key] == null_sha256
    assert difference[absent_key] == absent_sha256
    assert difference[present_key] != difference[absent_key]


@pytest.mark.parametrize("reference,candidate,reference_present", [
    ([None], [], True),
    ([], [None], False),
])
def test_present_null_and_absent_list_item_have_distinct_hashes(
        packaging, reference, candidate, reference_present):
    difference, = list(packaging._differences(reference, candidate))
    null_sha256 = "74234e98afe7498fb5daf1f36ac2d78acc339464f950703b8c019892f982b90b"
    absent_sha256 = "feb0e82e6d4e278cde90a7ae8c85488c89227c452d53bb0384db99b02a2ebbab"
    assert difference["path"] == "/0"
    assert difference["kind"] == "missing_item"
    assert difference["reference_present"] is reference_present
    assert difference["candidate_present"] is not reference_present
    present_key = "reference_value_sha256" if reference_present else "candidate_value_sha256"
    absent_key = "candidate_value_sha256" if reference_present else "reference_value_sha256"
    assert difference[present_key] == null_sha256
    assert difference[absent_key] == absent_sha256
    assert difference[present_key] != difference[absent_key]


def test_missing_mapping_container_and_present_empty_mapping_share_disclosed_leaf_rows(
        packaging):
    reference = {"Section": {"Value": 1}}
    absent_container = list(packaging._differences(reference, {}))
    present_empty_mapping = list(packaging._differences(reference, {"Section": {}}))
    assert absent_container == present_empty_mapping
    assert absent_container == [{
        "path": "/Section/Value", "kind": "missing_key",
        "reference_present": True, "candidate_present": False,
        "reference_value_sha256": packaging._value_hash(1),
        "candidate_value_sha256": packaging._value_hash(None, present=False),
    }]
    assert packaging._missing_member_hash_rule()["container_limit"] == {
        "id": "recursive-leaf-only-v2",
        "mapping_key_missing_nonempty_mapping_action": "recurse_with_empty_mapping",
        "mapping_key_missing_nonempty_mapping_container_row_emitted": False,
        "mapping_key_missing_nonempty_mapping_distinguishes_absent_from_empty": False,
        "mapping_key_missing_other_value_action": "hash_whole_value",
        "list_missing_member_action": "hash_whole_member",
        "list_missing_member_row_emitted": True,
    }


@pytest.mark.parametrize("reference,candidate,reference_present", [
    ([{"Value": 1}], [], True),
    ([], [{"Value": 1}], False),
])
def test_missing_list_mapping_member_hashes_whole_member(
        packaging, reference, candidate, reference_present):
    difference, = list(packaging._differences(reference, candidate))
    assert difference["path"] == "/0"
    assert difference["kind"] == "missing_item"
    assert difference["reference_present"] is reference_present
    assert difference["candidate_present"] is not reference_present
    present_key = "reference_value_sha256" if reference_present else "candidate_value_sha256"
    absent_key = "candidate_value_sha256" if reference_present else "reference_value_sha256"
    assert difference[present_key] == packaging._value_hash({"Value": 1})
    assert difference[absent_key] == packaging._value_hash(None, present=False)
    limit = packaging._missing_member_hash_rule()["container_limit"]
    assert limit["list_missing_member_action"] == "hash_whole_member"
    assert limit["list_missing_member_row_emitted"] is True


def test_missing_mapping_key_with_list_hashes_whole_value(packaging):
    difference, = list(packaging._differences({"Lines": [{"Name": "A"}]}, {}))
    assert difference == {
        "path": "/Lines", "kind": "missing_key",
        "reference_present": True, "candidate_present": False,
        "reference_value_sha256": packaging._value_hash([{"Name": "A"}]),
        "candidate_value_sha256": packaging._value_hash(None, present=False),
    }
    assert packaging._missing_member_hash_rule()["container_limit"][
        "mapping_key_missing_other_value_action"] == "hash_whole_value"


def _bundle(tmp_path, master, includes):
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "master.yml").write_text(master, encoding="utf-8")
    for name, text in includes.items():
        (bundle / name).write_text(text, encoding="utf-8")
    return bundle


def test_generated_sections_merge_explicit_include_records(packaging, tmp_path):
    bundle = _bundle(tmp_path, "- includefile: a.yml\n- includefile: b.yml\n",
                     {"a.yml": "General:\n  X: 1\n", "b.yml": "General:\n  Y: 2\nLines: []\n"})
    assert packaging._generated_sections(bundle) == {"General": {"X": 1, "Y": 2}, "Lines": []}


@pytest.mark.parametrize("master,includes", [
    # a duplicate key would otherwise hide the first assignment from the comparison
    ("- includefile: a.yml\n", {"a.yml": "General:\n  X: 1\n  X: 2\n"}),
    # include records name exactly one file and nothing else
    ("- includefile: a.yml\n  extra: 1\n", {"a.yml": "General: {}\n"}),
    ("- a.yml\n", {"a.yml": "General: {}\n"}),
    ("includefile: a.yml\n", {"a.yml": "General: {}\n"}),
    # a generated include is a direct section mapping, not a further include
    ("- includefile: a.yml\n", {"a.yml": "includefile: b.yml\n", "b.yml": "General: {}\n"}),
    ("- includefile: a.yml\n", {"a.yml": "- General\n"}),
    # paths stay inside the bundle
    ("- includefile: ../a.yml\n", {}),
])
def test_generated_sections_refuse_ambiguous_includes(packaging, tmp_path, master, includes):
    (tmp_path / "a.yml").write_text("General: {}\n", encoding="utf-8")
    bundle = _bundle(tmp_path, master, includes)
    with pytest.raises(ValueError):
        packaging._generated_sections(bundle)