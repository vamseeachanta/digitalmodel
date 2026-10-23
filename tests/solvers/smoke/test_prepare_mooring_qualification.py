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
    assert differences["schema_version"] == 3
    assert differences["blocking_difference_count"] == differences["difference_count"]
    assert sum(differences["category_counts"].values()) == differences["difference_count"]
    assert differences["missing_member_hash_rule"] == packaging._missing_member_hash_rule()
    absent = packaging._value_hash(None, present=False)
    for item in differences["differences"]:
        if item["kind"] in ("missing_key", "missing_item"):
            assert (item["reference_value_sha256"] == absent) is not item["reference_present"]
            assert (item["candidate_value_sha256"] == absent) is not item["candidate_present"]
    assert all("path" in item for item in differences["differences"])
    # Golden output binds the observed source/input digests and every strict
    # row. This checks reproducibility, not an independent physical benchmark;
    # arithmetic and classification expectations are tested separately below.
    recorded = SCRIPT.parents[1] / "docs/benchmarks/mooring_buoy/reference-classification.json"
    assert differences == json.loads(recorded.read_text(encoding="utf-8"))


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
        "category": "missing_data",
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
        "category": "missing_data",
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


@pytest.mark.parametrize("reference,candidate,category", [
    (4, 4.0, "equal_decimal_numeric"),
    (4.0, 4, "equal_decimal_numeric"),
    (0, -0.0, "equal_decimal_numeric"),
    (10**20, 1e20, "equal_decimal_numeric"),
    (10**20 + 1, 1e20, "value_or_type"),
    (4, 5.0, "value_or_type"),
    (4.0, 4.000000000000001, "value_or_type"),
    (True, 1, "value_or_type"),
    (False, 0.0, "value_or_type"),
    (4, "4", "value_or_type"),
    (float("inf"), 4, "value_or_type"),
    (float("nan"), 4, "value_or_type"),
    (float("inf"), float("-inf"), "value_or_type"),
    (float("nan"), float("nan"), "value_or_type"),
    (True, "Yes", "boolean_spelling"),
    ("Yes", True, "boolean_spelling"),
    (False, "No", "boolean_spelling"),
    ("No", False, "boolean_spelling"),
    (True, "No", "value_or_type"),
    (False, "Yes", "value_or_type"),
    (True, "yes", "value_or_type"),
    (1, "Yes", "value_or_type"),
    (None, "", "null_empty_string"),
    ("", None, "null_empty_string"),
    (None, "~", "value_or_type"),
])
def test_scalar_diagnostics_preserve_strict_differences(
        packaging, reference, candidate, category):
    """Independent checks: 40/10 = 4; 0 = -0; 10**20 = 1e20;
    10**20 + 1 differs by one; 5 - 4 = 1; the decimal spellings
    4.000000000000001 and 4.0 differ by 0.000000000000001.
    These are diagnostic equalities, with no acceptance tolerance.
    Booleans and nulls follow YAML 1.1 type definitions (2005-01-18),
    https://yaml.org/type/bool.html and https://yaml.org/type/null.html;
    only matching, case-sensitive Yes/No pairs are diagnostic candidates.
    YAML float (same edition, https://yaml.org/type/float.html) defines
    infinities and NaN; neither supplies finite decimal equality evidence.
    """
    difference, = list(packaging._differences(reference, candidate))
    assert difference["kind"] == "value_or_type"
    assert difference["category"] == category
    assert difference["reference_value_sha256"] == packaging._value_hash(reference)
    assert difference["candidate_value_sha256"] == packaging._value_hash(candidate)


@pytest.mark.parametrize("reference,candidate,expected", [
    ({"General": {"DefaultViewAzimuth": 4}}, {"General": {}},
     "presentation_key_candidate"),
    ({"General": {}}, {"General": {"DefaultViewCentre": [0, 0, 0]}},
     "presentation_key_candidate"),
    ({"Lines": [{"ContactPen": [1, "Solid", "Blue"]}]}, {"Lines": [{}]},
     "presentation_key_candidate"),
    ({"Environment": {"VerticalWindVariationFactor": None}}, {"Environment": {}},
     "missing_data"),
    ({"Other": {"DefaultViewAzimuth": 4}}, {"Other": {}}, "missing_data"),
    ({"General": {"DefaultViewAzimuthExtra": 4}}, {"General": {}}, "missing_data"),
    ({"Lines": [{"Mass": 4}]}, {"Lines": []}, "missing_data"),
    ({"a/b~c": None}, {}, "missing_data"),
])
def test_missing_categories_are_narrow_and_presence_is_retained(
        packaging, reference, candidate, expected):
    """Synthetic key inventory from #2104; no physical values or waivers.
    Wind variation is a model control, with no evidence of display-only use.
    The remaining numbers are arbitrary structural fixtures.
    """
    difference, = list(packaging._differences(reference, candidate))
    assert difference["category"] == expected
    assert difference["reference_present"] is not difference["candidate_present"]


def test_presentation_value_change_is_not_a_missing_key_candidate(packaging):
    """Independent hand calculation: azimuth 5 - 4 = 1, a value change."""
    difference, = list(packaging._differences(
        {"General": {"DefaultViewAzimuth": 4}},
        {"General": {"DefaultViewAzimuth": 5}},
    ))
    assert difference["category"] == "value_or_type"


@pytest.mark.parametrize("reference,candidate", [(4, 4), (4.0, 4.0), ("Yes", "Yes")])
def test_identical_scalars_do_not_create_diagnostics(packaging, reference, candidate):
    """Reflexive equality of literal fixtures needs no normalization."""
    assert list(packaging._differences(reference, candidate)) == []


def test_diagnostic_report_does_not_waive_representation_candidates(
        packaging, tmp_path, monkeypatch):
    """Hand count: one numeric pair + one bool pair + one null pair +
    one absent view key + one changed value + one absent wind key = six
    blocking rows. 4 = 40/10 and 5 - 4 = 1; no native equivalence follows.
    """
    reference = {"General": {"DefaultViewAzimuth": 4}, "number": 4,
                 "flag": True, "script": None, "changed": 4,
                 "Environment": {"VerticalWindVariationFactor": None}}
    candidate = {"General": {}, "number": 4.0, "flag": "Yes", "script": "",
                 "changed": 5, "Environment": {}}
    monkeypatch.setattr(packaging, "read_yaml", lambda path: reference)
    monkeypatch.setattr(packaging, "_generated_sections", lambda path: candidate)
    packaging._reference_report(tmp_path, {"source": {"sha256": "synthetic"}, "files": []})
    report = json.loads((tmp_path / "reference-differences.json").read_text())
    assert report["schema_version"] == 3
    assert report["difference_count"] == report["blocking_difference_count"] == 6
    assert report["category_counts"] == {
        "equal_decimal_numeric": 1, "boolean_spelling": 1, "null_empty_string": 1,
        "presentation_key_candidate": 1, "value_or_type": 1, "missing_data": 1,
    }
    assert report["reference_compatible"] is False
    assert report["native_verified"] is False
    assert report["engineering_parity"] is False
    for decision in report["category_decisions"].values():
        assert decision["semantic_status"] == "NOT_EVALUATED"
        assert decision["blocking"] is True
        assert decision["basis"]
        assert decision["required_evidence"]
