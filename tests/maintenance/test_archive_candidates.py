"""Tests for the archive-candidate generator: blob dedup and model-YAML features (M06)."""
import importlib.util
import sys
from pathlib import Path

_MOD = (Path(__file__).resolve().parents[2]
        / "scripts" / "maintenance" / "archive_candidates.py")
_spec = importlib.util.spec_from_file_location("archive_candidates", _MOD)
ac = importlib.util.module_from_spec(_spec)
sys.modules["archive_candidates"] = ac
_spec.loader.exec_module(ac)


def _row(path, sha, size):
    return {"path": path, "sha256_blob": sha, "blob_size_bytes": size}


# --- blob dedup ---------------------------------------------------------------

def test_dedup_groups_counts_duplicates_and_bytes_saved():
    rows = [_row("a/x.png", "s1", 100), _row("b/x.png", "s1", 100), _row("c/y.png", "s1", 100),
            _row("d/z.pdf", "s2", 50), _row("e/w.pdf", "s3", 70), _row("f/w.pdf", "s3", 70)]
    d = ac.dedup_blobs(rows)
    assert d["files"] == 6
    assert d["unique_blobs"] == 3
    assert d["duplicate_groups"] == 2
    assert d["duplicate_files"] == 3          # copies beyond the first in each group
    assert d["bytes_total"] == 490
    assert d["bytes_unique"] == 220
    assert d["bytes_saved"] == 270
    # canonical copy is the lexicographically first path of each group
    assert d["canonical"]["s1"] == "a/x.png"
    assert d["canonical"]["s3"] == "e/w.pdf"
    big = d["groups"][0]
    assert big["sha256_blob"] == "s1" and big["count"] == 3 and big["bytes_saved"] == 200


def test_dedup_no_duplicates():
    d = ac.dedup_blobs([_row("a", "s1", 1), _row("b", "s2", 2)])
    assert d["duplicate_groups"] == 0 and d["bytes_saved"] == 0 and d["groups"] == []


def test_blob_store_path_is_content_addressed_and_keeps_extension():
    sha = "ab" + "0" * 62
    assert ac.blob_store_path(sha, "docs/x/Report.PDF") == f"blobs/sha256/ab/{sha}.pdf"
    assert ac.blob_store_path(sha, "docs/x/noext") == f"blobs/sha256/ab/{sha}"


def test_blob_map_maps_every_path_to_one_blob():
    rows = [_row("a/x.png", "s1" * 32, 10), _row("b/x.png", "s1" * 32, 10), _row("c.pdf", "s2" * 32, 5)]
    m = ac.blob_map(rows)
    assert [r["path"] for r in m] == ["a/x.png", "b/x.png", "c.pdf"]
    assert m[0]["blob_path"] == m[1]["blob_path"]
    assert m[0]["is_canonical_copy"] is True and m[1]["is_canonical_copy"] is False
    assert m[2]["is_canonical_copy"] is True


# --- src/tests reference exclusion --------------------------------------------

def test_reference_patterns_include_path_tail_dirs_and_unique_basename():
    pats = ac.reference_patterns("docs/domains/orcaflex/library/model_library/a01/spec.yml",
                                 basename_unique=False)
    assert "docs/domains/orcaflex/library/model_library/a01/spec.yml" in pats
    assert "a01/spec.yml" in pats
    assert "spec.yml" not in pats                      # generic basename alone is not evidence
    assert "docs/domains/orcaflex/library/model_library" in pats   # directory reference
    assert "docs/domains" not in pats                  # too shallow to be evidence
    pats_u = ac.reference_patterns("docs/x/y/unique_name.yml", basename_unique=True)
    assert "unique_name.yml" in pats_u


def test_referenced_by_code_matches_any_pattern():
    hits = {"a01/spec.yml"}
    assert ac.referenced_by_code("docs/domains/orcaflex/library/model_library/a01/spec.yml",
                                 hits, basename_unique=False)
    assert not ac.referenced_by_code("docs/q/r/s/t.yml", hits, basename_unique=False)


# --- model YAML features ------------------------------------------------------

NATIVE = {
    "General": {"UnitsSystem": "SI", "StageDuration": [7, 35]},
    "Environment": {"WaveTrains": [{"Name": "Wave1", "WaveType": "JONSWAP", "WaveHs": 6}],
                    "SeabedModel": "Elastic"},
    "LineTypes": [{"Name": "My private type", "Category": "General", "OD": 0.3},
                  {"Name": "Other", "Category": "Homogeneous pipe", "OD": 0.2}],
    "Vessels": [{"Name": "SomeShip", "VesselType": "Secret Type", "PrimaryMotion": "None"}],
}


def test_model_kind_detection():
    assert ac.model_yaml_kind([NATIVE]) == "orcaflex-native"
    assert ac.model_yaml_kind([{"LineTypes": [{"Name": "a"}]}]) == "orcaflex-native"
    assert ac.model_yaml_kind([{"metadata": {"name": "x"}, "environment": {}}]) == "spec"
    assert ac.model_yaml_kind([{"Bodies": [], "SolveType": "x"}]) == "orcawave"
    assert ac.model_yaml_kind([{"title": "x"}]) is None
    assert ac.model_yaml_kind([None, "text"]) is None


def test_extract_features_sections_keys_and_enum_values():
    f = ac.extract_features([NATIVE])
    assert "section:LineTypes" in f and "section:Vessels" in f
    assert "key:Environment/WaveTrains/WaveType" in f        # list indices collapsed
    assert "key:LineTypes/OD" in f
    assert "value:Environment/WaveTrains/WaveType=jonswap" in f      # values case-normalised
    assert "value:LineTypes/Category=homogeneous pipe" in f
    assert "value:Environment/SeabedModel=elastic" in f


def test_extract_features_hides_group_maps_and_reference_values():
    doc = {"Groups": {"Structure": {"Jacket": "Model", "Barge": "Jacket"}},
           "Vessels": [{"Name": "x", "vessel_type": "Private Type", "SupportCoordinateSystem": "Ramp Pivot"}],
           "Shapes": [{"type": "X65+coating"}],
           "environment": {"waves": {"type": "Dean stream"}},
           "Ship_7Seas": {"Bridle 1": {"Length": 3}}}
    f = ac.extract_features([doc])
    joined = "\n".join(f)
    for secret in ("Jacket", "Barge", "Private Type", "Ramp Pivot", "X65", "7Seas", "Bridle"):
        assert secret not in joined, secret
    assert "key:Groups/Structure/*" in f
    assert "value:environment/waves/type=dean stream" in f


def test_extract_features_never_emits_object_names_or_references():
    f = ac.extract_features([NATIVE])
    joined = "\n".join(f)
    for secret in ("My private type", "SomeShip", "Secret Type", "Wave1"):
        assert secret not in joined
    # numbers are data, not features
    assert not any(x.startswith("value:") and x.endswith("=6") for x in f)


def test_extract_features_collapses_name_keyed_maps():
    doc = {"lines": {"riser A 01": {"length": 10}, "riser B 02": {"length": 12}}}
    f = ac.extract_features([doc])
    assert not any("riser A" in x for x in f)
    assert "key:lines/*/length" in f


def test_candidate_keep_set_covers_features_found_only_in_candidates():
    ff = {"keep/a.yml": {"x", "y"},          # stays in repo
          "cand/b.yml": {"x", "z"},          # z exists only in candidates
          "cand/c.yml": {"z", "w"},          # w exists only here -> c covers z and w
          "cand/d.yml": {"y"}}               # y stays reachable via keep/a.yml
    keep, only = ac.candidate_keep_set(ff, {"cand/b.yml", "cand/c.yml", "cand/d.yml"})
    assert only == {"z", "w"}
    assert keep == ["cand/c.yml"]


def test_unique_features_and_cover():
    ff = {"a.yml": {"x", "y"}, "b.yml": {"x", "z"}, "c.yml": {"x"}}
    u = ac.unique_features(ff)
    assert u == {"a.yml": ["y"], "b.yml": ["z"]}
    cover = ac.feature_cover(ff)
    assert set(cover) == {"a.yml", "b.yml"}
    covered = set().union(*(ff[p] for p in cover))
    assert covered == {"x", "y", "z"}
