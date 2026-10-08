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

# Owner decision C02 (2026-10-08): explicit file references only. A bare directory
# mention no longer excludes everything under it; a directory counts only when code
# reads files from it by pattern, and then only the matching files are excluded.

D = "docs/a/b/c/d"
TRACKED = [f"{D}/model.dat", f"{D}/other.dat", f"{D}/run.sim", f"{D}/sub/deep.sim",
           f"{D}/named_only.png", f"{D}/xmodel2.dat", "docs/x/y/z/w/lonely.pdf",
           "docs/x/y/z/w/sub/lonelier.pdf", "src/pkg/mod.py", "tests/test_a.py"]
CANDS = [p for p in TRACKED if not p.startswith(("src/", "tests/"))]


def _refs(code):
    return ac.explicit_references(code, CANDS, TRACKED)


def test_explicit_repo_path_excludes_only_that_file():
    r = _refs({"tests/test_a.py": f'P = "{D}/model.dat"\n'})
    assert set(r) == {f"{D}/model.dat"}
    assert r[f"{D}/model.dat"]["evidence"] == "path"
    assert r[f"{D}/model.dat"]["source"] == "tests/test_a.py"


def test_path_join_to_a_file_is_an_explicit_path():
    code = ('from pathlib import Path\n'
            'REPO = Path(__file__).resolve().parents[1]\n'
            'F = REPO / "docs" / "a" / "b/c" / "d" / "other.dat"\n')
    r = _refs({"tests/test_a.py": code})
    assert set(r) == {f"{D}/other.dat"}
    assert r[f"{D}/other.dat"]["evidence"] == "path"


def test_filename_only_mention_excludes_by_exact_filename():
    r = _refs({"src/pkg/mod.py": "# reads named_only.png at runtime\n"})
    assert set(r) == {f"{D}/named_only.png"}
    assert r[f"{D}/named_only.png"]["evidence"] == "filename"


def test_filename_must_match_whole_name_not_a_substring():
    # "model2.dat" is not "xmodel2.dat"; "xmodel.dat" is not "model.dat"
    r = _refs({"src/pkg/mod.py": 'a = "model2.dat"; b = "xmodel.dat"\n'})
    assert r == {}


def test_glob_from_a_resolved_directory_excludes_only_matching_files():
    code = ('from pathlib import Path\n'
            'REPO = Path(__file__).resolve().parents[1]\n'
            f'DATA = REPO / "{D}"\n'
            'SIMS = sorted(DATA.glob("*.sim"))\n')
    r = _refs({"tests/test_a.py": code})
    assert set(r) == {f"{D}/run.sim"}           # not sub/deep.sim, not the .dat files
    assert r[f"{D}/run.sim"]["evidence"] == "glob"
    assert r[f"{D}/run.sim"]["matched"] == f"{D}/*.sim"


def test_rglob_and_walk_are_recursive_patterns():
    code = ('import os\nfrom pathlib import Path\n'
            f'A = Path("{D}")\n'
            'x = list(A.rglob("*.sim"))\n'
            'for _ in os.walk("docs/x/y/z/w"):\n    pass\n')
    r = _refs({"tests/test_a.py": code})
    assert set(r) == {f"{D}/run.sim", f"{D}/sub/deep.sim",
                      "docs/x/y/z/w/lonely.pdf", "docs/x/y/z/w/sub/lonelier.pdf"}


def test_glob_module_with_joined_literal_pattern():
    code = ('import glob, os\n'
            f'files = glob.glob(os.path.join("{D}", "*.dat"))\n')
    r = _refs({"src/pkg/mod.py": code})
    assert set(r) == {f"{D}/model.dat", f"{D}/other.dat", f"{D}/xmodel2.dat"}


def test_glob_literal_in_a_config_fixture():
    r = _refs({"tests/fixtures/cfg.yml": f"inputs: {D}/*.sim\n"})
    assert set(r) == {f"{D}/run.sim"}


def test_bare_directory_mention_does_not_exclude():
    code = ('from pathlib import Path\n'
            'ROOT = Path("docs/x/y/z/w")      # named, never read by pattern\n'
            f'# see {D} for the inputs\n'
            f'OTHER = "{D}/sub"\n')
    r = _refs({"tests/test_a.py": code, "tests/fixtures/notes.md": f"Data lives in {D}/\n"})
    assert r == {}


def test_unanchored_pattern_is_not_evidence():
    # "*.dat" with no resolvable directory cannot be shown to read any candidate
    r = _refs({"src/pkg/mod.py": 'import glob\nfiles = glob.glob("*.dat")\nP = some_dir().glob("*.sim")\n'})
    assert r == {}


def test_glob_match_semantics():
    assert ac.glob_match("a/*.sim", "a/run.sim")
    assert not ac.glob_match("a/*.sim", "a/sub/run.sim")
    assert ac.glob_match("a/**/*.sim", "a/run.sim")
    assert ac.glob_match("a/**/*.sim", "a/b/c/run.sim")
    assert ac.glob_match("a/**", "a/b/c.txt")
    assert not ac.glob_match("a/*/x.yml", "a/x.yml")


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
