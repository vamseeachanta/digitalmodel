"""R01 regressions: deeply nested inputs, uncertainty and scan provenance."""
import importlib.util
import csv
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/maintenance/archive_candidates.py"
sys.path.insert(0, str(SCRIPT.parent))
spec = importlib.util.spec_from_file_location("archive_candidates", SCRIPT)
ac = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = ac
spec.loader.exec_module(ac)


def test_depth_eight_model_keeps_unique_feature(tmp_path):
    nested = {"RareSolverMethod": "Explicit"}
    for _ in range(8):
        nested = {"Settings": nested}
    path = tmp_path / "model.yml"
    path.write_text(yaml.safe_dump({"General": nested}))
    _, kind, features, error = ac._parse_features((str(tmp_path), "model.yml"))
    assert error is None and kind == "orcaflex-native"
    staying = {}
    for _ in range(8):
        staying = {"Settings": staying}
    keep, _ = ac.candidate_keep_set({"model.yml": set(features),
                                   "staying.yml": ac.extract_features([{"General": staying}])},
                                  {"model.yml"})
    assert keep == ["model.yml"]
    assert any("RareSolverMethod" in f for f in features)


def test_cyclic_yaml_reports_hold_reason(tmp_path):
    (tmp_path / "model.yml").write_text("General: &loop\n  Settings: *loop\n")
    _, _, _, error = ac._parse_features((str(tmp_path), "model.yml"))
    assert error == "CyclicYamlError"


def test_shared_alias_features_are_kept_at_both_paths():
    common = {"RareSolverMethod": "Explicit"}
    features = ac.extract_features([{"General": common, "Environment": common}])
    assert "key:General/RareSolverMethod" in features
    assert "key:Environment/RareSolverMethod" in features


def test_feature_walk_has_no_python_recursion_limit():
    node = {"RareSolverMethod": "Explicit"}
    for _ in range(1100):
        node = {"Settings": node}
    assert any("RareSolverMethod" in f for f in ac.extract_features([{"General": node}]))


def test_deep_reference_and_registry_paths_are_excluded():
    from archive_reference_safety import reference_safety
    nested = {"file": "../../inputs/model.dat"}
    for _ in range(8):
        nested = {"Settings": nested}
    candidates = ["docs/inputs/model.dat", "examples/input.sim", "docs/page.png"]
    texts = {"docs/examples/deep/model.yml": yaml.safe_dump({"General": nested}),
             "data/inputs.yaml": "input: examples/input.sim\n",
             "mkdocs.yml": "nav:\n  - Figure: page.png\n"}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert set(excluded) == set(candidates)
    assert held == {}


def test_ambiguous_and_runtime_references_are_held():
    from archive_reference_safety import reference_safety
    candidates = ["docs/a/report.html", "docs/b/report.html", "docs/models/a.dat",
                  "other/unrelated.pdf"]
    texts = {"examples/run.py": 'p = f"docs/models/{name}.dat"\n',
             "examples/config.yml": "report: report.html\n"}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {}
    assert set(held) == set(candidates[:3])


def test_runtime_join_and_unanchored_glob_hold_potential_inputs():
    from archive_reference_safety import reference_safety
    candidates = ["docs/models/a.dat", "other/b.dat", "docs/a.sim", "docs/free.png"]
    texts = {"examples/run.py": ('from pathlib import Path\n'
                                'root = Path("docs/models")\n'
                                'p = root / (name + ".dat")\n'
                                'q = unknown.glob("*.sim")\n')}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {}
    assert set(held) == {"docs/models/a.dat", "docs/a.sim"}


def test_formatted_runtime_path_is_held_instead_of_assumed_literal():
    from archive_reference_safety import reference_safety
    candidates = ["docs/models/       a.dat", "docs/models/a.dat"]
    source = "examples/run.py"
    text = 'name = "a"\np = f"docs/models/{name:>8}.dat"\n'
    refs = ac.explicit_references({source: text}, candidates, candidates + [source])
    assert refs == {}
    _, held, _ = reference_safety({source: text}, candidates, candidates + [source])
    assert set(held) == set(candidates)


def test_filename_prefilter_preserves_boundaries_and_spaced_names():
    candidates = ["docs/model.dat", "docs/file name.pdf", "docs/unused.png"]
    text = 'wrong = "xmodel.dat"\np = "file name.pdf"\nq = "model.dat"\n'
    refs = ac.explicit_references({"src/read.py": text}, candidates, candidates)
    assert set(refs) == set(candidates[:2])


def test_retained_model_dependency_is_excluded():
    from archive_reference_safety import reference_safety
    candidates = ["docs/large.yml", "docs/inputs/a.dat"]
    texts = {"docs/large.yml": "General:\n  Settings:\n    BaseFile: inputs/a.dat\n"}
    excluded, held, _ = reference_safety(texts, candidates, candidates)
    assert set(excluded) == {"docs/inputs/a.dat"}
    assert held == {}


@pytest.mark.parametrize("tail", ["General: &loop\n  Settings: *loop\n", "General: [broken\n"])
def test_unparseable_or_cyclic_model_keeps_readable_dependencies(tail):
    from archive_reference_safety import reference_safety
    candidates = ["docs/model.yml", "docs/inputs/a.dat"]
    texts = {"docs/model.yml": "BaseFile: inputs/a.dat\n" + tail}
    excluded, held, gaps = reference_safety(texts, candidates, candidates)
    assert "docs/inputs/a.dat" in set(excluded) | set(held)
    assert "docs/model.yml" in held
    assert gaps


def test_generated_manifest_is_not_a_consumer():
    from archive_reference_safety import reference_safety
    candidates = ["docs/a/input.dat"]
    texts = {"docs/archive/archive-candidates-2026-10-08.summary.json": "input.dat",
             "docs/archive/consumer.yml": "input: ../a/input.dat\n"}
    excluded, _, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded[candidates[0]]["source"] == "docs/archive/consumer.yml"


def test_runtime_holds_preserve_d01_hygiene_exception():
    from archive_reference_safety import reference_safety
    source = "tests/legal/test_published_pages_have_no_internal_paths.py"
    candidates = ["docs/a.html", "docs/b.html"]
    texts = {source: 'pages = root.rglob("*.html")\nexplicit = "docs/a.html"\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + [source],
                                         hygiene_sources={source})
    assert set(excluded) == {"docs/a.html"}
    assert held == {}


def _git(repo, *args):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.check_output(["git", "-C", str(repo), *args], env=env).decode().strip()


def test_provenance_uses_merge_base_and_rejects_dirty_inputs(tmp_path, monkeypatch):
    from archive_reference_safety import scan_provenance
    _git(tmp_path, "init")
    _git(tmp_path, "config", "user.name", "Test")
    _git(tmp_path, "config", "user.email", "test@example.invalid")
    (tmp_path / "input.dat").write_text("original")
    _git(tmp_path, "add", ".")
    _git(tmp_path, "commit", "-m", "base")
    base = _git(tmp_path, "rev-parse", "HEAD")
    _git(tmp_path, "update-ref", "refs/remotes/origin/main", base)
    (tmp_path / "input.dat").write_text("revision")
    _git(tmp_path, "commit", "-am", "change")
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.setenv(name, str(tmp_path / "does-not-exist"))
    result = scan_provenance(str(tmp_path))
    assert result["base_commit"] == base
    assert result["scanned_head"] == _git(tmp_path, "rev-parse", "HEAD") != base
    assert result["scanned_index_tree"] == _git(tmp_path, "rev-parse", "HEAD^{tree}")
    (tmp_path / "input.dat").write_text("dirty")
    with pytest.raises(ValueError, match="dirty"):
        scan_provenance(str(tmp_path))


def test_resolvable_fstring_is_static_reference():
    candidates = ["docs/inputs/a.dat", "other/a.dat"]
    refs = ac.explicit_references(
        {"src/read.py": 'base = "docs/inputs"\np = f"{base}/a.dat"\n'},
        candidates, candidates + ["src/read.py"])
    assert set(refs) == {candidates[0]}


def test_generator_partitions_move_keep_hold_and_excluded(tmp_path, monkeypatch):
    repo = tmp_path / "repo"
    repo.mkdir()
    files = {"src/read.py": "# consumer\n",
             "docs/free.png": "image",
             "docs/input.dat": "input",
             "docs/held.sim": "simulation",
             "examples/run.py": 'p = f"docs/{name}.sim"\n',
             "docs/example.yml": "input: input.dat\nGeneral: {}\n",
             "docs/keep.yml": "General:\n  RareSolverMethod: Explicit\n#" + "x" * 1_000_001,
             "docs/bad.yml": "[ invalid\n#" + "x" * 1_000_001}
    for path, content in files.items():
        output = repo / path
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(content)
    _git(repo, "init")
    _git(repo, "config", "user.name", "Test")
    _git(repo, "config", "user.email", "test@example.invalid")
    _git(repo, "add", ".")
    _git(repo, "commit", "-m", "inputs")
    _git(repo, "update-ref", "refs/remotes/origin/main", _git(repo, "rev-parse", "HEAD"))
    (tmp_path / "previous.txt").write_text("")
    out_csv, out_json = tmp_path / "out.csv", tmp_path / "out.json"
    before = _git(Path.cwd(), "rev-parse", "HEAD")
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(name, str(Path.cwd() / ".git"))
    ac.main([str(repo), str(tmp_path / "previous.txt"), str(out_csv), str(out_json)])
    rows = list(csv.DictReader(out_csv.open()))
    summary = json.loads(out_json.read_text())
    excluded = {r["path"] for r in summary["excluded_referenced"]["paths"]}
    kept = {r["path"] for r in rows if r["keep_for_feature"] == "True"}
    held = {r["path"] for r in rows if r["needs_human_check"] == "True"}
    move = {r["path"] for r in rows} - kept - held
    assert excluded == {"docs/input.dat"}
    assert kept == {"docs/keep.yml"}
    assert held == {"docs/held.sim", "docs/bad.yml"}
    assert move == {"docs/free.png"}
    assert len(excluded | kept | held | move) == summary["rule_matches"] == 5
    assert summary["move_set_after_feature_keep"]["files"] == 1
    assert _git(Path.cwd(), "rev-parse", "HEAD") == before


def test_empty_candidate_manifest_has_headers(tmp_path):
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init")
    _git(repo, "config", "user.name", "Test")
    _git(repo, "config", "user.email", "test@example.invalid")
    (repo / "src").mkdir()
    (repo / "src/read.py").write_text("# no archive inputs\n")
    _git(repo, "add", ".")
    _git(repo, "commit", "-m", "empty selection")
    _git(repo, "update-ref", "refs/remotes/origin/main", _git(repo, "rev-parse", "HEAD"))
    (tmp_path / "previous.txt").write_text("")
    ac.main([str(repo), str(tmp_path / "previous.txt"), str(tmp_path / "out.csv"),
             str(tmp_path / "out.json"), "--blob-map", str(tmp_path / "map.csv")])
    assert list(csv.DictReader((tmp_path / "out.csv").open())) == []
    assert "path" in (tmp_path / "map.csv").read_text()
