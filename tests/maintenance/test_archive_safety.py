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


def test_yaml_references_preserve_escaped_and_template_paths():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat", "docs/models/a.dat"]
    texts = {"docs/run.yml": 'input: "docs/input\\u002edat"\nmodel: "docs/models/${name}.${ext}"\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert set(excluded) == {"docs/input.dat"}
    assert set(held) == {"docs/models/a.dat"}


def test_ambiguous_and_runtime_references_are_held():
    from archive_reference_safety import reference_safety
    candidates = ["docs/a/report.html", "docs/b/report.html", "docs/models/a.dat",
                  "other/unrelated.pdf"]
    texts = {"docs/run.py": 'p = f"docs/models/{name}.dat"\n',
             "examples/config.yml": "report: report.html\n"}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {}
    assert set(held) == set(candidates[:3])


def test_runtime_join_and_unanchored_glob_hold_potential_inputs():
    from archive_reference_safety import reference_safety
    candidates = ["docs/models/a.dat", "other/b.dat", "docs/a.sim", "docs/free.png"]
    texts = {"docs/run.py": ('from pathlib import Path\n'
                                'root = Path("docs/models")\n'
                                'p = root / (name + ".dat")\n'
                                'q = unknown.glob("*.sim")\n')}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {}
    assert set(held) == {"docs/models/a.dat", "docs/a.sim"}


def test_formatted_runtime_path_is_held_instead_of_assumed_literal():
    from archive_reference_safety import reference_safety
    candidates = ["docs/models/       a.dat", "docs/models/a.dat"]
    source = "docs/run.py"
    text = 'name = "a"\np = f"docs/models/{name:>8}.dat"\n'
    refs = ac.explicit_references({source: text}, candidates, candidates + [source])
    assert refs == {}
    _, held, _ = reference_safety({source: text}, candidates, candidates + [source])
    assert set(held) == set(candidates)


def test_numeric_formats_and_versions_are_not_file_references():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat", "docs/report.html"]
    texts = {"src/report.py": 'number = "{value:.3f}"\nversion = f"{major}.{minor}"\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {} and held == {}


def test_metadata_regex_directory_filters_and_string_formats_are_not_consumers():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat", "docs/report.html"]
    texts = {"src/validation.py": 'name_pattern = r"[A-Za-z0-9][A-Za-z0-9_.-]*"\n',
             ".claude/propagation.yml": 'exclude: ["*/.git/*", "*/.venv/*"]\n',
             "src/report.py": 'text = ",".join(["{value:.3f}"])\n',
             "scripts/directories.py": 'directory = root / "."\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {} and held == {}


def test_literal_string_join_can_resolve_an_input_filename():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat"]
    texts = {"src/read.py": 'filename = ".".join(["input", "dat"])\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert set(excluded) == set(candidates) and held == {}


def test_generic_format_in_actual_path_expression_requires_human_check():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat", "docs/report.html"]
    texts = {"docs/read.py": 'from pathlib import Path\np = Path(f"{stem}.{extension}")\n'}
    excluded, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert excluded == {} and set(held) == set(candidates)


def test_generic_format_in_path_division_requires_human_check():
    from archive_reference_safety import reference_safety
    candidates = ["docs/input.dat"]
    texts = {"docs/read.py": 'p = root / f"{stem}.{extension}"\n'}
    _, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert set(held) == set(candidates)


def test_batched_basename_counts_match_text_and_skip_binary(tmp_path):
    from archive_reference_safety import basename_references
    _git(tmp_path, "init")
    for name, data in {"one.txt": b"a.dat repeated a.dat\nb.pdf\n",
                       "two.txt": b"\xffa.dat\n",
                       "binary.bin": b"\0a.dat\n"}.items():
        (tmp_path / name).write_bytes(data)
    _git(tmp_path, "add", ".")
    result = basename_references(str(tmp_path), ["a.dat", "b.pdf", "unused.pdf"])
    assert set(result["a.dat"]) == {"one.txt", "two.txt"}
    assert result["b.pdf"] == ["one.txt"]
    assert result["unused.pdf"] == []


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
             "docs/run.py": 'p = f"docs/{name}.sim"\n',
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
    assert out_json.stat().st_size < 1_000_000
    assert "paths" not in summary["needs_human_check"]
    assert all("human_check_reasons" not in row for row in rows)
    detail = json.loads((tmp_path / summary["hold_detail_file"]).read_text())
    for row in rows:
        ids = json.loads(row["hold_consumers"])
        assert bool(ids) == (row["needs_human_check"] == "True")
        assert all(row["path"] in detail["candidates_by_consumer"][key] for key in ids)
    assert _git(Path.cwd(), "rev-parse", "HEAD") == before


@pytest.mark.parametrize("summary_name,custom_detail", [("out.json", False), ("summary", False), ("out.json", True)])
def test_empty_candidate_manifest_has_headers(tmp_path, summary_name, custom_detail):
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
    args = [str(repo), str(tmp_path / "previous.txt"), str(tmp_path / "out.csv"),
            str(tmp_path / summary_name), "--blob-map", str(tmp_path / "map.csv")]
    if custom_detail:
        args += ["--hold-detail", str(repo / "detail.json")]
    ac.main(args)
    summary = json.loads((tmp_path / summary_name).read_text())
    detail = json.loads((tmp_path / summary["hold_detail_file"]).read_text())
    assert detail["candidates_by_consumer"] == {}
    assert list(csv.DictReader((tmp_path / "out.csv").open())) == []
    assert "path" in (tmp_path / "map.csv").read_text()


@pytest.mark.parametrize("expression", ['unknown.glob("*.png")', 'root / f"{name}.png"'])
def test_dynamic_hold_stays_in_consumer_directory(expression):
    from archive_reference_safety import reference_safety
    candidates = ["examples/a/x.png", "examples/a/nested/x.png", "docs/b/x.png", "examples/ab/x.png"]
    texts = {"examples/a/run.py": "p = " + expression + "\n"}
    _, held, _ = reference_safety(texts, candidates, candidates + list(texts))
    assert set(held) == set(candidates[:2])


def test_compact_holds_store_evidence_once_per_consumer():
    from archive_reference_safety import compact_holds
    evidence = {"source": "a/run.py", "matched": "*.png", "reason": "runtime path or unresolved glob"}
    held = {"a/x.png": [evidence, evidence], "a/y.png": [evidence]}
    consumers, candidate_ids, detail = compact_holds(held)
    assert len(consumers) == 1
    consumer_id = next(iter(consumers))
    assert consumers[consumer_id]["candidate_count"] == 2
    assert consumers[consumer_id]["evidence"] == [{"pattern": "*.png", "reason": evidence["reason"], "candidate_count": 2}]
    assert candidate_ids == {"a/x.png": [consumer_id], "a/y.png": [consumer_id]}
    assert detail[consumer_id] == ["a/x.png", "a/y.png"]


def test_root_consumer_can_hold_root_tree():
    from archive_reference_safety import reference_safety
    candidates = ["a/x.png", "b/x.png"]
    _, held, _ = reference_safety({"run.py": 'p = root.glob("*.png")\n'}, candidates, candidates + ["run.py"])
    assert set(held) == set(candidates)


@pytest.mark.parametrize("text", ['p = f"docs/{name}.png"\n', 'p = "docs/*.png"\n[broken'])
def test_dynamic_repo_prefix_does_not_escape_consumer_tree(text):
    from archive_reference_safety import reference_safety
    candidates = ["docs/x.png", "examples/x.png"]
    _, held, _ = reference_safety({"examples/run.py": text}, candidates, candidates + ["examples/run.py"])
    assert "docs/x.png" not in held


def test_colliding_outputs_are_rejected_before_writes(tmp_path):
    summary = tmp_path / "out.json"
    summary.write_text("preserve")
    with pytest.raises(ValueError, match="distinct"):
        ac.main([str(tmp_path), "unused", str(tmp_path / "out.csv"), str(summary), "--hold-detail", str(summary)])
    assert summary.read_text() == "preserve"
