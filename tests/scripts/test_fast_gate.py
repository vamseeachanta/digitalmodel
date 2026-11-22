"""Selector, verdict and OWNERS.toml helpers of the advisory fast gate (shadow)."""

import json
from pathlib import Path
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/ci"))

import detect_touched_domains as detector  # noqa: E402
import fast_gate  # noqa: E402
import generate_owners  # noqa: E402

FIXTURES = Path(__file__).parent / "fixtures" / "fast_gate"
OWNERS = fast_gate.load_owners((FIXTURES / "OWNERS.toml").read_text(encoding="utf-8"))
CASES = json.loads((FIXTURES / "selection_cases.json").read_text(encoding="utf-8"))["cases"]
DOMAINS = [
    detector.Domain("citations", ("tests/citations/",)),
    detector.Domain("contracts", ("tests/docs/",)),
    detector.Domain("marine-engineering", ("tests/marine_ops/fowt/",)),
    detector.Domain("subsea", ("tests/subsea/",)),
]


@pytest.mark.parametrize("case", CASES, ids=[case["id"] for case in CASES])
def test_changed_paths_select_the_expected_test_files(case, tmp_path):
    # tmp_path holds no files: a test path outside OWNERS.toml reads as deleted.
    result = fast_gate.select(case["changed"], OWNERS, DOMAINS, root=tmp_path)
    assert result["selected"] == case["selected"]
    assert result["shards"] == case["shards"]
    assert result["fallback"] == case["fallback"]
    if "extras" in case:
        assert result["extras"] == case["extras"]
    assert sorted(result["reasons"]) == sorted(case["changed"]), "every changed path has a reason"


def test_new_test_file_outside_owners_falls_back_to_its_domain(tmp_path):
    new = tmp_path / "tests/subsea/test_new.py"
    new.parent.mkdir(parents=True)
    new.write_text("def test_new(): pass\n")
    result = fast_gate.select(["tests/subsea/test_new.py"], OWNERS, DOMAINS, root=tmp_path)
    assert result["selected"] == ["tests/subsea/riser/test_joint.py", "tests/subsea/test_catenary.py"]
    assert result["fallback"] == {"tests/subsea/test_new.py": ["subsea"]}


def test_package_documents_follow_the_detector(monkeypatch, tmp_path):
    monkeypatch.setattr(detector, "package_document_domains",
                        lambda path, domains: {"contracts"} if "/docs/" in path else None)
    result = fast_gate.select(["src/digitalmodel/citations/docs/agent-pack.md"], OWNERS, DOMAINS, root=tmp_path)
    assert result["selected"] == ["tests/docs/test_routing.py"]


def test_shard_needs_follow_the_selection(tmp_path):
    result = fast_gate.select(["uv.lock"], OWNERS, DOMAINS, root=tmp_path)
    assert result["node"] is True  # contracts runs the source-locator formatter
    assert result["gmsh"] is False
    assert fast_gate.select(["README.md"], OWNERS, DOMAINS, root=tmp_path)["node"] is False


def test_unsupported_owners_version_is_rejected():
    with pytest.raises(ValueError):
        fast_gate.load_owners("version = 2\n")


@pytest.mark.parametrize("code,count,verdict", [
    (None, 0, "pass"), (0, 3, "pass"), (5, 3, "pass"), (1, 3, "fail"), (2, 3, "fail"), (None, 3, "fail"),
])
def test_fast_verdict(code, count, verdict):
    assert fast_gate.verdict_of(code, count) == verdict


@pytest.mark.parametrize("result,has_domains,verdict", [
    ("success", "true", "pass"), ("failure", "true", "fail"), ("skipped", "false", "pass"),
    ("skipped", "true", "unknown"), ("cancelled", "true", "unknown"), ("", "", "unknown"),
])
def test_matrix_verdict(result, has_domains, verdict):
    assert fast_gate.matrix_verdict(result, has_domains) == verdict


@pytest.mark.parametrize("fast,matrix,agree", [
    ("pass", "pass", True), ("fail", "fail", True), ("pass", "fail", False), ("fail", "pass", False),
    ("unknown", "pass", None), ("not-run", "fail", None),
])
def test_agreement(fast, matrix, agree):
    assert fast_gate.agreement(fast, matrix) == {"pair": f"{fast}/{matrix}", "agree": agree}


def test_agree_writes_both_verdicts_for_the_head(tmp_path):
    selection = tmp_path / "selection.json"
    selection.write_text(json.dumps({"base": "b" * 40, "changed": ["src/digitalmodel/units.py"],
                                     "selected": ["tests/subsea/test_catenary.py"], "shards": ["subsea"],
                                     "fallback": {}, "owners_current": True}))
    result = tmp_path / "result.json"
    result.write_text(json.dumps({"verdict": "fail", "shards": ["subsea"], "duration_seconds": 12.5,
                                  "exit_code": 1, "junit": {"tests": 4, "failures": 1, "errors": 0, "skipped": 0}}))
    out, summary = tmp_path / "out/shadow.json", tmp_path / "summary.md"
    code = fast_gate.main([
        "agree", "--selection", str(selection), "--result", str(result), "--event", "pull_request",
        "--pull-request", "7", "--head-sha", "a" * 40, "--run-id", "11", "--run-attempt", "1",
        "--fast-result", "success", "--matrix-result", "failure", "--has-domains", "true",
        "--matrix", json.dumps({"include": [{"domain": "subsea", "runner": "ubuntu-latest"}]}),
        "--out", str(out), "--summary", str(summary),
    ])
    assert code == 0
    record = json.loads(out.read_text())
    assert record["head_sha"] == "a" * 40
    assert record["fast_gate"]["verdict"] == "fail"
    assert record["domain_matrix"] == {"job_result": "failure", "verdict": "fail", "shards": ["subsea"]}
    assert (record["pair"], record["agree"]) == ("fail/fail", True)
    assert "fail/fail" in summary.read_text()


def test_agree_without_a_fast_result_is_not_comparable(tmp_path):
    out = tmp_path / "shadow.json"
    fast_gate.main(["agree", "--result", str(tmp_path / "missing.json"), "--event", "pull_request",
                    "--matrix-result", "success", "--has-domains", "true", "--out", str(out)])
    record = json.loads(out.read_text())
    assert (record["pair"], record["agree"]) == ("unknown/pass", None)


def test_agree_on_a_push_records_no_fast_verdict(tmp_path):
    out = tmp_path / "shadow.json"
    fast_gate.main(["agree", "--event", "push", "--matrix-result", "success", "--has-domains", "true",
                    "--out", str(out)])
    assert json.loads(out.read_text())["agree"] is None


def test_run_with_nothing_selected_passes_without_pytest(tmp_path):
    selection = tmp_path / "selection.json"
    selection.write_text(json.dumps({"selected": [], "shards": []}))
    out = tmp_path / "result.json"
    assert fast_gate.main(["run", "--selection", str(selection), "--junit", str(tmp_path / "j.xml"),
                           "--out", str(out)]) == 0
    record = json.loads(out.read_text())
    assert (record["verdict"], record["exit_code"], record["selected_count"]) == ("pass", None, 0)


@pytest.mark.parametrize("path,unit", [
    ("src/digitalmodel/units.py", "src/digitalmodel/units.py"),
    ("src/digitalmodel/subsea/__init__.py", "src/digitalmodel/subsea/"),
    ("src/digitalmodel/subsea/catenary.py", "src/digitalmodel/subsea/"),
    ("src/digitalmodel/subsea/riser/joint.py", "src/digitalmodel/subsea/riser/"),
    ("src/digitalmodel/subsea/riser/legacy/a.py", "src/digitalmodel/subsea/riser/"),
    ("scripts/ci/fast_gate.py", None),
])
def test_unit_of(path, unit):
    assert generate_owners.unit_of(path) == unit


def test_folder_compression_round_trips():
    universe = {"tests/a/test_1.py", "tests/a/test_2.py", "tests/a/b/test_3.py", "tests/c/test_4.py"}
    for chosen in ({"tests/a/test_1.py"}, {"tests/a/test_1.py", "tests/a/test_2.py", "tests/a/b/test_3.py"},
                   universe, {"tests/a/b/test_3.py", "tests/c/test_4.py"}):
        entries = generate_owners.compress(chosen, universe)
        assert generate_owners.expand(entries, universe) == chosen
    assert generate_owners.compress(universe - {"tests/a/test_2.py"}, universe) == [
        "tests/a/b/", "tests/a/test_1.py", "tests/c/"]


def test_gate_extras():
    assert generate_owners.gate_extras(
        "uv run --no-sources --with-editable '.[test,fowt]' python -m pytest tests/x/") == ["fowt", "test"]
    assert generate_owners.gate_extras("uv run --extra test --extra cfd python -m pytest") == ["cfd", "test"]


def test_load_only_leaves_out_imports_inside_functions(tmp_path):
    source = tmp_path / "engine.py"
    source.write_text("import digitalmodel.units\n"
                      "try:\n    import digitalmodel.optional\nexcept ImportError:\n    pass\n"
                      "def run():\n    import digitalmodel.fatigue\n"
                      "class Router:\n    def route(self):\n        from digitalmodel import subsea\n")
    every, _ = generate_owners.imported_names(source, "digitalmodel")
    loaded, _ = generate_owners.imported_names(source, "digitalmodel", load_only=True)
    assert {"digitalmodel.units", "digitalmodel.optional", "digitalmodel.fatigue",
            "digitalmodel.subsea"} <= every
    assert loaded == {"digitalmodel.units", "digitalmodel.optional"}


def test_imports_resolve_relative_and_string_references(tmp_path):
    package = tmp_path / "src/digitalmodel"
    (package / "subsea/riser").mkdir(parents=True)
    for name in ("__init__.py", "units.py", "subsea/__init__.py", "subsea/riser/__init__.py",
                 "subsea/riser/joint.py"):
        (package / name).write_text("")
    (package / "subsea/catenary.py").write_text("from .riser import joint\nfrom ..units import METRE\n")
    modules = generate_owners.module_files(tmp_path)
    names, _ = generate_owners.imported_names(package / "subsea/catenary.py", "digitalmodel.subsea")
    resolved = {generate_owners.resolve(name, modules) for name in names}
    assert {"digitalmodel.subsea.riser.joint", "digitalmodel.units"} <= resolved
    found = generate_owners.string_references(
        {"python -m digitalmodel.subsea.catenary", "src/digitalmodel/subsea/riser/data/table.yml"},
        modules, tmp_path)
    assert found == {"digitalmodel.subsea.catenary", "unit:src/digitalmodel/subsea/riser/"}
