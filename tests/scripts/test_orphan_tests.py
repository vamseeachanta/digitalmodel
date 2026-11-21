"""File selection across gated, auxiliary and manual workflows."""

import importlib.util
import json
from pathlib import Path
import subprocess
import sys

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/orphan_tests.py"


@pytest.fixture
def selection_tree(tmp_path):
    root = tmp_path / "repo"
    for path in ("tests/test_gate.py", "tests/other_test.py", "tests/test_missed.py",
                 "tests/excluded/test_hidden.py"):
        target = root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("def test_example(): pass\n")
    (root / "pytest.ini").write_text(
        "[pytest]\ntestpaths=tests\npython_files=test_*.py *_test.py\nnorecursedirs=excluded\n"
    )
    workflows = root / ".github/workflows"
    workflows.mkdir(parents=True)
    for name, event, command in [
        ("harness", "push", "python -m pytest tests/test_gate.py"),
        ("other", "pull_request", ">-\n          python -m pytest\n          tests/other_test.py"),
        ("release", "workflow_dispatch", 'pytest tests/ -m "not slow"'),
    ]:
        (workflows / f"{name}.yml").write_text(
            f"on: [{event}]\njobs:\n  check:\n    steps:\n      - run: {command}\n"
        )
    return root, tmp_path / "out"


def inventory_cli(root, out):
    result = subprocess.run([sys.executable, str(SCRIPT), "list", "--root", str(root),
                             "--output-dir", str(out)], capture_output=True, text=True)
    return result.returncode, json.loads((out / "list.json").read_text())


def test_file_selection_across_triggers(selection_tree):
    code, data = inventory_cli(*selection_tree)
    assert code == 1 and data["unknown"] == ["test root has no Git revision"]
    assert data["orphans"] == []
    assert data["automatic_orphans"] == ["tests/test_missed.py"]
    assert set(data["selected"]["tests/other_test.py"]) == {"other", "release"}
    assert data["functions"] == {"tests/test_missed.py": 1}
    assert "tests/excluded/test_hidden.py" not in data["selected"]


@pytest.mark.parametrize("operand", ['"$MORE"', '--ignore="$OMIT"'])
def test_unresolved_pytest_selector_fails_closed(selection_tree, operand):
    root, out = selection_tree
    (root / ".github/workflows/dynamic.yml").write_text(
        f"on: [push]\njobs:\n  test:\n    steps:\n      - run: pytest tests/test_gate.py {operand}\n"
    )
    code, data = inventory_cli(root, out)
    assert code != 0 and any("dynamic" in problem for problem in data["unknown"])
    assert data["automatic_orphans"] == ["tests/test_missed.py"]


def test_orphan_measurement_records_errors_skips_and_reconciles_nodes(tmp_path, monkeypatch):
    monkeypatch.syspath_prepend(str(Path(__file__).resolve().parents[2] / "scripts/ci"))
    import orphan_measure

    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.delenv(name, raising=False)
    root = tmp_path / "suite"
    root.mkdir()
    subprocess.run(["git", "init", "-q", str(root)], check=True)
    sources = {
        "test_live.py": (
            "import pytest\n"
            "def test_pass(): pass\n"
            "def test_fail(): assert False\n"
            "def test_skip(): pytest.skip('unavailable fixture')\n"
            "def test_error(missing_fixture): pass\n"
        ),
        "test_broken.py": "raise ImportError('missing collection fixture')\n",
        "test_empty.py": "",
        "test_module_skip.py": "import pytest\npytest.skip('offline', allow_module_level=True)\n",
    }
    for name, body in sources.items():
        (root / name).write_text(body)
    out = tmp_path / "result"
    out.mkdir()
    monkeypatch.setattr(orphan_measure, "_head", lambda root: "fixture")
    monkeypatch.setattr(orphan_measure, "tree_changes", lambda root: "")
    (out / "list.json").write_text(json.dumps({
        "base_commit": "fixture", "automatic_orphans": list(sources), "unknown": []
    }))
    assert orphan_measure.measure(root, out, "collect") == 1
    files = json.loads((out / "collect.json").read_text())["files"]
    assert len(files["test_live.py"]["nodes"]) == 4
    assert "missing collection fixture" in files["test_broken.py"]["error"]
    assert files["test_empty.py"]["status"] == "zero"
    assert "offline" in files["test_module_skip.py"]["reason"]
    assert orphan_measure.measure(root, out, "run") == 1
    report = json.loads((out / "run.json").read_text())
    row = report["files"]["test_live.py"]
    assert row["outcomes"] == {"pass": 1, "fail": 1, "error": 1, "skip": 1}
    assert report["skip_reasons"] == row["skip_reasons"] == {"unavailable fixture": 1}
    reason = files["test_module_skip.py"]["reason"]
    assert report["files"]["test_module_skip.py"]["reason"] == reason
    assert report["module_skip_reasons"] == {reason: 1}
    assert row["status"] == "complete" and (out / row["junit"]).exists()
    assert orphan_measure.measure(root, out, "run") == 1
    assert json.loads((out / "run.json").read_text())["files"] == report["files"]


def test_measurement_is_in_harness():
    root = SCRIPT.parents[2]
    text = (root / ".github/workflows/quality-gates-by-domain.yml").read_text()
    harness = text.split("- name: Run harness tests\n", 1)[1].split("- name:", 1)[0]
    assert Path(__file__).resolve().relative_to(root).as_posix() in harness.split()


@pytest.mark.parametrize("runner", ["make test", "python -m pip install pytest && pytest tests/",
                                    "python custom_check.py", "${{ matrix.command }}"])
def test_opaque_workflow_requires_review(selection_tree, runner):
    root, out = selection_tree
    (root / ".github/workflows/dynamic.yml").write_text(
        f"on: [push]\njobs:\n  test:\n    steps:\n      - run: {runner}\n"
    )
    code, data = inventory_cli(root, out)
    assert code != 0 and any("dynamic.yml" in problem for problem in data["unknown"])


def test_shard_commands_follow_gate_and_pr_extension(selection_tree):
    root, _ = selection_tree
    (root / "tests/DOMAINS.md").write_text("| alpha | `tests/test_missed.py` |\n")
    reader = root / "scripts/ci/detect_touched_domains.py"
    reader.parent.mkdir(parents=True)
    reader.write_bytes(SCRIPT.with_name(reader.name).read_bytes())
    (root / ".claude").mkdir()
    (root / ".claude/quality-gates.yaml").write_text(
        'gates:\n  tests-alpha:\n    command: "python -m pytest tests/test_gate.py"\n'
    )
    spec = importlib.util.spec_from_file_location("inventory", SCRIPT)
    tool = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(tool)
    base = 'command = gates[os.environ["GATE_NAME"]]["command"]'
    extension = '\nif os.environ["GATE_NAME"] == "tests-alpha":\n    command += " tests/other_test.py"'
    for text, suffix in [(base, ""), (base + extension, " tests/other_test.py")]:
        assert list(tool.domain_commands(root, text)) == [
            ("alpha", "python -m pytest tests/test_gate.py" + suffix)]


def test_timeout_preserves_finished_node_count(tmp_path, monkeypatch):
    monkeypatch.syspath_prepend(str(SCRIPT.parent))
    monkeypatch.setenv("PYTEST_DISABLE_PLUGIN_AUTOLOAD", "1")
    monkeypatch.delenv("PYTEST_ADDOPTS", raising=False)
    import orphan_measure

    (tmp_path / "test_wait.py").write_text(
        "import time\ndef test_done(): pass\ndef test_wait(): time.sleep(60)\n"
    )
    (tmp_path / "junit").mkdir()
    nodes = ["test_wait.py::test_done", "test_wait.py::test_wait"]
    row = orphan_measure._run_file(tmp_path, tmp_path, 0, nodes, 3)
    assert row["status"] == "timeout" and row["finished_nodes"] == 1
    assert "outcomes" not in row  # An unfinished JUnit document gives no verdict.
    monkeypatch.setattr(orphan_measure, "_pytest", lambda *args: (137, 0.001, []))
    row = orphan_measure._run_file(tmp_path, tmp_path, 1, nodes, 300)
    assert row["status"] == "unknown" and row["returncode"] == 137


def test_inconsistent_junit_is_excluded_from_verified_counts(tmp_path, monkeypatch):
    monkeypatch.syspath_prepend(str(SCRIPT.parent))
    import orphan_measure

    (tmp_path / "junit").mkdir()
    node = "test_live.py::test_pass"

    def inconsistent_result(root, stem, args, limit):
        stem.with_suffix(".xml").write_text('<testsuite><testcase name="test_pass"/></testsuite>')
        return 1, 0.1, [("finished", node, None)]

    monkeypatch.setattr(orphan_measure, "_pytest", inconsistent_result)
    row = orphan_measure._run_file(tmp_path, tmp_path, 0, [node], 300)
    assert row["status"] == "unknown"
    assert "outcomes" not in row


def test_base_guard_detects_new_files_with_inherited_git_bindings(tmp_path, monkeypatch):
    monkeypatch.syspath_prepend(str(SCRIPT.parent))
    import orphan_measure

    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.delenv(name, raising=False)
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    monkeypatch.setenv("GIT_DIR", str(tmp_path / "unrelated"))
    (tmp_path / "uv.lock").write_text("runtime resolution")
    assert not orphan_measure.tree_changes(tmp_path)
    (tmp_path / "test_stray.py").touch()
    assert "test_stray.py" in orphan_measure.tree_changes(tmp_path)


def test_invalid_pytest_config_records_unknown(selection_tree):
    root, out = selection_tree
    (root / "pytest.ini").write_text("[unfinished")
    code, data = inventory_cli(root, out)
    assert code != 0 and any("discovery" in reason for reason in data["unknown"])


def test_discovery_inside_hidden_checkout(selection_tree):
    root, out = selection_tree
    root = root.rename(root.with_name(".snapshot"))
    (root / "pytest.ini").write_text("[pytest]\ntestpaths=tests\nnorecursedirs=.* excluded\n")
    nested = root / "tests/module/test_nested.py"
    nested.parent.mkdir()
    nested.touch()
    _, data = inventory_cli(root, out)
    assert nested.relative_to(root).as_posix() in data["automatic_orphans"]


@pytest.mark.parametrize("source", ["config", "environment", "collect_ignore"])
def test_unresolved_collection_options(selection_tree, monkeypatch, source):
    root, out = selection_tree
    if source == "config":
        with (root / "pytest.ini").open("a") as handle:
            handle.write("addopts=--ignore=tests\n")
    elif source == "environment":
        monkeypatch.setenv("PYTEST_ADDOPTS", "-m slow")
    else:
        (root / "tests/conftest.py").write_text("collect_ignore = []\ncollect_ignore.append('test_gate.py')")
    _, data = inventory_cli(root, out)
    assert any("discovery" in item for item in data["unknown"])


@pytest.mark.parametrize("trigger, condition", [("on: [push]", "if: false"),
                                               ("on:\n  push:\n    tags: ['v*']", "")])
def test_unreviewed_triggers_keep_files_in_measurement(selection_tree, trigger, condition):
    root, out = selection_tree
    (root / ".github/workflows/conditional.yml").write_text(
        f"{trigger}\njobs:\n  check:\n    {condition}\n    steps:\n      - run: pytest tests/test_missed.py\n"
    )
    _, data = inventory_cli(root, out)
    assert any("conditional.yml" in item for item in data["unknown"])
    assert "tests/test_missed.py" in data["automatic_orphans"]


def test_plain_multiline_selector_does_not_expand_to_all(selection_tree):
    root, out = selection_tree
    (root / ".github/workflows/multiline.yml").write_text(
        "on: [push]\njobs:\n  check:\n    steps:\n      - run: pytest\n          tests/test_gate.py\n"
    )
    _, data = inventory_cli(root, out)
    assert data["automatic_orphans"] == ["tests/test_missed.py"]
