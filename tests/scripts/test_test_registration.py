# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Registration oracles use explicit set membership, independent of the audit."""

import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/check_test_registration.py"


def load_guard():
    spec = importlib.util.spec_from_file_location("registration_guard", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def fixture_repo(root, *, command="python -m pytest tests/example/", roots=None):
    """One discovered file belongs to one documented and executed root."""
    (root / "tests/example").mkdir(parents=True)
    (root / "tests/example/test_example.py").write_text("def test_example(): pass\n")
    (root / "tests/DOMAINS.md").write_text(
        "| Domain | Test roots | Notes |\n| --- | --- | --- |\n"
        "| example | `tests/example/` | Example |\n"
    )
    (root / "pytest.ini").write_text(
        "[pytest]\npython_files = test_*.py *_test.py\nnorecursedirs = .git\n"
    )
    (root / ".claude").mkdir()
    config = {
        "gates": {
            "tests-example": {
                "enabled": True,
                "domain": "example",
                "failure_action": "block",
                "test_roots": roots if roots is not None else ["tests/example/"],
                "command": command,
            }
        }
    }
    (root / ".claude/quality-gates.yaml").write_text(yaml.safe_dump(config))
    return root


def test_new_test_directory_is_rejected(tmp_path):
    """Hand calculation: {new file} minus {owned files} contains the new file."""
    repo = fixture_repo(tmp_path)
    new = repo / "tests/new/test_new.py"
    new.parent.mkdir()
    new.write_text("raise RuntimeError('must never be imported')\n")
    findings = load_guard().audit(repo)
    assert "undocumented|-|tests/new/test_new.py" in findings


def test_consistent_configuration_passes(tmp_path):
    """Hand calculation: the discovered singleton belongs to all three sets."""
    assert load_guard().audit(fixture_repo(tmp_path)) == set()


@pytest.mark.parametrize(
    ("command", "roots", "expected"),
    [
        ("python -m pytest tests/other/", ["tests/example/"], "missing-command"),
        ("python -m pytest tests/example/", ["tests/other/"], "missing-metadata"),
        (
            "python -m pytest --ignore tests/example/ tests/other/",
            ["tests/example/"],
            "missing-command",
        ),
        (
            "python -m pytest --ignore=tests/example tests/example/",
            ["tests/example/"],
            "command-excluded",
        ),
        (
            "python -m pytest --ignore-glob=tests/*/test_*.py tests/example/",
            ["tests/example/"],
            "command-excluded",
        ),
    ],
)
def test_metadata_is_not_execution(tmp_path, command, roots, expected):
    """Membership is computed from positional roots minus ignore arguments."""
    repo = fixture_repo(tmp_path, command=command, roots=roots)
    findings = load_guard().audit(repo)
    assert f"{expected}|example|tests/example/test_example.py" in findings


def test_disabled_gate_does_not_establish_execution(tmp_path):
    repo = fixture_repo(tmp_path)
    path = repo / ".claude/quality-gates.yaml"
    config = yaml.safe_load(path.read_text())
    config["gates"]["tests-example"]["enabled"] = False
    path.write_text(yaml.safe_dump(config))
    assert "disabled-gate|example|tests/example/test_example.py" in load_guard().audit(
        repo
    )


def test_existing_directory_cannot_hide_new_undispatched_file(tmp_path):
    """A file-only command covers its file, leaving the new sibling uncovered."""
    repo = fixture_repo(
        tmp_path, command="python -m pytest tests/example/test_example.py"
    )
    (repo / "tests/example/test_sibling.py").write_text("pass\n")
    assert (
        "missing-command|example|tests/example/test_sibling.py"
        in load_guard().audit(repo)
    )


@pytest.mark.parametrize(
    "source", ["collect_ignore", "collect_ignore_glob", "norecursedirs"]
)
def test_collection_exclusions_require_explicit_accounting(tmp_path, source):
    """An owned and commanded singleton removed by collection is still absent."""
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    if source == "norecursedirs":
        with (repo / "pytest.ini").open("a") as handle:
            handle.write("addopts = --ignore=tests/unused\n")
        (repo / "pytest.ini").write_text(
            (repo / "pytest.ini")
            .read_text()
            .replace("norecursedirs = .git", "norecursedirs = example")
        )
    else:
        pattern = (
            "example/test_example.py" if source == "collect_ignore" else "*/test_*.py"
        )
        (repo / "tests/conftest.py").write_text(f"{source} = [{pattern!r}]\n")
    assert (
        "collection-excluded|example|tests/example/test_example.py"
        in load_guard().audit(repo)
    )


def test_touched_routing_must_select_the_executing_domain(tmp_path, monkeypatch):
    repo = fixture_repo(tmp_path)
    guard = load_guard()
    monkeypatch.setattr(guard.detector, "touched_domains", lambda *args: [])
    assert "not-selected|example|tests/example/test_example.py" in guard.audit(repo)


def test_nonexistent_root_is_reported_even_without_a_discovered_file(tmp_path):
    repo = fixture_repo(tmp_path, roots=["tests/example/", "tests/missing/"])
    assert "missing-root|example|tests/missing/" in load_guard().audit(repo)


@pytest.mark.parametrize(
    "extra",
    [
        "-k example",
        "-m unit",
        "--pyargs",
        "tests/example/test_example.py::test_example",
        "; echo pytest",
        "--override-ini testpaths=tests/other",
    ],
)
def test_unmodelled_selection_fails_closed(tmp_path, extra):
    repo = fixture_repo(tmp_path, command=f"python -m pytest tests/example/ {extra}")
    with pytest.raises(ValueError):
        load_guard().audit(repo)


def test_exact_exceptions_do_not_cover_future_files(tmp_path):
    """Set difference: {old, new} minus {old} leaves precisely {new}."""
    repo = fixture_repo(tmp_path, command="python -m pytest tests/missing/")
    guard = load_guard()
    old = guard.audit(repo)
    exceptions = {
        finding: "Existing command gap; reconciliation remains open." for finding in old
    }
    assert guard.reconcile(old, exceptions) == set()
    (repo / "tests/example/test_sibling.py").write_text("pass\n")
    assert guard.reconcile(guard.audit(repo), exceptions) == {
        "missing-command|example|tests/example/test_sibling.py"
    }


@pytest.mark.parametrize(
    "exceptions",
    [
        {"missing-command|example|tests/example/test_example.py": ""},
        {"missing-command|example|tests/removed/test_removed.py": "Resolved gap"},
        {"missing-command|example|tests/example/*.py": "Too broad"},
    ],
)
def test_invalid_or_stale_exceptions_fail(tmp_path, exceptions):
    repo = fixture_repo(tmp_path)
    assert load_guard().reconcile(load_guard().audit(repo), exceptions)


@pytest.mark.parametrize(
    "command",
    [
        "echo -m pytest tests/example/",
        "false python -m pytest tests/example/",
        "uv run --help python -m pytest tests/example/",
    ],
)
def test_pytest_words_are_not_an_invocation(tmp_path, command):
    with pytest.raises(ValueError):
        load_guard().audit(fixture_repo(tmp_path, command=command))


@pytest.mark.parametrize(
    "declaration",
    [
        "collect_ignore = make_ignores()",
        "collect_ignore: list[str] = ['example/test_example.py']",
        "collect_ignore = []\ncollect_ignore += ['example/test_example.py']",
        "_tests_dir = Path('elsewhere')\ncollect_ignore = [str(_tests_dir / 'test_example.py')]",
        "def pytest_ignore_collect(path): return True",
    ],
)
def test_dynamic_collection_policy_fails_closed(tmp_path, declaration):
    repo = fixture_repo(tmp_path)
    (repo / "tests/conftest.py").write_text(declaration + "\n")
    with pytest.raises(ValueError):
        load_guard().audit(repo)


def test_glob_exclusion_of_a_parent_directory_is_accounted_for(tmp_path):
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    (repo / "tests/conftest.py").write_text("collect_ignore_glob = ['exam*']\n")
    assert (
        "collection-excluded|example|tests/example/test_example.py"
        in load_guard().audit(repo)
    )


def test_shell_globs_are_expanded_before_registration_checks(tmp_path):
    """The command's glob expands to the sole existing matching file."""
    repo = fixture_repo(
        tmp_path, command="uv run --no-project python -m pytest tests/example/test_*.py"
    )
    assert load_guard().audit(repo) == set()


@pytest.mark.parametrize(
    "argument", ["'tests/example/test_*.py'", r"tests/example/test_\*.py"]
)
def test_quoted_or_escaped_globs_are_not_shell_expanded(tmp_path, argument):
    """The shell passes a literal star, so pathname expansion cannot be assumed."""
    repo = fixture_repo(tmp_path, command=f"python -m pytest {argument}")
    with pytest.raises(ValueError):
        load_guard().audit(repo)


def test_independent_harness_is_a_valid_explicit_route(tmp_path, monkeypatch):
    repo = fixture_repo(tmp_path)
    workflow = repo / ".github/workflows/quality-gates-by-domain.yml"
    workflow.parent.mkdir(parents=True)
    workflow.write_text(
        yaml.safe_dump(
            {
                "jobs": {
                    "ci-harness-tests": {
                        "steps": [
                            {
                                "name": "Run harness tests",
                                "run": "python -m pytest tests/example/test_example.py",
                            }
                        ]
                    }
                }
            }
        )
    )
    guard = load_guard()
    monkeypatch.setattr(
        guard.detector, "NO_DOMAIN_PATHS", ("tests/example/test_example.py",)
    )
    assert guard.audit(repo) == set()


def test_guard_and_its_regressions_run_in_the_required_independent_harness():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text()
    )
    job = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in job
    step = next(
        item for item in job["steps"] if item["name"] == "Check test registration"
    )
    assert "if" not in step
    assert not step.get("continue-on-error", False)
    assert "scripts/ci/check_test_registration.py" in step["run"]
    regressions = next(
        item for item in job["steps"] if item["name"] == "Run harness tests"
    )
    assert "tests/scripts/test_test_registration.py" in regressions["run"]
    assert "ci-harness-tests" in workflow["jobs"]["domain-tests-required"]["needs"]


def test_repository_exceptions_are_exact_and_current():
    guard = load_guard()
    findings = guard.audit(ROOT)
    exceptions = json.loads(
        (ROOT / "config/ci/test-registration-exceptions.json").read_text()
    )
    assert guard.reconcile(findings, exceptions) == set()
    assert all(len(key.split("|")) == 3 for key in exceptions)
    assert all("*" not in key for key in exceptions)


def test_cli_has_useful_success_and_failure_status(tmp_path):
    repo = fixture_repo(tmp_path)
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(repo)],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    (repo / "tests/example/test_sibling.py").write_text("pass\n")
    config = repo / ".claude/quality-gates.yaml"
    data = yaml.safe_load(config.read_text())
    data["gates"]["tests-example"]["command"] = (
        "python -m pytest tests/example/test_example.py"
    )
    config.write_text(yaml.safe_dump(data))
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(repo)],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 1
    assert "missing-command|example|tests/example/test_sibling.py" in result.stdout
