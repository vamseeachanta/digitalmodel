# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Registration oracles use explicit set membership, independent of the audit."""

import glob
import importlib.util
import json
import os
import shutil
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
    (root / "tests/example/test_example.py").write_text(
        "def test_example(): pass\n", encoding="utf-8", newline="\n"
    )
    (root / "tests/DOMAINS.md").write_text(
        "| Domain | Test roots | Notes |\n| --- | --- | --- |\n"
        "| example | `tests/example/` | Example |\n",
        encoding="utf-8",
        newline="\n",
    )
    (root / "pytest.ini").write_text(
        "[pytest]\npython_files = test_*.py *_test.py\nnorecursedirs = .git\n",
        encoding="utf-8",
        newline="\n",
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
    (root / ".claude/quality-gates.yaml").write_text(
        yaml.safe_dump(config), encoding="utf-8", newline="\n"
    )
    return root


def test_new_test_directory_is_rejected(tmp_path):
    """Hand calculation: {new file} minus {owned files} contains the new file."""
    repo = fixture_repo(tmp_path)
    new = repo / "tests/new/test_new.py"
    new.parent.mkdir()
    new.write_text(
        "raise RuntimeError('must never be imported')\n", encoding="utf-8", newline="\n"
    )
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
            "python -m pytest --ignore-glob='tests/*/test_*.py' tests/example/",
            ["tests/example/"],
            "command-excluded",
        ),
        (
            "python -m pytest --ignore-glob='tests/exam?le' tests/",
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
    config = yaml.safe_load(path.read_text(encoding="utf-8"))
    config["gates"]["tests-example"]["enabled"] = False
    path.write_text(yaml.safe_dump(config), encoding="utf-8", newline="\n")
    assert "disabled-gate|example|tests/example/test_example.py" in load_guard().audit(
        repo
    )


def test_existing_directory_cannot_hide_new_undispatched_file(tmp_path):
    """A file-only command covers its file, leaving the new sibling uncovered."""
    repo = fixture_repo(
        tmp_path, command="python -m pytest tests/example/test_example.py"
    )
    (repo / "tests/example/test_sibling.py").write_text(
        "pass\n", encoding="utf-8", newline="\n"
    )
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
        with (repo / "pytest.ini").open("a", encoding="utf-8", newline="\n") as handle:
            handle.write("addopts = --ignore=tests/unused\n")
        (repo / "pytest.ini").write_text(
            (repo / "pytest.ini")
            .read_text(encoding="utf-8")
            .replace("norecursedirs = .git", "norecursedirs = example"),
            encoding="utf-8",
            newline="\n",
        )
    else:
        pattern = (
            "example/test_example.py" if source == "collect_ignore" else "*/test_*.py"
        )
        (repo / "tests/conftest.py").write_text(
            f"{source} = [{pattern!r}]\n", encoding="utf-8", newline="\n"
        )
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
        "-qkfixture",
        "-qmunit",
        "-vcother.ini",
        "-qoaddopts=--ignore=tests/example/",
        "-vpfixture_plugin",
        "-p fixture_plugin",
        "-p=fixture_plugin",
        "-p no:python",
        "-p no:unittest",
        "\r",
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
    (repo / "tests/example/test_sibling.py").write_text(
        "pass\n", encoding="utf-8", newline="\n"
    )
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
        "if True:\n    def pytest_ignore_collect(path): return True",
        "pytest_ignore_collect = lambda path: True",
        "from helper import pytest_ignore_collect",
        "from helper import callback as pytest_collect_file",
        "def pytest_pycollect_makemodule(path): return None",
        "def pytest_collect_directory(path): return None",
        "def pytest_make_collect_report(collector): return None",
        "def pytest_collection(session): return True",
        "from helper import *",
        "pytest_plugins = ['helper']",
        "import sys\nsys.modules[__name__].pytest_ignore_collect = lambda path: True",
        "def pytest_configure(config): config.option.ignore.append('example')",
        "collect_ignore = []\ncollect_ignore[:] = ['example/test_example.py']",
        "collect_ignore = []\ncollect_ignore[0:0] = ['example/test_example.py']",
    ],
)
def test_dynamic_collection_policy_fails_closed(tmp_path, declaration):
    repo = fixture_repo(tmp_path)
    (repo / "tests/conftest.py").write_text(
        declaration + "\n", encoding="utf-8", newline="\n"
    )
    with pytest.raises(ValueError):
        load_guard().audit(repo)


@pytest.mark.parametrize("pattern", ["exam*", "exam?le", "exam[pt]le"])
def test_glob_exclusion_of_a_parent_directory_is_accounted_for(tmp_path, pattern):
    """The matching ancestor removes the singleton even without a slash match."""
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    (repo / "tests/conftest.py").write_text(
        f"collect_ignore_glob = [{pattern!r}]\n", encoding="utf-8", newline="\n"
    )
    assert (
        "collection-excluded|example|tests/example/test_example.py"
        in load_guard().audit(repo)
    )


def test_scoped_directory_glob_excludes_descendant_files(tmp_path):
    """A nested directory exclusion removes its file within the conftest scope."""
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    (repo / "tests/example/conftest.py").write_text(
        "collect_ignore_glob = ['nest?d']\n", encoding="utf-8", newline="\n"
    )
    nested = repo / "tests/example/nested/test_nested.py"
    nested.parent.mkdir()
    nested.write_text("pass\n", encoding="utf-8", newline="\n")
    assert load_guard().audit(repo) == {
        "collection-excluded|example|tests/example/nested/test_nested.py"
    }


@pytest.mark.parametrize(
    ("root", "excluded"),
    [
        ("tests/", True),
        ("tests/example/", False),
        ("tests/exam*/", False),
        ("./tests/example/", False),
        ("tests/example/test_example.py", False),
        ("./tests/example/test_example.py", False),
        ("tests/example/test_*.py", False),
    ],
)
def test_explicit_roots_bypass_directory_glob_exclusions(tmp_path, root, excluded):
    """Native collection distinguishes explicit roots from recursive traversal."""
    repo = fixture_repo(tmp_path, command=f"python -m pytest {root}")
    (repo / "tests/conftest.py").write_text(
        "collect_ignore_glob = ['exam?le']\n", encoding="utf-8", newline="\n"
    )
    # Expand the unquoted argument as a shell would, preserving any ./ prefix.
    arguments = glob.glob(root, root_dir=repo)
    assert arguments
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "-p",
            "no:cacheprovider",
            *arguments,
        ],
        cwd=repo,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    assert result.returncode == (5 if excluded else 0), result.stdout + result.stderr
    node = "tests/example/test_example.py::test_example"
    assert (node in result.stdout) is not excluded
    expected = (
        {"collection-excluded|example|tests/example/test_example.py"}
        if excluded
        else set()
    )
    assert load_guard().audit(repo) == expected


def test_nonmatching_directory_glob_preserves_registration(tmp_path):
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    (repo / "tests/conftest.py").write_text(
        "collect_ignore_glob = ['exam?les']\n", encoding="utf-8", newline="\n"
    )
    assert load_guard().audit(repo) == set()


@pytest.mark.parametrize(
    "argument",
    [
        "tests/example/test_*.py",
        "'tests/example/'test_*.py",
        '"tests/example/"test_*.py',
        "tests/example/'test_'*.py",
        'tests/example/"test_"*.py',
    ],
)
def test_shell_globs_are_expanded_before_registration_checks(tmp_path, argument):
    """The command's glob expands to the sole existing matching file."""
    repo = fixture_repo(
        tmp_path, command=f"uv run --no-project python -m pytest {argument}"
    )
    assert load_guard().audit(repo) == set()


def shell_test_environment():
    """Keep inherited shell, Git and pytest settings out of the native oracle."""
    excluded_env = {
        "BASH_ENV",
        "ENV",
        "BASHOPTS",
        "SHELLOPTS",
        "GLOBIGNORE",
        "GIT_DIR",
        "GIT_WORK_TREE",
        "GIT_COMMON_DIR",
        "PYTEST_ADDOPTS",
        "PYTEST_PLUGINS",
    }
    env = {key: value for key, value in os.environ.items() if key not in excluded_env}
    env["PYTEST_DISABLE_PLUGIN_AUTOLOAD"] = "1"
    return env


def native_bash_collection(repo, argument):
    """Collect with checked shell defaults and only the fixture configuration."""
    bash = shutil.which("bash")
    if bash is None:
        pytest.skip("Native Bash is required for the shell comparison")
    return subprocess.run(
        [
            bash,
            "--noprofile",
            "--norc",
            "-c",
            "shopt -u dotglob globstar nullglob failglob extglob; "
            "for option in dotglob globstar nullglob failglob extglob; do "
            'if shopt -q "$option"; then exit 99; fi; done; '
            f'exec "$@" {argument}',
            "bash",
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-q",
            "-p",
            "no:cacheprovider",
            "--import-mode=importlib",
            "-c",
            str(repo / "pytest.ini"),
            "--rootdir",
            str(repo),
            "--confcutdir",
            str(repo),
        ],
        cwd=repo,
        env=shell_test_environment(),
        capture_output=True,
        text=True,
        encoding="utf-8",
    )


@pytest.mark.parametrize(
    "source", ["collect_ignore_glob", "collect_ignore", "norecursedirs"]
)
@pytest.mark.parametrize(
    ("argument", "collected"),
    [
        ("tests/", set()),
        ("tests/ tests/example/test_example.py", {"test_example.py"}),
        ("tests/ ./tests/example/test_example.py", {"test_example.py"}),
        (
            "tests/ tests/example/",
            {"test_example.py", "test_sibling.py"},
        ),
        (
            "tests/ tests/example/test_*.py",
            {"test_example.py", "test_sibling.py"},
        ),
    ],
)
def test_mixed_roots_bypass_exclusions_only_for_selected_files(
    tmp_path, source, argument, collected
):
    """An explicit file preserves its selection without admitting its sibling."""
    repo = fixture_repo(tmp_path, command=f"python -m pytest {argument}")
    files = {"test_example.py", "test_sibling.py"}
    (repo / "tests/example/test_sibling.py").write_text(
        "def test_sibling(): pass\n", encoding="utf-8", newline="\n"
    )
    if source == "norecursedirs":
        config = repo / "pytest.ini"
        config.write_text(
            config.read_text(encoding="utf-8").replace(
                "norecursedirs = .git", "norecursedirs = .git example"
            ),
            encoding="utf-8",
            newline="\n",
        )
    else:
        pattern = "exam?le" if source == "collect_ignore_glob" else "example"
        (repo / "tests/conftest.py").write_text(
            f"{source} = [{pattern!r}]\n", encoding="utf-8", newline="\n"
        )
    result = native_bash_collection(repo, argument)
    assert result.returncode == (0 if collected else 5), result.stdout + result.stderr
    assert {
        line.split("::")[0].removeprefix("tests/example/")
        for line in result.stdout.splitlines()
        if line.startswith("tests/example/") and "::" in line
    } == collected
    assert load_guard().audit(repo) == {
        f"collection-excluded|example|tests/example/{name}"
        for name in files - collected
    }


@pytest.mark.parametrize(
    ("argument", "expected"),
    [
        ("tests/example/**/test_*.py", {"nested/test_child.py"}),
        ("tests/example/*/test_*.py", {"nested/test_child.py"}),
        ("tests/example/**/**/test_*.py", {"nested/deeper/test_deep.py"}),
        ("tests/example/*_test.py", {"visible_test.py"}),
        (
            "tests/example/*/",
            {"nested/test_child.py", "nested/deeper/test_deep.py"},
        ),
        ("tests/example/test_example.py/", {"test_example.py"}),
        (
            "tests/example/*",
            {
                "test_example.py",
                "visible_test.py",
                "nested/test_child.py",
                "nested/deeper/test_deep.py",
            },
        ),
        ("tests/example/.hidden*", {".hidden/test_hidden.py", ".hidden_test.py"}),
        ("tests/example/.hidden/", {".hidden/test_hidden.py"}),
        ("tests/example/absent*.py", set()),
    ],
)
def test_registration_matches_default_bash_collection(tmp_path, argument, expected):
    """Recursive and hidden expansion must agree with native Bash and pytest."""
    repo = fixture_repo(tmp_path, command=f"python -m pytest {argument}")
    files = {
        "test_example.py",
        "visible_test.py",
        "nested/test_child.py",
        "nested/deeper/test_deep.py",
        ".hidden/test_hidden.py",
        ".hidden_test.py",
    }
    for name in files - {"test_example.py"}:
        path = repo / "tests/example" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("def test_fixture(): pass\n", encoding="utf-8", newline="\n")
    result = native_bash_collection(repo, argument)
    assert result.returncode == (0 if expected else 4), result.stdout + result.stderr
    assert {
        line.split("::")[0].removeprefix("tests/example/")
        for line in result.stdout.splitlines()
        if line.startswith("tests/example/") and "::" in line
    } == expected
    findings = load_guard().audit(repo)
    assert {
        name
        for name in files
        if f"missing-command|example|tests/example/{name}" not in findings
    } == expected
    assert (f"missing-root|example|{argument}" in findings) is (not expected)
    if not expected:
        assert "file or directory not found" in result.stderr


@pytest.mark.parametrize(
    "argument", ["tests/example/**/test_*.py", "tests/example/*_test.py"]
)
def test_shell_unmatched_roots_are_reported_missing(tmp_path, argument):
    """A deep or hidden match cannot rescue a root Bash leaves unexpanded."""
    repo = fixture_repo(tmp_path, command=f"python -m pytest {argument}")
    for name in ("nested/deeper/test_deep.py", ".hidden_test.py"):
        path = repo / "tests/example" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("def test_fixture(): pass\n", encoding="utf-8", newline="\n")
    assert f"missing-root|example|{argument}" in load_guard().audit(repo)


@pytest.mark.parametrize(
    "argument",
    [
        "tests/example/{test_example,missing}.py",
        "tests/example/test_[^x]*.py",
        "tests/example/test_[[:alpha:]]*.py",
        "tests/example/@(test_example).py",
    ],
)
def test_unmodelled_shell_patterns_fail_closed(tmp_path, argument):
    with pytest.raises(ValueError):
        load_guard().audit(
            fixture_repo(tmp_path, command=f"python -m pytest {argument}")
        )


DEPENDENCY_OPTION = (
    '--with "assetutilities @ git+https://github.com/'
    '${GITHUB_REPOSITORY%/*}/assetutilities.git@main"'
)


def test_recognized_dependency_prefix_preserves_registration(tmp_path):
    """The supported dependency expression does not alter pytest selection."""
    command = f"uv run --no-sources {DEPENDENCY_OPTION} --with-editable '.[test]' "
    repo = fixture_repo(tmp_path, command=command + "python -m pytest tests/example/")
    assert load_guard().audit(repo) == set()


@pytest.mark.parametrize(
    "option",
    [
        DEPENDENCY_OPTION.replace('"', "'"),
        DEPENDENCY_OPTION.replace("${GITHUB_REPOSITORY%/*}", "$GITHUB_REPOSITORY"),
        DEPENDENCY_OPTION.replace("%/*", "%%/*"),
        DEPENDENCY_OPTION.replace("%/*", "#*/"),
        DEPENDENCY_OPTION.replace("$", "\\$"),
        DEPENDENCY_OPTION.replace('--with "', '--with x"'),
        DEPENDENCY_OPTION + "suffix",
        DEPENDENCY_OPTION + "$(echo invalid)",
        DEPENDENCY_OPTION + "`echo invalid`",
        DEPENDENCY_OPTION + "; echo invalid",
        DEPENDENCY_OPTION + "\n",
        DEPENDENCY_OPTION.replace("--with ", "--with\n"),
        DEPENDENCY_OPTION + " --with $(echo invalid)",
    ],
)
def test_dependency_exception_does_not_allow_shell_composition(tmp_path, option):
    command = f"uv run {option} python -m pytest tests/example/"
    with pytest.raises(ValueError):
        load_guard().audit(fixture_repo(tmp_path, command=command))


@pytest.mark.parametrize("suffix", [" -k fixture", " --pyargs", " --with package"])
def test_dependency_exception_preserves_pytest_validation(tmp_path, suffix):
    command = f"uv run {DEPENDENCY_OPTION} python -m pytest tests/example/{suffix}"
    with pytest.raises(ValueError):
        load_guard().audit(fixture_repo(tmp_path, command=command))


@pytest.mark.parametrize(
    "command",
    [
        "uv run --with {package,echo} python -m pytest tests/example/",
        "uv run --with tests/* python -m pytest tests/example/",
        "python -m pytest tests/example/ --tb {short,tests/other}",
    ],
)
def test_unquoted_expansion_outside_roots_fails_closed(tmp_path, command):
    with pytest.raises(ValueError):
        load_guard().audit(fixture_repo(tmp_path, command=command))


@pytest.mark.parametrize("extra", ["--tb tests/*", "--tb=tests/*", "-p tests/*"])
def test_unquoted_option_value_globs_fail_closed(tmp_path, extra):
    with pytest.raises(ValueError):
        load_guard().audit(
            fixture_repo(tmp_path, command=f"python -m pytest tests/example/ {extra}")
        )


@pytest.mark.parametrize(
    "extra",
    [
        "-q",
        "-vv",
        "-rfE",
        "-p no:cacheprovider",
        "-p=no:cacheprovider",
        "-p no:randomly",
        "-p no:sugar",
    ],
)
def test_supported_reporting_options_preserve_registration(tmp_path, extra):
    command = f"python -m pytest tests/example/ {extra}"
    assert load_guard().audit(fixture_repo(tmp_path, command=command)) == set()


def test_missing_recursion_policy_requires_explicit_configuration(tmp_path):
    repo = fixture_repo(tmp_path)
    path = repo / "pytest.ini"
    path.write_text(
        path.read_text(encoding="utf-8").replace("norecursedirs = .git\n", ""),
        encoding="utf-8",
        newline="\n",
    )
    with pytest.raises(ValueError):
        load_guard().audit(repo)


def test_literal_marker_registration_is_supported(tmp_path):
    repo = fixture_repo(tmp_path)
    (repo / "tests/conftest.py").write_text(
        "def pytest_configure(config):\n"
        "    config.addinivalue_line('markers', 'fixture: a fixture marker')\n",
        encoding="utf-8",
        newline="\n",
    )
    assert load_guard().audit(repo) == set()


@pytest.mark.parametrize("pattern", ["example/nested", "tests/example/nested"])
def test_recursion_path_patterns_match_suffixes(tmp_path, pattern):
    repo = fixture_repo(tmp_path, command="python -m pytest tests/")
    path = repo / "pytest.ini"
    path.write_text(
        path.read_text(encoding="utf-8").replace(
            "norecursedirs = .git", f"norecursedirs = {pattern}"
        ),
        encoding="utf-8",
        newline="\n",
    )
    nested = repo / "tests/example/nested/test_nested.py"
    nested.parent.mkdir()
    nested.write_text("def test_nested(): pass\n", encoding="utf-8", newline="\n")
    native = native_bash_collection(repo, "tests/")
    assert native.returncode == 0, native.stdout + native.stderr
    assert "test_nested.py::" not in native.stdout
    assert (
        "collection-excluded|example|tests/example/nested/test_nested.py"
        in load_guard().audit(repo)
    )


def test_ini_ignore_globs_are_literal_arguments(tmp_path):
    repo = fixture_repo(tmp_path)
    with (repo / "pytest.ini").open("a", encoding="utf-8", newline="\n") as handle:
        handle.write("addopts = --ignore-glob=tests/example/test_*.py\n")
    assert (
        "collection-excluded|example|tests/example/test_example.py"
        in load_guard().audit(repo)
    )


def test_missing_harness_step_has_an_audit_diagnostic(tmp_path):
    repo = fixture_repo(tmp_path)
    workflow = repo / ".github/workflows/quality-gates-by-domain.yml"
    workflow.parent.mkdir(parents=True)
    workflow.write_text(
        yaml.safe_dump({"jobs": {"ci-harness-tests": {"steps": []}}}),
        encoding="utf-8",
        newline="\n",
    )
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(repo)],
        env=shell_test_environment(),
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    assert result.returncode == 1
    assert "Test registration audit failed:" in result.stderr
    assert "Traceback" not in result.stderr


@pytest.mark.parametrize(
    "argument",
    [
        "'tests/example/test_*.py'",
        '"tests/example/test_*.py"',
        r"tests/example/test_\*.py",
        "tests/example/'test_*.py'",
        'tests/example/"test_*.py"',
        "tests/exa'm?le'/test_example.py",
        'tests/exa"m?le"/test_example.py',
        "tests/example/test_'[e]'xample.py",
        'tests/example/test_"[e]"xample.py',
        "'tests/'example/'test_*.py'",
        '"tests/"example/"test_*.py"',
    ],
)
def test_quoted_or_escaped_globs_are_not_shell_expanded(tmp_path, argument):
    """Quoted wildcard characters remain literal rather than matching the file."""
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
        ),
        encoding="utf-8",
        newline="\n",
    )
    guard = load_guard()
    monkeypatch.setattr(
        guard.detector, "NO_DOMAIN_PATHS", ("tests/example/test_example.py",)
    )
    assert guard.audit(repo) == set()


def test_guard_and_its_regressions_run_in_the_required_independent_harness():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(
            encoding="utf-8"
        )
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
        (ROOT / "config/ci/test-registration-exceptions.json").read_text(
            encoding="utf-8"
        )
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
        encoding="utf-8",
    )
    assert result.returncode == 0, result.stderr
    (repo / "tests/example/test_sibling.py").write_text(
        "pass\n", encoding="utf-8", newline="\n"
    )
    config = repo / ".claude/quality-gates.yaml"
    data = yaml.safe_load(config.read_text(encoding="utf-8"))
    data["gates"]["tests-example"]["command"] = (
        "python -m pytest tests/example/test_example.py"
    )
    config.write_text(yaml.safe_dump(data), encoding="utf-8", newline="\n")
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(repo)],
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    assert result.returncode == 1
    assert "missing-command|example|tests/example/test_sibling.py" in result.stdout
