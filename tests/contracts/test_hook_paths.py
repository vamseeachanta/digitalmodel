"""Security hooks retain their scanners and repository-local configuration."""

from copy import deepcopy
from pathlib import Path, PureWindowsPath
import tomllib

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]


def _config():
    return yaml.safe_load(
        (ROOT / ".pre-commit-config.yaml").read_text(encoding="utf-8")
    )


def _hooks():
    config = _config()
    return {hook["id"]: hook for repo in config["repos"] for hook in repo["hooks"]}


def _argument_stays_in_repository(argument, root):
    value = (
        argument.split("=", 1)[1]
        if argument.startswith("--") and "=" in argument
        else argument
    )
    # Gitleaks v8.21.2 README, Usage: --config/-c accepts a file path.
    if argument.startswith("-c") and argument != "-c":
        value = argument[2:].removeprefix("=")
    if not value:
        return False
    if value == argument and value.startswith("-"):
        return True
    return _path_stays_in_repository(value, root)


def _path_stays_in_repository(value, root):
    """Validate a literal path; resolve follows symlinks and directory junctions."""
    if not value:
        return False
    windows_path = PureWindowsPath(value)
    if value.startswith("..") or windows_path.drive or windows_path.root:
        return False
    path = Path(value.replace("\\", "/"))
    return not path.is_absolute() and (root / path).resolve().is_relative_to(
        root.resolve()
    )


def _gitleaks_config_path():
    args = _hooks()["gitleaks"].get("args", [])
    for index, argument in enumerate(args):
        if argument in {"--config", "-c"}:
            assert index + 1 < len(args), "Gitleaks configuration requires a path"
            return args[index + 1]
        if argument.startswith("--config="):
            return argument.split("=", 1)[1]
    pytest.fail("Gitleaks must name a repository-owned configuration")


def _assert_argument_paths(hooks, root):
    for identifier, hook in hooks.items():
        args = iter(hook.get("args", []))
        for argument in args:
            if argument in {"--config", "-c"}:
                value = next(args, None)
                assert value is not None, f"{identifier}: configuration requires a path"
                if (
                    argument == "-c"
                    and hook.get("entry") == "python"
                    and hook.get("language") == "system"
                ):
                    continue  # Only an explicit Python command takes inline code.
                contained = _path_stays_in_repository(value, root)
            else:
                contained = _argument_stays_in_repository(argument, root)
            assert contained, f"{identifier}: hook argument leaves the repository"


def test_all_hook_argument_paths_stay_in_repository():
    _assert_argument_paths(_hooks(), ROOT)


def _assert_security_hook_sources(config):
    # Pinned upstream .pre-commit-hooks.yaml files define these executables.
    expected = {
        "bandit": ("https://github.com/PyCQA/bandit", ["pbr>=2.0.0"]),
        "gitleaks": ("https://github.com/gitleaks/gitleaks", []),
    }
    found = []
    for repo in config["repos"]:
        for hook in repo["hooks"]:
            if hook["id"] not in expected:
                continue
            found.append(hook["id"])
            source, dependencies = expected[hook["id"]]
            assert repo["repo"] == source, "Security hooks must use upstream sources"
            assert not {"entry", "language", "language_version"}.intersection(
                hook
            ), "Security hooks must inherit upstream executables and environments"
            assert (
                hook.get("additional_dependencies", []) == dependencies
            ), "Security hook dependencies must be the reviewed package requirements"
    assert sorted(found) == sorted(expected), "Each scanner must occur exactly once"


def test_security_hooks_inherit_upstream_execution():
    _assert_security_hook_sources(_config())


@pytest.mark.parametrize("identifier", ["bandit", "gitleaks"])
@pytest.mark.parametrize(
    "field,value",
    [
        ("entry", "../scanner"),
        ("language", "system"),
        ("language_version", "../python"),
        ("additional_dependencies", ["../scanner"]),
        ("repo", "../scanner"),
        ("duplicate", True),
    ],
)
def test_security_hook_execution_regressions(identifier, field, value):
    """A local executable, dependency or duplicate must fail the same CI contract."""
    config = deepcopy(_config())
    repo = next(
        repo
        for repo in config["repos"]
        if any(hook["id"] == identifier for hook in repo["hooks"])
    )
    hook = next(hook for hook in repo["hooks"] if hook["id"] == identifier)
    if field == "repo":
        repo["repo"] = value
    elif field == "duplicate":
        repo["hooks"].append(deepcopy(hook))
    else:
        hook[field] = value
    with pytest.raises(AssertionError):
        _assert_security_hook_sources(config)


@pytest.mark.parametrize("identifier", ["bandit", "gitleaks"])
def test_security_config_short_option_is_a_path(identifier, tmp_path):
    """Unlike python -c, a scanner's -c argument names a configuration file."""
    with pytest.raises(AssertionError):
        _assert_argument_paths(
            {identifier: {"args": ["-c", "../config.toml"]}}, tmp_path
        )


def test_python_inline_program_is_not_a_path(tmp_path):
    _assert_argument_paths(
        {
            "inline": {
                "entry": "python",
                "language": "system",
                "args": ["-c", "..."],  # Valid Python, rejected as a path.
            }
        },
        tmp_path,
    )


def test_gitleaks_configuration_exists_and_extends_defaults():
    argument = _gitleaks_config_path()
    assert _path_stays_in_repository(
        argument, ROOT
    ), "Gitleaks configuration must be repository-local"
    path = ROOT / argument
    assert path.is_file(), "Gitleaks configuration must exist in the repository"
    config = tomllib.loads(path.read_text(encoding="utf-8"))
    assert config["extend"]["useDefault"] is True
    assert "path" not in config["extend"]
    assert "rules" not in config
    assert "allowlist" not in config


def test_security_hook_identifiers_regression_pin():
    """Regression pin: both scanners are present on the unchanged base."""
    assert {"bandit", "gitleaks"}.issubset(_hooks())


@pytest.mark.parametrize(
    ("argument", "expected"),
    [
        ("../config.toml", False),
        ("--config=../config.toml", False),
        ("-c../config.toml", False),
        ("-c=../config.toml", False),
        ("-c..\\config.toml", False),
        ("-cconfig.toml", True),
        ("--config=", False),
        ("--config=--/../../config.toml", False),
        ("-c-/../../config.toml", False),
        ("nested/../../config.toml", False),
        ("..\\config.toml", False),
        (str(Path(Path.cwd().anchor) / "external.toml"), False),
        (str(PureWindowsPath("Z:") / "external.toml"), False),
        ("src/", True),
        ("--config=.gitleaks.toml", True),
        ("--verbose", True),
    ],
)
def test_argument_containment_cases(argument, expected, tmp_path):
    """Derive containment from path components, independently of the guard.

    Parent traversal leaves the root; each nested escape has more parents than
    local directories. Absolute and drive-qualified paths select another root.
    Attached -c uses the same path grammar as --config (pinned Gitleaks Usage).
    Empty assignments name no file; relative paths stay under the supplied root.
    """
    assert _argument_stays_in_repository(argument, tmp_path) is expected


@pytest.mark.parametrize(
    "argument", ["config.toml", "-cconfig.toml", "--config=config.toml"]
)
def test_argument_resolved_escape(argument, tmp_path):
    root = tmp_path / "repository"
    root.mkdir()
    outside = tmp_path / "external.toml"
    outside.write_text("title = 'external'\n", encoding="utf-8", newline="\n")
    (root / "config.toml").symlink_to(outside)
    assert not _argument_stays_in_repository(argument, root)


@pytest.fixture
def escaping_config(request, tmp_path):
    root = tmp_path / "repository"
    root.mkdir()
    outside = tmp_path / "external.toml"
    outside.write_text("[extend]\nuseDefault = true\n", encoding="utf-8", newline="\n")
    if request.param == "traversal":
        (root / "--").mkdir()
        argument = "--/../../external.toml"
    elif request.param == "directory_symlink":
        (root / "--").symlink_to(tmp_path, target_is_directory=True)
        argument = "--/external.toml"
    else:
        argument = request.param
        (root / argument).symlink_to(outside)
    return root, argument


@pytest.mark.parametrize("option", ["--config", "-c"])
@pytest.mark.parametrize(
    "escaping_config",
    [
        "traversal",
        "directory_symlink",
        "--config.toml",
        "-cconfig.toml",
        "--config=config.toml",
    ],
    indirect=True,
)
def test_separate_config_value_rejects_escape(option, escaping_config):
    """A consumed configuration value is a literal path, even with a flag prefix."""
    root, argument = escaping_config
    with pytest.raises(AssertionError, match="repository"):
        _assert_argument_paths({"gitleaks": {"args": [option, argument]}}, root)


@pytest.mark.parametrize(
    "escaping_config",
    [
        "traversal",
        "directory_symlink",
        "--config.toml",
        "-cconfig.toml",
        "--config=config.toml",
    ],
    indirect=True,
)
def test_gitleaks_separate_config_value_rejects_escape(escaping_config, monkeypatch):
    """The configuration-content contract must also reject an external valid TOML."""
    root, argument = escaping_config
    monkeypatch.setitem(globals(), "ROOT", root)
    monkeypatch.setitem(
        globals(), "_hooks", lambda: {"gitleaks": {"args": ["--config", argument]}}
    )
    with pytest.raises(AssertionError, match="repository"):
        test_gitleaks_configuration_exists_and_extends_defaults()


@pytest.mark.parametrize("option", ["--config", "-c"])
@pytest.mark.parametrize(
    "argument",
    ["config.toml", "--config.toml", "-cconfig.toml", "--config=config.toml"],
)
def test_separate_config_value_accepts_local_path(
    option, argument, tmp_path, monkeypatch
):
    """Flag-shaped filenames remain literal when they stay inside the repository."""
    (tmp_path / argument).write_text(
        "[extend]\nuseDefault = true\n", encoding="utf-8", newline="\n"
    )
    hooks = {"gitleaks": {"args": [option, argument]}}
    _assert_argument_paths(hooks, tmp_path)
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    monkeypatch.setitem(globals(), "_hooks", lambda: hooks)
    test_gitleaks_configuration_exists_and_extends_defaults()


@pytest.mark.parametrize("option", ["--config", "-c"])
@pytest.mark.parametrize("values", [[], [""]])
def test_separate_config_value_requires_nonempty_path(option, values, tmp_path):
    with pytest.raises(AssertionError):
        _assert_argument_paths({"gitleaks": {"args": [option, *values]}}, tmp_path)
