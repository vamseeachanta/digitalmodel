"""Exercise the spec hook against disposable Git indexes and real schemas."""

import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
HOOK = ROOT / "scripts/check_spec_validity.sh"
REAL_SPEC = ROOT / (
    "docs/domains/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml"
)


@pytest.fixture
def staged_repo(tmp_path):
    """Keep fixture commands independent of the caller's repository bindings."""
    env = os.environ.copy()
    for key in (
        "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
        "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
    ):
        env.pop(key, None)
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    return tmp_path, env


def git(staged_repo, *args):
    directory, env = staged_repo
    return subprocess.run(
        ["git", *args], cwd=directory, env=env, check=True,
        capture_output=True, text=True,
    )


def stage(staged_repo, name, content):
    directory, _ = staged_repo
    target = directory / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(content, encoding="utf-8")
    git(staged_repo, "add", "--", name)
    return target


def run_hook(staged_repo, **env_overrides):
    for tool in ("bash", "uv"):
        if shutil.which(tool) is None:
            pytest.skip(f"{tool} is required to exercise the shell hook")
    directory, env = staged_repo
    return subprocess.run(
        ["bash", str(HOOK)], cwd=directory, env={**env, **env_overrides},
        capture_output=True, text=True, timeout=60,
    )


def test_rejects_staged_invalid_water_depth(staged_repo):
    source = REAL_SPEC.read_text(encoding="utf-8").replace(
        "depth: 8 ", "depth: -10000 "
    )
    stage(staged_repo, "model/spec.yml", source)

    result = run_hook(staged_repo)

    assert result.returncode != 0
    assert "model/spec.yml:" in result.stderr
    line = next(
        i for i, text in enumerate(source.splitlines(), 1) if "depth: -10000" in text
    )
    assert f"model/spec.yml:{line}:" in result.stderr
    assert "environment.water.depth" in result.stderr
    assert "greater than 0" in result.stderr


def test_precommit_hook_is_registered():
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    hooks = [
        hook for repo in config["repos"] if repo["repo"] == "local"
        for hook in repo["hooks"] if hook["id"] == "orcaflex-spec-validate"
    ]
    assert len(hooks) == 1
    assert hooks[0]["entry"] == "scripts/check_spec_validity.sh"
    assert hooks[0]["language"] == "script"
    assert hooks[0]["pass_filenames"] is False
    assert "bash" in hooks[0]["description"]
    assert "uv" in hooks[0]["description"]
    assert "network" in hooks[0]["description"]
    assert os.access(HOOK, os.X_OK)
    pattern = re.compile(hooks[0]["files"])
    for name in ("spec.yml", "model/spec.yml", "model/test.spec.yml"):
        assert pattern.search(name)
    for name in ("notspec.yml", "model/master.yml", "spec.yaml", "spec.yml.bak"):
        assert not pattern.search(name)


def test_validates_real_spec_and_reports_count(staged_repo):
    source = REAL_SPEC.read_text(encoding="utf-8")
    stage(staged_repo, "model/spec.yml", source)
    stage(staged_repo, "another/model.spec.yml", source)

    result = run_hook(staged_repo)

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 2 specs validated"


@pytest.fixture
def alternative_specs():
    passing = {
        "name": "test", "moored_vessel": {"hull": {
            "loa": 200, "beam": 32, "draft": 12,
        }},
        "passing_ship": {"vessel_type": "tanker", "dimensions": {
            "loa": 250, "beam": 40, "draft": 14,
        }, "track": {"speed_knots": 5, "separation_m": 100}},
        "environment": {},
    }
    diffraction = {
        "vessel": {"name": "test", "geometry": {"mesh_file": "test.gdf"},
                   "inertia": {"mass": 1000, "centre_of_gravity": [0, 0, 0],
                               "radii_of_gyration": [1, 1, 1]}},
        "environment": {"water_depth": 100},
        "frequencies": {"input_type": "frequency", "values": [0.5]},
        "wave_headings": {"values": [0]},
    }
    jumper_dir = ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
    jumper = yaml.safe_load(next(jumper_dir.glob("*/spec.yml")).read_text())
    return {"passing": passing, "diffraction": diffraction, "jumper": jumper}


@pytest.mark.parametrize("kind", ["passing", "diffraction", "jumper"])
@pytest.mark.parametrize("valid", [True, False])
@pytest.mark.parametrize("in_domain", [True, False])
def test_dispatches_alternative_schemas(staged_repo, alternative_specs, kind, valid, in_domain):
    data = alternative_specs[kind]
    if not valid:
        if kind == "passing":
            data["passing_ship"]["track"]["speed_knots"] = -1
        elif kind == "diffraction":
            data["environment"]["water_depth"] = -1
        else:
            data["pipe"]["outer_diameter"] = -1
    domain = "orcawave" if kind == "diffraction" else "orcaflex"
    directory = f"docs/domains/{domain}" if in_domain else "examples"
    path = f"{directory}/test/spec.yml"
    stage(staged_repo, path, yaml.safe_dump(data))

    result = run_hook(staged_repo)

    assert result.returncode == (0 if valid else 1), result.stderr
    if valid:
        assert result.stdout.strip() == "OK: 1 specs validated"
    else:
        assert path + ":" in result.stderr
        reason = {
            "passing": "speed_knots must be positive",
            "diffraction": "water_depth must be positive",
            "jumper": "greater than 0",
        }[kind]
        assert reason in result.stderr
        assert "OK:" not in result.stdout


@pytest.mark.parametrize("source, line", [
    ("metadata: {}\nmetadata: {}\n", 2),
    ("metadata:\n  name: test\n  name: other\n", 3),
])
def test_rejects_duplicate_yaml_keys(staged_repo, source, line):
    stage(staged_repo, "model/spec.yml", source)

    result = run_hook(staged_repo)

    assert result.returncode == 1
    assert f"model/spec.yml:{line}:" in result.stderr
    assert "duplicate mapping key" in result.stderr
    assert "OK:" not in result.stdout


def test_no_specs_does_not_start_python_or_uv(staged_repo, tmp_path):
    stage(staged_repo, "model/master.yml", "invalid: [")
    tools = tmp_path / "tools"
    tools.mkdir()
    for name in ("bash", "git", "dirname", "mktemp", "rm"):
        executable = shutil.which(name)
        if executable is None:
            pytest.skip(f"{name} is required for the isolated PATH fixture")
        try:
            (tools / name).symlink_to(executable)
        except OSError as error:
            pytest.skip(f"symlink privilege unavailable: {error}")

    result = run_hook(staged_repo, PATH=str(tools))

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 0 specs validated"


@pytest.mark.parametrize("missing", ["bash", "uv"])
def test_hook_test_skips_missing_tools(staged_repo, monkeypatch, missing):
    which = shutil.which
    monkeypatch.setattr(shutil, "which", lambda name: None if name == missing else which(name))
    with pytest.raises(pytest.skip.Exception, match=missing):
        run_hook(staged_repo)


def test_symlink_fixture_skips_without_privilege(staged_repo, tmp_path, monkeypatch):
    def denied(*args, **kwargs):
        raise OSError("symlink privilege unavailable")

    monkeypatch.setattr(Path, "symlink_to", denied)
    with pytest.raises(pytest.skip.Exception, match="symlink privilege unavailable"):
        test_no_specs_does_not_start_python_or_uv(staged_repo, tmp_path)


def test_rejects_invalid_index_even_with_valid_working_copy(staged_repo):
    source = REAL_SPEC.read_text(encoding="utf-8")
    target = stage(staged_repo, "model/spec.yml", "metadata: {}\n")
    target.write_text(source, encoding="utf-8")

    result = run_hook(staged_repo)

    assert result.returncode == 1
    assert "model/spec.yml:" in result.stderr
    assert "Field required" in result.stderr


def test_valid_index_ignores_invalid_working_copy(staged_repo):
    target = stage(staged_repo, "model/spec.yml", REAL_SPEC.read_text())
    target.write_text("invalid: [", encoding="utf-8")

    result = run_hook(staged_repo)

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 1 specs validated"


@pytest.mark.parametrize("name", [
    "spec.yml", "a spaced/model.spec.yml", "a\ttab/spec.yml",
    "a\nnewline/spec.yml", "unicodé/spec.yml", "-leading/spec.yml",
])
def test_handles_spec_filename_boundaries(staged_repo, name):
    stage(staged_repo, name, REAL_SPEC.read_text())
    stage(staged_repo, "notspec.yml", "invalid: [")

    result = run_hook(staged_repo)

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 1 specs validated"


@pytest.mark.parametrize("source, reason", [
    ("", "expected a YAML mapping"),
    ("- not\n- a mapping\n", "expected a YAML mapping"),
    ("metadata:\n  name: [\n", "expected"),
    ("!!python/object:bad {}", "could not determine a constructor"),
    ("metadata: {}\n", "Field required"),
])
def test_invalid_yaml_fails_with_location(staged_repo, source, reason):
    stage(staged_repo, "model/spec.yml", source)

    result = run_hook(staged_repo)

    assert result.returncode == 1
    assert re.search(r"model/spec\.yml:\d+: ", result.stderr)
    assert reason in result.stderr
    assert "OK:" not in result.stdout


def commit_fixture(staged_repo):
    git(staged_repo, "-c", "user.name=Fixture", "-c", "user.email=fixture@example.com",
        "commit", "-qm", "fixture")


def test_validates_modified_spec(staged_repo):
    stage(staged_repo, "model/spec.yml", REAL_SPEC.read_text())
    commit_fixture(staged_repo)
    stage(staged_repo, "model/spec.yml", "invalid: [")

    result = run_hook(staged_repo)

    assert result.returncode == 1
    assert "model/spec.yml:" in result.stderr


def test_deleted_specs_are_ignored(staged_repo):
    stage(staged_repo, "model/spec.yml", "invalid: [")
    commit_fixture(staged_repo)
    git(staged_repo, "rm", "model/spec.yml")

    result = run_hook(staged_repo)

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 0 specs validated"


def test_renamed_spec_is_validated(staged_repo):
    stage(staged_repo, "old/spec.yml", "invalid: [")
    commit_fixture(staged_repo)
    git(staged_repo, "mv", "old/spec.yml", "old/renamed.spec.yml")

    result = run_hook(staged_repo)

    assert result.returncode == 1
    assert "old/renamed.spec.yml:" in result.stderr


def test_hook_works_in_linked_worktree(staged_repo, tmp_path):
    stage(staged_repo, "README", "fixture")
    commit_fixture(staged_repo)
    worktree = tmp_path / "linked"
    git(staged_repo, "worktree", "add", "--detach", str(worktree))
    linked_repo = worktree, staged_repo[1]
    stage(linked_repo, "model/spec.yml", REAL_SPEC.read_text())

    result = run_hook(linked_repo)

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "OK: 1 specs validated"


@pytest.mark.parametrize("valid", [True, False])
def test_precommit_executes_registered_hook(staged_repo, valid):
    pytest.importorskip("pre_commit")
    for tool in ("bash", "uv"):
        if shutil.which(tool) is None:
            pytest.skip(f"{tool} is required to execute the registered hook")
    directory, env = staged_repo
    env = {**env, "PYTHONPATH": str(ROOT / "src")}
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    hook = next(
        hook for repo in config["repos"] if repo["repo"] == "local"
        for hook in repo["hooks"] if hook["id"] == "orcaflex-spec-validate"
    )
    (directory / ".pre-commit-config.yaml").write_text(
        yaml.safe_dump({"repos": [{"repo": "local", "hooks": [hook]}]})
    )
    (directory / "scripts").mkdir()
    for name in ("check_spec_validity.sh", "check_spec_validity.py", "audit_all_specs.py"):
        shutil.copy2(ROOT / "scripts" / name, directory / "scripts" / name)
    stage(staged_repo, "model/spec.yml", REAL_SPEC.read_text() if valid else "invalid: [")

    result = subprocess.run(
        [sys.executable, "-m", "pre_commit", "run", "orcaflex-spec-validate"],
        cwd=directory, env=env, capture_output=True, text=True, timeout=60,
    )

    assert result.returncode == (0 if valid else 1), result.stdout + result.stderr
    assert ("Passed" if valid else "Failed") in result.stdout
    if not valid:
        assert "model/spec.yml:" in result.stdout
