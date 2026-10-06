"""Executed checkout provenance is tested without invoking engineering calculations."""
import importlib.util
import os
import subprocess
import sys
from pathlib import Path
from types import ModuleType

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/reporting"))
SPEC = importlib.util.spec_from_file_location(
    "portfolio_provenance_test", ROOT / "scripts/reporting/build_cp_review_portfolio.py",
)
assert SPEC and SPEC.loader
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)


def git(root: Path, *args: str) -> str:
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.check_output(["git", "-C", str(root), *args], env=env, text=True).strip()


@pytest.fixture
def checkout(tmp_path: Path) -> Path:
    git(tmp_path, "init", "-q")
    git(tmp_path, "config", "user.name", "Fixture")
    git(tmp_path, "config", "user.email", "fixture@example.invalid")
    (tmp_path / "src/digitalmodel").mkdir(parents=True)
    (tmp_path / "src/digitalmodel/__init__.py").write_text("# fixture\n")
    git(tmp_path, "add", ".")
    git(tmp_path, "commit", "-qm", "fixture")
    return tmp_path


def test_clean_revision_tracks_later_commit_and_ignores_git_bindings(checkout, monkeypatch):
    first = builder.checkout_revision(checkout)
    (checkout / "revision.txt").write_text("later revision\n")
    git(checkout, "add", ".")
    git(checkout, "commit", "-qm", "later")
    monkeypatch.setenv("GIT_DIR", str(checkout / "nonexistent"))
    assert builder.checkout_revision(checkout) == git(checkout, "rev-parse", "HEAD")
    assert builder.checkout_revision(checkout) != first
    assert len(first) == 40


@pytest.mark.parametrize("change", ["unstaged", "staged", "untracked"])
def test_dirty_checkout_rejects_direct_build_before_output(checkout, monkeypatch, change):
    target = checkout / ("untracked.py" if change == "untracked" else "src/digitalmodel/__init__.py")
    target.write_text("# altered executable source\n")
    if change == "staged":
        git(checkout, "add", ".")
    monkeypatch.setattr(builder, "ROOT", checkout)
    destination = checkout.parent / "candidate"
    with pytest.raises(ValueError, match="dirty"):
        builder.build_case(builder.CASES[0], destination, {})
    assert not destination.exists()


def test_non_git_and_nested_root_reject(tmp_path, checkout):
    with pytest.raises(ValueError, match="Git"):
        builder.checkout_revision(tmp_path / "absent")
    with pytest.raises(ValueError, match="root"):
        builder.checkout_revision(checkout / "src")


def test_dirty_cli_rejects_before_creating_candidate(checkout, monkeypatch):
    (checkout / "untracked.py").write_text("# altered\n")
    monkeypatch.setattr(builder, "ROOT", checkout)
    destination = checkout.parent / "cli-candidate"
    monkeypatch.setattr(sys, "argv", ["builder", "--output", str(destination)])
    with pytest.raises(ValueError, match="dirty"):
        builder.main()
    assert not destination.exists()


def test_receipt_records_observed_revision_without_calculation(checkout):
    revision = builder.checkout_revision(checkout)
    cfg = {"inputs": {"calculation_type": "fixture"},
           "results": {"status": {"result": "FAIL", "use_status": "pending"}}}
    receipt = builder.case_receipt(builder.CASES[0], cfg, [], source_revision=revision)
    assert receipt["source_revision"] == revision
    assert receipt["engineering_review"] == "pending"


def test_foreign_loaded_submodule_rejects(checkout, monkeypatch, tmp_path):
    foreign = ModuleType("digitalmodel.foreign")
    foreign.__file__ = str(tmp_path / "foreign.py")
    with pytest.raises(ValueError, match="import"):
        builder.validate_import_origins(checkout, {"digitalmodel.foreign": foreign})


def test_local_import_is_required_to_be_tracked(checkout, monkeypatch):
    local = ModuleType("digitalmodel.fixture")
    local.__file__ = str(checkout / "src/digitalmodel/__init__.py")
    builder.validate_import_origins(checkout, {"digitalmodel.fixture": local})
    local.__file__ = str(checkout / "src/digitalmodel/ignored.py")
    (checkout / "src/digitalmodel/ignored.py").write_text("# untracked\n")
    with pytest.raises(ValueError, match="tracked"):
        builder.validate_import_origins(checkout, {"digitalmodel.fixture": local})


def test_foreign_reporting_helper_rejects(checkout, tmp_path):
    helper = ModuleType("cp_review_document")
    helper.__file__ = str(tmp_path / "cp_review_document.py")
    with pytest.raises(ValueError, match="import"):
        builder.validate_import_origins(checkout, {"cp_review_document": helper})


def test_lazy_offshore_import_checked_before_direct_build(checkout, monkeypatch):
    loaded = {}
    offshore = "digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection"
    def fake_import(name):
        loaded[name] = ModuleType(name)
        loaded[name].__file__ = str(checkout.parent / "foreign" / (name + ".py"))
        return loaded[name]
    original_validator = builder.validate_import_origins
    def check_imports(root):
        # Initial empty registry passes; preload must populate it before execution.
        original_validator(root, {offshore: loaded[offshore]} if offshore in loaded else {})
    monkeypatch.setattr(builder, "ROOT", checkout)
    monkeypatch.setattr(builder, "implementation_loaded", lambda: False)
    monkeypatch.setattr(builder, "import_module", fake_import)
    monkeypatch.setattr(builder, "validate_import_origins", check_imports)
    destination = checkout.parent / "shadowed-candidate"
    with pytest.raises(ValueError, match="import"):
        builder.build_case(builder.CASES[4], destination, {})
    assert offshore in loaded
    assert not destination.exists()


def test_direct_build_refuses_preloaded_implementation(checkout, monkeypatch):
    monkeypatch.setattr(builder, "ROOT", checkout)
    monkeypatch.setattr(builder, "implementation_loaded", lambda: True)
    with pytest.raises(ValueError, match="preloaded"):
        builder.build_case(builder.CASES[0], checkout.parent / "preloaded", {})


def test_commit_change_after_preload_rejected_before_cli_output(checkout, monkeypatch):
    monkeypatch.setattr(builder, "ROOT", checkout)
    monkeypatch.setattr(builder, "implementation_loaded", lambda: False)
    monkeypatch.setattr(builder, "validate_import_origins", lambda root: None)
    def change_revision(root):
        (root / "new.txt").write_text("new commit\n")
        git(root, "add", ".")
        git(root, "commit", "-qm", "changed after import")
    monkeypatch.setattr(builder, "prepare_execution_source", change_revision)
    destination = checkout.parent / "changed-candidate"
    monkeypatch.setattr(sys, "argv", ["builder", "--output", str(destination)])
    with pytest.raises(ValueError, match="revision changed"):
        builder.main()
    assert not destination.exists()


@pytest.mark.parametrize("flag", ["--assume-unchanged", "--skip-worktree"])
def test_hidden_modified_import_rejected(checkout, flag):
    relative = "src/digitalmodel/__init__.py"
    git(checkout, "update-index", flag, relative)
    (checkout / relative).write_text("# hidden modification\n")
    assert not git(checkout, "status", "--porcelain")
    with pytest.raises(ValueError, match="hidden index"):
        builder.checkout_revision(checkout)
    local = ModuleType("digitalmodel.fixture")
    local.__file__ = str(checkout / relative)
    with pytest.raises(ValueError, match="bytes"):
        builder.validate_import_origins(checkout, {"digitalmodel.fixture": local})


def test_later_case_cannot_use_a_new_clean_revision(checkout, monkeypatch):
    first = builder.checkout_revision(checkout)
    (checkout / "later.txt").write_text("later\n")
    git(checkout, "add", ".")
    git(checkout, "commit", "-qm", "later")
    monkeypatch.setattr(builder, "ROOT", checkout)
    destination = checkout.parent / "mixed-revision"
    with pytest.raises(ValueError, match="revision changed"):
        builder.build_case(builder.CASES[1], destination, {}, source_revision=first)
    assert not destination.exists()


def test_fresh_process_can_preload_routes_without_running_them():
    code = (
        "import sys; from pathlib import Path; "
        f"sys.path[:0]=[{str(ROOT / 'src')!r}, {str(ROOT / 'scripts/reporting')!r}]; "
        "import build_cp_review_portfolio as b; "
        "assert not b.implementation_loaded(); b.prepare_execution_source(b.ROOT)"
    )
    result = subprocess.run([sys.executable, "-B", "-c", code], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr


def test_last_case_render_import_rejected_before_success(checkout, monkeypatch, capsys):
    loaded = {}
    original_validator = builder.validate_import_origins
    monkeypatch.setattr(builder, "ROOT", checkout)
    monkeypatch.setattr(builder, "implementation_loaded", lambda: False)
    monkeypatch.setattr(builder, "prepare_execution_source", lambda root: None)
    monkeypatch.setattr(builder, "validate_import_origins", lambda root: original_validator(root, loaded))
    def fake_build(case, destination, register, *, source_revision):
        if case[0] == "S11":
            foreign = ModuleType("digitalmodel.reporting.foreign")
            foreign.__file__ = str(checkout.parent / "foreign.py")
            loaded[foreign.__name__] = foreign
        return {"id": case[0], "title": case[1], "pack_state": "blocked"}
    monkeypatch.setattr(builder, "build_case", fake_build)
    destination = checkout.parent / "final-shadowed"
    monkeypatch.setattr(sys, "argv", ["builder", "--output", str(destination)])
    with pytest.raises(ValueError, match="import"):
        builder.main()
    assert capsys.readouterr().out == ""
