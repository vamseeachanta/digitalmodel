"""Public-surface structural snapshot (#1961, Stage 1, D5).

The snapshot exists because the surface it describes -- exported names,
signatures with their default literals, and module-execution dispatch -- is the
part of the codebase internal tests do not exercise. A module split once carried
the ``__main__`` dispatch away with it and turned a documented module-execution
path into a silent no-op; 723 passing tests did not notice, and one namespace
comparison did.

Two empty snapshots diff clean, so the census assertions here are not
decoration: they are the anti-vacuity guard.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
SNAPSHOT = REPO_ROOT / "scripts" / "legal" / "public_surface_snapshot.py"

OPENFOAM_INIT = "src/digitalmodel/solvers/openfoam/__init__.py"
SWEEP_MODULE = "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py"


def _git(cwd: Path, *args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=cwd, check=True, capture_output=True, text=True
    ).stdout


def _snapshot(root: Path, ref: str, *extra: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(SNAPSHOT), "--root", str(root), "--ref", ref, *extra],
        capture_output=True,
        text=True,
    )


def _ok(root: Path, ref: str, *extra: str) -> str:
    proc = _snapshot(root, ref, *extra)
    assert proc.returncode == 0, proc.stderr
    return proc.stdout


@pytest.fixture()
def scratch(tmp_path: Path) -> Path:
    root = tmp_path / "repo"
    (root / "src" / "digitalmodel").mkdir(parents=True)
    _git(root, "init", "-q", "-b", "main")
    _git(root, "config", "user.email", "t@example.invalid")
    _git(root, "config", "user.name", "t")
    module = root / "src" / "digitalmodel" / "m.py"
    module.write_text(
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def build(count=3, label=None, *, strict=False):\n"
        "    return count\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n"
        "\n"
        "\n"
        'if __name__ == "__main__":\n'
        "    raise SystemExit(build())\n",
        encoding="utf-8",
    )
    _git(root, "add", "-A")
    _git(root, "commit", "-qm", "seed")
    return root


def _rewrite(root: Path, text: str) -> str:
    (root / "src" / "digitalmodel" / "m.py").write_text(text, encoding="utf-8")
    _git(root, "add", "-A")
    _git(root, "commit", "-qm", "change")
    return _git(root, "rev-parse", "HEAD").strip()


# --------------------------------------------------------------------------- #
# Determinism and blob-sourcing
# --------------------------------------------------------------------------- #


def test_snapshot_is_deterministic_across_two_runs(scratch: Path) -> None:
    assert _ok(scratch, "HEAD") == _ok(scratch, "HEAD")


def test_snapshot_is_built_from_git_blobs_not_the_working_tree(scratch: Path) -> None:
    """A symmetric comparison of the same bytes through the same reader proves nothing."""
    before = _ok(scratch, "HEAD")
    (scratch / "src" / "digitalmodel" / "m.py").write_text(
        "__all__ = []\n", encoding="utf-8"
    )
    assert _ok(scratch, "HEAD") == before


# --------------------------------------------------------------------------- #
# Anti-vacuity: two empty snapshots diff clean
# --------------------------------------------------------------------------- #


def test_snapshot_census_of_this_repository_is_non_empty() -> None:
    modules = json.loads(_ok(REPO_ROOT, "HEAD"))["modules"]
    assert modules != {}


def test_snapshot_names_the_openfoam_solver_package() -> None:
    modules = json.loads(_ok(REPO_ROOT, "HEAD"))["modules"]
    assert OPENFOAM_INIT in modules


def test_snapshot_names_the_module_that_carried_the_main_dispatch_regression() -> None:
    modules = json.loads(_ok(REPO_ROOT, "HEAD"))["modules"]
    assert SWEEP_MODULE in modules


def test_snapshot_records_the_restored_main_dispatch_on_that_module() -> None:
    modules = json.loads(_ok(REPO_ROOT, "HEAD"))["modules"]
    assert modules[SWEEP_MODULE]["main_dispatch"] is True


def test_require_modules_rejects_a_snapshot_missing_a_named_module(scratch: Path) -> None:
    proc = _snapshot(scratch, "HEAD", "--require-module", OPENFOAM_INIT)
    assert proc.returncode == 3


def test_require_modules_accepts_a_snapshot_containing_the_named_module() -> None:
    proc = _snapshot(REPO_ROOT, "HEAD", "--require-module", OPENFOAM_INIT)
    assert proc.returncode == 0


# --------------------------------------------------------------------------- #
# The defect classes 723 passing tests missed
# --------------------------------------------------------------------------- #


def test_snapshot_records_signatures_with_their_default_literals(scratch: Path) -> None:
    modules = json.loads(_ok(scratch, "HEAD"))["modules"]
    assert modules["src/digitalmodel/m.py"]["symbols"]["build"] == (
        "(count=3, label=None, *, strict=False)"
    )


def test_snapshot_records_public_methods_of_public_classes(scratch: Path) -> None:
    modules = json.loads(_ok(scratch, "HEAD"))["modules"]
    assert modules["src/digitalmodel/m.py"]["symbols"]["Widget.render"] == "(self, width=80)"


def test_snapshot_records_the_declared_exports(scratch: Path) -> None:
    modules = json.loads(_ok(scratch, "HEAD"))["modules"]
    assert modules["src/digitalmodel/m.py"]["all"] == ["Widget", "build"]


def test_snapshot_detects_a_changed_default(scratch: Path) -> None:
    before = _ok(scratch, "HEAD")
    head = _rewrite(
        scratch,
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def build(count=4, label=None, *, strict=False):\n"
        "    return count\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n"
        "\n"
        "\n"
        'if __name__ == "__main__":\n'
        "    raise SystemExit(build())\n",
    )
    assert _ok(scratch, head) != before


def test_snapshot_detects_a_dropped_optional_parameter(scratch: Path) -> None:
    before = _ok(scratch, "HEAD")
    head = _rewrite(
        scratch,
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def build(count=3, *, strict=False):\n"
        "    return count\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n"
        "\n"
        "\n"
        'if __name__ == "__main__":\n'
        "    raise SystemExit(build())\n",
    )
    assert _ok(scratch, head) != before


def test_snapshot_detects_a_removed_main_dispatch(scratch: Path) -> None:
    """The exact #1574 regression, reproduced as an oracle."""
    head = _rewrite(
        scratch,
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def build(count=3, label=None, *, strict=False):\n"
        "    return count\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n",
    )
    modules = json.loads(_ok(scratch, head))["modules"]
    assert modules["src/digitalmodel/m.py"]["main_dispatch"] is False


def test_a_pure_body_edit_does_not_move_the_snapshot(scratch: Path) -> None:
    """The snapshot describes the surface, not the implementation."""
    before = _ok(scratch, "HEAD")
    head = _rewrite(
        scratch,
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def build(count=3, label=None, *, strict=False):\n"
        "    total = count * 2\n"
        "    return total // 2\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n"
        "\n"
        "\n"
        'if __name__ == "__main__":\n'
        "    raise SystemExit(build())\n",
    )
    assert _ok(scratch, head) == before


def test_private_symbols_are_not_part_of_the_public_surface(scratch: Path) -> None:
    before = _ok(scratch, "HEAD")
    head = _rewrite(
        scratch,
        '__all__ = ["build", "Widget"]\n'
        "\n"
        "\n"
        "def _helper(x=1):\n"
        "    return x\n"
        "\n"
        "\n"
        "def build(count=3, label=None, *, strict=False):\n"
        "    return count\n"
        "\n"
        "\n"
        "class Widget:\n"
        "    def render(self, width=80):\n"
        "        return width\n"
        "\n"
        "\n"
        'if __name__ == "__main__":\n'
        "    raise SystemExit(build())\n",
    )
    assert _ok(scratch, head) == before
