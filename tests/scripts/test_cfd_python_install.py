"""Locked CFD Python, provisioner, verifier, and CI contract tests."""

from __future__ import annotations

import subprocess
import tomllib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PYPROJECT = ROOT / "pyproject.toml"
PROVISION = ROOT / "scripts/setup/provision-cfd-box.sh"
VERIFY = ROOT / "scripts/setup/verify-cfd-box.sh"
WORKFLOW = ROOT / ".github/workflows/gmsh-meshing-tests.yml"

OPENFOAM_VERSION = "2312.260127-2"
OPENMPI_VERSION = "4.1.6-7ubuntu2"
INSTALLER_SHA256 = "f7fa288327e936b5a85e3e4a0b29bf039c06d214916f39400b830b63a3310b5b"
ASSETUTILITIES_COMMIT = "993f1b5ddc90b56ecf531bedb1b84f5efe096700"
ASSETUTILITIES_TREE = "5f7434fcb3a348a7e04bf9de228e6ce2c49a87cb"
ACTIONLINT_COMMIT = "914e7df21a07ef503a81201c76d2b11c789d3fca"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_cfd_extra_is_linux_safe_and_gmsh_locked() -> None:
    project = tomllib.loads(_read(PYPROJECT))
    cfd = project["project"]["optional-dependencies"]["cfd"]

    assert cfd == ["gmsh==4.15.1"]
    assert all("OrcFxAPI" not in requirement for requirement in cfd)


def test_provisioner_pins_and_holds_solver_packages() -> None:
    script = _read(PROVISION)

    assert "libglu1-mesa" in script
    assert f'OPENFOAM_PACKAGE_VERSION="{OPENFOAM_VERSION}"' in script
    assert f'OPENMPI_PACKAGE_VERSION="{OPENMPI_VERSION}"' in script
    assert '"openfoam2312-default=${OPENFOAM_PACKAGE_VERSION}"' in script
    assert '"openmpi-bin=${OPENMPI_PACKAGE_VERSION}"' in script
    assert '"libopenmpi-dev=${OPENMPI_PACKAGE_VERSION}"' in script
    assert "apt-mark hold" in script
    assert "openfoam2312-default openmpi-bin libopenmpi-dev" in script


def test_openfoam_repo_installer_is_checksum_verified_before_execution() -> None:
    script = _read(PROVISION)

    assert f'OPENFOAM_INSTALLER_SHA256="{INSTALLER_SHA256}"' in script
    assert 'curl -fsSL -o "$installer"' in script
    assert "sha256sum -c -" in script
    assert 'bash "$installer"' in script
    assert "add-debian-repo.sh |" not in script


def test_provisioner_uses_clean_frozen_cfd_environment() -> None:
    script = _read(PROVISION)

    assert "git diff --quiet" in script
    assert "git diff --cached --quiet" in script
    assert "git ls-files --others --exclude-standard" in script
    assert "uv sync --frozen --extra cfd --extra test" in script
    assert "uv pip install -q -e ." not in script
    assert "import gmsh" in script


def test_provisioner_pins_assetutilities_sibling_before_uv_sync() -> None:
    script = _read(PROVISION)

    assert "https://github.com/vamseeachanta/assetutilities.git" in script
    assert f'ASSETUTILITIES_COMMIT="{ASSETUTILITIES_COMMIT}"' in script
    assert f'ASSETUTILITIES_TREE="{ASSETUTILITIES_TREE}"' in script
    assert '$(dirname "$REPO_DIR")/assetutilities' in script
    assert 'git clone -q --no-checkout "$ASSETUTILITIES_URL"' in script
    assert "rev-parse HEAD" in script
    assert "rev-parse 'HEAD^{tree}'" in script
    dependency_gate = 'verify_pinned_dependency "$ASSETUTILITIES_DIR"'
    assert dependency_gate in script
    assert script.rindex(dependency_gate) < script.index("uv sync --frozen")


def test_repository_detection_accepts_linked_git_worktrees() -> None:
    provision = _read(PROVISION)
    verify = _read(VERIFY)

    assert 'git -C "$repo" rev-parse --is-inside-work-tree' in provision
    assert 'git -C "$ASSETUTILITIES_DIR" rev-parse --is-inside-work-tree' in verify
    assert '[[ -d "$repo/.git" ]]' not in provision
    assert '[[ -d "$ASSETUTILITIES_DIR/.git" ]]' not in verify


def test_verifier_checks_exact_packages_clean_tree_and_gmsh() -> None:
    script = _read(VERIFY)

    assert OPENFOAM_VERSION in script
    assert OPENMPI_VERSION in script
    assert "dpkg-query" in script
    assert "git diff --quiet" in script
    assert "git diff --cached --quiet" in script
    assert "git ls-files --others --exclude-standard" in script
    assert "uv run --frozen --extra cfd python" in script
    assert "import gmsh" in script
    assert "run_synthetic_tank_3d_smoke.py" in script
    assert "CFD_DISPATCH_RANKS=2" in script
    assert "decomposePar reconstructParMesh reconstructPar mpirun" in script


def test_verifier_attests_dependency_identity_and_package_holds() -> None:
    script = _read(VERIFY)

    assert ASSETUTILITIES_COMMIT in script
    assert ASSETUTILITIES_TREE in script
    assert '$(dirname "$REPO_ROOT")/assetutilities' in script
    assert "rev-parse HEAD" in script
    assert "rev-parse 'HEAD^{tree}'" in script
    assert "apt-mark showhold" in script
    for package in ("openfoam2312-default", "openmpi-bin", "libopenmpi-dev"):
        assert f'package_is_held "{package}"' in script


def test_shell_scripts_parse() -> None:
    for script in (PROVISION, VERIFY):
        result = subprocess.run(
            ["bash", "-n", str(script)],
            check=False,
            text=True,
            capture_output=True,
        )
        assert result.returncode == 0, result.stderr


def test_gmsh_workflow_uses_current_paths_and_frozen_uv() -> None:
    workflow = _read(WORKFLOW)

    assert "src/digitalmodel/modules/gmsh_meshing" not in workflow
    assert "tests/domains/gmsh_meshing" not in workflow
    assert "src/digitalmodel/solvers/gmsh_meshing/**" in workflow
    assert "src/digitalmodel/solvers/openfoam/**" in workflow
    assert "tests/solvers/gmsh_meshing/**" in workflow
    assert "tests/solvers/openfoam/**" in workflow
    assert "astral-sh/setup-uv@" in workflow
    assert "uv sync --frozen --extra cfd --extra test" in workflow
    assert "tests/solvers/gmsh_meshing/test_tank_fixture.py" in workflow
    assert "tests/solvers/openfoam/test_gmsh_bridge.py" in workflow


def test_gmsh_workflow_pins_sibling_and_validation_tools() -> None:
    workflow = _read(WORKFLOW)

    assert "working-directory: repos/digitalmodel" in workflow
    assert "path: repos/digitalmodel" in workflow
    assert "repository: vamseeachanta/assetutilities" in workflow
    assert f"ref: {ASSETUTILITIES_COMMIT}" in workflow
    assert "path: repos/assetutilities" in workflow
    assert workflow.count('"examples/cfd/synthetic_l_tank/input.yml"') == 2
    assert workflow.count('"docs/api/cfd/synthetic-l-tank-smoke.json"') == 2
    assert (
        "go install github.com/rhysd/actionlint/cmd/actionlint@"
        f"{ACTIONLINT_COMMIT}" in workflow
    )
    assert '"$(go env GOPATH)/bin/actionlint"' in workflow
    assert (
        "python -m digitalmodel.solvers.openfoam.smoke_evidence "
        "docs/api/cfd/synthetic-l-tank-smoke.json" in workflow
    )
