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


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_cfd_extra_is_linux_safe_and_gmsh_locked() -> None:
    project = tomllib.loads(_read(PYPROJECT))
    cfd = project["project"]["optional-dependencies"]["cfd"]

    assert cfd == ["gmsh==4.15.1"]
    assert all("OrcFxAPI" not in requirement for requirement in cfd)


def test_provisioner_pins_and_holds_solver_packages() -> None:
    script = _read(PROVISION)

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
    assert 'sha256sum -c -' in script
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
