"""Test configuration for diffraction module tests."""
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunConfig

FIXTURES_DIR = Path(__file__).parent / "fixtures"


@pytest.fixture
def fixtures_dir() -> Path:
    """Return path to test fixtures directory."""
    return FIXTURES_DIR


@pytest.fixture
def ship_raos_spec_path(fixtures_dir: Path) -> Path:
    """Path to ship RAO spec fixture."""
    return fixtures_dir / "spec_ship_raos.yml"


@pytest.fixture
def semisub_spec_path(fixtures_dir: Path) -> Path:
    """Path to semi-sub spec fixture."""
    return fixtures_dir / "spec_semisub.yml"


@pytest.fixture
def fpso_turret_spec_path(fixtures_dir: Path) -> Path:
    """Path to FPSO turret spec fixture."""
    return fixtures_dir / "spec_fpso_turret.yml"


@pytest.fixture
def dry_run_config(tmp_path: Path) -> RunConfig:
    """RunConfig pre-configured for dry-run testing."""
    return RunConfig(output_dir=tmp_path, dry_run=True)
