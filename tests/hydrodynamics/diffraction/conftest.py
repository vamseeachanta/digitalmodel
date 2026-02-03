"""Test configuration for diffraction module tests."""
from __future__ import annotations

from datetime import datetime
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunConfig
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    RAOComponent,
    RAOSet,
)

FIXTURES_DIR = Path(__file__).parent / "fixtures"

N_FREQ = 10
N_HEAD = 5
FREQUENCIES = np.linspace(0.05, 2.0, N_FREQ)
HEADINGS = np.array([0.0, 45.0, 90.0, 135.0, 180.0])


def _make_freq_data() -> FrequencyData:
    return FrequencyData(
        values=FREQUENCIES.copy(),
        periods=2.0 * np.pi / FREQUENCIES,
        count=N_FREQ,
        min_freq=float(FREQUENCIES[0]),
        max_freq=float(FREQUENCIES[-1]),
    )


def _make_heading_data() -> HeadingData:
    return HeadingData(
        values=HEADINGS.copy(),
        count=N_HEAD,
        min_heading=float(HEADINGS[0]),
        max_heading=float(HEADINGS[-1]),
    )


def _make_rao_component(dof: DOF) -> RAOComponent:
    rng = np.random.default_rng(seed=dof.value)
    if dof in (DOF.SURGE, DOF.SWAY, DOF.HEAVE):
        magnitude = rng.uniform(0.0, 1.5, size=(N_FREQ, N_HEAD))
    else:
        magnitude = rng.uniform(0.0, 5.0, size=(N_FREQ, N_HEAD))
    phase = rng.uniform(-180.0, 180.0, size=(N_FREQ, N_HEAD))
    return RAOComponent(
        dof=dof,
        magnitude=magnitude,
        phase=phase,
        frequencies=_make_freq_data(),
        headings=_make_heading_data(),
        unit="",
    )


def _make_rao_set() -> RAOSet:
    now = datetime.now().isoformat()
    return RAOSet(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        surge=_make_rao_component(DOF.SURGE),
        sway=_make_rao_component(DOF.SWAY),
        heave=_make_rao_component(DOF.HEAVE),
        roll=_make_rao_component(DOF.ROLL),
        pitch=_make_rao_component(DOF.PITCH),
        yaw=_make_rao_component(DOF.YAW),
        created_date=now,
        source_file="test_model.sim",
    )


def _make_matrix_set(matrix_type: str) -> AddedMassSet | DampingSet:
    rng = np.random.default_rng(seed=42 if matrix_type == "added_mass" else 99)
    freq_data = _make_freq_data()
    matrices = []
    for i in range(N_FREQ):
        m = rng.uniform(100, 1000, size=(6, 6))
        m = (m + m.T) / 2  # symmetric
        matrices.append(
            HydrodynamicMatrix(
                matrix=m,
                frequency=float(FREQUENCIES[i]),
                matrix_type=matrix_type,
                units={"linear": "kg", "angular": "kg.m^2"},
            )
        )
    cls = AddedMassSet if matrix_type == "added_mass" else DampingSet
    return cls(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        matrices=matrices,
        frequencies=freq_data,
        created_date=datetime.now().isoformat(),
    )


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


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


@pytest.fixture
def mock_rao_set() -> RAOSet:
    """Synthetic RAOSet with 10 frequencies, 5 headings, realistic shapes."""
    return _make_rao_set()


@pytest.fixture
def mock_diffraction_results() -> DiffractionResults:
    """Synthetic DiffractionResults with 10 frequencies, 5 headings."""
    now = datetime.now().isoformat()
    return DiffractionResults(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        raos=_make_rao_set(),
        added_mass=_make_matrix_set("added_mass"),
        damping=_make_matrix_set("damping"),
        created_date=now,
        source_files=["test_model.sim"],
    )


@pytest.fixture
def batch_config_path(tmp_path: Path, ship_raos_spec_path: Path) -> Path:
    """Temporary batch configuration YAML file."""
    config = {
        "jobs": [
            {
                "spec_path": str(ship_raos_spec_path),
                "job_name": "ship_raos",
                "dry_run": True,
            },
        ],
        "execution_mode": "sequential",
        "base_output_dir": str(tmp_path / "batch_output"),
    }
    config_file = tmp_path / "batch_config.yml"
    config_file.write_text(yaml.dump(config))
    return config_file
