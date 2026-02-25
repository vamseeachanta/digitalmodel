"""Shared fixtures for format converter tests."""

from pathlib import Path

import pytest
import yaml

# Project root for the digitalmodel submodule
DIGITALMODEL_ROOT = Path(__file__).resolve().parents[4]
EXAMPLES_DIR = DIGITALMODEL_ROOT / 'docs' / 'modules' / 'orcaflex' / 'examples'
PIPELINE_DIR = DIGITALMODEL_ROOT / 'docs' / 'modules' / 'orcaflex' / 'pipeline'


@pytest.fixture
def raw_single_dir() -> Path:
    """Path to raw single-file OrcaFlex examples."""
    return EXAMPLES_DIR / 'raw'


@pytest.fixture
def modular_dir() -> Path:
    """Path to modular OrcaFlex examples."""
    return EXAMPLES_DIR / 'modular'


@pytest.fixture
def a01_single_file(raw_single_dir: Path) -> Path:
    """Path to A01 Catenary riser single-file YAML."""
    p = raw_single_dir / 'A01' / 'A01 Catenary riser.yml'
    if not p.exists():
        pytest.skip(f"Test data not found: {p}")
    return p


@pytest.fixture
def a01_modular_dir(modular_dir: Path) -> Path:
    """Path to A01 Catenary riser modular directory."""
    p = modular_dir / 'A01' / 'A01 Catenary riser'
    if not p.exists():
        pytest.skip(f"Test data not found: {p}")
    return p


@pytest.fixture
def a01_master_file(a01_modular_dir: Path) -> Path:
    """Path to A01 Catenary riser master.yml."""
    return a01_modular_dir / 'master.yml'


@pytest.fixture
def spec_file() -> Path:
    """Path to 30in pipeline spec file."""
    p = PIPELINE_DIR / 'installation' / 'floating' / '30in_pipeline' / 'spec.yml'
    if not p.exists():
        pytest.skip(f"Test data not found: {p}")
    return p


@pytest.fixture
def sample_single_data() -> dict:
    """Minimal single-file OrcaFlex data for unit tests."""
    return {
        'General': {
            'UnitsSystem': 'SI',
            'ImplicitConstantTimeStep': 0.1,
            'StageDuration': [7, 35],
        },
        'Environment': {
            'WaterDepth': 100,
            'Density': 1.025,
            'WaveTrains': [
                {
                    'Name': 'Wave 1',
                    'WaveType': 'Dean stream',
                    'WaveHeight': 6,
                    'WavePeriod': 7,
                    'WaveDirection': 180,
                }
            ],
        },
        'LineTypes': [
            {
                'Name': 'Steel Riser',
                'OD': 0.3,
                'ID': 0.25,
            }
        ],
    }


@pytest.fixture
def sample_spec_data() -> dict:
    """Minimal spec data for unit tests."""
    return {
        'metadata': {
            'name': 'test_model',
            'description': 'Test model',
            'structure': 'pipeline',
            'operation': 'installation/floating',
        },
        'environment': {
            'water': {'depth': 100, 'density': 1.025},
            'waves': {'height': 6, 'period': 7, 'direction': 180},
            'current': {'speed': 0.7, 'direction': 270},
            'wind': {'speed': 0},
        },
        'pipeline': {
            'name': 'Test Line',
            'material': 'X65',
        },
        'simulation': {
            'time_step': 0.1,
            'stages': [7, 35],
        },
    }
