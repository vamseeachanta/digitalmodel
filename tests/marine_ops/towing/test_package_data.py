"""Verify setuptools includes the fixture consumed by the worked example."""
from pathlib import Path
import tomllib

from setuptools import Distribution
from setuptools.command.build_py import build_py


def test_installed_package_contains_worked_fixture():
    root = Path(__file__).resolve().parents[3]
    config = tomllib.loads((root / 'pyproject.toml').read_text())
    dist = Distribution({'packages': ['digitalmodel.marine_ops.towing'],
                         'package_dir': {'': str(root / 'src')},
                         'package_data': config['tool']['setuptools']['package-data']})
    command = build_py(dist)
    command.ensure_finalized()
    command.manifest_files = {}
    package = root / 'src/digitalmodel/marine_ops/towing'
    included = command.find_data_files('digitalmodel.marine_ops.towing', str(package))
    assert str(package / 'fixtures/barge_tug_500t_pull.yml') in included
    for reference in ('towing-plan-487.md', 'ocimf-static-towing-2020.md'):
        assert str(package / 'basis/wikis' / reference) in included
