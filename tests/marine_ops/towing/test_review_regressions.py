"""Regressions for the second-provider review of PR 4048."""
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tomllib

import pytest


def test_packaging_backend_is_a_declared_test_dependency():
    root = Path(__file__).resolve().parents[3]
    config = tomllib.loads((root / 'pyproject.toml').read_text())
    requirements = config['project']['optional-dependencies']['test']
    assert any(item.startswith('setuptools>=') for item in requirements)
    lock = tomllib.loads((root / 'uv.lock').read_text())
    package = next(item for item in lock['package'] if item['name'] == 'digitalmodel')
    assert {'name': 'setuptools'} in package['optional-dependencies']['test']
    assert {'name': 'setuptools', 'marker': "extra == 'test'", 'specifier': '>=61.0'} in (
        package['metadata']['requires-dist'])


def test_missing_vlcc_reference_fails_closed(tmp_path, monkeypatch):
    from digitalmodel.citations.schema import CitationResolutionError
    from digitalmodel.marine_ops.towing import ocimf_vlcc

    monkeypatch.setattr(ocimf_vlcc, '_BASIS_ROOT', tmp_path)
    with pytest.raises(CitationResolutionError, match='OCIMF-STATIC-TOWING-2020'):
        ocimf_vlcc.wind_coefficients(45)


def test_configured_citation_drift_is_not_hidden_by_synthetic_metadata(tmp_path):
    """Exercise the actual conftest with stale maintained metadata in isolation."""
    wiki = tmp_path / 'maintained'
    page = wiki / 'wikis/engineering/wiki/standards/ocimf-meg4.md'
    page.parent.mkdir(parents=True)
    page.write_text('---\ncode_id: OCIMF-MEG4\npublisher: OCIMF\n'
                    'revision: changed-maintained-edition\n---\n')
    suite = tmp_path / 'suite'
    suite.mkdir()
    shutil.copyfile(Path(__file__).with_name('conftest.py'), suite / 'conftest.py')
    (suite / 'test_drift.py').write_text('''
import pytest
from digitalmodel.citations.schema import CitationResolutionError
from digitalmodel.hydrodynamics.models import VesselProperties
from digitalmodel.marine_ops.towing.drift_force import compute, WindProfile

def test_maintained_revision_is_checked():
    with pytest.raises(CitationResolutionError, match='frontmatter_mismatch:revision'):
        compute(VesselProperties('synthetic', 80, 24, 4, 7000), 1, WindProfile(0))
''')
    env = {key: value for key, value in os.environ.items()
           if key not in {'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'}}
    env['LLM_WIKI_PATH'] = str(wiki)
    result = subprocess.run([sys.executable, '-m', 'pytest', str(suite),
                             '-m', 'not solver', '-q'],
                            env=env, capture_output=True, text=True, timeout=60)
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize('direction,expected', [
    (50, (215625, 1056250, -92719900)),
    (45, (245812.5, 983125, -92719900)),
])
def test_published_vlcc_coefficients_drive_production_interpolation(direction, expected):
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute, WindProfile

    # OCIMF Static Towing Assembly Guidelines 2020, MARIN 31746-1-PO:
    # p25 Table 1 VLCC Af=2300 m2, Al=6500 m2, Lpp=354.4 m;
    # p29 Table 5 rows 40/50 deg: (.48,.56,-.161), (.375,.65,-.161).
    # p10 section 4.4 specifies q=250 Pa at the test's 20 m/s and 1.25 kg/m3.
    # Retrievable publication copy is linked in examples/marine_ops/towing/README.html.
    vessel = VesselProperties('published VLCC verification', 360, 58, 14.4, 245386,
                              frontal_wind_area=2300, lateral_wind_area=6500)
    wind = WindProfile(20, direction, air_density_kg_m3=1.25,
                       coefficient_model='ocimf_2020_vlcc', moment_reference_length_m=354.4)
    result = compute(vessel, 0, wind)
    for actual, reference in zip((result.wind_x_n, result.wind_y_n, result.yaw_moment_nm),
                                  expected):
        assert actual == pytest.approx(reference, rel=0.05)
    citation = result.citations[-1]
    assert citation.code_id == 'OCIMF-STATIC-TOWING-2020'
    assert 'Table 5' in citation.section and '29' in citation.section


@pytest.mark.parametrize('direction,expected', [
    (45, (.4275, .605, -.161)), (405, (.4275, .605, -.161)),
    (-45, (.4275, -.605, .161)), (0, (.69, 0, 0)),
    (360, (.69, 0, 0)), (355, (.6975, -.0675, .0285)),
])
def test_vlcc_interpolation_signs_and_periodicity(direction, expected):
    from digitalmodel.marine_ops.towing.ocimf_vlcc import wind_coefficients

    assert wind_coefficients(direction) == pytest.approx(expected, rel=1e-12, abs=1e-12)


@pytest.mark.parametrize('kwargs,match', [
    ({'coefficient_model': 'unknown'}, 'coefficient model'),
    ({'coefficient_model': 'ocimf_2020_vlcc'}, 'reference length'),
    ({'coefficient_model': 'ocimf_2020_vlcc', 'moment_reference_length_m': 354.4,
      'coefficients': (.1, .2, .3)}, 'mutually exclusive'),
    ({'moment_reference_length_m': float('nan')}, 'reference length'),
])
def test_wind_model_inputs_fail_closed(kwargs, match):
    from digitalmodel.hydrodynamics.models import VesselProperties
    from digitalmodel.marine_ops.towing.drift_force import compute, WindProfile

    vessel = VesselProperties('synthetic', 80, 24, 4, 7000,
                              frontal_wind_area=96, lateral_wind_area=320)
    with pytest.raises(ValueError, match=match):
        compute(vessel, 0, WindProfile(20, 45, **kwargs))
