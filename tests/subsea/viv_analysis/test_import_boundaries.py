"""Screening must not import the fatigue backend as a side effect.

The dependency boundary is structural: the explorer imports screening, while
pylife belongs to the fatigue calculation path. No engineering value is used.
"""

import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[3]


def test_screening_and_explorer_import_without_fatigue_backend(tmp_path):
    environment = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        environment.pop(name, None)
    code = """
import importlib.abc
import runpy
import sys

class AbsentFatigueBackend(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] == 'pylife':
            raise ModuleNotFoundError(fullname, name=fullname)

sys.meta_path.insert(0, AbsentFatigueBackend())
sys.path.insert(0, sys.argv[1])
from digitalmodel.subsea.viv_analysis import VIVScreening
from digitalmodel.subsea.viv_analysis.screening import VIVScreening as DirectScreening
assert VIVScreening is DirectScreening
assert 'digitalmodel.subsea.viv_analysis.fatigue' not in sys.modules
runpy.run_path(sys.argv[2], run_name='import_only')
assert not any(name.split('.')[0] == 'pylife' for name in sys.modules)
"""
    result = subprocess.run(
        [sys.executable, "-I", "-B", "-c", code, str(ROOT / "src"),
         str(ROOT / "scripts/capabilities/build_viv_explorer.py")],
        cwd=tmp_path, env=environment, capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stderr


def test_fatigue_class_keeps_public_import_compatibility():
    # Test the export mechanism independently of backend wheel availability.
    code = """
import sys
import types
sys.path.insert(0, sys.argv[1])
from digitalmodel.subsea import viv_analysis
assert 'VIVFatigueCalculator' not in vars(viv_analysis)
backend = types.ModuleType('digitalmodel.subsea.viv_analysis.fatigue')
backend.VIVFatigueCalculator = type('VIVFatigueCalculator', (), {})
sys.modules[backend.__name__] = backend
from digitalmodel.subsea.viv_analysis import VIVFatigueCalculator
from digitalmodel.subsea.viv_analysis.fatigue import VIVFatigueCalculator as DirectFatigue
assert VIVFatigueCalculator is DirectFatigue
assert vars(viv_analysis)['VIVFatigueCalculator'] is DirectFatigue
"""
    result = subprocess.run(
        [sys.executable, "-I", "-B", "-c", code, str(ROOT / "src")],
        capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stderr


def test_public_discovery_and_unknown_attributes():
    import pytest
    from digitalmodel.subsea import viv_analysis

    assert set(viv_analysis.__all__) <= set(dir(viv_analysis))
    with pytest.raises(AttributeError, match="has no attribute 'unknown_calculator'"):
        viv_analysis.unknown_calculator
