"""Regression for the collection-time module replacements reported in #2312."""

import os
from pathlib import Path
import subprocess
import sys
from types import ModuleType

import pytest

from tests.engine_test_support import isolated_engine


ROOT = Path(__file__).resolve().parents[1]
ENGINE_TESTS = (
    "simple_engine_test.py",
    "test_engine.py",
    "test_engine_isolated.py",
)


@pytest.mark.parametrize("filename", ENGINE_TESTS)
def test_collection_preserves_cached_modules(filename, tmp_path):
    """Identity oracle: importing tests must preserve every cached dependency.

    The sentinels stand for modules imported earlier by domain tests. Expected
    identities are the original objects, independently of engine implementation.
    An import blocker also checks the initially absent case without loading any
    engineering dependencies or native solver APIs.
    """
    script = r"""
import importlib.abc
import importlib.util
from pathlib import Path
import sys
from types import ModuleType

root = Path(sys.argv[1])
filename = sys.argv[2]
sys.path.insert(0, str(root))
sys.path.insert(0, str(root / "src"))
prefixes = ("assetutilities", "digitalmodel", "scipy", "loguru", "tabulate")

class NoEngineeringImports(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] in prefixes:
            raise AssertionError(f"test collection imported {fullname}")

sys.meta_path.insert(0, NoEngineeringImports())
for cached in (False, True):
    names = (
        "digitalmodel.engine",
        "assetutilities.common.update_deep",
        "digitalmodel.signal_processing.signal_analysis.fatigue",
        "digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection",
        "digitalmodel.hydrodynamics.aqwa.mes_files",
        "scipy",
        "loguru",
        "tabulate",
    )
    if cached:
        for name in names:
            sys.modules[name] = ModuleType(name)
    before = {key: value for key, value in sys.modules.items()
              if key.split(".")[0] in prefixes}
    spec = importlib.util.spec_from_file_location("engine_collection_probe",
                                               root / "tests" / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    after = {key: value for key, value in sys.modules.items()
             if key.split(".")[0] in prefixes}
    assert after.keys() == before.keys(), "collection added dependency modules"
    assert all(after[key] is value for key, value in before.items()), (
        "collection replaced cached dependency modules"
    )
"""
    env = dict(os.environ, HYPOTHESIS_STORAGE_DIRECTORY=str(tmp_path / "hypothesis"))
    result = subprocess.run(
        [sys.executable, "-c", script, str(ROOT), filename],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize("cached", [False, True])
@pytest.mark.parametrize("fail", [False, True])
def test_execution_restores_modules_and_package_bindings(cached, fail, monkeypatch):
    """Restoration oracle: absent entries stay absent; existing objects survive.

    A cached importer keeps its original engine function throughout the scoped
    load. Exception and normal exits must have identical restoration semantics.
    No numerical engineering result or acceptance limit is used.
    """
    import digitalmodel

    dependency = "digitalmodel.hydrodynamics.aqwa"
    parent = ModuleType("digitalmodel.hydrodynamics")
    monkeypatch.setitem(sys.modules, parent.__name__, parent)
    monkeypatch.setattr(digitalmodel, "hydrodynamics", parent, raising=False)
    if cached:
        original_engine = ModuleType("digitalmodel.engine")
        original_engine.engine = object()
        original_dependency = ModuleType(dependency)
        original_dependency.Aqwa = object()
        monkeypatch.setitem(sys.modules, "digitalmodel.engine", original_engine)
        monkeypatch.setattr(digitalmodel, "engine", original_engine, raising=False)
        monkeypatch.setitem(sys.modules, dependency, original_dependency)
        monkeypatch.setattr(parent, "aqwa", original_dependency, raising=False)
        cached_function = original_engine.engine
    else:
        monkeypatch.delitem(sys.modules, "digitalmodel.engine", raising=False)
        monkeypatch.delattr(digitalmodel, "engine", raising=False)
        monkeypatch.delitem(sys.modules, dependency, raising=False)

    before = sys.modules.copy()
    package_before = vars(digitalmodel).copy()
    parent_before = vars(parent).copy()
    path_before = sys.path[:]
    try:
        with isolated_engine() as module:
            assert sys.modules["digitalmodel.engine"] is module
            assert digitalmodel.engine is module
            assert parent.aqwa is sys.modules[dependency]
            assert module.Aqwa is parent.aqwa.Aqwa
            if cached:
                assert original_engine.engine is cached_function
                assert module.engine is not cached_function
            cfg = {"basename": "transformation"}
            module.Transformation.return_value.router.return_value = cfg
            assert module.engine(cfg=cfg, config_flag=False) is cfg
            if fail:
                raise RuntimeError("test body failure")
    except RuntimeError as exc:
        assert fail and str(exc) == "test body failure"

    assert sys.modules.keys() == before.keys()
    assert all(sys.modules[name] is value for name, value in before.items())
    assert vars(digitalmodel) == package_before
    assert vars(parent) == parent_before
    assert sys.path == path_before


def test_import_failure_restores_modules(monkeypatch):
    """A loader exception must unwind replacements before an engine is yielded."""
    import digitalmodel
    from importlib.machinery import SourceFileLoader

    def fail_import(self, module):
        raise RuntimeError("engine import failure")

    monkeypatch.setattr(SourceFileLoader, "exec_module", fail_import)
    before = sys.modules.copy()
    package_before = vars(digitalmodel).copy()
    with pytest.raises(RuntimeError, match="engine import failure"):
        with isolated_engine():
            pytest.fail("an unsuccessful import must not yield an engine")
    assert sys.modules.keys() == before.keys()
    assert all(sys.modules[name] is value for name, value in before.items())
    assert vars(digitalmodel) == package_before


def test_installing_doubles_does_not_resolve_lazy_package_attributes(monkeypatch):
    """Reading a lazy attribute can import a real backend before it is mocked."""
    import digitalmodel

    parent = ModuleType("digitalmodel.hydrodynamics")

    def reject_lazy_import(name):
        raise AssertionError(f"resolved lazy package attribute: {name}")

    parent.__getattr__ = reject_lazy_import
    monkeypatch.setitem(sys.modules, parent.__name__, parent)
    monkeypatch.setitem(vars(digitalmodel), "hydrodynamics", parent)
    with isolated_engine() as module:
        assert module.Aqwa is parent.aqwa.Aqwa
    assert "aqwa" not in vars(parent)
