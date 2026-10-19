"""Root engine tests must not change imports seen by domain tests (#2312).

Expected results are object identities and exact state restoration, derived from
Python's import-cache contract; no engineering constants or tolerances apply.
Sources: Python 3.11 Language Reference, sections 5.3.1 (module cache) and
5.4.2 (submodules); pytest 8.4, How to monkeypatch/mock modules and environments.
"""

import os
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest


ROOT = Path(__file__).resolve().parents[1]
ENGINE_TESTS = (
    "simple_engine_test.py",
    "test_engine.py",
    "test_engine_isolated.py",
)


@pytest.mark.parametrize("filename", ENGINE_TESTS)
def test_collection_preserves_import_state(filename, tmp_path):
    """A cached consumer and every preloaded module must retain their identities.

    A fresh interpreter emulates collection without depending on collection order
    in the outer pytest run. Sentinels prevent loading any solver dependencies.
    """
    script = textwrap.dedent(
        """
        import runpy
        import sys
        from types import ModuleType
        from unittest.mock import Mock
        import digitalmodel

        engine_module = ModuleType("digitalmodel.engine")
        engine_module.engine = object()
        digitalmodel.engine = engine_module
        sys.modules[engine_module.__name__] = engine_module
        update = ModuleType("assetutilities.common.update_deep")
        update.AttributeDict = dict
        sys.modules[update.__name__] = update
        from digitalmodel.engine import engine as cached_consumer

        before = dict(sys.modules)
        before_path = list(sys.path)
        runpy.run_path(sys.argv[1])

        changed = [name for name, module in before.items()
                   if sys.modules.get(name) is not module]
        leaked = [name for name, module in sys.modules.items()
                  if name not in before and isinstance(module, Mock)]
        assert not changed, changed
        assert not leaked, leaked
        assert sys.path == before_path
        assert digitalmodel.engine is engine_module
        assert engine_module.engine is cached_consumer
        """
    )
    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join((str(ROOT / "src"), str(ROOT)))
    result = subprocess.run(
        [sys.executable, "-c", script, str(ROOT / "tests" / filename)],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize("preloaded", [False, True])
@pytest.mark.parametrize("raises", [False, True])
def test_execution_restores_engine_and_dependencies(monkeypatch, preloaded, raises):
    """Exact cache and parent identities must survive normal and error exits.

    The retained consumer emulates a domain module that imported engine before
    root tests ran. No numerical expected values are involved.
    """
    from types import ModuleType

    import digitalmodel
    from tests.engine_test_support import DEPENDENCIES, isolated_engine

    cached = ModuleType("digitalmodel.engine")
    cached.engine = object()
    consumer = cached.engine
    if preloaded:
        monkeypatch.setitem(sys.modules, cached.__name__, cached)
        monkeypatch.setattr(digitalmodel, "engine", cached, raising=False)
    else:
        monkeypatch.delitem(sys.modules, cached.__name__, raising=False)
        monkeypatch.delattr(digitalmodel, "engine", raising=False)

    # A separately cached dependency and its importer binding also survive.
    dependency_name = "digitalmodel.signal_processing.time_series.time_series_analysis"
    dependency = ModuleType(dependency_name)
    dependency.TimeSeriesAnalysis = object()
    monkeypatch.setitem(sys.modules, dependency_name, dependency)
    retained_class = dependency.TimeSeriesAnalysis
    before = dict(sys.modules)
    before_path = list(sys.path)

    class TestFailure(Exception):
        pass

    def exercise():
        with isolated_engine() as temporary:
            from digitalmodel.engine import engine

            assert engine is temporary.engine
            assert digitalmodel.engine is temporary
            cfg = {"basename": "transformation"}
            temporary.Transformation.return_value.router.return_value = cfg
            assert engine(cfg=cfg, config_flag=False) is cfg
            temporary.Transformation.return_value.router.assert_called_once_with(cfg)
            if raises:
                raise TestFailure

    if raises:
        with pytest.raises(TestFailure):
            exercise()
    else:
        exercise()

    assert all(sys.modules.get(name) is module for name, module in before.items())
    for name in (*DEPENDENCIES, "digitalmodel.engine"):
        assert sys.modules.get(name) is before.get(name)
    prefixes = ("digitalmodel", "assetutilities", "loguru")
    assert {name for name in sys.modules if name.startswith(prefixes)} == {
        name for name in before if name.startswith(prefixes)
    }
    assert sys.path == before_path
    assert dependency.TimeSeriesAnalysis is retained_class
    if preloaded:
        assert digitalmodel.engine is cached
        assert cached.engine is consumer
    else:
        assert "engine" not in vars(digitalmodel)


def test_import_failure_is_not_replaced_by_fake_engine(monkeypatch):
    """A broken engine import must fail the test and still restore its cache."""
    import importlib.machinery

    from tests.engine_test_support import isolated_engine

    before = dict(sys.modules)

    def fail_import(loader, module):
        raise ImportError("engine dependency unavailable")

    monkeypatch.setattr(
        importlib.machinery.SourceFileLoader, "exec_module", fail_import
    )
    with pytest.raises(ImportError, match="engine dependency unavailable"):
        with isolated_engine():
            pytest.fail("a broken import must not yield an engine")
    assert all(sys.modules.get(name) is module for name, module in before.items())
    assert sys.modules.get("digitalmodel.engine") is before.get("digitalmodel.engine")
