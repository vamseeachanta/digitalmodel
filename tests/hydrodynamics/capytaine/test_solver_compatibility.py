# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check solver requirements without installing the optional solver."""

from types import SimpleNamespace
from unittest.mock import Mock

import numpy as np
import pytest

from digitalmodel.hydrodynamics.capytaine import solver as adapter
from digitalmodel.hydrodynamics.capytaine.models import (
    BodyConfig, MeshConfig, WaveConditions,
)


def configured_solver():
    """Return the existing synthetic box setup without creating a native body."""
    return adapter.CapytaineSolver(
        BodyConfig(MeshConfig(predefined_shape="box")),
        WaveConditions(periods=np.array([10.0])),
    )


def test_old_solver_rejected_before_body_creation(monkeypatch):
    """The installed version is checked before any geometry or solve starts."""
    old_solver = SimpleNamespace(__version__="2.3.1")
    create_body = Mock(side_effect=AssertionError("body creation was reached"))
    monkeypatch.setattr(adapter, "_import_capytaine", lambda: old_solver)
    monkeypatch.setattr(adapter, "create_floating_body", create_body)
    with pytest.raises(RuntimeError, match=r"2\.3\.1.*Capytaine 3 or later"):
        configured_solver().solve()
    create_body.assert_not_called()


@pytest.mark.parametrize("version", ["2.0.0", "2.3.1"])
def test_old_solver_rejected_before_engine_construction(version):
    """The version error precedes lookup of version-three engine attributes."""
    with pytest.raises(RuntimeError, match=rf"{version}.*Capytaine 3 or later"):
        configured_solver()._build_solver(SimpleNamespace(__version__=version))


@pytest.mark.parametrize("version", ["3.0.0", "3.1.0.dev0"])
def test_supported_solver_builds_engine(version):
    """A version-three stand-in receives the existing configured engine options."""
    native = SimpleNamespace(
        __version__=version, Delhommeau=Mock(),
        DefaultMatrixEngine=Mock(), BEMSolver=Mock(),
    )
    result = configured_solver()._build_solver(native)
    assert result is native.BEMSolver.return_value
    native.DefaultMatrixEngine.assert_called_once_with(
        green_function=native.Delhommeau.return_value,
        linear_solver="lu_decomposition",
    )
    native.BEMSolver.assert_called_once_with(engine=native.DefaultMatrixEngine.return_value)


@pytest.mark.parametrize("cache_size", [0, 2])
def test_non_default_cache_setting_warns(cache_size):
    """An ineffective explicit cache setting is reported to existing callers."""
    from digitalmodel.hydrodynamics.capytaine.models import SolverConfig

    config = SolverConfig(matrix_cache_size=cache_size)
    solver = configured_solver()
    solver.solver_config = config
    native = SimpleNamespace(
        __version__="3.0.0", Delhommeau=Mock(),
        DefaultMatrixEngine=Mock(), BEMSolver=Mock(),
    )
    with pytest.warns(RuntimeWarning, match="matrix_cache_size.*no effect.*Capytaine 3"):
        solver._build_solver(native)
    assert config.matrix_cache_size == cache_size


def test_default_cache_setting_is_quiet():
    """The default retains its public value without an ineffective-setting warning."""
    import warnings
    from digitalmodel.hydrodynamics.capytaine.models import SolverConfig

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        config = SolverConfig()
        solver = configured_solver()
        solver.solver_config = config
        native = SimpleNamespace(
            __version__="3.0.0", Delhommeau=Mock(),
            DefaultMatrixEngine=Mock(), BEMSolver=Mock(),
        )
        solver._build_solver(native)
    assert config.matrix_cache_size == 1
    assert not caught


def test_mutated_cache_setting_warns_before_engine_construction():
    """Mutable configurations report the value actually used by the adapter."""
    solver = configured_solver()
    solver.solver_config.matrix_cache_size = 2
    native = SimpleNamespace(
        __version__="3.0.0", Delhommeau=Mock(),
        DefaultMatrixEngine=Mock(), BEMSolver=Mock(),
    )
    with pytest.warns(RuntimeWarning, match="matrix_cache_size.*no effect.*Capytaine 3"):
        solver._build_solver(native)


@pytest.mark.parametrize("version", ["2.0.0", "2.3.1"])
def test_direct_body_creation_rejects_old_solver_before_mesh(monkeypatch, version):
    """Public body creation reports the minimum version before touching native APIs."""
    from digitalmodel.hydrodynamics.capytaine import mesh_adapter

    native = SimpleNamespace(__version__=version)
    prepare_mesh = Mock(side_effect=AssertionError("mesh preparation was reached"))
    monkeypatch.setattr(mesh_adapter, "_import_capytaine", lambda: native)
    monkeypatch.setattr(mesh_adapter, "_create_predefined_mesh", prepare_mesh)
    with pytest.raises(RuntimeError, match=rf"{version}.*Capytaine 3 or later"):
        mesh_adapter.create_floating_body(BodyConfig(MeshConfig(predefined_shape="box")))
    prepare_mesh.assert_not_called()
