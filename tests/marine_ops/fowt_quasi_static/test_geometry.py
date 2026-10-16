# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Submerged union geometry, volume and reflection tests."""
from pathlib import Path

import numpy as np
import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]


def test_panel_coefficients_preserve_physical_reflection_symmetry():
    from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import solve_hydrodynamics, coefficient_arrays
    basis = yaml.safe_load((ROOT/"docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    dataset, _ = solve_hydrodynamics(basis, 6., np.array([.22, .23]), np.array([0., 30.]), n_threads=1)
    added, damping, excitation = coefficient_arrays(dataset)
    parity = np.diag([1., -1., 1., -1., 1., -1.])
    for array in (added, damping):
        assert abs(array-parity@array@parity).max()/abs(array).max() < 1e-10
    assert abs(excitation[:, 0, [1, 3, 5]]).max()/abs(excitation[:, 0]).max() < 1e-10


def test_submerged_union_volume_and_reflection():
    from digitalmodel.marine_ops.fowt_quasi_static.geometry import hull_mesh, analytical_volume
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    mesh = hull_mesh(basis, 4.0)
    exact = analytical_volume(basis)
    assert mesh.volume == pytest.approx(exact, rel=0.005)
    assert abs(exact / basis["published"]["displacement"]["value"] - 1) < 0.05
    center = np.sum(mesh.faces_areas[:, None] * mesh.faces_centers, axis=0)
    assert abs(center[1]) < 1e-7 * mesh.wet_surface_area


def test_surface_closure_and_waterplane_integrals():
    from digitalmodel.marine_ops.fowt_quasi_static.geometry import hull_mesh
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    mesh = hull_mesh(basis, 6.0)
    geometry = basis["geometry"]
    area = np.pi/4*(geometry["central_column_diameter"]["value"]**2
                   + 3*geometry["outer_column_diameter"]["value"]**2)
    np.testing.assert_allclose(mesh.volumes, mesh.volume, rtol=0.001)
    assert mesh.waterplane_area == pytest.approx(area, rel=0.005)


def test_horizontal_cap_panels_obey_the_specified_refinement_size():
    from digitalmodel.marine_ops.fowt_quasi_static.geometry import hull_mesh
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    step = 2.0
    mesh = hull_mesh(basis, step)
    assert mesh.faces_radiuses.max() < step


@pytest.mark.parametrize("threads", [None, 2])
def test_solver_thread_limit_reaches_both_frequency_kernels(monkeypatch, threads):
    import capytaine as cpt
    import xarray as xr
    from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import solve_hydrodynamics

    calls = []

    def capture(self, dataset, body, *, n_jobs, n_threads):
        calls.append((type(self.engine.green_function), n_jobs, n_threads))
        return xr.Dataset(coords={"omega": dataset.omega})

    monkeypatch.setattr(cpt.BEMSolver, "fill_dataset", capture)
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    kwargs = {} if threads is None else {"n_threads": threads}
    dataset, _ = solve_hydrodynamics(
        basis, 6., np.array([.02, .22]), np.array([0., 30.]), **kwargs)
    np.testing.assert_array_equal(dataset.omega, [.02, .22])
    expected = 1 if threads is None else threads
    assert calls == [(cpt.Delhommeau, 1, expected), (cpt.FinGreen3D, 1, expected)]


@pytest.mark.parametrize("threads", [0, -1, 1.5, True, "2", None])
def test_solver_rejects_invalid_thread_limits_before_meshing(monkeypatch, threads):
    from digitalmodel.marine_ops.fowt_quasi_static import hydrodynamics

    def unexpected_mesh(*args):
        pytest.fail("Invalid thread limit reached mesh construction")

    monkeypatch.setattr(hydrodynamics, "hull_mesh", unexpected_mesh)
    with pytest.raises(ValueError, match="positive integer"):
        hydrodynamics.solve_hydrodynamics(
            {}, 6., np.array([.22]), np.array([0.]), n_threads=threads)
