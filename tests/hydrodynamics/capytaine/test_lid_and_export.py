# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise explicit irregular-frequency treatment and native result readback.

Dimensional controls are synthetic test inputs, assumed, based on previous
experience. They exercise adapter behaviour independently of hull analyses.
"""


import numpy as np
import pytest
import xarray as xr

# Hosted checks do not install Capytaine, so this file is skipped there.
# Local: PYTHONPATH=src uv run --no-sources --with capytaine==3.0.0 python -m pytest \
#   tests/hydrodynamics/capytaine/test_lid_and_export.py -q
cpt = pytest.importorskip('capytaine')
pytestmark = pytest.mark.slow

from digitalmodel.hydrodynamics.capytaine.mesh_adapter import create_floating_body
from digitalmodel.hydrodynamics.capytaine.models import BEMResult, BodyConfig, MeshConfig
from digitalmodel.hydrodynamics.capytaine.results import export_netcdf


def box_config(**kwargs):
    """Return a small surface-piercing box with prescribed mass."""
    return BodyConfig(
        mesh=MeshConfig(predefined_shape='box', shape_params={
            'size': (2.0, 2.0, 2.0), 'resolution': (8, 8, 4),
        }),
        mass=1234.0,
        **kwargs,
    )


def test_requested_lid_is_attached():
    """A requested submerged lid is present in the returned body."""
    body = create_floating_body(box_config(use_lid=True, lid_z=-0.1))
    assert body.lid_mesh is not None
    assert body.lid_mesh.nb_faces > 0
    np.testing.assert_allclose(body.lid_mesh.vertices[:, 2], -0.1)


def test_empty_lid_raises():
    """A lid below the entire hull cannot silently become an untreated solve."""
    with pytest.raises(ValueError, match='lid'):
        create_floating_body(box_config(use_lid=True, lid_z=-2.0))


def test_prescribed_mass_is_retained():
    """The body retains the configured structural mass rather than inferring it."""
    assert create_floating_body(box_config()).mass == 1234.0


def test_native_export_retains_complex_excitation(tmp_path):
    """NetCDF round-trip retains native real and imaginary force components."""
    force = np.array([[[2.0 + 3.0j]]])
    dataset = xr.Dataset(
        {'excitation_force': (('omega', 'wave_direction', 'influenced_dof'), force)},
        coords={'omega': [0.5], 'wave_direction': [0.0], 'influenced_dof': ['Heave'], 'radiating_dof': ['Heave']},
    )
    path = export_netcdf(BEMResult(dataset=dataset), tmp_path / 'coefficients.nc')
    assert path.is_file()
    with xr.open_dataset(path) as stored:
        assert 'excitation_force' in stored
        assert stored.excitation_force.size == 2
        restored = stored.excitation_force.sel(complex='re') + 1j * stored.excitation_force.sel(complex='im')
        np.testing.assert_array_equal(restored.values, force)


@pytest.fixture(scope='module')
def gravity_result():
    """Solve one low-resolution box period with explicit dimensional properties."""
    from digitalmodel.hydrodynamics.capytaine.models import WaveConditions
    from digitalmodel.hydrodynamics.capytaine.solver import CapytaineSolver

    config = BodyConfig(
        mesh=MeshConfig(predefined_shape='box', shape_params={
            'size': (2.0, 2.0, 2.0), 'resolution': (2, 2, 2),
        }),
        mass=1234.0,
        inertia_matrix=np.diag([1234.0, 1234.0, 1234.0, 800.0, 900.0, 1000.0]),
    )
    waves = WaveConditions(periods=np.array([10.0]), g=9.80665)
    solver = CapytaineSolver(config, waves)
    return solver.solve(), solver.body, config


def test_gravity_reaches_boundary_element_problem(gravity_result):
    """The solved dataset uses the configured acceleration of gravity."""
    result, _, _ = gravity_result
    assert float(result.dataset.g) == 9.80665


def test_hydrostatics_use_configured_gravity(gravity_result):
    """The heave restoring coefficient uses the same gravity as the solve."""
    result, body, _ = gravity_result
    assert result.hydrostatic_stiffness[2, 2] == pytest.approx(
        1025.0 * 9.80665 * body.waterplane_area, rel=1e-12,
    )


def test_prescribed_inertia_is_retained(gravity_result):
    """The configured matrix survives hydrostatics and dataset assembly."""
    result, _, config = gravity_result
    np.testing.assert_array_equal(result.inertia_matrix, config.inertia_matrix)


def test_invalid_prescribed_inertia_raises():
    """Invalid user inertia cannot silently be replaced with inferred inertia."""
    from digitalmodel.hydrodynamics.capytaine.models import WaveConditions
    from digitalmodel.hydrodynamics.capytaine.solver import CapytaineSolver

    config = box_config(inertia_matrix=np.eye(3))
    solver = CapytaineSolver(config, WaveConditions(periods=np.array([10.0])))
    solver._body = create_floating_body(config)
    with pytest.raises(ValueError, match='inertia'):
        solver._compute_and_attach_hydrostatics(cpt)
