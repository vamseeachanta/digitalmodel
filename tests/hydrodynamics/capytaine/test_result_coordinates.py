# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Preserve coordinate order and dimensional properties in result extraction."""

import numpy as np
import xarray as xr

from digitalmodel.hydrodynamics.capytaine.models import (
    BodyConfig, DOF, MeshConfig, WaveConditions,
)
from digitalmodel.hydrodynamics.capytaine.solver import CapytaineSolver


def test_coefficients_follow_requested_coordinates():
    """Return forces by influenced row and motion column, in requested order."""
    dofs = [DOF.SURGE, DOF.HEAVE]
    waves = WaveConditions(omegas=np.array([0.3, 0.1]), headings=np.array([1.0, 0.0]))
    solver = CapytaineSolver(BodyConfig(MeshConfig(predefined_shape='box'), dofs=dofs), waves)
    stored_dofs = ['Heave', 'Surge']
    raw = np.arange(8.0).reshape(2, 2, 2)
    force = np.arange(8.0).reshape(2, 2, 2).astype(complex)
    solver._dataset = xr.Dataset(
        data_vars={
            'added_mass': (('omega', 'radiating_dof', 'influenced_dof'), raw),
            'radiation_damping': (('omega', 'radiating_dof', 'influenced_dof'), raw + 10.0),
            'excitation_force': (('omega', 'wave_direction', 'influenced_dof'), force),
        },
        coords={'omega': [0.1, 0.3], 'wave_direction': [0.0, 1.0],
                'radiating_dof': stored_dofs, 'influenced_dof': stored_dofs},
    )
    result = solver._extract_results()
    expected = raw[::-1, ::-1, ::-1].transpose(0, 2, 1)
    np.testing.assert_array_equal(result.added_mass, expected)
    np.testing.assert_array_equal(result.radiation_damping, expected + 10.0)
    np.testing.assert_array_equal(result.excitation_force, force[::-1, ::-1, ::-1])
    np.testing.assert_array_equal(result.dataset.omega.values, waves.omega_array)
