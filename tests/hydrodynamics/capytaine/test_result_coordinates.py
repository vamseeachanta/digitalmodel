# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Preserve coordinate order and dimensional properties in result extraction."""

import numpy as np
import pytest
import xarray as xr

from digitalmodel.hydrodynamics.capytaine.models import (
    BodyConfig, DOF, MeshConfig, WaveConditions,
)
from digitalmodel.hydrodynamics.capytaine.solver import CapytaineSolver
from digitalmodel.hydrodynamics.capytaine.results import excitation_force_table

from .synthetic_excitation import synthetic_excitation


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


@pytest.mark.parametrize('reverse_omega', [False, True], ids=['omega-kept', 'omega-reversed'])
@pytest.mark.parametrize('reverse_heading', [False, True], ids=['heading-kept', 'heading-reversed'])
@pytest.mark.parametrize('reverse_dof', [False, True], ids=['dof-kept', 'dof-reversed'])
@pytest.mark.parametrize('transpose', [False, True], ids=['dims-kept', 'dims-transposed'])
def test_excitation_table_follows_labels_and_display_units(
    synthetic_excitation, reverse_omega, reverse_heading, reverse_dof, transpose,
):
    """Display the fixture's hand calculations regardless of storage order."""
    result, expected_rows = synthetic_excitation
    # Reverse each coordinate independently; transpose also changes dimension order.
    result.dataset = result.dataset.isel({
        'omega': slice(None, None, -1 if reverse_omega else 1),
        'wave_direction': slice(None, None, -1 if reverse_heading else 1),
        'influenced_dof': slice(None, None, -1 if reverse_dof else 1),
    })
    if transpose:
        result.dataset = result.dataset.transpose(
            'influenced_dof', 'omega', 'wave_direction',
        )
    original = result.dataset.copy(deep=True)

    # The public default selects the first requested heading, even if stored last.
    assert excitation_force_table(result) == excitation_force_table(result, heading_idx=0)
    for heading_idx, (heading_degrees, rows) in enumerate(expected_rows.items()):
        lines = excitation_force_table(result, heading_idx=heading_idx).splitlines()
        assert lines[0] == f'Excitation Forces — heading = {heading_degrees} deg'
        assert lines[2].split() == ['Period', '(s)', '|F_Heave|', '|F_Surge|', '|F_Pitch|']
        assert [line.split() for line in lines[4:]] == rows

    xr.testing.assert_identical(result.dataset, original)
    assert np.iscomplexobj(result.dataset['excitation_force'].values)
