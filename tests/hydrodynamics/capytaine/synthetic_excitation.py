# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic excitation inputs and independent hand calculations.

The deliberately chosen complex pairs have magnitudes sqrt(real**2 + imag**2):
(3, 4) -> 5; (-5, 12) -> 13; (-8, -15) -> 17;
(7, -24) -> 25; (0, -6) -> 6; (-9, 0) -> 9;
(-20, 21) -> 29; (12, -35) -> 37; (-1, 1) -> sqrt(2);
(9, 40) -> 41; (-28, -45) -> 53; (0, 0) -> 0.
These are arithmetic test inputs, not physical reference results.

From omega*T = 2*pi radians per cycle, omega = pi and pi/2 rad/s give
T = 2 and 4 seconds respectively. From pi radians = 180 degrees, headings
pi/2 and -pi/6 radians display as 90 and -30 degrees. The literal expected
rows below round these hand calculations to the table's three decimal places;
sqrt(2) rounds to 1.414. No production conversion or magnitude helper is used.
"""

from math import pi

import numpy as np
import pytest
import xarray as xr

from digitalmodel.hydrodynamics.capytaine.models import BEMResult


@pytest.fixture
def synthetic_excitation():
    """Return labelled complex data and independently calculated display rows."""
    omegas = np.array([pi, pi / 2])
    headings = np.array([pi / 2, -pi / 6])
    dofs = ['Heave', 'Surge', 'Pitch']
    dataset = xr.Dataset(
        data_vars={
            'excitation_force': (
                ('omega', 'wave_direction', 'influenced_dof'),
                np.array([
                    [[3 + 4j, -5 + 12j, -8 - 15j],
                     [7 - 24j, -6j, -9 + 0j]],
                    [[-20 + 21j, 12 - 35j, -1 + 1j],
                     [9 + 40j, -28 - 45j, 0j]],
                ]),
            ),
        },
        coords={
            'omega': omegas,
            'wave_direction': headings,
            'influenced_dof': dofs,
        },
    )
    result = BEMResult(
        dataset=dataset,
        omegas=omegas,
        periods=2 * pi / omegas,
        headings=headings,
        dof_names=dofs,
    )
    expected_rows = {
        '90.0': [
            ['2.000', '5.000', '13.000', '17.000'],
            ['4.000', '29.000', '37.000', '1.414'],
        ],
        '-30.0': [
            ['2.000', '25.000', '6.000', '9.000'],
            ['4.000', '41.000', '53.000', '0.000'],
        ],
    }
    return result, expected_rows
