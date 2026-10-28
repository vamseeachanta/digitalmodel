# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent signed displacement controls for the public module report.

The source is elementary force balance, m*qdd + b*qd + k*q = F*exp(-i*w*t),
not a second implementation of the routine under test. These synthetic inputs
are specified controls, not measured or native boundary-element solver data.
For w=1 rad/s, M=2 kg, A=1 kg, K=5 N/m and B=1 N*s/m, the impedance is
2-i N/m. Forces +2 and -2 N give q=+/-((4+2i)/5) m. Adding 1 N*s/m damping
gives impedance 2-2i N/m and q=+/-((1+i)/2) m.

Unit attributes below declare the fixture's dimensional basis. The numeric
RAO API neither enforces these attributes nor supplies output unit metadata.
The quantity checked here is displacement in m; a wave-normalized RAO would
require an explicit wave-amplitude normalization.
"""

from math import atan, pi, sqrt

import numpy as np
import pytest
import xarray as xr

from digitalmodel.hydrodynamics.capytaine.models import BEMResult
from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual


def _signed_force_control():
    """Construct labeled arrays directly from the stated force-balance inputs."""
    matrix_dims = ("influenced_dof", "radiating_dof")
    dataset = xr.Dataset(
        data_vars={
            "added_mass": (("omega", *matrix_dims), [[[1.0]]], {"units": "kg"}),
            "radiation_damping": (
                ("omega", *matrix_dims), [[[1.0]]], {"units": "N*s/m"},
            ),
            "excitation_force": (
                ("omega", "wave_direction", "influenced_dof"),
                [[[2.0 + 0.0j], [-2.0 + 0.0j]]], {"units": "N"},
            ),
            "mass": (matrix_dims, [[2.0]], {"units": "kg"}),
            "stiffness": (matrix_dims, [[5.0]], {"units": "N/m"}),
        },
        coords={
            "omega": ("omega", [1.0], {"units": "rad/s"}),
            "wave_direction": ("wave_direction", [0.0, pi], {"units": "rad"}),
            "influenced_dof": ["Heave"],
            "radiating_dof": ["Heave"],
        },
    )
    return BEMResult(
        dataset=dataset,
        added_mass=dataset.added_mass.values.copy(),
        radiation_damping=dataset.radiation_damping.values.copy(),
        excitation_force=dataset.excitation_force.values.copy(),
        omegas=np.array([1.0]), periods=np.array([2.0 * pi]),
        headings=np.array([0.0, pi]), dof_names=["Heave"],
    )


@pytest.mark.parametrize(
    "extra_damping,positive_response,amplitude,positive_phase_deg",
    [
        (None, 4.0 / 5.0 + 2.0j / 5.0, 2.0 / sqrt(5.0), atan(0.5) * 180.0 / pi),
        (1.0, (1.0 + 1.0j) / 2.0, 1.0 / sqrt(2.0), 45.0),
    ],
    ids=["radiation-damping", "radiation-plus-extra-damping"],
)
def test_signed_manual_response_matches_independent_force_balance(
    extra_damping, positive_response, amplitude, positive_phase_deg,
):
    """Detect damping-sign, force-sign, phase-branch and heading-axis changes."""
    bem = _signed_force_control()
    extra = None if extra_damping is None else np.array([[extra_damping]])
    result = compute_rao_manual(
        bem, bem.dataset.mass.values, bem.dataset.stiffness.values, extra,
    )
    expected = np.array([[[positive_response], [-positive_response]]])
    expected_amplitude = np.array([[[amplitude], [amplitude]]])
    expected_phase_deg = np.array([
        [[positive_phase_deg], [positive_phase_deg - 180.0]],
    ])
    # Exact hand fractions, square roots and angles permit only double-precision
    # linear arithmetic and radian-to-degree rounding, not model uncertainty.
    np.testing.assert_allclose(result.rao, expected, rtol=0.0, atol=1e-12)
    np.testing.assert_allclose(
        result.rao_amplitude, expected_amplitude, rtol=0.0, atol=1e-12,
    )
    np.testing.assert_allclose(
        result.rao_phase, expected_phase_deg, rtol=0.0, atol=1e-12,
    )
    for values in (result.rao, result.rao_amplitude, result.rao_phase):
        assert values.shape == (1, 2, 1)
    np.testing.assert_array_equal(result.omegas, np.array([1.0]))
    np.testing.assert_array_equal(result.periods, np.array([2.0 * pi]))
    np.testing.assert_array_equal(result.headings, np.array([0.0, pi]))
    assert result.dof_names == ["Heave"]
    assert bem.dataset.added_mass.dims == ("omega", "influenced_dof", "radiating_dof")
    assert bem.dataset.excitation_force.dims == (
        "omega", "wave_direction", "influenced_dof",
    )
    assert bem.dataset.omega.attrs["units"] == "rad/s"
    assert bem.dataset.wave_direction.attrs["units"] == "rad"
    assert {
        name: bem.dataset[name].attrs["units"]
        for name in (
            "mass", "added_mass", "stiffness", "radiation_damping", "excitation_force",
        )
    } == {
        "mass": "kg", "added_mass": "kg", "stiffness": "N/m",
        "radiation_damping": "N*s/m", "excitation_force": "N",
    }
