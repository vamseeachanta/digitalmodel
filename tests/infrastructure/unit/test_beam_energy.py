# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for Euler-Bernoulli bending energy.

For v(x) = a + b*x + kappa*x**2/2, theta = dv/dx = b + kappa*x
and v'' = kappa. Thus integral(E*I*v''**2/2, x=0..L) = E*I*kappa**2*L/2.
Setting kappa to zero gives rigid translation, infinitesimal rigid rotation,
or their superposition. Endpoint values below follow these polynomials,
without using element strain recovery or stiffness coefficients as an oracle.

All material, section, length, curvature, displacement and slope values are
synthetic test inputs in SI units, not design data. The two cases deliberately
vary E, I, L and the sign of curvature. Rotation directions use the independent
3-4-5 triangle (including reflection) and the vertical unit vector.
The factors 1/2 and powers 2 follow the hand calculation above; 64 machine
epsilons allow roundoff in the small matrix products and trigonometry.
"""

import numpy as np
import pytest

from digitalmodel.infrastructure.base_solvers.structural.elements import BeamElement


@pytest.fixture(params=[(120e9, 6e-6, 2.5, 0.004), (73e9, 2.5e-6, 4.0, -0.003)])
def bending_case(request):
    """Return independently specified E [Pa], I [m^4], L [m], kappa [1/m]."""
    return request.param


def _beam(case, direction=(1.0, 0.0)):
    youngs_modulus, inertia, length, _ = case
    return BeamElement(
        elem_id=0,
        node1=0,
        node2=1,
        coord1=np.zeros(2),
        coord2=length * np.array(direction),
        E=youngs_modulus,
        I=inertia,
        # Synthetic area [m^2]; unused in the bending-energy integral.
        A=0.01,
    )


def _endpoints(length, curvature, offset=0.003, slope=-0.002):
    """Evaluate v and dv/dx at each end, ordered [v(0), theta(0), v(L), theta(L)]."""
    return np.array([
        offset,
        slope,
        offset + slope * length + curvature * length**2 / 2,
        slope + curvature * length,
    ])


def _assert_energy(stiffness, displacement, expected):
    energy = displacement @ stiffness @ displacement / 2
    # Absolute sum bounds cancellation error, including the zero-energy modes.
    magnitude = np.sum(np.abs(np.outer(displacement, displacement) * stiffness)) / 2
    tolerance = 64 * np.finfo(float).eps * magnitude
    assert energy == pytest.approx(expected, rel=0, abs=tolerance)


def test_beam_constant_curvature_energy(bending_case):
    """The quadratic displacement field stores the integrated bending energy."""
    youngs_modulus, inertia, length, curvature = bending_case
    displacement = _endpoints(length, curvature)
    expected = youngs_modulus * inertia * curvature**2 * length / 2

    _assert_energy(_beam(bending_case).compute_local_stiffness_matrix(), displacement, expected)


@pytest.mark.parametrize(
    "offset,slope",
    [(0.003, 0.0), (0.0, -0.002), (0.003, -0.002)],
    ids=["translation", "rotation", "translation-and-rotation"],
)
def test_beam_rigid_body_modes_have_zero_energy(bending_case, offset, slope):
    """Linear v has zero curvature, energy and restoring nodal force."""
    _, _, length, _ = bending_case
    displacement = _endpoints(length, curvature=0.0, offset=offset, slope=slope)
    stiffness = _beam(bending_case).compute_local_stiffness_matrix()

    _assert_energy(stiffness, displacement, expected=0.0)
    force_tolerance = (
        64 * np.finfo(float).eps
        * np.linalg.norm(stiffness, ord=np.inf)
        * np.linalg.norm(displacement, ord=np.inf)
    )
    np.testing.assert_allclose(stiffness @ displacement, 0.0, rtol=0, atol=force_tolerance)


@pytest.mark.parametrize("direction", [(3 / 5, 4 / 5), (3 / 5, -4 / 5), (0.0, 1.0)])
def test_rotated_beam_preserves_energy(bending_case, direction):
    """The public reduced-coordinate transformation preserves the hand integral.

    This checks the exposed four-coordinate change of basis, not a frame with
    independent axial and transverse translations at both nodes. For the
    orthogonal public map u_local = T*u_global, u_global = T.T*u_local.
    Nonzero first-end displacement and slope exercise the rotated coordinates.
    """
    youngs_modulus, inertia, length, curvature = bending_case
    local_displacement = _endpoints(length, curvature)
    expected = youngs_modulus * inertia * curvature**2 * length / 2
    beam = _beam(bending_case, direction)
    transform = beam.compute_transformation_matrix()
    global_displacement = transform.T @ local_displacement

    # An identity transformation would make the test insensitive to inclination.
    assert not np.allclose(global_displacement, local_displacement)
    np.testing.assert_allclose(
        transform @ global_displacement,
        local_displacement,
        rtol=64 * np.finfo(float).eps,
        atol=0,
    )
    _assert_energy(beam.compute_local_stiffness_matrix(), local_displacement, expected)
    _assert_energy(beam.compute_global_stiffness_matrix(), global_displacement, expected)
