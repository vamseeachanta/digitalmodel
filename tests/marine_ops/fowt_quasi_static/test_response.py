# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Spectral integration and independent panel-solver convergence."""
import math

import numpy as np
import pytest

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def test_response_uses_the_boundary_element_phasor_convention():
    import xarray as xr
    from capytaine.post_pro.impedance import rao_transfer_function
    from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import DOFS
    from digitalmodel.marine_ops.fowt_quasi_static.response import response_operators
    omega = np.array([0.8])
    mass = np.eye(6)
    stiffness = np.diag(np.arange(2., 8.))
    stiffness[0, 2] = stiffness[2, 0] = 0.2
    damping = np.eye(6)[None]*0.4
    excitation = np.tile(np.arange(1., 7.)+1j*np.arange(6.), (1, 3, 1))
    modes = {"influenced_dof": DOFS, "radiating_dof": DOFS}
    dimensions = ("influenced_dof", "radiating_dof")
    dataset = xr.Dataset({
        "inertia_matrix": (dimensions, mass),
        "hydrostatic_stiffness": (dimensions, stiffness),
        "added_mass": (("omega", *dimensions), np.zeros((1, 6, 6))),
        "radiation_damping": (("omega", *dimensions), damping),
    }, coords={"omega": omega, **modes})
    transfer = rao_transfer_function(dataset).transpose("omega", *dimensions).values
    expected = np.linalg.solve(transfer[0], excitation[0].T).T
    actual = response_operators(omega, np.zeros((1, 6, 6)), damping,
                                excitation, mass, stiffness, 0.)
    np.testing.assert_allclose(actual[0], expected, rtol=1e-12, atol=1e-12)


def test_gamma_one_spectral_moments_match_closed_form():
    from digitalmodel.marine_ops.fowt_quasi_static.response import spectrum, wave_statistics
    omega = np.geomspace(0.0001, 1000, 100000)
    hs, tp = 2.0, 8.0
    density = spectrum(omega, hs, tp, 1.0)
    m0 = _trapezoid(density, omega)
    m2 = _trapezoid(omega**2*density, omega)
    wp = 2*math.pi/tp
    assert m0 == pytest.approx(hs**2/16, rel=0.001)
    assert m2 == pytest.approx(hs**2/16*math.sqrt(1.25*math.pi)*wp**2, rel=0.001)
    result = wave_statistics(omega, np.ones(len(omega)), density, 10800)
    assert result["significant"] == pytest.approx(hs/2, rel=0.001)
    expected = math.sqrt(2*m0*math.log(10800/(2*math.pi*math.sqrt(m0/m2))))
    assert result["maximum"] == pytest.approx(expected, rel=0.001)


def test_single_cylinder_heave_added_mass_mesh_convergence():
    from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import cylinder_added_mass
    coarse = cylinder_added_mass(32)
    fine = cylinder_added_mass(48)
    assert coarse == pytest.approx(fine, rel=0.02)


def test_low_frequency_finite_depth_radiation_is_computed():
    import capytaine as cpt
    from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import low_frequency_solver
    mesh = cpt.mesh_vertical_cylinder(radius=5, length=21, center=(0, 0, -9.5),
                                      resolution=(2, 16, 6)).immersed_part()
    body = cpt.FloatingBody(mesh=mesh, dofs=cpt.rigid_body_dofs(rotation_center=(0, 0, 0)))
    problem = cpt.RadiationProblem(body=body, radiating_dof="Surge", omega=0.02,
                                   water_depth=200, rho=1025, g=9.81)
    result = low_frequency_solver().solve(problem)
    assert np.isfinite(result.added_mass["Surge"])
    assert result.added_mass["Surge"] > 0


def test_natural_periods_retain_rigid_body_coupling():
    from digitalmodel.marine_ops.fowt_quasi_static.response import natural_periods
    omega = np.geomspace(0.1, 10, 100)
    mass = np.eye(6)
    stiffness = np.diag([4., 16., 9., 25., 36., 49.])
    stiffness[0, 2] = stiffness[2, 0] = 1.0
    added = np.zeros((len(omega), 6, 6))
    expected = 2*np.pi/np.sqrt([(13-math.sqrt(29))/2, 16,
                                (13+math.sqrt(29))/2, 25, 36, 49])
    np.testing.assert_allclose(natural_periods(omega, added, mass, stiffness), expected, rtol=1e-8)
