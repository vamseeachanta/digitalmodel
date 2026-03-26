"""Sphere analytical benchmark for Capytaine BEM module.

Validates the Capytaine solver against the analytical solution for a
half-submerged sphere (Hulme 1982, Yeung 1981). The heave added mass of a
hemisphere of radius R at zero frequency approaches (2/3)*rho*pi*R^3.

This benchmark is a standard validation case used throughout the BEM
hydrodynamics community (e.g., WAMIT, AQWA, Nemoh validation reports).

References:
    - DNV-RP-C205 §7.1 — Potential flow theory
    - Hulme, A. (1982) — "The wave forces acting on a floating hemisphere
      undergoing forced periodic oscillations"
"""

import sys

import numpy as np
import pytest

# Capytaine env may be at a non-standard location
CAPYTAINE_ENV = "/mnt/local-analysis/capytaine-env/lib/python3.12/site-packages"
if CAPYTAINE_ENV not in sys.path:
    sys.path.insert(0, CAPYTAINE_ENV)

# Skip all tests if capytaine not available
cpt = pytest.importorskip("capytaine")

from digitalmodel.hydrodynamics.capytaine import (
    BodyConfig,
    CapytaineSolver,
    DOF,
    MeshConfig,
    WaveConditions,
    compute_rao,
    run_bem_analysis,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

SPHERE_RADIUS = 5.0
SPHERE_CENTER = (0, 0, 0)
RHO = 1025.0


@pytest.fixture
def sphere_body_config():
    """Half-submerged sphere body config for BEM analysis."""
    return BodyConfig(
        mesh=MeshConfig(
            predefined_shape="sphere",
            shape_params={
                "radius": SPHERE_RADIUS,
                "center": SPHERE_CENTER,
                "name": "test_sphere",
                "resolution": (30, 30),  # finer mesh for analytical accuracy
            },
            name="test_sphere",
        ),
        dofs=[DOF.HEAVE],
        center_of_mass=SPHERE_CENTER,
    )


@pytest.fixture
def wave_conditions():
    """Standard wave conditions for sphere test."""
    return WaveConditions.from_omega_range(
        omega_min=0.3,
        omega_max=3.0,
        n_omega=15,
        headings_deg=[0.0],
        water_depth=np.inf,
        rho=RHO,
    )


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestSphereValidation:
    """Validate Capytaine solver against analytical sphere solution."""

    def test_solver_runs_without_error(self, sphere_body_config, wave_conditions):
        """Solver should complete without raising exceptions."""
        result = run_bem_analysis(sphere_body_config, wave_conditions)
        assert result.added_mass is not None
        assert result.radiation_damping is not None
        assert result.excitation_force is not None

    def test_added_mass_shape(self, sphere_body_config, wave_conditions):
        """Added mass array should have correct dimensions."""
        result = run_bem_analysis(sphere_body_config, wave_conditions)
        n_omega = len(wave_conditions.omega_array)
        n_dof = len(sphere_body_config.dofs)
        assert result.added_mass.shape == (n_omega, n_dof, n_dof)

    def test_heave_added_mass_analytical(self):
        """Heave added mass should approach analytical value at low frequency.

        For a half-submerged sphere of radius R:
            A_33(omega→0) ≈ (2/3) * rho * pi * R^3

        We use a very low frequency (omega=0.1) and 15% tolerance to account
        for mesh discretization and finite-frequency effects.
        """
        body_cfg = BodyConfig(
            mesh=MeshConfig(
                predefined_shape="sphere",
                shape_params={
                    "radius": SPHERE_RADIUS,
                    "center": SPHERE_CENTER,
                    "name": "test_sphere_analytical",
                    "resolution": (40, 40),
                },
                name="test_sphere_analytical",
            ),
            dofs=[DOF.HEAVE],
            center_of_mass=SPHERE_CENTER,
        )
        low_freq_wc = WaveConditions(
            omegas=np.array([0.1, 0.2, 0.3]),
            headings=np.array([0.0]),
            water_depth=np.inf,
            rho=RHO,
        )

        result = run_bem_analysis(body_cfg, low_freq_wc)

        # Analytical heave added mass for hemisphere
        analytical_a33 = (2.0 / 3.0) * RHO * np.pi * SPHERE_RADIUS**3

        # Added mass at lowest frequency (closest to omega=0 limit)
        a33_low_freq = result.added_mass[0, 0, 0]  # first omega, heave-heave

        relative_error = abs(a33_low_freq - analytical_a33) / analytical_a33
        assert relative_error < 0.15, (
            f"Heave added mass at low freq: {a33_low_freq:.1f} vs "
            f"analytical: {analytical_a33:.1f} (error: {relative_error:.1%})"
        )

    def test_radiation_damping_positive(self, sphere_body_config, wave_conditions):
        """Radiation damping must be non-negative for all frequencies."""
        result = run_bem_analysis(sphere_body_config, wave_conditions)
        assert np.all(result.radiation_damping >= -1e-10), (
            "Radiation damping should be non-negative"
        )

    def test_excitation_force_nonzero(self, sphere_body_config, wave_conditions):
        """Excitation force should be nonzero for a body in waves."""
        result = run_bem_analysis(sphere_body_config, wave_conditions)
        exc_amp = np.abs(result.excitation_force)
        assert np.all(exc_amp > 0), "Excitation force should be nonzero"

    def test_hydrostatic_stiffness_computed(self, sphere_body_config, wave_conditions):
        """Hydrostatic stiffness should be computed when requested."""
        result = run_bem_analysis(sphere_body_config, wave_conditions, compute_hydrostatics=True)
        assert result.hydrostatic_stiffness is not None

    def test_dataset_contains_expected_variables(self, sphere_body_config, wave_conditions):
        """The xarray dataset should contain standard BEM output variables."""
        result = run_bem_analysis(sphere_body_config, wave_conditions)
        ds = result.dataset
        expected_vars = {"added_mass", "radiation_damping", "excitation_force"}
        assert expected_vars.issubset(set(ds.data_vars)), (
            f"Missing variables: {expected_vars - set(ds.data_vars)}"
        )


class TestSolverValidation:
    """Test input validation logic."""

    def test_rejects_negative_frequencies(self):
        with pytest.raises(ValueError, match="positive"):
            solver = CapytaineSolver(
                body_config=BodyConfig(
                    mesh=MeshConfig(predefined_shape="sphere", shape_params={"radius": 1.0}),
                    dofs=[DOF.HEAVE],
                ),
                wave_conditions=WaveConditions(omegas=np.array([-1.0, 0.5, 1.0])),
            )
            solver.solve()

    def test_rejects_empty_dofs(self):
        with pytest.raises(ValueError, match="DOF"):
            solver = CapytaineSolver(
                body_config=BodyConfig(
                    mesh=MeshConfig(predefined_shape="sphere", shape_params={"radius": 1.0}),
                    dofs=[],
                ),
                wave_conditions=WaveConditions(omegas=np.array([1.0])),
            )
            solver.solve()


class TestRAOComputation:
    """Test RAO computation from BEM results."""

    def test_rao_from_bem_result(self, sphere_body_config, wave_conditions):
        """RAO should be computable from BEM results with hydrostatics."""
        result = run_bem_analysis(sphere_body_config, wave_conditions, compute_hydrostatics=True)
        rao_result = compute_rao(result)

        assert rao_result.rao is not None
        assert rao_result.rao_amplitude is not None
        assert rao_result.rao_phase is not None

    def test_heave_rao_at_long_period(self, sphere_body_config, wave_conditions):
        """Heave RAO should approach 1.0 at very long periods (quasi-static limit)."""
        # Use longer periods to approach quasi-static
        long_period_wc = WaveConditions.from_period_range(
            t_min=10.0,
            t_max=30.0,
            n_periods=10,
            headings_deg=[0.0],
            water_depth=np.inf,
            rho=RHO,
        )
        result = run_bem_analysis(sphere_body_config, long_period_wc, compute_hydrostatics=True)
        rao_result = compute_rao(result)

        # At very long periods, heave RAO amplitude → 1.0
        heave_rao_long = rao_result.rao_amplitude[-1, 0, 0]  # longest period
        assert 0.5 < heave_rao_long < 1.5, (
            f"Heave RAO at T={long_period_wc.period_array[-1]:.1f}s: {heave_rao_long:.3f} "
            f"(expected ~1.0)"
        )
