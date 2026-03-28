"""OC4 DeepCWind semi-submersible benchmark for Capytaine.

Validates that Capytaine can solve a realistic multi-column semi-sub
geometry (1069 panels) and produces physically reasonable results.

Reference: OC4 project (Robertson et al., 2014)
  Mass = 14,072,718 kg, CoG = (-0.01, 0, -9.89) m
  Water depth = 200 m, rho = 1025 kg/m^3

This test does NOT compare against OrcaWave quantitatively (would
require OrcFxAPI for .owd extraction). It validates:
  - Solver completes on a complex geometry
  - Hydrostatic stiffness is non-trivial
  - Added mass magnitude is in the right order for this structure
  - Physical symmetry properties hold
"""

import sys
from pathlib import Path

import numpy as np
import pytest

CAPYTAINE_ENV = "/mnt/local-analysis/capytaine-env/lib/python3.12/site-packages"
if CAPYTAINE_ENV not in sys.path:
    sys.path.insert(0, CAPYTAINE_ENV)

cpt = pytest.importorskip("capytaine")

from digitalmodel.hydrodynamics.capytaine import (
    BodyConfig,
    DOF,
    MeshConfig,
    MeshFormat,
    WaveConditions,
    run_bem_analysis,
    compute_rao,
)

# ---------------------------------------------------------------------------
# OC4 Semi-sub properties
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parents[3]
OC4_MESH_PATH = (
    REPO_ROOT
    / "docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub mesh.gdf"
)

OC4_MASS = 14_072_718.0  # kg
OC4_COG = (-0.010, 0.0, -9.890)  # m
OC4_WATER_DEPTH = 200.0  # m
OC4_RHO = 1025.0  # kg/m^3

# Inertia at CoG in kg·m^2 (from OrcaWave spec, converted from te·m^2)
OC4_IXX = 11.3274e9
OC4_IYY = 11.3172e9
OC4_IZZ = 12.2542e9


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def oc4_result():
    """Run Capytaine on OC4 semi-sub mesh with a sparse frequency set."""
    if not OC4_MESH_PATH.exists():
        pytest.skip(f"OC4 mesh not found: {OC4_MESH_PATH}")

    # Sparse periods: 5, 8, 12, 18, 25 s (covers wave period range of interest)
    periods = np.array([5.0, 8.0, 12.0, 18.0, 25.0])

    body_config = BodyConfig(
        mesh=MeshConfig(
            path=OC4_MESH_PATH,
            format=MeshFormat.GDF,
            name="OC4_Semisub",
        ),
        dofs=[DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW],
        center_of_mass=OC4_COG,
    )

    wave_conditions = WaveConditions(
        periods=periods,
        headings=np.deg2rad([0.0, 90.0]),
        water_depth=OC4_WATER_DEPTH,
        rho=OC4_RHO,
    )

    return run_bem_analysis(body_config, wave_conditions, compute_hydrostatics=True)


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestOC4SolveCompletion:
    """Verify solver handles the complex semi-sub geometry."""

    def test_solve_completes(self, oc4_result):
        """Solver should complete without error on 1069-panel semi-sub."""
        assert oc4_result.added_mass is not None
        assert oc4_result.radiation_damping is not None
        assert oc4_result.excitation_force is not None

    def test_correct_dimensions(self, oc4_result):
        """Output arrays should have correct shape."""
        n_omega = 5
        n_heading = 2
        n_dof = 6
        assert oc4_result.added_mass.shape == (n_omega, n_dof, n_dof)
        assert oc4_result.radiation_damping.shape == (n_omega, n_dof, n_dof)
        assert oc4_result.excitation_force.shape == (n_omega, n_heading, n_dof)

    def test_hydrostatics_computed(self, oc4_result):
        """Hydrostatic stiffness should be computed for the semi-sub."""
        assert oc4_result.hydrostatic_stiffness is not None
        # Heave restoring stiffness should be significant for a semi-sub
        # K33 = rho * g * waterplane_area — for OC4 this is O(10^6) N/m
        k33 = oc4_result.hydrostatic_stiffness[2, 2]
        assert k33 > 1e4, f"Heave stiffness too small: {k33:.2e}"


class TestOC4PhysicalSanity:
    """Physical sanity checks on the OC4 results."""

    def test_heave_added_mass_order_of_magnitude(self, oc4_result):
        """Heave added mass should be O(10^6-10^8) kg for a large semi-sub.

        The displaced volume is ~14000 m^3, so added mass should be
        comparable to displaced mass (~14e6 kg).
        """
        a33_mean = np.mean(np.abs(oc4_result.added_mass[:, 2, 2]))
        assert 1e5 < a33_mean < 1e9, (
            f"Heave added mass {a33_mean:.2e} kg outside expected range"
        )

    def test_surge_added_mass_order_of_magnitude(self, oc4_result):
        """Surge added mass should be O(10^6-10^8) kg."""
        a11_mean = np.mean(np.abs(oc4_result.added_mass[:, 0, 0]))
        assert 1e5 < a11_mean < 1e9, (
            f"Surge added mass {a11_mean:.2e} kg outside expected range"
        )

    def test_translational_damping_non_negative(self, oc4_result):
        """Translational radiation damping should be non-negative."""
        for i, dof in enumerate(["Surge", "Sway", "Heave"]):
            diag = oc4_result.radiation_damping[:, i, i]
            assert np.all(diag >= -1.0), (
                f"{dof} damping has significant negative: min={diag.min():.2e}"
            )

    def test_excitation_varies_with_heading(self, oc4_result):
        """Surge excitation at 0 deg should differ from 90 deg."""
        surge_h0 = np.abs(oc4_result.excitation_force[:, 0, 0])  # heading 0
        surge_h90 = np.abs(oc4_result.excitation_force[:, 1, 0])  # heading 90
        # At 90 deg, surge excitation should be much smaller than at 0 deg
        ratio = np.mean(surge_h90) / (np.mean(surge_h0) + 1e-10)
        assert ratio < 0.5, (
            f"Surge excitation ratio h90/h0 = {ratio:.3f} — expected < 0.5"
        )


class TestOC4RAO:
    """Test RAO computation for the OC4 semi-sub."""

    def test_rao_computable(self, oc4_result):
        """RAO should be computable from the BEM results."""
        rao_result = compute_rao(oc4_result)
        assert rao_result.rao is not None
        assert rao_result.rao_amplitude is not None

    def test_heave_rao_bounded(self, oc4_result):
        """Heave RAO amplitude should be bounded (no blow-up)."""
        rao_result = compute_rao(oc4_result)
        heave_idx = oc4_result.dof_names.index("Heave")
        heave_rao = rao_result.rao_amplitude[:, 0, heave_idx]
        # RAO should not exceed ~10 for a well-designed semi-sub
        assert np.all(heave_rao < 50), (
            f"Heave RAO too large: max={heave_rao.max():.2f}"
        )
