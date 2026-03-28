"""WAMIT Case 2.1 benchmark: circular cylinder R=1, T=0.5, infinite depth.

Compares Capytaine BEM results against OrcaWave reference data extracted
from the WAMIT validation suite (L00_validation_wamit/2.1).

Reference geometry: bi-symmetric cylinder, 256 panels (quarter mesh),
50 frequencies 0.1-5.0 rad/s, infinite depth, rho=1000 kg/m^3.

The OrcaWave reference data is stored in OrcaFlex units:
  - added mass: te (1 te = 1000 kg)
  - damping: kN·s/m (1 kN·s/m = 1000 N·s/m)
  - RAO translation: m/m, RAO rotation: deg/m
"""

import sys
from pathlib import Path

import numpy as np
import pytest
import yaml

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
)

# ---------------------------------------------------------------------------
# Paths (relative to repo root)
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parents[3]
HYDRO_DATA_PATH = (
    REPO_ROOT
    / "docs/domains/orcawave/L00_validation_wamit/2.1/benchmark/hydro_data.yml"
)
MESH_PATH = (
    REPO_ROOT
    / "docs/domains/orcawave/L00_validation_wamit/2.1/benchmark/spec_orcawave/val_cylinder_r1_t05.gdf"
)

# ---------------------------------------------------------------------------
# Reference data loader
# ---------------------------------------------------------------------------


def _load_reference_data():
    """Load OrcaWave reference hydrodynamic data for case 2.1."""
    with open(HYDRO_DATA_PATH) as f:
        data = yaml.safe_load(f)

    # Use first solver entry (OrcaWave .owd)
    solver_key = list(data["solvers"].keys())[0]
    solver_data = data["solvers"][solver_key]

    omegas = np.array(data["grid"]["frequencies_rad_s"])

    # Added mass: list of 6x6 matrices per frequency, units = te
    am_raw = solver_data["added_mass"]
    added_mass = np.array(am_raw) * 1000.0  # te → kg

    # Damping: list of 6x6 matrices per frequency, units = kN·s/m
    damp_raw = solver_data["damping"]
    damping = np.array(damp_raw) * 1000.0  # kN·s/m → N·s/m

    # RAO surge magnitude: [n_freq × n_heading]
    surge_rao = np.array(solver_data["raos"]["surge"]["magnitude"])
    heave_rao = np.array(solver_data["raos"]["heave"]["magnitude"])

    return {
        "omegas": omegas,
        "added_mass": added_mass,  # (n_freq, 6, 6) in kg
        "damping": damping,  # (n_freq, 6, 6) in N·s/m
        "surge_rao": surge_rao,  # (n_freq, n_heading) m/m
        "heave_rao": heave_rao,  # (n_freq, n_heading) m/m
    }


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def reference():
    """Load reference data once for all tests in this module."""
    if not HYDRO_DATA_PATH.exists():
        pytest.skip(f"Reference data not found: {HYDRO_DATA_PATH}")
    return _load_reference_data()


@pytest.fixture(scope="module")
def capytaine_result():
    """Run Capytaine on the cylinder mesh — subset of frequencies for speed."""
    if not MESH_PATH.exists():
        pytest.skip(f"Mesh file not found: {MESH_PATH}")

    # Use every 5th frequency for speed (10 out of 50)
    omegas = np.arange(0.5, 5.1, 0.5)

    body_config = BodyConfig(
        mesh=MeshConfig(
            path=MESH_PATH,
            format=MeshFormat.GDF,
            name="cylinder_r1_t05",
        ),
        dofs=list(DOF),  # all 6 DOFs
        center_of_mass=(0.0, 0.0, 0.0),
    )

    wave_conditions = WaveConditions(
        omegas=omegas,
        headings=np.array([0.0]),
        water_depth=np.inf,
        rho=1000.0,
        g=9.80665,
    )

    return run_bem_analysis(body_config, wave_conditions, compute_hydrostatics=True)


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestCylinderAddedMass:
    """Compare Capytaine added mass against OrcaWave reference."""

    def test_surge_added_mass_correlation(self, capytaine_result, reference):
        """Surge-surge added mass should correlate strongly with reference."""
        ref_omegas = reference["omegas"]
        ref_a11 = reference["added_mass"][:, 0, 0]  # surge-surge

        # Interpolate reference to Capytaine frequencies
        cpt_omegas = capytaine_result.omegas
        ref_interp = np.interp(cpt_omegas, ref_omegas, ref_a11)
        cpt_a11 = capytaine_result.added_mass[:, 0, 0]  # surge-surge

        corr = np.corrcoef(cpt_a11, ref_interp)[0, 1]
        assert corr > 0.95, f"Surge added mass correlation: {corr:.4f} (expected > 0.95)"

    def test_heave_added_mass_correlation(self, capytaine_result, reference):
        """Heave-heave added mass should correlate strongly with reference."""
        ref_omegas = reference["omegas"]
        ref_a33 = reference["added_mass"][:, 2, 2]  # heave-heave

        cpt_omegas = capytaine_result.omegas
        ref_interp = np.interp(cpt_omegas, ref_omegas, ref_a33)
        cpt_a33 = capytaine_result.added_mass[:, 2, 2]

        corr = np.corrcoef(cpt_a33, ref_interp)[0, 1]
        assert corr > 0.95, f"Heave added mass correlation: {corr:.4f} (expected > 0.95)"

    def test_heave_added_mass_magnitude(self, capytaine_result, reference):
        """Heave added mass magnitude should be within 20% of reference at low freq."""
        ref_omegas = reference["omegas"]
        ref_a33 = reference["added_mass"][:, 2, 2]

        # Compare at omega ≈ 0.5 (low frequency, well resolved)
        idx_ref = np.argmin(np.abs(ref_omegas - 0.5))
        idx_cpt = np.argmin(np.abs(capytaine_result.omegas - 0.5))

        ref_val = ref_a33[idx_ref]
        cpt_val = capytaine_result.added_mass[idx_cpt, 2, 2]

        rel_err = abs(cpt_val - ref_val) / abs(ref_val)
        assert rel_err < 0.20, (
            f"Heave A33 at omega=0.5: Capytaine={cpt_val:.2f} vs "
            f"OrcaWave={ref_val:.2f} (error: {rel_err:.1%})"
        )


class TestCylinderDamping:
    """Compare Capytaine radiation damping against OrcaWave reference."""

    def test_heave_damping_correlation(self, capytaine_result, reference):
        """Heave radiation damping should correlate with reference."""
        ref_omegas = reference["omegas"]
        ref_b33 = reference["damping"][:, 2, 2]

        cpt_omegas = capytaine_result.omegas
        ref_interp = np.interp(cpt_omegas, ref_omegas, ref_b33)
        cpt_b33 = capytaine_result.radiation_damping[:, 2, 2]

        corr = np.corrcoef(cpt_b33, ref_interp)[0, 1]
        assert corr > 0.90, f"Heave damping correlation: {corr:.4f} (expected > 0.90)"

    def test_damping_mostly_non_negative(self, capytaine_result):
        """Translational damping (surge/sway/heave) should be non-negative.

        Rotational DOFs may show small negative values at irregular
        frequencies — this test excludes them since the reference case
        runs without irregular frequency removal.
        """
        for i in range(3):  # surge, sway, heave only
            diag = capytaine_result.radiation_damping[:, i, i]
            assert np.all(diag >= -1e-6), (
                f"DOF {i} has negative damping: min={diag.min():.6f}"
            )


class TestCylinderExcitation:
    """Verify excitation forces are physically reasonable."""

    def test_excitation_nonzero_surge_heave(self, capytaine_result):
        """Surge and heave should have nonzero excitation at heading=0.

        Sway, roll, yaw are physically zero for a symmetric body in
        head-on waves — not a solver error.
        """
        for dof in ("Surge", "Heave"):
            idx = capytaine_result.dof_names.index(dof)
            exc = np.abs(capytaine_result.excitation_force[:, 0, idx])
            assert np.any(exc > 1e-10), f"{dof} excitation is all zero"

    def test_heave_excitation_peak(self, capytaine_result):
        """Heave excitation should have a well-defined peak (resonance region)."""
        heave_idx = capytaine_result.dof_names.index("Heave")
        exc_heave = np.abs(capytaine_result.excitation_force[:, 0, heave_idx])
        assert exc_heave.max() > exc_heave.min() * 1.5, (
            "Heave excitation should show frequency dependence"
        )


class TestCylinderSymmetry:
    """Verify physical symmetry properties of the solution."""

    def test_surge_sway_symmetry(self, capytaine_result):
        """For a circular cylinder, surge and sway added mass should be equal."""
        a11 = capytaine_result.added_mass[:, 0, 0]  # surge
        a22 = capytaine_result.added_mass[:, 1, 1]  # sway

        rel_diff = np.abs(a11 - a22) / (np.abs(a11) + 1e-10)
        assert np.all(rel_diff < 0.05), (
            f"Surge/sway asymmetry: max relative diff = {rel_diff.max():.4f}"
        )

    def test_roll_pitch_symmetry(self, capytaine_result):
        """For a circular cylinder, roll and pitch added mass should be equal."""
        a44 = capytaine_result.added_mass[:, 3, 3]  # roll
        a55 = capytaine_result.added_mass[:, 4, 4]  # pitch

        rel_diff = np.abs(a44 - a55) / (np.abs(a44) + 1e-10)
        assert np.all(rel_diff < 0.05), (
            f"Roll/pitch asymmetry: max relative diff = {rel_diff.max():.4f}"
        )
