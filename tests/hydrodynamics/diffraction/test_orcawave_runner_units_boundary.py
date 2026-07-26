"""OrcFxAPI -> DiffractionResults unit boundary (#1550 W2).

`DiffractionResults` is an SI/kg type: `aqwa_converter`, `solver.orcawave_converter`
and `wamit_reference_loader` all populate it in kg (the last explicitly
dimensionalising with `rho = 1025.0 kg/m^3`). `orcawave_runner` was the only
producer placing OrcFxAPI's native tonne-based values into it while declaring
kg labels, so its numbers were 1000x low against every sibling producer.

OrcFxAPI native units, per Orcina and confirmed against the unit-cylinder
benchmark in docs/domains/orcawave/L00_validation_wamit/2.1:
  .addedMass  te, te.m, te.m^2   (mass basis -> uniform 1000x to kg)
  .damping    te/s               (kg/s == N.s/m dimensionally -> same 1000x)

The factor is uniform across all three coupling blocks because the tonne->kg
conversion is a mass conversion; the length exponent is identical on both sides.
"""
from __future__ import annotations

from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner


N_FREQ = 3
N_HEAD = 2

# Distinct so a wrong transpose cannot pass silently.
assert N_FREQ != N_HEAD


def _fake_diffraction() -> SimpleNamespace:
    """Minimal OrcFxAPI-shaped result object with known tonne-based values."""
    # Hz, DESCENDING - as OrcFxAPI returns them.
    frequencies = np.array([0.30, 0.20, 0.10], dtype=float)
    headings = np.array([0.0, 90.0], dtype=float)

    # Added mass in te: block-distinguishable, ascending with frequency index.
    added_mass = np.zeros((N_FREQ, 6, 6), dtype=float)
    damping = np.zeros((N_FREQ, 6, 6), dtype=float)
    for k in range(N_FREQ):
        added_mass[k, 0, 0] = 1000.0 + k       # te, linear-linear
        added_mass[k, 0, 3] = 10.0 + k         # te.m, linear-angular
        added_mass[k, 3, 3] = 500.0 + k        # te.m^2, angular-angular
        damping[k, 0, 0] = 2.0 + k             # te/s
        damping[k, 3, 3] = 4.0 + k             # te.m^2/s

    raos = np.zeros((N_HEAD, N_FREQ, 6), dtype=complex)

    return SimpleNamespace(
        frequencies=frequencies,
        headings=headings,
        addedMass=added_mass,
        damping=damping,
        displacementRAOs=raos,
    )


def _runner() -> OrcaWaveRunner:
    """A runner instance sufficient for the results builder, without __init__."""
    runner = object.__new__(OrcaWaveRunner)
    runner._result = SimpleNamespace(spec_name="UnitBoundaryVessel")
    runner._water_depth = 200.0
    runner._extract_hydrostatics = lambda *a, **k: None
    return runner


@pytest.fixture
def built_results():
    return _runner()._build_results_from_object(_fake_diffraction(), [])


class TestAddedMassUnitBoundary:
    """Added mass must leave the boundary in the units it is labelled with."""

    def test_added_mass_converted_to_kg(self, built_results) -> None:
        # Frequencies were descending in Hz; index 0 is now the LOWEST rad/s,
        # which is the last tonne value written (k = 2).
        matrices = built_results.added_mass.matrices
        surge = np.array([m.matrix[0, 0] for m in matrices])

        # te values were 1000, 1001, 1002 -> kg must be 1000x those.
        assert np.allclose(np.sort(surge), [1000e3, 1001e3, 1002e3])

    def test_added_mass_units_declare_kg(self, built_results) -> None:
        units = built_results.added_mass.matrices[0].units
        assert units["linear"] == "kg"
        assert units["angular"] == "kg.m^2"

    def test_all_coupling_blocks_scale_uniformly(self, built_results) -> None:
        m = built_results.added_mass.matrices[0].matrix
        # Every block is a mass conversion: same 1000x, whatever the length term.
        assert m[0, 0] == pytest.approx(1000e3, rel=1e-9) or m[0, 0] == pytest.approx(
            1002e3, rel=1e-9
        )
        assert m[0, 3] / m[0, 0] == pytest.approx(
            (10.0 + 2) / (1000.0 + 2), rel=1e-6
        ) or m[0, 3] / m[0, 0] == pytest.approx((10.0) / (1000.0), rel=1e-6)

    def test_magnitude_is_physically_plausible(self, built_results) -> None:
        # Order-of-magnitude anchor: a 1000 te added mass is 1e6 kg, not 1e3.
        # This is the assertion that fails if the fix is reverted to a relabel.
        surge = built_results.added_mass.matrices[0].matrix[0, 0]
        assert surge > 1e5, (
            f"surge added mass {surge} is tonne-scale; DiffractionResults is kg"
        )


class TestDampingUnitBoundary:
    """Damping carries the identical 1000x defect and the same fix."""

    def test_damping_converted_to_si(self, built_results) -> None:
        matrices = built_results.damping.matrices
        surge = np.array([m.matrix[0, 0] for m in matrices])
        assert np.allclose(np.sort(surge), [2e3, 3e3, 4e3])

    def test_damping_units_declare_si(self, built_results) -> None:
        units = built_results.damping.matrices[0].units
        assert units["linear"] == "N.s/m"
        assert units["angular"] == "N.m.s/rad"


class TestFrequencyHandlingUnchanged:
    """Regression fence: the frequency axis was already correct - keep it so."""

    def test_frequencies_ascending_rad_per_s(self, built_results) -> None:
        values = built_results.added_mass.frequencies.values
        # 0.10, 0.20, 0.30 Hz -> ascending rad/s
        assert np.all(np.diff(values) > 0)
        assert np.allclose(values, 2 * np.pi * np.array([0.10, 0.20, 0.30]))
