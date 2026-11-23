"""Tests for crane tip motion transfer function.

Verifies the geometric transfer from vessel CoG RAOs to crane tip RAOs
and spectral integration for significant motion/velocity.
"""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.crane_tip_motion import (
    _complex_rao,
    _decompose_complex,
    _jonswap_spectrum,
    crane_tip_raos,
    crane_tip_significant_motion,
    crane_tip_significant_velocity,
)
from digitalmodel.marine_ops.installation.models import (
    CraneTipConfig,
    Vessel,
)

_trapezoid = getattr(np, "trapezoid", None) or np.trapz

# Issue #2543 specifies these gamma cases and the 1% variance tolerance.
_GAMMAS = (1.0, 3.3, 7.0)
_ENERGY_TOLERANCE = 0.01
# Synthetic sea state from the worked example in
# docs/domains/installation/jonswap-normalisation.md: m0 = 4**2/16 = 1 m².
_HS_M = 4.0
_TP_S = 8.0


@pytest.fixture
def converged_frequencies():
    """Use the normalisation reference's 20001-point logarithmic grid.

    Basis: docs/domains/installation/jonswap-normalisation.md, Worked example
    and Equations and numerical method. With x=omega/wp, the unenhanced
    upper tail is 1-exp(-5/(4*100**4)); the lower tail at x=0.01 is
    exp(-5/(4*0.01**4)). The reference bounds enhancement outside
    [0.5, 2] by 2e-11 for the specified gamma cases. The full-spectrum
    multiplier is at most one, so these also bound the normalised tails.
    Grid refinement and band extension are checked explicitly below.
    """
    wp = 2 * np.pi / _TP_S
    return wp * np.geomspace(0.01, 100.0, 20001)


@pytest.fixture
def constant_complex_transfer(converged_frequencies):
    """Synthetic 3+4i m/m transfer: |H|=sqrt(3²+4²)=5 by hand.

    Both components are deliberately nonzero to distinguish complex
    magnitude from either component; the gain is independent of frequency.
    """
    return np.full(converged_frequencies.shape, 3.0 + 4.0j)


class TestComplexRAOConversion:
    """Complex RAO amplitude/phase roundtrip."""

    def test_zero_phase_roundtrip(self):
        amp = np.array([1.0, 2.0, 3.0])
        phase = np.array([0.0, 0.0, 0.0])
        z = _complex_rao(amp, phase)
        amp_out, phase_out = _decompose_complex(z)
        assert amp_out == pytest.approx(amp, abs=1e-10)
        assert phase_out == pytest.approx(phase, abs=1e-10)

    def test_nonzero_phase_roundtrip(self):
        amp = np.array([1.0, 2.0])
        phase = np.array([45.0, -90.0])
        z = _complex_rao(amp, phase)
        amp_out, phase_out = _decompose_complex(z)
        assert amp_out == pytest.approx(amp, abs=1e-10)
        assert phase_out == pytest.approx(phase, abs=1e-6)


class TestCraneTipRAOs:
    """Geometric transfer function: CoG → crane tip."""

    def test_output_contains_three_dofs(self, vessel):
        result = crane_tip_raos(vessel)
        assert set(result.keys()) == {"surge", "sway", "heave"}

    def test_output_shapes(self, vessel):
        result = crane_tip_raos(vessel)
        nf = len(vessel.rao_frequencies)
        nh = len(vessel.rao_headings)
        for dof in ("surge", "sway", "heave"):
            assert result[dof]["amplitude"].shape == (nf, nh)
            assert result[dof]["phase"].shape == (nf, nh)

    def test_crane_at_cog_returns_cog_raos(self, frequencies, headings, rao_data):
        """If crane tip is at CoG (offset=0), tip RAOs equal CoG RAOs."""
        vessel = Vessel(
            name="CogVessel",
            rao_frequencies=frequencies,
            rao_headings=headings,
            rao_data=rao_data,
            crane_tip=CraneTipConfig(x_m=0.0, y_m=0.0, z_m=0.0),
        )
        result = crane_tip_raos(vessel)
        # With zero offset and zero phase, heave_tip = heave_cog exactly
        assert result["heave"]["amplitude"] == pytest.approx(
            rao_data["heave"]["amplitude"], abs=1e-10
        )

    def test_crane_tip_heave_larger_than_cog(self, vessel):
        """Crane tip heave should generally be larger than CoG heave
        due to pitch/roll contributions at offset positions."""
        result = crane_tip_raos(vessel)
        peak_tip = result["heave"]["amplitude"].max()
        peak_cog = vessel.rao_data["heave"]["amplitude"].max()
        assert peak_tip > peak_cog

    def test_heave_transfer_formula(self, frequencies, headings):
        """Direct verification of heave_tip = heave - y*roll + x*pitch.

        Use unit RAOs with zero phase for simple arithmetic check.
        """
        nf, nh = len(frequencies), len(headings)
        ones = np.ones((nf, nh))
        zeros = np.zeros((nf, nh))

        rao_data = {
            "surge": {"amplitude": zeros, "phase": zeros},
            "sway": {"amplitude": zeros, "phase": zeros},
            "heave": {"amplitude": ones * 1.0, "phase": zeros},  # 1.0 m/m
            "roll": {"amplitude": ones * 1.0, "phase": zeros},   # 1.0 deg/m
            "pitch": {"amplitude": ones * 2.0, "phase": zeros},  # 2.0 deg/m
            "yaw": {"amplitude": zeros, "phase": zeros},
        }
        x, y, z = 10.0, 5.0, 20.0
        vessel = Vessel(
            name="Unit",
            rao_frequencies=frequencies,
            rao_headings=headings,
            rao_data=rao_data,
            crane_tip=CraneTipConfig(x_m=x, y_m=y, z_m=z),
        )
        result = crane_tip_raos(vessel)

        # heave_tip = heave - y*roll_rad + x*pitch_rad
        roll_rad = np.deg2rad(1.0)
        pitch_rad = np.deg2rad(2.0)
        expected_heave = 1.0 - y * roll_rad + x * pitch_rad
        assert result["heave"]["amplitude"][0, 0] == pytest.approx(expected_heave, rel=1e-6)

    def test_incomplete_raos_raises(self, frequencies, headings):
        vessel = Vessel(
            name="Bad",
            rao_frequencies=frequencies,
            rao_headings=headings,
            rao_data={"heave": {"amplitude": np.zeros((20, 5)), "phase": np.zeros((20, 5))}},
            crane_tip=CraneTipConfig(x_m=0.0, y_m=0.0, z_m=0.0),
        )
        with pytest.raises(ValueError, match="missing required DOFs"):
            crane_tip_raos(vessel)

    def test_override_crane_tip(self, vessel):
        """Can override crane tip position without modifying vessel."""
        override = CraneTipConfig(x_m=0.0, y_m=0.0, z_m=0.0)
        result = crane_tip_raos(vessel, crane_tip=override)
        # At CoG offset, heave_tip = heave_cog
        assert result["heave"]["amplitude"] == pytest.approx(
            vessel.rao_data["heave"]["amplitude"], abs=1e-10
        )


class TestJONSWAPSpectrum:
    """JONSWAP wave spectrum implementation."""

    def test_spectrum_positive(self):
        omega = np.linspace(0.1, 3.0, 100)
        S = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=3.3)
        assert np.all(S >= 0)

    def test_spectrum_peaks_near_wp(self):
        omega = np.linspace(0.1, 3.0, 300)
        S = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=3.3)
        peak_idx = np.argmax(S)
        # Peak should be near wp=0.8
        assert omega[peak_idx] == pytest.approx(0.8, abs=0.1)

    def test_hs_scaling(self):
        """Doubling Hs should quadruple m0 (and thus spectral energy)."""
        omega = np.linspace(0.1, 3.0, 500)
        S1 = _jonswap_spectrum(omega, hs=1.0, wp=0.8)
        S2 = _jonswap_spectrum(omega, hs=2.0, wp=0.8)
        m0_1 = _trapezoid(S1, omega)
        m0_2 = _trapezoid(S2, omega)
        assert m0_2 / m0_1 == pytest.approx(4.0, rel=0.05)

    def test_gamma_1_gives_pm(self):
        """gamma=1.0 should give Pierson-Moskowitz (no enhancement)."""
        omega = np.linspace(0.1, 3.0, 300)
        S = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=1.0)
        assert np.all(S >= 0)
        # PM spectrum is broader than JONSWAP
        S_j = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=3.3)
        assert np.max(S_j) > np.max(S)  # JONSWAP peak is sharper

    @pytest.mark.parametrize("gamma", _GAMMAS)
    def test_spectrum_energy_and_frequency_grid_convergence(
        self, gamma, converged_frequencies
    ):
        """Check m0=Hs²/16 and separate mesh error from bandwidth loss.

        Equation and tail basis: docs/domains/installation/
        jonswap-normalisation.md, Equations and numerical method.
        Synthetic Hs=4 gives wave variance 1 m² by hand. Halve the
        logarithmic step twice (10001, 20001, 40001 points), then extend
        both tails by a decade with 60001 points, retaining the fine
        grid step (six decades instead of four). Require each change
        to use less than 1/100 of the issue's 1% energy error budget.
        """
        wp = 2 * np.pi / _TP_S
        grids = (
            wp * np.geomspace(0.01, 100.0, 10001),
            converged_frequencies,
            wp * np.geomspace(0.01, 100.0, 40001),
            wp * np.geomspace(0.001, 1000.0, 60001),
        )
        moments = []
        expected_variance = _HS_M**2 / 16
        for omega in grids:
            density = _jonswap_spectrum(omega, _HS_M, wp, gamma=gamma)
            assert np.all(np.isfinite(density))
            assert np.all(density >= 0)
            moment = _trapezoid(density, omega)
            assert moment == pytest.approx(
                expected_variance, rel=_ENERGY_TOLERANCE
            )
            moments.append(moment)

        relative_changes = np.abs(np.diff(moments)) / expected_variance
        assert np.all(relative_changes < _ENERGY_TOLERANCE / 100)
        # Retain measured evidence in pytest's passed-test output (-rA).
        relative_errors = np.abs(np.array(moments) / expected_variance - 1)
        print(
            f"gamma={gamma}: max energy error={max(relative_errors):.1e}; "
            f"coarse/selected change={relative_changes[0]:.1e}; "
            f"selected/fine change={relative_changes[1]:.1e}; "
            f"extended-band change={relative_changes[2]:.1e} (fractions)"
        )


class TestSignificantMotion:
    """Spectral integration for significant motion."""

    def test_zero_hs_gives_zero_motion(self, vessel):
        tip = crane_tip_raos(vessel)
        heave_rao = tip["heave"]["amplitude"][:, 0]
        motion = crane_tip_significant_motion(heave_rao, 0.0, 8.0, vessel.rao_frequencies)
        assert motion == pytest.approx(0.0, abs=1e-10)

    def test_motion_increases_with_hs(self, vessel):
        tip = crane_tip_raos(vessel)
        heave_rao = tip["heave"]["amplitude"][:, 0]
        m1 = crane_tip_significant_motion(heave_rao, 1.0, 8.0, vessel.rao_frequencies)
        m2 = crane_tip_significant_motion(heave_rao, 2.0, 8.0, vessel.rao_frequencies)
        assert m2 > m1

    def test_motion_positive(self, vessel):
        tip = crane_tip_raos(vessel)
        heave_rao = tip["heave"]["amplitude"][:, 0]
        motion = crane_tip_significant_motion(heave_rao, 1.5, 8.0, vessel.rao_frequencies)
        assert motion > 0

    def test_velocity_positive(self, vessel):
        tip = crane_tip_raos(vessel)
        heave_rao = tip["heave"]["amplitude"][:, 0]
        vel = crane_tip_significant_velocity(heave_rao, 1.5, 8.0, vessel.rao_frequencies)
        assert vel > 0

    def test_velocity_increases_with_hs(self, vessel):
        tip = crane_tip_raos(vessel)
        heave_rao = tip["heave"]["amplitude"][:, 0]
        v1 = crane_tip_significant_velocity(heave_rao, 1.0, 8.0, vessel.rao_frequencies)
        v2 = crane_tip_significant_velocity(heave_rao, 2.0, 8.0, vessel.rao_frequencies)
        assert v2 > v1

    @pytest.mark.parametrize("gamma", _GAMMAS)
    def test_constant_transfer_significant_motion_matches_hand_value(
        self, gamma, converged_frequencies, constant_complex_transfer
    ):
        """|H|=5, Hs=4: response variance=25*4²/16=25 m² by hand.

        Basis: docs/domains/installation/jonswap-normalisation.md,
        Worked example. The API returns single-amplitude motion
        2*sqrt(25)=10 m; significant double amplitude is |H|*Hs=20 m.
        Neither expected response uses a production integration helper.
        Tolerance: issue #2543, 1%.
        """
        amplitude = np.abs(constant_complex_transfer)
        assert amplitude == pytest.approx(5.0)
        motion = crane_tip_significant_motion(
            amplitude, _HS_M, _TP_S, converged_frequencies, gamma=gamma
        )
        assert motion == pytest.approx(10.0, rel=_ENERGY_TOLERANCE)
        assert 2 * motion == pytest.approx(5.0 * _HS_M, rel=_ENERGY_TOLERANCE)

    @pytest.mark.parametrize("gamma", _GAMMAS)
    @pytest.mark.parametrize(
        "rotation", [1.0, 1.0j, -1.0, -1.0j],
        ids=["zero", "quarter-turn", "half-turn", "minus-quarter-turn"],
    )
    def test_constant_transfer_phase_preserves_energy(
        self, gamma, rotation, converged_frequencies, constant_complex_transfer
    ):
        """Quarter-turn rotations preserve |3+4i|²=25 at every frequency.

        The synthetic rotations are unit complex numbers. The reference's
        m0=Hs²/16 gives response variance 25 m² and single-amplitude
        motion 10 m for every phase. Both are independently known hand
        values (normalisation reference, Worked example); use issue
        #2543's 1% tolerance. Only amplitude enters the response API.
        """
        transfer = constant_complex_transfer * rotation
        amplitude, phase = _decompose_complex(transfer)
        assert amplitude == pytest.approx(5.0)
        # Independently reconstruct the rotated input, including its phase.
        np.testing.assert_allclose(
            amplitude * np.exp(1j * np.deg2rad(phase)), transfer
        )
        density = _jonswap_spectrum(
            converged_frequencies, _HS_M, 2 * np.pi / _TP_S, gamma=gamma
        )
        response_variance = _trapezoid(
            np.abs(transfer)**2 * density, converged_frequencies
        )
        assert response_variance == pytest.approx(25.0, rel=_ENERGY_TOLERANCE)
        motion = crane_tip_significant_motion(
            amplitude, _HS_M, _TP_S, converged_frequencies, gamma=gamma
        )
        assert motion == pytest.approx(10.0, rel=_ENERGY_TOLERANCE)

    @pytest.mark.parametrize("gamma", _GAMMAS)
    def test_zero_transfer_gives_zero_motion(self, gamma, converged_frequencies):
        """Zero gain gives zero response variance even with nonzero Hs.

        Hand calculation: 0² * Hs²/16 = 0 (normalisation reference,
        Worked example). Exact zero is expected without quadrature error.
        """
        motion = crane_tip_significant_motion(
            np.zeros_like(converged_frequencies), _HS_M, _TP_S,
            converged_frequencies, gamma=gamma,
        )
        assert motion == 0.0
