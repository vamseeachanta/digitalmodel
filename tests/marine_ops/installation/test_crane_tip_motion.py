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
        m0_1 = np.trapz(S1, omega)
        m0_2 = np.trapz(S2, omega)
        assert m0_2 / m0_1 == pytest.approx(4.0, rel=0.05)

    def test_gamma_1_gives_pm(self):
        """gamma=1.0 should give Pierson-Moskowitz (no enhancement)."""
        omega = np.linspace(0.1, 3.0, 300)
        S = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=1.0)
        assert np.all(S >= 0)
        # PM spectrum is broader than JONSWAP
        S_j = _jonswap_spectrum(omega, hs=2.0, wp=0.8, gamma=3.3)
        assert np.max(S_j) > np.max(S)  # JONSWAP peak is sharper


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
