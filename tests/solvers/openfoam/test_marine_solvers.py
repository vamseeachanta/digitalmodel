#!/usr/bin/env python3
"""
ABOUTME: Tests for marine solver pre-configurations covering wave loading,
current loading, greenwater, sloshing and VIV solver setups.
"""

import pytest
import math

from digitalmodel.solvers.openfoam.marine_solvers import (
    CurrentLoadingSetup,
    GreenWaterSetup,
    SloshingSetup,
    VIVSetup,
    WaveLoadingSetup,
)
from digitalmodel.solvers.openfoam.wave_models import (
    CurrentProfile,
    CurrentProfileType,
    IrregularWaveBC,
    RegularWaveBC,
    WaveTheory,
)


# ============================================================================
# WaveLoadingSetup tests
# ============================================================================


class TestWaveLoadingSetup:
    """Validate interFoam wave-loading solver configuration."""

    def test_wave_loading_uses_interfoam(self):
        """WaveLoadingSetup selects interFoam (VOF) solver."""
        setup = WaveLoadingSetup()
        assert setup.case.solver_config.solver_name == "interFoam"

    def test_wave_loading_is_multiphase(self):
        """WaveLoadingSetup enables multiphase flag."""
        setup = WaveLoadingSetup()
        assert setup.case.solver_config.is_multiphase is True

    def test_wave_loading_has_turbulence(self):
        """WaveLoadingSetup configures a turbulence model."""
        setup = WaveLoadingSetup()
        assert setup.case.turbulence_model is not None

    def test_wave_loading_control_dict_keys(self):
        """controlDict dict has required keys."""
        setup = WaveLoadingSetup()
        cd = setup.case.solver_config.to_control_dict()
        assert "application" in cd
        assert "startTime" in cd
        assert "endTime" in cd
        assert "deltaT" in cd


# ============================================================================
# CurrentLoadingSetup tests
# ============================================================================


class TestCurrentLoadingSetup:
    """Validate simpleFoam / pimpleFoam current-loading configuration."""

    def test_current_loading_uses_single_phase_solver(self):
        """CurrentLoadingSetup selects a single-phase solver."""
        setup = CurrentLoadingSetup()
        assert setup.case.solver_config.solver_name in (
            "simpleFoam", "pimpleFoam"
        )

    def test_current_loading_not_multiphase(self):
        """CurrentLoadingSetup does not enable multiphase."""
        setup = CurrentLoadingSetup()
        assert setup.case.solver_config.is_multiphase is False

    def test_steady_current_uses_simplefoam(self):
        """Steady current simulation selects simpleFoam."""
        setup = CurrentLoadingSetup(steady=True)
        assert setup.case.solver_config.solver_name == "simpleFoam"

    def test_unsteady_current_uses_pimplefoam(self):
        """Transient current simulation selects pimpleFoam."""
        setup = CurrentLoadingSetup(steady=False)
        assert setup.case.solver_config.solver_name == "pimpleFoam"


# ============================================================================
# GreenWaterSetup tests
# ============================================================================


class TestGreenWaterSetup:
    """Validate greenwater (deck overtopping) solver configuration."""

    def test_greenwater_uses_interfoam(self):
        """GreenWaterSetup selects interFoam."""
        setup = GreenWaterSetup()
        assert setup.case.solver_config.solver_name == "interFoam"

    def test_greenwater_is_multiphase(self):
        """GreenWaterSetup is multiphase (VOF)."""
        setup = GreenWaterSetup()
        assert setup.case.solver_config.is_multiphase is True


# ============================================================================
# SloshingSetup tests
# ============================================================================


class TestSloshingSetup:
    """Validate tank sloshing solver configuration."""

    def test_sloshing_uses_interfoam(self):
        """SloshingSetup selects interFoam."""
        setup = SloshingSetup()
        assert setup.case.solver_config.solver_name == "interFoam"

    def test_sloshing_is_multiphase(self):
        """SloshingSetup is multiphase for internal free surface."""
        setup = SloshingSetup()
        assert setup.case.solver_config.is_multiphase is True


# ============================================================================
# VIVSetup tests
# ============================================================================


class TestVIVSetup:
    """Validate vortex-induced vibration solver configuration."""

    def test_viv_uses_pimplefoam(self):
        """VIVSetup selects pimpleFoam for transient flow."""
        setup = VIVSetup()
        assert setup.case.solver_config.solver_name == "pimpleFoam"

    def test_viv_is_not_multiphase(self):
        """VIVSetup is single-phase (no free surface)."""
        setup = VIVSetup()
        assert setup.case.solver_config.is_multiphase is False


# ============================================================================
# RegularWaveBC tests
# ============================================================================


class TestRegularWaveBC:
    """Test Stokes wave boundary condition parameter generation."""

    def test_stokes_2nd_order_parameters(self):
        """Regular wave BC stores wave height and period."""
        wave = RegularWaveBC(
            wave_height=2.0,
            wave_period=10.0,
            water_depth=50.0,
            theory=WaveTheory.STOKES_2ND,
        )
        assert wave.wave_height == pytest.approx(2.0)
        assert wave.wave_period == pytest.approx(10.0)

    def test_wave_length_computed(self):
        """Regular wave BC computes wave length via dispersion relation."""
        wave = RegularWaveBC(
            wave_height=2.0,
            wave_period=10.0,
            water_depth=50.0,
        )
        # For T=10s in deep water: L ~ g*T^2/(2*pi) ~ 156m
        wl = wave.wave_length
        assert wl > 50.0
        assert wl < 300.0

    def test_wave_angular_frequency(self):
        """Angular frequency omega = 2*pi/T."""
        wave = RegularWaveBC(
            wave_height=2.0,
            wave_period=8.0,
            water_depth=100.0,
        )
        expected_omega = 2.0 * math.pi / 8.0
        assert wave.angular_frequency == pytest.approx(expected_omega, rel=1e-4)

    def test_wave_amplitude(self):
        """Wave amplitude is half the wave height."""
        wave = RegularWaveBC(
            wave_height=3.0,
            wave_period=10.0,
            water_depth=50.0,
        )
        assert wave.amplitude == pytest.approx(1.5)

    def test_to_dict_contains_required_keys(self):
        """RegularWaveBC to_dict() has required OpenFOAM wave BC keys."""
        wave = RegularWaveBC(
            wave_height=2.0,
            wave_period=10.0,
            water_depth=50.0,
        )
        d = wave.to_dict()
        assert "waveHeight" in d or "wave_height" in d
        assert "wavePeriod" in d or "wave_period" in d


# ============================================================================
# IrregularWaveBC tests
# ============================================================================


class TestIrregularWaveBC:
    """Test JONSWAP irregular wave BC parameters."""

    def test_jonswap_significant_wave_height(self):
        """IrregularWaveBC stores Hs."""
        wave = IrregularWaveBC(
            significant_wave_height=4.0,
            peak_period=12.0,
            water_depth=100.0,
        )
        assert wave.significant_wave_height == pytest.approx(4.0)

    def test_jonswap_gamma_default(self):
        """JONSWAP peak enhancement factor defaults to 3.3."""
        wave = IrregularWaveBC(
            significant_wave_height=4.0,
            peak_period=12.0,
            water_depth=100.0,
        )
        assert wave.gamma == pytest.approx(3.3)

    def test_jonswap_custom_gamma(self):
        """Custom JONSWAP gamma can be set."""
        wave = IrregularWaveBC(
            significant_wave_height=4.0,
            peak_period=12.0,
            water_depth=100.0,
            gamma=1.0,
        )
        assert wave.gamma == pytest.approx(1.0)


# ============================================================================
# CurrentProfile tests
# ============================================================================


class TestCurrentProfile:
    """Test current velocity profile generators."""

    def test_uniform_profile(self):
        """Uniform current has constant velocity at all depths."""
        cp = CurrentProfile(
            surface_speed=1.5,
            profile_type=CurrentProfileType.UNIFORM,
        )
        depths = [0.0, 5.0, 10.0, 50.0]
        speeds = [cp.speed_at_depth(d) for d in depths]
        assert all(s == pytest.approx(1.5) for s in speeds)

    def test_power_law_decays_with_depth(self):
        """Power-law profile has decreasing speed with depth."""
        cp = CurrentProfile(
            surface_speed=1.5,
            profile_type=CurrentProfileType.POWER_LAW,
            reference_depth=50.0,
        )
        speed_surface = cp.speed_at_depth(0.0)
        speed_deep = cp.speed_at_depth(40.0)
        assert speed_surface > speed_deep

    def test_log_law_profile_at_surface(self):
        """Log-law profile at z=0 returns surface speed."""
        cp = CurrentProfile(
            surface_speed=1.0,
            profile_type=CurrentProfileType.LOG_LAW,
            reference_depth=30.0,
        )
        speed = cp.speed_at_depth(0.0)
        assert speed == pytest.approx(1.0, rel=0.1)
