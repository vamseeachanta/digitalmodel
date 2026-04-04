"""Heat Transfer (ht) library integration tests.

Library: https://github.com/CalebBell/ht (v1.2.0, MIT)
Tests: import checks, known-value verification, edge cases, physics sanity.
All values in SI units.
"""

import math

import pytest

ht = pytest.importorskip("ht")

from ht.boiling_nucleic import Rohsenow
from ht.conduction import R_cylinder
from ht.conv_external import Nu_cylinder_Churchill_Bernstein
from ht.conv_internal import Nu_conv_internal
from ht.hx import effectiveness_from_NTU
from ht.insulation import k_material
from ht.radiation import q_rad


# ---------------------------------------------------------------------------
# Import and version checks
# ---------------------------------------------------------------------------

class TestImportAndVersion:
    """Verify ht library loads correctly and reports expected version."""

    def test_import_ht(self):
        """ht package imports without error."""
        import ht as _ht
        assert _ht is not None

    def test_version(self):
        """ht reports version 1.2.0."""
        assert ht.__version__ == "1.2.0"

    def test_submodules_importable(self):
        """Key submodules are importable."""
        from ht import conv_internal, conv_external, conduction  # noqa: F401
        from ht import boiling_nucleic, radiation, hx, insulation  # noqa: F401


# ---------------------------------------------------------------------------
# Internal convection tests
# ---------------------------------------------------------------------------

class TestInternalConvection:
    """Verify internal convection Nusselt number correlations."""

    def test_turbulent_known_value(self):
        """Nu for turbulent flow (Re=1e5, Pr=0.7) matches reference value."""
        Nu = Nu_conv_internal(Re=1e5, Pr=0.7)
        assert Nu == pytest.approx(183.71, rel=1e-2)

    def test_laminar_returns_constant(self):
        """Laminar flow (Re=1000) gives fully-developed Nu ~ 3.66."""
        Nu = Nu_conv_internal(Re=1000, Pr=0.7)
        assert Nu == pytest.approx(3.66, rel=1e-2)

    def test_nu_increases_with_re(self):
        """Nusselt number increases monotonically with Reynolds number."""
        Re_values = [500, 2300, 5000, 1e4, 1e5]
        Nu_values = [Nu_conv_internal(Re=Re, Pr=0.7) for Re in Re_values]
        for i in range(len(Nu_values) - 1):
            assert Nu_values[i + 1] > Nu_values[i], (
                f"Nu should increase: Nu(Re={Re_values[i]})={Nu_values[i]:.4f} "
                f">= Nu(Re={Re_values[i+1]})={Nu_values[i+1]:.4f}"
            )

    def test_nu_positive_for_positive_re(self):
        """Nu is positive for any positive Reynolds number."""
        for Re in [100, 1000, 1e4, 1e5, 1e6]:
            assert Nu_conv_internal(Re=Re, Pr=0.7) > 0

    def test_re_zero_raises(self):
        """Re=0 raises ValueError (log domain error in correlation)."""
        with pytest.raises(ValueError):
            Nu_conv_internal(Re=0, Pr=0.7)

    def test_low_prandtl_liquid_metal(self):
        """Very low Pr (liquid metal, Pr=0.01) still returns positive Nu."""
        Nu = Nu_conv_internal(Re=1e5, Pr=0.01)
        assert Nu > 0
        # Liquid metals: Nu is much lower than for Pr~1
        Nu_air = Nu_conv_internal(Re=1e5, Pr=0.7)
        assert Nu < Nu_air

    def test_high_prandtl_oil(self):
        """High Pr (oil, Pr=100) gives higher Nu than air at same Re."""
        Nu_oil = Nu_conv_internal(Re=1e5, Pr=100)
        Nu_air = Nu_conv_internal(Re=1e5, Pr=0.7)
        assert Nu_oil > Nu_air


# ---------------------------------------------------------------------------
# Conduction tests
# ---------------------------------------------------------------------------

class TestConduction:
    """Verify cylindrical conduction resistance."""

    def test_known_value_steel_pipe(self):
        """R_cylinder matches analytical ln(ro/ri)/(2*pi*k*L)."""
        ri, ro, L, k = 0.1, 0.12, 1.0, 50.0
        R = R_cylinder(ri, ro, L, k)
        R_expected = math.log(ro / ri) / (2 * math.pi * k * L)
        assert R == pytest.approx(R_expected, rel=1e-10)

    def test_resistance_increases_with_thickness(self):
        """Thicker wall produces higher thermal resistance."""
        R_thin = R_cylinder(0.1, 0.11, 1.0, 50.0)
        R_thick = R_cylinder(0.1, 0.15, 1.0, 50.0)
        assert R_thick > R_thin

    def test_resistance_decreases_with_conductivity(self):
        """Higher conductivity reduces thermal resistance."""
        R_steel = R_cylinder(0.1, 0.12, 1.0, 50.0)   # steel
        R_copper = R_cylinder(0.1, 0.12, 1.0, 400.0)  # copper
        assert R_copper < R_steel

    def test_resistance_inversely_proportional_to_length(self):
        """Doubling length halves resistance (parallel paths)."""
        R_1m = R_cylinder(0.1, 0.12, 1.0, 50.0)
        R_2m = R_cylinder(0.1, 0.12, 2.0, 50.0)
        assert R_2m == pytest.approx(R_1m / 2, rel=1e-10)

    def test_units_consistency(self):
        """R has units K/W: verify dimensional consistency.

        R = ln(ro/ri)/(2*pi*k*L)
        [R] = [-] / ([W/m/K] * [m]) = K/W  ✓
        """
        R = R_cylinder(0.15, 0.17, 1.0, 50.0)
        assert R > 0
        # Typical steel pipe: R should be very small (mK/W range)
        assert R < 0.01  # Less than 10 mK/W for steel


# ---------------------------------------------------------------------------
# Nucleate boiling tests
# ---------------------------------------------------------------------------

class TestNucleateBoiling:
    """Verify Rohsenow nucleate boiling correlation."""

    # Water at 1 atm, 100 °C saturation
    WATER_PARAMS = dict(
        rhol=957.9, rhog=0.5956, mul=2.79e-4,
        kl=0.68, Cpl=4217.0, Hvap=2.257e6, sigma=0.0589,
    )

    def test_known_value_te10(self):
        """Rohsenow with Te=10 K matches reference (~4557 W/m^2)."""
        q = Rohsenow(**self.WATER_PARAMS, Te=10.0)
        assert q == pytest.approx(4556.65, rel=1e-2)

    def test_heat_flux_increases_with_superheat(self):
        """Higher wall superheat produces higher boiling heat flux."""
        q_5 = Rohsenow(**self.WATER_PARAMS, Te=5.0)
        q_10 = Rohsenow(**self.WATER_PARAMS, Te=10.0)
        q_20 = Rohsenow(**self.WATER_PARAMS, Te=20.0)
        assert q_5 < q_10 < q_20

    def test_heat_flux_positive(self):
        """Boiling heat flux is positive for positive superheat."""
        q = Rohsenow(**self.WATER_PARAMS, Te=5.0)
        assert q > 0


# ---------------------------------------------------------------------------
# Radiation tests
# ---------------------------------------------------------------------------

class TestRadiation:
    """Verify thermal radiation heat flux calculations."""

    def test_known_value(self):
        """q_rad(0.9, 500, 300) matches Stefan-Boltzmann calculation."""
        q = q_rad(emissivity=0.9, T=500.0, T2=300.0)
        sigma = 5.670374419e-8
        q_expected = 0.9 * sigma * (500.0**4 - 300.0**4)
        assert q == pytest.approx(q_expected, rel=1e-3)

    def test_zero_temperature_difference(self):
        """No heat flux when both surfaces at same temperature."""
        q = q_rad(emissivity=0.9, T=300.0, T2=300.0)
        assert q == pytest.approx(0.0, abs=1e-10)

    def test_zero_emissivity(self):
        """Zero emissivity gives zero radiation."""
        q = q_rad(emissivity=0.0, T=500.0, T2=300.0)
        assert q == pytest.approx(0.0, abs=1e-10)

    def test_radiation_proportional_to_emissivity(self):
        """q_rad scales linearly with emissivity."""
        q_05 = q_rad(emissivity=0.5, T=500.0, T2=300.0)
        q_10 = q_rad(emissivity=1.0, T=500.0, T2=300.0)
        assert q_10 == pytest.approx(2.0 * q_05, rel=1e-10)

    def test_hot_to_cold_positive(self):
        """Heat flux is positive when T > T2."""
        q = q_rad(emissivity=0.9, T=400.0, T2=300.0)
        assert q > 0


# ---------------------------------------------------------------------------
# External cross-flow tests
# ---------------------------------------------------------------------------

class TestExternalCrossflow:
    """Verify Churchill-Bernstein external cylinder correlation."""

    def test_known_value(self):
        """Nu for Re=1e4, Pr=0.7 matches reference."""
        Nu = Nu_cylinder_Churchill_Bernstein(Re=1e4, Pr=0.7)
        assert Nu == pytest.approx(53.33, rel=1e-2)

    def test_nu_positive_for_positive_re(self):
        """Nu is always positive for Re > 0."""
        for Re in [1, 100, 1e4, 1e6]:
            Nu = Nu_cylinder_Churchill_Bernstein(Re=Re, Pr=0.7)
            assert Nu > 0

    def test_re_zero_returns_limiting_value(self):
        """Re=0 returns the limiting Nusselt number (~0.3)."""
        Nu = Nu_cylinder_Churchill_Bernstein(Re=0, Pr=0.7)
        assert Nu == pytest.approx(0.3, rel=0.1)

    def test_nu_increases_with_re(self):
        """Higher Re gives higher Nu for external flow."""
        Nu_low = Nu_cylinder_Churchill_Bernstein(Re=1e3, Pr=0.7)
        Nu_high = Nu_cylinder_Churchill_Bernstein(Re=1e5, Pr=0.7)
        assert Nu_high > Nu_low


# ---------------------------------------------------------------------------
# Heat exchanger tests
# ---------------------------------------------------------------------------

class TestHeatExchanger:
    """Verify NTU-effectiveness heat exchanger calculations."""

    def test_counterflow_known_value(self):
        """Counterflow effectiveness for NTU=1.5, Cr=0.5 matches reference."""
        eff = effectiveness_from_NTU(NTU=1.5, Cr=0.5, subtype="counterflow")
        assert eff == pytest.approx(0.6908, rel=1e-2)

    def test_effectiveness_bounded(self):
        """Effectiveness is always in [0, 1]."""
        for NTU in [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]:
            for Cr in [0.0, 0.25, 0.5, 0.75, 1.0]:
                eff = effectiveness_from_NTU(NTU=NTU, Cr=Cr, subtype="counterflow")
                assert 0.0 <= eff <= 1.0, f"eff={eff} out of [0,1] at NTU={NTU}, Cr={Cr}"

    def test_zero_ntu_gives_zero_effectiveness(self):
        """NTU=0 means no heat transfer -> effectiveness=0."""
        eff = effectiveness_from_NTU(NTU=0, Cr=0.5, subtype="counterflow")
        assert eff == pytest.approx(0.0, abs=1e-10)

    def test_condenser_approaches_unity(self):
        """Cr=0 (condenser/boiler) with large NTU -> eff ~ 1."""
        eff = effectiveness_from_NTU(NTU=100, Cr=0.0, subtype="counterflow")
        assert eff == pytest.approx(1.0, abs=1e-6)

    def test_counterflow_better_than_parallel(self):
        """Counterflow is always at least as effective as parallel flow."""
        for NTU in [0.5, 1.0, 2.0, 5.0]:
            eff_cf = effectiveness_from_NTU(NTU=NTU, Cr=0.5, subtype="counterflow")
            eff_pf = effectiveness_from_NTU(NTU=NTU, Cr=0.5, subtype="parallel")
            assert eff_cf >= eff_pf - 1e-10, (
                f"Counterflow ({eff_cf:.4f}) should >= parallel ({eff_pf:.4f}) at NTU={NTU}"
            )

    def test_effectiveness_increases_with_ntu(self):
        """Effectiveness increases monotonically with NTU."""
        NTU_values = [0.1, 0.5, 1.0, 2.0, 5.0]
        eff_values = [
            effectiveness_from_NTU(NTU=n, Cr=0.5, subtype="counterflow")
            for n in NTU_values
        ]
        for i in range(len(eff_values) - 1):
            assert eff_values[i + 1] > eff_values[i]


# ---------------------------------------------------------------------------
# Insulation material property tests
# ---------------------------------------------------------------------------

class TestInsulation:
    """Verify insulation thermal conductivity lookups."""

    def test_polyurethane_foam_k(self):
        """Spray-applied PU foam conductivity is ~0.026 W/m/K."""
        k = k_material("Spray-applied Polyurethane foam, 40 kg/m^3")
        assert k == pytest.approx(0.026, rel=0.1)

    def test_cellular_glass_k(self):
        """Cellular glass conductivity is ~0.048 W/m/K."""
        k = k_material("Cellular glass")
        assert k == pytest.approx(0.048, rel=0.1)

    def test_insulation_k_positive(self):
        """All insulation materials have positive conductivity."""
        materials = [
            "Mineral fiber",
            "Cellular glass",
            "Spray-applied Polyurethane foam, 40 kg/m^3",
            "Expanded polystyrene, extruded",
        ]
        for mat in materials:
            assert k_material(mat) > 0

    def test_insulation_k_less_than_steel(self):
        """Insulation conductivity is much less than steel (~50 W/m/K)."""
        k_insul = k_material("Spray-applied Polyurethane foam, 40 kg/m^3")
        assert k_insul < 1.0  # All insulations < 1 W/m/K


# ---------------------------------------------------------------------------
# Overall U-value integration test
# ---------------------------------------------------------------------------

class TestOverallUValue:
    """End-to-end integration: overall U for insulated subsea pipeline."""

    def test_pipeline_u_value_realistic(self):
        """Overall U-value for insulated subsea pipeline is physically realistic.

        Typical subsea insulated pipeline U: 0.1-10 W/m^2/K.
        Well-insulated (75 mm PU foam): ~0.2-0.5 W/m^2/K.
        Bare or lightly insulated: ~5-15 W/m^2/K.
        """
        ri = 0.1524
        ro = ri + 0.0159
        t_insul = 0.075
        ro_insul = ro + t_insul
        L = 1.0

        k_steel = 50.0
        k_insul = k_material("Spray-applied Polyurethane foam, 40 kg/m^3")

        # Internal: crude oil turbulent flow
        Nu_int = Nu_conv_internal(Re=1e5, Pr=71.43)
        h_int = Nu_int * 0.14 / (2 * ri)

        # External: seawater
        D_ext = 2 * ro_insul
        Re_ext = 1025 * 0.5 * D_ext / 1.08e-3
        Pr_ext = 3993 * 1.08e-3 / 0.596
        Nu_ext = Nu_cylinder_Churchill_Bernstein(Re=Re_ext, Pr=Pr_ext)
        h_ext = Nu_ext * 0.596 / D_ext

        # Resistances
        R_int = 1 / (h_int * 2 * math.pi * ri * L)
        R_wall = R_cylinder(ri, ro, L, k_steel)
        R_insul = R_cylinder(ro, ro_insul, L, k_insul)
        R_ext = 1 / (h_ext * 2 * math.pi * ro_insul * L)
        R_total = R_int + R_wall + R_insul + R_ext

        A_outer = 2 * math.pi * ro_insul * L
        U = 1 / (R_total * A_outer)

        # U should be in range 0.1-10 W/m^2/K for an insulated pipeline
        assert 0.1 < U < 15.0, f"U = {U:.2f} W/m^2/K outside realistic range"

    def test_insulation_dominates_resistance(self):
        """Insulation layer should dominate total thermal resistance."""
        ri = 0.1524
        ro = ri + 0.0159
        ro_insul = ro + 0.075
        L = 1.0

        R_wall = R_cylinder(ri, ro, L, 50.0)
        k_insul = k_material("Spray-applied Polyurethane foam, 40 kg/m^3")
        R_insul = R_cylinder(ro, ro_insul, L, k_insul)

        # Insulation resistance should be >> wall resistance
        assert R_insul > 10 * R_wall, (
            f"R_insul ({R_insul:.6e}) should dominate R_wall ({R_wall:.6e})"
        )
