# ABOUTME: USNA EN400 worked examples as pytest fixtures for naval architecture TDD
# ABOUTME: Tests call not-yet-implemented modules → xfail until modules are built
"""
USNA EN400 Worked Examples — Parametrized Test Fixtures

Source: USNA EN400 Principles of Ship Performance (Summer 2020)
Purpose: TDD validation infrastructure for naval architecture modules.
         Tests call implementation functions that do not exist yet → xfail.

Lifecycle:
    Phase 1 (now):  xfail — modules don't exist (ImportError)
    Phase 2 (next): modules exist but may return wrong values (AssertionError)
    Phase 3 (done): tests pass — implementation matches textbook answers

Usage:
    PYTHONPATH=src uv run python -m pytest tests/naval_architecture/ -v
    PYTHONPATH=src uv run python -m pytest tests/naval_architecture/ -k "hydrostatics"
    PYTHONPATH=src uv run python -m pytest tests/naval_architecture/ -k "stability"
"""

import pytest

# Common xfail: modules not yet implemented
_xfail_fundamentals = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.fundamentals not implemented"
)
_xfail_hydrostatics = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.hydrostatics not implemented"
)
_xfail_integration = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.integration not implemented"
)
_xfail_curves_of_form = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.curves_of_form not implemented"
)
_xfail_stability = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.stability not implemented"
)
_xfail_resistance = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.resistance not implemented"
)
_xfail_submarines = pytest.mark.xfail(
    reason="digitalmodel.naval_architecture.submarines not implemented"
)


# ---------------------------------------------------------------------------
# Chapter 1: Engineering Fundamentals
# ---------------------------------------------------------------------------
class TestEN400Ch1Fundamentals:
    """Basic engineering calculations — unit conversions, buoyancy."""

    @_xfail_fundamentals
    def test_example_1_1_mass_to_weight(self):
        """Example 1.1 (p.17): Convert 1 slug to weight in pounds."""
        from digitalmodel.naval_architecture.fundamentals import (
            mass_to_weight,
        )

        result = mass_to_weight(mass_slug=1.0)
        assert abs(result - 32.17) < 0.01

    @_xfail_fundamentals
    def test_example_1_2_displaced_volume_to_weight(self):
        """Example 1.2 (p.17): 4000 ft³ displaced volume → LT."""
        from digitalmodel.naval_architecture.fundamentals import (
            displaced_volume_to_weight_lt,
        )

        result = displaced_volume_to_weight_lt(
            volume_ft3=4000.0, water="saltwater"
        )
        assert abs(result - 114.29) < 0.1

    @_xfail_fundamentals
    def test_example_1_3_density_interpolation(self):
        """Example 1.3 (p.21): Interpolate fresh water density at 62.7°F."""
        from digitalmodel.naval_architecture.fundamentals import (
            interpolate_water_density,
        )

        result = interpolate_water_density(temp_f=62.7, water="freshwater")
        assert abs(result - 1.93776) < 0.00001


# ---------------------------------------------------------------------------
# Chapter 2: Hull Form and Geometry
# ---------------------------------------------------------------------------
class TestEN400Ch2HullFormGeometry:
    """Simpson's rule integration, waterplane area, sectional area."""

    @_xfail_hydrostatics
    def test_example_2_1_buoyant_force(self):
        """Example 2.1 (p.49): Buoyant force on 20 ft³ in salt water."""
        from digitalmodel.naval_architecture.hydrostatics import (
            buoyant_force,
        )

        result = buoyant_force(volume_ft3=20.0, water="saltwater")
        assert abs(result - 1280.0) < 1.0

    @_xfail_hydrostatics
    def test_example_2_2_submerged_volume_from_buoyancy(self):
        """Example 2.2 (p.49): Submerged volume for 4000 LT fresh water."""
        from digitalmodel.naval_architecture.hydrostatics import (
            submerged_volume,
        )

        result = submerged_volume(
            displacement_lt=4000.0, water="freshwater"
        )
        assert abs(result - 143590.0) < 100.0

    @_xfail_integration
    @pytest.mark.parametrize(
        "offsets_ft, spacing_ft, expected_area_ft2",
        [
            pytest.param(
                [0.0, 7.0, 10.0, 8.0, 0.0],
                15.0,
                480.0,
                id="example_2_3_waterplane_area",
            ),
        ],
    )
    def test_simpsons_first_rule_area(
        self, offsets_ft, spacing_ft, expected_area_ft2
    ):
        """Example 2.3 (p.70): Waterplane area via Simpson's 1st rule."""
        from digitalmodel.naval_architecture.integration import (
            simpsons_first_rule,
        )

        half_area = simpsons_first_rule(offsets_ft, spacing_ft)
        area = 2 * half_area
        assert abs(area - expected_area_ft2) < 5.0


# ---------------------------------------------------------------------------
# Chapter 3: Hydrostatics
# ---------------------------------------------------------------------------
class TestEN400Ch3Hydrostatics:
    """Draft, displacement, trim, curves of form, inclining experiment."""

    @_xfail_curves_of_form
    def test_example_3_1_displacement_from_curves_of_form(self):
        """Example 3.1 (p.100): DDG51 displacement at 21.0 ft draft."""
        from digitalmodel.naval_architecture.curves_of_form import (
            displacement_at_draft,
        )

        result = displacement_at_draft(vessel="DDG51", draft_ft=21.0)
        assert abs(result - 8600.0) < 200.0

    @_xfail_hydrostatics
    def test_example_3_2_vertical_cg_shift(self):
        """Example 3.2 (p.107): FFG-7 KG shift from weight addition."""
        from digitalmodel.naval_architecture.hydrostatics import (
            vertical_cg_after_weight_change,
        )

        result = vertical_cg_after_weight_change(
            displacement_lt=4092.0,
            initial_kg_ft=18.5,
            weight_lt=100.0,
            weight_kg_ft=30.0,
        )
        assert abs(result - 18.774) < 0.01

    @_xfail_hydrostatics
    def test_example_3_3_transverse_cg_shift(self):
        """Example 3.3 (p.110): Transverse CG shift from weight move."""
        from digitalmodel.naval_architecture.hydrostatics import (
            transverse_cg_after_weight_shift,
        )

        result = transverse_cg_after_weight_shift(
            displacement_lt=4092.0,
            initial_tcg_ft=2.0,
            weight_moved_lt=50.0,
            distance_ft=30.0,
            direction="port",
        )
        assert abs(result - 1.633) < 0.01

    @_xfail_hydrostatics
    def test_example_3_4_inclining_experiment(self):
        """Example 3.4 (p.122): GM from inclining experiment."""
        from digitalmodel.naval_architecture.hydrostatics import (
            gm_from_inclining_experiment,
        )

        result = gm_from_inclining_experiment(
            weight_moved_lt=30.0,
            distance_ft=20.0,
            displacement_lt=4092.0,
            tan_phi=0.0149,
        )
        assert abs(result - 9.84) < 0.5


# ---------------------------------------------------------------------------
# Chapter 4: Stability
# ---------------------------------------------------------------------------
class TestEN400Ch4Stability:
    """Righting arm curves, GZ, free surface effect, damage stability."""

    @_xfail_stability
    @pytest.mark.parametrize(
        "heel_deg, kn_ft, kg_ft, expected_gz_ft",
        [
            # Example 4.1 (p.153): DDG51 at 8600 LT, KG=23.84 ft
            pytest.param(0, 0.0, 23.84, 0.0, id="heel_0"),
            pytest.param(10, 5.08, 23.84, 0.94, id="heel_10"),
            pytest.param(20, 10.10, 23.84, 1.95, id="heel_20"),
            pytest.param(30, 15.02, 23.84, 3.10, id="heel_30"),
            pytest.param(40, 19.67, 23.84, 4.35, id="heel_40"),
        ],
    )
    def test_righting_arm_gz_correction(
        self, heel_deg, kn_ft, kg_ft, expected_gz_ft
    ):
        """GZ = KN(cross curves) - KG * sin(heel)."""
        from digitalmodel.naval_architecture.stability import (
            gz_from_cross_curves,
        )

        result = gz_from_cross_curves(
            heel_deg=heel_deg, kn_ft=kn_ft, kg_ft=kg_ft
        )
        assert abs(result - expected_gz_ft) < 0.15

    @_xfail_stability
    def test_example_4_2_cosine_correction_for_tcg(self):
        """Example 4.2 (p.157): GZ with TCG offset (cosine correction)."""
        from digitalmodel.naval_architecture.stability import (
            gz_with_tcg_correction,
        )

        result = gz_with_tcg_correction(
            gz_ft=1.95, tcg_ft=0.4, heel_deg=20.0
        )
        assert abs(result - 1.574) < 0.05

    @_xfail_stability
    def test_example_4_4_free_surface_correction(self):
        """Example 4.4 (p.170): Free surface effect on GM."""
        from digitalmodel.naval_architecture.stability import (
            free_surface_correction,
        )

        fsc, gm_eff = free_surface_correction(
            displacement_lt=4092.0,
            kg_ft=18.9,
            km_ft=22.49,
            tank_length_ft=40.0,
            tank_breadth_ft=30.0,
            rho_fluid=1.756,
        )
        assert abs(fsc - 0.555) < 0.05
        assert abs(gm_eff - 3.035) < 0.1


# ---------------------------------------------------------------------------
# Chapter 7: Resistance and Powering
# ---------------------------------------------------------------------------
class TestEN400Ch7ResistancePowering:
    """Ship resistance, EHP, model testing, Reynolds number."""

    @_xfail_resistance
    def test_example_7_1_shaft_horsepower(self):
        """Example 7.1 (p.253): SHP from EHP and propulsive coefficient."""
        from digitalmodel.naval_architecture.resistance import (
            shaft_horsepower,
        )

        result = shaft_horsepower(ehp=30000.0, propulsive_coefficient=0.60)
        assert abs(result - 50000.0) < 100.0

    @_xfail_resistance
    def test_example_7_2_reynolds_number(self):
        """Example 7.2 (p.260): Rn for 450 ft ship at 15 knots."""
        from digitalmodel.naval_architecture.resistance import (
            reynolds_number,
        )

        result = reynolds_number(
            length_ft=450.0,
            speed_knots=15.0,
            nu=1.2791e-5,
        )
        assert abs(result - 8.91e8) / 8.91e8 < 0.05

    @_xfail_resistance
    def test_example_7_3_model_frictional_resistance(self):
        """Example 7.3 (p.261): ITTC 1957 friction coefficient."""
        from digitalmodel.naval_architecture.resistance import (
            ittc_1957_cf,
        )

        rn_model = 5.0 * 15.0 / 1.2260e-5
        result = ittc_1957_cf(rn=rn_model)
        assert abs(result - 0.00327) < 0.0005

    @_xfail_resistance
    def test_example_7_4_froude_scaling(self):
        """Example 7.4 (p.275): Model-to-ship speed via Froude number."""
        from digitalmodel.naval_architecture.resistance import (
            froude_speed_scaling,
        )

        result_knots = froude_speed_scaling(
            model_speed_fps=3.0,
            ship_length_ft=435.0,
            model_length_ft=15.0,
        )
        assert abs(result_knots - 9.57) < 0.5


# ---------------------------------------------------------------------------
# Chapter 10: Submarines
# ---------------------------------------------------------------------------
class TestEN400Ch10Submarines:
    """Submarine buoyancy, trim, depth changes."""

    @_xfail_submarines
    def test_example_10_1_mbt_capacity(self):
        """Example 10.1 (p.349): MBT water required to submerge."""
        from digitalmodel.naval_architecture.submarines import (
            mbt_capacity,
        )

        result = mbt_capacity(
            surface_disp_lt=6900.0, submerged_disp_lt=7800.0
        )
        assert abs(result - 900.0) < 1.0

    @_xfail_submarines
    def test_example_10_2_water_density_compensation(self):
        """Example 10.2 (p.350): Water adjustment for density change."""
        from digitalmodel.naval_architecture.submarines import (
            density_compensation,
        )

        result = density_compensation(
            submerged_weight_lt=7200.0,
            rho_initial=1.9890,
            rho_final=1.9976,
        )
        assert result > 0
        assert abs(result - 31.1) < 2.0

    @_xfail_submarines
    def test_example_10_3_submerged_stability(self):
        """Example 10.3 (p.355): Submarine stability with weight shift."""
        from digitalmodel.naval_architecture.submarines import (
            submerged_bg_after_weight_shift,
        )

        result = submerged_bg_after_weight_shift(
            displacement_lt=7200.0,
            kb_ft=15.0,
            kg_ft=13.5,
            moved_weight_lt=6.0,
            old_pos_ft=10.0,
            new_pos_ft=20.0,
        )
        assert abs(result - 1.492) < 0.01
