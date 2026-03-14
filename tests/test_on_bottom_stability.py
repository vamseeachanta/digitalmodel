# ABOUTME: Tests for on-bottom stability — DNV-RP-F109 simplified method.
# ABOUTME: Covers submerged weight, hydrodynamic loads, and lateral/vertical stability.
"""Tests for on-bottom stability — DNV-RP-F109 simplified method."""
import pytest
import math


class TestSubmergedWeight:
    def test_concrete_coated_pipe(self):
        """Submerged weight of concrete-coated pipe."""
        from digitalmodel.geotechnical.on_bottom_stability import submerged_weight
        result = submerged_weight(
            od_steel_m=0.3239,
            wt_steel_m=0.0127,
            coating_thickness_m=0.05,
            rho_steel=7850.0,
            rho_coating=3040.0,
            rho_contents=800.0,
            rho_seawater=1025.0,
        )
        assert result.ws_n_per_m > 0
        assert result.buoyancy_n_per_m > 0

    def test_empty_pipe_may_float(self):
        from digitalmodel.geotechnical.on_bottom_stability import submerged_weight
        result = submerged_weight(
            od_steel_m=0.6096,
            wt_steel_m=0.0127,
            coating_thickness_m=0.0,
            rho_steel=7850.0,
            rho_coating=0.0,
            rho_contents=1.2,
            rho_seawater=1025.0,
        )
        assert isinstance(result.ws_n_per_m, float)


class TestHydrodynamicLoads:
    def test_drag_and_inertia_forces(self):
        """Hydrodynamic forces on seabed pipe per Morison."""
        from digitalmodel.geotechnical.on_bottom_stability import hydrodynamic_loads
        result = hydrodynamic_loads(
            od_total_m=0.4239,
            water_velocity_m_s=1.0,
            water_acceleration_m_s2=0.5,
            rho_seawater=1025.0,
            cd=0.7,
            cm=3.29,
        )
        assert result.drag_n_per_m > 0
        assert result.inertia_n_per_m > 0
        assert result.total_horizontal_n_per_m > 0

    def test_drag_force_per_meter(self):
        """Drag force per unit length: F_D = 0.5 * rho * C_D * D * U^2"""
        from digitalmodel.geotechnical.on_bottom_stability import (
            drag_force_per_meter,
        )
        result = drag_force_per_meter(
            current_velocity_ms=1.0,
            pipe_od_m=0.5,
            water_density_kg_m3=1025.0,
            drag_coeff=0.7,
        )
        expected = 0.5 * 1025.0 * 0.7 * 0.5 * 1.0**2
        assert abs(result - expected) < 1e-6

    def test_lift_force_per_meter(self):
        """Lift force per unit length: F_L = 0.5 * rho * C_L * D * U^2"""
        from digitalmodel.geotechnical.on_bottom_stability import (
            lift_force_per_meter,
        )
        result = lift_force_per_meter(
            current_velocity_ms=1.0,
            pipe_od_m=0.5,
            water_density_kg_m3=1025.0,
            lift_coeff=0.9,
        )
        expected = 0.5 * 1025.0 * 0.9 * 0.5 * 1.0**2
        assert abs(result - expected) < 1e-6

    def test_inertia_force_per_meter(self):
        """Inertia force per unit length: F_I = rho * C_M * pi/4 * D^2 * a"""
        from digitalmodel.geotechnical.on_bottom_stability import (
            inertia_force_per_meter,
        )
        result = inertia_force_per_meter(
            acceleration_ms2=0.5,
            pipe_od_m=0.5,
            water_density_kg_m3=1025.0,
            inertia_coeff=3.29,
        )
        expected = 1025.0 * 3.29 * math.pi / 4 * 0.5**2 * 0.5
        assert abs(result - expected) < 1e-4


class TestLateralStability:
    def test_stable_pipe(self):
        """Heavy pipe in mild conditions — should be stable."""
        from digitalmodel.geotechnical.on_bottom_stability import (
            lateral_stability_check,
        )
        result = lateral_stability_check(
            submerged_weight_n_per_m=2000.0,
            horizontal_force_n_per_m=500.0,
            lift_force_n_per_m=200.0,
            friction_coefficient=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is True
        assert result.utilization < 1.0
        assert result.standard == "DNV-RP-F109"

    def test_unstable_pipe(self):
        """Light pipe in severe conditions — should be unstable."""
        from digitalmodel.geotechnical.on_bottom_stability import (
            lateral_stability_check,
        )
        result = lateral_stability_check(
            submerged_weight_n_per_m=200.0,
            horizontal_force_n_per_m=1500.0,
            lift_force_n_per_m=800.0,
            friction_coefficient=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is False
        assert result.utilization > 1.0

    def test_zero_weight_raises(self):
        from digitalmodel.geotechnical.on_bottom_stability import (
            lateral_stability_check,
        )
        with pytest.raises(ValueError):
            lateral_stability_check(
                submerged_weight_n_per_m=0.0,
                horizontal_force_n_per_m=100.0,
                lift_force_n_per_m=50.0,
                friction_coefficient=0.6,
                safety_factor=1.1,
            )

    def test_check_lateral_stability_passes(self):
        """Pipeline is laterally stable when friction > horizontal load."""
        from digitalmodel.geotechnical.on_bottom_stability import (
            check_lateral_stability,
        )
        result = check_lateral_stability(
            submerged_weight_per_meter_kn=2.0,
            horizontal_load_per_meter_kn=0.5,
            lift_load_per_meter_kn=0.3,
            friction_coeff=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is True
        assert result.utilization < 1.0
        assert result.standard == "DNV-RP-F109"

    def test_check_lateral_stability_fails(self):
        """Pipeline is NOT laterally stable when friction < horizontal load."""
        from digitalmodel.geotechnical.on_bottom_stability import (
            check_lateral_stability,
        )
        result = check_lateral_stability(
            submerged_weight_per_meter_kn=0.5,
            horizontal_load_per_meter_kn=2.0,
            lift_load_per_meter_kn=0.5,
            friction_coeff=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is False
        assert result.utilization > 1.0


class TestVerticalStability:
    def test_vertical_stability_passes(self):
        """Pipeline is vertically stable when weight > lift force."""
        from digitalmodel.geotechnical.on_bottom_stability import (
            check_vertical_stability,
        )
        result = check_vertical_stability(
            submerged_weight_per_meter_kn=2.0,
            lift_load_per_meter_kn=0.5,
            safety_factor=1.1,
        )
        assert result.is_stable is True

    def test_vertical_stability_fails(self):
        from digitalmodel.geotechnical.on_bottom_stability import (
            check_vertical_stability,
        )
        result = check_vertical_stability(
            submerged_weight_per_meter_kn=0.3,
            lift_load_per_meter_kn=0.5,
            safety_factor=1.1,
        )
        assert result.is_stable is False

    def test_zero_weight_raises(self):
        from digitalmodel.geotechnical.on_bottom_stability import (
            check_vertical_stability,
        )
        with pytest.raises(ValueError):
            check_vertical_stability(
                submerged_weight_per_meter_kn=0.0,
                lift_load_per_meter_kn=0.5,
                safety_factor=1.1,
            )
