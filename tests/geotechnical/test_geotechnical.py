# ABOUTME: Tests for digitalmodel.geotechnical package — imports, calculations, edge cases.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1589).
"""
Tests for digitalmodel.geotechnical

Covers:
- Package imports (scour, piles, anchors, on_bottom_stability)
- Dataclass result objects
- Pure math calculations with known inputs
- Edge cases: invalid inputs, zero/negative values
- DNV/API standard references
"""

import math
import pytest


class TestPackageImport:
    """Verify geotechnical package is importable."""

    def test_import_package(self):
        import digitalmodel.geotechnical
        assert digitalmodel.geotechnical is not None

    def test_import_scour(self):
        from digitalmodel.geotechnical import scour
        assert hasattr(scour, "pipeline_scour_depth")
        assert hasattr(scour, "monopile_scour_depth")

    def test_import_piles(self):
        from digitalmodel.geotechnical import piles
        assert hasattr(piles, "skin_friction_clay")
        assert hasattr(piles, "axial_capacity")

    def test_import_anchors(self):
        from digitalmodel.geotechnical import anchors
        assert hasattr(anchors, "drag_anchor_capacity")
        assert hasattr(anchors, "suction_anchor_capacity")

    def test_import_on_bottom_stability(self):
        from digitalmodel.geotechnical import on_bottom_stability
        assert hasattr(on_bottom_stability, "submerged_weight")
        assert hasattr(on_bottom_stability, "lateral_stability_check")


class TestAnchors:
    """Test anchor holding capacity calculations."""

    def test_drag_anchor_soft_clay(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        result = drag_anchor_capacity(
            anchor_weight_kn=100.0, soil_type="soft_clay", anchor_type="stevpris"
        )
        assert result.holding_capacity_kn == pytest.approx(3000.0)
        assert result.efficiency == 30.0
        assert result.standard == "DNV-RP-E302"

    def test_drag_anchor_sand(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        result = drag_anchor_capacity(
            anchor_weight_kn=50.0, soil_type="sand", anchor_type="bruce"
        )
        assert result.holding_capacity_kn == pytest.approx(50.0 * 38.0)

    def test_drag_anchor_invalid_soil_type(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        with pytest.raises(ValueError, match="Unknown soil type"):
            drag_anchor_capacity(anchor_weight_kn=100, soil_type="gravel", anchor_type="stevpris")

    def test_drag_anchor_invalid_anchor_type(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        with pytest.raises(ValueError, match="Unknown anchor type"):
            drag_anchor_capacity(anchor_weight_kn=100, soil_type="sand", anchor_type="unknown")

    def test_drag_anchor_zero_weight_raises(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        with pytest.raises(ValueError):
            drag_anchor_capacity(anchor_weight_kn=0, soil_type="sand", anchor_type="stevpris")

    def test_suction_anchor_positive_capacity(self):
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        result = suction_anchor_capacity(diameter_m=5.0, length_m=15.0, su_kpa=20.0)
        assert result.total_capacity_kn > 0
        assert result.skin_friction_kn > 0
        assert result.reverse_end_bearing_kn > 0
        assert result.standard == "DNV-RP-E303"

    def test_suction_anchor_components_sum(self):
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        result = suction_anchor_capacity(diameter_m=5.0, length_m=15.0, su_kpa=20.0)
        assert result.total_capacity_kn == pytest.approx(
            result.skin_friction_kn + result.reverse_end_bearing_kn
        )

    def test_suction_anchor_zero_diameter_raises(self):
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        with pytest.raises(ValueError):
            suction_anchor_capacity(diameter_m=0, length_m=15.0, su_kpa=20.0)


class TestOnBottomStability:
    """Test on-bottom stability calculations (DNV-RP-F109)."""

    def test_submerged_weight_steel_pipe(self):
        from digitalmodel.geotechnical.on_bottom_stability import submerged_weight
        result = submerged_weight(
            od_steel_m=0.3048,       # 12 inch pipe
            wt_steel_m=0.0127,       # 0.5 inch wall
            coating_thickness_m=0.05,
            rho_steel=7850.0,
            rho_coating=2250.0,
            rho_contents=800.0,      # oil
            rho_seawater=1025.0,
        )
        # Pipe in oil service should have positive submerged weight
        assert result.ws_n_per_m > 0
        assert result.dry_weight_n_per_m > result.buoyancy_n_per_m

    def test_hydrodynamic_loads_positive(self):
        from digitalmodel.geotechnical.on_bottom_stability import hydrodynamic_loads
        result = hydrodynamic_loads(
            od_total_m=0.4,
            water_velocity_m_s=1.5,
            water_acceleration_m_s2=0.5,
            rho_seawater=1025.0,
            cd=0.7,
            cm=3.29,
        )
        assert result.drag_n_per_m > 0
        assert result.inertia_n_per_m > 0
        assert result.lift_n_per_m > 0
        assert result.total_horizontal_n_per_m == pytest.approx(
            result.drag_n_per_m + result.inertia_n_per_m
        )

    def test_lateral_stability_stable_pipe(self):
        from digitalmodel.geotechnical.on_bottom_stability import lateral_stability_check
        result = lateral_stability_check(
            submerged_weight_n_per_m=2000.0,
            horizontal_force_n_per_m=200.0,
            lift_force_n_per_m=100.0,
            friction_coefficient=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is True
        assert result.utilization < 1.0

    def test_lateral_stability_unstable_pipe(self):
        from digitalmodel.geotechnical.on_bottom_stability import lateral_stability_check
        result = lateral_stability_check(
            submerged_weight_n_per_m=100.0,
            horizontal_force_n_per_m=2000.0,
            lift_force_n_per_m=50.0,
            friction_coefficient=0.6,
            safety_factor=1.1,
        )
        assert result.is_stable is False
        assert result.utilization > 1.0

    def test_lateral_stability_zero_weight_raises(self):
        from digitalmodel.geotechnical.on_bottom_stability import lateral_stability_check
        with pytest.raises(ValueError):
            lateral_stability_check(
                submerged_weight_n_per_m=0.0,
                horizontal_force_n_per_m=100.0,
                lift_force_n_per_m=50.0,
                friction_coefficient=0.6,
                safety_factor=1.1,
            )


class TestScour:
    """Test scour depth calculations (DNV-RP-F107)."""

    def test_pipeline_scour_positive(self):
        from digitalmodel.geotechnical.scour import pipeline_scour_depth
        result = pipeline_scour_depth(
            pipe_od_m=0.5,
            current_velocity_ms=0.8,
            wave_orbital_velocity_ms=0.5,
            sediment_d50_mm=0.2,
        )
        assert result.scour_depth_m > 0
        assert result.pipe_od_m == 0.5
        assert result.standard == "DNV-RP-F107"

    def test_monopile_scour_positive(self):
        from digitalmodel.geotechnical.scour import monopile_scour_depth
        result = monopile_scour_depth(
            pile_diameter_m=6.0, current_velocity_ms=1.0, water_depth_m=20.0
        )
        assert result.scour_depth_m > 0
        assert result.pile_diameter_m == 6.0
