"""Tests for digitalmodel.orcaflex.installation_analysis module."""

import math

import pytest

from digitalmodel.orcaflex.installation_analysis import (
    DAFInput,
    SlingConfig,
    SplashZoneInput,
    VesselRAO,
    WeightItem,
    WeightManagement,
)


class TestVesselRAO:
    """Tests for vessel RAO and crane tip motion."""

    def test_crane_tip_heave_zero_hs(self):
        """Zero Hs should give zero crane tip motion."""
        rao = VesselRAO()
        assert rao.crane_tip_heave(0.0) == pytest.approx(0.0)

    def test_crane_tip_heave_increases_with_hs(self):
        """Crane tip heave should increase with Hs."""
        rao = VesselRAO()
        h1 = rao.crane_tip_heave(1.0)
        h2 = rao.crane_tip_heave(3.0)
        assert h2 > h1

    def test_crane_tip_velocity_positive(self):
        """Crane tip velocity should be positive for non-zero Hs."""
        rao = VesselRAO()
        v = rao.crane_tip_velocity(hs=2.0, tp=10.0)
        assert v > 0

    def test_velocity_increases_with_shorter_period(self):
        """Shorter period should give higher velocity."""
        rao = VesselRAO()
        v_long = rao.crane_tip_velocity(hs=2.0, tp=15.0)
        v_short = rao.crane_tip_velocity(hs=2.0, tp=6.0)
        assert v_short > v_long


class TestDAF:
    """Tests for Dynamic Amplification Factor."""

    def test_daf_at_least_one(self):
        """DAF should be at least 1.0."""
        daf_input = DAFInput(
            static_weight_kN=500.0,
            crane_tip_heave_m=1.0,
            sling_stiffness_kN_per_m=5000.0,
            lifted_mass_kg=50000.0,
        )
        result = daf_input.calculate_daf(wave_period=10.0)
        assert result["daf"] >= 1.0

    def test_daf_increases_with_heave(self):
        """Larger crane tip heave should give higher DAF."""
        base = dict(
            static_weight_kN=10000.0,
            sling_stiffness_kN_per_m=5000.0,
            lifted_mass_kg=50000.0,
            crane_tip_velocity_mps=0.5,
        )
        daf1 = DAFInput(crane_tip_heave_m=0.5, **base).calculate_daf()["daf"]
        daf2 = DAFInput(crane_tip_heave_m=2.0, **base).calculate_daf()["daf"]
        assert daf2 > daf1

    def test_daf_clamps_high_dynamic_loading(self):
        """Very high dynamic loading should clamp to the model cap."""
        daf_input = DAFInput(
            static_weight_kN=500.0,
            crane_tip_heave_m=2.0,
            sling_stiffness_kN_per_m=5000.0,
            lifted_mass_kg=50000.0,
            crane_tip_velocity_mps=0.5,
        )
        assert daf_input.calculate_daf()["daf"] == pytest.approx(5.0)

    def test_natural_period_positive(self):
        """System natural period should be positive."""
        daf_input = DAFInput(
            static_weight_kN=100.0,
            sling_stiffness_kN_per_m=10000.0,
            lifted_mass_kg=10000.0,
        )
        assert daf_input.natural_period > 0


class TestSlingConfig:
    """Tests for sling tension calculation."""

    def test_sling_tension_per_leg(self):
        """Sling tension should account for angle and number of slings."""
        sling = SlingConfig(num_slings=4, sling_angle_deg=60.0, sling_mbl_kN=2000.0)
        result = sling.calculate_sling_tension(hook_load_kN=1000.0)
        # DNVGL-ST-E273, 2.7-3 Portable offshore units, April 2016, §3.5.5:
        # matched four-leg SKL=1.33; T=1000*0.25*1.33*1.05/sin(60)=403.1 kN.
        assert result["sling_tension_per_leg_kN"] == pytest.approx(403.1)
        assert result["most_loaded_share"] == 0.25
        assert "equal split assumed" in result["load_share_basis"]

    @pytest.mark.parametrize("legs, skl", [(1, 1.0), (2, 1.1), (3, 1.1), (4, 1.33)])
    def test_default_skl_by_leg_count(self, legs, skl):
        sling = SlingConfig(num_slings=legs)
        result = sling.calculate_sling_tension(1000.0)
        assert sling.skew_load_factor is None
        assert sling.effective_skew_load_factor == skl
        assert result["skew_load_factor_input"] is None
        assert result["skew_load_factor"] == skl
        if legs > 1:
            assert "DNVGL-ST-E273" in result["skew_load_factor_source"]
            assert "April 2016" in result["skew_load_factor_source"]
            citation = result["citations"][0]
            assert citation["section"] == "3.5.5"
            assert citation["revision"] == "2016-04"
            assert citation["title"] == "2.7-3 Portable offshore units"
            assert citation["url"] == "https://www.dnv.com/rules-standards/"
            assert "POU standard" in citation["note"]
            assert "DNV-ST-N001 Sec.16" in citation["note"]
        else:
            assert "modelling convention" in result["clause_scope"]

    def test_offset_cog_exceeds_equal_split(self):
        # 2 points at x=-2,+2 m; extreme CoG x=+1 m. Moment balance:
        # R_right*4 = W*3 => shares 0.25,0.75 (SKL excludes this effect).
        sling = SlingConfig(
            num_slings=2, lift_points_m=[(-2.0, 0.0), (2.0, 0.0)],
            cog_offset_m=(1.0, 0.0),
        )
        result = sling.calculate_sling_tension(1000.0)
        equal = SlingConfig(num_slings=2).calculate_sling_tension(1000.0)
        assert result["sling_tension_per_leg_kN"] > equal["sling_tension_per_leg_kN"]
        assert result["most_loaded_share"] == pytest.approx(0.75)
        assert result["sling_tension_per_leg_kN"] == pytest.approx(1000.3)
        assert "statics" in result["load_share_basis"]

    def test_three_leg_hand_calculation(self):
        # Triangle (0,0),(6,0),(0,3), centroid (2,1), CoG (3,0.75).
        # Sum R=W; 6*R2=3*W; 3*R3=0.75*W => (0.25,0.5,0.25).
        sling = SlingConfig(
            num_slings=3, lift_points_m=[(0, 0), (6, 0), (0, 3)],
            cog_offset_m=(1.0, -0.25), sling_angle_deg=30, tilt_factor=1.0,
            sling_mbl_kN=3000,
        )
        result = sling.calculate_sling_tension(1000)
        assert result["most_loaded_share"] == pytest.approx(0.5)
        assert result["sling_tension_per_leg_kN"] == pytest.approx(1100.0)
        assert result["sling_utilisation"] == pytest.approx(0.3667)
        assert result["achieved_safety_factor"] == pytest.approx(2.73)
        assert result["pass"] is False

    def test_four_leg_extreme_cog_share_hand_calculation(self):
        # Independently supplied PL=40% at extreme CoG, not another SKL.
        sling = SlingConfig(
            num_slings=4, most_loaded_share=0.4, sling_angle_deg=30,
            tilt_factor=1.0,
        )
        result = sling.calculate_sling_tension(1000)
        assert result["sling_tension_per_leg_kN"] == pytest.approx(1064.0)
        assert result["most_loaded_share"] == 0.4
        assert "extreme CoG" in result["load_share_basis"]

    def test_explicit_skl_override_is_reported(self):
        result = SlingConfig(skew_load_factor=1.2).calculate_sling_tension(1000)
        assert result["skew_load_factor_input"] == 1.2
        assert result["skew_load_factor"] == 1.33
        assert result["sling_tension_per_leg_kN"] == pytest.approx(403.1)
        assert "explicit override" in result["skew_load_factor_source"]
        assert result["skew_load_factor_below_minimum"] is True
        assert result["achieved_safety_factor"] > 3.0
        assert result["pass"] is False

    @pytest.mark.parametrize("legs, minimum, tension", [
        (2, 1.1, 666.8), (3, 1.1, 444.6), (4, 1.33, 403.1),
    ])
    def test_below_minimum_skl_cannot_pass(self, legs, minimum, tension):
        result = SlingConfig(
            num_slings=legs, skew_load_factor=1.0, sling_mbl_kN=10000,
        ).calculate_sling_tension(1000)
        assert result["skew_load_factor_input"] == 1.0
        assert result["skew_load_factor"] == minimum
        assert result["sling_tension_per_leg_kN"] == pytest.approx(tension)
        assert result["skew_load_factor_below_minimum"] is True
        assert result["pass"] is False

    @pytest.mark.parametrize("factor", [None, 1.33, 1.5])
    def test_revalidation_preserves_default_provenance(self, factor):
        sling = SlingConfig(skew_load_factor=factor)
        before = sling.calculate_sling_tension(1000)
        restored = SlingConfig.model_validate(sling.model_dump())
        assert restored is not sling
        assert restored.skew_load_factor == factor
        assert restored.calculate_sling_tension(1000) == before
        assert before["pass"] is True

    @pytest.mark.parametrize("share", [0, 0.24, 1.01, float("nan"), float("inf")])
    def test_invalid_most_loaded_share(self, share):
        with pytest.raises(ValueError):
            SlingConfig(most_loaded_share=share).calculate_sling_tension(1000)

    @pytest.mark.parametrize("kwargs", [
        {"cog_offset_m": (1, 0)},
        {"num_slings": 2, "lift_points_m": [(0, 0), (0, 0)], "cog_offset_m": (0, 0)},
        {"num_slings": 2, "lift_points_m": [(-2, 0), (2, 0)], "cog_offset_m": (0, 1)},
        {"num_slings": 2, "lift_points_m": [(-2, 0), (2, 0)], "cog_offset_m": (3, 0)},
        {"num_slings": 3, "lift_points_m": [(0, 0), (1, 0), (2, 0)], "cog_offset_m": (0, 0)},
        {"num_slings": 3, "lift_points_m": [(0, 0), (6, 0), (0, 3)], "cog_offset_m": (5, 5)},
        {"num_slings": 2, "lift_points_m": [(-2, 0)], "cog_offset_m": (0, 0)},
        {"num_slings": 2, "lift_points_m": [(-2, 0), (2, 0)]},
        {"num_slings": 2, "lift_points_m": [(-2, 0), (2, 0)], "cog_offset_m": (float("nan"), 0)},
        {"num_slings": 2, "lift_points_m": [(-2, 0), (2, 0)], "cog_offset_m": (0, 0), "most_loaded_share": 0.6},
        {"num_slings": 4, "lift_points_m": [(-1, -1), (1, -1), (1, 1), (-1, 1)], "cog_offset_m": (0, 0)},
        {"num_slings": 5},
    ])
    def test_unsupported_or_inconsistent_geometry(self, kwargs):
        with pytest.raises(ValueError):
            SlingConfig(**kwargs).calculate_sling_tension(1000)

    def test_more_than_four_legs_requires_explicit_skl(self):
        result = SlingConfig(
            num_slings=5, skew_load_factor=1.5,
            skew_load_factor_justification="Independent five-leg tolerance assessment",
        ).calculate_sling_tension(1000)
        assert result["skew_load_factor"] == 1.5
        assert "explicit override" in result["skew_load_factor_source"]

    @pytest.mark.parametrize("load", [-1, float("nan"), float("inf")])
    def test_invalid_hook_load(self, load):
        with pytest.raises(ValueError):
            SlingConfig().calculate_sling_tension(load)

    def test_steeper_angle_lower_tension(self):
        """Steeper sling angle should give lower sling tension."""
        sling_steep = SlingConfig(
            sling_angle_deg=80.0, skew_load_factor=1.33,
            skew_load_factor_justification="Independent steep-angle rigging assessment",
        )
        sling_shallow = SlingConfig(sling_angle_deg=30.0)
        t_steep = sling_steep.calculate_sling_tension(500.0)["sling_tension_per_leg_kN"]
        t_shallow = sling_shallow.calculate_sling_tension(500.0)[
            "sling_tension_per_leg_kN"
        ]
        assert t_steep < t_shallow

    @pytest.mark.parametrize("kwargs", [
        {"sling_angle_deg": 75}, {"has_spreader_bars": True},
        {"portable_unit_mgw_t": 100.1}, {"matched_slings": False},
        {"num_slings": 5},
    ])
    def test_outside_clause_scope_requires_justified_skl(self, kwargs):
        for override in ({}, {"skew_load_factor": 1.5}, {
            "skew_load_factor": 1.5, "skew_load_factor_justification": "  ",
        }):
            with pytest.raises(ValueError, match="outside clause scope"):
                SlingConfig(**kwargs, **override)
        result = SlingConfig(
            **kwargs, skew_load_factor=1.5,
            skew_load_factor_justification="Independent rigging tolerance assessment",
        ).calculate_sling_tension(500)
        assert "outside clause scope" in result["clause_scope"]
        assert "outside clause scope" in result["skew_load_factor_source"]
        assert result["skew_load_factor"] == 1.5
        assert result["skew_load_factor_justification"] == "Independent rigging tolerance assessment"

    def test_clause_boundaries_and_matched_sling_assumption(self):
        result = SlingConfig(
            sling_angle_deg=60, portable_unit_mgw_t=100,
        ).calculate_sling_tension(500)
        assert result["skew_load_factor"] == 1.33
        assert result["clause_scope"] == "within clause scope"
        assert result["matched_slings"] is True
        assert "matched slings, EN 13414-1 tolerances" in result["skew_load_factor_source"]
        unknown_mass = SlingConfig().calculate_sling_tension(1000)
        assert "MGW <=100 t assumed; not verified" in unknown_mass["clause_scope"]

    def test_safety_factor_check(self):
        """Should pass with low load, fail with high load."""
        sling = SlingConfig(sling_mbl_kN=2000.0, safety_factor=3.0)
        result_low = sling.calculate_sling_tension(200.0)
        result_high = sling.calculate_sling_tension(5000.0)
        assert result_low["pass"] is True
        assert result_high["pass"] is False


class TestSplashZone:
    """Tests for splash zone analysis."""

    @pytest.mark.xfail(
        strict=True,
        reason="splash-zone added-mass force is divided by 1000 twice",
        raises=AssertionError,
    )
    def test_hook_load_equals_force_component_sum(self):
        """Hook load must include each reported force component once in kN."""
        sz = SplashZoneInput()
        result = sz.calculate_splash_zone_loads()
        components = math.fsum(
            result[key]
            for key in (
                "slamming_force_kN",
                "drag_force_kN",
                "added_mass_force_kN",
            )
        )
        # Four independently rounded outputs can differ by at most 0.2 kN.
        assert result["max_dynamic_hook_load_kN"] == pytest.approx(
            sz.dry_weight_kN + components, abs=0.2
        )

    def test_slamming_force_positive(self):
        """Slamming force should be positive."""
        sz = SplashZoneInput(
            dry_weight_kN=500.0,
            submerged_weight_kN=350.0,
            projected_area_m2=25.0,
            crane_tip_heave_m=1.5,
            wave_period_s=8.0,
        )
        result = sz.calculate_splash_zone_loads()
        assert result["slamming_force_kN"] > 0

    def test_max_load_exceeds_dry_weight(self):
        """Maximum dynamic load should exceed static dry weight."""
        sz = SplashZoneInput(crane_tip_heave_m=2.0)
        result = sz.calculate_splash_zone_loads()
        assert result["max_dynamic_hook_load_kN"] > sz.dry_weight_kN

    def test_zero_heave_small_dynamic(self):
        """Near-zero heave should give small dynamic loads."""
        sz = SplashZoneInput(crane_tip_heave_m=0.01)
        result = sz.calculate_splash_zone_loads()
        # Slamming should be very small
        assert result["slamming_force_kN"] < 1.0


class TestWeightManagement:
    """Tests for weight management."""

    def test_total_weight(self):
        """Total weight should sum all items."""
        wm = WeightManagement(
            items=[
                WeightItem(
                    description="Structure",
                    dry_weight_kN=500.0,
                    submerged_weight_kN=350.0,
                ),
                WeightItem(
                    description="Equipment",
                    dry_weight_kN=200.0,
                    submerged_weight_kN=180.0,
                ),
            ]
        )
        assert wm.total_dry_weight == pytest.approx(700.0)
        assert wm.total_submerged_weight == pytest.approx(530.0)

    def test_contingency(self):
        """Contingency should add percentage to dry weight."""
        wm = WeightManagement(
            items=[
                WeightItem(
                    description="Main", dry_weight_kN=1000.0, submerged_weight_kN=700.0
                )
            ],
            contingency_pct=10.0,
        )
        assert wm.total_with_contingency == pytest.approx(1100.0)

    def test_cog_calculation(self):
        """COG should be weighted average."""
        wm = WeightManagement(
            items=[
                WeightItem(description="A", dry_weight_kN=100.0, cog_x=1.0),
                WeightItem(description="B", dry_weight_kN=100.0, cog_x=3.0),
            ]
        )
        cog = wm.calculate_cog()
        assert cog["cog_x"] == pytest.approx(2.0)

    def test_summary_table(self):
        """Summary table should include totals."""
        wm = WeightManagement(
            items=[
                WeightItem(
                    description="Item1", dry_weight_kN=100.0, submerged_weight_kN=80.0
                ),
            ]
        )
        summary = wm.generate_summary()
        assert len(summary) >= 2  # items + totals
        assert summary[-1]["description"].startswith("TOTAL")
