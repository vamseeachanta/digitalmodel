# ABOUTME: Tests for worldenergydata vessel fleet → naval architecture adapter
# ABOUTME: Validates record normalization, partial data, and stability integration
"""Tests for vessel fleet adapter — worldenergydata → naval architecture pipeline."""

import pytest


# ── Conversion constants (duplicated here for test verification) ────
_M_TO_FT = 1.0 / 0.3048
_TONNES_TO_LT = 1.0 / 1.01605


# ── Sample records matching worldenergydata CSV shape ───────────────

SLEIPNIR_RECORD = {
    "VESSEL_NAME": "SLEIPNIR",
    "VESSEL_CATEGORY": "construction",
    "VESSEL_TYPE": "crane_vessel",
    "VESSEL_SUBTYPE": "semi_submersible",
    "OWNER": "Heerema Marine Contractors",
    "LOA_M": 220.0,
    "BEAM_M": 102.0,
    "DRAFT_M": 27.5,
    "DISPLACEMENT_TONNES": None,
    "GROSS_TONNAGE": None,
    "DP_CLASS": 3,
    "YEAR_BUILT": 2019,
    "MAIN_CRANE_CAPACITY_T": 10000.0,
}

THIALF_RECORD = {
    "VESSEL_NAME": "THIALF",
    "VESSEL_CATEGORY": "construction",
    "VESSEL_TYPE": "crane_vessel",
    "VESSEL_SUBTYPE": "semi_submersible",
    "OWNER": "Heerema Marine Contractors",
    "LOA_M": 201.0,
    "BEAM_M": 88.4,
    "DRAFT_M": 31.2,
    "DISPLACEMENT_TONNES": 71368.0,
    "GROSS_TONNAGE": 87647.0,
    "DP_CLASS": 3,
    "YEAR_BUILT": 1985,
    "MAIN_CRANE_CAPACITY_T": 14200.0,
}

PARTIAL_RECORD = {
    "VESSEL_NAME": "PARTIAL VESSEL",
    "VESSEL_CATEGORY": "construction",
    "LOA_M": 150.0,
    "BEAM_M": None,
    "DRAFT_M": None,
}

EMPTY_NAME_RECORD = {
    "VESSEL_NAME": "",
    "LOA_M": 100.0,
    "BEAM_M": 50.0,
    "DRAFT_M": 10.0,
}


class TestNormalizeFleetRecord:
    """Unit tests for normalize_fleet_record — column mapping + unit conversion."""

    def test_full_record_converts_units(self):
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(THIALF_RECORD)

        assert result["hull_id"] == "THIALF"
        assert result["name"] == "THIALF"
        assert result["loa_ft"] == pytest.approx(201.0 * _M_TO_FT, rel=1e-3)
        assert result["beam_ft"] == pytest.approx(88.4 * _M_TO_FT, rel=1e-3)
        assert result["draft_ft"] == pytest.approx(31.2 * _M_TO_FT, rel=1e-3)
        assert result["displacement_lt"] == pytest.approx(
            71368.0 * _TONNES_TO_LT, rel=1e-3
        )

    def test_sleipnir_no_displacement(self):
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(SLEIPNIR_RECORD)

        assert result["hull_id"] == "SLEIPNIR"
        assert result["loa_ft"] == pytest.approx(220.0 * _M_TO_FT, rel=1e-3)
        assert result["beam_ft"] == pytest.approx(102.0 * _M_TO_FT, rel=1e-3)
        # displacement_lt should be absent or None when source is None
        assert result.get("displacement_lt") is None

    def test_partial_record_preserves_available_fields(self):
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(PARTIAL_RECORD)

        assert result["hull_id"] == "PARTIAL VESSEL"
        assert result["loa_ft"] == pytest.approx(150.0 * _M_TO_FT, rel=1e-3)
        assert result.get("beam_ft") is None
        assert result.get("draft_ft") is None

    def test_empty_name_returns_none(self):
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(EMPTY_NAME_RECORD)
        assert result is None

    def test_preserves_vessel_metadata(self):
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(THIALF_RECORD)

        assert result.get("vessel_category") == "construction"
        assert result.get("vessel_type") == "crane_vessel"
        assert result.get("vessel_subtype") == "semi_submersible"
        assert result.get("year_built") == 1985

    def test_loa_ft_sleipnir_sanity(self):
        """Sleipnir is 220m ≈ 722 ft — sanity check."""
        from digitalmodel.naval_architecture.ship_data import normalize_fleet_record

        result = normalize_fleet_record(SLEIPNIR_RECORD)
        assert 700 < result["loa_ft"] < 750


class TestRegisterFleetVessels:
    """Tests for register_fleet_vessels — batch registration into _SHIPS."""

    def test_register_multiple_vessels(self):
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            get_ship,
        )

        # Use overwrite=True to handle test-ordering sensitivity (pytest-randomly)
        added, skipped = register_fleet_vessels(
            [SLEIPNIR_RECORD, THIALF_RECORD], overwrite=True
        )

        assert added >= 1
        # At least one should be retrievable
        ship = get_ship("SLEIPNIR")
        assert ship is not None
        assert ship["loa_ft"] == pytest.approx(220.0 * _M_TO_FT, rel=1e-3)

    def test_skips_invalid_partial_records(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels

        # PARTIAL_RECORD is missing beam_ft and draft_ft — should fail validation
        added, skipped = register_fleet_vessels([PARTIAL_RECORD])
        assert added == 0
        assert skipped == 1

    def test_skips_empty_name(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels

        added, skipped = register_fleet_vessels([EMPTY_NAME_RECORD])
        assert added == 0
        assert skipped == 1

    def test_does_not_overwrite_existing(self):
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            get_ship,
        )

        # First register
        register_fleet_vessels([THIALF_RECORD])
        original = get_ship("THIALF")

        # Second register — should skip
        added, skipped = register_fleet_vessels([THIALF_RECORD])
        assert skipped == 1
        assert added == 0

        # Original data unchanged
        after = get_ship("THIALF")
        assert after["loa_ft"] == original["loa_ft"]

    def test_overwrite_when_requested(self):
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            get_ship,
        )

        register_fleet_vessels([THIALF_RECORD])
        added, skipped = register_fleet_vessels([THIALF_RECORD], overwrite=True)
        assert added == 1
        assert skipped == 0


class TestStabilityIntegration:
    """Integration: fleet vessel dimensions feed into hydrostatics calculations."""

    def test_fleet_vessel_submerged_volume(self):
        """Register Thialf, then compute submerged volume from displacement."""
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            get_ship,
        )
        from digitalmodel.naval_architecture.hydrostatics import submerged_volume

        register_fleet_vessels([THIALF_RECORD])
        ship = get_ship("THIALF")
        assert ship is not None

        disp_lt = ship.get("displacement_lt")
        assert disp_lt is not None
        assert disp_lt > 0

        vol = submerged_volume(disp_lt)
        # Thialf: ~70k tonnes → ~70k LT → vol should be ~2.45M ft³
        assert vol > 1_000_000  # at least 1M ft³ for a 70k tonne vessel
        assert vol < 5_000_000  # but less than 5M ft³

    def test_fleet_vessel_bm_estimate(self):
        """Estimate BM from beam and draft for a registered fleet vessel.

        BM ≈ B² / (12 * T) is a standard rectangular waterplane approximation
        used in naval architecture for initial design estimates.
        """
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            get_ship,
        )

        register_fleet_vessels([SLEIPNIR_RECORD])
        ship = get_ship("SLEIPNIR")
        assert ship is not None

        beam_ft = ship["beam_ft"]
        draft_ft = ship["draft_ft"]

        # BM ≈ B² / (12 * T) — rectangular waterplane approximation
        bm_ft = beam_ft**2 / (12.0 * draft_ft)

        # Sleipnir: B=102m≈335ft, T=27.5m≈90ft → BM ≈ 335²/(12*90) ≈ 104 ft
        assert bm_ft > 50  # must be positive and substantial
        assert bm_ft < 200  # but physically reasonable

    def test_fleet_vessel_list_includes_registered(self):
        """After registration, list_ships includes fleet vessels."""
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels,
            list_ships,
        )

        register_fleet_vessels([SLEIPNIR_RECORD])
        ships = list_ships()
        assert "SLEIPNIR" in ships



DEEPWATER_HORIZON_RIG = dict(
    RIG_NAME="DEEPWATER HORIZON", RIG_TYPE="drillship",
    RIG_STATUS="decommissioned", OWNER="Transocean",
    LOA_M=121.0, BEAM_M=24.0, DISPLACEMENT_TONNES=52587.0,
    WATER_DEPTH_RATING_FT=10000, YEAR_BUILT=2001, DP_CLASS=3,
)

SEMISUB_RIG = dict(
    RIG_NAME="THUNDER HORSE PDQ", RIG_TYPE="semi_submersible",
    OWNER="BP", LOA_M=100.0, BEAM_M=100.0,
    DISPLACEMENT_TONNES=60000.0, WATER_DEPTH_RATING_FT=6000, YEAR_BUILT=2005,
)

PARTIAL_RIG_RECORD = dict(RIG_NAME="SOME RIG", RIG_TYPE="jack_up", WATER_DEPTH_RATING_FT=164)
EMPTY_RIG_RECORD = dict(RIG_TYPE="unknown")


class TestNormalizeDrillingRigRecord:
    def test_full_drillship_record(self):
        from digitalmodel.naval_architecture.ship_data import normalize_drilling_rig_record
        result = normalize_drilling_rig_record(DEEPWATER_HORIZON_RIG)
        assert result is not None
        assert result["hull_id"] == "DEEPWATER HORIZON"
        assert result["loa_ft"] == pytest.approx(121.0 * _M_TO_FT, rel=1e-3)
        assert result["displacement_lt"] == pytest.approx(52587.0 * _TONNES_TO_LT, rel=1e-3)

    def test_rig_type_preserved(self):
        from digitalmodel.naval_architecture.ship_data import normalize_drilling_rig_record
        result = normalize_drilling_rig_record(DEEPWATER_HORIZON_RIG)
        assert result["vessel_type"] == "drillship"
        assert result["vessel_category"] == "drilling"

    def test_water_depth_preserved(self):
        from digitalmodel.naval_architecture.ship_data import normalize_drilling_rig_record
        result = normalize_drilling_rig_record(DEEPWATER_HORIZON_RIG)
        assert result["water_depth_rating_ft"] == 10000

    def test_missing_rig_name_returns_none(self):
        from digitalmodel.naval_architecture.ship_data import normalize_drilling_rig_record
        assert normalize_drilling_rig_record(EMPTY_RIG_RECORD) is None

    def test_partial_rig(self):
        from digitalmodel.naval_architecture.ship_data import normalize_drilling_rig_record
        result = normalize_drilling_rig_record(PARTIAL_RIG_RECORD)
        assert result is not None
        assert result["hull_id"] == "SOME RIG"
        assert result.get("loa_ft") is None

    def test_register_rigs_without_draft(self):
        from digitalmodel.naval_architecture.ship_data import (
            normalize_drilling_rig_record, register_fleet_vessels,
        )
        normalized = normalize_drilling_rig_record(SEMISUB_RIG)
        assert normalized is not None
        added, skipped = register_fleet_vessels([normalized])
        assert skipped >= 1


class TestEstimateVesselHydrostatics:
    def test_thialf_with_displacement(self):
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels, get_ship, estimate_vessel_hydrostatics,
        )
        register_fleet_vessels([THIALF_RECORD])
        ship = get_ship("THIALF")
        hydro = estimate_vessel_hydrostatics(ship)
        assert 0.05 < hydro["cb"] < 0.95
        assert hydro["kb_ft"] > 0
        assert hydro["bm_ft"] > 0
        assert hydro["kg_ft"] > 0
        expected_gm = hydro["kb_ft"] + hydro["bm_ft"] - hydro["kg_ft"]
        assert hydro["gm_ft"] == pytest.approx(expected_gm, rel=1e-6)
        assert hydro["waterplane_area_ft2"] > 0

    def test_ddg51_from_registry(self):
        from digitalmodel.naval_architecture.ship_data import get_ship, estimate_vessel_hydrostatics
        ship = get_ship("DDG-51")
        hydro = estimate_vessel_hydrostatics(ship)
        assert hydro["gm_ft"] > 0
        assert hydro["gm_ft"] < 30

    def test_missing_displacement_fallback(self):
        from digitalmodel.naval_architecture.ship_data import estimate_vessel_hydrostatics
        dims = dict(loa_ft=500.0, beam_ft=60.0, draft_ft=20.0)
        hydro = estimate_vessel_hydrostatics(dims)
        assert 0.3 < hydro["cb"] < 0.9
        assert hydro["gm_ft"] > 0

    def test_missing_critical_dims_returns_none(self):
        from digitalmodel.naval_architecture.ship_data import estimate_vessel_hydrostatics
        assert estimate_vessel_hydrostatics(dict(loa_ft=100.0)) is None
        assert estimate_vessel_hydrostatics(dict()) is None


class TestCurvesOfFormHook:
    def test_ddg51_curves_still_work(self):
        from digitalmodel.naval_architecture.curves_of_form import displacement_at_draft
        disp = displacement_at_draft("DDG-51", 20.0)
        assert disp == pytest.approx(7200, abs=200)

    def test_registered_vessel_estimated_displacement(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels
        from digitalmodel.naval_architecture.curves_of_form import displacement_at_draft
        register_fleet_vessels([THIALF_RECORD])
        draft_ft = 31.2 * _M_TO_FT
        disp = displacement_at_draft("THIALF", draft_ft)
        assert disp > 0
        assert 20_000 < disp < 200_000

    def test_unknown_vessel_raises(self):
        from digitalmodel.naval_architecture.curves_of_form import displacement_at_draft
        with pytest.raises(NotImplementedError):
            displacement_at_draft("UNKNOWN-VESSEL-999", 20.0)


class TestFloatingPlatformStabilityIntegration:
    def test_thialf_stability_check_runs(self):
        from digitalmodel.naval_architecture.ship_data import (
            register_fleet_vessels, get_ship, estimate_vessel_hydrostatics,
        )
        from digitalmodel.naval_architecture.floating_platform_stability import (
            compute_gz_curve, compute_wind_heel, check_intact_stability,
        )
        register_fleet_vessels([THIALF_RECORD])
        ship = get_ship("THIALF")
        hydro = estimate_vessel_hydrostatics(ship)
        assert hydro is not None
        gm_m = hydro["gm_ft"] * 0.3048
        gz_curve = compute_gz_curve(gm_m)
        assert len(gz_curve) > 0
        wind = compute_wind_heel(
            wind_pressure_kpa=0.5, projected_area_m2=5000.0,
            heeling_arm_m=15.0, displacement_t=71368.0, gm_m=gm_m,
        )
        result = check_intact_stability("semisubmersible", gm_m, gz_curve, wind)
        assert isinstance(result.gm_m, float)
        assert result.gm_m > 0
        assert hasattr(result, "intact")
        assert isinstance(result.intact, bool)

    def test_ddg51_submerged_volume_via_estimate(self):
        from digitalmodel.naval_architecture.ship_data import get_ship, estimate_vessel_hydrostatics
        from digitalmodel.naval_architecture.hydrostatics import submerged_volume
        ship = get_ship("DDG-51")
        hydro = estimate_vessel_hydrostatics(ship)
        assert hydro is not None
        vol = submerged_volume(ship["displacement_lt"])
        assert 200_000 < vol < 500_000


# ── Hull Properties Module Tests ─────────────────────────────────────

class TestHullHydrostatics:
    """Tests for hull_properties.hull_hydrostatics."""

    def test_registered_vessel_returns_properties(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels
        from digitalmodel.naval_architecture.hull_properties import hull_hydrostatics

        register_fleet_vessels([THIALF_RECORD], overwrite=True)
        props = hull_hydrostatics("THIALF")

        assert props is not None
        assert props["hull_id"] == "THIALF"
        assert 0.05 < props["cb"] < 0.95
        assert props["gm_ft"] > 0
        assert props["waterplane_area_ft2"] > 0
        assert props["displacement_lt"] > 0
        assert props["submerged_volume_ft3"] > 0

    def test_unknown_vessel_returns_none(self):
        from digitalmodel.naval_architecture.hull_properties import hull_hydrostatics
        assert hull_hydrostatics("NONEXISTENT-VESSEL-XYZ") is None

    def test_ddg51_hull_hydrostatics(self):
        from digitalmodel.naval_architecture.hull_properties import hull_hydrostatics
        props = hull_hydrostatics("DDG-51")
        assert props is not None
        assert props["gm_ft"] > 0
        assert props["gm_ft"] < 30


class TestCapacityPlan:
    """Tests for hull_properties.capacity_plan."""

    def test_basic_capacity_plan(self):
        from digitalmodel.naval_architecture.hull_properties import capacity_plan
        plan = capacity_plan(loa_ft=600.0, beam_ft=80.0, draft_ft=25.0)
        assert plan["moulded_volume_ft3"] > 0
        assert plan["waterplane_area_ft2"] > 0
        assert plan["midship_area_ft2"] > 0
        assert plan["cargo_volume_ft3"] > 0
        assert plan["steel_weight_lt"] > 0

    def test_cargo_less_than_moulded(self):
        from digitalmodel.naval_architecture.hull_properties import capacity_plan
        plan = capacity_plan(loa_ft=600.0, beam_ft=80.0, draft_ft=25.0)
        assert plan["cargo_volume_ft3"] < plan["moulded_volume_ft3"]

    def test_invalid_dimensions_raise(self):
        from digitalmodel.naval_architecture.hull_properties import capacity_plan
        with pytest.raises(ValueError):
            capacity_plan(loa_ft=0, beam_ft=80.0, draft_ft=25.0)
        with pytest.raises(ValueError):
            capacity_plan(loa_ft=600.0, beam_ft=80.0, draft_ft=25.0, cb=0)


class TestStabilityCurveEstimate:
    """Tests for hull_properties.stability_curve_estimate."""

    def test_returns_multiple_points(self):
        from digitalmodel.naval_architecture.hull_properties import stability_curve_estimate
        curve = stability_curve_estimate(gm_ft=5.0, beam_ft=80.0)
        assert len(curve) > 5
        assert curve[0]["angle_deg"] == 0.0
        assert curve[0]["gz_ft"] == 0.0

    def test_gz_positive_at_moderate_angles(self):
        from digitalmodel.naval_architecture.hull_properties import stability_curve_estimate
        curve = stability_curve_estimate(gm_ft=5.0, beam_ft=80.0)
        # GZ should be positive at 30 degrees
        gz_30 = [p for p in curve if p["angle_deg"] == 30.0]
        assert len(gz_30) == 1
        assert gz_30[0]["gz_ft"] > 0

    def test_negative_gm_raises(self):
        from digitalmodel.naval_architecture.hull_properties import stability_curve_estimate
        with pytest.raises(ValueError):
            stability_curve_estimate(gm_ft=-1.0, beam_ft=80.0)


class TestHullWeightGroups:
    """Tests for hull_properties.hull_weight_groups."""

    def test_weight_groups_sum_to_displacement(self):
        from digitalmodel.naval_architecture.hull_properties import hull_weight_groups
        groups = hull_weight_groups(70000.0, vessel_type="crane_vessel")
        total = sum(groups.values())
        assert total == pytest.approx(70000.0, rel=1e-3)

    def test_fpso_weight_groups(self):
        from digitalmodel.naval_architecture.hull_properties import hull_weight_groups
        groups = hull_weight_groups(100000.0, vessel_type="fpso")
        assert groups["hull_lt"] > 0
        assert groups["payload_lt"] > groups["hull_lt"]  # FPSO payload-heavy


# ── Vessel Type → Platform Type Mapping Tests ────────────────────────

class TestVesselTypeToHostType:
    """Tests for concept_selection.vessel_type_to_host_type."""

    def test_semi_submersible_maps_to_semi(self):
        from digitalmodel.field_development.concept_selection import (
            vessel_type_to_host_type, HostType,
        )
        result = vessel_type_to_host_type(vessel_subtype="semi_submersible")
        assert result is HostType.SEMI

    def test_fpso_maps(self):
        from digitalmodel.field_development.concept_selection import (
            vessel_type_to_host_type, HostType,
        )
        assert vessel_type_to_host_type(vessel_type="fpso") is HostType.FPSO

    def test_crane_vessel_maps_to_semi(self):
        from digitalmodel.field_development.concept_selection import (
            vessel_type_to_host_type, HostType,
        )
        assert vessel_type_to_host_type(vessel_type="crane_vessel") is HostType.SEMI

    def test_subtype_takes_precedence(self):
        from digitalmodel.field_development.concept_selection import (
            vessel_type_to_host_type, HostType,
        )
        # subtype=semi_submersible overrides type=crane_vessel
        result = vessel_type_to_host_type(
            vessel_type="crane_vessel", vessel_subtype="semi_submersible",
        )
        assert result is HostType.SEMI

    def test_unknown_type_returns_none(self):
        from digitalmodel.field_development.concept_selection import vessel_type_to_host_type
        assert vessel_type_to_host_type(vessel_type="submarine") is None

    def test_none_inputs_returns_none(self):
        from digitalmodel.field_development.concept_selection import vessel_type_to_host_type
        assert vessel_type_to_host_type() is None


# ── Gyradius for Fleet Vessel Tests ──────────────────────────────────

class TestGyradiusForFleetVessel:
    """Tests for gyradius.gyradius_for_fleet_vessel."""

    def test_thialf_gyradius(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels
        from digitalmodel.naval_architecture.gyradius import gyradius_for_fleet_vessel

        register_fleet_vessels([THIALF_RECORD], overwrite=True)
        result = gyradius_for_fleet_vessel("THIALF")

        assert result.roll > 0
        assert result.pitch > 0
        assert result.yaw > 0
        assert result.displacement_t > 0
        # Thialf ~71k tonnes → displacement_t should be close
        assert 60_000 < result.displacement_t < 80_000

    def test_sleipnir_no_displacement_uses_estimate(self):
        from digitalmodel.naval_architecture.ship_data import register_fleet_vessels
        from digitalmodel.naval_architecture.gyradius import gyradius_for_fleet_vessel

        register_fleet_vessels([SLEIPNIR_RECORD], overwrite=True)
        result = gyradius_for_fleet_vessel("SLEIPNIR")
        assert result.roll > 0
        assert result.displacement_t > 0

    def test_unknown_vessel_raises(self):
        from digitalmodel.naval_architecture.gyradius import gyradius_for_fleet_vessel
        with pytest.raises(KeyError):
            gyradius_for_fleet_vessel("NONEXISTENT-VESSEL-999")

