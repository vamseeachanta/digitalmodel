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

        added, skipped = register_fleet_vessels([SLEIPNIR_RECORD, THIALF_RECORD])

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
