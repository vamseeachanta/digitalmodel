# ABOUTME: Tests for the worldenergydata intervention access-gap calibration adapter.
# ABOUTME: Asserts snapshot loads + band/fleet/cost figures surface + caveats preserved.
"""Tests for digitalmodel.well_access.wed_calibration (W7 cross-repo bridge)."""

from __future__ import annotations

import pytest

from digitalmodel.well_access import (
    WedCalibration,
    BandDemand,
    FleetSupply,
    load_wed_calibration,
)


@pytest.fixture()
def wed() -> WedCalibration:
    return load_wed_calibration()


class TestLoad:
    def test_loads_shipped_snapshot(self, wed):
        assert isinstance(wed, WedCalibration)
        assert wed.bands  # non-empty
        assert isinstance(wed.fleet, FleetSupply)

    def test_provenance_carries_source(self, wed):
        prov = wed.provenance
        assert prov["source_repo"] == "vamseeachanta/worldenergydata"
        assert prov["source_issue"] == "worldenergydata#638"
        assert prov["feeds_issue"] == "digitalmodel#890"
        assert prov["as_of"] == "2026-06-26"

    def test_bands_are_banddemand(self, wed):
        for b in wed.bands:
            assert isinstance(b, BandDemand)


class TestBandFiguresSurface:
    def test_5000_10000_band_figures(self, wed):
        b = wed.band("band_5000_10000")
        assert b.subsea_wells == 270
        assert b.interventions_per_well_floor == pytest.approx(6.434)
        assert b.demand_rig_days_per_yr_central == pytest.approx(2227.5)
        # binding GoM-resident utilization ratio ~4.4x -> the headline access risk
        assert b.utilization_ratio_gom_resident_central == pytest.approx(4.359)
        assert b.exposure_usd_per_yr_central == pytest.approx(1017967500)

    def test_band_500_3000_demand(self, wed):
        b = wed.band("band_500_3000")
        assert b.subsea_wells == 114
        assert b.interventions_per_yr_central == pytest.approx(17.1)

    def test_unknown_band_raises(self, wed):
        with pytest.raises(KeyError):
            wed.band("band_does_not_exist")

    def test_empirical_summary_carries_floor_note(self, wed):
        summary = wed.empirical_demand_summary()
        assert len(summary) == len(wed.bands)
        row = next(r for r in summary if r["band"] == "band_5000_10000")
        assert "lifetime UPPER proxy" in row["floor_note"]
        assert row["confidence"] == "forward_looking_risk"


class TestFleetSupply:
    def test_global_vs_gom_resident(self, wed):
        f = wed.fleet
        assert f.global_eligible_fleet_count == 471
        assert f.gom_resident_eligible_heavy_count == 2
        assert "Helix Q4000" in f.gom_resident_eligible_heavy_units
        # GEOGRAPHY (#628): ~0 resident dedicated LIGHT-intervention fleet
        assert f.gom_resident_light_intervention_count == 0
        # binding GoM-resident supply << global pool
        assert (
            f.rig_days_per_yr_gom_resident_central
            < f.rig_days_per_yr_global_central
        )

    def test_supply_scope_selector(self, wed):
        assert wed.heavy_pool_available_days_per_year("gom_resident") == pytest.approx(
            511.0
        )
        assert wed.heavy_pool_available_days_per_year("global") == pytest.approx(
            120340.5
        )
        with pytest.raises(ValueError):
            wed.heavy_pool_available_days_per_year("mars")


class TestCalibrationDerivations:
    def test_calibrated_resource_pools_use_real_supply(self, wed):
        pools = wed.calibrated_resource_pools(supply_scope="gom_resident")
        assert len(pools) == 1
        p = pools[0]
        assert p["resource_class"] == "modu"
        assert p["available_days_per_year"] == pytest.approx(511.0)
        assert p["_wed_supply_scope"] == "gom_resident"

    def test_access_multiplier_caps_when_demand_exceeds_supply(self, wed):
        # band_5000_10000 ratio ~4.359 -> m_access ~ 1/4.359 ~ 0.229
        m = wed.access_multiplier_from_supply_ratio("band_5000_10000")
        assert m["m_access"] == pytest.approx(0.2294, abs=1e-3)
        assert m["confidence"] == "forward_looking_risk"
        assert "FORWARD-LOOKING" in m["caveat"]

    def test_access_multiplier_unity_when_no_demand(self, wed):
        # shelf band has zero subsea wells / zero utilization ratio -> no suppression
        m = wed.access_multiplier_from_supply_ratio("shelf_lt_500")
        assert m["m_access"] == pytest.approx(1.0)


class TestCaveatsPreserved:
    def test_caveats_carry_forward_looking_and_geography(self, wed):
        joined = " ".join(wed.caveats)
        assert "FORWARD-LOOKING" in joined
        assert "FLOOR" in joined
        assert "#628" in joined  # GoM resident light fleet geography
        assert "PRESSURE CLASS" in joined

    def test_dayrate_helix_not_public(self, wed):
        helix = wed.dayrates["heavy_intervention_semi"]
        assert helix["rate_disclosed"] is False
        assert helix["median_usd_per_day"] is None


class TestRouterIntegrationAdditive:
    def test_router_wed_block_optional_and_off_by_default(self):
        # Without a wed_calibration block, the payload must not gain the wed key.
        from digitalmodel.well_access.router import router

        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            cfg = {
                "_config_dir_path": tmp,
                "well_access": {
                    "well_program": {
                        "n_wells": 6,
                        "field_life_years": 20,
                        "access_class": "subsea_hub_spoke",
                    },
                    "calibration": {
                        "base_rates": {
                            "workover": {
                                "duration_days": 45.0,
                                "resource_class": "modu",
                                "age_curve": [
                                    {
                                        "age_min_yr": 0,
                                        "age_max_yr": 20,
                                        "rate_per_well_year": 0.03,
                                    }
                                ],
                            }
                        }
                    },
                    "access_multiplier": {"subsea_hub_spoke": {"workover": 0.6}},
                    "outputs": {"directory": tmp},
                },
            }
            out = router(cfg)["well_access"]
            assert "wed_calibration" not in out

    def test_router_wed_block_engages_when_requested(self):
        from digitalmodel.well_access.router import router

        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            cfg = {
                "_config_dir_path": tmp,
                "well_access": {
                    "well_program": {
                        "n_wells": 6,
                        "field_life_years": 20,
                        "access_class": "subsea_hub_spoke",
                    },
                    "calibration": {
                        "base_rates": {
                            "workover": {
                                "duration_days": 45.0,
                                "resource_class": "modu",
                                "age_curve": [
                                    {
                                        "age_min_yr": 0,
                                        "age_max_yr": 20,
                                        "rate_per_well_year": 0.03,
                                    }
                                ],
                            }
                        }
                    },
                    "access_multiplier": {"subsea_hub_spoke": {"workover": 0.6}},
                    "wed_calibration": {
                        "use_wed_supply": True,
                        "supply_scope": "gom_resident",
                    },
                    "outputs": {"directory": tmp},
                },
            }
            out = router(cfg)["well_access"]
            assert "wed_calibration" in out
            assert (
                out["wed_calibration"]["provenance"]["source_issue"]
                == "worldenergydata#638"
            )
            # supply-calibrated pool feeds the resource queue
            modu = next(
                r
                for r in out["primary"]["resources"]
                if r["resource_class"] == "modu"
            )
            assert modu["available_days"] == pytest.approx(511.0 * 20)
