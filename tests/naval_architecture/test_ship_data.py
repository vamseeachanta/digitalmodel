# ABOUTME: Tests for ship hydrostatic data tables
# ABOUTME: Validates principal dimensions and cross curves for DDG-51, FFG-7, CVN-65, AOE-6
"""Tests for ship_data module — hydrostatic tables for 4 vessel classes."""

import pytest


class TestShipRegistry:
    """Ship principal dimensions registry."""

    def test_ddg51_exists(self):
        from digitalmodel.naval_architecture.ship_data import get_ship

        ship = get_ship("DDG-51")
        assert ship is not None
        assert ship["lwl_ft"] == pytest.approx(466, abs=5)

    def test_ffg7_exists(self):
        from digitalmodel.naval_architecture.ship_data import get_ship

        ship = get_ship("FFG-7")
        assert ship is not None
        assert ship["lwl_ft"] == pytest.approx(408, abs=5)

    def test_cvn65_exists(self):
        from digitalmodel.naval_architecture.ship_data import get_ship

        ship = get_ship("CVN-65")
        assert ship is not None
        assert ship["loa_ft"] == pytest.approx(1088, abs=10)

    def test_aoe6_exists(self):
        from digitalmodel.naval_architecture.ship_data import get_ship

        ship = get_ship("AOE-6")
        assert ship is not None

    def test_unknown_ship_returns_none(self):
        from digitalmodel.naval_architecture.ship_data import get_ship

        assert get_ship("UNKNOWN-99") is None

    def test_list_ships(self):
        from digitalmodel.naval_architecture.ship_data import list_ships

        ships = list_ships()
        assert len(ships) >= 4
        assert "DDG-51" in ships


class TestDDG51Hydrostatics:
    """DDG-51 hydrostatic data from EN400."""

    def test_cross_curves_exist(self):
        from digitalmodel.naval_architecture.ship_data import (
            get_cross_curves,
        )

        curves = get_cross_curves("DDG-51")
        assert curves is not None
        assert "heel_angles_deg" in curves
        assert "kn_values_ft" in curves

    def test_kn_at_10_deg(self):
        from digitalmodel.naval_architecture.ship_data import (
            get_cross_curves,
        )

        curves = get_cross_curves("DDG-51")
        # EN400 p.153: KN at 10° = 5.08 ft for 8600 LT
        idx = curves["heel_angles_deg"].index(10)
        assert curves["kn_values_ft"][idx] == pytest.approx(5.08, abs=0.1)

    def test_curves_of_form_exist(self):
        from digitalmodel.naval_architecture.ship_data import (
            get_curves_of_form,
        )

        cof = get_curves_of_form("DDG-51")
        assert cof is not None
        assert "drafts_ft" in cof
        assert "displacement_lt" in cof

    def test_displacement_at_16ft_draft(self):
        from digitalmodel.naval_architecture.ship_data import (
            get_curves_of_form,
        )

        cof = get_curves_of_form("DDG-51")
        # EN400 table: at T=16.00, delta=3992 LT
        idx = cof["drafts_ft"].index(16.0)
        assert cof["displacement_lt"][idx] == pytest.approx(3992, abs=50)


class TestIMOComplianceWithShipData:
    """Integration: run IMO checks on ship data."""

    def test_ddg51_passes_imo_intact(self):
        from digitalmodel.naval_architecture.ship_data import (
            get_ship,
            get_cross_curves,
        )
        from digitalmodel.naval_architecture.stability import (
            gz_from_cross_curves,
        )
        from digitalmodel.naval_architecture.damage_stability import (
            check_imo_intact_stability,
        )

        ship = get_ship("DDG-51")
        curves = get_cross_curves("DDG-51")
        kg = ship["kg_ft"]

        # Convert ft to meters for IMO check
        ft_to_m = 0.3048
        angles = curves["heel_angles_deg"]
        gz_m = []
        for i, angle in enumerate(angles):
            kn = curves["kn_values_ft"][i]
            gz = gz_from_cross_curves(angle, kn, kg)
            gz_m.append(gz * ft_to_m)

        gm_m = ship["gm_ft"] * ft_to_m
        result = check_imo_intact_stability(angles, gz_m, gm_m)
        assert result["overall_pass"] is True
