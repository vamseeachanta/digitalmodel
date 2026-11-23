"""
Tests for member and connection acceptability checks (Child-E).

Checks:
  - Von Mises stress vs 0.6*Fy allowable (ASD)
  - ASME combined stress
  - Unity ratios
  - Bolt shear, weld throat, pin shear
"""

import math
import pytest

from digitalmodel.structural.parachute.member_check import (
    tube_stress,
    von_mises_tube,
    unity_ratio,
    bolt_shear_check,
    weld_throat_check,
    pin_shear_check,
    check_all_members,
    MemberResult,
)
from digitalmodel.structural.parachute.frame_model import (
    CHROMOLY_4130,
    tube_section_properties,
)


class TestTubeStress:
    """Stress calculation for circular hollow tube members."""

    def test_axial_stress(self):
        props = tube_section_properties(1.5, 0.120)
        sigma = tube_stress(axial=1000.0, shear=0.0, moment=0.0,
                            A=props["A"], I=props["I"], od=1.5)
        assert abs(sigma["axial"] - 1000.0 / props["A"]) < 0.1

    def test_bending_stress(self):
        props = tube_section_properties(1.5, 0.120)
        sigma = tube_stress(axial=0.0, shear=0.0, moment=500.0,
                            A=props["A"], I=props["I"], od=1.5)
        expected = 500.0 * (1.5 / 2) / props["I"]
        assert abs(sigma["bending"] - expected) < 0.1

    def test_shear_stress(self):
        props = tube_section_properties(1.5, 0.120)
        sigma = tube_stress(axial=0.0, shear=2000.0, moment=0.0,
                            A=props["A"], I=props["I"], od=1.5)
        # Average shear stress = V / (A/2) for thin-walled tube
        assert sigma["shear_avg"] > 0


class TestVonMises:
    def test_pure_axial(self):
        vm = von_mises_tube(sigma_axial=10000.0, sigma_bending=0.0,
                            tau=0.0)
        assert abs(vm - 10000.0) < 0.1

    def test_pure_shear(self):
        vm = von_mises_tube(sigma_axial=0.0, sigma_bending=0.0,
                            tau=5000.0)
        expected = math.sqrt(3) * 5000.0
        assert abs(vm - expected) < 0.1

    def test_combined(self):
        vm = von_mises_tube(sigma_axial=10000.0, sigma_bending=5000.0,
                            tau=3000.0)
        sigma_total = 10000.0 + 5000.0  # worst case (same sign)
        expected = math.sqrt(sigma_total**2 + 3 * 3000.0**2)
        assert abs(vm - expected) < 0.1


class TestUnityRatio:
    def test_below_one_passes(self):
        ur = unity_ratio(demand=30000.0, capacity=63000.0 * 0.6)
        assert ur < 1.0

    def test_above_one_fails(self):
        ur = unity_ratio(demand=50000.0, capacity=63000.0 * 0.6)
        assert ur > 1.0

    def test_zero_demand(self):
        assert unity_ratio(0.0, 63000.0 * 0.6) == 0.0


class TestBoltShear:
    def test_single_bolt_passes(self):
        # 3/8" Grade 8 bolt, Fv = 90 ksi (shear allowable)
        result = bolt_shear_check(
            shear_force=500.0,
            bolt_diameter=0.375,
            n_bolts=1,
            fv_allowable=54_000.0,  # 0.6 * 90000
        )
        assert result["unity"] < 1.0

    def test_insufficient_bolts(self):
        result = bolt_shear_check(
            shear_force=50_000.0,
            bolt_diameter=0.375,
            n_bolts=1,
            fv_allowable=54_000.0,
        )
        assert result["unity"] > 1.0


class TestWeldThroat:
    def test_fillet_weld_passes(self):
        result = weld_throat_check(
            force=1000.0,
            weld_length=2.0,
            throat=0.120 * 0.707,  # leg = wall thickness
            fw_allowable=21_000.0,  # 0.3 * 70 ksi (E70 electrode)
        )
        assert result["unity"] < 1.0


class TestPinShear:
    def test_double_shear_pin(self):
        result = pin_shear_check(
            shear_force=5000.0,
            pin_diameter=0.5,
            n_shear_planes=2,
            fv_allowable=54_000.0,
        )
        assert result["unity"] < 1.0


class TestCheckAllMembers:
    """Integration: run all checks on GT1R frame solver output."""

    FAKE_MEMBER_FORCES = [
        {"id": 0, "label": "bar_left_end", "nodes": (0, 1),
         "axial_i": -5000, "shear_i": 200, "moment_i": 1000,
         "axial_j": 5000, "shear_j": -200, "moment_j": -500},
        {"id": 4, "label": "v_strut_left", "nodes": (1, 5),
         "axial_i": -15000, "shear_i": 100, "moment_i": 300,
         "axial_j": 15000, "shear_j": -100, "moment_j": -200},
    ]

    def test_returns_results(self):
        results = check_all_members(
            member_forces=self.FAKE_MEMBER_FORCES,
            section=tube_section_properties(1.5, 0.120),
            material=CHROMOLY_4130,
        )
        assert len(results) == 2

    def test_result_has_unity(self):
        results = check_all_members(
            member_forces=self.FAKE_MEMBER_FORCES,
            section=tube_section_properties(1.5, 0.120),
            material=CHROMOLY_4130,
        )
        for r in results:
            assert "unity_vm" in r
            assert "pass" in r

    def test_critical_member_identified(self):
        results = check_all_members(
            member_forces=self.FAKE_MEMBER_FORCES,
            section=tube_section_properties(1.5, 0.120),
            material=CHROMOLY_4130,
        )
        # V-strut has higher axial load → higher unity
        unities = {r["id"]: r["unity_vm"] for r in results}
        assert unities[4] > unities[0]
