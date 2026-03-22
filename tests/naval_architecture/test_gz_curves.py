# ABOUTME: Tests for GZ curve fixtures against stability.gz_from_cross_curves()
# ABOUTME: Validates DDG-51 and generic tanker conditions with IMO criteria
"""
GZ curve validation tests.

Loads gz_curves.yaml fixture, validates computed GZ against
gz_from_cross_curves(), and checks IMO intact stability criteria.
"""

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture.stability import gz_from_cross_curves

FIXTURE_DIR = Path(__file__).parents[1] / "fixtures" / "test_vectors"
FIXTURE_PATH = FIXTURE_DIR / "naval_architecture" / "gz_curves.yaml"


@pytest.fixture(scope="module")
def gz_data():
    """Load GZ curves fixture data."""
    with open(FIXTURE_PATH) as f:
        return yaml.safe_load(f)


@pytest.fixture(scope="module")
def conditions(gz_data):
    """Return list of test conditions from fixture."""
    return gz_data["conditions"]


def _get_condition(conditions, condition_id):
    """Find a condition by its id."""
    for c in conditions:
        if c["id"] == condition_id:
            return c
    raise ValueError(f"Condition {condition_id} not found")


def _area_trapezoid(heel_deg, gz_values):
    """Trapezoidal integration of GZ curve (result in length-rad)."""
    area = 0.0
    for i in range(len(heel_deg) - 1):
        dtheta = math.radians(heel_deg[i + 1] - heel_deg[i])
        area += 0.5 * (gz_values[i] + gz_values[i + 1]) * dtheta
    return area


def _area_between(heel_deg, gz_values, start_deg, end_deg):
    """Area under GZ curve between start and end angles (length-rad)."""
    area = 0.0
    for i in range(len(heel_deg) - 1):
        if heel_deg[i] >= start_deg and heel_deg[i + 1] <= end_deg:
            dtheta = math.radians(heel_deg[i + 1] - heel_deg[i])
            area += 0.5 * (gz_values[i] + gz_values[i + 1]) * dtheta
    return area


class TestDDG51FullLoad:
    """DDG-51 at 8600 LT, KG=23.84 ft."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get_condition(conditions, "ddg51_full_load")
        tol = cond["tolerance_ft"]
        for heel, kn, gz_expected in zip(
            cond["heel_angles_deg"],
            cond["kn_values_ft"],
            cond["gz_values_ft"],
        ):
            gz_calc = gz_from_cross_curves(heel, kn, cond["kg_ft"])
            assert gz_calc == pytest.approx(gz_expected, abs=tol), (
                f"GZ mismatch at {heel} deg: {gz_calc:.3f} vs {gz_expected}"
            )

    def test_gz_max_angle(self, conditions):
        cond = _get_condition(conditions, "ddg51_full_load")
        gz_vals = cond["gz_values_ft"]
        max_idx = gz_vals.index(max(gz_vals))
        assert cond["heel_angles_deg"][max_idx] == cond["gz_max_angle_deg"]

    def test_gz_max_value(self, conditions):
        cond = _get_condition(conditions, "ddg51_full_load")
        gz_max = max(cond["gz_values_ft"])
        assert gz_max == pytest.approx(cond["gz_max_ft"], abs=0.01)

    def test_vanishing_angle_positive_range(self, conditions):
        """GZ should be positive up to near the vanishing angle."""
        cond = _get_condition(conditions, "ddg51_full_load")
        van_angle = cond["vanishing_angle_deg"]
        for heel, gz in zip(
            cond["heel_angles_deg"], cond["gz_values_ft"]
        ):
            if 0 < heel < van_angle:
                assert gz > 0, f"GZ should be positive at {heel} deg"


class TestDDG51Light:
    """DDG-51 at 8600 LT, KG=21.0 ft (light condition)."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get_condition(conditions, "ddg51_light")
        tol = cond["tolerance_ft"]
        for heel, kn, gz_expected in zip(
            cond["heel_angles_deg"],
            cond["kn_values_ft"],
            cond["gz_values_ft"],
        ):
            gz_calc = gz_from_cross_curves(heel, kn, cond["kg_ft"])
            assert gz_calc == pytest.approx(gz_expected, abs=tol)

    def test_lower_kg_gives_larger_gz(self, conditions):
        """Lower KG should produce larger GZ at all positive angles."""
        full = _get_condition(conditions, "ddg51_full_load")
        light = _get_condition(conditions, "ddg51_light")
        for gz_full, gz_light, heel in zip(
            full["gz_values_ft"],
            light["gz_values_ft"],
            full["heel_angles_deg"],
        ):
            if heel > 0:
                assert gz_light > gz_full, (
                    f"Light GZ should exceed full at {heel} deg"
                )

    def test_vanishing_angle_greater_than_full_load(self, conditions):
        full = _get_condition(conditions, "ddg51_full_load")
        light = _get_condition(conditions, "ddg51_light")
        assert light["vanishing_angle_deg"] > full["vanishing_angle_deg"]


class TestGenericTanker:
    """Generic tanker Cb=0.80 in metric units."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get_condition(conditions, "generic_tanker_cb080")
        tol = cond["tolerance_m"]
        for heel, kn, gz_expected in zip(
            cond["heel_angles_deg"],
            cond["kn_values_m"],
            cond["gz_values_m"],
        ):
            gz_calc = gz_from_cross_curves(heel, kn, cond["kg_m"])
            assert gz_calc == pytest.approx(gz_expected, abs=tol)

    def test_gz_max_angle(self, conditions):
        cond = _get_condition(conditions, "generic_tanker_cb080")
        gz_vals = cond["gz_values_m"]
        max_idx = gz_vals.index(max(gz_vals))
        assert cond["heel_angles_deg"][max_idx] == cond["gz_max_angle_deg"]

    def test_positive_gz_at_90_deg(self, conditions):
        """Full-form tanker should have positive GZ even at 90 deg."""
        cond = _get_condition(conditions, "generic_tanker_cb080")
        assert cond["gz_values_m"][-1] > 0


class TestIMOCriteria:
    """IMO intact stability checks on GZ curves."""

    def test_gz_at_30_deg_exceeds_minimum(self, conditions, gz_data):
        """GZ at 30 deg must exceed 0.20 m (IMO A.749)."""
        criteria = gz_data["imo_intact_stability_criteria"]
        gz30_min = next(
            c["minimum"] for c in criteria if c["symbol"] == "GZ_30"
        )
        cond = _get_condition(conditions, "generic_tanker_cb080")
        idx_30 = cond["heel_angles_deg"].index(30)
        assert cond["gz_values_m"][idx_30] >= gz30_min

    def test_gz_max_angle_exceeds_25_deg(self, conditions, gz_data):
        """Angle of maximum GZ must be >= 25 deg (IMO A.749)."""
        criteria = gz_data["imo_intact_stability_criteria"]
        phi_min = next(
            c["minimum"] for c in criteria
            if c["symbol"] == "phi_GZmax"
        )
        for cond in conditions:
            gz_max_angle = cond["gz_max_angle_deg"]
            assert gz_max_angle >= phi_min, (
                f"{cond['id']}: GZmax at {gz_max_angle} deg < {phi_min}"
            )

    def test_area_0_to_30_exceeds_minimum(self, conditions, gz_data):
        """Area under GZ 0-30 deg must exceed 0.055 m-rad."""
        criteria = gz_data["imo_intact_stability_criteria"]
        a030_min = next(
            c["minimum"] for c in criteria if c["symbol"] == "A_0_30"
        )
        cond = _get_condition(conditions, "generic_tanker_cb080")
        area = _area_between(
            cond["heel_angles_deg"], cond["gz_values_m"], 0, 30
        )
        assert area >= a030_min

    def test_area_0_to_40_exceeds_minimum(self, conditions, gz_data):
        """Area under GZ 0-40 deg must exceed 0.09 m-rad."""
        criteria = gz_data["imo_intact_stability_criteria"]
        a040_min = next(
            c["minimum"] for c in criteria if c["symbol"] == "A_0_40"
        )
        cond = _get_condition(conditions, "generic_tanker_cb080")
        area = _area_between(
            cond["heel_angles_deg"], cond["gz_values_m"], 0, 40
        )
        assert area >= a040_min

    def test_area_30_to_40_exceeds_minimum(self, conditions, gz_data):
        """Area under GZ 30-40 deg must exceed 0.03 m-rad."""
        criteria = gz_data["imo_intact_stability_criteria"]
        a3040_min = next(
            c["minimum"] for c in criteria
            if c["symbol"] == "A_30_40"
        )
        cond = _get_condition(conditions, "generic_tanker_cb080")
        area = _area_between(
            cond["heel_angles_deg"], cond["gz_values_m"], 30, 40
        )
        assert area >= a3040_min
