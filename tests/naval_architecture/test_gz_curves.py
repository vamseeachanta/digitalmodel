# ABOUTME: Tests for GZ curve fixtures against stability.gz_from_cross_curves()
# ABOUTME: Validates DDG-51 and generic tanker conditions with IMO criteria
"""GZ curve validation tests — fixture data vs gz_from_cross_curves()."""

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture.stability import gz_from_cross_curves

FIXTURE_PATH = (
    Path(__file__).parents[1]
    / "fixtures/test_vectors/naval_architecture/gz_curves.yaml"
)
SHARED_ARTIFACT_PATH = (
    Path(__file__).parents[1]
    / "data/doc-intelligence/digitized-curves/gz-curves.yaml"
)


@pytest.fixture(scope="module")
def gz_data():
    with open(FIXTURE_PATH) as f:
        return yaml.safe_load(f)


@pytest.fixture(scope="module")
def conditions(gz_data):
    return gz_data["conditions"]


@pytest.fixture(scope="module")
def shared_gz_data():
    with open(SHARED_ARTIFACT_PATH) as f:
        return yaml.safe_load(f)


def _get(conditions, cid):
    for c in conditions:
        if c["id"] == cid:
            return c
    raise ValueError(f"Condition {cid} not found")


def _area_between(heel_deg, gz_values, start, end):
    """Trapezoidal area under GZ between start and end (length-rad)."""
    area = 0.0
    for i in range(len(heel_deg) - 1):
        if heel_deg[i] >= start and heel_deg[i + 1] <= end:
            dtheta = math.radians(heel_deg[i + 1] - heel_deg[i])
            area += 0.5 * (gz_values[i] + gz_values[i + 1]) * dtheta
    return area


def _imo_criterion(gz_data, symbol):
    return next(
        c["minimum"]
        for c in gz_data["imo_intact_stability_criteria"]
        if c["symbol"] == symbol
    )


class TestDDG51FullLoad:
    """DDG-51 at 8600 LT, KG=23.84 ft."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get(conditions, "ddg51_full_load")
        tol = cond["tolerance_ft"]
        for heel, kn, gz_exp in zip(
            cond["heel_angles_deg"],
            cond["kn_values_ft"],
            cond["gz_values_ft"],
        ):
            gz = gz_from_cross_curves(heel, kn, cond["kg_ft"])
            assert gz == pytest.approx(gz_exp, abs=tol), (
                f"GZ mismatch at {heel} deg"
            )

    def test_gz_max_angle(self, conditions):
        cond = _get(conditions, "ddg51_full_load")
        gz = cond["gz_values_ft"]
        assert cond["heel_angles_deg"][gz.index(max(gz))] == cond["gz_max_angle_deg"]

    def test_gz_max_value(self, conditions):
        cond = _get(conditions, "ddg51_full_load")
        assert max(cond["gz_values_ft"]) == pytest.approx(cond["gz_max_ft"], abs=0.01)

    def test_vanishing_angle_positive_range(self, conditions):
        cond = _get(conditions, "ddg51_full_load")
        van = cond["vanishing_angle_deg"]
        for heel, gz in zip(cond["heel_angles_deg"], cond["gz_values_ft"]):
            if 0 < heel < van:
                assert gz > 0, f"GZ should be positive at {heel} deg"


class TestDDG51Light:
    """DDG-51 at 8600 LT, KG=21.0 ft (light condition)."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get(conditions, "ddg51_light")
        tol = cond["tolerance_ft"]
        for heel, kn, gz_exp in zip(
            cond["heel_angles_deg"],
            cond["kn_values_ft"],
            cond["gz_values_ft"],
        ):
            gz = gz_from_cross_curves(heel, kn, cond["kg_ft"])
            assert gz == pytest.approx(gz_exp, abs=tol)

    def test_lower_kg_gives_larger_gz(self, conditions):
        full = _get(conditions, "ddg51_full_load")
        light = _get(conditions, "ddg51_light")
        for gz_f, gz_l, heel in zip(
            full["gz_values_ft"], light["gz_values_ft"],
            full["heel_angles_deg"],
        ):
            if heel > 0:
                assert gz_l > gz_f, f"Light GZ should exceed full at {heel}"

    def test_vanishing_angle_greater_than_full_load(self, conditions):
        full = _get(conditions, "ddg51_full_load")
        light = _get(conditions, "ddg51_light")
        assert light["vanishing_angle_deg"] > full["vanishing_angle_deg"]


class TestGenericTanker:
    """Generic tanker Cb=0.80 in metric units."""

    def test_gz_from_cross_curves_matches_fixture(self, conditions):
        cond = _get(conditions, "generic_tanker_cb080")
        tol = cond["tolerance_m"]
        for heel, kn, gz_exp in zip(
            cond["heel_angles_deg"],
            cond["kn_values_m"],
            cond["gz_values_m"],
        ):
            gz = gz_from_cross_curves(heel, kn, cond["kg_m"])
            assert gz == pytest.approx(gz_exp, abs=tol)

    def test_gz_max_angle(self, conditions):
        cond = _get(conditions, "generic_tanker_cb080")
        gz = cond["gz_values_m"]
        assert cond["heel_angles_deg"][gz.index(max(gz))] == cond["gz_max_angle_deg"]

    def test_positive_gz_at_90_deg(self, conditions):
        cond = _get(conditions, "generic_tanker_cb080")
        assert cond["gz_values_m"][-1] > 0


class TestIMOCriteria:
    """IMO intact stability checks on GZ curves."""

    def test_gz_at_30_deg_exceeds_minimum(self, conditions, gz_data):
        cond = _get(conditions, "generic_tanker_cb080")
        idx_30 = cond["heel_angles_deg"].index(30)
        assert cond["gz_values_m"][idx_30] >= _imo_criterion(gz_data, "GZ_30")

    def test_gz_max_angle_exceeds_25_deg(self, conditions, gz_data):
        phi_min = _imo_criterion(gz_data, "phi_GZmax")
        for cond in conditions:
            assert cond["gz_max_angle_deg"] >= phi_min, (
                f"{cond['id']}: GZmax angle below {phi_min}"
            )

    def test_area_0_to_30_exceeds_minimum(self, conditions, gz_data):
        cond = _get(conditions, "generic_tanker_cb080")
        area = _area_between(cond["heel_angles_deg"], cond["gz_values_m"], 0, 30)
        assert area >= _imo_criterion(gz_data, "A_0_30")

    def test_area_0_to_40_exceeds_minimum(self, conditions, gz_data):
        cond = _get(conditions, "generic_tanker_cb080")
        area = _area_between(cond["heel_angles_deg"], cond["gz_values_m"], 0, 40)
        assert area >= _imo_criterion(gz_data, "A_0_40")

    def test_area_30_to_40_exceeds_minimum(self, conditions, gz_data):
        cond = _get(conditions, "generic_tanker_cb080")
        area = _area_between(cond["heel_angles_deg"], cond["gz_values_m"], 30, 40)
        assert area >= _imo_criterion(gz_data, "A_30_40")


class TestFixtureCoverageContract:
    """Contract tests for WRK-1381 fixture breadth and traceability."""

    def test_has_at_least_ten_conditions(self, conditions):
        assert len(conditions) >= 10, "WRK-1381 requires >= 10 traced GZ conditions"

    def test_every_condition_declares_provenance_type(self, conditions):
        allowed = {"figure_digitized", "tabulated_transcription"}
        for cond in conditions:
            provenance_type = cond.get("provenance_type")
            assert provenance_type in allowed, (
                f"{cond['id']}: provenance_type must be one of {sorted(allowed)}"
            )

    def test_figure_digitized_conditions_include_accuracy_metadata(self, conditions):
        for cond in conditions:
            if cond.get("provenance_type") != "figure_digitized":
                continue
            checkpoints = cond.get("source_checkpoints")
            max_error_pct = cond.get("max_error_pct")
            assert isinstance(checkpoints, list) and len(checkpoints) >= 5, (
                f"{cond['id']}: figure_digitized conditions require >= 5 source_checkpoints"
            )
            assert max_error_pct is not None and max_error_pct <= 2.0, (
                f"{cond['id']}: figure_digitized conditions require max_error_pct <= 2.0"
            )


class TestSharedDigitizedCurveArtifact:
    """Contract tests for the shared digitized-curve artifact bridge."""

    def test_shared_artifact_exists(self):
        assert SHARED_ARTIFACT_PATH.exists(), (
            "WRK-1381 requires data/doc-intelligence/digitized-curves/gz-curves.yaml"
        )

    def test_shared_artifact_covers_fixture_condition_ids(
        self, conditions, shared_gz_data
    ):
        fixture_ids = {cond["id"] for cond in conditions}
        artifact_ids = {curve["id"] for curve in shared_gz_data["curves"]}
        assert artifact_ids == fixture_ids

    def test_shared_artifact_declares_standard_curve_arrays(self, shared_gz_data):
        for curve in shared_gz_data["curves"]:
            heel_angles = curve.get("heel_angles_deg")
            gz_values = curve.get("gz_values")
            assert isinstance(heel_angles, list) and heel_angles, (
                f"{curve['id']}: heel_angles_deg must be a non-empty list"
            )
            assert isinstance(gz_values, list) and gz_values, (
                f"{curve['id']}: gz_values must be a non-empty list"
            )
            assert len(heel_angles) == len(gz_values), (
                f"{curve['id']}: heel_angles_deg and gz_values must have equal length"
            )
