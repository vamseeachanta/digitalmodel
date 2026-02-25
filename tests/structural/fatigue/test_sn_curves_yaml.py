"""
Pinning Tests for SN Curve YAML Data
=====================================

Validates that:
1. The YAML file loads and has the expected structure.
2. Every curve's A, m, and fatigue_limit match the Python source dicts.
3. StandardSNCurves.get_curve() returns PowerLawSNCurve with correct params.
4. StandardSNCurves.list_curves() returns the correct counts per standard.
5. Python class dicts and YAML values are cross-consistent.
"""

import pytest
import yaml
from pathlib import Path

from digitalmodel.structural.fatigue.sn_curves import (
    StandardSNCurves,
    PowerLawSNCurve,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

YAML_PATH = Path(__file__).parents[3] / "data" / "fatigue" / "sn_curves.yaml"

EXPECTED_COUNTS = {
    "DNV": 14,
    "API": 5,
    "BS": 8,
    "AWS": 8,
    "DNV_MULTISLOPE": 2,
}

# Map standard name -> class attribute holding the dict
_STD_TO_ATTR = {
    "DNV": "DNV_CURVES",
    "API": "API_CURVES",
    "BS": "BS_CURVES",
    "AWS": "AWS_CURVES",
}


def _all_single_slope_cases():
    """Yield (standard, curve_class, expected_params) for parametrize."""
    for std, attr in _STD_TO_ATTR.items():
        curves = getattr(StandardSNCurves, attr)
        for cls_name, params in curves.items():
            yield std, cls_name, params


def _all_multislope_cases():
    """Yield (curve_class, expected_params) for multislope parametrize."""
    for cls_name, params in StandardSNCurves.DNV_MULTISLOPE_CURVES.items():
        yield cls_name, params


# ---------------------------------------------------------------------------
# YAML structure tests
# ---------------------------------------------------------------------------


class TestSNCurvesYAML:
    """Pinning tests for SN curve data integrity."""

    @pytest.fixture
    def yaml_data(self):
        with open(YAML_PATH) as f:
            return yaml.safe_load(f)

    # -- file-level checks --------------------------------------------------

    def test_yaml_file_exists_and_loads(self, yaml_data):
        assert "standards" in yaml_data
        assert "metadata" in yaml_data

    def test_yaml_metadata(self, yaml_data):
        meta = yaml_data["metadata"]
        assert meta["version"] == "1.0"
        assert meta["tier"] == 2

    def test_yaml_has_all_standards(self, yaml_data):
        standards = yaml_data["standards"]
        assert set(standards.keys()) == {"DNV", "API", "BS", "AWS"}

    # -- curve count checks -------------------------------------------------

    def test_yaml_curve_count_dnv(self, yaml_data):
        assert len(yaml_data["standards"]["DNV"]["curves"]) == EXPECTED_COUNTS["DNV"]

    def test_yaml_curve_count_api(self, yaml_data):
        assert len(yaml_data["standards"]["API"]["curves"]) == EXPECTED_COUNTS["API"]

    def test_yaml_curve_count_bs(self, yaml_data):
        assert len(yaml_data["standards"]["BS"]["curves"]) == EXPECTED_COUNTS["BS"]

    def test_yaml_curve_count_aws(self, yaml_data):
        assert len(yaml_data["standards"]["AWS"]["curves"]) == EXPECTED_COUNTS["AWS"]

    def test_yaml_multislope_count(self, yaml_data):
        assert (
            len(yaml_data["standards"]["DNV"]["multislope"])
            == EXPECTED_COUNTS["DNV_MULTISLOPE"]
        )


# ---------------------------------------------------------------------------
# Per-curve value pinning (YAML vs hard-coded expected)
# ---------------------------------------------------------------------------


class TestYAMLCurveValues:
    """Verify every YAML curve value matches the Python source constants."""

    @pytest.fixture
    def yaml_data(self):
        with open(YAML_PATH) as f:
            return yaml.safe_load(f)

    @pytest.mark.parametrize(
        "standard, curve_class, expected",
        list(_all_single_slope_cases()),
        ids=[f"{s}-{c}" for s, c, _ in _all_single_slope_cases()],
    )
    def test_yaml_single_slope_values(self, yaml_data, standard, curve_class, expected):
        yaml_curve = yaml_data["standards"][standard]["curves"][curve_class]
        assert yaml_curve["A"] == pytest.approx(expected["A"], rel=1e-6)
        assert yaml_curve["m"] == pytest.approx(expected["m"], rel=1e-6)
        assert yaml_curve["fatigue_limit"] == pytest.approx(
            expected["fatigue_limit"], rel=1e-6
        )

    @pytest.mark.parametrize(
        "curve_class, expected",
        list(_all_multislope_cases()),
        ids=[c for c, _ in _all_multislope_cases()],
    )
    def test_yaml_multislope_values(self, yaml_data, curve_class, expected):
        yaml_ms = yaml_data["standards"]["DNV"]["multislope"][curve_class]
        assert yaml_ms["slopes"] == pytest.approx(expected["slopes"], rel=1e-6)
        assert yaml_ms["constants"] == pytest.approx(expected["constants"], rel=1e-6)
        assert yaml_ms["transition_cycles"] == pytest.approx(
            expected["transition_cycles"], rel=1e-6
        )
        assert yaml_ms["fatigue_limit"] == pytest.approx(
            expected["fatigue_limit"], rel=1e-6
        )


# ---------------------------------------------------------------------------
# StandardSNCurves.get_curve() returns correct PowerLawSNCurve
# ---------------------------------------------------------------------------


class TestGetCurve:
    """Verify get_curve() constructs a PowerLawSNCurve with matching params."""

    @pytest.mark.parametrize(
        "standard, curve_class, expected",
        list(_all_single_slope_cases()),
        ids=[f"{s}-{c}" for s, c, _ in _all_single_slope_cases()],
    )
    def test_get_curve_returns_power_law(self, standard, curve_class, expected):
        curve = StandardSNCurves.get_curve(standard, curve_class)
        assert isinstance(curve, PowerLawSNCurve)
        assert curve.A == pytest.approx(expected["A"], rel=1e-6)
        assert curve.m == pytest.approx(expected["m"], rel=1e-6)
        assert curve.fatigue_limit == pytest.approx(
            expected["fatigue_limit"], rel=1e-6
        )


# ---------------------------------------------------------------------------
# StandardSNCurves.list_curves() counts
# ---------------------------------------------------------------------------


class TestListCurves:
    """Verify list_curves() returns the correct number of entries."""

    def test_list_curves_all(self):
        all_curves = StandardSNCurves.list_curves()
        for std, expected_count in EXPECTED_COUNTS.items():
            assert len(all_curves[std]) == expected_count, (
                f"{std}: expected {expected_count}, got {len(all_curves[std])}"
            )

    @pytest.mark.parametrize(
        "standard, expected_count",
        list(EXPECTED_COUNTS.items()),
        ids=list(EXPECTED_COUNTS.keys()),
    )
    def test_list_curves_per_standard(self, standard, expected_count):
        result = StandardSNCurves.list_curves(standard)
        assert len(result[standard]) == expected_count


# ---------------------------------------------------------------------------
# Cross-check: Python class dicts == YAML values (round-trip integrity)
# ---------------------------------------------------------------------------


class TestCrossCheck:
    """Ensure the Python class dicts and YAML file stay in sync."""

    @pytest.fixture
    def yaml_data(self):
        with open(YAML_PATH) as f:
            return yaml.safe_load(f)

    @pytest.mark.parametrize(
        "standard, attr",
        list(_STD_TO_ATTR.items()),
        ids=list(_STD_TO_ATTR.keys()),
    )
    def test_python_yaml_key_parity(self, yaml_data, standard, attr):
        """Python dict and YAML must have identical curve names."""
        python_keys = set(getattr(StandardSNCurves, attr).keys())
        yaml_keys = set(yaml_data["standards"][standard]["curves"].keys())
        assert python_keys == yaml_keys, (
            f"{standard} key mismatch: "
            f"Python-only={python_keys - yaml_keys}, "
            f"YAML-only={yaml_keys - python_keys}"
        )

    @pytest.mark.parametrize(
        "standard, curve_class, expected",
        list(_all_single_slope_cases()),
        ids=[f"{s}-{c}" for s, c, _ in _all_single_slope_cases()],
    )
    def test_python_yaml_value_match(self, yaml_data, standard, curve_class, expected):
        """Every value in the Python dict must equal the corresponding YAML value."""
        yaml_curve = yaml_data["standards"][standard]["curves"][curve_class]
        assert yaml_curve["A"] == pytest.approx(expected["A"], rel=1e-6)
        assert yaml_curve["m"] == pytest.approx(expected["m"], rel=1e-6)
        assert yaml_curve["fatigue_limit"] == pytest.approx(
            expected["fatigue_limit"], rel=1e-6
        )

    def test_multislope_key_parity(self, yaml_data):
        python_keys = set(StandardSNCurves.DNV_MULTISLOPE_CURVES.keys())
        yaml_keys = set(yaml_data["standards"]["DNV"]["multislope"].keys())
        assert python_keys == yaml_keys
