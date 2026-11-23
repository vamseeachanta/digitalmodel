"""Tests for sn_library — 221 S-N curves from 17 international standards."""

import math
import pytest

from digitalmodel.fatigue.sn_library import (
    get_catalog,
    get_library_curve,
    search_curves,
    list_standards,
    curve_count,
    summary_table,
    SNCurveRecord,
)


class TestSNLibraryCatalog:
    """Test the catalogue and registry functions."""

    def test_total_curve_count_is_221(self):
        """The library must expose exactly 221 S-N curves."""
        assert curve_count() == 221

    def test_17_standards_present(self):
        """All 17 international standards must be represented."""
        stds = list_standards()
        assert len(stds) == 17
        # Spot-check a few
        assert "DNV-RP-C203" in stds
        assert "API RP 2A" in stds
        assert "IIW" in stds
        assert "Eurocode 3" in stds
        assert "JIS B 8266" in stds

    def test_catalog_filter_by_standard(self):
        """Filter should return only curves from the requested standard."""
        cat = get_catalog()
        dnv = cat.filter(standard="DNV-RP-C203")
        # DNV: 14 classes × 3 envs = 42
        assert len(dnv) == 42
        for c in dnv:
            assert c.standard == "DNV-RP-C203"

    def test_catalog_filter_by_environment(self):
        """Filter by environment returns only matching curves."""
        cat = get_catalog()
        air = cat.filter(environment="air")
        assert all(c.environment == "air" for c in air)
        assert len(air) > 100  # most standards have air curves

    def test_catalog_filter_by_class(self):
        """Filter by curve class works across standards."""
        cat = get_catalog()
        d_curves = cat.filter(curve_class="D")
        # D class exists in DNV, BS 7608, PD 5500, AWS, etc.
        assert len(d_curves) >= 4


class TestSNLibraryLookup:
    """Test individual curve lookup."""

    def test_get_dnv_d_air(self):
        """Look up DNV D-curve in air — verify slope and intercept."""
        curve = get_library_curve("DNV-RP-C203:D:air")
        assert curve.standard == "DNV-RP-C203"
        assert curve.curve_class == "D"
        assert curve.m1 == 3.0
        assert abs(curve.log_a1 - 12.164) < 0.001

    def test_get_iiw_fat90(self):
        """Look up IIW FAT90 curve — verify it exists and has m1=3."""
        curve = get_library_curve("IIW:FAT90:air")
        assert curve.m1 == 3.0
        assert "IIW" in curve.standard

    def test_get_nonexistent_raises_keyerror(self):
        """Looking up a non-existent curve_id raises KeyError."""
        with pytest.raises(KeyError):
            get_library_curve("NONEXISTENT:X:air")


class TestSNLibraryCycleCalculation:
    """Test that SNCurveRecord.cycles() produces realistic results."""

    def test_dnv_d_air_at_100mpa(self):
        """DNV D in air at 100 MPa: N ≈ 10^12.164 / 100^3 = ~1.46e6."""
        curve = get_library_curve("DNV-RP-C203:D:air")
        N = curve.cycles(100.0)
        assert 1e6 < N < 2e6

    def test_dnv_d_air_below_endurance(self):
        """Below endurance limit, bilinear curve gives more cycles."""
        curve = get_library_curve("DNV-RP-C203:D:air")
        N_above = curve.cycles(curve.endurance_limit + 5.0)
        N_below = curve.cycles(curve.endurance_limit - 5.0)
        assert N_below > N_above

    def test_free_corrosion_no_endurance(self):
        """Free corrosion curves have no endurance limit — always finite N."""
        curve = get_library_curve("DNV-RP-C203:D:free_corrosion")
        assert curve.endurance_limit is None
        N = curve.cycles(10.0)
        assert math.isfinite(N)
        assert N > 0

    def test_summary_table_is_list_of_dicts(self):
        """summary_table() returns a list of dicts with expected keys."""
        table = summary_table()
        assert len(table) == 221
        assert "curve_id" in table[0]
        assert "m1" in table[0]
        assert "log_a1" in table[0]
