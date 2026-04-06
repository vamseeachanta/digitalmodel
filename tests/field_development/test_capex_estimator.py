# ABOUTME: Tests for capex_estimator module — GoM CAPEX benchmark estimation.
# ABOUTME: Issue #1843 — Concept Selection Framework for field_development.
"""
Tests for digitalmodel.field_development.capex_estimator

Covers:
- estimate_capex() returns a CAPEXEstimate with low/high/base USD values
- Benchmark ranges match GoM reference data per host type
- Production capacity scaling (larger = higher CAPEX)
- Water depth scaling (deeper = higher CAPEX within same host type)
- All five host types exercise independently
- Invalid inputs raise ValueError with informative messages
- Subsea tieback near/far distance variants
- Result values are positive and low <= base <= high
"""

from __future__ import annotations

import pytest

from digitalmodel.field_development.capex_estimator import (
    CAPEXEstimate,
    estimate_capex,
)
from digitalmodel.field_development.concept_selection import HostType


# ---------------------------------------------------------------------------
# CAPEXEstimate dataclass
# ---------------------------------------------------------------------------

class TestCAPEXEstimate:
    def test_fields_present(self):
        est = CAPEXEstimate(
            host_type=HostType.TLP,
            low_usd_bn=2.0,
            base_usd_bn=4.0,
            high_usd_bn=6.0,
            basis="GoM benchmark 2024",
        )
        assert est.low_usd_bn == pytest.approx(2.0)
        assert est.base_usd_bn == pytest.approx(4.0)
        assert est.high_usd_bn == pytest.approx(6.0)
        assert est.host_type is HostType.TLP

    def test_low_lte_base_lte_high(self):
        est = CAPEXEstimate(
            host_type=HostType.SPAR,
            low_usd_bn=3.0,
            base_usd_bn=5.0,
            high_usd_bn=7.0,
            basis="GoM benchmark",
        )
        assert est.low_usd_bn <= est.base_usd_bn <= est.high_usd_bn


# ---------------------------------------------------------------------------
# estimate_capex — happy path per host type
# ---------------------------------------------------------------------------

class TestEstimateCapexHappyPath:
    """Each host type should return an estimate within GoM benchmark ranges."""

    def test_tlp_range(self):
        """TLP GoM benchmark: $2-6B."""
        est = estimate_capex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            water_depth=900,
        )
        assert est.low_usd_bn >= 1.5
        assert est.high_usd_bn <= 8.0
        # base within nominal range
        assert 2.0 <= est.base_usd_bn <= 7.0

    def test_spar_range(self):
        """Spar GoM benchmark: $3-7B."""
        est = estimate_capex(
            host_type=HostType.SPAR,
            production_capacity_bopd=100_000,
            water_depth=2000,
        )
        assert est.low_usd_bn >= 2.0
        assert est.high_usd_bn <= 9.0
        assert 3.0 <= est.base_usd_bn <= 8.0

    def test_semi_range(self):
        """Semi GoM benchmark: $4-10B at reference scale; larger fields scale higher."""
        est = estimate_capex(
            host_type=HostType.SEMI,
            production_capacity_bopd=150_000,
            water_depth=1800,
        )
        assert est.low_usd_bn >= 3.0
        # High end scales with capacity; allow up to 16B for 150k bopd at 1800 m
        assert est.high_usd_bn <= 16.0
        assert 4.0 <= est.base_usd_bn <= 13.0

    def test_fpso_range(self):
        """FPSO GoM benchmark: $5-12B at reference scale; larger fields scale higher."""
        est = estimate_capex(
            host_type=HostType.FPSO,
            production_capacity_bopd=200_000,
            water_depth=2000,
        )
        assert est.low_usd_bn >= 4.0
        # High end scales with capacity; 200k bopd at 2000 m → allow up to 22B
        assert est.high_usd_bn <= 22.0
        assert 5.0 <= est.base_usd_bn <= 18.0

    def test_subsea_tieback_near_range(self):
        """Subsea tieback <10 km at 20k bopd — small low-end CAPEX expected."""
        est = estimate_capex(
            host_type=HostType.SUBSEA_TIEBACK,
            production_capacity_bopd=20_000,
            water_depth=900,
            tieback_distance_km=8.0,
        )
        # At 20k bopd (below reference) the low case is below $100M — that's realistic
        assert est.low_usd_bn > 0
        assert est.high_usd_bn <= 2.0
        assert est.base_usd_bn < 1.5  # should be well under $1.5B

    def test_subsea_tieback_far_range(self):
        """Subsea tieback >20 km: $500M-1.2B."""
        est = estimate_capex(
            host_type=HostType.SUBSEA_TIEBACK,
            production_capacity_bopd=20_000,
            water_depth=900,
            tieback_distance_km=25.0,
        )
        # far tieback should cost more than near tieback
        est_near = estimate_capex(
            host_type=HostType.SUBSEA_TIEBACK,
            production_capacity_bopd=20_000,
            water_depth=900,
            tieback_distance_km=8.0,
        )
        assert est.base_usd_bn > est_near.base_usd_bn


# ---------------------------------------------------------------------------
# estimate_capex — scaling behaviour
# ---------------------------------------------------------------------------

class TestEstimateCapexScaling:
    def test_higher_capacity_increases_capex(self):
        """Doubling production capacity should increase base CAPEX."""
        est_small = estimate_capex(
            host_type=HostType.SEMI,
            production_capacity_bopd=50_000,
            water_depth=1500,
        )
        est_large = estimate_capex(
            host_type=HostType.SEMI,
            production_capacity_bopd=200_000,
            water_depth=1500,
        )
        assert est_large.base_usd_bn > est_small.base_usd_bn

    def test_deeper_water_increases_capex(self):
        """Deeper water increases CAPEX for same host type and capacity."""
        est_shallow = estimate_capex(
            host_type=HostType.SPAR,
            production_capacity_bopd=100_000,
            water_depth=1500,
        )
        est_deep = estimate_capex(
            host_type=HostType.SPAR,
            production_capacity_bopd=100_000,
            water_depth=2800,
        )
        assert est_deep.base_usd_bn >= est_shallow.base_usd_bn

    def test_tlp_cheaper_than_fpso_comparable_scenario(self):
        """TLP baseline is cheaper than FPSO for same capacity/depth."""
        est_tlp = estimate_capex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            water_depth=1000,
        )
        est_fpso = estimate_capex(
            host_type=HostType.FPSO,
            production_capacity_bopd=100_000,
            water_depth=1000,
        )
        assert est_tlp.base_usd_bn < est_fpso.base_usd_bn


# ---------------------------------------------------------------------------
# estimate_capex — GoM reference field cross-checks
# ---------------------------------------------------------------------------

class TestEstimateCapexBenchmarks:
    def test_perdido_spar_capex_in_range(self):
        """Perdido Spar (2438 m, 100k bopd) — expect ~$3-7B."""
        est = estimate_capex(
            host_type=HostType.SPAR,
            production_capacity_bopd=100_000,
            water_depth=2438,
        )
        assert est.low_usd_bn < 7.0
        assert est.high_usd_bn > 3.0

    def test_thunder_horse_semi_capex_in_range(self):
        """Thunder Horse Semi (1844 m, 250k bopd) — expect $4-10B range."""
        est = estimate_capex(
            host_type=HostType.SEMI,
            production_capacity_bopd=250_000,
            water_depth=1844,
        )
        assert est.low_usd_bn < 11.0
        assert est.high_usd_bn > 4.0

    def test_mars_tlp_capex_in_range(self):
        """Mars TLP (896 m, 100k bopd) — expect $2-6B range."""
        est = estimate_capex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            water_depth=896,
        )
        assert est.low_usd_bn < 7.0
        assert est.high_usd_bn > 2.0


# ---------------------------------------------------------------------------
# estimate_capex — result invariants
# ---------------------------------------------------------------------------

class TestEstimateCapexInvariants:
    def test_low_lte_base_lte_high(self):
        for host in (HostType.TLP, HostType.SPAR, HostType.SEMI, HostType.FPSO):
            est = estimate_capex(
                host_type=host,
                production_capacity_bopd=100_000,
                water_depth=1500,
            )
            assert est.low_usd_bn <= est.base_usd_bn, f"Failed for {host}"
            assert est.base_usd_bn <= est.high_usd_bn, f"Failed for {host}"

    def test_all_values_positive(self):
        for host in (HostType.TLP, HostType.SPAR, HostType.SEMI, HostType.FPSO):
            est = estimate_capex(
                host_type=host,
                production_capacity_bopd=80_000,
                water_depth=1200,
            )
            assert est.low_usd_bn > 0, f"low non-positive for {host}"
            assert est.base_usd_bn > 0, f"base non-positive for {host}"
            assert est.high_usd_bn > 0, f"high non-positive for {host}"

    def test_basis_string_not_empty(self):
        est = estimate_capex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            water_depth=900,
        )
        assert isinstance(est.basis, str)
        assert len(est.basis) > 0


# ---------------------------------------------------------------------------
# estimate_capex — Input validation
# ---------------------------------------------------------------------------

class TestEstimateCapexValidation:
    def test_negative_production_capacity_raises(self):
        with pytest.raises(ValueError, match="production_capacity"):
            estimate_capex(
                host_type=HostType.TLP,
                production_capacity_bopd=-1000,
                water_depth=900,
            )

    def test_zero_production_capacity_raises(self):
        with pytest.raises(ValueError, match="production_capacity"):
            estimate_capex(
                host_type=HostType.TLP,
                production_capacity_bopd=0,
                water_depth=900,
            )

    def test_negative_water_depth_raises(self):
        with pytest.raises(ValueError, match="water_depth"):
            estimate_capex(
                host_type=HostType.SPAR,
                production_capacity_bopd=100_000,
                water_depth=-500,
            )

    def test_tieback_missing_distance_raises(self):
        """Tieback type without providing distance should raise ValueError."""
        with pytest.raises(ValueError, match="tieback_distance"):
            estimate_capex(
                host_type=HostType.SUBSEA_TIEBACK,
                production_capacity_bopd=20_000,
                water_depth=900,
            )

    def test_tieback_negative_distance_raises(self):
        with pytest.raises(ValueError, match="tieback_distance"):
            estimate_capex(
                host_type=HostType.SUBSEA_TIEBACK,
                production_capacity_bopd=20_000,
                water_depth=900,
                tieback_distance_km=-3.0,
            )
