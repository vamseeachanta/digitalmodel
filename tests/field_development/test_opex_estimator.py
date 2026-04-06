# ABOUTME: Tests for opex_estimator module — annual OPEX estimation for offshore host facilities.
# ABOUTME: Issue #1843 — Concept Selection Framework for field_development.
"""
Tests for digitalmodel.field_development.opex_estimator

Covers:
- OPEXEstimate dataclass: low/base/high annual USD values
- estimate_opex() happy path for all host types
- Scaling: higher production capacity → higher absolute OPEX
- Field age factor: ageing fields have higher OPEX per bbl
- OPEX/bbl metrics present and sensible (GoM range $15-40/bbl)
- Host type comparisons (FPSO higher fixed costs vs TLP)
- Input validation: negatives, zero capacity, invalid age
- Basis string provided
"""

from __future__ import annotations

import pytest

from digitalmodel.field_development.opex_estimator import (
    OPEXEstimate,
    estimate_opex,
)
from digitalmodel.field_development.concept_selection import HostType


# ---------------------------------------------------------------------------
# OPEXEstimate dataclass
# ---------------------------------------------------------------------------

class TestOPEXEstimate:
    def test_fields_present(self):
        est = OPEXEstimate(
            host_type=HostType.TLP,
            low_usd_mm_per_yr=80.0,
            base_usd_mm_per_yr=150.0,
            high_usd_mm_per_yr=250.0,
            opex_per_bbl_usd=18.0,
            basis="GoM benchmark 2024",
        )
        assert est.low_usd_mm_per_yr == pytest.approx(80.0)
        assert est.base_usd_mm_per_yr == pytest.approx(150.0)
        assert est.high_usd_mm_per_yr == pytest.approx(250.0)
        assert est.opex_per_bbl_usd == pytest.approx(18.0)
        assert est.host_type is HostType.TLP

    def test_low_lte_base_lte_high(self):
        est = OPEXEstimate(
            host_type=HostType.SPAR,
            low_usd_mm_per_yr=100.0,
            base_usd_mm_per_yr=180.0,
            high_usd_mm_per_yr=300.0,
            opex_per_bbl_usd=22.0,
            basis="benchmark",
        )
        assert est.low_usd_mm_per_yr <= est.base_usd_mm_per_yr
        assert est.base_usd_mm_per_yr <= est.high_usd_mm_per_yr


# ---------------------------------------------------------------------------
# estimate_opex — happy path per host type
# ---------------------------------------------------------------------------

class TestEstimateOpexHappyPath:
    def test_tlp_opex_reasonable(self):
        """TLP annual OPEX: typically $100-300M/yr for 100k bopd GoM field."""
        est = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=5,
        )
        assert est.low_usd_mm_per_yr > 0
        assert est.base_usd_mm_per_yr >= 50.0
        assert est.high_usd_mm_per_yr <= 600.0

    def test_spar_opex_reasonable(self):
        est = estimate_opex(
            host_type=HostType.SPAR,
            production_capacity_bopd=100_000,
            field_age_years=5,
        )
        assert est.base_usd_mm_per_yr > 0
        assert est.base_usd_mm_per_yr <= 700.0

    def test_semi_opex_reasonable(self):
        est = estimate_opex(
            host_type=HostType.SEMI,
            production_capacity_bopd=150_000,
            field_age_years=5,
        )
        assert est.base_usd_mm_per_yr > 0
        assert est.base_usd_mm_per_yr <= 900.0

    def test_fpso_opex_reasonable(self):
        est = estimate_opex(
            host_type=HostType.FPSO,
            production_capacity_bopd=200_000,
            field_age_years=5,
        )
        assert est.base_usd_mm_per_yr > 0
        assert est.base_usd_mm_per_yr <= 1200.0

    def test_subsea_tieback_opex_lowest(self):
        """Subsea tiebacks have lowest absolute OPEX (no standalone hull)."""
        est_tieback = estimate_opex(
            host_type=HostType.SUBSEA_TIEBACK,
            production_capacity_bopd=30_000,
            field_age_years=5,
        )
        est_tlp = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=30_000,
            field_age_years=5,
        )
        assert est_tieback.base_usd_mm_per_yr < est_tlp.base_usd_mm_per_yr


# ---------------------------------------------------------------------------
# estimate_opex — scaling
# ---------------------------------------------------------------------------

class TestEstimateOpexScaling:
    def test_higher_capacity_higher_absolute_opex(self):
        """More production → more absolute annual OPEX."""
        est_small = estimate_opex(
            host_type=HostType.SEMI,
            production_capacity_bopd=50_000,
            field_age_years=5,
        )
        est_large = estimate_opex(
            host_type=HostType.SEMI,
            production_capacity_bopd=200_000,
            field_age_years=5,
        )
        assert est_large.base_usd_mm_per_yr > est_small.base_usd_mm_per_yr

    def test_older_field_higher_opex_per_bbl(self):
        """Ageing infrastructure increases per-barrel OPEX."""
        est_new = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=2,
        )
        est_old = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=20,
        )
        assert est_old.opex_per_bbl_usd >= est_new.opex_per_bbl_usd

    def test_fpso_higher_fixed_costs_than_tlp(self):
        """FPSO has higher fixed costs (lease/manning) than TLP at same capacity."""
        est_tlp = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=5,
        )
        est_fpso = estimate_opex(
            host_type=HostType.FPSO,
            production_capacity_bopd=100_000,
            field_age_years=5,
        )
        assert est_fpso.base_usd_mm_per_yr >= est_tlp.base_usd_mm_per_yr


# ---------------------------------------------------------------------------
# estimate_opex — opex_per_bbl metrics
# ---------------------------------------------------------------------------

class TestEstimateOpexPerBbl:
    def test_opex_per_bbl_within_gom_range(self):
        """GoM deepwater OPEX per bbl: typically $15-50/bbl."""
        est = estimate_opex(
            host_type=HostType.SEMI,
            production_capacity_bopd=150_000,
            field_age_years=5,
        )
        assert 5.0 <= est.opex_per_bbl_usd <= 80.0

    def test_opex_per_bbl_positive(self):
        for host in (HostType.TLP, HostType.SPAR, HostType.SEMI, HostType.FPSO):
            est = estimate_opex(
                host_type=host,
                production_capacity_bopd=100_000,
                field_age_years=5,
            )
            assert est.opex_per_bbl_usd > 0, f"opex_per_bbl non-positive for {host}"

    def test_tieback_opex_per_bbl_competitive(self):
        """Tieback to existing host: very low per-bbl OPEX."""
        est = estimate_opex(
            host_type=HostType.SUBSEA_TIEBACK,
            production_capacity_bopd=30_000,
            field_age_years=3,
        )
        assert est.opex_per_bbl_usd < 30.0


# ---------------------------------------------------------------------------
# estimate_opex — result invariants
# ---------------------------------------------------------------------------

class TestEstimateOpexInvariants:
    def test_low_lte_base_lte_high_all_types(self):
        for host in (HostType.TLP, HostType.SPAR, HostType.SEMI, HostType.FPSO):
            est = estimate_opex(
                host_type=host,
                production_capacity_bopd=100_000,
                field_age_years=5,
            )
            assert est.low_usd_mm_per_yr <= est.base_usd_mm_per_yr, f"Failed for {host}"
            assert est.base_usd_mm_per_yr <= est.high_usd_mm_per_yr, f"Failed for {host}"

    def test_all_values_positive(self):
        for host in (HostType.TLP, HostType.SPAR, HostType.SEMI, HostType.FPSO):
            est = estimate_opex(
                host_type=host,
                production_capacity_bopd=80_000,
                field_age_years=5,
            )
            assert est.low_usd_mm_per_yr > 0
            assert est.base_usd_mm_per_yr > 0
            assert est.high_usd_mm_per_yr > 0

    def test_basis_string_not_empty(self):
        est = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=5,
        )
        assert isinstance(est.basis, str)
        assert len(est.basis) > 0


# ---------------------------------------------------------------------------
# estimate_opex — Input validation
# ---------------------------------------------------------------------------

class TestEstimateOpexValidation:
    def test_negative_production_capacity_raises(self):
        with pytest.raises(ValueError, match="production_capacity"):
            estimate_opex(
                host_type=HostType.TLP,
                production_capacity_bopd=-5000,
                field_age_years=5,
            )

    def test_zero_production_capacity_raises(self):
        with pytest.raises(ValueError, match="production_capacity"):
            estimate_opex(
                host_type=HostType.TLP,
                production_capacity_bopd=0,
                field_age_years=5,
            )

    def test_negative_field_age_raises(self):
        with pytest.raises(ValueError, match="field_age"):
            estimate_opex(
                host_type=HostType.TLP,
                production_capacity_bopd=100_000,
                field_age_years=-1,
            )

    def test_zero_field_age_allowed(self):
        """New field (age=0) is valid — greenfield OPEX estimate."""
        est = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=0,
        )
        assert est.base_usd_mm_per_yr > 0

    def test_very_old_field_allowed(self):
        """Field age = 30 years is physically plausible."""
        est = estimate_opex(
            host_type=HostType.TLP,
            production_capacity_bopd=100_000,
            field_age_years=30,
        )
        assert est.base_usd_mm_per_yr > 0
