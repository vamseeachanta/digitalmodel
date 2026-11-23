"""Tests for the floating-wind LCOE/TOTEX core engine (issue #1221).

Closed-form / licence-free. The anchor is the public Wood/OREC base case
(~$184/MWh); the remaining tests pin structural relationships (component split
sums to LCOE, sensible monotonic responses) rather than magic numbers.
"""

import math

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.economics import (
    ProjectEconomics,
    annual_energy_production_mwh,
    base_case,
    compute_lcoe,
    opex_year_usd,
)

# Wood/OREC base case anchor, from the deck.
_WOOD_BASE_LCOE = 184.0
_TOL = 0.02  # +/- 2%


@pytest.fixture
def econ() -> ProjectEconomics:
    return base_case()


def test_base_case_reproduces_wood_184(econ):
    """The packaged base case must reproduce the deck's $184/MWh anchor."""
    lcoe = compute_lcoe(econ).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(_WOOD_BASE_LCOE, rel=_TOL)


def test_base_case_farm_size(econ):
    """15 MW x 33 turbines -> 495 MW."""
    assert econ.farm_capacity_mw == pytest.approx(495.0)
    assert econ.farm_capacity_kw == pytest.approx(495_000.0)


def test_component_split_sums_to_lcoe(econ):
    """capex + opex + decomex per-MWh split must reconstitute the LCOE."""
    r = compute_lcoe(econ)
    total = r.capex_per_mwh + r.opex_per_mwh + r.decomex_per_mwh
    assert total == pytest.approx(r.lcoe_usd_per_mwh, rel=1e-9)
    # CAPEX dominates first-of-kind floating LCOE.
    assert r.capex_per_mwh > r.opex_per_mwh > r.decomex_per_mwh


def test_discounted_totex_over_energy_equals_lcoe(econ):
    r = compute_lcoe(econ)
    assert r.totex_discounted_usd / r.energy_discounted_mwh == pytest.approx(
        r.lcoe_usd_per_mwh, rel=1e-12
    )


def test_aep_matches_capacity_factor(econ):
    aep = annual_energy_production_mwh(econ)
    assert aep == pytest.approx(495.0 * 8760.0 * 0.5)


def test_higher_capacity_factor_lowers_lcoe(econ):
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    better = compute_lcoe(econ.model_copy(update={"capacity_factor": 0.60}))
    assert better.lcoe_usd_per_mwh < base


def test_higher_opex_raises_lcoe(econ):
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    worse = compute_lcoe(
        econ.model_copy(update={"opex_per_kw_year": econ.opex_per_kw_year * 1.5})
    )
    assert worse.lcoe_usd_per_mwh > base


def test_longer_design_life_lowers_lcoe(econ):
    """Deck slide 7: 25 -> 30 year design life drops LCOE (CAPEX spread wider)."""
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    fin30 = econ.financial.model_copy(update={"design_life_years": 30})
    longer = compute_lcoe(econ.model_copy(update={"financial": fin30}))
    assert longer.lcoe_usd_per_mwh < base


def test_opex_escalates_with_inflation(econ):
    """OPEX escalates from the year-1 basis at the inflation rate."""
    y1 = opex_year_usd(econ, 1)
    y2 = opex_year_usd(econ, 2)
    assert y2 == pytest.approx(y1 * (1.0 + econ.financial.inflation_rate))
    with pytest.raises(ValueError):
        opex_year_usd(econ, 0)


def test_zero_inflation_is_constant_opex(econ):
    fin = econ.financial.model_copy(update={"inflation_rate": 0.0})
    flat = econ.model_copy(update={"financial": fin})
    assert opex_year_usd(flat, 1) == pytest.approx(opex_year_usd(flat, 10))


def test_from_mapping_roundtrip(econ):
    data = econ.model_dump()
    rebuilt = ProjectEconomics.from_mapping(data)
    assert compute_lcoe(rebuilt).lcoe_usd_per_mwh == pytest.approx(
        compute_lcoe(econ).lcoe_usd_per_mwh
    )


def test_invalid_discount_rate_rejected():
    with pytest.raises(ValidationError):
        ProjectEconomics(
            turbine_rating_mw=15.0,
            turbine_count=33,
            capacity_factor=0.5,
            financial={
                "discount_rate": 1.5,  # out of (0, 1)
                "design_life_years": 25,
            },
            capex={
                "turbine": 2356.0,
                "substructure": 2278.0,
                "mooring": 707.0,
                "array_cable": 393.0,
                "export_cable": 628.0,
                "installation": 1100.0,
                "development": 392.0,
            },
            opex_per_kw_year=140.0,
        )
