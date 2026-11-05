# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic constant-rate annual forecasts, independently checked by subtraction."""

from dataclasses import replace
from pathlib import Path
import runpy

import pytest

from digitalmodel.asset_integrity import air_gap as ag
from digitalmodel.asset_integrity import offering_catalog as oc

REPO = Path(__file__).resolve().parents[2]


def basis():
    """Return deck 10, still water 2, crest 3 and minimum 1, all in metres."""
    return ag.AirGapBasis(ag.Elevation(10, "shared datum"),
                          ag.Elevation(2, "shared datum"), 3, 1)


def test_constant_rate_forecast_matches_hand_calculated_years():
    # S = 2 + 0.5*(year-2030); clearance = 5-S; margin = 4-S.
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2, 0.5), 2035)
    assert [s.date_label for s in result.samples] == list(range(2030, 2036))
    assert [s.air_gap.clearance_m for s in result.samples] == [3, 2.5, 2, 1.5, 1, 0.5]
    assert [s.air_gap.margin_m for s in result.samples] == [2, 1.5, 1, 0.5, 0, -0.5]
    assert result.samples[-2].air_gap.meets_required_clearance  # equality passes
    assert result.first_sampled_breach == 2035


def test_zero_rate_and_single_year_horizons():
    forecast = ag.SubsidenceForecast(2030, 2, 0)
    result = ag.forecast_air_gap(basis(), forecast, 2035)
    assert result.first_sampled_breach is None
    assert all(s.air_gap.clearance_m == 3 for s in result.samples)
    assert len(ag.forecast_air_gap(basis(), forecast, 2030).samples) == 1


def test_existing_breach_is_reported_at_reference_year():
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 5, 0), 2035)
    assert result.first_sampled_breach == 2030


def test_breach_outside_horizon_is_not_reported():
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2, 0.5), 2034)
    assert result.first_sampled_breach is None


@pytest.mark.parametrize("year", [1, 9999])
def test_boundary_years_are_accepted(year):
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(year, 0, 0), year)
    assert [s.date_label for s in result.samples] == [year]
    assert result.first_sampled_breach is None


def test_maximum_inclusive_horizon_is_bounded():
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(1, 0, 0), 9999)
    assert len(result.samples) == 9999
    assert result.samples[-1].date_label == 9999


def test_subannual_crossing_is_reported_only_at_first_annual_sample():
    # Minimum is reached after 2/0.75 years; first annual breach is year 3.
    result = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2, 0.75), 2034)
    assert result.first_sampled_breach == 2033


@pytest.mark.parametrize("field", ["reference_year", "end_year"])
@pytest.mark.parametrize("value", [True, 0, -1, 10000, 2030.0, "2030", None])
def test_invalid_years_are_rejected(field, value):
    forecast = ag.SubsidenceForecast(2030, 2, 0.5)
    end_year = 2035
    if field == "reference_year":
        forecast = replace(forecast, reference_year=value)
    else:
        end_year = value
    with pytest.raises(ValueError, match="year"):
        ag.forecast_air_gap(basis(), forecast, end_year)


def test_backwards_forecast_is_rejected():
    with pytest.raises(ValueError, match="year"):
        ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2, 0.5), 2029)


@pytest.mark.parametrize("field", ["subsidence_m", "rate_m_per_year"])
@pytest.mark.parametrize("value", [-1, True, None, "1", float("nan"), float("inf")])
def test_invalid_subsidence_and_rate_are_rejected(field, value):
    forecast = replace(ag.SubsidenceForecast(2030, 2, 0.5), **{field: value})
    with pytest.raises(ValueError):
        ag.forecast_air_gap(basis(), forecast, 2035)


def test_forecast_reuses_datum_validation():
    invalid = replace(basis(), still_water=ag.Elevation(2, "different datum"))
    with pytest.raises(ValueError, match="datum"):
        ag.forecast_air_gap(invalid, ag.SubsidenceForecast(2030, 2, 0.5), 2035)


def test_forecast_overflow_is_rejected():
    with pytest.raises(ValueError, match="finite"):
        ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 1e308, 1e308), 2032)


def test_fixed_platform_subsidence_has_explicit_engine_and_limits():
    catalog = oc.load()
    engine = catalog.ffs["engines"]["air-gap"]
    assert engine["module"] == "asset_integrity.air_gap"
    assert engine["status"] == "engine"
    row = next(r for r in catalog.by_industry("upstream-offshore-fixed")
               if "subsidence" in r.mechanism)
    assert "air-gap" in row.engines
    assert row.status == "engine"
    assert "no SIM verdict" in row.note


def test_synthetic_capability_page_is_generated_from_example():
    example = runpy.run_path(str(REPO / "examples/demos/asset_integrity/air_gap_forecast.py"))
    page = example["render"]()
    assert page == (REPO / "docs/domains/asset-integrity/air-gap-capability.md").read_text()
    assert "2035" in page
    assert "| 2034 | 4.0 | 1.0 | 0.0 | Yes |" in page
    assert "| 2035 | 4.5 | 0.5 | -0.5 | No |" in page
    assert "2.0 m" in page  # max(0, -margin at 2038) = 2
    assert "invented" in page
    assert "Live with it" in page and "Raise" in page and "Decommission" in page
