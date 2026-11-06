# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
    assert [s.subsidence_m for s in result.samples] == [2, 2.5, 3, 3.5, 4, 4.5]
    assert [s.air_gap.clearance_m for s in result.samples] == [3, 2.5, 2, 1.5, 1, 0.5]
    assert [s.air_gap.margin_m for s in result.samples] == [2, 1.5, 1, 0.5, 0, -0.5]
    assert result.samples[-2].air_gap.meets_required_clearance  # equality passes
    assert result.first_sampled_breach == 2035


def test_decimal_equality_does_not_report_a_sampled_breach():
    decimal_basis = ag.AirGapBasis(
        ag.Elevation(14.7, "shared datum"),
        ag.Elevation(2.3, "shared datum"), 5.3, 1.1,
    )
    forecast = ag.SubsidenceForecast(2030, 1.1, 0.7)
    result = ag.forecast_air_gap(decimal_basis, forecast, 2037)
    # S = 1.1 + 0.7*7 = 6.0; clearance = 14.7-6.0-2.3-5.3 = 1.1;
    # margin = 1.1-1.1 = 0.0 m. The next sample gives clearance 0.4 m.
    gap = result.samples[-1].air_gap
    assert gap.clearance_m == pytest.approx(1.1, abs=1e-14)
    assert gap.margin_m == pytest.approx(0.0, abs=1e-14)
    assert gap.meets_required_clearance
    assert result.first_sampled_breach is None
    later = ag.forecast_air_gap(decimal_basis, forecast, 2038)
    assert later.samples[-1].air_gap.clearance_m == pytest.approx(0.4)
    assert later.first_sampled_breach == 2038


@pytest.mark.parametrize("margin,meets", [
    (-2e-9, False), (-5e-10, True), (0.0, True), (5e-10, True),
])
def test_absolute_comparison_tolerance_preserves_raw_margin(margin, meets):
    # With zero water, crest and minimum, deck elevation is also the margin.
    probe = ag.AirGapBasis(
        ag.Elevation(margin, "shared datum"),
        ag.Elevation(0.0, "shared datum"), 0.0, 0.0,
    )
    gap = ag.calculate_air_gap(probe, 0.0)
    assert gap.clearance_m == margin
    assert gap.margin_m == margin
    assert gap.meets_required_clearance is meets


def test_comparison_tolerance_does_not_scale_with_clearance():
    # Clearance = 1,000,000 - 0.001; margin = -0.001 m, beyond 1e-9 m.
    probe = ag.AirGapBasis(
        ag.Elevation(1e6, "shared datum"),
        ag.Elevation(0.0, "shared datum"), 0.0, 1e6,
    )
    gap = ag.calculate_air_gap(probe, 0.001)
    assert gap.margin_m == pytest.approx(-0.001)
    assert not gap.meets_required_clearance


def test_sampled_results_retain_evaluated_subsidence():
    supplied = (ag.SubsidenceSample(2030, 2.125),
                ag.SubsidenceSample(2031, 1.75),
                ag.SubsidenceSample(2032, 3.5))
    result = ag.evaluate_sampled_air_gap(basis(), supplied)
    assert [s.subsidence_m for s in result.samples] == [2.125, 1.75, 3.5]
    # Clearance = 5-S, including a decreasing supplied movement.
    assert [s.air_gap.clearance_m for s in result.samples] == [2.875, 3.25, 1.5]


def test_sampled_result_preserves_legacy_construction_and_equality():
    sample = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2, 0), 2030).samples[0]
    legacy = ag.SampledAirGapResult(2030, sample.air_gap)
    assert legacy.subsidence_m is None
    assert sample.subsidence_m == 2
    assert legacy == sample


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
    with pytest.raises(ValueError, match=field):
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
    assert page.encode("utf-8") == (
        REPO / "docs/domains/asset-integrity/air-gap-capability.md"
    ).read_bytes(), "Rebuild with: PYTHONPATH=src python examples/demos/asset_integrity/air_gap_forecast.py"
    assert "2035" in page
    assert "| 2034 | 4.0 | 1.0 | 0.0 | Yes |" in page
    assert "| 2035 | 4.5 | 0.5 | -0.5 | No |" in page
    assert "2.0 m" in page  # max(0, -margin at 2038) = 2
    assert "invented" in page
    assert "Live with it" in page and "Raise" in page and "Decommission" in page


def test_capability_forecast_subsidence_comes_from_engine_results(monkeypatch):
    example = runpy.run_path(str(REPO / "examples/demos/asset_integrity/air_gap_forecast.py"))
    render = example["render"]
    replacement = ag.forecast_air_gap(basis(), ag.SubsidenceForecast(2030, 2.3, 0.5), 2038)
    monkeypatch.setitem(render.__globals__, "forecast_air_gap", lambda *_: replacement)
    # Returned S=2.3 gives clearance=5-2.3=2.7 and margin=2.7-1=1.7 m.
    assert "| 2030 | 2.3 | 2.7 | 1.7 | Yes |" in render()
