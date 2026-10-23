# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand calculations for subsidence, daily breach dates and silhouette drag."""

from dataclasses import replace
from datetime import date
import math

import pytest

from digitalmodel.structural.jacket_topside.air_gap import (
    AssessmentBasis,
    CrestCase,
    DesignBasis,
    Silhouette,
    SubsidenceHistory,
    Survey,
    assess_air_gap,
    screen_remediation,
    screen_wave_in_deck,
)


@pytest.fixture
def basis():
    return DesignBasis(
        deck_underside_elevation_m=15.0,
        hat_elevation_m=2.0,
        surge_m=1.0,
        vertical_datum="invented common datum",
        assessment_basis=AssessmentBasis("API RP 2SIM", "user supplied", "design basis"),
        crest_cases=(CrestCase("design", 100.0, 10.0, 0.5),),
    )


@pytest.fixture
def history():
    return SubsidenceHistory(
        (Survey(date(2016, 1, 1), 0.0), Survey(date(2020, 1, 1), 1.0)),
        forecast_rate_m_per_year=0.25,
    )


@pytest.fixture
def silhouette():
    return Silhouette(20.0, 3.0, 0.5, 4.0, 2.0, 1025.0, "invented coefficient")


def test_hand_worked_interpolation_and_forecast(basis, history):
    # 2016--2020 spans 1461 days; 2018-01-01 is 731 days in: 731/1461 m.
    assert history.loss_at(date(2018, 1, 1)) == pytest.approx(0.5003422313483915)
    # 2020--2024 is exactly four 365.25-day years: 1 + 4*0.25 = 2 m.
    result = assess_air_gap(basis, history, date(2020, 1, 1), (date(2024, 1, 1),))
    now, future = result.snapshots
    assert now.deck_elevation_m == pytest.approx(14.0)
    assert now.still_water_air_gap_m == pytest.approx(11.0)
    assert now.crest_clearance_m == pytest.approx(1.0)
    assert now.clearance_margin_m == pytest.approx(0.5)
    assert now.meets_required_clearance and not now.wave_contact
    assert future.deck_elevation_m == pytest.approx(13.0)
    assert future.still_water_air_gap_m == pytest.approx(10.0)
    assert future.crest_clearance_m == pytest.approx(0.0)
    assert not future.meets_required_clearance and future.wave_contact


def test_first_breach_is_first_day_strictly_below_requirement(basis, history):
    result = assess_air_gap(basis, history, date(2020, 1, 1), (date(2024, 1, 1),))
    breach = result.breaches[0]
    # 0.5 / 0.25 * 365.25 = 730.5 days after 2020-01-01.
    assert breach.first_breach_date == date(2022, 1, 1)
    assert breach.first_breach_year == 2022
    assert not breach.breached_at_history_start
    before = assess_air_gap(basis, history, date(2021, 12, 31)).snapshots[0]
    after = assess_air_gap(basis, history, date(2022, 1, 1)).snapshots[0]
    assert before.meets_required_clearance
    assert not after.meets_required_clearance


def test_forecast_equality_day_is_not_the_first_breach(basis, history):
    # 0.5 m / 0.125 m/year = exactly 1461 days: 2024-01-01.
    history = replace(history, forecast_rate_m_per_year=0.125)
    result = assess_air_gap(basis, history, date(2024, 1, 1), (date(2024, 1, 2),))
    assert result.snapshots[0].meets_required_clearance
    assert not result.snapshots[1].meets_required_clearance
    assert result.breaches[0].first_breach_date == date(2024, 1, 2)


def test_decimal_equality_is_not_a_false_breach_or_missed_contact(basis, silhouette):
    history = SubsidenceHistory((Survey(date(2020, 1, 1), 0),), 0)
    basis = replace(basis, deck_underside_elevation_m=10.2, hat_elevation_m=1,
                    surge_m=0, crest_cases=(CrestCase("decimal", 100, 8, 1.2),))
    result = assess_air_gap(basis, history, date(2020, 1, 1))
    assert result.snapshots[0].meets_required_clearance
    assert result.breaches[0].first_breach_date is None
    touching = replace(basis, deck_underside_elevation_m=10.3, hat_elevation_m=2.1,
                       crest_cases=(CrestCase("decimal", 100, 8.2, 0),))
    snapshot = assess_air_gap(touching, history, date(2020, 1, 1)).snapshots[0]
    assert snapshot.wave_contact
    load = screen_wave_in_deck(snapshot, silhouette)
    assert load.horizontal_force_kn == 0
    assert load.requires_full_assessment


def test_equality_passes_and_breach_can_occur_inside_survey_history(basis, history):
    basis = replace(basis, crest_cases=(CrestCase("design", 100, 10, 1.5),))
    result = assess_air_gap(basis, history, date(2020, 1, 1))
    assert result.breaches[0].first_breach_date == date(2018, 1, 1)
    # At the first observation the original 2 m crest clearance is sufficient.
    equality = replace(basis, deck_underside_elevation_m=15.5)
    snap = assess_air_gap(equality, history, date(2020, 1, 1)).snapshots[0]
    assert snap.clearance_margin_m == pytest.approx(0.0)
    assert snap.meets_required_clearance


def test_no_breach_means_only_within_reported_horizon(basis, history):
    result = assess_air_gap(basis, replace(history, forecast_rate_m_per_year=0),
                            date(2020, 1, 1), (date(2050, 1, 1),))
    assert result.breaches[0].first_breach_date is None
    assert result.breaches[0].first_breach_year is None
    assert result.breaches[0].through_date == date(2050, 1, 1)
    breached = replace(basis, deck_underside_elevation_m=12)
    result = assess_air_gap(breached, history, date(2020, 1, 1))
    assert result.breaches[0].first_breach_date == date(2016, 1, 1)
    assert result.breaches[0].breached_at_history_start


def test_multiple_return_periods_remain_separate(basis, history):
    basis = replace(basis, crest_cases=(
        CrestCase("operating", 10, 8, 0.5), CrestCase("extreme", 100, 10, 0.5),
    ))
    result = assess_air_gap(basis, history, date(2020, 1, 1), (date(2024, 1, 1),))
    assert [b.first_breach_year for b in result.breaches] == [None, 2022]
    assert len(result.snapshots) == 4
    assert result.snapshots[-1].case_name == "extreme"


def test_silhouette_hand_calculation_and_depth_cap(basis, history, silhouette):
    # At 2028: settlement 3 m, deck 12 m, total crest 13 m, immersion 1 m.
    snap = assess_air_gap(basis, history, date(2028, 1, 1)).snapshots[0]
    load = screen_wave_in_deck(snap, silhouette)
    # A = 20*1*0.5 = 10 m2; F = 0.5*1025*2*10*4^2 = 164000 N.
    assert load.wetted_silhouette_m2 == pytest.approx(10.0)
    assert load.horizontal_force_kn == pytest.approx(164.0)
    assert load.requires_full_assessment
    assert load.assessment_basis == basis.assessment_basis
    assert load.coefficient_basis == "invented coefficient"
    assert load.status == "screening_only"
    deep = replace(snap, crest_clearance_m=-10)
    assert screen_wave_in_deck(deep, silhouette).horizontal_force_kn == pytest.approx(492)


@pytest.mark.parametrize("clearance,contact", [(1.0, False), (0.0, True)])
def test_dry_or_touching_has_zero_drag_but_touch_requires_assessment(
    basis, history, silhouette, clearance, contact,
):
    snap = assess_air_gap(basis, history, date(2020, 1, 1)).snapshots[0]
    load = screen_wave_in_deck(replace(snap, crest_clearance_m=clearance), silhouette)
    assert load.horizontal_force_kn == 0
    assert load.requires_full_assessment is contact


def test_raise_height_restores_all_target_clearances(basis, history):
    options = screen_remediation(basis, history, date(2020, 1, 1), date(2028, 1, 1), 0.2)
    live, raising, retirement = options.options
    assert (live.name, raising.name, retirement.name) == (
        "live_with_it", "raise_topside", "decommission",
    )
    # Target deck is 12; crest + clearance = 13.5. Add 0.2 m allowance.
    assert raising.required_lift_m == pytest.approx(1.7)
    assert options.governing_case == "design"
    assert len(raising.checklist) == 7
    raised = replace(basis, deck_underside_elevation_m=16.7)
    target = assess_air_gap(raised, history, date(2028, 1, 1)).snapshots[0]
    assert target.clearance_margin_m == pytest.approx(0.2)
    assert "manning" in " ".join(live.checklist)
    assert "interfaces" in " ".join(raising.checklist)
    assert retirement.required_lift_m is None


def test_raising_never_negative_and_worst_case_governs(basis, history):
    assert screen_remediation(basis, history, date(2020, 1, 1),
                              date(2020, 1, 1)).options[1].required_lift_m == 0
    basis = replace(basis, crest_cases=(
        CrestCase("low", 10, 8, 0.5), CrestCase("high", 100, 10, 1.0),
    ))
    result = screen_remediation(basis, history, date(2020, 1, 1), date(2028, 1, 1))
    assert result.governing_case == "high"
    assert result.options[1].required_lift_m == pytest.approx(2.0)


@pytest.mark.parametrize("field,value", [
    ("deck_underside_elevation_m", math.nan), ("hat_elevation_m", math.inf),
    ("surge_m", -1), ("vertical_datum", ""), ("crest_cases", ()),
])
def test_invalid_design_inputs(basis, field, value):
    with pytest.raises((ValueError, TypeError)):
        replace(basis, **{field: value})


@pytest.mark.parametrize("surveys,rate", [
    ((), 0), ((Survey(date(2020, 1, 1), 1), Survey(date(2016, 1, 1), 0)), 0),
    ((Survey(date(2020, 1, 1), 1), Survey(date(2020, 1, 1), 2)), 0),
    ((Survey(date(2016, 1, 1), 1), Survey(date(2020, 1, 1), 0)), 0),
    ((Survey(date(2020, 1, 1), 0),), -1),
])
def test_invalid_history(surveys, rate):
    with pytest.raises(ValueError):
        SubsidenceHistory(surveys, rate)


def test_invalid_case_and_silhouette_inputs(basis, silhouette):
    for args in [("", 100, 10, 1), ("x", 0, 10, 1), ("x", 100, -1, 1),
                 ("x", 100, 10, -1), ("x", 100, math.nan, 1)]:
        with pytest.raises(ValueError):
            CrestCase(*args)
    with pytest.raises(ValueError):
        replace(basis, crest_cases=basis.crest_cases * 2)
    for field, value in [("width_m", 0), ("deck_depth_m", -1), ("solidity", 1.1),
                         ("velocity_m_s", -1), ("drag_coefficient", 0),
                         ("density_kg_m3", math.inf), ("coefficient_basis", "")]:
        with pytest.raises(ValueError):
            replace(silhouette, **{field: value})
    with pytest.raises(TypeError):
        replace(silhouette, width_m=True)


def test_reject_uncovered_dates_and_negative_allowance(basis, history):
    with pytest.raises(ValueError):
        history.loss_at(date(2015, 1, 1))
    with pytest.raises(ValueError):
        assess_air_gap(basis, history, date(2020, 1, 1), (date(2019, 1, 1),))
    with pytest.raises(ValueError):
        screen_remediation(basis, history, date(2020, 1, 1), date(2028, 1, 1), -1)


def test_assessment_basis_requires_identification():
    with pytest.raises(ValueError):
        AssessmentBasis("unsupported", "edition", "reference")
    with pytest.raises(ValueError):
        AssessmentBasis("ISO 19902", "", "reference")


def test_catalog_registers_air_gap():
    from digitalmodel.asset_integrity import offering_catalog as oc

    catalog = oc.load()
    engine = catalog.ffs["engines"]["air-gap"]
    assert engine["status"] == "engine"
    assert engine["module"] == "structural.jacket_topside.air_gap"
    rows = [row for row in catalog.by_industry("upstream-offshore-fixed")
            if "subsidence" in row.mechanism]
    assert len(rows) == 1
    assert "air-gap" in rows[0].engines and rows[0].status == "engine"
