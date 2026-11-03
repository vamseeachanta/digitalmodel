"""Hand calculations, public-source example, and rejection boundaries."""

from datetime import date, timedelta
import math

import pytest
from pint.errors import DimensionalityError

from digitalmodel.units import Q_
from digitalmodel.well.intervention import (
    Band,
    DeclineFit,
    Method,
    PaybackBand,
    Problem,
    RatePoint,
    candidate_screen,
    method_options,
    reference_estimate,
    screen_economics,
    uplift_from_shortfall,
    wireline_conveyance,
)


def test_arps_exponential_hyperbolic_harmonic_and_constant():
    start = date(2023, 1, 1)
    # Arps: exp(-0.01*100); (1+0.5*0.01*100)^(-2); 1/(1+1).
    for b, expected in [(0, 1000 / math.e), (0.5, 1000 / 2.25), (1, 500)]:
        fit = DeclineFit(1000, 0.01, b, start, "test fit")
        assert fit.rate_on(start + timedelta(days=100)) == pytest.approx(expected)
    assert DeclineFit(1000, 0, 0.5, start, "test").rate_on(start) == 1000
    assert DeclineFit(1000, 0.01, 1e-12, start, "test").rate_on(
        start + timedelta(days=100)
    ) == pytest.approx(1000 / math.e)


def test_rate_and_decline_unit_conversion():
    start = date(2023, 1, 1)
    a = DeclineFit(Q_(100, "bbl/day"), Q_(0.365, "1/year"), 0, start, "test")
    b = DeclineFit(
        Q_(100, "bbl/day").to("m**3/s"),
        Q_(0.365, "1/year").to("1/day"),
        0,
        start,
        "test",
    )
    assert a.rate_on(start + timedelta(days=100)) == pytest.approx(
        b.rate_on(start + timedelta(days=100))
    )
    with pytest.raises(DimensionalityError):
        DeclineFit(Q_(100, "psi"), 0.1, 0, start, "test")


@pytest.mark.parametrize(
    "qi,di,b",
    [
        (0, 0, 0),
        (-1, 0, 0),
        (1, -1, 0),
        (1, 0, -0.1),
        (1, 0, 1.1),
        (math.nan, 0, 0),
        (1, math.inf, 0),
    ],
)
def test_decline_rejects_invalid_parameters(qi, di, b):
    with pytest.raises(ValueError):
        DeclineFit(qi, di, b, date(2023, 1, 1), "test")


def test_fit_does_not_extrapolate_backwards():
    fit = DeclineFit(100, 0, 0, date(2023, 1, 2), "test")
    with pytest.raises(ValueError, match="start"):
        fit.rate_on(date(2023, 1, 1))


def screen(history, **kwargs):
    return candidate_screen(
        DeclineFit(100, 0, 0, date(2023, 1, 1), "test"),
        history,
        shortfall_fraction=0.2,
        consecutive_points=2,
        as_of=date(2023, 4, 1),
        max_age_days=40,
        max_gap_days=40,
        **kwargs,
    )


def test_candidate_requires_strict_persistent_recent_shortfall():
    points = [
        RatePoint(date(2023, 2, 1), 79, "test"),
        RatePoint(date(2023, 3, 1), 70, "test"),
    ]
    result = screen(points)
    assert result.candidate and result.shortfall_fraction == pytest.approx(0.3)
    assert result.predicted_bopd == 100
    assert not screen([points[0], RatePoint(points[1].date, 80, "test")]).candidate
    assert not screen(points[:1]).candidate
    assert not screen([]).candidate
    assert not screen([RatePoint(date(2023, 1, 1), 70, "test"), points[0]]).candidate
    assert not screen([RatePoint(date(2023, 1, 1), 70, "test"), points[1]]).candidate
    assert not screen([points[0], RatePoint(points[1].date, 0, "shut-in")]).candidate


def test_candidate_rejects_duplicate_unsorted_future_dates_and_missing_provenance():
    p = RatePoint(date(2023, 3, 1), 50, "test")
    for history in [
        [p, p],
        [p, RatePoint(date(2023, 2, 1), 50, "test")],
        [RatePoint(date(2023, 4, 2), 50, "test")],
    ]:
        with pytest.raises(ValueError):
            screen(history)
    with pytest.raises(ValueError):
        RatePoint(date(2023, 3, 1), 50, "")
    with pytest.raises(ValueError):
        candidate_screen(
            DeclineFit(100, 0, 0, date(2023, 1, 1), "test"),
            [p],
            shortfall_fraction=1,
            consecutive_points=1,
            as_of=p.date,
            max_age_days=30,
            max_gap_days=40,
        )


def test_uplift_is_explicit_gap_recovery_not_an_inferred_treatment_outcome():
    result = screen(
        [
            RatePoint(date(2023, 2, 1), 60, "test"),
            RatePoint(date(2023, 3, 1), 60, "test"),
        ]
    )
    # Fitted 100 minus measured 60; assumed recovery 25%-75% gives 10-30 bopd.
    assert uplift_from_shortfall(result, Band(0.25, 0.75)) == Band(10, 30)
    with pytest.raises(ValueError):
        uplift_from_shortfall(result, Band(0, 1.01))
    with pytest.raises(ValueError):
        uplift_from_shortfall(screen([]), Band(0, 1))
    above = screen(
        [
            RatePoint(date(2023, 2, 1), 120, "test"),
            RatePoint(date(2023, 3, 1), 120, "test"),
        ]
    )
    assert uplift_from_shortfall(above, Band(0.25, 0.75)) == Band(0, 0)


def test_every_problem_has_cited_options_and_subsea_access_is_separate():
    for problem in Problem:
        assert method_options(problem)
        for option in method_options(problem, subsea=True):
            assert option.sources and option.purpose
            if option.method in (Method.SLICKLINE, Method.E_LINE):
                assert Method.RLWI in option.access_classes
            elif option.method == Method.COILED_TUBING:
                assert Method.RLWI not in option.access_classes
                assert Method.RISER_BASED in option.access_classes
            else:
                assert option.access_classes == (Method.SUBSEA_RIG,)
    tubing = method_options(Problem.TUBING_FAILURE)
    assert {x.method for x in tubing} == {Method.HWU, Method.RIG_WORKOVER}
    assert method_options(Problem.DECLINING_RATE)[0].diagnostic
    assert method_options(Problem.INTEGRITY)[0].diagnostic
    # A non-retrievable valve cannot be assigned a wireline replacement.
    assert {
        x.method
        for x in method_options(Problem.VALVE_FAILURE, wireline_retrievable_valve=False)
    } == {Method.HWU, Method.RIG_WORKOVER}
    with pytest.raises(ValueError):
        method_options("unknown")


def test_reference_bands_are_sourced_ranges_and_not_offshore_budgets():
    for method in (
        Method.SLICKLINE,
        Method.E_LINE,
        Method.COILED_TUBING,
        Method.HWU,
        Method.RIG_WORKOVER,
    ):
        if method in (Method.SLICKLINE, Method.HWU):
            estimate = reference_estimate(
                method, duration_days=Band(2, 3), duration_basis="synthetic scenario"
            )
            assert estimate.duration_readiness == "caller_supplied_scenario"
        else:
            estimate = reference_estimate(method)
            assert estimate.duration_readiness == "public_reference_only"
        assert estimate.duration_days.low < estimate.duration_days.high
        assert estimate.day_rate_usd.low < estimate.day_rate_usd.high
        assert estimate.sources and "reference" in estimate.limitations.lower()
        assert estimate.duration_basis
        assert (
            estimate.cost_usd.low
            == estimate.duration_days.low * estimate.day_rate_usd.low
        )
        assert (
            estimate.cost_usd.high
            == estimate.duration_days.high * estimate.day_rate_usd.high
        )
    # Never silently assign an onshore unit day-rate to a subsea vessel.
    for method in (Method.RLWI, Method.RISER_BASED, Method.SUBSEA_RIG):
        with pytest.raises(ValueError, match="supplied"):
            reference_estimate(method)


@pytest.mark.parametrize(
    "method,duration,day_rate,cost",
    [
        # Duration: RPSEA Task 2 (2010) Table 6 survey; IATMI Table 1.
        # Hand costs: 4*3500=14000; 7*8000=56000 USD.
        (Method.E_LINE, (4, 7), (3500, 8000), (14000, 56000)),
        # Duration: RPSEA Table 6 sand wash; IATMI Table 1.
        # Hand costs: 4*12000=48000; 14*25000=350000 USD.
        (Method.COILED_TUBING, (4, 14), (12000, 25000), (48000, 350000)),
        # Duration: RPSEA Table 6 tubing/packer failure and casing leak.
        # Hand costs: 20*18000=360000; 30*28000=840000 USD.
        (Method.RIG_WORKOVER, (20, 30), (18000, 28000), (360000, 840000)),
    ],
)
def test_reference_endpoints_against_independent_literals(
    method, duration, day_rate, cost
):
    """Pin source inputs and hand costs independently of returned bands.

    Rate citation: Workover Rigs Explained (23 July 2026), Table 1 for
    wireline, CT and HWU; Basin-by-Basin Rate Overview for heavy mast.
    https://www.sdbeyondpetro.com/workover-rigs-explained-types-uses-buyer-s-guide-2026
    Duration citations and their links are in reference_estimate's docstring.
    Accessible duration/rate endpoints were transcribed from those sources.
    Unverified duration endpoints are rejected rather than pinned as defaults.
    """
    estimate = reference_estimate(method)
    assert estimate.duration_days == Band(*duration)
    assert estimate.day_rate_usd == Band(*day_rate)
    assert estimate.cost_usd == Band(*cost)


@pytest.mark.parametrize("method", [Method.SLICKLINE, Method.HWU])
def test_unverified_duration_defaults_are_rejected(method):
    with pytest.raises(ValueError, match="explicit duration_days and duration_basis"):
        reference_estimate(method)


@pytest.mark.parametrize("method", list(Method)[:5])
@pytest.mark.parametrize("basis", [None, "", "   "])
def test_explicit_reference_duration_requires_basis(method, basis):
    with pytest.raises(ValueError, match="duration_basis"):
        reference_estimate(method, duration_days=Band(2, 3), duration_basis=basis)


@pytest.mark.parametrize("method", [Method.SLICKLINE, Method.HWU])
def test_duration_basis_alone_cannot_enable_unverified_defaults(method):
    with pytest.raises(ValueError, match="duration_days"):
        reference_estimate(method, duration_basis="synthetic assumption")


@pytest.mark.parametrize(
    "method,rate,cost",
    [
        (Method.SLICKLINE, Band(3500, 8000), Band(7000, 24000)),
        (Method.HWU, Band(20000, 38000), Band(40000, 114000)),
        (Method.E_LINE, Band(3500, 8000), Band(7000, 24000)),
    ],
)
def test_explicit_duration_propagates_scenario_readiness_and_hand_costs(method, rate, cost):
    # Synthetic 2..3 day input; rate endpoints retain the cited rate basis.
    estimate = reference_estimate(
        method, duration_days=Band(2, 3), duration_basis="synthetic duration scenario"
    )
    assert estimate.duration_days == Band(2, 3)
    assert estimate.day_rate_usd == rate
    assert estimate.cost_usd == cost
    assert estimate.duration_basis == "synthetic duration scenario"
    assert estimate.duration_readiness == "caller_supplied_scenario"
    assert "not independently verified" in estimate.limitations
    from digitalmodel.well.intervention.methods import RATES

    assert estimate.sources == (RATES,)


def example_module():
    import runpy
    from pathlib import Path

    return runpy.run_path(
        str(
            Path(__file__).resolve().parents[3]
            / "examples/workflows/well-intervention-screening/example.py"
        )
    )


def example_scenario():
    """Synthetic required inputs, with no implied field calibration."""
    return dict(
        observation_dates=[date(2023, month, 16) for month in range(1, 5)],
        as_of=date(2023, 4, 30),
        shortfall_fraction=0.2,
        consecutive_points=1,
        max_age_days=31,
        max_gap_days=40,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(20000, 40000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=180,
        basis="synthetic inputs, not field estimates",
    )


def test_runnable_example_requires_every_scenario_input():
    run = example_module()["worked_example"]
    params = example_scenario()
    with pytest.raises(TypeError):
        run()
    for key in params:
        with pytest.raises(TypeError):
            run(**{k: v for k, v in params.items() if k != key})


def test_runnable_example_propagates_inputs_and_hand_calculations():
    run = example_module()["worked_example"]
    params = example_scenario()
    result = run(**params)
    assert result["candidate"]
    assert result["payback_from_start_days"] == pytest.approx([24.05, 92.2])
    assert result["net_cash_usd"] == (43900, 155950)
    assert result["economics_basis"] == params["basis"]
    assert result["scenario_readiness"] == "caller_supplied_scenario"
    # These changes must reach the screen, not a baked-in 20%/180-day case.
    changed = run(**(params | {"shortfall_fraction": 0.7, "horizon_days": 2}))
    assert not changed["candidate"]
    assert changed["net_cash_usd"] == (-42050, -22050)


def test_runnable_example_rejects_dates_outside_source_months():
    params = example_scenario()
    params["observation_dates"] = [date(2024, month, 16) for month in range(1, 5)]
    with pytest.raises(ValueError, match="source months"):
        example_module()["worked_example"](**params)


def test_runnable_example_cli_requires_scenario_file():
    import subprocess
    import sys

    result = subprocess.run(
        [sys.executable, example_module()["__file__"]],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 2
    assert "--scenario" in result.stderr


def test_runnable_example_cli_uses_explicit_json(tmp_path):
    import json
    import subprocess
    import sys

    params = example_scenario()
    params["observation_dates"] = [d.isoformat() for d in params["observation_dates"]]
    params["as_of"] = params["as_of"].isoformat()
    for key in ("uplift_bopd", "cost_usd", "duration_days"):
        params[key] = [params[key].low, params[key].high]
    scenario = tmp_path / "scenario.json"
    scenario.write_text(json.dumps(params))
    result = subprocess.run(
        [sys.executable, example_module()["__file__"], "--scenario", str(scenario)],
        capture_output=True,
        text=True,
        check=True,
    )
    values = json.loads(result.stdout)
    assert values["net_cash_usd"] == [43900, 155950]
    assert values["scenario_readiness"] == "caller_supplied_scenario"


def test_payback_includes_deferral_and_elapsed_job_time():
    result = screen_economics(
        baseline_bopd=100,
        uplift_bopd=Band(20, 40),
        cost_usd=Band(10000, 20000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=100,
        basis="illustrative scenario",
    )
    # Deferred oil=100*(2..4)=200..400 bbl; capital plus deferral=$20k..40k.
    # Daily uplift margin=$1k..2k; restart payback=10..40 d;
    # start payback=(2+10)..(4+40)=12..44 d.
    assert result.deferred_oil_bbl == Band(200, 400)
    assert result.payback_after_restart_days == PaybackBand(10, 40)
    assert result.payback_from_start_days == PaybackBand(12, 44)
    # Net oil over 100 days: 20*(100-4)-400=1520; 40*(100-2)-200=3720.
    assert result.incremental_oil_bbl == (1520, 3720)
    assert result.net_cash_usd == (56000, 176000)
    assert result.pays_out_in_all_cases


def test_no_payback_zero_uplift_nonpositive_margin_or_short_life():
    params = dict(
        baseline_bopd=100,
        uplift_bopd=Band(0, 20),
        cost_usd=Band(1000, 2000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=3,
        basis="test",
    )
    result = screen_economics(**params)
    assert result.payback_from_start_days.high is None
    assert not result.pays_out_in_all_cases
    assert result.net_cash_usd[1] < 0
    # No replacement of a negative net price by a positive gross price.
    for price in [0, -10]:
        r = screen_economics(**(params | {"net_price_usd_per_bbl": price}))
        assert r.payback_from_start_days.low is None
        assert not r.pays_out_in_all_cases


@pytest.mark.parametrize("low,high", [(-1, 1), (2, 1), (0, math.nan), (0, math.inf)])
def test_invalid_bands(low, high):
    with pytest.raises(ValueError):
        Band(low, high)


def test_economics_rejects_dimensions_nonfinite_inputs_and_missing_basis():
    params = dict(
        baseline_bopd=100,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(1000, 2000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=100,
        basis="test",
    )
    with pytest.raises(DimensionalityError):
        screen_economics(**(params | {"baseline_bopd": Q_(100, "psi")}))
    for key, value in [
        ("net_price_usd_per_bbl", math.nan),
        ("horizon_days", -1),
        ("basis", ""),
    ]:
        with pytest.raises(ValueError):
            screen_economics(**(params | {key: value}))
    a = screen_economics(**params)
    b = screen_economics(
        **(params | {"baseline_bopd": Q_(100, "bbl/day").to("m**3/s")})
    )
    assert a.net_cash_usd == pytest.approx(b.net_cash_usd)
    assert a.incremental_oil_bbl == pytest.approx(b.incremental_oil_bbl)
    assert a.payback_from_start_days.low == pytest.approx(b.payback_from_start_days.low)
    assert a.payback_from_start_days.high == pytest.approx(
        b.payback_from_start_days.high
    )


@pytest.mark.parametrize("inclination", [90, Q_(90, "degree"), Q_(math.pi / 2, "radian")])
def test_wireline_hook_calls_existing_pump_down_and_checks_clearance(
    monkeypatch, inclination
):
    from digitalmodel.well.completion.pump_down import kinematics

    actual = kinematics.solve_line_speed
    calls = []

    def wrapped(*args, **kwargs):
        calls.append(True)
        return actual(*args, **kwargs)

    monkeypatch.setattr(kinematics, "solve_line_speed", wrapped)
    args = dict(
        casing_id=Q_(4.778, "in"),
        tool_od=Q_(4.5, "in"),
        drift_id=Q_(4.653, "in"),
        min_radial_clearance=Q_(0.05, "in"),
        rate=Q_(15, "bbl/min"),
        density=Q_(8.34, "ppg"),
        buoyed_bha_weight=Q_(873, "lbf"),
        buoyed_cable_weight=Q_(229, "lbf/kft"),
        lateral_length=Q_(4900, "ft"),
        inclination_deg=inclination,
        mu=0.30,
        eccentricity=0.12,
        k=2,
    )
    result = wireline_conveyance(**args)
    assert result.status == "pump_down_screen_pass"
    assert result.line_speed_m_s == pytest.approx(
        Q_(319.18, "ft/min").to("m/s").magnitude, rel=0.001
    )
    assert calls == [True]
    calls.clear()
    options = method_options(Problem.SAND, subsea=True, wireline_inputs=args)
    assert calls == [True]  # Two line options share one existing-solver evaluation.
    assert all(
        x.conveyance.status == "pump_down_screen_pass"
        for x in options
        if x.method in (Method.SLICKLINE, Method.E_LINE)
    )
    assert options[-1].conveyance is None  # CT is not qualified by a wireline model.
    assert method_options(Problem.SAND)[0].conveyance is None
    assert (
        wireline_conveyance(**(args | {"rate": Q_(0, "bbl/min")})).status == "stalled"
    )
    assert (
        wireline_conveyance(**(args | {"min_radial_clearance": Q_(0.10, "in")})).status
        == "clearance_fail"
    )
    assert (
        wireline_conveyance(**(args | {"inclination_deg": 30})).status
        == "gravity_assisted_requires_review"
    )


def test_public_bsee_example():
    # BSEE OGOR-A Report 4081, 2023, issued 03-Mar-2026, pp.1-4:
    # https://www.data.bsee.gov/Production/Files/Production-A-2023.pdf
    # API 177194065600, completion D02, well E018D, lease 00016, WD030.
    # Jan 1820 bbl / 30 producing days = 60.666667 bopd;
    # Feb 1504 / 27 = 55.703704; Mar 1685 / 30 = 56.166667;
    # Apr 41 / 2 = 20.5. April's TWO producing days limit representativeness.
    # Same report pp.5-11: May-Dec 2023 show 0 bbl and 0 producing days for
    # D02. Zero-day months are not rate observations, so they are not passed
    # as history; the as_of date below uses only data available by April.
    # Illustrative two-point exponential fit Jan16 to Mar16 (59 d):
    # Di=ln((1820/30)/(1685/30))/59=0.001306287073 /d.
    # At Apr16 t=90 d, qfit=53.937639862; shortfall=1-20.5/qfit=0.619931461.
    # Fit is supplied to the API; no fit is obtained from a sibling repository.
    fit = DeclineFit(
        1820 / 30,
        0.001306287072616592,
        0,
        date(2023, 1, 16),
        "illustrative Jan/Mar two-point fit",
    )
    source = "BSEE OGOR-A 2023, report 4081, pp.1-4, API 177194065600 D02"
    history = [
        RatePoint(date(2023, m, 16), oil / days, source)
        for m, oil, days in [(1, 1820, 30), (2, 1504, 27), (3, 1685, 30), (4, 41, 2)]
    ]
    result = candidate_screen(
        fit,
        history,
        shortfall_fraction=0.2,
        consecutive_points=1,
        as_of=date(2023, 4, 30),
        max_age_days=31,
        max_gap_days=40,
    )
    assert result.candidate
    assert result.predicted_bopd == pytest.approx(53.937639862, rel=1e-8)
    assert result.shortfall_fraction == pytest.approx(0.619931461, rel=1e-8)
    # Hypothetical inputs, NOT observed BSEE treatment, costs or uplift:
    # C=$20k..40k, outage=2..4 d, uplift=10..20 bopd, margin=$50/bbl.
    # Deferred=41..82 bbl ($2050..4100); restart payback=22.05..88.2 d;
    # start payback=24.05..92.2 d. At 180 d net cash=$43,900..155,950.
    econ = screen_economics(
        baseline_bopd=20.5,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(20000, 40000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=180,
        basis="illustrative inputs, not field estimates",
    )
    assert econ.payback_from_start_days == PaybackBand(24.05, 92.2)
    assert econ.net_cash_usd == (43900, 155950)


@pytest.mark.parametrize(
    "name,value",
    [
        ("mu", -0.1),
        ("eccentricity", -0.1),
        ("k", 0),
        ("inclination_deg", 181),
        ("inclination_deg", Q_(-1, "degree")),
        ("inclination_deg", Q_(181, "degree")),
        ("inclination_deg", Q_(400, "degree")),
        ("inclination_deg", Q_(math.pi + 0.01, "radian")),
        ("inclination_deg", Q_(math.nan, "degree")),
        ("inclination_deg", Q_(math.inf, "radian")),
        ("inclination_deg", Q_(1, "m")),
        ("drift_id", Q_(5, "in")),
        ("density", Q_(1, "day")),
    ],
)
def test_conveyance_rejects_bad_physical_inputs(name, value):
    params = dict(
        casing_id=Q_(4.778, "in"),
        tool_od=Q_(4.5, "in"),
        drift_id=Q_(4.653, "in"),
        min_radial_clearance=Q_(0.05, "in"),
        rate=Q_(15, "bbl/min"),
        density=Q_(8.34, "ppg"),
        buoyed_bha_weight=Q_(873, "lbf"),
        buoyed_cable_weight=Q_(229, "lbf/kft"),
        lateral_length=Q_(4900, "ft"),
        inclination_deg=90,
        mu=0.30,
        eccentricity=0.12,
        k=2,
    )
    with pytest.raises((ValueError, DimensionalityError)):
        wireline_conveyance(**(params | {name: value}))


def test_economics_no_baseline_zero_cost_and_full_outage_horizon():
    r = screen_economics(
        baseline_bopd=0,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(0, 0),
        duration_days=Band(0, 0),
        net_price_usd_per_bbl=50,
        horizon_days=1,
        basis="test",
    )
    assert r.payback_from_start_days == PaybackBand(0, 0)
    assert r.net_cash_usd == (500, 1000)
    r = screen_economics(
        baseline_bopd=100,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(1000, 2000),
        duration_days=Band(5, 10),
        net_price_usd_per_bbl=50,
        horizon_days=2,
        basis="test",
    )
    # Both jobs are still offline; only 2*100=200 bbl are lost by H, not 5-10 days.
    assert r.incremental_oil_bbl == (-200, -200)
    assert r.net_cash_usd == (-12000, -11000)
    assert not r.pays_out_in_all_cases
