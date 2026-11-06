"""Public BSEE example; run from the repo root with PYTHONPATH=src.

Source: BSEE Well Production (OGOR-A) Report 4081, 2023, issued 03-Mar-2026,
pages 1-4. API 177194065600, interval D02, E018D, lease 00016, WD030:
https://www.data.bsee.gov/Production/Files/Production-A-2023.pdf
Downloaded and checked 2026-11-02; PDF SHA-256:
9d148eeb9927e73f396e2e50777202b1317ae24e79d73e98f49ccabd883689b6

Only source oil volumes and producing days are observations. Observation dates,
assessment date, threshold, recency limits, uplift, costs, margin, outage and
horizon are required caller inputs with a stated basis. No historical
intervention or recovery is asserted.
The same report (pages 5-11) shows 0 bbl and 0 producing days for this
completion from May to December 2023; zero-day months are not rate
observations and are not passed as history. The caller must qualify the
assessment date and the representative dates assigned to these monthly records.
"""

import argparse
from datetime import date
import json
import math
from pathlib import Path

from digitalmodel.well.intervention import (
    Band,
    DeclineFit,
    Problem,
    RatePoint,
    candidate_screen,
    method_options,
    screen_economics,
)


# Raw selected federal data values: month, oil [bbl], producing days [day].
# These rows are ONE completion, not a sum over the API's other intervals.
BSEE_ROWS = ((1, 1820, 30), (2, 1504, 27), (3, 1685, 30), (4, 41, 2))
SOURCE = "BSEE OGOR-A 2023, Report 4081 pp.1-4, API 177194065600 D02"


def worked_example(
    *,
    observation_dates: list[date],
    as_of: date,
    shortfall_fraction: float,
    consecutive_points: int,
    max_age_days: float,
    max_gap_days: float,
    uplift_bopd: Band,
    cost_usd: Band,
    duration_days: Band,
    net_price_usd_per_bbl: float,
    horizon_days: float,
    basis: str,
) -> dict:
    """Screen source rates using required caller assumptions, with no defaults.

    The exponential fit uses the first and third source rates and the elapsed
    days between their supplied representative dates. The fit form is an
    illustrative choice, not an inferred reservoir model. Input dates must
    belong to their source months; no monthly representative day is inferred.
    The basis must explain the date approximation and screening/economic inputs.
    Nothing is fetched or written by this function.
    """
    if len(observation_dates) != len(BSEE_ROWS) or any(
        not isinstance(day, date) or (day.year, day.month) != (2023, row[0])
        for day, row in zip(observation_dates, BSEE_ROWS)
    ):
        raise ValueError("observation_dates must match the four source months")
    if not isinstance(basis, str) or not basis.strip():
        raise ValueError("basis must describe the caller's scenario inputs")
    first_rate = BSEE_ROWS[0][1] / BSEE_ROWS[0][2]
    third_rate = BSEE_ROWS[2][1] / BSEE_ROWS[2][2]
    elapsed = (observation_dates[2] - observation_dates[0]).days
    fit = DeclineFit(
        first_rate,
        math.log(first_rate / third_rate) / elapsed,
        0,
        observation_dates[0],
        "illustrative first/third-rate exponential fit; " + basis,
    )
    history = [
        RatePoint(day, oil / days, SOURCE)
        for day, (_, oil, days) in zip(observation_dates, BSEE_ROWS)
    ]
    candidate = candidate_screen(
        fit,
        history,
        shortfall_fraction=shortfall_fraction,
        consecutive_points=consecutive_points,
        as_of=as_of,
        max_age_days=max_age_days,
        max_gap_days=max_gap_days,
    )
    economics = screen_economics(
        baseline_bopd=history[-1].rate_bopd,
        uplift_bopd=uplift_bopd,
        cost_usd=cost_usd,
        duration_days=duration_days,
        net_price_usd_per_bbl=net_price_usd_per_bbl,
        horizon_days=horizon_days,
        basis=basis,
    )
    return {
        "source": SOURCE,
        "scenario_readiness": "caller_supplied_scenario",
        "observation_dates": [day.isoformat() for day in observation_dates],
        "as_of": as_of.isoformat(),
        "candidate": candidate.candidate,
        "criterion": {
            "shortfall_fraction": shortfall_fraction,
            "consecutive_points": consecutive_points,
            "max_age_days": max_age_days,
            "max_gap_days": max_gap_days,
        },
        "predicted_bopd": candidate.predicted_bopd,
        "measured_bopd": candidate.measured_bopd,
        "shortfall_fraction": candidate.shortfall_fraction,
        "options_after_diagnosis": [
            x.method.value for x in method_options(Problem.DECLINING_RATE)
        ],
        "payback_from_start_days": [
            economics.payback_from_start_days.low,
            economics.payback_from_start_days.high,
        ],
        "horizon_days": horizon_days,
        "net_cash_usd": economics.net_cash_usd,
        "limitation": "April has only two producing days; representative dates and fit are approximate.",
        "economics_basis": economics.basis,
    }


def main() -> None:
    """Require a JSON scenario file; dates use ISO format and bands are pairs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scenario", type=Path, required=True)
    args = parser.parse_args()
    params = json.loads(args.scenario.read_text(encoding="utf-8"))
    params["observation_dates"] = [
        date.fromisoformat(value) for value in params["observation_dates"]
    ]
    params["as_of"] = date.fromisoformat(params["as_of"])
    for key in ("uplift_bopd", "cost_usd", "duration_days"):
        params[key] = Band(*params[key])
    print(json.dumps(worked_example(**params), indent=2))


if __name__ == "__main__":
    main()
