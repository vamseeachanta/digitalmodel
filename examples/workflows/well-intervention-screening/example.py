"""Public BSEE example; run from the repo root with PYTHONPATH=src.

Source: BSEE Well Production (OGOR-A) Report 4081, 2023, issued 03-Mar-2026,
pages 1-4. API 177194065600, interval D02, E018D, lease 00016, WD030:
https://www.data.bsee.gov/Production/Files/Production-A-2023.pdf
Downloaded and checked 2026-11-02; PDF SHA-256:
9d148eeb9927e73f396e2e50777202b1317ae24e79d73e98f49ccabd883689b6

Only source oil volumes and producing days are observations. Fit parameters,
midpoint approximation, threshold, uplift, costs, margin and outage are
illustrative inputs. No historical intervention or recovery is asserted.
The same report (pages 5-11) shows 0 bbl and 0 producing days for this
completion from May to December 2023; zero-day months are not rate
observations and are not passed as history. The screen uses only data
available by 30 April 2023.
"""

from datetime import date
import json

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


def worked_example() -> dict:
    """Return checked screening values; no source fetch or file writes.

    Illustrative fit: qi=1820/30; Di=ln(1820/1685)/59=0.001306287073 /d,
    b=0, start=16-Jan-2023. Fit is supplied, not fitted by the module.
    At 16-Apr q_fit=53.937639862 bopd; measured=41/2=20.5 bopd.
    Constant uplift economics: C=USD20k-40k, outage=2-4 d, u=10-20 bopd,
    net margin=USD50/bbl, H=180 d. Deferred=41-82 bbl; elapsed
    payback=24.05-92.2 d. Cost and uplift are not field-calibrated.
    """
    fit = DeclineFit(
        1820 / 30,
        0.001306287072616592,
        0,
        date(2023, 1, 16),
        "illustrative Jan/Mar two-point exponential fit",
    )
    history = [
        RatePoint(date(2023, month, 16), oil / days, SOURCE)
        for month, oil, days in BSEE_ROWS
    ]
    candidate = candidate_screen(
        fit,
        history,
        shortfall_fraction=0.2,
        consecutive_points=1,
        as_of=date(2023, 4, 30),
        max_age_days=31,
        max_gap_days=40,
    )
    economics = screen_economics(
        baseline_bopd=history[-1].rate_bopd,
        uplift_bopd=Band(10, 20),
        cost_usd=Band(20000, 40000),
        duration_days=Band(2, 4),
        net_price_usd_per_bbl=50,
        horizon_days=180,
        basis="illustrative inputs, not BSEE treatment or field cost estimates",
    )
    return {
        "source": SOURCE,
        "candidate": candidate.candidate,
        "criterion": "more than 20% below fit, one recent positive-rate observation",
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
        "net_cash_180_days_usd": economics.net_cash_usd,
        "limitation": "April has only two producing days; midpoint rates and fit are approximate.",
        "economics_basis": economics.basis,
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
