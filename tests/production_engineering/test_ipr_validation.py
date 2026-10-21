"""Vogel (1968), Eq. (1), pp. 88-89: independent benchmark and calibration.

Hand calculation (independent of the implementation): the published test is
65 BOPD at pwf=1500 psi, pr=2000 psi. At r=3/4, the Eq. (1) factor is
1-0.2*(3/4)-0.8*(9/16) = 1-0.15-0.45 = 0.4, so qmax=65/0.4=162.5 BOPD.
At pwf=500 psi, r=1/4, factor=1-0.05-0.05=0.9, hence q=146.25 BOPD.
For the inverse, 0.8r^2+0.2r-0.1=0 has positive root r=1/4: pwf=500 psi.
At r=0 and r=1, the rates are 162.5 and 0 BOPD. These fixed results are
stored in the fixture; none is obtained from a call to the model.

The p. 89 worked solution prints 162 and 146 BOPD, with graph-read factors
0.40 and 0.90. Its subsequent prose says 145, inconsistently; the worked
solution is the comparator. The exact qmax, 162.5, lies on the display
boundary of the printed 162. Half a displayed BOPD plus the arithmetic
allowance is used, without assuming a round-half-up convention. Arithmetic
checks use 1e-10 in output units, far below the printed precision but above floating-point roundoff for
these inputs. These are reproduction criteria, not empirical accuracy limits.
"""

import json
import math
import sys
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.production_engineering.ipr_models import ReservoirConditions, VogelIpr

# Numerical allowance for the hand calculation above; not an engineering limit.
# rel=0 below disables percentage slack; only this stated absolute error is allowed.
ARITHMETIC_ABS_TOL = 1e-10


@pytest.fixture
def vogel_example():
    """Load the source-traceable numerical facts and independent arithmetic."""
    path = Path(__file__).parent / "fixtures" / "vogel_1968.json"
    return json.loads(path.read_text(encoding="utf-8"))


@pytest.fixture
def reservoir(vogel_example):
    """Construct the saturated boundary; pb is not reported by the paper."""
    pr = vogel_example["published_inputs"]["reservoir_pressure_psi"]
    return ReservoirConditions(
        reservoir_pressure_psi=pr,
        bubble_point_psi=pr,  # Vogel (1968), p. 84: below-bubble-point scope.
        productivity_index_bopd_psi=vogel_example["hand_calculation"][
            "shut_in_pi_bopd_psi"
        ],
    )


@pytest.fixture
def calibrated(reservoir, vogel_example):
    """Calibrate with only the p. 89 rate/pressure test, not its answer."""
    data = vogel_example["published_inputs"]
    return VogelIpr.from_test_point(
        reservoir, data["test_rate_bopd"], data["test_pressure_psi"]
    )


@pytest.mark.parametrize("roundoff_steps", [0, 1])
def test_published_example(calibrated, vogel_example, roundoff_steps):
    """Reproduce the two integer outputs of Vogel (1968), p. 89 solution."""
    for _ in range(roundoff_steps):
        calibrated.qmax_bopd = math.nextafter(calibrated.qmax_bopd, math.inf)
    expected = vogel_example["published_outputs"]
    tolerance = (
        vogel_example["acceptance"]["display_absolute_tolerance_bopd"]
        + ARITHMETIC_ABS_TOL
    )
    assert calibrated.qmax_bopd == pytest.approx(
        expected["qmax_bopd"], abs=tolerance, rel=0
    )
    assert calibrated.flow_rate(expected["prediction_pressure_psi"]) == pytest.approx(
        expected["prediction_rate_bopd"], abs=tolerance, rel=0
    )


def test_calibrated_aof_hand_calculation(calibrated, vogel_example):
    """Eq. (1): 65/(1-0.15-0.45)=162.5 BOPD, independent calculation above."""
    assert calibrated.qmax_bopd == pytest.approx(
        vogel_example["hand_calculation"]["qmax_bopd"], abs=ARITHMETIC_ABS_TOL, rel=0
    )


def test_existing_forward_curve_hand_calculation(reservoir, vogel_example):
    """Test the existing constructor independently of the new calibration path."""
    hand = vogel_example["hand_calculation"]
    model = VogelIpr(reservoir, hand["qmax_bopd"])
    for pressure, expected in zip(
        hand["curve_pressure_psi"], hand["curve_rate_bopd"], strict=True
    ):
        assert model.flow_rate(pressure) == pytest.approx(
            expected, abs=ARITHMETIC_ABS_TOL, rel=0
        )


def test_existing_inverse_hand_calculation(reservoir, vogel_example):
    """Eq. (1)'s independent positive root r=1/4 gives pwf=500 psi."""
    hand = vogel_example["hand_calculation"]
    model = VogelIpr(reservoir, hand["qmax_bopd"])
    assert model.flowing_pressure(hand["prediction_rate_bopd"]) == pytest.approx(
        vogel_example["published_outputs"]["prediction_pressure_psi"],
        abs=ARITHMETIC_ABS_TOL,
        rel=0,
    )


# Nonfinite sentinels are invalid input probes, not engineering data.
@pytest.mark.parametrize("invalid", [float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("field", ["pr", "pb", "rate", "pwf"])
def test_calibration_rejects_nonfinite(reservoir, vogel_example, field, invalid):
    """Calibration has no finite physical interpretation with these sentinels."""
    data = vogel_example["published_inputs"]
    rate, pwf = data["test_rate_bopd"], data["test_pressure_psi"]
    if field == "pr":
        reservoir = replace(reservoir, reservoir_pressure_psi=invalid)
    elif field == "pb":
        reservoir = replace(reservoir, bubble_point_psi=invalid)
    elif field == "rate":
        rate = invalid
    else:
        pwf = invalid
    with pytest.raises(ValueError, match="finite"):
        VogelIpr.from_test_point(reservoir, rate, pwf)


# Zero and negative values probe Eq. (1)'s pr>0, qtest>0, 0<=pwf<pr domain
# (Vogel 1968, Eq. (1), pp. 88-89); they are not fitted constants.
@pytest.mark.parametrize(
    ("field", "invalid"),
    [("pr", 0.0), ("pr", -1.0), ("rate", 0.0), ("rate", -1.0), ("pwf", -1.0)],
)
def test_calibration_rejects_outside_domain(reservoir, vogel_example, field, invalid):
    """Reject invalid observations rather than clipping a calibration test."""
    data = vogel_example["published_inputs"]
    rate, pwf = data["test_rate_bopd"], data["test_pressure_psi"]
    if field == "pr":
        reservoir = replace(reservoir, reservoir_pressure_psi=invalid)
    elif field == "rate":
        rate = invalid
    else:
        pwf = invalid
    message = "test pressure" if field == "pwf" else "positive"
    with pytest.raises(ValueError, match=message):
        VogelIpr.from_test_point(reservoir, rate, pwf)


def test_calibration_rejects_shut_in(reservoir, vogel_example):
    """At r=1, Eq. (1) vanishes: a positive test rate cannot identify qmax."""
    with pytest.raises(ValueError, match="test pressure"):
        VogelIpr.from_test_point(
            reservoir,
            vogel_example["published_inputs"]["test_rate_bopd"],
            reservoir.reservoir_pressure_psi,
        )


def test_calibration_rejects_above_reservoir_pressure(reservoir, vogel_example):
    """A doubled reservoir pressure is a constructed out-of-domain probe."""
    with pytest.raises(ValueError, match="test pressure"):
        VogelIpr.from_test_point(
            reservoir,
            vogel_example["published_inputs"]["test_rate_bopd"],
            reservoir.reservoir_pressure_psi + reservoir.reservoir_pressure_psi,
        )


def test_calibration_rejects_undersaturated_reservoir(reservoir, vogel_example):
    """Vogel (1968), p. 84: pr>pb requires a different inflow treatment."""
    reservoir = replace(
        reservoir,
        bubble_point_psi=vogel_example["published_inputs"]["test_pressure_psi"],
    )
    with pytest.raises(ValueError, match="bubble point"):
        VogelIpr.from_test_point(
            reservoir,
            vogel_example["published_inputs"]["test_rate_bopd"],
            vogel_example["published_inputs"]["test_pressure_psi"],
        )


def test_calibration_at_zero_pressure(reservoir, vogel_example):
    """At r=0, Eq. (1) is unity, so qmax equals the observed oil rate."""
    rate = vogel_example["published_inputs"]["test_rate_bopd"]
    model = VogelIpr.from_test_point(reservoir, rate, 0.0)  # Eq. (1), r=0.
    assert model.qmax_bopd == rate


def test_calibration_rejects_overflow(reservoir, vogel_example):
    """A finite stress input overflows on division by 0.4; no physical claim."""
    with pytest.raises(ValueError, match="calibrated"):
        VogelIpr.from_test_point(
            reservoir,
            sys.float_info.max,  # Runtime floating-point bound, not physical data.
            vogel_example["published_inputs"]["test_pressure_psi"],
        )
