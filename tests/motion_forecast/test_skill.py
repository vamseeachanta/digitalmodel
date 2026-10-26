"""Forecast-skill tests (digitalmodel #1360)."""

from itertools import permutations

import numpy as np
import pytest

from digitalmodel.motion_forecast.measured import MeasuredMotion
from digitalmodel.motion_forecast.models import DOF_NAMES, MotionForecast
from digitalmodel.motion_forecast.skill import (
    SkillRecord,
    aggregate_skill,
    error_vs_lead_time,
)


def _fc(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MotionForecast(t=np.asarray(t, float), dof=base,
                          origin_time=float(t[0]), horizon=float(t[-1] - t[0]))


def _meas(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MeasuredMotion(t=np.asarray(t, float), dof=base)


def test_record_requires_overlap():
    fc = _fc(np.linspace(0, 50, 51), heave=np.zeros(51))
    m = _meas(np.linspace(100, 150, 51), heave=np.zeros(51))
    with pytest.raises(ValueError, match="overlap"):
        SkillRecord(fc, m)


def test_error_vs_lead_time_recovers_injected_growth():
    t = np.linspace(0.0, 100.0, 201)
    fc = _fc(t, heave=np.zeros_like(t))          # forecast predicts 0
    m = _meas(t, heave=0.01 * t)                 # drift grows with lead (= t)
    curve = error_vs_lead_time([SkillRecord(fc, m)], "heave", n_bins=5)
    centers = sorted(curve)
    rmses = [curve[c] for c in centers]
    assert rmses == sorted(rmses)                # monotonic increase with lead
    assert rmses[-1] > rmses[0]


def test_aggregate_skill_pools_residuals_not_mean_of_rmse():
    # record A: 10 samples, residual 1 ; record B: 90 samples, residual 3
    tA = np.linspace(0.0, 9.0, 10)
    tB = np.linspace(0.0, 89.0, 90)
    recs = [
        SkillRecord(_fc(tA, heave=np.zeros_like(tA)), _meas(tA, heave=np.ones_like(tA))),
        SkillRecord(_fc(tB, heave=np.zeros_like(tB)), _meas(tB, heave=np.full_like(tB, 3.0))),
    ]
    agg = aggregate_skill(recs)["heave"]
    pooled = np.sqrt((10 * 1.0 + 90 * 9.0) / 100.0)   # 2.863...
    assert agg.rmse == pytest.approx(pooled, rel=1e-9)
    assert agg.rmse != pytest.approx(2.0)             # != mean-of-rmse (1+3)/2
    assert agg.n_samples == 100


@pytest.fixture
def offset_skill_records():
    """Synthetic example with an independent hand calculation.

    Let u=(-1,-1,1,1), v=(-1,1,-1,1). Both have zero sum,
    u.u=v.v=4 and u.v=0. Each forecast is u plus a DC offset;
    the measured centered vectors are -u, -3u+4v, 4u+3v, u.
    Pearson r=(u.y)/sqrt((u.u)*(y.y)) therefore gives, in order,
    -1, -3/5, 4/5, 1, independently of the offsets below.

    Use forecast offsets (0,100,200,300), measured-minus-forecast
    mean offsets (1,-2,3,-4), and the shared clock (0,1,2,3).
    The measured-minus-forecast residuals are then:
    (3,3,-1,-1), (-2,6,-10,-2), (-3,3,3,9), (-4,-4,-4,-4).
    Their sums are 4,-8,12,-16; squared sums are 20,144,108,64.
    Hence n=16, bias=-8/16=-1/2, RMSE=sqrt(336/16)=sqrt(21).
    These synthetic choices and calculations supply all test constants.
    """
    u = np.array([-1.0, -1.0, 1.0, 1.0])
    v = np.array([-1.0, 1.0, -1.0, 1.0])
    centered_measured = (-u, -3 * u + 4 * v, 4 * u + 3 * v, u)
    offsets = (0, 100, 200, 300)
    biases = (1, -2, 3, -4)
    t = np.arange(len(u), dtype=float)
    return [
        SkillRecord(
            _fc(t, heave=u + offset),
            _meas(t, heave=measured + offset + bias),
        )
        for measured, offset, bias in zip(centered_measured, offsets, biases)
    ]


def test_skill_correlation_distribution_ignores_record_offsets(offset_skill_records):
    # Hand calculation from offset_skill_records: sorted r=(-1,-3/5,4/5,1).
    # For linear quantiles, index=(n-1)*p: median index=3/2 gives 1/10;
    # Q1 index=3/4 gives -7/10; Q3 index=9/4 gives 17/20.
    # Thus IQR=17/20-(-7/10)=31/20, not a pooled correlation.
    expected_correlations = (-1, -3 / 5, 4 / 5, 1)
    for record, expected in zip(offset_skill_records, expected_correlations):
        single = aggregate_skill([record])["heave"]
        assert single.correlation_median == pytest.approx(expected)

    for records in permutations(offset_skill_records):
        agg = aggregate_skill(records)["heave"]
        assert agg.n_samples == 16
        assert agg.bias == pytest.approx(-1 / 2)
        assert agg.rmse == pytest.approx(np.sqrt(21))
        assert agg.correlation_median == pytest.approx(1 / 10)
        assert agg.correlation_iqr == pytest.approx(31 / 20)

    # Concatenating the offset records gives centered sums of products 198004,
    # forecast squares 200016 and measured squares 196324 (by direct expansion
    # of the fixture's vectors). Its r cannot replace the per-record median.
    forecast = np.concatenate([r.forecast.dof["heave"] for r in offset_skill_records])
    measured = np.concatenate([r.measured.dof["heave"] for r in offset_skill_records])
    naive_correlation = np.corrcoef(forecast, measured)[0, 1]
    assert naive_correlation == pytest.approx(198004 / np.sqrt(200016 * 196324))
    assert naive_correlation != pytest.approx(1 / 10)


def test_constant_record_retains_residual_weight(offset_skill_records):
    # Extend the fixture's hand calculation with six constant samples:
    # forecast=50, measured=55, residual=5. Correlation is undefined.
    # n=16+6=22, sum=-8+6*5=22, squared sum=336+6*25=486.
    # Bias=22/22=1 and RMSE=sqrt(486/22)=sqrt(243/11).
    t = np.arange(6, dtype=float)
    constant = SkillRecord(
        _fc(t, heave=np.full_like(t, 50)),
        _meas(t, heave=np.full_like(t, 55)),
    )
    constant_skill = aggregate_skill([constant])["heave"]
    assert constant_skill.n_samples == 6
    assert constant_skill.bias == pytest.approx(5)
    assert constant_skill.rmse == pytest.approx(5)
    assert constant_skill.correlation_median is None
    assert constant_skill.correlation_iqr is None

    for records in permutations([*offset_skill_records, constant]):
        agg = aggregate_skill(records)["heave"]
        assert agg.n_samples == 22
        assert agg.bias == pytest.approx(1)
        assert agg.rmse == pytest.approx(np.sqrt(243 / 11))
        # Omitting the undefined correlation leaves the four-record quantiles.
        assert agg.correlation_median == pytest.approx(1 / 10)
        assert agg.correlation_iqr == pytest.approx(31 / 20)


def test_nan_residual_is_masked_not_poisoning():
    t = np.linspace(0.0, 40.0, 81)
    mh = np.ones_like(t)   # residual 1 vs a zero forecast
    mh[10] = np.nan        # one MRU dropout
    agg = aggregate_skill([SkillRecord(_fc(t, heave=np.zeros_like(t)),
                                       _meas(t, heave=mh))])["heave"]
    assert agg.rmse == pytest.approx(1.0)  # NaN masked, not poisoning the batch
    assert agg.n_samples == 80             # the dropout dropped


def test_aggregate_skill_empty_raises():
    with pytest.raises(ValueError, match="at least one"):
        aggregate_skill([])
