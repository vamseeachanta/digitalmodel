"""Deterministic regressions for the sustained-load merge gate.

Synthetic samples are hand-constructed, not measured performance data. The real
analyzer and sustained-load assertions run; only the timed runner is replaced.
Policy sources: repository issues 1889 and 1936, and the fix in PR 1938 (2026).
"""

import pytest

from . import test_load_testing as load_testing


def run_sustained_policy(monkeypatch, samples, *, errors=(), duration=10):
    """Feed synthetic seconds into the real analyzer and sustained-load test.

    The ten-second synthetic interval makes throughput hand-checkable: one
    hundred successful samples / ten seconds = ten requests per second.
    """
    runner = load_testing.LoadTestRunner()
    results = runner._analyze_results(
        [{'response_time': sample} for sample in samples], list(errors), duration
    )
    monkeypatch.setattr(runner, 'run_load_test', lambda *args, **kwargs: results)
    load_testing.TestStressTestScenarios().test_sustained_load(runner)
    return results


@pytest.mark.parametrize(
    'samples, expected_p95, expected_max',
    [
        ([0.0001] * 100, 0.0001, 0.0001),
        ([0.0001] * 99 + [0.01], 0.0001, 0.01),
        ([0.0001] * 94 + [0.001] * 6, 0.001, 0.001),
        ([0.049] * 100, 0.049, 0.049),
    ],
    ids=['uniform-fast', 'isolated-outlier', 'p95-jitter', 'below-ceiling'],
)
def test_absolute_p95_accepts_fast_or_jittery_samples(
    monkeypatch, samples, expected_p95, expected_max
):
    """Hand calculations using sorted zero-based index floor(0.95 * N).

    For N=100, p95 is sample 95. With 99 fast samples and one 10 ms
    outlier it is 0.1 ms; mean=(99*0.1+10)/100=0.199 ms, max/mean>5.
    With 94 fast and six 1 ms samples it is 1 ms;
    mean=(94*0.1+6*1)/100=0.154 ms, p95/mean>5.
    Both pass the existing 50 ms absolute policy (issue 1936 / PR 1938),
    and both historical ratio variants would reject at least one case.
    The uniform 49 ms case is strictly below that same ceiling.
    """
    results = run_sustained_policy(monkeypatch, samples)
    assert results['response_time_stats']['p95'] == expected_p95
    assert results['response_time_stats']['max'] == expected_max
    assert results['throughput_rps'] == 10  # 100 samples / 10 s, as above.
    assert results['error_rate'] == 0  # No failed samples were supplied.


@pytest.mark.parametrize('latency', [0.05, 0.051], ids=['at-ceiling', 'above-ceiling'])
def test_absolute_p95_rejects_sustained_latency(monkeypatch, latency):
    """Every sample equals 50 or 51 ms, so p95 equals that value.

    The strict p95 < 50 ms policy in issue 1936 / PR 1938 rejects equality
    and larger values, regardless of mean or max (both equal p95 here).
    """
    with pytest.raises(AssertionError, match='reaches or exceeds the'):
        run_sustained_policy(monkeypatch, [latency] * 100)


def test_sustained_error_gate_is_preserved(monkeypatch):
    """Two failures / (100 successes + 2 failures) > 1% (issue 1889).

    All successful response times are 0.1 ms; latency cannot cause rejection.
    """
    with pytest.raises(AssertionError, match='High error rate in sustained test'):
        run_sustained_policy(monkeypatch, [0.0001] * 100, errors=[{}, {}])


@pytest.mark.parametrize('successes', [79, 80], ids=['below-throughput', 'at-throughput'])
def test_sustained_throughput_gate_is_preserved(monkeypatch, successes):
    """79/10=7.9 and 80/10=8 RPS both fail strict RPS > 8 (issue 1889).

    All samples are successful and take 0.1 ms, isolating throughput rejection.
    """
    with pytest.raises(AssertionError, match='Low sustained throughput'):
        run_sustained_policy(monkeypatch, [0.0001] * successes)
