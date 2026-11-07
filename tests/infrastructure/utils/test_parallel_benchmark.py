"""Independent synthetic timing examples for parallel benchmark diagnostics."""

import json

import pytest

from digitalmodel.infrastructure.utils import parallel_benchmark


def test_timing_summary(monkeypatch):
    """Hand calculation: mean([2, 4])=3; mean([1, 2])=1.5; ratio=2.

    These are synthetic seconds, not a performance acceptance criterion.
    Python 3.11 statistics.mean defines mean as sum divided by count.
    """
    monkeypatch.setattr(parallel_benchmark.os, "cpu_count", lambda: 8)
    monkeypatch.setattr(
        parallel_benchmark.os, "sched_getaffinity", lambda pid: {2, 3}, raising=False
    )
    monkeypatch.setattr(
        parallel_benchmark.os, "getloadavg", lambda: (1.0, 2.0, 3.0), raising=False
    )
    report = parallel_benchmark.describe_parallel_run([2, 4], [1, 2], workers=2)

    assert report == {
        "sequential_seconds": [2, 4],
        "parallel_seconds": [1, 2],
        "sequential_mean_seconds": 3,
        "parallel_mean_seconds": 1.5,
        "speedup": 2,
        "workers": 2,
        "logical_cpu_count": 8,
        "affinity_cpu_count": 2,
        "load_average": [1.0, 2.0, 3.0],
        "cpu_quota": "NOT_EVALUATED",
    }
    assert json.loads(json.dumps(report, allow_nan=False)) == report


@pytest.mark.parametrize("unavailable", ["missing", "oserror"])
def test_unavailable_cpu_context(monkeypatch, unavailable):
    """Unknown platform context must remain unknown, never imply entitlement."""
    monkeypatch.setattr(parallel_benchmark.os, "cpu_count", lambda: None)

    def unsupported(*args):
        raise OSError("unavailable")

    for name in ("sched_getaffinity", "getloadavg"):
        if unavailable == "missing":
            monkeypatch.delattr(parallel_benchmark.os, name, raising=False)
        else:
            monkeypatch.setattr(parallel_benchmark.os, name, unsupported, raising=False)

    report = parallel_benchmark.describe_parallel_run([1], [2], workers=2)
    assert report["logical_cpu_count"] is None
    assert report["affinity_cpu_count"] is None
    assert report["load_average"] is None
    # Hand calculation for synthetic seconds: (1/1)/(2/1) = 0.5.
    assert report["speedup"] == 0.5
    assert report["cpu_quota"] == "NOT_EVALUATED"


@pytest.mark.parametrize("samples", [[], [0], [-1], [float("inf")], [float("nan")]])
@pytest.mark.parametrize("side", ["sequential", "parallel"])
def test_invalid_durations(samples, side):
    """A ratio needs nonempty, finite, positive wall-clock durations."""
    sequential, parallel = (samples, [1]) if side == "sequential" else ([1], samples)
    with pytest.raises(ValueError, match=f"{side}.*positive"):
        parallel_benchmark.describe_parallel_run(sequential, parallel, workers=2)


@pytest.mark.parametrize("workers", [0, -1, 1.5, True])
def test_invalid_worker_count(workers):
    """Executor worker count is a positive integer, not an acceptance limit."""
    with pytest.raises(ValueError, match="workers"):
        parallel_benchmark.describe_parallel_run([1], [1], workers=workers)


def test_samples_are_copied():
    """Later caller mutations must not alter the recorded observation."""
    sequential, parallel = [1], [2]
    report = parallel_benchmark.describe_parallel_run(sequential, parallel, workers=2)
    sequential.clear()
    parallel.clear()
    assert report["sequential_seconds"] == [1]
    assert report["parallel_seconds"] == [2]
