"""Exercise the benchmark's gate using controlled synthetic elapsed times."""

import json
from statistics import mean

import pytest

from tests.infrastructure.performance.test_benchmarks import (
    TestConcurrencyBenchmarks as _ConcurrencyBenchmarks,
)


@pytest.mark.parametrize(
    "sequential,parallel,passes",
    [([2, 4], [1, 2], True), ([1, 1], [1, 1], False), ([1.2, 1.2], [1, 1], False)],
)
def test_speedup_gate_retains_diagnostics(sequential, parallel, passes, capsys):
    """Hand ratios: 3/1.5=2 passes; 1/1=1 and 1.2/1=1.2 fail.

    Synthetic seconds control timing; the real process-pool workload still runs.
    The strict >1.2 boundary comes from issue #2103 (2026) and the original test,
    not a newly selected performance acceptance limit.
    """

    class ControlledTimings:
        def __init__(self):
            self.results = []
            self.samples = iter((sequential, parallel))

        def run_benchmark(self, func, *, iterations, tasks):
            func(tasks)
            samples = next(self.samples)
            self.results.append({"times": samples})
            return {"mean": mean(samples)}

    run = _ConcurrencyBenchmarks().test_parallel_processing_speedup
    timings = ControlledTimings()
    if passes:
        run(timings)
    else:
        with pytest.raises(AssertionError, match="Insufficient parallel speedup") as error:
            run(timings)
        # Pytest appends its assertion explanation after the explicit message.
        message = str(error.value).splitlines()[0]
        failure_report = json.loads(message.split("; ", maxsplit=1)[1])
        assert failure_report["sequential_seconds"] == sequential
        assert failure_report["parallel_seconds"] == parallel

    output = capsys.readouterr().out
    report = json.loads(output.split("Parallel benchmark observations: ")[1])
    assert report["sequential_seconds"] == sequential
    assert report["parallel_seconds"] == parallel
    assert report["cpu_quota"] == "NOT_EVALUATED"
