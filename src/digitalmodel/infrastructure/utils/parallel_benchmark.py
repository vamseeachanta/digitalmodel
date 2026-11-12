"""Timing observations for the parallel benchmark; no performance policy.

Means follow Python 3.11 statistics.mean (sum divided by sample count):
https://docs.python.org/3.11/library/statistics.html#statistics.mean
Speedup follows the existing test_parallel_processing_speedup definition:
sequential mean elapsed seconds / parallel mean elapsed seconds.
"""

import math
import os
from statistics import mean


def _durations(samples, name):
    values = list(samples)
    # Positive finite durations are the domain of the elapsed-time ratio.
    if not values or any(not math.isfinite(t) or t <= 0 for t in values):
        raise ValueError(f"{name} durations must be nonempty, finite and positive")
    return values


def describe_parallel_run(sequential_seconds, parallel_seconds, *, workers):
    """Return JSON-compatible observations without inferring CPU entitlement.

    CPU metadata uses Python 3.11 os.cpu_count, os.sched_getaffinity and
    os.getloadavg: https://docs.python.org/3.11/library/os.html
    The documented affinity argument zero selects the calling process.
    Load averages are returned in the documented order, without normalization.
    Affinity and logical counts do not establish cgroup quota or idle capacity.
    No paths, environment variables or host identifiers are recorded.
    """
    if isinstance(workers, bool) or not isinstance(workers, int) or workers <= 0:
        raise ValueError("workers must be a positive integer")
    sequential = _durations(sequential_seconds, "sequential")
    parallel = _durations(parallel_seconds, "parallel")
    sequential_mean, parallel_mean = mean(sequential), mean(parallel)

    try:
        affinity_count = len(os.sched_getaffinity(0))
    except (AttributeError, OSError):
        affinity_count = None
    try:
        load_average = list(os.getloadavg())
    except (AttributeError, OSError):
        load_average = None

    return {
        "sequential_seconds": sequential,
        "parallel_seconds": parallel,
        "sequential_mean_seconds": sequential_mean,
        "parallel_mean_seconds": parallel_mean,
        "speedup": sequential_mean / parallel_mean,
        "workers": workers,
        "logical_cpu_count": os.cpu_count(),
        "affinity_cpu_count": affinity_count,
        "load_average": load_average,
        "cpu_quota": "NOT_EVALUATED",
    }
