# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline, single-execution-domain MPI placement receipts and restart plans.

No launcher or solver is invoked. Flags are ordered argv tokens, never shell
text. Open MPI v5.0.6 mpirun, section 17.1.2.4.3, separates mapping, ranking
and binding; Microsoft Learn ``mpiexec`` documents distinct /affinity options.
No backend's defaults are inferred or translated into another backend's flags.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
import hashlib
import json
import math
import re


def _text(value: str, name: str) -> None:
    if not isinstance(value, str) or not value.strip() or "\x00" in value:
        raise ValueError(f"{name} must be nonempty text without NUL")


def _integer(value: int, name: str) -> None:
    # Rank and topology identifiers are nonnegative integers, not booleans.
    if type(value) is not int or value < 0:
        raise ValueError(f"{name} must be a nonnegative integer")


def _digest(value: str, name: str) -> None:
    # Derive length from the stdlib implementation instead of a stored constant.
    if (
        not isinstance(value, str)
        or len(value) != len(hashlib.sha256().hexdigest())
        or not re.fullmatch(r"[0-9a-f]+", value)
    ):
        raise ValueError(f"{name} must be lowercase SHA-256 hex")


def _flags(values: tuple[str, ...]) -> None:
    if not isinstance(values, tuple):
        raise ValueError("flags must be a tuple of argv tokens")
    for value in values:
        _text(value, "flag")


@dataclass(frozen=True)
class Launcher:
    """Caller-observed launcher identity; version must not be inferred."""

    backend: str
    executable: str
    version: str

    def __post_init__(self):
        for name in ("backend", "executable", "version"):
            _text(getattr(self, name), name)


@dataclass(frozen=True)
class RankPlacement:
    """Observed allowed CPU and socket sets; None means unavailable.

    IDs are meaningful only in the receipt's single execution domain. They
    represent effective affinity, not a promise about instantaneous CPU use.
    """

    rank: int
    cpus: tuple[int, ...] | None
    sockets: tuple[int, ...] | None

    def __post_init__(self):
        _integer(self.rank, "rank")
        for name in ("cpus", "sockets"):
            values = getattr(self, name)
            if values is None:
                continue
            if not isinstance(values, tuple) or not values:
                raise ValueError(f"{name} must be a nonempty tuple or None")
            for value in values:
                _integer(value, name)
            if len(set(values)) != len(values):
                raise ValueError(f"duplicate {name}")


def _coverage(observations: tuple[RankPlacement, ...] | None, ranks: int) -> bool:
    if observations is None:
        return False
    if not isinstance(observations, tuple):
        raise ValueError("observations must be a tuple or None")
    seen = set()
    for row in observations:
        if not isinstance(row, RankPlacement):
            raise ValueError("observations must contain RankPlacement records")
        if row.rank in seen or row.rank >= ranks:
            raise ValueError("duplicate or out-of-range rank")
        seen.add(row.rank)
    # Equality is the caller-requested coverage contract, not a fitted limit.
    return seen == set(range(ranks)) and all(
        row.cpus is not None and row.sockets is not None for row in observations
    )


@dataclass(frozen=True)
class PlacementReceipt:
    """Caller-supplied placement evidence associated with one checkpoint.

    case_sha256 covers mesh and numerical dictionaries, excluding intentional
    continuation controls. topology_sha256 identifies the execution domain AND
    CPU/socket numbering/topology. checkpoint_sha256 covers the restart state.
    Producers define and retain those manifests. validation_reference is an
    external validation attestation, not authentication or a throughput test.
    """

    launcher: Launcher
    ranks: int
    flags: tuple[str, ...]
    case_sha256: str
    checkpoint_sha256: str
    topology_sha256: str | None
    observations: tuple[RankPlacement, ...] | None
    validation_reference: str | None = None
    timing_samples: tuple[tuple[int, float], ...] | None = None

    def __post_init__(self):
        if not isinstance(self.launcher, Launcher):
            raise ValueError("launcher must be a Launcher record")
        _integer(self.ranks, "ranks")
        if not self.ranks:
            raise ValueError("ranks must be positive")
        _flags(self.flags)
        for name in ("case_sha256", "checkpoint_sha256"):
            _digest(getattr(self, name), name)
        if self.topology_sha256 is not None:
            _digest(self.topology_sha256, "topology_sha256")
        _coverage(self.observations, self.ranks)
        if self.validation_reference is not None:
            _text(self.validation_reference, "validation_reference")
        if self.timing_samples is not None:
            if not isinstance(self.timing_samples, tuple) or any(
                not isinstance(row, tuple) for row in self.timing_samples
            ):
                raise ValueError("timing_samples must be a tuple of sample tuples")
            iteration_timing(self.timing_samples)

    def to_json(self) -> str:
        """Deterministic receipt bytes; ordered flags and observations retained."""
        return json.dumps(
            asdict(self),
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
            allow_nan=False,
        )

    @classmethod
    def from_json(cls, text: str) -> PlacementReceipt:
        """Read retained receipts, rejecting unknown fields and duplicate keys."""

        def unique(pairs):
            result = {}
            for key, value in pairs:
                if key in result:
                    raise ValueError(f"duplicate receipt field: {key}")
                result[key] = value
            return result

        data = json.loads(text, object_pairs_hook=unique)
        data["launcher"] = Launcher(**data["launcher"])
        if not isinstance(data["flags"], list):
            raise ValueError("flags must be an array")
        data["flags"] = tuple(data["flags"])
        if data["observations"] is not None:
            if not isinstance(data["observations"], list):
                raise ValueError("observations must be an array or null")
            rows = []
            for row in data["observations"]:
                for name in ("cpus", "sockets"):
                    if row[name] is not None:
                        if not isinstance(row[name], list):
                            raise ValueError(f"{name} must be an array or null")
                        row[name] = tuple(row[name])
                rows.append(RankPlacement(**row))
            data["observations"] = tuple(rows)
        if data.get("timing_samples") is not None:
            if not isinstance(data["timing_samples"], list) or any(
                not isinstance(row, list) for row in data["timing_samples"]
            ):
                raise ValueError("timing_samples must be an array of sample arrays")
            data["timing_samples"] = tuple(tuple(row) for row in data["timing_samples"])
        return cls(**data)


@dataclass(frozen=True)
class ContinuationPlan:
    flags: tuple[str, ...]
    source: str
    override_reason: str | None
    baseline_sha256: str
    checkpoint_sha256: str


def plan_continuation(
    baseline: PlacementReceipt,
    *,
    launcher: Launcher,
    ranks: int,
    case_sha256: str,
    checkpoint_sha256: str,
    topology_sha256: str,
    flags: tuple[str, ...] | None = None,
    override_reason: str | None = None,
) -> ContinuationPlan:
    """Reuse a complete validated baseline; any explicit flags need a reason.

    Empty flags explicitly remove all optional flags. Overrides do not waive
    restart compatibility, nor establish that the new placement is validated.
    """
    if not baseline.topology_sha256:
        raise ValueError("baseline requires execution domain and topology evidence")
    if not baseline.validation_reference or not _coverage(
        baseline.observations, baseline.ranks
    ):
        raise ValueError("baseline requires validation evidence and complete affinity")
    _integer(ranks, "ranks")
    requested = dict(
        launcher=launcher,
        ranks=ranks,
        case_sha256=case_sha256,
        checkpoint_sha256=checkpoint_sha256,
        topology_sha256=topology_sha256,
    )
    for name, value in requested.items():
        if value != getattr(baseline, name):
            raise ValueError(f"{name} differs from validated baseline")
    if flags is None:
        if override_reason is not None:
            raise ValueError("reason supplied without flags override")
        selected, source = baseline.flags, "validated_baseline"
    else:
        _flags(flags)
        _text(override_reason, "override reason")
        selected, source = flags, "explicit_override"
    return ContinuationPlan(
        selected,
        source,
        override_reason,
        hashlib.sha256(baseline.to_json().encode("utf-8")).hexdigest(),
        checkpoint_sha256,
    )


@dataclass(frozen=True)
class PlacementCheck:
    status: str
    reason: str


def check_effective_placement(
    baseline: PlacementReceipt,
    observations: tuple[RankPlacement, ...] | None,
    topology_sha256: str | None,
) -> PlacementCheck:
    """Compare supplied post-launch evidence without inferring missing topology."""
    complete = _coverage(observations, baseline.ranks)
    if not complete or not _coverage(baseline.observations, baseline.ranks):
        return PlacementCheck(
            "NOT_EVALUATED", "complete rank CPU/socket evidence required"
        )
    if topology_sha256 is None or topology_sha256 != baseline.topology_sha256:
        return PlacementCheck(
            "NOT_EVALUATED", "same execution domain and topology required"
        )

    def sets(rows):
        return {row.rank: (frozenset(row.cpus), frozenset(row.sockets)) for row in rows}

    if sets(observations) != sets(baseline.observations):
        return PlacementCheck("FAIL", "effective affinity differs from baseline")
    return PlacementCheck("PASS", "effective affinity equals baseline")


@dataclass(frozen=True)
class IterationTiming:
    status: str
    intervals: tuple[tuple[int, int, float], ...]


def iteration_timing(samples: tuple[tuple[int, float], ...] | None) -> IterationTiming:
    """Seconds/iteration = elapsed-time difference / iteration-count difference.

    Python 3.11 Library Reference, time.monotonic: only differences between
    readings are valid, in seconds. Divide by the observed iteration increment
    to obtain the interval mean. Samples are cumulative seconds from one clock in
    one run segment. The first sample is only an anchor; restart clock resets
    need separate calls. No throughput acceptance threshold is supplied.
    """
    if samples is not None and not isinstance(samples, tuple):
        raise ValueError("samples must be a tuple or None")
    intervals = []
    previous = None
    for sample in samples or ():
        if not isinstance(sample, tuple) or len(sample) != 2:
            raise ValueError("each sample must be an iteration/elapsed pair")
        iteration, elapsed = sample
        _integer(iteration, "iteration")
        try:
            valid = (
                type(elapsed) in (int, float)
                and math.isfinite(elapsed)
                and elapsed >= 0
            )
        except OverflowError:
            valid = False
        if not valid:
            raise ValueError("elapsed seconds must be finite and nonnegative")
        if previous is not None:
            old_iteration, old_elapsed = previous
            if iteration <= old_iteration or elapsed < old_elapsed:
                raise ValueError(
                    "iteration must increase and elapsed seconds must not decrease"
                )
            try:
                seconds = (elapsed - old_elapsed) / (iteration - old_iteration)
            except OverflowError as exc:
                raise ValueError("timing interval exceeds numeric range") from exc
            intervals.append((old_iteration, iteration, seconds))
        previous = (iteration, elapsed)
    return IterationTiming("PASS" if intervals else "NOT_EVALUATED", tuple(intervals))
