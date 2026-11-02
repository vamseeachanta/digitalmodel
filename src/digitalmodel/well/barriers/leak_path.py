"""Directed leak-path screening with explicit failure events and consequence policy.

Probability basis: NRC NUREG-0492 (1981), chapter XI pp.12-13 and eq.XI-24;
Boolean operations: chapter VII. PFDavg = lambda_DU * tau / 2, independent
events, perfect periodic proof tests, instantaneous restoration, uniform demand
time and independent random test phases. These assumptions are not established
for an installed well by running this calculation. No annual leak frequency,
release rate, pressure-driven flow, fatality risk or acceptance verdict is given.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from functools import lru_cache
from typing import Sequence

from digitalmodel.citations import Citation

from .basis import boolean, citation, number, positive_integer, text


@dataclass(frozen=True)
class RateBand:
    """Per-mode DU rate in 1/hour; bounds are sensitivity inputs, not a PDF."""

    low_per_hour: float
    nominal_per_hour: float
    high_per_hour: float
    basis: str
    source_record: str | None = None


@dataclass(frozen=True)
class ProbabilityBand:
    low: float
    nominal: float
    high: float


@dataclass(frozen=True)
class FailureEvent:
    """Stable event ID shared by every connection affected by that failure.

Modes: leak_in_closed_position, failure_to_close, external_leak. A nonempty
common_cause_group marks dependence; multiple distinct events in such a group
are rejected. To model a specified common failure, reuse a single event ID on
each affected connection. Its probability and evidence remain caller inputs.
"""

    event_id: str
    mode: str
    rate: RateBand
    proof_test_interval_h: float
    common_cause_group: str | None = None
    known_failed: bool = False


@dataclass(frozen=True)
class Node:
    node_id: str
    kind: str  # source, cavity (annulus/tubing/wellhead void), sink


@dataclass(frozen=True)
class LeakConnection:
    """One directed route through a WBE, conditional on one failure event.

Parallel routes express OR; series routes express AND. Reverse flow needs a
separate connection. Eventless connections must be open conduits. A closed
valve activates its closed-leak event; an open valve activates failure-to-close
only on a closure demand. External-leak events apply in either state. Inactive
mode connections are omitted, never silently changed to an open conduit.
An open valve without closure credit needs an explicit eventless route.
"""

    from_node: str
    to_node: str
    element_id: str
    event_id: str | None
    valve_state: str = "closed"
    closure_demanded: bool = False


@dataclass(frozen=True)
class SinkConsequence:
    sink_id: str
    fluid: str  # water, oil, gas
    pressure_mpa: float
    h2s_mole_fraction: float


@dataclass(frozen=True)
class ConsequencePolicy:
    """Explicit comparison policy, not an ISO or regulatory consequence table.

The score is fluid weight * pressure multiplier * sour multiplier. Thresholds
and weights are all required inputs with an application basis. Pressure uses
MPa and H2S uses mole fraction (not ppm). Equality enters the higher class.
ISO 16530-1:2017 scope supplies the lifecycle frame only; this score is not
claimed to be its method. Sink fluid/pressure represent the release scenario.
"""

    high_pressure_mpa: float
    sour_h2s_mole_fraction: float
    water_weight: float
    oil_weight: float
    gas_weight: float
    high_pressure_multiplier: float
    sour_multiplier: float
    basis: str


@dataclass(frozen=True)
class LeakPath:
    node_ids: tuple[str, ...]
    element_ids: tuple[str, ...]
    event_ids: tuple[str, ...]
    probability: ProbabilityBand


@dataclass(frozen=True)
class RankedLeakRisk:
    source_id: str
    sink_id: str
    consequence_class: str
    severity_weight: float
    probability: ProbabilityBand
    risk: ProbabilityBand
    paths: tuple[LeakPath, ...]
    flags: tuple[str, ...]
    independence_basis: str
    consequence_basis: str
    rate_basis: tuple[tuple[str, str], ...]
    citations: tuple[Citation, ...]


_MODES = ("leak_in_closed_position", "failure_to_close", "external_leak")


def generic_topside_rate(mode: str) -> RateBand:
    """Illustrative valve-only proxy, PDS 2021 sec.4.4.1 printed p99.

DU 2.3e-6/h and aggregate review interval [2.1,2.6]e-6/h are scaled by fixed
LCP 0.15 or FTC 0.45 fractions. The split of DU and fixed-fraction propagation
are assumptions; bands are not per-mode confidence intervals. Delayed operation
is excluded. No external-leak or permanent cement failure rate is available.
The proxy excludes pilot valves and is not calibrated to a particular well.
"""
    citation("pds")
    fractions = {"leak_in_closed_position": 0.15, "failure_to_close": 0.45}
    if mode not in fractions:
        raise ValueError("no public generic external-leak rate; supply a sourced rate")
    f = fractions[mode]
    return RateBand(2.1e-6 * f, 2.3e-6 * f, 2.6e-6 * f,
                    f"PDS 2021 sec.4.4.1 p99; generic topside proxy; fixed {mode} "
                    f"fraction {f}; derived sensitivity band; not well-qualified", "pds")


def pfd_average(rate: RateBand, proof_test_interval_h: float) -> ProbabilityBand:
    """PFDavg = lambda_DU[/h] * tau[h] / 2, dimensionless.

NUREG-0492 (1981), chapter XI pp.12-13, periodically tested components.
The upper lambda*tau must be <=0.1; no clipping conceals invalid intervals.
Perfect full proof tests and restoration are required. Zero interval is invalid;
an explicit zero rate is valid. This is not an abandonment lifetime probability.
"""
    citation("nureg")
    if rate.source_record is not None:
        if rate.source_record != "pds":
            raise ValueError("unknown public rate source_record")
        citation(rate.source_record)
    text("rate basis", rate.basis)
    values = (rate.low_per_hour, rate.nominal_per_hour, rate.high_per_hour)
    for value in values:
        number("failure rate per hour", value)
    if not values[0] <= values[1] <= values[2]:
        raise ValueError("rate band must be ordered low <= nominal <= high")
    number("proof_test_interval_h", proof_test_interval_h, positive=True)
    if values[2] * proof_test_interval_h > 0.1:
        raise ValueError("lambda*tau exceeds the 0.1 rare-event approximation limit")
    return ProbabilityBand(*(v * proof_test_interval_h / 2 for v in values))


def _minimal(terms: Sequence[frozenset[str]]) -> tuple[frozenset[str], ...]:
    result: list[frozenset[str]] = []
    for term in sorted(set(terms), key=lambda t: (len(t), tuple(sorted(t)))):
        if not any(previous <= term for previous in result):
            result.append(term)
    return tuple(result)


def _union_probability(terms: tuple[frozenset[str], ...], q: dict[str, float]) -> float:
    """Exact Boolean union for supplied independent Bernoulli probabilities.

Conditioning on one event partitions the sample space into disjoint cases.
Unlike a sum or independent-path OR, repeated event IDs remain the same event.
Source: NUREG-0492, chapter VII Boolean/probability laws and XI-24.
"""
    @lru_cache(maxsize=None)
    def solve(parts: tuple[frozenset[str], ...]) -> float:
        if not parts:
            return 0.0
        if not parts[0]:
            return 1.0
        pivot = min(parts[0])
        yes = _minimal(tuple(p - {pivot} for p in parts))
        no = _minimal(tuple(p for p in parts if pivot not in p))
        return q[pivot] * solve(yes) + (1 - q[pivot]) * solve(no)
    return solve(_minimal(terms))


def _consequence(s: SinkConsequence, p: ConsequencePolicy) -> tuple[str, float]:
    text("sink_id", s.sink_id)
    if s.fluid not in ("water", "oil", "gas"):
        raise ValueError("fluid must be water, oil or gas")
    number("pressure_mpa", s.pressure_mpa)
    number("h2s_mole_fraction", s.h2s_mole_fraction)
    if s.h2s_mole_fraction > 1:
        raise ValueError("H2S must be a mole fraction <= 1")
    high = s.pressure_mpa >= p.high_pressure_mpa
    sour = s.h2s_mole_fraction >= p.sour_h2s_mole_fraction
    weight = getattr(p, f"{s.fluid}_weight")
    weight *= p.high_pressure_multiplier if high else 1
    weight *= p.sour_multiplier if sour else 1
    number("combined consequence weight", weight, positive=True)
    return f"{'HP' if high else 'LP'}_{'SOUR' if sour else 'SWEET'}_{s.fluid.upper()}", weight


def rank_leak_paths(
    nodes: Sequence[Node], connections: Sequence[LeakConnection],
    events: Sequence[FailureEvent], sinks: Sequence[SinkConsequence],
    policy: ConsequencePolicy, *, independence_basis: str,
    max_paths: int = 1000, max_expansions: int = 100_000,
) -> tuple[RankedLeakRisk, ...]:
    """Rank source/sink pairs by descending nominal PFD * consequence weight.

Simple directed paths become failure-event conjunctions; duplicate event sets
and strict supersets are absorbed per source/sink. Reported paths are minimal
failure sets, with one representative geometric route each. Exact Boolean union
handles shared events. Bounds evaluate the same monotone model at all-low and
all-high rates; they are sensitivity bounds, not system confidence intervals.

Work limits (100 nodes, 500 connections, 20 events plus traversal budgets) are
software bounds. Exceeding one raises rather than returning partial results.
Distinct events sharing a declared common cause are rejected; unreported common
causes and synchronized tests remain limitations, named on every result.
"""
    text("independence_basis", independence_basis)
    positive_integer("max_paths", max_paths)
    positive_integer("max_expansions", max_expansions)
    if len(nodes) > 100 or len(connections) > 500 or len(events) > 20:
        raise ValueError("graph size limit exceeded (100 nodes/500 connections/20 events)")
    text("consequence policy basis", policy.basis)
    for name in ("high_pressure_mpa", "sour_h2s_mole_fraction", "water_weight",
                 "oil_weight", "gas_weight", "high_pressure_multiplier", "sour_multiplier"):
        number(name, getattr(policy, name), positive=True)
    if policy.sour_h2s_mole_fraction > 1:
        raise ValueError("sour threshold must be a mole fraction <= 1")
    if policy.high_pressure_multiplier < 1 or policy.sour_multiplier < 1:
        raise ValueError("severity multipliers must be >= 1")
    node_map: dict[str, Node] = {}
    for n in nodes:
        text("node_id", n.node_id)
        if n.kind not in ("source", "cavity", "sink") or n.node_id in node_map:
            raise ValueError("invalid node kind or duplicate node_id")
        node_map[n.node_id] = n
    sources = sorted(n.node_id for n in nodes if n.kind == "source")
    if not sources or not any(n.kind == "sink" for n in nodes):
        raise ValueError("graph needs a source and a sink")
    sink_map = {}
    for s in sinks:
        if s.sink_id in sink_map or s.sink_id not in node_map or node_map[s.sink_id].kind != "sink":
            raise ValueError("invalid or duplicate sink consequence")
        sink_map[s.sink_id] = s
        _consequence(s, policy)
    if set(sink_map) != {n.node_id for n in nodes if n.kind == "sink"}:
        raise ValueError("every sink needs exactly one consequence scenario")

    event_map = {}
    probabilities = {}
    groups = set()
    for event in events:
        text("event_id", event.event_id)
        if event.event_id in event_map or event.mode not in _MODES:
            raise ValueError("duplicate event_id or invalid failure mode")
        if event.common_cause_group is not None:
            text("common_cause_group", event.common_cause_group)
            if event.common_cause_group in groups:
                raise ValueError("dependent common-cause events require an explicit shared event model")
            groups.add(event.common_cause_group)
        event_map[event.event_id] = event
        boolean("known_failed", event.known_failed)
        band = pfd_average(event.rate, event.proof_test_interval_h)
        probabilities[event.event_id] = ProbabilityBand(1, 1, 1) if event.known_failed else band
    adjacency: dict[str, list[LeakConnection]] = {n: [] for n in node_map}
    element_states: dict[str, tuple[str, bool]] = {}
    mode_events: dict[tuple[str, str], str] = {}
    for edge in connections:
        text("element_id", edge.element_id)
        if edge.from_node not in node_map or edge.to_node not in node_map:
            raise ValueError("unknown connection node")
        if edge.valve_state not in ("open", "closed"):
            raise ValueError("valve_state must be open or closed")
        boolean("closure_demanded", edge.closure_demanded)
        state = (edge.valve_state, edge.closure_demanded)
        if element_states.setdefault(edge.element_id, state) != state:
            raise ValueError("inconsistent element state or closure demand")
        if edge.event_id is None:
            if edge.valve_state != "open" or edge.closure_demanded:
                raise ValueError("eventless route must be an open conduit without closure credit")
            active = True
        else:
            if edge.event_id not in event_map:
                raise ValueError("unknown failure event")
            mode = event_map[edge.event_id].mode
            key = (edge.element_id, mode)
            if mode_events.setdefault(key, edge.event_id) != edge.event_id:
                raise ValueError("one element failure mode must share one event_id")
            active = (mode == "external_leak"
                      or (mode == "leak_in_closed_position" and edge.valve_state == "closed")
                      or (mode == "failure_to_close" and edge.valve_state == "open"
                          and edge.closure_demanded))
        if active:
            adjacency[edge.from_node].append(edge)
    for edges in adjacency.values():
        edges.sort(key=lambda e: (e.to_node, e.element_id, e.event_id or ""))

    # Enumerate all geometric simple paths first, without truncation.
    routes: dict[tuple[str, str], list[tuple[tuple[str, ...], tuple[LeakConnection, ...]]]] = {}
    expansions = 0
    path_count = 0

    def visit(source: str, path: tuple[str, ...], edges: tuple[LeakConnection, ...]) -> None:
        nonlocal expansions, path_count
        expansions += 1
        if expansions > max_expansions:
            raise ValueError("graph expansion limit exceeded")
        current = path[-1]
        if node_map[current].kind == "sink":
            path_count += 1
            if path_count > max_paths:
                raise ValueError("path enumeration limit exceeded")
            routes.setdefault((source, current), []).append((path, edges))
            return
        for edge in adjacency[current]:
            if edge.to_node not in path:
                visit(source, path + (edge.to_node,), edges + (edge,))

    for source in sources:
        visit(source, (source,), ())
    rows = []
    for source in sources:
        for sink_id, sink in sorted(sink_map.items()):
            representatives = {}
            for path, edges in routes.get((source, sink_id), ()):
                term = frozenset(e.event_id for e in edges if e.event_id is not None)
                representatives.setdefault(term, (path, edges))
            terms = _minimal(tuple(representatives))
            paths = []
            for term in terms:
                path, edges = representatives[term]
                band = ProbabilityBand(*(math.prod(getattr(probabilities[e], part) for e in term)
                                         for part in ("low", "nominal", "high")))
                paths.append(LeakPath(path, tuple(e.element_id for e in edges),
                                      tuple(sorted(term)), band))
            band = ProbabilityBand(*(_union_probability(terms, {
                key: getattr(value, part) for key, value in probabilities.items()
            }) for part in ("low", "nominal", "high")))
            label, weight = _consequence(sink, policy)
            risk = ProbabilityBand(*(getattr(band, part) * weight
                                     for part in ("low", "nominal", "high")))
            for value in (risk.low, risk.nominal, risk.high):
                number("risk score", value)
            flags = ["screening_only", "independent_events_and_test_phases_assumed",
                     "unmodelled_common_causes_excluded", "rate_bands_are_sensitivity_bounds"]
            if not paths:
                flags.append("no_modelled_path")
            used = sorted(set().union(*terms)) if terms else []
            if any(event_map[e].known_failed for e in used):
                flags.append("known_failed_event")
            source_records = sorted({event_map[e].rate.source_record for e in used
                                     if event_map[e].rate.source_record is not None})
            citations = (citation("nureg"),) + tuple(citation(key) for key in source_records)
            rows.append(RankedLeakRisk(
                source, sink_id, label, weight, band, risk, tuple(paths), tuple(flags),
                independence_basis, policy.basis,
                tuple((e, event_map[e].rate.basis) for e in used), citations,
            ))
    return tuple(sorted(rows, key=lambda r: (-r.risk.nominal, r.source_id, r.sink_id)))
