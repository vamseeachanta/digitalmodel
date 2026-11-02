"""Boolean leak paths, shared events and hand-calculated probability bands."""

from dataclasses import replace
from itertools import product

import pytest

from digitalmodel.well.barriers import (
    ConsequencePolicy, FailureEvent, LeakConnection, Node, RateBand,
    SinkConsequence, generic_topside_rate, pfd_average, rank_leak_paths,
)


POLICY = ConsequencePolicy(10, 0.01, 1, 2, 3, 5, 10,
                           "Synthetic comparison policy, not an acceptance criterion")
INDEPENDENCE = "Illustrative independent events and independent random proof-test phases"


def event(name, q=0.01, mode="leak_in_closed_position", **changes):
    # A synthetic 1000 h interval gives q=(2*q/1000)*1000/2 exactly.
    return replace(FailureEvent(name, mode, RateBand(q / 500, q / 500, q / 500,
        "Synthetic rate for Boolean arithmetic verification"), 1000), **changes)


def run(edges, events, nodes=None, sinks=None, **changes):
    return rank_leak_paths(
        nodes or (Node("r", "source"), Node("a", "cavity"), Node("s", "sink")),
        edges, events,
        sinks or (SinkConsequence("s", "gas", 50, 0.02),), POLICY,
        independence_basis=INDEPENDENCE, **changes,
    )


def test_pfd_units_and_generic_rate_band():
    # Public PDS 2021 sec 4.4.1 p99: aggregate DU rate 2.3e-6 /h,
    # interval [2.1,2.6]e-6 /h; fixed LCP fraction 0.15 is an assumption
    # when applied to the aggregate DU rate. q=3.45e-7*1000/2=0.0001725.
    rate = generic_topside_rate("leak_in_closed_position")
    q = pfd_average(rate, 1000)
    assert (q.low, q.nominal, q.high) == pytest.approx((0.0001575, 0.0001725, 0.000195))
    assert pfd_average(rate, 2000).nominal == pytest.approx(2 * q.nominal)
    assert generic_topside_rate("failure_to_close").nominal_per_hour == pytest.approx(1.035e-6)
    with pytest.raises(ValueError, match="external"):
        generic_topside_rate("external_leak")


def test_series_events_and_consequence_hand_example():
    edges = (LeakConnection("r", "a", "v1", "A"),
             LeakConnection("a", "s", "v2", "B"))
    rows = run(edges, (event("A"), event("B", 0.02)))
    # A AND B: .01*.02=.0002. Synthetic HP sour gas weight: 3*5*10=150.
    assert rows[0].probability.nominal == pytest.approx(0.0002)
    assert rows[0].risk.nominal == pytest.approx(0.03)
    assert rows[0].consequence_class == "HP_SOUR_GAS"
    assert rows[0].paths[0].event_ids == ("A", "B")


def test_shared_event_union_is_not_sum_or_independent_paths():
    edges = (LeakConnection("r", "a", "v1", "A"),
             LeakConnection("a", "s", "v2", "B"),
             LeakConnection("a", "s", "v3", "C"))
    row = run(edges, (event("A"), event("B", 0.02), event("C", 0.03)))[0]
    # A AND (B OR C) = .01*(.02+.03-.02*.03)=.000494.
    assert row.probability.nominal == pytest.approx(0.000494)
    assert len(row.paths) == 2


def test_repeated_event_is_one_failure_and_supersets_are_absorbed():
    edges = (LeakConnection("r", "a", "v1", "A"),
             LeakConnection("a", "s", "v1", "A"),
             LeakConnection("r", "s", "v2", "B"),
             LeakConnection("r", "s", "v1", "A"))
    row = run(edges, (event("A"), event("B", 0.02)))[0]
    assert row.probability.nominal == pytest.approx(0.0298)
    assert {p.event_ids for p in row.paths} == {("A",), ("B",)}


def test_cycles_disconnected_sink_and_open_conduit():
    edges = (LeakConnection("r", "a", "pipe", None, "open"),
             LeakConnection("a", "r", "return", None, "open"),
             LeakConnection("a", "s", "outlet", None, "open"))
    assert run(edges, ())[0].probability.nominal == 1
    row = run(edges[:1], ())[0]
    assert row.probability.nominal == 0
    assert row.paths == ()
    assert "no_modelled_path" in row.flags


@pytest.mark.parametrize("mode, state, demand, expected", [
    ("leak_in_closed_position", "closed", False, .01),
    ("leak_in_closed_position", "open", False, 0),
    ("failure_to_close", "open", True, .01),
    ("failure_to_close", "closed", True, 0),
    ("failure_to_close", "open", False, 0),
    ("external_leak", "open", False, .01),
    ("external_leak", "closed", False, .01),
])
def test_failure_mode_state_applicability(mode, state, demand, expected):
    edge = LeakConnection("r", "s", "v", "A", state, demand)
    assert run((edge,), (event("A", mode=mode),))[0].probability.nominal == expected


def test_common_cause_requires_explicit_shared_event_model():
    edges = (LeakConnection("r", "a", "v1", "A"), LeakConnection("a", "s", "v2", "B"))
    with pytest.raises(ValueError, match="common.cause"):
        run(edges, (event("A", common_cause_group="hydraulic"),
                    event("B", common_cause_group="hydraulic")))
    with pytest.raises(ValueError, match="independence"):
        rank_leak_paths((Node("r", "source"), Node("s", "sink")), (), (),
                        (SinkConsequence("s", "water", 1, 0),), POLICY,
                        independence_basis="")


@pytest.mark.parametrize("rate, tau", [
    (RateBand(0, -1, 1, "test"), 1), (RateBand(2, 1, 3, "test"), 1),
    (RateBand(0, 0, float("nan"), "test"), 1),
    (RateBand(0, 0, 1, "test"), 0), (RateBand(0, 0, 1, "test"), True),
    (RateBand(0, 0, 1, "test"), .2), (RateBand(0, 0, 0, ""), 1),
])
def test_invalid_pfd_and_outside_rare_event_approximation_rejected(rate, tau):
    with pytest.raises(ValueError):
        pfd_average(rate, tau)


def test_zero_rate_and_exact_rare_event_boundary():
    assert pfd_average(RateBand(0, 0, 0, "test"), 1000).nominal == 0
    assert pfd_average(RateBand(.0001, .0001, .0001, "test"), 1000).nominal == .05


def test_path_limit_raises_instead_of_returning_incomplete_ranking():
    edges = (LeakConnection("r", "s", "v1", "A"), LeakConnection("r", "s", "v2", "B"))
    with pytest.raises(ValueError, match="limit"):
        run(edges, (event("A"), event("B")), max_paths=1)


def test_graph_references_and_metadata_validation():
    cases = [
        ((LeakConnection("r", "s", "v", "missing"),), ()),
        ((LeakConnection("r", "missing", "v", None, "open"),), ()),
        ((LeakConnection("r", "s", "v", None, "closed"),), ()),
        ((LeakConnection("r", "s", "v", "A", "unknown"),), (event("A"),)),
        ((LeakConnection("r", "s", "v", "A"),), (event("A"), event("A"))),
    ]
    for edges, events in cases:
        with pytest.raises(ValueError):
            run(edges, events)


def test_shared_union_matches_exhaustive_boolean_reference():
    # Four independent events, three intersecting paths: AB, BC and CD.
    nodes = tuple(Node(n, "source" if n == "r" else "sink" if n == "s" else "cavity")
                  for n in ("r", "x", "y", "z", "s"))
    edges = tuple(LeakConnection(a, b, e, e) for a, b, e in (
        ("r", "x", "A"), ("x", "s", "B"),
        ("r", "y", "B"), ("y", "s", "C"),
        ("r", "z", "C"), ("z", "s", "D")))
    expected = 0
    for bits in product((False, True), repeat=4):
        if (bits[0] and bits[1]) or (bits[1] and bits[2]) or (bits[2] and bits[3]):
            mass = 1
            for failed, q in zip(bits, (.01, .02, .03, .04)):
                mass *= q if failed else 1 - q
            expected += mass
    row = run(edges, tuple(event(n, q) for n, q in zip("ABCD", (.01, .02, .03, .04))), nodes)[0]
    assert row.probability.nominal == pytest.approx(expected, rel=1e-12)


def test_confirmed_failed_element_has_probability_one():
    edges = (LeakConnection("r", "a", "v1", "A"), LeakConnection("a", "s", "v2", "B"))
    row = run(edges, (event("A", known_failed=True), event("B", .02)))[0]
    assert row.probability.nominal == pytest.approx(.02)
    assert "known_failed_event" in row.flags


def test_custom_rate_does_not_claim_a_pds_source():
    row = run((LeakConnection("r", "s", "v", "A"),), (event("A"),))[0]
    assert {c.code_id for c in row.citations} == {"NUREG-0492"}


@pytest.mark.parametrize("change", [
    {"pressure_mpa": float("nan")}, {"h2s_mole_fraction": 20}, {"fluid": "steam"},
])
def test_sink_units_and_scenario_validation(change):
    sink = replace(SinkConsequence("s", "gas", 50, .02), **change)
    with pytest.raises(ValueError):
        run((), (), sinks=(sink,))


def test_pressure_and_sour_policy_boundaries_and_ranking():
    nodes = (Node("r", "source"), Node("s", "sink"), Node("w", "sink"))
    edges = (LeakConnection("r", "s", "v1", "A"), LeakConnection("r", "w", "v2", "B"))
    rows = run(edges, (event("A"), event("B")), nodes,
               (SinkConsequence("w", "water", 1, 0), SinkConsequence("s", "gas", 10, .01)))
    assert [r.sink_id for r in rows] == ["s", "w"]
    assert rows[0].consequence_class == "HP_SOUR_GAS"
    assert rows[0].risk.nominal / rows[1].risk.nominal == pytest.approx(150)


def test_expansion_limit_and_node_metadata_fail_closed():
    with pytest.raises(ValueError, match="limit"):
        run((LeakConnection("r", "a", "pipe", None, "open"),), (), max_expansions=1)
    with pytest.raises(ValueError):
        run((), (), (Node("r", "source"), Node("s", "sink"), Node("s", "cavity")))


def test_wbe_state_and_failure_identity_are_consistent_across_connections():
    for second in (LeakConnection("a", "s", "v", "B"),
                   LeakConnection("a", "s", "v", "A", "open")):
        with pytest.raises(ValueError, match="element"):
            run((LeakConnection("r", "a", "v", "A"), second),
                (event("A"), event("B")))


def test_citation_records_fail_closed_without_touching_canonical_sources(tmp_path, monkeypatch):
    import hashlib
    import shutil
    from pathlib import Path

    from digitalmodel.citations import CitationResolutionError
    from digitalmodel.well.barriers import basis

    original = Path(basis.__file__).with_name("basis")
    before = {p: hashlib.sha256(p.read_bytes()).hexdigest() for p in original.rglob("*.md")}
    target = tmp_path / "basis"
    shutil.copytree(original, target)
    monkeypatch.setattr(basis, "__file__", str(tmp_path / "basis.py"))
    record = target / "wikis/well_integrity/pds.md"
    copied_hash = hashlib.sha256(record.read_bytes()).hexdigest()
    good_bytes = record.read_bytes()
    record.write_bytes(good_bytes.replace(b"revision: 2021", b"revision: 1900"))
    tampered_hash = hashlib.sha256(record.read_bytes()).hexdigest()
    with pytest.raises(CitationResolutionError, match="mismatch"):
        generic_topside_rate("leak_in_closed_position")
    record.write_bytes(good_bytes)
    restored_hash = hashlib.sha256(record.read_bytes()).hexdigest()
    assert copied_hash != tampered_hash and copied_hash == restored_hash
    record.unlink()
    with pytest.raises(CitationResolutionError, match="page_missing"):
        generic_topside_rate("leak_in_closed_position")
    assert before == {p: hashlib.sha256(p.read_bytes()).hexdigest() for p in before}
