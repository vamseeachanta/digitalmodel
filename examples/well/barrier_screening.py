"""Synthetic permanent-envelope screen and identical-anomaly risk comparison.

Run with PYTHONPATH=src uv run python examples/well/barrier_screening.py.
No measured well data is represented. Public sources and worked arithmetic:
src/digitalmodel/well/barriers/docs/well-barrier-screening.html.
"""

from dataclasses import asdict
import json

from digitalmodel.well.barriers import (
    ConsequencePolicy, FailureEvent, FlowZone, LeakConnection,
    Node, PermanentBarrier, SinkConsequence, generic_topside_rate,
    public_norsok_2013_criteria, rank_leak_paths, screen_permanent_barriers,
)


def worked_example() -> dict:
    """Return synthetic inputs and results; no files or external services used."""
    criteria = public_norsok_2013_criteria(
        "mobile_hydrocarbon", "open_hole", min_above_zone_md_m=50,
    )
    zones = (FlowZone("mobile_gas_zone", 2000, criteria),)
    plugs = tuple(PermanentBarrier(
        name, "mobile_gas_zone", top, top + 100, 35, 30, True, True, name,
        "Synthetic separate-placement, complete-cross-section and base-pressure evidence",
    ) for name, top in (("primary", 1800), ("secondary", 1600)))
    permanent = screen_permanent_barriers(
        zones, plugs, gas_migration=False, sustained_casing_pressure=False,
    )
    # Weights/thresholds are deliberately synthetic comparison inputs. They
    # are neither sourced standard limits nor calibrated consequence estimates.
    policy = ConsequencePolicy(10, .01, 1, 2, 3, 5, 10,
        "Synthetic: water/oil/gas weights 1/2/3; pressure >=10 MPa factor5; "
        "H2S >=0.01 mole fraction factor10; no acceptance threshold")
    rate = generic_topside_rate("leak_in_closed_position")
    # Identical anomaly in both wells: first valve is confirmed leaking.
    # It receives q=1, rather than the generic prior for an unfailed valve.
    events = (
        FailureEvent("closed_valve_1_leak", "leak_in_closed_position", rate,
                     1000, known_failed=True),
        FailureEvent("closed_valve_2_leak", "leak_in_closed_position", rate, 1000),
    )
    ranked = []
    for source, fluid, pressure, h2s in (
        ("hp_sour_gas", "gas", 50, .02), ("lp_water_injector", "water", 1, 0),
    ):
        nodes = (Node(source, "source"), Node("tubing_void", "cavity"),
                 Node("environment", "sink"))
        edges = (LeakConnection(source, "tubing_void", "valve_1", events[0].event_id),
                 LeakConnection("tubing_void", "environment", "valve_2", events[1].event_id))
        ranked.extend(rank_leak_paths(
            nodes, edges, events, (SinkConsequence("environment", fluid, pressure, h2s),),
            policy, independence_basis="Illustrative independent remaining failures; "
            "uniform demand time and independent random proof-test phases; not field validated",
        ))
    ranked.sort(key=lambda r: -r.risk.nominal)
    return {
        "data_status": "synthetic illustration; not engineering design input",
        "permanent_inputs": {"zones": [asdict(z) for z in zones],
                             "barriers": [asdict(b) for b in plugs]},
        "permanent_barrier_screen": asdict(permanent),
        "failure_events": [asdict(e) for e in events],
        "consequence_policy": asdict(policy),
        "ranked_wells": [asdict(r) for r in ranked],
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
