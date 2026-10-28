# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Repeat H1 and D1–D3 without independently qualifying the calculations.

Run with PYTHONPATH=src. With --check, require the retained JSON bytes to match.
Assertions reproduce known limitations; a PASS does not certify those behaviors.
"""

from __future__ import annotations

import argparse
import ast
from dataclasses import asdict, replace
import hashlib
import json
import math
from pathlib import Path

from digitalmodel.power.protection import relay_coordination as relay


SOURCE_REVISION = "2aa19e88306d9b4294e49a2b7849b07ae036b205"
SOURCE_SHA256 = "b8df96cb8c7aba8040a1ce4902e57aaa6aee9957e874fc6256c74798cc0f9b57"


def settings(item):
    """Serialize every relay field, using enum names and explicit units.

    Args:
        item: Overcurrent or differential relay settings to serialize.
    """
    return {
        key: value.name if isinstance(value, (relay.CurveType, relay.RelayType)) else value
        for key, value in asdict(item).items()
    }


def run_probes():
    """Return deterministic evidence after checking each documented observation."""
    source = Path(relay.__file__).read_bytes()
    if hashlib.sha256(source).hexdigest() != SOURCE_SHA256:
        raise RuntimeError("Calculation source digest differs from the evidence pin")

    def overcurrent(name, curve=relay.CurveType.VERY_INVERSE, dial=1.0):
        return relay.OvercurrentRelay(
            name, relay.RelayType.OVERCURRENT_51, curve, 100.0, dial, None
        )

    upstream = overcurrent("upstream", relay.CurveType.DEFINITE_TIME, 2.0)
    downstream = overcurrent("downstream", relay.CurveType.DEFINITE_TIME, 0.5)
    hand = relay.coordination_check(upstream, downstream, 500.0)
    expected_margin_s = 1.5  # Hand calculation: 2.0 s minus 0.5 s.
    assert math.isclose(hand["margin_s"], expected_margin_s, rel_tol=0, abs_tol=1e-12)
    assert hand["pass"] is True

    nan_pickup = replace(upstream, pickup_current_A=math.nan)
    nan_minimum = relay.DifferentialRelay("nan-minimum", 20.0, math.nan, 100.0)
    negative_time = relay.inverse_time_curve(
        500.0, 100.0, -0.5, relay.CurveType.DEFINITE_TIME
    )
    negative_restraint = relay.DifferentialRelay("negative-restraint", 20.0, 10.0, -1.0)
    negative_margin = relay.coordination_check(downstream, upstream, 500.0, -2.0)
    observed = {
        "nan_overcurrent_pickup_accepted": math.isnan(nan_pickup.pickup_current_A),
        "nan_differential_minimum_accepted": math.isnan(nan_minimum.minimum_operate_current_A),
        "negative_direct_definite_time_returned": negative_time == -0.5,
        "negative_restraint_accepted": negative_restraint.restraint_current_A == -1.0,
        "negative_requested_margin_passes": negative_margin["pass"] is True,
    }
    assert all(observed.values())

    tree = ast.parse(source)
    calculations = [
        node for node in tree.body
        if isinstance(node, ast.FunctionDef)
        and node.name in {"coordination_check", "time_current_intersection"}
    ]
    assert len(calculations) == 2
    ignored_reads = [
        node.attr for function in calculations for node in ast.walk(function)
        if isinstance(node, ast.Attribute) and isinstance(node.ctx, ast.Load)
        and node.attr in {"relay_type", "instantaneous_pickup_A"}
    ]
    differential = next(
        node for node in tree.body
        if isinstance(node, ast.ClassDef) and node.name == "DifferentialRelay"
    )
    operating_methods = [
        node.name for node in differential.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name != "__post_init__"
    ]
    assert ignored_reads == [] and operating_methods == []
    changed = relay.coordination_check(
        replace(upstream, instantaneous_pickup_A=200.0),
        replace(downstream, instantaneous_pickup_A=200.0), 500.0,
    )
    assert changed == hand

    very = overcurrent("very")
    moderately = overcurrent("moderately", relay.CurveType.MODERATELY_INVERSE)
    slower = overcurrent("slower", dial=2.0)
    coincident = replace(very, relay_id="coincident")
    endpoint_time_s = relay.inverse_time_curve(150.0, 100.0, 1.0, relay.CurveType.VERY_INVERSE)
    definite = overcurrent("endpoint", relay.CurveType.DEFINITE_TIME, endpoint_time_s)
    cases = [
        ("lower_bound_clipping", moderately, very, (0.0, 5000.0), 100),
        ("empty_effective_interval", moderately, very, (0.0, 101.0), 100),
        ("no_sign_change", very, slower, (150.0, 5000.0), 100),
        ("zero_iterations", moderately, very, (150.0, 5000.0), 0),
        ("coincident_curves", very, coincident, (150.0, 5000.0), 100),
        ("exact_lower_endpoint_root", very, definite, (150.0, 5000.0), 100),
    ]
    results = {}
    for name, first, second, current_range, iterations in cases:
        result = relay.time_current_intersection(
            first, second, current_range, tolerance=1e-3, max_iterations=iterations
        )
        if name == "lower_bound_clipping":
            assert result is not None
            assert result == relay.time_current_intersection(first, second, (101.0, 5000.0))
        elif name in {"empty_effective_interval", "no_sign_change", "zero_iterations"}:
            assert result is None
        elif name == "coincident_curves":
            assert result is not None and result[0] == 2575.0
        else:
            assert relay.inverse_time_curve(150.0, 100.0, 1.0, first.curve_type) == second.time_dial_setting
            assert result is not None and result[0] > 4999.9
            assert abs(result[1] - endpoint_time_s) > 1e-6
        second_settings = settings(second)
        if name == "exact_lower_endpoint_root":
            # Preserve the exact construction, rather than a rounded time dial.
            second_settings["time_dial_setting"] = (
                "inverse_time_curve(150.0, 100.0, 1.0, CurveType.VERY_INVERSE)"
            )
        results[name] = {
            "relay1": settings(first), "relay2": second_settings,
            "current_range_A": current_range, "tolerance_A": "1e-3",
            "max_iterations": iterations,
            "result_current_A_time_s_display": (
                None if result is None else [round(value, 1) for value in result]
            ),
            "observation_reproduced": True,
        }

    return {
        "source_revision": SOURCE_REVISION,
        "source_path": "src/digitalmodel/power/protection/relay_coordination.py",
        "source_sha256": SOURCE_SHA256,
        "script_path": "scripts/validation/protection_relay_probe.py",
        "script_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "qualification": "Diagnostic reproductions only; no independent numerical qualification.",
        "display": "Results rounded to one decimal place; assertions use unrounded values.",
        "H1": {
            "upstream": settings(upstream), "downstream": settings(downstream),
            "fault_current_A": 500.0, "min_margin_s": 0.3,
            "hand_expected_margin_s": expected_margin_s, "absolute_tolerance_s": "1e-12",
            "result": hand,
        },
        "D1": {
            "base_upstream": settings(upstream), "base_downstream": settings(downstream),
            "fault_current_A": 500.0,
            "nan_pickup": "Replace base upstream pickup_current_A with NaN",
            "nan_minimum": {"relay_id": "nan-minimum", "slope_percent": 20.0,
                            "minimum_operate_current_A": "NaN", "restraint_current_A": 100.0},
            "negative_time": {"pickup_current_A": 100.0, "time_dial_setting": -0.5,
                              "curve_type": "DEFINITE_TIME"},
            "negative_restraint": settings(negative_restraint),
            "negative_margin": {"upstream": "base_downstream", "downstream": "base_upstream",
                                "min_margin_s": -2.0, "result": negative_margin},
            "observations": observed,
        },
        "D2": {"audited_functions": [node.name for node in calculations],
               "ignored_setting_reads": ignored_reads, "differential_operating_methods": operating_methods,
               "changed_instantaneous_pickup_A": 200.0, "coordination_unchanged": changed == hand},
        "D3": results,
    }


def main():
    """Print the record, or check an existing record without rewriting it."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", type=Path, help="Compare with retained JSON evidence")
    args = parser.parse_args()
    record = json.dumps(run_probes(), indent=2, sort_keys=True, allow_nan=False) + "\n"
    if args.check:
        if args.check.read_bytes() != record.encode():
            parser.exit(1, "Retained evidence differs from the probe result\n")
        print("H1 and D1–D3: PASS; retained evidence matches")
    else:
        print(record, end="")


if __name__ == "__main__":
    main()
