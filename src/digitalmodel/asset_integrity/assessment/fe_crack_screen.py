"""Solver-free screening of FE-supplied crack parameters against an input FAD.

API 579-1/ASME FFS-1 (2016), Part 9, Assessment of Crack-Like Flaws, and
BS 7910 (2013), Assessment of flaws in metallic structures, define the intended
engineering context. Edition conformity remains NOT_EVALUATED: the caller
supplies the envelope, cutoff, material data and secondary multiplier.

The multiplicative secondary-stress form follows the public TWI paper (1999),
Quantification of Residual Stress Effects in the SINTAP Defect Assessment
Procedure for Welded Components. It is not labelled as an API Phi or BS rho
implementation. No licensed-source coefficients are reproduced here.
"""

from __future__ import annotations

import argparse
from copy import deepcopy
from datetime import date
from html import escape
import json
import math
from pathlib import Path

from .fe_crack_growth import basis, growth_life, interpolate, scalar, table_pairs

SECONDARY_SOURCE = (
    "TWI (1999), Quantification of Residual Stress Effects in the SINTAP "
    "Defect Assessment Procedure for Welded Components; multiplicative V method"
)
LIMITATIONS = (
    "Code-edition conformity, partial safety factors, FE mesh/contour qualification, "
    "material applicability and residual-stress qualification: NOT_EVALUATED.",
    "The supplied envelope is interpolated linearly; no extrapolation is permitted. "
    "Only supplied states are screened; failure between states is NOT_EVALUATED.",
    "Growth cycles run to the last supplied depth, not allowable service life. "
    "No fracture, ligament-exhaustion or small-scale-yielding limit is inferred.",
    "Paris growth near threshold, overloads, closure and stress-ratio effects are "
    "NOT_EVALUATED. E-ratio scaling is a user-declared sensitivity only.",
    "An implied nominal opening stress is a diagnostic comparator, not a "
    "substitute for a qualified reference-stress solution.",
)


def _validate(data):
    allowed = {
        "basis",
        "k_mat_mpa_sqrt_m",
        "yield_mpa",
        "lr_max",
        "envelope",
        "envelope_basis",
        "secondary_factor",
        "secondary_basis",
        "states",
        "growth",
        "delta_k_multipliers",
        "history",
        "history_tolerance_fraction",
        "history_basis",
        "consistency",
    }
    if not isinstance(data, dict) or set(data) - allowed:
        raise ValueError("unknown assessment inputs")
    try:
        for key in ("basis", "envelope_basis", "secondary_basis"):
            basis(data[key], key)
        for key in ("k_mat_mpa_sqrt_m", "yield_mpa", "lr_max", "secondary_factor"):
            scalar(data[key], key, positive=True)
        envelope = table_pairs(data["envelope"], "envelope")
        if envelope[0][0] != 0 or envelope[-1][0] != data["lr_max"]:
            raise ValueError("envelope must span zero through lr_max exactly")
        if envelope[0][1] <= 0 or any(
            right[1] > left[1] for left, right in zip(envelope, envelope[1:])
        ):
            raise ValueError(
                "envelope must be nonincreasing with positive initial height"
            )
        states = data["states"]
        if not isinstance(states, list) or len(states) < 2:
            raise ValueError("at least two crack states are required")
        previous = 0.0
        for row in states:
            if set(row) != {
                "a_m",
                "k_primary",
                "k_secondary",
                "k_residual",
                "reference_mpa",
                "delta_k",
            }:
                raise ValueError(
                    "each state requires depth, three K terms, reference stress and DeltaK"
                )
            for key, value in row.items():
                scalar(value, key)
            if row["a_m"] <= previous:
                raise ValueError(
                    "crack depths must be positive and strictly increasing"
                )
            previous = row["a_m"]
        return envelope, states
    except (KeyError, TypeError) as error:
        raise ValueError("missing or malformed required assessment inputs") from error


def _history_check(data, dk_table):
    if "history" not in data:
        if "history_tolerance_fraction" in data or "history_basis" in data:
            raise ValueError("history controls require a supplied history")
        return {
            "status": "NOT_EVALUATED",
            "reason": "No automated growth history supplied",
        }
    try:
        history = table_pairs(data["history"], "growth history")
        tolerance = scalar(data["history_tolerance_fraction"], "history tolerance")
        basis(data["history_basis"], "history basis")
    except KeyError as error:
        raise ValueError("history requires an explicit tolerance and basis") from error
    ratios = []
    for (a0, n0), (a1, n1) in zip(history, history[1:]):
        if n1 <= n0:
            raise ValueError("history cycles must strictly increase")
        clipped = [(a0, interpolate(dk_table, a0))]
        clipped.extend((a, k) for a, k in dk_table if a0 < a < a1)
        clipped.append((a1, interpolate(dk_table, a1)))
        prediction = growth_life(clipped, data["growth"])
        ratios.append(
            (n1 - n0) / prediction["cycles"] if prediction["cycles"] else None
        )
    passed = all(r is not None and abs(r - 1) <= tolerance for r in ratios)
    return {
        "status": "PASS" if passed else "FLAG",
        "cycle_ratios": ratios,
        "tolerance_fraction": tolerance,
        "basis": data["history_basis"],
    }


def _stress_check(data, states):
    if "consistency" not in data:
        return {
            "status": "NOT_EVALUATED",
            "reason": "No geometry factor or diagnostic band supplied",
        }
    block = data["consistency"]
    try:
        if set(block) != {"geometry_factor", "lower_ratio", "upper_ratio", "basis"}:
            raise ValueError(
                "consistency requires geometry factor, ratio band and basis"
            )
        y = scalar(block["geometry_factor"], "geometry factor", positive=True)
        low = scalar(block["lower_ratio"], "lower ratio")
        high = scalar(block["upper_ratio"], "upper ratio", positive=True)
        basis(block["basis"], "consistency basis")
    except (KeyError, TypeError) as error:
        raise ValueError("malformed consistency inputs") from error
    if low > high:
        raise ValueError("diagnostic ratio band is reversed")
    # AFGROW DTD Handbook (2002), section 2.2.1: K=sigma*Y*sqrt(pi*a).
    ratios = []
    for state in states:
        if state["reference_mpa"] == 0:
            ratios.append(None)
            continue
        denominator = scalar(
            y * math.sqrt(math.pi * state["a_m"]) * state["reference_mpa"],
            "diagnostic denominator",
            positive=True,
        )
        ratios.append(scalar(state["k_primary"] / denominator, "diagnostic ratio"))
    return {
        "status": "PASS"
        if all(r is not None and low <= r <= high for r in ratios)
        else "FLAG",
        "ratios": ratios,
        "lower_ratio": low,
        "upper_ratio": high,
        "basis": block["basis"],
    }


def assess_fe_crack(data):
    """Screen supplied states; return incomplete evidence rather than code approval."""
    data = deepcopy(data)
    envelope, states = _validate(data)
    rows = []
    for state in states:
        lr = scalar(state["reference_mpa"] / data["yield_mpa"], "Lr")
        # TWI (1999), multiplicative V method: primary + V*secondary.
        # Residual K is kept distinct in the input and included as secondary K.
        kr = scalar(
            (
                state["k_primary"]
                + data["secondary_factor"]
                * (state["k_secondary"] + state["k_residual"])
            )
            / data["k_mat_mpa_sqrt_m"],
            "Kr",
        )
        height = interpolate(envelope, lr) if lr <= data["lr_max"] else 0.0
        rows.append(
            dict(
                state,
                lr=lr,
                kr=kr,
                f_lr=height,
                inside=lr <= data["lr_max"] and kr <= height,
            )
        )
    dk_table = [(s["a_m"], s["delta_k"]) for s in states]
    try:
        growth = growth_life(dk_table, data["growth"])
    except KeyError as error:
        raise ValueError("growth law is required") from error
    sensitivities = [
        dict(growth_life(dk_table, data["growth"], multiplier=m), multiplier=m)
        for m in data.get("delta_k_multipliers", [])
    ]
    checks = {
        "history": _history_check(data, dk_table),
        "k_reference": _stress_check(data, states),
    }
    return {
        "screen": "INSIDE_SUPPLIED_ENVELOPE"
        if all(s["inside"] for s in rows)
        else "OUTSIDE_SUPPLIED_ENVELOPE",
        "passes": False,
        "evidence": "INCOMPLETE",
        "code_conformity": "NOT_EVALUATED",
        "inputs": data,
        "states": rows,
        "growth": growth,
        "sensitivities": sensitivities,
        "checks": checks,
        "limitations": list(LIMITATIONS),
        "sources": [SECONDARY_SOURCE, growth["source"]],
    }


def _display_checks(value):
    """Round comparisons for display; calculation values stay unchanged."""
    if isinstance(value, float):
        return f"{value:.1f}"
    if isinstance(value, dict):
        return {key: _display_checks(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_display_checks(item) for item in value]
    return value


def report_html(result):
    """Render a self-contained analysis with criteria, inputs and known limits."""

    def show(value):
        if isinstance(value, float):
            return f"{value:.1f}"
        return escape("NOT_EVALUATED" if value is None else str(value))

    def table(headers, rows):
        head = "".join(f"<th>{escape(h)}</th>" for h in headers)
        body = "".join(
            "<tr>" + "".join(f"<td>{show(v)}</td>" for v in row) + "</tr>"
            for row in rows
        )
        return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"

    parts = [
        '<!doctype html><html lang="en"><meta charset="utf-8">',
        "<title>Crack-like flaw analysis</title><style>body{max-width:70em;margin:2em auto;"
        "font:16px sans-serif;line-height:1.5}td,th{padding:.4em;border:1px solid #aaa}"
        "table{border-collapse:collapse}pre{white-space:pre-wrap}</style><body>",
        "<h1>Crack-like flaw analysis</h1><h2>Summary</h2>",
        f'<p>{escape(result["screen"])}: Kr is compared with the supplied f(Lr), '
        'subject to the supplied Lr cutoff. Evidence: INCOMPLETE; '
        'code conformity: NOT_EVALUATED. No service acceptance is established.</p>',
        "<h2>Revision history</h2>",
        table(
            ["Revision", "Date", "Change and reason"],
            [["A", date.today().isoformat(), "Initial analysis of supplied inputs."]],
        ),
        "<h2>Inputs and basis</h2>",
        "<pre>"
        + escape(json.dumps(result["inputs"], indent=2, allow_nan=False))
        + "</pre>",
        "<h2>Method and results</h2><p>Lr = reference stress / yield stress; "
        "Kr = (K primary + V × (K secondary + K residual)) / Kmat. "
        "N = integral da / (C × DeltaK(a)^m). The envelope and DeltaK are "
        "interpolated linearly between supplied points. Comparisons use unrounded values.</p>",
        table(
            ["Depth (m)", "Lr", "Kr", "f(Lr)", "Inside supplied envelope"],
            [
                (str(s["a_m"]), s["lr"], s["kr"], s["f_lr"], s["inside"])
                for s in result["states"]
            ],
        ),
        "<p>Table 1. Supplied crack states and envelope comparisons.</p>",
        "<h3>Growth to last supplied depth</h3>",
        table(
            ["Status", "Cycles", "Arrest depth (m)", "Minimum DeltaK / threshold"],
            [
                [
                    result["growth"]["status"],
                    result["growth"]["cycles"],
                    str(result["growth"]["arrest_a_m"]),
                    result["growth"]["minimum_threshold_ratio"],
                ]
            ],
        ),
        "<p>Table 2. Growth prediction; not allowable service life.</p>",
        "<h3>DeltaK sensitivities</h3>",
        table(
            ["Multiplier", "Status", "Cycles"],
            [
                (s["multiplier"], s["status"], s["cycles"])
                for s in result["sensitivities"]
            ],
        ),
        "<p>Table 3. User-specified changes to DeltaK, with threshold held per input rule.</p>",
        "<h3>Consistency checks</h3>",
        "<pre>"
        + escape(json.dumps(_display_checks(result["checks"]), indent=2))
        + "</pre>",
        "<h2>Limitations and way forward</h2><ul>",
    ]
    parts.extend("<li>" + escape(item) + "</li>" for item in result["limitations"])
    parts.extend(
        [
            "</ul><p>Obtain qualified FE evidence, material data, an edition-specific "
            "envelope and secondary-stress basis, growth applicability and governing "
            "depth criteria before a service disposition.</p><h2>References</h2><ul>"
        ]
    )
    parts.extend("<li>" + escape(s) + "</li>" for s in result["sources"])
    parts.append("</ul></body></html>")
    return "\n".join(parts)


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def main():
    """Read JSON and create an HTML report without overwriting any file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    data = json.loads(
        args.input.read_text(encoding="utf-8"), object_pairs_hook=_unique_object
    )
    report = report_html(assess_fe_crack(data))
    with args.output.open("x", encoding="utf-8") as stream:
        stream.write(report)


if __name__ == "__main__":
    main()
