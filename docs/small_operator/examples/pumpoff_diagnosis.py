#!/usr/bin/env python
# ABOUTME: Worked dynacard pump-off diagnosis for a marginal rod-pumped well
# ABOUTME: Reconstruction from publicly stated parameters — NOT a real card file

"""
Pump-off sequence diagnosis — worked example (catalogue ``so-lift-001``)
=======================================================================

Answers the question a marginal-well operator actually asked: *the card walked
across the screen over five minutes — what happened, and what do I change?*

What this is, and what it is not
--------------------------------
This is a **reconstruction**, not a diagnosis of anyone's well. We do not have
a card file — only phone photographs of an app screen and one stated rod
string. So the run below takes the parameters that were actually stated,
declares every other parameter as an assumption, and sweeps the pump-off
progression through the real ``digitalmodel`` solver.

The output is therefore useful for two things:

1. showing what the toolchain returns end to end, and
2. generating a precise **data ask** — every ``ASSUMED`` value below is a
   question we would rather put to the operator than answer ourselves.

This distinction is not pedantry. An earlier analysis of a card from this same
community was corrected in public because a gearbox rating had been inferred
from an equipment designation and a fillage had been computed off surface
rather than plunger stroke. Stating assumptions as assumptions is the fix.

Run
---
::

    PYTHONPATH=src python docs/small_operator/examples/pumpoff_diagnosis.py
"""

from __future__ import annotations

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_fluid_pound_card,
    generate_normal_card,
    surface_card_from_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

# ---------------------------------------------------------------------------
# Well definition
# ---------------------------------------------------------------------------
# STATED  — said publicly by the operator, quoted, not inferred.
# ASSUMED — our placeholder. Every one of these is a question for the operator.

# All STATED values are verbatim from the operator's own posts.
STATED = {
    "rod_length_ft": 4200.0,    # "4200' of 3/4\" steel rods"
    "rod_diameter_in": 0.75,    # ditto
    "pump_depth_ft": 4300.0,    # "4300' pump depth"
    "pump_bore_in": 1.25,       # "1-1/4\" top hold down insert pump"
    "stroke_length_in": 41.0,   # "41\" of stroke at surface"
    "spm": 6.4,                 # "6.4 SPM"
}

# Context the operator also gave, used as a sanity check rather than as solver
# input: 23 BOPD with no water, 25 psi on the casing, Baird valve set at 150 psi.
STATED_CONTEXT = {
    "oil_bopd": 23.0,
    "water_bwpd": 0.0,
    "casing_psi": 25.0,
    "baird_valve_psi": 150.0,
}

ASSUMED = {
    # Genuinely unknown. The "C-66" in the thread is the PRIME MOVER — the
    # operator's own word — so it is an engine designation and carries no
    # gearbox rating. A previous analysis of this same well was corrected in
    # public for treating it as one. This placeholder is a question, not an
    # answer, and no torque conclusion should be drawn from it.
    "gear_box_rating_in_lb": 57_000.0,
}

# Open discrepancy, deliberately not reconciled: the rod string is stated as
# 4200 ft but the pump depth as 4300 ft. That 100 ft could be pump length, a
# sub, or simply two different roundings. It is a question for the operator,
# not something to average away.

# The pump-off progression, anchored the way the operator described it: it
# started full and ended pumped off.
#
# ``None`` = the full-pump anchor (``generate_normal_card``). The remaining
# entries are ``drop_position`` on ``generate_fluid_pound_card``, its fillage
# knob — lower means the traveling valve transfers later, i.e. poorer fillage.
#
# Note the two generators are not one continuous scale. A pounding card is by
# construction never a full pump, so ``generate_fluid_pound_card`` saturates
# near 70% however high ``drop_position`` goes. The full-pump anchor has to
# come from ``generate_normal_card``, and the step between the two is a change
# of model, not a smaller increment of the same one.
SEQUENCE: list[float | None] = [
    None, 0.80, 0.75, 0.65, 0.55, 0.45, 0.40, 0.35, 0.32, 0.30, 0.25,
]


def build_context(drop_position: float | None, pump_bore_in: float) -> DynacardAnalysisContext:
    """Forward-model a pump card up the rod string into a surface card.

    The generators emit DOWNHOLE cards, so handing one straight to
    ``surface_card`` would ask the solver to strip a rod string that was never
    there. Running it up first gives the solver a real conversion to perform.
    """
    if drop_position is None:
        pump_card = generate_normal_card(seed=7)
    else:
        pump_card = generate_fluid_pound_card(seed=7, drop_position=drop_position)
    well = {
        "api14": f"RECON-{pump_bore_in}-{drop_position}",
        "surface_card": pump_card.model_dump(),  # placeholder, replaced below
        "rod_string": [
            {
                "diameter": STATED["rod_diameter_in"],
                "length": STATED["rod_length_ft"],
            }
        ],
        "pump": {"diameter": pump_bore_in, "depth": STATED["pump_depth_ft"]},
        "surface_unit": {
            "manufacturer": "Unknown",
            "unit_type": "C",
            "stroke_length": STATED["stroke_length_in"],
            "gear_box_rating": ASSUMED["gear_box_rating_in_lb"],
        },
        "spm": STATED["spm"],
    }
    seed_ctx = DynacardAnalysisContext(**well)
    well["surface_card"] = surface_card_from_pump_card(pump_card, seed_ctx).model_dump()
    return DynacardAnalysisContext(**well)


def run_sequence(pump_bore_in: float) -> list[tuple[float | None, float]]:
    """Return ``[(drop_position, fillage_pct), ...]`` through the real solver."""
    out = []
    for drop in SEQUENCE:
        ctx = build_context(drop, pump_bore_in)
        results = DynacardWorkflow(ctx).run_full_analysis()
        out.append((drop, float(results.pump_fillage)))
    return out


#: Tolerance for the monotonicity check, in fillage percentage points.
#: The generator saturates at the low end, so consecutive entries can describe
#: the *same* card and differ only by float rounding (observed: 9.4e-13). A
#: plateau is not a violation; a rise is.
MONOTONIC_TOL_PCT = 1e-6


def find_monotonicity_break(series: list[tuple[float | None, float]]) -> int | None:
    """Index where reported fillage *rises* as the pump empties further.

    Physically impossible: a pump that fills less cannot report more fillage.
    Any break here is an artefact of the analysis, not a property of the well,
    and everything at or below it must be treated as untrustworthy.

    This check found a real defect on 2026-08-05 — see the module docstring.
    It is kept in place as a standing assertion rather than deleted with the
    bug, because it costs nothing and it is the only thing that would catch a
    regression of the same shape.

    The full-pump anchor (index 0) is skipped, because the step from the
    normal-card model to the pounding-card model is a change of generator and
    a drop there carries no information about monotonicity.
    """
    for i in range(2, len(series)):
        if series[i][1] > series[i - 1][1] + MONOTONIC_TOL_PCT:
            return i
    return None


def main() -> None:
    print(__doc__.split("Run\n---")[0].strip())
    print()
    print("=" * 72)
    print("STATED by the operator (verbatim from his posts):")
    for k, v in STATED.items():
        print(f"  {k:24s} {v}")
    print("Context given, used as a cross-check not as solver input:")
    for k, v in STATED_CONTEXT.items():
        print(f"  {k:24s} {v}")
    print("ASSUMED by us — a question, not an answer:")
    for k, v in ASSUMED.items():
        print(f"  {k:24s} {v}")
    print("=" * 72)

    bore = STATED["pump_bore_in"]
    series = run_sequence(bore)

    print(f"\nPump-off progression, {bore}\" bore  (solver: everitt_jennings)")
    print(f"{'transfer point':>15} {'fillage %':>10}   note")
    break_idx = find_monotonicity_break(series)
    for i, (drop, fillage) in enumerate(series):
        flag = ""
        if break_idx is not None and i >= break_idx:
            flag = "  <-- UNTRUSTWORTHY (see below)"
        elif drop is None:
            flag = "  full-pump anchor (normal card)"
        label = "full" if drop is None else f"{drop:.2f}"
        print(f"{label:>15} {fillage:>10.2f}{flag}")

    # --- the honest part ---------------------------------------------------
    if break_idx is not None:
        bad_drop, bad_fill = series[break_idx]
        good_drop, good_fill = series[break_idx - 1]
        print(
            f"\n*** ANALYSIS LIMIT FOUND ***\n"
            f"    Reported fillage RISES from {good_fill:.1f}% to {bad_fill:.1f}% as the\n"
            f"    pump empties further (transfer {good_drop:.2f} -> {bad_drop:.2f}).\n"
            f"    A pump that fills less cannot fill more, so this is an artefact of\n"
            f"    the analysis, not a property of the well. Everything at or below\n"
            f"    this point is untrustworthy. DO NOT SHIP A SETPOINT.\n"
        )
        trustworthy = series[:break_idx]
    else:
        print(
            "\nMonotonicity check: PASSED.\n"
            "    Reported fillage falls without reversal all the way to the most\n"
            "    severely pumped-off card, so the whole range is usable.\n"
            "\n"
            "    This check is not decoration. On 2026-08-05 it failed here: fillage\n"
            "    fell to 18.8% and then jumped to 99.590% and pinned there — a fully\n"
            "    pounded-off pump reported as full. Cause was corner detection\n"
            "    picking a bottom-right corner at the top of the downstroke, where no\n"
            "    fluid load had transferred, so net stroke collapsed onto gross\n"
            "    stroke. Fixed by rejecting any BR corner at which less than 35% of\n"
            "    the downstroke's load drop has occurred; the four vendor-analysed\n"
            "    reference wells realise 0.675-0.866 and are untouched.\n"
            "    See tests/marine_ops/artificial_lift/dynacard/test_corners.py\n"
            "    ::TestBottomRightCornerUnderSeverePumpOff\n"
        )
        trustworthy = series

    # --- setpoint, from the validated region only ---------------------------
    print("Setpoint guidance")
    print("-" * 50)
    full = series[0][1]
    pounding = [f for d, f in trustworthy if d is not None]
    print(f"  full pump reads         : {full:.1f}%")
    print(f"  pounding cards resolve  : {min(pounding):.1f}% – {max(pounding):.1f}%")
    print(
        "\n  The community rule of thumb for this well class is a shutdown between\n"
        "  65% and 75% fillage. Read that against the numbers above before using it:\n"
        f"  a full pump here reports {full:.1f}%, not 100%. A threshold expressed as a\n"
        "  percentage of a nominal full barrel is NOT the same number as a threshold\n"
        "  expressed against what this toolchain reports. Calibrating the two against\n"
        "  the operator's own full-pump card is a prerequisite, not a detail —\n"
        f"  a 75% setpoint applied to a scale that tops out at {full:.0f}% leaves almost no\n"
        "  headroom, and would have the unit shutting down on a healthy pump."
    )

    # --- data ask -----------------------------------------------------------
    print("\nWhat we need to turn this into a real answer")
    print("-" * 50)
    for i, q in enumerate(
        [
            "The raw card file (any export), not a photo of the app screen.",
            "Gearbox make/model and rating. The C-66 is the prime mover, so we "
            "still have no gearbox rating and cannot do torque or counterbalance.",
            "Is the pump-off well the same well as the 41\"/4300 ft/6.4 SPM one? "
            "Both mention a C-66, but that does not make them the same well.",
            "Rod string is 4200 ft and the pump is at 4300 ft — what is the "
            "missing 100 ft?",
            "Whether the dyno is accelerometer-based (this changes how much of the "
            "card's start-of-stroke corner can be trusted at all).",
            "A full-pump card from this well, to calibrate what 100% actually "
            "reads as on this scale before any setpoint is set.",
        ],
        start=1,
    ):
        print(f"  {i}. {q}")


if __name__ == "__main__":
    main()
