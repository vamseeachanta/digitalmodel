"""Check that an OrcaFlex .sim is a completed simulation with a Line exposing strength result variables.

Usage:  uv run python scripts/validate_orcaflex_sim.py <path-to-.sim>
Prints VALIDATE: PASS or VALIDATE: FAIL with reasons. Read-only on the .sim.
Requires a licensed OrcFxAPI install.

Only ``SimulationStopped`` (with ``simulationComplete`` true where the API
exposes it) counts as a completed simulation. Paused, unstable, reset,
statics-only, and still-running states are rejected with a non-zero exit:
they can expose partial range-graph data that must not be post-processed as a
finished run. Mirrors ``digitalmodel.solvers.orcaflex.run_state``.
"""
from __future__ import annotations

import sys

#: The only state in which a dynamic simulation has completed normally.
COMPLETE_STATE = "SimulationStopped"

#: Why each non-complete state is rejected, for the operator reading the output.
REJECT_REASONS = {
    "SimulationStoppedUnstable": "the simulation went unstable",
    "SimulationPaused": "the simulation is paused, not finished",
    "RunningSimulation": "the simulation is still running",
    "Reset": "the model is reset; no simulation has been run",
    "CalculatingStatics": "statics are still being calculated; no simulation has been run",
    "InStaticState": "only statics were solved; no dynamic simulation has been run",
}

STRENGTH_VARS = ("Effective Tension", "Wall Tension", "Bend Moment", "Curvature", "Bend Radius")


def state_name(state) -> str:
    """Resolve a ModelState member to its NAME (``str()`` of an IntEnum is its integer)."""
    for attribute in ("name", "_name_"):
        value = getattr(state, attribute, None)
        if isinstance(value, str) and value:
            return value
    return str(state)


def completion_failure(model) -> str | None:
    """Return a failure reason, or ``None`` when the simulation genuinely completed."""
    name = state_name(getattr(model, "state", None))
    if name != COMPLETE_STATE:
        why = REJECT_REASONS.get(name, "the simulation did not complete")
        return f"state {name} is not a completed simulation ({why})"
    complete = getattr(model, "simulationComplete", True)
    if complete is not True:
        return f"state {name} but simulationComplete is {complete!r}"
    return None


def sample_line(api, line) -> dict:
    """Whole-simulation range-graph extremes for each strength variable on one Line."""
    period = api.Period(api.pnWholeSimulation)
    sample = {}
    for var in STRENGTH_VARS:
        try:
            rg = line.RangeGraph(var, period)
            sample[var] = (round(min(rg.Min), 4), round(max(rg.Max), 4))
        except Exception as exc:
            sample[var] = f"ERR {type(exc).__name__}: {exc}"
    return sample


def main(argv: list[str]) -> int:
    path = argv[1] if len(argv) > 1 else None
    if not path:
        print("VALIDATE: FAIL  (no .sim path given)")
        return 2

    try:
        import OrcFxAPI
    except Exception as exc:  # license / install / VPN
        print(f"VALIDATE: FAIL  import OrcFxAPI failed: {type(exc).__name__}: {exc}")
        return 1

    print(f"OrcFxAPI import: ok  (DLL version {OrcFxAPI.DLLVersion()})")

    try:
        model = OrcFxAPI.Model(path)
    except Exception as exc:
        print(f"VALIDATE: FAIL  could not load .sim: {type(exc).__name__}: {exc}")
        return 1

    print(f"model.state: {state_name(getattr(model, 'state', None))}")

    failure = completion_failure(model)
    if failure:
        print(f"VALIDATE: FAIL  ({failure})")
        return 1

    lines = [o for o in model.objects if getattr(o, "typeName", "") == "Line"]
    print(f"Line objects: {[o.name for o in lines]}")
    if not lines:
        print("VALIDATE: FAIL  (no Line object present)")
        return 1

    # Pass if any Line exposes every strength variable; report each Line checked.
    for line in lines:
        sample = sample_line(OrcFxAPI, line)
        print(f"range-graph sample for {line.name} (min over Min, max over Max):")
        for k, v in sample.items():
            print(f"   {k:18s} {v}")
        if not any(isinstance(v, str) for v in sample.values()):
            print("VALIDATE: PASS")
            return 0

    print("VALIDATE: FAIL  (no Line exposes all strength variables)")
    return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
