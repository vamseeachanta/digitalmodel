"""Riser campaign cases for the native parallel OrcaFlex runner (W4).

A case is a JSON object (``case_id``, ``analysis`` = ``statics`` | ``dynamics``, ``params``). The
params hold physical, SI values only - the mapping from a load-case matrix row to these values lives
with the (private) matrix, not here:

``base_spec``         path to a model-spec file whose ``model`` key is a :class:`RiserGlobalModelSpec`
``heading_deg``       direction the waves and current travel towards, from global x (vessel heading 0)
``offset_pct_wd``     static vessel offset, % of water depth, along ``heading_deg`` (+ = downstream)
``mud_density_kg_m3`` riser contents density (optional; the base value otherwise)
``top_tension_n``     tensioner total vertical tension (optional)
``current``           ``{"depth_speed_m_s": [[depth, speed], ...]}`` (direction = heading), optional
``regular_wave``      ``{"height_m", "period_s"}`` or ``irregular_wave`` ``{"hs_m", "tp_s", "gamma", "seed"}``
``dynamics``          ``{"time_step_s", "build_up_s", "duration_s"}`` (dynamics only)
``statics``           ``"seeded"`` (continuation from -2 % WD in <= 0.5 % WD steps with the tension-ring
                      vertical balance checked - needed by the conductor-founded model) or ``"direct"``
``statics_max_iterations``, ``statics_damping`` ([min, max]), ``statics_step_pct``
                      solver-path settings for large offsets (optional; the converged state is unchanged)
``modal_modes``       number of transverse riser modes to extract (statics cases), optional
``tensioners_failed`` failed tensioners (API RP 16Q n; the first lines from the first azimuth are removed and the
                      others keep the intact line tension), optional
``tensioner_representation`` ``"lines"`` (base) or ``"vertical_force"`` (equivalent string, no lateral tie)
``section_stiffness_factors`` ``{section name: factor}`` on EI and EA (stack-stiffness sensitivity)
``rao_origin_dx_m``   fore-aft shift of the RAO origin (riser position in the moonpool sensitivity)
``flex_joint_curves`` ``{"upper" | "lower": [[deg, N.m], ...]}`` from (0, 0): replaces a flex-joint curve
                      (flex-joint stiffness sensitivity)
``stack_segment_m``   upper limit on the stack segment lengths (m): the connectors sit on nodes, where the
                      effective tension jumps by half of each adjacent segment's weight (W405)
``proxy``             TIMING PROXIES only, not design cases:
                      ``{"kind": "drift_off", "speed_change_m_s": v}`` - the vessel accelerates uniformly
                      along ``heading_deg`` over the main stage, reaching ``v`` at its end;
                      ``{"kind": "disconnect", "tension_factor": f}`` - the riser lower end releases at
                      the start of the main stage and the tensioner tension drops to ``f`` x its value
                      (``f`` defaults to 1.02 x (riser effective weight + ring weight) / tensioner
                      vertical force, a crude instantaneous anti-recoil response).

Use with :func:`digitalmodel.solvers.orcaflex.parallel_runner.run_cases` and
``adapter="digitalmodel.drilling_riser.campaign:ADAPTER"``.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from .global_model.spec import RiserGlobalModelSpec

SEED_PCT = -2.0
STEP_PCT = 0.5
RESIDUAL_REL = 1.0e-3  # ring vertical balance (equilibrium): 1e-3 of the target
# tensioner vertical sum: a secondary branch detector (the ring yaw is the primary one). The lines are calibrated to
# the target at zero offset in still water; at an offset or in current the sum follows the line geometry (batch-1
# probes: up to -1.3 % at TT-MIN with one tensioner failed, 10-yr loop current, -10 % WD). The non-physical yawed
# branch sits at -5.7 %; 3 % separates the two.
TENSIONER_VERTICAL_REL = 3.0e-2
RING_YAW_MAX_DEG = 1.0
KN = 1000.0
G = 9.80665


def load_base_spec(path: str | Path) -> RiserGlobalModelSpec:
    doc = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
    return RiserGlobalModelSpec.model_validate(doc["model"] if "model" in doc else doc)


def offset_xy_m(spec: RiserGlobalModelSpec, pct_wd: float, heading_deg: float) -> tuple[float, float]:
    r = pct_wd / 100.0 * spec.environment.water_depth_m
    h = math.radians(heading_deg)
    return (r * math.cos(h), r * math.sin(h))


def case_spec(case: dict) -> RiserGlobalModelSpec:
    """The model spec of one case: the base spec with the case's environment, offset and settings."""
    p = case.get("params", {})
    base = load_base_spec(p["base_spec"])
    d = base.model_dump()
    heading = float(p.get("heading_deg", 0.0))
    if p.get("mud_density_kg_m3") is not None:
        d["contents"]["density_kg_m3"] = float(p["mud_density_kg_m3"])
    if p.get("top_tension_n") is not None:
        d["tensioners"]["total_vertical_tension_n"] = float(p["top_tension_n"])
    if p.get("tensioners_failed"):
        d["tensioners"]["failed_count"] = int(p["tensioners_failed"])
    if p.get("tensioner_representation"):
        d["tensioners"]["representation"] = p["tensioner_representation"]
        if d["tensioners"]["representation"] != "lines" and d["tensioners"].get("failed_count"):
            raise ValueError("a failed tensioner needs the 'lines' tensioner representation")
    for name, f in (p.get("section_stiffness_factors") or {}).items():
        hits = [s for key in ("inner_barrel", "riser", "stack") for s in d[key] if s["name"] == name]
        if not hits:
            raise ValueError(f"section_stiffness_factors: no section named {name!r}")
        for s in hits:
            s["ei_nm2"] *= float(f)
            s["ea_n"] *= float(f)
    if p.get("rao_origin_dx_m"):
        if not d.get("vessel_motion"):
            raise ValueError("rao_origin_dx_m needs vessel motion (RAOs) in the base spec")
        o = d["vessel_motion"]["rao_origin_m"]
        d["vessel_motion"]["rao_origin_m"] = (o[0] + float(p["rao_origin_dx_m"]), o[1], o[2])
    for side, curve in (p.get("flex_joint_curves") or {}).items():
        key = {"upper": "upper_flex_joint", "lower": "lower_flex_joint"}[side]
        pts = [(float(a), float(m)) for a, m in curve]
        k0 = pts[1][1] / pts[1][0] * 180.0 / math.pi
        d[key] = {"pivot_z_m": d[key]["pivot_z_m"], "rotational_stiffness_nm_per_rad": k0,
                  "moment_rotation_deg_nm": pts}
    if p.get("stack_segment_m") is not None:
        cap = float(p["stack_segment_m"])
        if not cap > 0:
            raise ValueError(f"stack_segment_m must be > 0, got {cap}")
        for s in d["stack"]:
            s["segment_length_m"] = min(s["segment_length_m"], cap)
    d["vessel_offset_m"] = offset_xy_m(base, float(p.get("offset_pct_wd", 0.0)), heading)
    d["current"] = ({"direction_deg": heading, "depth_speed_m_s": p["current"]["depth_speed_m_s"]}
                    if p.get("current") else None)
    d["regular_wave"] = d["irregular_wave"] = None
    if p.get("regular_wave"):
        d["regular_wave"] = {**p["regular_wave"], "direction_deg": heading}
    if p.get("irregular_wave"):
        d["irregular_wave"] = {**p["irregular_wave"], "direction_deg": heading}
    if case["analysis"] == "dynamics":
        d["dynamics"] = dict(p["dynamics"])
    d["name"] = f"{base.name}:{case['case_id']}"
    return RiserGlobalModelSpec.model_validate(d)


def seeded_statics(model, spec: RiserGlobalModelSpec, *, seed_pct: float = SEED_PCT,
                   step_pct: float = STEP_PCT, start: tuple[float, float] | None = None,
                   target: tuple[float, float] | None = None, solve=None) -> dict[str, Any]:
    """Statics by continuation from a -2 % WD seed (along x) - or from ``start`` - to the spec's vessel offset
    (or ``target``), in steps of at most ``step_pct`` % WD. From a straight start the conductor-founded model can
    converge to a non-physical branch (ring yawed 180 deg); the seed avoids it, and :func:`physical_state_checks`
    confirms it."""
    wd = spec.environment.water_depth_m
    x0, y0 = start if start is not None else (seed_pct / 100.0 * wd, 0.0)
    x1, y1 = target if target is not None else spec.vessel_offset_m
    solve = solve or model.CalculateStatics
    n = math.ceil(math.hypot(x1 - x0, y1 - y0) / (step_pct / 100.0 * wd) - 1e-9)
    if n <= 0:  # start == target: one solve there
        for name in ("Vessel", "TensionRing"):
            model[name].InitialX, model[name].InitialY = x0, y0
        solve()
        return {"method": "seeded continuation", "steps": 1}
    for i in range(n + 1):
        f = i / n
        x, y = x0 + f * (x1 - x0), y0 + f * (y1 - y0)
        for name in ("Vessel", "TensionRing"):
            model[name].InitialX, model[name].InitialY = x, y
        solve()
        if i < n:
            model.UseCalculatedPositions(True)
    return {"method": "seeded continuation", "steps": n + 1}


CURRENT_RAMP = (0.25, 0.5, 0.75, 1.0)
CALIBRATION_TOL = 1.0e-6
# statics paths, tried in order (W4 batch-1 probes, 2026-09-27): the static state is path-independent but the solver
# is not - a full-current start diverges for some model variants, a still-water continuation flips the ring to the
# yawed branch at large offsets for others
STATICS_PATHS = ("current_at_seed", "ramp_at_seed", "ramp_at_target", "fine_steps", "direct_at_target",
                 "aid_current", "tension_ramp", "neighbour_walk", "heading_walk", "mud_walk", "tension_walk")
AID_CURRENT_M_S = 0.05  # solver aid for still-water cases (removed before the final solve)


def statics_paths(*, current: bool) -> list[str]:
    """Statics paths in the order tried, for a case with or without current."""
    # stage A (2026-09-27): at 12.5 ppg whole offsets fail from the straight start on every seeded path while their
    # neighbours converge directly - the direct solve and the neighbour walk come next.
    # W408 (2026-09-28): the batch-1 divergences oscillate in the ring rotation and the inner barrel next to the slip
    # joint from any straight start near the case; a continuation in heading (from a heading that converges) or in the
    # contents density (from heavier contents) reaches the same regular state in 2-10 s per step
    if current:
        return ["current_at_seed", "direct_at_target", "heading_walk", "mud_walk", "neighbour_walk", "tension_ramp",
                "tension_walk", "ramp_at_seed", "ramp_at_target", "fine_steps"]
    # still water: the aiding current first - the plain seeded path fails on every 12.5 ppg variant only after the full
    # iteration budget (stage A, 2026-09-27: about 8 min per case at 57 workers)
    return ["aid_current", "direct_at_target", "neighbour_walk", "tension_ramp", "tension_walk", "mud_walk",
            "current_at_seed", "fine_steps"]


# W408 continuation paths
HEADING_WALK_STARTS_DEG = (90.0, 0.0, 180.0, 135.0)  # tried in order (the case heading is skipped)
HEADING_WALK_STEPS_DEG = (5.0, 2.5)  # the step of the turn: 5 deg for every start heading, then 2.5 deg
MUD_WALK_FACTOR = 1.12  # contents density at the start of the mud walk / case density (about 14.0 / 12.5 ppg)
MUD_WALK_STEPS = 6
MUD_WALK_LINES = ("InnerBarrel", "Riser")  # the lines that carry the bore contents (the stack lumps its own)
TENSION_WALK_START = 1.10  # line tension at the start of the tension walk / case setting
TENSION_WALK_STEP = 0.01


NEIGHBOUR_STARTS_PCT = (1.0, -1.0, 2.0, -2.0, 3.0, -3.0)  # % WD from the case offset, tried in order
WALK_STEP_PCT = 0.25
# a straight-start probe that converges needs about 50-100 iterations (3-6 s); one that fails runs the full budget
PROBE_ITERATIONS = 300


def _probe(model, solve) -> None:
    """A straight-start solve under a reduced iteration cap; on success the positions are kept, the cap restored and
    the state confirmed by one more solve (a data change resets the static state)."""
    g = getattr(model, "general", None)
    if g is None:
        solve()
        return
    cap = g.StaticsMaxIterations
    g.StaticsMaxIterations = min(cap, PROBE_ITERATIONS)
    solve()  # a failure propagates; the reduced cap is undone by the next reload or restored below
    model.UseCalculatedPositions(True)
    g.StaticsMaxIterations = cap
    solve()


def _capped_probe(model, solve) -> None:
    """:func:`_probe` that restores the full iteration budget when the probe fails as well."""
    g = getattr(model, "general", None)
    cap = g.StaticsMaxIterations if g is not None else None
    try:
        _probe(model, solve)
    finally:
        if g is not None:
            g.StaticsMaxIterations = cap


def _place(model, wd: float, pct: float, heading_deg: float) -> None:
    """Vessel and ring at ``pct`` % WD along ``heading_deg``."""
    r, h = pct / 100.0 * wd, math.radians(heading_deg)
    for name in ("Vessel", "TensionRing"):
        model[name].InitialX, model[name].InitialY = r * math.cos(h), r * math.sin(h)


# tension continuation (stage A: TT-MIN with a failed tensioner, LFJ tension about 60 kips at 12.5 ppg, diverged on
# every other path): solve at 1.5 x the case line tension, then step down to it at the case offset
TENSION_RAMP = (1.5, 1.3, 1.15, 1.05, 1.0)


class _PathFailed(RuntimeError):
    pass


_LAST_OK: dict[tuple, str] = {}  # per worker process: (mud density, current, tensioner model) -> last successful path


def _ring_tensioners(model) -> list:
    return [o for o in model.objects if o.typeName == "Winch" and o.name.startswith("Tensioner")]


def set_line_tension(model, tension_n: float) -> None:
    """Set every tensioner line (all winch stages) to ``tension_n``."""
    for w in _ring_tensioners(model):
        for i in range(w.GetDataRowCount("StageValue")):
            w.SetData("StageValue", i, tension_n / KN)


def calibrate_line_tension(model, spec: RiserGlobalModelSpec, *, solve, tol: float = CALIBRATION_TOL,
                           max_iter: int = 6) -> dict[str, Any]:
    """Scale the tensioner line tension until the vertical sum equals the target in the current static state. Run at
    zero offset in still water it gives the tensioner setting (the vertical target at the spaced-out position); at an
    offset or in current the vertical sum then follows the line geometry, as with a real tensioner setting."""
    target = tensioner_vertical_target_n(spec)
    factor, calls = 1.0, 0
    for _ in range(max_iter):
        f = target / _tensioner_vertical_n(model)  # read in the static state
        if abs(f - 1.0) < tol:
            break
        model.UseCalculatedPositions(True)  # before any data change (both reset the model)
        for w in _ring_tensioners(model):
            for i in range(w.GetDataRowCount("StageValue")):
                w.SetData("StageValue", i, w.GetData("StageValue", i) * f)
        factor *= f
        solve()
        calls += 1
    else:
        raise RuntimeError(f"tensioner line tension calibration did not converge ({max_iter} iterations)")
    line = _ring_tensioners(model)[0]
    return {"factor": factor, "statics_calls": calls, "target_n": target,
            "line_tension_n": line.GetData("StageValue", line.GetDataRowCount("StageValue") - 1) * KN}


def _ring_yaw_deg(model) -> float:
    return (float(model["TensionRing"].StaticResult("Rotation 3")) + 180.0) % 360.0 - 180.0


def _statics_path(model, spec: RiserGlobalModelSpec, path: str, *, speed: float, step: float, solve,
                  heading_deg: float = 0.0) -> None:
    wd = spec.environment.water_depth_m
    seed = (SEED_PCT / 100.0 * wd, 0.0)
    env = model.environment

    def ramp():
        for f in CURRENT_RAMP:
            model.UseCalculatedPositions(True)  # before the data change (both reset the model)
            env.RefCurrentSpeed = f * speed
            solve()

    if path == "current_at_seed":
        env.RefCurrentSpeed = speed
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
    elif path == "fine_steps":
        env.RefCurrentSpeed = speed
        seeded_statics(model, spec, step_pct=step / 2.0, start=seed, solve=solve)
    elif path == "ramp_at_seed":
        env.RefCurrentSpeed = 0.0
        seeded_statics(model, spec, step_pct=step, start=seed, target=seed, solve=solve)
        ramp()
        model.UseCalculatedPositions(True)
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
    elif path == "ramp_at_target":
        env.RefCurrentSpeed = 0.0
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
        ramp()
    elif path == "neighbour_walk":
        env.RefCurrentSpeed = speed
        h = math.radians(heading_deg)
        x1, y1 = spec.vessel_offset_m
        p = math.hypot(x1, y1) / wd * 100.0 * (1.0 if (x1 * math.cos(h) + y1 * math.sin(h)) >= 0 else -1.0)

        def place(pct):
            r = pct / 100.0 * wd
            for name in ("Vessel", "TensionRing"):
                model[name].InitialX, model[name].InitialY = r * math.cos(h), r * math.sin(h)

        start = None
        g = getattr(model, "general", None)
        cap = g.StaticsMaxIterations if g is not None else None
        for d in NEIGHBOUR_STARTS_PCT:
            place(p + d)
            try:
                if g is not None:
                    g.StaticsMaxIterations = min(cap, PROBE_ITERATIONS)
                solve()
            except Exception as exc:  # noqa: BLE001 - try the next neighbour (a licence fault propagates)
                if "licen" in str(exc).lower():
                    raise
                continue
            start = p + d
            break
        if start is None:
            raise _PathFailed("no neighbouring offset converged")
        k = max(1, math.ceil(abs(p - start) / WALK_STEP_PCT - 1e-9))
        for i in range(1, k + 1):
            model.UseCalculatedPositions(True)
            if g is not None:
                g.StaticsMaxIterations = cap  # the walk steps keep the full budget
            place(start + (p - start) * i / k)
            solve()
    elif path == "tension_ramp":
        wins = _ring_tensioners(model)
        base = [[w.GetData("StageValue", i) for i in range(w.GetDataRowCount("StageValue"))] for w in wins]

        def set_factor(f):
            for w, rows in zip(wins, base):
                for i, v in enumerate(rows):
                    w.SetData("StageValue", i, v * f)

        env.RefCurrentSpeed = speed
        set_factor(TENSION_RAMP[0])
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
        for f in TENSION_RAMP[1:]:
            model.UseCalculatedPositions(True)  # before the data change (both reset the model)
            set_factor(f)
            solve()
    elif path == "heading_walk":
        x1, y1 = spec.vessel_offset_m
        h = math.radians(heading_deg)
        p = math.hypot(x1, y1) / wd * 100.0 * (1.0 if (x1 * math.cos(h) + y1 * math.sin(h)) >= 0 else -1.0)
        if abs(p) < 1e-9 and not speed:
            raise _PathFailed("the heading walk needs an offset or a current")
        env.RefCurrentSpeed = speed
        errors = []
        # each start heading in turn, first with 5 deg steps then 2.5 deg: a walk can flip the ring to the yawed branch
        # at an intermediate heading (one tensioner failed, W408 re-run), and another start avoids it
        for step_deg in HEADING_WALK_STEPS_DEG:
            for h0 in HEADING_WALK_STARTS_DEG:
                if abs((h0 - heading_deg + 180.0) % 360.0 - 180.0) < 1e-9:
                    continue
                try:
                    env.RefCurrentDirection = h0
                    _place(model, wd, p, h0)
                    _capped_probe(model, solve)
                    turn = (heading_deg - h0 + 180.0) % 360.0 - 180.0
                    k = max(1, math.ceil(abs(turn) / step_deg - 1e-9))
                    for i in range(1, k + 1):
                        model.UseCalculatedPositions(True)  # before the data change (both reset the model)
                        hh = h0 + turn * i / k
                        env.RefCurrentDirection = hh
                        _place(model, wd, p, hh)
                        solve()
                    return
                except Exception as exc:  # noqa: BLE001 - the next start heading (a licence fault propagates)
                    if "licen" in str(exc).lower():
                        raise
                    errors.append(f"{h0:g} deg/{step_deg:g}: {str(exc).strip().splitlines()[-1][:80]}")
        raise _PathFailed("no start heading walked to the case: " + "; ".join(errors))
    elif path == "mud_walk":
        lines = [model[n] for n in MUD_WALK_LINES]
        rho = spec.contents.density_kg_m3 / 1000.0  # te/m^3
        env.RefCurrentSpeed = speed
        x, y = spec.vessel_offset_m
        for name in ("Vessel", "TensionRing"):
            model[name].InitialX, model[name].InitialY = x, y
        for ln in lines:
            ln.ContentsDensity = MUD_WALK_FACTOR * rho
        _capped_probe(model, solve)
        for i in range(1, MUD_WALK_STEPS + 1):
            model.UseCalculatedPositions(True)
            for ln in lines:
                ln.ContentsDensity = rho * (MUD_WALK_FACTOR + (1.0 - MUD_WALK_FACTOR) * i / MUD_WALK_STEPS)
            solve()
    elif path == "tension_walk":
        wins = _ring_tensioners(model)
        base = [[w.GetData("StageValue", i) for i in range(w.GetDataRowCount("StageValue"))] for w in wins]

        def scale(f):
            for w, rows in zip(wins, base):
                for i, v in enumerate(rows):
                    w.SetData("StageValue", i, v * f)

        scale(TENSION_WALK_START)
        # the case offset at the start tension by the seeded continuation (in still water with the aiding current,
        # removed at the end): a straight start at 1.10 x diverged or reached the yawed branch (W408 diagnosis)
        env.RefCurrentSpeed = speed if speed else AID_CURRENT_M_S
        if not speed:
            env.RefCurrentDirection = heading_deg
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
        if not speed:
            model.UseCalculatedPositions(True)
            env.RefCurrentSpeed = 0.0
            solve()
        k = max(1, math.ceil((TENSION_WALK_START - 1.0) / TENSION_WALK_STEP - 1e-9))
        for i in range(1, k + 1):
            model.UseCalculatedPositions(True)
            scale(TENSION_WALK_START + (1.0 - TENSION_WALK_START) * i / k)
            solve()
    elif path == "direct_at_target":
        env.RefCurrentSpeed = speed
        x, y = spec.vessel_offset_m
        for name in ("Vessel", "TensionRing"):
            model[name].InitialX, model[name].InitialY = x, y
        _probe(model, solve)
    elif path == "aid_current":
        env.RefCurrentSpeed = AID_CURRENT_M_S
        env.RefCurrentDirection = heading_deg
        seeded_statics(model, spec, step_pct=step, start=seed, solve=solve)
        model.UseCalculatedPositions(True)
        env.RefCurrentSpeed = 0.0
        solve()
    else:
        raise ValueError(f"unknown statics path {path!r}")


def robust_statics(model, spec: RiserGlobalModelSpec, params: dict, *, reload=None) -> dict[str, Any]:
    """Statics by the first of ``STATICS_PATHS`` that converges with the ring on the physical branch (yaw within
    1 deg after every solve); ``reload()`` restores the loaded model between attempts. Paths:

    * ``current_at_seed`` - the W2 path: full current, continuation from the -2 % WD seed to the case offset in
      <= ``statics_step_pct`` % WD steps;
    * ``ramp_at_seed`` - still water at the seed, current ramped in 25 % steps there, then the continuation;
    * ``ramp_at_target`` - still-water continuation to the case offset, then the current ramp;
    * ``fine_steps`` - as the first with half steps;
    * ``heading_walk`` - a straight start at the case offset along another heading (``HEADING_WALK_STARTS_DEG``),
      then offset and current turned to the case heading in <= 5 deg steps (W408);
    * ``mud_walk`` - a straight start with the bore contents at 1.12 x the case density, stepped down in six steps;
    * ``tension_walk`` - a solve at 1.10 x the line tension at the case offset, stepped down in 1 % steps.

    Without current the ramp paths are skipped. Licence faults propagate (the runner retries them); when every path
    fails the case is ``statics_diverged`` with each path's error. ``calibrate_tension`` (zero-offset still-water
    cases only) then scales the line tension to the vertical target and returns it (the tensioner setting).
    """
    from digitalmodel.solvers.orcaflex.parallel_runner import CaseFailed

    step = float(params.get("statics_step_pct", STEP_PCT))
    speed = float(model.environment.RefCurrentSpeed) if spec.current is not None else 0.0
    n_calls = [0]

    def solve():
        model.CalculateStatics()
        n_calls[0] += 1
        yaw = _ring_yaw_deg(model)
        if abs(yaw) > RING_YAW_MAX_DEG:
            raise _PathFailed(f"ring yaw {yaw:.1f} deg at vessel x {model['Vessel'].InitialX:.2f} m")

    paths = list(params.get("statics_paths") or statics_paths(current=bool(speed)))
    # the path that converges depends on the model variant and a failed attempt costs the full iteration budget:
    # try the last path that succeeded for this variant in this worker first (the static state is path-independent)
    variant = (round(spec.contents.density_kg_m3, 6), bool(speed), spec.tensioners.representation)
    if _LAST_OK.get(variant) in paths:
        paths.insert(0, paths.pop(paths.index(_LAST_OK[variant])))
    attempts: list[dict[str, str]] = []
    chosen = None
    path_start = 0
    for k, path in enumerate(paths):
        if k:
            if reload is None:
                break
            reload()
        path_start = n_calls[0]
        try:
            _statics_path(model, spec, path, speed=speed, step=step, solve=solve,
                          heading_deg=float(params.get("heading_deg", 0.0)))
            chosen = path
            break
        except Exception as exc:  # noqa: BLE001 - a path failure is recorded and the next path tried
            if "licen" in str(exc).lower():
                raise
            attempts.append({"strategy": path, "error": str(exc).strip().splitlines()[-1][:200]})
    if chosen is not None:
        _LAST_OK[variant] = chosen
    if chosen is None:
        raise CaseFailed("statics_diverged", "no statics path converged: "
                                             + "; ".join(f"{a['strategy']}: {a['error']}" for a in attempts))
    info: dict[str, Any] = {"method": "seeded continuation", "strategy": chosen, "attempts": attempts,
                            "steps": n_calls[0] - path_start, "statics_calls": n_calls[0]}
    if params.get("calibrate_tension"):
        if any(abs(v) > 1e-9 for v in spec.vessel_offset_m) or spec.current is not None:
            raise ValueError("calibrate_tension needs a zero-offset, still-water case")
        info["calibration"] = calibrate_line_tension(model, spec, solve=solve)
    return {**info, **physical_state_checks(model, spec)}

def _tensioner_vertical_n(model) -> float:
    from .global_model import orcaflex_run as orun

    return orun.tensioner_vertical_sum_n(model)


def _end_gz_n(model, line: str, end: str) -> float:
    from .global_model import orcaflex_run as orun

    ofx = orun._api()
    return model[line].StaticResult("End GZ force", ofx.oeEndA if end == "A" else ofx.oeEndB) * KN


def _ring_buoyancy_n(model, spec: RiserGlobalModelSpec) -> float:
    """Buoyancy of the wetted part of the tension ring (the ring sets down towards MSL at large offsets)."""
    return spec.environment.water_density_kg_m3 * G * float(model["TensionRing"].StaticResult("Wetted volume"))


def tensioner_vertical_target_n(spec: RiserGlobalModelSpec) -> float:
    """Vertical tensioner force the lines should deliver: the total, less the failed tensioners' share."""
    t = spec.tensioners
    return t.total_vertical_tension_n * (t.count - t.failed_count) / t.count


def physical_state_checks(model, spec: RiserGlobalModelSpec) -> dict[str, Any]:
    """The W2 milestone-4 checks of the static state, run on every case (raises ``CaseFailed``, status
    ``nonphysical_static``, never retried). The conductor-founded model has a non-physical branch: the ring
    yawed 180 deg with each tensioner line crossed to the opposite attachment (tensioner vertical 5,682 kN
    against 6,026 kN, LMRP-base tension 758 kN against 1,102 kN). Accepted only with

    * tension-ring yaw within 1 deg;
    * tensioner vertical sum (global winch end points) within 3 % of the target (``lines`` tensioners; the
      branch sits at -5.7 %, the physical line-geometry change at an offset is a few 1e-3);
    * ring vertical balance within 1e-3 of the target: tensioner vertical - ring weight + buoyancy of the wetted
      ring + global vertical end force of the riser at end A - that of the inner barrel at end B. The global end
      forces keep the balance exact at an offset, where the effective-tension form of ``ring_vertical_residual_n``
      is not.
    """
    from digitalmodel.solvers.orcaflex.parallel_runner import CaseFailed

    from .global_model.hand_checks import tension_references

    target = tensioner_vertical_target_n(spec)
    tol = RESIDUAL_REL * target
    tol_tv = TENSIONER_VERTICAL_REL * target
    yaw = float(model["TensionRing"].StaticResult("Rotation 3"))
    yaw = (yaw + 180.0) % 360.0 - 180.0
    out: dict[str, Any] = {"ring_yaw_deg": yaw}
    bad = []
    if abs(yaw) > RING_YAW_MAX_DEG:
        bad.append(f"tension ring yaw {yaw:.2f} deg")
    if spec.tensioners.representation == "lines":
        tv = _tensioner_vertical_n(model)
        buoy = _ring_buoyancy_n(model, spec)
        bal = (tv - tension_references(spec)["ring_weight_n"] + buoy + _end_gz_n(model, "Riser", "A")
               - _end_gz_n(model, "InnerBarrel", "B"))
        out.update(tensioner_vertical_n=tv, ring_balance_n=bal, ring_buoyancy_n=buoy)
        if abs(tv - target) > tol_tv:
            bad.append(f"tensioner vertical {tv / KN:.1f} kN against {target / KN:.1f} kN")
        if abs(bal) > tol:
            bad.append(f"ring balance {bal / KN:.1f} kN")
    if bad:
        raise CaseFailed("nonphysical_static", "non-physical static state: " + "; ".join(bad))
    return out


def apply_statics_settings(model, params: dict) -> dict[str, Any]:
    """Optional solver-path settings (``statics_max_iterations``, ``statics_damping`` = [min, max]). They change
    the iteration path to the static state, not the converged state; recorded in the results."""
    g = model.general
    out: dict[str, Any] = {}
    if params.get("statics_max_iterations"):
        g.StaticsMaxIterations = int(params["statics_max_iterations"])
        out["statics_max_iterations"] = g.StaticsMaxIterations
    if params.get("statics_damping"):
        lo, hi = (float(x) for x in params["statics_damping"])
        g.StaticsMinDamping, g.StaticsMaxDamping = lo, hi
        out["statics_damping"] = [g.StaticsMinDamping, g.StaticsMaxDamping]
    return out


def _stats(values) -> dict[str, float]:
    v = [float(x) for x in values]
    m = sum(v) / len(v)
    return {"min": min(v), "max": max(v), "mean": m,
            "std": math.sqrt(sum((x - m) ** 2 for x in v) / len(v))}


class RiserCampaignAdapter:
    """Adapter for :mod:`digitalmodel.solvers.orcaflex.parallel_runner`."""

    def __init__(self) -> None:
        self._specs: dict[str, RiserGlobalModelSpec] = {}
        self._masters: dict[str, Path] = {}

    def spec(self, case: dict) -> RiserGlobalModelSpec:
        if case["case_id"] not in self._specs:
            self._specs[case["case_id"]] = case_spec(case)
        return self._specs[case["case_id"]]

    def build(self, case: dict, model_dir: Path) -> Path:
        from .global_model.build import write_model

        self._specs.pop(case["case_id"], None)
        master = write_model(self.spec(case), model_dir) / "master.yml"
        self._masters[case["case_id"]] = master
        return master

    def prepare(self, model, case: dict) -> None:
        t = case.get("params", {}).get("tensioner_line_tension_n")
        if t:
            set_line_tension(model, float(t))  # the calibrated tensioner setting of this model variant
        proxy = case.get("params", {}).get("proxy")
        if not proxy:
            return
        spec = self.spec(case)
        if proxy["kind"] == "drift_off":
            v = model["Vessel"]
            v.PrimaryMotion = "Prescribed"
            last = v.GetDataRowCount("PrescribedMotionMode") - 1
            v.SetData("PrescribedMotionSpeedValue", last, float(proxy["speed_change_m_s"]))
            v.SetData("PrescribedMotionDirectionValue", last, float(case["params"].get("heading_deg", 0.0)))
        elif proxy["kind"] == "disconnect":
            from .global_model.hand_checks import tension_references

            ref = tension_references(spec)
            held = ref["riser_top_n"] - ref["riser_bottom_n"] + ref["ring_weight_n"]  # ring + riser after release
            f = proxy.get("tension_factor") or 1.02 * held / (ref["riser_top_n"] + ref["ring_weight_n"])
            model["Riser"].SetData("ConnectionReleaseStage", 1, 1)  # End B released at the start of stage 1
            for o in model.objects:
                if o.typeName == "Winch":
                    last = o.GetDataRowCount("StageValue") - 1
                    o.SetData("StageValue", last, o.GetData("StageValue", last) * f)
        else:
            raise ValueError(f"unknown proxy {proxy['kind']!r}")

    def statics(self, model, case: dict) -> dict[str, Any]:
        p = case.get("params", {})
        settings = apply_statics_settings(model, p)
        spec = self.spec(case)
        if p.get("statics", "seeded") == "seeded":
            master = self._masters.get(case["case_id"])

            def reload():
                threads = model.threadCount
                model.LoadData(str(master))
                model.threadCount = threads
                self.prepare(model, case)
                apply_statics_settings(model, p)

            return {**robust_statics(model, spec, p, reload=reload if master else None), **settings}
        model.CalculateStatics()
        return {"method": "direct", **settings, **physical_state_checks(model, spec)}

    def extract(self, model, case: dict) -> dict[str, Any]:
        from .global_model import orcaflex_run as orun

        from .global_model import w5_channels

        spec = self.spec(case)
        ofx = orun._api()
        w5 = w5_channels.extract(model, spec, case["analysis"], ofx)
        if case["analysis"] == "statics":
            out: dict[str, Any] = {**orun.static_responses(model, spec), **orun.end_effective_tensions(model),
                                   "ring_z_m": orun.ring_static_z_m(model), "w5": w5}
            n = case.get("params", {}).get("modal_modes")
            if n:
                out["modes"] = orun.riser_modal_periods(model, n_modes=int(n))
            return out
        out = dict(orun.governing_responses(model, spec))
        period = ofx.Period(1)
        ib, riser, ring = model["InnerBarrel"], model["Riser"], model["TensionRing"]
        out["series"] = {
            "ufj_angle_deg": _stats(ib.TimeHistory("Ez-Angle", period, ofx.oeEndA)),
            "lfj_angle_deg": _stats(riser.TimeHistory("Ez-Angle", period, ofx.oeEndB)),
            "te_top_kn": _stats(riser.TimeHistory("Effective tension", period, ofx.oeEndA)),
            "te_bottom_kn": _stats(riser.TimeHistory("Effective tension", period, ofx.oeEndB)),
            "ring_z_m": _stats(ring.TimeHistory("Z", period)),
            "vessel_x_m": _stats(model["Vessel"].TimeHistory("X", period)),
        }
        out["sample_count"] = len(riser.TimeHistory("Effective tension", period, ofx.oeEndA))
        out["w5"] = w5
        return out


ADAPTER = RiserCampaignAdapter()
