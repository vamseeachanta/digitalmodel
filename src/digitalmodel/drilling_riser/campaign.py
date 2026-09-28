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
``proxy``             TIMING PROXIES only, not design cases:
                      ``{"kind": "drift_off", "speed_change_m_s": v}`` - the vessel accelerates uniformly
                      along ``heading_deg`` over the main stage, reaching ``v`` at its end;
                      ``{"kind": "disconnect", "tension_factor": f}`` - the riser lower end releases at
                      the start of the main stage and the tensioner tension drops to ``f`` x its value
                      (``f`` defaults to 1.02 x (riser effective weight + ring weight) / tensioner
                      vertical force, a crude instantaneous anti-recoil response).

Open-water (C2) riser base specs (``kind: open_water``, :class:`OpenWaterRiserSpec`) take the same params, plus:

``contents_pressure_pa`` bore gauge pressure at the contents reference level (flowing / shut-in / test states)
``edp_release``       ``{"anti_recoil_factor": f}`` - an EDP disconnect case: the EDP / LRP interface releases at
                      the start of the main stage and the tensioner tension steps to ``f`` x the released submerged
                      weight: default ``EDP_ANTI_RECOIL_FACTOR`` 0.98 (owner decision W2B2: the string settles back
                      under control); ``EDP_RELEASE_GATE_FACTOR`` 1.02 only in the release qualification gate.
                      This is a modelled event, not a timing proxy.

Their statics default to ``"direct"`` (no tension ring, so no yawed-ring branch) and are checked by the frame
balance and the rotary vertical reaction.

Use with :func:`digitalmodel.solvers.orcaflex.parallel_runner.run_cases` and
``adapter="digitalmodel.drilling_riser.campaign:ADAPTER"``.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from .global_model.open_water import OpenWaterRiserSpec
from .global_model.spec import RiserGlobalModelSpec

SEED_PCT = -2.0
# C2 EDP disconnect (owner decision W2B2, 2026-09-28): the tension steps to 0.98 x the released submerged weight in the
# design cases (a 1.02 x step lifts the released string past the stroke within about 10 s); 1.02 x only in the release
# qualification gate (momentum / energy check)
EDP_ANTI_RECOIL_FACTOR = 0.98
EDP_RELEASE_GATE_FACTOR = 1.02
STEP_PCT = 0.5
RESIDUAL_REL = 1.0e-3
RING_YAW_MAX_DEG = 1.0
KN = 1000.0


def _spec_class(d: dict):
    return OpenWaterRiserSpec if d.get("kind") == "open_water" else RiserGlobalModelSpec


def load_base_spec(path: str | Path) -> RiserGlobalModelSpec | OpenWaterRiserSpec:
    """A drilling-riser or an open-water (``kind: open_water``) model spec."""
    doc = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
    d = doc["model"] if "model" in doc else doc
    return _spec_class(d).model_validate(d)


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
    if p.get("contents_pressure_pa") is not None:
        d["contents"]["pressure_pa"] = float(p["contents_pressure_pa"])
    d["name"] = f"{base.name}:{case['case_id']}"
    cls = _spec_class(d)
    if p.get("edp_release") is not None:
        if cls is not OpenWaterRiserSpec:
            raise ValueError("edp_release applies to open-water riser specs only")
        from .global_model.open_water import tension_references

        f = float(p["edp_release"].get("anti_recoil_factor", EDP_ANTI_RECOIL_FACTOR))
        d["edp_release"] = {"anti_recoil_tension_n": f * tension_references(cls.model_validate(d))["released_weight_n"]}
    return cls.model_validate(d)


def seeded_statics(model, spec: RiserGlobalModelSpec, *, seed_pct: float = SEED_PCT,
                   step_pct: float = STEP_PCT) -> dict[str, Any]:
    """Statics by continuation from a -2 % WD seed (along x) to the spec's vessel offset, in steps of at
    most ``step_pct`` % WD. From a straight start the conductor-founded model can converge to a non-physical
    branch (ring yawed 180 deg); the seed avoids it, and :func:`physical_state_checks` confirms it."""
    wd = spec.environment.water_depth_m
    x0, y0 = seed_pct / 100.0 * wd, 0.0
    x1, y1 = spec.vessel_offset_m
    n = max(1, math.ceil(math.hypot(x1 - x0, y1 - y0) / (step_pct / 100.0 * wd) - 1e-9))
    for i in range(n + 1):
        f = i / n
        x, y = x0 + f * (x1 - x0), y0 + f * (y1 - y0)
        for name in ("Vessel", "TensionFrame" if isinstance(spec, OpenWaterRiserSpec) else "TensionRing"):
            model[name].InitialX, model[name].InitialY = x, y
        model.CalculateStatics()
        if i < n:
            model.UseCalculatedPositions(True)
    return {"method": "seeded continuation", "steps": n + 1}


def _tensioner_vertical_n(model) -> float:
    from .global_model import orcaflex_run as orun

    return orun.tensioner_vertical_sum_n(model)


def _end_gz_n(model, line: str, end: str) -> float:
    from .global_model import orcaflex_run as orun

    ofx = orun._api()
    return model[line].StaticResult("End GZ force", ofx.oeEndA if end == "A" else ofx.oeEndB) * KN


def physical_state_checks(model, spec: RiserGlobalModelSpec) -> dict[str, Any]:
    """The W2 milestone-4 checks of the static state, run on every case (raises ``CaseFailed``, status
    ``nonphysical_static``, never retried). The conductor-founded model has a non-physical branch: the ring
    yawed 180 deg with each tensioner line crossed to the opposite attachment (tensioner vertical 5,682 kN
    against 6,026 kN, LMRP-base tension 758 kN against 1,102 kN). Accepted only with

    * tension-ring yaw within 1 deg;
    * tensioner vertical sum (global winch end points) within 1e-3 of the target (``lines`` tensioners);
    * ring vertical balance within 1e-3 of the target: tensioner vertical - ring weight + global vertical end
      force of the riser at end A - that of the inner barrel at end B. The global end forces keep the balance
      exact at an offset, where the effective-tension form of ``ring_vertical_residual_n`` is not.
    """
    from digitalmodel.solvers.orcaflex.parallel_runner import CaseFailed

    from .global_model.hand_checks import tension_references

    if isinstance(spec, OpenWaterRiserSpec):
        from .global_model import orcaflex_run as orun

        chk = orun.open_water_physical_checks(model, spec, rel_tol=RESIDUAL_REL)
        if not chk["physical"]:
            raise CaseFailed("nonphysical_static", f"non-physical static state: frame balance "
                             f"{chk['frame_balance_n'] / KN:.1f} kN, rotary vertical {chk['rotary_vertical_n'] / KN:.1f} kN")
        return chk
    target = spec.tensioners.total_vertical_tension_n
    tol = RESIDUAL_REL * target
    yaw = float(model["TensionRing"].StaticResult("Rotation 3"))
    yaw = (yaw + 180.0) % 360.0 - 180.0
    out: dict[str, Any] = {"ring_yaw_deg": yaw}
    bad = []
    if abs(yaw) > RING_YAW_MAX_DEG:
        bad.append(f"tension ring yaw {yaw:.2f} deg")
    if spec.tensioners.representation == "lines":
        tv = _tensioner_vertical_n(model)
        bal = (tv - tension_references(spec)["ring_weight_n"] + _end_gz_n(model, "Riser", "A")
               - _end_gz_n(model, "InnerBarrel", "B"))
        out.update(tensioner_vertical_n=tv, ring_balance_n=bal)
        if abs(tv - target) > tol:
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

    def spec(self, case: dict) -> RiserGlobalModelSpec:
        if case["case_id"] not in self._specs:
            self._specs[case["case_id"]] = case_spec(case)
        return self._specs[case["case_id"]]

    def build(self, case: dict, model_dir: Path) -> Path:
        from .global_model.build import write_model

        self._specs.pop(case["case_id"], None)
        return write_model(self.spec(case), model_dir) / "master.yml"

    def prepare(self, model, case: dict) -> None:
        proxy = case.get("params", {}).get("proxy")
        if not proxy:
            return
        spec = self.spec(case)
        if isinstance(spec, OpenWaterRiserSpec) and proxy["kind"] == "disconnect":
            raise ValueError("open-water riser: model the EDP disconnect with params 'edp_release', not a proxy")
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
        # the seeded route exists for the tension-ring yaw branch of the drilling riser; the open-water riser has no
        # ring and converges from its straight start (its default is direct statics)
        default = "direct" if isinstance(spec, OpenWaterRiserSpec) else "seeded"
        if p.get("statics", default) == "seeded":
            info = seeded_statics(model, spec, step_pct=float(p.get("statics_step_pct", STEP_PCT)))
        else:
            model.CalculateStatics()
            info = {"method": "direct"}
        return {**info, **settings, **physical_state_checks(model, spec)}

    def extract(self, model, case: dict) -> dict[str, Any]:
        from .global_model import orcaflex_run as orun

        spec = self.spec(case)
        ofx = orun._api()
        if isinstance(spec, OpenWaterRiserSpec):
            return self._extract_open_water(model, case, spec)
        if case["analysis"] == "statics":
            out: dict[str, Any] = {**orun.static_responses(model, spec), **orun.end_effective_tensions(model),
                                   "ring_z_m": orun.ring_static_z_m(model)}
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
        return out

    @staticmethod
    def _extract_open_water(model, case: dict, spec: OpenWaterRiserSpec) -> dict[str, Any]:
        from .global_model import orcaflex_run as orun

        ofx = orun._api()
        if case["analysis"] == "statics":
            out: dict[str, Any] = orun.open_water_static_responses(model, spec)
            n = case.get("params", {}).get("modal_modes")
            if n:
                out["modes"] = orun.riser_modal_periods(model, n_modes=int(n))
            return out
        out = dict(orun.open_water_governing_responses(model, spec))
        period = ofx.Period(1)
        up, riser = model["Upper"], model["Riser"]
        out["series"] = {
            "te_top_kn": _stats(up.TimeHistory("Effective tension", period, ofx.oeEndA)),
            "te_edp_kn": _stats(riser.TimeHistory("Effective tension", period, ofx.oeEndB)),
            "frame_z_m": _stats(model["TensionFrame"].TimeHistory("Z", period)),
            "edp_z_m": _stats(riser.TimeHistory("Z", period, ofx.oeEndB)),
            "vessel_x_m": _stats(model["Vessel"].TimeHistory("X", period)),
        }
        out["sample_count"] = len(up.TimeHistory("Effective tension", period, ofx.oeEndA))
        return out


ADAPTER = RiserCampaignAdapter()
