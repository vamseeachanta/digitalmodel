"""Hang-off and running configurations of the drilling-riser global model.

The riser is disconnected at the LMRP connector and hangs off the vessel (API RP 16Q / ISO 13624-1 survival modes):

* **hard** - the telescopic joint is collapsed and locked (the slip-joint constraint is fixed along z), the tensioners
  are disconnected, and the whole string hangs on the vessel at the upper flex joint (the spider / gimbal stand-in):
  vessel heave passes straight into the string.
* **soft** - the string hangs on the tensioners with the telescopic joint stroking. The tensioner system is modelled
  as one vertical gas spring at the tension ring (a spring/damper link anchored far above the ring in the vessel
  frame, so its force stays vertical), set to carry the hung weight at the static (mid-stroke) ring position; its
  stiffness is ``stiffness_fraction`` x the hung weight over ``half_stroke_m`` (class-typical, ASSUMED).

``with_lmrp`` keeps the LMRP (the top stack body) hanging free below the lower flex joint; without it the string ends
at the riser adaptor (the lower flex-joint body goes with the LMRP). The running string is the hard-hung string trimmed
to a deployed length with its payload (BOP + LMRP or LMRP only): the upper joints, run last, are the ones left out.
No foundation is modelled (the conductor stays on the well).
"""

from __future__ import annotations

from typing import Literal

from .build import CONN_KEY, HANG_OFF_SPRING as SPRING, SPRING_ANCHOR_HEIGHT_M  # noqa: F401  (re-exported)
from .hand_checks import effective_tension_chain, ring_weight_n, submerged_length_m
from .spec import LineSection, RiserGlobalModelSpec

G = 9.80665
LFJ_BODY = "LFJ upper body"
DEFAULT_STIFFNESS_FRACTION = 0.10  # ASSUMED: tension change over the usable half-stroke, fraction of the hung weight
DEFAULT_HALF_STROKE_M = 7.12  # usable tensioner stroke about mid-stroke (design data D-86 class value)
PAYLOADS = {"BOP + LMRP": ("LMRP", "BOP"), "LMRP only": ("LMRP",)}


def section_weight_n(s: LineSection, *, top_z_m: float, rho_water: float, rho_contents: float,
                     in_air: bool = False) -> float:
    """Weight of a vertical section with its bore contents (in water unless ``in_air``)."""
    import math

    mass = s.mass_per_m_kg + rho_contents * math.pi / 4 * s.bore_id_m ** 2
    wet = 0.0 if in_air else submerged_length_m(top_z_m, s.length_m)
    return (mass * s.length_m - rho_water * s.displaced_volume_per_m_m3 * wet) * G


def _hanging(spec: RiserGlobalModelSpec) -> list[LineSection]:
    return [*spec.riser, *(spec.stack if spec.hang_off is None or spec.hang_off.with_lmrp else [])]


def hung_weight_n(spec: RiserGlobalModelSpec) -> float:
    """Weight hung below the telescopic joint: the ring, the riser and (with the LMRP / payload) the stack bodies."""
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    chain = effective_tension_chain(_hanging(spec), top_z_m=spec.tension_ring.z_static_m, top_tension_n=0.0,
                                    rho_water=rho_w, rho_contents=rho_c)
    return ring_weight_n(spec) - chain[-1]["te_bottom_n"]


def _close(d: dict, stack: list[dict]) -> dict:
    """Stack below the (possibly moved) lower flex joint; the datum becomes the free bottom of the string."""
    d["stack"] = stack
    d["foundation"] = None
    z_lfj = d["upper_flex_joint"]["pivot_z_m"] - sum(s["length_m"] for s in d["inner_barrel"]) \
        - sum(s["length_m"] for s in d["riser"])
    d["lower_flex_joint"]["pivot_z_m"] = z_lfj
    d["wellhead_datum_z_m"] = z_lfj - sum(s["length_m"] for s in stack)
    return d


def hang_off_spec(spec: RiserGlobalModelSpec, mode: Literal["hard", "soft"], *, with_lmrp: bool = True,
                  stiffness_fraction: float = DEFAULT_STIFFNESS_FRACTION,
                  half_stroke_m: float = DEFAULT_HALF_STROKE_M) -> RiserGlobalModelSpec:
    """The hung-off string of ``spec`` (contents as given: the caller sets seawater for a displaced riser)."""
    if mode not in ("hard", "soft"):
        raise ValueError(f"hang-off mode must be 'hard' or 'soft', got {mode!r}")
    d = spec.model_dump()
    lmrp = d["stack"][0]
    if not with_lmrp:
        d["riser"] = [s for s in d["riser"] if s["name"] != LFJ_BODY]
    d = _close(d, [lmrp])
    d["hang_off"] = {"mode": "hard", "with_lmrp": with_lmrp}  # the hung weight does not depend on the mode
    h = type(spec).model_validate(d)
    if mode == "soft":
        w = hung_weight_n(h)
        d["hang_off"].update(mode="soft", spring_tension_n=w, spring_stiffness_n_per_m=stiffness_fraction * w / half_stroke_m,
                             stiffness_basis=f"ASSUMED: {stiffness_fraction:g} x hung weight over the "
                                             f"{half_stroke_m:g} m usable half-stroke")
        h = type(spec).model_validate(d)
    return h


def running_spec(spec: RiserGlobalModelSpec, *, deployed_pct_wd: float, payload: str) -> RiserGlobalModelSpec:
    """The running string hung off at the spider: ``deployed_pct_wd`` % of the riser joints (between the outer barrel
    and the lower flex-joint body) are run - the lowest first, so the upper joints are left out - with the payload
    hanging below the lower flex joint; the telescopic joint and ring stand in for the running tool (hard hang-off,
    ASSUMED). At 100 % the payload hangs at its connected elevation."""
    if payload not in PAYLOADS:
        raise ValueError(f"payload must be one of {sorted(PAYLOADS)}")
    if not 0 < deployed_pct_wd <= 100:
        raise ValueError(f"deployed length {deployed_pct_wd} % WD must be in (0, 100]")
    d = spec.model_dump()
    stack = [next(s for s in d["stack"] if s["name"] == n) for n in PAYLOADS[payload]]
    first, mid, last = d["riser"][0], d["riser"][1:], []
    if mid and mid[-1]["name"] == LFJ_BODY:
        mid, last = mid[:-1], [mid[-1]]
    cut = (1.0 - deployed_pct_wd / 100.0) * sum(s["length_m"] for s in mid)
    keep = []
    for s in mid:  # top down: the upper joints are the ones not yet run
        if cut >= s["length_m"] - 1e-9:
            cut -= s["length_m"]
            continue
        if cut > 0:
            s = dict(s, length_m=s["length_m"] - cut)
            cut = 0.0
        keep.append(s)
    d["riser"] = [first, *keep, *last]
    d = _close(d, stack)
    d["hang_off"] = {"mode": "hard", "with_lmrp": True, "running": {"deployed_pct_wd": deployed_pct_wd,
                                                                   "payload": payload}}
    return type(spec).model_validate(d)
