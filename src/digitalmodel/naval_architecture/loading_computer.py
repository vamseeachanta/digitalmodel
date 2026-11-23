# ABOUTME: Closed-form loading computer / loadicator for a parametric box hull.
# ABOUTME: Equilibrium, intact + damage stability, and longitudinal strength.
"""Loading computer ("loadicator") — screening tier, license-free.

Given a parametric box / barge hull and a loading condition (a set of weight
items), this module returns:

* **Equilibrium** — mean draft and trim so that buoyancy balances total weight
  and the longitudinal centre of buoyancy lines up under the centre of gravity.
* **Intact stability** — transverse ``GM`` and a wall-sided (Scribanti) ``GZ``
  righting-arm curve, checked against IMO IS Code 2008 criteria.
* **Damage stability** — lost-buoyancy method for one flooded compartment
  (parallel sinkage + the loss of waterplane inertia reduces ``GM``).
* **Longitudinal strength** — the still-water shear-force and bending-moment
  distribution from the weight − buoyancy load curve along the length, reported
  against an allowable.

Everything is closed-form for a rectangular (box) hull, so the capability is
self-contained and testable without an external 3-D mesh. The hydrostatic
relations for a box hull are::

    V (displaced volume)  = W / rho
    T (mean draft)        = V / (L * B)
    KB                    = T / 2
    BM_T (transverse)     = I_T / V = (L * B^3 / 12) / V = B^2 / (12 T)
    BM_L (longitudinal)   = I_L / V = (B * L^3 / 12) / V = L^2 / (12 T)
    KM                    = KB + BM
    GM                    = KM - KG

The GZ curve uses the wall-sided / Scribanti formula, valid up to deck-edge
immersion::

    GZ(phi) = (GM + 0.5 * BM_T * tan^2(phi)) * sin(phi)

This module REUSES the SI-unit primitives in
``digitalmodel.naval_architecture.damage_stability`` for the IMO criteria, GZ
area integration / interpolation, and lost-buoyancy parallel sinkage.

Units are SI throughout: lengths in metres, mass / weight in tonnes (t),
forces reported as tonnes-force (t) and bending moments as tonne-metres (t·m)
(plus kN·m), water density in t/m^3 (1.025 for seawater).

References:
- IMO Resolution MSC.267(85) — International Code on Intact Stability, 2008.
- Lewis (ed.), *Principles of Naval Architecture*, Vol. I (Stability and
  Strength), SNAME — hydrostatics, wall-sided GZ, lost-buoyancy method, and the
  still-water shear-force / bending-moment load-curve integration.
- D.R. Derrett & C.B. Barrass, *Ship Stability for Masters and Mates* —
  box-barge worked relations (BM = B^2/12T, lost buoyancy, trim).
"""

from __future__ import annotations

import json
import math
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

# REUSE: SI-unit stability primitives already in the naval_architecture package.
from digitalmodel.naval_architecture.damage_stability import (
    check_imo_intact_stability,
    gz_area_under_curve,
    interpolate_gz,
    lost_buoyancy_sinkage,
)

RHO_SEAWATER_T_M3 = 1.025  # t/m^3
RHO_FRESHWATER_T_M3 = 1.000  # t/m^3
G_M_S2 = 9.81  # m/s^2 (for tonnes-force -> kN)


# --------------------------------------------------------------------------- #
# Inputs
# --------------------------------------------------------------------------- #
@dataclass
class BoxHull:
    """A rectangular (box / barge) hull form.

    The longitudinal coordinate ``x`` runs from the aft end (x = 0) to the
    forward end (x = length_m). Midship is at length_m / 2.
    """

    length_m: float
    beam_m: float
    depth_m: float

    def __post_init__(self) -> None:
        if min(self.length_m, self.beam_m, self.depth_m) <= 0.0:
            raise ValueError("hull length, beam and depth must be positive")


@dataclass
class WeightItem:
    """A single weight in the loading condition.

    ``lcg_m`` is the longitudinal centre of gravity measured from the aft end
    (x = 0). ``vcg_m`` is the vertical centre of gravity above the keel.
    ``length_m`` is the longitudinal extent over which the weight is spread for
    the longitudinal-strength load curve (defaults to a short block centred on
    ``lcg_m``); use the full hull length for lightship.
    """

    name: str
    weight_t: float
    lcg_m: float
    vcg_m: float
    length_m: float | None = None

    def __post_init__(self) -> None:
        if self.weight_t < 0.0:
            raise ValueError(f"weight_t must be non-negative for {self.name!r}")


@dataclass
class DamageCase:
    """One flooded compartment for the lost-buoyancy method."""

    name: str
    length_m: float  # longitudinal extent of the compartment
    x_center_m: float  # longitudinal centre of the compartment from aft end
    permeability: float = 0.95  # fraction of volume floodable
    breadth_m: float | None = None  # defaults to full beam


# --------------------------------------------------------------------------- #
# Results
# --------------------------------------------------------------------------- #
@dataclass
class Equilibrium:
    displacement_t: float
    volume_m3: float
    draft_m: float
    trim_m: float  # positive = by the bow, negative = by the stern
    trim_direction: str
    lcg_m: float
    lcb_m: float
    kg_m: float
    kb_m: float
    bm_t_m: float  # transverse BM
    bm_l_m: float  # longitudinal BM
    km_m: float
    gm_t_m: float  # transverse metacentric height
    gm_l_m: float  # longitudinal metacentric height


@dataclass
class IntactStability:
    gm_m: float
    angles_deg: list[float]
    gz_m: list[float]
    max_gz_m: float
    angle_max_gz_deg: float
    area_0_30_m_rad: float
    area_0_40_m_rad: float
    imo_check: dict
    screening_status: str


@dataclass
class DamageStability:
    case: str
    sinkage_m: float
    damaged_draft_m: float
    intact_gm_m: float
    damaged_gm_m: float
    lost_waterplane_inertia_m4: float
    gm_reduction_m: float
    screening_status: str
    notes: list[str] = field(default_factory=list)


@dataclass
class LongitudinalStrength:
    n_stations: int
    x_m: list[float]
    weight_per_m_t: list[float]
    buoyancy_per_m_t: list[float]
    shear_force_t: list[float]
    bending_moment_t_m: list[float]
    max_shear_t: float
    x_max_shear_m: float
    max_bending_t_m: float
    x_max_bending_m: float
    max_bending_kn_m: float
    allowable_bending_t_m: float | None
    shear_end_residual_t: float  # should be ~0 in equilibrium
    moment_end_residual_t_m: float  # should be ~0 in equilibrium
    screening_status: str


# --------------------------------------------------------------------------- #
# Core calculations
# --------------------------------------------------------------------------- #
def total_weight_and_centroids(items: list[WeightItem]) -> tuple[float, float, float]:
    """Total weight, longitudinal CG and vertical CG (KG) of the condition."""
    w = sum(it.weight_t for it in items)
    if w <= 0.0:
        raise ValueError("total weight must be positive")
    lcg = sum(it.weight_t * it.lcg_m for it in items) / w
    kg = sum(it.weight_t * it.vcg_m for it in items) / w
    return w, lcg, kg


def solve_equilibrium(
    hull: BoxHull,
    items: list[WeightItem],
    rho_t_m3: float = RHO_SEAWATER_T_M3,
) -> Equilibrium:
    """Solve floating equilibrium (draft + trim) for a box hull.

    Buoyancy = weight fixes the mean draft; the hull then trims until the
    longitudinal centre of buoyancy lies under the centre of gravity. For a box
    hull the closed forms above apply.
    """
    w, lcg, kg = total_weight_and_centroids(items)
    volume = w / rho_t_m3
    draft = volume / (hull.length_m * hull.beam_m)
    if draft > hull.depth_m:
        raise ValueError(
            f"draft {draft:.3f} m exceeds hull depth {hull.depth_m} m — vessel sinks"
        )

    kb = draft / 2.0
    bm_t = hull.beam_m**2 / (12.0 * draft)
    bm_l = hull.length_m**2 / (12.0 * draft)
    km = kb + bm_t
    gm_t = km - kg
    gm_l = kb + bm_l - kg

    # Trim: LCB at midship for a box on even keel. The trimming lever (LCG-LCB)
    # is resisted by the longitudinal metacentric height. Trim over the length:
    #   tan(theta) = (LCG - LCB) / GM_L ;  trim = L * tan(theta)
    lcb = hull.length_m / 2.0
    trim = hull.length_m * (lcg - lcb) / gm_l
    if abs(trim) < 1e-9:
        direction = "even keel"
    elif trim > 0:
        direction = "by the bow"
    else:
        direction = "by the stern"

    return Equilibrium(
        displacement_t=w,
        volume_m3=volume,
        draft_m=draft,
        trim_m=trim,
        trim_direction=direction,
        lcg_m=lcg,
        lcb_m=lcb,
        kg_m=kg,
        kb_m=kb,
        bm_t_m=bm_t,
        bm_l_m=bm_l,
        km_m=km,
        gm_t_m=gm_t,
        gm_l_m=gm_l,
    )


def gz_wall_sided(gm_m: float, bm_t_m: float, heel_deg: float) -> float:
    """Wall-sided (Scribanti) righting arm.

    GZ = (GM + 0.5 * BM_T * tan^2(phi)) * sin(phi)

    Valid until the deck edge immerses or the bilge emerges; adequate for a
    box-hull screening curve. See PNA Vol. I, statical stability at large angles.
    """
    phi = math.radians(heel_deg)
    return (gm_m + 0.5 * bm_t_m * math.tan(phi) ** 2) * math.sin(phi)


def intact_stability(
    eq: Equilibrium,
    max_heel_deg: float = 60.0,
    step_deg: float = 5.0,
) -> IntactStability:
    """Build the GZ curve and check IMO IS Code 2008 intact criteria."""
    n = int(round(max_heel_deg / step_deg))
    angles = [i * step_deg for i in range(n + 1)]
    gz = [gz_wall_sided(eq.gm_t_m, eq.bm_t_m, a) for a in angles]

    max_gz = max(gz)
    angle_max = angles[gz.index(max_gz)]
    area_30 = gz_area_under_curve(angles, gz, 30.0)
    area_40 = gz_area_under_curve(angles, gz, 40.0)
    imo = check_imo_intact_stability(angles, gz, eq.gm_t_m)

    return IntactStability(
        gm_m=eq.gm_t_m,
        angles_deg=angles,
        gz_m=gz,
        max_gz_m=max_gz,
        angle_max_gz_deg=angle_max,
        area_0_30_m_rad=area_30,
        area_0_40_m_rad=area_40,
        imo_check=imo,
        screening_status="pass" if imo["overall_pass"] else "fail",
    )


def damage_stability(
    hull: BoxHull,
    eq: Equilibrium,
    damage: DamageCase,
    rho_t_m3: float = RHO_SEAWATER_T_M3,
) -> DamageStability:
    """Lost-buoyancy assessment for one flooded compartment.

    Lost-buoyancy method (PNA Vol. I / Derrett & Barrass): the flooded
    compartment is treated as open to the sea, so the volume of buoyancy it used
    to provide is lost and must be regained by parallel sinkage over the intact
    waterplane. The compartment's waterplane no longer contributes to the
    waterplane moment of inertia, so the transverse BM (and hence GM) drops::

        sinkage   = mu * v_compartment / A_wp                 (parallel rise)
        I_lost    = mu * (l * b^3 / 12)                        (own-centroid)
        GM_damage = KB' + (I_T - I_lost)/V - KG

    Displacement and KG are unchanged (lost buoyancy, not added weight).
    """
    breadth = damage.breadth_m if damage.breadth_m is not None else hull.beam_m
    if breadth > hull.beam_m:
        raise ValueError("damaged breadth cannot exceed the hull beam")
    if not 0.0 <= damage.permeability <= 1.0:
        raise ValueError("permeability must be 0.0-1.0")

    a_wp = hull.length_m * hull.beam_m
    # Volume of the compartment up to the intact waterline.
    comp_volume = damage.length_m * breadth * eq.draft_m
    # REUSE: parallel sinkage from the lost-buoyancy helper.
    sinkage = lost_buoyancy_sinkage(comp_volume, damage.permeability, a_wp)
    damaged_draft = eq.draft_m + sinkage

    # Lost waterplane transverse inertia (compartment's own contribution).
    i_intact = hull.length_m * hull.beam_m**3 / 12.0
    i_lost = damage.permeability * (damage.length_m * breadth**3 / 12.0)
    i_damaged = i_intact - i_lost

    kb_damaged = damaged_draft / 2.0
    bm_damaged = i_damaged / eq.volume_m3
    gm_damaged = kb_damaged + bm_damaged - eq.kg_m
    gm_reduction = eq.gm_t_m - gm_damaged

    notes: list[str] = []
    status = "pass"
    if gm_damaged <= 0.0:
        status = "fail"
        notes.append("damaged GM <= 0 — vessel is unstable after flooding")
    elif gm_damaged < 0.05:
        status = "fail"
        notes.append("damaged GM below 0.05 m residual-stability floor")
    if damaged_draft > hull.depth_m:
        status = "fail"
        notes.append("damaged draft exceeds hull depth — downflooding/loss")

    return DamageStability(
        case=damage.name,
        sinkage_m=sinkage,
        damaged_draft_m=damaged_draft,
        intact_gm_m=eq.gm_t_m,
        damaged_gm_m=gm_damaged,
        lost_waterplane_inertia_m4=i_lost,
        gm_reduction_m=gm_reduction,
        screening_status=status,
        notes=notes,
    )


def _cumulative_trapz(y: list[float], x: list[float]) -> list[float]:
    """Cumulative trapezoidal integral, same length as inputs, starting at 0."""
    out = [0.0]
    for i in range(1, len(x)):
        dx = x[i] - x[i - 1]
        out.append(out[-1] + 0.5 * (y[i] + y[i - 1]) * dx)
    return out


def longitudinal_strength(
    hull: BoxHull,
    items: list[WeightItem],
    eq: Equilibrium,
    n_stations: int = 201,
    allowable_bending_t_m: float | None = None,
) -> LongitudinalStrength:
    """Still-water shear-force and bending-moment distribution.

    The weight per unit length is built by smearing each item uniformly over its
    longitudinal extent. The buoyancy per unit length is a linear (trapezoidal)
    distribution whose total equals the displacement and whose centroid equals
    the LCG — i.e. the trimmed buoyancy of a box hull. The net load is
    integrated twice::

        q(x)  = b(x) - w(x)          [t/m]   (load curve)
        SF(x) = integral_0^x q dx    [t]     (shear force)
        BM(x) = integral_0^x SF dx   [t·m]   (bending moment)

    In equilibrium total weight = total buoyancy and the first moments match, so
    both SF and BM vanish at the free ends (PNA Vol. I, longitudinal strength).
    """
    if n_stations < 3:
        raise ValueError("n_stations must be >= 3")

    L = hull.length_m
    x = [i * L / (n_stations - 1) for i in range(n_stations)]

    # --- weight per unit length (smear each item over its extent) ---
    w_per_m = [0.0] * n_stations
    for it in items:
        extent = it.length_m if it.length_m else L / (n_stations - 1)
        x0 = it.lcg_m - extent / 2.0
        x1 = it.lcg_m + extent / 2.0
        # Clip to hull, keep the density so total integrates back to weight.
        x0c, x1c = max(0.0, x0), min(L, x1)
        span = x1c - x0c
        if span <= 0.0:
            # Point falls outside; lump onto nearest station.
            idx = min(range(n_stations), key=lambda k: abs(x[k] - it.lcg_m))
            dx = x[1] - x[0]
            w_per_m[idx] += it.weight_t / dx
            continue
        density = it.weight_t / span  # t/m over the (un-clipped) extent
        for k in range(n_stations):
            if x0c <= x[k] <= x1c:
                w_per_m[k] += density

    # --- buoyancy per unit length: linear (trimmed box waterplane) ---
    # b(x) = a + c*(x - L/2). The continuous closed form is a = W/L and
    # c = 12 W (LCG - L/2) / L^3. We instead pin (a, c) so the *discrete*
    # trapezoidal force and moment of the buoyancy exactly match those of the
    # discretised weight curve — this makes SF and BM vanish at both free ends
    # to floating-point regardless of how the weight blocks fall on the grid.
    phi = [xi - L / 2.0 for xi in x]
    ones = [1.0] * n_stations

    def _double_end(y: list[float]) -> float:
        """End value of the double cumulative-trapezoidal integral (~∫∫y)."""
        return _cumulative_trapz(_cumulative_trapz(y, x), x)[-1]

    A1 = _cumulative_trapz(ones, x)[-1]
    P1 = _cumulative_trapz(phi, x)[-1]
    Sw = _cumulative_trapz(w_per_m, x)[-1]
    A2 = _double_end(ones)
    P2 = _double_end(phi)
    Bw = _double_end(w_per_m)

    det = A1 * P2 - P1 * A2
    if abs(det) < 1e-12:
        # Degenerate grid — fall back to the closed-form trimmed distribution.
        a = eq.displacement_t / L
        c = 12.0 * eq.displacement_t * (eq.lcg_m - L / 2.0) / L**3
    else:
        a = (Sw * P2 - P1 * Bw) / det
        c = (A1 * Bw - Sw * A2) / det
    b_per_m = [a + c * pi for pi in phi]
    # A box waterplane cannot carry negative buoyancy; deep trim would need a
    # non-linear waterplane, out of scope for this screening load curve.

    # --- integrate ---
    q = [b_per_m[k] - w_per_m[k] for k in range(n_stations)]
    sf = _cumulative_trapz(q, x)
    bm = _cumulative_trapz(sf, x)

    max_sf = max(sf, key=abs)
    x_max_sf = x[sf.index(max_sf)]
    max_bm = max(bm, key=abs)
    x_max_bm = x[bm.index(max_bm)]

    status = "pass"
    if allowable_bending_t_m is not None and abs(max_bm) > allowable_bending_t_m:
        status = "fail"

    return LongitudinalStrength(
        n_stations=n_stations,
        x_m=x,
        weight_per_m_t=w_per_m,
        buoyancy_per_m_t=b_per_m,
        shear_force_t=sf,
        bending_moment_t_m=bm,
        max_shear_t=max_sf,
        x_max_shear_m=x_max_sf,
        max_bending_t_m=max_bm,
        x_max_bending_m=x_max_bm,
        max_bending_kn_m=max_bm * G_M_S2,
        allowable_bending_t_m=allowable_bending_t_m,
        shear_end_residual_t=sf[-1],
        moment_end_residual_t_m=bm[-1],
        screening_status=status,
    )


# --------------------------------------------------------------------------- #
# Router
# --------------------------------------------------------------------------- #
def _parse_items(raw: list[dict]) -> list[WeightItem]:
    return [
        WeightItem(
            name=it["name"],
            weight_t=float(it["weight_t"]),
            lcg_m=float(it["lcg_m"]),
            vcg_m=float(it["vcg_m"]),
            length_m=(None if it.get("length_m") is None else float(it["length_m"])),
        )
        for it in raw
    ]


def run_loading_computer(settings: dict) -> dict:
    """Run the full loadicator from a settings dict and return a result dict."""
    hull_cfg = settings["hull"]
    hull = BoxHull(
        length_m=float(hull_cfg["length_m"]),
        beam_m=float(hull_cfg["beam_m"]),
        depth_m=float(hull_cfg["depth_m"]),
    )
    rho = float(settings.get("water_density_t_m3", RHO_SEAWATER_T_M3))
    items = _parse_items(settings["weight_items"])

    eq = solve_equilibrium(hull, items, rho)
    intact = intact_stability(
        eq,
        max_heel_deg=float(settings.get("max_heel_deg", 60.0)),
        step_deg=float(settings.get("heel_step_deg", 5.0)),
    )

    ls_cfg = settings.get("longitudinal_strength", {})
    strength = longitudinal_strength(
        hull,
        items,
        eq,
        n_stations=int(ls_cfg.get("n_stations", 201)),
        allowable_bending_t_m=(
            None
            if ls_cfg.get("allowable_bending_t_m") is None
            else float(ls_cfg["allowable_bending_t_m"])
        ),
    )

    damage_results: list[dict] = []
    for dmg in settings.get("damage_cases", []) or []:
        case = DamageCase(
            name=dmg["name"],
            length_m=float(dmg["length_m"]),
            x_center_m=float(dmg["x_center_m"]),
            permeability=float(dmg.get("permeability", 0.95)),
            breadth_m=(None if dmg.get("breadth_m") is None else float(dmg["breadth_m"])),
        )
        damage_results.append(asdict(damage_stability(hull, eq, case, rho)))

    statuses = [intact.screening_status, strength.screening_status]
    statuses += [d["screening_status"] for d in damage_results]
    overall = "fail" if "fail" in statuses else "pass"

    return {
        "hull": asdict(hull),
        "water_density_t_m3": rho,
        "equilibrium": asdict(eq),
        "intact_stability": asdict(intact),
        "damage_stability": damage_results,
        "longitudinal_strength": asdict(strength),
        "screening_status": overall,
    }


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/loading_computer"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "loading_computer_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path


def router(cfg: dict) -> dict:
    """Workflow entry point reading ``cfg["loading_computer"]``.

    Mirrors the self-contained router pattern used by ``inspection_planning``:
    runs the loadicator, writes a JSON summary, stamps ``screening_status`` on
    both the module payload and the top-level cfg, and returns cfg.
    """
    settings = cfg.get("loading_computer") or {}
    result = run_loading_computer(settings)

    payload = {**settings, "result": result}
    payload["screening_status"] = result["screening_status"]
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["loading_computer"] = payload
    cfg["screening_status"] = result["screening_status"]
    return cfg
