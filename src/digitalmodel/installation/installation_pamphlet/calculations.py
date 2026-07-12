"""Pure, deterministic calculations for the installation-vessel pamphlet.

Every function here is a composable "workflow" step. They are deterministic:
no ``datetime.now``, no randomness, no network. The only file IO permitted is
through the explicit ``load_*`` helpers, which take an explicit path argument.

Physics / numbers are NOT invented here — they are produced by validated
digitalmodel engines (``vessel_suitability``, ``operation_envelope``,
``weather_window``, parametric RAO database). This module only orchestrates and
shapes those engine outputs into the pamphlet data structure.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any

# ----------------------------------------------------------------------------
# RAO basis vocabulary (lever upgraded as licensed runs complete)
#   barge      -> initial pessimistic 100 m tender-barge proxy
#   drillship  -> ship-shaped proxy, justified by a completed ship diffraction
#   fpso       -> alternate ship-shaped proxy
#   vessel     -> vessel-specific OrcaWave diffraction. ONLY "actual" once a real
#                 vessel RAO artifact is ingested (load_rao_set_from_artifact);
#                 with no artifact synced it honestly falls back to the
#                 ship-shaped proxy (see _effective_provenance below).
# ----------------------------------------------------------------------------
BASIS_MAP = {
    "barge": "Barge",
    "drillship": "Drillship",
    "fpso": "FPSO",
    "vessel": "Drillship",  # ship-shaped fallback proxy until a real RAO artifact is ingested
}
# Default (no-artifact) provenance per basis. "vessel" defaults to the honest
# ship_proxy fallback; it is upgraded to "actual" only when a real vessel RAO
# artifact is actually loaded (see _effective_provenance).
BASIS_PROVENANCE = {
    "barge": "proxy",
    "drillship": "ship_proxy",
    "fpso": "ship_proxy",
    "vessel": "ship_proxy",
}
BASIS_LABEL = {
    "barge": "Generic 100 m barge parametric RAO (initial proxy — pessimistic)",
    "drillship": (
        "Ship-shaped (drillship) parametric RAO — adapted basis, justified by "
        "completed FPSO diffraction run lr_acma_8de9c018adbd"
    ),
    "fpso": "Ship-shaped (FPSO 250 m) parametric RAO — adapted basis",
    "vessel": "Vessel-specific BokaLift 2 RAO (OrcaWave diffraction)",
}
BASIS_HUMAN = {
    "barge": "100 m barge proxy (pessimistic)",
    "drillship": "ship-shaped drillship proxy (justified by completed FPSO diffraction)",
    "fpso": "ship-shaped FPSO proxy",
    "vessel": "vessel-specific OrcaWave RAO",
}

# Honest fallback when basis resolves to "vessel" but NO real RAO artifact is
# available yet (vessel diffraction may be complete, but the OrcaWave RAO export
# has not been synced to this Linux box). We do NOT claim "actual" — we stay on
# the ship-shaped proxy and say so.
VESSEL_PROXY_NOTE = "vessel diffraction complete; RAO artifact not yet synced"
VESSEL_PROXY_LABEL = (
    "Ship-shaped proxy RAO — vessel diffraction complete; RAO artifact not yet "
    "synced (no vessel-specific OrcaWave RAO ingested)"
)
VESSEL_PROXY_HUMAN = "ship-shaped proxy (vessel diffraction complete; RAO artifact not yet synced)"


def _effective_provenance(rao_basis: str, rao_from_artifact: bool = False) -> str:
    """Honest provenance for a basis. "vessel" is only "actual" when a real RAO
    artifact was actually loaded; otherwise it stays on the ship-shaped proxy."""
    if rao_basis == "vessel":
        return "actual" if rao_from_artifact else "ship_proxy"
    return BASIS_PROVENANCE[rao_basis]


def _effective_label(rao_basis: str, rao_from_artifact: bool = False) -> str:
    """Envelope-basis label, honest about the vessel-without-artifact fallback."""
    if rao_basis == "vessel" and not rao_from_artifact:
        return VESSEL_PROXY_LABEL
    return BASIS_LABEL[rao_basis]


def _effective_human(rao_basis: str, rao_from_artifact: bool = False) -> str:
    """Short human basis note, honest about the vessel-without-artifact fallback."""
    if rao_basis == "vessel" and not rao_from_artifact:
        return VESSEL_PROXY_HUMAN
    return BASIS_HUMAN[rao_basis]

DEFAULT_OPERATIONS = ["transit", "dp_station_keeping", "heavy_lift"]
DEFAULT_TP_GRID_S = [6.0, 8.0, 10.0, 12.0, 14.0, 16.0]

# ----------------------------------------------------------------------------
# Splash-zone (lowering through the wave zone) constants — DNV-RP-H103 (2011)
# Sec 4 "Lifting through wave zone". Seawater density and g are fixed; the
# lowering speed and snap/slack-sling margin are documented screening values.
# ----------------------------------------------------------------------------
SEAWATER_DENSITY = 1025.0  # kg/m3
GRAVITY = 9.80665  # m/s2
SPLASH_V_LOWERING_MS = 0.5  # crane lowering speed through the wave zone (m/s)
SPLASH_SNAP_DAF = 1.30  # snap / slack-sling margin (DNV-RP-H103 splash-zone DAF)


# ---------------------------------------------------------------- loaders
def load_mudmats(path: str | Path) -> dict[str, dict[str, Any]]:
    """Load the mudmat/structure catalog keyed by structure id."""
    with Path(path).open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    return {m["id"]: m for m in data["structures"]}


def load_jumpers(path: str | Path) -> dict[str, dict[str, Any]]:
    """Load the rigid-jumper catalog keyed by jumper id."""
    with Path(path).open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    return {j["id"]: j for j in data["jumpers"]}


def load_structures(path: str | Path) -> dict[str, dict[str, Any]]:
    """Load the subsea production structure catalog (PLET/manifold/SPS) keyed by id.

    Same schema and normalized shape as ``load_mudmats`` — each record carries
    ``mass_properties.mass_air_te`` which the lift assembly consumes.
    """
    with Path(path).open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    return {s["id"]: s for s in data["structures"]}


# ---------------------------------------------------------------- real RAO artifact ingestion
_RAO_COMPONENT_KEYS = ("surge", "sway", "heave", "roll", "pitch", "yaw")


def _extract_rao_dict(data: Any, path: Path) -> dict[str, Any]:
    """Locate the ``RAOSet.to_dict()`` payload inside ``data``.

    Supports two on-disk shapes:
      * a bare ``RAOSet.to_dict()`` — top-level ``raos`` maps to the per-DOF
        component dicts (``surge``/``heave``/...);
      * a ``DiffractionResults.to_dict()`` wrapper — top-level ``raos`` is itself
        a nested ``RAOSet.to_dict()`` (so it carries ``vessel_name`` + an inner
        ``raos``). Any other top-level value that nests a RAOSet is also accepted.
    """
    if not isinstance(data, dict):
        raise ValueError(
            f"installation_pamphlet: RAO artifact {path} is not a JSON object"
        )
    raos = data.get("raos")
    if isinstance(raos, dict):
        # bare RAOSet.to_dict(): data["raos"] holds the per-DOF component dicts
        if any(k in raos for k in _RAO_COMPONENT_KEYS):
            return data
        # DiffractionResults-style wrapper: data["raos"] is itself a RAOSet.to_dict()
        if isinstance(raos.get("raos"), dict) or "vessel_name" in raos:
            return raos
    # last resort: any nested value that itself looks like a RAOSet.to_dict()
    for value in data.values():
        if (
            isinstance(value, dict)
            and isinstance(value.get("raos"), dict)
            and any(k in value["raos"] for k in _RAO_COMPONENT_KEYS)
        ):
            return value
    raise ValueError(
        f"installation_pamphlet: no RAOSet (top-level 'raos' with per-DOF "
        f"components) found in RAO artifact {path}"
    )


def load_rao_set_from_artifact(path: str | Path):
    """Ingest a real vessel RAO artifact (JSON) into an ``RAOSet``.

    This is the license-free Linux-side ingestion path for a vessel-specific
    OrcaWave diffraction result: a licensed host exports the RAOs as JSON
    (``RAOSet.to_dict()`` or a ``DiffractionResults.to_dict()`` wrapper) and this
    rebuilds the ``RAOSet`` — no OrcFxAPI, no heavy binaries. Pure given the file
    bytes (no datetime / random / network). Raises a clear error if the file is
    missing or malformed.
    """
    from digitalmodel.hydrodynamics.diffraction.output_schemas import RAOSet

    p = Path(path)
    if not p.exists():
        raise FileNotFoundError(
            f"installation_pamphlet: RAO artifact not found: {p}"
        )
    try:
        with p.open("r", encoding="utf-8") as stream:
            data = json.load(stream)
    except (json.JSONDecodeError, OSError) as exc:
        raise ValueError(
            f"installation_pamphlet: RAO artifact {p} is not valid JSON: {exc}"
        ) from exc
    rao_dict = _extract_rao_dict(data, p)
    try:
        return RAOSet.from_dict(rao_dict)
    except (KeyError, TypeError, ValueError) as exc:
        raise ValueError(
            f"installation_pamphlet: RAO artifact {p} is malformed "
            f"(could not rebuild RAOSet): {exc}"
        ) from exc


def resolve_rao_artifact(
    rao_basis: str,
    completed_runs: list[dict[str, Any]] | None,
    rao_artifact: str | Path | None = None,
) -> str | None:
    """Resolve the vessel RAO artifact path, or ``None`` if none is available.

    Only the ``"vessel"`` basis ingests a real artifact. An explicit config path
    (``rao_artifact``) wins; otherwise the path is taken from a ``completed_runs``
    entry carrying a ``rao_artifact`` field — preferring the BokaLift run that
    flipped the basis to ``"vessel"``.
    """
    if rao_basis != "vessel":
        return None
    if rao_artifact:
        return str(rao_artifact)
    runs = list(completed_runs or [])
    for r in runs:
        if "bokalift" in (r.get("input") or "").lower() and r.get("rao_artifact"):
            return str(r["rao_artifact"])
    for r in runs:
        if r.get("rao_artifact"):
            return str(r["rao_artifact"])
    return None


# ---------------------------------------------------------------- lift suitability
def assess_lifts(
    vessel_info: dict[str, Any],
    items: list[dict[str, Any]],
    radius_m: float,
    daf: float,
) -> list[dict[str, Any]]:
    """Crane lift suitability per item via the vessel_suitability engine.

    ``vessel_info`` must carry ``"name"`` (the vessel display name used by the
    engine lookup). ``items`` is a list of ``{"item","kind","mass_air_te"}``.
    Returns one row dict per lift item. Deterministic.
    """
    from digitalmodel.marine_ops.installation import vessel_suitability as vs

    vessel_name = vessel_info["name"]
    rows: list[dict[str, Any]] = []
    for it in items:
        te = float(it["mass_air_te"])
        req = vs.LiftRequirement(weight_te=te, radius_m=float(radius_m), daf=float(daf))
        res = vs.assess_named(vessel_name, req)
        swl = getattr(res, "swl_at_radius_te", None)
        rows.append(
            {
                "item": it["item"],
                "kind": it.get("kind"),
                "mass_air_te": te,
                "hook_load_te": round(te * float(daf), 1),
                "swl_at_radius_te": swl,
                "utilization_pct": round(100 * te * float(daf) / swl, 1) if swl else None,
                "defensible": getattr(res, "defensible", None),
                "score": getattr(res, "score", None),
                "limiting": list(getattr(res, "limiting_factors", []) or []),
            }
        )
    return rows


# ---------------------------------------------------------------- envelopes
def _build_rao_set(rao_basis: str, rao_artifact: str | Path | None = None):
    """Construct the RAOSet that drives the operation envelopes for ``rao_basis``.

    Returns ``(rao_set, from_artifact)``. When ``rao_basis == "vessel"`` AND a
    ``rao_artifact`` path is supplied, the REAL ingested vessel RAO drives the
    envelopes (``from_artifact=True``); otherwise the parametric ship/barge proxy
    is built from the validated vessel database (``from_artifact=False``).

    Parametric path ported verbatim from the validated prototype
    ``compute_pamphlet.py``.
    """
    if rao_basis == "vessel" and rao_artifact:
        return load_rao_set_from_artifact(rao_artifact), True

    import numpy as np

    from digitalmodel.orcawave.vessel_database import get_representative_raos
    from digitalmodel.hydrodynamics.diffraction.output_schemas import (
        DOF,
        FrequencyData,
        HeadingData,
        RAOComponent,
        RAOSet,
    )

    vessel_type = BASIS_MAP[rao_basis]
    rao_param = get_representative_raos(vessel_type)
    periods = np.array(rao_param.periods, dtype=float)
    freqs = 2 * math.pi / periods  # rad/s
    heads = np.array(rao_param.headings_deg, dtype=float)
    # engine wants [n_freq x n_head]; parametric stores [n_head x n_freq] -> transpose
    heave = np.array(rao_param.heave_rao).T
    roll = np.array(rao_param.roll_rao).T
    pitch = np.array(rao_param.pitch_rao).T
    zero = np.zeros_like(heave)

    def comp(dof, mag, unit):
        return RAOComponent(
            dof=dof,
            magnitude=mag,
            phase=np.zeros_like(mag),
            frequencies=FrequencyData(
                values=freqs,
                periods=periods,
                count=len(freqs),
                min_freq=float(freqs.min()),
                max_freq=float(freqs.max()),
            ),
            headings=HeadingData(
                values=heads,
                count=len(heads),
                min_heading=float(heads.min()),
                max_heading=float(heads.max()),
            ),
            unit=unit,
        )

    return (
        RAOSet(
            vessel_name=f"Installation vessel ({vessel_type} proxy RAO)",
            analysis_tool="parametric",
            water_depth=1200.0,
            surge=comp(DOF.SURGE, zero.copy(), "m/m"),
            sway=comp(DOF.SWAY, zero.copy(), "m/m"),
            heave=comp(DOF.HEAVE, heave, "m/m"),
            roll=comp(DOF.ROLL, roll, "deg/m"),
            pitch=comp(DOF.PITCH, pitch, "deg/m"),
            yaw=comp(DOF.YAW, zero.copy(), "deg/m"),
            created_date="2026-06-28",
        ),
        False,
    )


def compute_envelopes(
    rao_basis: str,
    operations: list[str],
    tp_grid_s: list[float],
    rao_artifact: str | Path | None = None,
) -> dict[str, Any]:
    """Operation envelopes (limiting Hs vs Tp) for each operation.

    Returns an ordered dict keyed by operation name, each value being
    ``{"alpha", "hs_by_tp", "gov_by_tp"}``. ``hs_by_tp`` / ``gov_by_tp`` keys are
    one-decimal Tp strings (e.g. ``"6.0"``) so JSON round-trips stably.

    When ``rao_basis == "vessel"`` and ``rao_artifact`` is supplied, the envelopes
    are driven by the REAL ingested vessel RAO instead of the parametric proxy.
    """
    from digitalmodel.marine_ops.operation_envelope import operation_envelope

    rset, _from_artifact = _build_rao_set(rao_basis, rao_artifact)
    tp_grid = [float(t) for t in tp_grid_s]
    env_out: dict[str, Any] = {}
    for op in operations:
        env = operation_envelope(rset, op, tp_range_s=tp_grid)
        hs_by_tp = {f"{round(p.tp_s, 1):.1f}": round(p.hs_limit_m, 2) for p in env.points}
        gov_by_tp = {f"{round(p.tp_s, 1):.1f}": p.governing_dof for p in env.points}
        env_out[op] = {
            "alpha": env.alpha,
            "hs_by_tp": hs_by_tp,
            "gov_by_tp": gov_by_tp,
        }
    return env_out


# ---------------------------------------------------------------- splash-zone envelope
def compute_splashzone_envelope(
    structure: dict[str, Any],
    tp_grid_s: list[float],
    v_lowering_ms: float = SPLASH_V_LOWERING_MS,
    snap_daf: float = SPLASH_SNAP_DAF,
) -> dict[str, Any]:
    """Closed-form per-structure splash-zone Hs limit vs Tp (DNV-RP-H103 Sec 4).

    Deterministic, pure (no datetime / random / network). For each peak period
    ``Tp`` it returns the significant wave height ``Hs`` at which the structure
    can no longer be lowered through the wave zone without risking a *snap*
    (slack-sling) load on the hoist wire.

    Wave-zone kinematics (deep-water linear wave theory, DNV-RP-C205 / RP-H103
    Sec 4.6). A characteristic surface vertical water-particle motion scales with
    Hs and Tp as

        v_w = pi * Hs / Tp          (vertical particle velocity amplitude, m/s)
        a_w = 2 * pi^2 * Hs / Tp^2  (vertical particle acceleration amplitude, m/s^2)

    (these are ``omega * zeta_a`` and ``omega^2 * zeta_a`` with omega = 2*pi/Tp and
    a wave amplitude zeta_a = Hs/2). The crane lowers the object at ``v_lowering``,
    so the slamming / drag relative velocity is ``v_rel = v_w + v_lowering``.

    Hydrodynamic forces on the object in the splash zone (DNV-RP-H103 Sec 4.6):

        F_slam   = 0.5 * rho * Cs * A_bottom * v_rel^2        (slamming impact)
        F_drag   = 0.5 * rho * Cd * A_bottom * v_rel^2        (form drag)
        F_inertia= rho * (1 + Ca) * V * a_w                   (Froude-Krylov + added mass)

    with rho = 1025 kg/m^3, ``Cs``/``Cd``/``Ca`` and ``A_bottom``/``V`` taken from the
    structure catalog (``hydrodynamic_coefficients``, ``projected_areas.bottom_m2``,
    ``mass_properties.displaced_volume_m3``).

    Governing (snap / slack-sling) criterion — DNV-RP-H103 Sec 4.6 "snap forces":
    the hoist wire must stay in tension over the whole motion cycle, i.e. the
    characteristic upward hydrodynamic force must not exceed the submerged weight
    reduced by a documented margin::

        F_slam + F_drag + F_inertia  <=  W_sub / snap_daf

    Solving that (a quadratic in Hs, since v_rel is affine in Hs and a_w is linear
    in Hs) gives the limiting ``Hs(Tp)``. The governing term is whichever of slam /
    drag / inertia is largest at that limit.

    A future OrcaFlex dynamic lowering run (licensed installation path) is the
    verification route for these closed-form limits — NOT computed here.
    """
    rho = float(SEAWATER_DENSITY)
    hc = structure["hydrodynamic_coefficients"]
    cs = float(hc["slamming_coefficient_Cs"])
    cd = float(hc["drag_coefficient_Cd"])
    ca = float(hc["added_mass_coefficient_Ca"])
    a_bottom = float(structure["projected_areas"]["bottom_m2"])
    mp = structure["mass_properties"]
    volume = float(mp["displaced_volume_m3"])
    w_sub_n = float(mp["submerged_weight_kn"]) * 1000.0

    v_low = float(v_lowering_ms)
    target = w_sub_n / float(snap_daf)  # max allowable upward hydrodynamic force (N)

    k_qs = 0.5 * rho * (cs + cd) * a_bottom  # slam+drag coefficient (x v_rel^2)
    k_in = rho * (1.0 + ca) * volume  # inertia coefficient (x acceleration)

    hs_by_tp: dict[str, float] = {}
    gov_by_tp: dict[str, str] = {}
    for tp in (float(t) for t in tp_grid_s):
        c1 = math.pi / tp  # v_w = c1 * Hs
        c2 = 2.0 * math.pi**2 / tp**2  # a_w = c2 * Hs
        # k_qs*(c1*Hs + v_low)^2 + k_in*c2*Hs = target  ->  a*Hs^2 + b*Hs + c = 0
        a = k_qs * c1**2
        b = 2.0 * k_qs * c1 * v_low + k_in * c2
        c = k_qs * v_low**2 - target
        disc = b * b - 4.0 * a * c
        if a <= 0.0 or disc < 0.0:
            hs = 0.0
        else:
            hs = max((-b + math.sqrt(disc)) / (2.0 * a), 0.0)
        v_rel = c1 * hs + v_low
        a_w = c2 * hs
        f_slam = 0.5 * rho * cs * a_bottom * v_rel**2
        f_drag = 0.5 * rho * cd * a_bottom * v_rel**2
        f_iner = k_in * a_w
        gov = max(
            ((f_slam, "slam"), (f_drag, "drag"), (f_iner, "inertia")),
            key=lambda x: x[0],
        )[1]
        key = f"{round(tp, 1):.1f}"
        hs_by_tp[key] = round(hs, 2)
        gov_by_tp[key] = gov

    return {
        "id": structure.get("id"),
        "name": structure.get("name"),
        "submerged_weight_kn": round(w_sub_n / 1000.0, 2),
        "bottom_area_m2": a_bottom,
        "daf": float(snap_daf),
        "v_lowering_ms": v_low,
        "hs_by_tp": hs_by_tp,
        "gov_by_tp": gov_by_tp,
    }


def compute_splashzone_set(
    lifts: list[dict[str, Any]],
    tp_grid_s: list[float],
    heavy_lift_env: dict[str, Any] | None = None,
) -> list[dict[str, Any]]:
    """Per-structure splash-zone envelopes for every lift carrying hydro data.

    Each lift item may carry a ``catalog_record`` (the full mudmat / structure
    catalog dict). Items that expose ``hydrodynamic_coefficients`` +
    ``projected_areas`` + ``mass_properties.displaced_volume_m3`` get a closed-form
    splash-zone envelope. When ``heavy_lift_env`` (the vessel-motion heavy-lift
    envelope from :func:`compute_envelopes`) is supplied, each structure also gets
    a ``combined_hs_by_tp`` = min(vessel heavy-lift limit, structure splash-zone
    limit) — the governing lowering limit. Deterministic.
    """
    out: list[dict[str, Any]] = []
    hl = (heavy_lift_env or {}).get("hs_by_tp") if heavy_lift_env else None
    for it in lifts:
        rec = it.get("catalog_record")
        if not isinstance(rec, dict):
            continue
        if "hydrodynamic_coefficients" not in rec or "projected_areas" not in rec:
            continue
        if "displaced_volume_m3" not in (rec.get("mass_properties") or {}):
            continue
        env = compute_splashzone_envelope(rec, tp_grid_s)
        env["item"] = it.get("item")
        env["kind"] = it.get("kind")
        if hl:
            combined: dict[str, float] = {}
            for key, hs in env["hs_by_tp"].items():
                v = hl.get(key)
                combined[key] = round(min(hs, v), 2) if v is not None else hs
            env["combined_hs_by_tp"] = combined
        out.append(env)
    return out


# ---------------------------------------------------------------- operability
def compute_operability(hs_scatter_limits: list[float]) -> list[dict[str, Any]]:
    """Weather-window operability table (% time below each Hs limit)."""
    from digitalmodel.orcaflex.weather_window import ScatterDiagram, OperabilityTable

    limits = [float(h) for h in hs_scatter_limits]
    return OperabilityTable(hs_limits=limits, scatter=ScatterDiagram()).generate_table()


# ---------------------------------------------------------------- RAO basis selection
def _is_diffraction(run: dict[str, Any]) -> bool:
    wf = (run.get("workflow") or "").lower()
    inp = (run.get("input") or "").lower()
    return (
        "diffraction" in wf
        or "diffraction" in inp
        or "orcawave" in wf
        or "orcawave" in inp
    )


def select_rao_basis(completed_runs: list[dict[str, Any]]) -> str:
    """Pure RAO-basis selection from a list of completed-run dicts.

    Rules (ported from the prototype adapt.py):
      * any run input contains "bokalift"                       -> "vessel"
      * a diffraction/orcawave run input contains "fpso"/"ship" -> "drillship"
      * any diffraction/orcawave run exists                     -> "drillship"
      * otherwise                                               -> "barge"
    """
    runs = list(completed_runs or [])
    diffraction = [r for r in runs if _is_diffraction(r)]
    if any("bokalift" in (r.get("input") or "").lower() for r in runs):
        return "vessel"
    if any(
        ("fpso" in (r.get("input") or "").lower() or "ship" in (r.get("input") or "").lower())
        for r in diffraction
    ):
        return "drillship"
    if diffraction:
        return "drillship"
    return "barge"


def select_basis_run_id(
    completed_runs: list[dict[str, Any]],
    rao_basis: str,
) -> str | None:
    """Run id that justifies the selected basis (None for the barge default)."""
    runs = list(completed_runs or [])
    diffraction = [r for r in runs if _is_diffraction(r)]
    if rao_basis == "vessel":
        for r in runs:
            if "bokalift" in (r.get("input") or "").lower():
                return r.get("run_id")
        return None
    if rao_basis in ("drillship", "fpso"):
        for r in diffraction:
            inp = (r.get("input") or "").lower()
            if "fpso" in inp or "ship" in inp:
                return r.get("run_id")
        if diffraction:
            return diffraction[0].get("run_id")
    return None


# ---------------------------------------------------------------- provenance manifest
def build_provenance(
    completed_runs: list[dict[str, Any]],
    rao_basis: str,
    vessel_name: str = "BokaLift 2",
    structure_catalog_used: bool = False,
    splashzone_computed: bool = False,
    rao_from_artifact: bool = False,
) -> dict[str, Any]:
    """Run-state provenance manifest (T1..T8). Pure.

    Returns the full run-state object (vessel, basis, provenance, basis run id,
    completed-run count, and the ordered task list) used to render the analysis
    progress panel and the per-section provenance badges.

    When ``structure_catalog_used`` is True the T8 task (manifold/PLET structure
    catalog) reflects the real ``subsea_structures.json`` catalog (status
    ``actual``) instead of the retired hardcoded proxy.

    When ``splashzone_computed`` is True the T5 task (per-structure splash-zone
    envelope) reflects the closed-form DNV-RP-H103 result (status ``actual``,
    first-principles, like T8) instead of the ``pending`` OrcaFlex placeholder.
    An OrcaFlex dynamic lowering run remains the future verification upgrade.

    ``rao_from_artifact`` governs the HONESTY of the RAO provenance (T3/T4): the
    ``"vessel"`` basis is only marked ``actual`` (OrcaWave diffraction) when a REAL
    vessel RAO artifact was actually ingested. With no artifact synced it stays on
    the ship-shaped proxy and carries the "RAO artifact not yet synced" note.
    """
    runs = list(completed_runs or [])
    prov = _effective_provenance(rao_basis, rao_from_artifact)
    basis_run_id = select_basis_run_id(runs, rao_basis)
    basis_note = _effective_human(rao_basis, rao_from_artifact)
    rao_source = "OrcaWave diffraction" if prov == "actual" else "parametric ship RAO"

    def task(tid, section, title, status, basis, source, run_id=None):
        return {
            "id": tid,
            "section": section,
            "title": title,
            "status": status,
            "basis": basis,
            "source": source,
            "run_id": run_id,
        }

    tasks = [
        task(
            "T1", 1, "Vessel particulars & crane curve", "actual",
            "Cited vessel database", "vessel_db.loader (curated + worldenergydata)",
        ),
        task(
            "T2", 2, "Crane lift suitability (6 items)", "actual",
            "Interpolated crane curve + DNV DAF", "vessel_suitability.assess_named",
        ),
        task(
            "T3", 3, "Vessel motion RAO", prov,
            basis_note, rao_source, basis_run_id,
        ),
        task(
            "T4", 3, "Operation envelopes (Hs/Tp)", prov,
            "operation_envelope on T3 RAO", "operation_envelope", basis_run_id,
        ),
        (
            task(
                "T5", 3, "Per-structure splash-zone envelope", "actual",
                "Closed-form DNV-RP-H103 splash-zone (slamming + drag + added-mass "
                "inertia, snap / slack-sling criterion) from catalog hydro coefficients",
                "calculations.compute_splashzone_envelope (closed-form; OrcaFlex "
                "dynamic run is the future verification path)",
            )
            if splashzone_computed
            else task(
                "T5", 3, "Per-structure splash-zone envelope", "pending",
                "Needs OrcaFlex installation run per structure",
                "OrcaFlex (licensed) — not yet queued",
            )
        ),
        task(
            "T6", 5, "Weather-window operability", prov,
            "weather_window engine on T4 limits", "weather_window.OperabilityTable",
        ),
        task(
            "T7", 7, "DP / mooring statistical risk", "actual",
            "IMCA + HSE RR444 + DNV statistics", "industry datasets",
        ),
        (
            task(
                "T8", 4, "Manifold / PLET structure catalog", "actual",
                "Catalog entries (PLET/manifold/SPS) computed from first principles "
                "(DNV-RP-H103)",
                "examples/demos/gtm/data/subsea_structures.json catalog",
            )
            if structure_catalog_used
            else task(
                "T8", 4, "Manifold / PLET structure catalog", "proxy",
                "Proxy entry — real catalog pending",
                "examples/demos/gtm (mudmat only)",
            )
        ),
    ]
    return {
        "vessel": vessel_name,
        "rao_basis": rao_basis,
        "rao_provenance": prov,
        "basis_run_id": basis_run_id,
        "n_completed_runs": len(runs),
        "tasks": tasks,
    }


# ---------------------------------------------------------------- assembly
def assemble_result(
    vessel_info: dict[str, Any],
    lifts: list[dict[str, Any]],
    radius_m: float,
    daf: float,
    rao_basis: str,
    operations: list[str],
    tp_grid_s: list[float],
    hs_scatter_limits: list[float],
    completed_runs: list[dict[str, Any]],
    rao_artifact: str | Path | None = None,
) -> dict[str, Any]:
    """Compose the full deterministic pamphlet ``result`` payload (pure).

    When ``rao_basis == "vessel"`` and a real vessel RAO artifact is resolvable
    (explicit ``rao_artifact`` path, or a ``completed_runs`` entry carrying a
    ``rao_artifact`` field), the operation envelopes are driven by the REAL
    ingested RAO and provenance is honestly marked ``actual``. Otherwise the
    vessel basis honestly falls back to the ship-shaped proxy.
    """
    artifact_path = resolve_rao_artifact(rao_basis, completed_runs, rao_artifact)
    rao_from_artifact = artifact_path is not None
    lift_rows = assess_lifts(vessel_info, lifts, radius_m, daf)
    envelopes = compute_envelopes(
        rao_basis, operations, tp_grid_s, rao_artifact=artifact_path
    )
    splashzone = compute_splashzone_set(
        lifts, tp_grid_s, heavy_lift_env=envelopes.get("heavy_lift")
    )
    op_table = compute_operability(hs_scatter_limits)
    structure_catalog_used = any(
        str(it.get("source")) == "structure" for it in lifts
    )
    provenance = build_provenance(
        completed_runs,
        rao_basis,
        vessel_name=vessel_info.get("name", "Installation vessel"),
        structure_catalog_used=structure_catalog_used,
        splashzone_computed=bool(splashzone),
        rao_from_artifact=rao_from_artifact,
    )
    return {
        "vessel": vessel_info,
        "lift_radius_m": float(radius_m),
        "daf": float(daf),
        "lifts": lift_rows,
        "envelopes": envelopes,
        "splashzone": splashzone,
        "tp_grid_s": [float(t) for t in tp_grid_s],
        "rao_basis": rao_basis,
        "rao_provenance": _effective_provenance(rao_basis, rao_from_artifact),
        "rao_from_artifact": rao_from_artifact,
        "rao_artifact": str(artifact_path) if artifact_path else None,
        "envelope_proxy": _effective_label(rao_basis, rao_from_artifact),
        "op_table": op_table,
        "provenance": provenance,
    }
