"""Atlas generation: sweep a workflow's parameter grid, fit the surrogate,
validate against held-out interior points, and stamp provenance.

Decision 1 of the spec. For the pilot the response is computed by the workflow's
own math (``get_sn_curve`` + ``miner_damage``), so the atlas is identical to a
live ``mooring_fatigue`` run rather than a re-derivation.
"""

from __future__ import annotations

import hashlib
import itertools
import json
import math
from functools import lru_cache
from pathlib import Path
from typing import Any, Callable

import pandas as pd

from digitalmodel.parametric.atlas import Atlas, Axis

REPO_ROOT = Path(__file__).resolve().parents[3]

# response functions keyed by workflow basename -----------------------------


def _mooring_fatigue_damage(point: dict[str, Any], environment: str) -> float:
    """Miner damage for a single (tension_range, n_cycles) cell — exactly the
    per-bin quantity the mooring_fatigue workflow sums."""
    from digitalmodel.fatigue.damage import miner_damage
    from digitalmodel.fatigue.sn_curves import get_sn_curve

    sn_curve = get_sn_curve(str(point["sn_curve"]), environment)
    stress_range = float(point["tension_range_kN"]) * 1000.0 / float(point["area_mm2"])
    df = miner_damage(
        pd.DataFrame([{"stress_range": stress_range, "cycles": float(point["n_cycles"])}]),
        sn_curve,
    )
    return float(df.attrs["total_damage"])


def _synthetic_rope_damage(
    point: dict[str, Any],
    tn_intercept: float = 0.259,
    tn_slope: float = 13.46,
    mean_load_knockdown: float = 0.0,
    load_ratio: float = 0.30,
) -> float:
    """T-N (tension-range) damage for a single (tension_range, n_cycles) cell of
    a synthetic rope, mirroring synthetic_rope_mooring_fatigue's formula:
    allowable = a_eff * (range/MBL)^-slope, damage = n / allowable. Covers the
    FATIGUE limit state only (creep + min-tension stay full-run checks)."""
    a_eff = tn_intercept * math.exp(-mean_load_knockdown * load_ratio)
    normalised_range = float(point["tension_range_kN"]) / float(point["MBL_kN"])
    allowable_cycles = a_eff * normalised_range ** (-tn_slope)
    return float(point["n_cycles"]) / allowable_cycles


def _code_check_utilisation(
    point: dict[str, Any],
    design_factor: float = 0.67,
    smts_ratio: float = 1.185,
    temperature_derating: float = 1.0,
) -> float:
    """API RP 2RD combined tension+bending utilisation for one (OD, WT, SMYS,
    tension, moment) point — the continuous quantity the boundary class
    interpolates before thresholding at 1.0."""
    import numpy as np

    from digitalmodel.orcaflex.code_check_engine import (
        APIRP2RDInput,
        check_api_rp_2rd,
    )

    smys = float(point["smys"])
    pipe = APIRP2RDInput(
        outer_diameter=float(point["outer_diameter"]),
        wall_thickness=float(point["wall_thickness"]),
        smys=smys,
        smts=smys * smts_ratio,
        design_factor=design_factor,
        temperature_derating=temperature_derating,
    )
    results = check_api_rp_2rd(
        pipe,
        np.array([0.0]),
        np.array([float(point["effective_tension_kN"])]),
        np.array([float(point["bending_moment_kNm"])]),
        np.array([0.0]),
    )
    return float(results[0].utilisation)


@lru_cache(maxsize=4)
def _rao_interpolator(database_path: str):
    import numpy as np
    import yaml

    from digitalmodel.hydrodynamics.interpolator import CoefficientsInterpolator
    from digitalmodel.hydrodynamics.models import RAOData

    path = Path(database_path)
    if not path.is_absolute():
        path = REPO_ROOT / path
    db = yaml.safe_load(path.read_text())["rao_tabulation"]["rao_database"]
    omega = np.array([2.0 * math.pi / float(p) for p in db["periods_s"]])
    order = np.argsort(omega)
    rao = RAOData(
        frequencies=omega[order],
        directions=np.array([float(h) for h in db["headings_deg"]]),
        amplitudes=np.array(db["amplitudes"], dtype=float)[order],
        phases=np.array(db["phases_deg"], dtype=float)[order],
        vessel_name=str(db.get("vessel_name", "atlas")),
    )
    interp = CoefficientsInterpolator()
    interp.load_raos(rao)
    return interp


def _rao_heave(point: dict[str, Any], database_path: str) -> float:
    """Interpolated heave amplitude (DOF index 2) at a single (frequency,
    heading), reusing the rao_tabulation workflow's own interpolator so the
    atlas reproduces the workflow exactly."""
    import numpy as np

    interp = _rao_interpolator(database_path)
    out = interp.interpolate_all_dofs(
        np.array([float(point["frequency_rad_s"])]),
        np.array([float(point["heading_deg"])]),
        method="linear",
    )
    return float(out.amplitudes[0, 0, 2])


def _free_span_utilisation(point: dict[str, Any]) -> float:
    """DNV-RP-F105 free-span VIV span utilisation (span/allowable) for one
    (span_length, od, wt, current) point."""
    from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
    from digitalmodel.subsea.pipeline.free_span.models import (
        BoundaryConditionF105,
        EnvironmentType,
        PipeSpanInput,
    )

    inp = PipeSpanInput(
        od_m=float(point["od_m"]),
        wt_m=float(point["wt_m"]),
        span_length_m=float(point["span_length_m"]),
        e_modulus_pa=207e9,
        steel_density_kgm3=7850.0,
        content_density_kgm3=900.0,
        water_density_kgm3=1025.0,
        current_velocity_ms=float(point["current_velocity_ms"]),
        wave_velocity_ms=0.0,
        seabed_gap_m=0.5,
        bc=BoundaryConditionF105("pinned-pinned"),
        sag_m=0.0,
        structural_damping=0.005,
        hydrodynamic_damping=0.010,
        sn_curve_class="F",
        environment=EnvironmentType("seawater_cp"),
        gamma_on_IL=1.1,
        gamma_on_CF=1.3,
        gamma_k=1.15,
    )
    result = FreespanVIVFatigue(
        inp, submerged_weight_N_m=850.0, alpha=1.0, KC=30.0
    ).assess()
    return float(result.span_utilization)


def _pile_capacity_kn(point: dict[str, Any]) -> float:
    """API RP 2GEO alpha-method axial pile capacity (kN) — geometry/soil only;
    the load case is applied at query time."""
    from digitalmodel.geotechnical.pile_capacity import alpha_method_capacity

    result = alpha_method_capacity(
        D=float(point["diameter_m"]),
        L=float(point["embedded_length_m"]),
        Su=float(point["Su_kpa"]),
        sigma_v=float(point["sigma_v_kpa"]),
        Nc=float(point.get("Nc", 9.0)),
    )
    return float(result.total_capacity_kn)


def _suction_anchor_capacity_kn(point: dict[str, Any]) -> float:
    """DNV-RP-E303 suction-caisson holding capacity (kN) — geometry/soil only."""
    from digitalmodel.geotechnical.anchors import suction_anchor_capacity

    result = suction_anchor_capacity(
        diameter_m=float(point["diameter_m"]),
        length_m=float(point["length_m"]),
        su_kpa=float(point["su_kpa"]),
        alpha=float(point.get("alpha", 0.65)),
        nc=float(point.get("nc", 9.0)),
        wall_thickness_m=float(point.get("wall_thickness_m", 0.04)),
    )
    return float(result.total_capacity_kn)


RESPONSE_FUNCS: dict[str, Callable[..., float]] = {
    "mooring_fatigue": _mooring_fatigue_damage,
    "synthetic_rope_mooring_fatigue": _synthetic_rope_damage,
    "code_check": _code_check_utilisation,
    "rao_tabulation": _rao_heave,
    "free_span": _free_span_utilisation,
    "pile_capacity": _pile_capacity_kn,
    "anchor_capacity": _suction_anchor_capacity_kn,
}


# grid construction ----------------------------------------------------------


def _grid_points(axes: list[Axis]) -> list[dict[str, Any]]:
    names = [ax.name for ax in axes]
    value_lists = [ax.values if ax.is_categorical else ax.grid for ax in axes]
    return [dict(zip(names, combo)) for combo in itertools.product(*value_lists)]


def _holdout_points(axes: list[Axis]) -> list[dict[str, Any]]:
    """Interior test points: geometric/arithmetic midpoints between every pair
    of adjacent knots on each continuous axis, at the first value of the other
    axes. Catches grid-too-coarse error the publish gate enforces against."""
    cont = [ax for ax in axes if not ax.is_categorical]
    cats = [ax for ax in axes if ax.is_categorical]
    base = {ax.name: ax.grid[len(ax.grid) // 2] for ax in cont}
    points: list[dict[str, Any]] = []
    cat_combos = list(
        itertools.product(*[ax.values for ax in cats])
    ) or [()]
    for cat_combo in cat_combos:
        cat_part = {ax.name: v for ax, v in zip(cats, cat_combo)}
        for ax in cont:
            for a, b in zip(ax.grid, ax.grid[1:]):
                mid = (a * b) ** 0.5 if ax.scale == "log" else (a + b) / 2.0
                pt = {**base, **cat_part, ax.name: mid}
                points.append(pt)
    return points


def _atlas_id(spec: dict[str, Any], provenance: dict[str, Any]) -> str:
    blob = json.dumps(
        {"spec": spec, "code_version": provenance.get("code_version"),
         "standards": provenance.get("standards")},
        sort_keys=True,
    )
    return hashlib.sha256(blob.encode()).hexdigest()[:12]


def generate_atlas(
    basename: str,
    physics: str,
    response: str,
    axes: list[Axis],
    *,
    response_kwargs: dict[str, Any] | None = None,
    tolerance: float = 0.10,
    code_version: str = "unknown",
    standards: list[dict[str, str]] | None = None,
    input_template: Path | None = None,
    workflow_id: str | None = None,
    content_fingerprint: str | None = None,
) -> Atlas:
    response_kwargs = response_kwargs or {}
    fn = RESPONSE_FUNCS[basename]

    rows = []
    for point in _grid_points(axes):
        rows.append({**point, response: fn(point, **response_kwargs)})
    grid = pd.DataFrame(rows)

    provenance = {
        "basename": basename,
        "code_version": code_version,
        "standards": standards or [],
        "response_kwargs": response_kwargs,
    }
    if workflow_id is not None:
        provenance["workflow_id"] = workflow_id
    if content_fingerprint is not None:
        # the fingerprint (refresh.py) is the staleness basis; deriving the id
        # from it keeps atlas_id stable across unrelated commits.
        provenance["content_fingerprint"] = content_fingerprint
    if input_template is not None and Path(input_template).exists():
        provenance["input_template_sha256"] = hashlib.sha256(
            Path(input_template).read_bytes()
        ).hexdigest()

    spec = {
        "physics": physics,
        "response": response,
        "axes": [ax.to_dict() for ax in axes],
    }
    atlas_id = (
        content_fingerprint[:12]
        if content_fingerprint
        else _atlas_id(spec, provenance)
    )
    atlas = Atlas(
        basename=basename,
        atlas_id=atlas_id,
        physics=physics,
        response=response,
        axes=axes,
        grid=grid,
        provenance=provenance,
    )

    # held-out validation -------------------------------------------------
    worst = 0.0
    samples = []
    for point in _holdout_points(axes):
        predicted = atlas.predict(point)
        actual = fn(point, **response_kwargs)
        rel = abs(predicted.value - actual) / actual if actual else 0.0
        worst = max(worst, rel)
        samples.append(
            {"point": point, "predicted": predicted.value, "actual": actual,
             "rel_error": rel, "in_range": predicted.in_range}
        )
    worst_samples = sorted(samples, key=lambda s: -s["rel_error"])[:15]
    atlas.validation = {
        "metric": "max_rel_error",
        "max_rel_error": worst,
        "threshold": tolerance,
        "passes": worst <= tolerance,
        "n_holdout": len(samples),
        "worst_samples": worst_samples,
    }
    return atlas
