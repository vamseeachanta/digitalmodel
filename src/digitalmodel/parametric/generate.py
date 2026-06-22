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
    # the atlas may grid directly on the derived stress axis (#830), else derive
    # stress from the raw tension/area pair.
    if "stress_MPa" in point:
        stress_range = float(point["stress_MPa"])
    else:
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


def _spectral_fatigue_annual_damage(
    point: dict[str, Any],
    gamma: float = 3.3,
    stress_gain_MPa_per_m: float = 20.0,
    sn_slope: float = 3.0,
    sn_intercept: float = 12.164,
) -> float:
    """Annual Dirlik fatigue damage for a JONSWAP sea state (Hs, Tp).

    SCREENING MODEL: the wave spectrum is mapped to a stress PSD via a fixed
    quasi-static stress gain g (MPa per m of wave amplitude),
    S_stress(f) = g**2 * S_wave(f). A real project replaces this with its own
    stress-RAO H(f); the gain is a per-location property baked into the atlas,
    not an axis. (Pattern follows structural/fatigue/scatter_fatigue.py.)"""
    import numpy as np

    from digitalmodel.fatigue.spectral_fatigue import (
        compute_spectral_moments,
        dirlik_damage,
    )
    from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

    omega, s_omega = WaveSpectra().jonswap(
        hs=float(point["Hs"]), tp=float(point["Tp"]), gamma=gamma
    )  # rad/s, m^2*s
    f_hz = omega / (2.0 * np.pi)
    s_wave_hz = s_omega * 2.0 * np.pi  # m^2/Hz
    stress_psd = (stress_gain_MPa_per_m**2) * s_wave_hz  # MPa^2/Hz (quasi-static)
    moments = compute_spectral_moments(f_hz, stress_psd)
    result = dirlik_damage(moments, sn_slope=sn_slope, sn_intercept=sn_intercept, duration=1.0)
    return float(result.damage_per_year)


# Reference FPSO configuration baked into the atlas (everything except the
# three axes Hs / Tp / water_depth). A project re-bakes its own vessel + mooring.
_FPSO_REF_VESSEL = {"loa": 330.0, "beam": 60.0, "draft": 22.0,
                    "freeboard": 25.0, "displacement": 320000.0}
_FPSO_REF_ENV = {"gamma": 3.3, "wave_heading": 45.0, "wind_speed": 20.0,
                 "wind_heading": 45.0, "current_speed": 1.5, "current_heading": 30.0}
_FPSO_REF_MOORING = {"n_lines": 8, "chain_diameter": 120.0, "chain_grade": "R4",
                     "chain_link_type": "Studless", "line_length": 2000.0,
                     "pretension": 1500000.0}


def _fpso_max_line_tension_N(point: dict[str, Any]) -> float:
    """Max mooring line tension (N) for a reference FPSO under sea state
    (Hs, Tp) at a given water_depth_m. Calls the workflow compute chain
    directly (no file side-effects)."""
    from digitalmodel.marine_ops.marine_engineering.mooring_analysis.fpso_full_workflow import (  # noqa: E501
        FPSOMooringFullWorkflow,
    )

    vessel = dict(_FPSO_REF_VESSEL)
    environment = {**_FPSO_REF_ENV, "Hs": float(point["Hs"]), "Tp": float(point["Tp"])}
    mooring = {**_FPSO_REF_MOORING, "water_depth": float(point["water_depth_m"])}
    cfg = {"vessel": vessel, "environment": environment, "mooring": mooring}

    wf = FPSOMooringFullWorkflow()
    spectrum = wf._wave_spectrum(environment)
    forces = wf._environmental_forces(cfg, vessel, environment, spectrum)
    chain = wf._chain_properties(mooring)
    line_model = wf._line_model(vessel, mooring, chain)
    equilibrium, line_tensions = wf._static_equilibrium(forces, line_model)
    result = wf._result(cfg, forces, spectrum, chain, equilibrium, line_tensions)
    return float(result["summary"]["max_line_tension_N"])


_VIV_REF_CROSSECTION = [{
    "Nominal_ID": None, "Nominal_OD": 12.75, "Design_WT": 0.5,
    "Corrosion_Allowance": 0.125, "average_internal_metal_loss_percentage": 0,
    "external_fluid": {"unit": "lb/inch^3", "density": 0.03721,
                       "kinematic_viscosity": 1.35e-6,
                       "pressure": {"unit": "psi", "pressure": None, "reference_depth": 410}},
    "internal_fluid": {"unit": "lb/inch^3", "density": 0.02218,
                       "pressure": {"unit": "psi", "pressure": 1095.2,
                                    "top_side_elevation": 65.62, "reference_water_depth": 410}},
    "Material": {"name": "steel", "grade": "API 5L X65",
                 "ThermalExpansionCoefficient": 0.0000117},
    "WeldFactor": {"Seamless": 1.0},
    "coatings": [{"material": None, "purpose": "insulation", "thickness": 0, "density": 0},
                 {"material": None, "purpose": "anti-corrosion", "thickness": 0.122, "density": 0.03396}],
    "code": ["30 CFR Part 250"],
    "Manufacturing": {"Coupling Mass Ratio": 0.0},
}]


def _viv_safety_factor_inline(point: dict[str, Any]) -> float:
    """Min in-line VIV safety factor (natural / vortex-shedding frequency) for a
    reference 12.75 in tubular at a given (span_length, current_speed). Calls the
    viv_analysis compute methods directly, skipping the file-writing save_results."""
    import copy

    from digitalmodel.subsea.viv_analysis.viv_analysis import VIVAnalysis
    from digitalmodel.subsea.viv_analysis.viv_tubular_members import VIVTubularMembers

    cross = copy.deepcopy(_VIV_REF_CROSSECTION)
    cross[0]["Nominal_OD"] = float(point.get("od_in", 12.75))
    cross[0]["Design_WT"] = float(point.get("wt_in", 0.5))
    cfg = {
        "calculation": {"name": "tubular_member_viv"},
        "inputs": {"seawater_density": 1025},
        "pipeline": {"length": 1000, "span_length": [float(point["span_length_ft"])],
                     "crossection": cross},
        "viv": {"st": 0.2},
        "environment": {"current": [{"label": "design_current",
                        "data": [{"depth": 0, "speed": float(point["current_speed_in_s"])}]}]},
        "modes": {"analysis": {"simply_supported": True, "clamped_clamped": False,
                               "free_free": False},
                  "eigen_values": {"clamped": [4.730, 7.853, 10.996]}},
        "default": {"config": {"generate_plots": False}},
    }
    cfg = VIVAnalysis().get_pipe_properties(cfg)
    tm = VIVTubularMembers()
    nat = tm.get_natural_frequencies(cfg)
    vs = tm.get_vs_frequencies(cfg)
    sf = tm.get_safety_factors(cfg, nat, vs)
    return float(sf["safety_factor_inline"].min())


def _fowt_mbr_utilisation(point: dict[str, Any]) -> float:
    """FOWT watch-circle vs dynamic-cable MBR utilisation (mbr_limit / governing
    bend radius) for one (watch_circle_radius, mbr_limit_m) point, at a fixed
    reference cable geometry. UC <= 1 passes (governing radius >= MBR limit)."""
    from digitalmodel.orcaflex.mooring_design_fowt import (
        DynamicCableConfig,
        check_watch_circle_vs_cable,
    )

    cable = DynamicCableConfig(
        suspended_length=320.0,
        hang_off_elevation=90.0,
        nominal_horizontal_span=260.0,
        mbr_limit_m=float(point["mbr_limit_m"]),
    )
    result = check_watch_circle_vs_cable(float(point["watch_circle_radius"]), cable)
    return float(result.mbr_limit_m / result.governing_bend_radius_m)


def _lifting_lug_utilisation(point: dict[str, Any]) -> float:
    """AISC ASD lifting-lug / padeye governing utilisation (max of pin-bearing,
    net-tension, shear-tear-out) for one (static_load_kN, plate_thickness_mm)
    point, at a fixed reference padeye geometry. UC <= 1 passes."""
    from digitalmodel.lifting_lug.workflow import design_load_kn, lug_checks

    factored = design_load_kn(float(point["static_load_kN"]), 2.0, 1.1)
    checks = lug_checks(
        design_load_kn=factored,
        total_thickness_mm=float(point["plate_thickness_mm"]),
        pin_diameter_mm=75.0,
        hole_diameter_mm=80.0,
        outer_radius_mm=110.0,
        yield_strength_mpa=355.0,
    )
    return float(max(c.utilization for c in checks))


def _esp_pump_utilisation(point: dict[str, Any]) -> float:
    """ESP governing screening utilisation (max of pump-stages / motor-power /
    rate-within-range checks) for one (flow_rate_m3_per_day, dynamic_fluid_level_m)
    point, at a fixed reference well + pump. UC <= 1 passes."""
    from digitalmodel.production_engineering.esp_pump_hydraulics import size_esp

    result = size_esp(
        flow_rate_m3_per_day=float(point["flow_rate_m3_per_day"]),
        dynamic_fluid_level_m=float(point["dynamic_fluid_level_m"]),
        pump_setting_depth_m=2500.0,
        wellhead_pressure_kpa=2000.0,
        specific_gravity=0.85,
        tubing_inner_diameter_m=0.076,
        head_per_stage_m=6.0,
        bhp_per_stage_hp=0.5,
        max_stages=400,
        motor_rating_hp=250.0,
        min_rate_m3_per_day=600.0,
        max_rate_m3_per_day=2000.0,
        hazen_williams_c=120.0,
    )
    return float(max(c["utilization"] for c in result.checks))


def _inspection_remaining_life_years(point: dict[str, Any]) -> float:
    """API 510/570/653 remaining life (yr) = corrosion_allowance / corrosion_rate
    for one (corrosion_rate_mm_yr, current_wall_thickness_mm) point, at a fixed
    reference t_min and code-max interval. Plain positive scalar served directly
    (life from rate + allowance); the half-life / code-cap decision is applied at
    query time, not baked into the grid."""
    from digitalmodel.asset_integrity.inspection_planning import plan_inspection

    plan = plan_inspection(
        current_mm=float(point["current_wall_thickness_mm"]),
        required_mm=6.0,
        code_max_interval_years=10.0,
        corrosion_rate=float(point["corrosion_rate_mm_yr"]),
    )
    return float(plan.remaining_life_years)


RESPONSE_FUNCS: dict[str, Callable[..., float]] = {
    "mooring_fatigue": _mooring_fatigue_damage,
    "synthetic_rope_mooring_fatigue": _synthetic_rope_damage,
    "spectral_fatigue": _spectral_fatigue_annual_damage,
    "fpso_mooring_full": _fpso_max_line_tension_N,
    "fowt_mooring": _fowt_mbr_utilisation,
    "lifting_lug": _lifting_lug_utilisation,
    "esp_pump_hydraulics": _esp_pump_utilisation,
    "inspection_planning": _inspection_remaining_life_years,
    "viv_analysis": _viv_safety_factor_inline,
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
    """Interior test entries: each is a midpoint between adjacent knots on one
    continuous axis (others at their median), tagged with the slice / axis /
    interval it measures so generation can build a per-interval error map.
    Catches grid-too-coarse error the publish gate enforces against."""
    cont = [ax for ax in axes if not ax.is_categorical]
    cats = [ax for ax in axes if ax.is_categorical]
    base = {ax.name: ax.grid[len(ax.grid) // 2] for ax in cont}
    entries: list[dict[str, Any]] = []
    cat_combos = list(itertools.product(*[ax.values for ax in cats])) or [()]
    for cat_combo in cat_combos:
        cat_part = {ax.name: v for ax, v in zip(cats, cat_combo)}
        cat_key = "|".join(str(v) for v in cat_combo)  # "" when no categorical axis
        for ax in cont:
            for i, (a, b) in enumerate(zip(ax.grid, ax.grid[1:])):
                mid = (a * b) ** 0.5 if ax.scale == "log" else (a + b) / 2.0
                entries.append({"point": {**base, **cat_part, ax.name: mid},
                                "cat_key": cat_key, "axis": ax.name, "interval": i})
    return entries


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
    # local_error_map[cat_key][axis] = [rel_error per interval] (#828)
    local_error_map: dict[str, dict[str, list[float]]] = {}
    for entry in _holdout_points(axes):
        point = entry["point"]
        predicted = atlas.predict(point)
        actual = fn(point, **response_kwargs)
        rel = abs(predicted.value - actual) / actual if actual else 0.0
        worst = max(worst, rel)
        samples.append(
            {"point": point, "predicted": predicted.value, "actual": actual,
             "rel_error": rel, "in_range": predicted.in_range}
        )
        per_axis = local_error_map.setdefault(entry["cat_key"], {}).setdefault(
            entry["axis"], [])
        per_axis.append(rel)  # intervals arrive in ascending order
    worst_samples = sorted(samples, key=lambda s: -s["rel_error"])[:15]
    atlas.validation = {
        "metric": "max_rel_error",
        "max_rel_error": worst,
        "threshold": tolerance,
        "passes": worst <= tolerance,
        "n_holdout": len(samples),
        "worst_samples": worst_samples,
        "local_error_map": local_error_map,
    }
    return atlas


def _refine_axis(axes: list[Axis], worst_point: dict[str, Any]):
    """Find the continuous axis whose worst-holdout value lies between knots and
    return (axis_name, new_knot, new_axes) with that midpoint inserted. Holdout
    points vary one axis at a time, so exactly one axis is off-knot."""
    import dataclasses

    for i, ax in enumerate(axes):
        if ax.is_categorical:
            continue
        val = worst_point.get(ax.name)
        if val is None:
            continue
        if not any(abs(val - k) <= 1e-9 * max(1.0, abs(k)) for k in ax.grid):
            new_grid = sorted(set(ax.grid) | {float(val)})
            new_axes = list(axes)
            new_axes[i] = dataclasses.replace(ax, grid=new_grid)  # preserves scale/derived
            return ax.name, float(val), new_axes
    return None, None, axes


def generate_atlas_adaptive(
    basename: str,
    physics: str,
    response: str,
    axes: list[Axis],
    *,
    max_rounds: int = 8,
    **kwargs: Any,
) -> Atlas:
    """Generate an atlas, then while it misses the tolerance gate, insert a knot
    at the worst held-out interval and re-validate (#829). Deterministic given
    the seed grid + source, so a refresh reproduces the same densified atlas.
    The densification log is recorded so coverage is never silently changed."""
    log: list[dict[str, Any]] = []
    atlas = generate_atlas(basename, physics, response, axes, **kwargs)
    rounds = 0
    while not atlas.validation["passes"] and rounds < max_rounds:
        name, knot, axes = _refine_axis(axes, atlas.validation["worst_samples"][0]["point"])
        if name is None:
            break  # nothing left to refine (e.g. categorical-only error)
        log.append({
            "round": rounds + 1, "axis": name, "added_knot": knot,
            "max_rel_error_before": atlas.validation["max_rel_error"],
        })
        atlas = generate_atlas(basename, physics, response, axes, **kwargs)
        rounds += 1
    atlas.validation["densification_log"] = log
    return atlas
