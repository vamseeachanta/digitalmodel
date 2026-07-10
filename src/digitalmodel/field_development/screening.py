# ABOUTME: Engineering screening layer — cable sizing, flowline diameter sweep,
# ABOUTME: and deterministic markdown screening report. Issue #1511 (epic #1507).
"""
digitalmodel.field_development.screening
=========================================

Workstream D of the field-development epic (#1507): screening-tier sizing
correlations layered on top of the #1508 tracer, plus a one-page deterministic
markdown report per layout iteration.

What this module does
---------------------
1. **Cable sizing screen** — conductor DC resistance (IEC 60228 material
   constants), a conductor-loss-only IEC 60287-1-1 ampacity estimate, the
   standard three-phase voltage-drop formula, and a smallest-passing-size
   sweep over the IEC 60228 standard cross-sections.
2. **Flowline diameter sweep** — REUSES the tracer's Darcy-Weisbach +
   Swamee-Jain pressure-drop screen
   (:func:`digitalmodel.field_development.onshore_layout.darcy_weisbach_pressure_drop`)
   and the ASME B36.10M pipe catalog
   (:mod:`digitalmodel.materials.line_pipe`) to find the smallest standard
   nominal size that meets a target arrival pressure AND the API RP 14E
   erosional-velocity limit.
3. **Screening report** — :func:`screening_report` writes a deterministic
   one-page markdown summary (assumptions, inputs, per-line results,
   pass/fail flags). No timestamps, fixed number formatting: identical inputs
   give byte-identical output.

ALL correlation constants (C-factor, resistivities, thermal resistance,
candidate sizes, ...) live in ``data/screening_defaults.yml``; nothing is
hardcoded here. Pass an edited copy of that mapping to override any default.

Scope honesty
-------------
Screening grade only. The ampacity estimate zeroes dielectric and
sheath/armour losses and lumps the IEC 60287 thermal-resistance build-up into
one number; the hydraulic screen is single-phase incompressible. Rated cable
and multiphase flow-assurance design are downstream workstreams of #1507, not
this slice.

This module CONSUMES the tracer's public layout objects (it reads
``FlowlineRoute`` attributes) and deliberately does not modify or re-route
them — layout-schema work is a parallel lane (#1509).

Usage
-----
>>> from digitalmodel.field_development.screening import (
...     flowline_diameter_sweep, size_cable, screening_report)
>>> sweep = flowline_diameter_sweep(
...     rate_m3_per_day=300.0, length_m=2000.0, elevation_change_m=15.0,
...     roughness_m=4.5e-5, density_kg_per_m3=850.0, viscosity_pa_s=2.0e-3,
...     inlet_pressure_kpa=2000.0, target_arrival_pressure_kpa=1500.0)
>>> sweep["selected"]["nps"]  # doctest: +SKIP
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any, Optional

import yaml

from digitalmodel.materials.line_pipe import line_pipe

from .onshore_layout import SECONDS_PER_DAY, FieldLayout, darcy_weisbach_pressure_drop

# --- unit conversions (exact definitions) ------------------------------------
_M_PER_FT = 0.3048
# 1 kg/m^3 in lb/ft^3 (= 0.45359237 kg/lb / 0.3048^3 m^3/ft^3, inverted).
_LB_PER_FT3_PER_KG_PER_M3 = 0.3048**3 / 0.45359237

_DEFAULTS_PATH = Path(__file__).parent / "data" / "screening_defaults.yml"


def load_screening_defaults(path: Optional[str | Path] = None) -> dict[str, Any]:
    """Load the screening-defaults YAML (bundled file unless ``path`` given)."""
    with open(path or _DEFAULTS_PATH, "r", encoding="utf-8") as fh:
        defaults = yaml.safe_load(fh)
    if not isinstance(defaults, dict):
        raise ValueError("screening defaults did not parse to a mapping")
    return defaults


# ---------------------------------------------------------------------------
# Erosional velocity — API RP 14E
# ---------------------------------------------------------------------------


def erosional_velocity_api_rp_14e(density_kg_per_m3: float, c_factor: float) -> float:
    """Erosional velocity limit [m/s] per API RP 14E.

    API RP 14E (Recommended Practice for Design and Installation of Offshore
    Production Platform Piping Systems, 5th ed., 1991), section 2.5a:

        Ve = C / sqrt(rho_m)   [Ve in ft/s, rho_m in lb/ft^3]

    with C = 100 for continuous, non-corrosive, solids-free service (lower C
    for corrosive or sandy service per the RP). This function converts the
    field-unit correlation to SI exactly; the C-factor keeps its RP 14E
    field-unit definition and comes from config, never hardcoded.
    """
    if density_kg_per_m3 <= 0:
        raise ValueError(f"density must be > 0, got {density_kg_per_m3!r}")
    if c_factor <= 0:
        raise ValueError(f"c_factor must be > 0, got {c_factor!r}")
    density_lb_ft3 = density_kg_per_m3 * _LB_PER_FT3_PER_KG_PER_M3
    return _M_PER_FT * c_factor / math.sqrt(density_lb_ft3)


# ---------------------------------------------------------------------------
# Cable screening — resistance, ampacity, voltage drop, sizing sweep
# ---------------------------------------------------------------------------


def conductor_resistance_ohm_per_m(
    area_mm2: float,
    temperature_c: float,
    material: str = "copper",
    materials: Optional[dict[str, Any]] = None,
) -> float:
    """Conductor DC resistance per metre at operating temperature.

    R(theta) = rho_20 * (1 + alpha_20 * (theta - 20)) / A — the standard
    linear temperature correction used by IEC 60287-1-1 (section 2.1.1, with
    the AC skin/proximity increments neglected at screening fidelity).
    Material constants (rho_20, alpha_20) per IEC 60228 come from the
    ``conductors.materials`` config section.
    """
    if area_mm2 <= 0:
        raise ValueError(f"area_mm2 must be > 0, got {area_mm2!r}")
    if materials is None:
        materials = load_screening_defaults()["conductors"]["materials"]
    if material not in materials:
        raise KeyError(
            f"material {material!r} not in config; available: {sorted(materials)}"
        )
    props = materials[material]
    rho20 = float(props["resistivity_20c_ohm_m"])
    alpha = float(props["temp_coefficient_per_k"])
    area_m2 = area_mm2 * 1.0e-6
    return rho20 * (1.0 + alpha * (temperature_c - 20.0)) / area_m2


def cable_ampacity_screening(
    resistance_ohm_per_m: float,
    max_conductor_temp_c: float,
    ambient_temp_c: float,
    thermal_resistance_k_m_per_w: float,
) -> float:
    """Continuous current rating [A] — conductor-loss-only IEC 60287 form.

    IEC 60287-1-1 (Electric cables — Calculation of the current rating)
    gives, for the permissible current, I = [dTheta / (R*T_total)]^0.5 once
    dielectric losses and sheath/armour loss factors are set to zero and the
    thermal-resistance build-up T1 + n(T2 + T3 + T4) is lumped into a single
    total ``thermal_resistance_k_m_per_w``. Screening tier only: real ratings
    need the full per-layer IEC 60287 (or vendor) calculation.
    """
    delta_theta = max_conductor_temp_c - ambient_temp_c
    if delta_theta <= 0:
        raise ValueError(
            "max_conductor_temp_c must exceed ambient_temp_c "
            f"(got {max_conductor_temp_c} <= {ambient_temp_c})"
        )
    if resistance_ohm_per_m <= 0 or thermal_resistance_k_m_per_w <= 0:
        raise ValueError("resistance and thermal resistance must be > 0")
    return math.sqrt(
        delta_theta / (resistance_ohm_per_m * thermal_resistance_k_m_per_w)
    )


def voltage_drop_three_phase_v(
    current_a: float,
    length_m: float,
    resistance_ohm_per_m: float,
    reactance_ohm_per_m: float,
    power_factor: float,
) -> float:
    """Three-phase line-to-line voltage drop [V].

    dU = sqrt(3) * I * L * (R' cos(phi) + X' sin(phi)) — the standard
    steady-state formula (cf. IEC 60364-5-52 Annex G; any power-distribution
    textbook). Lagging power factor assumed (sin(phi) >= 0).
    """
    if not 0.0 < power_factor <= 1.0:
        raise ValueError(f"power_factor must be in (0, 1], got {power_factor!r}")
    if current_a <= 0 or length_m < 0:
        raise ValueError("current_a must be > 0 and length_m >= 0")
    sin_phi = math.sqrt(1.0 - power_factor**2)
    return (
        math.sqrt(3.0)
        * current_a
        * length_m
        * (resistance_ohm_per_m * power_factor + reactance_ohm_per_m * sin_phi)
    )


def size_cable(
    load_kw: float,
    line_voltage_v: float,
    length_m: float,
    power_factor: float,
    defaults: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    """Smallest IEC 60228 conductor size passing ampacity + voltage-drop screens.

    Load current: I = P / (sqrt(3) * U_LL * pf) (three-phase). Each standard
    cross-section is screened for (a) ampacity >= load current
    (:func:`cable_ampacity_screening`) and (b) voltage drop within the
    configured percent limit (:func:`voltage_drop_three_phase_v`). Candidates
    are swept ascending; ``selected`` is the smallest passing size (``None``
    if none passes).
    """
    if load_kw <= 0 or line_voltage_v <= 0:
        raise ValueError("load_kw and line_voltage_v must be > 0")
    cfg = defaults or load_screening_defaults()
    cable_cfg = cfg["cable_screening"]
    materials = cfg["conductors"]["materials"]
    sizes = [float(s) for s in cfg["conductors"]["standard_sizes_mm2"]]
    material = str(cable_cfg["material"])
    temp_max = float(cable_cfg["max_conductor_temp_c"])
    temp_amb = float(cable_cfg["ambient_temp_c"])
    thermal_res = float(cable_cfg["thermal_resistance_k_m_per_w"])
    reactance = float(cable_cfg["reactance_ohm_per_m"])
    vdrop_limit_pct = float(cable_cfg["voltage_drop_limit_pct"])

    current = load_kw * 1.0e3 / (math.sqrt(3.0) * line_voltage_v * power_factor)

    candidates: list[dict[str, Any]] = []
    selected: Optional[dict[str, Any]] = None
    for size in sorted(sizes):
        resistance = conductor_resistance_ohm_per_m(
            size, temp_max, material=material, materials=materials
        )
        ampacity = cable_ampacity_screening(resistance, temp_max, temp_amb, thermal_res)
        vdrop = voltage_drop_three_phase_v(
            current, length_m, resistance, reactance, power_factor
        )
        vdrop_pct = 100.0 * vdrop / line_voltage_v
        row = {
            "size_mm2": size,
            "resistance_ohm_per_m": resistance,
            "ampacity_a": ampacity,
            "ampacity_ok": ampacity >= current,
            "voltage_drop_v": vdrop,
            "voltage_drop_pct": vdrop_pct,
            "voltage_drop_ok": vdrop_pct <= vdrop_limit_pct,
        }
        row["passes"] = bool(row["ampacity_ok"] and row["voltage_drop_ok"])
        candidates.append(row)
        if selected is None and row["passes"]:
            selected = row

    return {
        "load_kw": load_kw,
        "line_voltage_v": line_voltage_v,
        "length_m": length_m,
        "power_factor": power_factor,
        "current_a": current,
        "material": material,
        "max_conductor_temp_c": temp_max,
        "ambient_temp_c": temp_amb,
        "thermal_resistance_k_m_per_w": thermal_res,
        "voltage_drop_limit_pct": vdrop_limit_pct,
        "candidates": candidates,
        "selected": selected,
        "passes": selected is not None,
    }


# ---------------------------------------------------------------------------
# Flowline diameter sweep — reuses the tracer's Darcy-Weisbach screen
# ---------------------------------------------------------------------------


def flowline_diameter_sweep(
    rate_m3_per_day: float,
    length_m: float,
    elevation_change_m: float,
    roughness_m: float,
    density_kg_per_m3: float,
    viscosity_pa_s: float,
    inlet_pressure_kpa: float,
    target_arrival_pressure_kpa: float,
    defaults: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    """Smallest standard pipe size meeting arrival pressure + erosional limit.

    Sweeps the configured candidate NPS list (ascending) with inner diameters
    from the ASME B36.10M catalog (:mod:`digitalmodel.materials.line_pipe`,
    REUSED). Per candidate:

    - pressure drop via the tracer's Darcy-Weisbach + Swamee-Jain screen
      (:func:`~digitalmodel.field_development.onshore_layout.darcy_weisbach_pressure_drop`,
      REUSED — citations there);
    - arrival pressure = inlet - dp_total, screened against the target;
    - velocity screened against the API RP 14E erosional limit
      (:func:`erosional_velocity_api_rp_14e`, C-factor from config).

    ``selected`` is the smallest passing size (``None`` if none passes).
    """
    if inlet_pressure_kpa <= target_arrival_pressure_kpa:
        raise ValueError(
            "inlet_pressure_kpa must exceed target_arrival_pressure_kpa "
            f"(got {inlet_pressure_kpa} <= {target_arrival_pressure_kpa})"
        )
    cfg = defaults or load_screening_defaults()
    sweep_cfg = cfg["flowline_sweep"]
    c_factor = float(cfg["erosional"]["c_factor"])
    schedule = str(sweep_cfg["schedule"])
    grade = str(sweep_cfg["grade"])
    v_erosional = erosional_velocity_api_rp_14e(density_kg_per_m3, c_factor)

    candidates: list[dict[str, Any]] = []
    selected: Optional[dict[str, Any]] = None
    for nps in sorted(float(n) for n in sweep_cfg["candidate_nps"]):
        pipe = line_pipe(grade, nps=nps, schedule=schedule)
        inner_diameter_m = pipe.id_mm / 1.0e3
        drop = darcy_weisbach_pressure_drop(
            rate_m3_per_s=rate_m3_per_day / SECONDS_PER_DAY,
            length_m=length_m,
            inner_diameter_m=inner_diameter_m,
            roughness_m=roughness_m,
            density_kg_per_m3=density_kg_per_m3,
            viscosity_pa_s=viscosity_pa_s,
            elevation_change_m=elevation_change_m,
        )
        arrival = inlet_pressure_kpa - drop["dp_total_kpa"]
        row = {
            "nps": nps,
            "schedule": schedule,
            "inner_diameter_m": inner_diameter_m,
            "product": pipe.api_5l_reference,
            "velocity_m_per_s": drop["velocity_m_per_s"],
            "erosional_velocity_m_per_s": v_erosional,
            "velocity_ok": drop["velocity_m_per_s"] <= v_erosional,
            "dp_friction_kpa": drop["dp_friction_kpa"],
            "dp_elevation_kpa": drop["dp_elevation_kpa"],
            "dp_total_kpa": drop["dp_total_kpa"],
            "arrival_pressure_kpa": arrival,
            "arrival_ok": arrival >= target_arrival_pressure_kpa,
        }
        row["passes"] = bool(row["velocity_ok"] and row["arrival_ok"])
        candidates.append(row)
        if selected is None and row["passes"]:
            selected = row

    return {
        "rate_m3_per_day": rate_m3_per_day,
        "length_m": length_m,
        "elevation_change_m": elevation_change_m,
        "roughness_m": roughness_m,
        "density_kg_per_m3": density_kg_per_m3,
        "viscosity_pa_s": viscosity_pa_s,
        "inlet_pressure_kpa": inlet_pressure_kpa,
        "target_arrival_pressure_kpa": target_arrival_pressure_kpa,
        "erosional_c_factor": c_factor,
        "erosional_velocity_m_per_s": v_erosional,
        "candidates": candidates,
        "selected": selected,
        "passes": selected is not None,
    }


def sweep_layout_flowlines(
    layout: FieldLayout,
    fluid: dict[str, Any],
    inlet_pressure_kpa: float,
    target_arrival_pressure_kpa: float,
    defaults: Optional[dict[str, Any]] = None,
) -> list[dict[str, Any]]:
    """Run the diameter sweep for every routed flowline of a tracer layout.

    Consumes the PUBLIC attributes of the #1508 tracer's
    :class:`~digitalmodel.field_development.onshore_layout.FlowlineRoute`
    objects (terrain length, endpoint elevation change, rate, roughness) —
    read-only, so this stays decoupled from the parallel layout-schema lane
    (#1509). Each returned sweep dict gains ``id``/``from``/``to`` keys.
    """
    results: list[dict[str, Any]] = []
    for fl in layout.flowlines:
        sweep = flowline_diameter_sweep(
            rate_m3_per_day=fl.rate_m3_per_day,
            length_m=fl.terrain_length_m,
            elevation_change_m=fl.elevation_change_m,
            roughness_m=fl.roughness_m,
            density_kg_per_m3=float(fluid["density_kg_per_m3"]),
            viscosity_pa_s=float(fluid["viscosity_pa_s"]),
            inlet_pressure_kpa=inlet_pressure_kpa,
            target_arrival_pressure_kpa=target_arrival_pressure_kpa,
            defaults=defaults,
        )
        sweep["id"] = fl.flowline_id
        sweep["from"] = fl.from_id
        sweep["to"] = fl.to_id
        results.append(sweep)
    return results


# ---------------------------------------------------------------------------
# Screening report — one-page deterministic markdown
# ---------------------------------------------------------------------------

_CORRELATION_NOTES = (
    "Flowline hydraulics: Darcy-Weisbach with Swamee-Jain friction factor "
    "(Swamee & Jain 1976, J. Hydraulics Div. ASCE 102(5); laminar f = 64/Re). "
    "Single-phase incompressible, screening grade.",
    "Erosional velocity: API RP 14E section 2.5a, Ve = C/sqrt(rho_m) "
    "(field units), C-factor from config.",
    "Pipe inner diameters: ASME B36.10M catalog " "(digitalmodel.materials.line_pipe).",
    "Cable ampacity: IEC 60287-1-1 permissible-current equation reduced to "
    "its conductor-loss-only form with a lumped total thermal resistance. "
    "Screening grade — not a rated design.",
    "Voltage drop: dU = sqrt(3) I L (R' cos(phi) + X' sin(phi)) "
    "(cf. IEC 60364-5-52 Annex G). Conductor constants per IEC 60228.",
)


def _flag(ok: bool) -> str:
    return "PASS" if ok else "FAIL"


def _flowline_section(flowlines: list[dict[str, Any]]) -> list[str]:
    lines = [
        "## Flowline diameter sweep",
        "",
        "| Line | Rate [m3/d] | Length [m] | Selected | ID [mm] | v [m/s] "
        "| Ve 14E [m/s] | dP [kPa] | Arrival [kPa] | Result |",
        "|---|---|---|---|---|---|---|---|---|---|",
    ]
    for fl in flowlines:
        sel = fl.get("selected")
        if sel is not None:
            selected_txt = f"NPS {sel['nps']:g} {sel['schedule']}"
            detail = (
                f"{sel['inner_diameter_m'] * 1e3:.1f} | "
                f"{sel['velocity_m_per_s']:.3f} | "
                f"{sel['erosional_velocity_m_per_s']:.3f} | "
                f"{sel['dp_total_kpa']:.1f} | "
                f"{sel['arrival_pressure_kpa']:.1f}"
            )
        else:
            selected_txt = "none pass"
            detail = "- | - | - | - | -"
        lines.append(
            f"| {fl.get('id', '-')} | {fl['rate_m3_per_day']:.1f} | "
            f"{fl['length_m']:.1f} | {selected_txt} | {detail} | "
            f"{_flag(fl['passes'])} |"
        )
    lines.append("")
    return lines


def _cable_section(cables: list[dict[str, Any]]) -> list[str]:
    lines = [
        "## Cable sizing screen",
        "",
        "| Run | Load [kW] | U [V] | I [A] | Length [m] | Selected [mm2] "
        "| Ampacity [A] | dU [%] | Result |",
        "|---|---|---|---|---|---|---|---|---|",
    ]
    for cb in cables:
        sel = cb.get("selected")
        if sel is not None:
            detail = (
                f"{sel['size_mm2']:g} | {sel['ampacity_a']:.1f} | "
                f"{sel['voltage_drop_pct']:.2f}"
            )
        else:
            detail = "none pass | - | -"
        lines.append(
            f"| {cb.get('id', '-')} | {cb['load_kw']:.1f} | "
            f"{cb['line_voltage_v']:.0f} | {cb['current_a']:.1f} | "
            f"{cb['length_m']:.1f} | {detail} | {_flag(cb['passes'])} |"
        )
    lines.append("")
    return lines


def screening_report(results: dict[str, Any], path: str | Path) -> str:
    """Write a one-page deterministic markdown screening report.

    ``results`` keys (all optional except ``field``):

    - ``field``: layout/iteration name for the title;
    - ``assumptions``: iteration-specific assumption strings (rendered before
      the standard correlation citations);
    - ``flowlines``: list of :func:`flowline_diameter_sweep` dicts (with
      ``id`` keys, e.g. from :func:`sweep_layout_flowlines`);
    - ``cables``: list of :func:`size_cable` dicts (each may carry an ``id``).

    Deterministic by construction: no timestamps, fixed float formatting —
    identical inputs give byte-identical files. Returns the written path.
    """
    lines: list[str] = [
        f"# Screening report — {results['field']}",
        "",
        "Screening-tier sizing (epic #1507, workstream D). NOT a detailed "
        "design; every correlation below is cited and its fidelity limits "
        "are stated in `digitalmodel.field_development.screening`.",
        "",
        "## Assumptions",
        "",
    ]
    for item in results.get("assumptions", []):
        lines.append(f"- {item}")
    for note in _CORRELATION_NOTES:
        lines.append(f"- {note}")
    lines.append("")

    flowlines = results.get("flowlines", [])
    if flowlines:
        lines.extend(_flowline_section(flowlines))
    cables = results.get("cables", [])
    if cables:
        lines.extend(_cable_section(cables))

    overall = (
        all(r["passes"] for r in [*flowlines, *cables])
        if (flowlines or cables)
        else False
    )
    lines.append(f"**Overall: {_flag(overall)}**")
    lines.append("")

    dest = Path(path)
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text("\n".join(lines), encoding="utf-8")
    return str(dest)
