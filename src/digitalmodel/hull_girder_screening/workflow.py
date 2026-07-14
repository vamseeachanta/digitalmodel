"""Durable workflow: hull-girder longitudinal strength screening.

Wires :mod:`digitalmodel.naval_architecture.hull_girder_screening` into a
registry workflow: still-water shear-force / bending-moment integration from a
loading condition (lightship distribution + tank/point weights), utilisation
against user-supplied allowable SF/BM curves interpolated at frame positions,
and an optional hull-girder section-modulus check (scantlings or a
class-approved SM value).

SCREENING TIER ONLY: ranks and gates loading conditions. Class-compliance
conclusions still require the approved loading instrument or a class-endorsed
calculation; allowable curves and approved SM values are project inputs from
the approved loading manual.

Config schema (YAML basename ``hull_girder_screening``)::

    basename: hull_girder_screening
    hull_girder_screening:
      length_m: 100.0
      n_stations: 401                 # optional (default 401)
      water_density_t_m3: 1.025       # optional
      weights:
        lightship:                    # DistributedWeight blocks
          - {name: hull, weight_t: 2000.0, x_start_m: 0.0, x_end_m: 100.0}
        tanks:                        # more DistributedWeight blocks
          - {name: WBT1, weight_t: 500.0, x_start_m: 10.0, x_end_m: 25.0,
             lcg_m: 17.0}             # lcg_m optional (default mid-extent)
        point_weights:
          - {name: crane, weight_t: 50.0, x_m: 60.0, extent_m: 4.0}
      buoyancy:
        method: box                   # box | hydrostatics_table | direct
        # hydrostatics_table:
        #   stations:
        #     - {x_m: 0.0, drafts_m: [0.0, 8.0], areas_m2: [0.0, 120.0]}
        # direct:
        #   x_m: [...]
        #   buoyancy_per_m_t: [...]
        closure_correction: true      # optional (default true)
        closure_tolerance: 0.005      # optional; raw end-residual gate
      allowables:                     # optional; magnitudes, interpolated
        shear_force:                  # still-water (harbour) allowables
          x_m: [0.0, 25.0, 50.0, 75.0, 100.0]
          positive_t: [800.0, 3000.0, 3000.0, 3000.0, 800.0]
          negative_t: [800.0, 3000.0, 3000.0, 3000.0, 800.0]  # optional
        bending_moment:
          x_m: [0.0, 50.0, 100.0]
          hogging_t_m: [20000.0, 80000.0, 20000.0]
          sagging_t_m: [20000.0, 70000.0, 20000.0]            # optional
        total_shear_force:            # seagoing (sw + wave) allowables;
          x_m: [0.0, 50.0, 100.0]     # requires the wave_loads block
          positive_t: [2000.0, 5000.0, 2000.0]
          negative_t: [2000.0, 5000.0, 2000.0]                # optional
        total_bending_moment:
          x_m: [0.0, 50.0, 100.0]
          hogging_t_m: [40000.0, 160000.0, 40000.0]
          sagging_t_m: [40000.0, 150000.0, 40000.0]           # optional
      wave_loads:                     # optional; IACS UR S11 rule wave BM/SF
        beam_m: 20.0                  # envelopes (screening; loading manual
        block_coefficient: 0.9        # governs). Cb floored at 0.6 per S11.
        # length_m: 100.0             # rule length L (default: hull length)
      frames:                         # optional reporting positions
        - {name: "Fr 20", x_m: 25.0}
      utilization_limit: 1.0          # optional
      section_modulus:                # optional
        yield_mpa: 235.0              # or allowable via IACS 175/k
        approved_sm: {deck_m3: 5.5, keel_m3: 6.1, source: "loading manual"}
        # or scantlings:
        #   depth_m: 8.0
        #   elements:
        #     - {name: deck, breadth_m: 20.0, thickness_m: 0.02, z_m: 7.99}
        #     - {name: side_p, height_m: 8.0, thickness_m: 0.012, z_m: 4.0}
        #     - {name: bottom, breadth_m: 20.0, thickness_m: 0.022, z_m: 0.011}
        # wave: {beam_m: 20.0, block_coefficient: 0.9}  # optional S11 combo
      output_dir: results

Outputs: a station table CSV (x, weight/m, buoyancy/m, SF, BM — plus the S11
wave and total envelopes when ``wave_loads`` is set), a still-water frame
utilisation CSV, a total (sw + wave) utilisation CSV when ``wave_loads`` is
set, and a one-row summary CSV — all report_pack-ready — plus the same
content in the returned config under ``hull_girder_screening``.

Utilisation modes: the plain ``shear_force`` / ``bending_moment`` allowables
are the still-water (harbour) limits and gate the still-water SF/BM alone;
the ``total_*`` allowables are the seagoing permissible values from the
approved loading manual and gate ``|still-water + S11 wave|`` per sign.

Sign convention: x from the aft end; BM > 0 = hogging, BM < 0 = sagging.
Units: t, t/m, t·m (kN·m in the summary via g = 9.81).
"""

from __future__ import annotations

import csv
from dataclasses import asdict
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.naval_architecture.hull_girder_screening import (
    DEFAULT_CLOSURE_TOLERANCE,
    G_M_S2,
    RHO_SEAWATER_T_M3,
    AllowableCurve,
    DistributedWeight,
    HydroStation,
    PointWeight,
    SectionElement,
    WaveLoads,
    build_weight_curve,
    buoyancy_box,
    buoyancy_direct,
    buoyancy_hydrostatics_table,
    rule_wave_loads,
    section_modulus_screen,
    section_properties,
    still_water_sf_bm,
    total_utilization_at_frames,
    utilization_at_frames,
)

REPO_ROOT = Path(__file__).resolve().parents[3]

GOVERNANCE_NOTE = (
    "Screening tier only: ranks/gates loading conditions. Class-compliance "
    "conclusions require the approved loading instrument or a class-endorsed "
    "calculation."
)


def router(cfg: dict) -> dict:
    settings = cfg.get("hull_girder_screening") or {}
    length = _positive_float(settings, "length_m")
    n_stations = int(settings.get("n_stations", 401))
    if n_stations < 11:
        raise ValueError("hull_girder_screening n_stations must be >= 11")
    x = np.linspace(0.0, length, n_stations)

    distributed, points = _weights(settings)
    w_per_m = build_weight_curve(x, distributed, points)

    b_per_m, equilibrium = _buoyancy(settings, x, w_per_m)
    buoyancy_cfg = settings.get("buoyancy") or {}
    result = still_water_sf_bm(
        x,
        w_per_m,
        b_per_m,
        closure_correction=bool(buoyancy_cfg.get("closure_correction", True)),
        closure_tolerance=float(
            buoyancy_cfg.get("closure_tolerance", DEFAULT_CLOSURE_TOLERANCE)
        ),
        equilibrium=equilibrium,
    )

    allowable_sf = _allowable(settings, "shear_force", "positive_t", "negative_t")
    allowable_bm = _allowable(settings, "bending_moment", "hogging_t_m", "sagging_t_m")
    allowable_sf_total = _allowable(
        settings, "total_shear_force", "positive_t", "negative_t"
    )
    allowable_bm_total = _allowable(
        settings, "total_bending_moment", "hogging_t_m", "sagging_t_m"
    )
    frames = _frames(settings, allowable_sf, allowable_bm, length)
    limit = float(settings.get("utilization_limit", 1.0))
    utilization = utilization_at_frames(
        result, frames, allowable_sf, allowable_bm, utilization_limit=limit
    )

    wave = _wave_loads(settings, x, length)
    if wave is None and (allowable_sf_total or allowable_bm_total):
        raise ValueError(
            "hull_girder_screening total allowables require the wave_loads "
            "block (IACS UR S11 wave BM/SF inputs)"
        )
    total_utilization = (
        total_utilization_at_frames(
            result, wave, frames, allowable_sf_total, allowable_bm_total,
            utilization_limit=limit,
        )
        if wave is not None
        else []
    )

    sm_screens, sm_properties = _section_modulus(settings, result, wave)

    status = _screening_status(result, utilization, total_utilization, sm_screens)
    summary = _summary(
        result, utilization, total_utilization, wave, sm_screens, status, limit
    )

    stations_rows = [
        {
            "x_m": xi, "weight_per_m_t": wi, "buoyancy_per_m_t": bi,
            "shear_force_t": si, "bending_moment_t_m": mi,
        }
        for xi, wi, bi, si, mi in zip(
            result.x_m, result.weight_per_m_t, result.buoyancy_per_m_t,
            result.shear_force_t, result.bending_moment_t_m,
        )
    ]
    if wave is not None:
        for row, wv_hog, wv_sag, wv_sf_pos, wv_sf_neg in zip(
            stations_rows, wave.hogging_t_m, wave.sagging_t_m,
            wave.shear_positive_t, wave.shear_negative_t,
        ):
            row.update(
                {
                    "wave_hogging_t_m": wv_hog,
                    "wave_sagging_t_m": wv_sag,
                    "wave_shear_positive_t": wv_sf_pos,
                    "wave_shear_negative_t": wv_sf_neg,
                    "total_hogging_t_m": row["bending_moment_t_m"] + wv_hog,
                    "total_sagging_t_m": row["bending_moment_t_m"] + wv_sag,
                    "total_shear_positive_t": row["shear_force_t"] + wv_sf_pos,
                    "total_shear_negative_t": row["shear_force_t"] + wv_sf_neg,
                }
            )
    utilization_rows = [asdict(row) for row in utilization]
    total_utilization_rows = [asdict(row) for row in total_utilization]

    stations_csv = _output_path(cfg, settings, "hull_girder_stations.csv")
    utilization_csv = _output_path(cfg, settings, "hull_girder_utilization.csv")
    summary_csv = _output_path(cfg, settings, "hull_girder_summary.csv")
    _write_csv(stations_csv, stations_rows)
    _write_csv(utilization_csv, utilization_rows)
    _write_csv(summary_csv, [summary])
    total_utilization_csv = None
    if total_utilization_rows:
        total_utilization_csv = _output_path(
            cfg, settings, "hull_girder_total_utilization.csv"
        )
        _write_csv(total_utilization_csv, total_utilization_rows)

    cfg["hull_girder_screening"] = {
        "length_m": length,
        "n_stations": n_stations,
        "equilibrium": result.equilibrium,
        **summary,
        "stations": stations_rows,
        "utilization": utilization_rows,
        "total_utilization": total_utilization_rows,
        "wave_loads": None if wave is None else {
            "length_m": wave.length_m,
            "beam_m": wave.beam_m,
            "block_coefficient": wave.block_coefficient,
            "rule_block_coefficient": wave.rule_block_coefficient,
            "wave_coefficient": wave.wave_coefficient,
            "applicability_note": wave.applicability_note,
            "code_reference": "IACS UR S11 (Rev.7, 2010) 2.2.1-2.2.2",
        },
        "section_modulus": [
            {**asdict(screen), "check": asdict(screen.check)}
            for screen in sm_screens
        ],
        "section_properties": sm_properties,
        "stations_csv": _display_path(stations_csv),
        "utilization_csv": _display_path(utilization_csv),
        "total_utilization_csv": (
            None if total_utilization_csv is None
            else _display_path(total_utilization_csv)
        ),
        "summary_csv": _display_path(summary_csv),
        "governance": GOVERNANCE_NOTE,
    }
    cfg["screening_status"] = status
    return cfg


# --------------------------------------------------------------------------- #
# Input parsing
# --------------------------------------------------------------------------- #
def _weights(settings: dict) -> tuple[list[DistributedWeight], list[PointWeight]]:
    raw = settings.get("weights")
    if not isinstance(raw, dict):
        raise ValueError("hull_girder_screening weights mapping is required")
    distributed: list[DistributedWeight] = []
    for group in ("lightship", "tanks"):
        for item in raw.get(group) or []:
            distributed.append(
                DistributedWeight(
                    name=str(item["name"]),
                    weight_t=float(item["weight_t"]),
                    x_start_m=float(item["x_start_m"]),
                    x_end_m=float(item["x_end_m"]),
                    lcg_m=None if item.get("lcg_m") is None else float(item["lcg_m"]),
                )
            )
    points = [
        PointWeight(
            name=str(item["name"]),
            weight_t=float(item["weight_t"]),
            x_m=float(item["x_m"]),
            extent_m=None if item.get("extent_m") is None else float(item["extent_m"]),
        )
        for item in raw.get("point_weights") or []
    ]
    if not distributed and not points:
        raise ValueError("hull_girder_screening weights are empty")
    return distributed, points


def _buoyancy(
    settings: dict, x: np.ndarray, w_per_m: np.ndarray
) -> tuple[np.ndarray, dict]:
    raw = settings.get("buoyancy")
    if not isinstance(raw, dict):
        raise ValueError("hull_girder_screening buoyancy mapping is required")
    method = str(raw.get("method", "")).lower()
    if method == "box":
        return buoyancy_box(x, w_per_m)
    if method == "hydrostatics_table":
        stations_raw = raw.get("stations")
        if not isinstance(stations_raw, list) or len(stations_raw) < 2:
            raise ValueError(
                "hull_girder_screening buoyancy.stations needs >= 2 stations"
            )
        stations = [
            HydroStation(
                x_m=float(s["x_m"]),
                drafts_m=tuple(float(v) for v in s["drafts_m"]),
                areas_m2=tuple(float(v) for v in s["areas_m2"]),
            )
            for s in stations_raw
        ]
        rho = float(settings.get("water_density_t_m3", RHO_SEAWATER_T_M3))
        return buoyancy_hydrostatics_table(x, w_per_m, stations, rho)
    if method == "direct":
        return buoyancy_direct(
            x,
            np.asarray([float(v) for v in raw.get("x_m") or []]),
            np.asarray([float(v) for v in raw.get("buoyancy_per_m_t") or []]),
        )
    raise ValueError(
        "hull_girder_screening buoyancy.method must be box, "
        "hydrostatics_table or direct"
    )


def _allowable(
    settings: dict, key: str, positive_key: str, negative_key: str
) -> AllowableCurve | None:
    raw = (settings.get("allowables") or {}).get(key)
    if raw is None:
        return None
    if not isinstance(raw, dict):
        raise ValueError(f"hull_girder_screening allowables.{key} must be a mapping")
    x_m = tuple(float(v) for v in raw.get("x_m") or [])
    positive = tuple(float(v) for v in raw.get(positive_key) or [])
    negative_raw = raw.get(negative_key)
    negative = (
        positive if negative_raw is None else tuple(float(v) for v in negative_raw)
    )
    return AllowableCurve(x_m=x_m, positive=positive, negative=negative)


def _frames(
    settings: dict,
    allowable_sf: AllowableCurve | None,
    allowable_bm: AllowableCurve | None,
    length: float,
) -> list[tuple[str, float]]:
    raw = settings.get("frames")
    if raw:
        return [(str(item["name"]), float(item["x_m"])) for item in raw]
    # Default: the union of the allowable-curve knots, else 21 even stations.
    knots: set[float] = set()
    for curve in (allowable_sf, allowable_bm):
        if curve:
            knots.update(curve.x_m)
    positions = sorted(knots) if knots else list(np.linspace(0.0, length, 21))
    return [(f"x={xi:g} m", float(xi)) for xi in positions]


def _wave_loads(settings: dict, x, length: float) -> WaveLoads | None:
    raw = settings.get("wave_loads")
    if raw is None:
        return None
    if not isinstance(raw, dict):
        raise ValueError("hull_girder_screening wave_loads must be a mapping")
    beam = _positive_float(raw, "beam_m", "wave_loads.beam_m")
    cb = _positive_float(raw, "block_coefficient", "wave_loads.block_coefficient")
    rule_length = (
        length
        if raw.get("length_m") is None
        else _positive_float(raw, "length_m", "wave_loads.length_m")
    )
    return rule_wave_loads(x, beam, cb, length_m=rule_length)


def _section_modulus(
    settings: dict, result, wave: WaveLoads | None = None
) -> tuple[list, dict]:
    raw = settings.get("section_modulus")
    if raw is None:
        return [], {}
    if not isinstance(raw, dict):
        raise ValueError("hull_girder_screening section_modulus must be a mapping")
    yield_mpa = _positive_float(raw, "yield_mpa", "section_modulus.yield_mpa")
    approved = raw.get("approved_sm")
    scantlings = raw.get("scantlings")
    if (approved is None) == (scantlings is None):
        raise ValueError(
            "hull_girder_screening section_modulus needs exactly one of "
            "approved_sm or scantlings"
        )
    properties: dict = {}
    if approved is not None:
        sm_deck = _positive_float(approved, "deck_m3", "approved_sm.deck_m3")
        sm_keel = (
            None
            if approved.get("keel_m3") is None
            else float(approved["keel_m3"])
        )
        source = f"approved: {approved.get('source', 'unspecified')}"
    else:
        elements = [_element(item) for item in scantlings.get("elements") or []]
        properties = section_properties(
            elements,
            depth_m=(
                None
                if scantlings.get("depth_m") is None
                else float(scantlings["depth_m"])
            ),
        )
        sm_deck = properties["sm_deck_m3"]
        sm_keel = properties["sm_keel_m3"]
        source = "scantlings"
    # The SM screen's own wave block wins; otherwise reuse the S11 inputs
    # from the top-level wave_loads block so they are supplied only once.
    wave_cfg = raw.get("wave")
    if wave_cfg is None and wave is not None:
        wave_cfg = {
            "length_m": wave.length_m,
            "beam_m": wave.beam_m,
            "block_coefficient": wave.block_coefficient,
        }
    screens = section_modulus_screen(
        result, sm_deck, sm_keel, yield_mpa, source, wave=wave_cfg
    )
    return screens, properties


def _element(item: dict) -> SectionElement:
    name = str(item.get("name", "element"))
    z_m = float(item["z_m"])
    if item.get("area_m2") is not None:
        return SectionElement(
            name=name, z_m=z_m, area_m2=float(item["area_m2"]),
            i_self_m4=float(item.get("i_self_m4", 0.0)),
        )
    if item.get("breadth_m") is not None:
        return SectionElement.horizontal(
            name, float(item["breadth_m"]), float(item["thickness_m"]), z_m
        )
    if item.get("height_m") is not None:
        return SectionElement.vertical(
            name, float(item["height_m"]), float(item["thickness_m"]), z_m
        )
    raise ValueError(
        f"section element {name!r} needs area_m2, breadth_m or height_m"
    )


# --------------------------------------------------------------------------- #
# Status / summary
# --------------------------------------------------------------------------- #
def _screening_status(result, utilization, total_utilization, sm_screens) -> str:
    if not result.closure_ok:
        return "fail"
    if any(row.status == "fail" for row in utilization):
        return "fail"
    if any(row.status == "fail" for row in total_utilization):
        return "fail"
    if any(not screen.check.passes for screen in sm_screens):
        return "fail"
    return "pass"


def _summary(
    result, utilization, total_utilization, wave, sm_screens, status, limit
) -> dict[str, Any]:
    sf_utils = [r.shear_utilization for r in utilization if r.shear_utilization is not None]
    bm_utils = [r.bending_utilization for r in utilization if r.bending_utilization is not None]
    total_sf_utils = [
        r.shear_utilization for r in total_utilization
        if r.shear_utilization is not None
    ]
    total_bm_utils = [
        r.bending_utilization for r in total_utilization
        if r.bending_utilization is not None
    ]
    sm_utils = [s.check.utilization for s in sm_screens]
    wave_summary: dict[str, Any] = {}
    if wave is not None:
        wave_summary = {
            "wave_coefficient": wave.wave_coefficient,
            "rule_block_coefficient": wave.rule_block_coefficient,
            "wave_hogging_amidships_t_m": max(wave.hogging_t_m),
            "wave_sagging_amidships_t_m": min(wave.sagging_t_m),
            "wave_applicability_note": wave.applicability_note,
        }
    return {
        "method": "still_water_sf_bm_screening",
        "displacement_t": result.displacement_t,
        "lcg_m": result.lcg_m,
        "max_shear_t": result.max_shear_t,
        "x_max_shear_m": result.x_max_shear_m,
        "max_shear_kn": result.max_shear_t * G_M_S2,
        "max_hogging_t_m": result.max_hogging_t_m,
        "x_max_hogging_m": result.x_max_hogging_m,
        "max_hogging_kn_m": result.max_hogging_t_m * G_M_S2,
        "max_sagging_t_m": result.max_sagging_t_m,
        "x_max_sagging_m": result.x_max_sagging_m,
        "max_sagging_kn_m": result.max_sagging_t_m * G_M_S2,
        "closure_shear_fraction": result.closure_shear_fraction,
        "closure_moment_fraction": result.closure_moment_fraction,
        "closure_ok": result.closure_ok,
        **wave_summary,
        "max_shear_utilization": max(sf_utils) if sf_utils else None,
        "max_bending_utilization": max(bm_utils) if bm_utils else None,
        "max_total_shear_utilization": (
            max(total_sf_utils) if total_sf_utils else None
        ),
        "max_total_bending_utilization": (
            max(total_bm_utils) if total_bm_utils else None
        ),
        "max_section_modulus_utilization": max(sm_utils) if sm_utils else None,
        "utilization_limit": limit,
        "screening_status": status,
    }


# --------------------------------------------------------------------------- #
# Shared plumbing (sibling-module pattern)
# --------------------------------------------------------------------------- #
def _positive_float(source: dict, name: str, label: str | None = None) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"hull_girder_screening {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"hull_girder_screening {label} must be positive")
    return value


def _output_path(cfg: dict, settings: dict, suffix: str) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_{suffix}"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "hull_girder_screening"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("hull_girder_screening cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
