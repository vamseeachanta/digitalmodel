"""Drilling-riser operability atlas — exact node-cache (#1283, epic #1279 child E).

Precomputes the OPEN ANALYTICAL operating envelope
(:func:`digitalmodel.drilling_riser.envelope.compute_operating_envelope`,
``dynamic=False``) as an EXACT node grid, served O(1) for the monitoring surface,
go/no-go, and the capabilities explorer (#1284).

Design (per the #1283 T2 review):
  * response = ``governing_utilisation`` — the max utilisation over the mode's
    active limits at a node; operable iff <= 1.0.
  * ONE categorical axis ``config`` = generic ``"{stackup}__{mode}"`` tokens;
    continuous axes ``offset_pct`` + ``current_speed_mps`` ONLY (Hs/Tp are dead in
    the static tier — they enter only via RAO terms, which are zero here — and a
    metocean basin is not a physics dimension).
  * criteria are the PUBLIC published standard factors (API RP 16Q flex-joint
    limits, API STD 2RD von-Mises design factor) passed explicitly, so the build
    is OFFLINE and deterministic (no wiki getter resolution, no licensed table).

This is the C3-style dedicated-build path (a committed atlas + a
``parametric_query`` registry row), NOT the RESPONSE_FUNCS/refresh compute-row
path — the latter would need a wiki-dependent compute-row durable test and break
public CI. Staleness is guarded by :func:`content_fingerprint` + a determinism
test, not the refresh drift gate.
"""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import yaml

from digitalmodel.drilling_riser.envelope import (
    DEFAULT_MOONPOOL_CLEARANCE_MARGIN_M,
    EnvelopeCriteria,
    OperatingMode,
    RigEnvelopeLimits,
    RiserSection,
    SeaState,
    compute_operating_envelope,
)
from digitalmodel.parametric.atlas import Atlas, Axis
from digitalmodel.parametric.generate import _holdout_points

REPO_ROOT = Path(__file__).resolve().parents[3]
BASENAME = "drilling_riser_operability"
RESPONSE = "governing_utilisation"
PHYSICS = "linear"
TOLERANCE = 0.10  # held-out interpolation-error gate (screening; boundary escalates)
#: Governing utilisation is stored EXACT in [0, ceiling]; values above the
#: ceiling are clipped. Every clipped value is >> 1.0 and stays INOPERABLE, so the
#: clip changes no operable/inoperable verdict and preserves the entire
#: decision-relevant band around 1.0 exactly — it only keeps the served surface
#: interpolation-clean deep in the inoperable zone (where the exact magnitude is
#: not decision-relevant). The consumer never decides operability by interpolation
#: near the boundary (it escalates within the local-error band of 1.0).
UTILISATION_CEILING = 2.0

CONFIG_PATH = Path(__file__).with_name("operability_configs.yml")

#: Reviewed generic-token allowlist (governance): the built atlas's ``config``
#: axis values MUST equal exactly this set — no client token can enter via a YAML
#: edit without also editing this constant (which is code-reviewed).
ALLOWED_STACKUPS = ("rsu_a", "rsu_b")
ALLOWED_MODES = ("drilling", "connected", "hang_off")
ALLOWED_TOKENS = tuple(
    f"{s}__{m}" for s in ALLOWED_STACKUPS for m in ALLOWED_MODES
)

#: Coarse public band grids (k-anonymity): every config numeric constant + axis
#: knot must land on these bands, so no real project depth/tension is encoded.
BAND_GRID = {
    "water_depth_m": 500.0,      # 500 m depth bands
    "length_m": 500.0,
    "tension_n": 0.5e6,          # 0.5 MN tension bands
    "tj_stroke_m": 1.0,
    "moonpool_half_min_m": 1.0,
    "offset_pct": 1.0,           # continuous-axis knot bands
    "current_speed_mps": 0.5,
}
#: Nominal OD/wt allowed (nominal riser sizes, not project-specific).
ALLOWED_OD_M = (0.5334,)
ALLOWED_WT_M = (0.0254,)

#: Response-relevant source: a change to any of these can move the grid.
SOURCE_FILES = (
    "src/digitalmodel/drilling_riser/envelope.py",
    "src/digitalmodel/drilling_riser/riser_response.py",
    "src/digitalmodel/drilling_riser/conductor_response.py",
    "src/digitalmodel/drilling_riser/operability.py",
    "src/digitalmodel/orcaflex/code_check_engine.py",
    "src/digitalmodel/drilling_riser/envelope_modes.yml",
    "src/digitalmodel/drilling_riser/operability_atlas.py",
)


def load_config(path: Path = CONFIG_PATH) -> dict[str, Any]:
    return yaml.safe_load(Path(path).read_text())


def _tokens(cfg: dict[str, Any]) -> list[str]:
    return [f"{s}__{m}" for s in cfg["stackups"] for m in cfg["modes"]]


def governing_utilisation(point: dict[str, Any], *, cfg: dict[str, Any]) -> float:
    """Exact governing utilisation (max over the mode's active limits) at one
    ``(config, offset_pct, current_speed_mps)`` node. Raises on an all-NaN node."""
    token = str(point["config"])
    stackup_name, mode_name = token.split("__", 1)
    st = cfg["stackups"][stackup_name]
    crit = cfg["criteria"]
    result = compute_operating_envelope(
        section=RiserSection(
            outer_diameter_m=float(st["outer_diameter_m"]),
            wall_thickness_m=float(st["wall_thickness_m"]),
        ),
        water_depth_m=float(st["water_depth_m"]),
        length_m=float(st["length_m"]),
        tension_n=float(st["tension_n"]),
        criteria=EnvelopeCriteria(
            float(crit["flexjoint_angle_mean_deg"]),
            float(crit["flexjoint_angle_max_deg"]),
            float(crit["von_mises_design_factor"]),
        ),
        offsets_pct=[float(point["offset_pct"])],
        current_speeds_mps=[float(point["current_speed_mps"])],
        seastates=[SeaState(hs_m=0.0, tp_s=1.0)],  # static tier: Hs enters only via zero RAO
        # moonpool clearance (a geometric vessel-excursion limit) is omitted from
        # the generic configs: at %-of-deepwater offsets it dominates trivially, so
        # the atlas exercises the riser-mechanics limits (flex-joint angle,
        # von-Mises stress, tensioner stroke). Any moonpool_half_min_m in the config
        # is passed through (and must be > DEFAULT_MOONPOOL_CLEARANCE_MARGIN_M).
        rig_limits=RigEnvelopeLimits(
            tj_stroke_m=float(st["tj_stroke_m"]),
            moonpool_half_min_m=(
                float(st["moonpool_half_min_m"]) if "moonpool_half_min_m" in st else None
            ),
        ),
        mode=OperatingMode(mode_name),
    )
    cell = [result.per_limit_utilisation[k][0, 0, 0] for k in result.per_limit_utilisation]
    with np.errstate(invalid="ignore"):
        if np.all(np.isnan(cell)):
            raise ValueError(f"all-NaN governing node for {point!r} — no active limit computed")
        return float(min(np.nanmax(cell), UTILISATION_CEILING))


def content_fingerprint(cfg_path: Path = CONFIG_PATH, repo_root: Path = REPO_ROOT) -> str:
    """Hash of everything the atlas depends on: config YAML + response source +
    standard editions. A change to any flips the fingerprint (a determinism test
    asserts the committed atlas matches)."""
    cfg = load_config(cfg_path)
    basis = {
        "config_yaml": Path(cfg_path).read_bytes().decode(),
        "standards": cfg["provenance"]["standards"],
        "source_sha256": {
            rel: hashlib.sha256((repo_root / rel).read_bytes()).hexdigest()
            for rel in SOURCE_FILES
        },
        "spec": {"basename": BASENAME, "response": RESPONSE, "physics": PHYSICS},
    }
    return hashlib.sha256(json.dumps(basis, sort_keys=True).encode()).hexdigest()


def _axes(cfg: dict[str, Any]) -> list[Axis]:
    return [
        Axis(name="config", values=_tokens(cfg)),
        Axis(name="offset_pct", scale="linear", grid=[float(v) for v in cfg["axes"]["offset_pct"]]),
        Axis(name="current_speed_mps", scale="linear",
             grid=[float(v) for v in cfg["axes"]["current_speed_mps"]]),
    ]


def build_atlas(cfg_path: Path = CONFIG_PATH, repo_root: Path = REPO_ROOT) -> Atlas:
    """Build the exact operability atlas offline (no wiki). Deterministic."""
    cfg = load_config(cfg_path)
    axes = _axes(cfg)
    import itertools

    names = [ax.name for ax in axes]
    value_lists = [ax.values if ax.is_categorical else ax.grid for ax in axes]
    rows = []
    for combo in itertools.product(*value_lists):
        point = dict(zip(names, combo))
        rows.append({**point, RESPONSE: governing_utilisation(point, cfg=cfg)})
    grid = pd.DataFrame(rows)

    fp = content_fingerprint(cfg_path, repo_root)
    atlas = Atlas(
        basename=BASENAME,
        atlas_id=fp[:12],
        physics=PHYSICS,
        response=RESPONSE,
        axes=axes,
        grid=grid,
        provenance={
            "basename": BASENAME,
            "kind": "computed",
            "standards": cfg["provenance"]["standards"],
            "content_fingerprint": fp,
            "tier": "analytical-static",
            "disclaimer": (
                "Static analytical operability surface (dynamic amplification, "
                "drift-off and VIV are the solver tier). Screening estimate — a "
                "full on-demand run remains the document of record."
            ),
        },
    )

    # held-out interior validation + per-interval local-error map (mirrors
    # generate_atlas so the query path's local_error/straddle rail works).
    worst = 0.0
    samples: list[dict[str, Any]] = []
    local_error_map: dict[str, dict[str, list[float]]] = {}
    for entry in _holdout_points(axes):
        point = entry["point"]
        predicted = atlas.predict(point)
        actual = governing_utilisation(point, cfg=cfg)
        rel = abs(predicted.value - actual) / actual if actual else 0.0
        worst = max(worst, rel)
        samples.append({"point": point, "predicted": predicted.value, "actual": actual,
                        "rel_error": rel, "in_range": predicted.in_range})
        local_error_map.setdefault(entry["cat_key"], {}).setdefault(entry["axis"], []).append(rel)
    atlas.validation = {
        "metric": "max_rel_error",
        "max_rel_error": worst,
        "threshold": TOLERANCE,
        "passes": worst <= TOLERANCE,
        "n_holdout": len(samples),
        "worst_samples": sorted(samples, key=lambda s: -s["rel_error"])[:15],
        "local_error_map": local_error_map,
    }
    return atlas
