#!/usr/bin/env python3
# ABOUTME: GTM Demo 1 — DNV Freespan/VIV Analysis
# ABOUTME: Parametric screening: ~680 cases across pipelines and jumpers per DNV-RP-F105
"""
GTM Demo 1: DNV Freespan / VIV Screening Analysis
====================================================

Runs parametric freespan VIV screening per DNV-RP-F105 simplified methodology:
  - 3 pipeline sizes (8", 12", 16") x 8 spans x 5 currents x 4 gap ratios = 480 cases
  - 1 jumper (8" rigid) x 8 spans x 5 currents x 5 gap ratios = 200 cases
  - Total: ~680 parametric cases

Produces:
  - 5 interactive Plotly charts
  - Branded HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_01_dnv_freespan_viv.py

    # Reuse cached results (skip recalculation):
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_01_dnv_freespan_viv.py --from-cache

    # Force recalculation even if cache exists:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_01_dnv_freespan_viv.py --force
"""

from __future__ import annotations

import argparse
import json
import logging
import math
import re
import sys
import time
import warnings
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Imports — graceful handling
# ---------------------------------------------------------------------------
try:
    import numpy as np
    import pandas as pd
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
except ImportError as exc:
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: uv pip install numpy pandas plotly")
    sys.exit(1)

try:
    from report_template import GTMReportBuilder, COLORS, CHART_PALETTE
except ImportError:
    try:
        from examples.demos.gtm.report_template import GTMReportBuilder, COLORS, CHART_PALETTE
    except ImportError as exc:
        print(f"[ERROR] Cannot import report template: {exc}")
        print("        Ensure PYTHONPATH includes 'examples/demos/gtm' directory.")
        sys.exit(1)

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
logging.basicConfig(
    level=logging.WARNING,
    format="%(levelname)s | %(name)s | %(message)s",
)
logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "data"
OUTPUT_DIR = SCRIPT_DIR / "output"
RESULTS_DIR = SCRIPT_DIR / "results"
INPUTS_DIR = SCRIPT_DIR / "inputs"

# Committed default Sweep Config — the Baseline Run (reproduces today's exact 680-case
# matrix). Loaded only inside the recompute branch so --from-cache works without it (NBC-1).
DEFAULT_SWEEP_CONFIG_PATH = INPUTS_DIR / "demo_01_freespan.yml"

# Physical constants
SEAWATER_DENSITY = 1025.0       # kg/m3
GRAVITY = 9.80665               # m/s2
STEEL_DENSITY = 7850.0          # kg/m3
STEEL_YOUNGS_MODULUS = 207e9    # Pa
COATING_DENSITY = 950.0         # kg/m3
CONTENT_DENSITY = 800.0         # kg/m3 (oil-filled, conservative for freespan)

# DNV-RP-F105 beam boundary condition coefficients
C_N_PINNED = 3.5596             # Pinned-pinned first mode (pi^2)
C_N_FIXED = 22.373              # Fixed-fixed first mode
C_N_DEFAULT = C_N_PINNED        # Conservative: assume pinned supports

# Added mass coefficient (potential flow around cylinder)
CA_DEFAULT = 1.0

# VIV onset thresholds per DNV-RP-F105
VR_ONSET_IL = 1.0               # In-line VIV onset reduced velocity
VR_ONSET_CF = 3.0               # Cross-flow VIV onset reduced velocity
VR_LOCKIN_LOW = 4.0             # Lock-in range lower bound
VR_LOCKIN_HIGH = 8.0            # Lock-in range upper bound

# Safety factor for fatigue screening
GAMMA_F = 1.3

# Parameter matrix — Pipelines
PIPELINE_SIZES = ["8in", "12in", "16in"]
PIPELINE_SPAN_LENGTHS_M = [10, 20, 30, 40, 50, 60, 70, 80]
CURRENT_VELOCITIES_MS = [0.2, 0.4, 0.6, 0.8, 1.0]
PIPELINE_GAP_RATIOS = [0.5, 1.0, 2.0, 5.0]

# Parameter matrix — Jumpers
JUMPER_SPAN_LENGTHS_M = [5, 10, 15, 20, 25, 30, 35, 40]
JUMPER_GAP_RATIOS = [0.5, 1.0, 2.0, 5.0, float("inf")]  # inf = mid-water

# The exact 15 keys every JSON result case must carry (frozen golden contract, ADR-0002).
# The Results Store enriches the DataFrame with extra columns but MUST NOT mutate these dicts;
# a guard in main() asserts this set before save_results_json so a mutation regression fails loudly.
FROZEN_RESULT_KEYS = frozenset({
    "pipe_type", "nominal_size", "od_m", "wt_m", "span_m", "v_current_ms",
    "e_over_d", "f_n_hz", "v_r", "a_over_d", "status", "max_allowable_span_m",
    "m_eff_kg_per_m", "case_id", "schedule",
})

# Default run identifier for the committed Baseline Run (the reference seed for the Store).
BASELINE_RUN_ID = "baseline"

# Allowed run_id character set. A run_id becomes a filesystem directory name under the Store
# (<base_dir>/parametric/demo_01/<run_id>/) and the per-run report dir, so it must be a single
# safe path segment: no separators, no "." / ".." traversal, no empty string (D1, path-traversal).
_RUN_ID_RE = re.compile(r"^[A-Za-z0-9._-]+$")


def validate_run_id(run_id: str) -> str:
    """Validate a run_id is a single safe path segment; return it unchanged if valid.

    Rejects empty string, ".", "..", and anything containing a character outside
    ``[A-Za-z0-9._-]`` (e.g. "/" or "\\" path separators). Raises ValueError with a clean
    message on rejection. "baseline" and ordinary names like "acme_q2" are accepted.
    """
    if not isinstance(run_id, str) or run_id in ("", ".", "..") or not _RUN_ID_RE.match(run_id):
        raise ValueError(
            f"invalid --run-id {run_id!r}: must match [A-Za-z0-9._-]+ and may not be "
            "'', '.', or '..' (it becomes a directory name in the Results Store)."
        )
    return run_id

# Demo identifier — the per-run report/store partition name (matches results_store.DEMO_ID).
DEMO_ID = "demo_01"

# Status labels
STATUS_PASS = "PASS"
STATUS_INLINE_ONLY = "INLINE_ONLY"
STATUS_FAIL_CF = "FAIL_CF"
STATUS_FAIL_LOCKIN = "FAIL_LOCKIN"

# Status colours for charts
STATUS_COLORS = {
    STATUS_PASS: COLORS["success"],         # green
    STATUS_INLINE_ONLY: COLORS["warning"],  # amber
    STATUS_FAIL_CF: "#ed8936",              # orange
    STATUS_FAIL_LOCKIN: COLORS["danger"],   # red
}
STATUS_NUMERIC = {
    STATUS_PASS: 0,
    STATUS_INLINE_ONLY: 1,
    STATUS_FAIL_CF: 2,
    STATUS_FAIL_LOCKIN: 3,
}


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

def _resolve_catalog_path(config_dir: Path, catalog_ref: str) -> Path:
    """Resolve a Sweep Config catalog reference to a concrete file path (FIX 2).

    An absolute ``catalog_ref`` is used as-is. A relative one is resolved against the yaml
    file's own directory (``config_dir``) so an ``--input`` yaml's catalogs take effect.
    The committed Baseline yaml (in inputs/) references ``data/pipelines.json`` while the
    catalog actually lives under ``SCRIPT_DIR/data`` (== DATA_DIR, a sibling of inputs/);
    if the yaml-relative path does not resolve to an existing file we fall back to
    ``DATA_DIR/<basename>`` so the Baseline run stays byte-identical.
    """
    ref = Path(catalog_ref)
    candidate = ref if ref.is_absolute() else (config_dir / ref)
    candidate = candidate.resolve()
    if candidate.is_file():
        return candidate
    fallback = DATA_DIR / ref.name
    return fallback.resolve()


def load_pipe_catalog(path: Optional[Path] = None) -> Tuple[Dict, List[Dict]]:
    """Load pipeline catalog from JSON. Returns (full_data, pipes_list).

    ``path`` is the resolved catalog file; when omitted it defaults to today's
    ``DATA_DIR/pipelines.json`` so the Baseline is unchanged.
    """
    if path is None:
        path = DATA_DIR / "pipelines.json"
    with open(path, "r") as f:
        data = json.load(f)
    return data, data["pipes"]


def load_jumper_catalog(path: Optional[Path] = None) -> Tuple[Dict, Dict]:
    """Load jumper catalog from JSON. Returns (full_data, common_properties).

    ``path`` is the resolved catalog file; when omitted it defaults to today's
    ``DATA_DIR/rigid_jumpers.json`` so the Baseline is unchanged.
    """
    if path is None:
        path = DATA_DIR / "rigid_jumpers.json"
    with open(path, "r") as f:
        data = json.load(f)
    return data, data["common_properties"]


def get_pipe_by_size(pipes: List[Dict], nominal_size: str) -> Optional[Dict]:
    """Find pipe entry by nominal size string, e.g. '8in'."""
    for pipe in pipes:
        if pipe["nominal_size"] == nominal_size:
            return pipe
    return None


def get_thickest_wt(pipe: Dict) -> Dict:
    """Return the thickest wall thickness entry for a pipe (most conservative for freespan)."""
    return max(pipe["wall_thicknesses"], key=lambda wt: wt["wt_m"])


def get_thinnest_wt(pipe: Dict) -> Dict:
    """Return the thinnest wall thickness entry for a pipe (worst case for VIV)."""
    return min(pipe["wall_thicknesses"], key=lambda wt: wt["wt_m"])


# ---------------------------------------------------------------------------
# DNV-RP-F105 Freespan/VIV Calculations (self-contained)
# ---------------------------------------------------------------------------

def calc_second_moment_of_area(od_m: float, wt_m: float) -> float:
    """Second moment of area (I) for a hollow circular section [m4].

    I = pi/64 * (OD^4 - ID^4)
    """
    id_m = od_m - 2.0 * wt_m
    return math.pi / 64.0 * (od_m**4 - id_m**4)


def calc_mass_per_meter(
    od_m: float,
    wt_m: float,
    steel_density: float = STEEL_DENSITY,
    coating_thickness_m: float = 0.003,
    coating_density: float = COATING_DENSITY,
    content_density: float = CONTENT_DENSITY,
) -> Dict[str, float]:
    """Calculate mass contributions per meter of pipe.

    Returns dict with steel, coating, content, added_mass, and effective total.
    """
    id_m = od_m - 2.0 * wt_m

    # Steel mass
    a_steel = math.pi / 4.0 * (od_m**2 - id_m**2)
    m_steel = a_steel * steel_density

    # Coating mass
    od_coated = od_m + 2.0 * coating_thickness_m
    a_coating = math.pi / 4.0 * (od_coated**2 - od_m**2)
    m_coating = a_coating * coating_density

    # Content mass (inside pipe)
    a_bore = math.pi / 4.0 * id_m**2
    m_content = a_bore * content_density

    # Added mass (hydrodynamic, based on displaced water)
    a_displaced = math.pi / 4.0 * od_m**2
    m_added = SEAWATER_DENSITY * a_displaced * CA_DEFAULT

    # Effective mass for vibration = structural + content + added
    m_eff = m_steel + m_coating + m_content + m_added

    return {
        "m_steel": m_steel,
        "m_coating": m_coating,
        "m_content": m_content,
        "m_added": m_added,
        "m_eff": m_eff,
    }


def calc_natural_frequency(
    od_m: float,
    wt_m: float,
    span_m: float,
    content_density: float = CONTENT_DENSITY,
    coating_thickness_m: float = 0.003,
    c_n: float = C_N_DEFAULT,
) -> float:
    """Natural frequency of a freespan [Hz].

    f_n = C_n / (2*pi) * sqrt(EI / (m_eff * L^4))

    Parameters
    ----------
    od_m : outer diameter [m]
    wt_m : wall thickness [m]
    span_m : span length [m]
    content_density : internal fluid density [kg/m3]
    coating_thickness_m : anti-corrosion coating thickness [m]
    c_n : boundary condition coefficient (3.56 pinned, 22.4 fixed)

    Returns
    -------
    Natural frequency in Hz. Returns 0.0 if span_m <= 0.
    """
    if span_m <= 0:
        return 0.0

    ei = STEEL_YOUNGS_MODULUS * calc_second_moment_of_area(od_m, wt_m)
    masses = calc_mass_per_meter(
        od_m, wt_m,
        coating_thickness_m=coating_thickness_m,
        content_density=content_density,
    )
    m_eff = masses["m_eff"]

    if m_eff <= 0:
        return 0.0

    f_n = c_n / (2.0 * math.pi) * math.sqrt(ei / (m_eff * span_m**4))
    return f_n


def calc_reduced_velocity(v_current: float, f_n: float, od_m: float) -> float:
    """Reduced velocity V_R = V / (f_n * D).

    Returns float('inf') if f_n or od_m is zero.
    """
    denom = f_n * od_m
    if denom <= 0:
        return float("inf")
    return v_current / denom


def calc_gap_ratio_correction(e_over_d: float) -> float:
    """Gap ratio proximity correction factor for VIV onset.

    For pipes close to seabed (e/D < 1.0), VIV onset is delayed.
    Returns a multiplier >= 1.0 to apply to onset V_R thresholds.

    For e/D = inf (mid-water), no proximity effect: factor = 1.0.
    """
    if math.isinf(e_over_d):
        return 1.0
    return 1.0 + max(0.0, (1.0 - e_over_d) * 0.5)


def calc_response_amplitude(v_r: float, v_r_onset_il: float, v_r_onset_cf: float) -> float:
    """Estimate VIV response amplitude ratio A/D.

    Simplified model per DNV-RP-F105:
    - Below IL onset: A/D = 0
    - IL regime: A/D_IL = min(0.15, 0.15 * V_R / V_R_onset_CF)
    - CF regime: A/D_CF = min(0.9, 0.4 * (V_R - 3.0)) * gamma_f
    """
    if v_r < v_r_onset_il:
        return 0.0
    elif v_r < v_r_onset_cf:
        # In-line regime
        return min(0.15, 0.15 * v_r / v_r_onset_cf)
    else:
        # Cross-flow regime
        a_d = 0.4 * (v_r - 3.0) * GAMMA_F
        return min(0.9, max(0.0, a_d))


def screen_viv(v_r: float, e_over_d: float = 1.0) -> Tuple[str, float]:
    """Screen a single case for VIV per DNV-RP-F105.

    Parameters
    ----------
    v_r : reduced velocity V_R
    e_over_d : gap ratio (e/D)

    Returns
    -------
    (status, a_over_d) : screening result and estimated amplitude ratio
    """
    proximity = calc_gap_ratio_correction(e_over_d)
    v_r_onset_il = VR_ONSET_IL * proximity
    v_r_onset_cf = VR_ONSET_CF * proximity

    a_over_d = calc_response_amplitude(v_r, v_r_onset_il, v_r_onset_cf)

    if v_r < v_r_onset_il:
        status = STATUS_PASS
    elif v_r < v_r_onset_cf:
        status = STATUS_INLINE_ONLY
    elif VR_LOCKIN_LOW <= v_r <= VR_LOCKIN_HIGH:
        status = STATUS_FAIL_LOCKIN
    else:
        status = STATUS_FAIL_CF

    return status, a_over_d


# Status notes for calc_max_allowable_span_status (#653).
MAX_SPAN_STATUS_OK = "ok"
MAX_SPAN_STATUS_NO_CURRENT = "no_current"
MAX_SPAN_STATUS_NO_ONSET = "no_cf_onset_in_range"

# Default upper bound (metres) for the max-allowable-span bisection search.
MAX_SPAN_SEARCH_CEILING_M = 500.0


def calc_max_allowable_span_status(
    od_m: float,
    wt_m: float,
    v_current: float,
    e_over_d: float = 1.0,
    content_density: float = CONTENT_DENSITY,
    coating_thickness_m: float = 0.003,
    c_n: float = C_N_DEFAULT,
    l_hi: float = MAX_SPAN_SEARCH_CEILING_M,
) -> Dict[str, Any]:
    """Find max span before cross-flow VIV onset, with an explicit status (#653).

    Iteratively solves ``V_R(L) = V_R_onset_CF`` for span length ``L`` via bisection
    over ``[1 m, l_hi]``. Because ``f_n ~ 1/L^2`` ⇒ ``V_R ~ L^2`` is monotonically
    increasing in ``L``, cross-flow onset (if it occurs in range) is cleanly bracketed.

    Distinguishes three cases instead of silently saturating at the ceiling:

    - ``v_current <= 0``  -> ``span_m=None``, status ``no_current`` (no VIV ever).
    - onset never reached within ``[1, l_hi]`` (even at ``L=l_hi`` the reduced velocity
      stays below the target) -> ``span_m=None``, status ``no_cf_onset_in_range``.
      Previously this branch returned ``≈ l_hi`` (500.0), indistinguishable from a
      genuine ~500 m limit or the no-current early return.
    - a real onset is bracketed -> ``span_m=<bisection result>``, status ``ok``.

    ``c_n`` is the beam boundary-condition coefficient (resolved from the Sweep Config's
    ``boundary_condition`` label). For the Baseline it equals C_N_PINNED == C_N_DEFAULT,
    so the ``ok``-case span is byte-identical to the pre-yaml run.

    Returns a dict ``{"span_m": float|None, "status": str, "search_ceiling_m": float}``.
    """
    if v_current <= 0:
        # No current => no VIV ever; there is no finite onset span.
        return {
            "span_m": None,
            "status": MAX_SPAN_STATUS_NO_CURRENT,
            "search_ceiling_m": l_hi,
        }

    proximity = calc_gap_ratio_correction(e_over_d)
    v_r_target = VR_ONSET_CF * proximity

    def _v_r_at(span: float) -> float:
        f_n = calc_natural_frequency(
            od_m, wt_m, span,
            content_density=content_density,
            coating_thickness_m=coating_thickness_m,
            c_n=c_n,
        )
        return calc_reduced_velocity(v_current, f_n, od_m)

    # No-onset detection: V_R is monotonically increasing in span, so if even the
    # ceiling span does not reach the onset target, onset never occurs in range.
    if _v_r_at(l_hi) < v_r_target:
        return {
            "span_m": None,
            "status": MAX_SPAN_STATUS_NO_ONSET,
            "search_ceiling_m": l_hi,
        }

    # Onset is bracketed in [l_lo, l_hi]; bisect for the crossing span.
    l_lo, hi = 1.0, l_hi
    for _ in range(60):  # 60 iterations => ~1e-18 m precision
        l_mid = (l_lo + hi) / 2.0
        if _v_r_at(l_mid) < v_r_target:
            l_lo = l_mid  # Span too short, VIV not yet onset
        else:
            hi = l_mid  # Span too long, VIV onset

    return {
        "span_m": (l_lo + hi) / 2.0,
        "status": MAX_SPAN_STATUS_OK,
        "search_ceiling_m": l_hi,
    }


def calc_max_allowable_span(
    od_m: float,
    wt_m: float,
    v_current: float,
    e_over_d: float = 1.0,
    content_density: float = CONTENT_DENSITY,
    coating_thickness_m: float = 0.003,
    c_n: float = C_N_DEFAULT,
    l_hi: float = MAX_SPAN_SEARCH_CEILING_M,
) -> Optional[float]:
    """Find max span length before cross-flow VIV onset (#653).

    Thin numeric wrapper over :func:`calc_max_allowable_span_status`. Returns the max
    allowable span in metres for a genuine onset, or ``None`` when there is no onset in
    range (no current, or onset never reached within ``[1, l_hi]``). ``None`` replaces
    the previous silent ``500.0`` saturation so a clipped/no-onset artifact is no longer
    indistinguishable from a genuine ~500 m limit.
    """
    return calc_max_allowable_span_status(
        od_m, wt_m, v_current,
        e_over_d=e_over_d,
        content_density=content_density,
        coating_thickness_m=coating_thickness_m,
        c_n=c_n,
        l_hi=l_hi,
    )["span_m"]


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------

def run_single_case(
    pipe_type: str,
    nominal_size: str,
    od_m: float,
    wt_m: float,
    span_m: float,
    v_current: float,
    e_over_d: float,
    content_density: float = CONTENT_DENSITY,
    coating_thickness_m: float = 0.003,
    c_n: float = C_N_DEFAULT,
) -> Dict[str, Any]:
    """Run freespan VIV screening for a single parametric case.

    ``c_n`` is the beam boundary-condition coefficient (resolved from the Sweep Config's
    ``boundary_condition`` label; for the Baseline it equals C_N_PINNED == C_N_DEFAULT, so
    behaviour is byte-identical to the pre-yaml run). Returns a dict with all inputs and
    computed results.
    """
    f_n = calc_natural_frequency(
        od_m, wt_m, span_m,
        content_density=content_density,
        coating_thickness_m=coating_thickness_m,
        c_n=c_n,
    )
    v_r = calc_reduced_velocity(v_current, f_n, od_m)
    status, a_over_d = screen_viv(v_r, e_over_d)
    max_span = calc_max_allowable_span(
        od_m, wt_m, v_current,
        e_over_d=e_over_d,
        content_density=content_density,
        coating_thickness_m=coating_thickness_m,
        c_n=c_n,
    )
    # #653: no onset in range (or no current) => None sentinel, not a fake 500 m.
    max_span_out = None if max_span is None else round(max_span, 1)

    # Effective mass for reference
    masses = calc_mass_per_meter(
        od_m, wt_m,
        coating_thickness_m=coating_thickness_m,
        content_density=content_density,
    )

    return {
        "pipe_type": pipe_type,
        "nominal_size": nominal_size,
        "od_m": od_m,
        "wt_m": wt_m,
        "span_m": span_m,
        "v_current_ms": v_current,
        "e_over_d": e_over_d if not math.isinf(e_over_d) else "inf",
        "f_n_hz": round(f_n, 4),
        "v_r": round(v_r, 3),
        "a_over_d": round(a_over_d, 4),
        "status": status,
        "max_allowable_span_m": max_span_out,
        "m_eff_kg_per_m": round(masses["m_eff"], 2),
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def _select_wt(pipe: Dict, wt_selection: str) -> Dict:
    """Resolve a wt_selection LABEL to a wall-thickness entry.

    Baseline uses "thinnest" (worst case for VIV) — identical to the pre-yaml run.
    """
    if wt_selection == "thinnest":
        return get_thinnest_wt(pipe)
    if wt_selection == "thickest":
        return get_thickest_wt(pipe)
    raise ValueError(
        f"wt_selection {wt_selection!r} not supported by run_parametric_sweep "
        "(expected 'thinnest' or 'thickest'; 'explicit' is a Phase-2 follow-up)."
    )


def _config_diverges_from_chart_defaults(config: "ResolvedSweepConfig") -> bool:
    """True if any promoted axis is multi-valued OR differs from the module default (FIX 3).

    Phase-1 charts/summary/metadata reflect the module defaults (CONTENT_DENSITY, thinnest
    WT, pinned c_n). If either sub-sweep promotes content_density, boundary_condition, or
    wt_selection to a multi-valued axis OR to a single non-default value, the charts/summary
    no longer reflect the config — main() warns. The Baseline (all three length-1 at the
    module defaults) does NOT diverge, so no warning fires and stdout stays identical.
    """
    for sub in (config.pipelines, config.jumpers):
        if list(sub.content_density_kg_m3) != [CONTENT_DENSITY]:
            return True
        if list(sub.boundary_condition_labels) != ["pinned"]:
            return True
        if list(sub.wt_selection) != ["thinnest"]:
            return True
    return False


def run_parametric_sweep(
    pipe_catalog: List[Dict],
    jumper_props: Dict,
    config: "ResolvedSweepConfig",
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run full parametric sweep across pipelines and jumpers.

    The sweep axes come from the resolved Sweep Config (``config``), iterated as a
    cross-product. The nesting order preserves today's EXACT emission order (BD-4):
    the pipelines sub-sweep runs entirely before the jumpers sub-sweep, and within each
    sub-sweep the order is ``size -> span -> current -> gap``. The three promoted axes
    (content_density, boundary_condition coefficient, wt_selection) are iterated in the
    OUTERMOST positions; being length-1 in the Baseline they add zero iterations and do
    not perturb the PL-/JM- case numbering.

    Returns (results_list, results_dataframe).
    """
    # Phase-1: only cases[] is config-driven; charts/summary/metadata reflect module-default
    # constants (CONTENT_DENSITY, thinnest WT, pinned c_n). Full config-threading is a
    # Phase-2 follow-up (see docs/adr/0002). main() emits a runtime warning when the resolved
    # config diverges from those defaults so the chart/summary mismatch is not silent.
    results: List[Dict[str, Any]] = []
    # Parallel lists collected in lock-step with `results` (one entry per appended case).
    # They enrich the DataFrame ONLY (not the JSON results dicts), promoting the three
    # outer axes (content density, boundary-condition LABEL, wt_selection) to df columns
    # for the Results Store. The JSON cases[] must stay 15-key (asserted before save).
    enrich_content_density: List[float] = []
    enrich_boundary_condition: List[str] = []
    enrich_wt_selection: List[str] = []
    case_id = 0

    pl = config.pipelines
    jm = config.jumpers

    # --- Pipeline cases ---
    # Promoted axes (content_density / boundary-condition c_n / wt_selection) multiply in
    # as the outermost factors; length-1 in the Baseline => total == today's 480.
    total_pipeline = (
        len(pl.content_density_kg_m3) * len(pl.c_n_values) * len(pl.wt_selection)
        * len(pl.sizes) * len(pl.span_lengths_m)
        * len(pl.current_velocities_ms) * len(pl.gap_ratios)
    )
    print(f"  Pipeline cases: {total_pipeline}")

    for content_density in pl.content_density_kg_m3:
        for c_n, bc_label in zip(pl.c_n_values, pl.boundary_condition_labels):
            for wt_sel in pl.wt_selection:
                for size_name in pl.sizes:
                    pipe = get_pipe_by_size(pipe_catalog, size_name)
                    if pipe is None:
                        logger.warning("Pipe size %s not found in catalog, skipping", size_name)
                        continue

                    od_m = pipe["od_m"]
                    # Worst-case WT per the resolved wt_selection (Baseline: thinnest).
                    wt_entry = _select_wt(pipe, wt_sel)
                    wt_m = wt_entry["wt_m"]
                    coating_t = pipe["anti_corrosion_coating"]["thickness_m"]

                    for span_m in pl.span_lengths_m:
                        for v_curr in pl.current_velocities_ms:
                            for e_d in pl.gap_ratios:
                                case_id += 1
                                result = run_single_case(
                                    pipe_type="pipeline",
                                    nominal_size=size_name,
                                    od_m=od_m,
                                    wt_m=wt_m,
                                    span_m=span_m,
                                    v_current=v_curr,
                                    e_over_d=e_d,
                                    content_density=content_density,
                                    coating_thickness_m=coating_t,
                                    c_n=c_n,
                                )
                                result["case_id"] = f"PL-{case_id:04d}"
                                result["schedule"] = wt_entry["schedule_approx"]
                                results.append(result)
                                enrich_content_density.append(float(content_density))
                                enrich_boundary_condition.append(bc_label)
                                enrich_wt_selection.append(wt_sel)

    pipeline_count = case_id
    print(f"    Completed {pipeline_count} pipeline cases")

    # --- Jumper cases ---
    # The rigid jumper is single-bodied (common_properties); the `sizes` axis is length-1
    # and selects the same body each time. Promoted axes again outermost / length-1.
    total_jumper = (
        len(jm.content_density_kg_m3) * len(jm.c_n_values) * len(jm.wt_selection)
        * len(jm.sizes) * len(jm.span_lengths_m)
        * len(jm.current_velocities_ms) * len(jm.gap_ratios)
    )
    print(f"  Jumper cases: {total_jumper}")

    od_m_j = jumper_props["od_m"]
    wt_m_j = jumper_props["wt_m"]
    coating_t_j = jumper_props["anti_corrosion_coating"]["thickness_m"]

    for content_density in jm.content_density_kg_m3:
        for c_n, bc_label in zip(jm.c_n_values, jm.boundary_condition_labels):
            for _wt_sel in jm.wt_selection:
                for _size_name in jm.sizes:
                    for span_m in jm.span_lengths_m:
                        for v_curr in jm.current_velocities_ms:
                            for e_d in jm.gap_ratios:
                                case_id += 1
                                result = run_single_case(
                                    pipe_type="jumper",
                                    nominal_size="8in-jumper",
                                    od_m=od_m_j,
                                    wt_m=wt_m_j,
                                    span_m=span_m,
                                    v_current=v_curr,
                                    e_over_d=e_d,
                                    content_density=content_density,
                                    coating_thickness_m=coating_t_j,
                                    c_n=c_n,
                                )
                                result["case_id"] = f"JM-{case_id - pipeline_count:04d}"
                                result["schedule"] = "Sch 120 (jumper)"
                                results.append(result)
                                enrich_content_density.append(float(content_density))
                                enrich_boundary_condition.append(bc_label)
                                enrich_wt_selection.append(_wt_sel)

    jumper_count = case_id - pipeline_count
    print(f"    Completed {jumper_count} jumper cases")

    df = pd.DataFrame(results)
    # Enrich the DataFrame ONLY (the JSON results dicts stay 15-key). These three promoted
    # axes are collected in lock-step with `results` above, so they align row-for-row.
    df["content_density_kg_m3"] = enrich_content_density
    df["boundary_condition"] = enrich_boundary_condition
    df["wt_selection"] = enrich_wt_selection
    return results, df


# ---------------------------------------------------------------------------
# Chart 1: Natural Frequency vs Span Length
# ---------------------------------------------------------------------------

def build_chart_1_frequency_vs_span(
    pipe_catalog: List[Dict],
    jumper_props: Dict,
) -> go.Figure:
    """Natural frequency vs span length for each pipe size + jumper overlay."""
    fig = go.Figure()

    spans = np.linspace(5, 100, 200)
    line_styles = ["solid", "dash", "dot", "dashdot"]

    for i, size_name in enumerate(PIPELINE_SIZES):
        pipe = get_pipe_by_size(pipe_catalog, size_name)
        if pipe is None:
            continue
        od_m = pipe["od_m"]
        wt_m = get_thinnest_wt(pipe)["wt_m"]
        coating_t = pipe["anti_corrosion_coating"]["thickness_m"]

        freqs = [
            calc_natural_frequency(od_m, wt_m, s, CONTENT_DENSITY, coating_t)
            for s in spans
        ]
        fig.add_trace(go.Scatter(
            x=spans, y=freqs,
            mode="lines",
            name=f'Pipeline {size_name} (WT={get_thinnest_wt(pipe)["wt_mm"]:.1f}mm)',
            line=dict(color=CHART_PALETTE[i], width=2.5),
            hovertemplate="Span: %{x:.0f} m<br>f_n: %{y:.3f} Hz<extra></extra>",
        ))

    # Jumper overlay (same OD as 8" pipeline but thicker wall)
    od_j = jumper_props["od_m"]
    wt_j = jumper_props["wt_m"]
    coat_j = jumper_props["anti_corrosion_coating"]["thickness_m"]
    freqs_j = [
        calc_natural_frequency(od_j, wt_j, s, CONTENT_DENSITY, coat_j)
        for s in spans
    ]
    fig.add_trace(go.Scatter(
        x=spans, y=freqs_j,
        mode="lines",
        name=f"Jumper 8in (WT={jumper_props['wt_mm']:.1f}mm)",
        line=dict(color=CHART_PALETTE[4], width=2.5, dash="dash"),
        hovertemplate="Span: %{x:.0f} m<br>f_n: %{y:.3f} Hz<extra></extra>",
    ))

    fig.update_layout(
        title="Natural Frequency vs Span Length",
        xaxis_title="Span Length (m)",
        yaxis_title="Natural Frequency (Hz)",
        yaxis_type="log",
        yaxis=dict(dtick=1, minor=dict(ticks="inside")),
        legend=dict(x=0.55, y=0.95),
        height=500,
    )

    return fig


# ---------------------------------------------------------------------------
# Chart 2: VIV Onset Screening Map (heatmap per pipe size)
# ---------------------------------------------------------------------------

def build_chart_2_viv_onset_map(
    pipe_catalog: List[Dict],
) -> go.Figure:
    """Heatmap of VIV screening status: span vs current for each pipe size.

    One subplot per pipe size, at e/D = 1.0 (typical).
    """
    n_sizes = len(PIPELINE_SIZES)
    fig = make_subplots(
        rows=1, cols=n_sizes,
        subplot_titles=[f"Pipeline {s}" for s in PIPELINE_SIZES],
        horizontal_spacing=0.08,
    )

    e_over_d = 1.0  # Typical gap ratio for screening map
    spans = np.array(PIPELINE_SPAN_LENGTHS_M, dtype=float)
    currents = np.array(CURRENT_VELOCITIES_MS, dtype=float)

    colorscale = [
        [0.0, COLORS["success"]],    # PASS = 0
        [0.33, COLORS["warning"]],   # INLINE_ONLY = 1
        [0.66, "#ed8936"],           # FAIL_CF = 2
        [1.0, COLORS["danger"]],     # FAIL_LOCKIN = 3
    ]

    for col_idx, size_name in enumerate(PIPELINE_SIZES, 1):
        pipe = get_pipe_by_size(pipe_catalog, size_name)
        if pipe is None:
            continue
        od_m = pipe["od_m"]
        wt_m = get_thinnest_wt(pipe)["wt_m"]
        coating_t = pipe["anti_corrosion_coating"]["thickness_m"]

        # Build status matrix: rows=currents, cols=spans
        z = np.zeros((len(currents), len(spans)))
        text_matrix = [['' for _ in spans] for _ in currents]

        for j, span_m in enumerate(spans):
            for i, v_curr in enumerate(currents):
                f_n = calc_natural_frequency(od_m, wt_m, span_m, CONTENT_DENSITY, coating_t)
                v_r = calc_reduced_velocity(v_curr, f_n, od_m)
                status, a_d = screen_viv(v_r, e_over_d)
                z[i, j] = STATUS_NUMERIC[status]
                text_matrix[i][j] = f"{status}<br>V_R={v_r:.1f}<br>A/D={a_d:.3f}"

        fig.add_trace(
            go.Heatmap(
                z=z,
                x=spans,
                y=currents,
                text=text_matrix,
                hovertemplate="Span: %{x} m<br>Current: %{y} m/s<br>%{text}<extra></extra>",
                colorscale=colorscale,
                zmin=0, zmax=3,
                showscale=(col_idx == n_sizes),
                colorbar=dict(
                    title="Status",
                    tickvals=[0, 1, 2, 3],
                    ticktext=["PASS", "INLINE", "FAIL_CF", "LOCKIN"],
                    len=0.6,
                ),
            ),
            row=1, col=col_idx,
        )

        fig.update_xaxes(title_text="Span (m)", row=1, col=col_idx)
        if col_idx == 1:
            fig.update_yaxes(title_text="Current (m/s)", row=1, col=col_idx)

    fig.update_layout(
        title=f"VIV Onset Screening Map (e/D = {e_over_d})",
        height=450,
    )

    return fig


# ---------------------------------------------------------------------------
# Chart 3: Max Allowable Span Heatmap
# ---------------------------------------------------------------------------

def build_chart_3_max_span_heatmap(
    pipe_catalog: List[Dict],
    jumper_props: Dict,
) -> go.Figure:
    """Heatmap: pipe size vs current velocity, cell = max allowable span at e/D=1.0."""
    e_over_d = 1.0
    currents = np.array(CURRENT_VELOCITIES_MS, dtype=float)

    # Include pipeline sizes + jumper
    size_labels = [f"PL {s}" for s in PIPELINE_SIZES] + ["Jumper 8in"]
    n_rows = len(size_labels)
    n_cols = len(currents)

    z = np.zeros((n_rows, n_cols))
    text_matrix = [['' for _ in range(n_cols)] for _ in range(n_rows)]

    for row_i, size_name in enumerate(PIPELINE_SIZES):
        pipe = get_pipe_by_size(pipe_catalog, size_name)
        if pipe is None:
            continue
        od_m = pipe["od_m"]
        wt_m = get_thinnest_wt(pipe)["wt_m"]
        coating_t = pipe["anti_corrosion_coating"]["thickness_m"]

        for col_j, v_curr in enumerate(currents):
            max_span = calc_max_allowable_span(
                od_m, wt_m, v_curr, e_over_d, CONTENT_DENSITY, coating_t,
            )
            # #653: no onset in range => NaN cell + "n/a" label, not a fake 500 m.
            z[row_i, col_j] = np.nan if max_span is None else max_span
            text_matrix[row_i][col_j] = "n/a" if max_span is None else f"{max_span:.1f} m"

    # Jumper row
    row_j = len(PIPELINE_SIZES)
    od_j = jumper_props["od_m"]
    wt_j = jumper_props["wt_m"]
    coat_j = jumper_props["anti_corrosion_coating"]["thickness_m"]
    for col_j, v_curr in enumerate(currents):
        max_span = calc_max_allowable_span(
            od_j, wt_j, v_curr, e_over_d, CONTENT_DENSITY, coat_j,
        )
        # #653: no onset in range => NaN cell + "n/a" label, not a fake 500 m.
        z[row_j, col_j] = np.nan if max_span is None else max_span
        text_matrix[row_j][col_j] = "n/a" if max_span is None else f"{max_span:.1f} m"

    fig = go.Figure(data=go.Heatmap(
        z=z,
        x=[f"{v:.1f}" for v in currents],
        y=size_labels,
        text=text_matrix,
        texttemplate="%{text}",
        hovertemplate="Pipe: %{y}<br>Current: %{x} m/s<br>Max Span: %{text}<extra></extra>",
        colorscale="RdYlGn",
        reversescale=False,
        colorbar=dict(title="Max Span (m)"),
    ))

    fig.update_layout(
        title="Max Allowable Freespan Before Cross-Flow VIV Onset (e/D = 1.0)",
        xaxis_title="Current Velocity (m/s)",
        yaxis_title="Pipe Size",
        height=400,
    )

    return fig


# ---------------------------------------------------------------------------
# Chart 4: Pass/Fail Matrix
# ---------------------------------------------------------------------------

def build_chart_4_pass_fail_matrix(df: pd.DataFrame) -> go.Figure:
    """Grid of screening results for pipelines at e/D = 1.0.

    X = span length, Y = current velocity. One subplot per pipe size.
    """
    # Filter to e/D = 1.0 and pipelines only
    mask = (df["pipe_type"] == "pipeline") & (df["e_over_d"] == 1.0)
    df_filt = df[mask].copy()

    n_sizes = len(PIPELINE_SIZES)
    fig = make_subplots(
        rows=1, cols=n_sizes,
        subplot_titles=[f"Pipeline {s}" for s in PIPELINE_SIZES],
        horizontal_spacing=0.08,
    )

    spans = sorted(df_filt["span_m"].unique())
    currents = sorted(df_filt["v_current_ms"].unique())

    colorscale = [
        [0.0, COLORS["success"]],
        [0.33, COLORS["warning"]],
        [0.66, "#ed8936"],
        [1.0, COLORS["danger"]],
    ]

    for col_idx, size_name in enumerate(PIPELINE_SIZES, 1):
        subset = df_filt[df_filt["nominal_size"] == size_name]

        z = np.full((len(currents), len(spans)), np.nan)
        text_matrix = [['' for _ in spans] for _ in currents]

        for _, row in subset.iterrows():
            j = spans.index(row["span_m"])
            i = currents.index(row["v_current_ms"])
            z[i, j] = STATUS_NUMERIC.get(row["status"], 0)
            text_matrix[i][j] = f'{row["status"]}<br>V_R={row["v_r"]:.1f}'

        fig.add_trace(
            go.Heatmap(
                z=z,
                x=spans,
                y=currents,
                text=text_matrix,
                hovertemplate="Span: %{x} m<br>Current: %{y} m/s<br>%{text}<extra></extra>",
                colorscale=colorscale,
                zmin=0, zmax=3,
                showscale=(col_idx == n_sizes),
                colorbar=dict(
                    title="Status",
                    tickvals=[0, 1, 2, 3],
                    ticktext=["PASS", "INLINE", "FAIL_CF", "LOCKIN"],
                    len=0.6,
                ),
            ),
            row=1, col=col_idx,
        )

        fig.update_xaxes(title_text="Span (m)", row=1, col=col_idx)
        if col_idx == 1:
            fig.update_yaxes(title_text="Current (m/s)", row=1, col=col_idx)

    fig.update_layout(
        title="Pass/Fail Screening Matrix — Pipelines at e/D = 1.0",
        height=450,
    )

    return fig


# ---------------------------------------------------------------------------
# Chart 5: Jumper vs Pipeline Comparison
# ---------------------------------------------------------------------------

def build_chart_5_jumper_vs_pipeline(
    pipe_catalog: List[Dict],
    jumper_props: Dict,
) -> go.Figure:
    """Max allowable current velocity vs span length: 8" pipeline vs 8" jumper.

    Shows how the jumper's thicker wall (18.26mm vs 8.18mm) extends
    the allowable span or current envelope.
    """
    fig = go.Figure()

    spans = np.linspace(5, 80, 150)
    e_over_d = 1.0

    # 8" Pipeline (thinnest WT)
    pipe_8 = get_pipe_by_size(pipe_catalog, "8in")
    if pipe_8 is not None:
        od_pl = pipe_8["od_m"]
        wt_pl = get_thinnest_wt(pipe_8)["wt_m"]
        coat_pl = pipe_8["anti_corrosion_coating"]["thickness_m"]
        wt_mm_pl = get_thinnest_wt(pipe_8)["wt_mm"]

        max_currents_pl = []
        for s in spans:
            # Bisection: find max current where status is still PASS or INLINE
            v_lo, v_hi = 0.0, 3.0
            for _ in range(50):
                v_mid = (v_lo + v_hi) / 2.0
                f_n = calc_natural_frequency(od_pl, wt_pl, s, CONTENT_DENSITY, coat_pl)
                v_r = calc_reduced_velocity(v_mid, f_n, od_pl)
                status, _ = screen_viv(v_r, e_over_d)
                if status in (STATUS_PASS, STATUS_INLINE_ONLY):
                    v_lo = v_mid
                else:
                    v_hi = v_mid
            max_currents_pl.append((v_lo + v_hi) / 2.0)

        fig.add_trace(go.Scatter(
            x=spans, y=max_currents_pl,
            mode="lines",
            name=f"Pipeline 8in (WT={wt_mm_pl:.1f}mm)",
            line=dict(color=CHART_PALETTE[0], width=2.5),
            fill="tozeroy",
            fillcolor="rgba(44,82,130,0.1)",
            hovertemplate="Span: %{x:.0f} m<br>Max V: %{y:.2f} m/s<extra></extra>",
        ))

    # 8" Jumper (thicker WT)
    od_j = jumper_props["od_m"]
    wt_j = jumper_props["wt_m"]
    coat_j = jumper_props["anti_corrosion_coating"]["thickness_m"]

    max_currents_j = []
    for s in spans:
        v_lo, v_hi = 0.0, 3.0
        for _ in range(50):
            v_mid = (v_lo + v_hi) / 2.0
            f_n = calc_natural_frequency(od_j, wt_j, s, CONTENT_DENSITY, coat_j)
            v_r = calc_reduced_velocity(v_mid, f_n, od_j)
            status, _ = screen_viv(v_r, e_over_d)
            if status in (STATUS_PASS, STATUS_INLINE_ONLY):
                v_lo = v_mid
            else:
                v_hi = v_mid
        max_currents_j.append((v_lo + v_hi) / 2.0)

    fig.add_trace(go.Scatter(
        x=spans, y=max_currents_j,
        mode="lines",
        name=f"Jumper 8in (WT={jumper_props['wt_mm']:.1f}mm)",
        line=dict(color=CHART_PALETTE[4], width=2.5, dash="dash"),
        fill="tozeroy",
        fillcolor="rgba(128,90,213,0.08)",
        hovertemplate="Span: %{x:.0f} m<br>Max V: %{y:.2f} m/s<extra></extra>",
    ))

    # Add annotation
    fig.add_annotation(
        x=50, y=1.8,
        text="Above curve = CF VIV onset<br>(detailed analysis required)",
        showarrow=False,
        font=dict(size=11, color=COLORS["danger"]),
        bgcolor="rgba(255,255,255,0.85)",
        bordercolor=COLORS["danger"],
        borderwidth=1,
        borderpad=6,
    )

    fig.update_layout(
        title="Jumper vs Pipeline: Max Allowable Current Before CF VIV (e/D = 1.0)",
        xaxis_title="Span Length (m)",
        yaxis_title="Max Current Velocity (m/s)",
        yaxis=dict(range=[0, 3.0]),
        legend=dict(x=0.55, y=0.95),
        height=500,
    )

    return fig


# ---------------------------------------------------------------------------
# Summary statistics
# ---------------------------------------------------------------------------

def build_summary_stats(df: pd.DataFrame) -> pd.DataFrame:
    """Build a summary table of screening results by pipe type and size."""
    rows = []

    for pipe_type in ["pipeline", "jumper"]:
        sub = df[df["pipe_type"] == pipe_type]
        for size in sub["nominal_size"].unique():
            ss = sub[sub["nominal_size"] == size]
            total = len(ss)
            n_pass = len(ss[ss["status"] == STATUS_PASS])
            n_inline = len(ss[ss["status"] == STATUS_INLINE_ONLY])
            n_fail_cf = len(ss[ss["status"] == STATUS_FAIL_CF])
            n_lockin = len(ss[ss["status"] == STATUS_FAIL_LOCKIN])
            pass_rate = (n_pass + n_inline) / total * 100 if total > 0 else 0

            rows.append({
                "Type": pipe_type.title(),
                "Size": size,
                "Cases": total,
                "PASS": n_pass,
                "INLINE_ONLY": n_inline,
                "FAIL_CF": n_fail_cf,
                "FAIL_LOCKIN": n_lockin,
                "Acceptable %": f"{pass_rate:.0f}%",
            })

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Report builder
# ---------------------------------------------------------------------------

def build_report(
    fig1: go.Figure,
    fig2: go.Figure,
    fig3: go.Figure,
    fig4: go.Figure,
    fig5: go.Figure,
    summary_df: pd.DataFrame,
    results: List[Dict],
    total_cases: int,
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report.

    ``output_path`` defaults (inside the signature) to the legacy Baseline location so the
    Baseline call stays byte-identical; a named Run passes a per-run path (BD per-run report).
    """
    if output_path is None:
        output_path = OUTPUT_DIR / "demo_01_freespan_report.html"
    report = GTMReportBuilder(
        title="DNV Freespan / VIV Screening Analysis",
        subtitle=f"~{total_cases} parametric cases across 3 pipe sizes and rigid jumpers",
        demo_id="demo_01",
        case_count=total_cases,
        code_refs=[
            "DNV-RP-F105 (2017) — Free Spanning Pipelines",
            "DNV-RP-C205 (2010) — Environmental Conditions and Loads",
            "DNV-OS-F101 (2013) — Submarine Pipeline Systems",
        ],
    )

    # Methodology
    methodology_html = """
    <p>This analysis implements <strong>DNV-RP-F105 simplified VIV screening</strong>
    for pipeline and jumper freespans. The methodology evaluates whether
    vortex-induced vibrations will develop under steady current loading,
    using the reduced velocity parameter V<sub>R</sub> = V / (f<sub>n</sub> &times; D).</p>

    <h3>Screening Steps</h3>
    <ol>
        <li><strong>Natural frequency</strong> — beam-on-elastic-foundation model
            with pinned-pinned boundary conditions (conservative).
            f<sub>n</sub> = C<sub>n</sub>/(2&pi;) &times; &radic;(EI / m<sub>eff</sub>L<sup>4</sup>)</li>
        <li><strong>Reduced velocity</strong> — V<sub>R</sub> = V<sub>current</sub> / (f<sub>n</sub> &times; D)</li>
        <li><strong>VIV onset check</strong> — in-line onset at V<sub>R</sub> = 1.0,
            cross-flow onset at V<sub>R</sub> = 3.0, lock-in range 4.0&ndash;8.0</li>
        <li><strong>Gap ratio correction</strong> — seabed proximity (e/D &lt; 1) delays VIV onset</li>
        <li><strong>Response amplitude</strong> — simplified A/D estimation for screening</li>
    </ol>

    <h3>Parameter Matrix</h3>
    <ul>
        <li><strong>Pipelines:</strong> 8&quot;, 12&quot;, 16&quot; &times; 8 spans (10&ndash;80 m)
            &times; 5 currents (0.2&ndash;1.0 m/s) &times; 4 gap ratios = 480 cases</li>
        <li><strong>Jumpers:</strong> 8&quot; rigid &times; 8 spans (5&ndash;40 m)
            &times; 5 currents &times; 5 gap ratios (incl. mid-water) = 200 cases</li>
    </ul>

    <h3>Screening Criteria</h3>
    <ul>
        <li><span class="status-pass">PASS</span> — no VIV expected (V<sub>R</sub> &lt; 1.0)</li>
        <li><span class="status-marginal">INLINE_ONLY</span> — in-line VIV only
            (1.0 &le; V<sub>R</sub> &lt; 3.0), typically acceptable</li>
        <li><span class="status-fail">FAIL_CF</span> — cross-flow VIV onset
            (V<sub>R</sub> &ge; 3.0), detailed analysis needed</li>
        <li><span class="status-fail">FAIL_LOCKIN</span> — lock-in regime
            (4.0 &le; V<sub>R</sub> &le; 8.0), critical condition</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Summary table
    report.add_table(
        "Screening Results Summary",
        summary_df,
        subtitle="Pass rate includes PASS + INLINE_ONLY (acceptable for most applications).",
    )

    # Charts
    report.add_chart("freq_vs_span", fig1,
                     title="Chart 1: Natural Frequency vs Span Length",
                     subtitle="Log-scale frequency showing how longer spans reduce f_n. "
                              "Jumper's thicker wall gives higher frequency at same OD.")

    report.add_chart("viv_onset_map", fig2,
                     title="Chart 2: VIV Onset Screening Map",
                     subtitle="Span-current screening at e/D = 1.0. "
                              "Green = safe, yellow = inline only, orange/red = VIV concern.")

    report.add_chart("max_span_heatmap", fig3,
                     title="Chart 3: Max Allowable Span Heatmap",
                     subtitle="Maximum freespan before cross-flow VIV onset at e/D = 1.0.")

    report.add_chart("pass_fail_matrix", fig4,
                     title="Chart 4: Pass/Fail Screening Matrix",
                     subtitle="Full parametric screening results for all pipeline sizes at e/D = 1.0.")

    report.add_chart("jumper_vs_pipeline", fig5,
                     title="Chart 5: Jumper vs Pipeline Comparison",
                     subtitle="Allowable current envelope: 8in pipeline (Sch 40) vs 8in jumper (Sch 120). "
                              "Thicker jumper wall extends safe operating envelope.")

    # Live mode teaser
    report.add_live_mode_teaser(
        analysis_type="freespan VIV screening updates continuously with "
                      "measured current profiles from ADCPs and vessel-mounted sensors"
    )

    # Assumptions
    report.add_assumptions([
        "Pinned-pinned boundary conditions (conservative — actual supports provide partial fixity)",
        "Steady uniform current perpendicular to pipe axis (worst case orientation)",
        "No wave-induced loading considered (current-only screening)",
        "Effective axial force = 0 (no temperature/pressure-induced axial load data)",
        "Added mass coefficient Ca = 1.0 (potential flow theory)",
        "Content density = 800 kg/m3 (oil-filled, conservative for empty pipe)",
        "Coating thickness from pipe catalog (3LPP or FBE)",
        "Thinnest available wall thickness used per pipe size (worst case for VIV)",
        "No soil-pipe interaction stiffness modelled (free span only)",
        "Single-mode response only (higher modes may be excited at very high V_R)",
    ])

    # Build and save
    output_path.parent.mkdir(parents=True, exist_ok=True)
    html = report.build(output_path)
    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# JSON results saver
# ---------------------------------------------------------------------------

def save_results_json(
    results: List[Dict],
    summary_df: pd.DataFrame,
    total_cases: int,
) -> Path:
    """Save results to JSON with metadata."""
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    results_path = RESULTS_DIR / "demo_01_freespan_results.json"

    json_output = {
        "metadata": {
            "demo": "GTM Demo 1: DNV Freespan / VIV Screening Analysis",
            "code_ref": "DNV-RP-F105 (2017)",
            "total_cases": total_cases,
            "pipeline_sizes": PIPELINE_SIZES,
            "pipeline_span_lengths_m": PIPELINE_SPAN_LENGTHS_M,
            "jumper_span_lengths_m": JUMPER_SPAN_LENGTHS_M,
            "current_velocities_ms": CURRENT_VELOCITIES_MS,
            "pipeline_gap_ratios": PIPELINE_GAP_RATIOS,
            "jumper_gap_ratios": [x if not math.isinf(x) else "inf" for x in JUMPER_GAP_RATIOS],
            "content_density_kg_m3": CONTENT_DENSITY,
            "steel_youngs_modulus_pa": STEEL_YOUNGS_MODULUS,
            "vr_onset_il": VR_ONSET_IL,
            "vr_onset_cf": VR_ONSET_CF,
            "vr_lockin_range": [VR_LOCKIN_LOW, VR_LOCKIN_HIGH],
            "boundary_condition": "pinned-pinned (C_n = 3.56)",
            "timestamp": datetime.now(timezone.utc).isoformat(),
        },
        "summary": summary_df.to_dict(orient="records"),
        "cases": results,
    }

    with open(results_path, "w") as f:
        json.dump(json_output, f, indent=2, default=str)

    return results_path


def load_cached_results(results_path: Path) -> Tuple[List[Dict], pd.DataFrame]:
    """Load previously saved JSON results."""
    with open(results_path, "r") as f:
        data = json.load(f)

    cases = data["cases"]
    df = pd.DataFrame(cases)

    # Restore e_over_d infinity for jumper mid-water cases
    df["e_over_d"] = df["e_over_d"].apply(
        lambda x: float("inf") if x == "inf" else float(x)
    )

    return cases, df


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="GTM Demo 1: DNV Freespan / VIV Screening Analysis",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Load results from cached JSON instead of recomputing.",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force recalculation even if cached results exist.",
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=None,
        help="Path to a Sweep Config yaml (defaults to the committed "
             "inputs/demo_01_freespan.yml Baseline).",
    )
    parser.add_argument(
        "--run-id",
        type=str,
        default=BASELINE_RUN_ID,
        help="Run identifier for the Results Store + per-run report dir "
             f"(default '{BASELINE_RUN_ID}', the Baseline Run). A named run writes its report "
             "to output/parametric/demo_01/<run_id>/report.html and seeds <run_id> in the Store.",
    )
    return parser.parse_args()


# ---------------------------------------------------------------------------
# Results Store subcommands: `lookup` + `rebuild-db` (BD-1..BD-3, N2)
#
# These are dispatched by an argv-sniff at the TOP of main() / __main__ so they NEVER touch
# the existing sweep parser — the no-arg / --from-cache / --force / --input sweep path stays
# provably byte-identical. Each subcommand builds its OWN ArgumentParser over sys.argv[2:].
# ---------------------------------------------------------------------------

# Columns shown by `lookup`, in a clean operator-facing order.
_LOOKUP_DISPLAY_COLS = [
    "case_id", "pipe_type", "nominal_size", "span_m", "v_current_ms",
    "e_over_d", "f_n_hz", "v_r", "a_over_d", "status", "max_allowable_span_m",
]


def lookup_main(argv: List[str]) -> int:
    """`lookup` subcommand: read-only filtered SELECT over the Results Store cache (BD-2/BD-3).

    Returns a process exit code (0 = ok, non-zero = error such as a missing db).
    """
    import sqlite3

    import results_store as rs

    parser = argparse.ArgumentParser(
        prog="demo_01_dnv_freespan_viv.py lookup",
        description="Look up cases in the demo_01 Results Store (read-only SQLite cache).",
    )
    parser.add_argument("--run-id", type=str, default=BASELINE_RUN_ID,
                        help=f"Run to query (default '{BASELINE_RUN_ID}').")
    parser.add_argument("--size", type=str, default=None,
                        help='Nominal size filter, INCLUDING the "in" suffix (e.g. "12in").')
    parser.add_argument("--current", type=float, default=None,
                        help="Current velocity filter in m/s (e.g. 0.6).")
    parser.add_argument("--gap", type=str, default=None,
                        help='Gap ratio e/D filter; finite (e.g. "1.0") or "inf" for mid-water.')
    parser.add_argument("--span", type=int, default=None, help="Span length filter in metres.")
    parser.add_argument("--status", type=str, default=None,
                        help="Status filter (PASS / INLINE_ONLY / FAIL_CF / FAIL_LOCKIN).")
    parser.add_argument("--base-dir", type=Path, default=RESULTS_DIR,
                        help="Store base dir (default the demo's results/ dir).")
    args = parser.parse_args(argv)

    # D1: validate run_id before using it (clean error + non-zero exit, no traceback).
    try:
        validate_run_id(args.run_id)
    except ValueError as exc:
        print(f"[ERROR] {exc}")
        return 2

    db_path = rs._db_path(args.base_dir)
    if not db_path.exists():
        # BD-3: do NOT create the db; clear, actionable message; non-zero exit.
        print(
            f"No results.db at {db_path} — run a sweep first "
            f"(e.g. `... --run-id {BASELINE_RUN_ID}`) or `... rebuild-db`."
        )
        return 1

    # BD-3: open strictly read-only via a file: URI so a query never mutates/creates the cache.
    conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    try:
        # Distinct message for an unknown run_id vs. a zero-row filter result.
        known_runs = [r[0] for r in conn.execute("SELECT run_id FROM runs ORDER BY run_id")]
        if args.run_id not in known_runs:
            available = ", ".join(known_runs) if known_runs else "(none)"
            print(f"run_id '{args.run_id}' not found; available: {available}")
            return 1

        # BD-2: build a parameterized WHERE, canonicalizing each filter to the STORED form.
        where = ["run_id = ?"]
        params: List[Any] = [args.run_id]
        if args.size is not None:
            where.append("nominal_size = ?")
            params.append(str(args.size))
        if args.current is not None:
            where.append("v_current_ms = ?")
            params.append(float(args.current))
        if args.gap is not None:
            where.append("e_over_d = ?")
            # D2: a bad --gap value (not a number or 'inf') gets a clean message + non-zero
            # exit, not a raw ValueError traceback.
            try:
                gap_token = rs._e_over_d_token(args.gap)  # "1.0"/"inf" canonical TEXT token
            except ValueError:
                print(f"invalid --gap value: {args.gap} (expected a number or 'inf')")
                return 2
            params.append(gap_token)
        if args.span is not None:
            where.append("span_m = ?")
            params.append(int(args.span))
        if args.status is not None:
            where.append("status = ?")
            params.append(str(args.status))

        cols = ", ".join(_LOOKUP_DISPLAY_COLS)
        sql = (
            f"SELECT {cols} FROM cases WHERE {' AND '.join(where)} "
            "ORDER BY pipe_type, nominal_size, span_m, v_current_ms, e_over_d"
        )
        rows = conn.execute(sql, params).fetchall()
    finally:
        conn.close()

    if not rows:
        print("0 cases match the given filters.")
        return 0

    # Clean fixed-width table.
    widths = [len(c) for c in _LOOKUP_DISPLAY_COLS]
    str_rows = [[("" if v is None else str(v)) for v in row] for row in rows]
    for r in str_rows:
        for i, cell in enumerate(r):
            widths[i] = max(widths[i], len(cell))
    header = "  ".join(c.ljust(widths[i]) for i, c in enumerate(_LOOKUP_DISPLAY_COLS))
    print(header)
    print("  ".join("-" * widths[i] for i in range(len(_LOOKUP_DISPLAY_COLS))))
    for r in str_rows:
        print("  ".join(cell.ljust(widths[i]) for i, cell in enumerate(r)))
    print(f"\n{len(rows)} case(s) matched.")
    return 0


def rebuild_main(argv: List[str]) -> int:
    """`rebuild-db` subcommand: regenerate the SQLite cache + index.csv from the text source (N2)."""
    import sqlite3

    import results_store as rs

    parser = argparse.ArgumentParser(
        prog="demo_01_dnv_freespan_viv.py rebuild-db",
        description="Rebuild the demo_01 Results Store SQLite cache + index.csv from "
                    "the per-run cases.csv / manifest.json text source of truth.",
    )
    parser.add_argument("--base-dir", type=Path, default=RESULTS_DIR,
                        help="Store base dir (default the demo's results/ dir).")
    args = parser.parse_args(argv)

    db_path = rs.rebuild_db(args.base_dir)
    print(f"Results Store rebuilt: {db_path}")
    conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    try:
        n_runs = conn.execute("SELECT COUNT(*) FROM runs").fetchone()[0]
        n_cases = conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0]
        per_run = conn.execute(
            "SELECT run_id, COUNT(*) FROM cases GROUP BY run_id ORDER BY run_id"
        ).fetchall()
    finally:
        conn.close()
    print(f"  runs: {n_runs}   cases: {n_cases}")
    for run_id, n in per_run:
        print(f"    {run_id}: {n} cases")
    return 0


def main() -> None:
    """Run the full demo pipeline."""
    args = parse_args()

    # D1: validate run_id BEFORE any run dir / report path is built from it (path-traversal /
    # empty / collision guard). Clean error + non-zero exit, never a raw traceback.
    try:
        validate_run_id(args.run_id)
    except ValueError as exc:
        print(f"[ERROR] {exc}")
        sys.exit(2)

    # D4: --from-cache regenerates the Baseline report only; a named --run-id produces no
    # Results Store row in the cache branch (sweep_config is None). Warn, don't change behavior.
    if args.from_cache and args.run_id != BASELINE_RUN_ID:
        warnings.warn(
            f"--from-cache regenerates the baseline report only; --run-id '{args.run_id}' "
            "will not produce a Results Store row (run without --from-cache to compute a "
            "named run).",
            stacklevel=2,
        )

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 1: DNV Freespan / VIV Screening Analysis")
    print("  DNV-RP-F105 Simplified Methodology")
    print("=" * 60)

    # Decide cache-vs-recompute up front (no printed output here, so order is unchanged).
    # The Sweep Config is loaded ONLY in the recompute branch so --from-cache works
    # without the yaml (NBC-1). When recomputing, the resolved catalog paths from the
    # config drive catalog loading (FIX 2); the cache branch keeps today's defaults.
    results_path = RESULTS_DIR / "demo_01_freespan_results.json"
    use_cache = args.from_cache and results_path.exists() and not args.force

    sweep_config = None
    pipe_catalog_path: Optional[Path] = None
    jumper_catalog_path: Optional[Path] = None
    if not use_cache:
        from sweep_config import load_sweep_config

        config_path = args.input or DEFAULT_SWEEP_CONFIG_PATH
        sweep_config = load_sweep_config(config_path)
        # Resolve relative catalog paths RELATIVE TO THE YAML FILE'S OWN DIRECTORY (not cwd),
        # so an --input yaml's catalogs take effect. The committed Baseline yaml lives in
        # inputs/ but references data/pipelines.json (which sits under SCRIPT_DIR/data, a
        # sibling of inputs/, == today's DATA_DIR). To keep the Baseline byte-identical, fall
        # back to DATA_DIR/<basename> when the yaml-relative path does not resolve to a file.
        config_dir = Path(config_path).resolve().parent
        pipe_catalog_path = _resolve_catalog_path(config_dir, sweep_config.pipelines_catalog)
        jumper_catalog_path = _resolve_catalog_path(config_dir, sweep_config.jumpers_catalog)

    # 1. Load input data
    print("\n[1/7] Loading input data...")
    pipe_data, pipe_catalog = load_pipe_catalog(pipe_catalog_path)
    print(f"  Loaded {len(pipe_catalog)} pipe sizes from pipelines.json")

    jumper_data, jumper_props = load_jumper_catalog(jumper_catalog_path)
    print(f"  Loaded jumper catalog: {jumper_props['nominal_size']} "
          f"(OD={jumper_props['od_mm']}mm, WT={jumper_props['wt_mm']}mm)")

    # Verify required pipe sizes exist
    for size in PIPELINE_SIZES:
        pipe = get_pipe_by_size(pipe_catalog, size)
        if pipe is None:
            print(f"  [WARN] Pipe size {size} not found in catalog!")
        else:
            wt = get_thinnest_wt(pipe)
            print(f"  {size}: OD={pipe['od_mm']}mm, WT={wt['wt_mm']}mm ({wt['schedule_approx']})")

    # 2. Run or load parametric sweep
    if use_cache:
        print("\n[2/7] Loading cached results...")
        results, df = load_cached_results(results_path)
        total_cases = len(results)
        print(f"  Loaded {total_cases} cached cases from {results_path.name}")
    else:
        print("\n[2/7] Running parametric sweep...")
        print(f"  Loaded sweep config: {config_path.name}")
        # Phase-1 honesty (FIX 3): only cases[] is config-driven. If the config promotes any
        # of content_density / boundary_condition / wt_selection beyond the chart defaults,
        # the charts/summary/metadata still reflect baseline defaults — warn so it is not
        # silent. The Baseline does NOT diverge, so this never fires for it (stdout identical).
        if _config_diverges_from_chart_defaults(sweep_config):
            warnings.warn(
                "Sweep config promotes content_density / boundary_condition / wt_selection "
                "beyond the chart defaults; charts/summary reflect baseline defaults — only "
                "the cases table reflects this config (Phase-1; see docs/adr/0002).",
                stacklevel=2,
            )
        results, df = run_parametric_sweep(pipe_catalog, jumper_props, sweep_config)
        total_cases = len(results)
        print(f"  Total cases: {total_cases}")

    # 3. Build summary statistics
    print("\n[3/7] Building summary statistics...")
    summary_df = build_summary_stats(df)
    print(summary_df.to_string(index=False))

    # Count key metrics
    n_pass = len(df[df["status"] == STATUS_PASS])
    n_inline = len(df[df["status"] == STATUS_INLINE_ONLY])
    n_fail_cf = len(df[df["status"] == STATUS_FAIL_CF])
    n_lockin = len(df[df["status"] == STATUS_FAIL_LOCKIN])
    print(f"\n  Overall: {n_pass} PASS, {n_inline} INLINE, "
          f"{n_fail_cf} FAIL_CF, {n_lockin} LOCKIN")

    # 4. Build charts
    print("\n[4/7] Building charts...")

    print("  Chart 1: Natural Frequency vs Span Length...")
    fig1 = build_chart_1_frequency_vs_span(pipe_catalog, jumper_props)

    print("  Chart 2: VIV Onset Screening Map...")
    fig2 = build_chart_2_viv_onset_map(pipe_catalog)

    print("  Chart 3: Max Allowable Span Heatmap...")
    fig3 = build_chart_3_max_span_heatmap(pipe_catalog, jumper_props)

    print("  Chart 4: Pass/Fail Screening Matrix...")
    fig4 = build_chart_4_pass_fail_matrix(df)

    print("  Chart 5: Jumper vs Pipeline Comparison...")
    fig5 = build_chart_5_jumper_vs_pipeline(pipe_catalog, jumper_props)

    # 5. Build HTML report
    print("\n[5/7] Building HTML report...")
    # Baseline Run keeps the legacy output path (byte-identical); a named Run writes a per-run
    # report under output/parametric/demo_01/<run_id>/report.html.
    if args.run_id == BASELINE_RUN_ID:
        report_path: Optional[Path] = None  # build_report defaults to the legacy Baseline path
    else:
        report_path = OUTPUT_DIR / "parametric" / DEMO_ID / args.run_id / "report.html"
    build_report(fig1, fig2, fig3, fig4, fig5, summary_df, results, total_cases,
                 output_path=report_path)
    if report_path is not None:
        print(f"  Per-run report written to: {report_path}")

    # 6. Save JSON results
    print("\n[6/7] Saving JSON results...")
    # GUARD: the JSON cases[] must stay exactly the 15 frozen keys. If df-enrichment (or any
    # future change) ever mutated a result dict, fail loudly here rather than silently break
    # the golden byte-identity.
    if results:
        actual_keys = set(results[0].keys())
        assert actual_keys == set(FROZEN_RESULT_KEYS), (
            "JSON result keys drifted from the 15 frozen keys "
            f"(expected {sorted(FROZEN_RESULT_KEYS)}, got {sorted(actual_keys)})"
        )
    saved_path = save_results_json(results, summary_df, total_cases)
    print(f"  Results saved to: {saved_path}")

    # 6b. Results Store (SQLite + per-run CSV/manifest). RECOMPUTE branch only — the
    # --from-cache branch has no resolved config (sweep_config is None) to snapshot.
    if sweep_config is not None:
        from results_store import write_run

        run_dir = write_run(
            run_id=args.run_id,
            resolved_config=sweep_config,
            df=df,
            summary_df=summary_df,
            base_dir=RESULTS_DIR,
        )
        print(f"  Results Store updated: {run_dir}")

    # 7. Done
    elapsed = time.time() - start_time
    print(f"\n[7/7] Complete!")
    print(f"{'=' * 60}")
    print(f"  Total cases analysed:  {total_cases}")
    print(f"  HTML report:           output/demo_01_freespan_report.html")
    print(f"  JSON results:          results/demo_01_freespan_results.json")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print(f"{'=' * 60}")


if __name__ == "__main__":
    # Argv-sniff dispatch (BD-1): the `lookup` / `rebuild-db` subcommands are handled by their
    # OWN parsers over sys.argv[2:] and NEVER reach the sweep parser, so the no-arg / --from-cache
    # / --force / --input sweep path is provably untouched and byte-identical.
    if len(sys.argv) > 1 and sys.argv[1] == "lookup":
        sys.exit(lookup_main(sys.argv[2:]))
    elif len(sys.argv) > 1 and sys.argv[1] == "rebuild-db":
        sys.exit(rebuild_main(sys.argv[2:]))
    else:
        main()
