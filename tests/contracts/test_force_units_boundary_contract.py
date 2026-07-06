"""Repo-level force/moment/pressure UNITS contract (#1447).

Guards the kN<->N (factor-1000) trap that caused three silent incidents in the
drilling-riser twin epic (#1372) -- each caught only by an adversarial plan
reviewer, never by CI:

  * #1373/PR#1379 -- schema mixed kN with an "SI internally" claim.
  * #1375/PR#1410 -- env-force model returns kN, DPState thrust is N; the slip
    silently reported "station held" for cases that actually drift off.
  * #1376/PR#1422 -- kN.m moment mistaken for a dimensionless UC.

This file enforces the convention documented in ``docs/UNITS.md`` in three layers:

  (a) NAMING-CONVENTION AST SCAN -- every new bare-float force-class parameter or
      dataclass/pydantic field in the four scanned packages must carry a unit
      suffix. Real legacy offenders are FROZEN in ``LEGACY_ALLOWLIST`` (a scan
      snapshot, ``# frozen 2026-07-06, do not extend``); the scan fails only on a
      NEW unsuffixed force name. An inline ``ast.parse`` self-test proves the
      scanner flags a fresh unsuffixed param.

  (b) BOUNDARY CONVERSION CHECKS -- five real kN<->N seams, each with a
      hand-calculated expectation and a documented x1000/div-1000
      mutation-rejection proof.

  (c) CENTRAL MAGNITUDE-WINDOW TABLE (``PLAUSIBLE``) -- one place to tune; every
      window is chosen so BOTH the x1000 and the div-1000 corruption of a typical
      value land outside it.

Out-of-scope / known residual vocabulary gaps (deferred, not silently missed):
  * Domain force-nouns outside the ``FORCE_NAME_RE`` regex (e.g. ``weight``,
    ``buoyancy``, ``slam``, ``daf``) are not scanned by name -- they surface only
    through the boundary tests where they cross a numeric seam.
  * Label-only kN touchpoints that are strings, not numeric seams (e.g.
    ``batch_parametric.py`` ``metric="max_tension_kN"``) are not conversions and
    are deliberately not covered here.
  * kPa/Pa pressure seams beyond the scanned packages (follow-on if a scan
    surfaces them).
"""
from __future__ import annotations

import ast
import math
import re
from pathlib import Path

import pytest

pytestmark = pytest.mark.contracts

# --------------------------------------------------------------------------- #
# Layer (a): naming-convention AST scanner
# --------------------------------------------------------------------------- #

#: Repo root and the digitalmodel source tree (this file: tests/contracts/*).
_REPO_ROOT = Path(__file__).resolve().parents[2]
_SRC_ROOT = _REPO_ROOT / "src"

#: Packages scanned for the force-units naming rule.
SCANNED_PACKAGES = (
    "digitalmodel/drilling_riser",
    "digitalmodel/orcaflex",
    "digitalmodel/subsea/mooring_analysis",
    "digitalmodel/motion_forecast",
)

#: A name is "force-class" if it contains one of these tokens (case-insensitive).
FORCE_NAME_RE = re.compile(
    r"(tension|force|thrust|moment|mbl|pretension|capacity|load)", re.IGNORECASE
)

#: Accepted force/moment/pressure unit suffixes. Case-insensitive so legacy
#: spellings ``_kN``, ``_kNm``, ``_MPa`` are honoured alongside ``_kn``/``_knm``/
#: ``_mpa``. Anchored to the END of the name.
UNIT_SUFFIX_RE = re.compile(r"_(n|kn|nm|knm|pa|mpa|kpa)$", re.IGNORECASE)

#: Annotations that count as a bare-float context.
_FLOAT_ANNOTATIONS = {
    "float",
    "Optional[float]",
    "float|None",
    "None|float",
    "typing.Optional[float]",
}


def _is_float_annotation(ann: ast.expr | None) -> bool:
    if ann is None:
        return False
    try:
        s = ast.unparse(ann).replace(" ", "")
    except Exception:  # pragma: no cover - defensive
        return False
    return s in _FLOAT_ANNOTATIONS


def _is_numeric_const(node: ast.expr | None) -> bool:
    """A bare int/float literal default (excludes bool, which is an int subclass)."""
    return (
        isinstance(node, ast.Constant)
        and isinstance(node.value, (int, float))
        and not isinstance(node.value, bool)
    )


def _scan_tree(tree: ast.AST, relpath: str) -> set[tuple[str, str]]:
    """Return ``{(relpath, symbol)}`` force-class names in a bare-float context
    that LACK a unit suffix."""
    offenders: set[tuple[str, str]] = set()

    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            posargs = list(node.args.posonlyargs) + list(node.args.args)
            default_map: dict[str, ast.expr] = {}
            if node.args.defaults:
                tail = posargs[len(posargs) - len(node.args.defaults):]
                for a, d in zip(tail, node.args.defaults):
                    default_map[a.arg] = d
            for a, d in zip(node.args.kwonlyargs, node.args.kw_defaults):
                if d is not None:
                    default_map[a.arg] = d

            for a in posargs + list(node.args.kwonlyargs):
                name = a.arg
                if name in ("self", "cls"):
                    continue
                if not FORCE_NAME_RE.search(name):
                    continue
                float_ctx = _is_float_annotation(a.annotation)
                if not float_ctx and a.annotation is None:
                    # bare numeric default with no annotation
                    float_ctx = _is_numeric_const(default_map.get(name))
                if float_ctx and not UNIT_SUFFIX_RE.search(name):
                    offenders.add((relpath, name))

        elif isinstance(node, ast.ClassDef):
            for stmt in node.body:
                if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                    name = stmt.target.id
                    if not FORCE_NAME_RE.search(name):
                        continue
                    if _is_float_annotation(stmt.annotation) and not UNIT_SUFFIX_RE.search(name):
                        offenders.add((relpath, name))

    return offenders


def scan_offenders(src_root: Path = _SRC_ROOT) -> set[tuple[str, str]]:
    """Scan every module in ``SCANNED_PACKAGES`` for unsuffixed force names."""
    offenders: set[tuple[str, str]] = set()
    for pkg in SCANNED_PACKAGES:
        pkg_dir = src_root / pkg
        for py in sorted(pkg_dir.rglob("*.py")):
            relpath = py.relative_to(src_root).as_posix()
            tree = ast.parse(py.read_text(encoding="utf-8"), filename=str(py))
            offenders |= _scan_tree(tree, relpath)
    return offenders


# --- Frozen snapshot of the REAL legacy offenders (TDD step 1). --------------- #
# frozen 2026-07-06, do not extend. New code MUST carry a unit suffix
# (_n/_kn/_nm/_knm/_pa/_mpa/_kpa) on bare-float force/tension/thrust/moment/mbl/
# pretension/capacity/load names -- fix the name, never append here.
LEGACY_ALLOWLIST: frozenset[tuple[str, str]] = frozenset(
    {
        # kN/kN.m legacy force seams (kN-ness in comments/docstrings only):
        ("digitalmodel/drilling_riser/envelope.py", "conductor_moment_capacity_kn_m"),
        ("digitalmodel/orcaflex/mooring_design.py", "horizontal_tension"),
        ("digitalmodel/orcaflex/mooring_design.py", "mbl"),
        ("digitalmodel/orcaflex/mooring_design.py", "pretension"),
        ("digitalmodel/orcaflex/mooring_design.py", "target_pretension"),
        ("digitalmodel/orcaflex/mooring_design.py", "top_tension"),
        ("digitalmodel/orcaflex/mooring_design.py", "vertical_tension_top"),
        ("digitalmodel/orcaflex/pipelay_analysis.py", "lay_tension"),
        ("digitalmodel/orcaflex/pipelay_analysis.py", "top_tension"),
        ("digitalmodel/orcaflex/viv_screening.py", "effective_tension"),
        ("digitalmodel/subsea/mooring_analysis/catenary.py", "horizontal_tension"),
        ("digitalmodel/subsea/mooring_analysis/catenary.py", "target_fairlead_tension"),
        ("digitalmodel/subsea/mooring_analysis/cli.py", "fairlead_tension"),
        ("digitalmodel/subsea/mooring_analysis/cli.py", "horizontal_tension"),
        ("digitalmodel/subsea/mooring_analysis/cli.py", "pretension"),
        # SI-native (N) legacy names lacking an explicit suffix:
        ("digitalmodel/motion_forecast/top_tension.py", "min_tension"),
        ("digitalmodel/motion_forecast/top_tension.py", "static_tension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "actual_mbl"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "current_force"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "fairlead_tension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "holding_capacity"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "horizontal_tension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "max_tension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "mbl"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "min_mbl_required"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "pretension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "total_force"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "touchdown_tension"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "wave_drift_force"),
        ("digitalmodel/subsea/mooring_analysis/models.py", "wind_force"),
        # Regex vocabulary false-positives frozen as-is (documented residual gaps;
        # dimensionless factors, geometric second moments [m^4], metre offsets,
        # kg masses, per-metre distributed loads, and percentage fields):
        ("digitalmodel/drilling_riser/envelope.py", "tensioner_stroke_m"),
        ("digitalmodel/drilling_riser/riser_response.py", "current_load_n_per_m"),
        ("digitalmodel/drilling_riser/section.py", "moment_of_inertia"),
        ("digitalmodel/drilling_riser/section.py", "polar_moment"),
        ("digitalmodel/motion_forecast/top_tension.py", "payload_mass"),
        ("digitalmodel/orcaflex/installation_analysis.py", "skew_load_factor"),
        ("digitalmodel/orcaflex/mooring_design.py", "mean_load_factor"),
        ("digitalmodel/orcaflex/riser_config.py", "tensioner_offset"),
        ("digitalmodel/orcaflex/riser_input_schema.py", "tensioner_offset"),
        ("digitalmodel/orcaflex/synthetic_rope_design.py", "dynamic_tension_range_pct_mbl"),
        ("digitalmodel/orcaflex/synthetic_rope_design.py", "mean_load_pct_mbl"),
    }
)


def test_scanner_flags_new_unsuffixed_force_param():
    """Self-test: the scanner MUST flag a fresh unsuffixed force param and MUST
    accept the suffixed spelling (inline ``ast.parse``, no filesystem)."""
    bad = ast.parse("def compute(new_tension: float) -> float:\n    return new_tension\n")
    good = ast.parse("def compute(new_tension_n: float) -> float:\n    return new_tension_n\n")

    bad_hits = _scan_tree(bad, "<self-test>")
    good_hits = _scan_tree(good, "<self-test>")

    assert ("<self-test>", "new_tension") in bad_hits, (
        "scanner failed to flag an unsuffixed force param"
    )
    assert good_hits == set(), (
        f"scanner wrongly flagged a suffixed force param: {good_hits}"
    )


def test_scanner_flags_unsuffixed_dataclass_field():
    """Self-test: the scanner MUST flag an unsuffixed float *field* too."""
    src = (
        "from dataclasses import dataclass\n"
        "@dataclass\n"
        "class C:\n"
        "    holding_capacity: float = 0.0\n"
        "    holding_capacity_kn: float = 0.0\n"
    )
    hits = _scan_tree(ast.parse(src), "<self-test>")
    assert ("<self-test>", "holding_capacity") in hits
    assert ("<self-test>", "holding_capacity_kn") not in hits


def test_no_new_unsuffixed_force_names():
    """Every force-class bare-float name in the scanned packages either carries a
    unit suffix or is a FROZEN legacy offender. A NEW unsuffixed name fails here."""
    offenders = scan_offenders()
    new = sorted(offenders - LEGACY_ALLOWLIST)
    assert not new, (
        "New unsuffixed force-class name(s) -- add a unit suffix "
        "(_n/_kn/_nm/_knm/_pa/_mpa/_kpa), do NOT extend LEGACY_ALLOWLIST:\n"
        + "\n".join(f"  {f}::{n}" for f, n in new)
    )


def test_allowlist_has_no_stale_entries():
    """The frozen allowlist must not carry entries that no longer exist (rot
    guard): a fixed/renamed legacy name should be DELETED from the allowlist."""
    offenders = scan_offenders()
    stale = sorted(LEGACY_ALLOWLIST - offenders)
    assert not stale, (
        "Stale LEGACY_ALLOWLIST entries (name fixed/renamed -- delete them):\n"
        + "\n".join(f"  {f}::{n}" for f, n in stale)
    )


# --------------------------------------------------------------------------- #
# Layer (c): central magnitude-window table
# --------------------------------------------------------------------------- #

#: Plausible magnitude windows (inclusive) for each seam quantity. Each window is
#: chosen so BOTH the x1000 and the div-1000 corruption of the typical value
#: (``_TYPICAL`` below) land OUTSIDE it -- proven by
#: ``test_windows_reject_factor_1000_mutations``. Tuning happens here only.
PLAUSIBLE: dict[str, tuple[float, float]] = {
    "top_tension_n": (1e5, 1e8),      # riser top tension [N] (MN class)
    "env_force_n": (1e5, 1e8),        # total environmental drift force [N]
    "thrust_n": (1e5, 1e7),           # DP available/commanded thrust [N]
    "mooring_mbl_kn": (1e2, 1e5),     # chain/rope/sling MBL [kN]
    "sling_tension_kn": (1e1, 1e4),   # sling leg tension [kN]
    "von_mises_util": (1e-2, 1e1),    # dimensionless combined UC
}

#: A representative value in each window (used only to PROVE the window rejects
#: both factor-1000 corruptions).
_TYPICAL: dict[str, float] = {
    "top_tension_n": 2.0e6,
    "env_force_n": 1.796e7,
    "thrust_n": 8.0e5,
    "mooring_mbl_kn": 6978.0,
    "sling_tension_kn": 1894.5,
    "von_mises_util": 0.1646,
}


def _in_window(key: str, value: float) -> bool:
    lo, hi = PLAUSIBLE[key]
    return lo <= value <= hi


def test_windows_reject_factor_1000_mutations():
    """Every plausible window admits its typical value but rejects BOTH the
    x1000 and the div-1000 corruption -- the property that makes the boundary
    checks below catch a kN<->N slip."""
    for key, typical in _TYPICAL.items():
        assert _in_window(key, typical), f"{key}: typical {typical} not in window"
        assert not _in_window(key, typical * 1000.0), f"{key}: x1000 not rejected"
        assert not _in_window(key, typical / 1000.0), f"{key}: div-1000 not rejected"


# --------------------------------------------------------------------------- #
# Layer (b): boundary conversion checks (hand-calc + mutation-rejection proof)
# --------------------------------------------------------------------------- #

from digitalmodel.drilling_riser.drift_off import (  # noqa: E402
    DRIFT_OFF,
    ESCALATE,
    STATION_HELD,
    drift_off_screen,
    environmental_force_n,
)
from digitalmodel.drilling_riser.envelope import (  # noqa: E402
    EnvelopeCriteria,
    RiserSection,
    SeaState,
    compute_operating_envelope,
)
from digitalmodel.drilling_riser.telemetry_inputs import (  # noqa: E402
    DPState,
    parse_snapshots,
)
from digitalmodel.subsea.mooring_analysis.models import (  # noqa: E402
    EnvironmentalConditions,
)


def test_envelope_von_mises_n_to_kn_seam():
    """``envelope._von_mises_utilisation`` feeds ``check_api_rp_2rd`` with
    ``tensions_kn = tension_n / 1000`` and ``moments_knm = moment_nm / 1000``.

    Hand-calc (offset=0, current=0 -> bending moment M=0, so the combined UC is
    the tension term only)::

        util = (T / Ty) / design_factor
        Ty   = SMYS * A_steel ,  A_steel = pi/4 (OD^2 - ID^2)

    With OD=0.5334 m, WT=0.0254 m, SMYS=4.48e8 Pa, df=0.67, T=2.0e6 N:
        A_steel = pi/4 (0.5334^2 - 0.4826^2) = 0.040477 m^2
        Ty      = 4.48e8 * 0.040477 = 1.8134e7 N
        util    = (2.0e6 / 1.8134e7) / 0.67 = 0.1646

    Mutation proof: a x1000 slip on the N->kN seam sends T -> 2.0e9 kN and
    util -> ~1.6e5; a div-1000 slip (feeding N as kN) sends util -> ~164. Both
    leave the plausible UC window (asserted below).
    """
    section = RiserSection(
        outer_diameter_m=0.5334, wall_thickness_m=0.0254, smys_pa=4.48e8
    )
    criteria = EnvelopeCriteria(
        flexjoint_angle_mean_deg=5.0,
        flexjoint_angle_max_deg=10.0,
        von_mises_design_factor=0.67,
    )
    tension_n = 2.0e6
    result = compute_operating_envelope(
        section=section,
        water_depth_m=1500.0,
        length_m=1500.0,
        tension_n=tension_n,
        criteria=criteria,
        offsets_pct=[0.0],
        current_speeds_mps=[0.0],
        seastates=[SeaState(hs_m=0.0, tp_s=8.0)],
    )
    util = float(result.per_limit_utilisation["von_mises"][0, 0, 0])

    id_m = 0.5334 - 2.0 * 0.0254
    a_steel = math.pi / 4.0 * (0.5334**2 - id_m**2)
    ty_n = 4.48e8 * a_steel
    expected = (tension_n / ty_n) / 0.67  # ~= 0.1646

    assert math.isclose(util, expected, rel_tol=1e-3, abs_tol=1e-4)
    assert _in_window("top_tension_n", tension_n)
    assert _in_window("von_mises_util", util)
    # factor-1000 mutations at this seam both leave the UC window
    assert not _in_window("von_mises_util", expected * 1000.0)  # x1000 slip
    assert not _in_window("von_mises_util", expected / 1000.0)  # div-1000 slip


def test_drift_off_kn_to_n_seam_go_nogo_flip():
    """``drift_off.environmental_force_n`` converts the kN env-force model to N
    (``* 1000.0``) before differencing against N-valued DP thrust. This guards
    the exact #1375 silent-"station held" failure.

    Hand-calc of the env force (designer empirical model, kN then x1000):
        F_wave = 0.5*1025*9.81*Hs^2*B/1000 = 0.5*1025*9.81*8^2*40/1000 = 12870.72 kN
        F_curr = 0.5*1025*1.0*(L*T)*V^2/1000 = 0.5*1025*3750*1.5^2/1000 = 4324.21875 kN
        F_wind = 0.5*1.225*1.0*A*V^2/1000 = 0.5*1.225*2000*25^2/1000   = 765.625 kN
        F_tot  = 17960.56375 kN  ->  f_env_n = 17,960,563.75 N

    Mutation proof: with eta=1.0 and 1.0 MN surviving thrust the correct N
    conversion gives f_net = 1.696e7 N > 0 (a drift/NO-GO verdict). Had the seam
    dropped the x1000 (kN read as N), f_env would be 17,960 N < 1.0 MN thrust ->
    f_net <= 0 -> STATION_HELD reported while the vessel is actually drifting off.
    """
    condition = EnvironmentalConditions(
        wave_hs=8.0,
        wave_tp=12.0,
        wave_direction=0.0,
        current_speed=1.5,
        current_direction=0.0,
        wind_speed=25.0,
        wind_direction=0.0,
    )
    config = {
        "vessel": {
            "length_m": 250.0,
            "beam_m": 40.0,
            "draft_m": 15.0,
            "displacement_t": 100000.0,
            "windage_area_m2": 2000.0,
        },
        "water_depth_ref_m": 1500.0,
        "m_eff_low_kg": 5.0e7,
        "m_eff_high_kg": 8.0e7,
        "t_eds_s": 120.0,
        "thrust_efficiency": 1.0,
    }

    f_env_n, _components_kn = environmental_force_n(condition, config)
    expected_f_env_n = 17_960_563.75
    assert math.isclose(f_env_n, expected_f_env_n, rel_tol=1e-6)
    assert _in_window("env_force_n", f_env_n)

    dp = DPState(total_available_thrust_n=1.0e6)
    section = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
    criteria = EnvelopeCriteria(
        flexjoint_angle_mean_deg=5.0,
        flexjoint_angle_max_deg=10.0,
        von_mises_design_factor=0.67,
    )
    res = drift_off_screen(
        dp,
        condition,
        section=section,
        water_depth_m=1500.0,
        length_m=1500.0,
        tension_n=2.0e6,
        criteria=criteria,
        config=config,
    )
    # correct N conversion -> net drift force > 0 -> a NO-GO verdict
    assert res.status in (DRIFT_OFF, ESCALATE)
    assert res.status != STATION_HELD
    assert math.isclose(res.f_net_n, f_env_n - 1.0e6, rel_tol=1e-9)

    # the div-1000 slip flips the verdict to a SILENT station-held (#1375)
    corrupted_f_net = f_env_n / 1000.0 - 1.0e6
    assert corrupted_f_net <= 0.0  # would classify STATION_HELD
    assert (f_env_n - 1.0e6) > 0.0  # correct conversion classifies drift
    assert not _in_window("env_force_n", f_env_n / 1000.0)  # div-1000 out of window
    assert not _in_window("env_force_n", f_env_n * 1000.0)  # x1000 out of window


def test_telemetry_kn_to_n_parse_seam():
    """``telemetry_inputs`` applies ``_KN_TO_N = 1000.0`` at the parse boundary:
    a fixture reporting 800 kN available thrust must yield exactly 800_000.0 N."""
    snaps = parse_snapshots(
        [
            {
                "vessel_offset_m": 12.0,
                "top_tension_kn": 2000,
                "total_available_thrust_kn": 800,
                "total_commanded_thrust_kn": 600,
            }
        ]
    )
    assert len(snaps) == 1
    s = snaps[0]
    assert s.dp.total_available_thrust_n == 800_000.0  # exact x1000
    assert s.dp.total_commanded_thrust_n == 600_000.0
    assert s.top_tension_n == 2_000_000.0
    assert _in_window("thrust_n", s.dp.total_available_thrust_n)
    assert _in_window("top_tension_n", s.top_tension_n)
    # a div-1000 slip -> 800 N; a x1000 slip -> 8e8 N; both leave the window
    assert not _in_window("thrust_n", 800.0)
    assert not _in_window("thrust_n", 800_000_000.0)


def test_mooring_check_mbl_plausibility_seam():
    """``mooring_design.MooringLineDesign.check_mbl`` divides a kN tension by a
    kN MBL. Hand-calc: 3000 kN / 6978 kN (R4 84 mm chain) = 0.4299."""
    from digitalmodel.orcaflex.mooring_design import (
        MOORING_MATERIAL_LIBRARY,
        MooringLineDesign,
        MooringLineSegment,
    )

    design = MooringLineDesign(
        segments=[MooringLineSegment(material_key="R4_84mm_chain", length=1000.0)]
    )
    max_tension_kn = 3000.0
    util = design.check_mbl(max_tension_kn)

    mbl = MOORING_MATERIAL_LIBRARY["R4_84mm_chain"].mbl  # 6978.0 kN
    assert _in_window("mooring_mbl_kn", mbl)
    expected = round(max_tension_kn / mbl, 4)  # 0.4299
    assert util["R4_84mm_chain"] == expected
    # if MBL were read in N (x1000) the util would be ~4.3e-4 (false huge margin);
    # a div-1000 MBL -> util ~430 (absurd). Both MBL corruptions leave the window.
    assert not _in_window("mooring_mbl_kn", mbl * 1000.0)  # x1000
    assert not _in_window("mooring_mbl_kn", mbl / 1000.0)  # div-1000


def test_sling_tension_kn_seam():
    """``installation_analysis.SlingConfig.calculate_sling_tension(hook_load_kN)``
    -- same-class kN seam. Hand-calc with defaults (n=4, 60 deg, SKF=1.25,
    tilt=1.05):
        T = W*SKF*tilt/(n*sin60) = 5000*1.25*1.05/(4*0.8660254) = 1894.5 kN
    """
    from digitalmodel.orcaflex.installation_analysis import SlingConfig

    cfg = SlingConfig()
    hook_load_kN = 5000.0
    out = cfg.calculate_sling_tension(hook_load_kN)

    sin60 = math.sin(math.radians(60.0))
    expected_t = round(5000.0 * 1.25 * 1.05 / (4.0 * sin60), 1)  # 1894.5
    assert out["sling_tension_per_leg_kN"] == expected_t
    assert _in_window("sling_tension_kn", out["sling_tension_per_leg_kN"])
    assert _in_window("mooring_mbl_kn", cfg.sling_mbl_kN)  # 2000 kN sling MBL
    # a x1000 hook-load slip -> ~1.9e6 kN; a div-1000 slip -> ~1.9 kN; both out
    assert not _in_window("sling_tension_kn", expected_t * 1000.0)  # x1000
    assert not _in_window("sling_tension_kn", expected_t / 1000.0)  # div-1000


if __name__ == "__main__":  # pragma: no cover - allowlist authoring helper
    for f, n in sorted(scan_offenders()):
        print(f"        ({f!r}, {n!r}),")
