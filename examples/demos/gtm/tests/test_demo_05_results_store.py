# ABOUTME: Tests for the demo_05 SQLite Results Store (additive; prefixed phase columns).
# ABOUTME: Rebuild-identity (per-cell+typeof), None->NULL ERROR round-trip, composite PK, repo-rel paths.
"""Tests for the demo_05 SQLite results store.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_05_results_store.py -q
"""
from __future__ import annotations

import csv
import json
import os
import sqlite3
import sys
from pathlib import Path

import pytest

HERE = Path(os.path.dirname(os.path.abspath(__file__)))
GTM_DIR = HERE.parent
SRC_DIR = GTM_DIR.parent.parent.parent / "src"

sys.path.insert(0, str(GTM_DIR))

from results_store_demo05 import (  # noqa: E402
    _CASE_COL_NAMES,
    _PHASE_SPECS,
    _phase_col,
    _q,
    rebuild_db,
    write_run,
)
import demo_05_deepwater_rigid_jumper_installation as demo  # noqa: E402
from sweep_config_demo05 import load_demo05_config  # noqa: E402

_BASELINE_CONFIG_PATH = GTM_DIR / "inputs" / "demo_05_jumper.yml"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _demo_root(results_dir: Path) -> Path:
    return results_dir / "parametric" / "demo_05"


def _dump_cells(db_path: Path):
    """Return {pk-tuple: {col: (value, typeof)}} for every cases row."""
    cols = list(_CASE_COL_NAMES)
    typeof_select = ", ".join(f"{_q(c)}, typeof({_q(c)})" for c in cols)
    conn = sqlite3.connect(str(db_path))
    try:
        rows = conn.execute(f"SELECT {typeof_select} FROM cases").fetchall()
    finally:
        conn.close()
    out = {}
    for row in rows:
        cell = {c: (row[2 * i], row[2 * i + 1]) for i, c in enumerate(cols)}
        pk = (
            cell["run_id"][0],
            cell["vessel_id"][0],
            cell["jumper_id"][0],
            cell["water_depth_m"][0],
            cell["hs_m"][0],
        )
        out[pk] = cell
    return out


# Every phase column name (everything that must go NULL on the ERROR path's phases={}).
_PHASE_COLS = [
    _phase_col(prefix, suffix)
    for prefix, suffixes in _PHASE_SPECS
    for suffix, _typ in suffixes
]


def _good_case(vessel_id, jumper_id, depth, hs, status="GO", max_util=0.25):
    """A full demo_05 record with all five phase dicts populated (feasible)."""
    return {
        "vessel": "Large CSV",
        "vessel_id": vessel_id,
        "jumper_id": jumper_id,
        "jumper_name": "Jumper-60m",
        "length_m": 60.0,
        "water_depth_m": depth,
        "hs_m": hs,
        "tp_s": round(4.0 * (hs ** 0.5), 2),
        "phases": {
            "lift_off": {
                "phase": "lift_off", "mass_total_kg": 59389.4, "hook_load_te": 65.36,
                "crane_swl_te": 5000, "daf": 1.10, "utilisation": 0.0131, "status": "PASS",
            },
            "in_air_bending": {
                "phase": "in_air_bending", "weight_per_m_n": 906.49, "lift_daf": 1.10,
                "lift_span_m": 22.5, "w_eff_n_per_m": 997.14, "bending_moment_knm": 63.1,
                "bending_stress_mpa": 118.0, "allowable_stress_mpa": 268.8,
                "utilisation": max_util, "status": "PASS",
            },
            "splash_zone": {
                "phase": "splash_zone", "slamming_force_kn": 12.3, "drag_force_kn": 0.4,
                "hook_load_splash_te": 86.2, "crane_swl_te": 5000, "daf": 1.30,
                "v_wave_ms": 2.22, "v_rel_ms": 2.72, "utilisation": 0.0172, "status": "PASS",
            },
            "lowering": {
                "phase": "lowering", "static_tension_kn": 105.3, "dynamic_load_kn": 8.1,
                "max_tension_te": 11.6, "wire_mbl_te": 1200, "allowable_te": 1020.0,
                "utilisation": 0.0114, "status": "PASS",
            },
            "tie_in": {
                "phase": "tie_in", "tiein_span_m": 12.6, "current_load_n_per_m": 33.69,
                "self_weight_load_n_per_m": 0.0, "resultant_load_n_per_m": 33.69,
                "deflection_mm": 0.9, "tolerance_mm": 50.0, "utilisation": 0.018,
                "status": "PASS",
            },
        },
        "phase_utilisations": {
            "lift_off": 0.0131, "in_air_bending": max_util, "splash_zone": 0.0172,
            "lowering": 0.0114, "tie_in": 0.018,
        },
        "overall_status": status,
        "governing_phase": "in_air_bending",
        "max_utilisation": max_util,
    }


def _error_case(vessel_id, jumper_id, depth, hs):
    """The normalized ERROR record: phases absent, max_utilisation None
    (mirrors run_parametric_sweep's except branch). The store flattens the missing
    phases to NULL columns identically on write and rebuild."""
    return {
        "vessel": "Large CSV",
        "jumper_name": "Jumper-100m",
        "vessel_id": vessel_id,   # not emitted by the real except branch, but the PK needs it
        "jumper_id": jumper_id,   # for the synthetic round-trip; the real branch's missing keys
        "length_m": 100.0,        # would just be NULL (and unusable as a PK, hence supplied here).
        "water_depth_m": depth,
        "hs_m": hs,
        "overall_status": "ERROR",
        "max_utilisation": None,
        "governing_phase": "boom",  # the except branch stores str(exc) here.
        # No "phases" key at all -> every phase column NULL.
    }


class _Cfg:
    """Minimal stand-in for ResolvedDemo05Config for synthetic write_run calls."""
    demo_id = "demo_05"
    code_ref = ""
    vessels: list = ["Large CSV", "Medium CSV"]
    lengths_m: list = [20.0, 60.0, 100.0]
    depths: list = [500, 1500, 3000]
    hs: list = [1.0, 2.0, 3.0]
    daf_liftoff = 1.10
    daf_splash = 1.30
    rigging_mass_kg = 5000.0
    cs_slamming = 3.141592653589793
    cd_cylinder = 1.2
    ca_cylinder = 1.0
    v_lowering = 0.5
    v_current = 0.5
    splash_submerged_length_m = 10.0
    wire_allowable_factor = 0.85
    bending_allowable = 0.6
    cable_unit_weight_sub_n_per_m = 50.0
    tie_in_tolerance_mm = 50.0
    reference_hs = 2.0
    tp_coefficient = 4.0
    go_marginal_threshold = 0.85
    nogo_utilisation = 1.0
    seawater_density_kg_m3 = 1025.0
    gravity_m_s2 = 9.80665
    lift_span_fraction = 0.5
    lift_span_m = None
    lift_moment_coeff = 8.0
    tiein_unsupported_span_fraction = 0.28
    tiein_unsupported_span_m = None
    tiein_include_self_weight = False
    tiein_deflection_coeff = 76.8
    vessels_path = Path("/mnt/local-analysis/digitalmodel/examples/demos/gtm/data/csv_hlv_vessels.json")
    jumpers_path = Path("/mnt/local-analysis/digitalmodel/examples/demos/gtm/data/rigid_jumpers.json")
    results_root = Path("/mnt/local-analysis/digitalmodel/examples/demos/gtm/results")
    output_root = Path("/mnt/local-analysis/digitalmodel/examples/demos/gtm/output")
    source_path = Path("/mnt/local-analysis/digitalmodel/examples/demos/gtm/inputs/demo_05_jumper.yml")


# ---------------------------------------------------------------------------
# Schema / column-pinning tests
# ---------------------------------------------------------------------------

def test_column_count_and_names_pinned(tmp_path):
    """56 columns; the prefixed phase columns are present, bare keys are NOT."""
    results = [_good_case("CSV-001", "JMP-060", 1500, 2.0)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        cols = [r[1] for r in conn.execute("PRAGMA table_info(cases)")]
        assert len(cols) == 56
        assert cols == _CASE_COL_NAMES
        for c in (
            "lift_off_utilisation", "lift_off_status",
            "in_air_bending_lift_span_m", "in_air_bending_w_eff_n_per_m",
            "in_air_bending_bending_stress_mpa",
            "splash_zone_utilisation", "lowering_max_tension_te",
            "tie_in_tiein_span_m", "tie_in_resultant_load_n_per_m",
            "tie_in_self_weight_load_n_per_m",
        ):
            assert c in cols, f"missing prefixed column {c}"
        # Bare repeated phase-dict keys never collide into a single column (they only
        # ever appear prefixed). length_m / hs_m / tp_s ARE legitimate top-level columns.
        for bare in ("status", "utilisation", "phase", "daf", "crane_swl_te"):
            assert bare not in cols, f"bare key {bare} should not be a column"
        # water_depth_m is INTEGER; hs_m is REAL; status/phase are TEXT.
        assert conn.execute(
            "SELECT DISTINCT typeof(water_depth_m) FROM cases"
        ).fetchall() == [("integer",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(hs_m) FROM cases"
        ).fetchall() == [("real",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(\"lift_off_status\") FROM cases"
        ).fetchall() == [("text",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(\"in_air_bending_phase\") FROM cases"
        ).fetchall() == [("text",)]
        # A span-model output is REAL.
        assert conn.execute(
            "SELECT DISTINCT typeof(\"in_air_bending_lift_span_m\") FROM cases"
        ).fetchall() == [("real",)]
    finally:
        conn.close()


def test_csv_header_equals_declared_columns(tmp_path):
    """The per-run cases.csv header == declared columns (minus run_id), 55 cols."""
    results = [_good_case("CSV-001", "JMP-060", 1500, 2.0)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    cases_csv = _demo_root(base) / "baseline" / "cases.csv"
    with cases_csv.open("r", newline="", encoding="utf-8") as fh:
        header = next(csv.reader(fh))
    expected = [c for c in _CASE_COL_NAMES if c != "run_id"]
    assert header == expected
    assert len(header) == 55


def test_composite_pk_uniqueness(tmp_path):
    """The composite (run_id, vessel_id, jumper_id, water_depth_m, hs_m) PK is enforced:
    a duplicate within a run raises IntegrityError; distinct keys all coexist."""
    # Same PK, different status — the executemany INSERT must raise on the dupe within a run.
    results = [
        _good_case("CSV-001", "JMP-060", 1500, 2.0, status="GO"),
        _good_case("CSV-001", "JMP-060", 1500, 2.0, status="MARGINAL"),
    ]
    base = tmp_path / "results"
    with pytest.raises(sqlite3.IntegrityError):
        write_run("baseline", _Cfg(), results, base)

    # Distinct PKs across the five key axes all coexist.
    results = [
        _good_case("CSV-001", "JMP-060", 1500, 2.0),
        _good_case("CSV-002", "JMP-060", 1500, 2.0),   # vary vessel_id
        _good_case("CSV-001", "JMP-100", 1500, 2.0),   # vary jumper_id
        _good_case("CSV-001", "JMP-060", 3000, 2.0),   # vary depth
        _good_case("CSV-001", "JMP-060", 1500, 3.0),   # vary hs
    ]
    base2 = tmp_path / "results2"
    write_run("baseline", _Cfg(), results, base2)
    db = _demo_root(base2) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        assert conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0] == 5
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# Core identity test: direct-write db == rebuild-from-cases.csv db (per-cell + typeof).
# ---------------------------------------------------------------------------

def test_rebuild_identity_cell_for_cell(tmp_path):
    """Direct-write db == rebuild-from-cases.csv db, every cell + every typeof."""
    results = [
        _good_case("CSV-001", "JMP-020", 500, 1.0, status="GO", max_util=0.05),
        _good_case("CSV-002", "JMP-060", 1500, 2.0, status="MARGINAL", max_util=0.88),
        _good_case("CSV-001", "JMP-100", 3000, 3.0, status="NO_GO", max_util=1.22),
        _error_case("CSV-002", "JMP-100", 2000, 2.5),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 4
    rebuild_db(base)
    after = _dump_cells(db)
    assert before == after, "rebuild not per-cell + per-typeof identical"


# ---------------------------------------------------------------------------
# The headline finding: ERROR-path phases={} -> NULL round-trip.
# ---------------------------------------------------------------------------

def test_error_phases_none_null_round_trip(tmp_path):
    """An ERROR row (no/empty phases dict + None max_utilisation) -> NULL columns,
    identically on BOTH the direct-write path AND the rebuild-from-cases.csv path."""
    results = [
        _good_case("CSV-001", "JMP-060", 1500, 2.0, status="GO", max_util=0.18),
        _error_case("CSV-002", "JMP-100", 2000, 2.5),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"

    direct = _dump_cells(db)
    rebuild_db(base)
    rebuilt = _dump_cells(db)
    assert direct == rebuilt, "rebuild not per-cell + per-typeof identical (ERROR/NULL)"

    # --- The ERROR row: phases={} -> every phase column NULL; None -> SQL NULL. ---
    err_pk = ("baseline", "CSV-002", "JMP-100", 2000, 2.5)
    err = direct[err_pk]
    for col in _PHASE_COLS:
        value, tof = err[col]
        assert value is None, f"ERROR {col}: expected NULL, got {value!r}"
        assert tof == "null", f"ERROR {col}: expected typeof null, got {tof}"
    # max_utilisation None -> NULL (never 0, never "nan").
    value, tof = err["max_utilisation"]
    assert value is None and tof == "null"
    # Real top-level fields survive with their declared types.
    assert err["overall_status"] == ("ERROR", "text")
    assert err["governing_phase"] == ("boom", "text")
    assert err["water_depth_m"] == (2000, "integer")  # BD-3 INTEGER even on ERROR
    assert err["hs_m"] == (2.5, "real")               # BD-3 REAL even on ERROR

    # --- The GO row: phase columns carry their real values + typeof. ---
    good_pk = ("baseline", "CSV-001", "JMP-060", 1500, 2.0)
    good = direct[good_pk]
    assert good["lift_off_status"] == ("PASS", "text")
    assert good["in_air_bending_utilisation"] == (0.18, "real")
    assert good["in_air_bending_lift_span_m"] == (22.5, "real")
    assert good["tie_in_self_weight_load_n_per_m"] == (0.0, "real")
    assert good["max_utilisation"] == (0.18, "real")


# ---------------------------------------------------------------------------
# Artifacts + run counts.
# ---------------------------------------------------------------------------

def test_write_run_emits_four_artifacts_and_counts(tmp_path):
    results = [
        _good_case("CSV-001", "JMP-020", 500, 1.0, status="GO"),
        _good_case("CSV-001", "JMP-060", 1500, 2.0, status="MARGINAL", max_util=0.9),
        _good_case("CSV-001", "JMP-100", 3000, 3.0, status="NO_GO", max_util=1.22),
        _error_case("CSV-002", "JMP-100", 2000, 2.5),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    root = _demo_root(base)
    assert (root / "baseline" / "cases.csv").exists()
    assert (root / "baseline" / "manifest.json").exists()
    assert (root / "index.csv").exists()
    assert (root / "results.db").exists()

    db = root / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        run_id, total, n_go, n_marg, n_nogo, n_err = conn.execute(
            "SELECT run_id, total_cases, n_go, n_marginal, n_nogo, n_error FROM runs"
        ).fetchone()
        assert run_id == "baseline"
        assert total == 4
        assert (n_go, n_marg, n_nogo, n_err) == (1, 1, 1, 1)
        assert n_go + n_marg + n_nogo + n_err == total
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# Manifest path fields are repo-relative (no machine-specific absolute paths).
# ---------------------------------------------------------------------------

def test_manifest_paths_are_repo_relative(tmp_path):
    """The committed manifest carries NO machine-specific absolute path: every path
    field is re-anchored at examples/ (no leading /mnt/ or /home/)."""
    results = [_good_case("CSV-001", "JMP-060", 1500, 2.0)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    manifest = json.loads(
        (_demo_root(base) / "baseline" / "manifest.json").read_text(encoding="utf-8")
    )
    rc = manifest["resolved_config"]
    for field in ("vessels_path", "jumpers_path", "results_root", "output_root", "source_path"):
        val = rc[field]
        assert val.startswith("examples/"), f"{field} not repo-relative: {val!r}"
        assert "/mnt/" not in val and "/home/" not in val, f"{field} leaks abs path: {val!r}"
    # input_ref (top-level) is also repo-relative.
    assert manifest["input_ref"].startswith("examples/")
    assert "/mnt/" not in manifest["input_ref"] and "/home/" not in manifest["input_ref"]


# ---------------------------------------------------------------------------
# The baseline (300-case) run: direct-write == rebuild, cell-for-cell + per-typeof.
# ---------------------------------------------------------------------------

def test_baseline_rebuild_identity(tmp_path):
    """The full 300-case baseline run rebuilds per-cell + per-typeof identically.

    The baseline is all-feasible (no ERROR cases), so the phases={}/None->NULL round-trip
    is covered by the synthetic test_error_phases_none_null_round_trip, not here.
    """
    config = load_demo05_config(_BASELINE_CONFIG_PATH)
    vessels_path = GTM_DIR / "data" / "csv_hlv_vessels.json"
    jumpers_path = GTM_DIR / "data" / "rigid_jumpers.json"
    vessels = json.loads(vessels_path.read_text(encoding="utf-8"))["vessels"]
    jdata = json.loads(jumpers_path.read_text(encoding="utf-8"))
    common, jumpers = jdata["common_properties"], jdata["jumpers"]
    results, _df = demo.run_parametric_sweep(vessels, common, jumpers, config=config)

    base = tmp_path / "results"
    write_run("baseline", config, results, base)
    db = _demo_root(base) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 300
    rebuild_db(base)
    after = _dump_cells(db)
    assert before == after
