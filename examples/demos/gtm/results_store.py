#!/usr/bin/env python3
# ABOUTME: demo_01 Results Store — SQLite fast-read cache + per-run CSV/manifest text source (ADR-0003).
# ABOUTME: write_run() persists one run; rebuild_db() regenerates the cache byte-identically from text.
"""demo_01 Results Store (sqlite3 stdlib only).

Per ADR-0003 the Store is a two-layer artifact:

  - **Text source of truth (git-tracked):** per-run ``cases.csv`` + ``manifest.json`` and a
    repo-level ``index.csv``. These are the committed, portable, mergeable artifacts.
  - **SQLite cache (gitignored, rebuildable):** ``results.db`` — the fast ``SELECT … WHERE``
    read path for live demo lookups. It is DERIVED from the text source; ``rebuild_db()``
    regenerates it byte-identically (per-cell, including SQLite ``typeof()``).

Layout under ``<base_dir>``::

    parametric/demo_01/index.csv                 (one row per run)
    parametric/demo_01/results.db                (SQLite cache — gitignored)
    parametric/demo_01/<run_id>/cases.csv        (enriched per-case table)
    parametric/demo_01/<run_id>/manifest.json    (resolved config snapshot + provenance)

Canonical-token rule (BD-1, byte-identity):
  ``e_over_d`` is stored as a TEXT canonical token on BOTH write paths (direct-write and
  rebuild): the demo's serialized form — a decimal string for finite gaps ("0.5", "1.0", …)
  and the literal "inf" for the jumper mid-water gap. ``span_m`` is INTEGER. Reading CSVs back
  uses explicit dtypes / ``keep_default_na=False`` so the token and the int survive the round
  trip and the rebuilt db matches the direct-write db cell-for-cell.
"""
from __future__ import annotations

import csv
import json
import math
import sqlite3
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List

try:
    import pandas as pd
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError("results_store requires pandas") from exc


DEMO_ID = "demo_01"

# Status keys, kept local so the module is importable without the demo module.
_STATUS_PASS = "PASS"
_STATUS_INLINE = "INLINE_ONLY"
_STATUS_FAIL_CF = "FAIL_CF"
_STATUS_FAIL_LOCKIN = "FAIL_LOCKIN"

# ---------------------------------------------------------------------------
# Explicit typed schema for the `cases` table (BD-1).
# Order matters: this is the canonical column order for CSV + SQLite on BOTH paths.
# (column_name, sqlite_type). `run_id` is the partition/PK component.
# ---------------------------------------------------------------------------
_CASE_COLUMNS: List[tuple[str, str]] = [
    ("run_id", "TEXT"),
    # --- inputs (incl. the 3 promoted axes) ---
    ("pipe_type", "TEXT"),
    ("nominal_size", "TEXT"),
    ("od_m", "REAL"),
    ("wt_m", "REAL"),
    ("span_m", "INTEGER"),
    ("v_current_ms", "REAL"),
    ("e_over_d", "TEXT"),          # canonical text token: "0.5"... or "inf" (BD-1)
    ("content_density_kg_m3", "REAL"),
    ("boundary_condition", "TEXT"),
    ("wt_selection", "TEXT"),
    ("schedule", "TEXT"),
    ("case_id", "TEXT"),
    # --- outputs ---
    ("f_n_hz", "REAL"),
    ("v_r", "REAL"),
    ("a_over_d", "REAL"),
    ("status", "TEXT"),
    ("max_allowable_span_m", "REAL"),
    ("m_eff_kg_per_m", "REAL"),
]

_CASE_COL_NAMES = [name for name, _ in _CASE_COLUMNS]
_CASE_COL_TYPES = {name: typ for name, typ in _CASE_COLUMNS}

# Columns coerced from their textual / dataframe form into the typed Python value the
# SQLite column expects. REAL -> float, INTEGER -> int, TEXT -> str.
_REAL_COLS = {name for name, typ in _CASE_COLUMNS if typ == "REAL"}
_INT_COLS = {name for name, typ in _CASE_COLUMNS if typ == "INTEGER"}
_TEXT_COLS = {name for name, typ in _CASE_COLUMNS if typ == "TEXT"}

_RUNS_COLUMNS: List[tuple[str, str]] = [
    ("run_id", "TEXT PRIMARY KEY"),
    ("utc", "TEXT"),
    ("resolved_yaml", "TEXT"),
    ("git_sha", "TEXT"),
    ("total_cases", "INTEGER"),
    ("n_pass", "INTEGER"),
    ("n_inline", "INTEGER"),
    ("n_fail_cf", "INTEGER"),
    ("n_lockin", "INTEGER"),
]

_INDEX_COLUMNS = [
    "run_id", "utc", "total_cases",
    "n_pass", "n_inline", "n_fail_cf", "n_lockin",
    "input_ref", "git_sha",
]


# ---------------------------------------------------------------------------
# Canonical serialization helpers
# ---------------------------------------------------------------------------

def _e_over_d_token(value: Any) -> str:
    """Canonical TEXT token for e_over_d (BD-1).

    Matches the demo's serialized form: finite gaps as their decimal string ("0.5", "1.0"),
    the jumper mid-water gap as "inf". Accepts the dataframe's mixed representation (float for
    finite, the string "inf" for mid-water) and the CSV round-trip (always strings).
    """
    if isinstance(value, str):
        s = value.strip()
        if s.lower() == "inf":
            return "inf"
        # A finite numeric string from a CSV round trip; normalize through float() so
        # "1.0" / "0.5" stay the same token the direct-write path produced.
        f = float(s)
    elif isinstance(value, float) and math.isinf(value):
        return "inf"
    else:
        f = float(value)
    # Reject NaN (and any non-finite that is not +inf) with a clear error rather than
    # silently emitting the token "nan", which would corrupt the canonical column (BD-1, D5).
    if math.isnan(f):
        raise ValueError(
            f"e_over_d gap token cannot be NaN; got {value!r}. Finite gaps must be a "
            "decimal value and the mid-water gap must be the float inf."
        )
    return str(f)


def _json_safe(obj: Any) -> Any:
    """Recursively convert a value to a JSON-safe form, mapping inf -> the token "inf"."""
    if isinstance(obj, float):
        if math.isinf(obj):
            return "inf"
        if math.isnan(obj):  # pragma: no cover — not produced by the demo.
            return "nan"
        return obj
    if isinstance(obj, dict):
        return {k: _json_safe(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_json_safe(v) for v in obj]
    if isinstance(obj, Path):
        return str(obj)
    return obj


def _serialize_resolved_config(resolved_config: Any) -> Dict[str, Any]:
    """Serialize a ResolvedSweepConfig to a JSON-safe dict snapshot.

    Works structurally (no import of the dataclass type) so the module stays decoupled.
    .inf gap-ratio values become the string "inf".
    """
    def sub(s: Any) -> Dict[str, Any]:
        return {
            "sizes": list(s.sizes),
            "span_lengths_m": [int(x) for x in s.span_lengths_m],
            "current_velocities_ms": [float(x) for x in s.current_velocities_ms],
            "gap_ratios": [_e_over_d_token(x) for x in s.gap_ratios],
            "content_density_kg_m3": [float(x) for x in s.content_density_kg_m3],
            "c_n_values": [float(x) for x in s.c_n_values],
            "boundary_condition_labels": list(s.boundary_condition_labels),
            "wt_selection": list(s.wt_selection),
        }

    return {
        "demo_id": resolved_config.demo_id,
        "code_ref": resolved_config.code_ref,
        "pipelines_catalog": resolved_config.pipelines_catalog,
        "jumpers_catalog": resolved_config.jumpers_catalog,
        "source_path": str(resolved_config.source_path),
        "pipelines": sub(resolved_config.pipelines),
        "jumpers": sub(resolved_config.jumpers),
    }


def _git_sha() -> str:
    """Current git SHA, or "unknown" on any failure (detached/missing/non-repo)."""
    try:
        out = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True, text=True, check=True,
            cwd=Path(__file__).resolve().parent,
        )
        return out.stdout.strip() or "unknown"
    except Exception:
        return "unknown"


def _summary_counts(summary_df: "pd.DataFrame") -> Dict[str, int]:
    """Aggregate the per-(type,size) summary table into the four overall status counts."""
    return {
        "PASS": int(summary_df["PASS"].sum()),
        "INLINE_ONLY": int(summary_df["INLINE_ONLY"].sum()),
        "FAIL_CF": int(summary_df["FAIL_CF"].sum()),
        "FAIL_LOCKIN": int(summary_df["FAIL_LOCKIN"].sum()),
    }


# ---------------------------------------------------------------------------
# Path helpers
# ---------------------------------------------------------------------------

def _store_root(base_dir: Path) -> Path:
    return Path(base_dir) / "parametric" / DEMO_ID


def _db_path(base_dir: Path) -> Path:
    return _store_root(base_dir) / "results.db"


def _index_path(base_dir: Path) -> Path:
    return _store_root(base_dir) / "index.csv"


# ---------------------------------------------------------------------------
# Row coercion: df row (or CSV row) -> typed tuple in canonical column order
# ---------------------------------------------------------------------------

def _typed_case_row(run_id: str, raw: Dict[str, Any]) -> tuple:
    """Coerce one case record into a tuple of typed values in `_CASE_COL_NAMES` order.

    `raw` carries the case fields (df row as a dict, or a CSV row). The canonical e_over_d
    token and span int are produced identically on both the direct-write and rebuild paths.
    """
    values: List[Any] = []
    for name in _CASE_COL_NAMES:
        if name == "run_id":
            values.append(run_id)
            continue
        if name == "e_over_d":
            values.append(_e_over_d_token(raw["e_over_d"]))
            continue
        v = raw[name]
        if name in _INT_COLS:
            values.append(int(v))
        elif name in _REAL_COLS:
            values.append(float(v))
        else:  # TEXT
            values.append(str(v))
    return tuple(values)


# ---------------------------------------------------------------------------
# SQLite schema + transactional per-run write
# ---------------------------------------------------------------------------

def _create_schema(conn: sqlite3.Connection) -> None:
    runs_cols = ", ".join(f"{n} {t}" for n, t in _RUNS_COLUMNS)
    case_cols = ", ".join(f"{n} {t}" for n, t in _CASE_COLUMNS)
    conn.execute(f"CREATE TABLE IF NOT EXISTS runs ({runs_cols})")
    conn.execute(
        f"CREATE TABLE IF NOT EXISTS cases ({case_cols}, "
        "PRIMARY KEY (run_id, case_id))"
    )


def _upsert_run_into_db(
    conn: sqlite3.Connection,
    run_id: str,
    run_row: Dict[str, Any],
    case_rows: List[tuple],
) -> None:
    """Transactionally replace a run: delete-then-insert (never duplicate/orphan)."""
    placeholders = ", ".join("?" for _ in _CASE_COL_NAMES)
    run_cols = [n for n, _ in _RUNS_COLUMNS]
    run_placeholders = ", ".join("?" for _ in run_cols)
    conn.execute("BEGIN")
    try:
        conn.execute("DELETE FROM cases WHERE run_id=?", (run_id,))
        conn.execute("DELETE FROM runs WHERE run_id=?", (run_id,))
        conn.execute(
            f"INSERT INTO runs ({', '.join(run_cols)}) VALUES ({run_placeholders})",
            tuple(run_row[c] for c in run_cols),
        )
        conn.executemany(
            f"INSERT INTO cases ({', '.join(_CASE_COL_NAMES)}) VALUES ({placeholders})",
            case_rows,
        )
        conn.execute("COMMIT")
    except Exception:
        conn.execute("ROLLBACK")
        raise


# ---------------------------------------------------------------------------
# CSV writer (canonical tokens) + index upsert
# ---------------------------------------------------------------------------

def _write_cases_csv(path: Path, run_id: str, df: "pd.DataFrame") -> None:
    """Write the enriched per-case table to CSV with canonical tokens (no run_id column)."""
    csv_cols = [c for c in _CASE_COL_NAMES if c != "run_id"]
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(csv_cols)
        for _, row in df.iterrows():
            raw = row.to_dict()
            out: List[Any] = []
            for name in csv_cols:
                if name == "e_over_d":
                    out.append(_e_over_d_token(raw["e_over_d"]))
                elif name in _INT_COLS:
                    out.append(int(raw[name]))
                elif name in _REAL_COLS:
                    out.append(float(raw[name]))
                else:
                    out.append(str(raw[name]))
            writer.writerow(out)


def _write_index(path: Path, rows: List[Dict[str, Any]]) -> None:
    """Write index.csv from `rows`, each normalized to `_INDEX_COLUMNS` (missing -> "")."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=_INDEX_COLUMNS)
        writer.writeheader()
        for r in rows:
            writer.writerow({k: str(r.get(k, "")) for k in _INDEX_COLUMNS})


def _index_row_from_manifest(manifest: Dict[str, Any]) -> Dict[str, Any]:
    """Derive one index.csv row from a per-run manifest.json (the rebuild source)."""
    counts = manifest["summary_counts"]
    return {
        "run_id": manifest["run_id"],
        "utc": manifest["utc"],
        "total_cases": int(manifest["total_cases"]),
        "n_pass": int(counts["PASS"]),
        "n_inline": int(counts["INLINE_ONLY"]),
        "n_fail_cf": int(counts["FAIL_CF"]),
        "n_lockin": int(counts["FAIL_LOCKIN"]),
        "input_ref": manifest.get("raw_source_path", ""),
        "git_sha": manifest.get("git_sha", ""),
    }


def _upsert_index(base_dir: Path, row: Dict[str, Any]) -> None:
    """Read existing index.csv, replace the row with same run_id, rewrite (not blind append)."""
    path = _index_path(base_dir)
    rows: List[Dict[str, str]] = []
    if path.exists():
        with path.open("r", newline="", encoding="utf-8") as fh:
            # Normalize each prior row to the CURRENT columns so a schema drift in an existing
            # index.csv (extra/missing columns) can't crash the DictWriter (extrasaction='raise')
            # or KeyError a future write. Unknown columns are dropped; missing ones default "".
            rows = [
                {k: r.get(k, "") for k in _INDEX_COLUMNS}
                for r in csv.DictReader(fh)
            ]
    rows = [r for r in rows if r.get("run_id") != row["run_id"]]
    rows.append({k: str(row[k]) for k in _INDEX_COLUMNS})
    _write_index(path, rows)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def write_run(
    run_id: str,
    resolved_config: Any,
    df: "pd.DataFrame",
    summary_df: "pd.DataFrame",
    base_dir: Path,
) -> Path:
    """Persist one run: per-run cases.csv + manifest.json, index.csv upsert, SQLite upsert.

    The four artifacts are written SEQUENTIALLY in this order:
        1. ``<run_id>/cases.csv``     (text source of truth)
        2. ``<run_id>/manifest.json`` (text source of truth)
        3. ``index.csv``             (derived view — upsert by run_id)
        4. ``results.db``            (derived SQLite cache)
    Only step 4 is transactional (``_upsert_run_into_db`` BEGIN/COMMIT/ROLLBACK). Steps 1-3
    are plain file writes, so a crash mid-``write_run`` can leave the text artifacts and the
    derived db/index out of sync (e.g. cases.csv updated but the db rollback'd, or index.csv
    not yet rewritten). ``rebuild_db`` is the RECOVERY path: it discards and regenerates BOTH
    derived artifacts (results.db and index.csv) from the per-run cases.csv + manifest.json,
    reconciling any partial write back to the text source of truth.

    Returns the per-run directory (``<base_dir>/parametric/demo_01/<run_id>/``).
    """
    base_dir = Path(base_dir)
    run_dir = _store_root(base_dir) / run_id
    run_dir.mkdir(parents=True, exist_ok=True)

    counts = _summary_counts(summary_df)
    total_cases = int(len(df))
    utc = datetime.now(timezone.utc).isoformat()
    git_sha = _git_sha()
    resolved_snapshot = _serialize_resolved_config(resolved_config)
    input_ref = getattr(resolved_config, "source_path", "")

    # 1. cases.csv (canonical tokens)
    cases_csv = run_dir / "cases.csv"
    _write_cases_csv(cases_csv, run_id, df)

    # 2. manifest.json
    manifest = {
        "run_id": run_id,
        "demo_id": DEMO_ID,
        "utc": utc,
        "git_sha": git_sha,
        "code_ref": getattr(resolved_config, "code_ref", ""),
        "raw_source_path": str(input_ref),
        "total_cases": total_cases,
        "summary_counts": {
            "PASS": counts["PASS"],
            "INLINE_ONLY": counts["INLINE_ONLY"],
            "FAIL_CF": counts["FAIL_CF"],
            "FAIL_LOCKIN": counts["FAIL_LOCKIN"],
        },
        "resolved_config": _json_safe(resolved_snapshot),
    }
    with (run_dir / "manifest.json").open("w", encoding="utf-8") as fh:
        json.dump(manifest, fh, indent=2)

    # 3. index.csv upsert
    _upsert_index(base_dir, {
        "run_id": run_id,
        "utc": utc,
        "total_cases": total_cases,
        "n_pass": counts["PASS"],
        "n_inline": counts["INLINE_ONLY"],
        "n_fail_cf": counts["FAIL_CF"],
        "n_lockin": counts["FAIL_LOCKIN"],
        "input_ref": str(input_ref),
        "git_sha": git_sha,
    })

    # 4. SQLite upsert (delete-then-insert, transactional)
    case_rows = [_typed_case_row(run_id, row.to_dict()) for _, row in df.iterrows()]
    run_row = {
        "run_id": run_id,
        "utc": utc,
        "resolved_yaml": json.dumps(_json_safe(resolved_snapshot), sort_keys=True),
        "git_sha": git_sha,
        "total_cases": total_cases,
        "n_pass": counts["PASS"],
        "n_inline": counts["INLINE_ONLY"],
        "n_fail_cf": counts["FAIL_CF"],
        "n_lockin": counts["FAIL_LOCKIN"],
    }
    conn = sqlite3.connect(_db_path(base_dir))
    try:
        _create_schema(conn)
        _upsert_run_into_db(conn, run_id, run_row, case_rows)
    finally:
        conn.close()

    return run_dir


def rebuild_db(base_dir: Path) -> Path:
    """Drop + recreate the SQLite cache AND index.csv from all per-run cases.csv + manifest.json.

    This is the recovery path (see ``write_run``): both derived artifacts — ``results.db`` and
    ``index.csv`` — are regenerated from scratch from the per-run text source of truth, so a
    partial ``write_run`` is reconciled. ``index.csv`` gets one row per per-run manifest.json.

    Reads CSVs with explicit dtypes / keep_default_na=False so e_over_d stays the text token
    and span_m stays int (BD-1). The result is byte-identical (per-cell, incl. typeof()) to the
    direct-write db. Returns the db path.
    """
    base_dir = Path(base_dir)
    db_path = _db_path(base_dir)
    if db_path.exists():
        db_path.unlink()
    root = _store_root(base_dir)
    index_rows: List[Dict[str, Any]] = []

    conn = sqlite3.connect(db_path)
    try:
        _create_schema(conn)
        if not root.exists():
            _write_index(_index_path(base_dir), index_rows)
            return db_path
        for run_dir in sorted(p for p in root.iterdir() if p.is_dir()):
            cases_csv = run_dir / "cases.csv"
            manifest_path = run_dir / "manifest.json"
            if not (cases_csv.exists() and manifest_path.exists()):
                continue
            run_id = run_dir.name

            # All columns read as str (keep_default_na=False stops pandas turning "inf"/blank
            # into floats/NaN); _typed_case_row then coerces per the explicit schema (BD-1).
            df = pd.read_csv(cases_csv, dtype=str, keep_default_na=False)

            with manifest_path.open("r", encoding="utf-8") as fh:
                manifest = json.load(fh)
            counts = manifest["summary_counts"]
            run_row = {
                "run_id": run_id,
                "utc": manifest["utc"],
                "resolved_yaml": json.dumps(
                    _json_safe(manifest["resolved_config"]), sort_keys=True
                ),
                "git_sha": manifest["git_sha"],
                "total_cases": int(manifest["total_cases"]),
                "n_pass": int(counts["PASS"]),
                "n_inline": int(counts["INLINE_ONLY"]),
                "n_fail_cf": int(counts["FAIL_CF"]),
                "n_lockin": int(counts["FAIL_LOCKIN"]),
            }
            case_rows = [
                _typed_case_row(run_id, row.to_dict()) for _, row in df.iterrows()
            ]
            _upsert_run_into_db(conn, run_id, run_row, case_rows)
            index_rows.append(_index_row_from_manifest(manifest))
    finally:
        conn.close()

    # Regenerate index.csv from scratch (one row per manifest) — a derived view like the db.
    _write_index(_index_path(base_dir), index_rows)

    return db_path
