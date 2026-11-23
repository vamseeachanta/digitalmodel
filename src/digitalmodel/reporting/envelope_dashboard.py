#!/usr/bin/env python3
"""Interactive utilisation envelope dashboard (single self-contained HTML).

ABOUTME: Turns a long-format table of utilisation results over a sea-state grid
(Hs x Tp x check) into one offline HTML page that shows the governing check in
every sea state and how close every other check is to its limit.

Input is one row per (sea state, check) with the columns in
:data:`REQUIRED_COLUMNS`:

``hs_m``
    Significant wave height, m (finite, > 0).
``tp_s``
    Peak period, s (finite, > 0).
``check``
    Name of the check or criterion.
``utilisation``
    U = demand / limit (finite, >= 0).

Optional columns ``demand``, ``limit`` and ``units`` are carried into the
detail view when present. A (sea state, check) pair that is absent from the
input renders as "not evaluated"; it is never coloured.

Colour bands (default threshold 0.80):

* green  ``U < amber_threshold`` (default 0.80)
* amber  ``amber_threshold <= U <= 1.0``
* red    ``U > 1.0``

The governing check of a sea state is the check with the highest U; ties go to
the check that appears first in the input.

Example::

    python -m digitalmodel.reporting.envelope_dashboard results.csv out.html \\
        --title "Lift envelope" --amber-threshold 0.80
"""

from __future__ import annotations

import argparse
import csv
import html
import json
import math
import re
from importlib import resources
from pathlib import Path
from typing import Any, Iterable, Mapping, Optional, Sequence, Union

DEFAULT_AMBER_THRESHOLD = 0.80
"""Default threshold 0.80: amber from U = 0.80."""

REQUIRED_COLUMNS = ("hs_m", "tp_s", "check", "utilisation")
OPTIONAL_COLUMNS = ("demand", "limit", "units")

_TEMPLATE = "envelope_dashboard.html"

Records = Sequence[Mapping[str, Any]]
Source = Union[str, Path, Iterable[Mapping[str, Any]], Any]


# ---------------------------------------------------------------------------
# Validation helpers
# ---------------------------------------------------------------------------


def _check_threshold(amber_threshold: float) -> float:
    t = _finite(amber_threshold, "amber_threshold")
    if not 0.0 < t < 1.0:
        raise ValueError(f"amber_threshold must lie in (0, 1); got {t}")
    return t


def _finite(value: Any, name: str) -> float:
    if isinstance(value, bool):
        raise ValueError(f"{name} must be a finite number; got {value!r}")
    if isinstance(value, str):
        if not value.strip():
            raise ValueError(f"{name} must be a finite number; got a blank value")
        try:
            value = float(value)
        except ValueError:
            raise ValueError(f"{name} must be a finite number; got {value!r}") from None
    try:
        x = float(value)
    except (TypeError, ValueError):
        raise ValueError(f"{name} must be a finite number; got {value!r}") from None
    if not math.isfinite(x):
        raise ValueError(f"{name} must be a finite number; got {x}")
    return x


def classify_band(u: float, amber_threshold: float = DEFAULT_AMBER_THRESHOLD) -> str:
    """Return ``"green"``, ``"amber"`` or ``"red"`` for utilisation ``u``.

    Green below the amber threshold, amber from the threshold up to and
    including U = 1.0, red above U = 1.0.
    """
    t = _check_threshold(amber_threshold)
    x = _finite(u, "utilisation")
    if x < 0:
        raise ValueError(f"utilisation must not be negative; got {x}")
    if x > 1.0:
        return "red"
    if x >= t:
        return "amber"
    return "green"


def load_records(source: Source) -> list[dict[str, Any]]:
    """Load records from a CSV path, a DataFrame-like object or an iterable.

    Column presence is checked here; values are validated by
    :func:`build_payload`.
    """
    if isinstance(source, (str, Path)):
        with open(source, newline="", encoding="utf-8-sig") as f:
            # lines starting with '#' are comments (e.g. a provenance header)
            reader = csv.DictReader(line for line in f if not line.startswith("#"))
            missing = [
                c for c in REQUIRED_COLUMNS if c not in (reader.fieldnames or [])
            ]
            if missing:
                raise ValueError(
                    f"{Path(source).name}: missing required column(s) {missing}"
                )
            rows = [dict(r) for r in reader]
    elif hasattr(source, "to_dict") and hasattr(source, "columns"):
        rows = source.to_dict("records")
    else:
        rows = [dict(r) for r in source]
    _check_columns(rows)
    return rows


def _check_columns(rows: Records) -> None:
    if not rows:
        raise ValueError("no records supplied")
    for n, row in enumerate(rows):
        missing = [c for c in REQUIRED_COLUMNS if c not in row]
        if missing:
            raise ValueError(f"record {n}: missing required column(s) {missing}")


def _clean(rows: Records) -> list[dict[str, Any]]:
    _check_columns(rows)
    out, seen = [], set()
    for n, row in enumerate(rows):
        hs = _finite(row["hs_m"], f"record {n} hs_m")
        tp = _finite(row["tp_s"], f"record {n} tp_s")
        if hs <= 0 or tp <= 0:
            raise ValueError(f"record {n}: hs_m and tp_s must be positive")
        check = str(row["check"]).strip()
        if not check:
            raise ValueError(f"record {n}: blank check name")
        u = _finite(row["utilisation"], f"record {n} utilisation")
        if u < 0:
            raise ValueError(f"record {n}: utilisation must not be negative; got {u}")
        key = (hs, tp, check)
        if key in seen:
            raise ValueError(
                f"record {n}: duplicate row for Hs {hs}, Tp {tp}, check {check!r}"
            )
        seen.add(key)
        rec: dict[str, Any] = {"hs_m": hs, "tp_s": tp, "check": check, "utilisation": u}
        for opt in ("demand", "limit"):
            v = row.get(opt)
            if v is not None and not (isinstance(v, str) and not v.strip()):
                if not isinstance(v, str) and isinstance(v, float) and math.isnan(v):
                    continue  # absent value in a DataFrame column
                rec[opt] = _finite(v, f"record {n} {opt}")
        units = row.get("units")
        if isinstance(units, str) and units.strip():
            rec["units"] = units.strip()
        out.append(rec)
    return out


# ---------------------------------------------------------------------------
# Core
# ---------------------------------------------------------------------------


def governing_checks(rows: Records) -> dict[tuple[float, float], tuple[str, float]]:
    """Return ``{(hs, tp): (check, U)}`` for the highest-U check per sea state."""
    gov: dict[tuple[float, float], tuple[str, float]] = {}
    for r in _clean(rows):
        key = (r["hs_m"], r["tp_s"])
        if key not in gov or r["utilisation"] > gov[key][1]:
            gov[key] = (r["check"], r["utilisation"])
    return gov


def build_payload(
    rows: Records,
    *,
    title: str,
    amber_threshold: float = DEFAULT_AMBER_THRESHOLD,
    notes: Sequence[str] = (),
    source_label: str = "",
    synthetic: bool = False,
) -> dict[str, Any]:
    """Validate ``rows`` and return the compact JSON-ready dashboard payload."""
    t = _check_threshold(amber_threshold)
    recs = _clean(rows)

    order: list[str] = []
    meta: dict[str, dict[str, Any]] = {}
    for r in recs:
        if r["check"] not in meta:
            order.append(r["check"])
            meta[r["check"]] = {"id": r["check"]}
        m = meta[r["check"]]
        for opt in ("limit", "units"):
            if opt in r:
                if opt in m and m[opt] != r[opt]:
                    raise ValueError(
                        f"check {r['check']!r}: inconsistent {opt} ({m[opt]!r} vs {r[opt]!r})"
                    )
                m[opt] = r[opt]
    index = {c: k for k, c in enumerate(order)}

    grid: dict[tuple[float, float], dict[str, list]] = {}
    for r in recs:
        cell = grid.setdefault(
            (r["hs_m"], r["tp_s"]),
            {"u": [None] * len(order), "demand": [None] * len(order)},
        )
        k = index[r["check"]]
        cell["u"][k] = r["utilisation"]
        cell["demand"][k] = r.get("demand")

    cells = []
    for (hs, tp), c in sorted(grid.items()):
        gk = max(
            (k for k, u in enumerate(c["u"]) if u is not None),
            key=lambda k: (c["u"][k], -k),
        )
        cell = {
            "hs": hs,
            "tp": tp,
            "u": c["u"],
            "demand_supplied": [d is not None for d in c["demand"]],
            "governing": gk,
            "band": classify_band(c["u"][gk], t),
        }
        if any(d is not None for d in c["demand"]):
            cell["demand"] = c["demand"]
        cells.append(cell)

    return {
        "title": title,
        "amber_threshold": t,
        "checks": [meta[c] for c in order],
        "hs": sorted({c["hs"] for c in cells}),
        "tp": sorted({c["tp"] for c in cells}),
        "cells": cells,
        "notes": [str(n) for n in notes],
        "source": source_label,
        "synthetic": bool(synthetic),
    }


def _template() -> str:
    return (
        resources.files("digitalmodel.reporting")
        .joinpath("assets", _TEMPLATE)
        .read_text(encoding="utf-8")
    )


def _json_for_script(data: Mapping[str, Any]) -> str:
    """JSON safe to embed inside ``<script>``: no ``</``, ``<!--`` or ``&`` sequences."""
    text = json.dumps(data, allow_nan=False, separators=(",", ":"), ensure_ascii=False)
    return text.replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")


def render_dashboard(
    source: Source,
    *,
    title: str,
    amber_threshold: float = DEFAULT_AMBER_THRESHOLD,
    notes: Sequence[str] = (),
    source_label: str = "",
    synthetic: bool = False,
) -> str:
    """Return the dashboard HTML for ``source`` (path, DataFrame or records)."""
    rows = load_records(source)
    if not source_label and isinstance(source, (str, Path)):
        source_label = Path(source).name  # file name only, never a machine path
    payload = build_payload(
        rows,
        title=title,
        amber_threshold=amber_threshold,
        notes=notes,
        source_label=source_label,
        synthetic=synthetic,
    )
    replacements = {
        "__TITLE__": html.escape(title),
        "__ENVELOPE_DATA__": _json_for_script(payload),
    }
    return re.sub(
        r"__TITLE__|__ENVELOPE_DATA__",
        lambda match: replacements[match.group()],
        _template(),
    )


def write_dashboard(
    source: Source,
    output: Union[str, Path],
    *,
    title: str,
    amber_threshold: float = DEFAULT_AMBER_THRESHOLD,
    notes: Sequence[str] = (),
    source_label: str = "",
    synthetic: bool = False,
    overwrite: bool = False,
) -> Path:
    """Write the dashboard to ``output`` and verify it by reading it back."""
    output = Path(output)
    if output.exists() and not overwrite:
        raise FileExistsError(
            f"{output.name} exists; pass overwrite=True to replace it"
        )
    text = render_dashboard(
        source,
        title=title,
        amber_threshold=amber_threshold,
        notes=notes,
        source_label=source_label,
        synthetic=synthetic,
    )
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("w", encoding="utf-8", newline="\n") as f:
        f.write(text)
    if output.read_text(encoding="utf-8") != text:
        raise OSError(f"{output.name}: readback mismatch")
    return output


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n", 1)[0])
    ap.add_argument(
        "csv",
        help="long-format CSV: hs_m, tp_s, check, utilisation[, demand, limit, units]",
    )
    ap.add_argument("out", help="output HTML file")
    ap.add_argument("--title", default="Utilisation envelope")
    ap.add_argument("--amber-threshold", type=float, default=DEFAULT_AMBER_THRESHOLD)
    ap.add_argument(
        "--note", action="append", default=[], help="basis/limitation note (repeatable)"
    )
    ap.add_argument(
        "--synthetic", action="store_true", help="label the page as synthetic data"
    )
    ap.add_argument("--overwrite", action="store_true")
    a = ap.parse_args(argv)
    out = write_dashboard(
        a.csv,
        a.out,
        title=a.title,
        amber_threshold=a.amber_threshold,
        notes=a.note,
        synthetic=a.synthetic,
        overwrite=a.overwrite,
    )
    print(f"wrote {out.name} ({out.stat().st_size:,} bytes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
