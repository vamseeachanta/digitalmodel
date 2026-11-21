#!/usr/bin/env python3
"""Compare the evaluated cells of two fender design workbooks.

Both workbooks are recalculated with the ``formulas`` package; every cell of
the first (``before``) is compared with the same cell of the second
(``after``). Numbers must agree to ``--rel`` (default exact), text and errors
exactly. Sheets named with ``--skip-sheet`` (default README) are left out.
Cells only in ``after`` are counted as added. Exit status 1 when a cell of
``before`` changed or disappeared.

Usage::

    python compare_workbook_cells.py before.xlsx after.xlsx [--skip-sheet README] [--rel 0]
"""

from __future__ import annotations

import argparse
import math
import sys
import warnings
from collections import Counter
from pathlib import Path


def evaluate(path: Path) -> dict[str, object]:
    """Every evaluated cell (``SHEET!A1`` -> value; errors as their text)."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        import formulas
        sol = formulas.ExcelModel().loads(str(path)).finish().calculate()
    out = {}
    for k, v in sol.items():
        key = str(k).upper().replace("'", "").split("]")[-1]
        if ":" in key:          # ranges duplicate their cells
            continue
        try:
            x = v.value[0][0]
        except (TypeError, IndexError, AttributeError):
            continue
        if hasattr(x, "item"):
            x = x.item()
        out[key] = x if isinstance(x, (int, float, str, bool)) or x is None else str(x)
    return out


def same(a, b, rel: float) -> bool:
    if isinstance(a, bool) or isinstance(b, bool):
        return a == b
    if isinstance(a, (int, float)) and isinstance(b, (int, float)):
        if math.isnan(a) or math.isnan(b):
            return math.isnan(a) and math.isnan(b)
        return a == b if rel == 0 else math.isclose(a, b, rel_tol=rel, abs_tol=0.0)
    return a == b


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("before", type=Path)
    ap.add_argument("after", type=Path)
    ap.add_argument("--skip-sheet", action="append", default=None)
    ap.add_argument("--rel", type=float, default=0.0)
    a = ap.parse_args(argv)
    skip = {s.upper() for s in (a.skip_sheet or ["README"])}
    before, after = evaluate(a.before), evaluate(a.after)
    keep = lambda k: k.split("!")[0] not in skip  # noqa: E731
    changed = [k for k in sorted(before) if keep(k) and k in after and not same(before[k], after[k], a.rel)]
    missing = [k for k in sorted(before) if keep(k) and k not in after]
    added = [k for k in sorted(after) if keep(k) and k not in before]
    n = sum(1 for k in before if keep(k))
    numbers = sum(1 for k in before if keep(k) and isinstance(before[k], (int, float))
                  and not isinstance(before[k], bool))
    print(f"compared {n} evaluated cells of {a.before.name} ({numbers} numbers), skipping {sorted(skip)}")
    print(f"unchanged {n - len(changed) - len(missing)}; changed {len(changed)}; missing {len(missing)}; "
          f"added {len(added)}")
    if added:
        by = Counter(k.split("!")[0] or "(defined names)" for k in added)
        print("added by sheet: " + ", ".join(f"{s} {c}" for s, c in sorted(by.items())))
    kinds = Counter("text" if isinstance(before[k], str) else "number" for k in changed)
    if changed:
        print("changed by kind: " + ", ".join(f"{k} {c}" for k, c in sorted(kinds.items())))
    for k in changed[:50]:
        print(f"CHANGED {k}: {before[k]!r} -> {after[k]!r}")
    for k in missing[:50]:
        print(f"MISSING {k}: {before[k]!r}")
    return 1 if changed or missing else 0


if __name__ == "__main__":
    sys.exit(main())
