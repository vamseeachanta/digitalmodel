"""Qualification-gate evaluation: demand against reference at a fixed tolerance."""

from __future__ import annotations

import math
from typing import Any


def _usable(x: Any) -> bool:
    return isinstance(x, (int, float)) and not isinstance(x, bool) and math.isfinite(x)


def evaluate_gate(gate_id: str, *, demand: float | None, reference: float | None, tolerance: float,
                  unit: str = "", note: str = "") -> dict[str, Any]:
    """deviation = (demand - reference) / |reference|; PASS when |deviation| <= tolerance.

    A missing, non-finite or zero reference gives NOT_EVALUATED, never PASS.
    """
    row: dict[str, Any] = {"gate": gate_id, "demand": demand, "reference": reference,
                           "tolerance": tolerance, "unit": unit, "deviation": None,
                           "status": "NOT_EVALUATED", "note": note}
    if not (_usable(demand) and _usable(reference)) or reference == 0:
        return row
    dev = (demand - reference) / abs(reference)
    row["deviation"] = dev
    row["status"] = "PASS" if abs(dev) <= tolerance + 1e-12 else "FAIL"
    return row
