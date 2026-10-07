#!/usr/bin/env python3
"""
ABOUTME: Mesh-convergence study helper — runs an evaluator over a sequence of
mesh refinement levels, tracks relative change of a key result, and reports
convergence against a tolerance and optional analytical reference.
"""

from dataclasses import dataclass, field
from typing import Any, Callable, Optional, Sequence


@dataclass
class ConvergenceLevel:
    label: str
    value: float
    n_nodes: int = 0
    n_elements: int = 0
    rel_change: Optional[float] = None
    error_vs_reference: Optional[float] = None
    extra: dict = field(default_factory=dict)


class MeshConvergenceStudy:
    """
    Drive a mesh-convergence sweep.

    ``evaluate(level)`` must return a dict with at least ``"value"`` and may
    include ``"n_nodes"``, ``"n_elements"`` and any other keys (kept in
    ``extra``). ``rel_change`` is ``|v_i - v_{i-1}| / |v_i|``.
    """

    def __init__(self, tolerance: float = 0.01, reference: Optional[float] = None):
        if tolerance <= 0:
            raise ValueError("tolerance must be positive")
        self.tolerance = tolerance
        self.reference = reference
        self.levels: list[ConvergenceLevel] = []

    def run(
        self,
        levels: Sequence[Any],
        evaluate: Callable[[Any], dict],
        label: Optional[Callable[[Any], str]] = None,
    ) -> list[ConvergenceLevel]:
        self.levels = []
        prev: Optional[float] = None
        for lvl in levels:
            res = evaluate(lvl)
            value = float(res["value"])
            rel = None
            if prev is not None:
                denom = abs(value) if value != 0 else 1.0
                rel = abs(value - prev) / denom
            err = None
            if self.reference not in (None, 0):
                err = abs(value - self.reference) / abs(self.reference)
            extra = {
                k: v for k, v in res.items()
                if k not in ("value", "n_nodes", "n_elements")
            }
            self.levels.append(
                ConvergenceLevel(
                    label=label(lvl) if label else str(lvl),
                    value=value,
                    n_nodes=int(res.get("n_nodes", 0)),
                    n_elements=int(res.get("n_elements", 0)),
                    rel_change=rel,
                    error_vs_reference=err,
                    extra=extra,
                )
            )
            prev = value
        return self.levels

    @property
    def converged(self) -> bool:
        if len(self.levels) < 2:
            return False
        last = self.levels[-1].rel_change
        return last is not None and last <= self.tolerance

    @property
    def converged_at(self) -> Optional[str]:
        """Label of the first level whose change from the previous is within tol."""
        for lvl in self.levels[1:]:
            if lvl.rel_change is not None and lvl.rel_change <= self.tolerance:
                return lvl.label
        return None

    def is_monotone(self) -> bool:
        """True if successive relative changes never increase."""
        changes = [l.rel_change for l in self.levels if l.rel_change is not None]
        return all(b <= a for a, b in zip(changes, changes[1:]))

    def summary_table(self, value_name: str = "value", fmt: str = ".6E") -> str:
        """Markdown table of the sweep."""
        hdr = f"| level | nodes | elements | {value_name} | rel. change | error vs ref |"
        sep = "|---|---:|---:|---:|---:|---:|"
        rows = [hdr, sep]
        for l in self.levels:
            rc = "-" if l.rel_change is None else f"{l.rel_change * 100:.3f}%"
            er = (
                "-" if l.error_vs_reference is None
                else f"{l.error_vs_reference * 100:.3f}%"
            )
            rows.append(
                f"| {l.label} | {l.n_nodes} | {l.n_elements} | "
                f"{format(l.value, fmt)} | {rc} | {er} |"
            )
        return "\n".join(rows)

    def to_records(self) -> list[dict]:
        return [
            {
                "label": l.label,
                "n_nodes": l.n_nodes,
                "n_elements": l.n_elements,
                "value": l.value,
                "rel_change": l.rel_change,
                "error_vs_reference": l.error_vs_reference,
                **l.extra,
            }
            for l in self.levels
        ]


def richardson_extrapolation(
    coarse: float, fine: float, refinement_ratio: float = 2.0, order: float = 2.0
) -> float:
    """
    Richardson extrapolation of the mesh-independent value from two levels
    with uniform refinement ratio ``r`` and assumed convergence order ``p``:
    ``f_inf = f_fine + (f_fine - f_coarse) / (r**p - 1)``.
    """
    if refinement_ratio <= 1:
        raise ValueError("refinement_ratio must exceed 1")
    if order <= 0:
        raise ValueError("order must be positive")
    return fine + (fine - coarse) / (refinement_ratio ** order - 1.0)
