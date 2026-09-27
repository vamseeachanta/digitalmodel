"""Provisional constants: values taken from open literature, not a standard.

Issue #2247 (owner direction 2026-09-27): where the governing standard is
not on file, a model may still carry a default threshold or material
constant, but only as a :class:`ProvisionalValue` that names

* the open-literature ``source`` the number was read from (author, title,
  year, DOI or URL), and
* the ``pending_standard`` clause that must confirm or replace it once the
  standard is obtained.

A provisional value is never presented as coming from a standard. Models
that consume provisional values stay behind ``experimental=True``
(:mod:`digitalmodel.cathodic_protection._experimental`).

This module is self-contained (standard library only) so other lanes can
import it without pulling in any model code.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Mapping

__all__ = [
    "ProvisionalValue",
    "ProvisionalValueError",
    "render_provisional",
    "render_provisional_table",
]


class ProvisionalValueError(ValueError):
    """Raised when a :class:`ProvisionalValue` is structurally invalid."""


@dataclass(frozen=True)
class ProvisionalValue:
    """A constant read from open literature, pending confirmation by a standard.

    Attributes
    ----------
    value : float
        The number, in ``units``.
    units : str
        Units of ``value`` (use ``"dimensionless"`` for ratios).
    source : str
        Literature source: author(s), title, year, and DOI or URL. Must be
        non-empty.
    note : str
        How the number was read from the source (table, slide, equation,
        any interpretation made).
    provisional : bool
        ``True`` until the value is confirmed against ``pending_standard``.
    pending_standard : str
        The standard clause that must confirm the value. Required while
        ``provisional`` is true.
    """

    value: float
    units: str
    source: str
    note: str = ""
    provisional: bool = True
    pending_standard: str = ""

    def __post_init__(self) -> None:
        if isinstance(self.value, bool) or not isinstance(self.value, (int, float)):
            raise ProvisionalValueError(f"value must be a real number, got {self.value!r}")
        if not math.isfinite(float(self.value)):
            raise ProvisionalValueError(f"value must be finite, got {self.value!r}")
        for name in ("units", "source"):
            text = getattr(self, name)
            if not isinstance(text, str) or not text.strip():
                raise ProvisionalValueError(f"ProvisionalValue.{name} must be a non-empty string")
        if self.provisional and not self.pending_standard.strip():
            raise ProvisionalValueError(
                "a provisional value must name the pending_standard clause that will confirm it"
            )

    def __float__(self) -> float:
        return float(self.value)


def render_provisional(pv: ProvisionalValue, name: str | None = None) -> str:
    """Render one provisional value as a single human-readable line.

    Example: ``"i_ac limit = 30 A/m2 [PROVISIONAL; source: ...; confirm
    against: ISO 18086:2019 criteria clause]"``.
    """
    head = f"{name} = " if name else ""
    status = "PROVISIONAL" if pv.provisional else "confirmed"
    parts = [f"{head}{pv.value:g} {pv.units} [{status}; source: {pv.source}"]
    if pv.note:
        parts.append(f"note: {pv.note}")
    if pv.pending_standard:
        parts.append(f"confirm against: {pv.pending_standard}")
    return "; ".join(parts) + "]"


def render_provisional_table(values: Mapping[str, ProvisionalValue]) -> str:
    """Render a mapping of provisional values as a Markdown table."""
    lines = [
        "| Name | Value | Units | Source | Confirm against |",
        "|------|-------|-------|--------|-----------------|",
    ]
    for key, pv in values.items():
        lines.append(
            f"| `{key}` | {pv.value:g} | {pv.units} | {pv.source} | {pv.pending_standard or '-'} |"
        )
    return "\n".join(lines)
