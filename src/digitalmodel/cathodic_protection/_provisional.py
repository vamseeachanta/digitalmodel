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
from typing import Mapping, Optional

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
    range_low, range_high : float, optional
        Both ends of the range the source gives, when it gives a range
        (owner decision 2026-09-27: store both ends).
    conservative_end : {"low", "high"}, optional
        Which end ``value`` is: the design uses the conservative end (the
        one that shortens life or lowers capacity). Required with a range.
    """

    value: float
    units: str
    source: str
    note: str = ""
    provisional: bool = True
    pending_standard: str = ""
    range_low: Optional[float] = None
    range_high: Optional[float] = None
    conservative_end: Optional[str] = None

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
        self._check_range()

    def _check_range(self) -> None:
        ends = (self.range_low, self.range_high)
        if all(e is None for e in ends):
            if self.conservative_end is not None:
                raise ProvisionalValueError("conservative_end given without range_low/range_high")
            return
        if self.range_low is None or self.range_high is None:
            raise ProvisionalValueError("give both range_low and range_high, or neither")
        if not self.range_low <= self.range_high:
            raise ProvisionalValueError(
                f"range_low {self.range_low!r} exceeds range_high {self.range_high!r}"
            )
        if self.conservative_end not in ("low", "high"):
            raise ProvisionalValueError(
                "conservative_end must be 'low' or 'high' when a range is given"
            )
        chosen = self.range_low if self.conservative_end == "low" else self.range_high
        if not math.isclose(float(self.value), chosen, rel_tol=1e-12, abs_tol=0.0):
            raise ProvisionalValueError(
                f"value {self.value!r} is not the conservative ({self.conservative_end}) "
                f"end {chosen!r} of the range"
            )

    @property
    def range_text(self) -> str:
        """``"low-high (conservative: end)"`` or ``""`` when no range is stored."""
        if self.range_low is None or self.range_high is None:
            return ""
        return f"{self.range_low:g}-{self.range_high:g} (conservative: {self.conservative_end})"

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
    if pv.range_text:
        parts.append(f"range: {pv.range_text}")
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
