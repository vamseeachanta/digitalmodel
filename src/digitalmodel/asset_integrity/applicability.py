# ABOUTME: Shared applicability/validity layer for the FFS strength methods —
# ABOUTME: one Applicability(ok, flags, notes) record raised instead of silent extrapolation.
"""Applicability flags for the FFS remaining-strength methods (issue #1094).

Every assessment method here (ASME B31G family, DNV-RP-F101, API 579 Level 2)
is calibrated over a bounded range — relative depth, sizing accuracy, flaw
length parameter.  Outside that range the closed forms still return a number,
and the 2026-06-28 adversarial review found that number was returned silently.

This module gives every method the same small record to say so:

    Applicability(ok=False,
                  flags=["B31G_DT_GT_0.80"],
                  notes=["d/t=0.900 exceeds the ASME B31G validity limit 0.80 ..."])

The number is always still computed and returned — the flag travels next to it.
Downstream, :func:`~digitalmodel.asset_integrity.assessment.ffs_decision.FFSDecision.decide`
turns any raised flag into an ``ESCALATE`` verdict instead of a numeric one and
the HTML report lists the flags in its footer.

``flags`` are short stable machine codes (safe to match on); ``notes`` are the
human-readable explanation, one per flag, in the same order.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, Mapping, Optional

# ---------------------------------------------------------------------------
# Flag codes.  Stable identifiers; the numeric limit is part of the code so a
# future limit change is visible to any consumer that matches on it.
# ---------------------------------------------------------------------------
B31G_RELATIVE_DEPTH_FLAG = "B31G_DT_GT_0.80"
"""ASME B31G / Modified B31G / RSTRENG: relative depth d/t exceeds 0.80."""

DNV_F101_RELATIVE_DEPTH_FLAG = "DNV_F101_DT_GT_0.85"
"""DNV-RP-F101 single-defect capacity: relative depth d/t exceeds 0.85."""

DNV_F101_SIZING_STD_FLAG = "DNV_F101_STD_GT_0.16"
"""DNV-RP-F101 Part-A PSF: sizing StD[d/t] beyond the 0.16 calibration range."""

API579_FOLIAS_LAMBDA_FLAG = "API579_FOLIAS_LAMBDA_GT_20"
"""API 579-1 Table 4.4 Folias factor: shell parameter lambda beyond 20 (frozen)."""


@dataclass
class Applicability:
    """Whether a result lies inside its method's qualified range.

    Attributes:
        ok: ``True`` when no applicability limit was exceeded.
        flags: stable machine codes for every limit exceeded (empty when ok).
        notes: one human-readable note per flag, same order.
    """

    ok: bool = True
    flags: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)

    def to_dict(self) -> dict:
        """JSON-friendly form."""
        return {
            "ok": bool(self.ok),
            "flags": list(self.flags),
            "notes": list(self.notes),
        }

    @property
    def note(self) -> Optional[str]:
        """All notes joined with ``"; "``, or ``None`` when nothing is flagged.

        This is the legacy ``details["applicability_note"]`` value.
        """
        return "; ".join(self.notes) if self.notes else None

    def legacy_details(self) -> dict:
        """The pre-#1094 ``details`` keys, kept so existing consumers still work."""
        return {"within_applicability": bool(self.ok), "applicability_note": self.note}


def flagged(flag: str, note: str) -> Applicability:
    """A single raised flag."""
    return Applicability(ok=False, flags=[flag], notes=[note])


def check_upper_limit(
    value: float,
    limit: float,
    *,
    flag: str,
    quantity: str,
    method: str,
    advice: str = "assess by repair/replace criteria",
) -> Applicability:
    """Flag ``value > limit`` (values equal to the limit are inside the range).

    Args:
        value: the quantity checked (e.g. ``d/t``).
        limit: the method's calibrated upper bound.
        flag: stable code to raise, one of the module constants.
        quantity: symbol for the note, e.g. ``"d/t"``.
        method: method / standard name for the note.
        advice: what to do instead of trusting the extrapolated number.
    """
    if value <= limit:
        return Applicability()
    return flagged(
        flag,
        f"{quantity}={value:.3f} exceeds the {method} validity limit "
        f"{limit:.2f}; {advice}",
    )


def merge(*items: Optional[Applicability]) -> Applicability:
    """Combine several records; ``None`` entries are skipped.

    The result is ok only when every input is ok; flags and notes keep their
    input order.
    """
    flags: list[str] = []
    notes: list[str] = []
    for item in items:
        if item is None:
            continue
        flags.extend(item.flags)
        notes.extend(item.notes)
    return Applicability(ok=not flags, flags=flags, notes=notes)


def from_details(details: Mapping, *, flag: str) -> Applicability:
    """Rebuild a record from the legacy ``within_applicability`` /
    ``applicability_note`` detail keys (missing keys mean "within range")."""
    if details.get("within_applicability", True):
        return Applicability()
    note = details.get("applicability_note") or f"{flag} raised"
    return flagged(flag, str(note))


def collect(results: Iterable) -> Applicability:
    """Merge the ``applicability`` attribute of every object that has one."""
    return merge(*(getattr(r, "applicability", None) for r in results))


__all__ = [
    "API579_FOLIAS_LAMBDA_FLAG",
    "Applicability",
    "B31G_RELATIVE_DEPTH_FLAG",
    "DNV_F101_RELATIVE_DEPTH_FLAG",
    "DNV_F101_SIZING_STD_FLAG",
    "check_upper_limit",
    "collect",
    "flagged",
    "from_details",
    "merge",
]
