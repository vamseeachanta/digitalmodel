"""Shared diffraction validation helper (#611 / #625 Phase-1).

This module provides a single entry point, :func:`run_validation`, used by both
the OrcaWave and AQWA runners (and, in a later increment, their batch runners)
so that runner-level and batch-level validation never diverge.

Responsibilities
----------------
- Accept an optional :class:`DiffractionResults` plus an output directory and an
  input/report *stem*.
- Invoke :func:`validate_results` (which wraps :class:`OutputValidator`),
  writing the JSON report to ``<output_dir>/<stem>_validation.json``.
- Map the validator's internal verdict to a *run-contract* verdict.
- Flatten the nested issue report into a flat ``list[str]`` for convenient
  display / storage on the result objects.

Verdict semantics
-----------------
The validator (:meth:`OutputValidator._determine_overall_status`) only ever
returns the canonical strings ``PASS``, ``WARNING``, or ``FAIL``.  Those are
preserved *exactly* — this helper never normalizes ``WARNING`` to ``WARN`` (the
``WARN`` alias, per locked decision D3, is a CLI display concern only).

This helper adds two run-contract states the validator itself cannot express:

- ``ERROR``   — validation was attempted but raised an exception.
- ``SKIPPED`` — validation was disabled, or no :class:`DiffractionResults`
  were available (dry-run, no-executable fallback, failed solver, no output).

These five strings are the only values :class:`ValidationOutcome.verdict` may
take.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)
from digitalmodel.hydrodynamics.diffraction.output_validator import (
    validate_results,
)

# Canonical verdicts the underlying validator can produce. Must never be
# normalized (e.g. WARNING -> WARN).
VERDICT_PASS = "PASS"
VERDICT_WARNING = "WARNING"
VERDICT_FAIL = "FAIL"

# Run-contract verdicts layered on top of the validator output.
VERDICT_ERROR = "ERROR"
VERDICT_SKIPPED = "SKIPPED"

#: Every verdict string the helper may emit.
ALL_VERDICTS = frozenset(
    {
        VERDICT_PASS,
        VERDICT_WARNING,
        VERDICT_FAIL,
        VERDICT_ERROR,
        VERDICT_SKIPPED,
    }
)


@dataclass
class ValidationOutcome:
    """Result of a (possibly skipped) validation pass.

    Attributes:
        verdict: One of ``PASS``/``WARNING``/``FAIL``/``ERROR``/``SKIPPED``.
        report: The full validation report dict, or ``None`` when skipped /
            errored before a report could be produced.
        report_path: Path to the written JSON report, or ``None`` if no report
            was written.
        issues: Flattened list of human-readable issue strings (empty when the
            verdict is ``PASS``/``SKIPPED``).
        reason: Optional human-readable explanation, primarily for ``SKIPPED``
            (e.g. ``"No diffraction results produced in dry-run mode"``) and
            ``ERROR`` (the exception text).
    """

    verdict: str = VERDICT_SKIPPED
    report: dict[str, Any] | None = None
    report_path: Path | None = None
    issues: list[str] = field(default_factory=list)
    reason: str | None = None


def flatten_issues(report: dict[str, Any]) -> list[str]:
    """Flatten the nested validator report into a flat list of issue strings.

    The validator report is a dict whose values are either:
      - a scalar (e.g. ``vessel_name``, ``overall_status``) — ignored,
      - a ``list[str]`` of issues, or
      - a ``dict`` whose values are ``list[str]`` of issues (the common shape).

    Each emitted string is prefixed with its category path (e.g.
    ``"physical_validity.added_mass: Negative diagonal term ..."``) so the
    origin of an issue is preserved when stored flat.
    """
    issues: list[str] = []
    for category, value in report.items():
        if isinstance(value, list):
            for item in value:
                issues.append(f"{category}: {item}")
        elif isinstance(value, dict):
            for subcategory, sub in value.items():
                if isinstance(sub, list):
                    for item in sub:
                        issues.append(f"{category}.{subcategory}: {item}")
    return issues


def run_validation(
    results: DiffractionResults | None,
    output_dir: Path | str | None,
    stem: str,
    *,
    enabled: bool = True,
    skip_reason: str | None = None,
) -> ValidationOutcome:
    """Validate diffraction results, returning a run-contract outcome.

    Args:
        results: The results to validate. ``None`` (or ``enabled=False``)
            yields a ``SKIPPED`` outcome.
        output_dir: Directory the JSON report is written into. If ``None`` no
            report file is written (the in-memory report dict is still
            returned). Created if it does not exist.
        stem: Filename stem for the report, written as
            ``<stem>_validation.json``.
        enabled: When ``False`` validation is skipped regardless of *results*.
        skip_reason: Explanation attached to a ``SKIPPED`` outcome. Defaults to
            a sensible message based on why the skip occurred.

    Returns:
        A :class:`ValidationOutcome`. The verdict is one of the five canonical
        strings; ``WARNING`` is never normalized to ``WARN``.
    """
    if not enabled:
        return ValidationOutcome(
            verdict=VERDICT_SKIPPED,
            reason=skip_reason or "Validation disabled",
        )

    if results is None:
        return ValidationOutcome(
            verdict=VERDICT_SKIPPED,
            reason=skip_reason or "No diffraction results available",
        )

    report_path: Path | None = None
    if output_dir is not None:
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        report_path = output_dir / f"{stem}_validation.json"

    try:
        report = validate_results(results, output_file=report_path)
    except Exception as exc:  # noqa: BLE001 - any validator failure is ERROR
        return ValidationOutcome(
            verdict=VERDICT_ERROR,
            report_path=report_path if (report_path and report_path.exists()) else None,
            reason=str(exc),
        )

    verdict = report.get("overall_status", VERDICT_ERROR)
    # Preserve canonical strings exactly; never alias WARNING -> WARN.
    issues = flatten_issues(report)

    return ValidationOutcome(
        verdict=verdict,
        report=report,
        report_path=report_path,
        issues=issues,
    )
