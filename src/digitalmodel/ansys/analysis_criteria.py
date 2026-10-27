"""Sourced offline acceptance checks; no solver execution or default design limits.

Contract: ENG-ANALYSIS-CRITERIA Revision 1 (2026), AC-01 and AC-07–08,
``docs/standards/analysis-parametric-lookup-criteria.html``. Execution,
convergence and strength are distinct claims. Source applicability and checker
competence remain responsibilities of the externally supplied verification register.
"""
from __future__ import annotations

from copy import deepcopy
from decimal import Decimal
from pathlib import Path

from digitalmodel.ansys.analysis_records import (
    decimal_text, nonempty, require_fields, verify_reference,
)

SUPPORTED_CRITERIA = frozenset({"equilibrium", "convergence", "strength"})
CRITERION_FIELDS = frozenset({
    "criterion_id", "quantity", "unit", "observed", "limit", "comparator", "source", "evidence",
})
SOURCE_FIELDS = frozenset({"document", "edition", "clause"})
SOURCE_PLACEHOLDERS = frozenset({
    "tbd", "n/a", "na", "-", "unknown", "x", "none", "not applicable",
    "not available", "unspecified", "unverified", "?",
})


def _validate_source(source: dict) -> None:
    """Require a structured citation without asserting source applicability."""
    require_fields(source, SOURCE_FIELDS, "acceptance source")
    if set(source) != SOURCE_FIELDS:
        raise ValueError("unknown acceptance source fields")
    for field in SOURCE_FIELDS:
        value = nonempty(source[field], "acceptance source " + field)
        if value.strip().casefold() in SOURCE_PLACEHOLDERS:
            raise ValueError("placeholder acceptance source " + field)


def evaluate_criteria(criteria: dict, resolver: dict[str, Path]) -> dict[str, str]:
    """Evaluate explicit decimal comparisons; missing checks remain NOT_EVALUATED.

    Each observed quantity and its limit share the declared unit; no conversions,
    interpolation, capacity derivation or engineering qualification occur here.
    All thresholds and their applicability evidence are required caller inputs.
    Invalid supplied records and inaccessible/changed evidence raise ValueError.
    """
    if not isinstance(criteria, dict) or set(criteria) - SUPPORTED_CRITERIA:
        raise ValueError("invalid acceptance criteria matrix")
    results = {}
    for name in sorted(SUPPORTED_CRITERIA):
        if name not in criteria:
            results[name] = "NOT_EVALUATED"
            continue
        item = criteria[name]
        require_fields(item, CRITERION_FIELDS, "acceptance criterion")
        if set(item) != CRITERION_FIELDS:
            raise ValueError("unknown acceptance criterion fields")
        for field in ("criterion_id", "quantity", "unit"):
            nonempty(item[field], field)
        _validate_source(item["source"])
        if item["comparator"] not in ("le", "ge", "abs_le"):
            raise ValueError("unsupported acceptance comparator")
        if not isinstance(item["evidence"], list) or not item["evidence"]:
            raise ValueError("acceptance criterion evidence required")
        for reference in item["evidence"]:
            verify_reference(reference, resolver)
        values = [None if item[key] is None else Decimal(decimal_text(item[key]))
                  for key in ("observed", "limit")]
        observed, limit = values
        if item["comparator"] == "abs_le" and limit is not None and limit < 0:
            raise ValueError("absolute acceptance criterion limit is negative")
        if observed is None or limit is None:
            results[name] = "NOT_EVALUATED"
        else:
            # Direct Decimal ordering is exact and ignores ambient precision.
            if item["comparator"] == "abs_le":
                observed = observed.copy_abs()
            passed = observed >= limit if item["comparator"] == "ge" else observed <= limit
            results[name] = "PASS" if passed else "FAIL"
    return results


def qualify_response_criteria(entry: dict, response: dict, intended_use: str,
                              resolver: dict[str, Path]) -> dict[str, dict]:
    """Enforce frozen intended-use requirements without implying capacity readiness."""
    if not isinstance(response, dict) or response.get("value") is None:
        raise ValueError("response value required for acceptance criteria")
    response_value = Decimal(decimal_text(response["value"]))
    requirements = entry.get("acceptance_requirements")
    if not isinstance(requirements, dict):
        raise ValueError("acceptance criteria requirements required")
    required = requirements.get(intended_use)
    if (not isinstance(required, list) or not required
            or any(not isinstance(name, str) for name in required)
            or len(set(required)) != len(required)
            or set(required) - SUPPORTED_CRITERIA
            or not {"equilibrium", "convergence"}.issubset(required)):
        raise ValueError("invalid intended-use acceptance criteria requirements")
    criteria = entry.get("acceptance_criteria", {})
    results = evaluate_criteria(criteria, resolver)
    if any(results[name] != "PASS" for name in required) or "FAIL" in results.values():
        raise ValueError("acceptance criteria incomplete or failed: " + str(results))
    if "strength" in criteria:
        strength = criteria["strength"]
        if (strength["quantity"] != response.get("definition")
                or strength["unit"] != response.get("unit")
                or (strength["observed"] is not None
                    and Decimal(decimal_text(strength["observed"])) != response_value)):
            raise ValueError("strength acceptance criteria incompatible with response")
    return {
        name: {"status": status, **{
            field: deepcopy(criteria.get(name, {}).get(field))
            for field in sorted(CRITERION_FIELDS)
        }}
        for name, status in results.items()
    }
