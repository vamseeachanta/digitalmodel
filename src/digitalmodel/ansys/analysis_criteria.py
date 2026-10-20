"""Sourced offline acceptance checks; no solver execution or default design limits.

Contract: ENG-ANALYSIS-CRITERIA Revision 1 (2026), AC-01 and AC-07–08,
``docs/standards/analysis-parametric-lookup-criteria.html``. Execution,
convergence and strength are distinct claims. Source applicability and checker
competence remain responsibilities of the externally supplied verification register.
"""
from __future__ import annotations

from decimal import Decimal
from pathlib import Path

from digitalmodel.ansys.analysis_records import (
    decimal_text, nonempty, require_fields, verify_reference,
)

REQUIRED_CRITERIA = frozenset({"equilibrium", "convergence", "strength"})


def evaluate_criteria(criteria: dict, resolver: dict[str, Path]) -> dict[str, str]:
    """Evaluate explicit decimal comparisons; missing checks remain NOT_EVALUATED.

    Each observed quantity and its limit share the declared unit; no conversions,
    interpolation, capacity derivation or engineering qualification occur here.
    All thresholds and their applicability evidence are required caller inputs.
    Invalid supplied records and inaccessible/changed evidence raise ValueError.
    """
    if not isinstance(criteria, dict) or set(criteria) - REQUIRED_CRITERIA:
        raise ValueError("invalid acceptance criteria matrix")
    results = {}
    for name in sorted(REQUIRED_CRITERIA):
        if name not in criteria:
            results[name] = "NOT_EVALUATED"
            continue
        item = criteria[name]
        require_fields(item, {"criterion_id", "quantity", "unit", "observed", "limit",
                              "comparator", "source", "evidence"}, "acceptance criterion")
        for field in ("criterion_id", "quantity", "unit", "source"):
            nonempty(item[field], field)
        if item["comparator"] not in ("le", "ge"):
            raise ValueError("unsupported acceptance comparator")
        if not isinstance(item["evidence"], list) or not item["evidence"]:
            raise ValueError("acceptance criterion evidence required")
        for reference in item["evidence"]:
            verify_reference(reference, resolver)
        values = [None if item[key] is None else Decimal(decimal_text(item[key]))
                  for key in ("observed", "limit")]
        observed, limit = values
        if observed is None or limit is None:
            results[name] = "NOT_EVALUATED"
        else:
            # Direct Decimal ordering is exact and ignores ambient precision.
            passed = observed <= limit if item["comparator"] == "le" else observed >= limit
            results[name] = "PASS" if passed else "FAIL"
    return results


def qualify_response_criteria(entry: dict, response: dict, intended_use: str,
                              resolver: dict[str, Path]) -> dict[str, str]:
    """Enforce frozen intended-use requirements without implying capacity readiness."""
    requirements = entry.get("acceptance_requirements")
    if not isinstance(requirements, dict):
        raise ValueError("acceptance criteria requirements required")
    required = requirements.get(intended_use)
    if (not isinstance(required, list) or not required
            or any(not isinstance(name, str) for name in required)
            or len(set(required)) != len(required)
            or set(required) - REQUIRED_CRITERIA
            or not {"equilibrium", "convergence"}.issubset(required)):
        raise ValueError("invalid intended-use acceptance criteria requirements")
    criteria = entry.get("acceptance_criteria", {})
    results = evaluate_criteria(criteria, resolver)
    if any(results[name] != "PASS" for name in required):
        raise ValueError("acceptance criteria incomplete or failed: " + str(results))
    if "strength" in criteria:
        strength = criteria["strength"]
        if (strength["quantity"] != response["definition"]
                or strength["unit"] != response["unit"]
                or (strength["observed"] is not None
                    and Decimal(decimal_text(strength["observed"])) != Decimal(response["value"]))):
            raise ValueError("strength acceptance criteria incompatible with response")
    return results
