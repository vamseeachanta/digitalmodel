# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Emit an offline S12 applicability checklist as JSON on standard output.

Run with ``python scripts/reporting/build_cp_s12_qualification.py``. Successful
execution means that a checklist was produced, not that a design passed.
All checks remain NOT_EVALUATED; this tool cannot select engineering treatments.
It imports no CP engine and does not register a case or admit an edition.

Source: DNV RP B401, Cathodic Protection Design (1993), clauses 6.3.16-6.3.17
(printed p. 16) and 6.7.2-6.7.7 (printed p. 23). Exact source identity and
page locators come from docs/domains/cathodic_protection/benchmarks/S12.json.
No numerical standard values or engineering equations are transcribed here.
"""

from __future__ import annotations

from collections.abc import Mapping
from copy import deepcopy
import json
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
SPEC = ROOT / "docs/domains/cathodic_protection/benchmarks/S12.json"
# Source identity pinned by S12.json; the public source bytes matched this
# digest during source retrieval. Offline reporting does not recheck those bytes.
SOURCE_SHA256 = "6e3b0008e41dc030f1a353d3d8d3c18bbb894712ed6992442809e762d2e9f13a"

# Each record describes an unresolved applicability decision, not an engineering
# acceptance criterion. Citations resolve against the existing S12 specification.
CHECKS = (
    {
        "check_id": "tsa_thickness_basis",
        "references": ("tsa",),
        "reason": "The declared TSA thickness and its applicability are unset.",
        "required_evidence": [
            "Independently declared nominal sprayed-aluminium thickness and unit.",
            "Qualification against the nominal-thickness basis in clause 6.3.16.",
            "Interface-temperature evidence and applicability of clause 6.3.17.",
        ],
        "next_action": "Record the thickness and temperature basis before selecting mean demand.",
    },
    {
        "check_id": "tsa_initial_final_treatment",
        "references": ("tsa", "mass_output"),
        "reason": (
            "Clause 6.3.16 excludes the bare-steel initial/final density basis; "
            "an installed-arrangement output treatment has not been qualified."
        ),
        "required_evidence": [
            "An explicit TSA initial/final applicability decision under clause 6.3.16.",
            "Its effect on the installed-arrangement assessment under clauses 7.7-7.8.",
        ],
        "next_action": (
            "Qualify the TSA treatment separately; neither zero demand nor "
            "later-edition densities may fill this gap."
        ),
    },
    {
        "check_id": "bracelet_depletion_applicability",
        "references": ("resistance", "depletion", "utilisation"),
        "reason": (
            "Bracelet subtype, mounting and depleted geometry are unset; "
            "clause 6.7.6 versus 6.7.7 has not been decided."
        ),
        "required_evidence": [
            "Subtype and mounting, including coating, exposed ends and gap treatment.",
            "Evidence for or against the pipeline half-shell conditions of clause 6.7.6.",
            "Applicable resistance shape in Table 6.7.1 and clause 6.7.7 applicability.",
            "Consistent fresh/final geometry, net mass, inserts and utilisation basis.",
        ],
        "next_action": (
            "Qualify the depletion rule before computing final exposed area; "
            "the word bracelet alone does not establish pipeline applicability."
        ),
    },
)


def build_report(spec: Mapping[str, Any]) -> dict[str, Any]:
    """Return unresolved, cited decisions from the planning-only specification.

    Reject numerical or differently sourced specifications. Supplied approval
    claims cannot upgrade these checks; qualification requires a later change.
    """
    expected = {
        "case_id": "S12",
        "status": "specification_only",
        "executable": False,
        "numerical_inputs": None,
        "expected_results": None,
    }
    for key, value in expected.items():
        if key not in spec or type(spec[key]) is not type(value) or spec[key] != value:
            raise ValueError(f"S12 planning boundary violated: {key}")
    source = spec.get("source_document", {})
    route = spec.get("proposed_route", {})
    if source.get("edition") != "1993" or route.get("edition") != "1993":
        raise ValueError("S12 requires the specified 1993 source and route")
    if source.get("sha256") != SOURCE_SHA256:
        raise ValueError("S12 source digest differs from the pinned specification")

    rows = []
    for check in CHECKS:
        citations = {}
        for key in check["references"]:
            citation = spec.get("citations", {}).get(key)
            if not isinstance(citation, Mapping) or not citation.get("locator"):
                raise ValueError(f"Missing S12 citation or locator: {key}")
            citations[key] = deepcopy(citation)
        rows.append(
            {
                **deepcopy({k: v for k, v in check.items() if k != "references"}),
                "status": "NOT_EVALUATED",
                "selected_treatment": None,
                "citations": citations,
            }
        )
    return {
        "case_id": "S12",
        "status": "NOT_EVALUATED",
        "executable": False,
        "numerical_results": None,
        "source_document": {
            key: source[key] for key in ("document", "edition", "url", "sha256")
        },
        "checks": rows,
        "limits": (
            "Applicability checklist only. Source bytes and engineering applicability "
            "are not verified offline. Source-cell/equation qualification, numerical "
            "inputs, independent hand results, tolerances, edition implementation "
            "and report release remain unresolved outside this checklist."
        ),
    }


def main() -> None:
    """Print a checklist; successful generation does not imply qualification."""
    spec = json.loads(SPEC.read_text(encoding="utf-8"))
    print(json.dumps(build_report(spec), indent=2))


if __name__ == "__main__":
    main()
