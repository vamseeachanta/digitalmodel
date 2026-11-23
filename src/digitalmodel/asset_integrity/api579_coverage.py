# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Non-calculating API 579 intake for shell distortion, fatigue, laminations, fire.

Source: API 579-1/ASME FFS-1, December 2021, public table of contents,
Parts 8, 11, 13 and 14, sections .3 (data) and .4 (assessment).
The contents establish clause locations only. No normative equations, limits,
material properties or applicability rules are implemented here.
"""

from collections.abc import Mapping
import json
from pathlib import Path


_SOURCE_URL = (
    "https://www.asme.org/getmedia/5726e673-eb65-456e-913d-3d0dce0359ff/"
    "asme-ffs-1_2021_toc.pdf"
)
# Part numbers identify chapters in the cited 2021 contents, not coefficients.
_PARTS = {
    8: ("weld misalignment and shell distortion", "Review measured geometry and construction tolerances."),
    14: ("fatigue", "Establish stress histories, material curve basis and fatigue applicability."),
    13: ("laminations", "Establish flaw characterization and edition-specific geometric rules."),
    11: ("fire damage", "Obtain metallurgical investigation and heat-exposure-zone assessment."),
}
# Intake categories follow the headings .3.1, .3.2 and .3.3 in that contents.
# Recording a reference does not verify its content or inspection sufficiency.
_EVIDENCE_KEYS = ("design_data", "operating_history", "inspection_data")
_RELATED = {
    8: ["digitalmodel.asset_integrity.shell_distortion_screen"],
    14: ["digitalmodel.fatigue.sn_curves", "digitalmodel.fatigue.rainflow"],
    13: [],
    11: [],
}


def assess_coverage(*, part: int, level: int = 1, evidence: Mapping | None = None) -> dict:
    """Record evidence references and route gaps; always return NOT_EVALUATED.

    ``level`` records the requested tier; it does not establish applicability.
    The default denotes a request for Level 1 (2021 sections .4.2), not a
    numeric acceptance limit. Evidence values are nonempty reference strings.
    No referenced file is read, and related calculation modules are not run.
    """
    if type(part) is not int or part not in _PARTS:
        raise ValueError("part must identify a supported API 579 chapter")
    # Level identifiers per 2021 sections .4.2, .4.3 and .4.4.
    if type(level) is not int or level not in (1, 2, 3):
        raise ValueError("level must be an integer assessment tier")
    evidence = {} if evidence is None else evidence
    if not isinstance(evidence, Mapping) or any(key not in _EVIDENCE_KEYS for key in evidence):
        raise ValueError("evidence must map supported intake categories to references")
    if any(not isinstance(value, str) or not value.strip() for value in evidence.values()):
        raise ValueError("evidence references must be nonempty strings")
    missing = [key for key in _EVIDENCE_KEYS if key not in evidence]
    limitation = "Criteria and edition-matched published-example validation are not implemented."
    if part == 8:
        limitation += " Existing 2016 piping feature checks do not establish 2021 component acceptance."
    if part == 14:
        limitation += " Existing DNV S-N curves and cycle counting are not Part 14 qualification."
    return {
        "part": part,
        "mechanism": _PARTS[part][0],
        "requested_level": level,
        "verdict": "NOT_EVALUATED",
        "evidence_status": "INCOMPLETE" if missing else "RECORDED",
        "evidence": dict(evidence),
        "missing_evidence": missing,
        "criteria_implemented": False,
        "published_example_validated": False,
        "limitations": limitation,
        "next_action": _PARTS[part][1] + " Obtain sourced criteria and a published validation case.",
        "related_modules": list(_RELATED[part]),
        "source": {
            "document": "API 579-1/ASME FFS-1",
            "edition": "2021",
            "data_clause": f"{part}.3",
            "assessment_clause": f"{part}.4",
            "url": _SOURCE_URL,
            "basis": "Public contents only; normative procedure not established.",
        },
    }


def router(cfg: dict) -> dict:
    """Attach batch intake results to an engine configuration without file writes."""
    requests = cfg.get("coverage_requests")
    if not isinstance(requests, list) or not requests:
        raise ValueError("coverage_requests must be a nonempty list")
    for request in requests:
        if (not isinstance(request, Mapping) or "part" not in request
                or set(request) - {"part", "level", "evidence"}):
            raise ValueError("each coverage request needs part and supported input keys")
    assessments = [assess_coverage(**request) for request in requests]
    cfg["api579_coverage_screen"] = {
        "verdict": "NOT_EVALUATED",
        "assessments": assessments,
    }
    return cfg


def main() -> None:
    """Read an intake YAML and print JSON without importing the legacy engine."""
    import argparse

    import yaml

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    args = parser.parse_args()
    cfg = yaml.safe_load(args.input.read_text(encoding="utf-8"))
    if not isinstance(cfg, dict):
        parser.error("input must be a mapping")
    print(json.dumps(router(cfg)["api579_coverage_screen"], indent=2))


if __name__ == "__main__":
    main()
