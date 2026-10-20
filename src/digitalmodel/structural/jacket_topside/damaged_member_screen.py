"""Offline damaged tubular screening; no API/ISO fitness verdict is established.

Actions: kip, kip-in. Geometry: in. Stress: ksi. The remaining wall is an
explicit inspection interpretation, not a minimum substituted for an average.
"""
from __future__ import annotations

import argparse
from dataclasses import asdict
from datetime import date
import json
import math
from pathlib import Path

from digitalmodel.asset_integrity.applicability import Applicability
from digitalmodel.asset_integrity.assessment.ffs_decision import decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

from .dented_axial_screen import CITATIONS, DentedAxialInput, screen_dented_axial
from .member_checks import MemberLoads, TubularSection, member_checks

CORROSION_SOURCE = {
    "designation": "Comparison of API, ISO, and NORSOK Offshore Structural Standards",
    "year": 2012,
    "locator": "BOEMRE TA&R 677, Appendix A: ISO 19902:2007 A13.8 / NORSOK N-004 corroded members",
    "url": "https://www.bsee.gov/sites/bsee.gov/files/research-reports/677aa.pdf",
}
LIMITATIONS = (
    "NOT_EVALUATED: API RP 2SIM and ISO 19902 engineering acceptance; "
    "published experimental qualification is missing.",
    "Uniform corrosion uses an explicit representative remaining wall and unchanged OD. "
    "External diameter loss and localized patches require separate geometry/strength assessment.",
    "Existing member checks are diagnostic only: compression, bending compactness, "
    "and tension interaction have not been qualified for this workflow.",
    "NOT_EVALUATED: joint capacity, applied-bending dent reduction, pressure, "
    "fatigue, cracks, and combined corrosion/dent strength.",
    "Flooded/severed members request Level 3 engineering review by workflow policy; "
    "this is not a claimed API assessment clause or a calculated pushover/RSR result.",
    "No licensed or open-source solver is executed. A system model, loads, "
    "boundary conditions and a reviewed assessment basis are needed for the handoff.",
)


def _number(block, key, *, positive=True):
    value = block.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{key} must be an explicit finite number")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(f"{key} must be finite and {'positive' if positive else 'nonnegative'}")
    return value


def _condition(block, key):
    value = block.get(key)
    if not isinstance(value, bool):
        raise ValueError(f"{key} must be an explicit boolean")
    return value


def _finite_payload(value):
    """Undefined diagnostic numbers remain null in strict JSON."""
    if isinstance(value, float) and not math.isfinite(value):
        return None
    if isinstance(value, dict):
        return {key: _finite_payload(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_finite_payload(item) for item in value]
    return value


def _corrosion(item):
    raw = item["section"]
    section = TubularSection(**{
        key: _number(raw, key) for key in
        ("outer_diameter", "wall_thickness", "yield_strength", "elastic_modulus", "effective_length")
    })
    # Annulus geometry: OD > ID >= 0; factor 2 converts wall to diametral loss.
    if section.wall_thickness * 2 >= section.outer_diameter:
        raise ValueError("wall_thickness must be less than the radius")
    wall = _number(item, "remaining_thickness")
    if wall > section.wall_thickness:
        raise ValueError("remaining_thickness cannot exceed nominal wall_thickness")
    section.wall_thickness = wall
    actions = item["actions"]
    axial = _number(actions, "axial_force", positive=False)
    bending = _number(actions, "bending_moment", positive=False)
    # Elementary beam theory: N=integral(sigma dA), M=integral(sigma*y dA).
    # Stress Analysis Manual (1986), section 1.3.1.1, eq. 1-1 (M*c/I);
    # axial stress follows directly from uniform sigma integrated over area.
    # https://engineeringlibrary.org/reference/simple-beam-bending-air-force-stress-manual
    fa = axial / section.cross_sectional_area
    fb = bending / section.section_modulus
    loads = MemberLoads(fa=fa, fb=fb, Cm=_number(actions, "Cm"),
                        is_tension=_condition(actions, "is_tension"))
    checks = member_checks(section, loads)
    return {
        "status": "NOT_EVALUATED", "remaining_thickness_in": wall,
        "area_in2": section.cross_sectional_area,
        "section_modulus_in3": section.section_modulus,
        "diameter_to_thickness": section.dt_ratio,
        "axial_stress_ksi": fa, "bending_stress_ksi": fb,
        "diagnostic_checks": {name: asdict(value) for name, value in checks.items()},
        "citation": CORROSION_SOURCE,
    }


def _decision(flags):
    # Only the flagged branch is used: no default decision band or life criterion
    # is applied. NaN denotes an unevaluated margin, not a strength measurement.
    decision = decide(
        math.nan, math.nan, math.nan, "jacket_member",
        applicability=Applicability(False, flags, ["; ".join(LIMITATIONS)]),
    ).to_dict()
    # Shared jacket policy calls its margin RSR. No system ratio exists here.
    decision["governing_criterion"] = (
        "Engineering acceptance NOT_EVALUATED: " + ", ".join(flags)
    )
    return _finite_payload(decision)


def screen_member(item):
    """Compute bounded diagnostics; all unqualified fitness decisions escalate.

    Flooding/severance takes precedence over local strength calculations.
    Independent dent inputs are never combined with a corroded-section result.
    """
    identifier = item.get("id")
    if not isinstance(identifier, str) or not identifier.strip():
        raise ValueError("id must be a nonempty string")
    flooded = _condition(item, "flooded")
    severed = _condition(item, "severed")
    uniform = _condition(item, "uniform_corrosion")
    flags = ["EXPERIMENTAL_VALIDATION_MISSING"]
    result = {"id": identifier, "status": "NOT_EVALUATED", "dent": None,
              "corrosion": None, "handoff": None,
              "engineering_acceptance_established": False}
    if flooded or severed:
        flags.extend(name for name, active in (("FLOODED", flooded), ("SEVERED", severed)) if active)
        result["handoff"] = {
            "requested_assessment": "Level 3 engineering review",
            "basis": "workflow routing policy; system reserve strength not evaluated",
            "solver_executed": False,
            "required_inputs": ["system geometry/connectivity", "damage and inspection record",
                                "load combinations", "boundary conditions", "assessment criteria"],
        }
    elif "remaining_thickness" in item:
        if uniform:
            result["corrosion"] = _corrosion(item)
            flags.append("MEMBER_CHECKS_NOT_QUALIFIED")
        else:
            flags.append("LOCALIZED_CORROSION")
        if "dent" in item:
            flags.append("COMBINED_DAMAGE_NOT_QUALIFIED")
    elif "dent" in item:
        result["dent"] = asdict(screen_dented_axial(DentedAxialInput(**item["dent"])))
        flags.extend(result["dent"]["applicability_flags"])
    else:
        flags.append("DAMAGE_INPUT_MISSING")
    result["flags"] = flags
    result["decision"] = _decision(flags)
    result["citations"] = [asdict(citation) for citation in CITATIONS] + [CORROSION_SOURCE]
    return _finite_payload(result)


def screen_campaign(config):
    """Compose strict JSON data and the shared offline FFS screening report."""
    members = config.get("members")
    if not isinstance(members, list) or not members:
        raise ValueError("members must be a nonempty list")
    results = [screen_member(item) for item in members]
    if len({item["id"] for item in results}) != len(results):
        raise ValueError("member ids must be unique")
    sections = {
        "Summary": {"Assessment": "NOT_EVALUATED; engineering review required"},
        "Revision history": {"Revision": "0", "Date": date.today().isoformat(),
                             "Change and reason": "Initial damaged/corroded member screening"},
    }
    for index, result in enumerate(results, start=1):
        metrics = {"status": result["status"], "flags": ", ".join(result["flags"])}
        if result["dent"]:
            metrics.update({key: result["dent"][key] for key in ("capacity_ratio", "utilization", "verdict")})
        if result["corrosion"]:
            metrics.update({key: value for key, value in result["corrosion"].items()
                            if key not in ("diagnostic_checks", "citation")})
        if result["handoff"]:
            metrics["handoff"] = result["handoff"]["requested_assessment"]
        sections[f"Member {index}: {result['id']}"] = {
            key: f"{value:.1f}" if isinstance(value, float) else value
            for key, value in metrics.items()
        }
    sections["Sources"] = {"Uniform corrosion": " ".join(str(v) for v in CORROSION_SOURCE.values()),
                           "Dent and bow": " ".join(str(v) for v in asdict(CITATIONS[2]).values())}
    report = FFSReport.generate_screening_html(
        "Jacket members", "Damaged and corroded jacket member analysis",
        results[0]["decision"], sections, LIMITATIONS,
    )
    return {"workflow": "jacket-damaged-member-screen", "verdict": "ESCALATE",
            "engineering_acceptance_established": False,
            "members": results, "limitations": list(LIMITATIONS), "report_html": report}


def main():
    """Run the durable YAML workflow without importing any solver adapter."""
    import yaml

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.output.suffix.lower() != ".json":
        parser.error("--output must name a .json file (a sibling .html is also written)")
    if args.input.resolve() in (args.output.resolve(), args.output.with_suffix(".html").resolve()):
        parser.error("output must not overwrite the input")
    result = screen_campaign(yaml.safe_load(args.input.read_text(encoding="utf-8")))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, allow_nan=False), encoding="utf-8")
    args.output.with_suffix(".html").write_text(result["report_html"], encoding="utf-8")


if __name__ == "__main__":
    main()
