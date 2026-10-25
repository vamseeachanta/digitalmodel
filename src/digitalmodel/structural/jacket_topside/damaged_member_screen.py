"""Offline damaged tubular screening; no API/ISO fitness verdict is established.

Actions: kip, kip-in. Geometry: in. Stress: ksi. The remaining wall is an
explicit inspection interpretation, not a minimum substituted for an average.
"""
from __future__ import annotations

import argparse
from collections.abc import Mapping
from dataclasses import asdict
import json
import math
from pathlib import Path

from digitalmodel.asset_integrity.applicability import Applicability
from digitalmodel.asset_integrity.assessment.ffs_decision import decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

from .dented_axial_screen import CITATIONS, DentedAxialInput, screen_dented_axial
from .member_checks import TubularSection

CORROSION_SOURCE = {
    "designation": "Comparison of API, ISO, and NORSOK Offshore Structural Standards",
    "year": 2012,
    "locator": "BOEMRE TA&R 677, Appendix A: ISO 19902:2007 A13.8 / NORSOK N-004 corroded members (secondary citation)",
    "url": "https://www.bsee.gov/sites/bsee.gov/files/research-reports/677aa.pdf",
}
LIMITATIONS = (
    "NOT_EVALUATED: API RP 2SIM and ISO 19902 engineering acceptance; "
    "published experimental qualification is missing.",
    "Uniform corrosion uses an explicit representative remaining wall and unchanged OD. "
    "External diameter loss and localized patches require separate geometry/strength assessment.",
    "Only section geometry, stresses, D/t and KL/r are reported for corrosion; "
    "member strength checks have not been qualified for this workflow.",
    "NOT_EVALUATED: joint capacity, applied-bending dent reduction, pressure, "
    "fatigue, cracks, and combined corrosion/dent strength.",
    "Flooded/severed members request Level 3 engineering review by workflow policy; "
    "this is not a claimed API assessment clause or a calculated pushover/RSR result.",
    "No licensed or open-source solver is executed. A system model, loads, "
    "boundary conditions and a reviewed assessment basis are needed for the handoff.",
)
FLAG_NOTES = {
    "EXPERIMENTAL_VALIDATION_MISSING": "Published experimental qualification is missing.",
    "FLOODED": "Flooded member requires system-level engineering review.",
    "SEVERED": "Severed member requires system-level engineering review.",
    "MEMBER_CHECKS_NOT_QUALIFIED": "Corroded member strength checks are not qualified.",
    "LOCALIZED_CORROSION": "Localized wall loss requires separate geometry and strength assessment.",
    "COMBINED_DAMAGE_NOT_QUALIFIED": "Combined corrosion and dent strength is not qualified.",
    "CORROSION_INPUT_MISSING": "Uniform corrosion requires an explicit remaining_thickness.",
    "DAMAGE_INPUT_MISSING": "No remaining thickness or dent input was supplied.",
}


def _mapping(value, name):
    if not isinstance(value, Mapping):
        raise ValueError(f"{name} must be a mapping")
    return value


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
    raw = _mapping(item.get("section"), "section")
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
    actions = _mapping(item.get("actions"), "actions")
    axial = _number(actions, "axial_force", positive=False)
    bending = _number(actions, "bending_moment", positive=False)
    # Elementary beam theory: N=integral(sigma dA), M=integral(sigma*y dA).
    # Stress Analysis Manual (1986), section 1.3.1.1, eq. 1-1 (M*c/I);
    # axial stress follows directly from uniform sigma integrated over area.
    # https://engineeringlibrary.org/reference/simple-beam-bending-air-force-stress-manual
    fa = axial / section.cross_sectional_area
    fb = bending / section.section_modulus
    # Keep the explicit input contract; no strength/interaction check uses it yet.
    _number(actions, "Cm")
    _condition(actions, "is_tension")
    return {
        "status": "NOT_EVALUATED", "remaining_thickness_in": wall,
        "area_in2": section.cross_sectional_area,
        "section_modulus_in3": section.section_modulus,
        "diameter_to_thickness": section.dt_ratio,
        "slenderness_ratio": section.slenderness_ratio,
        "axial_stress_ksi": fa, "bending_stress_ksi": fb,
        "citation": CORROSION_SOURCE,
    }


def _decision(flags):
    # Only the flagged branch is used: no default decision band or life criterion
    # is applied. NaN denotes an unevaluated margin, not a strength measurement.
    decision = decide(
        math.nan, math.nan, math.nan, "jacket_member",
        applicability=Applicability(False, flags, [
            f"{flag}: " + FLAG_NOTES.get(
                flag, f"Dent input {flag} is outside the supported study conditions."
            ) for flag in flags
        ]),
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
    item = _mapping(item, "member")
    identifier = item.get("id")
    if not isinstance(identifier, str) or not identifier.strip():
        raise ValueError("id must be a nonempty string")
    flooded = _condition(item, "flooded")
    severed = _condition(item, "severed")
    uniform = _condition(item, "uniform_corrosion")
    flags = ["EXPERIMENTAL_VALIDATION_MISSING"]
    if not (flooded or severed) and uniform and "remaining_thickness" not in item:
        flags.append("CORROSION_INPUT_MISSING")
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
        if uniform:
            flags.append("COMBINED_DAMAGE_NOT_QUALIFIED")
    else:
        flags.append("DAMAGE_INPUT_MISSING")
    result["flags"] = flags
    result["decision"] = _decision(flags)
    result["citations"] = [asdict(citation) for citation in CITATIONS] + [CORROSION_SOURCE]
    return _finite_payload(result)


def screen_campaign(config):
    """Compose strict JSON data and the shared offline FFS screening report."""
    members = _mapping(config, "config").get("members")
    if not isinstance(members, list) or not members:
        raise ValueError("members must be a nonempty list")
    results = [screen_member(item) for item in members]
    if len({item["id"] for item in results}) != len(results):
        raise ValueError("member ids must be unique")
    sections = {
        "Summary": {"Assessment": "NOT_EVALUATED; engineering review required"},
        "Revision history": {
            "Revision 0, 2026-10-20": "Initial damaged/corroded member screening",
            "Revision 1, 2026-10-22": (
                "Remove unqualified strength outputs, retain campaign flags, "
                "preserve precision and validate missing inputs."
            ),
        },
    }
    for index, result in enumerate(results, start=1):
        metrics = {"status": result["status"], "flags": ", ".join(result["flags"])}
        if result["dent"]:
            metrics.update({key: result["dent"][key] for key in ("capacity_ratio", "utilization", "verdict")})
        if result["corrosion"]:
            metrics.update({key: value for key, value in result["corrosion"].items()
                            if key != "citation"})
        if result["handoff"]:
            metrics["handoff"] = result["handoff"]["requested_assessment"]
        sections[f"Member {index}: {result['id']}"] = {
            key: (format(value, ".3f" if key in {
                "remaining_thickness_in", "capacity_ratio", "utilization",
                "diameter_to_thickness", "slenderness_ratio",
            } else ".1f") if isinstance(value, (int, float)) else value)
            for key, value in metrics.items()
        }
    sections["Sources"] = {"Uniform corrosion": " ".join(str(v) for v in CORROSION_SOURCE.values()),
                           "Dent and bow": " ".join(str(v) for v in asdict(CITATIONS[2]).values())}
    flags = list(dict.fromkeys(flag for result in results for flag in result["flags"]))
    report = FFSReport.generate_screening_html(
        "Jacket members", "Damaged and corroded jacket member analysis",
        _decision(flags), sections, LIMITATIONS,
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
