# ABOUTME: Geometry-only requests for unqualified uniform local-loss envelopes.
# ABOUTME: Keeps both assessment levels unevaluated until Part 5 qualification.
"""Prepare envelope requests without executing an assessment or an inversion.

Basis: issue 2287 (2026), Parametric basis and Output and inversion contract;
docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md, Tables 1/3.
The latter records unresolved API 579-1/ASME FFS-1 2021 Part 5 procedures.
These requests complement uniform_loss_diagnostic; raw engine verdicts are
deliberately absent. No request, source string or digest grants qualification.

All lengths are inches and pressure is psi. Geometry is current external loss
in straight cylindrical pipe. FCA, material allowables, load suitability and
method applicability are NOT evaluated. No pressure or acceptance default is
inferred. Search bounds are caller inputs, never code-required minimum walls.
"""

from __future__ import annotations

import hashlib
import json
import math
from collections.abc import Iterable, Mapping

TARGET_STANDARD = "API 579-1/ASME FFS-1 2021 Part 5"
SOURCE_AUDIT = "docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md"
# Study resolution only, not a standard limit: issue 2287 (2026),
# Output and inversion contract, tolerance <= 0.0001 in.
MAX_WALL_TOLERANCE_IN = 0.0001


def _level_request(level: int) -> dict:
    """List missing evidence; source locators are requests, not verified clauses."""
    gaps = (
        ("SOURCE_EDITION_NOT_QUALIFIED", "Exact edition, errata and permitted use"),
        ("PREREQUISITES_NOT_EVALUATED", "Part 2 and Part 3 prerequisites"),
        ("APPLICABILITY_NOT_QUALIFIED", "Length, width, weld, spacing and wall limits"),
        (
            "REFERENCE_WALL_NOT_QUALIFIED",
            "Sound/flaw wall and separate FCA definitions",
        ),
        (
            "MATERIAL_LOAD_BASIS_NOT_QUALIFIED",
            "Allowables and pressure/load suitability",
        ),
        ("PROCEDURE_NOT_QUALIFIED", f"Part 5 Level {level} assessment and rerating"),
        ("INDEPENDENT_VALIDATION_MISSING", "Edition-matched independent examples"),
        ("INVERSION_NOT_EVALUATED", "Monotonicity, routing continuity and brackets"),
    )
    return {
        "level": level,
        "method": TARGET_STANDARD,
        "status": "NOT_EVALUATED",
        "allowable_remaining_wall_in": None,
        "criterion": None,
        "margin": None,
        "reason_codes": [code for code, _ in gaps],
        "checks": [
            {
                "reason_code": code,
                "requirement": requirement,
                "status": "NOT_EVALUATED",
                "source": f"{SOURCE_AUDIT}; Tables 1/3",
            }
            for code, requirement in gaps
        ],
    }


def _validate_inputs(inputs: Mapping[str, float]) -> None:
    """Reject nonphysical geometry and numerical search requests."""
    try:
        invalid = any(
            type(value) not in (float, int) or not math.isfinite(value) or value <= 0
            for value in inputs.values()
        )
    except OverflowError as error:
        raise ValueError("Inputs must be representable real numbers") from error
    if invalid:
        raise ValueError("All inputs must be finite positive real numbers")
    # Pure geometry/study constraints, not API 579 applicability thresholds.
    if not (inputs["sound_wall_in"] <= inputs["nominal_wall_in"] < inputs["od_in"] / 2):
        raise ValueError("Require sound wall <= nominal wall < OD/2")
    if inputs["width_fraction"] > 1:
        raise ValueError("Width fraction cannot exceed one full circumference")
    if not (
        inputs["lower_wall_in"] < inputs["upper_wall_in"] <= inputs["sound_wall_in"]
    ):
        raise ValueError("Require lower wall < upper wall <= sound wall")
    if inputs["wall_tolerance_in"] > min(
        MAX_WALL_TOLERANCE_IN, inputs["upper_wall_in"] - inputs["lower_wall_in"]
    ):
        raise ValueError("Tolerance exceeds study resolution or search interval")


def _geometry(od_in, sound_wall_in, axial_length_in, width_fraction) -> dict:
    """Calculate geometry-only coordinates from the requested current wall."""
    # Issue 2287 (2026), Parametric basis: Di=D-2t, Dm=D-t,
    # c=f*pi*Dm, angle=360*f, and s/sqrt(Di*t_sound).
    # 2 is the radius/diameter identity; 360 denotes degrees per full turn.
    # Arc/angle source: OpenStax, Precalculus 2e (2021), section 5.1,
    # "Converting Between Degrees and Radians" and "Finding the Length of an Arc":
    # https://openstax.org/books/precalculus-2e/pages/5-1-angles
    # This geometric ratio is NOT a qualified Part 5 lambda or Tc definition.
    inside = od_in - 2 * sound_wall_in
    mean = od_in - sound_wall_in
    scale_squared = inside * sound_wall_in
    if not math.isfinite(scale_squared) or scale_squared <= 0:
        raise ValueError("Geometry normalization is not representable")
    geometry = {
        "inside_diameter_in": inside,
        "mean_diameter_in": mean,
        "width_arc_in": width_fraction * math.pi * mean,
        "width_angle_deg": 360 * width_fraction,
        "sound_wall_normalized_length": axial_length_in / math.sqrt(scale_squared),
    }
    if any(not math.isfinite(value) or value <= 0 for value in geometry.values()):
        raise ValueError("Derived geometry is not finite and positive")
    return geometry


def prepare_envelope_cell(
    *,
    od_in: float,
    nominal_wall_in: float,
    sound_wall_in: float,
    axial_length_in: float,
    width_fraction: float,
    pressure_psi: float,
    lower_wall_in: float,
    upper_wall_in: float,
    wall_tolerance_in: float,
) -> dict:
    """Validate one physical request and withhold both allowable-wall ordinates.

    Bounds are in current remaining wall; no FCA adjustment is performed.
    The full circumference is geometrically representable but carries no
    assertion of applicability. Only a nonempty search interval within sound
    wall is admitted. Inputs and derived geometry must be finite and positive.
    """
    inputs = dict(
        od_in=od_in,
        nominal_wall_in=nominal_wall_in,
        sound_wall_in=sound_wall_in,
        axial_length_in=axial_length_in,
        width_fraction=width_fraction,
        pressure_psi=pressure_psi,
        lower_wall_in=lower_wall_in,
        upper_wall_in=upper_wall_in,
        wall_tolerance_in=wall_tolerance_in,
    )
    _validate_inputs(inputs)
    return {
        "inputs": inputs,
        "geometry": _geometry(od_in, sound_wall_in, axial_length_in, width_fraction),
        "loss_surface": "external",
        "assessment_status": "UNQUALIFIED",
        "asset_acceptance": None,
        # Issue 2287 requests distinct Level 1 and Level 2 outputs.
        "levels": [_level_request(level) for level in (1, 2)],
    }


def prepare_envelope_study(cases: Iterable[Mapping[str, float]]) -> dict:
    """Assemble valid cells in input order with a deterministic input digest.

    This is a request inventory, not a numerical study. All cells are validated
    before returning; an invalid request never yields a partial result.
    Repeated requests are retained as supplied, not counted as qualified cases.
    The digest covers canonical input JSON only, not sources or qualification.
    """
    cells = [prepare_envelope_cell(**case) for case in cases]
    if not cells:
        raise ValueError("At least one envelope request is required")
    canonical = json.dumps(
        [cell["inputs"] for cell in cells],
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")
    return {
        "meta": {
            "target_standard": TARGET_STANDARD,
            "purpose": "Geometry-only envelope requests; no engineering acceptance",
            "source_audit": SOURCE_AUDIT,
            "source_status": "Exact-edition technical qualification not established",
            "source_ids": ["api-standards", "ace-codes-standards-library"],
            "units": {
                "length": "in",
                "pressure": "psi",
                "angle": "deg",
                "fractions": "1",
                "normalized_length": "1",
            },
            "qualified_cases": 0,
            "config_sha256": hashlib.sha256(canonical).hexdigest(),
            "digest_scope": "canonical input JSON; ordered cells; no source bytes",
        },
        "cells": cells,
    }
