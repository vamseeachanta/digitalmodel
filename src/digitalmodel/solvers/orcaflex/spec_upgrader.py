"""Detect model type from generic spec and suggest domain-specific upgrade.

Heuristics:
- Riser: vertical line from vessel to seabed, flex joint, buoyancy modules
- Mooring: multiple radial lines from vessel, anchor endpoints, chain/wire segments
- Pipeline: single long line, pipeline material, coating definitions
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any


class DetectedModelType(str, Enum):
    RISER = "riser"
    MOORING = "mooring"
    PIPELINE = "pipeline"
    GENERIC = "generic"  # can't determine


@dataclass
class DetectionResult:
    model_type: DetectedModelType
    confidence: float  # 0.0-1.0
    evidence: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

_RISER_NAME_PATTERNS = re.compile(
    r"(flex|riser|umbilical|hose|jumper)", re.IGNORECASE
)
_MOORING_NAME_PATTERNS = re.compile(
    r"(chain|wire|polyester|nylon|hmpe|leg)", re.IGNORECASE
)
_PIPELINE_NAME_PATTERNS = re.compile(
    r"(pipe|pipeline|coating|flowline)", re.IGNORECASE
)


def _get_generic(spec: dict) -> dict:
    """Return the generic section, or empty dict."""
    return spec.get("generic", {}) or {}


def _get_lines(generic: dict) -> list[dict]:
    return generic.get("lines", []) or []


def _get_line_types(generic: dict) -> list[dict]:
    return generic.get("line_types", []) or []


def _get_vessels(generic: dict) -> list[dict]:
    return generic.get("vessels", []) or []


def _get_connection_data(line: dict) -> list[list]:
    """Extract [EndA, EndB] connection rows from a line's properties.

    The connection key is a compound YAML key like:
      Connection, ConnectionX, ConnectionY, ConnectionZ, ...
    Each value is a list of two rows (EndA, EndB).
    """
    props = line.get("properties", {}) or {}
    for key, val in props.items():
        if isinstance(key, str) and key.startswith("Connection,"):
            if isinstance(val, list) and len(val) >= 2:
                return val
    return []


def _connection_name(row: list) -> str:
    """Return the connection name (first element) of a connection row."""
    if isinstance(row, list) and len(row) > 0:
        return str(row[0])
    return ""


def _connection_z(row: list) -> float | None:
    """Return the Z coordinate (4th element, index 3) of a connection row."""
    if isinstance(row, list) and len(row) > 3:
        try:
            return float(row[3])
        except (TypeError, ValueError):
            return None
    return None


def _line_segment_data(line: dict) -> list[list]:
    """Extract LineType/Length/TargetSegmentLength rows."""
    props = line.get("properties", {}) or {}
    for key, val in props.items():
        if isinstance(key, str) and "LineType" in key and "Length" in key:
            if isinstance(val, list):
                return val
    return []


def _total_line_length(line: dict) -> float:
    """Sum all segment lengths for a line."""
    total = 0.0
    for seg in _line_segment_data(line):
        if isinstance(seg, list) and len(seg) >= 2:
            try:
                total += float(seg[1])
            except (TypeError, ValueError):
                pass
    return total


def _segment_type_names(line: dict) -> list[str]:
    """Return all line type names used in segments."""
    names = []
    for seg in _line_segment_data(line):
        if isinstance(seg, list) and len(seg) >= 1:
            names.append(str(seg[0]))
    return names


# ---------------------------------------------------------------------------
# Per-type scoring
# ---------------------------------------------------------------------------

def _score_riser(spec: dict) -> tuple[float, list[str]]:
    """Score likelihood the spec is a riser model."""
    generic = _get_generic(spec)
    lines = _get_lines(generic)
    line_types = _get_line_types(generic)
    vessels = _get_vessels(generic)
    evidence: list[str] = []
    score = 0.0

    # Signal: has vessel(s)
    if vessels:
        # Check if a line connects to a vessel at its top end
        vessel_names = {v.get("name", "") for v in vessels}
        for line in lines:
            conn = _get_connection_data(line)
            if len(conn) >= 2:
                end_a_name = _connection_name(conn[0])
                end_b_name = _connection_name(conn[1])
                # Riser pattern: one end at vessel, other at seabed/anchored
                vessel_end = end_a_name in vessel_names or end_b_name in vessel_names
                anchor_end = "Anchored" in (end_a_name, end_b_name)
                if vessel_end and anchor_end:
                    # Check vertical span
                    z_a = _connection_z(conn[0])
                    z_b = _connection_z(conn[1])
                    if z_a is not None and z_b is not None:
                        span = abs(z_a - z_b)
                        if span > 5.0:
                            score += 0.3
                            evidence.append(
                                f"Line '{line.get('name')}' spans {span:.1f}m "
                                f"vertically from vessel to seabed"
                            )
                            break

    # Signal: line type names suggest riser
    for lt in line_types:
        name = lt.get("name", "")
        if _RISER_NAME_PATTERNS.search(name):
            score += 0.3
            evidence.append(f"Line type name '{name}' matches riser pattern")
            break

    # Signal: line names suggest riser
    for line in lines:
        name = line.get("name", "")
        if _RISER_NAME_PATTERNS.search(name):
            score += 0.2
            evidence.append(f"Line name '{name}' matches riser pattern")
            break

    # Signal: has buoyancy modules (buoys_6d) attached to lines
    buoys = generic.get("buoys_6d", []) or []
    for buoy in buoys:
        props = buoy.get("properties", {}) or {}
        # Buoyancy modules typically have supported lines or net buoyancy
        volume = buoy.get("volume", 0) or 0
        if volume and float(volume) > 0:
            score += 0.2
            evidence.append(
                f"Buoy '{buoy.get('name')}' has positive volume "
                f"(buoyancy module)"
            )
            break

    # Signal: flex joints present
    flex_joints = generic.get("flex_joints", []) or []
    if flex_joints:
        score += 0.3
        evidence.append(f"Has {len(flex_joints)} flex joint(s)")

    # Signal: single line (or very few) -- risers typically have 1-2 lines
    if 1 <= len(lines) <= 3:
        score += 0.1
        evidence.append(f"Has {len(lines)} line(s), typical for riser")

    return min(score, 1.0), evidence


def _score_mooring(spec: dict) -> tuple[float, list[str]]:
    """Score likelihood the spec is a mooring model."""
    generic = _get_generic(spec)
    lines = _get_lines(generic)
    line_types = _get_line_types(generic)
    vessels = _get_vessels(generic)
    evidence: list[str] = []
    score = 0.0

    # Signal: multiple lines radiating from same vessel/buoy
    if vessels or generic.get("buoys_6d"):
        connection_sources: dict[str, int] = {}
        anchored_count = 0
        for line in lines:
            conn = _get_connection_data(line)
            if len(conn) >= 2:
                end_a = _connection_name(conn[0])
                end_b = _connection_name(conn[1])
                if end_a and end_a != "Anchored":
                    connection_sources[end_a] = (
                        connection_sources.get(end_a, 0) + 1
                    )
                if end_b and end_b != "Anchored":
                    connection_sources[end_b] = (
                        connection_sources.get(end_b, 0) + 1
                    )
                if end_a == "Anchored" or end_b == "Anchored":
                    anchored_count += 1

        # Multiple lines from same source
        for source, count in connection_sources.items():
            if count >= 3:
                score += 0.25
                evidence.append(
                    f"{count} lines connect to '{source}' (radial pattern)"
                )
                break

        # Multiple anchored endpoints
        if anchored_count >= 3:
            score += 0.25
            evidence.append(
                f"{anchored_count} lines have 'Anchored' endpoints"
            )

    # Signal: line types with chain/wire/polyester names
    for lt in line_types:
        name = lt.get("name", "")
        if _MOORING_NAME_PATTERNS.search(name):
            score += 0.25
            evidence.append(
                f"Line type name '{name}' matches mooring pattern"
            )
            break

    # Signal: line names with mooring keywords (Leg, chain, wire)
    mooring_line_count = 0
    for line in lines:
        name = line.get("name", "")
        if _MOORING_NAME_PATTERNS.search(name):
            mooring_line_count += 1
    if mooring_line_count >= 2:
        score += 0.25
        evidence.append(
            f"{mooring_line_count} lines have mooring-type names"
        )

    # Signal: winches with pretension
    winches = generic.get("winches", []) or []
    if winches:
        score += 0.15
        evidence.append(f"Has {len(winches)} winch(es)")

    # Signal: many lines (moorings typically 3+, often 6-16)
    if len(lines) >= 4:
        score += 0.1
        evidence.append(f"Has {len(lines)} lines, typical for mooring")

    return min(score, 1.0), evidence


def _score_pipeline(spec: dict) -> tuple[float, list[str]]:
    """Score likelihood the spec is a pipeline model."""
    generic = _get_generic(spec)
    lines = _get_lines(generic)
    line_types = _get_line_types(generic)
    vessels = _get_vessels(generic)
    evidence: list[str] = []
    score = 0.0

    # Signal: no vessels
    if not vessels:
        score += 0.15
        evidence.append("No vessels defined (typical for pipeline)")

    # Signal: single long line
    for line in lines:
        length = _total_line_length(line)
        if length > 100.0:
            score += 0.3
            evidence.append(
                f"Line '{line.get('name')}' is {length:.0f}m long"
            )
            break

    # Signal: line type with pipeline/coating keywords
    for lt in line_types:
        name = lt.get("name", "")
        if _PIPELINE_NAME_PATTERNS.search(name):
            score += 0.3
            evidence.append(
                f"Line type name '{name}' matches pipeline pattern"
            )
            break

    # Signal: coating properties on line types
    for lt in line_types:
        props = lt.get("properties", {}) or {}
        coating = props.get("CoatingThickness")
        if coating is not None and coating != 0:
            score += 0.2
            evidence.append(
                f"Line type '{lt.get('name')}' has coating "
                f"(thickness={coating})"
            )
            break

    # Signal: line name contains pipeline
    for line in lines:
        name = line.get("name", "")
        if _PIPELINE_NAME_PATTERNS.search(name):
            score += 0.2
            evidence.append(f"Line name '{name}' matches pipeline pattern")
            break

    # Signal: both ends anchored (no vessel attachment)
    for line in lines:
        conn = _get_connection_data(line)
        if len(conn) >= 2:
            end_a = _connection_name(conn[0])
            end_b = _connection_name(conn[1])
            if end_a == "Anchored" and end_b == "Anchored":
                score += 0.15
                evidence.append(
                    f"Line '{line.get('name')}' has both ends anchored"
                )
                break

    # Signal: support types (sleepers, etc.)
    support_types = generic.get("support_types", []) or []
    if support_types:
        score += 0.1
        evidence.append(
            f"Has {len(support_types)} support type(s) "
            f"(e.g. sleepers)"
        )

    return min(score, 1.0), evidence


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def detect_model_type(spec: dict) -> DetectionResult:
    """Analyze a generic spec and detect the actual model type.

    Scores the spec against riser, mooring, and pipeline heuristics
    and returns the highest-scoring type.  If no type scores above
    0.3, returns ``DetectedModelType.GENERIC``.
    """
    generic = _get_generic(spec)
    if not generic:
        return DetectionResult(
            model_type=DetectedModelType.GENERIC,
            confidence=0.0,
            evidence=["No 'generic' section found in spec"],
        )

    riser_score, riser_evidence = _score_riser(spec)
    mooring_score, mooring_evidence = _score_mooring(spec)
    pipeline_score, pipeline_evidence = _score_pipeline(spec)

    scores = {
        DetectedModelType.RISER: (riser_score, riser_evidence),
        DetectedModelType.MOORING: (mooring_score, mooring_evidence),
        DetectedModelType.PIPELINE: (pipeline_score, pipeline_evidence),
    }

    best_type = max(scores, key=lambda t: scores[t][0])
    best_score, best_evidence = scores[best_type]

    # Require a minimum confidence threshold
    if best_score < 0.3:
        all_evidence = []
        for _score, ev in scores.values():
            for e in ev:
                all_evidence.append(e)
        if not all_evidence:
            all_evidence.append("No strong signals for any model type")
        return DetectionResult(
            model_type=DetectedModelType.GENERIC,
            confidence=best_score,
            evidence=all_evidence,
        )

    return DetectionResult(
        model_type=best_type,
        confidence=best_score,
        evidence=best_evidence,
    )


def upgrade_spec(spec: dict, target_type: DetectedModelType) -> dict:
    """Rewrite a generic spec to use domain-specific sections.

    Not implemented in v1 -- detection only.
    """
    raise NotImplementedError(
        "upgrade_spec is planned for v2. "
        "Use detect_model_type() for detection only."
    )


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import sys

    import yaml

    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <spec.yml>")
        sys.exit(1)

    spec_path = Path(sys.argv[1])
    with open(spec_path) as f:
        spec = yaml.safe_load(f)

    result = detect_model_type(spec)
    print(f"Type: {result.model_type.value} (confidence: {result.confidence:.0%})")
    for e in result.evidence:
        print(f"  - {e}")
