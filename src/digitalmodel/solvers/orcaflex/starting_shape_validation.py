# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline starting-shape geometry screen; no solver bindings are loaded.

Basis:
* AQG r0 section 4 G0.6 (acceptance policy, not a licensed standard),
  transcribed in solvers/orcaflex/docs/agent-pack.md, C06.
* OpenStax, Calculus Volume 3 (2016), section 2.2, Theorem 2.2:
  https://openstax.org/books/calculus-volume-3/pages/2-2-vectors-in-three-dimensions
  Each chord d = sqrt(dx**2 + dy**2 + dz**2); polyline length = sum(d).
* Orcina online help (accessed 2026-10-19), Line data: User specified
  starting shape; Structure; Object connections:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linedata,Userspecifiedstartingshape.htm
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linedata,Structure.htm
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Objectconnections.htm

Rows describe node positions; sections run from end A to end B and contain
unstretched lengths. This compares polyline lengths to those lengths as an
AQG screen, not as an assertion of static equilibrium or allowable strain.
Diameter provenance cannot be inferred from XYZ rows. Unsupported dependencies,
units and coordinate frames remain explicitly NOT_EVALUATED.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import dist, fsum, isfinite
from typing import Any, Literal

# AQG r0 section 4 G0.6, C06 in solvers/orcaflex/docs/agent-pack.md:
ARC_LENGTH_RELATIVE_LIMIT = 0.001  # 0.1 percent expressed as a fraction.
ENDPOINT_LIMIT_M = 0.01  # Endpoint distance, metres; inclusive.

Status = Literal["PASS", "FAIL", "NOT_EVALUATED"]


@dataclass(frozen=True)
class StartingShapeFinding:
    line: str
    check: str
    status: Status
    message: str
    actual: float | None = None
    limit: float | None = None


def _finite(value: Any) -> bool:
    try:
        return (isinstance(value, (int, float)) and not isinstance(value, bool)
                and isfinite(value))
    except OverflowError:
        return False


def _table(mapping: dict, required: tuple[str, ...]) -> list[dict] | None:
    """Read combined YAML headers or parallel arrays, without broadcasting."""
    candidates = []
    for key, rows in mapping.items():
        if not isinstance(key, str) or "," not in key:
            continue
        columns = [c.strip() for c in key.split(",")]
        if not set(required).issubset(columns):
            continue
        if len(columns) != len(set(columns)) or not isinstance(rows, list):
            raise ValueError("Invalid table header or rows")
        parsed = []
        for row in rows:
            if not isinstance(row, list) or len(row) > len(columns):
                raise ValueError("Invalid table row")
            item = dict(zip(columns, row))
            if not set(required).issubset(item):
                raise ValueError("Table row is missing required columns")
            parsed.append(item)
        candidates.append(parsed)
    if any(column in mapping for column in required):
        arrays = [mapping.get(column) for column in required]
        if not all(isinstance(array, list) for array in arrays):
            raise ValueError("Parallel table columns must all be arrays")
        if len({len(array) for array in arrays}) != 1:
            raise ValueError("Parallel table columns have different row counts")
        parsed = [dict(zip(required, values)) for values in zip(*arrays)]
        # Preserve optional segmentation columns for section checks.
        for column in ("NumberOfSegments", "TargetSegmentLength"):
            if required == ("LineType", "Length") and column in mapping:
                values = mapping[column]
                if not isinstance(values, list) or len(values) != len(parsed):
                    raise ValueError("Optional column has a different row count")
                for item, value in zip(parsed, values):
                    item[column] = value
        candidates.append(parsed)
    if len(candidates) > 1:
        raise ValueError("Ambiguous duplicate table encodings")
    return candidates[0] if candidates else None


def _has_dependencies(value: Any, seen: set[int] | None = None) -> bool:
    """Do not mistake a partial mapping for resolved inherited geometry."""
    seen = set() if seen is None else seen
    if not isinstance(value, (dict, list)) or id(value) in seen:
        return False
    seen.add(id(value))
    if isinstance(value, dict):
        if "BaseFile" in value or "includefile" in value:
            return True
        value = value.values()
    return any(_has_dependencies(item, seen) for item in value)


def validate_starting_shapes(model: dict) -> list[StartingShapeFinding]:
    """Inspect explicitly user-specified lines in a flat model mapping.

    PASS applies only to the named check. NOT_EVALUATED is never geometric
    acceptance. A complete acceptance also requires independent shape provenance
    and native statics, neither of which this offline screen establishes.
    """
    findings = []
    lines = model.get("Lines")
    if not isinstance(lines, list):
        return findings
    general = model.get("General")
    si = isinstance(general, dict) and general.get("UnitsSystem") == "SI"
    unresolved = _has_dependencies(model)
    for index, line in enumerate(lines):
        if not isinstance(line, dict) or line.get("StaticsStep1") != "User specified":
            continue
        name = str(line.get("Name", f"Lines[{index}]"))

        def add(check, status, message, actual=None, limit=None):
            findings.append(StartingShapeFinding(
                name, check, status, message, actual, limit,
            ))

        if unresolved:
            add("table", "NOT_EVALUATED", "Unresolved BaseFile or include dependencies")
            continue
        _validate_line(line, si, add)
    return findings


def _validate_line(line, si, add):
    shape_columns = ("StartingShapeX", "StartingShapeY", "StartingShapeZ")
    try:
        rows = _table(line, shape_columns)
        if rows is None:
            # Partial shape headers must not masquerade as an absent table.
            partial = any(isinstance(k, str) and k.startswith("StartingShape")
                          for k in line)
            add("table", "FAIL", "Malformed starting-shape table" if partial
                else "Missing starting-shape table")
            return
        # Orcina, User specified starting shape: one position per node.
        # A nonempty line needs at least its two end nodes (topological count).
        if len(rows) < 2 or any(not _finite(row[c]) for row in rows for c in shape_columns):
            raise ValueError("At least two finite XYZ node positions are required")
    except ValueError as exc:
        add("table", "FAIL", f"Malformed starting-shape table: {exc}")
        return
    points = [tuple(row[c] for c in shape_columns) for row in rows]
    add("table", "PASS", "Finite XYZ node positions supplied")
    add("dimensions", "NOT_EVALUATED",
        "XYZ rows carry no source section-count or diameter provenance")
    if not si:
        for check in ("arc_length", "sections", "end_a", "end_b"):
            add(check, "NOT_EVALUATED", "Explicit SI units are required by this screen")
        return
    try:
        chords = [dist(a, b) for a, b in zip(points, points[1:])]
        if not all(isfinite(value) for value in chords):
            raise ValueError("Non-finite chord length")
        shape_length = fsum(chords)
    except (ValueError, OverflowError):
        add("arc_length", "FAIL", "Node differences exceed finite geometry range")
        return
    _validate_lengths(line, chords, shape_length, add)
    _validate_endpoints(line, points, add)


def _validate_lengths(line, chords, shape_length, add):
    try:
        sections = _table(line, ("LineType", "Length"))
        if sections is None:
            raise LookupError("Explicit section lengths and line types are required")
        if not sections or any(not _finite(s["Length"]) or s["Length"] <= 0
                               or not isinstance(s["LineType"], str) or not s["LineType"]
                               for s in sections):
            raise ValueError("Sections require positive finite lengths and line type names")
        total = fsum(s["Length"] for s in sections)
    except LookupError as exc:
        for check in ("arc_length", "sections"):
            add(check, "NOT_EVALUATED", str(exc))
        return
    except (ValueError, OverflowError) as exc:
        add("sections", "FAIL", f"Malformed section table: {exc}")
        add("arc_length", "NOT_EVALUATED", "Valid section lengths are required")
        return
    if line.get("LengthAndEndOrientations", "Explicit") != "Explicit":
        for check in ("arc_length", "sections"):
            add(check, "NOT_EVALUATED", "Calculated section lengths are not resolved")
        return
    # AQG r0 G0.6 relative mismatch = abs(Lshape - Lline) / Lline.
    error = abs(shape_length - total) / total
    add("arc_length", "PASS" if error <= ARC_LENGTH_RELATIVE_LIMIT else "FAIL",
        "Polyline length compared with owning line's unstretched section sum",
        error, ARC_LENGTH_RELATIVE_LIMIT)

    # Orcina Structure: target length takes precedence over NumberOfSegments.
    # Rounding and automatic segmentation are deliberately deferred.
    counts = [s.get("NumberOfSegments") for s in sections]
    if any(s.get("TargetSegmentLength") not in (None, "~") for s in sections) or any(
        n is None for n in counts
    ):
        add("sections", "NOT_EVALUATED", "Explicit active segment counts are required")
        return
    if any(not isinstance(n, int) or isinstance(n, bool) or n <= 0 for n in counts):
        add("sections", "FAIL", "Segment counts must be positive integers")
        return
    # Consecutive sections share a boundary node: N nodes = sum(nsegments) + 1.
    # Compare chord counts instead, avoiding any tolerance on this exact count.
    if sum(counts) != len(chords):
        add("sections", "FAIL", "Node count differs from owning line's segmentation")
        return
    offset = 0
    for section, count in zip(sections, counts):
        section_length = fsum(chords[offset:offset + count])
        error = abs(section_length - section["Length"]) / section["Length"]
        if error > ARC_LENGTH_RELATIVE_LIMIT:
            add("sections", "FAIL", "Section polyline length differs from owning section",
                error, ARC_LENGTH_RELATIVE_LIMIT)
            return
        offset += count
    add("sections", "PASS", "Node count and each section polyline length agree")


def _validate_endpoints(line, points, add):
    columns = ("Connection", "ConnectionX", "ConnectionY", "ConnectionZ")
    try:
        connections = _table(line, columns)
        if connections is not None and len(connections) != 2:
            raise ValueError("Exactly two connection rows are required")
    except ValueError as exc:
        for check in ("end_a", "end_b"):
            add(check, "FAIL", f"Malformed connection table: {exc}")
        return
    for check, index in (("end_a", 0), ("end_b", -1)):
        if connections is None or connections[index]["Connection"] != "Fixed":
            # Orcina Object connections: anchored z is seabed-relative; child
            # coordinates are parent-relative. Free ends impose no restraint.
            add(check, "NOT_EVALUATED", "An explicit Fixed global connection is required")
            continue
        target = tuple(connections[index][c] for c in columns[1:])
        if not all(_finite(v) for v in target):
            add(check, "FAIL", "Connection XYZ must be finite numeric coordinates")
            continue
        error = dist(points[index], target)
        add(check, "PASS" if error <= ENDPOINT_LIMIT_M else "FAIL",
            "Node distance from owning line's fixed end", error, ENDPOINT_LIMIT_M)
