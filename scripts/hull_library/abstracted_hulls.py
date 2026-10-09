"""Decode DWGs in memory and export only allowlisted numeric CAD coordinates."""

import argparse
import hashlib
import io
import json
import logging
import math
import os
import subprocess
from contextlib import contextmanager, redirect_stderr, redirect_stdout
from itertools import pairwise
from pathlib import Path

from ezdxf import path as cad_path
from ezdxf import recover

ALLOWED_TYPES = frozenset({"LINE", "POLYLINE", "LWPOLYLINE", "ARC", "SPLINE"})


def digest(source):
    return hashlib.sha256(Path(source).read_bytes()).hexdigest()


@contextmanager
def private_diagnostics():
    previous = logging.root.manager.disable
    logging.disable(logging.CRITICAL)
    try:
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            yield
    finally:
        logging.disable(previous)


def read_dwg(source, converter):
    """No temporary DXF; converter stdout and diagnostics remain in memory."""
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    try:
        result = subprocess.run(
            [str(converter), "-v0", "-O", "DXF", str(Path(source).resolve())],
            capture_output=True,
            check=False,
            timeout=300,
            env=env,
        )
        if result.returncode != 0:
            raise ValueError("Decoder failed")
        with private_diagnostics():
            doc, audit = recover.read(io.BytesIO(result.stdout))
        if audit.errors:
            raise ValueError("Unresolved DXF audit errors")
        return doc, {"errors": len(audit.errors), "fixes": len(audit.fixes)}
    except Exception:  # noqa: BLE001 - private payloads require sanitized failures
        raise ValueError("DWG decoding failed; diagnostics withheld") from None


def mesh_curves(entity):
    if entity.is_poly_face_mesh:
        result = []
        for face in entity.faces():
            points = [v.dxf.location for v in face[:-1]]
            if points:
                result.append(points + [points[0]])
        return result
    m, n = entity.dxf.m_count, entity.dxf.n_count
    grid = [
        [entity.get_mesh_vertex((i, j)).dxf.location for j in range(n)]
        for i in range(m)
    ]
    rows = [list(row) for row in grid]
    columns = [[grid[i][j] for i in range(m)] for j in range(n)]
    if entity.is_m_closed:
        for column in columns:
            column.append(column[0])
    if entity.is_n_closed:
        for row in rows:
            row.append(row[0])
    return rows + columns


def entity_curves(entity):
    if entity.dxftype() == "POLYLINE":
        if entity.is_polygon_mesh or entity.is_poly_face_mesh:
            return mesh_curves(entity)
        if entity.is_3d_polyline:
            points = list(entity.points())
            if entity.is_closed and points:
                points.append(points[0])
            return [points]
    if entity.dxftype() == "LINE":
        return [[entity.dxf.start, entity.dxf.end]]
    return [list(cad_path.make_path(entity).flattening(distance=0.01))]


def coordinate_curves(doc, spatial_only=False):
    """Modelspace only: no INSERT expansion, layers, handles, text or XDATA."""
    curves = []
    with private_diagnostics():
        for entity in doc.modelspace():
            if entity.dxftype() not in ALLOWED_TYPES:
                continue
            if spatial_only and not (
                entity.dxftype() == "POLYLINE"
                and (
                    entity.is_3d_polyline
                    or entity.is_polygon_mesh
                    or entity.is_poly_face_mesh
                )
            ):
                continue
            for points in entity_curves(entity):
                values = [[float(c) for c in p] for p in points]
                if any(not math.isfinite(c) for p in values for c in p):
                    raise ValueError("Nonfinite coordinate")
                if len(values) > 1:
                    curves.append(values)
    return curves


def intersections(curves, axis, coordinate):
    """Polyline/plane intersections; these are offsets, not connected contours."""
    result = set()
    for curve in curves:
        for a, b in pairwise(curve):
            delta = b[axis] - a[axis]
            if delta == 0:
                if a[axis] == coordinate:
                    result.add(tuple(a))
                    result.add(tuple(b))
                continue
            fraction = (coordinate - a[axis]) / delta
            if 0 <= fraction <= 1:
                point = [a[i] + fraction * (b[i] - a[i]) for i in range(3)]
                point[axis] = coordinate
                result.add(tuple(point))
    return [list(p) for p in sorted(result)]


def units_status(doc, record, source_hash):
    declared = record.get("unit_inference", {}).get("status") == "declared"
    matches = source_hash == record.get("source_sha256")
    unit = doc.header.get("$INSUNITS")
    return (
        "mm_declared"
        if (declared and matches and unit == 4 and record["dxf"]["insunits"] == unit)
        else "units_unverified"
    )


def attribute_values_absent(doc, payload):
    """Values stay in memory; never return offending values or assertion locals."""
    text = payload.decode("utf-8").casefold()
    with private_diagnostics():
        for entity in doc.entitydb.values():
            if entity.dxftype() in {"ATTRIB", "ATTDEF"}:
                value = entity.dxf.get("text", "").strip().casefold()
                if value and value in text:
                    return False
    return True


def geometry_payload(doc, source_hash, audit):
    curves = coordinate_curves(doc, spatial_only=True)
    if not curves:
        raise ValueError("No spatial modelspace curves")
    bounds = [
        [min(p[i] for c in curves for p in c) for i in range(3)],
        [max(p[i] for c in curves for p in c) for i in range(3)],
    ]
    planes = {}
    for axis, name in enumerate(("plane_x", "plane_y", "plane_z")):
        lo, hi = bounds[0][axis], bounds[1][axis]
        planes[name] = [
            {
                "coordinate": lo + (hi - lo) * step / 20,
                "offsets": intersections(curves, axis, lo + (hi - lo) * step / 20),
            }
            for step in range(21)
        ]
    return {
        "schema_version": 1,
        "source_sha256": source_hash,
        "units": "mm_declared",
        "physical_scale": "unverified",
        "hull_axes": "unverified",
        "engineering_readiness": "unqualified",
        "audit": audit,
        "bounds": bounds,
        "curves": curves,
        "planes": planes,
    }


def export_document(doc, record, source_hash, target, audit, privacy_documents=None):
    if units_status(doc, record, source_hash) != "mm_declared":
        return False
    payload = (
        json.dumps(
            geometry_payload(doc, source_hash, audit),
            separators=(",", ":"),
            allow_nan=False,
        ).encode()
        + b"\n"
    )
    if not all(
        attribute_values_absent(other, payload)
        for other in [doc, *(privacy_documents or [])]
    ):
        raise ValueError("Metadata collision; output withheld")
    target = Path(target)
    target.parent.mkdir(parents=True, exist_ok=True)
    with target.open("xb") as stream:
        stream.write(payload)
    if target.read_bytes() != payload:
        raise ValueError("Output verification failed")
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--converter",
        required=True,
        type=Path,
        help="dwgread from scripts/tools/build-libredwg.sh",
    )
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument(
        "--allow-declared-units",
        action="store_true",
        help="Owner-authorized publication of declared-unit geometry",
    )
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    probe = json.loads((root / "docs/reports/dwg-conversion/probe.json").read_text())
    decoded = []
    try:
        for record in probe["sources"]:
            source = root / "docs/domains/freecad/src/hulls" / record["source"]
            before = digest(source)
            doc, audit = read_dwg(source, args.converter)
            status = units_status(doc, record, before)
            coordinate_curves(doc, spatial_only=(status == "mm_declared"))
            decoded.append((source, before, doc, audit, record, status))
        if any(digest(source) != before for source, before, *_ in decoded):
            raise ValueError("Source changed")
        privacy_documents = [entry[2] for entry in decoded]
        for index, (_, before, doc, audit, record, status) in enumerate(decoded):
            written = False
            if args.allow_declared_units:
                written = export_document(
                    doc,
                    record,
                    before,
                    args.output / f"hull-{index + 1:02d}.json",
                    audit,
                    privacy_documents=privacy_documents,
                )
            print(f"hull-{index + 1:02d}: {status}; written={written}")
    except Exception:  # noqa: BLE001 - private payloads require sanitized failures
        print("Extraction blocked; diagnostics withheld")
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
