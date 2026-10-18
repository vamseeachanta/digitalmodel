"""Offline pack gates and explicit records for unavailable native qualification.

Criteria are pack requirements (agent-pack.md section 5), not solver certification.
Engineering equations and assumptions are listed in engineering-citations.md.
"""

from hashlib import sha256
import ast
from pathlib import Path

import numpy as np

from .panel_mesh import compute_mesh_summary, compute_panel_quality
from .rao_processing import RAOTable


BODY_FRAME = "x_forward_y_port_z_up"


def native_gate_record(check):
    """Return a deferred record without importing or querying any solver."""
    if check not in {"C1-OW", "C1-AQWA", "C4-IRREGULAR"}:
        raise ValueError("Unknown native check")
    return {
        "check": check,
        "status": "NOT_RUN",
        "reason": "Native qualification requires an external permitted solver run",
    }


def require_native_gate(check):
    """Fail loudly when native results are required by a consumer."""
    record = native_gate_record(check)
    raise RuntimeError(f"{record['check']}: {record['status']}: {record['reason']}")


def implementation_map():
    """Map direct-file package functions to their producing module using AST."""
    result = {}
    for path in sorted(Path(__file__).parent.glob("*.py")):
        for node in ast.parse(path.read_text()).body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                result[f"{path.stem}.{node.name}"] = path.name
    return result


def check_balance(mesh, mass_kg, rho=1025.0):
    """Require rho*V and declared free-floating mass within 1 percent."""
    volume = compute_mesh_summary(mesh).displaced_volume
    if (
        not np.isfinite(mass_kg)
        or mass_kg <= 0
        or volume <= 0
        or not np.isfinite(rho)
        or rho <= 0
    ):
        raise ValueError("Positive finite mass, density and outward volume required")
    relative_error = abs(rho * volume / mass_kg - 1)
    if relative_error > 0.01:
        raise ValueError("Buoyancy/mass balance exceeds 1 percent")
    return relative_error


def compute_buoyancy_centroid(mesh):
    """Divergence moments: int(x*z*nz), int(y*z*nz), int(z*z*nz/2).

    Exact quadratic triangle integration. Requires a full submerged closed
    surface, optionally lidless at z=0; symmetry is deliberately unsupported.
    """
    if mesh.isx or mesh.isy:
        raise ValueError("Centroid requires a reconstructed full mesh")
    volume = compute_mesh_summary(mesh).displaced_volume
    if volume <= 0:
        raise ValueError("Positive signed volume required")
    moments = np.zeros(3)
    for panel in mesh.panels:
        verts = np.asarray(panel.vertices)
        for ids in ((0, 1, 2), (0, 2, 3)):
            tri = verts[list(ids)]
            az = np.cross(tri[1] - tri[0], tri[2] - tri[0])[2] / 2
            z = tri[:, 2]
            for axis in (0, 1, 2):
                v = tri[:, axis]
                mean_product = (v.sum() * z.sum() + (v * z).sum()) / 12
                moments[axis] += az * mean_product * (0.5 if axis == 2 else 1)
    return {"frame": BODY_FRAME, "centre_of_buoyancy_m": (moments / volume).tolist()}


def check_mesh_resolution(mesh, shortest_wavelength_m, panel_limit):
    """Assert pack limits: aspect <=3, edge <=lambda/6, count <=limit."""
    if (
        not mesh.panels
        or shortest_wavelength_m <= 0
        or panel_limit <= 0
        or not np.isfinite(shortest_wavelength_m)
    ):
        raise ValueError(
            "Explicit positive wavelength, limit and nonempty mesh required"
        )
    qualities = [compute_panel_quality(panel) for panel in mesh.panels]
    edge = max(
        np.linalg.norm(np.asarray(p.vertices)[i] - np.asarray(p.vertices)[(i + 1) % 4])
        for p in mesh.panels
        for i in range(4)
    )
    if (
        max(q.aspect_ratio for q in qualities) > 3
        or edge > shortest_wavelength_m / 6
        or len(mesh.panels) > panel_limit
    ):
        raise ValueError("Mesh resolution gate failed")
    return {"max_edge_m": float(edge), "panel_count": len(mesh.panels)}


def check_waterline(mesh, design_draft_m):
    """Assert full-mesh waterline closure at 1 mm and draft within 1 cm.

    Only waterlines represented by panel edges at z=0 are supported. Crossing
    panels must be clipped upstream. Production native geometry stays NOT_RUN.
    """
    if (
        mesh.isx
        or mesh.isy
        or not mesh.panels
        or not np.isfinite(design_draft_m)
        or design_draft_m <= 0
    ):
        raise ValueError("Require full mesh and positive finite design draft")
    ends = []
    for panel in mesh.panels:
        v = np.asarray(panel.vertices)
        if np.all(abs(v[:, 2]) <= 1e-9):
            continue
        for a, b in zip(v, np.roll(v, -1, axis=0)):
            if abs(a[2]) <= 1e-9 and abs(b[2]) <= 1e-9:
                ends.extend([a, b])
    if not ends or any(
        sum(np.linalg.norm(a - b) <= 0.001 for b in ends) != 2 for a in ends
    ):
        raise ValueError(
            "Waterline closure gap exceeds 1 mm or boundary is not a polygon"
        )
    draft = -min(v[2] for p in mesh.panels for v in p.vertices)
    if abs(draft - design_draft_m) > 0.01:
        raise ValueError("Design draft mismatch exceeds 1 cm")
    gap = max(
        min(np.linalg.norm(a - b) for j, b in enumerate(ends) if j != i)
        for i, a in enumerate(ends)
    )
    return {
        "closure_gap_m": float(gap),
        "draft_m": float(draft),
        "waterplane_area_m2": compute_mesh_summary(mesh).waterplane_area,
    }


def check_normals(mesh):
    """Require positive signed volume, nonzero areas and opposite shared edges.

    Intended for closed full meshes. Full reconstruction is required for half
    meshes; local orientation cannot prove arbitrary self-intersection absent.
    """
    if mesh.isx or mesh.isy:
        raise ValueError("Normals gate requires reconstructed full mesh")
    summary = compute_mesh_summary(mesh)
    if not mesh.panels or summary.displaced_volume <= 0:
        raise ValueError("Normals gate requires positive signed volume")
    edges = {}
    for panel in mesh.panels:
        if compute_panel_quality(panel).area <= 0:
            raise ValueError("Zero-area panel")
        vertices = [tuple(v) for v in panel.vertices]
        for a, b in zip(vertices, vertices[1:] + vertices[:1]):
            edges[(a, b)] = edges.get((a, b), 0) + 1
    if any(n != 1 or edges.get((b, a), 0) != 1 for (a, b), n in edges.items()):
        raise ValueError("Unclosed surface or inconsistent shared-edge normals")
    return {"signed_volume_m3": summary.displaced_volume, "inconsistent_edges": 0}


def check_hydrodynamics(coefficients, heave_raos):
    """Check radiation reciprocity, damping and longest supplied-period heave.

    Heave evidence must be a Heave RAOTable in m/m; every heading is checked.
    Passing the supplied longest period does not establish the asymptotic limit.
    """
    for matrix in coefficients.added_mass + coefficients.radiation_damping:
        values = matrix.to_numpy()
        if not np.all(np.isfinite(values)) or not np.allclose(
            values, values.T, rtol=0.02, atol=1e-12
        ):
            raise ValueError("Radiation matrix asymmetry exceeds 2 percent")
    for matrix in coefficients.radiation_damping:
        if not np.all(np.isfinite(matrix.to_numpy())) or np.any(
            np.diag(matrix.to_numpy()) < 0
        ):
            raise ValueError("Negative or nonfinite radiation damping")
    if not coefficients.added_mass or not coefficients.radiation_damping:
        raise ValueError("Missing radiation coefficients")
    if not isinstance(heave_raos, RAOTable) or (
        heave_raos.dof.lower() != "heave" or heave_raos.unit != "m/m"
    ):
        raise ValueError("Long-wave heave requires a Heave RAOTable in m/m")
    periods = np.asarray(heave_raos.periods)
    headings = np.asarray(heave_raos.headings_deg)
    try:
        amplitudes = np.asarray(heave_raos.amplitudes)
    except ValueError as exc:
        raise ValueError("Long-wave heave requires complete amplitude rows") from exc
    if (
        not periods.size or not headings.size
        or not np.all(np.isfinite(periods)) or np.any(periods <= 0)
        or not np.all(np.isfinite(headings))
        or amplitudes.shape != (headings.size, periods.size)
        or not np.all(np.isfinite(amplitudes))
    ):
        raise ValueError("Long-wave heave requires finite complete RAO data")
    longest_period = periods.max()
    heave = amplitudes[:, periods == longest_period]
    if np.any(abs(heave - 1) > 0.02):
        raise ValueError("Long-wave heave differs from unity by more than 2 percent")
    worst = heave.flat[np.argmax(abs(heave - 1))]
    return {"status": "PASS", "heave_m_per_m": float(worst),
            "heave_period_s": float(longest_period)}


def build_trace(
    input_bytes, quantities, gates, disposition, maintainer_signature, *, required_quantities
):
    """Require complete coverage of independently declared offline quantities.

    Extra traced quantities cannot replace a missing required quantity.
    This trace never certifies a native run.
    """
    if not quantities or not gates or not disposition or not maintainer_signature:
        raise ValueError(
            "Missing trace quantities, gates, disposition or maintainer signature"
        )
    if (
        isinstance(required_quantities, str) or not required_quantities
        or any(not isinstance(name, str) or not name for name in required_quantities)
    ):
        raise ValueError("Explicit nonempty required quantities must be supplied")
    required = set(required_quantities)
    traced = required.intersection(quantities)
    coverage = 100.0 * len(traced) / len(required)
    missing = required.difference(traced)
    if missing:
        raise ValueError(
            f"Trace coverage {coverage:.1f} percent; missing required quantities: "
            + ", ".join(sorted(missing))
        )
    for value in quantities.values():
        if (
            not {"producer", "value", "unit"} <= value.keys()
            or not value["producer"]
            or not value["unit"]
        ):
            raise ValueError(
                "Each quantity requires a producing package function, value and unit"
            )
        if not np.all(np.isfinite(value["value"])):
            raise ValueError("Trace quantity is nonfinite")
    return {
        "input_sha256": sha256(input_bytes).hexdigest(),
        "quantities": quantities,
        "gates": gates,
        "disposition": disposition,
        "maintainer_signature": maintainer_signature,
        "coverage_percent": coverage,
        "required_quantities": sorted(required),
    }
