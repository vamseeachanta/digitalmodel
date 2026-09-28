#!/usr/bin/env python3
"""Screen repo-local panel inventory; retain failed meshes in the report.

Run from the repository root: python scripts/hull_library/screen_inventory.py
Exit 1 means some inventory meshes failed; successful signatures are still saved.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import math
import re
from pathlib import Path

import numpy as np
import yaml

from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline
from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
    require_hullprod,
    screen_panel_mesh,
    screen_trimesh,
)
from digitalmodel.hydrodynamics.hull_library.panel_catalog import PanelCatalogEntry
from digitalmodel.hydrodynamics.hull_library.panel_inventory import _infer_hull_type

ROOT = Path(__file__).resolve().parents[2]
CATALOG = "docs/domains/hull_library/curvature-signature-catalog.yaml"
TABLE = "docs/domains/hull_library/curvature-signature-table.md"
EXTENSIONS = {".gdf", ".dat", ".stl"}
COLUMNS = [
    "hull_id",
    "hull_type",
    "panels",
    "lref",
    "lref_mode",
    "I_D",
    "I_D_plus",
    "I_D_minus",
    "a_flat",
    "a_single",
    "a_elliptic",
    "a_saddle",
    "reliability",
    "crease_dominated",
]


def local_path(root: Path, path: str | Path) -> Path:
    """Reject references and symlinks outside the selected repository."""
    resolved = (root / path).resolve()
    if not resolved.is_relative_to(root):
        raise ValueError("mesh and output paths must remain inside the repository")
    return resolved


def discover_meshes(root: Path) -> list[Path]:
    """Enumerate before parsing, including missing consolidation sources."""
    paths = set()
    manifest = root / "scripts/consolidate_panels.py"
    if manifest.exists():
        tree = ast.parse(manifest.read_text(encoding="utf-8"))
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                isinstance(t, ast.Name) and t.id == "GDF_COPIES" for t in node.targets
            ):
                paths.update(
                    local_path(root, row[0]) for row in ast.literal_eval(node.value)
                )
    for directory, extensions in (
        (root / "data/hull_library/panels", {".gdf", ".dat"}),
        (root / "docs/domains/orcawave/examples", {".gdf"}),
    ):
        paths.update(
            local_path(root, p)
            for p in directory.rglob("*")
            if p.is_file() and p.suffix.lower() in extensions
        )
    return sorted(paths)


def mesh_id(path: Path, root: Path) -> str:
    """Use a readable stem plus relative-path digest to avoid basename collisions."""
    relative = path.relative_to(root).as_posix()
    stem = re.sub(r"[^a-z0-9]+", "_", path.stem.lower()).strip("_")
    return f"inventory_{stem}_{hashlib.sha256(relative.encode()).hexdigest()[:10]}"


def catalog_data(root: Path, catalog: Path) -> dict:
    """Preserve existing metadata and add public sources missing from the catalog."""
    data = (
        yaml.safe_load(catalog.read_text(encoding="utf-8")) if catalog.exists() else {}
    )
    data = data or {"version": "1.0"}
    rows = data.setdefault("entries", [])
    known = {local_path(root, row["file_path"]) for row in rows}
    ids = {row["hull_id"] for row in rows}
    if len(ids) != len(rows):
        raise ValueError("duplicate hull_id in catalog")
    for path in discover_meshes(root):
        if path in known:
            continue
        hull_id = mesh_id(path, root)
        if hull_id in ids:
            raise ValueError("discovered hull_id collides with catalog")
        rows.append(
            {
                "hull_id": hull_id,
                "hull_type": _infer_hull_type(path.stem).value,
                "name": path.stem,
                "source": "repo_inventory",
                "panel_format": "aqwa_dat" if path.suffix.lower() == ".dat" else "gdf",
                "file_path": path.relative_to(root).as_posix(),
            }
        )
        known.add(path)
        ids.add(hull_id)
    return data


def reference_length(row: dict) -> float | None:
    """Use declared dimensions only; absent lengths invoke HullProd auto mode."""
    for key in ("length_m", "Lpp", "length_bp"):
        if row.get(key) is not None:
            value = float(row[key])
            if not math.isfinite(value) or value <= 0:
                raise ValueError("reference length must be finite and positive")
            return value
    return None


def screen_entry(root: Path, row: dict) -> tuple[dict, str | None]:
    """Store the typed signature or a bounded failure reason, never mesh headers."""
    path = local_path(root, row["file_path"])
    row.pop("curvature_signature", None)
    result = {"hull_id": row["hull_id"], "hull_type": row["hull_type"]}
    if not path.is_file():
        return result, "missing mesh file"
    try:
        lref = reference_length(row)
    except (TypeError, ValueError):
        return result, "invalid reference length (expected finite positive metres)"
    entry = PanelCatalogEntry.model_validate(row)
    try:
        mesh = MeshPipeline().load(path)
    except Exception as exc:  # noqa: BLE001 - retain per-hull failure
        return result, f"mesh load failed ({type(exc).__name__})"
    result["panels"] = mesh.n_panels
    row["panel_count"] = mesh.n_panels
    row["vertex_count"] = mesh.n_vertices
    try:
        entry.curvature_signature = screen_panel_mesh(
            mesh,
            lref=lref,
            hull_type=entry.hull_type,
            keep_fields=False,
        ).signature
    except Exception as exc:  # noqa: BLE001 - retain per-hull failure
        return result, f"curvature screen failed ({type(exc).__name__})"
    signature = entry.curvature_signature.model_dump(mode="json")
    row["curvature_signature"] = signature
    result.update(signature)
    return result, None


def cylinder_control():
    """Open cylinder: same sampling as test_curvature_screen._cylinder."""
    import trimesh

    nc, nz, radius, height = 48, 26, 6.0, 26.0
    theta = np.linspace(0, 2 * np.pi, nc, endpoint=False)
    z = np.linspace(-height / 2, height / 2, nz + 1)
    vertices = np.array(
        [[radius * np.cos(t), radius * np.sin(t), zz] for zz in z for t in theta]
    )
    faces = []
    for j in range(nz):
        for i in range(nc):
            a, b = j * nc + i, j * nc + (i + 1) % nc
            c, d = (j + 1) * nc + i, (j + 1) * nc + (i + 1) % nc
            faces.extend([[a, b, d], [a, d, c]])
    return trimesh.Trimesh(vertices, np.asarray(faces), process=False)


def wigley_control():
    """Wigley half hull: same alternating diagonals and grid as the tests."""
    import trimesh

    nx, nz, length, breadth, draft = 160, 40, 100.0, 10.0, 6.25
    x, z = np.linspace(-length / 2, length / 2, nx + 1), np.linspace(-draft, 0, nz + 1)
    xx, zz = np.meshgrid(x, z, indexing="ij")
    yy = breadth / 2 * (1 - (2 * xx / length) ** 2) * (1 - (zz / draft) ** 2)
    vertices = np.column_stack((xx.ravel(), yy.ravel(), zz.ravel()))
    faces = []
    for i in range(nx):
        for j in range(nz):
            a, b = i * (nz + 1) + j, i * (nz + 1) + j + 1
            c, d = (i + 1) * (nz + 1) + j, (i + 1) * (nz + 1) + j + 1
            faces.extend(
                ([a, c, b], [b, c, d]) if (i + j) % 2 else ([a, c, d], [a, d, b])
            )
    return trimesh.Trimesh(vertices, np.asarray(faces), process=False)


def control_signatures() -> dict:
    """Compute analytical comparison meshes using the existing adapter."""
    import trimesh

    meshes = [
        ("sphere", trimesh.creation.icosphere(subdivisions=3, radius=1.0), 2.0),
        ("cylinder", cylinder_control(), 12.0),
        ("wigley", wigley_control(), 100.0),
    ]
    return {
        f"control_{name}": screen_trimesh(
            mesh,
            lref=lref,
            hull_type="ship" if name == "wigley" else name,
            keep_fields=False,
        ).signature
        for name, mesh, lref in meshes
    }


def cell(value) -> str:
    """Keep report cells stable, readable, and confined to one Markdown row."""
    if value is None:
        return "-"
    if isinstance(value, float):
        return f"{value:.6g}"
    return str(value).replace("|", "&#124;").replace("\n", " ").replace("\r", " ")


def write_table(
    table: Path, rows: list[dict], failures: dict, sources: list[dict]
) -> None:
    """Emit all attempted hulls and explicit failure reasons beside the controls."""
    lines = [
        "# Hull inventory curvature signatures",
        "",
        "Serani & Maki (2026), *Geometry-Based Metrics for Early-Stage Hull-Form",
        "Producibility Screening*, arXiv:2609.27544. See [screening guidance](curvature-screening.md).",
        "",
        "Generated with HullProd through the existing curvature adapter. Repo-local sources only.",
        "Declared length_m, Lpp, or length_bp supplies lref; otherwise HullProd chooses it automatically.",
        "Panels are source panels (controls: assessed triangles); symmetry expansion can increase the assessed count.",
        "Compare matched mesh densities and reference lengths. Poor reliability is not a successful quality gate.",
        "Crease-dominated signatures describe panel junctions, not plate producibility.",
        "Controls reproduce test_curvature_screen.py: unit sphere (subdivision 3), open cylinder",
        "(radius 6, height 26, 48 x 26), and Wigley half hull (100 x 10 x 6.25, 160 x 40).",
        "",
        "Regenerate: python scripts/hull_library/screen_inventory.py (exit 1 if any hull fails).",
        "",
        "| " + " | ".join(COLUMNS) + " |",
        "| " + " | ".join(["---"] * len(COLUMNS)) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(cell(row.get(key)) for key in COLUMNS) + " |")
    lines.extend(["", "## Failed screens", ""])
    lines.extend(f"- {cell(key)}: {reason}." for key, reason in failures.items())
    if not failures:
        lines.append("None.")
    lines.extend(["", "## Inventory sources", ""])
    lines.extend(
        f"- {cell(row['hull_id'])}: {cell(row['file_path'])}" for row in sources
    )
    table.parent.mkdir(parents=True, exist_ok=True)
    table.write_text("\n".join(lines) + "\n", encoding="utf-8")


def screen_inventory(
    root: Path, catalog: Path, table: Path, *, controls: bool = True
) -> dict:
    """Update supported catalog meshes and return hull_id -> failure reason."""
    root = Path(root).resolve()
    catalog, table = local_path(root, catalog), local_path(root, table)
    if catalog == table:
        raise ValueError("catalog and table must be different paths")
    data = catalog_data(root, catalog)
    sources = [
        row
        for row in data["entries"]
        if Path(row["file_path"]).suffix.lower() in EXTENSIONS
    ]
    require_hullprod()
    rows, failures = [], {}
    for source in sources:
        row, failure = screen_entry(root, source)
        if failure:
            failures[source["hull_id"]] = failure
            row["reliability"] = "FAILED"
        rows.append(row)
    if controls:
        for hull_id, signature in control_signatures().items():
            rows.append(
                dict(
                    signature.model_dump(mode="json"),
                    hull_id=hull_id,
                    panels=signature.panel_count,
                )
            )
    write_table(table, rows, failures, sources)
    catalog.parent.mkdir(parents=True, exist_ok=True)
    catalog.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return failures


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--catalog", type=Path, default=Path(CATALOG))
    parser.add_argument("--table", type=Path, default=Path(TABLE))
    parser.add_argument("--no-controls", action="store_true")
    args = parser.parse_args(argv)
    failures = screen_inventory(
        args.root, args.catalog, args.table, controls=not args.no_controls
    )
    for hull_id, reason in failures.items():
        print(f"{hull_id}: {reason}")
    print(f"Inventory report written; {len(failures)} failed screens.")
    return int(bool(failures))


if __name__ == "__main__":
    raise SystemExit(main())
