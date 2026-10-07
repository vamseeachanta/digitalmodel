"""
ABOUTME: Analytical controls used in the HullProd evaluation: Wigley hull, cylinder and the
hull_library test ship at several resolutions, to separate geometry from mesh sensitivity.

Usage (from a venv that has hullprod AND digitalmodel importable):
    python convergence_checks.py <work_dir>
The test-ship block needs the digitalmodel repo importable (it reuses the pytest fixture
profile in tests/hydrodynamics/hull_library/conftest.py).
"""

from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import trimesh
from hullprod import assess

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]


def wigley_half(length=100.0, breadth=10.0, draft=6.25, nx=80, nz=20) -> trimesh.Trimesh:
    """Standard Wigley starboard half: y = B/2 (1-(2x/L)^2)(1-(z/T)^2)."""
    x = np.linspace(-0.5 * length, 0.5 * length, nx + 1)
    z = np.linspace(-draft, 0.0, nz + 1)
    xx, zz = np.meshgrid(x, z, indexing="ij")
    yy = 0.5 * breadth * (1.0 - (2.0 * xx / length) ** 2) * (1.0 - (zz / draft) ** 2)
    verts = np.column_stack((xx.ravel(), yy.ravel(), zz.ravel()))
    row = nz + 1
    faces = []
    for i in range(nx):
        for j in range(nz):
            a = i * row + j
            b = a + 1
            c = (i + 1) * row + j
            d = c + 1
            if (i + j) % 2:
                faces.extend(([a, c, b], [b, c, d]))
            else:
                faces.extend(([a, c, d], [a, d, b]))
    return trimesh.Trimesh(verts, np.asarray(faces), process=False)


def cylinder(radius=6.0, height=26.0, nc=19, nz=13) -> trimesh.Trimesh:
    """Open cylinder shell (OC4-like column). Exactly developable: expect a_C_single == 1."""
    th = np.linspace(0, 2 * np.pi, nc, endpoint=False)
    z = np.linspace(-height / 2, height / 2, nz + 1)
    verts = np.array([[radius * np.cos(t), radius * np.sin(t), zz] for zz in z for t in th])
    faces = []
    for j in range(nz):
        for i in range(nc):
            a = j * nc + i
            b = j * nc + (i + 1) % nc
            c = (j + 1) * nc + i
            d = (j + 1) * nc + (i + 1) % nc
            faces.extend([[a, b, d], [a, d, c]])
    return trimesh.Trimesh(verts, np.asarray(faces), process=False)


def fmt(sig: dict) -> str:
    a = sig["a_C"]
    return (
        f"I_D={sig['I_D']:.3f} (+{sig['I_D_plus']:.3f}/-{sig['I_D_minus']:.3f}) "
        f"flat={a['flat']:.3f} single={a['single']:.3f} "
        f"ell={a['elliptic']:.3f} sad={a['saddle']:.3f}"
    )


def test_ship_meshes(work: Path):
    """Generate the hull_library fixture ship with HullMeshGenerator at three resolutions."""
    sys.path.insert(0, str(REPO / "tests" / "hydrodynamics" / "hull_library"))
    import conftest  # noqa: E402  pytest fixture module; __wrapped__ gives the plain function

    from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import export_gdf
    from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
        HullMeshGenerator,
        MeshGeneratorConfig,
    )
    from gdf_to_stl import mirror, quads_to_mesh, read_gdf

    profile = conftest.ship_profile.__wrapped__()
    for n in (500, 2000, 8000):
        for adaptive in (False, True):
            cfg = MeshGeneratorConfig(target_panels=n, adaptive_density=adaptive)
            mesh = HullMeshGenerator().generate(profile, cfg)
            tag = f"dm_ship_{n}_{'adapt' if adaptive else 'unif'}"
            gdf = export_gdf(mesh, work / f"{tag}.gdf")
            quads, isx, isy, _ = read_gdf(str(gdf))
            quads_to_mesh(mirror(quads, isx, isy)).export(work / f"{tag}.stl")
            yield tag, work / f"{tag}.stl"


def main() -> None:
    work = Path(sys.argv[1])
    work.mkdir(parents=True, exist_ok=True)

    print("== Wigley (L=100) refinement, lref=100")
    levels = {"coarse": (40, 10), "medium": (80, 20), "fine": (160, 40), "xfine": (320, 80)}
    for name, (nx, nz) in levels.items():
        p = work / f"wigley_{name}.stl"
        wigley_half(nx=nx, nz=nz).export(p)
        print(f"  {name:6s} {nx}x{nz}: {fmt(assess(p, lref=100.0).signature)}")

    print("== Cylinder D=12 H=26 refinement, lref=12 (expect single=1, I_D=0)")
    for nc, nz in ((19, 13), (48, 26), (120, 60)):
        p = work / f"cyl_{nc}x{nz}.stl"
        cylinder(nc=nc, nz=nz).export(p)
        print(f"  {nc}x{nz}: {fmt(assess(p, lref=12.0).signature)}")

    print("== hull_library test ship via HullMeshGenerator, lref=100")
    try:
        for tag, p in test_ship_meshes(work):
            print(f"  {tag}: {fmt(assess(p, lref=100.0).signature)}")
    except ImportError as exc:
        print(f"  skipped test-ship block (digitalmodel not importable): {exc}")


if __name__ == "__main__":
    sys.path.insert(0, str(HERE))
    main()
