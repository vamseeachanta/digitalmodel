"""Mapped quarter-plate mesh for the research-only CalculiX crack spike.

No gmsh dependency: explicit polar layers and an extrusion preserve crack-plane
node identities. The benchmark is fixed in surface_crack_post.py.
"""
import math
from pathlib import Path

from surface_crack_post import BENCHMARK, E_MPA, HALF_LENGTH_MM, NU


def section(n):
    """Rectangular x-z section; the circular crack front is the r=a ring."""
    a, b, t = (BENCHMARK[k] for k in ("a_mm", "width_mm", "t_mm"))
    angles = sorted(set([math.pi*i/(2*n) for i in range(n+1)] + [math.atan(t/b)]))
    points, rings = [(0.0, 0.0)], []
    for i in range(1, 2*n+1):
        ring = []
        for phi in angles:
            outer = min(b/max(math.cos(phi), 1e-10), t/max(math.sin(phi), 1e-10))
            radius = (a*(1-(1-i/n)**2) if i <= n
                      else a+(outer-a)*((i-n)/n)**2)
            ring.append(len(points))
            points.append((radius*math.cos(phi), radius*math.sin(phi)))
        rings.append(ring)
    triangles = [(0, rings[0][j+1], rings[0][j]) for j in range(len(angles)-1)]
    quads = [(rings[i][j], rings[i][j+1], rings[i+1][j+1], rings[i+1][j])
             for i in range(2*n-1) for j in range(len(angles)-1)]
    return angles, points, rings, triangles, quads


def build_mesh(n):
    if not isinstance(n, int) or n < 3:
        raise ValueError("at least three radial samples required")
    angles, points, rings, triangles, quads = section(n)
    size = len(points)
    nodes = [(x, HALF_LENGTH_MM*(j/n)**2, z) for j in range(n+1) for x, z in points]
    wedges = [tuple(p+1+j*size for p in face)+tuple(p+1+(j+1)*size for p in face)
              for j in range(n) for face in triangles]
    hexes = [tuple(p+1+j*size for p in face)+tuple(p+1+(j+1)*size for p in face)
             for j in range(n) for face in quads]
    intact = [p+1 for ring in rings[n-1:] for p in ring]
    symmetry = [i+1 for i, (x, _, _) in enumerate(nodes) if abs(x) < 1e-10]
    samples = []
    for j, phi in enumerate(angles):
        ids = [rings[n-1+k][j]+1 for k in (1, 2, 3)]
        distances = [math.hypot(nodes[p-1][0], nodes[p-1][2])-BENCHMARK["a_mm"]
                     for p in ids]
        samples.append(dict(phi_deg=math.degrees(phi), nodes=ids, radii_mm=distances))
    return dict(nodes=nodes, wedges=wedges, hexes=hexes, intact=intact,
                symmetry=symmetry, anchor=1+n*size, angles=angles, samples=samples,
                opening_node=rings[n-2][-1]+1, n=n,
                loaded_wedges=list(range((n-1)*len(triangles)+1, n*len(triangles)+1)),
                loaded_hexes=list(range(len(wedges)+(n-1)*len(quads)+1,
                                        len(wedges)+n*len(quads)+1)))


def write_set(output, name, nodes):
    output.write(f"*NSET,NSET={name}\n")
    for start in range(0, len(nodes), 16):
        output.write(",".join(str(p) for p in nodes[start:start+16])+"\n")


def write_deck(path, model):
    """C3D6/C3D8 face 2 is the extrusion end; negative pressure is tension.

    CalculiX 2.21 manual sections 6.2.1, 6.2.9 and *DLOAD (face numbering).
    One z anchor removes rigid translation without imposing a support plane.
    """
    with Path(path).open("w") as out:
        out.write("*HEADING\nPublic synthetic surface-crack research benchmark\n*NODE\n")
        for node, xyz in enumerate(model["nodes"], 1):
            out.write(f"{node},"+",".join(f"{x:.12g}" for x in xyz)+"\n")
        offset = 0
        for kind, key in [("C3D6", "wedges"), ("C3D8", "hexes")]:
            out.write(f"*ELEMENT,TYPE={kind},ELSET=EALL\n")
            for i, conn in enumerate(model[key], offset+1):
                out.write(f"{i},"+",".join(map(str, conn))+"\n")
            offset += len(model[key])
        write_set(out, "INTACT", model["intact"])
        write_set(out, "XSYM", model["symmetry"])
        write_set(out, "ALL", list(range(1, len(model["nodes"])+1)))
        out.write(f"*MATERIAL,NAME=ELASTIC\n*ELASTIC\n{E_MPA},{NU}\n"
                  "*SOLID SECTION,ELSET=EALL,MATERIAL=ELASTIC\n"
                  "*BOUNDARY\nXSYM,1,1\nINTACT,2,2\n")
        out.write(f"{model['anchor']},3,3\n*STEP\n*STATIC\n*DLOAD\n")
        for element in model["loaded_wedges"]+model["loaded_hexes"]:
            out.write(f"{element},P2,{-BENCHMARK['sigma_m_mpa']}\n")
        out.write("*NODE FILE\nU\n*EL FILE\nS\n*NODE PRINT,NSET=INTACT,TOTALS=ONLY\n"
                  "RF\n*END STEP\n")
