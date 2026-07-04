"""Generate a 4-block annular O-grid blockMeshDict for a 2D cylinder.

Body-fitted O-grid: cylinder radius R -> circular far-field radius Ro, 4 quadrant
blocks, radial grading (fine at the wall). 2D (1 cell in z, empty patches).
"""
import math
from pathlib import Path

R = 0.5          # cylinder radius (D = 1)
Ro = 20.0        # far-field radius (40 D)
NR = 110         # radial cells (graded)
NA = 48          # azimuthal cells per quadrant (192 around)
RGRAD = 120.0    # radial expansion ratio (first cell ~7e-3, ample for Re=100 BL)
TZ = 1.0         # span (z thickness), 1 cell

# inner (cylinder) and outer points at 0,90,180,270 deg, z=0 then z=TZ
ang = [0, 90, 180, 270]
def pt(r, a_deg, z):
    a = math.radians(a_deg)
    return (r * math.cos(a), r * math.sin(a), z)

V = []  # vertices
for z in (0.0, TZ):
    for a in ang:
        V.append(pt(R, a, z))   # inner 0..3 (z0), 8..11 (z1)
    for a in ang:
        V.append(pt(Ro, a, z))  # outer 4..7 (z0), 12..15 (z1)

def vfmt(v):
    return f"    ( {v[0]:.6f} {v[1]:.6f} {v[2]:.6f} )"

# index helpers: inner i(k)=k (+8 for top); outer o(k)=4+k (+8 for top)
def i(k, top=False):
    return (k % 4) + (8 if top else 0)
def o(k, top=False):
    return 4 + (k % 4) + (8 if top else 0)

blocks, arcs, cyl_faces, far_faces, fb_faces = [], [], [], [], []
for q in range(4):                      # quadrant q: from angle ang[q] to ang[q+1]
    a0, a1 = i(q), i(q + 1)
    b0, b1 = o(q), o(q + 1)
    a0t, a1t, b0t, b1t = i(q, 1), i(q + 1, 1), o(q, 1), o(q + 1, 1)
    # local-x = radial (inner->outer), local-y = azimuthal (q->q+1), local-z = span
    blocks.append(
        f"    hex ({a0} {b0} {b1} {a1} {a0t} {b0t} {b1t} {a1t}) "
        f"({NR} {NA} 1) simpleGrading ({RGRAD} 1 1)"
    )
    mid = ang[q] + 45.0
    pin, pout = pt(R, mid, 0.0), pt(Ro, mid, 0.0)
    pint, poutt = pt(R, mid, TZ), pt(Ro, mid, TZ)
    arcs.append(f"    arc {a0} {a1} ({pin[0]:.6f} {pin[1]:.6f} {pin[2]:.6f})")
    arcs.append(f"    arc {b0} {b1} ({pout[0]:.6f} {pout[1]:.6f} {pout[2]:.6f})")
    arcs.append(f"    arc {a0t} {a1t} ({pint[0]:.6f} {pint[1]:.6f} {pint[2]:.6f})")
    arcs.append(f"    arc {b0t} {b1t} ({poutt[0]:.6f} {poutt[1]:.6f} {poutt[2]:.6f})")
    # inner (cylinder) face: local-x=0 -> (a0 a1 a1t a0t)
    cyl_faces.append(f"            ({a0} {a1} {a1t} {a0t})")
    # outer (far-field) face: local-x=max -> (b0 b0t b1t b1)
    far_faces.append(f"            ({b0} {b0t} {b1t} {b1})")
    # front/back (z) faces: z0 (a0 b0 b1 a1), z1 (a0t a1t b1t b0t)
    fb_faces.append(f"            ({a0} {b0} {b1} {a1})")
    fb_faces.append(f"            ({a0t} {a1t} {b1t} {b0t})")

txt = f"""FoamFile {{ version 2.0; format ascii; class dictionary; object blockMeshDict; }}
convertToMeters 1;

vertices
(
{chr(10).join(vfmt(v) for v in V)}
);

blocks
(
{chr(10).join(blocks)}
);

edges
(
{chr(10).join(arcs)}
);

boundary
(
    cylinder
    {{
        type wall;
        faces
        (
{chr(10).join(cyl_faces)}
        );
    }}
    farfield
    {{
        type patch;
        faces
        (
{chr(10).join(far_faces)}
        );
    }}
    frontAndBack
    {{
        type empty;
        faces
        (
{chr(10).join(fb_faces)}
        );
    }}
);

mergePatchPairs ();
"""

out = Path(__file__).parent / "system" / "blockMeshDict"
out.parent.mkdir(parents=True, exist_ok=True)
out.write_text(txt)
print("wrote", out, "| vertices", len(V), "blocks", len(blocks))
