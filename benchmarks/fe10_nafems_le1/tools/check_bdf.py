# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline checks of the FE10 MYSTRAN decks (standard library only; no solver).

Parses the free-field bulk data and checks: control and case cards; unique GRID and CQUAD4 ids; every
reference resolves (grids, PSHELL -> MAT1); material and thickness (G0.5); element signed area > 0
(normal +z), aspect ratio and interior angles (FE2 limits: aspect <= 5, angles 45 to 135 deg; reported
for information on the deliberately distorted patch); total area against the exact quarter annulus
(LE1); inner and outer edge grids on their ellipses; symmetry constraints on x = 0 and y = 0; 3456 on
every grid; load resultant against p t (BO, AO) (LE1, FE5-type balance); patch boundary displacements
equal to the linear field. pyNastran's cross-referenced read (FE0) and MYSTRAN's input check (FE1)
are pending the execution environment.

Usage: python -I check_bdf.py <deck.bdf> --kind le1|patch
"""
import argparse
import math
import sys

E, NU, T, P = 210000.0, 0.3, 100.0, 10.0
AI, BI, AO, BO = 2000.0, 1000.0, 3250.0, 2750.0


def main() -> None:
    """Check the requested deck's geometry, references and load definition."""
    ap = argparse.ArgumentParser()
    ap.add_argument("deck")
    ap.add_argument("--kind", required=True, choices=["le1", "patch"])
    a = ap.parse_args()
    raw = open(a.deck, "rb").read()
    res = []

    def chk(cid: str, ok: bool, detail: str) -> None:
        res.append(ok)
        print("%-36s %s  %s" % (cid, "PASS" if ok else "FAIL", detail))

    try:
        text = raw.decode("ascii")
        ok_ascii = True
    except UnicodeDecodeError:
        text, ok_ascii = raw.decode("utf-8", "replace"), False
    lines = text.splitlines()
    ctrl = lines[: lines.index("BEGIN BULK") + 1] if "BEGIN BULK" in lines else []
    chk("ASCII; SOL 101, CEND, BEGIN BULK, ENDDATA", ok_ascii and "SOL 101" in ctrl and "CEND" in ctrl and lines[-1] == "ENDDATA",
        "%d lines" % len(lines))
    need = ["SPC = 1", "DISP = ALL", "STRESS = ALL"] + (["LOAD = 1"] if a.kind == "le1" else [])
    chk("case control", all(n in ctrl for n in need), "%s" % [n for n in need if n not in ctrl] or "all present")
    spc1: dict[int, set[str]]
    grids, quads, pshell, mat1, spc1, spcv, forces = {}, {}, {}, {}, {}, {}, []
    dup = []
    for l in lines[len(ctrl):]:
        if not l or l.startswith("$") or l == "ENDDATA":
            continue
        fl = l.split(",")
        c = fl[0]
        if c == "GRID":
            g = int(fl[1])
            if g in grids:
                dup.append(("GRID", g))
            grids[g] = (float(fl[3]), float(fl[4]), float(fl[5]))
        elif c == "CQUAD4":
            e = int(fl[1])
            if e in quads:
                dup.append(("CQUAD4", e))
            quads[e] = (int(fl[2]), [int(x) for x in fl[3:7]])
        elif c == "PSHELL":
            pshell[int(fl[1])] = (int(fl[2]), float(fl[3]), fl[4], fl[6])
        elif c == "MAT1":
            mat1[int(fl[1])] = (float(fl[2]), float(fl[4]))
        elif c == "SPC1":
            for grid_text in fl[3:]:
                spc1.setdefault(int(grid_text), set()).update(fl[2])
        elif c == "SPC":
            spcv[(int(fl[2]), fl[3])] = float(fl[4])
            spc1.setdefault(int(fl[2]), set()).update(fl[3])
        elif c == "FORCE":
            m = float(fl[4])
            forces.append((int(fl[2]), m * float(fl[5]), m * float(fl[6]), m * float(fl[7])))
    chk("unique ids", not dup, "%d GRID, %d CQUAD4; duplicates %s" % (len(grids), len(quads), dup or "none"))
    badref = [e for e, (pid, g) in quads.items() if pid not in pshell or any(x not in grids for x in g) or len(set(g)) != 4]
    chk("references resolve", not badref and all(v[0] in mat1 for v in pshell.values()), "%d elements with a bad reference" % len(badref))
    chk("G0.5 material and thickness", mat1.get(1) == (E, NU) and pshell.get(1, (0, 0))[1] == T,
        "MAT1 %s; PSHELL t %s mm (units N, mm, MPa)" % (mat1.get(1), pshell.get(1, (0, 0))[1]))
    area, neg, ar_max, ang = 0.0, 0, 0.0, [180.0, 0.0]
    edges: dict[tuple[int, ...], int] = {}
    for e, (pid, quad_nodes) in quads.items():
        p = [grids[x] for x in quad_nodes]
        s = 0.5 * sum(p[i][0] * p[(i + 1) % 4][1] - p[(i + 1) % 4][0] * p[i][1] for i in range(4))
        area += s
        neg += s <= 0
        el = [math.dist(p[i][:2], p[(i + 1) % 4][:2]) for i in range(4)]
        ar_max = max(ar_max, max(el) / min(el))
        for i in range(4):
            edge_u = [p[i - 1][k] - p[i][k] for k in range(2)]
            edge_v = [p[(i + 1) % 4][k] - p[i][k] for k in range(2)]
            t = math.degrees(math.acos(max(-1.0, min(1.0, (edge_u[0] * edge_v[0] + edge_u[1] * edge_v[1]) / (math.hypot(*edge_u) * math.hypot(*edge_v))))))
            ang = [min(ang[0], t), max(ang[1], t)]
        for i in range(4):
            k = tuple(sorted((quad_nodes[i], quad_nodes[(i + 1) % 4])))
            edges[k] = edges.get(k, 0) + 1
    chk("element signed area > 0 (normal +z)", neg == 0, "%d elements with area <= 0; total area %.6g mm^2" % (neg, area))
    quality = ar_max <= 5 and 45 <= ang[0] and ang[1] <= 135
    if a.kind == "le1":
        chk("FE2 aspect <= 5, angles 45-135 deg", quality, "largest aspect %.2f; interior angles %.1f to %.1f deg" % (ar_max, ang[0], ang[1]))
    else:
        print("%-36s %s  %s" % ("FE2 quality (information, patch)", "INFO", "largest aspect %.2f; interior angles %.1f to %.1f deg (distortion intended)" % (ar_max, ang[0], ang[1])))
    over = [k for k, n in edges.items() if n > 2]
    chk("edges shared by > 2 elements: 0", not over, "%d" % len(over))
    allg = set(grids)
    chk("3456 constrained at every grid", all(set("3456") <= spc1.get(g, set()) for g in allg), "")
    if a.kind == "le1":
        exact = math.pi / 4 * (AO * BO - AI * BI)
        chk("area against exact quarter annulus", abs(area - exact) / exact <= 0.01, "mesh %.1f, exact %.1f mm^2, difference %.2f %%" % (area, exact, (area - exact) / exact * 100))
        inner = [g for g, (x, y, z) in grids.items() if abs((x / AI) ** 2 + (y / BI) ** 2 - 1) < 1e-6]
        outer = [g for g, (x, y, z) in grids.items() if abs((x / AO) ** 2 + (y / BO) ** 2 - 1) < 1e-6]
        free = {k for k, n in edges.items() if n == 1}
        free_nodes = {x for k in free for x in k}
        chk("edge grids on their ellipses", len(inner) >= 2 and len(outer) >= 2 and free_nodes <= set(inner) | set(outer) | {g for g, c in grids.items() if c[0] == 0.0 or c[1] == 0.0},
            "%d grids on the inner ellipse, %d on the outer; every free-edge grid on an ellipse or a symmetry line" % (len(inner), len(outer)))
        y0 = [g for g, c in grids.items() if c[1] == 0.0]
        x0 = [g for g, c in grids.items() if c[0] == 0.0]
        chk("symmetry: u_y = 0 on y = 0, u_x = 0 on x = 0", all("2" in spc1.get(g, set()) for g in y0) and all("1" in spc1.get(g, set()) for g in x0)
            and not any("1" in spc1.get(g, set()) for g in allg - set(x0)) and not any("2" in spc1.get(g, set()) for g in allg - set(y0)),
            "%d grids on y = 0, %d on x = 0" % (len(y0), len(x0)))
        d = [g for g, c in grids.items() if c[0] == AI and c[1] == 0.0]
        chk("point D grid at (2000, 0)", len(d) == 1, "grid %s" % d)
        fx, fy = sum(x[1] for x in forces), sum(x[2] for x in forces)
        chk("load resultant = p t (BO, AO)", abs(fx - P * T * BO) <= 1e-6 * P * T * BO and abs(fy - P * T * AO) <= 1e-6 * P * T * AO and all(x[0] in outer for x in forces),
            "sum Fx %.6g N (expected %.6g), sum Fy %.6g N (expected %.6g); %d loaded grids, all on the outer ellipse" % (fx, P * T * BO, fy, P * T * AO, len(forces)))
    else:
        corners = {1: (0.0, 0.0), 2: (2.0, 0.0), 3: (2.0, 1.0), 4: (0.0, 1.0)}
        okv = True
        for g, (x, y) in corners.items():
            u, v = 1e-3 * x + 0.5e-3 * y, 0.5e-3 * x + 1e-3 * y
            gu = spcv.get((g, "1"), 0.0 if "1" in spc1.get(g, set()) else None)
            gv = spcv.get((g, "2"), 0.0 if "2" in spc1.get(g, set()) else None)
            okv &= gu is not None and gv is not None and abs(gu - u) <= 1e-15 and abs(gv - v) <= 1e-15
        interior_free = all(not ({"1", "2"} & spc1.get(g, set())) for g in (5, 6, 7, 8))
        chk("patch: boundary displacements = linear field", okv and interior_free, "corners 1-4 prescribed; interior grids 5-8 free in-plane")
        chk("patch: area 2.0 mm^2", abs(area - 2.0) <= 1e-12, "%.12g" % area)
    print("RESULT %s: %d checks, %d fail" % ("PASS" if all(res) else "FAIL", len(res), res.count(False)))
    sys.exit(0 if all(res) else 1)


if __name__ == "__main__":
    main()
