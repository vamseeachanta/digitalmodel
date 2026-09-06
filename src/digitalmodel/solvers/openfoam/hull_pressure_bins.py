"""Pressure force on a hull patch, binned along a direction, from written OpenFOAM fields.

Why: the total pressure force on a bare hull at low Froude number sits near zero and wobbles for
thousands of pseudo-iterations while the bow and stern contributions, each large, converge
separately. Binning the pressure force along the hull length shows which part of the hull is
still moving and lets a convergence criterion be set on the delivered-power-relevant total
rather than on the near-zero net pressure component.

Inputs: the serial polyMesh (points, faces, boundary; ascii or binary) and a reconstructed
volScalarField `p` (Pa) whose boundaryField for the patch is a nonuniform List<scalar> in the
same face order as the serial polyMesh. Per face: F = p * Sf, with Sf the outward area vector
of the patch face (OpenFOAM patch faces point out of the domain, i.e. into the hull), so the
force ON the hull is -sum(p * Sf); the sign convention here follows the forces function object
(pressure force = sum over faces of Sf * (p - pRef)), which is what force.dat carries.

CLI:
  python -m digitalmodel.solvers.openfoam.hull_pressure_bins <case> --patch hull --times 2000,2500,3000 \
      --nbin 20 --direction 1 0 0 [--json out.json]
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

import numpy as np

from .hull_face_resolution import _read_boundary, _read_faces, _read_points, _is_binary


def _patch_field_scalar(path: Path, patch: str) -> np.ndarray:
    """Read the nonuniform List<scalar> of `patch` from a volScalarField file (ascii or binary)."""
    data = np.memmap(path, dtype=np.uint8, mode="r")
    raw = bytes(data)
    m = re.search(rb"\n\s*" + patch.encode() + rb"\s*\{", raw)
    if not m:
        raise ValueError(f"{path}: patch {patch} not in boundaryField")
    m2 = re.compile(rb"nonuniform\s+List<scalar>\s*\n?\s*(\d+)\s*\n?\(").search(raw, m.end())
    if not m2:
        # uniform value
        mu = re.compile(rb"value\s+uniform\s+([-+0-9.eE]+)").search(raw, m.end())
        if mu:
            return np.array([float(mu.group(1))])
        raise ValueError(f"{path}: no List<scalar> for patch {patch}")
    n = int(m2.group(1)); pos = m2.end()
    if _is_binary(path):
        return np.frombuffer(raw, dtype="<f8", count=n, offset=pos).copy()
    txt = raw[pos:pos + 40 * n].decode("ascii", errors="ignore")
    vals = re.findall(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", txt)[:n]
    return np.array([float(v) for v in vals])


def patch_faces_geometry(polymesh: Path, patch: str):
    bnd = _read_boundary(polymesh / "boundary")
    if patch not in bnd:
        raise ValueError(f"patch {patch} not in {polymesh / 'boundary'}")
    n, start = bnd[patch]          # _read_boundary returns (nFaces, startFace)
    faces = _read_faces(polymesh / "faces", start, n)
    wanted = set(v for f in faces for v in f)
    pts = _read_points(polymesh / "points", wanted)
    Sf = np.zeros((n, 3)); Cf = np.zeros((n, 3))
    for i, f in enumerate(faces):
        P = np.array([pts[v] for v in f], dtype=float)
        # Newell area vector and area-weighted centre via triangle fan
        c0 = P.mean(axis=0); s = np.zeros(3); cw = np.zeros(3); aw = 0.0
        for k in range(len(P)):
            a = P[k]; b = P[(k + 1) % len(P)]
            t = 0.5 * np.cross(a - c0, b - c0); ta = np.linalg.norm(t)
            s += t; cw += ta * (a + b + c0) / 3.0; aw += ta
        Sf[i] = s; Cf[i] = cw / aw if aw > 0 else c0
    return Sf, Cf


def bin_pressure_force(case: Path, patch: str, times, nbin: int, direction, lo=None, hi=None):
    case = Path(case)
    Sf, Cf = patch_faces_geometry(case / "constant" / "polyMesh", patch)
    d = np.array(direction, dtype=float); d /= np.linalg.norm(d)
    x = Cf @ d
    lo = float(x.min()) if lo is None else lo; hi = float(x.max()) if hi is None else hi
    edges = np.linspace(lo, hi, nbin + 1)
    idx = np.clip(np.searchsorted(edges, x, side="right") - 1, 0, nbin - 1)
    out = {"patch": patch, "nbin": nbin, "direction": d.tolist(), "edges": edges.tolist(), "faces": int(len(x)),
           "area_m2": float(np.linalg.norm(Sf, axis=1).sum()), "times": {}}
    for t in times:
        p = _patch_field_scalar(case / str(t) / "p", patch)
        if p.size == 1:
            p = np.full(len(x), p[0])
        F = (p[:, None] * Sf)            # forces-FO convention: sum(Sf * p)
        Fx = F @ d
        per_bin = np.array([Fx[idx == b].sum() for b in range(nbin)])
        out["times"][str(t)] = {"total_along_direction": float(Fx.sum()), "per_bin": per_bin.tolist(),
                                "total_vector": F.sum(axis=0).tolist()}
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("case"); ap.add_argument("--patch", default="hull"); ap.add_argument("--times", required=True)
    ap.add_argument("--nbin", type=int, default=20); ap.add_argument("--direction", nargs=3, type=float, default=[1, 0, 0])
    ap.add_argument("--lo", type=float); ap.add_argument("--hi", type=float); ap.add_argument("--json")
    a = ap.parse_args(argv)
    times = [t.strip() for t in a.times.split(",")]
    r = bin_pressure_force(a.case, a.patch, times, a.nbin, a.direction, a.lo, a.hi)
    e = r["edges"]
    print(f"patch {r['patch']}: {r['faces']} faces, area {r['area_m2']:.1f} m2, {r['nbin']} bins from {e[0]:.1f} to {e[-1]:.1f} m along {r['direction']}")
    hdr = "bin      x-range [m]   " + "".join(f"{('F_x@'+t):>14}" for t in times)
    print(hdr)
    for b in range(r["nbin"]):
        row = f"{b:3d} {e[b]:8.1f} {e[b+1]:8.1f}   "
        row += "".join(f"{r['times'][t]['per_bin'][b]/1000:14.1f}" for t in times)
        print(row)
    print("total [kN]              " + "".join(f"{r['times'][t]['total_along_direction']/1000:14.1f}" for t in times))
    if a.json:
        Path(a.json).write_text(json.dumps(r, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
