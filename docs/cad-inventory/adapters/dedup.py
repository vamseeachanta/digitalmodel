#!/usr/bin/env python3
"""Full-estate Tier-A dedup over the neutral-STEP corpus (#1007/#1017).

Two dedup signals, no ML:
  (1) EXACT content duplicate  -> sha256 (catches cross-folder mirror copies)
  (2) geometric NEAR-duplicate -> scale/rotation-invariant OCC fingerprint
Files >25MB deferred (logged, not silently dropped). De-identified output:
paths reported as sha1[:12] + top-folder only.
"""
import os, sys, hashlib, json, math, time
import numpy as np
from OCC.Core.STEPControl import STEPControl_Reader
from OCC.Core.IFSelect import IFSelect_RetDone
from OCC.Core.TopExp import TopExp_Explorer
from OCC.Core.TopAbs import TopAbs_FACE, TopAbs_EDGE, TopAbs_SOLID
from OCC.Core.Bnd import Bnd_Box
from OCC.Core.BRepBndLib import brepbndlib
from OCC.Core.GProp import GProp_GProps
from OCC.Core.BRepGProp import brepgprop
from OCC.Core.BRepAdaptor import BRepAdaptor_Surface
from OCC.Core.GeomAbs import (GeomAbs_Plane, GeomAbs_Cylinder, GeomAbs_Cone,
                              GeomAbs_Sphere, GeomAbs_Torus)
from OCC.Core.TopoDS import topods

D = os.path.dirname(__file__)
LIST = os.path.join(D, "step_all.tsv")
MAXB = 25 * 1024 * 1024

def sha256(p):
    h = hashlib.sha256()
    with open(p, "rb") as f:
        for b in iter(lambda: f.read(1 << 20), b""): h.update(b)
    return h.hexdigest()

def read(p):
    r = STEPControl_Reader()
    if r.ReadFile(p) != IFSelect_RetDone: raise RuntimeError("read")
    r.TransferRoots(); return r.OneShape()

def cnt(shape, t):
    e = TopExp_Explorer(shape, t); n = 0
    while e.More(): n += 1; e.Next()
    return n

def descriptor(shape):
    box = Bnd_Box(); brepbndlib.Add(shape, box)
    xm,ym,zm,xM,yM,zM = box.Get()
    dims = sorted([xM-xm, yM-ym, zM-zm]); d = dims[2] or 1.0
    props = GProp_GProps(); brepgprop.VolumeProperties(shape, props)
    pp = props.PrincipalProperties(); m = pp.Moments()
    msum = sum(abs(x) for x in m) or 1.0
    inertia = sorted([abs(x)/msum for x in m])[:2]
    kinds = {GeomAbs_Plane:0,GeomAbs_Cylinder:0,GeomAbs_Cone:0,GeomAbs_Sphere:0,GeomAbs_Torus:0}
    other = 0; e = TopExp_Explorer(shape, TopAbs_FACE)
    while e.More():
        t = BRepAdaptor_Surface(topods.Face(e.Current()), True).GetType()
        if t in kinds: kinds[t] += 1
        else: other += 1
        e.Next()
    fc = list(kinds.values()) + [other]; tot = max(1, sum(fc))
    fh = [x/tot for x in fc]
    nf, ne = cnt(shape, TopAbs_FACE), cnt(shape, TopAbs_EDGE)
    return [dims[0]/d, dims[1]/d] + inertia + fh + [math.log1p(nf), math.log1p(ne)], props.Mass(), nf

rows = []
with open(LIST) as f:
    for line in f:
        s, p = line.rstrip("\n").split("\t", 1)
        rows.append((int(s), p))
todo = [(s, p) for s, p in rows if s <= MAXB]
deferred = [p for s, p in rows if s > MAXB]
print(f"corpus={len(rows)}  processing<=25MB={len(todo)}  deferred>25MB={len(deferred)}", flush=True)

recs = []; t0 = time.time()
for i, (s, p) in enumerate(todo):
    rec = {"path": p, "top": p.split("/")[3] if len(p.split("/"))>3 else "",
           "id": hashlib.sha1(p.encode()).hexdigest()[:12], "size": s}
    try:
        rec["sha256"] = sha256(p)
        vec, vol, nf = descriptor(read(p))
        rec["vec"] = vec; rec["vol"] = vol; rec["faces"] = nf
    except Exception as e:
        rec["error"] = str(e)[:80]
    recs.append(rec)
    if (i+1) % 100 == 0:
        print(f"  {i+1}/{len(todo)}  {time.time()-t0:.0f}s", flush=True)

ok = [r for r in recs if "vec" in r]
print(f"parsed ok={len(ok)}  errors={len(recs)-len(ok)}  in {time.time()-t0:.0f}s", flush=True)

# (1) exact content duplicates by sha256
from collections import defaultdict
bysha = defaultdict(list)
for r in recs:
    if "sha256" in r: bysha[r["sha256"]].append(r)
exact = {k: v for k, v in bysha.items() if len(v) > 1}
exact_files = sum(len(v) for v in exact.values())
exact_wasted = sum((len(v)-1)*v[0]["size"] for v in exact.values())

# (2) geometric near-duplicates among DISTINCT-content files (one rep per sha)
reps = [next(iter(v)) for v in bysha.values() if "vec" in v[0]]
X = np.array([r["vec"] for r in reps], dtype=float)
med = np.median(X, 0); iqr = np.subtract(*np.percentile(X, [75, 25], 0)); iqr[iqr == 0] = 1.0
Z = (X - med) / iqr
near = []
TAU = 0.15
for i in range(len(reps)):
    di = np.linalg.norm(Z - Z[i], axis=1)
    di[i] = np.inf
    j = int(np.argmin(di))
    if di[j] < TAU and i < j:
        near.append((reps[i], reps[j], float(di[j])))
near.sort(key=lambda t: t[2])

out = {
    "corpus_total": len(rows), "processed": len(todo), "deferred_gt25mb": len(deferred),
    "parsed_ok": len(ok), "parse_errors": len(recs)-len(ok),
    "exact_dup_groups": len(exact), "exact_dup_files": exact_files,
    "exact_dup_wasted_bytes": exact_wasted,
    "distinct_content_files": len(reps),
    "near_dup_pairs": len(near), "near_tau": TAU,
    "top_exact_groups": [
        {"n": len(v), "size": v[0]["size"], "tops": sorted({r["top"] for r in v}),
         "ids": [r["id"] for r in v][:6]}
        for v in sorted(exact.values(), key=lambda v:(len(v)*v[0]["size"]), reverse=True)[:15]],
    "top_near_pairs": [
        {"d": round(d,4), "a": a["id"], "b": b["id"], "a_top": a["top"], "b_top": b["top"],
         "faces": [a.get("faces"), b.get("faces")]}
        for a, b, d in near[:25]],
}
json.dump(out, open(os.path.join(D, "dedup-report.json"), "w"), indent=1)
print(json.dumps({k: out[k] for k in (
    "corpus_total","processed","parsed_ok","parse_errors","exact_dup_groups",
    "exact_dup_files","exact_dup_wasted_bytes","distinct_content_files","near_dup_pairs")}, indent=1))
print("DONE_DEDUP")
