#!/usr/bin/env python3
"""Tier-A geometric-fingerprint similarity probe (CAD-AI spike #1017, no ML).

Per STEP solid: scale/rotation-invariant shape descriptor from OpenCASCADE
(bbox aspect, normalized principal inertia, face-type histogram, log topo
counts). Then nearest-neighbour + does each known variant pair find itself?
"""
import os, math, glob
import numpy as np
from OCC.Core.STEPControl import STEPControl_Reader
from OCC.Core.IFSelect import IFSelect_RetDone
from OCC.Core.TopExp import TopExp_Explorer
from OCC.Core.TopAbs import TopAbs_SOLID, TopAbs_FACE, TopAbs_EDGE
from OCC.Core.Bnd import Bnd_Box
from OCC.Core.BRepBndLib import brepbndlib
from OCC.Core.GProp import GProp_GProps
from OCC.Core.BRepGProp import brepgprop
from OCC.Core.BRepAdaptor import BRepAdaptor_Surface
from OCC.Core.GeomAbs import (GeomAbs_Plane, GeomAbs_Cylinder, GeomAbs_Cone,
                              GeomAbs_Sphere, GeomAbs_Torus)
from OCC.Core.TopoDS import topods

IN = os.path.join(os.path.dirname(__file__), "sim_in")

def read(path):
    r = STEPControl_Reader()
    if r.ReadFile(path) != IFSelect_RetDone:
        raise RuntimeError("read fail")
    r.TransferRoots()
    return r.OneShape()

def count(shape, t):
    e = TopExp_Explorer(shape, t); n = 0
    while e.More(): n += 1; e.Next()
    return n

def face_hist(shape):
    kinds = {"plane":0,"cyl":0,"cone":0,"sphere":0,"torus":0,"other":0}
    e = TopExp_Explorer(shape, TopAbs_FACE)
    while e.More():
        s = BRepAdaptor_Surface(topods.Face(e.Current()), True)
        t = s.GetType()
        if   t == GeomAbs_Plane:    kinds["plane"]  += 1
        elif t == GeomAbs_Cylinder: kinds["cyl"]    += 1
        elif t == GeomAbs_Cone:     kinds["cone"]   += 1
        elif t == GeomAbs_Sphere:   kinds["sphere"] += 1
        elif t == GeomAbs_Torus:    kinds["torus"]  += 1
        else:                       kinds["other"]  += 1
        e.Next()
    tot = max(1, sum(kinds.values()))
    return [v/tot for v in kinds.values()]

def descriptor(shape):
    box = Bnd_Box(); brepbndlib.Add(shape, box)
    xmn,ymn,zmn,xmx,ymx,zmx = box.Get()
    dims = sorted([xmx-xmn, ymx-ymn, zmx-zmn]) or [1,1,1]
    d = dims[2] if dims[2] else 1.0
    aspect = [dims[0]/d, dims[1]/d]                      # scale-free
    props = GProp_GProps(); brepgprop.VolumeProperties(shape, props)
    pp = props.PrincipalProperties()
    m = pp.Moments()                                     # 3 principal moments
    msum = sum(abs(x) for x in m) or 1.0
    inertia = sorted([abs(x)/msum for x in m])[:2]       # scale/mass-free
    nf, ne = count(shape, TopAbs_FACE), count(shape, TopAbs_EDGE)
    topo = [math.log1p(nf), math.log1p(ne)]
    return np.array(aspect + inertia + face_hist(shape) + topo, dtype=float)

files = sorted(glob.glob(os.path.join(IN, "*.stp")))
labels, vecs = [], []
for f in files:
    name = os.path.splitext(os.path.basename(f))[0]
    try:
        vecs.append(descriptor(read(f))); labels.append(name)
    except Exception as e:
        print("SKIP", name, e)

X = np.vstack(vecs)
# z-score standardize each feature across the sample
mu, sd = X.mean(0), X.std(0); sd[sd == 0] = 1.0
Z = (X - mu) / sd
# pairwise euclidean distance
n = len(labels)
D = np.zeros((n, n))
for i in range(n):
    for j in range(n):
        D[i, j] = np.linalg.norm(Z[i] - Z[j])

print(f"\n{n} parts. Nearest neighbour of each (lower dist = more similar):\n")
for i in range(n):
    order = np.argsort(D[i])
    nn = order[1]  # skip self
    print(f"  {labels[i]:18s} -> {labels[nn]:18s}  d={D[i,nn]:.3f}")

# Evaluate known variant pairs: grpA/B/C _hmf <-> _afg
print("\nKnown-variant-pair test (is the partner the nearest neighbour?):")
pairs = [("grpA_hyd_hmf","grpA_hyd_afg"),("grpB_ck_hmf","grpB_ck_afg"),("grpC_bst_hmf","grpC_bst_afg")]
idx = {l:i for i,l in enumerate(labels)}
hit = 0
for a, b in pairs:
    if a in idx and b in idx:
        i, j = idx[a], idx[b]
        rank = int(np.where(np.argsort(D[i])==j)[0][0])  # 0=self
        ok = rank == 1
        hit += ok
        print(f"  {a} <-> {b}: dist={D[i,j]:.3f}, partner rank={rank} (1=nearest) {'HIT' if ok else 'miss'}")
print(f"\npairs where partner is the #1 nearest neighbour: {hit}/{len(pairs)}")
# also: are the 6 shells closer to each other than to the 4 out-group parts?
shells = [i for i,l in enumerate(labels) if l.startswith('grp')]
out = [i for i,l in enumerate(labels) if l.startswith('out')]
if shells and out:
    within = np.mean([D[i,j] for i in shells for j in shells if i<j])
    between = np.mean([D[i,j] for i in shells for j in out])
    print(f"mean dist within shells={within:.3f}  vs shells->out-group={between:.3f}  (within < between = good)")
