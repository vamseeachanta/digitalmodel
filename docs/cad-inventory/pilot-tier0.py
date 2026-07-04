#!/usr/bin/env python3
"""Tier-0 license-free CAD pilot (digitalmodel #1015).

Exercises pythonocc-core (OCCT) + ezdxf + trimesh against real share files:
  read STEP/IGES -> metrics (solids/bbox/volume/mass) -> export STEP/STL/glTF/3MF
  -> JSON sidecar -> round-trip validation. Plus ezdxf on a DXF and a Parasolid
  read attempt (expected to fail -> confirms the no-OSS-reader gap).
Every capability is wrapped so partial failures still report.
"""
import json, os, traceback, subprocess

IN  = os.path.join(os.path.dirname(__file__), "in")
OUT = os.path.join(os.path.dirname(__file__), "out")
os.makedirs(OUT, exist_ok=True)
STEEL = 7850.0  # kg/m^3, assumed; STEP units assumed mm

report = {"env": {}, "geometry": [], "dxf": {}, "parasolid": {}, "ap242_write": {}}

def grep_count(path, kw):
    try:
        return int(subprocess.run(["grep","-ac",kw,path],capture_output=True,text=True).stdout.strip() or 0)
    except Exception:
        return None

# ---- env versions ----
try:
    import OCC
    report["env"]["pythonocc"] = OCC.VERSION
except Exception as e:
    report["env"]["pythonocc"] = f"IMPORT FAIL: {e}"
for mod in ("ezdxf", "trimesh", "numpy"):
    try:
        report["env"][mod] = __import__(mod).__version__
    except Exception as e:
        report["env"][mod] = f"IMPORT FAIL: {e}"

# ---- OCC helpers ----
def occ_read_step(path):
    from OCC.Core.STEPControl import STEPControl_Reader
    from OCC.Core.IFSelect import IFSelect_RetDone
    r = STEPControl_Reader()
    if r.ReadFile(path) != IFSelect_RetDone:
        raise RuntimeError("STEP read not done")
    r.TransferRoots()
    return r.OneShape()

def occ_read_iges(path):
    from OCC.Core.IGESControl import IGESControl_Reader
    from OCC.Core.IFSelect import IFSelect_RetDone
    r = IGESControl_Reader()
    if r.ReadFile(path) != IFSelect_RetDone:
        raise RuntimeError("IGES read not done")
    r.TransferRoots()
    return r.OneShape()

def shape_metrics(shape):
    from OCC.Core.TopExp import TopExp_Explorer
    from OCC.Core.TopAbs import TopAbs_SOLID, TopAbs_FACE
    from OCC.Core.Bnd import Bnd_Box
    from OCC.Core.BRepBndLib import brepbndlib
    from OCC.Core.GProp import GProp_GProps
    from OCC.Core.BRepGProp import brepgprop
    def count(t):
        e = TopExp_Explorer(shape, t); n = 0
        while e.More(): n += 1; e.Next()
        return n
    nsolid, nface = count(TopAbs_SOLID), count(TopAbs_FACE)
    box = Bnd_Box(); brepbndlib.Add(shape, box)
    xmin,ymin,zmin,xmax,ymax,zmax = box.Get()
    props = GProp_GProps(); brepgprop.VolumeProperties(shape, props)
    vol_mm3 = props.Mass()  # "Mass" of volume props == volume in model units^3
    com = props.CentreOfMass()
    return {
        "solids": nsolid, "faces": nface,
        "bbox_mm": [round(xmax-xmin,3), round(ymax-ymin,3), round(zmax-zmin,3)],
        "volume_mm3": round(vol_mm3,3),
        "mass_kg_steel": round(vol_mm3*1e-9*STEEL,4),
        "centre_of_mass": [round(com.X(),3), round(com.Y(),3), round(com.Z(),3)],
    }

def occ_write_step(shape, path, schema="AP214"):
    from OCC.Core.STEPControl import STEPControl_Writer, STEPControl_AsIs
    from OCC.Core.Interface import Interface_Static
    Interface_Static.SetCVal("write.step.schema", schema)
    w = STEPControl_Writer()
    w.Transfer(shape, STEPControl_AsIs)
    w.Write(path)

def occ_mesh_to_stl(shape, path, lin=1.0):
    from OCC.Core.BRepMesh import BRepMesh_IncrementalMesh
    from OCC.Core.StlAPI import StlAPI_Writer
    BRepMesh_IncrementalMesh(shape, lin)
    StlAPI_Writer().Write(shape, path)

# ---- geometry pipeline over STEP/IGES inputs ----
GEO = [
    ("part_single.step", "step"),
    ("assembly.stp",      "step"),
    ("part.igs",          "iges"),
]
for fname, kind in GEO:
    src = os.path.join(IN, fname)
    entry = {"file": fname, "kind": kind, "size_bytes": os.path.getsize(src)}
    try:
        shape = occ_read_step(src) if kind == "step" else occ_read_iges(src)
        entry["metrics"] = shape_metrics(shape)
        if kind == "step":
            entry["product_definition_count"] = grep_count(src, "PRODUCT_DEFINITION")
        base = os.path.splitext(fname)[0]
        # export STEP (AP214) + STL, then derive glTF/3MF via trimesh
        step_out = os.path.join(OUT, base + ".master.step")
        stl_out  = os.path.join(OUT, base + ".stl")
        occ_write_step(shape, step_out, "AP214")
        occ_mesh_to_stl(shape, stl_out, 1.0)
        entry["exports"] = {"step": os.path.basename(step_out), "stl": os.path.basename(stl_out)}
        try:
            import trimesh
            m = trimesh.load(stl_out)
            glb = os.path.join(OUT, base + ".glb"); m.export(glb)
            tmf = os.path.join(OUT, base + ".3mf"); m.export(tmf)
            entry["exports"]["glb"] = os.path.basename(glb)
            entry["exports"]["3mf"] = os.path.basename(tmf)
            entry["mesh"] = {"vertices": int(len(m.vertices)), "faces": int(len(m.faces)),
                             "watertight": bool(m.is_watertight)}
        except Exception as e:
            entry["trimesh_error"] = f"{e}"
        # round-trip: re-read the master STEP and compare invariants
        try:
            rt = shape_metrics(occ_read_step(step_out))
            o = entry["metrics"]
            entry["roundtrip"] = {
                "solids_match": rt["solids"] == o["solids"],
                "bbox_delta_mm": [round(abs(a-b),4) for a,b in zip(rt["bbox_mm"], o["bbox_mm"])],
                "volume_rel_err": (round(abs(rt["volume_mm3"]-o["volume_mm3"])/o["volume_mm3"],6)
                                   if o["volume_mm3"] else None),
            }
        except Exception as e:
            entry["roundtrip_error"] = f"{e}"
        # write JSON sidecar next to outputs
        with open(os.path.join(OUT, base + ".json"), "w") as f:
            json.dump(entry, f, indent=1)
    except Exception as e:
        entry["error"] = f"{type(e).__name__}: {e}"
        entry["trace"] = traceback.format_exc().splitlines()[-3:]
    report["geometry"].append(entry)

# ---- AP242 write capability test ----
try:
    shape = occ_read_step(os.path.join(IN, "part_single.step"))
    ap242 = os.path.join(OUT, "ap242_test.step")
    occ_write_step(shape, ap242, "AP242DIS")
    schema = grep_count(ap242, "AP242") or 0
    hdr = subprocess.run(["grep","-a","-m1","FILE_SCHEMA",ap242],capture_output=True,text=True).stdout.strip()
    report["ap242_write"] = {"attempted_schema": "AP242DIS", "wrote_file": os.path.exists(ap242),
                             "header_schema": hdr, "ap242_token_hits": schema}
except Exception as e:
    report["ap242_write"] = {"error": f"{e}"}

# ---- ezdxf on a real DXF ----
try:
    import ezdxf
    doc = ezdxf.readfile(os.path.join(IN, "drawing_nesting.dxf"))
    msp = doc.modelspace()
    from collections import Counter
    ent = Counter(e.dxftype() for e in msp)
    report["dxf"] = {
        "dxf_version": doc.dxfversion, "acad_release": getattr(doc, "acad_release", "?"),
        "layers": len(list(doc.layers)), "blocks": len(list(doc.blocks)),
        "modelspace_entities": sum(ent.values()),
        "entity_types": dict(ent.most_common(8)),
    }
except Exception as e:
    report["dxf"] = {"error": f"{e}"}

# ---- Parasolid read attempt (expected: no OSS reader) ----
try:
    shape = occ_read_step(os.path.join(IN, "part.x_t"))  # OCC has no x_t reader; this should fail
    report["parasolid"] = {"read": "UNEXPECTED SUCCESS", "metrics": shape_metrics(shape)}
except Exception as e:
    report["parasolid"] = {"read": "FAILED (expected — no OSS Parasolid reader)", "error": f"{type(e).__name__}: {e}"}

with open(os.path.join(OUT, "pilot-report.json"), "w") as f:
    json.dump(report, f, indent=1)
print(json.dumps(report, indent=1))
