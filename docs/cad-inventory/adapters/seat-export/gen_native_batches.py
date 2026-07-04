#!/usr/bin/env python3
"""Generate per-project batch lists of native seat-only CAD for STEP export.

From the readability index, split native files by project and by tool
(SolidWorks vs Inventor), as Windows-mapped paths (Z:\\ = the share root), so a
licensed seat can run the batch_export macros incrementally, active/curated
projects first (intact assembly refs), the ri-hoard last.
PRIVATE output (real paths) under /mnt/ace/_step-export/.
"""
import csv, os, re, collections, json
IDX = "/mnt/ace/_cad-index/cad-readability-index.tsv"
OUT = "/mnt/ace/_step-export/batches"; os.makedirs(OUT, exist_ok=True)

SW = {"sldprt","sldasm"}
INV = {"ipt","iam"}
OTHER_SEAT = {"prt","catpart","x_t","x_b","f3d","f3z"}   # listed separately (no macro)

def winpath(p):
    return "Z:\\" + p.replace("/mnt/ace/","",1).replace("/","\\")
def safe(name):
    return re.sub(r"[^A-Za-z0-9._-]","_", name)[:80]
# export priority: active/curated first, hoard last
def prio(proj):
    if "ri-hoard" in proj: return 9
    if proj.startswith("digitalmodel/subsea-risers"): return 1
    if "preexisting" in proj: return 7
    if proj.startswith("docs/"): return 2
    return 3

sw = collections.defaultdict(list); inv = collections.defaultdict(list); other = collections.defaultdict(list)
for r in csv.DictReader(open(IDX), delimiter="\t"):
    if r["readability"] != "seat-only": continue
    f = r["format"]; pj = r["project"]
    if f in SW: sw[pj].append(r["path"])
    elif f in INV: inv[pj].append(r["path"])
    elif f in OTHER_SEAT: other[pj].append(r["path"])

projects = sorted(set(list(sw)+list(inv)+list(other)), key=lambda p:(prio(p), p))
manifest = []
for pj in projects:
    base = f"{prio(pj)}_{safe(pj)}"
    rec = {"project": pj, "priority": prio(pj), "sw": len(sw.get(pj,[])),
           "inv": len(inv.get(pj,[])), "other": len(other.get(pj,[]))}
    for tag, d in (("sw",sw),("inv",inv),("other",other)):
        if d.get(pj):
            fn = os.path.join(OUT, f"{base}.{tag}.txt")
            with open(fn,"w") as o: o.write("\r\n".join(winpath(p) for p in d[pj]) + "\r\n")
            rec[tag+"_file"] = os.path.basename(fn)
    manifest.append(rec)

tot = {"sw": sum(r["sw"] for r in manifest), "inv": sum(r["inv"] for r in manifest),
       "other": sum(r["other"] for r in manifest), "projects": len(projects)}
json.dump({"totals": tot, "batches": manifest}, open(os.path.join(OUT,"_manifest.json"),"w"), indent=1)
print(json.dumps(tot, indent=1))
print("priority order (first 12):")
for r in manifest[:12]:
    print(f"  P{r['priority']} {r['project']}: SW={r['sw']} INV={r['inv']} other={r['other']}")
print(f"\nbatches written to {OUT}")
