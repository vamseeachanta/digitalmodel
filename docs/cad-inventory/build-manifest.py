#!/usr/bin/env python3
"""Classify the raw CAD/CAM find output into a manifest + aggregate tables.

Input : cad-raw.tsv  (TAB: size_bytes \t mtime \t path)
Output: cad-file-manifest.csv  + aggregate tables to stdout.
"""
import csv, re, sys, collections, os

RAW = "cad-raw.tsv"
OUT = "cad-file-manifest.csv"

# extension -> (ecosystem, category)
EXT = {
    "sldprt": ("SolidWorks", "native"),
    "sldasm": ("SolidWorks", "native"),
    "ipt":    ("Autodesk Inventor", "native"),
    "iam":    ("Autodesk Inventor", "native"),
    "dwg":    ("AutoCAD", "native-2d"),
    "dxf":    ("AutoCAD (exchange)", "neutral-2d"),
    "catpart":("CATIA", "native"),
    "prt":    ("NX / generic (.prt)", "native"),
    "f3d":    ("Fusion 360", "native"),
    "f3z":    ("Fusion 360", "native"),
    "mcam":   ("Mastercam", "cam"),
    "nc":     ("NC G-code OR NetCDF (ambiguous)", "cam-ambiguous"),
    "cnc":    ("CNC G-code", "cam"),
    "tap":    ("CNC G-code", "cam"),
    "gcode":  ("CNC G-code", "cam"),
    "step":   ("STEP (neutral)", "neutral-3d"),
    "stp":    ("STEP (neutral)", "neutral-3d"),
    "iges":   ("IGES (neutral)", "neutral-3d"),
    "igs":    ("IGES (neutral)", "neutral-3d"),
    "x_t":    ("Parasolid (neutral)", "neutral-3d"),
    "x_b":    ("Parasolid (neutral)", "neutral-3d"),
    "stl":    ("Mesh / 3D-print", "mesh"),
    "3mf":    ("Mesh / 3D-print", "mesh"),
    "sim":    ("OrcaFlex", "sim"),
    "dat":    ("OrcaFlex OR generic data (ambiguous)", "sim-ambiguous"),
}

PREEXIST = re.compile(r"\.preexisting-before-repo-move-[\d-]+")
DEP = re.compile(r"/(\.venv|site-packages|node_modules|\.git|__pycache__|\.tox)/")
INBOX = "/docs/disciplines/knowledge_skills/projects/ri/"

rows = []
with open(RAW, encoding="utf-8", errors="replace") as f:
    for line in f:
        line = line.rstrip("\n")
        if not line:
            continue
        parts = line.split("\t", 2)
        if len(parts) != 3:
            continue
        size, mtime, path = parts
        try:
            size = int(size)
        except ValueError:
            continue
        ext = path.rsplit(".", 1)[-1].lower() if "." in os.path.basename(path) else ""
        eco, cat = EXT.get(ext, ("UNKNOWN", "unknown"))
        comps = path.split("/")            # ['', 'mnt', 'ace', '<top>', ...]
        top = comps[3] if len(comps) > 3 else ""
        is_pre = bool(PREEXIST.search("/" + top + "/"))
        canonical = PREEXIST.sub("", top)  # active-folder name (strip archive suffix)
        is_dep = bool(DEP.search(path))
        is_inbox = INBOX in path
        rows.append({
            "format": ext, "ecosystem": eco, "category": cat,
            "size_bytes": size, "mtime": mtime,
            "top_folder": top, "project_group": canonical,
            "is_preexisting": is_pre, "is_dependency": is_dep,
            "is_inbox_dump": is_inbox, "path": path,
        })

# which canonical project_groups have a live (non-preexisting) twin present?
live_groups = {r["project_group"] for r in rows if not r["is_preexisting"]}

cols = ["format","ecosystem","category","size_bytes","mtime","top_folder",
        "project_group","is_preexisting","is_dependency","is_inbox_dump","path"]
with open(OUT, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader()
    for r in rows:
        w.writerow(r)

def human(n):
    for u in ["B","KB","MB","GB","TB"]:
        if n < 1024: return f"{n:.1f}{u}"
        n /= 1024
    return f"{n:.1f}PB"

total = len(rows)
print(f"TOTAL rows: {total}")
print(f"TOTAL size: {human(sum(r['size_bytes'] for r in rows))}")
print(f"dependency-dir files (excluded from headline): {sum(r['is_dependency'] for r in rows)}")
print(f"ri/ inbox-dump files: {sum(r['is_inbox_dump'] for r in rows)}")
print(f"preexisting-archive files: {sum(r['is_preexisting'] for r in rows)}")

# "deduped / signal" set = drop dependency files AND drop preexisting copies
# whose canonical group also has a live twin (true archive duplicates).
def is_signal(r):
    if r["is_dependency"]:
        return False
    if r["is_preexisting"] and r["project_group"] in live_groups:
        return False
    return True
sig = [r for r in rows if is_signal(r)]
print(f"\nSIGNAL rows (dedup preexisting w/ live twin + drop deps): {len(sig)}")
print(f"SIGNAL minus ri-inbox: {sum(1 for r in sig if not r['is_inbox_dump'])}")

def table(title, rs, key):
    print(f"\n=== {title} ===")
    c = collections.Counter(r[key] for r in rs)
    sz = collections.defaultdict(int)
    for r in rs: sz[r[key]] += r["size_bytes"]
    for k, n in c.most_common(40):
        print(f"{n:>9}  {human(sz[k]):>10}  {k}")

table("BY FORMAT (all rows)", rows, "format")
table("BY ECOSYSTEM (all rows)", rows, "ecosystem")
table("BY TOP FOLDER (all rows)", rows, "top_folder")
table("BY FORMAT (signal set)", sig, "format")
table("BY ECOSYSTEM (signal set)", sig, "ecosystem")
table("BY TOP FOLDER (signal set)", sig, "top_folder")
# signal set excluding the ri inbox dump = the 'real curated project CAD'
proj = [r for r in sig if not r["is_inbox_dump"]]
table("BY TOP FOLDER (signal, excl ri-inbox) = curated project CAD", proj, "project_group")
table("BY ECOSYSTEM (signal, excl ri-inbox)", proj, "ecosystem")

import os, json, gzip, shutil
print(f"\nmanifest bytes (uncompressed): {os.path.getsize(OUT)}")

# gzip the manifest for committing (151MB raw -> repo-friendly)
with open(OUT, "rb") as fi, gzip.open(OUT + ".gz", "wb", compresslevel=9) as fo:
    shutil.copyfileobj(fi, fo)
print(f"manifest bytes (gzipped):     {os.path.getsize(OUT + '.gz')}")

# emit machine-readable summary for the README build
def counts(rs, key):
    c = collections.Counter(r[key] for r in rs)
    sz = collections.defaultdict(int)
    for r in rs: sz[r[key]] += r["size_bytes"]
    return {k: {"n": n, "bytes": sz[k]} for k, n in c.most_common()}

summary = {
    "total_rows": total,
    "total_bytes": sum(r["size_bytes"] for r in rows),
    "dependency_files": sum(r["is_dependency"] for r in rows),
    "ri_inbox_files": sum(r["is_inbox_dump"] for r in rows),
    "preexisting_files": sum(r["is_preexisting"] for r in rows),
    "signal_rows": len(sig),
    "deduped_removed": total - len(sig),
    "by_format_all": counts(rows, "format"),
    "by_ecosystem_all": counts(rows, "ecosystem"),
    "by_topfolder_all": counts(rows, "top_folder"),
    "by_format_signal": counts(sig, "format"),
    "by_ecosystem_signal": counts(sig, "ecosystem"),
    "by_topfolder_signal": counts(sig, "top_folder"),
    "curated_by_group": counts(proj, "project_group"),
    "curated_by_ecosystem": counts(proj, "ecosystem"),
    "live_groups_with_archive_twin": sorted(
        g for g in live_groups
        if any(r["is_preexisting"] and r["project_group"] == g for r in rows)),
}
with open("summary.json", "w") as f:
    json.dump(summary, f, indent=1)
print("wrote summary.json")
