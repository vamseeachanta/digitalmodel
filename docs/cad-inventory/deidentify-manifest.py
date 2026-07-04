#!/usr/bin/env python3
"""De-identify the manifest for the PUBLIC repo.

Raw manifest (cad-file-manifest.csv) keeps full paths incl. a personal name +
client/field linkage -> must NOT go public. This writes a de-identified manifest
that keeps every analytically-useful column but replaces the leaf `path` with a
stable hash and relabels the two external-company top folders.
"""
import csv, hashlib, gzip

IN  = "cad-file-manifest.csv"
OUT = "cad-file-manifest-deidentified.csv"

# external-company folder names -> neutral labels
RELABEL = {
    "saipem": "epc-partner",
    "saipem.preexisting-before-repo-move-20260520-064502": "epc-partner.preexisting",
    "doris": "eng-partner",
    "doris.preexisting-before-repo-move-20260520-064502": "eng-partner.preexisting",
}
def relabel(v):
    return RELABEL.get(v, v)

with open(IN, newline="", encoding="utf-8") as fi, \
     open(OUT, "w", newline="", encoding="utf-8") as fo:
    r = csv.DictReader(fi)
    cols = ["format","ecosystem","category","size_bytes","mtime",
            "top_folder","project_group","is_preexisting","is_dependency",
            "is_inbox_dump","path_sha1"]
    w = csv.DictWriter(fo, fieldnames=cols)
    w.writeheader()
    n = 0
    for row in r:
        w.writerow({
            "format": row["format"], "ecosystem": row["ecosystem"],
            "category": row["category"], "size_bytes": row["size_bytes"],
            "mtime": row["mtime"],
            "top_folder": relabel(row["top_folder"]),
            "project_group": relabel(row["project_group"]),
            "is_preexisting": row["is_preexisting"],
            "is_dependency": row["is_dependency"],
            "is_inbox_dump": row["is_inbox_dump"],
            "path_sha1": hashlib.sha1(row["path"].encode("utf-8")).hexdigest()[:16],
        })
        n += 1

with open(OUT, "rb") as fi, gzip.open(OUT + ".gz", "wb", compresslevel=9) as fo:
    import shutil; shutil.copyfileobj(fi, fo)

import os
print(f"rows: {n}")
print(f"deid gz bytes: {os.path.getsize(OUT + '.gz')}")
# leak self-check on the de-identified output
import subprocess
bad = subprocess.run(
    ["grep","-iEc","bassey|macondo|yellowtail|trion|blk31|woodfibre|perdido|ballymore|verderg|cameron|saipem|doris", OUT],
    capture_output=True, text=True).stdout.strip()
print(f"sensitive-token hits in deid manifest: {bad}")
