#!/usr/bin/env python3
"""De-identify the manifest for the PUBLIC repo.

Raw manifest (cad-file-manifest.csv) keeps full paths incl. a personal name +
client/field linkage -> must NOT go public. This writes a de-identified manifest
that keeps every analytically-useful column but replaces the leaf `path` with a
stable hash and relabels the external-company top folders.

The company names it relabels, and the tokens its leak self-check searches for,
are themselves identifiers, so they are NOT in this public file. They are read
from a private JSON file named by DEIDENTIFY_MAP:

    {"relabel": {"<raw folder>": "<neutral label>", ...},
     "sensitive_tokens": ["<token>", ...]}

Without it the script refuses to run rather than publishing an un-relabelled
manifest.
"""
import csv, gzip, hashlib, json, os, re, shutil, sys

IN  = "cad-file-manifest.csv"
OUT = "cad-file-manifest-deidentified.csv"

_map_path = os.environ.get("DEIDENTIFY_MAP")
if not _map_path or not os.path.isfile(_map_path):
    sys.exit("deidentify-manifest: set DEIDENTIFY_MAP to the private relabel "
             "file; refusing to write a manifest without it")
with open(_map_path, encoding="utf-8") as fh:
    _private = json.load(fh)
RELABEL = dict(_private["relabel"])
SENSITIVE = [str(t) for t in _private["sensitive_tokens"]]
if not RELABEL or not SENSITIVE:
    sys.exit("deidentify-manifest: the private map has an empty relabel table "
             "or token list; refusing to continue")


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
    shutil.copyfileobj(fi, fo)

print(f"rows: {n}")
print(f"deid gz bytes: {os.path.getsize(OUT + '.gz')}")
# Leak self-check on the de-identified output, in-process (no grep dependency).
rx = re.compile("|".join(re.escape(t) for t in SENSITIVE), re.IGNORECASE)
with open(OUT, encoding="utf-8") as fh:
    bad = sum(1 for line in fh if rx.search(line))
print(f"sensitive-token hits in deid manifest: {bad}")
if bad:
    sys.exit("deidentify-manifest: the de-identified output still carries "
             "sensitive tokens; do not publish it")
