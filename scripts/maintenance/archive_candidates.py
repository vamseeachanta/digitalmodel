"""Regenerate the archive-candidate manifest (PR #2146 rule) against a checkout.

Usage: python scripts/maintenance/archive_candidates.py <checkout> <pr2146_deleted_list.txt> <out_csv> <out_json>

Read-only: lists candidates, never moves or deletes. See docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md.
The PR #2146 list is `git diff --name-only --diff-filter=D 7e71d6b2 48ff7b64`.
"""
import csv, hashlib, json, os, subprocess, sys, collections
from concurrent.futures import ThreadPoolExecutor

REPO, PR_LIST, OUT_CSV, OUT_JSON = sys.argv[1:5]

ALWAYS = {
    # solver inputs / results / CAD-mesh
    "dat", "lis", "qtf", "sim", "owr", "igs", "stl", "dwg", "dxf", "engd", "scdoc", "gz",
    # report renders
    "html",
    # images
    "png", "jpg", "jpeg", "jfif", "gif", "svg", "bmp", "tif", "tiff", "webp",
    # office documents
    "pptx", "ppt", "docx", "doc", "pdf", "xlsx", "xls",
}
SIZE_GATED = {"yml", "yaml", "csv"}  # only when blob > 1,000,000 bytes
SIZE_GATE = 1_000_000
EXCLUDED_PREFIXES = ("src/", "tests/", "docs/api/", "assets/logo/")
CLASS = {}
for e in ("dat", "lis", "qtf", "sim", "owr", "igs", "stl", "dwg", "dxf", "engd", "scdoc", "gz", "yml", "yaml", "csv"):
    CLASS[e] = "solver-inputs"
CLASS["html"] = "html-report-renders"
for e in ("png", "jpg", "jpeg", "jfif", "gif", "svg", "bmp", "tif", "tiff", "webp"):
    CLASS[e] = "documentation-images"
for e in ("pptx", "ppt", "docx", "doc", "pdf", "xlsx", "xls"):
    CLASS[e] = "office-documents"


def git(*a, inp=None):
    return subprocess.run(["git", "-c", "core.quotepath=false", "-C", REPO, *a],
                          input=inp, capture_output=True, check=True).stdout


def ext(p):
    b = os.path.basename(p).lower()
    return b.rsplit(".", 1)[1] if "." in b else ""


# tracked files with blob ids
entries = []
for rec in git("ls-files", "-s", "-z").split(b"\0"):
    if not rec:
        continue
    meta, path = rec.split(b"\t", 1)
    mode, oid, _stage = meta.split()
    if mode == b"160000":
        continue
    entries.append((path.decode("utf-8"), oid.decode()))
oids = "\n".join(o for _, o in entries).encode() + b"\n"
sizes = [int(l.split()[2]) for l in git("cat-file", "--batch-check", inp=oids).decode().splitlines()]
blob_size = {p: s for (p, _), s in zip(entries, sizes)}
blob_oid = dict(entries)
all_paths = [p for p, _ in entries]

cands = []
for p in all_paths:
    if p.startswith(EXCLUDED_PREFIXES):
        continue
    e = ext(p)
    if e in ALWAYS or (e in SIZE_GATED and blob_size[p] > SIZE_GATE):
        cands.append(p)
cand_set = set(cands)
print("candidates", len(cands), file=sys.stderr)

# last commit date, one history pass
need = set(cands)
last = {}
proc = subprocess.Popen(["git", "-c", "core.quotepath=false", "-C", REPO, "log", "--format=@@%cs",
                         "--name-only", "--no-renames", "HEAD"], stdout=subprocess.PIPE)
cur = None
for raw in proc.stdout:
    line = raw.decode("utf-8", "replace").rstrip("\r\n")
    if line.startswith("@@"):
        cur = line[2:]
    elif line and line in need:
        last[line] = cur
        need.discard(line)
        if not need:
            break
proc.kill()
print("dates", len(last), file=sys.stderr)

# references: git grep -l -F <basename> over tracked text files, one call per unique basename
bases = sorted({os.path.basename(p) for p in cands})


def refs(b):
    r = subprocess.run(["git", "-c", "core.quotepath=false", "-C", REPO, "grep", "-l", "-I", "-F", "-z", "-e", b],
                       capture_output=True)
    return b, [x.decode("utf-8") for x in r.stdout.split(b"\0") if x]


with ThreadPoolExecutor(8) as ex:
    refmap = dict(ex.map(refs, bases))
print("refs done", file=sys.stderr)

pr = set(l.strip() for l in open(PR_LIST, encoding="utf-8") if l.strip())

rows = []
for p in sorted(cands):
    fp = os.path.join(REPO, p)
    with open(fp, "rb") as f:
        data = f.read()
    blob = git("cat-file", "blob", blob_oid[p])
    r = [x for x in refmap[os.path.basename(p)] if x != p]
    r_nc = [x for x in r if x not in cand_set]
    rows.append({
        "path": p,
        "top_level": p.split("/")[0] if "/" in p else "(root)",
        "ext": ext(p),
        "class": CLASS[ext(p)],
        "size_bytes": len(data),
        "blob_size_bytes": blob_size[p],
        "sha256": hashlib.sha256(data).hexdigest(),
        "sha256_blob": hashlib.sha256(blob).hexdigest(),
        "last_commit_date": last.get(p, ""),
        "referenced": bool(r),
        "ref_count": len(r),
        "ref_count_noncandidate": len(r_nc),
        "first_ref_path": (r_nc or r or [""])[0],
        "in_pr2146": p in pr,
    })

with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()), lineterminator="\n")
    w.writeheader()
    w.writerows(rows)


def agg(key):
    d = collections.defaultdict(lambda: {"files": 0, "size_bytes": 0, "blob_size_bytes": 0})
    for r in rows:
        d[r[key]]["files"] += 1
        d[r[key]]["size_bytes"] += r["size_bytes"]
        d[r[key]]["blob_size_bytes"] += r["blob_size_bytes"]
    return dict(sorted(d.items(), key=lambda kv: -kv[1]["blob_size_bytes"]))


head = git("rev-parse", "HEAD").decode().strip()
pr_not_cand = sorted(pr - cand_set)
summary = {
    "generated": __import__("datetime").date.today().isoformat(),
    "base_commit": head,
    "rule": {
        "extensions_any_size": sorted(ALWAYS),
        "extensions_size_gated": sorted(SIZE_GATED),
        "size_gate_bytes_exclusive": SIZE_GATE,
        "excluded_prefixes": list(EXCLUDED_PREFIXES),
        "scope": "git ls-files (tracked only); extension match case-insensitive; size gate on git blob size",
        "source": "recovered from PR #2146 (head 48ff7b64) against its merge base 7e71d6b2",
    },
    "lfs_tracked_files": len([x for x in git("lfs", "ls-files", "-n").decode().splitlines() if x]),
    "totals": {"files": len(rows), "size_bytes": sum(r["size_bytes"] for r in rows),
               "blob_size_bytes": sum(r["blob_size_bytes"] for r in rows)},
    "referenced": {"any": sum(r["referenced"] for r in rows),
                   "by_non_candidate": sum(r["ref_count_noncandidate"] > 0 for r in rows)},
    "by_top_level": agg("top_level"),
    "by_class": agg("class"),
    "comparison_with_pr2146": {
        "pr2146_deleted_paths": len(pr),
        "candidates_also_in_pr2146": sum(r["in_pr2146"] for r in rows),
        "candidates_new_since_pr2146": sum(not r["in_pr2146"] for r in rows),
        "pr2146_paths_not_candidates": len(pr_not_cand),
        "pr2146_not_candidates_breakdown": {
            "under_src_or_tests": sum(p.startswith(("src/", "tests/")) for p in pr_not_cand),
            "absent_from_current_main": sum(p not in blob_size for p in pr_not_cand),
        },
    },
}
with open(OUT_JSON, "w", encoding="utf-8") as f:
    json.dump(summary, f, indent=2)
    f.write("\n")
print(json.dumps(summary["totals"]), file=sys.stderr)
