"""Regenerate the archive-candidate manifest (PR #2146 rule) against a checkout.

Usage:
  python scripts/maintenance/archive_candidates.py <checkout> <pr2146_deleted_list.txt> <out_csv> <out_json>
      [--blob-map <out_blob_map_csv>] [--features <out_feature_inventory_json>]

Read-only: lists candidates, never moves, copies or deletes. See
docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md. The PR #2146 list is
`git diff --name-only --diff-filter=D 7e71d6b2 48ff7b64`.

Additions for owner decision M06 (2026-10-08):
- candidates referenced by any file under src/ or tests/ are dropped from the list;
- candidates are deduplicated by git-blob SHA-256 and mapped to a content-addressed
  store (`/mnt/ace/digitalmodel/blobs/sha256/<aa>/<sha256>.<ext>`, one copy per blob);
- every tracked model YAML (OrcaFlex native, OrcaWave, modular spec) outside src/ and
  tests/ is parsed into a normalised feature set; files carrying a feature no other
  file has are reported, with a greedy cover that keeps every feature reachable.
"""
import argparse
import collections
import csv
import datetime
import hashlib
import json
import os
import re
import subprocess
import sys
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor

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
CODE_PREFIXES = ("src/", "tests/")
ARCHIVE_ROOT = "/mnt/ace/digitalmodel"
CLASS = {}
for _e in ("dat", "lis", "qtf", "sim", "owr", "igs", "stl", "dwg", "dxf", "engd", "scdoc", "gz", "yml", "yaml", "csv"):
    CLASS[_e] = "solver-inputs"
CLASS["html"] = "html-report-renders"
for _e in ("png", "jpg", "jpeg", "jfif", "gif", "svg", "bmp", "tif", "tiff", "webp"):
    CLASS[_e] = "documentation-images"
for _e in ("pptx", "ppt", "docx", "doc", "pdf", "xlsx", "xls"):
    CLASS[_e] = "office-documents"


def ext(p):
    b = os.path.basename(p).lower()
    return b.rsplit(".", 1)[1] if "." in b else ""


# --- blob dedup and content-addressed store -----------------------------------

def dedup_blobs(rows):
    """Group rows by `sha256_blob`; one kept copy per unique content."""
    groups = collections.defaultdict(list)
    for r in rows:
        groups[r["sha256_blob"]].append(r)
    canonical = {sha: min(g, key=lambda r: r["path"])["path"] for sha, g in groups.items()}
    dup = []
    for sha, g in groups.items():
        if len(g) > 1:
            size = g[0]["blob_size_bytes"]
            dup.append({"sha256_blob": sha, "count": len(g), "blob_size_bytes": size,
                        "bytes_saved": size * (len(g) - 1),
                        "paths": sorted(r["path"] for r in g)})
    dup.sort(key=lambda d: (-d["bytes_saved"], d["sha256_blob"]))
    total = sum(r["blob_size_bytes"] for r in rows)
    unique = sum(g[0]["blob_size_bytes"] for g in groups.values())
    return {"files": len(rows), "unique_blobs": len(groups), "duplicate_groups": len(dup),
            "duplicate_files": sum(d["count"] - 1 for d in dup),
            "bytes_total": total, "bytes_unique": unique, "bytes_saved": total - unique,
            "canonical": canonical, "groups": dup}


def blob_store_path(sha, path):
    e = ext(path)
    return f"blobs/sha256/{sha[:2]}/{sha}" + (f".{e}" if e else "")


def blob_map(rows):
    """Every original repo path -> its single content-addressed blob (relative to ARCHIVE_ROOT)."""
    canonical = dedup_blobs(rows)["canonical"]
    out = []
    for r in sorted(rows, key=lambda r: r["path"]):
        sha = r["sha256_blob"]
        out.append({"path": r["path"], "sha256_blob": sha, "blob_size_bytes": r["blob_size_bytes"],
                    "blob_path": blob_store_path(sha, canonical[sha]),
                    "is_canonical_copy": canonical[sha] == r["path"]})
    return out


# --- src/ and tests/ reference exclusion --------------------------------------

MIN_DIR_DEPTH = 4  # a directory path is evidence only when it is this specific


def reference_patterns(path, basename_unique):
    """Fixed strings whose presence in src/ or tests/ marks `path` as referenced by code."""
    parts = path.split("/")
    pats = {path}
    if len(parts) >= 2:
        pats.add("/".join(parts[-2:]))
    if basename_unique:
        pats.add(parts[-1])
    for i in range(MIN_DIR_DEPTH, len(parts)):
        pats.add("/".join(parts[:i]))
    return pats


def referenced_by_code(path, hits, basename_unique):
    return bool(reference_patterns(path, basename_unique) & hits)


# --- model YAML feature inventory ---------------------------------------------

ORCAFLEX_SECTIONS = {
    "General", "Environment", "Lines", "LineTypes", "Vessels", "VesselTypes", "6DBuoys", "3DBuoys",
    "Shapes", "Winches", "Links", "Constraints", "Groups", "VariableData", "BaseFile", "ClumpTypes",
    "SupportTypes", "FrictionCoefficients", "FlexJointTypes", "StiffenerTypes", "WingTypes",
    "LineContactData", "P-yModels", "Turbines", "DragChainTypes", "FlexJoints", "LineContacts",
    "CodeChecks", "MorisonElementTypes", "Attachments", "ExpansionTables", "MultibodyGroups",
    "WakeModels", "SolidFrictionCoefficients", "BrowserGroups", "AttachmentTypes", "Pipes",
}
ORCAWAVE_KEYS = {"Bodies", "SolveType", "LoadRAOCalculationMethod", "PeriodOrFrequency", "WaveHeading"}
SPEC_KEYS = {"environment", "geometry", "lines", "vessels", "buoys", "simulation", "structure"}

ENUM_KEY = re.compile(
    r"[a-z](Model|Method|Calculation|Category|Variation|Formulation|Mode|Motion|Interpolation|"
    r"Strategy|Order|Profile|Behaviour)$|^(Category|Mode|UnitsSystem)$"
    r"|(^|_)(model|method|kind|solver|structure|operation|category)$|_type$")
ENUM_TYPE_KEYS = {"WaveType", "SeabedType", "WindType", "SolveType", "SpectrumType", "CurrentType",
                  "WaveSpectrumType", "BoundaryType", "ContactType", "DampingType", "StiffnessType",
                  "WaveKinematicsType"}
# keys whose value names another object (a reference), never an option
REFERENCE_KEYS = {"VesselType", "LineType", "ClumpType", "BuoyType", "WingType", "StiffenerType",
                  "FlexJointType", "SupportType", "DragChainType", "AttachmentType", "Connection",
                  "Draught", "Name", "name", "ShapeType", "TurbineType", "CoatingType", "LiningType",
                  "PyModel", "SupportCoordinateSystem", "Model", "vessel_type", "line_type",
                  "clump_type", "BaseFile", "IncludeFile"}
# lowercase `type` is an option only under these parents (elsewhere it names a line or vessel type)
TYPE_OPTION_PARENTS = {"waves", "wave", "wind", "current", "spectrum", "seabed"}
ENUM_VALUE = re.compile(r"^[A-Za-z][A-Za-z +\-/().,']{0,39}$")
# a key is property vocabulary when it is CamelCase / snake_case (no spaces, not ending in a digit),
# a comma list of such words, or an OrcaFlex section such as 6DBuoys / P-yModels
VOCAB_KEY = re.compile(
    r"^(?:\d?[A-Z][A-Za-z0-9]*[A-Za-z]|[a-z][a-z0-9]*(?:_[a-z0-9]+)*[a-z]|[A-Z]-[a-z][A-Za-z]*|[A-Za-z])$")
# maps whose keys are always object names
NAME_MAP_PARENTS = {"Structure", "State", "BrowserGroups"}
MAX_DEPTH = 5


def _vocab(k):
    return all(VOCAB_KEY.match(part.strip()) for part in k.split(","))


def model_yaml_kind(docs):
    keys = set()
    for d in docs:
        if isinstance(d, dict):
            keys |= {str(k) for k in d}
    if keys & ORCAFLEX_SECTIONS:
        return "orcaflex-native"
    if keys & ORCAWAVE_KEYS:
        return "orcawave"
    if "metadata" in keys and keys & SPEC_KEYS:
        return "spec"
    return None


def _is_enum_key(k, parent):
    if k in REFERENCE_KEYS:
        return False
    if k == "type":
        return parent in TYPE_OPTION_PARENTS
    if k.endswith("Type"):
        return k in ENUM_TYPE_KEYS
    return bool(ENUM_KEY.search(k))


def _name_keyed(d, parent):
    """A mapping whose keys are object names: any non-vocabulary key, or a known name map.
    All keys of such a mapping collapse to '*' so sibling names are not emitted either."""
    if parent in NAME_MAP_PARENTS:
        return True
    return any(not _vocab(str(k)) for k in d)


def _walk(node, path, out, depth):
    if depth > MAX_DEPTH:
        return
    if isinstance(node, dict):
        parent = path.rsplit("/", 1)[-1] if path else ""
        # top level holds sections: collapse only the non-vocabulary keys there
        collapse_all = bool(path) and _name_keyed(node, parent)
        for k, v in node.items():
            k = str(k).strip()
            collapse = collapse_all or not _vocab(k)
            seg = "*" if collapse else k
            p = f"{path}/{seg}" if path else seg
            if not path:
                out.add(f"section:{seg}")
            out.add(f"key:{p}")
            if (not collapse and isinstance(v, str) and _is_enum_key(k, parent)
                    and ENUM_VALUE.match(v.strip())):
                out.add(f"value:{p}={v.strip().lower()}")
            _walk(v, p, out, depth + 1)
    elif isinstance(node, list):
        for item in node:
            _walk(item, path, out, depth)


def extract_features(docs):
    """Normalised structural features: sections, key paths (list indices and object names
    collapsed) and enumerated option values. Object names and numbers are never features."""
    out = set()
    for d in docs:
        if isinstance(d, (dict, list)):
            _walk(d, "", out, 0)
    return out


def unique_features(file_features):
    owners = collections.defaultdict(list)
    for p, fs in file_features.items():
        for f in fs:
            owners[f].append(p)
    res = collections.defaultdict(list)
    for f, ps in owners.items():
        if len(ps) == 1:
            res[ps[0]].append(f)
    return {p: sorted(fs) for p, fs in sorted(res.items())}


def feature_cover(file_features):
    """Greedy set cover: a small set of files that together carry every feature.
    Ties break on path for a reproducible result."""
    remaining = set().union(*file_features.values()) if file_features else set()
    chosen = []
    # files with a unique feature are mandatory
    for p in unique_features(file_features):
        chosen.append(p)
        remaining -= file_features[p]
    while remaining:
        best = min(file_features, key=lambda p: (-len(file_features[p] & remaining), p))
        chosen.append(best)
        remaining -= file_features[best]
    return chosen


def candidate_keep_set(file_features, candidates):
    """Features carried only by archive candidates would leave the repository with them.
    Return (candidate files to keep as library inputs, those candidate-only features)."""
    staying = set().union(*(fs for p, fs in file_features.items() if p not in candidates)) \
        if any(p not in candidates for p in file_features) else set()
    cand_ff = {p: fs for p, fs in file_features.items() if p in candidates}
    only = set().union(*cand_ff.values()) - staying if cand_ff else set()
    keep = []
    remaining = set(only)
    while remaining:
        best = min(cand_ff, key=lambda p: (-len(cand_ff[p] & remaining), p))
        keep.append(best)
        remaining -= cand_ff[best]
    return sorted(keep), only


def _parse_features(args):
    repo, path = args
    import yaml
    loader = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
    try:
        with open(os.path.join(repo, path), encoding="utf-8") as f:
            docs = list(yaml.load_all(f, Loader=loader))
    except Exception as e:  # noqa: BLE001 - reported, never fatal
        return path, None, [], type(e).__name__
    kind = model_yaml_kind(docs)
    if kind is None:
        return path, None, [], None
    return path, kind, sorted(extract_features(docs)), None


# --- generator ----------------------------------------------------------------

def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("repo")
    ap.add_argument("pr_list")
    ap.add_argument("out_csv")
    ap.add_argument("out_json")
    ap.add_argument("--blob-map")
    ap.add_argument("--features")
    a = ap.parse_args(argv)
    REPO = a.repo

    def git(*args, inp=None):
        return subprocess.run(["git", "-c", "core.quotepath=false", "-C", REPO, *args],
                              input=inp, capture_output=True, check=True).stdout

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

    rule_cands = []
    for p in all_paths:
        if p.startswith(EXCLUDED_PREFIXES):
            continue
        e = ext(p)
        if e in ALWAYS or (e in SIZE_GATED and blob_size[p] > SIZE_GATE):
            rule_cands.append(p)
    print("rule candidates", len(rule_cands), file=sys.stderr)

    # drop anything referenced by src/ or tests/ (one git grep over code for every pattern)
    base_count = collections.Counter(os.path.basename(p) for p in all_paths)
    pats_by_path = {p: reference_patterns(p, base_count[os.path.basename(p)] == 1) for p in rule_cands}
    all_pats = sorted(set().union(*pats_by_path.values()))
    r = subprocess.run(["git", "-c", "core.quotepath=false", "-C", REPO, "grep", "-I", "-h", "-o", "-F",
                        "-f", "-", "--", *CODE_PREFIXES],
                       input=("\n".join(all_pats) + "\n").encode("utf-8"), capture_output=True)
    hits = {l.strip() for l in r.stdout.decode("utf-8", "replace").replace("\\", "/").splitlines()}
    hits &= set(all_pats)
    excluded = []
    for p in rule_cands:
        m = sorted(pats_by_path[p] & hits)
        if m:
            excluded.append({"path": p, "matched": m[0], "blob_size_bytes": blob_size[p]})
    excl_set = {x["path"] for x in excluded}
    cands = [p for p in rule_cands if p not in excl_set]
    cand_set = set(cands)
    print("excluded (src/tests refs)", len(excluded), "candidates", len(cands), file=sys.stderr)

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

    # references: git grep -l -F <basename> over tracked text files, one call per unique basename
    def refs(b):
        rr = subprocess.run(["git", "-c", "core.quotepath=false", "-C", REPO, "grep", "-l", "-I", "-F", "-z",
                             "-e", b], capture_output=True)
        return b, [x.decode("utf-8") for x in rr.stdout.split(b"\0") if x]

    with ThreadPoolExecutor(8) as ex:
        refmap = dict(ex.map(refs, sorted({os.path.basename(p) for p in cands})))

    pr = set(l.strip() for l in open(a.pr_list, encoding="utf-8") if l.strip())

    # model YAML feature inventory over every tracked YAML outside src/ and tests/
    yaml_paths = [p for p in all_paths if ext(p) in ("yml", "yaml") and not p.startswith(CODE_PREFIXES)]
    with ProcessPoolExecutor() as ex:
        parsed = list(ex.map(_parse_features, [(REPO, p) for p in yaml_paths], chunksize=16))
    kinds = {p: k for p, k, _, _ in parsed if k}
    file_features = {p: set(fs) for p, k, fs, _ in parsed if k}
    parse_fail = sorted(p for p, _, _, err in parsed if err)
    uniq = unique_features(file_features)
    cover = feature_cover(file_features)
    keep_set, cand_only = candidate_keep_set(file_features, cand_set)
    keep_paths = set(keep_set)
    print("model yaml", len(file_features), "with unique features", len(uniq),
          "candidate-only features", len(cand_only), "keep", len(keep_set), file=sys.stderr)

    rows = []
    for p in sorted(cands):
        with open(os.path.join(REPO, p), "rb") as f:
            data = f.read()
        blob = git("cat-file", "blob", blob_oid[p])
        rf = [x for x in refmap[os.path.basename(p)] if x != p]
        r_nc = [x for x in rf if x not in cand_set]
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
            "referenced": bool(rf),
            "ref_count": len(rf),
            "ref_count_noncandidate": len(r_nc),
            "first_ref_path": (r_nc or rf or [""])[0],
            "in_pr2146": p in pr,
            "model_yaml_kind": kinds.get(p, ""),
            "unique_features": len(uniq.get(p, [])),
        })

    dd = dedup_blobs(rows)
    group_of = {}
    for i, g in enumerate(dd["groups"], 1):
        for pp in g["paths"]:
            group_of[pp] = i
    for row in rows:
        row["dup_group"] = group_of.get(row["path"], "")
        row["blob_store_path"] = blob_store_path(row["sha256_blob"], dd["canonical"][row["sha256_blob"]])
        row["is_canonical_copy"] = dd["canonical"][row["sha256_blob"]] == row["path"]
        row["keep_for_feature"] = row["path"] in keep_paths

    with open(a.out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()), lineterminator="\n")
        w.writeheader()
        w.writerows(rows)

    if a.blob_map:
        with open(a.blob_map, "w", newline="", encoding="utf-8") as f:
            bm = blob_map(rows)
            w = csv.DictWriter(f, fieldnames=list(bm[0].keys()), lineterminator="\n")
            w.writeheader()
            w.writerows(bm)

    def agg(key, rs):
        d = collections.defaultdict(lambda: {"files": 0, "size_bytes": 0, "blob_size_bytes": 0})
        for row in rs:
            d[row[key]]["files"] += 1
            d[row[key]]["size_bytes"] += row["size_bytes"]
            d[row[key]]["blob_size_bytes"] += row["blob_size_bytes"]
        return dict(sorted(d.items(), key=lambda kv: -kv[1]["blob_size_bytes"]))

    move_rows = [row for row in rows if not row["keep_for_feature"]]
    move_dd = dedup_blobs(move_rows)
    head = git("rev-parse", "HEAD").decode().strip()
    pr_not_cand = sorted(pr - set(rule_cands))
    feat_owner_count = collections.Counter(f for fs in file_features.values() for f in fs)
    summary = {
        "generated": datetime.date.today().isoformat(),
        "base_commit": head,
        "rule": {
            "extensions_any_size": sorted(ALWAYS),
            "extensions_size_gated": sorted(SIZE_GATED),
            "size_gate_bytes_exclusive": SIZE_GATE,
            "excluded_prefixes": list(EXCLUDED_PREFIXES),
            "excluded_if_referenced_from": list(CODE_PREFIXES),
            "reference_evidence": ("full path, last two path components, the basename when it is unique "
                                   f"among tracked files, or any ancestor directory at depth >= {MIN_DIR_DEPTH}"),
            "scope": "git ls-files (tracked only); extension match case-insensitive; size gate on git blob size",
            "source": "recovered from PR #2146 (head 48ff7b64) against its merge base 7e71d6b2",
        },
        "lfs_tracked_files": len([x for x in git("lfs", "ls-files", "-n").decode().splitlines() if x]),
        "rule_matches": len(rule_cands),
        "excluded_referenced_by_src_or_tests": {
            "files": len(excluded), "blob_size_bytes": sum(x["blob_size_bytes"] for x in excluded),
            "paths": excluded},
        "totals": {"files": len(rows), "size_bytes": sum(row["size_bytes"] for row in rows),
                   "blob_size_bytes": sum(row["blob_size_bytes"] for row in rows)},
        "dedup": {k: dd[k] for k in ("files", "unique_blobs", "duplicate_groups", "duplicate_files",
                                     "bytes_total", "bytes_unique", "bytes_saved")},
        "dedup_top_groups": [{k: g[k] for k in ("sha256_blob", "count", "blob_size_bytes", "bytes_saved")}
                             | {"example_path": g["paths"][0]} for g in dd["groups"][:20]],
        "kept_for_feature": {"files": len(rows) - len(move_rows),
                                    "blob_size_bytes": sum(row["blob_size_bytes"] for row in rows)
                                    - sum(row["blob_size_bytes"] for row in move_rows)},
        "move_set_after_feature_keep": {k: move_dd[k] for k in ("files", "unique_blobs", "duplicate_groups",
                                                                 "duplicate_files", "bytes_total",
                                                                 "bytes_unique", "bytes_saved")},
        "archive_store": {"root": ARCHIVE_ROOT, "layout": "blobs/sha256/<first two hex>/<sha256>.<ext>",
                          "one_copy_per_unique_blob": True},
        "referenced": {"any": sum(row["referenced"] for row in rows),
                       "by_non_candidate": sum(row["ref_count_noncandidate"] > 0 for row in rows)},
        "by_top_level": agg("top_level", rows),
        "by_class": agg("class", rows),
        "model_yaml": {
            "yaml_files_scanned": len(yaml_paths), "parse_failures": len(parse_fail),
            "model_files": len(file_features),
            "by_kind": dict(collections.Counter(kinds.values())),
            "distinct_features": len(feat_owner_count),
            "unique_features": sum(len(v) for v in uniq.values()),
            "files_with_unique_features": len(uniq),
            "feature_cover_files": len(cover),
            "candidates_that_are_model_yaml": sum(bool(row["model_yaml_kind"]) for row in rows),
            "candidates_with_unique_features": sum(row["path"] in uniq for row in rows),
            "features_only_in_candidates": len(cand_only),
            "candidates_kept_for_feature": len(keep_set),
        },
        "comparison_with_pr2146": {
            "pr2146_deleted_paths": len(pr),
            "candidates_also_in_pr2146": sum(row["in_pr2146"] for row in rows),
            "candidates_new_since_pr2146": sum(not row["in_pr2146"] for row in rows),
            "pr2146_paths_not_rule_matches": len(pr_not_cand),
            "pr2146_not_candidates_breakdown": {
                "under_src_or_tests": sum(p.startswith(CODE_PREFIXES) for p in pr_not_cand),
                "absent_from_current_main": sum(p not in blob_size for p in pr_not_cand),
            },
        },
    }
    with open(a.out_json, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)
        f.write("\n")

    if a.features:
        inv = {
            "generated": summary["generated"],
            "base_commit": head,
            "definition": {
                "scope": "every tracked *.yml/*.yaml outside src/ and tests/ that parses as an OrcaFlex "
                         "native model, an OrcaWave model or a modular spec",
                "feature_kinds": {
                    "section": "top-level section (object type collection, e.g. Lines, VesselTypes, environment)",
                    "key": "key path with list indices collapsed and object-name maps collapsed to '*'",
                    "value": "enumerated option value of a solver setting (e.g. wave type, seabed model)",
                },
                "excluded": "object names, cross-references to named objects, free text and numbers",
            },
            "summary": summary["model_yaml"],
            "parse_failures": parse_fail,
            "feature_file_counts": dict(sorted(feat_owner_count.items())),
            "files_with_unique_features": {p: {"kind": kinds[p], "is_archive_candidate": p in cand_set,
                                               "unique_features": fs} for p, fs in uniq.items()},
            "feature_cover": sorted(cover),
            "features_only_in_candidates": sorted(cand_only),
            "candidates_kept_for_feature": keep_set,
        }
        with open(a.features, "w", encoding="utf-8") as f:
            json.dump(inv, f, indent=1)
            f.write("\n")
    print(json.dumps(summary["totals"]), json.dumps(summary["dedup"]), file=sys.stderr)


if __name__ == "__main__":
    main()
