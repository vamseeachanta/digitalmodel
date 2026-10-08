"""Regenerate the archive-candidate manifest (PR #2146 rule) against a checkout.

Usage:
  python scripts/maintenance/archive_candidates.py <checkout> <pr2146_deleted_list.txt> <out_csv> <out_json>
      [--blob-map <out_blob_map_csv>] [--features <out_feature_inventory_json>]

Read-only: lists candidates, never moves, copies or deletes. See
docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md. The PR #2146 list is
`git diff --name-only --diff-filter=D 7e71d6b2 48ff7b64`.

Additions for owner decision M06 (2026-10-08):
- candidates that src/ or tests/ explicitly read are dropped from the list (explicit
  path, exact filename, or a resolvable glob pattern; narrowed by owner decision C02 so
  a bare directory mention no longer excludes everything under it);
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


# --- src/ and tests/ reference exclusion (owner decision C02) -----------------
#
# A candidate is excluded only on explicit evidence that code reads that file:
#   path     - its repo path appears in src/ or tests/ text, or a Python path expression
#              (literal, Path(__file__).parents[n] / "...", os.path.join) resolves to it;
#   filename - its exact filename appears as a whole token in src/ or tests/ text;
#   glob     - a glob / rglob / iterdir / listdir / walk pattern in code, anchored on a
#              resolvable repo directory, matches it (only the matching files).
# A bare directory mention is not evidence for anything under it.
#
# Owner decision D01 (2026-10-08) narrows two forms further:
#   - a pattern in a hygiene sweep (HYGIENE_SCANS) is not evidence: such a sweep scans
#     whatever exists for a defect and consumes nothing; its explicit paths still count;
#   - a filename shared by more than one tracked path is ambiguous: it counts only when
#     path-qualified, i.e. a trailing path of two or more segments that names exactly
#     the files it matches (a full repo path is the limiting case).

# source file -> reason its glob/rglob patterns are not consumer evidence
HYGIENE_SCANS = {
    "tests/legal/test_published_pages_have_no_internal_paths.py":
        "DOCS_ROOT.rglob('*.html') sweeps every published page for absolute paths; "
        "pages it names explicitly still count",
}

GLOB_CHARS = set("*?[")
_PATH_CLASSES = {"Path", "PurePath", "PosixPath", "PurePosixPath", "WindowsPath", "PureWindowsPath"}
_IDENTITY_FUNCS = {"str", "fspath", "abspath", "realpath", "normpath", "expanduser"}
_IDENTITY_METHODS = {"resolve", "absolute", "expanduser", "as_posix"}
_MAX_VALUES = 16


def glob_match(pattern, path):
    """pathlib-style glob over '/' segments: '*' stays within a segment, '**' spans any."""
    import fnmatch
    pp, sp = pattern.split("/"), path.split("/")

    def m(i, j):
        if i == len(pp):
            return j == len(sp)
        if pp[i] == "**":
            return any(m(i + 1, k) for k in range(j, len(sp) + 1))
        return j < len(sp) and fnmatch.fnmatchcase(sp[j], pp[i]) and m(i + 1, j + 1)
    return m(0, 0)


def _norm(p):
    out = []
    for s in p.replace("\\", "/").split("/"):
        if s in ("", "."):
            continue
        if s == "..":
            if not out:
                return None          # leaves the repository
            out.pop()
        else:
            out.append(s)
    return "/".join(out)


class _Repo:
    def __init__(self, tracked):
        self.top = {p.split("/")[0] for p in tracked}
        self.dirs = {""}
        for p in tracked:
            parts = p.split("/")
            for i in range(1, len(parts)):
                self.dirs.add("/".join(parts[:i]))

    def anchor(self, s):
        """Repo-relative form of a path string, or None when it is not provably in the repo."""
        s = s.replace("\\", "/")
        parts = [x for x in s.split("/") if x not in ("", ".")]
        if not parts:
            return None
        absolute = s.startswith("/") or (len(parts[0]) == 2 and parts[0][1] == ":")
        if not absolute:
            return _norm(s) if parts[0] in self.top else None
        for i in range(len(parts)):  # absolute path to some other checkout of this repo
            if parts[i] in self.top and (len(parts) == i + 1 or "/".join(parts[i:i + 2]) in self.dirs
                                         or _norm("/".join(parts[i:])) in self.dirs):
                return _norm("/".join(parts[i:]))
        return None


def _module_file(dotted, level, cur, files):
    if level:
        base = cur.split("/")[:-level]
        dotted_path = "/".join(base + ([*dotted.split(".")] if dotted else []))
        cands = [dotted_path + ".py", dotted_path + "/__init__.py"]
    else:
        d = dotted.replace(".", "/")
        cands = [d + ".py", d + "/__init__.py", "src/" + d + ".py", "src/" + d + "/__init__.py"]
    return next((c for c in cands if c in files), None)


class _PyRefs:
    """Static evaluation of path expressions in one Python module."""

    def __init__(self, path, tree, ctx):
        import ast
        self.ast, self.path, self.ctx = ast, path, ctx
        self.env = collections.defaultdict(list)
        self.imports = {}
        for n in ast.walk(tree):
            if isinstance(n, ast.Assign):
                for t in n.targets:
                    if isinstance(t, ast.Name):
                        self.env[t.id].append(n.value)
            elif isinstance(n, ast.AnnAssign) and isinstance(n.target, ast.Name) and n.value is not None:
                self.env[n.target.id].append(n.value)
            elif isinstance(n, ast.ImportFrom):
                mf = _module_file(n.module or "", n.level, path, ctx["py"])
                if mf:
                    for a in n.names:
                        self.imports[a.asname or a.name] = (mf, a.name)
        self.tree = tree

    # values are ("p", repo_rel) for paths rooted at __file__, ("s", text) otherwise
    def to_path(self, v):
        return v[1] if v[0] == "p" else self.ctx["repo"].anchor(v[1])

    def _join(self, a, b):
        if b[0] == "p":
            return b
        s = b[1].replace("\\", "/")
        if s.startswith("/") or (len(s) > 1 and s[1] == ":"):
            return ("s", s)
        if a[0] == "p":
            r = _norm(a[1] + "/" + s)
            return ("p", r) if r is not None else None
        return ("s", (a[1].rstrip("/") + "/" + s) if a[1] else s)

    def _parent(self, v, n=1):
        for _ in range(n):
            if v[0] == "p":
                if v[1] == "":
                    return None
                v = ("p", v[1].rsplit("/", 1)[0] if "/" in v[1] else "")
            else:
                v = ("s", v[1].rstrip("/").rsplit("/", 1)[0] if "/" in v[1] else "")
        return v

    def ev(self, node, depth=0, seen=frozenset()):
        ast = self.ast
        if depth > 25:
            return set()
        r = set()
        e = lambda x: self.ev(x, depth + 1, seen)  # noqa: E731
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            r.add(("s", node.value))
        elif isinstance(node, ast.JoinedStr):
            if all(isinstance(v, ast.Constant) for v in node.values):
                r.add(("s", "".join(str(v.value) for v in node.values)))
        elif isinstance(node, ast.Name):
            if node.id == "__file__":
                r.add(("p", self.path))
            elif node.id not in seen:
                for x in self.env.get(node.id, []):
                    r |= self.ev(x, depth + 1, seen | {node.id})
                if not r and node.id in self.imports:
                    mf, name = self.imports[node.id]
                    other = self.ctx["module"](mf)
                    if other is not None and other is not self:
                        r |= {v for x in other.env.get(name, []) for v in other.ev(x, depth + 1)}
        elif isinstance(node, ast.BinOp) and isinstance(node.op, ast.Div):
            for a in e(node.left):
                for b in e(node.right):
                    j = self._join(a, b)
                    if j:
                        r.add(j)
        elif isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add):
            for a in e(node.left):
                for b in e(node.right):
                    if a[0] == "s" and b[0] == "s":
                        r.add(("s", a[1] + b[1]))
        elif isinstance(node, ast.Attribute) and node.attr == "parent":
            r |= {p for v in e(node.value) if (p := self._parent(v))}
        elif (isinstance(node, ast.Subscript) and isinstance(node.value, ast.Attribute)
              and node.value.attr == "parents" and isinstance(node.slice, ast.Constant)
              and isinstance(node.slice.value, int)):
            r |= {p for v in e(node.value.value) if (p := self._parent(v, node.slice.value + 1))}
        elif isinstance(node, ast.Call):
            f = node.func
            fname = f.id if isinstance(f, ast.Name) else f.attr if isinstance(f, ast.Attribute) else None
            recv_is_module = isinstance(f, ast.Attribute) and isinstance(f.value, (ast.Name, ast.Attribute))
            if fname in _PATH_CLASSES or (fname == "join" and recv_is_module and self._is_ospath(f.value)):
                if not node.args:
                    r.add(("p", ""))            # Path() is the working directory: the repo root
                else:
                    vals = e(node.args[0])
                    for arg in node.args[1:]:
                        av = e(arg)
                        vals = {j for a in vals for b in av if (j := self._join(a, b))}
                    r |= vals
            elif fname == "cwd" and isinstance(f, ast.Attribute):
                r.add(("p", ""))
            elif fname == "dirname" and node.args:
                r |= {p for v in e(node.args[0]) if (p := self._parent(v))}
            elif fname in _IDENTITY_FUNCS and node.args and (
                    not isinstance(f, ast.Attribute) or fname == "fspath" or self._is_ospath(f.value)):
                r |= e(node.args[0])
            elif isinstance(f, ast.Attribute) and fname in _IDENTITY_METHODS:
                r |= e(f.value)
            elif isinstance(f, ast.Attribute) and fname == "joinpath":
                vals = e(f.value)
                for arg in node.args:
                    av = e(arg)
                    vals = {j for a in vals for b in av if (j := self._join(a, b))}
                r |= vals
        if len(r) > _MAX_VALUES:
            r = set(sorted(r)[:_MAX_VALUES])
        return r

    def _is_ospath(self, node):
        ast = self.ast
        if isinstance(node, ast.Attribute):
            return node.attr == "path"          # os.path
        return isinstance(node, ast.Name) and node.id in ("path", "posixpath", "ntpath", "osp")

    def scan(self):
        """Yield ("path", repo_rel) for resolved file paths and ("glob", pattern) for read patterns."""
        ast = self.ast
        for n in ast.walk(self.tree):
            vals = None
            if isinstance(n, ast.Call):
                f = n.func
                fname = f.id if isinstance(f, ast.Name) else f.attr if isinstance(f, ast.Attribute) else None
                if isinstance(f, ast.Attribute) and fname in ("glob", "rglob", "iterdir") and not (
                        isinstance(f.value, ast.Name) and f.value.id == "glob"):
                    pats = {("s", "*")} if fname == "iterdir" else (self.ev(n.args[0]) if n.args else set())
                    for rv in self.ev(f.value):
                        base = self.to_path(rv)
                        if base is None:
                            continue
                        for pv in pats:
                            if pv[0] != "s" or not pv[1]:
                                continue
                            mid = "/**/" if fname == "rglob" else "/"
                            yield "glob", (base + mid + pv[1]).lstrip("/")
                    continue
                recursive = any(k.arg == "recursive" and isinstance(k.value, ast.Constant) and k.value.value
                                for k in n.keywords)
                if fname in ("glob", "iglob") and n.args:
                    for v in self.ev(n.args[0]):
                        p = self.to_path(v)
                        if p is not None and GLOB_CHARS & set(p):
                            yield "glob", p if recursive else re.sub(r"(^|/)\*\*(?=/|$)", r"\1*", p)
                    continue
                if fname in ("listdir", "scandir", "walk") and n.args:
                    for v in self.ev(n.args[0]):
                        p = self.to_path(v)
                        if p is not None:
                            yield "glob", (p + ("/**" if fname == "walk" else "/*")).lstrip("/")
                    continue
                if fname in _PATH_CLASSES or fname == "join" or fname == "joinpath":
                    vals = self.ev(n)
            elif isinstance(n, ast.BinOp) and isinstance(n.op, ast.Div):
                vals = self.ev(n)
            elif isinstance(n, ast.Constant) and isinstance(n.value, str) and len(n.value) < 512:
                vals = {("s", n.value)}
            for v in vals or ():
                p = self.to_path(v)
                if p is None:
                    continue
                yield ("glob" if GLOB_CHARS & set(p) else "path"), p


_TEXT_TOKEN = re.compile(r"[^\s'\"`,;()<>{}|=]+")


def explicit_references(code_texts, candidates, tracked):
    """Map each candidate that code in `code_texts` ({repo path: text}, src/ and tests/)
    explicitly reads to {"evidence", "matched", "source"}. See the rule above."""
    import ast
    repo = _Repo(tracked)
    cand_set = set(candidates)
    by_base = collections.defaultdict(list)
    for p in candidates:
        by_base[os.path.basename(p)].append(p)
    found = {}
    rank = {"path": 0, "filename": 1, "glob": 2}

    def add(p, ev, matched, src):
        cur = found.get(p)
        if cur is None or (rank[ev], src) < (rank[cur["evidence"]], cur["source"]):
            found[p] = {"evidence": ev, "matched": matched, "source": src}

    names = sorted(by_base, key=len, reverse=True)
    name_re = re.compile(r"(?<![A-Za-z0-9_.\-])(" + "|".join(map(re.escape, names)) + r")(?![A-Za-z0-9_\-])") \
        if names else None

    py_files = {p for p in code_texts if p.endswith(".py")}
    modules = {}
    ctx = {"repo": repo, "py": py_files}

    def module(p):
        if p not in modules:
            modules[p] = None
            try:
                modules[p] = _PyRefs(p, ast.parse(code_texts[p]), ctx)
            except (SyntaxError, ValueError, RecursionError):
                pass
        return modules[p]
    ctx["module"] = module

    tracked_base_count = collections.Counter(os.path.basename(p) for p in tracked)

    def qualified(p, text):
        """Shortest trailing path of p (two or more segments) that appears in text as a
        whole token and that no other tracked file ends with; None if there is none."""
        parts = p.split("/")
        for k in range(2, len(parts) + 1):
            suf = "/".join(parts[-k:])
            if re.search(r"(?<![A-Za-z0-9_.\-])" + re.escape(suf) + r"(?![A-Za-z0-9_\-])", text) \
                    and sum(t == suf or t.endswith("/" + suf) for t in tracked) == 1:
                return suf
        return None

    patterns = []
    for src in sorted(code_texts):
        text = code_texts[src].replace("\\\\", "/").replace("\\", "/")
        if name_re is not None:
            for b in set(name_re.findall(text)):
                shared = tracked_base_count[b] > 1
                for p in by_base[b]:
                    if p in text:
                        add(p, "path", p, src)
                    elif not shared:
                        add(p, "filename", b, src)
                    else:
                        suf = qualified(p, text)
                        if suf:
                            add(p, "path", suf, src)
        if src in py_files:
            mod = module(src)
            if mod is not None:
                try:
                    for kind, p in mod.scan():
                        if kind == "path" and p in cand_set:
                            add(p, "path", p, src)
                        elif kind == "glob" and src not in HYGIENE_SCANS:
                            patterns.append((p, src))
                except RecursionError:
                    pass
        else:
            for tok in _TEXT_TOKEN.findall(text):
                if GLOB_CHARS & set(tok) and "/" in tok and src not in HYGIENE_SCANS:
                    p = repo.anchor(tok)
                    if p is not None:
                        patterns.append((p, src))
    for pat, src in sorted(set(patterns)):
        prefix = []
        for seg in pat.split("/"):
            if GLOB_CHARS & set(seg) or seg == "**":
                break
            prefix.append(seg)
        pre = "/".join(prefix)
        for p in candidates:
            if (not pre or p.startswith(pre + "/")) and glob_match(pat, p):
                add(p, "glob", pat, src)
    return found


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

    # drop candidates that src/ or tests/ explicitly read (owner decision C02)
    base_count = collections.Counter(os.path.basename(p) for p in all_paths)
    code_files = [x.decode("utf-8") for x in git("grep", "-I", "-l", "-z", "-e", "", "--", *CODE_PREFIXES)
                  .split(b"\0") if x]
    code_texts = {}
    for cf in code_files:
        with open(os.path.join(REPO, cf), "rb") as f:
            code_texts[cf] = f.read().decode("utf-8", "replace")
    refs_found = explicit_references(code_texts, rule_cands, all_paths)
    excluded = []
    for p in rule_cands:
        if p in refs_found:
            x = refs_found[p]
            excluded.append({"path": p, "evidence": x["evidence"], "matched": x["matched"],
                             "source": x["source"], "blob_size_bytes": blob_size[p],
                             "basename_unique": base_count[os.path.basename(p)] == 1})
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
            "reference_evidence": (
                "explicit file references only (owner decision C02, 2026-10-08): the file's repo path "
                "appears in src/ or tests/ text or a Python path expression resolves to it; its exact "
                "filename appears as a whole token; or a glob/rglob/iterdir/listdir/walk pattern anchored "
                "on a resolvable repo directory matches it. A bare directory mention excludes nothing."),
            "scope": "git ls-files (tracked only); extension match case-insensitive; size gate on git blob size",
            "source": "recovered from PR #2146 (head 48ff7b64) against its merge base 7e71d6b2",
        },
        "lfs_tracked_files": len([x for x in git("lfs", "ls-files", "-n").decode().splitlines() if x]),
        "rule_matches": len(rule_cands),
        "excluded_referenced_by_src_or_tests": {
            "files": len(excluded), "blob_size_bytes": sum(x["blob_size_bytes"] for x in excluded),
            "by_evidence": {ev: {"files": sum(x["evidence"] == ev for x in excluded),
                                 "blob_size_bytes": sum(x["blob_size_bytes"] for x in excluded
                                                        if x["evidence"] == ev)}
                            for ev in ("path", "filename", "glob")},
            "filename_only_with_non_unique_basename": {
                "files": sum(x["evidence"] == "filename" and not x["basename_unique"] for x in excluded),
                "blob_size_bytes": sum(x["blob_size_bytes"] for x in excluded
                                       if x["evidence"] == "filename" and not x["basename_unique"])},
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
