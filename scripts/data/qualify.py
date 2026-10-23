# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report content-trim dependencies without executing consumers or removing files.

Method: issue #2554 retention requirements; Python 3.11 ast node semantics;
QGIS 3.40 User Guide, 26.1.1 (QGZ contains QGS XML). This is a bounded static
inspection, not proof that unreferenced content can be removed.
"""
import argparse
import ast
import fnmatch
import json
from pathlib import Path, PurePosixPath
import posixpath
import re
import stat
import xml.etree.ElementTree as ET
import zipfile


# Repository format policy, not numerical engineering acceptance criteria.
EXECUTABLE = {".py", ".sh", ".bat", ".cmd", ".ps1", ".lsp", ".js", ".m"}
CONFIGURATION = {".yaml", ".yml", ".json", ".toml", ".ini", ".cfg", ".qgs", ".qgz"}
TEXT_CONSUMERS = EXECUTABLE | CONFIGURATION | {".md", ".txt", ".rst", ".xml"}
WRITES = {"to_csv", "to_json", "to_excel", "write_text", "write_bytes", "savefig"}
READS = {"read_csv", "read_json", "read_excel", "read_text", "read_bytes", "LoadData"}


def _path(root, name):
    path = PurePosixPath(name)
    if (not path.parts or path.is_absolute() or ".." in path.parts
            or "\\" in name or ":" in name or str(path) != name
            or any(part.rstrip(" .").casefold() == ".git" for part in path.parts)):
        raise ValueError("Expected a relative non-metadata path")
    target = root / name
    target.resolve().relative_to(root.resolve())
    return target


def classify(path):
    """Protect code and configuration before considering its documentation path."""
    suffix = path.suffix.lower()
    if suffix in EXECUTABLE or path.stat().st_mode & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH):
        return "executable"
    with path.open("rb") as stream:
        if stream.read(len(b"#!")) == b"#!":
            return "executable"
    if suffix in CONFIGURATION:
        return "configuration"
    return "documentation" if "docs" in path.parts else "data"


def _pattern(value):
    value = value.replace("\\", "/").split("|", maxsplit=1)[0]
    return re.sub(r"\$\{[^}]+\}|\{[^}]+\}|\$\w+|%\w+%", "*", value)


def _pathlike(value):
    return ("/" in value or "\\" in value
            or bool(re.search(r"\.[A-Za-z][A-Za-z0-9]*$", value)))


def _matches(candidate, reference, consumer):
    reference = _pattern(reference)
    if reference.startswith("/") or ":" in reference:
        return False
    for base in (str(PurePosixPath(consumer).parent), "."):
        pattern = posixpath.normpath(posixpath.join(base, reference))
        if pattern.startswith("../"):
            continue
        if (pattern == "." or fnmatch.fnmatchcase(candidate, pattern)
                or candidate.startswith(pattern.rstrip("/") + "/")
                or fnmatch.fnmatchcase(str(PurePosixPath(candidate).with_suffix("")), pattern)):
            return True
    return False


def _value(node, bindings):
    """Resolve literal aliases and path construction; unknown values stay unknown."""
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    if isinstance(node, ast.Name):
        return bindings.get(node.id)
    if isinstance(node, ast.JoinedStr):
        return "".join(part.value if isinstance(part, ast.Constant) else "*"
                       for part in node.values)
    if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Div)):
        left, right = _value(node.left, bindings), _value(node.right, bindings)
        if left is not None and right is not None:
            return left + ("/" if isinstance(node.op, ast.Div) else "") + right
    if isinstance(node, ast.Call):
        if isinstance(node.func, ast.Name) and node.func.id in {"Path", "str"} and node.args:
            return _value(node.args[0], bindings)
        if isinstance(node.func, ast.Attribute) and node.func.attr == "format":
            return _value(node.func.value, bindings)
    return None


def _python_references(text):
    tree = ast.parse(text)
    references, unresolved, bindings = [], [], {}
    # Traversal of branches, loops and function bodies is deliberately not a
    # flow proof. Keep otherwise unreferenced candidates when such flow exists.
    if any(isinstance(node, (ast.If, ast.For, ast.While, ast.FunctionDef,
                             ast.AsyncFunctionDef, ast.Try, ast.With, ast.Match))
           for node in ast.walk(tree)):
        unresolved.append("Python control flow requires review")
    for node in sorted(ast.walk(tree), key=lambda item: getattr(item, "lineno", 0)):
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name):
                    bindings[target.id] = _value(node.value, bindings)
        if not isinstance(node, ast.Call):
            continue
        name = node.func.attr if isinstance(node.func, ast.Attribute) else getattr(node.func, "id", "")
        if name in {"Path", "str", "format"}:
            continue
        if name in {"read_text", "read_bytes", "write_text", "write_bytes"}:
            arguments = [node.func.value]
        else:
            arguments = node.args + [kw.value for kw in node.keywords
                                     if kw.arg in {"path", "path_or_buf", "filepath_or_buffer"}]
        role = "output" if name in WRITES else "read" if name in READS else "input"
        if name == "open":
            mode = _value(node.args[1], bindings) if len(node.args) > 1 else "r"
            mode = next((_value(kw.value, bindings) for kw in node.keywords if kw.arg == "mode"), mode)
            # Updating, appending or exclusive creation may consume existing
            # bytes; only an explicit truncating write is output-only.
            role = "output" if mode in {"w", "wb", "wt"} else "read"
            arguments = node.args[:1] + [kw.value for kw in node.keywords if kw.arg == "file"]
        if name in READS | WRITES | {"open"} and not arguments:
            unresolved.append("Python dependency has no resolved path")
        for argument in arguments:
            value = _value(argument, bindings)
            if value is None:
                unresolved.append("Python call has an unresolved argument")
            elif _pathlike(value) or value in {".", ".."} or name in READS | WRITES | {"open"}:
                references.append((value, role))
        if not arguments and name not in READS | WRITES | {"open"}:
            unresolved.append("Python call has no classified dependency arguments")
    return references, unresolved


def _references(path):
    if path.suffix.lower() == ".qgz":
        # Read archive members in memory; never extract member paths to disk.
        with zipfile.ZipFile(path) as archive:
            projects = [name for name in archive.namelist() if name.lower().endswith(".qgs")]
            if not projects:
                return [], ["Compressed GIS container has no QGS member"]
            texts = [archive.read(name) for name in projects]
        return [(element.text.strip(), "input") for text in texts
                for element in ET.fromstring(text).iter("datasource")
                if element.text and element.text.strip()], []
    text = path.read_text(encoding="utf-8-sig")
    if path.suffix.lower() == ".py":
        return _python_references(text)
    if path.suffix.lower() == ".qgs":
        return [(element.text.strip(), "input") for element in ET.fromstring(text).iter("datasource")
                if element.text and element.text.strip()], []
    # Preserve quoted paths (including spaces) and unquoted config/shell tokens.
    tokens = re.findall(r'''"([^"\n]+)"|'([^'\n]+)'|([^\s:=,\[\]<>]+)''', text)
    return [(next(part for part in groups if part), "input") for groups in tokens], []


def qualify(root, candidates, consumers):
    """Return an explicit conservative disposition for every supplied candidate.

    Only supplied consumers are inspected. REVIEW_REQUIRED never authorizes a
    deletion: it means no dependency was identified within that declared scope.
    """
    root = Path(root)
    candidates = list(dict.fromkeys(candidates))
    consumers = list(dict.fromkeys(consumers))
    rows = {}
    for candidate in candidates:
        path = _path(root, candidate)
        kind = classify(path)
        rows[candidate] = {"path": candidate, "kind": kind, "role": "unclassified",
                           "disposition": "REVIEW_REQUIRED", "consumers": [],
                           "generators": [], "unresolved": []}
        if kind in {"executable", "configuration"}:
            rows[candidate]["disposition"] = "RETAIN_REQUIRED"
            rows[candidate]["role"] = kind
    unresolved = [] if consumers else ["Consumer scope is empty"]
    for consumer in consumers:
        path = _path(root, consumer)
        if path.suffix.lower() not in TEXT_CONSUMERS and path.suffix:
            unresolved.append(f"{consumer}: unsupported consumer format")
            continue
        try:
            references, unknown = _references(path)
            unresolved.extend(f"{consumer}: {reason}" for reason in unknown)
        except (OSError, ValueError, SyntaxError, ET.ParseError, zipfile.BadZipFile):
            unresolved.append(f"{consumer}: unreadable or invalid consumer")
            continue
        for reference, role in references:
            matched = [name for name in candidates if _matches(name, reference, consumer)]
            # A layer's same-stem files form one conservative retention group.
            # QGIS 3.40 26.1.1: datasource bytes remain external to the project.
            if reference.split("|", maxsplit=1)[0].lower().endswith(".shp"):
                stems = [name[:-len(".shp")] for name in matched if name.lower().endswith(".shp")]
                # The .shp itself need not be a trim candidate.
                for name in candidates:
                    shape = (name.split(".shp.", maxsplit=1)[0] + ".shp"
                             if ".shp." in name else str(PurePosixPath(name).with_suffix(".shp")))
                    if _matches(shape, reference, consumer):
                        stems.append(shape[:-len(".shp")])
                matched += [name for name in candidates
                            if any(name.startswith(stem + ".") for stem in stems)]
            for name in set(matched):
                rows[name]["generators" if role == "output" else "consumers"].append(consumer)
            resolved = matched or any(_matches(name, reference, consumer)
                                       for name in _existing_paths(root, path, reference))
            if not resolved and (role == "read" or role == "input" and _pathlike(reference)):
                unresolved.append(f"{consumer}: dependency could not be resolved")
    for row in rows.values():
        row["consumers"] = sorted(set(row["consumers"]))
        row["generators"] = sorted(set(row["generators"]))
        row["unresolved"] = sorted(set(unresolved))
        if row["consumers"]:
            row.update(disposition="RETAIN_REQUIRED", role="input")
        elif row["disposition"] != "RETAIN_REQUIRED":
            if unresolved:
                row["disposition"] = "RETAIN_UNRESOLVED"
            if row["generators"]:
                row["role"] = "generated-output"
    return list(rows.values())


def _existing_paths(root, consumer, reference):
    """Check exact dependencies outside the trim set without scanning the tree."""
    reference = _pattern(reference)
    for base in (consumer.parent, root):
        target = base / reference
        try:
            name = target.resolve().relative_to(root.resolve()).as_posix()
            if target.exists():
                yield name
        except (OSError, ValueError):
            continue


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--candidate", action="append", required=True)
    parser.add_argument("--consumer", action="append", default=[])
    args = parser.parse_args(argv)
    print(json.dumps(qualify(args.root, args.candidate, args.consumer), indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
