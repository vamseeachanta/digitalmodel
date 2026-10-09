"""Conservative R01 reference holds and reproducible Git scan provenance."""
import ast
import fnmatch
import os
import posixpath
import re
import subprocess

import yaml

# Report producers enumerate inputs; their output is not a consumer dependency.
REPORT_PRODUCERS = {
    "scripts/maintenance/archive_candidates.py",
    "scripts/maintenance/archive_reference_safety.py",
}


class CyclicYamlError(ValueError):
    """An ancestor alias prevents a finite complete YAML traversal."""


def git_environment():
    """Explicit cwd must win over inherited repository/index/object bindings."""
    return {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}


def scan_provenance(repo):
    def git(*args):
        return subprocess.check_output(["git", "-C", repo, *args],
                                       env=git_environment()).decode().strip()

    dirty = git("status", "--porcelain", "--untracked-files=no")
    if dirty:
        raise ValueError("Refusing dirty tracked scan inputs: " + dirty)
    return {"base_commit": git("merge-base", "origin/main", "HEAD"),
            "scanned_head": git("rev-parse", "HEAD"),
            "scanned_index_tree": git("write-tree"),
            "tracked_inputs_clean": True}


def lfs_pointer_paths(repo):
    """Detect indexed LFS pointer headers without requiring the git-lfs executable."""
    result = subprocess.run(["git", "-C", repo, "grep", "--cached", "-l", "-z",
                             "-e", "^version https://git-lfs.github.com/spec/v1$"],
                            env=git_environment(), capture_output=True)
    if result.returncode not in (0, 1):
        raise subprocess.CalledProcessError(result.returncode, result.args, result.stderr)
    paths = []
    for path in result.stdout.decode().split("\0"):
        if path:
            blob = subprocess.check_output(["git", "-C", repo, "show", ":" + path],
                                           env=git_environment())
            if blob.startswith(b"version https://git-lfs.github.com/spec/v1\n"):
                paths.append(path)
    return paths


def yaml_scalars(docs, cycles=None):
    """Iterative, unlimited traversal; aliases at distinct paths remain visible."""
    stack = [(d, frozenset()) for d in docs]
    while stack:
        node, ancestors = stack.pop()
        if isinstance(node, (dict, list)):
            if id(node) in ancestors:
                if cycles is None:
                    raise CyclicYamlError("recursive YAML alias")
                cycles.append(True)
                continue
            ancestors = ancestors | {id(node)}
            children = list(node.keys()) + list(node.values()) if isinstance(node, dict) else node
            stack.extend((v, ancestors) for v in children)
        elif isinstance(node, str):
            yield node


def _locations(value, source):
    value = value.replace("\\", "/").strip()
    if "://" in value or value.startswith("/"):
        return set()
    locations = {posixpath.normpath(value),
                 posixpath.normpath(posixpath.join(posixpath.dirname(source), value))}
    if posixpath.basename(source) in {"mkdocs.yml", "mkdocs.yaml"}:
        locations.add(posixpath.normpath(posixpath.join("docs", value)))
    return {p for p in locations if not p.startswith("../")}


def _template(value):
    return re.sub(r"\$?\{[^{}]*\}|%\([^)]*\)[a-z]|%[sd]", "*", value)


def _python_template(node, env, seen=frozenset()):
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    if isinstance(node, ast.Name):
        return (_python_template(env[node.id], env, seen | {node.id})
                if node.id in env and node.id not in seen else "*")
    if isinstance(node, ast.JoinedStr):
        return "".join(_python_template(v.value if isinstance(v, ast.FormattedValue) else v,
                                      env, seen) for v in node.values)
    if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Div)):
        separator = "/" if isinstance(node.op, ast.Div) else ""
        return _python_template(node.left, env, seen) + separator + _python_template(node.right, env, seen)
    if isinstance(node, ast.Call):
        name = node.func.id if isinstance(node.func, ast.Name) else getattr(node.func, "attr", "")
        if name in {"Path", "PurePath", "join", "joinpath"}:
            values = list(node.args)
            if name == "joinpath":
                values.insert(0, node.func.value)
            return "/".join(_python_template(v, env, seen) for v in values)
    return "*"


def _python_strings(text):
    """Literal/template strings; unresolved globs retain suffix evidence."""
    tree = ast.parse(text)
    env = {t.id: n.value for n in ast.walk(tree) if isinstance(n, ast.Assign)
           for t in n.targets if isinstance(t, ast.Name)}
    covered = set()
    for node in ast.walk(tree):
        if id(node) in covered:
            continue
        if isinstance(node, (ast.JoinedStr, ast.BinOp, ast.Call)):
            value = _python_template(node, env)
            if value != "*":
                covered.update(id(child) for child in ast.walk(node))
                yield value
        elif isinstance(node, ast.Constant) and isinstance(node.value, str):
            yield node.value


def _value_evidence(value, source, candidates, tracked, by_base):
    value = value.strip().replace("\\", "/").split("#", 1)[0]
    locations = _locations(value, source)
    exact = locations & tracked
    if exact:
        return "static", exact & candidates, value
    pattern = _template(value)
    dynamic = pattern != value or any(c in value for c in "*?[")
    if dynamic:
        locations = _locations(pattern, source)
        hits = {p for p in candidates if any(fnmatch.fnmatchcase(p, q) for q in locations)}
        # Unknown root: a filename suffix still identifies potentially affected files.
        if "/" not in pattern or pattern.startswith("*/"):
            hits |= {p for p in candidates
                     if fnmatch.fnmatchcase(posixpath.basename(p), posixpath.basename(pattern))}
        return "runtime path or unresolved glob", hits, pattern
    basename = posixpath.basename(value)
    matches = by_base.get(basename, set())
    if len(matches) == 1 and "/" not in value:
        return "static", matches & candidates, value
    return "ambiguous or unresolved reference", matches & candidates, value


def reference_safety(texts, candidates, tracked, hygiene_sources=frozenset()):
    """Return proven exclusions, candidate-level human holds, and scan gaps.

    Only executable Python and YAML are scanned here. CSV/JSON archive manifests
    and Markdown review reports are never interpreted as consumers. Explicit
    references from candidate models count too, protecting retained dependencies.
    """
    excluded, held, gaps = {}, {}, []
    candidates, tracked = set(candidates), set(tracked)
    by_base = {}
    for path in tracked:
        by_base.setdefault(posixpath.basename(path), set()).add(path)
    loader = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
    for source, text in sorted(texts.items()):
        if source in REPORT_PRODUCERS:
            continue
        uncertain = False
        try:
            if source.endswith((".yml", ".yaml")):
                cycles = []
                values = list(yaml_scalars(yaml.load_all(text, Loader=loader), cycles))
                if cycles:
                    gaps.append({"source": source, "reason": "CyclicYamlError"})
                    if source in candidates:
                        held[source] = [{"source": source, "reason": "CyclicYamlError"}]
            elif source.endswith(".py"):
                values = list(_python_strings(text))
            else:
                continue
        except (yaml.YAMLError, CyclicYamlError, SyntaxError, RecursionError) as error:
            gaps.append({"source": source, "reason": type(error).__name__})
            if source in candidates:
                held[source] = [{"source": source, "reason": type(error).__name__}]
            # Readable path tokens remain evidence even when the source cannot parse.
            values = re.findall(r"[A-Za-z0-9_./{}*?%$\\-]+", text)
            uncertain = True
        for value in sorted(set(values)):
            if len(value) > 512 or "." not in value or "://" in value:
                continue
            reason, hits, matched = _value_evidence(value, source, candidates, tracked, by_base)
            if source in hygiene_sources and reason == "runtime path or unresolved glob":
                continue
            if uncertain:
                reason = "readable reference in unparseable source"
            for path in sorted(hits - {source}):
                evidence = {"source": source, "matched": matched, "reason": reason}
                if reason == "static":
                    excluded.setdefault(path, {"source": source, "matched": matched,
                                               "evidence": "path"})
                else:
                    held.setdefault(path, []).append(evidence)
    return excluded, {p: evidence for p, evidence in held.items() if p not in excluded}, gaps
