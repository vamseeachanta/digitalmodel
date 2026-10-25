# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Refresh a curated implementation map and expose unresolved search candidates.

Run with Python, PyYAML and ruamel.yaml installed. Engineering classifications remain curated;
name matches alone never establish that a definition implements a method.
"""

import argparse
import ast
import copy
import hashlib
import io
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import warnings
from collections import Counter, defaultdict
from collections.abc import Iterator
from typing import Any

import yaml

_SEARCH_SPEC = importlib.util.spec_from_file_location(
    "implementation_map_search", Path(__file__).with_name("implementation_map_search.py"))
assert _SEARCH_SPEC is not None and _SEARCH_SPEC.loader is not None
_SEARCH_MODULE = importlib.util.module_from_spec(_SEARCH_SPEC)
_SEARCH_SPEC.loader.exec_module(_SEARCH_MODULE)

FAMILY_IDS = (
    "catenary-mooring-line-statics", "effective-tension-riser-stack-up",
    "orcaflex-text-model-writers", "rao-readers-and-orcaflex-converters",
    "irregular-frequency-treatment", "wave-spectra", "wind-and-current-force-routes",
    "sn-curves-and-miner-damage", "cathodic-protection-calculations",
    "api-5ct-5l-grade-tables", "external-solver-batch-run-stacks",
    "dnv-rp-e305-on-bottom-stability", "linear-wave-kinematics",
    "second-order-slow-drift-response", "extreme-statistics", "design-waves",
    "sloshing", "geotechnics", "scour",
)
REQUIRED_KEYS = {
    "schema_version", "repository", "revision", "searched", "families",
    "byte_identical_groups", "identical_body_groups", "known_defects",
    "other_defects", "seed_verification", "rejected_candidates", "notes",
}
EXCLUDED = {
    "__pycache__", ".pytest_cache", "_build", "build", "dist", "generated",
    "htmlcov", "node_modules", "output", "outputs", "results", "site",
}
DEFINITION = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)


def definitions(text: str) -> dict[str, Any]:
    """Index exact lexical qualified names without importing source modules."""
    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        tree = ast.parse(text)
    found: dict[str, Any] = {}

    def visit(node: ast.AST, prefix: str = "") -> None:
        for child in ast.iter_child_nodes(node):
            name = prefix
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                name = f"{prefix}.{child.name}" if prefix else child.name
                found[name] = child
            visit(child, name)

    visit(tree)
    return found


def start_line(node: Any) -> int:
    """Return the first decorator line or the definition line."""
    lines: list[int] = [node.lineno] + [d.lineno for d in node.decorator_list]
    return min(lines)


def body_digest(node: ast.AST) -> str:
    """Hash the AST after removing docstrings and source locations."""
    class StripDocstrings(ast.NodeTransformer):
        """Normalize docstrings in copied definition trees."""

        def generic_visit(self, current: ast.AST) -> ast.AST:
            """Remove only the leading string expression in each body."""
            super().generic_visit(current)
            body = getattr(current, "body", None)
            if isinstance(body, list) and body and isinstance(body[0], ast.Expr):
                value = body[0].value
                if isinstance(value, ast.Constant) and isinstance(value.value, str):
                    setattr(current, "body", body[1:])
            return current

    clean = StripDocstrings().visit(copy.deepcopy(node))
    return hashlib.sha256(ast.dump(clean, include_attributes=False).encode()).hexdigest()


class Sources:
    """Cache bounded, checkout-contained source reads and parsed definitions."""

    def __init__(self, root: str | Path) -> None:
        """Bind all reads to a resolved checkout root."""
        self.root = Path(root).resolve()
        self.targets: dict[str, Path] = {}
        self.contents: dict[str, bytes] = {}
        self.nodes: dict[str, dict[str, Any]] = {}

    def target(self, path: str) -> Path:
        """Validate relative paths and reject symlinks outside the checkout."""
        if not isinstance(path, str):
            raise ValueError("invalid path")
        if path in self.targets:
            return self.targets[path]
        relative = Path(path)
        target = (self.root / relative).resolve()
        if relative.is_absolute() or ".." in relative.parts or not target.is_relative_to(self.root):
            raise ValueError("path escapes checkout")
        self.targets[path] = target
        return target

    def read(self, path: str) -> bytes:
        """Read each source file once as bytes."""
        if path not in self.contents:
            self.contents[path] = self.target(path).read_bytes()
        return self.contents[path]

    def index(self, path: str) -> dict[str, Any]:
        """Cache exact qualified definitions from parseable Python files."""
        if path not in self.nodes:
            try:
                self.nodes[path] = definitions(self.read(path).decode("utf-8"))
            except (SyntaxError, UnicodeError):
                self.nodes[path] = {}
        return self.nodes[path]


def records(value: Any, context: str = "") -> Iterator[tuple[dict[str, Any], str]]:
    """Yield reference-bearing mappings, including nested delegate references."""
    if isinstance(value, list):
        for item in value:
            yield from records(item, context)
    elif isinstance(value, dict):
        if "path" in value:
            yield value, context
        for key, item in value.items():
            yield from records(item, key if key == "seed_verification" else context)


def _schema_problems(doc: dict[str, Any]) -> list[str]:
    """Check required keys, family identities and tracked defect identities."""
    problems = [f"missing key: {key}" for key in sorted(REQUIRED_KEYS - doc.keys())]
    families = {f["id"]: f for f in doc.get("families", [])}
    for family_id in FAMILY_IDS:
        if not families.get(family_id, {}).get("implementations"):
            problems.append(f"missing or empty family: {family_id}")
    if len(families) != len(doc.get("families", [])):
        problems.append("duplicate family id")
    issues = {d.get("number") for d in doc.get("known_defects", [])}
    for issue in (2050, 2107, 2543):
        if issue not in issues:
            problems.append(f"missing known defect: {issue}")
    return problems


def _check_path(sources: Sources, path: str, problems: list[str], directory: bool = False) -> bool:
    """Validate one reference and collect a hard failure when it is invalid."""
    try:
        target = sources.target(path)
        if directory and target.is_dir():
            if not any(target.iterdir()):
                problems.append(f"empty seed directory: {path}")
            return False
        if not target.is_file():
            problems.append(f"missing file: {path}")
            return False
        return True
    except (ValueError, OSError):
        problems.append(f"invalid or escaping path: {path}")
        return False


def check_map(doc: Any, root: str | Path) -> list[str]:
    """Return hard schema, path and qualified-symbol failures for a live checkout."""
    if not isinstance(doc, dict):
        return ["document must be a mapping"]
    problems = _schema_problems(doc)
    sources = Sources(root)

    for row, context in records(doc):
        absent = context == "seed_verification" and row.get("exists") in (False, "no")
        if row.get("moved_to"):
            destination = row["moved_to"]
            if _check_path(sources, destination, problems) and row.get("symbol"):
                if row["symbol"] not in sources.index(destination):
                    problems.append(f"missing moved qualified symbol: {destination}:{row['symbol']}")
        if absent:
            continue
        path = row["path"]
        if _check_path(sources, path, problems, context == "seed_verification"):
            symbol = row.get("symbol")
            if Path(path).suffix == ".py" and symbol and symbol not in sources.index(path):
                problems.append(f"missing qualified symbol: {path}:{symbol}")
    entries = [e for f in doc.get("families", []) for e in f.get("implementations", [])]
    for family in doc.get("families", []):
        mapped = {(e["path"], e.get("symbol")) for e in family.get("implementations", [])}
        for row in doc.get("rejected_candidates", []):
            if family["id"] in row.get("families", []) and (row["path"], row.get("symbol")) in mapped:
                problems.append(f"mapped and rejected: {family['id']}:{row['path']}:{row.get('symbol')}")
    identities = {(e["path"], e.get("symbol")) for e in entries}
    for entry in entries:
        for other in entry.get("identical_to", []):
            identity = (other["path"], other.get("symbol"))
            if identity not in identities or identity == (entry["path"], entry.get("symbol")):
                problems.append(f"unknown identical member: {identity}")
    for group in doc.get("byte_identical_groups", []):
        for path in group["paths"]:
            _check_path(sources, path, problems)
    # Evidence coordinates are snapshot metadata; evidence paths still must exist.
    for entry in _all_dicts(doc):
        evidence = entry.get("evidence")
        if isinstance(evidence, str) and ":L" in evidence:
            _check_path(sources, evidence.rsplit(":L", 1)[0], problems)
    return sorted(set(problems))


def _all_dicts(value: Any) -> Iterator[dict[str, Any]]:
    if isinstance(value, dict):
        yield value
        for item in value.values():
            yield from _all_dicts(item)
    elif isinstance(value, list):
        for item in value:
            yield from _all_dicts(item)


def map_staleness(doc: dict[str, Any], root: str | Path) -> list[str]:
    """Report snapshot drift separately from missing live reference failures."""
    sources = Sources(root)
    stale = set()
    for row, context in records(doc):
        if context == "seed_verification" and row.get("exists") in (False, "no"):
            continue
        path = row["path"]
        try:
            data = sources.read(path)
        except (ValueError, OSError):
            continue  # Missing references are hard failures in check_map.
        digest = row.get("file_sha256")
        if digest and hashlib.sha256(data).hexdigest() != digest:
            stale.add(f"file digest drift: {path}")
        symbol = row.get("symbol")
        node = sources.index(path).get(symbol) if symbol and Path(path).suffix == ".py" else None
        if node is not None:
            if "line" in row and row["line"] not in (node.lineno, start_line(node)):
                stale.add(f"line drift: {path}:{symbol}")
            if row.get("body_sha256") and body_digest(node) != row["body_sha256"]:
                stale.add(f"body digest drift: {path}:{symbol}")
        elif row.get("line") is not None and not symbol:
            if not isinstance(row["line"], int) or not 1 <= row["line"] <= len(data.splitlines()):
                stale.add(f"line drift: {path}")
    for group in doc.get("byte_identical_groups", []):
        try:
            if any(hashlib.sha256(sources.read(p)).hexdigest() != group["sha256"] for p in group["paths"]):
                stale.add(f"byte group drift: {group['id']}")
        except (ValueError, OSError):
            continue
    for group in doc.get("identical_body_groups", []):
        for row in group["members"]:
            try:
                node = sources.index(row["path"]).get(row["symbol"])
                if node is None or body_digest(node) != group["sha256"]:
                    stale.add(f"body group drift: {group['id']}")
            except (ValueError, OSError):
                continue
    return sorted(stale)


def git_output(root: str | Path, *args: str) -> bytes:
    """Resolve Git state from the requested checkout, not inherited bindings."""
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.check_output(["git", "-C", str(root), *args], env=env)


def inventory(root: str | Path, tracked_paths: list[str], sources: Sources | None = None) -> tuple[dict[str, str], list[dict[str, Any]], dict[str, int]]:
    """Enumerate tracked files, applying the map's bounded text-search exclusions."""
    sources = sources or Sources(root)
    texts: dict[str, str] = {}
    folders: defaultdict[str, Counter[str]] = defaultdict(Counter)
    exclusions: Counter[str] = Counter()
    for path in sorted(tracked_paths):
        relative = Path(path)
        folders[relative.parts[0] if len(relative.parts) > 1 else "(root)"][relative.suffix or "(none)"] += 1
        if path in ("docs/registry/implementation-map.yaml", "docs/registry/implementation-map.md"):
            exclusions["registry_outputs"] += 1
            continue
        if set(relative.parts) & EXCLUDED:
            exclusions["generated_output_folders"] += 1
            continue
        if relative.name.endswith(".lock") or relative.name in ("package-lock.json", "yarn.lock"):
            exclusions["lock_files"] += 1
            continue
        if (sources.root / relative).is_symlink() and not (sources.root / relative).resolve().is_relative_to(sources.root):
            exclusions["external_links"] += 1
            continue
        target = sources.target(path)
        if not target.is_file():
            exclusions["not_regular_file"] += 1
            continue
        if target.stat().st_size > 1024 * 1024:
            exclusions["over_1_MiB"] += 1
            continue
        data = sources.read(path)
        try:
            if b"\0" in data:
                raise UnicodeError
            texts[path] = data.decode("utf-8")
        except UnicodeError:
            exclusions["binary"] += 1
    return texts, [{"folder": k, "files_by_extension": dict(sorted(v.items()))}
                   for k, v in sorted(folders.items())], dict(sorted(exclusions.items()))


def _reconcile_names(family: dict[str, Any], doc: dict[str, Any], texts: dict[str, str], sources: Sources, patterns: list[re.Pattern[str]]) -> tuple[int, int]:
    """Reconcile named non-test definitions against individual dispositions."""
    mapped = {(r["path"], r.get("symbol")) for r in family["implementations"]}
    rejected = {(r["path"], r.get("symbol")) for r in doc["rejected_candidates"]
                if family["id"] in r.get("families", []) and r.get("symbol")}
    named_hits, unresolved = 0, 0
    for path in sorted(texts):
        parts = Path(path).parts
        if not path.endswith(".py") or "tests" in parts or Path(path).name.startswith("test_"):
            continue
        for symbol, node in sources.index(path).items():
            if not any(p.search(node.name) for p in patterns):
                continue
            named_hits += 1
            if (path, symbol) in mapped | rejected:
                continue
            unresolved += 1
            doc["rejected_candidates"].append({
                "path": path, "symbol": symbol, "line": start_line(node),
                "families": [family["id"]], "disposition": "needs_review",
                "reason": "Name matches a recorded search pattern; engineering membership has not been established. Excluded from implementation counts pending inspection.",
            })
    return named_hits, unresolved


def _refresh_rejections(doc: dict[str, Any], sources: Sources) -> None:
    """Drop vanished candidates and retain only families without a mapped row."""
    mapped = {f["id"]: {(e["path"], e.get("symbol")) for e in f["implementations"]}
              for f in doc["families"]}
    retained = []
    for row in doc["rejected_candidates"]:
        path, symbol = row["path"], row.get("symbol")
        if not sources.target(path).is_file():
            continue
        if symbol and path.endswith(".py") and symbol not in sources.index(path):
            continue
        families = row.get("families", [])
        remaining = [f for f in families if (path, symbol) not in mapped.get(f, set())]
        if families and not remaining:
            continue
        if families:
            row["families"] = remaining
        retained.append(row)
    doc["rejected_candidates"] = retained


def reconcile(doc: dict[str, Any], texts: dict[str, str], sources: Sources) -> None:
    """Account for each non-test definition name matched by recorded patterns.

    A file-level rejection cannot dispose of a named definition. Newly discovered
    names receive an unresolved rejection and make the family explicitly partial.
    The classification must be inspected before it can establish completeness.
    """
    doc["rejected_candidates"] = [r for r in doc["rejected_candidates"]
                                   if r.get("disposition") != "needs_review"]
    for family in doc["families"]:
        print(f"Reconciling {family['id']}", flush=True)
        scope = family.get("inventory_scope")
        family_texts = (
            {p: texts[p] for p in scope["source_paths"] if p in texts}
            if scope else texts
        )
        patterns = [re.compile(s["pattern"], re.I) for s in family["search_signals"]]
        audit = []
        for signal, pattern in zip(family["search_signals"], patterns):
            hits = [p for p, text in family_texts.items() if _SEARCH_MODULE.matches(pattern, text)]
            audit.append({"search": signal["search"], "matching_files": len(hits)})
        family["search_audit"] = audit
        named_hits, unresolved = _reconcile_names(family, doc, family_texts, sources, patterns)
        family["reconciliation"] = {"matched_definitions": named_hits,
                                    "unresolved_definitions": unresolved}
        family["coverage"] = "partial" if unresolved or scope else "reconciled_names"
        family["coverage_reason"] = (
            "Only the listed source paths were inspected; name reconciliation does not establish semantic completeness."
            if scope else
            "Unresolved name matches are recorded individually in rejected_candidates; classification requires inspection."
            if unresolved else "All name matches in searched non-test Python definitions are accounted for; dynamic code and semantic completeness remain unestablished."
        )


def rebuild_groups(doc: dict[str, Any], sources: Sources) -> None:
    """Recompute file and normalized-body groups from current implementation rows."""
    entries = [e for f in doc["families"] for e in f["implementations"]]
    files: defaultdict[str, set[str]] = defaultdict(set)
    bodies: defaultdict[str, dict[tuple[str, str], dict[str, Any]]] = defaultdict(dict)
    for entry in entries:
        entry.pop("identical_to", None)
        path, symbol = entry["path"], entry.get("symbol")
        files[hashlib.sha256(sources.read(path)).hexdigest()].add(path)
        if symbol and path.endswith(".py"):
            node = sources.index(path).get(symbol)
            if node is not None:
                digest = body_digest(node)
                entry["body_sha256"] = digest
                bodies[digest][(path, symbol)] = {"path": path, "symbol": symbol, "line": start_line(node)}
    doc["byte_identical_groups"] = [
        {"id": f"file-{i:03}", "sha256": digest, "paths": sorted(paths),
         "family_ids": [f["id"] for f in doc["families"]
                        if any(e["path"] in paths for e in f["implementations"])]}
        for i, (digest, paths) in enumerate(sorted((d, p) for d, p in files.items() if len(p) > 1), 1)
    ]
    doc["identical_body_groups"] = [
        {"id": f"body-{i:03}", "sha256": digest, "members": [rows[k] for k in sorted(rows)]}
        for i, (digest, rows) in enumerate(sorted((d, r) for d, r in bodies.items() if len(r) > 1), 1)
    ]
    for family in doc["families"]:
        identities = {(e["path"], e.get("symbol")) for e in family["implementations"]}
        groups = [g["id"] for g in doc["byte_identical_groups"] if family["id"] in g["family_ids"]]
        for group in doc["identical_body_groups"]:
            if any((r["path"], r["symbol"]) in identities for r in group["members"]):
                groups.append(group["id"])
        family["identical_groups"] = groups
    # Identical-body links are resolved through group membership, avoiding stale coordinates.


def _refresh_references(doc: dict[str, Any], sources: Sources) -> None:
    """Refresh snapshot hashes and coordinates without changing classifications."""
    for row, context in records(doc):
        path = row["path"]
        if context == "seed_verification" and row.get("exists") in (False, "no"):
            continue
        if not sources.target(path).is_file():
            continue
        if "file_sha256" in row:
            row["file_sha256"] = hashlib.sha256(sources.read(path)).hexdigest()
        symbol = row.get("symbol")
        node = sources.index(path).get(symbol) if path.endswith(".py") and symbol else None
        if node is not None and "line" in row:
            row["line"] = start_line(node)
        # Definition evidence spans follow the same qualified definition as their row.
        if node is not None:
            for evidence in [row, *row.get("defects", [])]:
                if isinstance(evidence.get("evidence"), str) and evidence["evidence"].startswith(path + ":L"):
                    evidence["evidence"] = f"{path}:L{start_line(node)}-L{node.end_lineno}"


def regenerate(doc: dict[str, Any], root: str | Path, tracked_paths: list[str] | None = None, revision: str | None = None) -> dict[str, Any]:
    """Refresh derived metadata while retaining inspected engineering judgments."""
    root = Path(root).resolve()
    if tracked_paths is None:
        tracked_paths = git_output(root, "ls-files", "-z").decode().rstrip("\0").split("\0")
    doc = copy.deepcopy(doc)
    doc["revision"] = revision or git_output(root, "rev-parse", "HEAD").decode().strip()
    sources = Sources(root)
    # Historical patterns stored regex escapes twice. Normalize exactly one layer.
    for family in doc["families"]:
        if "inventory_scope" in family:
            family["inventory_scope"]["revision"] = doc["revision"]
        for signal in family["search_signals"]:
            signal["pattern"] = signal["pattern"].replace("\\\\", "\\")
        for entry in family["implementations"]:
            entry["signals"] = [s.replace("\\\\", "\\") for s in entry.get("signals", [])]
    texts, folders, exclusions = inventory(root, tracked_paths, sources)
    _refresh_rejections(doc, sources)
    _refresh_references(doc, sources)
    reconcile(doc, texts, sources)
    rebuild_groups(doc, sources)
    doc["searched"].update(tracked_files=len(tracked_paths), text_files_searched=len(texts),
                          folders=folders, exclusions=exclusions,
                          python_ast_parse_failures=sum(1 for p in texts if p.endswith(".py") and not sources.index(p) and _syntax_error(texts[p])),
                          method="Tracked files are searched independently with recorded regular expressions; Python definitions are confirmed with AST and name matches are reconciled individually.")
    seeds = [s["path"] for s in doc["seed_verification"] if s.get("exists") not in (False, "no")]
    doc["searched"]["beyond_seed_implementations"] = sum(
        not any(e["path"] == p or (p.endswith("/") and e["path"].startswith(p)) for p in seeds)
        for f in doc["families"] for e in f["implementations"]
    )
    return doc


def _syntax_error(text: str) -> bool:
    try:
        definitions(text)
        return False
    except SyntaxError:
        return True


def render_map(original: str, doc: dict[str, Any]) -> str:
    """Preserve unchanged YAML scalars, quotes and anchors during regeneration."""
    from ruamel.yaml import YAML

    emitter = YAML()
    emitter.preserve_quotes = True
    emitter.width = 1000000
    current = emitter.load(original)

    from ruamel.yaml.comments import CommentedMap, CommentedSeq

    anchored = {str(value): value for row in _all_dicts(current)
                for value in row.values() if isinstance(value, str)
                and getattr(getattr(value, "anchor", None), "value", None)}

    def merge(old: Any, new: Any) -> Any:
        # Aliases can share containers and scalars: never mutate the source tree.
        if old == new:
            return old
        if isinstance(old, dict) and isinstance(new, dict):
            result = CommentedMap()
            for key, value in new.items():
                result[key] = merge(old.get(key), value)
            return result
        if isinstance(old, list) and isinstance(new, list):
            sequence = CommentedSeq(merge(old[i] if i < len(old) else None, item)
                                    for i, item in enumerate(new))
            if hasattr(old, "fa") and old.fa.flow_style():
                sequence.fa.set_flow_style()
            return sequence
        if isinstance(new, str):
            if new in anchored:
                return anchored[new]
            if isinstance(old, str) and type(old) is not str:
                return type(old)(new)
        return new

    output = io.StringIO()
    emitter.dump(merge(current, doc), output)
    rendered = output.getvalue()
    if yaml.safe_load(rendered) != doc:
        raise ValueError("rendered map readback differs")
    return rendered


def main() -> int:
    """Run reference validation, staleness reporting, or explicit regeneration."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--map", default="docs/registry/implementation-map.yaml")
    parser.add_argument("--write", action="store_true")
    parser.add_argument("--checkpoint", type=Path,
                        help="Save derived metadata before YAML rendering for recovery")
    args = parser.parse_args()
    sources = Sources(args.root)
    target = sources.target(args.map)
    doc = yaml.safe_load(target.read_text(encoding="utf-8"))
    if args.write:
        print("Regenerating tracked-file searches and digests.", flush=True)
        doc = regenerate(doc, sources.root)
        if args.checkpoint:
            args.checkpoint.write_text(json.dumps(doc), encoding="utf-8")
            if json.loads(args.checkpoint.read_text(encoding="utf-8")) != doc:
                raise ValueError("checkpoint readback differs")
        print("Searches complete; validating and rendering metadata.", flush=True)
    problems = check_map(doc, sources.root)
    for problem in problems:
        print("ERROR:", problem)
    if problems:
        return 1
    if args.write:
        target.write_text(render_map(target.read_text(encoding="utf-8"), doc), encoding="utf-8")
        if yaml.safe_load(target.read_text(encoding="utf-8")) != doc:
            raise ValueError("generated map readback differs")
    stale = map_staleness(doc, sources.root)
    for warning in stale:
        print("STALE:", warning)
    print(f"References valid; {len(stale)} staleness diagnostics.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
