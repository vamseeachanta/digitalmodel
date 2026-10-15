# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate implementation-map references against the current checkout."""

import ast
import copy
import warnings
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
MAP_PATH = ROOT / "docs/registry/implementation-map.yaml"
FAMILY_IDS = (
    "catenary-mooring-line-statics",
    "effective-tension-riser-stack-up",
    "orcaflex-text-model-writers",
    "rao-readers-and-orcaflex-converters",
    "irregular-frequency-treatment",
    "wave-spectra",
    "wind-and-current-force-routes",
    "sn-curves-and-miner-damage",
    "cathodic-protection-calculations",
    "api-5ct-5l-grade-tables",
    "external-solver-batch-run-stacks",
    "dnv-rp-e305-on-bottom-stability",
)
REQUIRED_KEYS = {
    "schema_version", "repository", "revision", "searched", "families",
    "byte_identical_groups", "identical_body_groups", "known_defects",
    "other_defects", "seed_verification", "rejected_candidates", "notes",
}


def check_map(doc, root):
    """Return reference and schema problems without running mapped code."""
    root = Path(root).resolve()
    problems = []
    text_cache = {}
    ast_cache = {}
    if not isinstance(doc, dict):
        return ["document must be a mapping"]
    for key in sorted(REQUIRED_KEYS - doc.keys()):
        problems.append(f"missing key: {key}")
    families = {f["id"]: f for f in doc.get("families", [])}
    for family_id in FAMILY_IDS:
        if not families.get(family_id, {}).get("implementations"):
            problems.append(f"missing or empty family: {family_id}")
    issues = {d.get("number") for d in doc.get("known_defects", [])}
    for issue in (2050, 2107, 2543):
        if issue not in issues:
            problems.append(f"missing known defect: {issue}")

    def check_path(path, allow_directory=False):
        if not isinstance(path, str):
            problems.append(f"invalid path: {path!r}")
            return None
        relative = Path(path)
        target = root / relative
        if relative.is_absolute() or ".." in relative.parts:
            problems.append(f"path escapes checkout: {path}")
            return None
        if allow_directory and target.is_dir():
            if not any(f.is_file() for f in target.rglob("*")):
                problems.append(f"empty seed directory: {path}")
            return None
        if not target.is_file():
            problems.append(f"missing file: {path}")
            return None
        if path not in text_cache:
            text_cache[path] = target.read_text(encoding="utf-8").splitlines()
        return target

    def walk(value, context=""):
        if isinstance(value, list):
            for item in value:
                walk(item, context)
        elif isinstance(value, dict):
            path = value.get("path")
            absent_seed = context == "seed_verification" and value.get("exists") in (False, "no")
            if path is not None and not absent_seed:
                target = check_path(path, context == "seed_verification")
                if target is not None:
                    line = value.get("line")
                    if line is not None and (
                        not isinstance(line, int) or not 1 <= line <= len(text_cache[path])
                    ):
                        problems.append(f"invalid line: {path}:{line}")
                    symbol = value.get("symbol")
                    if target.suffix == ".py" and symbol and line is not None:
                        if path not in ast_cache:
                            try:
                                # Legacy escape warnings do not invalidate a definition.
                                # Record them locally even when the suite escalates warnings.
                                with warnings.catch_warnings(record=True):
                                    warnings.simplefilter("always", SyntaxWarning)
                                    ast_cache[path] = ast.parse("\n".join(text_cache[path]))
                            except SyntaxError:
                                ast_cache[path] = None
                        tree = ast_cache[path]
                        name = symbol.rsplit(".", 1)[-1]
                        if tree is None or not any(
                            isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))
                            and n.name == name and n.lineno <= line <= n.end_lineno
                            for n in ast.walk(tree)
                        ):
                            problems.append(f"symbol does not contain line: {path}:{symbol}:{line}")
            for key, item in value.items():
                if key == "paths":
                    for member in item:
                        check_path(member)
                elif key == "evidence" and isinstance(item, str) and ":L" in item:
                    evidence_path, span = item.rsplit(":L", 1)
                    if check_path(evidence_path) is not None:
                        try:
                            start, end = (int(v.removeprefix("L")) for v in span.split("-"))
                            if not 1 <= start <= end <= len(text_cache[evidence_path]):
                                raise ValueError
                        except ValueError:
                            problems.append(f"invalid evidence: {item}")
                else:
                    walk(item, key if key == "seed_verification" else context)

    walk(doc)
    entries = [e for f in doc.get("families", []) for e in f.get("implementations", [])]
    identities = {(e["path"], e.get("symbol"), e["line"]) for e in entries}
    for entry in entries:
        for other in entry.get("identical_to", []):
            identity = (other["path"], other.get("symbol"), other["line"])
            if identity not in identities or identity == (
                entry["path"], entry.get("symbol"), entry["line"]
            ):
                problems.append(f"unknown identical member: {identity}")
    return problems


def test_implementation_map_references():
    doc = yaml.safe_load(MAP_PATH.read_text(encoding="utf-8"))
    assert check_map(doc, ROOT) == []


def test_missing_path_is_reported():
    doc = copy.deepcopy(yaml.safe_load(MAP_PATH.read_text(encoding="utf-8")))
    missing = "src/implementation_map_missing_probe.py"
    doc["families"][0]["implementations"][0]["path"] = missing
    problems = check_map(doc, ROOT)
    assert any(missing in problem for problem in problems), problems
