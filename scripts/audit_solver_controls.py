# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read-only solver-control inventory; source observations never qualify a run.

Policy basis: issue #2096; docs/domains/ghs/neutral-hydrostatics-canary.md,
"Execution gates"; docs/plans/2026-09-11-issue-2089-windows-containment.md.
No engineering equations, coefficients or acceptance limits are introduced.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import importlib.util
import json
import os
import re
import sys
from functools import lru_cache
from pathlib import Path


# Control identifiers follow issue #2096 and its review-learning comments.
CONTROLS = (
    "staged_input_binding",
    "runtime_image_and_argv",
    "windows_path_aliases",
    "atomic_process_assignment",
    "noninheritable_job_handles",
    "owned_timeout",
    "independent_cleanup",
    "historical_process_census",
    "recovery_state_consistency",
    "positive_trigger_evidence",
    "license_recovery",
    "observed_native_format",
    "public_fixture_clearance",
)
STATUSES = {"STATIC_ONLY", "NOT_EVALUATED", "GAP"}
EXECUTION_APIS = {
    "subprocess.run",
    "subprocess.Popen",
    "subprocess.call",
    "subprocess.check_call",
    "subprocess.check_output",
    "subprocess.getoutput",
    "subprocess.getstatusoutput",
    "os.system",
    "os.popen",
    "OrcFxAPI.Model",
    "OrcFxAPI.Diffraction",
    "win32com.client.Dispatch",
}
SOURCE_SCOPE = "src/digitalmodel/solvers"
TEST_SCOPE = "tests/solvers"
DEFAULT_CATALOG = "docs/domains/solvers/control-audit.json"


def _tree(source: str) -> ast.Module:
    try:
        return ast.parse(source)
    except (SyntaxError, ValueError) as error:
        raise ValueError("invalid Python source") from error


def _dotted(node: ast.AST) -> str:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        base = _dotted(node.value)
        return base + "." + node.attr if base else ""
    return ""


def execution_sites(source: str) -> list[dict]:
    """Enumerate lexical candidates, without evaluating imports or expressions.

    Aliases are module-wide approximations: shadowing can overcount; dynamic
    dispatch, wrappers, factories and facade assignments can be missed.
    Windows ABI method names are candidates regardless of receiver identity.
    Arguments and source text are deliberately excluded from the report.
    """
    tree = _tree(source)
    aliases = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for item in node.names:
                aliases[item.asname or item.name.split(".")[0]] = (
                    item.name if item.asname else item.name.split(".")[0]
                )
        elif isinstance(node, ast.ImportFrom) and not node.level:
            for item in node.names:
                aliases[item.asname or item.name] = f"{node.module}.{item.name}"
    sites = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        name = _dotted(node.func)
        head, separator, tail = name.partition(".")
        name = aliases.get(head, head) + (separator + tail if separator else "")
        if name in EXECUTION_APIS:
            sites.append({"line": node.lineno, "api": name})
        elif name.rsplit(".", 1)[-1] in {"CreateProcessW", "AssignProcessToJobObject"}:
            sites.append(
                {"line": node.lineno, "api": "Windows." + name.rsplit(".", 1)[-1]}
            )
    return sorted(sites, key=lambda row: (row["line"], row["api"]))


def _file(root: Path, name: str) -> Path:
    if (
        not isinstance(name, str)
        or not name
        or "\\" in name
        or ":" in name
        or any(part in {"", ".", ".."} for part in name.split("/"))
    ):
        raise ValueError("invalid relative evidence path")
    path = root
    for part in name.split("/"):
        path = path / part
        if path.is_symlink():
            raise ValueError("symlink in audit scope")
    if not path.is_file():
        raise ValueError("missing evidence file")
    return path


def _python_files(root: Path, scope: str) -> list[str]:
    directory = root / scope
    if not directory.is_dir():
        raise ValueError("missing audit scope")
    if root.is_symlink():
        raise ValueError("symlink in audit scope")
    # Validate directory components before traversing; never follow links.
    current = root
    for part in scope.split("/"):
        current /= part
        if current.is_symlink():
            raise ValueError("symlink in audit scope")
    found = []

    def unreadable(_error):
        raise ValueError("unreadable audit scope")

    for directory, dirs, files in os.walk(
        directory, followlinks=False, onerror=unreadable
    ):
        for name in dirs:
            if (Path(directory) / name).is_symlink():
                raise ValueError("symlink in audit scope")
        for name in files:
            if name.endswith(".py"):
                relative = (Path(directory) / name).relative_to(root).as_posix()
                _file(root, relative)
                found.append(relative)
    return sorted(found)


@lru_cache(maxsize=None)
def _domain_detector():
    """Reuse the shipped ownership parser, never load code from the target tree."""
    path = Path(__file__).with_name("ci") / "detect_touched_domains.py"
    spec = importlib.util.spec_from_file_location("_solver_audit_domains", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _ownership(root: Path) -> dict:
    detector = _domain_detector()
    domains = detector.parse_domains(_file(root, "tests/DOMAINS.md"))
    tests = [
        p
        for p in _python_files(root, TEST_SCOPE)
        if Path(p).name.startswith("test_") or Path(p).name.endswith("_test.py")
    ]
    unowned = [
        p
        for p in tests
        if not any(
            detector.path_matches_root(p, pattern)
            for domain in domains
            for pattern in domain.roots
        )
    ]
    return {
        "status": "GAP" if unowned else "OWNED",
        "owned": len(tests) - len(unowned),
        "unowned": unowned,
        "limitation": "Ownership metadata only; collection and execution not proved.",
    }


def _symbol_nodes(tree: ast.AST, prefix: str = "") -> dict[str, list]:
    symbols = {}
    for node in getattr(tree, "body", []):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            name = prefix + node.name
            symbols.setdefault(name, []).append(node)
            for key, values in _symbol_nodes(node, name + ".").items():
                symbols.setdefault(key, []).extend(values)
    return symbols


def _evidence(root: Path, record: dict) -> dict:
    if (
        not isinstance(record, dict)
        or set(record) != {"path", "symbol", "sha256"}
        or not isinstance(record["symbol"], str)
    ):
        raise ValueError("invalid evidence reference")
    path = _file(root, record["path"])
    if path.suffix != ".py":
        raise ValueError("Python symbol reference required")
    raw = path.read_bytes()
    # Byte-identity contract reused from scripts/verify_solver_evidence.py::verify.
    if hashlib.sha256(raw).hexdigest() != record["sha256"]:
        raise ValueError("evidence digest changed; review and repin required")
    nodes = _symbol_nodes(_tree(raw.decode("utf-8"))).get(record["symbol"], [])
    if len(nodes) != 1:
        raise ValueError("missing or ambiguous evidence symbol")
    node = nodes[0]
    return dict(
        record,
        sha256=hashlib.sha256(raw).hexdigest(),
        start_line=node.lineno,
        end_line=node.end_lineno,
    )


def _observations(root: Path, catalog: list) -> list[dict]:
    if not isinstance(catalog, list):
        raise ValueError("catalog must be a list")
    result, seen = [], set()
    keys = {"id", "family", "control", "status", "observation", "evidence", "follow_up"}
    for row in catalog:
        if not isinstance(row, dict) or set(row) != keys:
            raise ValueError("invalid observation fields")
        if (
            not all(
                isinstance(row[k], str) and row[k].strip() for k in keys - {"evidence"}
            )
            or row["control"] not in CONTROLS
            or row["status"] not in STATUSES
            or not re.fullmatch(r"[a-z][a-z0-9_-]*", row["id"])
            or not re.fullmatch(r"[a-z][a-z0-9_-]*", row["family"])
            or not re.fullmatch(r"#[0-9]+", row["follow_up"])
            or not isinstance(row["evidence"], list)
            or not row["evidence"]
        ):
            raise ValueError("invalid observation claim")
        if row["id"] in seen:
            raise ValueError("duplicate observation id")
        seen.add(row["id"])
        evidence = [_evidence(root, item) for item in row["evidence"]]
        if row["status"] == "GAP" and not any(
            item["path"].startswith("tests/")
            and item["symbol"].split(".")[-1].startswith("test_")
            for item in evidence
        ):
            raise ValueError("gap requires a test reproducer")
        result.append(dict(row, evidence=evidence))
    return sorted(result, key=lambda row: row["id"])


def audit(root: Path, catalog: list) -> dict:
    """Return deterministic source evidence; never grant execution or promotion."""
    root = Path(root)
    inventory = {}
    for name in _python_files(root, SOURCE_SCOPE):
        relative = Path(name).relative_to(SOURCE_SCOPE)
        family = relative.parts[0] if len(relative.parts) > 1 else "package-root"
        entry = inventory.setdefault(
            family, {"family": family, "python_files": 0, "execution_sites": []}
        )
        entry["python_files"] += 1
        source = _file(root, name).read_text(encoding="utf-8")
        entry["execution_sites"].extend(
            dict(path=name, **site) for site in execution_sites(source)
        )
    observations = _observations(root, catalog)
    controls = []
    for control in CONTROLS:
        rows = [row for row in observations if row["control"] == control]
        statuses = {row["status"] for row in rows}
        status = (
            "GAP"
            if "GAP" in statuses
            else "STATIC_ONLY"
            if "STATIC_ONLY" in statuses
            else "NOT_EVALUATED"
        )
        controls.append({"control": control, "status": status, "observations": rows})
    return {
        "source_scope": SOURCE_SCOPE,
        "test_scope": TEST_SCOPE,
        "launch_allowed": False,
        "qualification": "NOT_EVALUATED",
        "inventory": [inventory[key] for key in sorted(inventory)],
        "test_ownership": _ownership(root),
        "controls": controls,
        "limitations": [
            "Lexical candidates only; aliases can overcount and dynamic calls can be missed.",
            "Source digests and symbol locators identify bytes, not observation truth.",
            "Observations cover named functions only, not every member of a family.",
            "No adapter imports, runtime observations, approval issuance or fixture promotion.",
        ],
    }


def main(argv: list[str] | None = None) -> int:
    """Print the audit or a content-free diagnostic for invalid evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[1]
    )
    parser.add_argument("--catalog", type=Path)
    args = parser.parse_args(argv)
    try:
        catalog = args.catalog or args.root / DEFAULT_CATALOG
        result = audit(args.root, json.loads(catalog.read_text(encoding="utf-8")))
    except (OSError, ValueError):
        # Do not echo untrusted file contents, paths, parser errors or identifiers.
        print("solver-control audit failed: invalid scope or evidence", file=sys.stderr)
        return 2
    print(json.dumps(result, indent=2, sort_keys=True))
    # A completed audit can contain gaps. This is not an adapter acceptance gate.
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
