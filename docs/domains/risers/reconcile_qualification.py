# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reconcile the historical riser inventory and Table C without importing tests.

Run with --check to detect drift, or --write to repair the generated fields.
An explicit --write --revision REF selects a new committed snapshot. Source
digests, test membership and syntax classifications come from that commit;
editorial requirements and evidence dispositions are preserved.
"""

import argparse
import ast
import copy
import hashlib
import json
import os
import posixpath
import re
import subprocess
from html import escape
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
REGISTER = ROOT / "docs/domains/risers/global-model-qualification.json"
REPORT = REGISTER.with_suffix(".html")
TEST_DIRECTORY = "tests/drilling_riser/global_model"


def git_bytes(*args):
    """Read the intended repository, independent of inherited Git bindings."""
    env = os.environ.copy()
    for name in (
        "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
        "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
    ):
        env.pop(name, None)
    return subprocess.run(
        ["git", "-C", str(ROOT), *args], env=env, check=True, capture_output=True,
    ).stdout


def snapshot_bytes(revision, path):
    """Read a repository-relative blob at an immutable commit."""
    relative = Path(path)
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("Snapshot paths must stay inside the repository")
    if not (ROOT / relative).resolve().is_relative_to(ROOT):
        raise ValueError("Snapshot paths must resolve inside the repository")
    return git_bytes("cat-file", "blob", f"{revision}:{relative.as_posix()}")


def solver_marked(node):
    """Recognize explicit solver markers without executing decorators."""
    return any(
        isinstance(part, ast.Attribute) and part.attr == "solver"
        for decorator in node.decorator_list for part in ast.walk(decorator)
    )


def coverage_entry(path, raw):
    """Inventory named functions and direct collection probes as syntax only."""
    tree = ast.parse(raw)
    functions = [node for node in ast.walk(tree)
                 if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                 and node.name.startswith("test_")]
    module_solver = any(
        isinstance(node, ast.Assign)
        and any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in node.targets)
        and any(isinstance(p, ast.Attribute) and p.attr == "solver"
                for p in ast.walk(node.value))
        for node in tree.body
    )
    native = sorted(n.name for n in functions if module_solver or solver_marked(n))
    other = sorted(n.name for n in functions if n.name not in native)
    outside_bodies = []
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            outside_bodies.extend(node.decorator_list)
        else:
            outside_bodies.append(node)
    probes = any(
        isinstance(part, ast.Call) and isinstance(part.func, ast.Attribute)
        and part.func.attr == "available"
        for node in outside_bodies for part in ast.walk(node)
    )
    classification = "NO_DIRECT_COLLECTION_PROBE_FOUND"
    if probes:
        classification = "MIXED_WITH_COLLECTION_PROBE" if other else "SOLVER_WITH_COLLECTION_PROBE"
    return {
        "file": path,
        "sha256": hashlib.sha256(raw).hexdigest(),
        "classification": classification,
        "collection_availability_probe": probes,
        "solver_marked_tests": native,
        "other_tests": other,
        "execution_status": "NOT_RUN",
    }


def reconcile_register(register, revision=None):
    """Refresh generated fields only, retaining the documented evidence gaps."""
    result = copy.deepcopy(register)
    revision = revision or result["snapshot_revision"]
    if not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError("snapshot_revision must be a full immutable commit digest")
    result["snapshot_revision"] = revision
    for source in result["sources"].values():
        source["sha256"] = hashlib.sha256(snapshot_bytes(revision, source["path"])).hexdigest()
    paths = git_bytes(
        "ls-tree", "-r", "--name-only", "-z", revision, "--", TEST_DIRECTORY + "/",
    ).decode("utf-8").rstrip("\0").split("\0")
    paths = sorted(path for path in paths if posixpath.dirname(path) == TEST_DIRECTORY
                   and Path(path).match("test_*.py"))
    if not paths:
        raise ValueError("The snapshot contains no global-model test files")
    result["test_coverage"] = [
        coverage_entry(path, snapshot_bytes(revision, path)) for path in paths
    ]
    return result


def reconcile_table(text, entries):
    """Render Table C from the same inventory retained in the register."""
    section = text.index("<h2>Offline and native test coverage</h2>")
    start = text.index("<tbody>", section) + len("<tbody>")
    end = text.index("</tbody>", start)
    rows = []
    for row in entries:
        path = escape(row["file"], quote=True)
        link = escape(posixpath.relpath(row["file"], "docs/domains/risers"), quote=True)
        cells = [str(len(row["solver_marked_tests"])), str(len(row["other_tests"])),
                 row["classification"], row["execution_status"]]
        rows.append(
            f'<tr data-test-file="{path}"><td><a href="{link}">'
            f'{escape(Path(row["file"]).name)}</a></td>'
            + "".join(f"<td>{escape(cell)}</td>" for cell in cells) + "</tr>"
        )
    return text[:start] + "\n" + "\n".join(rows) + "\n" + text[end:]


def main(argv=None):
    """Check or write both outputs after all snapshot reads have succeeded."""
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--write", action="store_true")
    parser.add_argument("--revision", help="Committed revision to select with --write")
    args = parser.parse_args(argv)
    if args.revision and not args.write:
        parser.error("--revision requires --write")
    try:
        revision = None
        if args.revision:
            revision = git_bytes(
                "rev-parse", "--verify", "--end-of-options", args.revision + "^{commit}",
            ).decode("ascii").strip()
        register = reconcile_register(json.loads(REGISTER.read_text(encoding="utf-8")), revision)
        report = reconcile_table(REPORT.read_text(encoding="utf-8"), register["test_coverage"])
        outputs = (
            (REGISTER, (json.dumps(register, indent=2, ensure_ascii=False) + "\n").encode("utf-8")),
            (REPORT, report.encode("utf-8")),
        )
        changed = [path for path, content in outputs if path.read_bytes() != content]
        if args.check:
            for path in changed:
                print(f"Reconciliation required: {path.name}")
            return int(bool(changed))
        for path, content in outputs:
            if path in changed:
                path.write_bytes(content)
    except (OSError, ValueError, KeyError, SyntaxError, subprocess.CalledProcessError):
        print("Reconciliation failed: check the snapshot revision, paths and document structure")
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
