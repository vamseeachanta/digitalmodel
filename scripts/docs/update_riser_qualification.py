# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reconcile the static qualification inventory with a retained Git snapshot.

No solver code is imported. --write updates fingerprints and Table C, never
engineering criteria, evidence records or qualification dispositions.
"""

import argparse
import ast
import hashlib
import json
import os
import re
import subprocess
from html import escape
from pathlib import Path

REGISTER = "docs/domains/risers/global-model-qualification.json"
REPORT = "docs/domains/risers/global-model-qualification.html"
TESTS = "tests/drilling_riser/global_model"


def repository_file(root, relative):
    """Resolve a contained repository path before any filesystem access."""
    path = (root / relative).resolve()
    if Path(relative).is_absolute() or not path.is_relative_to(root.resolve()):
        raise ValueError("Path must stay inside the repository")
    return path


def git_read(root, *args):
    """Read Git objects without inherited repository bindings or console windows."""
    env = {key: value for key, value in os.environ.items()
           if not key.upper().startswith("GIT_")}
    return subprocess.run(
        ["git", *args], cwd=root, env=env, check=True, capture_output=True,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    ).stdout


def snapshot_files(root, revision):
    """Read source pins and top-level test files from one immutable revision."""
    if not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError("Snapshot revision must be a full lowercase commit digest")
    data = json.loads(repository_file(root, REGISTER).read_text(encoding="utf-8"))
    names = git_read(root, "ls-tree", "--name-only", "-z", revision, TESTS + "/")
    paths = {name.decode("utf-8") for name in names.split(b"\0") if name}
    paths = {name for name in paths if Path(name).name.startswith("test_")
             and name.endswith(".py")}
    paths.update(source["path"] for source in data["sources"].values())
    result = {}
    for name in sorted(paths):
        repository_file(root, name)
        result[name] = git_read(root, "cat-file", "blob", f"{revision}:{name}")
    return result


def inventory_row(name, raw):
    """Classify named functions and direct collection probes using syntax only."""
    tree = ast.parse(raw)
    functions = [n for n in ast.walk(tree)
                 if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))
                 and n.name.startswith("test_")]
    def solver(node):
        return any(isinstance(p, ast.Attribute) and p.attr == "solver"
                   for p in ast.walk(node))
    module_solver = any(
        isinstance(n, ast.Assign)
        and any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in n.targets)
        and solver(n.value) for n in tree.body
    )
    native = sorted(n.name for n in functions
                    if module_solver or any(solver(d) for d in n.decorator_list))
    other = sorted(n.name for n in functions if n.name not in native)
    outside = []
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            outside.extend(node.decorator_list)
        else:
            outside.append(node)
    probe = any(isinstance(p, ast.Call) and isinstance(p.func, ast.Attribute)
                and p.func.attr == "available" for n in outside for p in ast.walk(n))
    classification = "NO_DIRECT_COLLECTION_PROBE_FOUND"
    if probe:
        classification = "MIXED_WITH_COLLECTION_PROBE" if other else "SOLVER_WITH_COLLECTION_PROBE"
    return {"file": name, "sha256": hashlib.sha256(raw).hexdigest(),
            "classification": classification, "collection_availability_probe": probe,
            "solver_marked_tests": native, "other_tests": other, "execution_status": "NOT_RUN"}


def render_inventory(text, rows):
    """Replace only Table C's body, preserving the authored evidence discussion."""
    heading = text.index("<h2>Offline and native test coverage</h2>")
    start = text.index("<tbody>", heading) + len("<tbody>")
    end = text.index("</tbody>", start)
    rendered = []
    for row in rows:
        name = escape(Path(row["file"]).name)
        link = escape("../../../" + row["file"], quote=True)
        rendered.append(
            f'<tr><td><a href="{link}">{name}</a></td>'
            f'<td>{len(row["solver_marked_tests"])}</td>'
            f'<td>{len(row["other_tests"])}</td>'
            f'<td>{escape(row["classification"])}</td>'
            f'<td>{escape(row["execution_status"])}</td></tr>'
        )
    return text[:start] + "\n" + "\n".join(rendered) + "\n" + text[end:]


def reconcile(root, write=False, revision=None):
    """Check or reconcile both outputs; retain the selected snapshot by default."""
    register_path = repository_file(root, REGISTER)
    report_path = repository_file(root, REPORT)
    original = register_path.read_text(encoding="utf-8")
    report = report_path.read_text(encoding="utf-8")
    data = json.loads(original)
    revision = revision or data["snapshot_revision"]
    files = snapshot_files(root, revision)
    data["snapshot_revision"] = revision
    for source in data["sources"].values():
        source["sha256"] = hashlib.sha256(files[source["path"]]).hexdigest()
    data["test_coverage"] = [inventory_row(name, raw) for name, raw in sorted(files.items())
                             if name.startswith(TESTS + "/")]
    updated = json.dumps(data, indent=2, ensure_ascii=False) + "\n"
    rendered = render_inventory(report, data["test_coverage"])
    stale = updated != original or rendered != report
    if stale and write:
        register_path.write_text(updated, encoding="utf-8", newline="\n")
        report_path.write_text(rendered, encoding="utf-8", newline="\n")
    return int(stale and not write)


def main():
    """Expose explicit check and write modes for a repository snapshot."""
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--write", action="store_true")
    parser.add_argument("--revision", help="Full commit digest; default: retained snapshot")
    args = parser.parse_args()
    try:
        result = reconcile(Path(__file__).resolve().parents[2], args.write, args.revision)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(2, f"Snapshot reconciliation failed ({type(exc).__name__}).\n")
    print("Inventory is stale; reconcile explicitly with --write." if result else "Inventory reconciled.")
    return result


if __name__ == "__main__":
    raise SystemExit(main())
