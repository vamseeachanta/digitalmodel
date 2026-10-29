# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Inventory comment-normalized duplicates without loading a solver or YAML.

Run from a checkout with --output-dir pointing to scratch storage. Outputs are
a proposal CSV, a separate reference CSV and a compact JSON count summary.
"""

import argparse
import csv
import hashlib
import json
import os
import re
import subprocess
from collections import defaultdict
from pathlib import Path

PREFIX = "docs/domains/orcaflex/"
MODEL_KEYS = re.compile(
    rb"^(?:General|Environment|LineTypes|Vessels|BaseFile|IncludeFile):", re.M
)


def content_digest(data: bytes) -> str:
    """Discard full comment lines; retain blank lines, spacing and inline comments."""
    clean = b"\n".join(
        line for line in data.splitlines() if not line.lstrip().startswith(b"#")
    )
    return hashlib.sha256(clean).hexdigest()


def family(paths: list[str]) -> str:
    """Assign each set once; mixed sets go to other."""
    short = [p.removeprefix(PREFIX) for p in paths]
    if all(p.startswith(("mooring/buoy/", "mooring/buoy_converted/")) for p in short):
        return "buoy"
    if all(p.startswith("library/tier2_fast/") for p in short):
        return "tier2_fast"
    prefixes = ("examples/raw/", "examples/yml/", "library/model_library/")
    if all(p.startswith(prefixes) for p in short):
        return "examples_and_model_library"
    return "other"


def canonical_key(path: str, group: str) -> tuple:
    """Propose a location only; this does not establish provenance or validity."""
    short = path.removeprefix(PREFIX)
    rank = 0
    if group == "buoy":
        rank = int(not (short.startswith("mooring/buoy/") and "/modular/" not in short))
    elif group == "examples_and_model_library":
        prefixes = ("examples/raw/", "examples/yml/", "library/model_library/")
        rank = next(i for i, prefix in enumerate(prefixes) if short.startswith(prefix))
    return rank, len(Path(path).parts), path.casefold(), path


def tracked_paths(root: Path) -> list[str]:
    """Enumerate tracked inputs without inheriting another checkout's bindings."""
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    result = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=root,
        env=env,
        capture_output=True,
        check=True,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    )
    return [name for name in result.stdout.decode("utf-8").split("\0") if name]


def duplicate_sets(root: Path, tracked: list[str]) -> tuple[int, list]:
    """Include library/test YAML and other tracked YAML with a model top-level key."""
    groups = defaultdict(list)
    scanned = 0
    for name in tracked:
        path = root / name
        if path.suffix.lower() not in {".yml", ".yaml"}:
            continue
        if path.is_symlink() or not path.is_file():
            continue
        data = path.read_bytes()
        known_root = name.startswith((PREFIX, "tests/solvers/orcaflex/"))
        if not known_root and not MODEL_KEYS.search(data):
            continue
        groups[content_digest(data)].append(name)
        scanned += 1
    sets = sorted(
        (key, sorted(paths)) for key, paths in groups.items() if len(paths) > 1
    )
    return scanned, sets


def reference_hits(root: Path, tracked: list[str], sets: list) -> tuple[int, dict]:
    """Find literal basename mentions, explicitly ambiguous for repeated basenames."""
    if not sets:
        return 0, {}
    names = {Path(p).name for _, paths in sets for p in paths}
    pattern = re.compile(
        "|".join(re.escape(n) for n in sorted(names, key=lambda n: (-len(n), n)))
    )
    references = defaultdict(set)
    documents = 0
    for name in tracked:
        if not name.startswith(("docs/", "tests/")):
            continue
        path = root / name
        if path.suffix.lower() not in {".md", ".rst", ".html", ".txt", ".py"}:
            continue
        if path.is_symlink() or not path.is_file():
            continue
        if path.name in {"DUPLICATE_SETS.md", "duplicate_inventory.py"}:
            continue
        documents += 1
        with path.open(encoding="utf-8", errors="surrogateescape") as stream:
            for number, line in enumerate(stream, 1):
                for match in pattern.finditer(line):
                    references[match.group()].add(f"{name}:{number}")
    return documents, references


def write_table(output: Path, sets: list, references: dict) -> dict:
    """Write proposals with a basename join key into the separate reference table."""
    counts: dict[str, dict[str, int]] = defaultdict(
        lambda: {"sets": 0, "files": 0, "redundant": 0}
    )
    with output.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(
            [
                "set_digest",
                "family",
                "path",
                "proposed_canonical",
                "reference_basename",
                "reference_count",
            ]
        )
        for digest, paths in sets:
            group = family(paths)
            proposed = min(paths, key=lambda p: canonical_key(p, group))
            counts[group]["sets"] += 1
            counts[group]["files"] += len(paths)
            counts[group]["redundant"] += len(paths) - 1
            for path in paths:
                basename = Path(path).name
                reference_count = len(references.get(basename, ()))
                writer.writerow(
                    [
                        digest,
                        group,
                        path,
                        str(path == proposed).lower(),
                        basename,
                        reference_count,
                    ]
                )
    return dict(counts)


def write_reference_table(output: Path, references: dict) -> None:
    """Store each ambiguous basename/location pair once, in deterministic order.

    A join is a discovery candidate only; a basename does not resolve a caller
    to any particular copy. Missing basenames have no reference rows.
    """
    with output.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(["basename", "location"])
        for basename, locations in sorted(references.items()):
            for location in sorted(locations):
                writer.writerow([basename, location])


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[4]
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    root = args.root.resolve()
    output = args.output_dir.resolve()
    if output == root or root in output.parents:
        parser.error("output directory must be outside the checkout")
    output.mkdir(parents=True, exist_ok=True)
    tracked = tracked_paths(root)
    scanned, sets = duplicate_sets(root, tracked)
    print(
        f"Hashed {scanned} candidate YAML files; {len(sets)} duplicate sets", flush=True
    )
    documents, references = reference_hits(root, tracked, sets)
    table = output / "duplicate_sets.csv"
    counts = write_table(table, sets, references)
    reference_table = output / "duplicate_references.csv"
    write_reference_table(reference_table, references)
    result = {
        "schema_version": "2",
        "scanned": scanned,
        "sets": len(sets),
        "files": sum(len(paths) for _, paths in sets),
        "redundant": sum(len(paths) - 1 for _, paths in sets),
        "families": counts,
        "documents": documents,
        "table_bytes": table.stat().st_size,
        "reference_rows": sum(len(locations) for locations in references.values()),
        "reference_table_bytes": reference_table.stat().st_size,
    }
    text = json.dumps(result, indent=2) + "\n"
    (output / "duplicate_counts.json").write_text(text, encoding="utf-8", newline="")
    print(text)


if __name__ == "__main__":
    main()
