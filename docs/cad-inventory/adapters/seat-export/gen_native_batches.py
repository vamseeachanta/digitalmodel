#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Generate private CAD batch lists using caller-supplied storage locations.

Only path lists and a manifest are written; no CAD application is invoked.
The index and generated lists contain private paths and stay outside public Git.
"""

import argparse
import collections
import csv
import json
from pathlib import Path, PurePosixPath, PureWindowsPath
import re


# Format groups and queue ranks retain the original adapter's workflow rules;
# these are classification labels, not engineering constants.
SW = {"sldprt", "sldasm"}
INV = {"ipt", "iam"}
OTHER_SEAT = {"prt", "catpart", "x_t", "x_b", "f3d", "f3z"}


def _windows_component(value):
    if not value or re.search(r'[<>:"|?*\\\x00-\x1f]', value):
        raise ValueError("source path contains a non-Windows filename component")


def _roots(source_root, windows_root):
    source = PurePosixPath(source_root)
    if not source.is_absolute() or ".." in source.parts:
        raise ValueError("source root must be an absolute POSIX path without traversal")
    if re.search(r"[\x00-\x1f]", str(source_root)):
        raise ValueError("source root must not contain control characters")
    target = PureWindowsPath(windows_root)
    if not target.is_absolute() or re.search(r"[\x00-\x1f]", str(windows_root)):
        raise ValueError("Windows root must be an absolute drive or UNC path")
    if ".." in target.parts:
        raise ValueError("Windows root must not contain traversal")
    for part in target.parts[1:]:
        _windows_component(part)
    return source, target


def winpath(path, source_root, windows_root):
    """Map a contained POSIX path by components, never by string replacement."""
    source, target = _roots(source_root, windows_root)
    candidate = PurePosixPath(path)
    if ".." in candidate.parts:
        raise ValueError("source path must not contain traversal")
    relative = candidate.relative_to(source)
    if not relative.parts:
        raise ValueError("source path must name a file beneath the source root")
    for part in relative.parts:
        _windows_component(part)
    return str(target.joinpath(*relative.parts))


def safe(name):
    # Retain the original adapter's 80-character filename stem convention.
    return re.sub(r"[^A-Za-z0-9._-]", "_", name)[:80]


def prio(project):
    """Retain the original curated-first, archive-last queue order."""
    if "ri-hoard" in project:
        return 9
    if project.startswith("digitalmodel/subsea-risers"):
        return 1
    if "preexisting" in project:
        return 7
    if project.startswith("docs/"):
        return 2
    return 3


def _read_groups(index, source_root, windows_root):
    groups = collections.defaultdict(lambda: collections.defaultdict(list))
    with Path(index).open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        required = {"path", "format", "readability", "project"}
        columns = reader.fieldnames or []
        if not required.issubset(columns) or len(columns) != len(set(columns)):
            raise ValueError("index has missing or duplicate columns")
        for row in reader:
            if row["readability"] != "seat-only":
                continue
            extension = row["format"]
            tag = "sw" if extension in SW else "inv" if extension in INV else "other"
            if extension not in SW | INV | OTHER_SEAT:
                continue
            project = row["project"]
            if not project or re.search(r"[\x00-\x1f]", project):
                raise ValueError("index has an empty or multiline project group")
            if not row["path"]:
                raise ValueError("index has an empty source path")
            groups[project][tag].append(winpath(row["path"], source_root, windows_root))
    return groups


def _prepare_batches(groups):
    manifest, files = [], {}
    names = set()
    for project in sorted(groups, key=lambda value: (prio(value), value)):
        base = f"{prio(project)}_{safe(project)}"
        # Lists are consumed on Windows, where filenames are case-insensitive.
        if base.casefold() in names:
            raise ValueError("project group filename collision")
        names.add(base.casefold())
        record = {"project": project, "priority": prio(project)}
        for tag in ("sw", "inv", "other"):
            paths = groups[project].get(tag, [])
            record[tag] = len(paths)
            if paths:
                name = f"{base}.{tag}.txt"
                files[name] = ("\r\n".join(paths) + "\r\n").encode("utf-8")
                record[tag + "_file"] = name
        manifest.append(record)
    totals = {
        tag: sum(record[tag] for record in manifest) for tag in ("sw", "inv", "other")
    }
    totals["projects"] = len(manifest)
    return {"totals": totals, "batches": manifest}, files


def generate_batches(index, output_dir, source_root, windows_root):
    """Validate all selected rows before writing into a new output directory."""
    _roots(source_root, windows_root)
    groups = _read_groups(index, source_root, windows_root)
    manifest, files = _prepare_batches(groups)
    files["_manifest.json"] = (json.dumps(manifest, indent=1) + "\n").encode("utf-8")
    output = Path(output_dir)
    # A fresh directory preserves prior runs and prevents partial overwrites.
    output.mkdir(parents=True, exist_ok=False)
    for name, content in files.items():
        with (output / name).open("xb") as stream:
            stream.write(content)
    return manifest


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--index", required=True, type=Path, help="private readability TSV"
    )
    parser.add_argument(
        "--output-dir", required=True, type=Path, help="new private output directory"
    )
    parser.add_argument(
        "--source-root", required=True, help="POSIX archive root used in index paths"
    )
    parser.add_argument(
        "--windows-root", required=True, help="mapped drive or UNC archive root"
    )
    args = parser.parse_args(argv)
    try:
        manifest = generate_batches(
            args.index, args.output_dir, args.source_root, args.windows_root
        )
    except (OSError, ValueError, csv.Error) as error:
        # OS exceptions can include private input paths; expose the error class only.
        parser.exit(
            1,
            f"batch generation failed ({type(error).__name__}); check private inputs\n",
        )
    print(json.dumps(manifest["totals"], indent=1))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
