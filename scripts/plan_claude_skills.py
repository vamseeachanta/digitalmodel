# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regenerate relative skill pointers, or preview them with --dry-run."""

from __future__ import annotations

import argparse
import datetime
import json
import os
from pathlib import Path

# Issue #2009, Scope: these three pointers and one YYYYMMDD propagation marker.
NAMES = ("guidelines", "meta", "workflows")


def skill_destination(repo, skills_dir=None):
    """Reject redirects and conflicting parents before creating directories."""
    destination = Path(os.path.abspath(skills_dir or repo / ".claude/skills"))
    if destination == repo or not destination.is_relative_to(repo):
        raise ValueError("--skills-dir must be a subdirectory of the repository")
    for parent in (destination, *destination.parents):
        if parent == repo:
            break
        if parent.is_symlink() or (parent.exists() and not parent.is_dir()):
            raise ValueError("existing skill directory requires manual review")
    return destination


def pointer_plan(repo, hub, date, skills_dir=None):
    """Check all sources and the calendar date before returning the recipe."""
    repo = repo.resolve()
    internal = hub.resolve() / ".claude/skills/_internal"
    if not repo.is_dir():
        raise ValueError("repository directory is absent")
    missing = [name for name in NAMES if not (internal / name).is_dir()]
    if missing:
        raise ValueError(
            "Clone workspace-hub as a sibling, or select its checkout with "
            "--workspace-hub; internal skills missing: " + ", ".join(missing)
        )
    # Issue #2009, Scope: YYYYMMDD has eight ASCII digits and a valid date.
    if len(date) != 8 or not date.isascii() or not date.isdigit():
        raise ValueError("date must use YYYYMMDD")
    try:
        datetime.datetime.strptime(date, "%Y%m%d")
    except ValueError as error:
        raise ValueError("date must be a valid calendar date in YYYYMMDD") from error
    destination = skill_destination(repo, skills_dir)
    relative_destination = destination.relative_to(repo)
    artifacts = [
        {
            "path": (relative_destination / name).as_posix(),
            "kind": "symlink",
            "target": os.path.relpath(internal / name, destination),
        }
        for name in NAMES
    ]
    artifacts.append(
        {
            "path": (relative_destination / ".ecosystem-propagated").as_posix(),
            "kind": "file",
            "content": date + "\n",
        }
    )
    return {"mode": "plan", "artifacts": artifacts}


def apply_plan(plan, destination):
    """Preflight all entries, then repair links without modifying their targets."""
    for row in plan["artifacts"]:
        path = destination / Path(row["path"]).name
        if row["kind"] == "symlink":
            if path.is_symlink():
                continue
            if path.exists():
                raise ValueError(
                    "existing pointer entry requires manual review: " + path.name
                )
        elif path.is_symlink() or (path.exists() and not path.is_file()):
            raise ValueError("existing propagation marker requires manual review")
    destination.mkdir(parents=True, exist_ok=True)
    for row in plan["artifacts"]:
        path = destination / Path(row["path"]).name
        if row["kind"] == "symlink":
            if path.is_symlink():
                if os.readlink(path) == row["target"]:
                    continue
                path.unlink()
            path.symlink_to(row["target"], target_is_directory=True)
        else:
            path.write_text(row["content"], encoding="utf-8")


def main():
    """Create the four artifacts by default; keep explicit --apply compatible."""
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=root)
    parser.add_argument("--workspace-hub", type=Path)
    parser.add_argument("--date", default=datetime.date.today().strftime("%Y%m%d"))
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--apply", action="store_true", help="create pointers (the default)"
    )
    mode.add_argument(
        "--dry-run", action="store_true", help="print the recipe without writes"
    )
    parser.add_argument(
        "--skills-dir", type=Path, help="destination inside the repository"
    )
    args = parser.parse_args()
    repo = args.repo.resolve()
    hub = args.workspace_hub or repo.parent / "workspace-hub"
    try:
        plan = pointer_plan(repo, hub, args.date, args.skills_dir)
        if not args.dry_run:
            apply_plan(plan, skill_destination(repo, args.skills_dir))
            plan["mode"] = "apply"
    except (ValueError, OSError) as error:
        parser.exit(1, f"skill pointers: {error}\n")
    print(json.dumps(plan, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
