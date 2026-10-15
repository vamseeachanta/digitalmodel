# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Plan relative skill pointers; apply only when explicitly requested."""

from __future__ import annotations

import argparse
import datetime
import json
import os
from pathlib import Path

NAMES = ("guidelines", "meta", "workflows")


def pointer_plan(repo, hub, date, skills_dir=None):
    """Check all source directories before returning a four-artifact recipe."""
    repo = repo.resolve()
    internal = hub.resolve() / ".claude/skills/_internal"
    destination = skills_dir or repo / ".claude/skills"
    if not repo.is_dir():
        raise ValueError("repository directory is absent")
    missing = [name for name in NAMES if not (internal / name).is_dir()]
    if missing:
        raise ValueError("Clone workspace-hub with its internal skills; missing: "
                         + ", ".join(missing))
    if len(date) != 8 or not date.isdigit():
        raise ValueError("date must use YYYYMMDD")
    datetime.datetime.strptime(date, "%Y%m%d")
    artifacts = [
        {"path": f".claude/skills/{name}", "kind": "symlink",
         "target": os.path.relpath(internal / name, destination)}
        for name in NAMES
    ]
    artifacts.append({"path": ".claude/skills/.ecosystem-propagated", "kind": "file",
                      "content": date + "\n"})
    return {"mode": "plan", "artifacts": artifacts}



def apply_plan(plan, destination):
    """Create pointers after preflight; preserve any conflicting user entry."""
    if destination.is_symlink():
        raise ValueError("existing skill directory symlink requires manual review")
    for row in plan["artifacts"]:
        path = destination / Path(row["path"]).name
        if row["kind"] == "symlink":
            if path.is_symlink() and os.readlink(path) == row["target"]:
                continue
            if path.exists() or path.is_symlink():
                raise ValueError("existing pointer entry requires manual review: " + path.name)
        elif path.is_symlink() or (path.exists() and not path.is_file()):
            raise ValueError("existing propagation marker requires manual review")
    destination.mkdir(parents=True, exist_ok=True)
    for row in plan["artifacts"]:
        path = destination / Path(row["path"]).name
        if row["kind"] == "symlink":
            if not path.is_symlink():
                path.symlink_to(row["target"], target_is_directory=True)
        else:
            path.write_text(row["content"], encoding="utf-8")


def main():
    """Print the recipe, optionally creating its four artifacts after preflight."""
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=root)
    parser.add_argument("--workspace-hub", type=Path)
    parser.add_argument("--date", default=datetime.date.today().strftime("%Y%m%d"))
    parser.add_argument("--apply", action="store_true", help="create the planned pointers")
    parser.add_argument("--skills-dir", type=Path, help="explicit destination for isolated fixtures")
    args = parser.parse_args()
    hub = args.workspace_hub or args.repo.resolve().parent / "workspace-hub"
    try:
        plan = pointer_plan(args.repo, hub, args.date, args.skills_dir)
        if args.apply:
            apply_plan(plan, args.skills_dir or args.repo.resolve() / ".claude/skills")
    except (ValueError, OSError) as error:
        parser.exit(1, f"skill pointers: {error}\n")
    print(json.dumps(plan, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
