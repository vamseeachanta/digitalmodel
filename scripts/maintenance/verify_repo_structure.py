# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate tracked and unignored paths against the repository contract.

The checker reads Git paths and YAML only. It never moves or removes a file.
By default, unignored new files are checked too, so proposed additions cannot
avoid validation merely because they have not been staged.
"""

from __future__ import annotations

import argparse
import datetime
import fnmatch
import re
import subprocess
from pathlib import Path, PurePosixPath

import yaml

FIELDS = ("owner", "reason", "category", "reviewed_on",
          "follow_up_issue_or_permanent_justification")
PLACEHOLDER = re.compile(r"\b(todo|tbd|placeholder)\b", re.I)


def matches(path, pattern):
    """Match recursive patterns, including zero intermediate directories."""
    return (fnmatch.fnmatchcase(path, pattern)
            or ("/**/" in pattern and fnmatch.fnmatchcase(path, pattern.replace("/**/", "/"))))


def valid_path(path):
    """Accept repository-relative normalized POSIX paths only."""
    return (isinstance(path, str) and bool(path) and not path.startswith("/")
            and "\\" not in path and ".." not in path.split("/")
            and "." not in path.split("/") and "//" not in path)


def validate_config(config):
    """Reject malformed rules and incomplete or placeholder exceptions."""
    if not isinstance(config, dict) or config.get("version") != 1:
        return ["config: schema-version"]
    errors = []
    for field in ("allowed_tracked_notebooks", "generated_patterns", "ci_artifact_patterns",
                  "forbidden_patterns", "durable_exceptions", "required_metadata_for_exceptions"):
        if not isinstance(config.get(field), list):
            errors.append(f"config: {field}")
    root = config.get("root_allowlist", {})
    if not isinstance(root, dict) or any(not isinstance(root.get(key), list)
                                         for key in ("files", "directories")):
        errors.append("config: root-allowlist")
    if errors:
        return errors
    for key in ("files", "directories"):
        if any(not valid_path(value) or "/" in value for value in root[key]):
            errors.append("config: root-allowlist-path")
    for field in ("allowed_tracked_notebooks", "generated_patterns", "ci_artifact_patterns",
                  "forbidden_patterns"):
        if any(not valid_path(value) for value in config[field]):
            errors.append(f"config: {field}-path")
    if set(config["required_metadata_for_exceptions"]) != set(FIELDS):
        errors.append("config: exception-required-fields")
    for number, row in enumerate(config["durable_exceptions"], 1):
        label = f"exception[{number}]"
        if not isinstance(row, dict):
            errors.append(f"{label}: metadata")
            continue
        if not valid_path(row.get("pattern")):
            errors.append(f"{label}: pattern")
        for field in FIELDS:
            value = row.get(field)
            if (not isinstance(value, str) or not value.strip() or PLACEHOLDER.search(value)
                    or value.strip().casefold() in {"pending", "unknown", "unassigned"}):
                errors.append(f"{label}: {field}")
        try:
            datetime.date.fromisoformat(row.get("reviewed_on", ""))
        except (ValueError, TypeError):
            errors.append(f"{label}: reviewed_on-date")
        follow = row.get("follow_up_issue_or_permanent_justification", "")
        category = row.get("category")
        if category == "temporary_durable_generated_evidence":
            if not isinstance(follow, str) or not re.fullmatch(
                r"(?:#[1-9]\d*|https://github\.com/[^/\s]+/[^/\s]+/issues/[1-9]\d*)", follow
            ):
                errors.append(f"{label}: concrete-follow-up")
        elif category == "permanent_durable_generated_evidence":
            if not isinstance(follow, str) or not follow.startswith("permanent: ") or len(follow) < 40:
                errors.append(f"{label}: permanent-justification")
        else:
            errors.append(f"{label}: category")
    return sorted(set(errors))


def check_paths(paths, config):
    """Report every path rule violation, allowing only validated exceptions."""
    if errors := validate_config(config):
        return errors
    errors = []
    for path in sorted(set(paths)):
        if not valid_path(path):
            errors.append("<invalid-path>: relative-path")
            continue
        if any(matches(path, p) for p in config["forbidden_patterns"]):
            errors.append(f"{path}: forbidden-path")
        generated = any(matches(path, p) for p in
                        config["generated_patterns"] + config["ci_artifact_patterns"])
        excepted = any(matches(path, row["pattern"]) for row in config["durable_exceptions"])
        if generated and not excepted:
            errors.append(f"{path}: generated-tracked-path")
        if path.endswith(".ipynb") and path not in config["allowed_tracked_notebooks"]:
            errors.append(f"{path}: notebook-allowlist")
        root = PurePosixPath(path).parts[0]
        if "/" in path:
            allowed = root in config["root_allowlist"]["directories"] or (generated and excepted)
        else:
            allowed = path in config["root_allowlist"]["files"]
        if not allowed:
            errors.append(f"{path}: root-allowlist")
    return errors


def git_paths(repo):
    """Read index and unignored additions with NUL-safe Git output."""
    result = subprocess.run(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
                            cwd=repo, check=True, capture_output=True)
    return [path for path in result.stdout.decode("utf-8").split("\0") if path]


def main():
    """Exit nonzero on contract violations, naming paths and rules only."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--config", type=Path)
    args = parser.parse_args()
    config_path = args.config or args.repo / "config/repo_structure.yml"
    try:
        config = yaml.safe_load(config_path.read_text(encoding="utf-8"))
        paths = git_paths(args.repo)
        errors = check_paths(paths, config)
    except (OSError, subprocess.CalledProcessError, yaml.YAMLError):
        parser.exit(2, "structure: cannot read repository paths or contract\n")
    if errors:
        for error in errors:
            print(error)
        return 1
    print(f"repository structure OK: {len(set(paths))} paths")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
