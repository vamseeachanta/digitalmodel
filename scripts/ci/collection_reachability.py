# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Observe pytest collection in an explicitly bounded, trusted fixture tree.

Collection imports code. This tool is an observer, not a sandbox; only reviewed
synthetic roots are qualified for use until a real-tree import audit is complete.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    import pytest


def inventory(root: Path) -> tuple[list[Path], list[str]]:
    """Do not follow links, including file links and linked test roots."""
    def read_error(error: OSError) -> None:
        raise error

    files, links = [], []
    for directory, dirs, names in os.walk(root, followlinks=False, onerror=read_error):
        base = Path(directory)
        for name in sorted(dirs + names):
            path = base / name
            if path.is_symlink():
                links.append(path.relative_to(root).as_posix())
            elif path.is_file():
                files.append(path)
        dirs[:] = [name for name in dirs if not (base / name).is_symlink()]
    return sorted(files), sorted(links)


def source_digest(root: Path, files: list[Path]) -> str:
    """Bind Python sources and pytest configuration to their relative names."""
    digest = hashlib.sha256()
    for path in files:
        if path.suffix == ".py" or path.name in {
            "pytest.ini", ".pytest.ini", "pyproject.toml", "tox.ini", "setup.cfg",
            "pytest.toml", ".pytest.toml",
        }:
            data = path.read_bytes()
            digest.update(path.relative_to(root).as_posix().encode() + b"\0")
            digest.update(str(len(data)).encode() + b"\0" + data)
    return digest.hexdigest()


class Observation:
    """Capture items before deselection, without parsing display output."""

    def __init__(self, root: Path, output: Path) -> None:
        """Initialize empty observation state for one selected root."""
        self.root = root
        self.output = output
        self.data: dict = {"nodes": [], "errors": [], "skips": [], "completed": False}

    def pytest_configure(self, config: pytest.Config) -> None:
        """Record the effective configuration used by this collection."""
        self.data["configuration"] = {
            name: list(config.getini(name))
            for name in ("python_files", "testpaths", "norecursedirs")
        }
        self.data["configuration"]["root_matches"] = config.rootpath == self.root
        self.data["configuration"]["config_file"] = (
            config.inipath.relative_to(self.root).as_posix() if config.inipath else None
        )

    def pytest_itemcollected(self, item: pytest.Item) -> None:
        """Use item paths, which are independent of parameter-ID syntax."""
        self.data["nodes"].append({
            "nodeid": item.nodeid.replace(str(self.root), "[root]"),
            "path": item.path.relative_to(self.root).as_posix(),
        })

    def pytest_collectreport(self, report: pytest.CollectReport) -> None:
        """Retain dispositions without serializing raw exception payloads."""
        if report.failed or report.skipped:
            key = "errors" if report.failed else "skips"
            self.data[key].append(report.nodeid.replace(str(self.root), "[root]"))

    def pytest_sessionfinish(self, session: pytest.Session, exitstatus: int) -> None:
        """Write completion evidence even when collection failed."""
        self.data.update(completed=True, exitstatus=int(exitstatus))
        self.output.write_text(json.dumps(self.data), encoding="utf-8")


def collect(root: Path, output: Path) -> int:
    """Run inside a child interpreter rooted at the selected fixture."""
    import pytest

    return int(pytest.main([
        "--collect-only", "--rootdir", str(root), "--confcutdir", str(root),
        "-p", "no:cacheprovider", "-q",
    ], plugins=[Observation(root, output)]))


def run_child(root: Path, output: Path, timeout: float) -> tuple[dict, list[str]]:
    """Capture bounded child execution; fresh evidence cannot reuse a report."""
    from _pytest.config.findpaths import locate_config

    _, config_file, _ = locate_config(root, [root])
    if config_file is not None and not config_file.resolve().is_relative_to(root):
        return {}, ["configuration_outside_root"]
    with tempfile.TemporaryDirectory(prefix="collection-", dir=output.parent) as directory:
        scratch = Path(directory)
        evidence = scratch / "evidence.json"
        env = {key: value for key, value in os.environ.items()
               if not key.startswith(("GIT_", "PYTEST_")) and key != "PYTHONPATH"}
        env.update(PYTHONDONTWRITEBYTECODE="1", PYTEST_DISABLE_PLUGIN_AUTOLOAD="1")
        try:
            with (scratch / "stdout.txt").open("wb") as stdout, (
                scratch / "stderr.txt"
            ).open("wb") as stderr:
                child = subprocess.run([
                    sys.executable, str(Path(__file__).resolve()), "--_collect", str(root),
                    str(evidence),
                ], cwd=root, env=env, stdout=stdout, stderr=stderr, timeout=timeout,
                    creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        except subprocess.TimeoutExpired:
            return {}, ["child_timeout"]
        except OSError:
            return {}, ["child_start_failed"]
        if not evidence.is_file() or evidence.is_symlink():
            return {"child_exit": child.returncode}, ["missing_evidence"]
        try:
            data = json.loads(evidence.read_text(encoding="utf-8"))
            validate_evidence(data)
        except (OSError, UnicodeError, ValueError, KeyError, TypeError):
            return {"child_exit": child.returncode}, ["invalid_evidence"]
        reasons = []
        if child.returncode not in (0, 5) or data["exitstatus"] != child.returncode:
            reasons.append("child_failed")
        data["child_exit"] = child.returncode
        return data, reasons


def relative_path(value: object) -> bool:
    """Require normalized, nonempty relative item/configuration paths."""
    return (isinstance(value, str) and bool(value) and "\\" not in value
            and ":" not in value and not value.startswith("/")
            and all(part not in ("", ".", "..") for part in value.split("/")))


def validate_evidence(data: dict) -> None:
    """Reject incomplete or malformed evidence rather than supplying defaults."""
    if (not isinstance(data, dict) or data["completed"] is not True
            or type(data["exitstatus"]) is not int):
        raise ValueError("incomplete evidence")
    config = data["configuration"]
    if not isinstance(config, dict) or config["root_matches"] is not True:
        raise ValueError("wrong root")
    for key in ("python_files", "testpaths", "norecursedirs"):
        if not isinstance(config[key], list) or not all(
            isinstance(pattern, str) and pattern for pattern in config[key]
        ):
            raise ValueError("invalid configuration")
    if not config["python_files"] or (config["config_file"] is not None
                                     and not relative_path(config["config_file"])):
        raise ValueError("unqualified configuration")
    if not isinstance(data["nodes"], list):
        raise ValueError("invalid items")
    for node in data["nodes"]:
        if (not isinstance(node, dict) or not relative_path(node["path"])
                or not isinstance(node["nodeid"], str) or not node["nodeid"]):
            raise ValueError("invalid item")
    for key in ("errors", "skips"):
        if not isinstance(data[key], list) or not all(
            isinstance(item, str) for item in data[key]
        ):
            raise ValueError("invalid collection dispositions")


def observe(root: Path, output: Path, timeout: float) -> dict:
    """Compare independently enumerated candidates with collected item paths."""
    files, links = inventory(root)
    before = source_digest(root, files)
    data, reasons = ({}, ["untraversed_links"]) if links else run_child(root, output, timeout)
    after_files, after_links = inventory(root)
    after = source_digest(root, after_files)
    if after != before:
        reasons.append("source_changed")
    if after_links and "untraversed_links" not in reasons:
        reasons.append("untraversed_links")
    from _pytest.pathlib import fnmatch_ex

    patterns = data.get("configuration", {}).get("python_files", [])
    candidates = sorted({
        path.parent.relative_to(root).as_posix()
        for path in files if path.is_relative_to(root / "tests")
        and any(fnmatch_ex(pattern, Path(path.name))
                for pattern in patterns)
    })
    nodes = data.get("nodes", [])
    observed = {Path(node["path"]).parent.as_posix() for node in nodes}
    missing = sorted(set(candidates) - observed)
    for condition, reason in ((not candidates, "empty_candidates"),
                              (not nodes, "zero_nodes"),
                              (missing, "unreachable_directories"),
                              (data.get("errors"), "collection_errors")):
        if condition:
            reasons.append(reason)
    return dict(data, schema_version=1, nodes=nodes, candidates=candidates,
                observed_directories=sorted(observed), missing=missing,
                incomplete_links=sorted(set(links + after_links)), reasons=reasons,
                source_sha256=before, source_sha256_after=after,
                status="fail" if reasons else "pass")


def shareable(value: Any, root: Path) -> Any:
    """Redact locators, including drive prefixes and paths in parameter IDs."""
    if isinstance(value, dict):
        return {key: shareable(item, root) for key, item in value.items()}
    if isinstance(value, list):
        return [shareable(item, root) for item in value]
    if isinstance(value, str):
        text = value.replace(str(root), "[root]").replace(root.as_posix(), "[root]")
        return re.sub(r'''(?<![\w.*\]])(?:[A-Za-z]:|/)[^\s\[\]"']*''',
                      "[path removed]", text)
    return value


def main() -> int:
    """Require explicit source, output and caller-controlled fixture boundary."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", required=True, type=Path)
    parser.add_argument("--boundary", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--timeout", type=float, default=30)
    args = parser.parse_args()
    root, boundary, output = (path.resolve() for path in (
        args.root, args.boundary, args.output,
    ))
    if (not root.is_dir() or not boundary.is_dir() or not root.is_relative_to(boundary)
            or not output.is_relative_to(boundary) or output.is_relative_to(root)
            or not output.parent.is_dir() or output.is_dir()):
        parser.error("source and output must be bounded; output must be outside source")
    if not math.isfinite(args.timeout) or not 0 < args.timeout <= 300:
        parser.error("timeout must be finite and between zero and 300 seconds")
    try:
        report = observe(root, output, args.timeout)
    except OSError:
        report = {"schema_version": 1, "status": "fail", "reasons": ["source_io_error"]}
    output.write_text(json.dumps(shareable(report, root), indent=2, sort_keys=True)
                      + "\n", encoding="utf-8")
    return 0 if report["status"] == "pass" else 1


if __name__ == "__main__":
    if len(sys.argv) == 4 and sys.argv[1] == "--_collect":
        raise SystemExit(collect(Path(sys.argv[2]), Path(sys.argv[3])))
    raise SystemExit(main())
