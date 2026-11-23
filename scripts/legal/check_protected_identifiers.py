#!/usr/bin/env python3
"""Fail-closed, byte-oriented scanner for protected identifiers (#1961, Stage 1).

Enumerates the complete tracked tree -- or the staged index, or a pinned
historical tree -- classifies every path, and matches class rules whose values
are supplied at run time and are never committed anywhere in this repository.

Stage 1 only. Without the rule authority this tool exercises synthetic values,
reports its authority as UNAUTHENTICATED, and asserts nothing about production
cleanliness. Its load-bearing verification is the retrospective corpus in
``verify_public_surface.sh``, which judges it against a leak population its
author did not construct.

Exit codes are distinct on purpose, so a skipped or vacuous run is visibly
different from a clean one:

  0  clean          1  findings          2  manifest schema error
  3  no authority   4  fail-closed (unclassified path, oversize artifact, IO)
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import protected_surface_ownership as ownership  # noqa: E402

EXIT_CLEAN, EXIT_FINDINGS, EXIT_SCHEMA, EXIT_NO_AUTHORITY, EXIT_FAIL_CLOSED = 0, 1, 2, 3, 4
COMMIT_MESSAGE_PATH = "<commit-message>"


class FailClosed(RuntimeError):
    """Ambiguity is never resolved in favour of a clean result."""


def _git(root: Path, *args: str) -> bytes:
    proc = subprocess.run(["git", *args], cwd=str(root), capture_output=True)
    if proc.returncode != 0:
        raise FailClosed(f"git {' '.join(args)}: {proc.stderr.decode('utf-8', 'replace').strip()}")
    return proc.stdout


class Source:
    """Where bytes come from: the worktree, the index, or a pinned tree."""

    def __init__(self, root: Path, ref: str | None, staged: bool) -> None:
        self.root, self.ref, self.staged = root, ref, staged

    def enumerate(self) -> list[str]:
        if self.ref:
            out = _git(self.root, "ls-tree", "-r", "-z", "--name-only", self.ref)
        else:
            out = _git(self.root, "ls-files", "-z")
        return sorted(p for p in out.decode("utf-8", "surrogateescape").split("\0") if p)

    def size(self, path: str) -> int:
        if self.ref or self.staged:
            spec = f"{self.ref}:{path}" if self.ref else f":{path}"
            return int(_git(self.root, "cat-file", "-s", spec).decode().strip())
        return os.lstat(self.root / path).st_size

    def read(self, path: str) -> bytes:
        if self.ref or self.staged:
            spec = f"{self.ref}:{path}" if self.ref else f":{path}"
            return _git(self.root, "cat-file", "blob", spec)
        target = self.root / path
        if target.is_symlink():
            # Never follow. The link text is bytes and is scanned; whatever the
            # link points at is outside this tree's responsibility and is not read.
            return os.readlink(target).encode("utf-8", "surrogateescape")
        with open(target, "rb") as handle:
            return handle.read()


def _load_rules(path: Path, manifest: dict) -> tuple[str, dict]:
    try:
        raw = json.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise LookupError(f"rule authority unavailable: {exc}") from exc
    rules = raw.get("rules")
    if not isinstance(rules, dict):
        raise LookupError("rule authority carries no rules")
    if set(rules) != set(manifest["rules"]):
        raise ownership.ManifestError(
            "rule values do not cover exactly the manifest's declared rule ids"
        )
    for rule_id, declared in manifest["rules"].items():
        values = rules[rule_id].get("values")
        expected = 1 if declared["class"] == "A" else 2
        if not isinstance(values, list) or len(values) != expected:
            raise ownership.ManifestError(
                f"rule {rule_id}: class {declared['class']} needs exactly {expected} value(s)"
            )
    authority = "AUTHENTICATED" if raw.get("authority") == "authenticated" else "UNAUTHENTICATED"
    return authority, rules


def _line_bounds(data: bytes, offset: int) -> tuple[int, bytes]:
    """Return the 1-based line number and the raw line containing ``offset``."""
    start = data.rfind(b"\n", 0, offset) + 1
    end = data.find(b"\n", offset)
    end = len(data) if end == -1 else end
    return data.count(b"\n", 0, start) + 1, data[start:end]


def _suppressed_spans(line: bytes, patterns: list[str]) -> list[tuple[int, int]]:
    text = line.decode("utf-8", "replace")
    spans = []
    for pattern in patterns:
        for match in re.finditer(pattern, text):
            spans.append((match.start(), match.end()))
    return spans


def _class_a_hits(data: bytes, value: str) -> list[int]:
    needle = value.lower().encode("utf-8")
    lowered = data.lower()
    offsets, start = [], 0
    while True:
        found = lowered.find(needle, start)
        if found == -1:
            return offsets
        offsets.append(found)
        start = found + 1


def _accept(data: bytes, offset: int, sentinel: bytes, patterns: list[str]) -> tuple[bool, int]:
    line_no, line = _line_bounds(data, offset)
    if sentinel in line:
        return False, line_no
    if patterns:
        column = offset - (data.rfind(b"\n", 0, offset) + 1)
        prefix = line[:column].decode("utf-8", "replace")
        for lo, hi in _suppressed_spans(line, patterns):
            if lo <= len(prefix) < hi:
                return False, line_no
    return True, line_no


def _scan_class_a(path: str, data: bytes, rule_id: str, value: str, ctx: dict) -> list[dict]:
    findings = []
    for offset in _class_a_hits(data, value):
        accepted, line_no = _accept(data, offset, ctx["sentinel"], ctx["patterns"])
        if accepted:
            findings.append(
                {"rule_id": rule_id, "class": "A", "path": path, "line": line_no,
                 "byte_offset": offset}
            )
    return findings


def _scan_class_b(path: str, data: bytes, rule_id: str, values: list[str], ctx: dict) -> list[dict]:
    """Same-file co-occurrence. No character or line window: any width would be
    a constant fitted to the data it judges."""
    lowered = data.lower()
    offsets = [lowered.find(v.lower().encode("utf-8")) for v in values]
    if any(o == -1 for o in offsets):
        return []
    first = min(offsets)
    accepted, line_no = _accept(data, first, ctx["sentinel"], ctx["patterns"])
    if not accepted:
        return []
    return [{"rule_id": rule_id, "class": "B", "path": path, "line": line_no,
             "byte_offset": first}]


def _scan_blob(path: str, data: bytes, manifest: dict, rules: dict, patterns: list[str]) -> list[dict]:
    ctx = {"sentinel": manifest["line_sentinel"].encode("utf-8"), "patterns": patterns}
    findings: list[dict] = []
    for rule_id, declared in sorted(manifest["rules"].items()):
        values = rules[rule_id]["values"]
        if declared["class"] == "A":
            findings.extend(_scan_class_a(path, data, rule_id, values[0], ctx))
        else:
            findings.extend(_scan_class_b(path, data, rule_id, values, ctx))
    return findings


def _classify_all(manifest: dict, paths: list[str]) -> dict[str, tuple[str, list[str]]]:
    classified = {}
    for path in paths:
        try:
            classified[path] = ownership.classify(manifest, path)
        except ownership.ManifestError as exc:
            raise FailClosed(str(exc)) from exc
    return classified


def _scan_paths(source: Source, manifest: dict, rules: dict, paths: list[str],
                classified: dict) -> list[dict]:
    limit = manifest["limits"]["max_file_bytes"]
    findings: list[dict] = []
    for path in paths:
        try:
            if source.size(path) > limit:
                raise FailClosed(f"artifact exceeds limits.max_file_bytes: {path}")
            data = source.read(path)
        except OSError as exc:
            raise FailClosed(f"unreadable artifact {path}: {exc}") from exc
        findings.extend(_scan_blob(path, data, manifest, rules, classified[path][1]))
    return findings


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--rules", required=True, type=Path)
    parser.add_argument("--root", type=Path, default=None)
    parser.add_argument("--ref", default=None, help="scan a pinned tree instead of the worktree")
    parser.add_argument("--staged", action="store_true", help="scan index blobs")
    parser.add_argument("--commit-message-file", type=Path, default=None)
    parser.add_argument("--print-enumeration", action="store_true")
    parser.add_argument("--enumerate-only", action="store_true")
    parser.add_argument("--json", action="store_true")
    return parser.parse_args(argv)


def _emit(report: dict, as_json: bool, code: int) -> int:
    if as_json:
        print(json.dumps(report, indent=2, sort_keys=True))
    else:
        print(f"authority: {report['authority']}")
        print(f"enumerated: {report['enumerated']}")
        print(f"findings: {len(report['findings'])}")
        for finding in report["findings"]:
            print(
                f"  {finding['rule_id']} class={finding['class']} "
                f"{finding['path']}:{finding['line']} offset={finding['byte_offset']}"
            )
        for error in report["errors"]:
            print(f"ERROR {error}", file=sys.stderr)
    return code


def _empty_report(authority: str) -> dict:
    return {"authority": authority, "enumerated": 0, "findings": [], "errors": []}


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(sys.argv[1:] if argv is None else argv)
    root = (args.root or Path.cwd()).resolve()

    try:
        manifest = ownership.load(args.manifest)
    except ownership.ManifestError as exc:
        report = _empty_report("UNKNOWN")
        report["errors"] = [str(exc)]
        return _emit(report, args.json, EXIT_SCHEMA)

    try:
        # D6a is authority-independent: the census must be cross-checkable with no
        # rule values at all, or the cross-check itself becomes authority-gated.
        authority, rules = (
            ("UNAUTHENTICATED", {})
            if args.enumerate_only and not args.rules.exists()
            else _load_rules(args.rules, manifest)
        )
    except LookupError as exc:
        report = _empty_report("UNAUTHENTICATED")
        report["errors"] = [str(exc)]
        return _emit(report, args.json, EXIT_NO_AUTHORITY)
    except ownership.ManifestError as exc:
        report = _empty_report("UNKNOWN")
        report["errors"] = [str(exc)]
        return _emit(report, args.json, EXIT_SCHEMA)

    report = _empty_report(authority)
    source = Source(root, args.ref, args.staged)
    try:
        paths = source.enumerate()
        classified = _classify_all(manifest, paths)
        report["enumerated"] = len(paths)
        if args.print_enumeration:
            report["enumeration"] = paths
            report["classification"] = {p: classified[p][0] for p in paths}
        if not args.enumerate_only:
            report["findings"] = _scan_paths(source, manifest, rules, paths, classified)
            if args.commit_message_file is not None:
                data = Path(args.commit_message_file).read_bytes()
                report["findings"].extend(
                    _scan_blob(COMMIT_MESSAGE_PATH, data, manifest, rules, [])
                )
    except (FailClosed, OSError) as exc:
        report["errors"] = [str(exc)]
        return _emit(report, args.json, EXIT_FAIL_CLOSED)

    return _emit(report, args.json, EXIT_FINDINGS if report["findings"] else EXIT_CLEAN)


if __name__ == "__main__":
    raise SystemExit(main())
