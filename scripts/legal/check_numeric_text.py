"""Fail when a text-only cleanup changed a number in a data file.

An identifier cleanup edits names, paths and header lines. It must not change
a single numeric value, yet a JSON file re-serialised during one changed two
floats in their last digit and a test run rewrote two timing artefacts. This
compares, per changed JSON/YAML/CSV file, the multiset of numeric tokens
between two refs.

A token counts as data unless it sits inside a word or a path (``run3``,
``<drive>\\9999 job``) or on a comment line (``# File: ...``), which is where a
redaction legitimately removes digits.

Usage::

    python scripts/legal/check_numeric_text.py BASE HEAD [-- pathspec...]
    python scripts/legal/check_numeric_text.py BASE HEAD --allow-removed-integers

``--allow-removed-integers`` accepts integers that disappear without
replacement (a crane model number dropped with the name that held it); an
added or changed number, or a removed decimal, still fails.

Exit status: 0 no change, 1 numeric change, 3 usage or git error.
"""

from __future__ import annotations

import argparse
import collections
import re
import subprocess
import sys

DATA_EXT = (".json", ".yaml", ".yml", ".csv")
NUM = re.compile(
    r"(?<![A-Za-z0-9_.\\/-])[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?"
    r"(?![A-Za-z_\\/])"
)
COMMENT = re.compile(r"(?m)^\s*#.*$")


def _git(args: list[str]) -> bytes:
    out = subprocess.run(["git", *args], capture_output=True)
    if out.returncode != 0:
        print(
            f"check_numeric_text: `git {' '.join(args)}` failed: "
            f"{out.stderr.decode(errors='replace').strip()}",
            file=sys.stderr,
        )
        sys.exit(3)
    return out.stdout


def numbers(text: str) -> collections.Counter:
    return collections.Counter(NUM.findall(COMMENT.sub("", text)))


def _is_integer(token: str) -> bool:
    return re.fullmatch(r"[-+]?\d+", token) is not None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("base")
    ap.add_argument("head")
    ap.add_argument("pathspec", nargs="*")
    ap.add_argument("--allow-removed-integers", action="store_true")
    args = ap.parse_args()

    for ref in (args.base, args.head):
        _git(["rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"])
    listing = _git(
        [
            "diff",
            "--name-only",
            "--no-renames",
            "--diff-filter=M",
            "-z",
            args.base,
            args.head,
            "--",
            *args.pathspec,
        ]
    )
    paths = [
        p
        for p in listing.decode("utf-8", "surrogateescape").split("\0")
        if p and p.lower().endswith(DATA_EXT)
    ]
    changed: list[str] = []
    for p in paths:
        a = numbers(_git(["show", f"{args.base}:{p}"]).decode("utf-8", "replace"))
        b = numbers(_git(["show", f"{args.head}:{p}"]).decode("utf-8", "replace"))
        if a == b:
            continue
        added, removed = b - a, a - b
        if (
            args.allow_removed_integers
            and not added
            and all(_is_integer(t) for t in removed)
        ):
            continue
        changed.append(
            f"{p}: added {sorted(added.elements())[:6]} "
            f"removed {sorted(removed.elements())[:6]}"
        )
    print(f"check_numeric_text: {len(paths)} changed data file(s) compared")
    if changed:
        print(f"check_numeric_text: {len(changed)} file(s) changed a number:")
        for c in changed:
            print(f"  {c}")
        return 1
    print("check_numeric_text: no numeric change")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
