"""Fail when a text-only cleanup changed a number in a data file.

An identifier cleanup edits names, paths and header lines. It must not change
a single numeric value, yet a JSON file re-serialised during one changed two
floats in their last digit and a test run rewrote two timing artefacts. This
compares every changed or renamed JSON/YAML/CSV file between two refs, number
by number at the place the number sits:

* JSON and YAML are parsed and each numeric value is keyed by its path
  (``Bodies/[0]/Mass``). A swapped pair, a changed value, a deleted or an
  added numeric field all fail. Key order does not matter. YAML is read as
  YAML, so ``12_000`` is the number 12000.
* A redaction that renames a key (an object named after a vessel) passes
  only when every number is unchanged at the same position and each new key
  name is new to its mapping; a rename onto an existing name is a swap and
  fails. Such files are listed for review.
* CSV is keyed by row and column index, so swapped cells and deleted rows
  fail.
* Numbers inside a string value (``"Lifter 5000"``) are compared per key as a
  multiset. A digit inside a word or a path (``run3``, ``<drive>\\9999 job``)
  is not a number, which is where a redaction legitimately removes digits.
* A file that does not parse falls back to its ordered sequence of numeric
  tokens outside comment lines, which still fails on a swap or a deletion.

Files are paired across renames (``git diff -M``); added and deleted files are
listed but have no counterpart to compare.

Usage::

    python scripts/legal/check_numeric_text.py BASE HEAD [-- pathspec...]
    python scripts/legal/check_numeric_text.py BASE HEAD --allow-removed-integers

``--allow-removed-integers`` accepts only integers that disappear from inside
a string value (a crane model number dropped with the name that held it). A
removed numeric field or cell is never excused, nor is an added or changed
number or a removed decimal.

Exit status: 0 no change, 1 numeric change, 3 usage or git error.
"""

from __future__ import annotations

import argparse
import collections
import csv
import io
import json
import math
import re
import subprocess
import sys

import yaml

DATA_EXT = (".json", ".yaml", ".yml", ".csv")
#: A number standing on its own in text: not inside a word or a path.
NUM = re.compile(
    r"(?<![A-Za-z0-9_.\\/-])[-+]?(?:\d[\d_]*\.[\d_]*|\.\d[\d_]*|\d[\d_]*)"
    r"(?:[eE][-+]?\d+)?(?![A-Za-z_\\/])"
)
COMMENT = re.compile(r"(?m)^\s*#.*$")
CELL_NUM = re.compile(r"\s*[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?\s*")
#: The C parser when PyYAML has it: solver YAML runs to megabytes. It resolves
#: scalars exactly as SafeLoader does.
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)


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


class _Blobs:
    """``git cat-file --batch``: one process for every blob read."""

    def __init__(self) -> None:
        self.proc = None

    def read(self, spec: str) -> bytes:
        if self.proc is None:
            self.proc = subprocess.Popen(
                ["git", "cat-file", "--batch"],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
            )
        self.proc.stdin.write(spec.encode("utf-8", "surrogateescape") + b"\n")
        self.proc.stdin.flush()
        header = self.proc.stdout.readline().split()
        if len(header) != 3 or header[1] != b"blob":
            print(f"check_numeric_text: cannot read {spec}", file=sys.stderr)
            sys.exit(3)
        size = int(header[2])
        data = self.proc.stdout.read(size)
        self.proc.stdout.read(1)  # the newline after the content
        return data


def _token(t: str) -> str:
    """A text token in canonical form: underscores are digit separators."""
    return t.replace("_", "")


def _value(v) -> str:
    """A parsed number as text that keeps its type and every digit."""
    if isinstance(v, float) and math.isnan(v):
        return "nan"
    return repr(v)


class View:
    """Numbers by location: ``fields`` maps a location to a numeric value,
    ``embedded`` to the multiset of number tokens inside a string value."""

    def __init__(self) -> None:
        self.fields: dict[str, str] = {}
        self.embedded: dict[str, collections.Counter] = {}

    def string(self, where: str, s: str) -> None:
        found = collections.Counter(_token(t) for t in NUM.findall(s))
        if found:
            self.embedded[where] = found


class Numbers:
    """The numbers of one file version.

    ``named`` locates each number by key names (``Vessels/v1/X``);
    ``placed`` by position alone (``#0/#3/#1``), which is what stays fixed
    when a redaction renames a key. ``keys`` holds each mapping's key list by
    position. ``sequence`` is set instead when the file did not parse.
    """

    def __init__(self) -> None:
        self.named = View()
        self.placed = View()
        self.keys: dict[str, list[str]] = {}
        self.sequence: list[str] | None = None

    def string(self, named: str, placed: str, s: str) -> None:
        self.named.string(named, s)
        self.placed.string(placed, s)

    def field(self, named: str, placed: str, value: str) -> None:
        self.named.fields[named] = value
        self.placed.fields[placed] = value

    def walk(self, node, named: str, placed: str) -> None:
        if isinstance(node, bool) or node is None:
            return
        if isinstance(node, (int, float)):
            self.field(named, placed, _value(node))
        elif isinstance(node, str):
            self.string(named, placed, node)
        elif isinstance(node, dict):
            self.keys[placed] = [str(k) for k in node]
            for i, (k, v) in enumerate(node.items()):
                key = str(k)
                # A number used as a key is a field too, located by itself.
                if isinstance(k, (int, float)) and not isinstance(k, bool):
                    self.field(
                        f"{named}/<key {key}>", f"{placed}/<key #{i}>", _value(k)
                    )
                elif isinstance(k, str):
                    self.string(f"{named}/<key {key}>", f"{placed}/<key #{i}>", key)
                self.walk(v, f"{named}/{key}", f"{placed}/#{i}")
        elif isinstance(node, (list, tuple)):
            for i, v in enumerate(node):
                self.walk(v, f"{named}/[{i}]", f"{placed}/[{i}]")
        else:  # dates and other scalars YAML resolves: compare as text
            self.string(named, placed, str(node))


def _fallback(text: str) -> Numbers:
    n = Numbers()
    n.sequence = [_token(t) for t in NUM.findall(COMMENT.sub("", text))]
    return n


def numbers(path: str, text: str) -> Numbers:
    ext = path.lower().rsplit(".", 1)[-1]
    n = Numbers()
    try:
        if ext == "json":
            n.walk(json.loads(text), "", "")
        elif ext in ("yaml", "yml"):
            for i, doc in enumerate(yaml.load_all(text, Loader=_YAML_LOADER)):
                n.walk(doc, f"doc{i}", f"doc{i}")
        else:
            for r, row in enumerate(csv.reader(io.StringIO(text))):
                for c, cell in enumerate(row):
                    where = f"row {r + 1} col {c + 1}"
                    if CELL_NUM.fullmatch(cell):
                        n.field(where, where, cell.strip())
                    else:
                        n.string(where, where, cell)
    except (ValueError, yaml.YAMLError, csv.Error, RecursionError):
        return _fallback(text)
    return n


def _is_integer(token: str) -> bool:
    return re.fullmatch(r"[-+]?\d+", token) is not None


def _renames_only(a: Numbers, b: Numbers) -> int | None:
    """The number of keys renamed, when renaming is the only structural
    change: the same mappings with the same key counts, and every renamed
    key new to its mapping -- a key renamed to a name its mapping already
    had is a swap, which moves numbers between fields. None otherwise."""
    if a.keys.keys() != b.keys.keys():
        return None
    renamed = 0
    for where, ka in a.keys.items():
        kb = b.keys[where]
        if len(ka) != len(kb):
            return None
        sa, sb = set(ka), set(kb)
        for x, y in zip(ka, kb):
            if x != y:
                if y in sa or x in sb:
                    return None
                renamed += 1
    return renamed


def compare(
    a: Numbers, b: Numbers, allow_removed_integers: bool
) -> tuple[list[str], int]:
    """Differences between two versions, and the number of keys renamed
    with every number left in place. No differences: no number changed."""
    if a.sequence is not None or b.sequence is not None:
        sa, sb = a.sequence, b.sequence
        if sa is None or sb is None:
            return ["one version parses and the other does not"], 0
        if sa == sb:
            return [], 0
        return [f"number sequence differs ({len(sa)} -> {len(sb)} numbers)"], 0
    diffs = _compare_view(a.named, b.named, allow_removed_integers)
    if not diffs:
        return [], 0
    renamed = _renames_only(a, b)
    if renamed and not _compare_view(a.placed, b.placed, allow_removed_integers):
        return [], renamed
    return diffs, 0


def _compare_view(a: View, b: View, allow_removed_integers: bool) -> list[str]:
    out: list[str] = []
    for where in sorted(set(a.fields) | set(b.fields)):
        va, vb = a.fields.get(where), b.fields.get(where)
        if va == vb:
            continue
        if vb is None:
            out.append(f"{where or '/'}: {va} removed")
        elif va is None:
            out.append(f"{where or '/'}: {vb} added")
        else:
            out.append(f"{where or '/'}: {va} -> {vb}")
    for where in sorted(set(a.embedded) | set(b.embedded)):
        ea = a.embedded.get(where, collections.Counter())
        eb = b.embedded.get(where, collections.Counter())
        added, removed = eb - ea, ea - eb
        if not added and not removed:
            continue
        if (
            allow_removed_integers
            and not added
            and all(_is_integer(t) for t in removed)
        ):
            continue
        out.append(
            f"{where or '/'} (in text): added {sorted(added.elements())[:6]} "
            f"removed {sorted(removed.elements())[:6]}"
        )
    return out


def pairs(base: str, head: str, pathspec: list[str]):
    """(old path, new path) of modified and renamed data files, and the
    data files added or deleted, from ``git diff -M``."""
    raw = _git(
        [
            "diff",
            "--name-status",
            "-M",
            "-z",
            base,
            head,
            "--",
            *pathspec,
        ]
    ).decode("utf-8", "surrogateescape")
    fields = raw.split("\0")
    compared: list[tuple[str, str]] = []
    unpaired: list[str] = []
    i = 0
    while i < len(fields) and fields[i]:
        status = fields[i]
        if status[0] in "RC":
            old, new = fields[i + 1], fields[i + 2]
            i += 3
        else:
            old = new = fields[i + 1]
            i += 2
        is_data = old.lower().endswith(DATA_EXT) or new.lower().endswith(DATA_EXT)
        if not is_data:
            continue
        if status[0] in "MRCT":
            compared.append((old, new))
        else:
            unpaired.append(f"{status[0]} {new}")
    return compared, unpaired


def main() -> int:
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError):
            pass
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("base")
    ap.add_argument("head")
    ap.add_argument("pathspec", nargs="*")
    ap.add_argument("--allow-removed-integers", action="store_true")
    args = ap.parse_args()

    for ref in (args.base, args.head):
        _git(["rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"])
    compared, unpaired = pairs(args.base, args.head, args.pathspec)
    changed: list[str] = []
    rekeyed: list[str] = []
    blobs = _Blobs()
    for old, new in compared:
        ta = blobs.read(f"{args.base}:{old}").decode("utf-8", "replace")
        tb = blobs.read(f"{args.head}:{new}").decode("utf-8", "replace")
        a, b = numbers(old, ta), numbers(new, tb)
        if (a.sequence is None) != (b.sequence is None):
            # One version does not parse: compare both as token sequences.
            a, b = _fallback(ta), _fallback(tb)
        diffs, keys_renamed = compare(a, b, args.allow_removed_integers)
        label = new if old == new else f"{old} -> {new}"
        if diffs:
            changed.append(f"{label}:\n      " + "\n      ".join(diffs[:8]))
            if len(diffs) > 8:
                changed[-1] += f"\n      ... {len(diffs) - 8} more"
        elif keys_renamed:
            rekeyed.append(f"{label}: {keys_renamed} renamed key(s)")
    renamed = sum(1 for o, n in compared if o != n)
    print(
        f"check_numeric_text: {len(compared)} changed data file(s) compared "
        f"({renamed} renamed)"
    )
    if rekeyed:
        # Listed, not failed: the new key names are for review, the numbers
        # are identical at every position.
        print(
            f"check_numeric_text: {len(rekeyed)} file(s) renamed key(s) with "
            f"every number unchanged in place:"
        )
        for r in rekeyed:
            print(f"  {r}")
    if unpaired:
        print(
            f"check_numeric_text: {len(unpaired)} data file(s) added or deleted "
            f"with no counterpart to compare:"
        )
        for u in unpaired:
            print(f"  {u}")
    if changed:
        print(f"check_numeric_text: {len(changed)} file(s) changed a number:")
        for c in changed:
            print(f"  {c}")
        return 1
    print("check_numeric_text: no numeric change")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
