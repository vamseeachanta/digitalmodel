#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Count the shards the detector selects for recent merges and for pull-request heads.

Read-only: it runs Git queries in the current checkout and prints one line per
commit. It applies the detector's own functions in the order ``main()`` applies
them, and reads record pins once per commit instead of once per call.

Columns, each a shard count:

* ``two-commit``: base tip against the head, the comparison before the fork-point change;
* ``fork-point``: fork point against the head, without the source route table;
* ``routed``: fork point against the head, with ``scripts/ci/src_domain_routes.json``;
* one further column per ``--table NAME=PATH``.

Use, in a checkout whose object store holds the base and heads::

    python scripts/ci/measure_shard_selection.py --base-ref origin/main --merges 50
    python scripts/ci/measure_shard_selection.py --heads heads.txt   # one "<label> <commit>" per line
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import detect_touched_domains as detector  # noqa: E402

PREFIX = "src/digitalmodel/"


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, text=True, capture_output=True).stdout.strip()


def units(changed: list[str]) -> str:
    """The source units a change touches, in the table's own granularity."""
    found = set()
    for path in changed:
        if path.startswith(PREFIX):
            parts = path[len(PREFIX):].split("/")
            found.add("/".join(parts[:2]) + "/" if len(parts) > 2 else parts[0] + ("/" if len(parts) > 1 else ""))
    return " ".join(sorted(found))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--base-ref", default="HEAD", help="base revision for merge and pull-request measurements")
    parser.add_argument("--merges", type=int, default=0, help="number of first-parent commits to measure")
    parser.add_argument("--heads", type=Path, help='file of "<label> <commit>" lines to measure against the base')
    parser.add_argument("--table", action="append", default=[], help="NAME=PATH of another route table")
    parser.add_argument("--domains-file", type=Path, default=Path("tests/DOMAINS.md"))
    args = parser.parse_args(argv)
    domains = detector.parse_domains(args.domains_file)
    tables = {"routed": detector.load_src_routes()}
    for item in args.table:
        name, _, path = item.partition("=")
        tables[name] = detector.load_src_routes(Path(path))
    pins_at: dict[str, dict[str, set[str]]] = {}

    def count(start: str, head: str, refs: tuple[str, ...], routes: detector.SrcRoutes | None) -> int:
        pins: dict[str, set[str]] = {}
        for ref in dict.fromkeys(refs):
            if ref not in pins_at:
                pins_at[ref] = detector.git_pinned_domains(ref, domains)
            for path, names in pins_at[ref].items():
                pins.setdefault(path, set()).update(names)
        changed = detector.git_changed_files(start, head)
        return len(detector.touched_domains(changed, domains, start, head, pinned_domains=pins,
                                            src_routes=routes))

    tip = git("rev-parse", "--verify", "--end-of-options", f"{args.base_ref}^{{commit}}")
    print(f"tip {tip} shards {len(domains)}", flush=True)
    for commit in git("rev-list", "--first-parent", "-n", str(args.merges), tip).split() if args.merges else ():
        parent = git("rev-parse", f"{commit}^")
        refs = (parent, commit)
        row = [f"unrouted {count(parent, commit, refs, None)}"]
        row += [f"{name} {count(parent, commit, refs, table)}" for name, table in tables.items()]
        touched = units(detector.git_changed_files(parent, commit))
        print(f"merge {commit[:8]} {' '.join(row)} src: {touched}", flush=True)
    for line in args.heads.read_text(encoding="utf-8").splitlines() if args.heads else ():
        label, head = line.split()
        head = git("rev-parse", head)
        fork = git("merge-base", tip, head)
        behind = git("rev-list", "--count", f"{head}..{tip}")
        row = [f"two-commit {count(tip, head, (tip, head), None)}",
               f"fork-point {count(fork, head, (tip, fork, head), None)}"]
        row += [f"{name} {count(fork, head, (tip, fork, head), table)}" for name, table in tables.items()]
        touched = units(detector.git_changed_files(fork, head))
        print(f"head {label} {head[:8]} behind {behind} {' '.join(row)} src: {touched}", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
