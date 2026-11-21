#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Record which files under ``src/digitalmodel`` a test shard reads while it runs.

The record is the evidence behind ``scripts/ci/src_domain_routes.json``: a shard
that never opens a file of a package cannot change its result when that package
changes, so a change to the package need not run that shard.

``install`` writes a start-up hook into the site-packages folder of the
interpreter that runs it. From then on every Python process of that environment,
including test subprocesses and workers, appends to ``<out>/<pid>.txt`` the path
of each file it opens and each folder it lists under the source root. The hook
uses the interpreter's audit events ``open``, ``os.scandir`` and ``os.listdir``,
so imports, data files read through Python and walks over the tree are all seen.

Not seen: a file opened by compiled code that bypasses Python's ``open``, a
process started with ``-S``, and a process of another interpreter.

Use, once per shard, in a clean checkout::

    uv run --no-sources python scripts/ci/trace_src_reads.py install --out <dir>/<domain>
    <the shard's gate command>

and afterwards ``python scripts/ci/derive_src_routes.py --reads <dir> --commit <sha> --write``.
"""

from __future__ import annotations

import argparse
import os
import sysconfig
from pathlib import Path

HOOK_NAME = "_dm_src_read_trace"
HOOK = '''\
"""Start-up hook written by scripts/ci/trace_src_reads.py; delete both files to remove."""
import os
import sys

SRC = {src!r}
OUT = {out!r}
_seen = set()
_sink = [0, -1]


def _note(path, listing):
    try:
        if path is None:
            path = "."
        elif isinstance(path, int):
            return
        path = os.fsdecode(path)
        if "digitalmodel" not in path and os.path.isabs(path):
            return
        full = os.path.abspath(path)
        for root in SRC:
            if full == root or full.startswith(root + os.sep):
                break
        else:
            return
        rel = full[len(root) + 1:].replace(os.sep, "/") + ("/" if listing else "")
        if rel in _seen:
            return
        _seen.add(rel)
        pid = os.getpid()
        if _sink[0] != pid:
            _sink[0] = pid
            _sink[1] = os.open(os.path.join(OUT, "%d.txt" % pid),
                               os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o644)
        os.write(_sink[1], (rel + "\\n").encode("utf-8", "replace"))
    except Exception:
        pass


def _hook(event, args):
    if event == "open":
        _note(args[0], False)
    elif event == "os.scandir" or event == "os.listdir":
        _note(args[0], True)


sys.addaudithook(_hook)
'''


def install(out: Path, src: Path, site: Path) -> Path:
    """Write the hook module and its ``.pth`` loader into ``site``."""
    out.mkdir(parents=True, exist_ok=True)
    module = site / f"{HOOK_NAME}.py"
    roots = tuple(dict.fromkeys((os.path.abspath(src), str(src.resolve()))))  # as typed, and with links resolved
    module.write_text(HOOK.format(src=roots, out=str(out.resolve())), encoding="utf-8")
    (site / f"{HOOK_NAME}.pth").write_text(f"import {HOOK_NAME}\n", encoding="utf-8")
    return module


def read_records(reads: Path) -> dict[str, set[str]]:
    """Shard name -> recorded paths, from ``<reads>/<shard>/<pid>.txt``."""
    found: dict[str, set[str]] = {}
    for folder in sorted(p for p in reads.iterdir() if p.is_dir()):
        paths: set[str] = set()
        for record in sorted(folder.glob("*.txt")):
            paths.update(line for line in record.read_text(encoding="utf-8").splitlines() if line)
        found[folder.name] = paths
    return found


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="action", required=True)
    setup = sub.add_parser("install", help="write the hook into this interpreter's site-packages")
    setup.add_argument("--out", type=Path, required=True, help="folder for this shard's records")
    setup.add_argument("--src", type=Path, default=Path("src/digitalmodel"))
    args = parser.parse_args(argv)
    module = install(args.out, args.src, Path(sysconfig.get_paths()["purelib"]))
    print(f"read trace hook installed: {module}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
