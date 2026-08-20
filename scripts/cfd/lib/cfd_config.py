#!/usr/bin/env python3
"""Read the CFD chain case registry for the shell scripts.

The chain is bash, but its configuration is YAML (repo convention: config
belongs in a reviewable .yml, not baked into a script). This helper is the
single reader, so the shell never parses YAML by hand.

Usage:
    cfd_config.py <config.yml> cases                 -> newline-separated names
    cfd_config.py <config.yml> get <case> <key>      -> one value, or defaults
    cfd_config.py <config.yml> decompose <ranks>     -> "n n n"

Exit 1 with a message on stderr when a case or key is unknown; the shell
treats any non-zero exit as fatal, so an unknown case fails closed rather
than silently solving the wrong thing.
"""
from __future__ import annotations

import sys
from pathlib import Path

import yaml


def _load(path: str) -> dict:
    p = Path(path)
    if not p.is_file():
        sys.exit(f"cfd_config: no such config: {path}")
    return yaml.safe_load(p.read_text()) or {}


def main(argv: list[str]) -> int:
    if len(argv) < 3:
        sys.exit(__doc__)
    cfg = _load(argv[1])
    cmd = argv[2]
    cases = {c["name"]: c for c in cfg.get("cases", [])}
    defaults = cfg.get("defaults", {})

    if cmd == "cases":
        print("\n".join(cases))
        return 0

    if cmd == "get":
        if len(argv) != 5:
            sys.exit("cfd_config: usage: get <case> <key>")
        name, key = argv[3], argv[4]
        if name not in cases:
            sys.exit(f"cfd_config: unknown case {name!r}; "
                     f"known: {', '.join(cases) or '(none)'}")
        val = cases[name].get(key, defaults.get(key))
        if val is None:
            sys.exit(f"cfd_config: case {name!r} has no key {key!r} "
                     "and no default")
        print(val)
        return 0

    if cmd == "decompose":
        if len(argv) != 4:
            sys.exit("cfd_config: usage: decompose <ranks>")
        table = defaults.get("decompose_n", {})
        ranks = int(argv[3])
        vec = table.get(ranks)
        if vec is None:
            sys.exit(f"cfd_config: no decomposition vector for {ranks} ranks; "
                     f"known: {sorted(table)}")
        if vec[0] * vec[1] * vec[2] != ranks:
            sys.exit(f"cfd_config: decompose_n {vec} does not multiply to "
                     f"{ranks}; decomposePar would exit fatally")
        print(" ".join(str(v) for v in vec))
        return 0

    sys.exit(f"cfd_config: unknown command {cmd!r}")


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
