#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build and check ``scripts/ci/src_domain_routes.json``, the measured source routes.

The table says, for each unit under ``src/digitalmodel``, which test shards read
it. The shard detector uses it so that a change inside one package selects the
shards that read that package instead of every shard.

A unit is a top-level module file, a top-level package folder, or a folder one
level below a package (``solvers/orcaflex/``). Two sources of evidence, no others:

1. Recorded reads. ``scripts/ci/trace_src_reads.py`` records every file a shard
   opens and every folder it lists under the source root while the shard runs.
   A shard reads a unit when it opened or listed anything inside it.
2. Names in test files. A shard also reads a unit that a Python file under its
   test roots names as ``digitalmodel.<unit>``, ``from digitalmodel[.<unit>]
   import <unit>`` or the path ``src/digitalmodel/<unit>``. This needs no trace,
   so ``--check`` runs it on every change and fails when a test starts to name
   a unit whose row lacks its shard.

Units without a route, which keep the full matrix:

* read by every shard (the shared core);
* read by no shard: nothing shows who tests it;
* not in the table: added after the trace (a new subfolder takes its package's row).

A shard that did not run to the end of its tests is counted as reading every unit.

Use::

    python scripts/ci/derive_src_routes.py --check
    python scripts/ci/derive_src_routes.py --write                       # add newly named units
    python scripts/ci/derive_src_routes.py --reads <dir> --commit <sha> \\
        --results <result.json> --write                                  # rebuild from a new trace
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import detect_touched_domains as detector  # noqa: E402
import trace_src_reads  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
SRC = "src/digitalmodel"
EVERY = "shared core: every shard reads it"
NOBODY = "no shard opened it during the trace"
TRUSTED = ("pass", "fail")  # the tests ran to the end, so the record is complete
DOTTED = re.compile(r"\bdigitalmodel\.([A-Za-z_]\w*)(?:\.([A-Za-z_]\w*))?")
PATHED = re.compile(r"src[/\\]+digitalmodel[/\\]+([A-Za-z_]\w*)(\.py)?(?:[/\\]+([A-Za-z_]\w*))?")
FROM = re.compile(r"^[ \t]*from[ \t]+digitalmodel(?:\.([A-Za-z_]\w*))?[ \t]+import[ \t]+(\([^)]*\)|[^\n]*)",
                  re.MULTILINE)


def keys_for(record: str) -> list[str]:
    """Unit keys for one recorded path (a trailing slash marks a folder listing)."""
    listing = record.endswith("/")
    parts = [part for part in record.split("/") if part]
    if "__pycache__" in parts:  # a compiled file stands for its source file
        at = parts.index("__pycache__")
        rest = parts[at + 1:]
        parts = parts[:at] + ([rest[-1].split(".")[0] + ".py"] if rest else [])
        listing = not rest
    if not parts:
        return []
    if len(parts) == 1:
        return [parts[0] + ("/" if listing else "")]
    keys = [parts[0] + "/"]
    if len(parts) > 2 or listing:
        keys.append(f"{parts[0]}/{parts[1]}/")
    return keys


def disk_units(root: Path) -> set[str]:
    """Every unit present in the tree."""
    found: set[str] = set()
    for entry in (root / SRC).iterdir():
        if entry.name == "__pycache__":
            continue
        if entry.is_dir():
            found.add(entry.name + "/")
            found.update(f"{entry.name}/{child.name}/" for child in entry.iterdir()
                         if child.is_dir() and child.name != "__pycache__")
        else:
            found.add(entry.name)
    return found


def named_units(root: Path, domains: list) -> dict[str, set[str]]:
    """Shard -> units that the Python files under its test roots name."""
    units = disk_units(root)

    def keys(first: str | None, second: str | None) -> set[str]:
        found: set[str] = set()
        if first and first + "/" in units:
            found.add(first + "/")
            if second and f"{first}/{second}/" in units:
                found.add(f"{first}/{second}/")
        elif first and first + ".py" in units:
            found.add(first + ".py")
        return found

    named: dict[str, set[str]] = {}
    for domain in domains:
        files: set[Path] = set()
        for item in domain.roots:
            matches = list(root.glob(item))
            if not matches:
                raise ValueError(f"{domain.name}: test root {item!r} matched nothing")
            for path in matches:
                if path.is_dir():
                    files.update(path.rglob("*.py"))
                elif path.suffix == ".py":
                    files.add(path)
        found: set[str] = set()
        for file in sorted(files):
            text = file.read_text(encoding="utf-8", errors="replace")
            if "digitalmodel" not in text:
                continue
            for first, second in DOTTED.findall(text):
                found |= keys(first, second)
            for first, is_file, second in PATHED.findall(text):
                found |= keys(first, None if is_file else second)
            for package, imported in FROM.findall(text):
                for name in re.findall(r"[A-Za-z_]\w*", re.sub(r"\bas\s+\w+", "", imported)):
                    found |= keys(package, name) if package else keys(name, None)
        named[domain.name] = found
    return named


def assemble(readers: dict[str, set[str]], names: list[str], trace: dict) -> dict:
    """Turn unit -> shards into the table: partial rows route, the rest are listed."""
    routes: dict[str, list[str]] = {}
    unrouted: dict[str, str] = {}
    for unit in sorted(readers):
        route = sorted(readers[unit])
        if not route:
            unrouted[f"{SRC}/{unit}"] = NOBODY
        elif route == sorted(names):
            unrouted[f"{SRC}/{unit}"] = EVERY
        else:
            routes[f"{SRC}/{unit}"] = route
    return {"trace": trace, "routes": routes, "unrouted": unrouted}


def close_parents(readers: dict[str, set[str]]) -> None:
    """A package row holds every shard that reads one of its subfolders."""
    for unit, shards in readers.items():
        parent = unit.split("/")[0] + "/"
        if unit.count("/") == 2 and parent in readers:
            readers[parent] |= shards


def build(root: Path, reads: dict[str, set[str]], commit: str, shards: dict[str, str]) -> dict:
    """Build the table from recorded reads plus the units the test files name."""
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    names = [domain.name for domain in domains]
    units = disk_units(root)
    readers: dict[str, set[str]] = {unit: set() for unit in units}
    for shard, records in reads.items():
        if shard not in names:
            continue
        for record in records:
            for key in keys_for(record):
                if key in readers:
                    readers[key].add(shard)
    for shard, found in named_units(root, domains).items():
        for key in found:
            readers[key].add(shard)
    untraced = {name for name in names
                if not reads.get(name) or shards.get(name) not in TRUSTED}
    for shards_of_unit in readers.values():
        if shards_of_unit:
            shards_of_unit |= untraced
    close_parents(readers)
    return assemble(readers, names, {"commit": commit, "shards": dict(sorted(shards.items()))})


def resolved_row(table: detector.SrcRoutes, unit: str) -> str | None:
    """The table row the detector uses for a unit, when that row is a route."""
    candidates = [f"{SRC}/{unit}"]
    if unit.count("/") == 2:
        candidates.append(f"{SRC}/{unit.split('/')[0]}/")
    for candidate in candidates:
        if candidate in table:
            return candidate if table[candidate] is not None else None
    return None


def gaps(root: Path, payload: dict) -> list[tuple[str, str]]:
    """(shard, row) pairs where a test file names a unit whose row lacks the shard."""
    table: detector.SrcRoutes = {key: None for key in payload.get("unrouted", {})}
    table.update({key: tuple(route) for key, route in payload["routes"].items()})
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    found: set[tuple[str, str]] = set()
    for shard, units in named_units(root, domains).items():
        for unit in units:
            row = resolved_row(table, unit)
            if row is not None and shard not in table[row]:
                found.add((shard, row))
    return sorted(found)


def drift(root: Path, payload: dict) -> list[str]:
    return [f"{shard} names {row}, whose route lacks it" for shard, row in gaps(root, payload)]


def with_named_units(root: Path, payload: dict) -> dict:
    """Add each shard to the rows of the units its test files name."""
    names = [domain.name for domain in detector.parse_domains(root / "tests/DOMAINS.md")]
    prefix = len(SRC) + 1
    readers = {key[prefix:]: set(route) for key, route in payload["routes"].items()}
    for shard, row in gaps(root, payload):
        readers[row[prefix:]].add(shard)
    kept = {key: reason for key, reason in payload.get("unrouted", {}).items()}
    for unit in list(readers):  # a package row that is already unrouted stays so
        parent = unit.split("/")[0] + "/"
        if unit.count("/") == 2 and parent not in readers and f"{SRC}/{parent}" not in kept:
            readers[parent] = set()
    close_parents(readers)
    table = assemble(readers, names, payload.get("trace", {}))
    table["unrouted"] = dict(sorted({**kept, **table["unrouted"]}.items()))
    return table


def render(payload: dict) -> str:
    """One row per line, keys sorted, so a regenerated table reads as a row-by-row difference."""
    def rows(mapping: dict) -> str:
        body = ",\n".join(f"  {json.dumps(key)}: {json.dumps(mapping[key])}" for key in sorted(mapping))
        return "{\n" + body + "\n }" if mapping else "{}"

    return ("{\n"
            f' "trace": {json.dumps(payload.get("trace", {}), sort_keys=True)},\n'
            f' "routes": {rows(payload["routes"])},\n'
            f' "unrouted": {rows(payload.get("unrouted", {}))}\n'
            "}\n")


def shard_states(results: list[Path]) -> dict[str, str]:
    """Shard -> state from local gate result files; a non-test exit counts as ``error``."""
    states: dict[str, str] = {}
    for path in results:
        for module in json.loads(path.read_text(encoding="utf-8"))["modules"]:
            crashed = module["status"] == "fail" and module.get("code") != 1
            states[module["name"]] = "error" if crashed else module["status"]
    return states


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--reads", type=Path, help="folder of <shard>/<pid>.txt records from a trace")
    parser.add_argument("--commit", help="commit the trace ran on")
    parser.add_argument("--results", type=Path, action="append", default=[],
                        help="result.json of the run that made the trace (repeatable)")
    parser.add_argument("--write", action="store_true", help="write the table")
    parser.add_argument("--check", action="store_true", help="exit 1 when a test names a unit outside its row")
    args = parser.parse_args(argv)
    target = ROOT / detector.SRC_ROUTES_PATH
    if args.reads:
        if not args.commit:
            parser.error("--reads needs --commit")
        payload = build(ROOT, trace_src_reads.read_records(args.reads), args.commit,
                        shard_states(args.results))
    else:
        payload = json.loads(target.read_text(encoding="utf-8"))
        found = drift(ROOT, payload)
        if args.check or not args.write:
            print("\n".join(found) if found else "source routes cover every unit the tests name")
            if found:
                print("run: python scripts/ci/derive_src_routes.py --write")
            return 1 if found and args.check else 0
        payload = with_named_units(ROOT, payload)
    if args.write:
        target.write_text(render(payload), encoding="utf-8", newline="\n")
    else:
        sys.stdout.write(render(payload))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
