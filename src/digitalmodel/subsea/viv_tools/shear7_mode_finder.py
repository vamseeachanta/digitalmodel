"""Read Shear7 excited modes and the first mode's power-in node list.

Format basis: ModesFind.m and FindModes.m (2009), count/sentinel parsing and
NodeList output loops, inspected for the compatibility port in issue 1642.
No solver is imported. Counts and identifiers come from the result file;
no physical coefficients, thresholds or acceptance criteria are supplied.
"""

from __future__ import annotations

import argparse
import csv
from dataclasses import dataclass
import json
import math
from pathlib import Path
import re
import sys
from typing import Sequence


class ModeParseError(ValueError):
    """A required Shear7 section is missing, malformed or truncated."""


@dataclass(frozen=True)
class ModeResult:
    """One result; node IDs belong only to power_in_mode, as in ModesFind.m."""

    filename: str
    excited_modes: tuple[int, ...]
    power_in_mode: int | None
    power_in_nodes: tuple[int, ...]


# Source: ModesFind.m (2009), three sentinel strings and row formats.
_FLAGS = re.IGNORECASE | re.MULTILINE
_COUNT = re.compile(
    r"^\s*the\s+number\s+of\s+modes\s+above\s+cutoff\s+is\s*:\s*(\d+)\s*$",
    _FLAGS,
)
_REGION = re.compile(
    r"^\s*Mode\s+No\.\s+No\.\s+of\s+Nodes\s+Length\s+Ratio\s*$", _FLAGS
)
_MODE_LABEL = re.compile(r"^\s*mode\s+number\s*:\s*(\d+)\s*$", _FLAGS)
_NODES = re.compile(r"^\s*node\s+number\s+Cl\s+Fn/Fvo\s*$", _FLAGS)
_FLOAT = r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?"
_INT_ROW = re.compile(r"\s*(\d+)\s*")
_REGION_ROW = re.compile(rf"\s*(\d+)\s+(\d+)\s+({_FLOAT})\s*")
_NODE_ROW = re.compile(rf"\s*(\d+)\s+({_FLOAT})\s+({_FLOAT})\s*")


def _rows(text: str, count: int, pattern: re.Pattern, label: str) -> list[tuple]:
    """Read counted rows, allowing only known table decoration before data."""
    rows = []
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or re.fullmatch(r"[-=]+", stripped):
            continue
        if not rows and re.fullmatch(r"These\s+modes\s+are\s*:", stripped, re.I):
            continue
        match = pattern.fullmatch(line)
        if match is None:
            raise ModeParseError(f"Malformed or truncated {label} table")
        rows.append(match.groups())
        if len(rows) == count:
            return rows
    raise ModeParseError(f"Truncated {label} table: expected {count} rows")


def _after(text: str, pattern: re.Pattern, label: str) -> str:
    match = pattern.search(text)
    if match is None:
        raise ModeParseError(f"Missing {label} marker")
    return text[match.end() :]


def _finite(*values: str) -> bool:
    return all(
        math.isfinite(float(v.replace("D", "E").replace("d", "e"))) for v in values
    )


def parse_modes(text: str, *, filename: str = "") -> ModeResult:
    """Parse one output, preserving first-region selection from ModesFind.m.

    A declared zero-mode count is a valid empty result. Missing evidence is an
    error. Later modal regions are deliberately excluded from NodeList.
    """
    marker = _COUNT.search(text)
    if marker is None:
        raise ModeParseError("Missing or invalid above-cutoff mode count")
    count = int(marker.group(1))
    if count == 0:
        return ModeResult(filename, (), None, ())
    tail = text[marker.end() :]
    modes = tuple(int(row[0]) for row in _rows(tail, count, _INT_ROW, "mode"))
    if min(modes) <= 0 or len(set(modes)) != count:
        raise ModeParseError("Mode identifiers must be positive and unique")
    region_tail = _after(tail, _REGION, "modal excitation region")
    region = _rows(region_tail, 1, _REGION_ROW, "region")[0]
    mode, node_count = int(region[0]), int(region[1])
    if mode not in modes or not _finite(region[2]):
        raise ModeParseError("First region must belong to an excited mode")
    if node_count == 0:
        return ModeResult(filename, modes, mode, ())
    node_tail = _after(region_tail, _NODES, "power-in node")
    header = _NODES.search(region_tail)
    labels = list(_MODE_LABEL.finditer(region_tail[: header.start()]))
    if labels and int(labels[-1].group(1)) != mode:
        raise ModeParseError("Node table mode differs from first region mode")
    rows = _rows(node_tail, node_count, _NODE_ROW, "power-in node")
    nodes = tuple(int(row[0]) for row in rows)
    if min(nodes) <= 0 or len(set(nodes)) != node_count:
        raise ModeParseError("Node identifiers must be positive and unique")
    if not all(_finite(*row[1:]) for row in rows):
        raise ModeParseError("Power-in coefficients must be finite")
    return ModeResult(filename, modes, mode, nodes)


def _input_files(directory: Path, extension: str) -> tuple[Path, ...]:
    suffix = extension.removeprefix(".")
    if not re.fullmatch(r"[A-Za-z0-9_-]+", suffix):
        raise ValueError("extension must be a literal file suffix")
    if not directory.is_dir():
        raise NotADirectoryError("Input directory does not exist")
    files = tuple(
        sorted(
            p for p in directory.iterdir() if p.is_file() and p.suffix == f".{suffix}"
        )
    )
    if not files:
        raise FileNotFoundError("No matching result files")
    return files


def find_modes(
    directory: str | Path, extension: str = ".out"
) -> tuple[ModeResult, ...]:
    """Read sorted matching files without changing cwd or writing inputs."""
    return _read_files(_input_files(Path(directory), extension))


def _read_files(files: Sequence[Path]) -> tuple[ModeResult, ...]:
    results = []
    for path in files:
        try:
            results.append(
                parse_modes(path.read_text(encoding="utf-8-sig"), filename=path.name)
            )
        except ModeParseError as exc:
            raise ModeParseError(f"{path.name}: {exc}") from exc
    return tuple(results)


def node_list(results: Sequence[ModeResult]) -> tuple[tuple[int, ...], ...]:
    """Transpose node lists by ordinal position with legacy zero padding.

    Source: ModesFind.m (2009), NodeList(j,i) assignment and output loops.
    Zero is a missing-entry sentinel; this is not a node-membership matrix.
    """
    height = max((len(r.power_in_nodes) for r in results), default=0)
    return tuple(
        tuple(r.power_in_nodes[i] if i < len(r.power_in_nodes) else 0 for r in results)
        for i in range(height)
    )


def _write_exports(results: tuple[ModeResult, ...], output: Path, fmt: str) -> None:
    files = [r.filename for r in results]
    modes = {"files": files, "excited_modes": [list(r.excited_modes) for r in results]}
    power = {
        "files": files,
        "power_in_modes": [r.power_in_mode for r in results],
        "node_list": [list(row) for row in node_list(results)],
    }
    # Serialize both payloads before opening any destination (including YAML import).
    payloads = [modes, power]
    if fmt == "yaml":
        import yaml

        serialized = [yaml.safe_dump(p, sort_keys=False) for p in payloads]
    elif fmt == "json":
        serialized = [json.dumps(p, indent=2) + "\n" for p in payloads]
    else:
        serialized = []
    output.mkdir(parents=True, exist_ok=True)
    for index, name in enumerate(("ExcitedModes", "PowerInRegion")):
        with (output / f"{name}.{fmt}").open(
            "w", encoding="utf-8", newline=""
        ) as stream:
            if fmt != "csv":
                stream.write(serialized[index])
            elif index == 0:
                writer = csv.writer(stream)
                writer.writerow(("file", "excited_modes"))
                writer.writerows(
                    (r.filename, " ".join(map(str, r.excited_modes))) for r in results
                )
            else:
                writer = csv.writer(stream)
                writer.writerow(files)
                writer.writerows(node_list(results))


def main(argv: Sequence[str] | None = None) -> int:
    """Run the FindModes directory driver and export the two result tables."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--extension", default=".out")
    parser.add_argument(
        "--output-format", choices=("csv", "json", "yaml"), default="csv"
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        inputs = _input_files(args.directory, args.extension)
        targets = {
            args.output_dir / f"{name}.{args.output_format}"
            for name in ("ExcitedModes", "PowerInRegion")
        }
        if {p.resolve() for p in inputs} & {p.resolve() for p in targets}:
            raise ValueError("Export destinations must not overwrite input files")
        if any(t.exists() and t.samefile(p) for t in targets for p in inputs):
            raise ValueError("Export destinations must not overwrite input files")
        results = _read_files(inputs)
        _write_exports(results, args.output_dir, args.output_format)
    except (OSError, ValueError, ImportError) as exc:
        print(f"shear7-mode-finder: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
