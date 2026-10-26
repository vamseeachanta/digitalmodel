# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read structural INP topology and references without loading a solver."""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass, field
import math
from pathlib import Path


@dataclass(frozen=True)
class Finding:
    """A gate finding with explicit entities and a corrective action."""

    severity: str
    code: str
    entity_ids: tuple[str, ...]
    message: str
    fix_hint: str
    line: int = 0


@dataclass
class Block:
    """A keyword, its parameters and its non-comment data records."""

    keyword: str
    params: dict[str, str]
    rows: list[list[str]]
    line: int


# Connectivity widths including midside nodes: CalculiX 2.22 User's Manual,
# section 6.2 (Element Types), https://www.dhondt.de/ccx_2.22.pdf.
# ROTARYI/DASHPOT2 are legacy reader aliases, not qualified CalculiX types;
# their property cards remain blocking below. Unknown types fail closed.
ELEMENT_NODES = {
    "C3D4": 4, "C3D10": 10, "C3D8": 8, "C3D8R": 8,
    "C3D20": 20, "C3D20R": 20, "C3D6": 6, "C3D15": 15,
    "CPS3": 3, "CPS6": 6, "CPS4": 4, "CPS4R": 4, "CPS8": 8,
    "CPS8R": 8, "CPE3": 3, "CPE6": 6, "CPE4": 4, "CPE4R": 4,
    "CPE8": 8, "CPE8R": 8, "CAX3": 3, "CAX4": 4, "CAX4R": 4,
    "CAX6": 6, "CAX8": 8, "CAX8R": 8, "S3": 3, "S4": 4,
    "S4R": 4, "S6": 6, "S8": 8, "S8R": 8, "B31": 2,
    "B31R": 2, "B32": 3, "B32R": 3, "T3D2": 2, "T3D3": 3,
    "MASS": 1, "ROTARYI": 1, "SPRING2": 2, "DASHPOT2": 2,
}
MASS_TYPES = {"MASS", "ROTARYI"}
# Compatible families within ELEMENT_NODES: CalculiX 2.22 User's Manual,
# sections 7.3 (*BEAM SECTION), 7.81 (*MASS), 7.117 (*SHELL SECTION),
# 7.118 (*SOLID SECTION), 7.121 (*SPRING), and 6.2.15-18 (membranes).
# Empty tuples retain detection of unqualified legacy property syntax without
# assigning elements; no supported membrane topology is currently read.
SECTION_KEYWORDS = {
    "SOLID SECTION": ("C3D", "CPS", "CPE", "CAX", "T3D"),
    "SHELL SECTION": ("S3", "S4", "S6", "S8"),
    "BEAM SECTION": ("B31", "B32"),
    "BEAM GENERAL SECTION": (), "MEMBRANE SECTION": (),
    "MASS": ("MASS",), "ROTARY INERTIA": (),
    "SPRING": ("SPRING2",), "DASHPOT": (),
}
DEFINITIONS = {
    "MATERIAL": ("material", "NAME"),
    "SURFACE": ("surface", "NAME"),
    "AMPLITUDE": ("amplitude", "NAME"),
    "SURFACE INTERACTION": ("interaction", "NAME"),
}


def number(value: str) -> float:
    """Accept solver exponent notation and reject nonfinite numeric data."""
    result = float(value.upper().replace("D", "E"))
    if not math.isfinite(result):
        raise ValueError("Nonfinite numeric data")
    return result


def positive_id(value: str) -> int:
    """Require positive integer entity identifiers."""
    result = int(value)
    if result <= 0:
        raise ValueError("Entity identifiers must be positive")
    return result


def read_blocks(path: Path, active: tuple[Path, ...] = ()) -> list[Block]:
    """Expand relative includes, detecting cycles and keyword continuations."""
    path = path.resolve()
    if path in active:
        raise ValueError("Cyclic include")
    # Local resource limit, not a solver limit: reserve Python recursion
    # headroom so excessive nesting becomes a finding rather than a traceback.
    if len(active) >= 64:
        raise ValueError("Include nesting exceeds 64 files")
    lines = path.read_text(encoding="utf-8").removeprefix("\ufeff").splitlines()
    result: list[Block] = []
    current: Block | None = None
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        i += 1
        if not line or line.startswith("**"):
            continue
        if line.startswith("*"):
            start = i
            while line.endswith(",") and i < len(lines):
                following = lines[i].strip()
                if not following or following.startswith("**"):
                    i += 1
                    continue
                if following.startswith("*") or "=" not in following:
                    break
                line += following
                i += 1
            fields = [v.strip() for v in line[1:].split(",")]
            if not fields[0]:
                raise ValueError("Empty keyword")
            keyword = " ".join(fields[0].upper().split())
            params = {}
            for value in fields[1:]:
                if not value:
                    continue
                name, sep, data = value.partition("=")
                params[name.strip().upper()] = data.strip().strip("\"'") if sep else ""
            current = Block(keyword, params, [], start)
            if keyword == "INCLUDE":
                target = params.get("INPUT")
                if not target:
                    raise ValueError("Include requires INPUT")
                result.extend(read_blocks(path.parent / target, (*active, path)))
                current = None
            else:
                result.append(current)
        elif current is not None:
            current.rows.append([v.strip() for v in line.split(",")])
        else:
            raise ValueError("Data outside a keyword block")
    return result


@dataclass
class Deck:
    """Resolved mesh, definitions and reference usage for a structural deck."""

    blocks: list[Block]
    findings: list[Finding] = field(default_factory=list)
    nodes: dict[int, tuple[float, ...]] = field(default_factory=dict)
    elements: dict[int, tuple[str, tuple[int, ...]]] = field(default_factory=dict)
    sets: dict[str, dict[str, list[str]]] = field(default_factory=lambda: {"nset": {}, "elset": {}})
    definitions: dict[str, set[str]] = field(default_factory=lambda: {
        "material": set(), "surface": set(), "amplitude": set(), "interaction": set(),
    })
    used: set[tuple[str, str]] = field(default_factory=set)
    resolved: dict[tuple[str, str], set[int]] = field(default_factory=dict)

    def error(self, code: str, entities: Iterable[str], message: str,
              hint: str, line: int = 0) -> None:
        """Append an error without losing other independent diagnostics."""
        self.findings.append(Finding("error", code, tuple(entities), message, hint, line))

    def parse(self) -> None:
        """Collect definitions before resolving forward and nested references."""
        for block in self.blocks:
            try:
                self._definition(block)
            except (ValueError, IndexError, KeyError, OverflowError):
                self.error("parse_error", (f"keyword:{block.keyword}",),
                           "Malformed or duplicate definition", "Correct the keyword data and identifiers.",
                           block.line)
        for kind, groups in self.sets.items():
            for name in groups:
                self.resolve_set(kind, name)
        for eid, (_, nodes) in self.elements.items():
            for nid in nodes:
                if nid not in self.nodes:
                    self.missing("node", str(nid), (f"element:{eid}",))
        if not self.nodes or not self.elements:
            self.error("empty_mesh", ("mesh",), "No usable nodes or elements",
                       "Supply a structural mesh and complete analysis definitions.")

    def _definition(self, block: Block) -> None:
        p, k = block.params, block.keyword
        if k == "NODE":
            for row in block.rows:
                nid = positive_id(row[0])
                if nid in self.nodes or not 2 <= len(row) <= 4:
                    raise ValueError("Invalid node")
                self.nodes[nid] = tuple(number(v) for v in row[1:]) + (0.0,) * (4 - len(row))
                if "NSET" in p:
                    self.sets["nset"].setdefault(p["NSET"].upper(), []).append(str(nid))
        elif k == "ELEMENT":
            kind = p["TYPE"].upper()
            width = ELEMENT_NODES.get(kind)
            if width is None:
                self.error("unsupported_topology", (f"element_type:{kind}",),
                           "Element topology is not supported by this gate",
                           "Extend and test the topology reader before running this deck.", block.line)
                return
            pending = []
            for row in block.rows:
                pending.extend(value for value in row if value)
                if len(pending) < width + 1 and row[-1] == "":
                    continue
                if len(pending) != width + 1:
                    raise ValueError("Invalid element width")
                eid, *nodes = map(positive_id, pending)
                if eid in self.elements:
                    raise ValueError("Duplicate element")
                self.elements[eid] = (kind, tuple(nodes))
                if "ELSET" in p:
                    self.sets["elset"].setdefault(p["ELSET"].upper(), []).append(str(eid))
                pending = []
            if pending:
                raise ValueError("Incomplete element")
        elif k in {"NSET", "ELSET"}:
            kind = k.lower()
            members = self.sets[kind].setdefault(p[k].upper(), [])
            for row in block.rows:
                values = [v for v in row if v]
                if "GENERATE" in p:
                    if len(values) not in {2, 3}:
                        raise ValueError("Invalid set generation")
                    start, stop = map(positive_id, values[:2])
                    stride = positive_id(values[2]) if len(values) == 3 else 1
                    if stop < start:
                        raise ValueError("Descending generation")
                    # Local ID-span cap bounds expansion memory/time; it is not
                    # a CalculiX limit. Missing generated IDs remain findings.
                    if stop - start > 10_000_000:
                        raise ValueError("Set generation exceeds supported size")
                    members.extend(str(v) for v in range(start, stop + 1, stride))
                else:
                    members.extend(v.upper() for v in values)
        elif k in DEFINITIONS:
            kind, param = DEFINITIONS[k]
            name = p[param].upper()
            if not name or name in self.definitions[kind]:
                raise ValueError("Invalid named definition")
            self.definitions[kind].add(name)

    def missing(self, kind: str, name: str, context: tuple[str, ...] = (),
                line: int = 0) -> None:
        """Report a missing reference with both target and referring entities."""
        self.error("missing_reference", (f"{kind}:{name}", *context),
                   f"Undefined {kind} reference", "Define the referenced entity or correct its name or ID.", line)

    def resolve_set(self, kind: str, name: str,
                    active: tuple[str, ...] = ()) -> set[int]:
        """Resolve set unions and GENERATE records independent of definition order."""
        name = name.upper()
        key = (kind, name)
        if key in self.resolved:
            return self.resolved[key]
        # Local recursion cap reserves headroom for resolution calls, not a
        # solver restriction on set nesting (same resource policy as includes).
        if name in active or len(active) >= 64:
            self.error("parse_error", (f"{kind}:{name}",), "Cyclic set reference",
                    "Use explicit members or an acyclic set union with at most 64 levels.")
            return set()
        if name not in self.sets[kind]:
            self.missing(kind, name)
            return set()
        ids: set[int] = set()
        universe = self.nodes if kind == "nset" else self.elements
        for item in self.sets[kind][name]:
            if item.isdigit():
                value = int(item)
                if value not in universe:
                    self.missing("node" if kind == "nset" else "element", item, (f"{kind}:{name}",))
                else:
                    ids.add(value)
            else:
                self.used.add((kind, item))
                ids.update(self.resolve_set(kind, item, (*active, name)))
        self.resolved[key] = ids
        return ids

    def target(self, kind: str, value: str, line: int = 0) -> set[int]:
        """Resolve a direct mesh ID or a named set and retain usage."""
        value = value.upper()
        if value.isdigit():
            universe = self.nodes if kind == "nset" else self.elements
            if int(value) in universe:
                return {int(value)}
            self.missing("node" if kind == "nset" else "element", value, line=line)
            return set()
        self.used.add((kind, value))
        return self.resolve_set(kind, value)

    def reference(self, kind: str, value: str, line: int = 0) -> None:
        """Track a symbolic reference against its own namespace."""
        value = value.upper()
        self.used.add((kind, value))
        if kind in self.sets:
            self.resolve_set(kind, value)
        elif value not in self.definitions[kind]:
            self.missing(kind, value, line=line)
