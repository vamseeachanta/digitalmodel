"""Structural parser and validator for deterministic ASCII Gmsh MSH 2.2."""

from __future__ import annotations

import math
import shlex
from dataclasses import dataclass
from pathlib import Path
from typing import Any


PHYSICAL_NAMES = {
    "fluid": (3, 1),
    "walls": (2, 2),
    "atmosphere": (2, 3),
}


@dataclass(frozen=True)
class Msh2Contract:
    version: str
    binary: bool
    element_types: frozenset[int]
    triangle_count: int
    tetrahedron_count: int
    untagged_element_count: int
    element_ids_are_unique: bool
    physical_names: dict[str, tuple[int, int]]
    physical_ids: frozenset[int]
    min_signed_determinant: float


def inspect_msh2(path: Path | str) -> Msh2Contract:
    """Structurally parse the ASCII MSH2 sections used by the bridge."""
    sections = _read_sections(Path(path))
    version_parts = sections["MeshFormat"][0].split()
    nodes = _parse_nodes(sections["Nodes"])
    physical_names = _parse_physical_names(sections["PhysicalNames"])
    elements = _parse_elements(sections["Elements"])
    determinants = [
        _signed_determinant([nodes[tag] for tag in item[3]])
        for item in elements if item[1] == 4
    ]
    element_ids = [item[0] for item in elements]
    physical_ids = frozenset(item[2] for item in elements if item[2] is not None)
    return Msh2Contract(
        version=version_parts[0],
        binary=version_parts[1] != "0",
        element_types=frozenset(item[1] for item in elements),
        triangle_count=sum(item[1] == 2 for item in elements),
        tetrahedron_count=sum(item[1] == 4 for item in elements),
        untagged_element_count=sum(item[2] is None for item in elements),
        element_ids_are_unique=len(element_ids) == len(set(element_ids)),
        physical_names=physical_names,
        physical_ids=physical_ids,
        min_signed_determinant=min(determinants) if determinants else math.nan,
    )


def validate_msh2_contract(contract: Msh2Contract) -> None:
    if contract.version != "2.2" or contract.binary:
        raise ValueError("source mesh must be ASCII MSH 2.2")
    if contract.element_types != {2, 4}:
        raise ValueError(f"source mesh element types differ: {contract.element_types}")
    if contract.physical_names != PHYSICAL_NAMES:
        raise ValueError("source mesh physical groups differ from contract")
    if contract.physical_ids != {1, 2, 3}:
        raise ValueError("source mesh physical IDs must be globally unique")
    if contract.untagged_element_count or not contract.element_ids_are_unique:
        raise ValueError("source mesh contains untagged or duplicate elements")
    if not math.isfinite(contract.min_signed_determinant) or (
        contract.min_signed_determinant <= 0.0
    ):
        raise ValueError("source mesh contains non-positive tetrahedra")


def _read_sections(path: Path) -> dict[str, list[str]]:
    lines = path.read_text(encoding="ascii").splitlines()
    sections: dict[str, list[str]] = {}
    index = 0
    while index < len(lines):
        marker = lines[index]
        if marker.startswith("$") and not marker.startswith("$End"):
            name = marker[1:]
            end = f"$End{name}"
            try:
                end_index = lines.index(end, index + 1)
            except ValueError as exc:
                raise ValueError(f"unterminated MSH section {name}") from exc
            sections[name] = lines[index + 1 : end_index]
            index = end_index
        index += 1
    required = {"MeshFormat", "PhysicalNames", "Nodes", "Elements"}
    missing = required - sections.keys()
    if missing:
        raise ValueError(f"MSH missing sections: {sorted(missing)}")
    return sections


def _parse_nodes(lines: list[str]) -> dict[int, Any]:
    import numpy as np

    count = int(lines[0])
    rows = lines[1:]
    if len(rows) != count:
        raise ValueError("MSH node count does not match node rows")
    return {
        int(parts[0]): np.asarray([float(value) for value in parts[1:4]])
        for parts in (line.split() for line in rows)
    }


def _parse_physical_names(lines: list[str]) -> dict[str, tuple[int, int]]:
    count = int(lines[0])
    parsed: dict[str, tuple[int, int]] = {}
    for line in lines[1:]:
        dimension, physical_id, name = shlex.split(line)
        parsed[name] = (int(dimension), int(physical_id))
    if len(parsed) != count:
        raise ValueError("MSH physical-name count is inconsistent")
    return parsed


def _parse_elements(
    lines: list[str],
) -> list[tuple[int, int, int | None, tuple[int, ...]]]:
    count = int(lines[0])
    parsed = []
    for line in lines[1:]:
        values = [int(value) for value in line.split()]
        element_id, element_type, tag_count = values[:3]
        tags = values[3 : 3 + tag_count]
        node_tags = tuple(values[3 + tag_count :])
        parsed.append(
            (element_id, element_type, tags[0] if tags else None, node_tags)
        )
    if len(parsed) != count:
        raise ValueError("MSH element count does not match element rows")
    return parsed


def _signed_determinant(points: Any) -> float:
    a = points[1] - points[0]
    b = points[2] - points[0]
    c = points[3] - points[0]
    return float(
        a[0] * (b[1] * c[2] - b[2] * c[1])
        - a[1] * (b[0] * c[2] - b[2] * c[0])
        + a[2] * (b[0] * c[1] - b[1] * c[0])
    )
