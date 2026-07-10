"""Structural, fail-closed validation for converted OpenFOAM polyMesh files."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any


class PolyMeshContractError(ValueError):
    """Raised when converted mesh evidence violates the frozen bridge contract."""


@dataclass(frozen=True)
class BoundaryContract:
    wall_patches: tuple[str, ...] = ("walls",)
    atmosphere_patch: str = "atmosphere"
    fluid_zone: str = "fluid"

    @property
    def patch_names(self) -> tuple[str, ...]:
        return self.wall_patches + (self.atmosphere_patch,)

    def expected_type(self, patch_name: str) -> str:
        return "wall" if patch_name in self.wall_patches else "patch"


DEFAULT_BOUNDARY_CONTRACT = BoundaryContract()


@dataclass(frozen=True)
class BoundaryPatch:
    name: str
    patch_type: str
    n_faces: int
    start_face: int


@dataclass(frozen=True)
class CellZone:
    name: str
    zone_type: str
    cell_labels: tuple[int, ...]


@dataclass(frozen=True)
class CheckMeshSummary:
    cell_count: int
    face_count: int
    internal_face_count: int


@dataclass(frozen=True)
class PolyMeshContract:
    patches: tuple[BoundaryPatch, ...]
    zones: tuple[CellZone, ...]
    cell_count: int
    face_count: int
    internal_face_count: int

    @property
    def boundary_face_count(self) -> int:
        return self.face_count - self.internal_face_count

    @property
    def patch_names(self) -> tuple[str, ...]:
        return tuple(patch.name for patch in self.patches)

    @property
    def fluid_cell_labels(self) -> tuple[int, ...]:
        return self.zones[0].cell_labels


class _TokenStream:
    def __init__(self, tokens: list[str]) -> None:
        self.tokens = tokens
        self.index = 0

    def peek(self) -> str | None:
        return self.tokens[self.index] if self.index < len(self.tokens) else None

    def pop(self) -> str:
        token = self.peek()
        if token is None:
            raise PolyMeshContractError("unexpected end of OpenFOAM file")
        self.index += 1
        return token

    def expect(self, expected: str) -> None:
        actual = self.pop()
        if actual != expected:
            raise PolyMeshContractError(
                f"expected token {expected!r}, received {actual!r}"
            )


def _tokenize(text: str) -> list[str]:
    tokens: list[str] = []
    index = 0
    punctuation = "{}();[]"
    while index < len(text):
        char = text[index]
        if char.isspace():
            index += 1
        elif text.startswith("//", index):
            newline = text.find("\n", index + 2)
            index = len(text) if newline < 0 else newline + 1
        elif text.startswith("/*", index):
            end = text.find("*/", index + 2)
            if end < 0:
                raise PolyMeshContractError("unterminated OpenFOAM block comment")
            index = end + 2
        elif char in punctuation:
            tokens.append(char)
            index += 1
        elif char in "\"'":
            token, index = _quoted_token(text, index)
            tokens.append(token)
        else:
            start = index
            while index < len(text):
                if text[index].isspace() or text[index] in punctuation:
                    break
                if text.startswith("//", index) or text.startswith("/*", index):
                    break
                index += 1
            tokens.append(text[start:index])
    return tokens


def _quoted_token(text: str, start: int) -> tuple[str, int]:
    quote = text[start]
    characters: list[str] = []
    index = start + 1
    while index < len(text):
        if text[index] == "\\" and index + 1 < len(text):
            characters.append(text[index + 1])
            index += 2
        elif text[index] == quote:
            return "".join(characters), index + 1
        else:
            characters.append(text[index])
            index += 1
    raise PolyMeshContractError("unterminated quoted OpenFOAM token")


def _parse_dictionary(stream: _TokenStream) -> dict[str, Any]:
    stream.expect("{")
    parsed: dict[str, Any] = {}
    while stream.peek() != "}":
        key = stream.pop()
        if key in parsed:
            raise PolyMeshContractError(f"duplicate OpenFOAM key {key!r}")
        if stream.peek() == "{":
            parsed[key] = _parse_dictionary(stream)
        else:
            parsed[key] = _parse_value(stream)
    stream.expect("}")
    return parsed


def _parse_value(stream: _TokenStream) -> tuple[str, ...]:
    value: list[str] = []
    depth = 0
    while True:
        token = stream.pop()
        if token == ";" and depth == 0:
            return tuple(value)
        if token in ("(", "["):
            depth += 1
        elif token in (")", "]"):
            depth -= 1
            if depth < 0:
                raise PolyMeshContractError("unbalanced OpenFOAM value list")
        value.append(token)


def _parse_named_list(path: Path) -> list[tuple[str, dict[str, Any]]]:
    stream = _TokenStream(_tokenize(path.read_text(encoding="utf-8")))
    if stream.peek() == "FoamFile":
        stream.pop()
        _parse_dictionary(stream)
    count = _integer(stream.pop(), "entry count")
    stream.expect("(")
    parsed = []
    for _ in range(count):
        name = stream.pop()
        parsed.append((name, _parse_dictionary(stream)))
    stream.expect(")")
    if stream.peek() is not None:
        raise PolyMeshContractError(f"unexpected trailing token {stream.peek()!r}")
    return parsed


def _integer(value: str, field: str) -> int:
    try:
        return int(value)
    except ValueError as exc:
        raise PolyMeshContractError(f"{field} must be an integer") from exc


def _scalar(entries: dict[str, Any], key: str) -> str:
    value = entries.get(key)
    if not isinstance(value, tuple) or len(value) != 1:
        raise PolyMeshContractError(f"OpenFOAM entry {key!r} must be scalar")
    return value[0]


def parse_boundary(path: Path | str) -> tuple[BoundaryPatch, ...]:
    patches = []
    for name, entries in _parse_named_list(Path(path)):
        patches.append(
            BoundaryPatch(
                name=name,
                patch_type=_scalar(entries, "type"),
                n_faces=_integer(_scalar(entries, "nFaces"), "nFaces"),
                start_face=_integer(_scalar(entries, "startFace"), "startFace"),
            )
        )
    return tuple(patches)


def parse_cell_zones(path: Path | str) -> dict[str, CellZone]:
    zones: dict[str, CellZone] = {}
    for name, entries in _parse_named_list(Path(path)):
        if name in zones:
            raise PolyMeshContractError(f"duplicate cellZone {name!r}")
        zones[name] = CellZone(
            name=name,
            zone_type=_scalar(entries, "type"),
            cell_labels=_label_list(entries.get("cellLabels")),
        )
    return zones


def _label_list(value: Any) -> tuple[int, ...]:
    if not isinstance(value, tuple) or "(" not in value or ")" not in value:
        raise PolyMeshContractError("cellLabels must be an OpenFOAM label list")
    opening = value.index("(")
    closing = len(value) - 1 - value[::-1].index(")")
    if opening == 0 or closing <= opening:
        raise PolyMeshContractError("cellLabels list structure is invalid")
    expected = _integer(value[opening - 1], "cellLabels count")
    labels = tuple(_integer(token, "cell label") for token in value[opening + 1 : closing])
    if len(labels) != expected:
        raise PolyMeshContractError("cellLabels count does not match list length")
    return labels


def parse_check_mesh_output(output: str, return_code: int) -> CheckMeshSummary:
    if return_code != 0:
        raise PolyMeshContractError(f"checkMesh returned return code {return_code}")
    if "FOAM FATAL ERROR" in output or "FOAM FATAL IO ERROR" in output:
        raise PolyMeshContractError("checkMesh output contains a FATAL marker")
    lines = [line.strip() for line in output.splitlines()]
    if "Mesh OK." not in lines:
        raise PolyMeshContractError("checkMesh output is missing 'Mesh OK.'")
    failed = _failed_check_count(lines)
    if failed != 0:
        raise PolyMeshContractError(f"checkMesh reported Failed {failed} mesh checks")
    counts = _check_mesh_counts(lines)
    return CheckMeshSummary(
        cell_count=counts["cells"],
        face_count=counts["faces"],
        internal_face_count=counts["internal faces"],
    )


def _failed_check_count(lines: list[str]) -> int:
    matches = []
    for line in lines:
        tokens = line.rstrip(".").split()
        if len(tokens) == 4 and tokens[0] == "Failed" and tokens[2:] == ["mesh", "checks"]:
            matches.append(_integer(tokens[1], "failed mesh check count"))
    if len(matches) != 1:
        raise PolyMeshContractError("checkMesh must report one failed-check count")
    return matches[0]


def _check_mesh_counts(lines: list[str]) -> dict[str, int]:
    wanted = {"cells", "faces", "internal faces"}
    counts: dict[str, int] = {}
    for line in lines:
        if ":" not in line:
            continue
        name, raw_value = (part.strip() for part in line.split(":", 1))
        if name in wanted:
            if name in counts:
                raise PolyMeshContractError(f"duplicate checkMesh count for {name}")
            counts[name] = _integer(raw_value.split()[0], name)
    missing = wanted - counts.keys()
    if missing:
        raise PolyMeshContractError(
            f"checkMesh output is missing counts: {', '.join(sorted(missing))}"
        )
    return counts


def validate_poly_mesh_contract(
    poly_mesh_dir: Path | str,
    *,
    check_mesh_output: str,
    check_mesh_return_code: int,
    boundary_contract: BoundaryContract = DEFAULT_BOUNDARY_CONTRACT,
) -> PolyMeshContract:
    poly_mesh = Path(poly_mesh_dir)
    summary = parse_check_mesh_output(check_mesh_output, check_mesh_return_code)
    patches = parse_boundary(poly_mesh / "boundary")
    zones_by_name = parse_cell_zones(poly_mesh / "cellZones")
    _validate_patches(patches, summary, boundary_contract)
    _validate_zones(zones_by_name, summary.cell_count, boundary_contract)
    return PolyMeshContract(
        patches=patches,
        zones=tuple(zones_by_name.values()),
        cell_count=summary.cell_count,
        face_count=summary.face_count,
        internal_face_count=summary.internal_face_count,
    )


def _validate_patches(
    patches: tuple[BoundaryPatch, ...],
    summary: CheckMeshSummary,
    contract: BoundaryContract,
) -> None:
    names = tuple(patch.name for patch in patches)
    if "defaultFaces" in names:
        raise PolyMeshContractError("defaultFaces is forbidden by the mesh contract")
    if names != contract.patch_names:
        raise PolyMeshContractError(
            f"patch names {names} do not equal {contract.patch_names}"
        )
    for patch in patches:
        expected_type = contract.expected_type(patch.name)
        if patch.patch_type != expected_type:
            raise PolyMeshContractError(
                f"patch {patch.name} must have type {expected_type}"
            )
        if patch.n_faces <= 0:
            raise PolyMeshContractError(f"patch {patch.name} must have positive nFaces")
    _validate_patch_ranges(patches, summary)


def _validate_patch_ranges(
    patches: tuple[BoundaryPatch, ...], summary: CheckMeshSummary
) -> None:
    cursor = summary.internal_face_count
    for patch in sorted(patches, key=lambda item: item.start_face):
        if patch.start_face > cursor:
            raise PolyMeshContractError(f"gap before patch {patch.name}")
        if patch.start_face < cursor:
            raise PolyMeshContractError(f"overlap before patch {patch.name}")
        cursor += patch.n_faces
    boundary_count = summary.face_count - summary.internal_face_count
    if cursor != summary.face_count or sum(p.n_faces for p in patches) != boundary_count:
        raise PolyMeshContractError("boundary face count does not match checkMesh")


def _validate_zones(
    zones: dict[str, CellZone], cell_count: int, contract: BoundaryContract
) -> None:
    if set(zones) != {contract.fluid_zone}:
        raise PolyMeshContractError(
            f"cellZones must contain exactly {contract.fluid_zone!r}"
        )
    zone = zones[contract.fluid_zone]
    if zone.zone_type != "cellZone":
        raise PolyMeshContractError("fluid zone must have type cellZone")
    if len(zone.cell_labels) != len(set(zone.cell_labels)):
        raise PolyMeshContractError("fluid zone contains duplicate cell labels")
    if set(zone.cell_labels) != set(range(cell_count)):
        raise PolyMeshContractError("fluid zone must cover every cell exactly once")
