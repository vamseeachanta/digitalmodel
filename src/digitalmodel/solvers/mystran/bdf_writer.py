#!/usr/bin/env python3
"""
ABOUTME: Nastran bulk-data (BDF) writer for MYSTRAN — converts gmsh-style node
and element arrays into a free-field SOL 101 deck with materials, properties,
SPCs, and nodal loads.
"""

from pathlib import Path
from typing import Optional, Sequence

import numpy as np


class BDFWriter:
    """
    Write a MYSTRAN-compatible Nastran deck from mesh arrays.

    Node and element connectivity are **0-based** on input (gmsh convention,
    matching :class:`digitalmodel.solvers.calculix.INPWriter`) and written
    1-based. Element IDs are assigned sequentially across element blocks in
    insertion order.

    Element blocks use gmsh type names as keys, e.g. ``"Hexahedron 8"``.
    """

    # gmsh element name -> (Nastran card, nodes per element)
    ELEMENT_MAP = {
        "Line 2": ("CBAR", 2),
        "Triangle 3": ("CTRIA3", 3),
        "Quadrilateral 4": ("CQUAD4", 4),
        "Tetrahedron 4": ("CTETRA", 4),
        "Tetrahedron 10": ("CTETRA", 10),
        "Hexahedron 8": ("CHEXA", 8),
        "Hexahedron 20": ("CHEXA", 20),
    }

    _BAR_CARDS = {"CBAR"}
    _SHELL_CARDS = {"CTRIA3", "CQUAD4"}
    _SOLID_CARDS = {"CTETRA", "CHEXA"}

    def __init__(
        self,
        nodes: np.ndarray,
        elements: dict,
        title: str = "digitalmodel MYSTRAN analysis",
        sol: int = 101,
    ):
        self.nodes = np.asarray(nodes, dtype=float)
        self.elements = elements
        self.title = title
        self.sol = sol

        self._materials: dict[str, int] = {}
        self._material_cards: list[str] = []
        self._properties: dict[str, int] = {}  # element type name -> PID
        self._property_cards: list[str] = []
        self._orientation: dict[str, tuple] = {}
        self._spcs: list[tuple[str, list[int], float]] = []
        self._forces: list[tuple[list[int], float, tuple]] = []
        self._moments: list[tuple[list[int], float, tuple]] = []
        self._outputs = {"DISP": True, "STRESS": True, "SPCFORCE": True}
        self._params: list[tuple[str, str]] = []

    # ------------------------------------------------------------------
    # Materials and properties
    # ------------------------------------------------------------------

    def add_material(
        self, name: str, E: float, nu: float, rho: float = 0.0
    ) -> int:
        """Add an isotropic MAT1. Returns the material ID."""
        mid = len(self._materials) + 1
        self._materials[name] = mid
        self._material_cards.append(
            f"MAT1,{mid},{_f(E)},,{_f(nu)},{_f(rho)}"
        )
        return mid

    def add_bar_section(
        self,
        element_type: str,
        material: str,
        area: float,
        i1: float,
        i2: float,
        j: float,
        orientation: Sequence[float] = (0.0, 0.0, 1.0),
        stress_points: Optional[Sequence[tuple[float, float]]] = None,
    ) -> int:
        """
        Add a PBAR for ``element_type`` (must map to CBAR).

        ``stress_points`` are up to four (y, z) recovery points; without them
        MYSTRAN reports zero bar stresses.
        """
        card = self._card_for(element_type)
        if card not in self._BAR_CARDS:
            raise ValueError(f"{element_type} is not a bar element type")
        pid = self._new_pid(element_type)
        mid = self._mid(material)
        line = f"PBAR,{pid},{mid},{_f(area)},{_f(i1)},{_f(i2)},{_f(j)}"
        if stress_points:
            pts = list(stress_points)[:4]
            flat: list[str] = []
            for y, z in pts:
                flat += [_f(y), _f(z)]
            while len(flat) < 8:
                flat += ["0.0", "0.0"]
            # PBAR continuation: C1,C2,D1,D2,E1,E2,F1,F2
            line += f",,,+P{pid}\n+P{pid}," + ",".join(flat)
        self._property_cards.append(line)
        self._orientation[element_type] = tuple(float(v) for v in orientation)
        return pid

    def add_solid_section(
        self, element_type: str, material: str, integration_order: int = 2
    ) -> int:
        """Add a PSOLID for a CTETRA/CHEXA block.

        MYSTRAN requires the IN (integration order) field for HEXA8.
        """
        card = self._card_for(element_type)
        if card not in self._SOLID_CARDS:
            raise ValueError(f"{element_type} is not a solid element type")
        pid = self._new_pid(element_type)
        mid = self._mid(material)
        self._property_cards.append(f"PSOLID,{pid},{mid},0,{integration_order}")
        return pid

    def add_shell_section(
        self, element_type: str, material: str, thickness: float
    ) -> int:
        """Add a PSHELL (membrane + bending, same material) for a shell block."""
        card = self._card_for(element_type)
        if card not in self._SHELL_CARDS:
            raise ValueError(f"{element_type} is not a shell element type")
        pid = self._new_pid(element_type)
        mid = self._mid(material)
        self._property_cards.append(
            f"PSHELL,{pid},{mid},{_f(thickness)},{mid},,{mid}"
        )
        return pid

    # ------------------------------------------------------------------
    # Constraints, loads, output
    # ------------------------------------------------------------------

    def add_spc(
        self, node_ids: Sequence[int], dofs: str = "123456", value: float = 0.0
    ) -> None:
        """Constrain ``dofs`` (Nastran digits, e.g. ``"123"``) at 0-based nodes."""
        dofs = "".join(sorted(set(str(dofs))))
        if not dofs or any(c not in "123456" for c in dofs):
            raise ValueError(f"Invalid DOF string: {dofs!r}")
        self._spcs.append((dofs, [int(n) for n in node_ids], float(value)))

    def add_force(
        self,
        node_ids: Sequence[int],
        magnitude: float,
        direction: Sequence[float] = (0.0, 0.0, 1.0),
    ) -> None:
        """Apply a FORCE of ``magnitude`` along ``direction`` at each node."""
        self._forces.append(
            ([int(n) for n in node_ids], float(magnitude), tuple(direction))
        )

    def add_moment(
        self,
        node_ids: Sequence[int],
        magnitude: float,
        direction: Sequence[float] = (0.0, 0.0, 1.0),
    ) -> None:
        """Apply a MOMENT of ``magnitude`` about ``direction`` at each node."""
        self._moments.append(
            ([int(n) for n in node_ids], float(magnitude), tuple(direction))
        )

    def set_outputs(
        self, disp: bool = True, stress: bool = True, spcforce: bool = True
    ) -> None:
        self._outputs = {"DISP": disp, "STRESS": stress, "SPCFORCE": spcforce}

    def add_param(self, name: str, value) -> None:
        self._params.append((name, str(value)))

    # ------------------------------------------------------------------
    # Write
    # ------------------------------------------------------------------

    def write(self, path: Path) -> Path:
        """Write the deck. Returns the path written."""
        path = Path(path)
        self._validate()
        lines: list[str] = []

        # Executive control
        lines.append(f"ID {path.stem.upper()[:8]},DIGITALMODEL")
        lines.append(f"SOL {self.sol}")
        lines.append("CEND")

        # Case control
        lines.append(f"TITLE = {self.title}")
        lines.append("SUBTITLE = generated by digitalmodel.solvers.mystran")
        if self._spcs:
            lines.append("SPC = 1")
        if self._forces or self._moments:
            lines.append("LOAD = 1")
        for key, enabled in self._outputs.items():
            if enabled:
                lines.append(f"{key} = ALL")

        # Bulk data
        lines.append("BEGIN BULK")
        for name, value in self._params:
            lines.append(f"PARAM,{name},{value}")
        lines.append("$ --- grids ---")
        for i, (x, y, z) in enumerate(self.nodes, start=1):
            lines.append(f"GRID,{i},,{_f(x)},{_f(y)},{_f(z)}")

        lines.append("$ --- elements ---")
        eid = 1
        for etype, block in self.elements.items():
            card, npn = self.ELEMENT_MAP[etype]
            pid = self._properties[etype]
            conn = np.asarray(block["connectivity"], dtype=int)
            if conn.ndim != 2 or conn.shape[1] != npn:
                raise ValueError(
                    f"{etype}: expected {npn} nodes per element, got shape {conn.shape}"
                )
            for row in conn:
                gids = [str(int(g) + 1) for g in row]
                if card in self._BAR_CARDS:
                    ox, oy, oz = self._orientation[etype]
                    lines.append(
                        f"{card},{eid},{pid},{gids[0]},{gids[1]},"
                        f"{_f(ox)},{_f(oy)},{_f(oz)}"
                    )
                else:
                    lines.extend(_split_card(card, eid, pid, gids))
                eid += 1

        lines.append("$ --- properties and materials ---")
        lines.extend(self._property_cards)
        lines.extend(self._material_cards)

        if self._spcs:
            lines.append("$ --- single-point constraints ---")
            for dofs, ids, value in self._spcs:
                if value == 0.0:
                    lines.extend(_spc1_lines(dofs, ids))
                else:
                    for n in ids:
                        lines.append(f"SPC,1,{n + 1},{dofs},{_f(value)}")

        if self._forces or self._moments:
            lines.append("$ --- loads ---")
            for ids, mag, (dx, dy, dz) in self._forces:
                for n in ids:
                    lines.append(
                        f"FORCE,1,{n + 1},,{_f(mag)},{_f(dx)},{_f(dy)},{_f(dz)}"
                    )
            for ids, mag, (dx, dy, dz) in self._moments:
                for n in ids:
                    lines.append(
                        f"MOMENT,1,{n + 1},,{_f(mag)},{_f(dx)},{_f(dy)},{_f(dz)}"
                    )

        lines.append("ENDDATA")
        path.write_text("\n".join(lines) + "\n")
        return path

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _card_for(self, element_type: str) -> str:
        if element_type not in self.ELEMENT_MAP:
            raise ValueError(
                f"Unsupported element type {element_type!r}; "
                f"supported: {sorted(self.ELEMENT_MAP)}"
            )
        return self.ELEMENT_MAP[element_type][0]

    def _new_pid(self, element_type: str) -> int:
        if element_type in self._properties:
            raise ValueError(f"Property already defined for {element_type}")
        pid = len(self._properties) + 1
        self._properties[element_type] = pid
        return pid

    def _mid(self, material: str) -> int:
        if material not in self._materials:
            raise ValueError(f"Unknown material {material!r}; call add_material")
        return self._materials[material]

    def _validate(self) -> None:
        if len(self.nodes) == 0:
            raise ValueError("No nodes to write")
        for etype in self.elements:
            self._card_for(etype)
            if etype not in self._properties:
                raise ValueError(
                    f"No section/property defined for element type {etype!r}"
                )
        n = len(self.nodes)
        for _, ids, _ in self._spcs:
            _check_ids(ids, n, "SPC")
        for ids, _, _ in self._forces + self._moments:
            _check_ids(ids, n, "load")


# ----------------------------------------------------------------------
# Module helpers
# ----------------------------------------------------------------------

def _f(x: float) -> str:
    """Format a float for free-field bulk data (MYSTRAN accepts long tokens)."""
    x = float(x)
    if x == 0.0:
        return "0.0"
    s = f"{x:.8G}"
    if "." not in s and "E" not in s:
        s += ".0"
    return s


def _split_card(card: str, eid: int, pid: int, gids: list[str]) -> list[str]:
    """Emit a connectivity card with continuation lines (max 9 fields/line)."""
    fields = [card, str(eid), str(pid)] + gids
    tag = f"+{card[1]}{eid}"
    first = fields[:9]
    rest = fields[9:]
    if not rest:
        return [",".join(first)]
    lines = [",".join(first) + f",{tag}"]
    while rest:
        chunk, rest = rest[:8], rest[8:]
        line = f"{tag}," + ",".join(chunk)
        if rest:
            line += f",{tag}"
        lines.append(line)
    return lines


def _spc1_lines(dofs: str, ids: Sequence[int]) -> list[str]:
    """SPC1 cards with at most 6 grids per line (no continuation needed)."""
    out = []
    gids = [str(int(n) + 1) for n in ids]
    for i in range(0, len(gids), 6):
        out.append(f"SPC1,1,{dofs}," + ",".join(gids[i:i + 6]))
    return out


def _check_ids(ids: Sequence[int], n: int, what: str) -> None:
    for i in ids:
        if i < 0 or i >= n:
            raise ValueError(f"{what} node index {i} out of range 0..{n - 1}")
