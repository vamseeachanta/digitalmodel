"""ABOUTME: Read the physical basis out of an AQWA .dat deck so another solver can be set up from the same numbers.

Reproducing an AQWA case in a second solver starts with knowing what the deck
actually says, rather than what a summary of it says. The L01 benchmark in this
repository is the cautionary example: its OrcaWave companion carries a mass of
44 082.20 t against the deck's 45 184.268 t, because the displaced volume in
cubic metres was entered as a mass in tonnes and the 1.025 t/m^3 factor was
dropped. A 2.44% mass error moves every natural frequency and every response.

This module reads the deck and returns the numbers, so the comparison can be
built from the source rather than from a transcription.

What it reads
-------------
Deck 3 ``MATE``    structure mass
Deck 4 ``GEOM PMAS``  the six inertia values, ordered Ixx Ixy Ixz Iyy Iyz Izz
Deck 1 ``COOR``    the coordinates of the node the mass is attached to
Deck 5 ``GLOB``    water depth, density, gravitational acceleration
Deck 6 ``FDR1``    frequencies, as ``HRTZ`` cards in Hz, and headings
Deck 2 ``ELM1``    the waterline z from ``ZLWL``
Deck 7 ``WFS1``    additional damping and stiffness, if any

Units are whatever the deck declares. AQWA works in a user-consistent set, so a
deck stating "Metric: kg, m [N]" yields mass in kg and inertia in kg.m^2, and
nothing here converts them.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass, field
from pathlib import Path

__all__ = ["AqwaBasis", "read_aqwa_basis"]


@dataclass(frozen=True)
class AqwaBasis:
    """The physical basis a deck declares."""

    mass: float | None = None
    inertia: tuple[float, float, float, float, float, float] | None = None
    centre_of_gravity: tuple[float, float, float] | None = None
    cog_node: int | None = None
    water_depth: float | None = None
    water_density: float | None = None
    gravity: float | None = None
    waterline_z: float | None = None
    frequencies_rad_s: tuple[float, ...] = ()
    headings_deg: tuple[float, ...] = ()
    additional_damping: tuple[tuple[float, ...], ...] | None = None
    additional_stiffness: tuple[tuple[float, ...], ...] | None = None
    unit_system: str | None = None
    options: tuple[str, ...] = ()
    panel_count: int = 0
    source: str | None = None
    warnings: tuple[str, ...] = field(default=())

    @property
    def radii_of_gyration(self) -> tuple[float, float, float] | None:
        """Radii about the mass node, from the diagonal inertia terms."""
        if self.mass is None or not self.mass or self.inertia is None:
            return None
        ixx, _, _, iyy, _, izz = self.inertia
        return (math.sqrt(ixx / self.mass), math.sqrt(iyy / self.mass),
                math.sqrt(izz / self.mass))

    @property
    def displaced_volume_if_balanced(self) -> float | None:
        """Volume the hull must displace for mass to equal displacement."""
        if self.mass is None or not self.water_density:
            return None
        return self.mass / self.water_density

    @property
    def has_additional_damping(self) -> bool:
        return self.additional_damping is not None

    def describe(self) -> str:
        lines = [f"AQWA basis from {self.source or 'deck'}"]
        if self.unit_system:
            lines.append(f"  units                  {self.unit_system}")
        if self.mass is not None:
            lines.append(f"  mass                   {self.mass:,.3f}")
        if self.centre_of_gravity is not None:
            x, y, z = self.centre_of_gravity
            lines.append(f"  centre of gravity      ({x}, {y}, {z})"
                         f"  node {self.cog_node}")
        if self.inertia is not None:
            ixx, ixy, ixz, iyy, iyz, izz = self.inertia
            lines.append(f"  inertia Ixx Iyy Izz    {ixx:.6g} {iyy:.6g} {izz:.6g}")
            if any(v for v in (ixy, ixz, iyz)):
                lines.append(f"  products Ixy Ixz Iyz   {ixy:.6g} {ixz:.6g} {iyz:.6g}")
        r = self.radii_of_gyration
        if r:
            lines.append(f"  radii of gyration      {r[0]:.4f} {r[1]:.4f} {r[2]:.4f}")
        for label, value in (("water depth", self.water_depth),
                             ("water density", self.water_density),
                             ("gravity", self.gravity),
                             ("waterline z", self.waterline_z)):
            if value is not None:
                lines.append(f"  {label:22s} {value}")
        v = self.displaced_volume_if_balanced
        if v is not None:
            lines.append(f"  volume if balanced     {v:,.3f}")
        if self.frequencies_rad_s:
            lines.append(f"  frequencies rad/s      "
                         f"{[round(f, 4) for f in self.frequencies_rad_s]}")
        if self.headings_deg:
            lines.append(f"  headings deg           {list(self.headings_deg)}")
        lines.append(f"  panels                 {self.panel_count}")
        lines.append(f"  additional damping     "
                     f"{'yes' if self.has_additional_damping else 'none'}")
        if self.options:
            lines.append(f"  options                {' '.join(self.options)}")
        for w in self.warnings:
            lines.append(f"  NOTE: {w}")
        return "\n".join(lines)


_NUM = r"[-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?"


def _floats(text: str) -> list[float]:
    out = []
    for token in re.findall(_NUM, text):
        try:
            out.append(float(token.replace("d", "e").replace("D", "E")))
        except ValueError:
            continue
    return out


def _matrix_from_cards(cards: dict[int, list[float]]) -> tuple[tuple[float, ...], ...]:
    rows = []
    for i in range(1, 7):
        row = cards.get(i, [0.0] * 6)
        rows.append(tuple((row + [0.0] * 6)[:6]))
    return tuple(rows)


def _find_node_coordinates(
    lines: list[str], node: int
) -> tuple[float, float, float] | None:
    """Locate a node's coordinates on a fixed-column AQWA coordinate card.

    Deck 1 coordinate cards are column positional, not whitespace delimited:
    columns 1-6 hold the structure number and 7-11 the node, so a structure 1
    node 98000 is written ``     198000`` and reads as the single number 198000
    if the line is split on whitespace. The three coordinates occupy columns
    21-30, 31-40 and 41-50.
    """
    target = f"{node:>5d}"
    for raw in lines:
        if len(raw) < 50 or raw.lstrip().startswith("*"):
            continue
        if raw[6:11] != target:
            continue
        if any(tag in raw for tag in ("PMAS", "QPPL", "TPPL", "ELM")):
            continue
        try:
            x = float(raw[20:30])
            y = float(raw[30:40])
            z = float(raw[40:50])
        except ValueError:
            continue
        return (x, y, z)
    return None


def read_aqwa_basis(path: str | Path) -> AqwaBasis:
    """Read the physical basis from an AQWA deck.

    Fields the deck does not define come back as ``None`` rather than a guess,
    and anything ambiguous is recorded in ``warnings`` rather than resolved
    silently.
    """
    path = Path(path)
    text = path.read_text(errors="replace")
    lines = text.splitlines()

    mass = inertia = cog = cog_node = None
    depth = density = gravity = waterline = None
    unit_system = None
    freqs: list[float] = []
    headings: list[float] = []
    options: list[str] = []
    damping_cards: dict[int, list[float]] = {}
    stiffness_cards: dict[int, list[float]] = {}
    panels = 0
    notes: list[str] = []

    for raw in lines:
        line = raw.rstrip()
        body = line.lstrip()
        if body.startswith("*"):
            if "Unit System" in line:
                unit_system = line.split(":", 1)[-1].strip()
            continue
        if body.startswith("OPTIONS"):
            options.extend(body.split()[1:])
            continue
        if "QPPL" in line or "TPPL" in line:
            panels += 1
            continue

        token = body.split()[0] if body.split() else ""

        if "PMAS" in line and "GEOM" not in line:
            nums = _floats(line)
            if len(nums) >= 8 and "(" not in line:
                # struct, node, then six inertia values
                cog_node = int(nums[1])
                inertia = tuple(nums[2:8])
        if token == "DPTH":
            v = _floats(body)
            depth = v[0] if v else None
        elif token == "DENS":
            v = _floats(body)
            density = v[0] if v else None
        elif token == "ACCG":
            v = _floats(body)
            gravity = v[0] if v else None
        elif token == "ZLWL":
            v = _floats(body)
            waterline = v[0] if v else None
        elif "HRTZ" in line:
            v = _floats(line)
            if len(v) >= 4:
                freqs.append(2.0 * math.pi * v[-1])
        elif "DIRN" in line:
            v = _floats(line)
            if len(v) >= 4:
                headings.append(v[-1])
        elif "FIDP" in line:
            v = _floats(line)
            if len(v) >= 7:
                damping_cards[int(v[0])] = v[1:7]
        elif "FISK" in line:
            v = _floats(line)
            if len(v) >= 7:
                stiffness_cards[int(v[0])] = v[1:7]

    # Mass sits on a bare MATE data card: struct, node, mass.
    in_mate = False
    for raw in lines:
        body = raw.strip()
        if body == "MATE":
            in_mate = True
            continue
        if in_mate:
            if body in ("END", "FINI") or body.startswith("*"):
                in_mate = False
                continue
            v = _floats(body)
            if len(v) >= 3:
                mass = v[2]
                if cog_node is None:
                    cog_node = int(v[1])
                in_mate = False

    if cog_node is not None:
        cog = _find_node_coordinates(lines, cog_node)

    if mass is None:
        notes.append("no MATE mass card found")
    if inertia is None:
        notes.append("no GEOM PMAS inertia card found")
    if cog is None and cog_node is not None:
        notes.append(f"mass node {cog_node} has no coordinate card in this deck")
    if damping_cards and not stiffness_cards:
        notes.append("deck carries additional damping (FIDP) but no FISK")

    return AqwaBasis(
        mass=mass, inertia=inertia, centre_of_gravity=cog, cog_node=cog_node,
        water_depth=depth, water_density=density, gravity=gravity,
        waterline_z=waterline,
        frequencies_rad_s=tuple(sorted(set(round(f, 9) for f in freqs))),
        headings_deg=tuple(sorted(set(headings))),
        additional_damping=_matrix_from_cards(damping_cards) if damping_cards else None,
        additional_stiffness=_matrix_from_cards(stiffness_cards) if stiffness_cards else None,
        unit_system=unit_system, options=tuple(options), panel_count=panels,
        source=str(path), warnings=tuple(notes),
    )
