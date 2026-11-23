"""
ABOUTME: GT1R parachute frame geometry and material model
ABOUTME: Node coordinates, member connectivity, 4130 chromoly properties
"""

import math
from dataclasses import dataclass, field
from typing import Dict, List, Tuple

# 4130 Chromoly Steel — ASTM A519, normalized condition
# Reference: AISI 4130 datasheet, MIL-S-6758
CHROMOLY_4130 = {
    "name": "AISI 4130 Chromoly (Normalized)",
    "fy_psi": 63_000,       # Yield strength (psi)
    "fu_psi": 97_000,       # Ultimate tensile strength (psi)
    "E_psi": 29_700_000,    # Elastic modulus (psi)
    "nu": 0.29,             # Poisson's ratio
    "density_pci": 0.284,   # Density (lb/in^3)
    "G_psi": 11_500_000,    # Shear modulus (psi)
}


def tube_section_properties(od: float, wall: float) -> Dict[str, float]:
    """
    Compute section properties for a circular hollow tube.

    Args:
        od: Outer diameter (inches)
        wall: Wall thickness (inches)

    Returns:
        Dict with A, I, S, r (in^2, in^4, in^3, in)
    """
    if wall <= 0:
        raise ValueError("Wall thickness must be positive")
    if wall >= od / 2:
        raise ValueError("Wall thickness must be less than radius")

    id_ = od - 2 * wall
    area = math.pi / 4 * (od**2 - id_**2)
    inertia = math.pi / 64 * (od**4 - id_**4)
    section_mod = inertia / (od / 2)
    radius_gyr = math.sqrt(inertia / area)

    return {"A": area, "I": inertia, "S": section_mod, "r": radius_gyr,
            "OD": od, "wall": wall, "ID": id_}


@dataclass
class FrameModel:
    """2D frame model with nodes, members, BCs, and connections."""
    nodes: Dict[int, Tuple[float, float]]
    members: List[Dict]
    fixed_nodes: List[int]
    load_node: int
    connections: Dict[int, Dict]
    material: Dict[str, float] = field(default_factory=lambda: CHROMOLY_4130.copy())


def build_gt1r_frame(
    bar_od: float = 1.5,
    bar_wall: float = 0.120,
    strut_od: float = 1.5,
    strut_wall: float = 0.120,
) -> FrameModel:
    """
    Build the GT1R parachute frame geometry from hand-sketch dimensions.

    Coordinate system (rear view, transverse-vertical):
      X = transverse (left-right), Y = vertical (up positive)
      Origin at left C3 mount.

    Nodes:
      N0 (0, 0)      — left C3 mount (fixed)
      N1 (6, 0)      — left bar-strut junction
      N2 (18, 0)     — center bar, C1 bolt
      N3 (30, 0)     — right bar-strut junction
      N4 (36, 0)     — right C3 mount (fixed)
      N5 (18, -7.25) — coupler pin (load application point)
    """
    nodes = {
        0: (0.0, 0.0),
        1: (6.0, 0.0),
        2: (18.0, 0.0),
        3: (30.0, 0.0),
        4: (36.0, 0.0),
        5: (18.0, -7.25),
    }

    bar_props = tube_section_properties(bar_od, bar_wall)
    strut_props = tube_section_properties(strut_od, strut_wall)

    members = [
        {"id": 0, "nodes": (0, 1), "label": "bar_left_end",
         "section": bar_props},
        {"id": 1, "nodes": (1, 2), "label": "bar_left_center",
         "section": bar_props},
        {"id": 2, "nodes": (2, 3), "label": "bar_center_right",
         "section": bar_props},
        {"id": 3, "nodes": (3, 4), "label": "bar_right_end",
         "section": bar_props},
        {"id": 4, "nodes": (1, 5), "label": "v_strut_left",
         "section": strut_props},
        {"id": 5, "nodes": (3, 5), "label": "v_strut_right",
         "section": strut_props},
    ]

    connections = {
        0: {"type": "C3", "desc": "Weld — rigid BC at left frame rail"},
        1: {"type": "C0", "desc": "Weld — bar to strut junction (left)"},
        2: {"type": "C1", "desc": "Bolt + pin — center bar connection"},
        3: {"type": "C0", "desc": "Weld — bar to strut junction (right)"},
        4: {"type": "C3", "desc": "Weld — rigid BC at right frame rail"},
        5: {"type": "coupler_pin", "desc": "Double pin — strut convergence"},
    }

    return FrameModel(
        nodes=nodes,
        members=members,
        fixed_nodes=[0, 4],
        load_node=5,
        connections=connections,
    )
