"""
ABOUTME: 3D frame geometry for GT1R drag car parachute system
ABOUTME: Rear trunk frame + under-chassis frame assemblies with 3D coordinates
"""

import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from digitalmodel.structural.parachute.frame_model import (
    CHROMOLY_4130,
    tube_section_properties,
)

BEND_CLR_3D = 5.25  # 3D bend radius (3 x OD) for 1.75" tube


@dataclass
class Node3D:
    """A node in 3D space."""

    id: int
    x: float
    y: float
    z: float
    label: str
    assembly: str


@dataclass
class Connection:
    """Boundary condition or connection at a node."""

    node_id: int
    conn_type: str
    bc_type: str
    description: str


@dataclass
class Member3D:
    """A structural member connecting two nodes."""

    id: int
    start_node: int
    end_node: int
    label: str
    assembly: str
    section: Dict[str, float]
    is_bend: bool = False
    bend_clr: float = 0.0

    def length(self, nodes: Dict[int, Node3D]) -> float:
        """Compute member length from node coordinates (chord length)."""
        n0 = nodes[self.start_node]
        n1 = nodes[self.end_node]
        return math.sqrt(
            (n1.x - n0.x) ** 2
            + (n1.y - n0.y) ** 2
            + (n1.z - n0.z) ** 2
        )


@dataclass
class FrameGeometry3D:
    """Complete 3D frame geometry with nodes, members, connections."""

    nodes: Dict[int, Node3D]
    members: List[Member3D]
    connections: List[Connection]
    material: Dict[str, float] = field(
        default_factory=lambda: CHROMOLY_4130.copy()
    )

    def assembly_nodes(self, name: str) -> List[Node3D]:
        """Return nodes referenced by members in the given assembly."""
        member_node_ids = set()
        for m in self.members:
            if m.assembly == name:
                member_node_ids.add(m.start_node)
                member_node_ids.add(m.end_node)
        return [n for n in self.nodes.values() if n.id in member_node_ids]

    def assembly_members(self, name: str) -> List[Member3D]:
        """Return members belonging to the given assembly."""
        return [m for m in self.members if m.assembly == name]

    def fixed_node_ids(self) -> List[int]:
        """Return node IDs with fixed boundary conditions."""
        return [c.node_id for c in self.connections if c.bc_type == "fixed"]

    def bolted_node_ids(self) -> List[int]:
        """Return node IDs with bolted boundary conditions."""
        return [c.node_id for c in self.connections if c.bc_type == "bolted"]

    def load_node_id(self) -> Optional[int]:
        """Return node ID with coupler_pin connection (load point)."""
        for c in self.connections:
            if c.conn_type == "coupler_pin":
                return c.node_id
        return None

    def bend_members(self) -> List[Member3D]:
        """Return members that are bends (not straight)."""
        return [m for m in self.members if m.is_bend]


def build_gt1r_frame_3d(
    tube_od: float = 1.75,
    tube_wall: float = 0.120,
    bend_clr: float = BEND_CLR_3D,
) -> FrameGeometry3D:
    """
    Build 3D GT1R parachute frame with rear trunk and under-chassis assemblies.

    Coordinate system (vehicle-centric):
        X = transverse (+ right / passenger, - left / driver)
        Y = longitudinal (+ forward / engine, - rearward / bumper)
        Z = vertical (+ up / roof, - down / ground)
        Origin = N0 (C2 weld junction, shared between assemblies)

    Args:
        tube_od: Tube outer diameter, inches (default 1.75")
        tube_wall: Tube wall thickness, inches (default 0.120")
        bend_clr: Bend centerline radius, inches (default 5.25" = 3D)

    Returns:
        FrameGeometry3D with both assemblies.
    """
    props = tube_section_properties(tube_od, tube_wall)

    nodes: Dict[int, Node3D] = {
        # --- Shared ---
        0: Node3D(0, 0.0, 0.0, 0.0, "c2_weld_junction", "shared"),
        # --- Assembly 1: Rear Trunk — centerline ---
        1: Node3D(1, 0.0, -7.25, 0.0, "parachute_bracket", "rear_trunk"),
        2: Node3D(2, 0.0, -4.0, 0.0, "coupler_pin", "rear_trunk"),
        3: Node3D(3, 0.0, 8.5, 1.0, "center_spine_mid", "rear_trunk"),
        4: Node3D(4, 0.0, 20.5, 2.5, "center_bar_c1_bolt", "rear_trunk"),
        # --- Assembly 1: Rear Trunk — +X bar ---
        5: Node3D(5, 12.0, 20.5, 2.5, "right_strut_junction", "rear_trunk"),
        6: Node3D(6, 15.0, 20.5, 2.5, "right_bar_bend", "rear_trunk"),
        7: Node3D(7, 18.0, 20.5, -0.5, "right_c3_mount", "rear_trunk"),
        # --- Assembly 1: Rear Trunk — -X bar (mirrored) ---
        8: Node3D(8, -12.0, 20.5, 2.5, "left_strut_junction", "rear_trunk"),
        9: Node3D(9, -15.0, 20.5, 2.5, "left_bar_bend", "rear_trunk"),
        10: Node3D(10, -18.0, 20.5, -0.5, "left_c3_mount", "rear_trunk"),
        # --- Assembly 2: Under-Chassis — +X ---
        11: Node3D(11, 16.0, 0.0, 0.0, "right_chassis_bar_end", "under_chassis"),
        12: Node3D(12, 21.0, 5.0, -4.0, "right_b1_bolted", "under_chassis"),
        # --- Assembly 2: Under-Chassis — -X (mirrored) ---
        13: Node3D(13, -16.0, 0.0, 0.0, "left_chassis_bar_end", "under_chassis"),
        14: Node3D(14, -21.0, 5.0, -4.0, "left_b1_bolted", "under_chassis"),
    }

    members = [
        # --- Assembly 1: Rear Trunk ---
        # Center spine (N1 → N2 → N0 → N3 → N4)
        Member3D(0, 1, 2, "parachute_arm", "rear_trunk", props),
        Member3D(1, 2, 0, "center_spine_lower", "rear_trunk", props),
        Member3D(2, 0, 3, "center_spine_mid", "rear_trunk", props),
        Member3D(3, 3, 4, "center_spine_upper", "rear_trunk", props),
        # V-struts (through N3)
        Member3D(4, 5, 3, "v_strut_right", "rear_trunk", props),
        Member3D(5, 8, 3, "v_strut_left", "rear_trunk", props),
        # Horizontal bar — straight segments
        Member3D(6, 4, 5, "bar_right_inner", "rear_trunk", props),
        Member3D(7, 4, 8, "bar_left_inner", "rear_trunk", props),
        Member3D(8, 5, 6, "bar_right_mid", "rear_trunk", props),
        Member3D(10, 8, 9, "bar_left_mid", "rear_trunk", props),
        # Horizontal bar — bend segments (smooth bends at ends)
        Member3D(9, 6, 7, "bar_right_bend", "rear_trunk", props,
                 is_bend=True, bend_clr=bend_clr),
        Member3D(11, 9, 10, "bar_left_bend", "rear_trunk", props,
                 is_bend=True, bend_clr=bend_clr),
        # --- Assembly 2: Under-Chassis ---
        # Straight segments
        Member3D(12, 0, 11, "chassis_bar_right", "under_chassis", props),
        Member3D(14, 0, 13, "chassis_bar_left", "under_chassis", props),
        # Bend segments (smooth bends to subframe)
        Member3D(13, 11, 12, "chassis_bar_right_bend", "under_chassis", props,
                 is_bend=True, bend_clr=bend_clr),
        Member3D(15, 13, 14, "chassis_bar_left_bend", "under_chassis", props,
                 is_bend=True, bend_clr=bend_clr),
    ]

    connections = [
        # Shared origin
        Connection(0, "C2", "free", "Weld — junction between assemblies"),
        # Assembly 1: Rear Trunk
        Connection(1, "bracket", "free", "Parachute attachment bracket"),
        Connection(2, "coupler_pin", "free", "Double pin — chute arm connection"),
        Connection(3, "weld", "free", "Weld — center spine intermediate"),
        Connection(4, "C1", "free", "Bolt + pin — center bar (shear)"),
        Connection(5, "C0", "free", "Weld — right bar to strut junction"),
        Connection(6, "weld", "free", "Weld — right bar bend point"),
        Connection(7, "C3", "fixed", "Weld — right C3 mount to frame rail"),
        Connection(8, "C0", "free", "Weld — left bar to strut junction"),
        Connection(9, "weld", "free", "Weld — left bar bend point"),
        Connection(10, "C3", "fixed", "Weld — left C3 mount to frame rail"),
        # Assembly 2: Under-Chassis
        Connection(11, "weld", "free", "Weld — right chassis bar end"),
        Connection(12, "B1", "bolted", "Bolted — right B1 (M8, 6 bolts) to subframe"),
        Connection(13, "weld", "free", "Weld — left chassis bar end"),
        Connection(14, "B1", "bolted", "Bolted — left B1 (M8, 6 bolts) to subframe"),
    ]

    return FrameGeometry3D(
        nodes=nodes,
        members=members,
        connections=connections,
    )
