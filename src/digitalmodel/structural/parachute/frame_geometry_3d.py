"""
ABOUTME: 3D frame geometry for GT1R drag car parachute system
ABOUTME: Rear trunk frame + under-hood frame assemblies with 3D coordinates
"""

import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from digitalmodel.structural.parachute.frame_model import (
    CHROMOLY_4130,
    tube_section_properties,
)


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

    def length(self, nodes: Dict[int, Node3D]) -> float:
        """Compute member length from node coordinates."""
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

    def load_node_id(self) -> Optional[int]:
        """Return node ID with coupler_pin connection (load point)."""
        for c in self.connections:
            if c.conn_type == "coupler_pin":
                return c.node_id
        return None


def build_gt1r_frame_3d(
    bar_od: float = 1.5,
    bar_wall: float = 0.120,
    strut_od: float = 1.5,
    strut_wall: float = 0.120,
    arm_length: float = 18.0,
) -> FrameGeometry3D:
    """
    Build 3D GT1R parachute frame with rear trunk and under-hood assemblies.

    Coordinate system: X=transverse, Y=longitudinal, Z=vertical.
    Origin at left C3 mount.

    Args:
        bar_od: Horizontal bar outer diameter (inches)
        bar_wall: Horizontal bar wall thickness (inches)
        strut_od: V-strut outer diameter (inches)
        strut_wall: V-strut wall thickness (inches)
        arm_length: Parachute arm length (inches)

    Returns:
        FrameGeometry3D with both assemblies and frame rails.
    """
    bar_props = tube_section_properties(bar_od, bar_wall)
    strut_props = tube_section_properties(strut_od, strut_wall)

    hood_y = 60.0
    hood_z = 12.0

    # --- Assembly 1: Rear Trunk Frame ---
    nodes: Dict[int, Node3D] = {
        0: Node3D(0, 0.0, 0.0, 0.0, "left_c3_mount", "rear_trunk"),
        1: Node3D(1, 6.0, 0.0, 0.0, "left_bar_strut_junc", "rear_trunk"),
        2: Node3D(2, 18.0, 0.0, 0.0, "center_c1_bolt", "rear_trunk"),
        3: Node3D(3, 30.0, 0.0, 0.0, "right_bar_strut_junc", "rear_trunk"),
        4: Node3D(4, 36.0, 0.0, 0.0, "right_c3_mount", "rear_trunk"),
        5: Node3D(
            5, 18.0, 0.0, -7.25, "coupler_pin", "rear_trunk"
        ),
        6: Node3D(
            6, 18.0, -arm_length, -7.25, "rear_bracket", "rear_trunk"
        ),
    }

    # --- Assembly 2: Under-Hood Frame ---
    nodes[7] = Node3D(7, 0.0, hood_y, hood_z, "hood_bar_left", "under_hood")
    nodes[8] = Node3D(
        8, 14.0, hood_y, hood_z, "hood_bar_right", "under_hood"
    )
    nodes[9] = Node3D(
        9, 19.0, hood_y, hood_z - 1, "b1_bolted", "under_hood"
    )
    nodes[10] = Node3D(
        10, 14.0, hood_y + 8, hood_z, "p1_pinned", "under_hood"
    )

    # --- Members ---
    members = [
        # Rear trunk horizontal bar
        Member3D(0, 0, 1, "bar_left_end", "rear_trunk", bar_props),
        Member3D(1, 1, 2, "bar_left_center", "rear_trunk", bar_props),
        Member3D(2, 2, 3, "bar_center_right", "rear_trunk", bar_props),
        Member3D(3, 3, 4, "bar_right_end", "rear_trunk", bar_props),
        # Rear trunk v-struts
        Member3D(4, 1, 5, "v_strut_left", "rear_trunk", strut_props),
        Member3D(5, 3, 5, "v_strut_right", "rear_trunk", strut_props),
        # Parachute arm
        Member3D(6, 5, 6, "parachute_arm", "rear_trunk", strut_props),
        # Under-hood members
        Member3D(7, 7, 8, "hood_bar_main", "under_hood", bar_props),
        Member3D(8, 8, 9, "hood_bar_end", "under_hood", bar_props),
        Member3D(9, 8, 10, "curved_arm", "under_hood", strut_props),
        # Frame rails connecting assemblies
        Member3D(10, 0, 7, "frame_rail_left", "frame_rail", bar_props),
        Member3D(11, 4, 8, "frame_rail_right", "frame_rail", bar_props),
    ]

    # --- Connections ---
    connections = [
        # Rear trunk
        Connection(0, "C3", "fixed", "Weld — rigid BC at left frame rail"),
        Connection(4, "C3", "fixed", "Weld — rigid BC at right frame rail"),
        Connection(1, "C0", "free", "Weld — bar to strut junction (left)"),
        Connection(3, "C0", "free", "Weld — bar to strut junction (right)"),
        Connection(2, "C1", "free", "Bolt + pin — center bar connection"),
        Connection(
            5, "coupler_pin", "free", "Double pin — strut convergence"
        ),
        Connection(6, "bracket", "free", "Rear parachute bracket"),
        # Under-hood
        Connection(7, "C2", "free", "Hood bar left end"),
        Connection(8, "C2", "free", "Hood bar right junction"),
        Connection(9, "B1", "bolted", "Bolted connection under hood"),
        Connection(10, "P1", "pinned", "Pinned BC at hood arm"),
    ]

    return FrameGeometry3D(
        nodes=nodes,
        members=members,
        connections=connections,
    )
