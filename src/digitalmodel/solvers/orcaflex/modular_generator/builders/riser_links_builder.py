"""Builder for OrcaFlex Link objects (tethers, spring/dampers) on riser models.

Links connect two points with axial stiffness. Tethers resist tension only
and are commonly used for pliant wave riser restraints connecting the riser
at the buoyancy zone to a seabed anchor.

OrcaFlex YAML format for Links uses a multi-column connection key:
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo
with list-of-lists values where each inner list is one connection endpoint.
"""

import math
from typing import Any

from ..schema.riser import LinkType
from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("08_riser_links.yml", order=93)
class RiserLinksBuilder(BaseBuilder):
    """Builds the Links section for riser models.

    Generates Link definitions (tethers, spring/dampers) that constrain
    riser geometry. Runs after riser_lines_builder (order=92) so line
    names are available in context for connection references.

    Reference: OrcaFlex Link documentation.
    """

    # Object sections emitted by build(); read by
    # writers.basefile.object_section_order() to derive a section
    # dependency order from this builder's registered order.
    _sections = (
        "Links",
    )

    def should_generate(self) -> bool:
        """Only generate for riser models with links defined."""
        riser = self.spec.riser if self.spec.is_riser() else None
        return riser is not None and (len(riser.links) > 0 or len(riser.tensioners) > 0)

    def build(self) -> dict[str, Any]:
        """Build the Links section from riser link definitions.

        Returns:
            Dictionary with 'Links' key containing list of link definitions.
        """
        riser = self.spec.riser
        links_list = []
        link_names = []

        for riser_link in riser.links:
            link_def = self._build_link(riser_link)
            links_list.append(link_def)
            link_names.append(riser_link.name)

        for tensioner in riser.tensioners:
            for link_def in self._build_tensioner_links(tensioner, riser):
                links_list.append(link_def)
                link_names.append(link_def["Name"])

        self._register_entity("riser_link_names", link_names)

        return {"Links": links_list}

    def _build_link(self, riser_link) -> dict[str, Any]:
        """Build a single Link definition in OrcaFlex YAML format.

        Args:
            riser_link: RiserLink object from spec.

        Returns:
            Dictionary representing an OrcaFlex Link entry.
        """

        # Map link type enum to OrcaFlex string
        link_type_map = {
            LinkType.TETHER: "Tether",
            LinkType.SPRING_DAMPER: "Spring/Damper",
        }

        # Build connection array: each connection is a list
        connection_data = []
        for conn in riser_link.connections:
            entry = [conn.object_name, conn.x, conn.y, conn.z]
            if conn.z_relative_to:
                entry.append(conn.z_relative_to)
            connection_data.append(entry)

        link_def = {
            "Name": riser_link.name,
            "LinkType": link_type_map[riser_link.link_type],
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionzRelativeTo": connection_data,
            "UnstretchedLength": riser_link.unstretched_length,
            "Stiffness": riser_link.stiffness,
        }

        if riser_link.link_type == LinkType.SPRING_DAMPER:
            # Native spring/damper data (public example "H03 Floating and stowed lines"):
            # a linear spring uses UnstretchedLength and Stiffness (kN/m).
            link_def["LinearSpring"] = "Yes"
            link_def["LinearDamper"] = "Yes"
            link_def["DampingConstant"] = 0

        if riser_link.release_stage is not None:
            link_def["ReleaseStage"] = riser_link.release_stage

        return link_def

    @staticmethod
    def tensioner_geometry(tensioner, riser) -> list[dict[str, float]]:
        """Vessel points, link length, tension and unstretched length per unit.

        The riser attachment point is the free End A node. Coordinates of the
        vessel points are in vessel axes (vessel position and heading).
        """
        line = next(line for line in riser.lines if line.name == tensioner.line)
        vessel = riser.vessel
        heading = math.radians(vessel.orientation[2])
        dx = line.end_a.position[0] - vessel.position[0]
        dy = line.end_a.position[1] - vessel.position[1]
        top_x = math.cos(heading) * dx + math.sin(heading) * dy
        top_y = -math.sin(heading) * dx + math.cos(heading) * dy
        attach_z = line.end_a.position[2] - vessel.position[2]
        dz = tensioner.vessel_z - attach_z
        if dz <= 0:
            raise ValueError(
                f"Tensioner '{tensioner.name}': vessel_z {tensioner.vessel_z} m must be "
                f"above the riser attachment ({attach_z} m in vessel axes)"
            )
        length = math.hypot(tensioner.radius, dz)
        tension = tensioner.total_tension / tensioner.count * length / dz
        unstretched = length - tension / tensioner.stiffness
        if unstretched <= 0:
            raise ValueError(
                f"Tensioner '{tensioner.name}': stiffness {tensioner.stiffness} kN/m is "
                f"too low for {tension:.1f} kN over {length:.3f} m"
            )
        units = []
        for i in range(tensioner.count):
            angle = math.radians(tensioner.start_angle + 360.0 * i / tensioner.count)
            units.append({
                "x": top_x + tensioner.radius * math.cos(angle),
                "y": top_y + tensioner.radius * math.sin(angle),
                "z": tensioner.vessel_z,
                "length": length,
                "tension": tension,
                "unstretched_length": unstretched,
            })
        return units

    def _build_tensioner_links(self, tensioner, riser) -> list[dict[str, Any]]:
        """Linear spring/damper links for one tensioner system (#3976)."""
        links = []
        for i, unit in enumerate(self.tensioner_geometry(tensioner, riser), start=1):
            link_def = {
                "Name": f"{tensioner.name}_{i}",
                "LinkType": "Spring/Damper",
                "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                "ConnectionzRelativeTo": [
                    [riser.vessel.name, round(unit["x"], 6), round(unit["y"], 6),
                     round(unit["z"], 6)],
                    [tensioner.line, 0, 0, tensioner.arc_length, "End A"],
                ],
                "LinearSpring": "Yes",
                "UnstretchedLength": round(unit["unstretched_length"], 6),
                "Stiffness": tensioner.stiffness,
                "LinearDamper": "Yes",
                "DampingConstant": tensioner.damping,
            }
            links.append(link_def)
        return links
