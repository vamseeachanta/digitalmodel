"""Builder for OrcaFlex Link objects (tethers, spring/dampers) on riser models.

Links connect two points with axial stiffness. Tethers resist tension only
and are commonly used for pliant wave riser restraints connecting the riser
at the buoyancy zone to a seabed anchor.

OrcaFlex YAML format for Links uses a multi-column connection key:
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo
with list-of-lists values where each inner list is one connection endpoint.
"""

from typing import Any

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

    def should_generate(self) -> bool:
        """Only generate for riser models with links defined."""
        return self.spec.is_riser() and len(self.spec.riser.links) > 0

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

        self._register_entity("riser_link_names", link_names)

        return {"Links": links_list}

    def _build_link(self, riser_link) -> dict[str, Any]:
        """Build a single Link definition in OrcaFlex YAML format.

        Args:
            riser_link: RiserLink object from spec.

        Returns:
            Dictionary representing an OrcaFlex Link entry.
        """
        from ..schema.riser import LinkType

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

        if riser_link.release_stage is not None:
            link_def["ReleaseStage"] = riser_link.release_stage

        return link_def
