"""Builder for the Lines section of OrcaFlex riser models.

This builder generates Lines with riser configuration including:
- End connections (vessel at top, anchor/seabed at bottom)
- Segment array with LineType, Length, TargetSegmentLength
- Buoyancy zones for wave configurations
- Clump attachments for discrete buoyancy/weights
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("07_riser_lines.yml", order=92)
class RiserLinesBuilder(BaseBuilder):
    """Builds the Lines section for riser models.

    Generates Line definitions including:
    - Complete connection specification (End A at vessel, End B at anchor)
    - Segment array with line types, lengths, and mesh sizes
    - Statics method appropriate for configuration
    - Contents specification

    Reference: OrcaFlex Line documentation.
    """

    def should_generate(self) -> bool:
        """Only generate for riser models."""
        return self.spec.is_riser()

    def build(self) -> dict[str, Any]:
        """Build the Lines section from riser configuration.

        Returns:
            Dictionary with 'Lines' key containing list of riser line definitions.
        """
        riser = self.spec.riser
        lines_list = []
        line_names = []

        for riser_line in riser.lines:
            line_def = self._build_riser_line(riser_line, riser)
            lines_list.append(line_def)
            line_names.append(riser_line.name)

        # Register line names for cross-builder reference
        self._register_entity("riser_line_names", line_names)
        self._register_entity("line_names", line_names)

        return {"Lines": lines_list}

    def _build_riser_line(self, riser_line, riser) -> dict[str, Any]:
        """Build a single riser line definition.

        Args:
            riser_line: RiserLine object from spec.
            riser: Parent Riser object for cross-references.

        Returns:
            Dictionary representing an OrcaFlex Line definition.
        """
        # Build segment table entries
        segment_data = self._build_segment_data(riser_line.sections)

        # Build connection data
        connection_data = self._build_connection_data(riser_line)

        line_def = {
            "Name": riser_line.name,
            "IncludeTorsion": "Yes" if riser_line.include_torsion else "No",
            "TopEnd": "End A",
            "LengthAndEndOrientations": "Explicit",
            "Representation": "Finite element",
            "PyModel": "(none)",
            "PreBendSpecifiedBy": "Bend angle",
            "DragFormulation": "Standard",
            "StaticsVIV": "None",
            "DynamicsVIV": "None",
            "WaveCalculationMethod": "Specified by environment",
            # Connection specification (multi-column format)
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo": connection_data["connections"],
            # Connection stiffness
            "ConnectionxBendingStiffness, ConnectionyBendingStiffness, "
            "ConnectionTwistingStiffness, ConnectionDamping": connection_data["stiffness"],
            # Segment data (multi-column format)
            "LineType, Length, TargetSegmentLength": segment_data,
            # Statics method
            "StaticsStep1": riser_line.statics_method,
            "LayAzimuth": riser_line.lay_azimuth,
            # Contents
            "ContentsMethod": "Uniform",
            "ContentsDensity": riser_line.contents.density,
        }

        # Add StaticsStep2 if specified (needed for wave configurations)
        if riser_line.statics_step2:
            line_def["StaticsStep2"] = riser_line.statics_step2

        # Add clump attachments if defined (multi-column format)
        if riser_line.clump_attachments:
            attachments = self._build_clump_attachments(riser_line.clump_attachments)
            if attachments:
                line_def[
                    "AttachmentType, Attachmentx, Attachmenty, "
                    "Attachmentz, AttachmentzRelativeTo"
                ] = attachments

        return line_def

    def _build_segment_data(self, sections: list) -> list[list]:
        """Build segment data array for OrcaFlex Line definition.

        Each segment entry is [LineType, Length, TargetSegmentLength].

        Args:
            sections: List of RiserSection objects from spec.

        Returns:
            List of segment data entries.
        """
        segment_data = []

        for section in sections:
            entry = [
                section.line_type,  # LineType name
                section.length,  # Total length
                section.segment_length,  # Target mesh segment length
            ]
            segment_data.append(entry)

        return segment_data

    def _build_connection_data(self, riser_line) -> dict[str, list]:
        """Build connection specification data for End A and End B.

        Args:
            riser_line: RiserLine object from spec.

        Returns:
            Dictionary with 'connections' and 'stiffness' keys.
        """
        end_a = riser_line.end_a
        end_b = riser_line.end_b

        # End A connection (typically vessel)
        conn_a = self._format_connection(end_a)

        # End B connection (typically anchor)
        conn_b = self._format_connection(end_b)

        # Connection stiffness based on end bending stiffness
        stiffness = [
            [end_a.bending_stiffness, None, None],
            [end_b.bending_stiffness, None, None],
        ]

        return {
            "connections": [conn_a, conn_b],
            "stiffness": stiffness,
        }

    def _format_connection(self, end_conn) -> list:
        """Format an EndConnection as OrcaFlex connection array.

        Args:
            end_conn: EndConnection object from spec.

        Returns:
            List representing OrcaFlex connection data.
        """
        from ..schema.riser import ConnectionType

        # Map connection type to OrcaFlex format
        if end_conn.type == ConnectionType.VESSEL:
            conn_type = end_conn.name  # Use vessel name
        elif end_conn.type == ConnectionType.ANCHOR:
            conn_type = "Anchored"
        elif end_conn.type == ConnectionType.FIXED:
            conn_type = "Fixed"
        elif end_conn.type == ConnectionType.FREE:
            conn_type = "Free"
        elif end_conn.type == ConnectionType.BUOY:
            conn_type = end_conn.name  # Use buoy name
        else:
            conn_type = "Fixed"

        return [
            conn_type,
            end_conn.position[0],  # X
            end_conn.position[1],  # Y
            end_conn.position[2],  # Z
            end_conn.azimuth,
            end_conn.declination,
            end_conn.gamma,
            None,  # ReleaseStage
        ]

    def _build_clump_attachments(self, clump_config) -> list[list]:
        """Build attachment list in OrcaFlex multi-column format.

        Generates attachments at arc-length positions based on spacing.
        Output format matches OrcaFlex YAML:
            [AttachmentType, x, y, z, zRelativeTo]

        Args:
            clump_config: ClumpAttachments object from spec.

        Returns:
            List of [type, x, y, z, reference] entries.
        """
        positions = []
        current = clump_config.start_arc_length
        while current <= clump_config.end_arc_length + 1e-9:
            positions.append(round(current, 6))
            current += clump_config.spacing

        attachments = []
        for arc_length in positions:
            attachments.append([
                clump_config.clump_type,
                0,  # x offset
                0,  # y offset
                arc_length,  # z = arc length from reference
                "End A",  # z relative to End A
            ])

        return attachments
