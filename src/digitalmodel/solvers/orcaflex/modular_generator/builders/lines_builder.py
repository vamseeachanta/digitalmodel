"""Builder for the Lines section of OrcaFlex models.

This builder generates Lines with pipeline configuration including:
- Connection definitions (End A = Fixed, End B = buoy reference)
- Segment array with LineType, Length, TargetSegmentLength
- Attachments at arc-length positions based on buoyancy module spacing
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("07_lines.yml", order=90)
class LinesBuilder(BaseBuilder):
    """Builds the Lines section of the OrcaFlex model.

    Generates Line definitions including:
    - Complete connection specification (End A and End B)
    - Segment array with line types, lengths, and mesh sizes
    - Inline attachments for buoyancy modules at calculated positions
    - Connection stiffness and arclength settings

    The output format uses OrcaFlex's multi-column key syntax for
    compact representation of connection and segment data.

    Reference: 07_lines.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the Lines section from pipeline and equipment configuration.

        Returns:
            Dictionary with 'Lines' key containing list of line definitions.
        """
        pipeline = self.spec.pipeline
        lines_list = []

        # Get end buoy name from context (populated by BuoysBuilder)
        end_buoy_name = self.context.end_buoy_name
        bm_buoy_name = self.context.bm_buoy_name

        # Build main pipeline line
        main_line = self._build_pipeline_line(
            name=pipeline.name,
            segments=pipeline.segments,
            end_buoy_name=end_buoy_name,
            bm_buoy_name=bm_buoy_name,
        )
        lines_list.append(main_line)

        # Add winch wire for floating installations with tugs
        if self.spec.equipment.tugs:
            total_length = sum(s.length for s in pipeline.segments)
            winch_wire = self._build_winch_wire_line(
                end_buoy_name=end_buoy_name,
                pipeline_length=total_length,
            )
            lines_list.append(winch_wire)
            self._register_entity("additional_line_names", ["Winch wire"])

        # Register line names for cross-builder reference
        line_names = [pipeline.name]
        self._register_entity("line_names", line_names)
        self._register_entity("main_pipeline_name", pipeline.name)

        return {"Lines": lines_list}

    def _build_pipeline_line(
        self,
        name: str,
        segments: list,
        end_buoy_name: str,
        bm_buoy_name: str,
    ) -> dict[str, Any]:
        """Build the main pipeline line definition.

        Args:
            name: Pipeline name (e.g., "30'' Line")
            segments: List of Segment objects from spec
            end_buoy_name: Name of the 6D buoy for End B connection
            bm_buoy_name: Name of the buoyancy module buoy type

        Returns:
            Dictionary representing an OrcaFlex Line definition.
        """
        # Build segment table entries
        segment_data = self._build_segment_data(segments)

        # Build connection data
        connection_data = self._build_connection_data(end_buoy_name)

        # Build attachments for buoyancy modules
        attachments = self._build_attachments(bm_buoy_name)

        line_def = {
            "Name": name,
            "IncludeTorsion": "Yes",
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
            # Connection arclength settings
            "ConnectionInitialArclength, ConnectionPayoutRate, "
            "ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, "
            "ConnectionUseSmoothGrowth": connection_data["arclength"],
            # Segment data (multi-column format)
            "LineType, Length, TargetSegmentLength": segment_data,
        }

        # Add attachments if present
        if attachments:
            line_def["Attachments"] = attachments

        return line_def

    def _build_segment_data(self, segments: list) -> list[list]:
        """Build segment data array for OrcaFlex Line definition.

        Each segment entry is [LineType, Length, TargetSegmentLength].

        Args:
            segments: List of Segment objects from spec.

        Returns:
            List of segment data entries.
        """
        segment_data = []

        for segment in segments:
            entry = [
                segment.type,  # LineType name
                segment.length,  # Total length
                segment.segment_length,  # Target mesh segment length
            ]
            segment_data.append(entry)

        return segment_data

    def _build_connection_data(self, end_buoy_name: str) -> dict[str, list]:
        """Build connection specification data for End A and End B.

        Connection logic depends on installation type:
        - Floating: End A=Fixed, End B=end buoy
        - S-lay: End A=Free (vessel/stinger), End B=Anchored

        Args:
            end_buoy_name: Name of the 6D buoy for End B connection (floating).

        Returns:
            Dictionary with 'connections', 'stiffness', and 'arclength' keys.
        """
        seabed_slope = self.spec.environment.seabed.slope
        is_s_lay = self.spec.is_s_lay()

        if is_s_lay:
            vessel_name = self.spec.equipment.vessel.name

            # End A: Free - pipeline pays out from vessel
            end_a = [
                vessel_name,  # Connected to vessel
                0,  # X offset
                0,  # Y offset
                0,  # Z offset
                0,  # Azimuth
                90,  # Declination
                0,  # Gamma
                None,  # ReleaseStage
            ]

            # End B: Anchored at seabed
            end_b = [
                "Anchored",  # Anchored at seabed
                0,  # X position
                0,  # Y position
                0,  # Z position (at seabed)
                0,  # Azimuth
                90 + seabed_slope,  # Declination
                0,  # Gamma
                None,  # ReleaseStage
            ]
        else:
            # Floating installation (existing logic)
            end_a = [
                "Fixed",  # Connection type
                -101,  # X position (before pipeline start)
                0,  # Y position
                4.505,  # Z position (slightly above seabed)
                0,  # Azimuth
                90 + seabed_slope,  # Declination
                0,  # Gamma
                None,  # ReleaseStage
            ]

            end_b = [
                end_buoy_name,  # Connection to 6D buoy
                0,  # X offset
                0,  # Y offset
                0,  # Z offset
                0,  # Azimuth
                90,  # Declination (horizontal)
                0,  # Gamma
                None,  # ReleaseStage
            ]

        # Connection stiffness (End A, End B)
        stiffness = [
            ["Infinity", None, "Infinity"],  # End A: rigid
            ["Infinity", None, "Infinity"],  # End B: rigid
        ]

        # Arclength settings (End A, End B)
        arclength = [
            [None, 0, 0.001],  # End A
            [None, 0, 0.001],  # End B
        ]

        return {
            "connections": [end_a, end_b],
            "stiffness": stiffness,
            "arclength": arclength,
        }

    def _build_attachments(self, bm_buoy_name: str) -> list[dict[str, Any]]:
        """Build attachment list for buoyancy modules.

        Generates attachments at arc-length positions based on BM spacing.

        Args:
            bm_buoy_name: Name of the buoyancy module buoy type.

        Returns:
            List of attachment dictionaries.
        """
        if not self.spec.equipment.buoyancy_modules:
            return []

        bm_config = self.spec.equipment.buoyancy_modules
        positions = self.spec.get_buoyancy_module_positions()

        attachments = []
        for i, arc_length in enumerate(positions):
            attachment = {
                "AttachmentType": bm_buoy_name,
                "AttachmentName": f"Attached 6D buoy{i + 3}",  # Start from 3
                "AttachmentPosition": [
                    -1.25,  # X offset from line centerline
                    0,  # Y offset
                    arc_length,  # Arc length from End A
                    "End A",  # Reference point
                ],
                "AttachmentOrientation": [0, 0, 0],
            }
            attachments.append(attachment)

        return attachments

    def _build_winch_wire_line(
        self,
        end_buoy_name: str,
        pipeline_length: float,
    ) -> dict[str, Any]:
        """Build a winch wire line from end buoy to fixed vessel point.

        For floating installations, the winch wire provides End B restraint
        by connecting the 6D buoy at the vessel end to a fixed point
        beyond the pipeline end. Without this, statics will not converge.

        Args:
            end_buoy_name: Name of the 6D buoy (e.g., "6D buoy1").
            pipeline_length: Total pipeline length (m) for positioning fixed end.

        Returns:
            Dictionary representing an OrcaFlex Line definition for winch wire.
        """
        fixed_x = pipeline_length + 60  # Fixed point beyond pipeline end

        return {
            "Name": "Winch wire",
            "IncludeTorsion": "No",
            "TopEnd": "End A",
            "LengthAndEndOrientations": "Explicit",
            "Representation": "Finite element",
            "PyModel": "(none)",
            "PreBendSpecifiedBy": "Bend angle",
            "DragFormulation": "Standard",
            "StaticsVIV": "None",
            "DynamicsVIV": "None",
            "WaveCalculationMethod": "Specified by environment",
            # Connection: 9-column header, 8-value rows (ConnectionzRelativeTo implicit null)
            # End A Gamma=-5.75° aligns wire with 6D buoy orientation for statics convergence
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo": [
                [end_buoy_name, 0, 0, 0, 0, 0, -5.753732922130843, None],
                ["Fixed", fixed_x, 0, 2, 0, 0, 180, None],
            ],
            "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [
                [0, 0],
                [0, None],
            ],
            "ConnectionInitialArclength, ConnectionPayoutRate, "
            "ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, "
            "ConnectionUseSmoothGrowth": [
                [None, 0, 0.001],
                [None, 0, 0.001],
            ],
            "LineType, Length, TargetSegmentLength": [
                ["Winch wire_LT", 165, 5],
            ],
            "StaticsStep1": "User specified",
            "ContentsMethod": "Uniform",
            "ContentsDensity": 0,
        }
