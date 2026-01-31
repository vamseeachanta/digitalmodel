"""Builder for the Buoys section of OrcaFlex models.

This builder generates 6DBuoys and 3DBuoys sections, including tugs,
rollers, buoyancy modules (BM), and other buoy types.

IMPORTANT: OrcaFlex separates 6D buoys (6 degrees of freedom) and
3D buoys (3 degrees of freedom) into separate lists.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


# Default wireframe for box-shaped buoys (cube vertices and edges)
DEFAULT_WIREFRAME_VERTICES = [
    [3, 3, 3],
    [-3, 3, 3],
    [-3, -3, 3],
    [3, -3, 3],
    [3, 3, -3],
    [-3, 3, -3],
    [-3, -3, -3],
    [3, -3, -3],
]

DEFAULT_WIREFRAME_EDGES = [
    [1, 2, None],
    [2, 3, None],
    [3, 4, None],
    [4, 1, None],
    [5, 6, None],
    [6, 7, None],
    [7, 8, None],
    [8, 5, None],
    [1, 5, None],
    [2, 6, None],
    [3, 7, None],
    [4, 8, None],
]


@BuilderRegistry.register("08_buoys.yml", order=80)
class BuoysBuilder(BaseBuilder):
    """Builds the Buoys section of the OrcaFlex model.

    Generates both 6DBuoys and 3DBuoys lists, including:
    - Tugs: 6D buoys at calculated positions (first_position + i*spacing)
    - Rollers: 6D buoy with support geometry for pipe routing
    - Buoyancy Modules (BM): 6D buoys attached inline to pipeline
    - 3D reference buoys for mid-pipe markers

    Reference: 08_buoys.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the 6DBuoys and 3DBuoys sections from equipment configuration.

        Returns:
            Dictionary with '6DBuoys' and '3DBuoys' keys containing lists
            of buoy definitions.
        """
        six_d_buoys = []
        three_d_buoys = []

        buoy_names_6d = []
        buoy_names_3d = []

        # Get pipeline name from context or spec
        pipeline_name = self.spec.pipeline.name

        # Build rollers (6D buoy with support geometry)
        if self.spec.equipment.rollers:
            roller_buoy = self._build_roller(pipeline_name)
            six_d_buoys.append(roller_buoy)
            buoy_names_6d.append("Rollers")

        # Build tugs (6D buoys at calculated positions)
        if self.spec.equipment.tugs:
            tug_buoys = self._build_tugs(pipeline_name)
            six_d_buoys.extend(tug_buoys)
            buoy_names_6d.extend([f"Tug{i + 1}" for i in range(len(tug_buoys))])

        # Build buoyancy module template (6D buoy for inline attachment)
        if self.spec.equipment.buoyancy_modules:
            bm_buoy = self._build_buoyancy_module(pipeline_name)
            six_d_buoys.append(bm_buoy)
            buoy_names_6d.append("BM")

        # Build 6D end buoy for pipeline end connection
        end_buoy = self._build_end_buoy()
        six_d_buoys.append(end_buoy)
        buoy_names_6d.append("6D buoy1")

        # Build 3D mid-pipe marker
        mid_pipe = self._build_mid_pipe_marker(pipeline_name)
        three_d_buoys.append(mid_pipe)
        buoy_names_3d.append("Mid-pipe")

        # Register buoy names for cross-builder reference
        self._register_entity("buoy_names_6d", buoy_names_6d)
        self._register_entity("buoy_names_3d", buoy_names_3d)
        self._register_entity("all_buoy_names", buoy_names_6d + buoy_names_3d)
        self._register_entity("end_buoy_name", "6D buoy1")
        self._register_entity("bm_buoy_name", "BM")

        return {
            "6DBuoys": six_d_buoys,
            "3DBuoys": three_d_buoys,
        }

    def _build_roller(self, pipeline_name: str) -> dict[str, Any]:
        """Build roller buoy with support geometry.

        Args:
            pipeline_name: Name of the pipeline for SupportedLine reference.

        Returns:
            Dictionary representing a 6D buoy with support configuration.
        """
        rollers = self.spec.equipment.rollers
        pos = rollers.position

        # Build support positions (typically 4 supports for V-shaped rollers)
        supports = []
        support_positions = [
            [17.5, 3.5, -1.6],
            [-21.5, 2.5, -1.6],
            [17.5, 3.5, 1.6],
            [-21.5, 2.5, 1.6],
        ]
        support_orientations = [
            [0, 0, 0],
            [0, 0, 0],
            [0, 180, 0],
            [0, 180, 0],
        ]

        for i in range(min(rollers.supports, len(support_positions))):
            supports.append({
                "SupportType": rollers.support_type,
                "SupportCoordinateSystem": "Coordinate system1",
                "SupportPosition": support_positions[i],
                "SupportOrientation": support_orientations[i],
            })

        return {
            "Name": "Rollers",
            "BuoyType": "Lumped buoy",
            "Connection": "Fixed",
            "DampingRelativeTo": "Earth",
            "DisturbanceVessel": "(none)",
            "WaveCalculationMethod": "Specified by environment",
            "InitialPosition": pos,
            "InitialAttitude": [90, 180, 0],
            # Inertia
            "Mass": 0,
            "MomentsOfInertia": [0, 0, 0],
            "CentreOfMass": [0, 0, 0],
            # Properties
            "LumpedBuoyAddedMassMethod": "Diagonal values",
            "Volume": 0,
            "Height": 6,
            "CentreOfVolume": [0, 0, 0],
            "UnitDampingForce": [0, 0, 0],
            "UnitDampingMoment": [0, 0, 0],
            "DragArea": [0, 0, 0],
            "DragAreaMoment": [0, 0, 0],
            "DragForceCoefficient": [0, 0, 0],
            "DragMomentCoefficient": [0, 0, 0],
            "HydrodynamicMass": [0, 0, 0],
            "HydrodynamicInertia": [0, 0, 0],
            "AddedMassCoefficient": [0, 0, 0],
            "AddedInertiaCoefficient": [0, 0, 0],
            "FluidAccelerationForceCoefficient": [0, 0, 0],
            "LumpedBuoySlamArea": 0,
            "LumpedBuoySlamForceDataEntry": 0,
            "LumpedBuoySlamForceDataExit": 0,
            # Supports
            "SupportGeometrySpecification": "Explicit",
            "SupportReleaseStage": None,
            "SupportCoordinateSystems": [{
                "SupportCoordinateSystemName": "Coordinate system1",
                "SupportCoordinateSystemPos": [0, 0, 0],
                "SupportCoordinateSystemOrientation": [0, 0, 0],
            }],
            "SupportedLine": [pipeline_name],
            "Supports": supports,
            # Contact
            "SeabedFrictionCoefficient": 0,
            "TotalContactArea": 0,
            # Drawing
            "WireFrameOrigin": [0, 0, 0],
            "WireFrameSymmetry": "None",
            "WireFrameType": "Edges",
            "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
            "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
        }

    def _build_tugs(self, pipeline_name: str) -> list[dict[str, Any]]:
        """Build tug buoys at calculated positions.

        Positions are calculated as: first_position + i * spacing

        Args:
            pipeline_name: Name of the pipeline for SupportedLine reference.

        Returns:
            List of dictionaries representing 6D tug buoys.
        """
        tugs_config = self.spec.equipment.tugs
        props = tugs_config.properties
        first_pos = tugs_config.first_position
        spacing = tugs_config.spacing

        # Get moments of inertia or use default
        moi = props.moments_of_inertia or [100, 100, 100]

        tug_buoys = []
        for i in range(tugs_config.count):
            x = first_pos[0] + i * spacing
            y = first_pos[1]
            z = first_pos[2]

            tug = {
                "Name": f"Tug{i + 1}",
                "BuoyType": "Lumped buoy",
                "Connection": "Fixed",
                "DampingRelativeTo": "Earth",
                "DisturbanceVessel": "(none)",
                "WaveCalculationMethod": "Specified by environment",
                "InitialPosition": [x, y, z],
                "InitialAttitude": [90, 180, 0],
                # Inertia
                "Mass": props.mass,
                "MomentsOfInertia": moi,
                "CentreOfMass": [0, 0, 0],
                # Properties
                "LumpedBuoyAddedMassMethod": "Diagonal values",
                "Volume": props.volume,
                "BulkModulus": "Infinity",
                "Height": props.height,
                "CentreOfVolume": [0, 0, 0],
                "UnitDampingForce": [0, 0, 0],
                "UnitDampingMoment": [0, 0, 0],
                "DragArea": [0, 0, 0],
                "DragAreaMoment": [0, 0, 0],
                "DragForceCoefficient": [0, 0, 0],
                "DragMomentCoefficient": [0, 0, 0],
                "HydrodynamicMass": [None, None, None],
                "HydrodynamicInertia": [0, 0, 0],
                "AddedMassCoefficient": [0, 0, 0],
                "AddedInertiaCoefficient": [0, 0, 0],
                "FluidAccelerationForceCoefficient": [None, None, None],
                "LumpedBuoySlamArea": 0,
                "LumpedBuoySlamForceDataEntry": 0,
                "LumpedBuoySlamForceDataExit": 0,
                # Supports
                "SupportGeometrySpecification": "Explicit",
                "SupportReleaseStage": None,
                "SupportCoordinateSystems": [{
                    "SupportCoordinateSystemName": "Coordinate system1",
                    "SupportCoordinateSystemPos": [0, 0, 0],
                    "SupportCoordinateSystemOrientation": [0, 0, 0],
                }],
                "SupportedLine": [pipeline_name],
                "Supports": [{
                    "SupportType": "Support type1",
                    "SupportCoordinateSystem": "Coordinate system1",
                    "SupportPosition": [0, 0, 0],
                    "SupportOrientation": [0, 0, 0],
                }],
                # Contact
                "SeabedFrictionCoefficient": 0,
                "TotalContactArea": 0,
                # Drawing
                "WireFrameOrigin": [0, 0, 0],
                "WireFrameSymmetry": "None",
                "WireFrameType": "Edges",
                "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
                "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
            }
            tug_buoys.append(tug)

        return tug_buoys

    def _build_buoyancy_module(self, pipeline_name: str) -> dict[str, Any]:
        """Build buoyancy module (BM) template for inline attachment.

        Args:
            pipeline_name: Name of the pipeline for Connection reference.

        Returns:
            Dictionary representing a 6D buoy configured as buoyancy module.
        """
        bm_config = self.spec.equipment.buoyancy_modules
        props = bm_config.properties

        # BM is connected to the pipeline (not fixed)
        # Initial position is relative to the line connection
        return {
            "Name": "BM",
            "BuoyType": "Lumped buoy",
            "Connection": pipeline_name,
            "DampingRelativeTo": "Earth",
            "DisturbanceVessel": "(none)",
            "WaveCalculationMethod": "Specified by environment",
            "InitialPosition": [-1.25, 0, 3.9],  # Offset from line
            "ConnectionzRelativeTo": "End A",
            "InitialAttitude": [0, 0, 0],
            # Inertia
            "Mass": props.mass,
            "MomentsOfInertia": [1, 1, 1],
            "CentreOfMass": [0, 0, 0],
            # Properties
            "LumpedBuoyAddedMassMethod": "Diagonal values",
            "Volume": props.volume,
            "BulkModulus": "Infinity",
            "Height": props.height,
            "CentreOfVolume": [0, 0, 0],
            "UnitDampingForce": [0, 0, 0],
            "UnitDampingMoment": [0, 0, 0],
            "DragArea": [5.25, 5.25, 1.767],
            "DragAreaMoment": [0, 0, 0],
            "DragForceCoefficient": [1.18, 1.18, 0.88],
            "DragMomentCoefficient": [0, 0, 0],
            "HydrodynamicMass": [None, None, None],
            "HydrodynamicInertia": [0, 0, 0],
            "AddedMassCoefficient": [0, 0, 0],
            "AddedInertiaCoefficient": [0, 0, 0],
            "FluidAccelerationForceCoefficient": [None, None, None],
            "LumpedBuoySlamArea": 0,
            "LumpedBuoySlamForceDataEntry": 0,
            "LumpedBuoySlamForceDataExit": 0,
            # Contact
            "SeabedFrictionCoefficient": 0,
            "TotalContactArea": 0,
            # Drawing (smaller box for BM)
            "WireFrameOrigin": [0, 0, 0],
            "WireFrameSymmetry": "None",
            "WireFrameType": "Edges",
            "VertexX, VertexY, VertexZ": [
                [0.75, 0.75, 1.75],
                [-0.75, 0.75, 1.75],
                [-0.75, -0.75, 1.75],
                [0.75, -0.75, 1.75],
                [0.75, 0.75, -1.75],
                [-0.75, 0.75, -1.75],
                [-0.75, -0.75, -1.75],
                [0.75, -0.75, -1.75],
            ],
            "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
        }

    def _build_end_buoy(self) -> dict[str, Any]:
        """Build a free 6D buoy for pipeline end connection.

        This buoy represents the free end of the pipeline during
        floating installation.

        Returns:
            Dictionary representing a free 6D buoy.
        """
        # Calculate approximate position based on pipeline length
        total_length = self.spec.get_total_pipeline_length()

        # Estimate position (this would be refined by statics)
        estimated_x = total_length * 0.98  # Near end of pipeline
        estimated_y = -13  # Slight lateral offset
        estimated_z = -1.3  # Below water surface

        return {
            "Name": "6D buoy1",
            "BuoyType": "Lumped buoy",
            "Connection": "Free",
            "DegreesOfFreedomInStatics": "All",
            "DampingRelativeTo": "Earth",
            "DisturbanceVessel": "(none)",
            "WaveCalculationMethod": "Specified by environment",
            "InitialPosition": [estimated_x, estimated_y, estimated_z],
            "InitialAttitude": [0.96, 0.18, 4.27],
            # Inertia
            "Mass": 0,
            "MomentsOfInertia": [0, 0, 0],
            "CentreOfMass": [0, 0, 0],
            # Properties
            "LumpedBuoyAddedMassMethod": "Diagonal values",
            "Volume": 0,
            "Height": 6,
            "CentreOfVolume": [0, 0, 0],
            "UnitDampingForce": [0, 0, 0],
            "UnitDampingMoment": [0, 0, 0],
            "DragArea": [0, 0, 0],
            "DragAreaMoment": [0, 0, 0],
            "DragForceCoefficient": [0, 0, 0],
            "DragMomentCoefficient": [0, 0, 0],
            "HydrodynamicMass": [0, 0, 0],
            "HydrodynamicInertia": [0, 0, 0],
            "AddedMassCoefficient": [0, 0, 0],
            "AddedInertiaCoefficient": [0, 0, 0],
            "FluidAccelerationForceCoefficient": [0, 0, 0],
            "LumpedBuoySlamArea": 0,
            "LumpedBuoySlamForceDataEntry": 0,
            "LumpedBuoySlamForceDataExit": 0,
            # Contact
            "SeabedFrictionCoefficient": 0,
            "TotalContactArea": 0,
            # Drawing
            "Pen": [1, "Solid", "$0080FF"],
            "WireFrameOrigin": [0, 0, 0],
            "WireFrameSymmetry": "None",
            "WireFrameType": "Edges",
            "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
            "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
        }

    def _build_mid_pipe_marker(self, pipeline_name: str) -> dict[str, Any]:
        """Build 3D buoy marker at mid-pipe position.

        Args:
            pipeline_name: Name of the pipeline for Connection reference.

        Returns:
            Dictionary representing a 3D buoy marker.
        """
        total_length = self.spec.get_total_pipeline_length()
        mid_position = total_length / 2

        return {
            "Name": "Mid-pipe",
            "Connection": pipeline_name,
            "InitialPosition": [0, 0, mid_position],
            "ConnectionzRelativeTo": "End A",
            "Mass": 0,
            "Volume": 0,
            "BulkModulus": "Infinity",
            "Height": 1,
            "SeabedFrictionCoefficient": 0,
            "ContactArea": None,
            "DisturbanceVessel": "(none)",
            "WaveCalculationMethod": "Specified by environment",
            "DragArea": [0, 0, 0],
        }
