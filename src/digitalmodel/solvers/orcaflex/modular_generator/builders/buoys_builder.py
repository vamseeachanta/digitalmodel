"""Builder for the Buoys section of OrcaFlex models.

This builder generates 6DBuoys and 3DBuoys sections, including tugs,
rollers, buoyancy modules (BM), and other buoy types.

IMPORTANT: OrcaFlex separates 6D buoys (6 degrees of freedom) and
3D buoys (3 degrees of freedom) into separate lists.
"""

import math
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

    def should_generate(self) -> bool:
        """Generate if pipeline model (floating or with roller arrangement)."""
        if not self.spec.is_pipeline():
            return False
        if not self.spec.is_s_lay():
            return True
        # S-lay with route rollers still needs buoy generation
        return self.spec.equipment.get_effective_rollers() is not None

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

        # Build rollers (6D buoys with support geometry)
        roller_names = []
        effective_rollers = self.spec.equipment.get_effective_rollers()
        if effective_rollers is not None:
            roller_buoys, roller_names = self._build_roller_arrangement(
                pipeline_name, effective_rollers
            )
            six_d_buoys.extend(roller_buoys)
            buoy_names_6d.extend(roller_names)

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
        self._register_entity("roller_buoy_names", roller_names)

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

    def _build_roller_arrangement(
        self, pipeline_name: str, arrangement
    ) -> tuple[list[dict[str, Any]], list[str]]:
        """Build roller buoys from a RollerArrangement.

        Args:
            pipeline_name: Pipeline name for SupportedLine reference.
            arrangement: RollerArrangement instance.

        Returns:
            Tuple of (list of buoy dicts, list of buoy names).
        """
        buoys = []
        names = []

        for idx, station in enumerate(arrangement.stations):
            name = f"Roller_{idx + 1}" if len(arrangement.stations) > 1 else "Rollers"

            supports = self.get_support_geometry(station, arrangement.type)

            buoy = {
                "Name": name,
                "BuoyType": "Lumped buoy",
                "Connection": "Fixed",
                "DampingRelativeTo": "Earth",
                "DisturbanceVessel": "(none)",
                "WaveCalculationMethod": "Specified by environment",
                "InitialPosition": station.position,
                "InitialAttitude": [90, 180, 0],
                "Mass": 0,
                "MomentsOfInertia": [0, 0, 0],
                "CentreOfMass": [0, 0, 0],
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
                "SupportGeometrySpecification": "Explicit",
                "SupportReleaseStage": None,
                "SupportCoordinateSystems": [{
                    "SupportCoordinateSystemName": "Coordinate system1",
                    "SupportCoordinateSystemPos": [0, 0, 0],
                    "SupportCoordinateSystemOrientation": [0, 0, 0],
                }],
                "SupportedLine": [pipeline_name],
                "Supports": supports,
                "SeabedFrictionCoefficient": station.friction_coefficient,
                "TotalContactArea": 0,
                "WireFrameOrigin": [0, 0, 0],
                "WireFrameSymmetry": "None",
                "WireFrameType": "Edges",
                "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
                "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
            }
            buoys.append(buoy)
            names.append(name)

        return buoys, names

    @staticmethod
    def get_support_geometry(station, roller_type) -> list[dict[str, Any]]:
        """Calculate support positions based on roller type and geometry.

        V-roller geometry (per support pair):
            half_angle = v_angle / 2
            r = diameter / 2
            lateral_offset = r * sin(radians(half_angle))
            vertical_offset = -r * cos(radians(half_angle))

        Args:
            station: RollerStation instance.
            roller_type: RollerType enum value.

        Returns:
            List of support dicts for OrcaFlex.
        """
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        supports = []
        r = station.diameter / 2
        h_off = station.height_offset

        if roller_type == RollerType.V_ROLLER and station.v_angle is not None:
            half_angle = station.v_angle / 2
            lateral = r * math.sin(math.radians(half_angle))
            vertical = -r * math.cos(math.radians(half_angle)) + h_off

            # Primary pair
            supports.append({
                "SupportType": station.support_type,
                "SupportCoordinateSystem": "Coordinate system1",
                "SupportPosition": [0, lateral, vertical],
                "SupportOrientation": [0, 0, 0],
            })
            supports.append({
                "SupportType": station.support_type,
                "SupportCoordinateSystem": "Coordinate system1",
                "SupportPosition": [0, -lateral, vertical],
                "SupportOrientation": [0, 180, 0],
            })

            # Additional supports distributed along X-axis
            for j in range(2, station.support_count):
                x_offset = (j - 1) * 0.5 * (1 if j % 2 == 0 else -1)
                side = 1 if j % 2 == 0 else -1
                supports.append({
                    "SupportType": station.support_type,
                    "SupportCoordinateSystem": "Coordinate system1",
                    "SupportPosition": [x_offset, side * lateral, vertical],
                    "SupportOrientation": [0, 0 if side > 0 else 180, 0],
                })

        elif roller_type == RollerType.FLAT:
            # Flat: supports in horizontal row
            for j in range(station.support_count):
                y_offset = (j - station.support_count / 2 + 0.5) * r
                supports.append({
                    "SupportType": station.support_type,
                    "SupportCoordinateSystem": "Coordinate system1",
                    "SupportPosition": [0, y_offset, h_off],
                    "SupportOrientation": [0, 0, 0],
                })

        elif roller_type == RollerType.CRADLE:
            # Cradle: semicircular arc below pipe
            for j in range(station.support_count):
                angle = math.pi * (j + 0.5) / station.support_count
                y = r * math.cos(angle)
                z = -r * math.sin(angle) + h_off
                supports.append({
                    "SupportType": station.support_type,
                    "SupportCoordinateSystem": "Coordinate system1",
                    "SupportPosition": [0, y, z],
                    "SupportOrientation": [0, 0, 0],
                })

        return supports

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
