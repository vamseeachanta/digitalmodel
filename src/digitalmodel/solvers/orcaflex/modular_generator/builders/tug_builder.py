"""TugBuilder: SRP sub-builder for tug buoy generation."""
from __future__ import annotations

from typing import Any

from .base import BaseBuilder
from ._buoy_geometry import DEFAULT_WIREFRAME_EDGES, DEFAULT_WIREFRAME_VERTICES


class TugBuilder(BaseBuilder):
    """Builds tug buoys at calculated positions along the pipeline.

    Not registered in BuilderRegistry — instantiated by BuoysBuilder orchestrator.
    """

    def build(self) -> dict[str, Any]:
        """Build tug buoys from equipment.tugs configuration.

        Positions: first_position + i * [spacing, 0, 0]

        Returns:
            Dict with 'buoys' (list of 6D buoy dicts) and 'names' (list of str).
        """
        tugs_config = self.spec.equipment.tugs
        if tugs_config is None:
            return {"buoys": [], "names": []}

        pipeline_name = self.spec.pipeline.name
        props = tugs_config.properties
        first_pos = tugs_config.first_position
        spacing = tugs_config.spacing
        moi = props.moments_of_inertia or [100, 100, 100]

        buoys: list[dict[str, Any]] = []
        names: list[str] = []

        for i in range(tugs_config.count):
            name = f"Tug{i + 1}"
            x = first_pos[0] + i * spacing
            y = first_pos[1]
            z = first_pos[2]

            buoy: dict[str, Any] = {
                "Name": name,
                "BuoyType": "Lumped buoy",
                "Connection": "Fixed",
                "DampingRelativeTo": "Earth",
                "DisturbanceVessel": "(none)",
                "WaveCalculationMethod": "Specified by environment",
                "InitialPosition": [x, y, z],
                "InitialAttitude": [90, 180, 0],
                "Mass": props.mass,
                "MomentsOfInertia": moi,
                "CentreOfMass": [0, 0, 0],
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
                "SeabedFrictionCoefficient": 0,
                "TotalContactArea": 0,
                "WireFrameOrigin": [0, 0, 0],
                "WireFrameSymmetry": "None",
                "WireFrameType": "Edges",
                "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
                "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
            }
            buoys.append(buoy)
            names.append(name)

        return {"buoys": buoys, "names": names}
