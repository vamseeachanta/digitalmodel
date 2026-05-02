"""BuoyancyBuilder: SRP sub-builder for buoyancy module (BM) generation."""
from __future__ import annotations

from typing import Any

from .base import BaseBuilder
from ._buoy_geometry import DEFAULT_WIREFRAME_EDGES

# BM uses a smaller bespoke box — NOT shared with other builders
_BM_WIREFRAME_VERTICES = [
    [0.75, 0.75, 1.75],
    [-0.75, 0.75, 1.75],
    [-0.75, -0.75, 1.75],
    [0.75, -0.75, 1.75],
    [0.75, 0.75, -1.75],
    [-0.75, 0.75, -1.75],
    [-0.75, -0.75, -1.75],
    [0.75, -0.75, -1.75],
]


class BuoyancyBuilder(BaseBuilder):
    """Builds the buoyancy module (BM) template for inline pipeline attachment.

    Not registered in BuilderRegistry — instantiated by BuoysBuilder orchestrator.
    """

    def build(self) -> dict[str, Any]:
        """Build BM buoy from equipment.buoyancy_modules configuration.

        Returns:
            Dict with 'buoy' (6D buoy dict or None) and 'name' (str or None).
        """
        bm_config = self.spec.equipment.buoyancy_modules
        if bm_config is None:
            return {"buoy": None, "name": None}

        pipeline_name = self.spec.pipeline.name
        props = bm_config.properties

        buoy: dict[str, Any] = {
            "Name": "BM",
            "BuoyType": "Lumped buoy",
            "Connection": pipeline_name,
            "DampingRelativeTo": "Earth",
            "DisturbanceVessel": "(none)",
            "WaveCalculationMethod": "Specified by environment",
            "InitialPosition": [-1.25, 0, 3.9],
            "ConnectionzRelativeTo": "End A",
            "InitialAttitude": [0, 0, 0],
            "Mass": props.mass,
            "MomentsOfInertia": [1, 1, 1],
            "CentreOfMass": [0, 0, 0],
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
            "SeabedFrictionCoefficient": 0,
            "TotalContactArea": 0,
            "WireFrameOrigin": [0, 0, 0],
            "WireFrameSymmetry": "None",
            "WireFrameType": "Edges",
            "VertexX, VertexY, VertexZ": _BM_WIREFRAME_VERTICES,
            "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
        }

        return {"buoy": buoy, "name": "BM"}
