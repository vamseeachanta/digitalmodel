"""EndBuoyBuilder: SRP sub-builder for end buoy + mid-pipe marker generation."""
from __future__ import annotations

from typing import Any

from .base import BaseBuilder
from ._buoy_geometry import DEFAULT_WIREFRAME_EDGES, DEFAULT_WIREFRAME_VERTICES


class EndBuoyBuilder(BaseBuilder):
    """Builds the free 6D end buoy and 3D mid-pipe marker.

    Only component that produces a 3DBuoy (Mid-pipe marker).
    Not registered in BuilderRegistry — instantiated by BuoysBuilder orchestrator.
    """

    def build(self) -> dict[str, Any]:
        """Build end buoy and mid-pipe marker.

        Returns:
            Dict with keys:
              'six_d_buoy': 6D end buoy dict
              'six_d_name': "6D buoy1"
              'three_d_buoy': 3D mid-pipe marker dict
              'three_d_name': "Mid-pipe"
        """
        return {
            "six_d_buoy": self._build_end_buoy(),
            "six_d_name": "6D buoy1",
            "three_d_buoy": self._build_mid_pipe_marker(),
            "three_d_name": "Mid-pipe",
        }

    def _build_end_buoy(self) -> dict[str, Any]:
        total_length = self.spec.get_total_pipeline_length()
        estimated_x = total_length * 0.98
        estimated_y = -13
        estimated_z = -1.3

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
            "SeabedFrictionCoefficient": 0,
            "TotalContactArea": 0,
            "Pen": [1, "Solid", "$0080FF"],
            "WireFrameOrigin": [0, 0, 0],
            "WireFrameSymmetry": "None",
            "WireFrameType": "Edges",
            "VertexX, VertexY, VertexZ": DEFAULT_WIREFRAME_VERTICES,
            "EdgeFrom, EdgeTo, EdgeDiameter": DEFAULT_WIREFRAME_EDGES,
        }

    def _build_mid_pipe_marker(self) -> dict[str, Any]:
        pipeline_name = self.spec.pipeline.name
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
