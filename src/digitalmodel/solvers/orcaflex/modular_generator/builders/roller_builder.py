"""RollerBuilder: SRP sub-builder for roller buoy generation."""
from __future__ import annotations

import math
from typing import Any

from .base import BaseBuilder
from ._buoy_geometry import DEFAULT_WIREFRAME_EDGES, DEFAULT_WIREFRAME_VERTICES


class RollerBuilder(BaseBuilder):
    """Builds roller buoys for pipeline support arrangements.

    Not registered in BuilderRegistry — instantiated by BuoysBuilder orchestrator.
    """

    def build(self) -> dict[str, Any]:
        """Build roller buoys from effective roller arrangement.

        Returns:
            Dict with 'buoys' (list of 6D buoy dicts) and 'names' (list of str).
        """
        arrangement = self.spec.equipment.get_effective_rollers()
        if arrangement is None:
            return {"buoys": [], "names": []}

        pipeline_name = self.spec.pipeline.name
        buoys: list[dict[str, Any]] = []
        names: list[str] = []

        for idx, station in enumerate(arrangement.stations):
            name = f"Roller_{idx + 1}" if len(arrangement.stations) > 1 else "Rollers"
            supports = self.get_support_geometry(station, arrangement.type)

            buoy: dict[str, Any] = {
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

        self._register_entity("roller_buoy_names", names)
        return {"buoys": buoys, "names": names}

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

        supports: list[dict[str, Any]] = []
        r = station.diameter / 2
        h_off = station.height_offset

        if roller_type == RollerType.V_ROLLER and station.v_angle is not None:
            half_angle = station.v_angle / 2
            lateral = r * math.sin(math.radians(half_angle))
            vertical = -r * math.cos(math.radians(half_angle)) + h_off

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
            for j in range(station.support_count):
                y_offset = (j - station.support_count / 2 + 0.5) * r
                supports.append({
                    "SupportType": station.support_type,
                    "SupportCoordinateSystem": "Coordinate system1",
                    "SupportPosition": [0, y_offset, h_off],
                    "SupportOrientation": [0, 0, 0],
                })

        elif roller_type == RollerType.CRADLE:
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
