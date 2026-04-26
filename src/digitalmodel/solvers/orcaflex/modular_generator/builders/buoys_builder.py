"""BuoysBuilder orchestrator for the 6DBuoys/3DBuoys section (Approach B shim).

Delegates to four SRP sub-builders; aggregates results in canonical order:
  rollers → tugs → BM → 6D buoy1 (6DBuoys); Mid-pipe (3DBuoys)

Public symbol BuoysBuilder is PRESERVED (no registry/manifest/fixture changes).
get_support_geometry @staticmethod forwarded to RollerBuilder for call-site compat.
"""
from __future__ import annotations

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry
from .roller_builder import RollerBuilder
from .tug_builder import TugBuilder
from .buoyancy_builder import BuoyancyBuilder
from .end_buoy_builder import EndBuoyBuilder

# Re-export constants so existing importers keep working
from ._buoy_geometry import DEFAULT_WIREFRAME_VERTICES, DEFAULT_WIREFRAME_EDGES  # noqa: F401


@BuilderRegistry.register("08_buoys.yml", order=80)
class BuoysBuilder(BaseBuilder):
    """Orchestrates the Buoys section from four SRP sub-builders.

    Registered at 08_buoys.yml (Approach B: single file, byte-identical output).
    """

    def should_generate(self) -> bool:
        if not self.spec.is_pipeline():
            return False
        if not self.spec.is_s_lay():
            return True
        return self.spec.equipment.get_effective_rollers() is not None

    def build(self) -> dict[str, Any]:
        six_d_buoys: list[dict[str, Any]] = []
        three_d_buoys: list[dict[str, Any]] = []
        buoy_names_6d: list[str] = []
        buoy_names_3d: list[str] = []

        # Rollers
        roller_result = RollerBuilder(self.spec, self.context).build()
        six_d_buoys.extend(roller_result["buoys"])
        buoy_names_6d.extend(roller_result["names"])

        # Tugs
        tug_result = TugBuilder(self.spec, self.context).build()
        six_d_buoys.extend(tug_result["buoys"])
        buoy_names_6d.extend(tug_result["names"])

        # Buoyancy module
        bm_result = BuoyancyBuilder(self.spec, self.context).build()
        if bm_result["buoy"] is not None:
            six_d_buoys.append(bm_result["buoy"])
            buoy_names_6d.append(bm_result["name"])

        # End buoy + mid-pipe marker
        end_result = EndBuoyBuilder(self.spec, self.context).build()
        six_d_buoys.append(end_result["six_d_buoy"])
        buoy_names_6d.append(end_result["six_d_name"])
        three_d_buoys.append(end_result["three_d_buoy"])
        buoy_names_3d.append(end_result["three_d_name"])

        self._register_entity("buoy_names_6d", buoy_names_6d)
        self._register_entity("buoy_names_3d", buoy_names_3d)
        self._register_entity("all_buoy_names", buoy_names_6d + buoy_names_3d)
        self._register_entity("end_buoy_name", "6D buoy1")
        self._register_entity("bm_buoy_name", "BM")
        self._register_entity("roller_buoy_names", roller_result["names"])

        return {
            "6DBuoys": six_d_buoys,
            "3DBuoys": three_d_buoys,
        }

    @staticmethod
    def get_support_geometry(station, roller_type) -> list[dict[str, Any]]:
        """Shim: delegates to RollerBuilder.get_support_geometry for call-site compat."""
        return RollerBuilder.get_support_geometry(station, roller_type)
