"""
ABOUTME: Analytical hydrostatic calculations from hull geometry -- displacement,
waterplane area, KB, BM, stability parameters computed from section integration.
"""

from __future__ import annotations

import numpy as np

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile


class HullHydrostatics:
    """Compute hydrostatic properties from a HullProfile via numerical
    integration of cross-sectional station geometry.

    All integrations use the trapezoidal rule over the station set.
    """

    def __init__(self, profile: HullProfile) -> None:
        self._profile = profile
        self._stations = sorted(
            profile.stations, key=lambda s: s.x_position
        )
        self._draft = profile.draft

    # ------------------------------------------------------------------
    # Sectional properties
    # ------------------------------------------------------------------

    def compute_sectional_areas(self) -> list[tuple[float, float]]:
        """Return (x, area) pairs for each station.

        Area = 2 * integral_0^T y(z) dz  (port + starboard).
        """
        results: list[tuple[float, float]] = []
        for station in self._stations:
            offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
            z_vals: list[float] = []
            y_vals: list[float] = []
            for z, y in offsets:
                if z <= self._draft:
                    z_vals.append(z)
                    y_vals.append(y)
            # Add interpolated value at exactly draft if needed
            if z_vals and z_vals[-1] < self._draft:
                z_arr = [o[0] for o in offsets]
                y_arr = [o[1] for o in offsets]
                y_at_draft = float(np.interp(self._draft, z_arr, y_arr))
                z_vals.append(self._draft)
                y_vals.append(y_at_draft)
            if len(z_vals) < 2:
                results.append((station.x_position, 0.0))
                continue
            area = 2.0 * float(np.trapz(y_vals, z_vals))
            results.append((station.x_position, area))
        return results

    def compute_displaced_volume(self) -> float:
        """Integrate sectional areas along length: V = integral A(x) dx."""
        areas = self.compute_sectional_areas()
        x_arr = np.array([a[0] for a in areas])
        a_arr = np.array([a[1] for a in areas])
        return float(np.trapz(a_arr, x_arr))

    def compute_displacement(
        self, seawater_density: float = 1.025
    ) -> float:
        """Displacement in tonnes = volume * density."""
        return self.compute_displaced_volume() * seawater_density

    # ------------------------------------------------------------------
    # Waterplane
    # ------------------------------------------------------------------

    def compute_waterplane_area(self) -> float:
        """Awp = 2 * integral_0^L y_draft(x) dx."""
        x_vals: list[float] = []
        y_vals: list[float] = []
        for station in self._stations:
            offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
            z_arr = [o[0] for o in offsets]
            y_arr = [o[1] for o in offsets]
            y_at_draft = float(np.interp(self._draft, z_arr, y_arr))
            x_vals.append(station.x_position)
            y_vals.append(y_at_draft)
        return 2.0 * float(np.trapz(y_vals, x_vals))

    # ------------------------------------------------------------------
    # Stability parameters
    # ------------------------------------------------------------------

    def compute_kb(self) -> float:
        """Vertical centre of buoyancy via moment integration.

        KB = (1/V) * integral_0^L integral_0^T z * 2*y(z) dz dx
        """
        volume = self.compute_displaced_volume()
        if volume == 0:
            return 0.0
        moment_x: list[float] = []
        x_vals: list[float] = []
        for station in self._stations:
            offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
            z_vals: list[float] = []
            zy_vals: list[float] = []
            for z, y in offsets:
                if z <= self._draft:
                    z_vals.append(z)
                    zy_vals.append(z * y)
            if z_vals and z_vals[-1] < self._draft:
                z_arr = [o[0] for o in offsets]
                y_arr = [o[1] for o in offsets]
                y_at_t = float(np.interp(self._draft, z_arr, y_arr))
                z_vals.append(self._draft)
                zy_vals.append(self._draft * y_at_t)
            if len(z_vals) < 2:
                moment_x.append(0.0)
            else:
                moment_x.append(2.0 * float(np.trapz(zy_vals, z_vals)))
            x_vals.append(station.x_position)
        total_moment = float(np.trapz(moment_x, x_vals))
        return total_moment / volume

    def compute_bm_transverse(self) -> float:
        """BM = Ixx / V where Ixx = second moment of waterplane area.

        Ixx = 2/3 * integral_0^L y_draft(x)^3 dx  (for symmetric hull).
        """
        volume = self.compute_displaced_volume()
        if volume == 0:
            return 0.0
        x_vals: list[float] = []
        y3_vals: list[float] = []
        for station in self._stations:
            offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
            z_arr = [o[0] for o in offsets]
            y_arr = [o[1] for o in offsets]
            y_at_draft = float(np.interp(self._draft, z_arr, y_arr))
            x_vals.append(station.x_position)
            y3_vals.append(y_at_draft ** 3)
        ixx = (2.0 / 3.0) * float(np.trapz(y3_vals, x_vals))
        return ixx / volume

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------

    def compute_all(self) -> dict:
        """Return all hydrostatic values in a single dict."""
        volume = self.compute_displaced_volume()
        kb = self.compute_kb()
        bm = self.compute_bm_transverse()
        return {
            "displaced_volume": volume,
            "displacement": self.compute_displacement(),
            "waterplane_area": self.compute_waterplane_area(),
            "kb": kb,
            "bm_transverse": bm,
            "gm_transverse": kb + bm,  # KG assumed at keel for now
        }
