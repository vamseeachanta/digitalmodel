#!/usr/bin/env python3
"""
ABOUTME: OCIMF wind and current loading calculations for vessels following
Oil Companies International Marine Forum guidelines.
"""

import numpy as np
from typing import Dict, Tuple, Optional
from .models import VesselProperties, EnvironmentalConditions


class OCIMFLoading:
    """
    Calculate wind and current loads per OCIMF guidelines

    Implements environmental loading calculations from:
    - OCIMF: Mooring Equipment Guidelines (MEG4)
    - OCIMF: Prediction of Wind and Current Loads on VLCCs

    Reference: OCIMF MEG4 Chapter 3 - Environmental Forces
    """

    def __init__(self):
        """Initialize OCIMF loading calculator"""
        self.rho_air = 1.225  # kg/m³ (air density at 15°C)
        self.rho_water = 1025.0  # kg/m³ (seawater density)

    def wind_load(
        self,
        vessel: VesselProperties,
        wind_speed: float,
        wind_direction: float = 0.0,
        air_density: Optional[float] = None
    ) -> Dict[str, float]:
        """
        Calculate wind loads on vessel

        F = 0.5 · ρ · V² · A · C

        Args:
            vessel: VesselProperties object
            wind_speed: 1-hour mean wind speed at 10m height (m/s)
            wind_direction: Wind direction from bow (degrees, 0=head wind)
            air_density: Air density (kg/m³), optional

        Returns:
            Dictionary with wind loads:
            - Fx: Surge force (N)
            - Fy: Sway force (N)
            - Mz: Yaw moment (N·m)

        Reference: OCIMF MEG4 Section 3.3
        """
        if air_density is None:
            air_density = self.rho_air

        # Convert direction to radians
        theta = np.deg2rad(wind_direction)

        # Estimate wind areas if not provided
        if vessel.frontal_wind_area is None:
            # Frontal area ≈ beam × freeboard (or estimated from vessel size)
            freeboard = vessel.freeboard if vessel.freeboard else vessel.beam * 0.3
            frontal_area = vessel.beam * freeboard
        else:
            frontal_area = vessel.frontal_wind_area

        if vessel.lateral_wind_area is None:
            # Lateral area ≈ length × freeboard
            freeboard = vessel.freeboard if vessel.freeboard else vessel.beam * 0.3
            lateral_area = vessel.length_overall * freeboard
        else:
            lateral_area = vessel.lateral_wind_area

        # Wind coefficients (simplified empirical formulas)
        # For detailed analysis, use OCIMF wind tunnel data

        # Surge coefficient (longitudinal)
        Cx = 0.85 * np.abs(np.cos(theta))

        # Sway coefficient (lateral)
        Cy = 0.95 * np.abs(np.sin(theta))

        # Yaw moment coefficient
        Cmz = 0.18 * np.sin(theta) * np.cos(theta)

        # Dynamic pressure
        q = 0.5 * air_density * wind_speed**2

        # Calculate forces
        # Surge force (longitudinal)
        Fx = q * frontal_area * Cx * np.sign(np.cos(theta))

        # Sway force (transverse)
        Fy = q * lateral_area * Cy * np.sign(np.sin(theta))

        # Yaw moment (about center)
        # Moment arm ≈ L/2 for distributed lateral loading
        Mz = q * lateral_area * vessel.length_overall * Cmz

        return {
            'Fx_surge_N': float(Fx),
            'Fy_sway_N': float(Fy),
            'Mz_yaw_Nm': float(Mz),
            'wind_speed_m_s': wind_speed,
            'wind_direction_deg': wind_direction,
        }

    def current_load(
        self,
        vessel: VesselProperties,
        current_speed: float,
        current_direction: float = 0.0,
        water_density: Optional[float] = None
    ) -> Dict[str, float]:
        """
        Calculate current loads on vessel

        F = 0.5 · ρ · V² · L · T · C

        Args:
            vessel: VesselProperties object
            current_speed: Current speed (m/s)
            current_direction: Current direction from bow (degrees, 0=head current)
            water_density: Water density (kg/m³), optional

        Returns:
            Dictionary with current loads:
            - Fx: Surge force (N)
            - Fy: Sway force (N)
            - Mz: Yaw moment (N·m)

        Reference: OCIMF MEG4 Section 3.2
        """
        if water_density is None:
            water_density = self.rho_water

        # Convert direction to radians
        theta = np.deg2rad(current_direction)

        # Reference area (wetted surface)
        L = vessel.length_overall
        T = vessel.draft

        # Longitudinal projected area
        A_long = vessel.beam * T

        # Lateral projected area
        A_lat = L * T

        # Current coefficients (empirical)
        # Based on vessel L/B ratio and block coefficient

        # Surge coefficient
        Cx = 0.45 + 0.1 * np.abs(np.cos(theta))

        # Sway coefficient
        Cy = 1.2 + 0.4 * np.abs(np.cos(theta))

        # Yaw moment coefficient
        Cmz = 0.4 * np.sin(theta) * (1 + np.abs(np.cos(theta)))

        # Dynamic pressure
        q = 0.5 * water_density * current_speed**2

        # Calculate forces
        # Surge force
        if abs(theta) < np.pi/4 or abs(theta) > 3*np.pi/4:
            # More head/stern current
            Fx = q * A_long * Cx * np.sign(np.cos(theta))
        else:
            # Beam current
            Fx = q * A_long * Cx * 0.3 * np.sign(np.cos(theta))

        # Sway force
        Fy = q * A_lat * Cy * np.sign(np.sin(theta))

        # Yaw moment
        Mz = q * A_lat * L * Cmz

        return {
            'Fx_surge_N': float(Fx),
            'Fy_sway_N': float(Fy),
            'Mz_yaw_Nm': float(Mz),
            'current_speed_m_s': current_speed,
            'current_direction_deg': current_direction,
        }

    def combined_environmental_load(
        self,
        vessel: VesselProperties,
        env: EnvironmentalConditions
    ) -> Dict[str, any]:
        """
        Calculate combined wind and current loads

        Args:
            vessel: VesselProperties object
            env: EnvironmentalConditions with wind and current

        Returns:
            Dictionary with individual and combined loads
        """
        # Wind loads
        wind_loads = self.wind_load(
            vessel,
            env.wind_speed,
            env.wind_direction,
            env.air_density
        )

        # Current loads
        current_loads = self.current_load(
            vessel,
            env.current_speed,
            env.current_direction,
            env.water_density
        )

        # Combined loads (vector sum)
        Fx_total = wind_loads['Fx_surge_N'] + current_loads['Fx_surge_N']
        Fy_total = wind_loads['Fy_sway_N'] + current_loads['Fy_sway_N']
        Mz_total = wind_loads['Mz_yaw_Nm'] + current_loads['Mz_yaw_Nm']

        # Resultant force magnitude and direction
        F_resultant = np.sqrt(Fx_total**2 + Fy_total**2)
        resultant_direction = np.rad2deg(np.arctan2(Fy_total, Fx_total))

        return {
            'wind_loads': wind_loads,
            'current_loads': current_loads,
            'combined_loads': {
                'Fx_surge_N': float(Fx_total),
                'Fy_sway_N': float(Fy_total),
                'Mz_yaw_Nm': float(Mz_total),
                'F_resultant_N': float(F_resultant),
                'resultant_direction_deg': float(resultant_direction),
            },
        }

    def estimate_wind_area(
        self,
        vessel: VesselProperties,
        area_type: str = 'lateral'
    ) -> float:
        """
        Estimate wind exposed area for vessel

        Args:
            vessel: VesselProperties object
            area_type: 'frontal' or 'lateral'

        Returns:
            Estimated wind area (m²)
        """
        # Estimate freeboard
        freeboard = vessel.freeboard if vessel.freeboard else vessel.beam * 0.3

        if area_type == 'frontal':
            # Frontal wind area
            return vessel.beam * freeboard
        elif area_type == 'lateral':
            # Lateral wind area
            return vessel.length_overall * freeboard
        else:
            raise ValueError(f"Unknown area_type '{area_type}'. Use 'frontal' or 'lateral'")

    def newman_drift_force(
        self,
        vessel: VesselProperties,
        wave_height: float,
        wave_period: float,
        wave_direction: float = 0.0
    ) -> Dict[str, float]:
        """
        Estimate mean drift force due to waves (Newman's approximation)

        F_drift ≈ 0.5 · ρ · g · B · H² · C_d

        Args:
            vessel: VesselProperties object
            wave_height: Significant wave height (m)
            wave_period: Peak wave period (s)
            wave_direction: Wave direction from bow (degrees)

        Returns:
            Dictionary with drift forces

        Note: This is a simplified estimation. For accurate results,
        use QTF from diffraction analysis.
        """
        theta = np.deg2rad(wave_direction)

        # Wave parameters
        wave_length = 1.56 * wave_period**2  # Deep water approximation
        k = 2 * np.pi / wave_length  # Wave number

        # Drift force coefficient (empirical)
        # Beam seas typically have highest drift forces
        Cd_surge = 0.05 * (1 + 0.5 * np.abs(np.cos(theta)))
        Cd_sway = 0.15 * np.abs(np.sin(theta))

        # Mean drift forces
        Fx_drift = 0.5 * self.rho_water * 9.81 * vessel.beam * wave_height**2 * Cd_surge
        Fy_drift = 0.5 * self.rho_water * 9.81 * vessel.length_overall * wave_height**2 * Cd_sway

        return {
            'Fx_drift_N': float(Fx_drift),
            'Fy_drift_N': float(Fy_drift),
            'wave_height_m': wave_height,
            'wave_period_s': wave_period,
            'wave_direction_deg': wave_direction,
        }
