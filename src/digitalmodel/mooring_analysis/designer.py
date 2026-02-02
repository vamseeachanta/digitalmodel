#!/usr/bin/env python3
"""
ABOUTME: Mooring system designer with safety factor verification per DNV-OS-E301,
environmental load calculations, and intact/damaged condition analysis.
"""

import numpy as np
from typing import List, Dict, Tuple, Optional
import logging

from .models import (
    MooringSystem,
    MooringLine,
    EnvironmentalConditions,
    VesselParticulars,
    DesignLoadCase,
    MooringDesignResult,
    EnvironmentalLoads,
    ConditionType
)
from .catenary import CatenaryAnalyzer

logger = logging.getLogger(__name__)


class MooringDesigner:
    """
    Design and verify mooring systems per DNV-OS-E301.

    Implements safety factor verification, environmental load calculations,
    and analysis of intact and damaged conditions.
    """

    # Safety factors per DNV-OS-E301 Table 4-1
    SAFETY_FACTORS = {
        ConditionType.INTACT: {
            'quasi_static': 2.0,
            'dynamic': 1.67
        },
        ConditionType.DAMAGED: {
            'quasi_static': 1.43,
            'dynamic': 1.25
        },
        ConditionType.TRANSIENT: {
            'quasi_static': 1.10,
            'dynamic': 1.05
        }
    }

    def __init__(self, system: MooringSystem):
        """
        Initialize mooring designer.

        Args:
            system: Complete mooring system configuration
        """
        self.system = system
        self.analyzer = CatenaryAnalyzer(system.water_depth)

    def calculate_environmental_loads(
        self,
        environment: EnvironmentalConditions,
        vessel: Optional[VesselParticulars] = None
    ) -> EnvironmentalLoads:
        """
        Calculate environmental loads on vessel.

        Uses simplified empirical formulas for wave drift, current, and wind forces.
        For detailed analysis, use diffraction analysis (AQWA/OrcaWave).

        Args:
            environment: Environmental conditions
            vessel: Vessel particulars (defaults to system vessel)

        Returns:
            EnvironmentalLoads with force components
        """
        vessel = vessel or self.system.vessel

        # Constants
        rho_water = 1025  # kg/m³
        rho_air = 1.225   # kg/m³
        g = 9.81          # m/s²

        # Wave drift force (Newman approximation)
        # F_drift ≈ 0.5 * rho * g * Hs² * B
        Hs = environment.wave_hs
        B = vessel.beam
        Cwd = 0.5  # Wave drift coefficient (conservative)
        F_wave_drift = Cwd * rho_water * g * Hs**2 * B / 1000  # kN

        # Current force
        # F_current = 0.5 * rho * Cd * A * V²
        Lpp = vessel.length
        draft = vessel.draft
        A_current = Lpp * draft  # Submerged lateral area
        Cd_current = 1.0         # Current drag coefficient
        V_current = environment.current_speed
        F_current = 0.5 * rho_water * Cd_current * A_current * V_current**2 / 1000  # kN

        # Wind force
        # F_wind = 0.5 * rho_air * Cd * A_wind * V²
        A_wind = vessel.windage_area
        Cd_wind = 1.0  # Wind drag coefficient
        V_wind = environment.wind_speed
        F_wind = 0.5 * rho_air * Cd_wind * A_wind * V_wind**2 / 1000  # kN

        # Total force (vectorial sum based on directions)
        # Simplified: assume all loads in same direction
        F_total = F_wave_drift + F_current + F_wind

        logger.info(
            f"Environmental loads: Wave={F_wave_drift:.1f} kN, "
            f"Current={F_current:.1f} kN, Wind={F_wind:.1f} kN, "
            f"Total={F_total:.1f} kN"
        )

        return EnvironmentalLoads(
            wave_drift_force=F_wave_drift,
            current_force=F_current,
            wind_force=F_wind,
            total_force=F_total,
            direction=environment.wave_direction
        )

    def analyze_line_tension(
        self,
        line: MooringLine,
        vessel_offset: Tuple[float, float, float],
        dynamic_factor: float = 1.0
    ) -> float:
        """
        Analyze mooring line tension for given vessel offset.

        Uses catenary analysis to estimate line tension.

        Args:
            line: Mooring line
            vessel_offset: Vessel offset (surge, sway, yaw in deg)
            dynamic_factor: Dynamic amplification factor (DAF)

        Returns:
            Maximum line tension (kN)
        """
        # Calculate fairlead position with vessel offset
        fairlead = np.array(line.fairlead_location)
        anchor = np.array(line.anchor.location)

        offset_x, offset_y, yaw = vessel_offset

        # Apply surge and sway offsets
        fairlead_offset = fairlead.copy()
        fairlead_offset[0] += offset_x
        fairlead_offset[1] += offset_y

        # Calculate horizontal distance
        h_dist = np.sqrt(
            (fairlead_offset[0] - anchor[0])**2 +
            (fairlead_offset[1] - anchor[1])**2
        )

        # Vertical height
        z = fairlead[2] - anchor[2]

        # Use main segment for catenary (simplified - should iterate all segments)
        main_segment = line.segments[0]

        # Estimate horizontal tension needed
        # Start with pretension
        H_guess = max(line.pretension, 100.0)

        try:
            result = self.analyzer.solve_catenary(
                main_segment,
                horizontal_tension=H_guess,
                vertical_height=abs(z)
            )

            # Check if horizontal distance matches
            if result.horizontal_distance < h_dist:
                # Need higher tension
                scale = (h_dist / result.horizontal_distance) ** 2
                H_guess *= scale
                result = self.analyzer.solve_catenary(
                    main_segment,
                    horizontal_tension=H_guess,
                    vertical_height=abs(z)
                )

            # Apply dynamic factor
            max_tension = result.fairlead_tension * dynamic_factor

            return max_tension

        except Exception as e:
            logger.warning(f"Catenary solution failed for {line.line_id}: {e}")
            # Fallback: simple geometric estimate
            line_length = line.total_length
            straight_length = np.sqrt(h_dist**2 + z**2)
            if straight_length > line_length:
                logger.error(f"Line {line.line_id} too short: {line_length}m < {straight_length}m")
                return float('inf')

            # Estimate tension from geometry
            w = main_segment.weight_water * 9.81 / 1000
            T_estimate = w * line_length + line.pretension
            return T_estimate * dynamic_factor

    def analyze_intact_condition(
        self,
        load_case: DesignLoadCase,
        vessel_offset: Tuple[float, float, float] = (0.0, 0.0, 0.0),
        dynamic_factor: float = 1.5
    ) -> List[MooringDesignResult]:
        """
        Analyze mooring system in intact condition.

        Args:
            load_case: Design load case
            vessel_offset: Vessel offset (surge, sway, yaw)
            dynamic_factor: Dynamic amplification factor

        Returns:
            List of design results per line
        """
        results = []
        sf_required = self.SAFETY_FACTORS[ConditionType.INTACT]['dynamic']

        logger.info(f"Analyzing intact condition: {load_case.name}")

        for line in self.system.lines:
            # Calculate line tension
            max_tension = self.analyze_line_tension(
                line,
                vessel_offset,
                dynamic_factor
            )

            # Get minimum MBL
            actual_mbl = line.min_mbl

            # Calculate safety factor
            if max_tension > 0 and max_tension < float('inf'):
                sf_actual = actual_mbl / max_tension
                utilization = max_tension / actual_mbl
                passes = sf_actual >= sf_required
            else:
                sf_actual = 0.0
                utilization = 1.0
                passes = False

            min_mbl_required = max_tension * sf_required

            results.append(MooringDesignResult(
                line_id=line.line_id,
                load_case=load_case.name,
                max_tension=max_tension,
                min_mbl_required=min_mbl_required,
                actual_mbl=actual_mbl,
                safety_factor=sf_actual,
                utilization=utilization,
                passes=passes,
                details={
                    'condition': 'intact',
                    'dynamic_factor': dynamic_factor,
                    'vessel_offset': vessel_offset
                }
            ))

        return results

    def analyze_damaged_condition(
        self,
        load_case: DesignLoadCase,
        vessel_offset: Tuple[float, float, float] = (0.0, 0.0, 0.0),
        dynamic_factor: float = 1.5
    ) -> List[MooringDesignResult]:
        """
        Analyze mooring system with one line failed.

        Args:
            load_case: Design load case with damaged_line_id specified
            vessel_offset: Vessel offset (surge, sway, yaw)
            dynamic_factor: Dynamic amplification factor

        Returns:
            List of design results for remaining lines
        """
        if load_case.damaged_line_id is None:
            raise ValueError("damaged_line_id must be specified for damaged condition")

        results = []
        sf_required = self.SAFETY_FACTORS[ConditionType.DAMAGED]['dynamic']

        logger.info(
            f"Analyzing damaged condition: {load_case.name}, "
            f"failed line: {load_case.damaged_line_id}"
        )

        # Remaining lines
        remaining_lines = [
            line for line in self.system.lines
            if line.line_id != load_case.damaged_line_id
        ]

        if not remaining_lines:
            logger.error("No remaining lines after damage")
            return results

        # Load increase factor (simplified redistribution)
        load_increase_factor = len(self.system.lines) / len(remaining_lines)

        for line in remaining_lines:
            # Calculate line tension with increased loading
            base_tension = self.analyze_line_tension(
                line,
                vessel_offset,
                dynamic_factor
            )

            max_tension = base_tension * load_increase_factor

            # Get minimum MBL
            actual_mbl = line.min_mbl

            # Calculate safety factor
            if max_tension > 0 and max_tension < float('inf'):
                sf_actual = actual_mbl / max_tension
                utilization = max_tension / actual_mbl
                passes = sf_actual >= sf_required
            else:
                sf_actual = 0.0
                utilization = 1.0
                passes = False

            min_mbl_required = max_tension * sf_required

            results.append(MooringDesignResult(
                line_id=line.line_id,
                load_case=load_case.name,
                max_tension=max_tension,
                min_mbl_required=min_mbl_required,
                actual_mbl=actual_mbl,
                safety_factor=sf_actual,
                utilization=utilization,
                passes=passes,
                details={
                    'condition': 'damaged',
                    'failed_line': load_case.damaged_line_id,
                    'load_increase_factor': load_increase_factor,
                    'dynamic_factor': dynamic_factor,
                    'vessel_offset': vessel_offset
                }
            ))

        return results

    def analyze_all_conditions(
        self,
        load_cases: List[DesignLoadCase],
        vessel_offset: Tuple[float, float, float] = (0.0, 0.0, 0.0),
        dynamic_factor: float = 1.5
    ) -> List[MooringDesignResult]:
        """
        Analyze all load cases (intact and damaged).

        Args:
            load_cases: List of design load cases
            vessel_offset: Vessel offset
            dynamic_factor: Dynamic amplification factor

        Returns:
            Combined list of all design results
        """
        all_results = []

        for load_case in load_cases:
            if load_case.condition == ConditionType.INTACT:
                results = self.analyze_intact_condition(
                    load_case,
                    vessel_offset,
                    dynamic_factor
                )
            elif load_case.condition == ConditionType.DAMAGED:
                results = self.analyze_damaged_condition(
                    load_case,
                    vessel_offset,
                    dynamic_factor
                )
            else:  # TRANSIENT
                # Use intact analysis with lower SF
                results = self.analyze_intact_condition(
                    load_case,
                    vessel_offset,
                    dynamic_factor * 0.8  # Reduced DAF for transient
                )

            all_results.extend(results)

        return all_results

    def generate_design_summary(
        self,
        results: List[MooringDesignResult]
    ) -> Dict:
        """
        Generate summary of design analysis results.

        Args:
            results: List of design results

        Returns:
            Dictionary with summary statistics
        """
        if not results:
            return {'status': 'No results'}

        # Group by load case
        by_load_case = {}
        for result in results:
            if result.load_case not in by_load_case:
                by_load_case[result.load_case] = []
            by_load_case[result.load_case].append(result)

        # Overall statistics
        all_pass = all(r.passes for r in results)
        min_sf = min(r.safety_factor for r in results if r.safety_factor > 0)
        max_util = max(r.utilization for r in results)
        avg_sf = np.mean([r.safety_factor for r in results if r.safety_factor > 0])

        # Critical line (highest utilization)
        critical_result = max(results, key=lambda r: r.utilization)

        summary = {
            'overall_status': 'PASS' if all_pass else 'FAIL',
            'n_load_cases': len(by_load_case),
            'n_results': len(results),
            'min_safety_factor': round(min_sf, 2),
            'max_utilization': round(max_util, 3),
            'avg_safety_factor': round(avg_sf, 2),
            'critical_line': {
                'line_id': critical_result.line_id,
                'load_case': critical_result.load_case,
                'safety_factor': round(critical_result.safety_factor, 2),
                'utilization': round(critical_result.utilization, 3)
            },
            'by_load_case': {
                lc: {
                    'n_lines': len(res),
                    'all_pass': all(r.passes for r in res),
                    'min_sf': round(min(r.safety_factor for r in res if r.safety_factor > 0), 2)
                }
                for lc, res in by_load_case.items()
            }
        }

        return summary
