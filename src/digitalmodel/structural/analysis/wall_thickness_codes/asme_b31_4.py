"""ASME B31.4 wall thickness design code implementation."""

# ABOUTME: ASME B31.4 liquid transportation pipeline design code strategy implementation
# ABOUTME: Barlow burst, elastic-plastic collapse transition, propagation buckling checks

import math
import logging
from typing import Dict, List, Tuple

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from .base import register_code

logger = logging.getLogger(__name__)


@register_code(DesignCode.ASME_B31_4)
class AsmeB314Strategy:
    """ASME B31.4 liquid transportation systems for hydrocarbons.

    Pipeline Transportation Systems for Liquids and Slurries (2019 edition).

    Burst: Barlow formula with design factor F=0.72, longitudinal joint factor E,
    and temperature derating factor T (S403.2.1).
    Collapse: API-like elastic-plastic transition (S403.2.2).
    Propagation: Battelle/AGA formula (S403.2.3).
    """

    code_name = "ASME-B31.4"
    check_names: List[str] = ["burst", "collapse", "propagation"]

    # ASME B31.4 design factors (S403.2.1)
    DESIGN_FACTOR_F: float = 0.72
    JOINT_FACTOR_E: float = 1.0   # Seamless pipe
    TEMP_FACTOR_T: float = 1.0    # Temperature <= 120 deg C

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        results: Dict[str, Tuple[float, Dict[str, float]]] = {}
        results["burst"] = self._check_burst(geometry, material, loads)
        results["collapse"] = self._check_collapse(geometry, material, loads)
        results["propagation"] = self._check_propagation(geometry, material, loads)
        return results

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Compute plastic moment capacity M_p in N*m."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        return material.smys * (D**3 - d_i**3) / 6

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Compute plastic axial capacity S_p in N."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        A = math.pi / 4 * (D**2 - d_i**2)
        return A * material.smys

    def _check_burst(
        self, geometry: PipeGeometry, material: PipeMaterial, loads: DesignLoads,
    ) -> Tuple[float, Dict[str, float]]:
        """ASME B31.4 S403.2.1 internal pressure containment (burst) check.

        p_d = 2 * S * t * F * E * T / D
        where:
            S = SMYS (specified minimum yield strength)
            t = wall_thickness - corrosion_allowance (effective thickness)
            F = design factor (0.72)
            E = longitudinal joint factor (1.0 for seamless)
            T = temperature derating factor (1.0 for T <= 120 deg C)
            D = outer diameter

        Utilisation = p_internal / p_d
        """
        D = geometry.outer_diameter
        t_eff = geometry.wall_thickness - geometry.corrosion_allowance
        S = material.smys

        F = self.DESIGN_FACTOR_F
        E_j = self.JOINT_FACTOR_E
        T_factor = self.TEMP_FACTOR_T

        p_d = 2 * S * t_eff * F * E_j * T_factor / D

        p_int = loads.internal_pressure

        if p_d <= 0:
            utilisation = float("inf") if p_int > 0 else 0.0
        else:
            utilisation = p_int / p_d

        details = {
            "S": S,
            "t_eff": t_eff,
            "F": F,
            "E_j": E_j,
            "T_factor": T_factor,
            "p_d": p_d,
            "p_int": p_int,
            "utilisation": utilisation,
        }

        logger.info(
            "ASME B31.4 burst: p_d=%.2f MPa, util=%.3f",
            p_d / 1e6, utilisation,
        )
        return utilisation, details

    def _check_collapse(
        self, geometry: PipeGeometry, material: PipeMaterial, loads: DesignLoads,
    ) -> Tuple[float, Dict[str, float]]:
        """ASME B31.4 S403.2.2 external pressure collapse check.

        p_el = 2 * E * (t/D)^3 / (1 - nu^2)
        p_y = 2 * SMYS * t / D
        p_c = p_el * p_y / sqrt(p_el^2 + p_y^2)
        Design factor f_c = 0.80.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        E = material.youngs_modulus
        nu = material.poissons_ratio
        smys = material.smys

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_y = 2 * smys * t / D
        p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)

        f_c = 0.80
        p_c_design = f_c * p_c

        p_e = loads.external_pressure

        if p_c_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_c_design

        details = {
            "p_el": p_el,
            "p_y": p_y,
            "p_c": p_c,
            "f_c": f_c,
            "p_c_design": p_c_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info(
            "ASME B31.4 collapse: p_el=%.2f, p_y=%.2f, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_y / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    def _check_propagation(
        self, geometry: PipeGeometry, material: PipeMaterial, loads: DesignLoads,
    ) -> Tuple[float, Dict[str, float]]:
        """ASME B31.4 S403.2.3 propagation buckling check.

        p_pr = 24 * SMYS * (t/D)^2.4  (Battelle/AGA formula)
        Design factor f_p = 0.80.
        Uses nominal wall thickness.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys

        p_pr = 24 * smys * (t / D) ** 2.4

        f_p = 0.80
        p_pr_design = f_p * p_pr

        p_e = loads.external_pressure

        if p_pr_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_pr_design

        details = {
            "p_pr": p_pr,
            "f_p": f_p,
            "p_pr_design": p_pr_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info("ASME B31.4 propagation: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details
