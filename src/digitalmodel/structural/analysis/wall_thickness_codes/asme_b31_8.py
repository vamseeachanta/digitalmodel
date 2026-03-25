# ABOUTME: ASME B31.8 gas transmission pipeline design code strategy implementation
# ABOUTME: Barlow burst, elastic-plastic collapse transition, propagation checks

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


@register_code(DesignCode.ASME_B31_8)
class AsmeB318Strategy:
    """ASME B31.8 gas transmission and distribution piping systems.

    Burst: Barlow formula with design factor F, longitudinal joint factor E,
    and temperature derating factor T.
    Collapse: API-like elastic-plastic transition.
    """

    code_name = "ASME-B31.8"
    check_names = ["burst", "collapse", "propagation"]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        results = {}
        results["burst"] = self._check_burst(geometry, material, loads)
        results["collapse"] = self._check_collapse(geometry, material, loads)
        results["propagation"] = self._check_propagation(geometry, material, loads)
        return results

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        return material.smys * (D**3 - d_i**3) / 6

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        A = math.pi / 4 * (D**2 - d_i**2)
        return A * material.smys

    def _check_burst(self, geometry, material, loads):
        """ASME B31.8 Barlow burst pressure check.

        p_d = 2 * S * t * F * E * T / D
        where:
            S = SMYS (specified minimum yield strength)
            t = nominal wall thickness
            F = design factor (0.72 for Class 1 Division 2)
            E = longitudinal joint factor (1.0 for seamless)
            T = temperature derating factor (1.0 for T <= 121 deg C)
            D = outer diameter

        Utilisation = p_net / p_d
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        S = material.smys

        # Design factor F per ASME B31.8 Table 841.114B (Class 1, Div 2)
        F = 0.72
        # Longitudinal joint factor per Table 841.115B (seamless = 1.0)
        E_j = 1.0
        # Temperature derating factor per Table 841.116B (T <= 121 deg C)
        T_factor = 1.0

        p_d = 2 * S * t * F * E_j * T_factor / D

        p_net = loads.internal_pressure - loads.external_pressure

        if p_d <= 0:
            utilisation = float("inf") if p_net > 0 else 0.0
        else:
            utilisation = p_net / p_d

        details = {
            "S": S,
            "t": t,
            "F": F,
            "E_j": E_j,
            "T_factor": T_factor,
            "p_d": p_d,
            "p_net": p_net,
            "utilisation": utilisation,
        }

        logger.info(
            "ASME B31.8 burst: p_d=%.2f MPa, util=%.3f",
            p_d / 1e6, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        """ASME B31.8 collapse check -- API-like transition formula.

        p_el = 2*E*(t/D)^3 / (1-nu^2)
        p_y = 2*SMYS*t/D
        p_c = p_el*p_y / sqrt(p_el^2 + p_y^2)
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
            "ASME B31.8 collapse: p_el=%.2f, p_y=%.2f, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_y / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    def _check_propagation(self, geometry, material, loads):
        """ASME B31.8 propagation buckling.

        p_pr = 24 * SMYS * (t/D)^2.4
        Design factor f_p = 0.80.
        Uses nominal wall thickness (consistent with Barlow).
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

        logger.info("ASME B31.8 propagation: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details
