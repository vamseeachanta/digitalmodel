# ABOUTME: API STD 2RD riser design code strategy (LRFD, current riser standard)
# ABOUTME: Design-factor-based approach with Method 1 (linear) and Method 2 (cosine) M-T interaction

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


@register_code(DesignCode.API_STD_2RD)
class ApiStd2rdStrategy:
    """API STD 2RD (current LRFD) riser design checks.

    Uses design-factor approach with separate internal/external factors:
      SLS: F_d(int)=0.80, F_d(ext)=0.60
      ULS: F_d(int)=0.80, F_d(ext)=0.60
      ALS: F_d(int)=1.00, F_d(ext)=1.00
    """

    code_name = "API-STD-2RD"
    check_names = ["burst", "collapse"]

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
        return results

    def compute_plastic_moment(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """Plastic moment capacity M_p = (4/pi) * M_y (thin-wall formula).

        M_y = (pi/4) * SMYS * (D - t)^2 * t
        M_p = (4/pi) * M_y = SMYS * (D - t)^2 * t
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        M_y = (math.pi / 4) * smys * (D - t) ** 2 * t
        M_p = (4 / math.pi) * M_y
        return M_p

    def compute_plastic_tension(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """Yield tension T_y = SMYS * A."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        A = math.pi / 4 * (D**2 - d_i**2)
        return A * material.smys

    def _check_burst(self, geometry, material, loads):
        """Burst check: p_b = 0.45 * (SMYS + SMTS) * ln(OD / (OD - 2t)).

        Design factor F_d = 0.80 (SLS/ULS internal pressure).
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        smts = material.smts

        p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))

        f_d = 0.80
        p_b_design = f_d * p_b

        p_net = loads.internal_pressure - loads.external_pressure

        if p_b_design <= 0:
            utilisation = float("inf") if p_net > 0 else 0.0
        else:
            utilisation = p_net / p_b_design

        details = {
            "p_b": p_b,
            "f_d": f_d,
            "p_b_design": p_b_design,
            "p_net": p_net,
            "utilisation": utilisation,
        }

        logger.info(
            "API STD 2RD burst: p_b=%.2f MPa, f_d=%.2f, util=%.3f",
            p_b / 1e6, f_d, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        """Collapse check: transition formula.

        Design factor F_d = 0.60 (SLS/ULS external pressure).
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        E = material.youngs_modulus
        nu = material.poissons_ratio
        smys = material.smys

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_y = 2 * smys * t / D

        p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)

        f_c = 0.60
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
            "API STD 2RD collapse: p_c=%.2f MPa, f_c=%.2f, util=%.3f",
            p_c / 1e6, f_c, utilisation,
        )
        return utilisation, details
