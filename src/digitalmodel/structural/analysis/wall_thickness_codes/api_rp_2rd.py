# ABOUTME: API RP 2RD riser design code strategy (WSD, legacy riser standard)
# ABOUTME: Combined stress (Von Mises) <= C_f * C_a * SMYS with hoop check

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


@register_code(DesignCode.API_RP_2RD)
class ApiRp2rdStrategy:
    """API RP 2RD (legacy WSD) riser design checks.

    Combined stress check: sigma_vm <= C_f * C_a * SMYS
    where C_a = 2/3 (basic allowable stress factor) and C_f varies by condition.
    Effective design factor f_d = C_f * C_a.
    """

    code_name = "API-RP-2RD"
    check_names = ["burst", "collapse", "hoop"]

    # C_a = 2/3 per API RP 2RD basic allowable
    C_A = 2.0 / 3.0

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
        results["hoop"] = self._check_hoop(geometry, material, loads)
        return results

    def compute_plastic_moment(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """Plastic moment capacity — same as API RP 1111."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        return material.smys * (D**3 - d_i**3) / 6

    def compute_plastic_tension(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """Plastic axial capacity — same as API RP 1111."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        A = math.pi / 4 * (D**2 - d_i**2)
        return A * material.smys

    def _check_burst(self, geometry, material, loads):
        """Burst check: p_b = 0.45 * (SMYS + SMTS) * ln(OD / (OD - 2t)).

        Design factor f_d = C_a * C_f(Operating=1.0) = 0.667.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        smts = material.smts

        p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))

        f_d = self.C_A  # Operating: C_f=1.0, so f_d = C_a = 2/3
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
            "API RP 2RD burst: p_b=%.2f MPa, f_d=%.3f, util=%.3f",
            p_b / 1e6, f_d, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        """Collapse check: transition formula p_c = p_el*p_y/sqrt(p_el^2+p_y^2).

        Design factor f_c = C_a = 0.667.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        E = material.youngs_modulus
        nu = material.poissons_ratio
        smys = material.smys

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_y = 2 * smys * t / D

        p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)

        f_c = self.C_A
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
            "API RP 2RD collapse: p_c=%.2f MPa, f_c=%.3f, util=%.3f",
            p_c / 1e6, f_c, utilisation,
        )
        return utilisation, details

    def _check_hoop(self, geometry, material, loads):
        """Hoop stress check: sigma_h <= 0.6 * SMYS.

        Additional riser-specific check per API RP 2RD.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        d_i = D - 2 * t
        smys = material.smys

        p_net = loads.internal_pressure - loads.external_pressure
        sigma_h = p_net * d_i / (2 * t) if t > 0 else 0.0

        sigma_allow = 0.6 * smys

        if sigma_allow <= 0:
            utilisation = float("inf") if sigma_h > 0 else 0.0
        else:
            utilisation = abs(sigma_h) / sigma_allow

        details = {
            "sigma_h": sigma_h,
            "sigma_allow": sigma_allow,
            "utilisation": utilisation,
        }

        logger.info(
            "API RP 2RD hoop: sigma_h=%.2f MPa, allow=%.2f MPa, util=%.3f",
            sigma_h / 1e6, sigma_allow / 1e6, utilisation,
        )
        return utilisation, details
