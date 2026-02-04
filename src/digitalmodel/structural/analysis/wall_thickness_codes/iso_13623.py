# ABOUTME: ISO 13623 international pipeline transportation systems design code strategy
# ABOUTME: Pressure containment, elastic-plastic collapse, propagation buckling checks

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


@register_code(DesignCode.ISO_13623)
class Iso13623Strategy:
    """ISO 13623 petroleum and natural gas industries — pipeline transportation systems.

    Pressure containment: p_d <= 2*t*f_y*e_j / (D*gamma_s)
    Collapse: Elastic-plastic transition (DNV-like cubic).
    Propagation: p_pr = 24 * sigma_y * (t/D)^2.4
    """

    code_name = "ISO-13623"
    check_names = ["pressure_containment", "collapse", "propagation"]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        results = {}
        results["pressure_containment"] = self._check_pressure_containment(geometry, material, loads)
        results["collapse"] = self._check_collapse(geometry, material, loads)
        results["propagation"] = self._check_propagation(geometry, material, loads)
        return results

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * (D - t2) ** 2 * t2

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * math.pi * (D - t2) * t2

    def _check_pressure_containment(self, geometry, material, loads):
        """ISO 13623 pressure containment check.

        p_d = 2 * t * f_y * e_j / (D * gamma_s)
        where:
            t = corroded wall thickness (t2)
            f_y = SMYS
            e_j = joint factor (1.0 for seamless)
            D = outer diameter
            gamma_s = safety factor (1.39 for normal safety class)

        Utilisation = p_net / p_d
        """
        D = geometry.outer_diameter
        t = geometry.t2  # Corroded wall thickness
        f_y = material.smys
        e_j = 1.0  # Joint factor for seamless pipe

        # Safety factor per ISO 13623 Table 3 (normal safety class)
        gamma_s = 1.39

        p_d = 2 * t * f_y * e_j / (D * gamma_s)

        p_net = loads.internal_pressure - loads.external_pressure

        if p_d <= 0:
            utilisation = float("inf") if p_net > 0 else 0.0
        else:
            utilisation = p_net / p_d

        details = {
            "t": t,
            "f_y": f_y,
            "e_j": e_j,
            "gamma_s": gamma_s,
            "p_d": p_d,
            "p_net": p_net,
            "utilisation": utilisation,
        }

        logger.info(
            "ISO 13623 pressure containment: p_d=%.2f MPa, util=%.3f",
            p_d / 1e6, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        """ISO 13623 collapse check — elastic-plastic transition (DNV-like cubic).

        Elastic: p_el = 2*E*(t/D)^3 / (1-nu^2)
        Plastic: p_p = 2*sigma_y*alpha_fab*t/D
        Characteristic collapse from cubic interaction.
        Design factor f_c = 0.80.
        """
        D = geometry.outer_diameter
        t = geometry.t2  # Corroded
        E = material.youngs_modulus
        nu = material.poissons_ratio
        sigma_y = material.smys
        alpha_fab = material.alpha_fab

        f_0 = 0.005  # Ovality

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_p = sigma_y * alpha_fab * 2 * t / D

        p_c = self._solve_collapse_pressure(p_el, p_p, f_0, D, t)

        f_c = 0.80
        p_c_design = f_c * p_c

        p_e = loads.external_pressure

        if p_c_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_c_design

        details = {
            "t": t,
            "p_el": p_el,
            "p_p": p_p,
            "f_0": f_0,
            "p_c": p_c,
            "f_c": f_c,
            "p_c_design": p_c_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info(
            "ISO 13623 collapse: p_el=%.2f MPa, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    @staticmethod
    def _solve_collapse_pressure(
        p_el: float, p_p: float, f_0: float, D: float, t: float,
        tol: float = 1.0, max_iter: int = 200,
    ) -> float:
        """Solve cubic for characteristic collapse pressure (DNV-like)."""
        coeff = p_el * p_p * f_0 * D / t

        def g(pc):
            return (pc - p_el) * (pc**2 - p_p**2) - pc * coeff

        lo, hi = 0.0, max(p_el, p_p) * 1.5
        if g(lo) * g(hi) > 0:
            hi = max(p_el, p_p) * 3.0

        for _ in range(max_iter):
            mid = (lo + hi) / 2
            if hi - lo < tol:
                return mid
            if g(mid) > 0:
                lo = mid
            else:
                hi = mid
        return (lo + hi) / 2

    def _check_propagation(self, geometry, material, loads):
        """ISO 13623 propagation buckling.

        p_pr = 24 * sigma_y * (t/D)^2.4
        Design factor f_p = 0.80.
        Uses corroded wall thickness.
        """
        D = geometry.outer_diameter
        t = geometry.t2
        sigma_y = material.smys

        p_pr = 24 * sigma_y * (t / D) ** 2.4

        f_p = 0.80
        p_pr_design = f_p * p_pr

        p_e = loads.external_pressure

        if p_pr_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_pr_design

        details = {
            "t": t,
            "p_pr": p_pr,
            "f_p": f_p,
            "p_pr_design": p_pr_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info("ISO 13623 propagation: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details
