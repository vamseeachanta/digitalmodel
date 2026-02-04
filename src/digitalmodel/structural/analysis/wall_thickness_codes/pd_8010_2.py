# ABOUTME: PD 8010-2 UK offshore pipeline code strategy implementation
# ABOUTME: Hoop stress burst, Timoshenko collapse, propagation buckling checks

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


@register_code(DesignCode.PD_8010_2)
class Pd80102Strategy:
    """PD 8010-2 (UK offshore pipeline code) design checks.

    Key difference from DNV: single design factor approach (f_d = 0.72)
    rather than partial safety factors.
    """

    code_name = "PD-8010-2"
    check_names = ["hoop_stress", "collapse", "propagation"]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        results = {}
        results["hoop_stress"] = self._check_hoop_stress(geometry, material, loads)
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

    def _check_hoop_stress(self, geometry, material, loads):
        """PD 8010-2 hoop stress (burst) check.

        sigma_h = p_d * D / (2 * t_min)
        Check: sigma_h <= f_d * sigma_y
        Design factor f_d = 0.72 (typical offshore).
        Uses minimum wall thickness t_min = t - corrosion_allowance.
        """
        D = geometry.outer_diameter
        t_min = geometry.t2  # Corroded wall thickness
        smys = material.smys
        f_d = 0.72

        p_d = loads.internal_pressure - loads.external_pressure

        if t_min <= 0:
            sigma_h = float("inf") if p_d > 0 else 0.0
        else:
            sigma_h = p_d * D / (2 * t_min)

        sigma_allow = f_d * smys

        if sigma_allow <= 0:
            utilisation = float("inf") if sigma_h > 0 else 0.0
        else:
            utilisation = sigma_h / sigma_allow

        details = {
            "t_min": t_min,
            "sigma_h": sigma_h,
            "f_d": f_d,
            "sigma_allow": sigma_allow,
            "p_d": p_d,
            "utilisation": utilisation,
        }

        logger.info(
            "PD 8010-2 hoop stress: sigma_h=%.2f MPa, sigma_allow=%.2f MPa, util=%.3f",
            sigma_h / 1e6, sigma_allow / 1e6, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        """PD 8010-2 collapse check — Timoshenko elastic-plastic transition.

        Uses same cubic approach as DNV but with PD 8010-2 parameters.
        Elastic: p_el = 2*E*(t/D)^3 / (1-nu^2)
        Plastic: p_p = 2*sigma_y*t/D
        Characteristic collapse solved from cubic interaction.
        Design factor f_c = 0.80 for collapse.
        """
        D = geometry.outer_diameter
        t = geometry.t2  # Corroded
        E = material.youngs_modulus
        nu = material.poissons_ratio
        sigma_y = material.smys

        f_0 = 0.005  # Ovality

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_p = 2 * sigma_y * t / D

        # Solve cubic for characteristic collapse (same formulation as DNV)
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
            "PD 8010-2 collapse: p_el=%.2f MPa, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    @staticmethod
    def _solve_collapse_pressure(
        p_el: float, p_p: float, f_0: float, D: float, t: float,
        tol: float = 1.0, max_iter: int = 200,
    ) -> float:
        """Solve Timoshenko cubic for characteristic collapse pressure."""
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
        """PD 8010-2 propagation buckling.

        p_pr = 24 * sigma_y * (t/D)^2.4
        Design factor f_p = 0.80.
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

        logger.info("PD 8010-2 propagation: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details
