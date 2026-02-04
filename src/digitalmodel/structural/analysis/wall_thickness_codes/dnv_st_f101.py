# ABOUTME: DNV-ST-F101 submarine pipeline design code strategy implementation
# ABOUTME: Extracts burst, collapse, propagation, combined loading checks from monolithic analyzer

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


@register_code(DesignCode.DNV_ST_F101)
class DnvStF101Strategy:
    """DNV-ST-F101 (2021) submarine pipeline design checks."""

    code_name = "DNV-ST-F101"
    check_names = ["pressure_containment", "collapse", "propagation_buckling", "combined_loading"]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        results = {}
        results["pressure_containment"] = self._check_pressure_containment(geometry, material, loads, factors)
        results["collapse"] = self._check_collapse(geometry, material, loads, factors)
        results["propagation_buckling"] = self._check_propagation_buckling(geometry, material, loads, factors)
        results["combined_loading"] = self._check_combined_loading(geometry, material, loads, factors)
        return results

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * material.alpha_fab * (D - t2) ** 2 * t2

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * material.alpha_fab * math.pi * (D - t2) * t2

    def _check_pressure_containment(self, geometry, material, loads, factors):
        t1 = geometry.t1
        D = geometry.outer_diameter
        smys = material.smys
        smts = material.smts

        # Material strength factors per DNV-ST-F101 Table 5-4
        f_y = 0.96
        f_u = 0.96

        # Characteristic burst pressure
        f_cb = min(f_y * smys, f_u * smts / 1.15)
        p_b = (2 * t1 / (D - t1)) * (2 / math.sqrt(3)) * f_cb

        # Design pressure resistance
        gamma_sc = factors.gamma_sc
        gamma_m = factors.gamma_m
        gamma_inc = factors.gamma_inc
        p_b_design = p_b / (gamma_sc * gamma_m * gamma_inc)

        # Net pressure demand
        p_net = loads.internal_pressure - loads.external_pressure

        if p_b_design <= 0:
            utilisation = float("inf") if p_net > 0 else 0.0
        else:
            utilisation = p_net / p_b_design

        details = {
            "t1": t1,
            "f_cb": f_cb,
            "p_b": p_b,
            "gamma_sc": gamma_sc,
            "gamma_m": gamma_m,
            "gamma_inc": gamma_inc,
            "p_b_design": p_b_design,
            "p_net": p_net,
            "utilisation": utilisation,
        }

        logger.info(
            "Pressure containment: p_b=%.2f MPa, p_b_design=%.2f MPa, util=%.3f",
            p_b / 1e6, p_b_design / 1e6, utilisation,
        )
        return utilisation, details

    def _check_collapse(self, geometry, material, loads, factors):
        D = geometry.outer_diameter
        t2 = geometry.t2
        E = material.youngs_modulus
        nu = material.poissons_ratio
        f_y = material.smys
        alpha_fab = material.alpha_fab

        f_0 = 0.005

        p_el = 2 * E * (t2 / D) ** 3 / (1 - nu**2)
        p_p = f_y * alpha_fab * 2 * t2 / D

        p_c = self._solve_collapse_pressure(p_el, p_p, f_0, D, t2)

        gamma_sc = factors.gamma_sc
        gamma_m = factors.gamma_m
        gamma_inc = factors.gamma_inc
        p_c_design = p_c / (gamma_sc * gamma_m * gamma_inc)

        p_e = loads.external_pressure

        if p_c_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_c_design

        details = {
            "t2": t2,
            "p_el": p_el,
            "p_p": p_p,
            "f_0": f_0,
            "p_c": p_c,
            "p_c_design": p_c_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info(
            "Collapse: p_el=%.2f MPa, p_p=%.2f MPa, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_p / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    @staticmethod
    def _solve_collapse_pressure(
        p_el: float, p_p: float, f_0: float, D: float, t: float,
        tol: float = 1.0, max_iter: int = 200,
    ) -> float:
        """Solve DNV-ST-F101 Eq 5.9 for characteristic collapse pressure."""
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

    def _check_propagation_buckling(self, geometry, material, loads, factors):
        D = geometry.outer_diameter
        t2 = geometry.t2
        f_y = material.smys
        alpha_fab = material.alpha_fab

        p_pr = 35 * f_y * alpha_fab * (t2 / D) ** 2.5

        gamma_sc = factors.gamma_sc
        gamma_m = factors.gamma_m
        gamma_inc = factors.gamma_inc
        p_pr_design = p_pr / (gamma_sc * gamma_m * gamma_inc)

        p_e = loads.external_pressure

        if p_pr_design <= 0:
            utilisation = float("inf") if p_e > 0 else 0.0
        else:
            utilisation = p_e / p_pr_design

        details = {
            "t2": t2,
            "p_pr": p_pr,
            "p_pr_design": p_pr_design,
            "p_e": p_e,
            "utilisation": utilisation,
        }

        logger.info("Propagation buckling: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details

    def _check_combined_loading(self, geometry, material, loads, factors):
        D = geometry.outer_diameter
        t2 = geometry.t2
        smys = material.smys
        alpha_fab = material.alpha_fab

        gamma_sc = factors.gamma_sc
        gamma_m = factors.gamma_m

        M_p = smys * alpha_fab * (D - t2) ** 2 * t2
        S_p = smys * alpha_fab * math.pi * (D - t2) * t2

        M_Sd = abs(loads.bending_moment)
        S_Sd = abs(loads.effective_tension)

        # Need collapse pressure for pressure term
        _, collapse_details = self._check_collapse(geometry, material, loads, factors)
        p_c = collapse_details["p_c"]

        p_e = loads.external_pressure
        p_i = loads.internal_pressure

        if p_c > 0:
            pressure_ratio = (p_i - p_e) / p_c
        else:
            pressure_ratio = 0.0

        if M_p > 0 and S_p > 0:
            moment_term = (gamma_sc * gamma_m * M_Sd / M_p) ** 2
            tension_term = (gamma_sc * gamma_m * S_Sd / S_p) ** 2
            pressure_term = abs(pressure_ratio)
            utilisation_sq = moment_term + tension_term + pressure_term**2
            utilisation = math.sqrt(utilisation_sq)
        else:
            utilisation = float("inf") if (M_Sd > 0 or S_Sd > 0) else 0.0

        details = {
            "M_p": M_p,
            "S_p": S_p,
            "M_Sd": M_Sd,
            "S_Sd": S_Sd,
            "p_c": p_c,
            "pressure_ratio": pressure_ratio,
            "utilisation": utilisation,
        }

        logger.info("Combined loading: util=%.3f", utilisation)
        return utilisation, details
