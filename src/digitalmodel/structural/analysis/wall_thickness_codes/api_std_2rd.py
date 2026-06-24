"""API STD 2RD wall thickness design code implementation."""

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
    check_names = ["burst", "collapse", "combined"]

    # Internal-pressure design factor for the combined (Method 1) check.
    # API STD 2RD Method 1 uses the SLS/ULS internal-pressure factor (0.80)
    # as the base, then corrects it for the pressure utilisation.
    COMBINED_DESIGN_FACTOR = 0.80

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
        results["combined"] = self._check_combined(geometry, material, loads)
        return results

    def compute_yield_moment(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """First-yield (elastic) moment M_y = (pi/4) * SMYS * (D - t)^2 * t."""
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        return (math.pi / 4) * smys * (D - t) ** 2 * t

    def compute_plastic_moment(
        self, geometry: PipeGeometry, material: PipeMaterial
    ) -> float:
        """Plastic moment capacity M_p = (4/pi) * M_y (thin-wall formula).

        M_y = (pi/4) * SMYS * (D - t)^2 * t
        M_p = (4/pi) * M_y = SMYS * (D - t)^2 * t
        """
        M_y = self.compute_yield_moment(geometry, material)
        return (4 / math.pi) * M_y

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

    def _check_combined(self, geometry, material, loads):
        """Combined moment-tension-pressure utilisation (API STD 2RD Method 1).

        Method 1 is the linear (conservative) moment-tension interaction.
        The internal-pressure design factor is corrected for the pressure
        utilisation, then the tension and moment demands are normalised by
        their first-yield capacities:

            p_b           = 0.45 * (SMYS + SMTS) * ln(OD / (OD - 2t))
            pressure_ratio= (p_i - p_e) / p_b
            F_d_corr      = sqrt(max(F_d^2 - pressure_ratio^2, 0))
            T_y           = SMYS * A
            M_y           = (pi/4) * SMYS * (D - t)^2 * t
            utilisation   = (|T|/T_y + |M|/M_y) / F_d_corr

        This matches the legacy ``APISTD2RDAnalyzer`` Method 1 utilisation
        (``calculate_method1_limits`` + ``calculate_utilization``); the ratio
        form is dimensionless so it is unit-system independent. Validated by
        the cross-check unit test against that legacy analyzer.
        """
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        smts = material.smts

        d_i = D - 2 * t
        A = math.pi / 4 * (D**2 - d_i**2)
        T_y = A * smys
        M_y = self.compute_yield_moment(geometry, material)

        p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))
        p_net = loads.internal_pressure - loads.external_pressure
        pressure_ratio = p_net / p_b if p_b > 0 else 0.0

        f_d = self.COMBINED_DESIGN_FACTOR
        arg = f_d**2 - pressure_ratio**2
        f_d_corr = math.sqrt(arg) if arg > 0 else 0.0

        tension_term = abs(loads.effective_tension) / T_y if T_y > 0 else 0.0
        moment_term = abs(loads.bending_moment) / M_y if M_y > 0 else 0.0
        demand = tension_term + moment_term

        if f_d_corr <= 0:
            utilisation = float("inf") if demand > 0 else 0.0
        else:
            utilisation = demand / f_d_corr

        details = {
            "p_b": p_b,
            "p_net": p_net,
            "pressure_ratio": pressure_ratio,
            "f_d": f_d,
            "f_d_corrected": f_d_corr,
            "yield_tension": T_y,
            "yield_moment": M_y,
            "tension_term": tension_term,
            "moment_term": moment_term,
            "utilisation": utilisation,
        }

        logger.info(
            "API STD 2RD combined (Method 1): F_d_corr=%.3f, T/Ty=%.3f, "
            "M/My=%.3f, util=%.3f",
            f_d_corr, tension_term, moment_term, utilisation,
        )
        return utilisation, details
