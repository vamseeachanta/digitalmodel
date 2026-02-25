# ABOUTME: API RP 1111 offshore hydrocarbon pipeline design code strategy implementation
# ABOUTME: Edition-aware factors for 3rd Ed (1999) and 4th Ed (2015), burst/collapse/propagation

import math
import logging
from typing import Dict, List, Optional, Tuple

from digitalmodel.structural.analysis.wall_thickness import (
    CodeEdition,
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from .base import register_code

logger = logging.getLogger(__name__)

# Edition-keyed design factor tables.
# Keys are edition years; values are dicts of design factors.
# 3rd Edition (1999, reaffirmed 2009): tighter propagation design factor (0.72)
# 4th Edition (2015): relaxed propagation design factor to 0.80
EDITION_FACTORS: Dict[int, Dict[str, float]] = {
    1999: {"f_d": 0.72, "f_c": 0.80, "f_p": 0.72},
    2015: {"f_d": 0.72, "f_c": 0.80, "f_p": 0.80},
}

EDITION_METADATA: Dict[int, CodeEdition] = {
    1999: CodeEdition(DesignCode.API_RP_1111, 1999, "3rd Edition"),
    2015: CodeEdition(DesignCode.API_RP_1111, 2015, "4th Edition"),
}

LATEST_EDITION = 2015


@register_code(DesignCode.API_RP_1111)
class ApiRp1111Strategy:
    """API RP 1111 offshore hydrocarbon pipeline design checks.

    Supports edition-aware factor lookup. When no edition is specified,
    defaults to the latest edition (4th Ed, 2015).

    Available editions:
        - 1999: 3rd Edition (reaffirmed 2009)
        - 2015: 4th Edition
    """

    code_name = "API-RP-1111"
    check_names = ["burst", "collapse", "propagation"]

    def __init__(self, edition: Optional[int] = None):
        if edition is None:
            edition = LATEST_EDITION
        if edition not in EDITION_FACTORS:
            available = sorted(EDITION_FACTORS.keys())
            raise ValueError(
                f"Unknown edition {edition} for API RP 1111. "
                f"Available editions: {available}"
            )
        self.edition_year = edition
        self._factors = EDITION_FACTORS[edition]
        self.edition_info = EDITION_METADATA[edition]
        logger.info(
            "ApiRp1111Strategy: using %s", self.edition_info.display_label
        )

    def get_edition_factors(self) -> Dict[str, float]:
        """Return the design factors for the current edition."""
        return dict(self._factors)

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
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys
        smts = material.smts

        d_over_t = D / t

        if d_over_t <= 15:
            p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))
        else:
            p_b = 0.90 * (smys + smts) * t / (D - t)

        f_d = self._factors["f_d"]
        p_b_design = f_d * p_b

        p_net = loads.internal_pressure - loads.external_pressure

        if p_b_design <= 0:
            utilisation = float("inf") if p_net > 0 else 0.0
        else:
            utilisation = p_net / p_b_design

        details = {
            "d_over_t": d_over_t,
            "p_b": p_b,
            "f_d": f_d,
            "p_b_design": p_b_design,
            "p_net": p_net,
            "utilisation": utilisation,
        }

        logger.info("API burst: p_b=%.2f MPa, f_d=%s, util=%.3f", p_b / 1e6, f_d, utilisation)
        return utilisation, details

    def _check_collapse(self, geometry, material, loads):
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        E = material.youngs_modulus
        nu = material.poissons_ratio
        smys = material.smys

        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        p_y = 2 * smys * t / D

        p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)

        f_c = self._factors["f_c"]
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
            "API collapse: p_el=%.2f, p_y=%.2f, p_c=%.2f MPa, util=%.3f",
            p_el / 1e6, p_y / 1e6, p_c / 1e6, utilisation,
        )
        return utilisation, details

    def _check_propagation(self, geometry, material, loads):
        D = geometry.outer_diameter
        t = geometry.wall_thickness
        smys = material.smys

        p_pr = 24 * smys * (t / D) ** 2.4

        f_p = self._factors["f_p"]
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

        logger.info("API propagation: p_pr=%.2f MPa, util=%.3f", p_pr / 1e6, utilisation)
        return utilisation, details
