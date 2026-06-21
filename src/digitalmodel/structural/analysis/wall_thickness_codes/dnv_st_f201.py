"""DNV-ST-F201 dynamic-riser LRFD wall thickness design code implementation."""

# ABOUTME: DNV-ST-F201 (Dynamic Risers) LRFD pressure-containment / burst strategy
# ABOUTME: Registered in CODE_REGISTRY; mirrors the DnvStF101Strategy pattern

import math
import logging
from typing import Dict, List, Optional, Tuple

from digitalmodel.structural.analysis.wall_thickness import (
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness import DesignCode
from .base import register_code

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# DNV-ST-F201 LRFD design factors (Dynamic Risers).
#
# Standard: DNV-ST-F201 "Dynamic risers" (2021 edition; formerly DNV-OS-F201,
# 2010). The pressure-containment (bursting) limit state is checked in the
# Load and Resistance Factor Design (LRFD) format of Sec. 5.
#
# Burst / pressure-containment limit state (DNV-ST-F201 Sec. 5, "Bursting"):
#
#     (p_li - p_e) <= p_b(t1) / (gamma_m * gamma_SC)
#
# where the characteristic burst resistance is
#
#     p_b(t) = (2 * t / (D - t)) * (2 / sqrt(3)) * min(f_y, f_u / 1.15)
#
# with
#     f_y = SMYS * alpha_U          (yield strength incl. material strength factor)
#     f_u = SMTS * alpha_U          (tensile strength incl. material strength factor)
#     alpha_U = 0.96                (material strength factor, normal supply; -> 1.00
#                                    only with supplementary requirement "U")
#     t1 = corroded, fabrication-reduced wall thickness (PipeGeometry.t1)
#
# Resistance factors (DNV-ST-F201):
#     gamma_m  = 1.15   material resistance factor (ULS / SLS)
#     gamma_SC = safety-class resistance factor for pressure containment:
#                 Low    = 1.046
#                 Normal = 1.138   (-> "medium" SafetyClass)
#                 High   = 1.308
#
# Load side (LRFD): the design net internal overpressure may be amplified by a
# dynamic-amplification factor gamma_dyn (default 1.0) to represent the dynamic
# pressure response of a riser. All of these are exposed and overridable.
# ---------------------------------------------------------------------------

DEFAULT_FACTORS: Dict[str, float] = {
    "alpha_U": 0.96,    # material strength factor (normal supply)
    "gamma_m": 1.15,    # material resistance factor
    "gamma_dyn": 1.0,   # dynamic amplification factor on the pressure load
    "gamma_SC": {
        "low": 1.046,
        "medium": 1.138,   # "Normal" safety class
        "high": 1.308,
    },
}


@register_code(DesignCode.DNV_ST_F201)
class DnvStF201Strategy:
    """DNV-ST-F201 (Dynamic risers) pressure-containment (burst) check, LRFD.

    Encodes the DNV-ST-F201 bursting limit state in the Load and Resistance
    Factor Design (LRFD) format:

        (gamma_dyn * (p_li - p_e)) <= p_b(t1) / (gamma_m * gamma_SC)

    expressed as a utilisation

        u = (gamma_dyn * (p_li - p_e)) / (p_b(t1) / (gamma_m * gamma_SC))

    so u <= 1.0 satisfies the limit state.

    Characteristic burst resistance (Sec. 5):

        p_b(t) = (2 t / (D - t)) * (2 / sqrt(3)) * min(f_y, f_u / 1.15)
        f_y = SMYS * alpha_U,   f_u = SMTS * alpha_U

    Assumed / default factor values (all overridable via the constructor):
        alpha_U  = 0.96   material strength factor (normal supply)
        gamma_m  = 1.15   material resistance factor
        gamma_dyn= 1.0    dynamic amplification on the pressure demand
        gamma_SC = {low: 1.046, normal/medium: 1.138, high: 1.308}

    The safety-class factor gamma_SC is selected from the DesignFactors
    SafetyClass at check time, matching the DnvStF101Strategy convention.
    """

    code_name = "DNV-ST-F201"
    check_names = ["pressure_containment"]

    def __init__(
        self,
        alpha_U: Optional[float] = None,
        gamma_m: Optional[float] = None,
        gamma_dyn: Optional[float] = None,
        gamma_SC: Optional[Dict[str, float]] = None,
    ):
        self.alpha_U = DEFAULT_FACTORS["alpha_U"] if alpha_U is None else alpha_U
        self.gamma_m = DEFAULT_FACTORS["gamma_m"] if gamma_m is None else gamma_m
        self.gamma_dyn = DEFAULT_FACTORS["gamma_dyn"] if gamma_dyn is None else gamma_dyn
        self.gamma_SC = dict(DEFAULT_FACTORS["gamma_SC"]) if gamma_SC is None else dict(gamma_SC)
        logger.info(
            "DnvStF201Strategy: alpha_U=%.3f gamma_m=%.3f gamma_dyn=%.3f",
            self.alpha_U, self.gamma_m, self.gamma_dyn,
        )

    def get_factors(self) -> Dict:
        """Return the LRFD factors in use (for reporting / determinism checks)."""
        return {
            "alpha_U": self.alpha_U,
            "gamma_m": self.gamma_m,
            "gamma_dyn": self.gamma_dyn,
            "gamma_SC": dict(self.gamma_SC),
        }

    def _gamma_sc(self, factors: DesignFactors) -> float:
        """Pick the pressure-containment safety-class factor for the run."""
        sc = factors.safety_class.value  # "low" | "medium" | "high"
        return self.gamma_SC[sc]

    def run_checks(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Dict[str, Tuple[float, Dict[str, float]]]:
        return {
            "pressure_containment": self._check_pressure_containment(
                geometry, material, loads, factors
            )
        }

    def compute_plastic_moment(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Plastic moment capacity M_p in N*m (same definition as DNV-ST-F101)."""
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * material.alpha_fab * (D - t2) ** 2 * t2

    def compute_plastic_tension(self, geometry: PipeGeometry, material: PipeMaterial) -> float:
        """Plastic axial capacity S_p in N (same definition as DNV-ST-F101)."""
        D = geometry.outer_diameter
        t2 = geometry.t2
        return material.smys * material.alpha_fab * math.pi * (D - t2) * t2

    def _check_pressure_containment(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
    ) -> Tuple[float, Dict[str, float]]:
        """DNV-ST-F201 Sec. 5 bursting (pressure-containment) limit state, LRFD."""
        t1 = geometry.t1
        D = geometry.outer_diameter

        # Characteristic strengths with material strength factor alpha_U
        f_y = material.smys * self.alpha_U
        f_u = material.smts * self.alpha_U

        # Characteristic burst resistance p_b(t1)
        f_cb = min(f_y, f_u / 1.15)
        p_b = (2 * t1 / (D - t1)) * (2 / math.sqrt(3)) * f_cb

        # Design resistance
        gamma_sc = self._gamma_sc(factors)
        p_b_design = p_b / (self.gamma_m * gamma_sc)

        # Factored pressure demand (net internal overpressure x dynamic factor)
        p_net = loads.internal_pressure - loads.external_pressure
        p_demand = self.gamma_dyn * p_net

        if p_b_design <= 0:
            utilisation = float("inf") if p_demand > 0 else 0.0
        else:
            utilisation = p_demand / p_b_design

        details = {
            "t1": t1,
            "alpha_U": self.alpha_U,
            "f_cb": f_cb,
            "p_b": p_b,
            "gamma_m": self.gamma_m,
            "gamma_sc": gamma_sc,
            "gamma_dyn": self.gamma_dyn,
            "p_b_design": p_b_design,
            "p_net": p_net,
            "p_demand": p_demand,
            "utilisation": utilisation,
        }

        logger.info(
            "DNV-ST-F201 pressure containment: p_b=%.2f MPa, p_b_design=%.2f MPa, util=%.3f",
            p_b / 1e6, p_b_design / 1e6, utilisation,
        )
        return utilisation, details
