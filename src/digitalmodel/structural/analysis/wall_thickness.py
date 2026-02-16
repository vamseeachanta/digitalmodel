# ABOUTME: Pipeline wall thickness design checks per DNV-ST-F101 and API RP 1111
# ABOUTME: Dataclass + analyzer pattern for burst, collapse, propagation, combined loading

"""
Pipeline Wall Thickness Design Checks

Design code checks per DNV-ST-F101 (submarine pipeline systems) and
API RP 1111 (offshore hydrocarbon pipelines).

Implements:
- Pressure containment (burst) - DNV Sec 5 D401, API 4.3.1.1
- Local buckling / collapse     - DNV Sec 5 D500, API 4.3.2
- Propagation buckling          - DNV Sec 5 D600, API 4.3.3
- Combined loading (P+M+T)      - DNV Sec 5 D700

Standards: DNV-ST-F101 (2021), API RP 1111 (2015)
Units: SI throughout (metres, Pascals, Newtons)
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, Optional
import math
import logging

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class DesignCode(Enum):
    DNV_ST_F101 = "DNV-ST-F101"
    API_RP_1111 = "API-RP-1111"
    API_RP_2RD = "API-RP-2RD"
    API_STD_2RD = "API-STD-2RD"
    PD_8010_2 = "PD-8010-2"
    ASME_B31_8 = "ASME-B31.8"
    ISO_13623 = "ISO-13623"


class SafetyClass(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"


class FabricationType(Enum):
    SEAMLESS = "seamless"
    UOE = "uoe"
    UO = "uo"


# ---------------------------------------------------------------------------
# Dataclasses
# ---------------------------------------------------------------------------

@dataclass
class PipeGeometry:
    """Pipe geometric properties (SI units: metres)."""

    outer_diameter: float
    wall_thickness: float
    corrosion_allowance: float = 0.0
    fabrication_tolerance: float = 0.125  # 12.5 % default per DNV

    def __post_init__(self):
        if self.outer_diameter <= 0:
            raise ValueError("Outer diameter must be positive")
        if self.wall_thickness <= 0:
            raise ValueError("Wall thickness must be positive")
        if self.wall_thickness >= self.outer_diameter / 2:
            raise ValueError("Wall thickness must be less than half the outer diameter")
        if self.corrosion_allowance < 0:
            raise ValueError("Corrosion allowance must be non-negative")
        if not 0 <= self.fabrication_tolerance < 1:
            raise ValueError("Fabrication tolerance must be in [0, 1)")

    @property
    def t1(self) -> float:
        """Corroded, fabrication-reduced wall thickness per DNV-ST-F101."""
        return (self.wall_thickness - self.corrosion_allowance) * (
            1 - self.fabrication_tolerance
        )

    @property
    def t2(self) -> float:
        """Corroded wall thickness (no fabrication tolerance deduction)."""
        return self.wall_thickness - self.corrosion_allowance

    @property
    def inner_diameter(self) -> float:
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def d_over_t(self) -> float:
        return self.outer_diameter / self.wall_thickness


@dataclass
class PipeMaterial:
    """Pipe material properties (SI units: Pascals)."""

    grade: str
    smys: float  # Specified Minimum Yield Strength (Pa)
    smts: float  # Specified Minimum Tensile Strength (Pa)
    youngs_modulus: float = 207e9  # Pa
    poissons_ratio: float = 0.3
    fabrication_type: FabricationType = FabricationType.SEAMLESS

    def __post_init__(self):
        if self.smys <= 0:
            raise ValueError("SMYS must be positive")
        if self.smts <= 0:
            raise ValueError("SMTS must be positive")
        if self.smts < self.smys:
            raise ValueError("SMTS must be >= SMYS")
        if self.youngs_modulus <= 0:
            raise ValueError("Young's modulus must be positive")

    @property
    def alpha_fab(self) -> float:
        """Fabrication factor per DNV-ST-F101 Table 5-5."""
        return {
            FabricationType.SEAMLESS: 1.00,
            FabricationType.UOE: 0.93,
            FabricationType.UO: 0.85,
        }[self.fabrication_type]


@dataclass
class DesignLoads:
    """Applied design loads (SI units)."""

    internal_pressure: float = 0.0  # Pa  (local incidental pressure p_li)
    external_pressure: float = 0.0  # Pa
    bending_moment: float = 0.0     # N·m
    effective_tension: float = 0.0  # N

    @property
    def net_internal_pressure(self) -> float:
        return max(self.internal_pressure - self.external_pressure, 0.0)


@dataclass
class DesignFactors:
    """Partial safety factors per DNV-ST-F101 Table 5-3."""

    safety_class: SafetyClass = SafetyClass.MEDIUM

    @property
    def gamma_sc(self) -> float:
        """Safety class resistance factor."""
        return {
            SafetyClass.LOW: 1.046,
            SafetyClass.MEDIUM: 1.138,
            SafetyClass.HIGH: 1.308,
        }[self.safety_class]

    @property
    def gamma_m(self) -> float:
        """Material resistance factor."""
        return 1.15

    @property
    def gamma_inc(self) -> float:
        """Condition load effect factor (normal operation)."""
        return 1.0


@dataclass
class WallThicknessResult:
    """Aggregated results from all wall thickness design checks."""

    checks: Dict[str, float] = field(default_factory=dict)
    is_safe: bool = True
    governing_check: Optional[str] = None
    max_utilisation: float = 0.0
    details: Dict[str, Dict[str, float]] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Analyzer
# ---------------------------------------------------------------------------

class WallThicknessAnalyzer:
    """Pipeline wall thickness design checker.

    Supports DNV-ST-F101 and API RP 1111 design codes.
    """

    def __init__(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        loads: DesignLoads,
        factors: DesignFactors,
        code: DesignCode = DesignCode.DNV_ST_F101,
    ):
        self.geometry = geometry
        self.material = material
        self.loads = loads
        self.factors = factors
        self.code = code
        logger.info(
            f"WallThicknessAnalyzer initialised: code={code.value}, "
            f"OD={geometry.outer_diameter:.4f} m, WT={geometry.wall_thickness:.4f} m, "
            f"grade={material.grade}"
        )

    # ------------------------------------------------------------------
    # DNV-ST-F101 checks
    # ------------------------------------------------------------------

    def check_pressure_containment(self) -> tuple[float, Dict[str, float]]:
        """DNV-ST-F101 Sec 5 D401 — pressure containment (burst).

        Returns:
            (utilisation, details_dict)
        """
        t1 = self.geometry.t1
        D = self.geometry.outer_diameter
        smys = self.material.smys
        smts = self.material.smts

        # Material strength factors per DNV-ST-F101 Table 5-4
        f_y = 0.96
        f_u = 0.96

        # Characteristic burst pressure — Eq 5.8 (general, thin-wall form)
        f_cb = min(f_y * smys, f_u * smts / 1.15)
        p_b = (2 * t1 / (D - t1)) * (2 / math.sqrt(3)) * f_cb

        # Design pressure resistance
        gamma_sc = self.factors.gamma_sc
        gamma_m = self.factors.gamma_m
        gamma_inc = self.factors.gamma_inc
        p_b_design = p_b / (gamma_sc * gamma_m * gamma_inc)

        # Net pressure demand
        p_net = self.loads.internal_pressure - self.loads.external_pressure

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
            f"Pressure containment: p_b={p_b / 1e6:.2f} MPa, "
            f"p_b_design={p_b_design / 1e6:.2f} MPa, util={utilisation:.3f}"
        )
        return utilisation, details

    def check_collapse(self) -> tuple[float, Dict[str, float]]:
        """DNV-ST-F101 Sec 5 D500 — local buckling (external pressure / collapse).

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t2 = self.geometry.t2
        E = self.material.youngs_modulus
        nu = self.material.poissons_ratio
        f_y = self.material.smys
        alpha_fab = self.material.alpha_fab

        # Ovality (initial out-of-roundness) — default 0.5 % per DNV
        f_0 = 0.005

        # Elastic collapse pressure — Eq 5.10
        p_el = 2 * E * (t2 / D) ** 3 / (1 - nu**2)

        # Plastic collapse pressure — Eq 5.11
        p_p = f_y * alpha_fab * 2 * t2 / D

        # Characteristic collapse pressure — solve cubic (Eq 5.9)
        # (p_c - p_el)(p_c^2 - p_p^2) = p_c * p_el * p_p^2 * f_0 * D/t2
        # Rearranged: p_c^3 - p_el*p_c^2 - p_p^2*p_c + p_el*p_p^2
        #             - p_el*p_p^2*f_0*(D/t2)*p_c = 0
        # Solve numerically with bisection between 0 and p_el
        p_c = self._solve_collapse_pressure(p_el, p_p, f_0, D, t2)

        # Design collapse resistance
        gamma_sc = self.factors.gamma_sc
        gamma_m = self.factors.gamma_m
        gamma_inc = self.factors.gamma_inc
        p_c_design = p_c / (gamma_sc * gamma_m * gamma_inc)

        # External pressure demand
        p_e = self.loads.external_pressure

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
            f"Collapse: p_el={p_el / 1e6:.2f} MPa, p_p={p_p / 1e6:.2f} MPa, "
            f"p_c={p_c / 1e6:.2f} MPa, util={utilisation:.3f}"
        )
        return utilisation, details

    @staticmethod
    def _solve_collapse_pressure(
        p_el: float, p_p: float, f_0: float, D: float, t: float,
        tol: float = 1.0, max_iter: int = 200,
    ) -> float:
        """Solve DNV-ST-F101 Eq 5.9 for characteristic collapse pressure.

        Uses bisection on:
            g(p_c) = (p_c - p_el)*(p_c^2 - p_p^2) - p_c*p_el*p_p*f_0*D/t

        Note: RHS uses p_p (not p_p^2) for dimensional consistency (Pa^3).
        """
        coeff = p_el * p_p * f_0 * D / t

        def g(pc: float) -> float:
            return (pc - p_el) * (pc**2 - p_p**2) - pc * coeff

        # Bracket: p_c is between 0 and max(p_el, p_p)
        lo, hi = 0.0, max(p_el, p_p) * 1.5
        # g(0) = -p_el * (-p_p^2) - 0 = p_el*p_p^2 > 0 typically
        # g(hi) should be negative for well-chosen hi
        # Ensure proper bracket
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

    def check_propagation_buckling(self) -> tuple[float, Dict[str, float]]:
        """DNV-ST-F101 Sec 5 D600 — propagation buckling.

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t2 = self.geometry.t2
        f_y = self.material.smys
        alpha_fab = self.material.alpha_fab

        # Propagation pressure — Eq 5.17
        p_pr = 35 * f_y * alpha_fab * (t2 / D) ** 2.5

        gamma_sc = self.factors.gamma_sc
        gamma_m = self.factors.gamma_m
        gamma_inc = self.factors.gamma_inc
        p_pr_design = p_pr / (gamma_sc * gamma_m * gamma_inc)

        p_e = self.loads.external_pressure

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

        logger.info(
            f"Propagation buckling: p_pr={p_pr / 1e6:.2f} MPa, util={utilisation:.3f}"
        )
        return utilisation, details

    def check_combined_loading(self) -> tuple[float, Dict[str, float]]:
        """DNV-ST-F101 Sec 5 D700 — combined loading (pressure + bending + tension).

        Evaluates the interaction equation for local buckling under combined loads.

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t2 = self.geometry.t2
        smys = self.material.smys
        alpha_fab = self.material.alpha_fab

        gamma_sc = self.factors.gamma_sc
        gamma_m = self.factors.gamma_m

        # Plastic moment capacity — Eq 5.24
        M_p = smys * alpha_fab * (D - t2) ** 2 * t2

        # Plastic axial force capacity — Eq 5.25
        S_p = smys * alpha_fab * math.pi * (D - t2) * t2

        # Design moment and tension
        M_Sd = abs(self.loads.bending_moment)
        S_Sd = abs(self.loads.effective_tension)

        # Collapse pressure for combined check
        _, collapse_details = self.check_collapse()
        p_c = collapse_details["p_c"]

        p_e = self.loads.external_pressure
        p_i = self.loads.internal_pressure

        # Pressure term
        if p_c > 0:
            pressure_ratio = (p_i - p_e) / p_c
        else:
            pressure_ratio = 0.0

        # Interaction equation — simplified form of DNV Eq 5.21
        # {gamma_SC*gamma_m*|M_Sd|/M_p}^2 + {gamma_SC*gamma_m*S_Sd/S_p}^2 + ... <= 1
        if M_p > 0 and S_p > 0:
            moment_term = (gamma_sc * gamma_m * M_Sd / M_p) ** 2
            tension_term = (gamma_sc * gamma_m * S_Sd / S_p) ** 2
            # Pressure contribution via flow stress correction
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

        logger.info(f"Combined loading: util={utilisation:.3f}")
        return utilisation, details

    # ------------------------------------------------------------------
    # API RP 1111 checks
    # ------------------------------------------------------------------

    def check_api_burst(self) -> tuple[float, Dict[str, float]]:
        """API RP 1111 Sec 4.3.1.1 — burst pressure.

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t = self.geometry.wall_thickness
        smys = self.material.smys
        smts = self.material.smts

        d_over_t = D / t

        if d_over_t <= 15:
            # Thick-wall (Barlow with log)
            p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))
        else:
            # Thin-wall
            p_b = 0.90 * (smys + smts) * t / (D - t)

        # Design factor (API RP 1111 Table 1 — typical 0.72 for hydrotest)
        f_d = 0.72

        p_b_design = f_d * p_b

        p_net = self.loads.internal_pressure - self.loads.external_pressure

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

        logger.info(
            f"API burst: p_b={p_b / 1e6:.2f} MPa, f_d={f_d}, util={utilisation:.3f}"
        )
        return utilisation, details

    def check_api_collapse(self) -> tuple[float, Dict[str, float]]:
        """API RP 1111 Sec 4.3.2 — collapse pressure.

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t = self.geometry.wall_thickness
        E = self.material.youngs_modulus
        nu = self.material.poissons_ratio
        smys = self.material.smys

        # Elastic collapse
        p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)

        # Yield collapse (plastic)
        p_y = 2 * smys * t / D

        # Transition collapse — API uses the smaller of elastic and yield
        # with a transition formula
        p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)

        f_c = 0.80  # Collapse design factor per API RP 1111
        p_c_design = f_c * p_c

        p_e = self.loads.external_pressure

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
            f"API collapse: p_el={p_el / 1e6:.2f}, p_y={p_y / 1e6:.2f}, "
            f"p_c={p_c / 1e6:.2f} MPa, util={utilisation:.3f}"
        )
        return utilisation, details

    def check_api_propagation(self) -> tuple[float, Dict[str, float]]:
        """API RP 1111 Sec 4.3.3 — propagation buckling.

        Returns:
            (utilisation, details_dict)
        """
        D = self.geometry.outer_diameter
        t = self.geometry.wall_thickness
        smys = self.material.smys

        # Propagation pressure
        p_pr = 24 * smys * (t / D) ** 2.4

        f_p = 0.80  # Propagation design factor
        p_pr_design = f_p * p_pr

        p_e = self.loads.external_pressure

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

        logger.info(
            f"API propagation: p_pr={p_pr / 1e6:.2f} MPa, util={utilisation:.3f}"
        )
        return utilisation, details

    # ------------------------------------------------------------------
    # Orchestrator
    # ------------------------------------------------------------------

    def perform_analysis(self) -> WallThicknessResult:
        """Run all applicable design checks for the selected code."""
        from .wall_thickness_codes import CODE_REGISTRY

        strategy_cls = CODE_REGISTRY.get(self.code)
        if strategy_cls is None:
            raise ValueError(f"No strategy registered for {self.code}")

        strategy = strategy_cls()
        raw_results = strategy.run_checks(self.geometry, self.material, self.loads, self.factors)

        checks: Dict[str, float] = {}
        details: Dict[str, Dict[str, float]] = {}
        for check_name, (util, detail) in raw_results.items():
            checks[check_name] = util
            details[check_name] = detail

        max_util = max(checks.values()) if checks else 0.0
        governing = max(checks, key=checks.get) if checks else None

        result = WallThicknessResult(
            checks=checks,
            is_safe=max_util <= 1.0,
            governing_check=governing,
            max_utilisation=max_util,
            details=details,
        )

        logger.info(
            "Analysis complete (%s): governing=%s, max_util=%.3f, safe=%s",
            self.code.value, governing, max_util, result.is_safe,
        )
        return result
