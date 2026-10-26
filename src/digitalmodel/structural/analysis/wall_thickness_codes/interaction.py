# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report adapters for the result of our implemented wall-thickness checks.

No design criterion is inferred for a route without a registered combined check.
Its optional von Mises curve is explicitly a screening calculation.
"""

import math
from dataclasses import replace

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    SafetyClass,
)


def _combined_check_name(strategy):
    """Fail closed for the two screening routes.

    RP 1111 needs reviewed registration and explicit review-evidence wiring
    before its report can switch to a combined-check claim. RP 2RD remains
    screening. The other two adapters consume their current registered checks.
    """
    if strategy.code_name in (DesignCode.API_RP_1111.value, DesignCode.API_RP_2RD.value):
        return None
    return next((key for key in ("combined", "combined_loading")
                 if key in strategy.check_names), None)


def report_conditions(strategy):
    """Expose implemented factors without inventing operating-condition uplifts.

    The API strategies implement one fixed factor set. F101's combined check
    accepts DesignFactors, so each supported safety class can be evaluated.
    Pressure checks retain their existing factor selection independently.
    F101 scales load terms by gamma_sc*gamma_m; dividing a unit normalised
    load by that product gives the equivalent displayed interaction factor.
    """
    if _combined_check_name(strategy) == "combined_loading":
        return [
            dict(
                name=f"{safety.value.title()} Safety Class",
                factors=(factors := DesignFactors(safety_class=safety)),
                f_d=1 / (factors.gamma_sc * factors.gamma_m),
                basis="simplified form; not checked against the standard's text; corroded wall",
                color=color, dash=dash,
            )
            for safety, color, dash in [
                (SafetyClass.LOW, "#2ca02c", "solid"),
                (SafetyClass.MEDIUM, "#1f77b4", "dash"),
                (SafetyClass.HIGH, "#d62728", "dot"),
            ]
        ]
    if _combined_check_name(strategy) == "combined":
        name = "Implemented factor set"
        factor = strategy.COMBINED_DESIGN_FACTOR
        basis = "Method 1 (Linear): result of our implementation"
    elif hasattr(strategy, "C_A"):
        name = "Operating"
        factor = strategy.C_A
        basis = "SCREENING (von Mises); no registered combined-loading check"
    elif hasattr(strategy, "get_edition_factors"):
        name = "Normal Operating"
        factor = strategy.get_edition_factors()["f_d"]
        basis = "SCREENING (von Mises); reviewed combined-loading registration unavailable"
    else:
        raise ValueError(f"No M-T report adapter for {strategy.code_name}")
    return [dict(name=name, factors=DesignFactors(), f_d=factor, basis=basis + "; nominal wall",
                 color="#1f77b4", dash="solid")]


def screening_utilisation(tension, moment, area, modulus, hoop, allowable):
    """Plane-stress screening from the deviatoric-stress invariant.

    First-principles basis: subtract the mean principal stress to obtain
    deviatoric stresses s_i. Their invariant is J2=sum(s_i**2)/2. Calibrating
    to uniaxial yield gives equivalent stress squared=3*J2, or half the sum
    of squared principal-stress differences. For (axial, hoop, 0), expansion gives
    axial**2 - axial*hoop + hoop**2. Force equilibrium gives axial=T/A;
    linear bending gives the two extreme-fibre stresses T/A +/- M/Z.
    Checking both fibres makes the result invariant under moment reversal.
    This does not establish a code-specific combined-loading criterion.
    """
    stresses = [tension / area + moment / modulus,
                tension / area - moment / modulus]
    equivalent = max(math.sqrt(s**2 - s * hoop + hoop**2) for s in stresses)
    return equivalent / allowable if allowable > 0 else float("inf")


class ReportInteraction:
    """Evaluate and trace one condition through the strategy's own check.

    Bisection maintains a feasible lower bound and an infeasible upper bound.
    Halving that bracket 60 times bounds the load error by its initial width
    divided by 2**60. Returning the lower bound keeps boundary points feasible.
    The supported criteria have a connected admissible interval along each
    searched ray; this adapter does not assume that for other criteria.

    Screening section basis, from first principles: integrating dA=r*dr*dtheta
    over an annulus gives A=pi*(Ro**2-Ri**2) and I=pi*(Ro**4-Ri**4)/4.
    The extreme-fibre modulus is Z=I/Ro. Equilibrium of a cut cylinder gives
    2*wall*hoop=net_pressure*inner. These screening quantities use nominal wall.
    """

    def __init__(self, strategy, geometry, material, loads, condition):
        self.strategy = strategy
        self.geometry = geometry
        self.material = material
        self.loads = loads
        self.condition = condition
        self.factors = condition["factors"]
        self.check_name = _combined_check_name(strategy)
        self.tension_scale = strategy.compute_plastic_tension(geometry, material)
        self.moment_scale = strategy.compute_plastic_moment(geometry, material)
        self.pressure_feasible = True
        if self.check_name:
            _, details = strategy.run_checks(
                geometry, material, loads, self.factors,
            )[self.check_name]
            # The STD strategy returns zero at zero demand even when its
            # pressure correction vanishes. Do not turn that into a curve.
            if "f_d_corrected" in details:
                self.pressure_feasible = details["f_d_corrected"] > 0
        else:
            diameter = geometry.outer_diameter
            wall = geometry.wall_thickness
            inner = diameter - 2 * wall
            self.area = math.pi * (diameter**2 - inner**2) / 4
            self.modulus = math.pi * (diameter**4 - inner**4) / (32 * diameter)
            self.hoop = (loads.internal_pressure - loads.external_pressure) * inner / (2 * wall)

    def utilisation(self, tension, moment):
        """Return the strategy result, or an explicitly identified screening result."""
        if not self.pressure_feasible:
            return float("inf")
        if self.check_name:
            loads = replace(self.loads, effective_tension=tension, bending_moment=moment)
            return self.strategy.run_checks(
                self.geometry, self.material, loads, self.factors,
            )[self.check_name][0]
        return screening_utilisation(
            tension, moment, self.area, self.modulus, self.hoop,
            self.condition["f_d"] * self.material.smys,
        )

    def limit(self, vary, fixed=0.0, sign=1):
        """Return a feasible limit; no curve is reported if the origin fails."""
        evaluate = (lambda x: self.utilisation(fixed, sign * x)) if vary == "M" else (
            lambda x: self.utilisation(sign * x, fixed)
        )
        if evaluate(0) > 1:
            return 0.0
        lo = 0.0
        hi = self.moment_scale if vary == "M" else self.tension_scale
        for _ in range(20):
            if evaluate(hi) > 1:
                break
            hi *= 2
        else:
            raise ValueError("Could not bracket the interaction boundary")
        for _ in range(60):
            mid = (lo + hi) / 2
            if evaluate(mid) <= 1:
                lo = mid
            else:
                hi = mid
        return sign * lo

    def limits(self):
        """Condition limits; missing feasible curves have no quoted capacity."""
        feasible = self.utilisation(0, 0) < 1
        return dict(
            name=self.condition["name"], f_d=self.condition["f_d"],
            feasible=feasible,
            T_pos_kN=self.limit("T") / 1e3 if feasible else 0.0,
            T_neg_kN=self.limit("T", sign=-1) / 1e3 if feasible else 0.0,
            M_allow_kNm=self.limit("M") / 1e3 if feasible else 0.0,
        )

    def trace(self, n_points=200):
        """Return this condition's own tension grid and positive moment boundary."""
        if n_points < 1:
            raise ValueError("n_points must be positive")
        limits = self.limits()
        if not limits["feasible"]:
            return [], []
        # Keep the original SI bounds, avoiding a unit round trip at endpoints.
        negative = self.limit("T", sign=-1)
        positive = self.limit("T")
        tensions = [negative + (positive - negative) * i / n_points
                    for i in range(n_points + 1)]
        tensions[0], tensions[-1] = negative, positive
        return tensions, [self.limit("M", fixed=t) for t in tensions]
