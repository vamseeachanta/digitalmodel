# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Optional OpenTURNS reliability for an unbounded, single-slope S-N model.

The model is N = A * S**(-m), matching the power-law equation documented in
``structural.fatigue.sn_curves.PowerLawSNCurve``. Coefficients are required
inputs; this class does not apply that curve's cutoffs or fatigue limits.
Stress S and intercept A are independent lognormal variables, specified by
arithmetic means and coefficients of variation. The slope m is deterministic.
Life arguments are cycle counts, with stress units consistent with A.

Sources (online editions accessed 2026):
* NIST/SEMATECH e-Handbook of Statistical Methods, section 1.3.6.6.9,
  "Lognormal Distribution", moments and coefficient of variation:
  https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm
* Same handbook, section 1.3.6.6.1, "Normal Distribution", standardization:
  https://www.itl.nist.gov/div898/handbook/eda/section3/eda3661.htm
* OpenTURNS 1.26 documentation, Normal.computeCDF:
  https://openturns.github.io/openturns/1.26/user_manual/_generated/openturns.Normal.html

Algebra from the S-N equation gives g = ln A - m*ln S - ln Nd.
The handbook moments give v_X = ln(1 + CoV_X**2) and
mu_lnX = ln(E[X]) - v_X/2. Independence then gives
mu_g = mu_lnA - m*mu_lnS - ln Nd and sigma_g**2 = v_A + m**2*v_S.
Failure is g <= 0; Pf = Phi(-mu_g/sigma_g). This normal margin is exact
under the stated assumptions, with no optimization or sampling error.
There is no governing acceptance target or material calibration in this module.
"""

from dataclasses import dataclass
import math

__all__ = ["FatigueReliability"]


def _log_std(cov: float) -> float:
    """Invert the handbook lognormal CoV formula without large-CoV overflow."""
    # NIST section 1.3.6.6.9: ln(1+c^2) = 2*ln(hypot(1,c)).
    # The equivalent log1p form preserves precision for small c.
    # sqrt(log1p(c*c)) = c*(1 - c*c/4 + ...). Below sqrt(ulp(1)),
    # the relative correction is below float precision; avoid squaring c.
    if cov < math.sqrt(math.ulp(1.0)):
        return cov
    variance = 2 * math.log(math.hypot(1, cov)) if cov > 1 else math.log1p(cov * cov)
    return math.sqrt(variance)


@dataclass(frozen=True)
class FatigueReliability:
    """Constant-amplitude reliability with explicitly supplied uncertainty.

    Args:
        A: Arithmetic mean of the S-N intercept, in cycles times stress**m.
        m: Positive, deterministic S-N slope.
        stress_range: Arithmetic mean stress range, in the units used by A.
        cov_stress: Nonnegative stress coefficient of variation.
        cov_sn: Nonnegative intercept coefficient of variation.

    A, m and stress_range must be positive and finite; CoVs must be finite.
    Zero CoV means a point mass. Correlation, fatigue limits, piecewise curves,
    rainflow spectra and calendar-time conversion are outside this model.
    """

    A: float
    m: float
    stress_range: float
    cov_stress: float
    cov_sn: float

    def __post_init__(self) -> None:
        # Domains follow the positive lognormal support in NIST 1.3.6.6.9;
        # zero variance is the explicitly supported deterministic limit.
        for name in ("A", "m", "stress_range"):
            value = getattr(self, name)
            if not math.isfinite(value) or value <= 0:
                raise ValueError(f"{name} must be positive and finite")
        for name in ("cov_stress", "cov_sn"):
            value = getattr(self, name)
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{name} must be nonnegative and finite")

    def _standardized_margin(self, life: float) -> tuple[float, float]:
        if not math.isfinite(life) or life <= 0:
            raise ValueError("life in cycles must be positive and finite")
        sigma_a = _log_std(self.cov_sn)
        sigma_s = _log_std(self.cov_stress)
        # NIST 1.3.6.6.9 moments, substituted into the S-N log margin above.
        # Keep the deterministic log margin separate from variance corrections.
        terms = (
            math.log(self.A),
            -self.m * math.log(self.stress_range),
            -math.log(life),
        )
        sigma = math.hypot(sigma_a, self.m * sigma_s)
        if not all(math.isfinite(term) for term in terms) or not math.isfinite(sigma):
            raise ValueError("log margin exceeds floating-point range")
        mean = math.fsum(terms)
        if sigma == 0:
            return mean, sigma
        # Divide before multiplying: sigma_a**2 and sigma_s**2 may underflow
        # even when their contributions to beta remain representable.
        beta = math.fsum(
            (
                mean / sigma,
                -(sigma_a / sigma) * sigma_a / 2,
                (self.m * sigma_s / sigma) * sigma_s / 2,
            )
        )
        return beta, sigma

    def _deterministic_failure(self, life: float, log_margin: float) -> bool:
        # Use the original N=A/S**m equation when representable so that an
        # exactly equal cycle count is not perturbed by logarithm roundoff.
        try:
            allowable = self.A / self.stress_range**self.m
        except (OverflowError, ZeroDivisionError):
            return log_margin <= 0
        if math.isfinite(allowable) and allowable > 0:
            return allowable <= life
        return log_margin <= 0

    def failure_probability(self, design_life: float) -> float:
        """Return P(N <= design_life), with design_life measured in cycles.

        Install ``digitalmodel[reliability]`` to evaluate a random margin.
        A deterministic margin is evaluated directly, including equality.
        """
        margin, sigma = self._standardized_margin(design_life)
        if sigma == 0:
            # Point-mass probability of the documented event g <= 0.
            return float(self._deterministic_failure(design_life, margin))
        try:
            import openturns as ot
        except ModuleNotFoundError as exc:
            if exc.name != "openturns":
                raise
            raise ImportError(
                "Install digitalmodel[reliability] for OpenTURNS probabilities"
            ) from exc
        # OpenTURNS 1.26 Normal.computeCDF evaluates P(g <= threshold).
        return float(ot.Normal().computeCDF(-margin))

    def reliability_index(self, target_life: float) -> float:
        """Return beta for target_life in cycles without inverting rounded Pf.

        NIST 1.3.6.6.1 standardization gives beta=mu_g/sigma_g. The direct
        ratio retains finite indices when the reported probability underflows.
        At zero variance the normal quantile limits are signed infinities.
        With positive subnormal sigma, a nonzero log margin can exceed the
        float range after division: beta is then signed infinity, and Pf
        reaches the corresponding limit of zero or one.
        """
        margin, sigma = self._standardized_margin(target_life)
        if sigma == 0:
            return (
                -math.inf
                if self._deterministic_failure(target_life, margin)
                else math.inf
            )
        return margin
