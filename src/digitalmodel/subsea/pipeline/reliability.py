# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Probability calculations for independent normal scalar resistance and load.

Mathematical basis (derived here): for g = R - L, linearity gives
E[g] = mu_R - mu_L. Independence makes the covariance vanish, so
Var[g] = sigma_R**2 + sigma_L**2. A linear combination of independent
normal variables is normal; standardizing g therefore gives
beta = (mu_R - mu_L) / sqrt(sigma_R**2 + sigma_L**2) and
P(g < 0) = Phi(-beta) = erfc(beta / sqrt(2)) / 2. The constants in the
last identity follow from substituting t = z / sqrt(2) in the normalized
normal density exp(-z**2 / 2) / sqrt(2*pi).

This is a probability calculation, not ISO qualification or a target-reliability
criterion. No design factors, distribution fitting or unit conversion are used.
"""

import math


def normal_limit_state(
    resistance_mean: float,
    resistance_std: float,
    load_mean: float,
    load_std: float,
) -> tuple[float, float]:
    """Return beta and P(resistance - load < 0) for independent normal inputs.

    All inputs must use caller-consistent units. Either deviation may be zero
    (a deterministic input), but their combined variance must be positive.
    Independence and normality are assumptions supplied by the caller.

    Args:
        resistance_mean: Finite mean resistance.
        resistance_std: Finite, nonnegative resistance standard deviation.
        load_mean: Finite mean load.
        load_std: Finite, nonnegative load standard deviation.

    Returns:
        A tuple containing the dimensionless reliability index beta and the
        probability of a strictly negative margin, in that order. Positive beta
        means positive mean margin. Extreme tails may round to zero or one;
        beta may overflow to signed infinity when its magnitude exceeds the
        floating-point range.

    Raises:
        ValueError: If a mean is not finite, a deviation is not finite or is
            negative, or both deviations are zero.
    """
    for name, mean in (
        ("resistance_mean", resistance_mean),
        ("load_mean", load_mean),
    ):
        if not math.isfinite(mean):
            raise ValueError(f"{name} must be finite")
    for name, std in (
        ("resistance_std", resistance_std),
        ("load_std", load_std),
    ):
        if not math.isfinite(std) or std < 0:
            raise ValueError(f"{name} must be finite and nonnegative")
    scale = max(resistance_std, load_std)
    if scale == 0:
        raise ValueError("combined variance must be positive")

    # hypot evaluates sqrt(sigma_R**2 + sigma_L**2) without squaring overflow
    # or underflow. Normalize only when subtraction or hypot itself overflows.
    margin_mean = resistance_mean - load_mean
    margin_std = math.hypot(resistance_std, load_std)
    if math.isfinite(margin_mean) and math.isfinite(margin_std):
        beta = margin_mean / margin_std
    else:
        scaled_std = math.hypot(resistance_std / scale, load_std / scale)
        if math.isfinite(margin_mean):
            beta = (margin_mean / scale) / scaled_std
        else:
            beta = (resistance_mean / scale - load_mean / scale) / scaled_std

    # Use the complementary integral directly to retain small positive tails.
    probability = math.erfc(beta / math.sqrt(2)) / 2
    return beta, probability
