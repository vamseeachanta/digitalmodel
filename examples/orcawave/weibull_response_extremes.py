# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic independent response peaks: inputs, fit, diagnostics and extremes.

Run with the repository's src directory on PYTHONPATH. This example fits
supplied peaks and constructs a reference spectrum; it does not run a solver.
Equations and sampling limitations: docs/domains/orcawave/weibull-response-statistics.md.
All numerical settings below are constructed reproducibility inputs, not
measured data, empirical coefficients or design acceptance criteria.
"""

from dataclasses import asdict
import math
from typing import Any

import numpy as np

from digitalmodel.orcawave.motion_statistics import ScatterCell
from digitalmodel.orcawave.weibull_fit import (
    bootstrap_return_level,
    compare_rayleigh_extreme,
    fit_weibull,
    goodness_of_fit,
)


def run_example(
    *, seed: int = 2026, sample_count: int = 5000, n_resamples: int = 200
) -> dict[str, Any]:
    """Generate Rayleigh peaks and fit all three Weibull parameters.

    NIST e-Handbook 1.3.6.6.8: lambda=sqrt(2)*sigma and k=2 recover Rayleigh.
    Choose sigma=1 response unit, gamma=0, N=1000 independent cycles.
    The one-cell spectrum S=.5 at omega=[1,2,3] has m0=1 and m2=4.5
    by trapezoidal integration, matching the generating sigma. Time is
    N*Tz, converted with the existing long_term_extreme Julian year basis.
    """
    if (
        isinstance(sample_count, bool)
        or not isinstance(sample_count, int)
        or sample_count < 3
    ):
        raise ValueError("Require at least three synthetic response peaks")
    sigma, cycles = 1.0, 1000
    samples = (
        math.sqrt(2) * sigma * np.random.default_rng(seed).weibull(2, sample_count)
    )
    # The known synthetic origin permits an upper location bound of zero;
    # measured peaks require an independently justified bracket instead.
    fit_settings = dict(method="mle", location_bounds=(-0.5, 0), shape_bounds=(0.5, 5))
    fit = fit_weibull(samples, **fit_settings)
    gof = goodness_of_fit(samples, fit)
    interval = bootstrap_return_level(
        samples,
        cycles=cycles,
        confidence=0.95,
        n_resamples=n_resamples,
        seed=seed + 1,
        **fit_settings,
    )
    tz = 2 * math.pi / math.sqrt(4.5)
    comparison = compare_rayleigh_extreme(
        fit,
        scatter=[ScatterCell(hs=1, tp=1, probability=1)],
        rao_amplitude=np.ones(3),
        omega=np.array([1.0, 2.0, 3.0]),
        spectrum_func=lambda omega, hs, tp: np.full_like(omega, 0.5),
        return_period_years=cycles * tz / (365.25 * 24 * 3600),
    )
    return dict(
        inputs=dict(
            seed=seed,
            sample_count=sample_count,
            generating_shape=2,
            generating_scale=math.sqrt(2),
            generating_location=0,
            cycles=cycles,
            n_resamples=n_resamples,
            **fit_settings,
        ),
        fit=asdict(fit),
        goodness_of_fit=asdict(gof),
        bootstrap=asdict(interval),
        comparison=asdict(comparison),
    )


def render_report(result: dict[str, Any]) -> str:
    """Render a compact analysis with qualifications and rounded comparisons."""
    inputs = result["inputs"]
    fit = result["fit"]
    law = fit["distribution"]
    gof = result["goodness_of_fit"]
    interval = result["bootstrap"]
    comparison = result["comparison"]
    lines = [
        "# Synthetic response extreme analysis",
        "",
        "Summary: independent synthetic Rayleigh peaks fitted with a shifted Weibull law.",
        "",
        "## Revision history",
        "",
        "| Revision | Date | Change and reason |",
        "| --- | --- | --- |",
        "| 1 | 2026-10-22 | Add a synthetic example to demonstrate fitting and its limits. |",
        "",
        "## Inputs and method",
        "",
        f"Peaks: {inputs['sample_count']}; seed: {inputs['seed']}; cycles: {inputs['cycles']}.",
        f"Method: {fit['method']}; location bounds: {inputs['location_bounds']}; "
        f"shape bounds: {inputs['shape_bounds']}.",
        "Response and fitted dimensional parameters use the same arbitrary response unit.",
        "",
        "## Answer",
        "",
        f"Shape: {law['shape']:.1f}; scale: {law['scale']:.1f}; location: {law['location']:.1f}.",
        f"KS: {gof['ks']['statistic']:.1e}, {gof['ks']['status']}; "
        f"AD: {gof['ad']['statistic']:.1e}, {gof['ad']['status']}.",
        f"Return level: {comparison['weibull_return_level']:.1f}; "
        f"Rayleigh reference: {comparison['rayleigh_return_level']:.1f}.",
        f"Ratio: {comparison['ratio']:.1f}; difference: {comparison['difference_percent']:.1f}%.",
        f"Confidence: {100 * interval['confidence']:.1f}%; "
        f"replicates completed: {interval['completed']}; failed: {interval['failed']}; "
        f"fallbacks: {interval['fallback_count']}.",
    ]
    if interval["status"] == "EVALUATED":
        lines.append(
            f"Percentile interval: [{interval['lower']:.1f}, {interval['upper']:.1f}]."
        )
    else:
        lines.append(f"Interval: NOT_EVALUATED; {interval['reason']}.")
    lines += [
        "",
        "## Limits and way forward",
        "",
        "Fitted-law goodness-of-fit critical values are NOT_EVALUATED.",
        "Location affects the tail; repeat with independently justified bounds.",
        "Percentile coverage is unvalidated for bound-active or non-regular fits.",
        "The threshold represents one expected exceedance, not an expected maximum.",
        "Obtain independent peak records and calibrate fit decisions before design use.",
        "",
        "Equations, sources and derivations: docs/domains/orcawave/weibull-response-statistics.md.",
    ]
    lines += [f"Fit qualification: {note}." for note in fit["notes"]]
    return "\n".join(lines) + "\n"


if __name__ == "__main__":
    print(render_report(run_example()), end="")
