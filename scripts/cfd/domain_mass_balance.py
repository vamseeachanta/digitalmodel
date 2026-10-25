# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Assess maximum water-content drift without starting a solver.

Source: OpenFOAM v2312 (2023), VoF/alphaEqn.H, lines 124–128:
https://api.openfoam.com/2312/VoF_2alphaEqn_8H_source.html
The printed Phase-1 fraction is alpha1.weightedAverage(mesh.Vsc()).
On a fixed-volume domain, f = V_water/V_domain, hence
abs(V_water - V_initial)/V_initial = abs(f - f_initial)/f_initial.
This algebra supplies no acceptance limit: the caller supplies that criterion.
It is a water-content stability gate, not a boundary-flux conservation residual.
"""

from __future__ import annotations

import argparse
from fractions import Fraction
import json
import math
from pathlib import Path
import re
from typing import Iterable

QUANTITY_SOURCE = (
    "OpenFOAM v2312 (2023), VoF/alphaEqn.H, lines 124–128; "
    "https://api.openfoam.com/2312/VoF_2alphaEqn_8H_source.html"
)
PHASE = re.compile(r"^\s*Phase-1 volume fraction\s*=\s*(\S+)")
TIME = re.compile(r"^\s*Time\s*=\s*(.*?)\s*$")


def not_evaluated(reason: str) -> dict:
    """Return an explicit missing-evidence result with JSON-finite values."""
    return {
        "status": "NOT_EVALUATED", "reason": reason,
        "quantity_source": QUANTITY_SOURCE, "tolerance_source": "caller input",
        "sample_count": 0, "initial_fraction": None, "final_fraction": None,
        "max_relative_drift": None, "max_relative_drift_sample": None,
        "relative_tolerance": None,
    }


def evaluate_drift(fractions: Iterable[float | Fraction],
                   tolerance: float | str | None) -> dict:
    """Compare every sample with the first; equality meets the caller's limit.

    Fractions lie in [0, 1] by their volume-fraction definition in the source
    cited above. A positive initial water content and two samples are needed
    to define a relative change; no denominator floor or default limit is used.
    Decimal tokens are represented as exact rational numbers for arithmetic and
    comparison. Float callers use their decimal string representation. Only
    reported quantities are converted to floats; no acceptance slack is added.
    """
    result = not_evaluated("at least two water-content samples are required")
    samples = list(fractions)
    result["sample_count"] = len(samples)
    if len(samples) < 2:
        return result
    try:
        values = [Fraction(str(v)) for v in samples]
    except (ValueError, ZeroDivisionError):
        result["reason"] = "water fractions must be finite and between zero and one"
        return result
    if any(v < 0 or v > 1 for v in values):
        result["reason"] = "water fractions must be finite and between zero and one"
        return result
    if values[0] == 0:
        result["reason"] = "initial water fraction must be positive"
        return result
    drifts = [abs(v - values[0]) / values[0] for v in values]
    maximum = max(drifts)
    try:
        reported_maximum = float(maximum)
    except OverflowError:
        reported_maximum = math.inf
    if not math.isfinite(reported_maximum):
        result["reason"] = "relative drift cannot be represented as a finite number"
        return result
    result.update(initial_fraction=float(values[0]), final_fraction=float(values[-1]),
                  max_relative_drift=reported_maximum,
                  max_relative_drift_sample=drifts.index(maximum))
    try:
        limit = Fraction(str(tolerance))
        valid_limit = limit >= 0 and math.isfinite(float(limit))
    except (ValueError, ZeroDivisionError, OverflowError):
        valid_limit = False
    if not valid_limit:
        result["reason"] = "a finite nonnegative relative tolerance is required"
        return result
    result["relative_tolerance"] = float(limit)
    result["status"] = "PASS" if maximum <= limit else "FAIL"
    result["reason"] = "maximum drift compared with the caller's inclusive limit"
    return result


def assess_log(lines: Iterable[str], tolerance: float | str | None, *,
               fixed_domain: bool = False) -> dict:
    """Read all native phase records, rejecting damaged or time-reset logs.

    Repeated records within a timestep are retained. Missing records cannot
    establish coverage of the complete solve; the input shall be the full log.
    """
    if not fixed_domain:
        return not_evaluated("confirm a fixed domain volume before using phase fractions")
    fractions = []
    previous_time = None
    for line in lines:
        time_match = TIME.match(line)
        if time_match:
            try:
                current = float(time_match[1])
            except ValueError:
                return not_evaluated("invalid time record")
            if not math.isfinite(current) or (
                previous_time is not None and current < previous_time
            ):
                return not_evaluated("nonfinite or decreasing time record")
            previous_time = current
        if line.lstrip().startswith("Phase-1 volume fraction"):
            if "Min(alpha.water)" not in line or "Max(alpha.water)" not in line:
                return not_evaluated("phase record does not identify alpha.water")
            match = PHASE.match(line)
            try:
                if match is None:
                    raise ValueError("missing phase fraction")
                value = Fraction(numeric_token(match[1]))
            except (ValueError, ZeroDivisionError):
                return not_evaluated("invalid phase-fraction record")
            fractions.append(value)
    return evaluate_drift(fractions, tolerance)


def numeric_token(value: str) -> str:
    """Validate the CLI number syntax while retaining its decimal precision."""
    float(value)
    return value


def main(argv: list[str] | None = None) -> int:
    """Print text or JSON; only an evaluated PASS returns success."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("log", type=Path)
    parser.add_argument("--max-relative-drift", type=numeric_token,
                        help="caller criterion as a fraction, not a percentage")
    parser.add_argument("--fixed-domain", action="store_true",
                        help="confirm that total domain volume stays fixed")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args(argv)
    try:
        with args.log.open(encoding="utf-8", errors="strict") as stream:
            result = assess_log(stream, args.max_relative_drift,
                                fixed_domain=args.fixed_domain)
    except (OSError, UnicodeError):
        result = not_evaluated("solver log is missing or unreadable")
    if args.json:
        print(json.dumps(result, allow_nan=False))
    else:
        print(f"  MASS BALANCE        : {result['status']}")
        print(f"  mass-balance basis  : {result['reason']}")
        if result["max_relative_drift"] is not None:
            # Percent is the dimensionless ratio multiplied by 100 by definition.
            print(f"  maximum mass drift  : {100 * result['max_relative_drift']:.1f} %")
            print(f"  phase records       : {result['sample_count']}")
        if result["relative_tolerance"] is not None:
            print(f"  caller limit        : {100 * result['relative_tolerance']:.1f} %")
    return 0 if result["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
