# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compute catenary references with the standard library alone.

Force is N internally; length is m. The vertical force is zero at touchdown.
Use --benchmark to emit only the benchmark.json document.
"""

import argparse
import json
import math


INPUTS = (
    ("C1", "Chain-like line in the plain-catenary regime", 1000.0, 1e9, 520.0, 0.10, 0.1100219),
    ("C2", "Light compliant line in the elastic-catenary regime", 200.0, 2e7, 570.0, 0.05, 0.0224069),
)
FOLDER = "docs/benchmarks/catenary_closed_form"


def bisect(function, low, high):
    """Solve an increasing bracketed function to better than 1e-12 relative."""
    if not function(low) <= 0 <= function(high):
        raise ValueError("Root is not bracketed")
    for _ in range(200):
        middle = (low + high) / 2
        if function(middle) < 0:
            low = middle
        else:
            high = middle
        if high - low <= 1e-14 * max(abs(middle), 1e-300):
            return (low + high) / 2
    raise ArithmeticError("Bisection did not converge")


def quantities(H, s, x, w):
    """Evaluate top tension, angle from vertical and suspended horizontal span."""
    return {
        "H_N": H,
        "s_m": s,
        "top_tension_kN": math.hypot(H, w * s) / 1000,
        "top_angle_deg_from_vertical": math.degrees(math.atan2(H, w * s)),
        "touchdown_horizontal_distance_from_top_m": x,
    }


def plain(w, L, h, X):
    """Solve the inextensible catenary on a rigid frictionless seabed."""
    def suspended(H):
        s = math.sqrt(h * h + 2 * h * H / w)
        return s, H / w * math.asinh(w * s / H)

    upper = w * (L * L - h * h) / (2 * h)
    H = bisect(lambda H: L - suspended(H)[0] + suspended(H)[1] - X,
               upper * 1e-12, upper)
    s, x = suspended(H)
    if not 0 < s < L:
        raise ValueError("Plain catenary has no grounded portion")
    return quantities(H, s, x, w)


def elastic(w, EA, L, h, X):
    """Solve the integrated elastic equations by nested bracketed bisection."""
    def suspended(H):
        def height(s):
            # Rationalized form avoids cancellation at large H.
            return w * s * s / (2 * EA) + w * s * s / (math.hypot(H, w * s) + H)
        upper_s = L
        while height(upper_s) < h:
            upper_s *= 2
        s = bisect(lambda s: height(s) - h, 0.0, upper_s)
        return s, H * s / EA + H / w * math.asinh(w * s / H)

    def residual(H):
        s, x = suspended(H)
        return (L - s) * (1 + H / EA) + x - X

    low, high = w * h * 1e-12, w * L
    while residual(high) < 0:
        high *= 2
    H = bisect(residual, low, high)
    s, x = suspended(H)
    if not 0 < s < L:
        raise ValueError("Elastic catenary has no grounded portion")
    return quantities(H, s, x, w)


def integrate_rk4(H, s, w, EA, steps=20000):
    """Integrate dx/ds and dz/ds from touchdown by classical fourth-order RK."""
    ds = s / steps

    def derivative(at):
        V = w * at
        T = math.hypot(H, V)
        return H / T + H / EA, V / T + V / EA

    x, z = 0.0, 0.0
    for i in range(steps):
        k1 = derivative(i * ds)
        k2 = derivative((i + 0.5) * ds)
        k3 = derivative((i + 0.5) * ds)
        k4 = derivative((i + 1) * ds)
        x += ds * (k1[0] + 2 * k2[0] + 2 * k3[0] + k4[0]) / 6
        z += ds * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1]) / 6
    return x, z


def shooting(w, EA, L, h, X):
    """Solve for H and s using RK4 residuals and damped two-variable Newton.

    Initial guesses depend only on the inputs. No closed-form solution or
    integrated catenary expression is used in this solve.
    """
    def residual(H, s):
        x, z = integrate_rk4(H, s, w, EA)
        return (L - s) * (1 + H / EA) + x - X, z - h

    H, s = w * h, 0.7 * L
    for _ in range(30):
        rx, rz = residual(H, s)
        norm = math.hypot(rx, rz)
        if norm < 1e-12 * max(h, X):
            x, _ = integrate_rk4(H, s, w, EA)
            return quantities(H, s, x, w)
        dH, ds = H * 1e-5, s * 1e-5
        rh = residual(H + dH, s)
        rs = residual(H, s + ds)
        a, c = (rh[0] - rx) / dH, (rh[1] - rz) / dH
        b, d = (rs[0] - rx) / ds, (rs[1] - rz) / ds
        determinant = a * d - b * c
        step_H = (-rx * d + b * rz) / determinant
        step_s = (-a * rz + c * rx) / determinant
        factor = 1.0
        for _ in range(40):
            candidate_H, candidate_s = H + factor * step_H, s + factor * step_s
            if candidate_H > 0 and 0 < candidate_s < L:
                if math.hypot(*residual(candidate_H, candidate_s)) < norm:
                    H, s = candidate_H, candidate_s
                    break
            factor /= 2
        else:
            raise ArithmeticError("RK4 shooting step did not decrease the residual")
    raise ArithmeticError("RK4 shooting did not converge")


def calculate():
    """Return the benchmark document, unrounded answers and self-check evidence."""
    cases, computed, checks = [], [], []
    for name, description, w, EA, X, diameter, mass in INPUTS:
        p = plain(w, 600, 150, X)
        e = elastic(w, EA, 600, 150, X)
        governing = "plain" if p["top_tension_kN"] * 1000 / EA <= 0.005 else "elastic"
        value = p if governing == "plain" else e
        expected = {key: round(value[key], 3) for key in (
            "top_tension_kN", "top_angle_deg_from_vertical",
            "touchdown_horizontal_distance_from_top_m",
        )}
        suffix = "plain" if name == "C1" else "elastic"
        case = {
            "case": name, "description": description,
            "inputs": {
                "w_N_per_m": w, "EA_N": EA, "L_m": 600, "h_m": 150, "X_m": X,
                "outer_diameter_m": diameter, "mass_te_per_m": mass,
                "EA_kN": EA / 1000, "EI_kN_m2": 0,
            },
            "governing_form": governing,
            "top_strain_percent": value["top_tension_kN"] * 1000 / EA * 100,
            "model_file": f"{FOLDER}/inputs/catenary_{name.lower()}_{suffix}.yml",
            "expected": expected,
        }
        if name == "C1":
            case["elastic_form_reference"] = {
                key: round(e[key], 3) for key in expected
            }
        if name == "C2":
            case["plain_form_top_tension_kN"] = round(p["top_tension_kN"], 3)
            case["plain_form_excess_percent"] = round((p["top_tension_kN"] / e["top_tension_kN"] - 1) * 100, 1)
        cases.append(case)
        computed.append({"case": name, **value})
        large = elastic(w, 1e26, 600, 150, X)
        rk = shooting(w, EA, 600, 150, X)
        check = {
            "case": name, "large_EA_N": 1e26,
            "large_EA_H_relative_difference": abs(large["H_N"] / p["H_N"] - 1),
            "rk4_steps": 20000,
            "rk4_tension_relative_difference": abs(rk["top_tension_kN"] / e["top_tension_kN"] - 1),
            "rk4_angle_difference_deg": abs(rk["top_angle_deg_from_vertical"] - e["top_angle_deg_from_vertical"]),
        }
        assert check["large_EA_H_relative_difference"] < 1e-9, check
        assert check["rk4_tension_relative_difference"] < 1e-9, check
        assert check["rk4_angle_difference_deg"] < 1e-9, check
        checks.append(check)
    document = {
        "benchmark": "catenary_closed_form",
        "units": {"force": "kN", "length": "m", "angle": "degrees"},
        "cases": cases,
        "calibrates": {
            "description": "static hand-calculation check of a single line against the closed-form catenary",
            "band": {
                "top_tension": {"kind": "relative", "percent": 5.0},
                "top_angle": {"kind": "absolute", "degrees": 1.0},
                "touchdown_horizontal_distance": {"kind": "relative", "percent": 5.0},
            },
            "applies_to": "each case, program result against the expected value",
        },
        "results_to_export": [
            "Effective tension at End B (kN)",
            "Declination at End B (degrees, 0 to 180 from global +Z, tangent A to B); "
            "angle from vertical = min(Declination, 180 - Declination)",
            "Horizontal distance from End B to touchdown (m): X minus the touchdown x coordinate",
        ],
        "sources": [
            {"citation": "Faltinsen, Sea Loads on Ships and Offshore Structures, 1990",
             "equation_number": "equation number not checked"},
            {"citation": "Irvine, Cable Structures, 1981",
             "equation_number": "equation number not checked",
             "public_equations": "https://github.com/xabi80/HSP/blob/03143504bb05ed7ad4a71a3288b3826b8a521046/tests/validation/test_m4_catenary_irvine.py#L7-L17",
             "commit": "03143504bb05ed7ad4a71a3288b3826b8a521046", "lines": [7, 17]},
        ],
    }
    return {"benchmark": document, "computed": computed, "self_checks": checks}


def main():
    """Print the computed answers and checks, or the benchmark document alone."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--benchmark", action="store_true")
    args = parser.parse_args()
    result = calculate()
    print(json.dumps(result["benchmark"] if args.benchmark else result, indent=2))


if __name__ == "__main__":
    main()
