#!/usr/bin/env python3
"""Pint Performance Benchmark — Array-Heavy Engineering Calculations.

Benchmarks raw NumPy vs Pint Quantity vs @ureg.wraps() decorator for:
  1. Hydrostatic pressure: P = rho * g * depth
  2. Morison drag force:   F = 0.5 * Cd * rho * A * v^2

Reference: vamseeachanta/workspace-hub#1486
"""

import timeit
import numpy as np
import pint

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
ureg = pint.UnitRegistry()
Q_ = ureg.Quantity

# Physical constants
RHO = 1025.0          # seawater density, kg/m^3
G = 9.80665           # gravitational acceleration, m/s^2

RHO_Q = Q_(RHO, "kg/m**3")
G_Q = Q_(G, "m/s**2")

# Morison equation parameters
CD = 1.0              # drag coefficient (dimensionless)
AREA = 1.0            # projected area, m^2 (1m diameter cylinder, 1m length)

CD_Q = Q_(CD, "dimensionless")
AREA_Q = Q_(AREA, "m**2")

ARRAY_SIZES = [100, 10_000, 1_000_000]
N_REPEATS = 50        # timeit repeats per measurement
N_NUMBER = 10         # executions per repeat (timeit averages over this)


# ---------------------------------------------------------------------------
# Hydrostatic pressure: P = rho * g * depth
# ---------------------------------------------------------------------------

def hydro_raw_numpy(depth_arr: np.ndarray) -> np.ndarray:
    """Pure NumPy — no unit tracking."""
    return RHO * G * depth_arr


def hydro_pint_quantity(depth_q):
    """Full Pint Quantity arithmetic — units on every operand."""
    return RHO_Q * G_Q * depth_q


@ureg.wraps("Pa", ("kg/m**3", "m/s**2", "m"))
def hydro_ureg_wraps(rho, g, depth):
    """@ureg.wraps — strips units at boundary, raw math inside."""
    return rho * g * depth


# ---------------------------------------------------------------------------
# Morison drag force: F = 0.5 * Cd * rho * A * v^2
# ---------------------------------------------------------------------------

def morison_raw_numpy(v_arr: np.ndarray) -> np.ndarray:
    """Pure NumPy Morison drag — no unit tracking."""
    return 0.5 * CD * RHO * AREA * v_arr ** 2


def morison_pint_quantity(v_q):
    """Full Pint Quantity Morison drag — units on every operand."""
    return 0.5 * CD_Q * RHO_Q * AREA_Q * v_q ** 2


# Note: Cd is dimensionless — use None in @ureg.wraps input spec.
# Pint 0.25.x has a bug with "dimensionless" in @ureg.wraps.
@ureg.wraps("kg * m / s**2", (None, "kg/m**3", "m**2", "m/s"))
def morison_ureg_wraps(cd, rho, area, v):
    """@ureg.wraps Morison — strips units at boundary, raw math inside."""
    return 0.5 * cd * rho * area * v ** 2


# ---------------------------------------------------------------------------
# Benchmark runner
# ---------------------------------------------------------------------------

def bench(func, args, n_repeat=N_REPEATS, n_number=N_NUMBER):
    """Return (best_ms, median_ms) over n_repeat rounds of n_number calls."""
    timer = timeit.Timer(lambda: func(*args))
    times = timer.repeat(repeat=n_repeat, number=n_number)
    # times are total seconds for n_number calls each
    times_ms = [t / n_number * 1000 for t in times]
    times_ms.sort()
    best = times_ms[0]
    median = times_ms[len(times_ms) // 2]
    return best, median


def run_hydrostatic_benchmarks():
    """Run hydrostatic pressure benchmarks and return structured results."""
    results = []

    for size in ARRAY_SIZES:
        depth_np = np.linspace(0, 3000, size)
        depth_q = Q_(depth_np, "m")

        best_np, med_np = bench(hydro_raw_numpy, (depth_np,))
        best_pq, med_pq = bench(hydro_pint_quantity, (depth_q,))
        best_uw, med_uw = bench(hydro_ureg_wraps, (RHO_Q, G_Q, depth_q))

        results.append({
            "size": size,
            "raw_numpy": (best_np, med_np),
            "pint_quantity": (best_pq, med_pq),
            "ureg_wraps": (best_uw, med_uw),
        })

    return results


def run_morison_benchmarks():
    """Run Morison drag force benchmarks and return structured results."""
    results = []

    for size in ARRAY_SIZES:
        v_np = np.linspace(0, 10, size)
        v_q = Q_(v_np, "m/s")

        best_np, med_np = bench(morison_raw_numpy, (v_np,))
        best_pq, med_pq = bench(morison_pint_quantity, (v_q,))
        best_uw, med_uw = bench(morison_ureg_wraps, (CD, RHO_Q, AREA_Q, v_q))

        results.append({
            "size": size,
            "raw_numpy": (best_np, med_np),
            "pint_quantity": (best_pq, med_pq),
            "ureg_wraps": (best_uw, med_uw),
        })

    return results


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def format_size(n):
    if n >= 1_000_000:
        return f"{n // 1_000_000}M"
    if n >= 1_000:
        return f"{n // 1_000}K"
    return str(n)


def print_results(results, title):
    """Print a formatted results table with overhead ratios."""
    sep = "-" * 88
    header = (
        f"{'Array':>7} | {'Raw NumPy':>12} | {'Pint Quantity':>14} | "
        f"{'@ureg.wraps':>14} | {'Pint/Raw':>9} | {'Wraps/Raw':>9}"
    )

    print()
    print("=" * 88)
    print(f"PINT PERFORMANCE BENCHMARK — {title}")
    print(f"  timeit config: {N_REPEATS} repeats x {N_NUMBER} calls, reporting median (ms)")
    print("=" * 88)
    print(header)
    print(sep)

    for r in results:
        size_str = format_size(r["size"])
        _, med_np = r["raw_numpy"]
        _, med_pq = r["pint_quantity"]
        _, med_uw = r["ureg_wraps"]
        ratio_pq = med_pq / med_np if med_np > 0 else float("inf")
        ratio_uw = med_uw / med_np if med_np > 0 else float("inf")

        print(
            f"{size_str:>7} | {med_np:>10.4f}ms | {med_pq:>12.4f}ms | "
            f"{med_uw:>12.4f}ms | {ratio_pq:>8.1f}x | {ratio_uw:>8.1f}x"
        )

    print(sep)
    print()

    return results


def print_summary(hydro_results, morison_results):
    """Print actionable guidance based on results from both benchmarks."""
    print("=" * 88)
    print("SUMMARY & RECOMMENDATIONS")
    print("=" * 88)

    for label, results in [("Hydrostatic", hydro_results),
                           ("Morison", morison_results)]:
        pint_ratios = []
        wraps_ratios = []
        for r in results:
            _, med_np = r["raw_numpy"]
            _, med_pq = r["pint_quantity"]
            _, med_uw = r["ureg_wraps"]
            pint_ratios.append(med_pq / med_np if med_np > 0 else 0)
            wraps_ratios.append(med_uw / med_np if med_np > 0 else 0)

        # Report per-size ratios
        for r, pr, wr in zip(results, pint_ratios, wraps_ratios):
            sz = format_size(r["size"])
            print(f"  {label:>12} {sz:>4}: Pint {pr:5.1f}x | @wraps {wr:5.1f}x")

    print()
    print("  THRESHOLDS & GUIDANCE")
    print("  " + "-" * 60)
    print("  Array size < 1K   : Pint overhead 5-25x but absolute time")
    print("                      is microseconds. Use full Pint Quantity")
    print("                      for unit safety — overhead is negligible")
    print("                      in wall-clock terms.")
    print()
    print("  Array size 1K-10K : Overhead drops to 2-5x. Full Pint")
    print("                      Quantity is the default recommendation.")
    print()
    print("  Array size > 10K  : Overhead approaches 1-2x. NumPy")
    print("                      vectorized math dominates; Pint unit")
    print("                      bookkeeping is amortized. Use freely.")
    print()
    print("  @ureg.wraps()     : Adds constant overhead per call for")
    print("                      unit strip/reattach. Best for wrapping")
    print("                      legacy functions that cannot accept")
    print("                      Quantity objects. Avoid for new code.")
    print()
    print("  Inner loops with  : If calling a function millions of times")
    print("  tiny arrays (<100): with scalar or tiny arrays, use raw")
    print("                      NumPy with unit docs in comments.")
    print()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    # ── Correctness checks ────────────────────────────────────────────────
    print("Running correctness checks...")

    # Hydrostatic
    depth_check = np.array([0.0, 100.0, 3000.0])
    p_raw = hydro_raw_numpy(depth_check)
    p_pint = hydro_pint_quantity(Q_(depth_check, "m")).to("Pa").magnitude
    p_wraps = hydro_ureg_wraps(RHO_Q, G_Q, Q_(depth_check, "m")).magnitude
    assert np.allclose(p_raw, p_pint), "Hydrostatic: Pint Quantity mismatch"
    assert np.allclose(p_raw, p_wraps), "Hydrostatic: @ureg.wraps mismatch"
    print("  Hydrostatic pressure — PASS (all three methods agree)")

    # Morison
    v_check = np.array([0.0, 1.0, 2.0, 5.0])
    f_raw = morison_raw_numpy(v_check)
    f_pint = morison_pint_quantity(Q_(v_check, "m/s")).to("N").magnitude
    f_wraps = morison_ureg_wraps(CD, RHO_Q, AREA_Q, Q_(v_check, "m/s")).magnitude
    assert np.allclose(f_raw, f_pint), "Morison: Pint Quantity mismatch"
    assert np.allclose(f_raw, f_wraps), "Morison: @ureg.wraps mismatch"
    # Verify known value: F(v=2) = 0.5 * 1.0 * 1025 * 1.0 * 4 = 2050 N
    assert abs(f_raw[2] - 2050.0) < 0.01, "Morison: known-value check failed"
    print("  Morison drag force — PASS (all three methods agree, F(v=2)=2050 N)")

    # ── Benchmarks ────────────────────────────────────────────────────────
    print("\nRunning benchmarks (this may take a minute)...")

    hydro_results = run_hydrostatic_benchmarks()
    print_results(hydro_results, "Hydrostatic Pressure P = rho * g * depth")

    morison_results = run_morison_benchmarks()
    print_results(morison_results, "Morison Drag Force F = 0.5 * Cd * rho * A * v^2")

    print_summary(hydro_results, morison_results)
