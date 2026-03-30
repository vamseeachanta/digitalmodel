#!/usr/bin/env python3
"""Pint Performance Benchmark — Array-Heavy Calculations.

Benchmarks raw NumPy vs Pint Quantity vs @ureg.wraps() decorator for
hydrostatic pressure: P = rho * g * depth.

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

ARRAY_SIZES = [100, 10_000, 1_000_000]
N_REPEATS = 50        # timeit repeats per measurement
N_NUMBER = 10         # executions per repeat (timeit averages over this)


# ---------------------------------------------------------------------------
# Calculation variants
# ---------------------------------------------------------------------------

def calc_raw_numpy(depth_arr: np.ndarray) -> np.ndarray:
    """Pure NumPy — no unit tracking."""
    return RHO * G * depth_arr


def calc_pint_quantity(depth_q):
    """Full Pint Quantity arithmetic — units on every operand."""
    return RHO_Q * G_Q * depth_q


@ureg.wraps("Pa", ("kg/m**3", "m/s**2", "m"))
def calc_ureg_wraps(rho, g, depth):
    """@ureg.wraps — strips units at boundary, raw math inside."""
    return rho * g * depth


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


def run_benchmarks():
    """Run all benchmarks and return structured results."""
    results = []

    for size in ARRAY_SIZES:
        depth_np = np.linspace(0, 3000, size)
        depth_q = Q_(depth_np, "m")

        # 1. Raw NumPy
        best_np, med_np = bench(calc_raw_numpy, (depth_np,))

        # 2. Pint Quantity
        best_pq, med_pq = bench(calc_pint_quantity, (depth_q,))

        # 3. @ureg.wraps
        best_uw, med_uw = bench(calc_ureg_wraps, (RHO_Q, G_Q, depth_q))

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


def print_results(results):
    """Print a formatted results table with overhead ratios."""
    sep = "-" * 88
    header = (
        f"{'Array':>7} | {'Raw NumPy':>12} | {'Pint Quantity':>14} | "
        f"{'@ureg.wraps':>14} | {'Pint/Raw':>9} | {'Wraps/Raw':>9}"
    )

    print()
    print("=" * 88)
    print("PINT PERFORMANCE BENCHMARK — Hydrostatic Pressure P = rho * g * depth")
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


def print_summary(results):
    """Print actionable guidance based on results."""
    print("SUMMARY & RECOMMENDATIONS")
    print("-" * 88)

    # Compute average overhead ratios
    pint_ratios = []
    wraps_ratios = []
    for r in results:
        _, med_np = r["raw_numpy"]
        _, med_pq = r["pint_quantity"]
        _, med_uw = r["ureg_wraps"]
        pint_ratios.append(med_pq / med_np if med_np > 0 else 0)
        wraps_ratios.append(med_uw / med_np if med_np > 0 else 0)

    avg_pint = sum(pint_ratios) / len(pint_ratios)
    avg_wraps = sum(wraps_ratios) / len(wraps_ratios)

    print(f"  Average overhead — Pint Quantity: {avg_pint:.1f}x | @ureg.wraps: {avg_wraps:.1f}x")
    print()
    print("  Guidance:")
    print("  - Pint Quantity overhead shrinks dramatically with array size.")
    print("    At 1M elements the overhead is negligible (~1x) because NumPy")
    print("    vectorized math dominates the per-operation unit bookkeeping.")
    print("  - @ureg.wraps() adds unit-strip/reattach cost on every call.")
    print("    It is slower than direct Pint Quantity for simple expressions.")
    print("    Prefer it only when wrapping legacy code that cannot accept Quantity.")
    print("  - For small arrays (<10K): overhead is real but absolute time is tiny.")
    print("    Full Pint Quantity is fine — unit safety is worth microseconds.")
    print("  - For large arrays (>=10K): Pint Quantity approaches raw NumPy speed.")
    print("    Use it freely. Avoid @ureg.wraps() unless interfacing with non-Pint code.")
    print("  - For inner-loop numerics called millions of times with tiny arrays:")
    print("    use raw NumPy with manual unit documentation in comments/docstrings.")
    print()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    # Verify correctness before benchmarking
    depth_check = np.array([0.0, 100.0, 3000.0])
    p_raw = calc_raw_numpy(depth_check)
    p_pint = calc_pint_quantity(Q_(depth_check, "m")).to("Pa").magnitude
    p_wraps = calc_ureg_wraps(RHO_Q, G_Q, Q_(depth_check, "m")).magnitude
    assert np.allclose(p_raw, p_pint), "Pint Quantity result mismatch"
    assert np.allclose(p_raw, p_wraps), "@ureg.wraps result mismatch"
    print("Correctness check passed — all three methods agree.")

    results = run_benchmarks()
    print_results(results)
    print_summary(results)
