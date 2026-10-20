# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run a lazy-wave example with independently calculable hang-off forces.

Run with the package installed, or with PYTHONPATH=src from the repository root.
See docs/domains/catenary/lazy_wave_force_example.md for inputs and derivation.
"""

import math

from digitalmodel.marine_ops.marine_analysis.catenary import (
    LazyWaveConfiguration,
    LazyWaveResults,
    LazyWaveSolver,
)


def solve_example() -> tuple[LazyWaveConfiguration, LazyWaveResults]:
    """Return a synthetic 3-4-5 force triangle and its lazy-wave solution.

    NREL/TP-500-41958 (November 2007), section 2.5, equations (2-41b),
    (2-43b) and (2-44), with VA=0 and EA tending to infinity, give
    V=w*S, d=sqrt(a*a+S*S)-a and T=sqrt(H*H+V*V), where H=w*a.
    Choose a=300 m, S=400 m: d=200 m, q=atan(3/4) from vertical.
    The synthetic weight w=10 N/m then gives H=3000, V=4000, T=5000 N.
    """
    # Constructed inputs, not field data or recommended design values. The
    # 300 m seabed depth minus 100 m sag elevation gives the required 200 m
    # hang-off span. The 200 m hog elevation and -10 N/m buoyed weight are
    # synthetic choices; all are documented in lazy_wave_force_example.md.
    config = LazyWaveConfiguration(
        hangoff_angle=math.degrees(math.atan2(3.0, 4.0)),
        hangoff_below_msl=0.0,
        vertical_distance=300.0,
        sag_bend_elevation=100.0,
        hog_bend_above_seabed=200.0,
        weight_without_buoyancy=10.0,
        weight_with_buoyancy=-10.0,
    )
    return config, LazyWaveSolver().solve(config)


def main() -> None:
    """Print inputs, method, solution and the converted dictionary forces."""
    config, result = solve_example()
    print("LAZY-WAVE CATENARY ANALYSIS")
    print("Synthetic inputs; no design acceptance assessment")
    print(f"Hang-off angle from vertical: {config.hangoff_angle:.1f} degrees")
    print(f"Hang-off below mean sea level: {config.hangoff_below_msl:.1f} m (datum)")
    print(f"Hang-off to seabed: {config.vertical_distance:.1f} m")
    print(f"Sag bend elevation: {config.sag_bend_elevation:.1f} m")
    print(f"Hog bend elevation: {config.hog_bend_above_seabed:.1f} m")
    print(f"Bare submerged weight: {config.weight_without_buoyancy:.1f} N/m")
    print(f"Net buoyed weight: {config.weight_with_buoyancy:.1f} N/m")
    print("Method: V = w*S; H = w*a; T = hypot(H, V)")
    print("Hand calculation: a=300.0 m, S=400.0 m, d=200.0 m")
    print(f"Derived vertex radius: {result.hangoff_to_sag.bend_radius:.1f} m")
    print(f"Hang-off arc length: {result.hangoff_to_sag.arc_length:.1f} m")
    print(f"Horizontal force (Fh): {result.horizontal_force:.1f} N")
    print(f"Vertical force (Fv): {result.vertical_force:.1f} N")
    print(f"Total tension (T): {result.total_tension:.1f} N")
    net_weight = math.fsum(s.weight_per_length * s.arc_length for s in result.segments)
    print(f"Signed suspended weight: {net_weight:.1f} N")
    angle = math.degrees(math.atan2(result.horizontal_force, result.vertical_force))
    print(f"Recovered angle from forces: {angle:.1f} degrees")
    print(f"Total suspended arc length: {result.total_arc_length:.1f} m")
    print(f"Total horizontal distance: {result.total_horizontal_distance:.1f} m")
    print(f"Water-depth closure residual: {result.vertical_closure_error:.1f} m")
    converted = LazyWaveSolver().to_legacy_dict(result, config)
    print(f"Converted summary Fv: {converted['Summary']['Fv']:.1f} N")
    print(f"Converted summary T: {converted['Summary']['T']:.1f} N")


if __name__ == "__main__":
    main()
