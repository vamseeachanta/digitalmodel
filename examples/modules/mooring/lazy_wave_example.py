"""
Example usage of the lazy-wave catenary solver.

This example demonstrates how to analyze a lazy-wave riser configuration
with buoyancy modules using the modern API.
"""

import sys
from pathlib import Path

# Add src to path for direct execution
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.marine_ops.marine_engineering.catenary import (
    LazyWaveSolver,
    LazyWaveConfiguration
)


def main():
    """Run lazy-wave catenary analysis example."""

    # Define lazy-wave configuration
    # This represents a typical deep-water riser with buoyancy modules
    config = LazyWaveConfiguration(
        hangoff_angle=15.0,              # Departure angle from vessel [degrees]
        hangoff_below_msl=50.0,          # Hang-off depth [m]
        hog_bend_above_seabed=300.0,     # Hog bend elevation [m]
        sag_bend_elevation=150.0,        # Sag bend elevation [m]
        weight_without_buoyancy=1000.0,  # Bare riser weight [N/m]
        weight_with_buoyancy=-500.0,     # With buoyancy modules [N/m] (negative)
        vertical_distance=500.0,         # Total vertical span [m]
        hangoff_bend_radius=2000.0       # Initial bend radius [m]
    )

    print("="*60)
    print("LAZY-WAVE CATENARY ANALYSIS")
    print("="*60)
    print("\nConfiguration:")
    print(f"  Hang-off angle: {config.hangoff_angle}°")
    print(f"  Hang-off depth: {config.hangoff_below_msl} m")
    print(f"  Hog bend elevation: {config.hog_bend_above_seabed} m")
    print(f"  Sag bend elevation: {config.sag_bend_elevation} m")
    print(f"  Bare riser weight: {config.weight_without_buoyancy} N/m")
    print(f"  With buoyancy: {config.weight_with_buoyancy} N/m")
    print(f"  Bend radius: {config.hangoff_bend_radius} m")

    # Solve lazy-wave catenary
    solver = LazyWaveSolver()
    results = solver.solve(config)

    print("\n" + "-"*60)
    print("RESULTS")
    print("-"*60)

    # Forces
    print("\nForces at Hang-off:")
    print(f"  Horizontal force (Fh): {results.horizontal_force:,.0f} N")
    print(f"  Vertical force (Fv):   {results.vertical_force:,.0f} N")
    print(f"  Force ratio (Fv/Fh):   {results.vertical_force/results.horizontal_force:.3f}")

    # Geometry
    print("\nOverall Geometry:")
    print(f"  Total arc length:        {results.total_arc_length:,.1f} m")
    print(f"  Total horizontal dist:   {results.total_horizontal_distance:,.1f} m")

    # Segment breakdown
    print("\nSegment Breakdown:")
    print(f"  1. Hang-off to sag:")
    print(f"     Arc length: {results.hangoff_to_sag.arc_length:.1f} m")
    print(f"     Horizontal: {results.hangoff_to_sag.horizontal_distance:.1f} m")

    print(f"  2. Sag to buoyancy:")
    print(f"     Arc length: {results.sag_to_buoyancy.arc_length:.1f} m")
    print(f"     Horizontal: {results.sag_to_buoyancy.horizontal_distance:.1f} m")

    print(f"  3. Buoyancy to hog:")
    print(f"     Arc length: {results.buoyancy_to_hog.arc_length:.1f} m")
    print(f"     Horizontal: {results.buoyancy_to_hog.horizontal_distance:.1f} m")

    print(f"  4. Hog to buoyancy end:")
    print(f"     Arc length: {results.hog_to_buoyancy_end.arc_length:.1f} m")
    print(f"     Horizontal: {results.hog_to_buoyancy_end.horizontal_distance:.1f} m")

    print(f"  5. Buoyancy to touchdown:")
    print(f"     Arc length: {results.buoyancy_to_touchdown.arc_length:.1f} m")
    print(f"     Horizontal: {results.buoyancy_to_touchdown.horizontal_distance:.1f} m")

    # Summary sections
    print("\nSummary Sections:")
    print(f"  Hang-off to buoyancy:")
    print(f"    S = {results.summary['HangOffToBuoyancy']['S']:.1f} m")
    print(f"    X = {results.summary['HangOffToBuoyancy']['X']:.1f} m")

    print(f"  Buoyancy section:")
    print(f"    S = {results.summary['Buoyancy']['S']:.1f} m")
    print(f"    X = {results.summary['Buoyancy']['X']:.1f} m")

    print(f"  Buoyancy to touchdown:")
    print(f"    S = {results.summary['BuoyancyToTouchDown']['S']:.1f} m")
    print(f"    X = {results.summary['BuoyancyToTouchDown']['X']:.1f} m")

    # Legacy compatibility
    print("\n" + "-"*60)
    print("LEGACY FORMAT CONVERSION")
    print("-"*60)

    legacy_dict = solver.to_legacy_dict(results, config)
    print(f"\nLegacy dict contains {len(legacy_dict)} keys:")
    print(f"  {', '.join(legacy_dict.keys())}")
    print(f"\nLegacy Summary Fh: {legacy_dict['Summary']['Fh']:,.0f} N")
    print(f"Legacy Summary Fv: {legacy_dict['Summary']['Fv']:,.0f} N")

    print("\n" + "="*60)
    print("ANALYSIS COMPLETE")
    print("="*60)


if __name__ == '__main__':
    main()
