#!/usr/bin/env python3
"""Pint Physical Units Library — Proof of Concept.

Demonstrates Pint integration for digitalmodel engineering calculations.
Reference: vamseeachanta/workspace-hub#1459
"""

import numpy as np
import pint

# ---------------------------------------------------------------------------
# 1. Setup — create a shared UnitRegistry (one per application)
# ---------------------------------------------------------------------------
ureg = pint.UnitRegistry()
Q_ = ureg.Quantity  # shorthand constructor


def demo_basic_conversions():
    """Basic unit conversions relevant to offshore engineering."""
    print("=" * 60)
    print("1. BASIC UNIT CONVERSIONS")
    print("=" * 60)

    # Length
    depth = Q_(100, "meter")
    print(f"  {depth} = {depth.to('feet'):.4f}")

    # Force
    force = Q_(500, "kilonewton")
    print(f"  {force} = {force.to('lbf'):.4f}")

    # Pressure
    pressure = Q_(10, "megapascal")
    print(f"  {pressure} = {pressure.to('psi'):.4f}")

    # Mass per volume (density)
    rho = Q_(1025, "kg/m**3")
    print(f"  {rho} = {rho.to('lb/ft**3'):.4f}")

    # Temperature
    temp = Q_(4, "degC")
    print(f"  {temp} = {temp.to('degF'):.4f}")

    print()


def demo_numpy_integration():
    """NumPy array operations with units attached."""
    print("=" * 60)
    print("2. NUMPY ARRAY OPERATIONS WITH UNITS")
    print("=" * 60)

    depths = Q_(np.array([10, 50, 100, 500, 1000]), "meter")
    print(f"  Depths (m):  {depths.magnitude}")
    print(f"  Depths (ft): {depths.to('feet').magnitude.round(1)}")

    # Element-wise operations preserve units
    g = Q_(9.80665, "m/s**2")
    rho = Q_(1025, "kg/m**3")
    pressures = rho * g * depths
    print(f"  Hydrostatic pressures: {pressures.to('kPa').magnitude.round(1)} kPa")

    # Statistical operations
    print(f"  Mean pressure: {pressures.to('kPa').magnitude.mean():.1f} kPa")
    print(f"  Max pressure:  {pressures.to('MPa').magnitude.max():.3f} MPa")

    print()


def demo_unit_validation():
    """Demonstrate that Pint catches incompatible unit math."""
    print("=" * 60)
    print("3. UNIT VALIDATION — CATCHING ERRORS")
    print("=" * 60)

    length = Q_(10, "meter")
    force = Q_(100, "kN")

    # Valid: force / area = pressure
    area = Q_(2, "m**2")
    stress = force / area
    print(f"  Force / Area = {stress.to('kPa'):.1f}  ✓")

    # Invalid: adding length + force
    try:
        _ = length + force
        print("  ERROR: should have raised DimensionalityError")
    except pint.DimensionalityError as e:
        print(f"  length + force → DimensionalityError  ✓")
        print(f"    ({e})")

    # Invalid: assigning wrong units
    try:
        pressure = Q_(10, "MPa")
        _ = pressure.to("meter")
    except pint.DimensionalityError:
        print(f"  pressure.to('meter') → DimensionalityError  ✓")

    print()


def demo_offshore_hydrostatic():
    """Offshore-relevant: hydrostatic pressure = ρ·g·h with proper units."""
    print("=" * 60)
    print("4. OFFSHORE EXAMPLE — HYDROSTATIC PRESSURE")
    print("=" * 60)

    # Seawater properties
    rho_sw = Q_(1025, "kg/m**3")       # seawater density
    g = Q_(9.80665, "m/s**2")          # gravitational acceleration
    depth = Q_(1500, "meter")           # water depth (deepwater)

    # Hydrostatic pressure: P = ρ·g·h
    P_hydro = rho_sw * g * depth
    print(f"  Water depth:  {depth}")
    print(f"  Seawater ρ:   {rho_sw}")
    print(f"  g:            {g}")
    print()
    print(f"  Hydrostatic pressure:")
    print(f"    {P_hydro.to('MPa'):.3f}")
    print(f"    {P_hydro.to('psi'):.1f}")
    print(f"    {P_hydro.to('bar'):.2f}")
    print(f"    {P_hydro.to('atm'):.2f}")

    # Pipeline collapse check example
    print()
    print("  Pipeline collapse check (simplified):")
    OD = Q_(323.9, "mm")               # 12.75" pipe
    t = Q_(20.6, "mm")                 # wall thickness
    SMYS = Q_(450, "MPa")              # X65 steel
    D_over_t = (OD / t).to("dimensionless")
    P_collapse = 2 * SMYS * (t / OD)   # simplified Barlow
    print(f"    OD = {OD} ({OD.to('inch'):.3f})")
    print(f"    t  = {t} ({t.to('inch'):.3f})")
    print(f"    D/t = {D_over_t:.1f}")
    print(f"    Simplified collapse pressure = {P_collapse.to('MPa'):.2f}")
    print(f"    Hydrostatic / Collapse = {(P_hydro / P_collapse).to('dimensionless'):.3f}")

    print()


def demo_unit_contexts():
    """Show Pint's context system for non-standard conversions."""
    print("=" * 60)
    print("5. ADDITIONAL FEATURES")
    print("=" * 60)

    # String parsing — useful for config-driven calculations
    q = ureg.parse_expression("25.4 millimeter")
    print(f"  Parsed '25.4 millimeter' → {q.to('inch')}")

    # Compact representation
    volume = Q_(1500, "liter")
    print(f"  {volume} = {volume.to('m**3')}")

    # Formatting
    force = Q_(1234.5, "kN")
    print(f"  Default:  {force}")
    print(f"  Compact:  {force.to_compact()}")
    print(f"  Format:   {force:.2f~P}")  # pretty-print with 2 decimals

    print()


if __name__ == "__main__":
    print(f"Pint version: {pint.__version__}")
    print(f"NumPy version: {np.__version__}")
    print()

    demo_basic_conversions()
    demo_numpy_integration()
    demo_unit_validation()
    demo_offshore_hydrostatic()
    demo_unit_contexts()

    print("All demonstrations completed successfully.")
