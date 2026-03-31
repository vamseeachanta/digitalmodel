#!/usr/bin/env python3
"""Fluids Piping Design Library — Evaluation Script.

Demonstrates fluids integration for offshore piping design workflows.
Reference: vamseeachanta/workspace-hub#1450
Library: https://github.com/CalebBell/fluids (v1.3.0, MIT)
"""

import math

from fluids.core import Reynolds, dP_from_K
from fluids.fittings import (
    K_gate_valve_Crane,
    K_globe_valve_Crane,
    bend_rounded,
)
from fluids.friction import friction_factor, one_phase_dP
from fluids.safety_valve import API520_A_g, API520_round_size


def demo_friction_factors():
    """Darcy-Weisbach friction factor calculation."""
    print("=" * 60)
    print("1. FRICTION FACTOR CALCULATION")
    print("=" * 60)

    # Turbulent flow — Clamond exact solution (default)
    fd = friction_factor(Re=1e5, eD=1e-4)
    print(f"  Turbulent (Re=1e5, eD=1e-4):  fd = {fd:.6f}")

    # Laminar flow — 64/Re
    fd_lam = friction_factor(Re=1000)
    print(f"  Laminar   (Re=1000):           fd = {fd_lam:.6f}  (64/Re = {64/1000:.6f})")

    # Smooth pipe
    fd_smooth = friction_factor(Re=1e5, eD=0)
    print(f"  Smooth    (Re=1e5, eD=0):      fd = {fd_smooth:.6f}")

    # Rough pipe
    fd_rough = friction_factor(Re=1e5, eD=1e-2)
    print(f"  Rough     (Re=1e5, eD=1e-2):   fd = {fd_rough:.6f}")

    # Named correlations
    for method in ["Colebrook", "Swamee_Jain_1976", "Haaland"]:
        fd_m = friction_factor(Re=1e5, eD=1e-4, Method=method)
        print(f"  {method:30s} fd = {fd_m:.6f}")
    print()


def demo_pipe_pressure_drop():
    """Pipe pressure drop for a representative offshore scenario.

    Scenario: 8-inch seawater injection line, 100 m horizontal run.
    """
    print("=" * 60)
    print("2. PIPE PRESSURE DROP — Offshore Seawater Injection")
    print("=" * 60)

    D = 0.2032            # 8 inch NPS
    rho = 1025.0          # seawater density kg/m3
    mu = 0.001            # dynamic viscosity Pa.s
    V = 2.0               # flow velocity m/s
    roughness = 4.57e-5   # commercial steel roughness m
    L = 100.0             # pipe length m

    A = math.pi * D**2 / 4
    m = rho * V * A
    Re = Reynolds(V=V, D=D, rho=rho, mu=mu)

    print(f"  Pipe ID:        {D*1000:.1f} mm ({D/0.0254:.1f} in)")
    print(f"  Flow velocity:  {V} m/s")
    print(f"  Mass flow rate: {m:.1f} kg/s")
    print(f"  Reynolds number:{Re:.0f}")

    dP = one_phase_dP(m=m, rho=rho, mu=mu, D=D, roughness=roughness, L=L)
    print(f"  Pressure drop:  {dP:.0f} Pa = {dP/1000:.2f} kPa over {L} m")

    # Hand-calc verification: dP = fd * (L/D) * 0.5 * rho * V^2
    fd = friction_factor(Re=Re, eD=roughness / D)
    dP_hand = fd * (L / D) * 0.5 * rho * V**2
    print(f"  Hand-calc check:{dP_hand:.0f} Pa (fd={fd:.6f})")
    print(f"  Match:          {abs(dP - dP_hand) / dP * 100:.2f}% difference")
    print()


def demo_fitting_k_factors():
    """K-factor lookup for common fittings (Crane TP-410 method)."""
    print("=" * 60)
    print("3. FITTING K-FACTORS (Crane TP-410)")
    print("=" * 60)

    D = 0.2032  # 8 inch
    Re = Reynolds(V=2.0, D=D, rho=1025, mu=0.001)
    rho = 1025.0
    V = 2.0

    fittings = {
        "Gate valve (full open)": K_gate_valve_Crane(D1=D, D2=D, angle=0.0),
        "Globe valve": K_globe_valve_Crane(D1=D, D2=D),
        "90° elbow (r/D=1.5)": bend_rounded(Di=D, angle=90, rc=1.5 * D, Re=Re),
        "45° elbow (r/D=1.5)": bend_rounded(Di=D, angle=45, rc=1.5 * D, Re=Re),
    }

    print(f"  {'Fitting':<30s} {'K':>8s} {'dP (Pa)':>10s}")
    print(f"  {'-'*30} {'-'*8} {'-'*10}")
    for name, K in fittings.items():
        dP = dP_from_K(K=K, rho=rho, V=V)
        print(f"  {name:<30s} {K:8.4f} {dP:10.1f}")
    print()


def demo_relief_valve_sizing():
    """API 520 relief valve sizing for gas service."""
    print("=" * 60)
    print("4. RELIEF VALVE SIZING (API 520)")
    print("=" * 60)

    # Natural gas relief scenario
    m = 1.0        # mass flow rate kg/s
    T = 350.0      # temperature K (77°C)
    Z = 0.95       # compressibility factor
    MW = 28.97     # molecular weight (air equivalent)
    k = 1.4        # isentropic coefficient
    P1 = 1e6       # set pressure Pa (10 barg)
    P2 = 101325    # back pressure Pa (atmospheric)

    A_required = API520_A_g(m=m, T=T, Z=Z, MW=MW, k=k, P1=P1, P2=P2)
    A_standard = API520_round_size(A_required)

    print(f"  Set pressure:    {P1/1e5:.0f} bar abs")
    print(f"  Flow rate:       {m} kg/s")
    print(f"  Required area:   {A_required*1e6:.1f} mm² ({A_required*1e4:.2f} cm²)")
    print(f"  Standard orifice:{A_standard*1e6:.1f} mm² ({A_standard*1e4:.2f} cm²)")
    print(f"  Oversize factor: {A_standard/A_required:.2f}x")
    print()


def main():
    """Run all demonstrations."""
    print()
    print("FLUIDS LIBRARY EVALUATION — Piping Design Workflows")
    print("Library: fluids v1.3.0 | License: MIT")
    print("Repo: https://github.com/CalebBell/fluids")
    print()

    demo_friction_factors()
    demo_pipe_pressure_drop()
    demo_fitting_k_factors()
    demo_relief_valve_sizing()

    print("=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print("  fluids provides production-ready implementations of:")
    print("  - Darcy-Weisbach friction factors (29 correlations + exact)")
    print("  - Single-phase pipe pressure drop")
    print("  - Crane TP-410 fitting loss coefficients")
    print("  - API 520/526 relief valve sizing")
    print("  All pure Python, MIT licensed, well-documented.")
    print("  Recommended for adoption in digitalmodel piping workflows.")
    print()


if __name__ == "__main__":
    main()
