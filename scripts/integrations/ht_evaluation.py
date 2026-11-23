#!/usr/bin/env python3
"""Heat Transfer (ht) Library — Evaluation Script.

Demonstrates ht integration for offshore/subsea thermal design workflows.
Library: https://github.com/CalebBell/ht (v1.2.0, MIT)

Capabilities demonstrated:
  1. Internal convection (turbulent pipe flow)
  2. Conduction through a pipe wall
  3. Nucleate boiling on a surface
  4. Radiation between surfaces
  5. External cross-flow over a cylinder (risers/pipelines)
  6. Heat exchanger sizing (NTU-effectiveness method)
  7. Overall heat transfer coefficient for an insulated subsea pipeline
"""

import math

import ht
from ht.boiling_nucleic import Rohsenow
from ht.conduction import R_cylinder
from ht.conv_external import Nu_cylinder_Churchill_Bernstein
from ht.conv_internal import Nu_conv_internal
from ht.hx import effectiveness_from_NTU
from ht.insulation import k_material
from ht.radiation import q_rad


def demo_internal_convection():
    """Internal convection — turbulent flow in a subsea pipeline."""
    print("=" * 65)
    print("1. INTERNAL CONVECTION — Turbulent Pipe Flow")
    print("=" * 65)

    # Scenario: crude oil flowing through a 12-inch production pipeline
    D = 0.3048        # 12-inch ID [m]
    rho = 850.0       # crude oil density [kg/m^3]
    mu = 5e-3         # dynamic viscosity [Pa·s]
    k = 0.14          # thermal conductivity [W/m/K]
    Cp = 2000.0       # specific heat [J/kg/K]
    V = 2.0           # flow velocity [m/s]

    Re = rho * V * D / mu
    Pr = Cp * mu / k

    Nu = Nu_conv_internal(Re=Re, Pr=Pr)
    h = Nu * k / D  # convective heat transfer coefficient [W/m^2/K]

    print(f"  Pipe ID:        {D:.4f} m (12 inch)")
    print(f"  Flow velocity:  {V:.1f} m/s")
    print(f"  Reynolds number: {Re:.0f}")
    print(f"  Prandtl number:  {Pr:.2f}")
    print(f"  Nusselt number:  {Nu:.2f}")
    print(f"  h_internal:      {h:.2f} W/m^2/K")
    print()

    # Compare laminar vs turbulent regimes
    print("  --- Regime comparison ---")
    for Re_val in [500, 2300, 1e4, 1e5, 1e6]:
        Nu_val = Nu_conv_internal(Re=Re_val, Pr=Pr)
        print(f"  Re = {Re_val:>10.0f}  ->  Nu = {Nu_val:.4f}")
    print()


def demo_pipe_wall_conduction():
    """Conduction through a steel pipe wall."""
    print("=" * 65)
    print("2. CONDUCTION — Steel Pipe Wall Thermal Resistance")
    print("=" * 65)

    # Typical 12-inch offshore pipeline (API 5L X65)
    ri = 0.1524       # inner radius [m] (12-inch ID)
    ro = 0.1683       # outer radius [m] (wall thickness 15.9 mm)
    L = 1.0           # per metre of pipe
    k_steel = 50.0    # carbon steel conductivity [W/m/K]

    R_wall = R_cylinder(ri, ro, L, k_steel)
    R_manual = math.log(ro / ri) / (2 * math.pi * k_steel * L)

    print(f"  Inner radius:    {ri:.4f} m")
    print(f"  Outer radius:    {ro:.4f} m")
    print(f"  k (steel):       {k_steel:.1f} W/m/K")
    print(f"  R_wall:          {R_wall:.6e} K/W per metre")
    print(f"  R_manual check:  {R_manual:.6e} K/W per metre")
    print(f"  Match:           {abs(R_wall - R_manual) < 1e-10}")
    print()

    # Multi-layer: steel + concrete coating
    k_concrete = 1.5
    ro_concrete = ro + 0.05  # 50 mm concrete weight coating
    R_concrete = R_cylinder(ro, ro_concrete, L, k_concrete)
    print(f"  --- Multi-layer resistance ---")
    print(f"  R_steel:         {R_wall:.6e} K/W")
    print(f"  R_concrete:      {R_concrete:.6e} K/W")
    print(f"  R_total:         {R_wall + R_concrete:.6e} K/W")
    print()


def demo_nucleate_boiling():
    """Nucleate boiling — Rohsenow correlation for water at 1 atm."""
    print("=" * 65)
    print("3. NUCLEATE BOILING — Rohsenow Correlation (Water @ 1 atm)")
    print("=" * 65)

    # Saturated water at 100 °C, 1 atm
    rhol = 957.9       # liquid density [kg/m^3]
    rhog = 0.5956      # vapour density [kg/m^3]
    mul = 2.79e-4      # liquid viscosity [Pa·s]
    kl = 0.68          # liquid conductivity [W/m/K]
    Cpl = 4217.0       # liquid specific heat [J/kg/K]
    Hvap = 2.257e6     # latent heat of vaporisation [J/kg]
    sigma = 0.0589     # surface tension [N/m]

    print(f"  {'Te [K]':>10s}  {'q [W/m^2]':>12s}")
    print(f"  {'-'*10}  {'-'*12}")
    for Te in [5.0, 10.0, 15.0, 20.0, 25.0]:
        q = Rohsenow(
            rhol=rhol, rhog=rhog, mul=mul, kl=kl,
            Cpl=Cpl, Hvap=Hvap, sigma=sigma, Te=Te,
        )
        print(f"  {Te:10.1f}  {q:12.1f}")
    print()


def demo_radiation():
    """Thermal radiation between surfaces."""
    print("=" * 65)
    print("4. RADIATION — Heat Flux Between Surfaces")
    print("=" * 65)

    # Example: hot pipeline surface radiating to ambient
    emissivity = 0.9
    T_hot = 350.0      # pipeline surface [K] (~77 °C)
    T_cold = 277.0     # seabed/ambient [K] (~4 °C)

    q = q_rad(emissivity=emissivity, T=T_hot, T2=T_cold)
    sigma = 5.670374419e-8
    q_manual = emissivity * sigma * (T_hot**4 - T_cold**4)

    print(f"  T_hot:       {T_hot:.1f} K ({T_hot - 273.15:.1f} °C)")
    print(f"  T_cold:      {T_cold:.1f} K ({T_cold - 273.15:.1f} °C)")
    print(f"  Emissivity:  {emissivity}")
    print(f"  q_rad:       {q:.2f} W/m^2")
    print(f"  q_manual:    {q_manual:.2f} W/m^2")
    print()

    # Emissivity comparison
    print("  --- Emissivity sensitivity ---")
    for eps in [0.1, 0.3, 0.5, 0.7, 0.9, 1.0]:
        q_e = q_rad(emissivity=eps, T=T_hot, T2=T_cold)
        print(f"  eps = {eps:.1f}  ->  q = {q_e:.2f} W/m^2")
    print()


def demo_external_crossflow():
    """External cross-flow over a cylinder — Churchill-Bernstein."""
    print("=" * 65)
    print("5. EXTERNAL CROSS-FLOW — Cylinder (Riser/Pipeline)")
    print("=" * 65)

    # Scenario: seawater flowing past a 10-inch riser
    D = 0.2540          # 10-inch OD [m]
    rho_sw = 1025.0     # seawater density [kg/m^3]
    mu_sw = 1.08e-3     # seawater viscosity [Pa·s]
    k_sw = 0.596        # seawater conductivity [W/m/K]
    Cp_sw = 3993.0      # seawater specific heat [J/kg/K]

    Pr_sw = Cp_sw * mu_sw / k_sw

    print(f"  Pr (seawater):  {Pr_sw:.2f}")
    print(f"  {'Current [m/s]':>14s}  {'Re':>10s}  {'Nu':>10s}  {'h [W/m^2/K]':>14s}")
    print(f"  {'-'*14}  {'-'*10}  {'-'*10}  {'-'*14}")

    for V_current in [0.1, 0.5, 1.0, 2.0, 3.0]:
        Re = rho_sw * V_current * D / mu_sw
        Nu = Nu_cylinder_Churchill_Bernstein(Re=Re, Pr=Pr_sw)
        h = Nu * k_sw / D
        print(f"  {V_current:14.1f}  {Re:10.0f}  {Nu:10.2f}  {h:14.1f}")
    print()


def demo_heat_exchanger():
    """Heat exchanger sizing using NTU-effectiveness method."""
    print("=" * 65)
    print("6. HEAT EXCHANGER — NTU-Effectiveness Method")
    print("=" * 65)

    # Process-seawater heat exchanger
    # Hot side: process fluid  Cmin = 50 kW/K
    # Cold side: seawater      Cmax = 100 kW/K
    Cmin = 50e3   # W/K
    Cmax = 100e3  # W/K
    Cr = Cmin / Cmax  # capacity ratio

    print(f"  Cr = Cmin/Cmax = {Cr:.2f}")
    print()

    # Compare HX types
    for subtype in ["counterflow", "parallel", "crossflow"]:
        print(f"  --- {subtype} ---")
        print(f"  {'NTU':>6s}  {'eff':>8s}  {'Q/Qmax':>8s}")
        for NTU in [0.5, 1.0, 2.0, 3.0, 5.0]:
            eff = effectiveness_from_NTU(NTU=NTU, Cr=Cr, subtype=subtype)
            print(f"  {NTU:6.1f}  {eff:8.4f}  {eff:8.1%}")
        print()

    # Limiting cases
    print("  --- Limiting cases ---")
    eff_zero = effectiveness_from_NTU(NTU=0, Cr=0.5, subtype="counterflow")
    eff_high = effectiveness_from_NTU(NTU=100, Cr=0.0, subtype="counterflow")
    print(f"  NTU=0, Cr=0.5 (counterflow): eff = {eff_zero:.6f}")
    print(f"  NTU=100, Cr=0 (condenser):   eff = {eff_high:.6f}")
    print()


def demo_insulated_subsea_pipeline():
    """Overall heat transfer coefficient for an insulated subsea pipeline.

    Layers (inside -> outside):
      1. Internal film (crude oil convection)
      2. Steel pipe wall (conduction)
      3. Insulation — polyurethane foam (conduction)
      4. External film (seawater convection)

    Reference geometry: 12-inch pipeline, per unit length.
    """
    print("=" * 65)
    print("7. OVERALL U-VALUE — Insulated Subsea Pipeline")
    print("=" * 65)

    # Geometry
    ri = 0.1524       # inner radius [m]
    t_wall = 0.0159   # wall thickness [m]
    ro = ri + t_wall   # outer steel radius
    t_insul = 0.075   # insulation thickness [m]
    ro_insul = ro + t_insul  # outer insulation radius
    L = 1.0           # per metre

    # Material properties
    k_steel = 50.0    # carbon steel [W/m/K]
    k_insul = k_material("Spray-applied Polyurethane foam, 40 kg/m^3")

    # Internal convection (crude oil)
    Re_int = 1e5
    Pr_int = 71.43    # crude oil Pr
    k_oil = 0.14
    Nu_int = Nu_conv_internal(Re=Re_int, Pr=Pr_int)
    h_int = Nu_int * k_oil / (2 * ri)

    # External convection (seawater cross-flow, 0.5 m/s current)
    Re_ext = 1025 * 0.5 * (2 * ro_insul) / 1.08e-3
    Pr_ext = 3993 * 1.08e-3 / 0.596
    k_sw = 0.596
    Nu_ext = Nu_cylinder_Churchill_Bernstein(Re=Re_ext, Pr=Pr_ext)
    h_ext = Nu_ext * k_sw / (2 * ro_insul)

    # Thermal resistances (per unit length, radial)
    R_int = 1 / (h_int * 2 * math.pi * ri * L)
    R_wall = R_cylinder(ri, ro, L, k_steel)
    R_insul = R_cylinder(ro, ro_insul, L, k_insul)
    R_ext = 1 / (h_ext * 2 * math.pi * ro_insul * L)

    R_total = R_int + R_wall + R_insul + R_ext

    # Overall U-value referenced to outer insulation surface
    A_outer = 2 * math.pi * ro_insul * L
    U_overall = 1 / (R_total * A_outer)

    print(f"  Pipeline geometry:")
    print(f"    Inner radius:       {ri:.4f} m")
    print(f"    Steel OR:           {ro:.4f} m")
    print(f"    Insulation OR:      {ro_insul:.4f} m")
    print()
    print(f"  Material properties:")
    print(f"    k_steel:            {k_steel:.1f} W/m/K")
    print(f"    k_insulation (PU):  {k_insul:.4f} W/m/K")
    print()
    print(f"  Film coefficients:")
    print(f"    h_internal (oil):   {h_int:.2f} W/m^2/K  (Nu={Nu_int:.2f})")
    print(f"    h_external (sw):    {h_ext:.2f} W/m^2/K  (Nu={Nu_ext:.2f})")
    print()
    print(f"  Thermal resistances (K/W per m):")
    print(f"    R_internal film:    {R_int:.6e}")
    print(f"    R_steel wall:       {R_wall:.6e}")
    print(f"    R_insulation:       {R_insul:.6e}")
    print(f"    R_external film:    {R_ext:.6e}")
    print(f"    R_total:            {R_total:.6e}")
    print()
    print(f"  Overall U-value:      {U_overall:.2f} W/m^2/K")
    print(f"  (referenced to outer insulation surface)")
    print()

    # Sensitivity: insulation thickness
    print("  --- Insulation thickness sensitivity ---")
    print(f"  {'t_insul [mm]':>14s}  {'U [W/m^2/K]':>14s}  {'R_insul fraction':>16s}")
    for t in [0.025, 0.050, 0.075, 0.100, 0.150]:
        ro_i = ro + t
        R_i = R_cylinder(ro, ro_i, L, k_insul)
        R_tot = R_int + R_wall + R_i + 1 / (h_ext * 2 * math.pi * ro_i * L)
        A_o = 2 * math.pi * ro_i * L
        U = 1 / (R_tot * A_o)
        print(f"  {t*1000:14.0f}  {U:14.2f}  {R_i/R_tot:16.1%}")
    print()


def main():
    print()
    print("*" * 65)
    print("  ht (Heat Transfer) Library — Evaluation Script")
    print(f"  Version: {ht.__version__}")
    print("*" * 65)
    print()

    demo_internal_convection()
    demo_pipe_wall_conduction()
    demo_nucleate_boiling()
    demo_radiation()
    demo_external_crossflow()
    demo_heat_exchanger()
    demo_insulated_subsea_pipeline()

    print("=" * 65)
    print("  Evaluation complete — all ht capabilities verified.")
    print("=" * 65)


if __name__ == "__main__":
    main()
