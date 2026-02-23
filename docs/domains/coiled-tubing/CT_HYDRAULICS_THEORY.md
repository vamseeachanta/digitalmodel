# Coiled Tubing Hydraulics Theory

This document summarizes the steady-state calculations used by the
`ct_hydraulics` module. The intent is to provide transparent, reproducible
pressure-loss estimates for coiled tubing (CT) circulation and annular return
flow.

Scope
- Single-phase liquid (no gas cut).
- Steady-state flow in CT and annulus.
- Darcy-Weisbach friction with laminar/turbulent friction factor.
- Hydrostatic pressure based on fluid density and TVD.

Core Equations

1) Area and Velocity

Pipe area:
  A = pi/4 * D^2

Annulus area:
  A_ann = pi/4 * (D_hole^2 - D_inner^2)

Velocity:
  V = Q / A

2) Hydraulic Diameter (Annulus)

For a concentric annulus:
  D_h = D_hole - D_inner

3) Reynolds Number

  Re = rho * V * D_h / mu

4) Friction Factor

Laminar (Re < 2100):
  f = 64 / Re

Turbulent (Haaland approximation):
  f = [ -1.8 * log10( (e/D_h / 3.7)^1.11 + 6.9/Re ) ]^-2

5) Darcy-Weisbach Pressure Loss

  dP = f * (L / D_h) * (rho * V^2 / 2)

6) Volumes and Circulation Times

  Volume = A * L
  Top-down time (CT) = CT volume / rate
  Bottoms-up time (annulus) = annulus volume / rate

7) Hydrostatic Pressure

  P_h = 0.052 * MW_ppg * TVD_ft

8) Equivalent Circulating Density (ECD)

  ECD_ppg = MW_ppg + dP_annulus / (0.052 * TVD_ft)

9) Pump Pressure (steady circulation)

  P_pump = P_wellhead + dP_CT + dP_BHA + dP_annulus + dP_reel

Notes and Limitations
- Power-law or Herschel-Bulkley fluids are not modeled directly. Use effective
  viscosity in cP to approximate the CT and annulus rheology.
- Temperature effects, drag reduction, and cuttings transport are not included.
- Annular hydraulic diameter assumes concentric geometry unless overridden.

Reference Data
The example input file `examples/domains/input_files/ct_hydraulics/ga_drilling_antech_hydraulics.yml`
is derived from the AnTech Cerberus report for GA Drilling Modelling (v0.6).
