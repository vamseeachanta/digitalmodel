#!/usr/bin/env python
# ABOUTME: Sizes pump-off severity and a slow-down SPM from an operator's stated well data
# ABOUTME: Arithmetic only -- no card file needed, every input is a public quote

"""
What SPM would stop this well pounding? — from stated data alone
================================================================

A community thread told the operator, correctly, that his well is pumping off
and that the fix is to slow the unit or set a shutdown. Nobody gave him a
**number**. This does, using only figures he posted publicly, so every step can
be checked by hand.

The chain is deliberately boring:

1. Fluid load on the plunger, from pump bore and fluid column.
2. Elastic rod stretch under that load — this is why plunger stroke is shorter
   than surface stroke, and why fillage computed on surface stroke is wrong.
3. Pump displacement at 100% fillage, at the current SPM.
4. Compare with the barrels he actually reports → implied fillage.
5. Solve for the SPM at which displacement matches inflow, i.e. the speed at
   which the pump would run full instead of pounding.

Nothing here needs a card file. That is the point — it is checkable by the
operator with a calculator, which is what makes it worth posting.

Run::

    PYTHONPATH=src python docs/small_operator/examples/pumpoff_spm_from_stated_data.py
"""

from __future__ import annotations

import math

from digitalmodel.marine_ops.artificial_lift.dynacard.poc_settings import (
    estimate_production_bpd,
)

# ---------------------------------------------------------------------------
# STATED by the operator, verbatim, in the July "Dynamometer Discussions" post
# ---------------------------------------------------------------------------
STATED = {
    "pump_bore_in": 1.25,          # '1-1/4" top hold down insert pump'
    "pump_depth_ft": 4300.0,       # "4300' pump depth"
    "surface_stroke_in": 41.0,     # '41" of stroke at surface'
    "spm": 6.4,                    # "6.4 SPM"
    "rod_diameter_in": 0.75,       # "4200' of 3/4\" steel rods"
    "rod_length_ft": 4200.0,       # ditto
    "oil_bopd": 23.0,              # "Well made 23 Bbls of oil yesterday. No water"
    "water_bwpd": 0.0,             # "No water"
}

# ---------------------------------------------------------------------------
# ASSUMED — each is a question, and each is tested in the sensitivity block
# ---------------------------------------------------------------------------
ASSUMED = {
    "oil_specific_gravity": 0.85,  # typical Gulf Coast crude; he did not state API
    "fluid_over_pump_ft": 4300.0,  # pumped off => fluid level at the pump, so the
                                   # full column stands on the plunger. This is the
                                   # CONSERVATIVE end: less fluid means less stretch
                                   # and MORE plunger stroke.
    "youngs_modulus_psi": 30.0e6,  # steel
}

SEA_WATER_PSI_PER_FT = 0.433  # psi/ft per unit specific gravity


def plunger_area_sq_in(bore_in: float) -> float:
    return math.pi / 4.0 * bore_in ** 2


def rod_area_sq_in(diameter_in: float) -> float:
    return math.pi / 4.0 * diameter_in ** 2


def fluid_load_lb(bore_in: float, fluid_column_ft: float, specific_gravity: float) -> float:
    """Static fluid load carried by the plunger on the upstroke."""
    gradient_psi_per_ft = SEA_WATER_PSI_PER_FT * specific_gravity
    return gradient_psi_per_ft * fluid_column_ft * plunger_area_sq_in(bore_in)


def rod_stretch_in(load_lb: float, rod_length_ft: float, rod_diameter_in: float,
                   modulus_psi: float) -> float:
    """Elastic stretch of the rod string as it picks the fluid load up.

    dL = F L / (A E). This is the stroke the plunger never sees.
    """
    return load_lb * (rod_length_ft * 12.0) / (rod_area_sq_in(rod_diameter_in) * modulus_psi)


def spm_for_full_pump(target_bpd: float, bore_in: float, plunger_stroke_in: float,
                      fillage: float = 0.95) -> float:
    """SPM at which displacement equals inflow, so the pump runs full."""
    per_spm = estimate_production_bpd(bore_in, 1.0, plunger_stroke_in,
                                      pump_efficiency=fillage, runtime_fraction=1.0)
    return target_bpd / per_spm


def report(fluid_over_pump_ft: float, sg: float, label: str) -> dict:
    fo = fluid_load_lb(STATED["pump_bore_in"], fluid_over_pump_ft, sg)
    stretch = rod_stretch_in(fo, STATED["rod_length_ft"], STATED["rod_diameter_in"],
                             ASSUMED["youngs_modulus_psi"])
    plunger_stroke = STATED["surface_stroke_in"] - stretch
    capacity = estimate_production_bpd(STATED["pump_bore_in"], STATED["spm"],
                                       plunger_stroke, pump_efficiency=1.0)
    fillage = STATED["oil_bopd"] / capacity if capacity else float("nan")
    slow_spm = spm_for_full_pump(STATED["oil_bopd"], STATED["pump_bore_in"], plunger_stroke)
    return dict(label=label, fo=fo, stretch=stretch, plunger_stroke=plunger_stroke,
                capacity=capacity, fillage=fillage, slow_spm=slow_spm)


def main() -> None:
    print(__doc__.split("Run::")[0].strip())
    print("\n" + "=" * 72)
    print("STATED by the operator:")
    for k, v in STATED.items():
        print(f"  {k:24s} {v}")
    print("ASSUMED (tested below):")
    for k, v in ASSUMED.items():
        print(f"  {k:24s} {v}")
    print("=" * 72)

    base = report(ASSUMED["fluid_over_pump_ft"], ASSUMED["oil_specific_gravity"], "base")

    print("\nStep by step")
    print("-" * 72)
    print(f"  plunger area                 {plunger_area_sq_in(STATED['pump_bore_in']):8.3f} in^2")
    print(f"  rod area (3/4\")              {rod_area_sq_in(STATED['rod_diameter_in']):8.4f} in^2")
    print(f"  fluid load on plunger        {base['fo']:8.0f} lb")
    print(f"  elastic rod stretch          {base['stretch']:8.1f} in   <- stroke the plunger never sees")
    print(f"  plunger stroke               {base['plunger_stroke']:8.1f} in   (surface {STATED['surface_stroke_in']:.0f}\" minus stretch)")
    print(f"  pump capacity at 100% fill   {base['capacity']:8.1f} bbl/d at {STATED['spm']} SPM")
    print(f"  reported production          {STATED['oil_bopd']:8.1f} bbl/d")
    print(f"  => implied fillage           {base['fillage']*100:8.0f} %")

    print("\nThe number nobody gave him")
    print("-" * 72)
    print(f"  If inflow is the {STATED['oil_bopd']:.0f} bbl/d he already makes, the pump only needs")
    print(f"  {base['slow_spm']:.1f} SPM to lift it running full — against {STATED['spm']} SPM today.")
    print(f"  Same barrels. No pound. Roughly {(1 - base['slow_spm']/STATED['spm'])*100:.0f}% fewer cycles on the rods,")
    print("  gearbox and tubing per barrel.")

    print("\nSensitivity — the assumptions that could move this")
    print("-" * 72)
    print(f"  {'case':38s} {'stretch':>8} {'stroke':>8} {'fillage':>8} {'SPM':>6}")
    cases = [
        (4300.0, 0.85, "base: pumped off, 0.85 SG"),
        (3000.0, 0.85, "fluid 1300 ft above pump"),
        (2000.0, 0.85, "fluid 2300 ft above pump"),
        (4300.0, 0.78, "lighter oil, 0.78 SG"),
        (4300.0, 0.95, "heavier/some water, 0.95 SG"),
    ]
    for ft, sg, label in cases:
        r = report(ft, sg, label)
        print(f"  {label:38s} {r['stretch']:7.1f}\" {r['plunger_stroke']:7.1f}\" "
              f"{r['fillage']*100:7.0f}% {r['slow_spm']:6.1f}")

    print("\n  Across every case the conclusion is the same shape: the pump is")
    print("  roughly half full, and the unit is turning about half again to twice")
    print("  as fast as the well can feed it.")

    print("\nWhat would change the answer")
    print("-" * 72)
    for i, q in enumerate([
        "If the well can actually give more than 23 bbl/d, slowing down costs "
        "production — a fluid level shot settles that in one visit.",
        "If the August pump-off card is a different well from the 41\"/4300 ft/"
        "6.4 SPM one, none of this transfers.",
        "Overtravel is ignored, so plunger stroke here is the low end and the "
        "capacity is the low end with it. That makes the implied fillage the "
        "OPTIMISTIC case — real fillage is likely lower still.",
        "Gearbox rating is still unknown, so nothing here says whether the unit "
        "is inside its torque envelope at any speed.",
    ], start=1):
        print(f"  {i}. {q}")


if __name__ == "__main__":
    main()
