#!/usr/bin/env python
# ABOUTME: Rod stress, compression zone depth, and cycle count from stated well data
# ABOUTME: Predicts WHERE tubing wear should appear, so the operator can falsify it

"""
Rod loading, the compression zone, and why the splits move
==========================================================

Follow-on to ``pumpoff_spm_from_stated_data.py``. Same publicly stated well,
no card file, three questions that the thread has not touched:

1. **Are the rods over-stressed?** If they are, the fix is metallurgy or a
   bigger string. If they are not, repeated tubing damage is a *side-load*
   problem, not a tension problem — which points somewhere completely
   different.

2. **How much of the string goes into compression when the pump pounds, and
   where does that zone sit?** This produces a falsifiable prediction: the
   depths at which tubing wear should cluster. The operator can check it
   against his own failure records in ten minutes, and it is worth publishing
   precisely because he can prove it wrong.

3. **Does the casing pressure support gas interference at all?** He asked
   whether the noise on his card could be gas decompressing. He also posted a
   casing pressure. Those two facts constrain each other.

Everything is screening-grade closed form — Mills acceleration factor and API
Modified Goodman, not a wave-equation solution. Labelled as such throughout,
because the difference matters when someone acts on it.

Run::

    PYTHONPATH=src python docs/small_operator/examples/rod_loading_and_neutral_point.py
"""

from __future__ import annotations

import math

# --- STATED by the operator (July thread, verbatim) ------------------------
SURFACE_STROKE_IN = 41.0
PUMP_DEPTH_FT = 4300.0
SPM = 6.4
ROD_DIA_IN = 0.75
ROD_LEN_FT = 4200.0
PUMP_BORE_IN = 1.25
OIL_BOPD = 23.0
CASING_PSI = 25.0
BAIRD_VALVE_PSI = 150.0

# --- ASSUMED ---------------------------------------------------------------
OIL_SG = 0.85
ROD_WT_LB_PER_FT = 1.63     # 3/4 in API steel sucker rod, nominal
ROD_GRADE_TENSILE_PSI = 115_000.0   # Grade D minimum tensile
STEEL_SG = 7.85
SLOW_SPM = 4.0              # from the companion script

ROD_AREA = math.pi / 4 * ROD_DIA_IN ** 2
PLUNGER_AREA = math.pi / 4 * PUMP_BORE_IN ** 2


def fluid_load_lb() -> float:
    return 0.433 * OIL_SG * PUMP_DEPTH_FT * PLUNGER_AREA


def buoyancy_factor() -> float:
    return 1.0 - OIL_SG / STEEL_SG


def acceleration_factor(spm: float) -> float:
    """Mills acceleration factor, alpha = S N^2 / 70500."""
    return SURFACE_STROKE_IN * spm ** 2 / 70_500.0


def rod_loads(spm: float) -> dict:
    """Peak and minimum polished-rod load, screening-grade."""
    w_air = ROD_WT_LB_PER_FT * ROD_LEN_FT
    w_buoyant = w_air * buoyancy_factor()
    alpha = acceleration_factor(spm)
    pprl = w_buoyant + fluid_load_lb() + w_air * alpha
    mprl = w_buoyant - w_air * alpha
    return dict(w_air=w_air, w_buoyant=w_buoyant, alpha=alpha, pprl=pprl, mprl=mprl)


def goodman(spm: float) -> dict:
    """API Modified Goodman utilisation at the top of the string.

    SA = (T/4 + 0.5625 * Smin) * SF, with service factor 1.0 for
    non-corrosive. The top rod carries the whole string, so it governs.
    """
    L = rod_loads(spm)
    s_max = L["pprl"] / ROD_AREA
    s_min = L["mprl"] / ROD_AREA
    s_allow = (ROD_GRADE_TENSILE_PSI / 4.0 + 0.5625 * s_min) * 1.0
    return dict(s_max=s_max, s_min=s_min, s_range=s_max - s_min,
                s_allow=s_allow, utilisation=s_max / s_allow)


ROD_ACOUSTIC_VELOCITY_IN_S = 16_850.0 * 12.0   # steel sucker rod, ~16,850 ft/s
STEEL_DENSITY_LB_S2_IN4 = 0.283 / 386.4        # 0.283 lb/in^3 over g


def impact_velocity_in_s(fillage: float, plunger_stroke_in: float, spm: float) -> float:
    """Plunger velocity at the instant it meets fluid.

    The plunger does not free-fall; it follows the unit. Treating the motion as
    simple harmonic, position measured down from the top of the stroke is
    ``x = S/2 (1 - cos t)``, so meeting fluid after falling a fraction
    ``(1 - fillage)`` of the stroke fixes the crank angle:

        cos t = 2 * fillage - 1

    and the velocity there is ``v_max * sin t``.

    This has a consequence that is not obvious and matters here: velocity peaks
    at mid-stroke, so **the hardest pound is at roughly 50% fillage**, not at
    the lowest fillage. A nearly empty barrel meets fluid late in the stroke
    where the plunger has already slowed down.
    """
    v_max = math.pi * plunger_stroke_in * spm / 60.0
    cos_t = max(-1.0, min(1.0, 2.0 * fillage - 1.0))
    return v_max * math.sqrt(1.0 - cos_t ** 2)


def pound_compression_lb(fillage: float, plunger_stroke_in: float, spm: float) -> float:
    """Compressive load driven into the bottom of the string by the pound.

    NOT a static load transfer. Once the travelling valve opens, static
    equilibrium leaves the rod at the pump in zero tension — the string simply
    hangs. Compression is purely dynamic: the string arrives with momentum, is
    stopped by the fluid, and the arrest propagates as a stress wave.

    One-dimensional elastic impact gives the stress directly from the acoustic
    impedance, ``sigma = rho * c * v``, with no fitted constant.
    """
    v = impact_velocity_in_s(fillage, plunger_stroke_in, spm)
    stress_psi = STEEL_DENSITY_LB_S2_IN4 * ROD_ACOUSTIC_VELOCITY_IN_S * v
    return stress_psi * ROD_AREA


def compression_zone_ft(compressive_load_lb: float) -> float:
    """Length of rod held in compression above the pump.

    The compressive wave is absorbed by the buoyant weight of the rod above
    it, so ``Lc = Fc / w_buoyant_per_ft``. Sucker rod is far too slender to
    carry compression, so every foot of that zone buckles into contact with
    the tubing — which makes the zone the depth interval over which
    rod-on-tubing wear is expected.
    """
    w_per_ft = ROD_WT_LB_PER_FT * buoyancy_factor()
    return compressive_load_lb / w_per_ft


def main() -> None:
    print(__doc__.split("Run::")[0].strip())
    fo = fluid_load_lb()

    # --- 1. rod stress -----------------------------------------------------
    print("\n" + "=" * 72)
    print("1. Are the rods over-stressed?")
    print("=" * 72)
    for spm, tag in ((SPM, "as run today"), (SLOW_SPM, "slowed")):
        L, G = rod_loads(spm), goodman(spm)
        print(f"\n  {spm:.1f} SPM ({tag})")
        print(f"    rod weight in air        {L['w_air']:8.0f} lb")
        print(f"    buoyant weight           {L['w_buoyant']:8.0f} lb   (BF {buoyancy_factor():.4f})")
        print(f"    acceleration factor      {L['alpha']:8.4f}")
        print(f"    peak polished rod load   {L['pprl']:8.0f} lb")
        print(f"    min  polished rod load   {L['mprl']:8.0f} lb")
        print(f"    max stress               {G['s_max']:8.0f} psi")
        print(f"    Goodman allowable     {G['s_allow']:8.0f} psi")
        print(f"    utilisation              {G['utilisation']*100:8.0f} %")

    g_now = goodman(SPM)
    verdict = ("NOT the problem — the string has margin"
               if g_now["utilisation"] < 0.9 else "AT OR OVER the limit")
    print(f"\n  Verdict: rod tension is {verdict}.")
    print("  So repeated tubing damage on this well is a SIDE-LOAD story,")
    print("  not an overload story. Which leads to question 2.")

    # --- 2. compression zone ----------------------------------------------
    print("\n" + "=" * 72)
    print("2. Where should the tubing wear be?")
    print("=" * 72)
    plunger_stroke = 33.6   # from the companion script
    print(f"\n  Buoyant rod weight per foot: "
          f"{ROD_WT_LB_PER_FT * buoyancy_factor():.3f} lb/ft")
    print(f"  Plunger stroke used:         {plunger_stroke:.1f} in "
          f"(from the companion script)")
    print(f"\n  {'fillage':>8} {'impact vel':>12} {'compression':>12} "
          f"{'zone above pump':>17} {'neutral point':>15}")
    print(f"  {'':>8} {'in/s':>12} {'lb':>12} {'ft':>17} {'ft depth':>15}")
    rows = []
    for fillage in (0.95, 0.80, 0.59, 0.50, 0.40, 0.20):
        v = impact_velocity_in_s(fillage, plunger_stroke, SPM)
        fc = pound_compression_lb(fillage, plunger_stroke, SPM)
        Lc = compression_zone_ft(fc)
        neutral_ft = PUMP_DEPTH_FT - Lc
        rows.append((fillage, v, fc, Lc, neutral_ft))
        print(f"  {fillage*100:7.0f}% {v:12.1f} {fc:12.0f} {Lc:17.0f} {neutral_ft:15.0f}")

    at_59 = [r for r in rows if abs(r[0] - 0.59) < 1e-9][0]
    worst = max(rows, key=lambda r: r[3])
    print(f"\n  At the ~59% fillage implied by his own production, the bottom")
    print(f"  {at_59[3]:.0f} ft of rod is driven into compression — everything below")
    print(f"  about {at_59[4]:.0f} ft. PREDICTION: tubing wear should cluster in the")
    print("  lower string, not spread evenly up the hole.")
    print(f"\n  Note where the worst case sits: {worst[0]*100:.0f}% fillage, not the")
    print("  emptiest barrel. Impact velocity peaks at mid-stroke, so a pump")
    print("  that is half full pounds HARDER than one that is nearly empty —")
    print("  the nearly empty one meets fluid late, after the plunger has")
    print("  already slowed. This well is sitting close to the worst of it.")
    print("\n  And because the zone length tracks fillage, the neutral point")
    print("  MOVES as fillage changes through the day. That is what 'never on")
    print("  the same joint' looks like from the inside.")
    print("\n  FALSIFIABLE: if his splits are evenly distributed, or cluster in")
    print("  the top third, this mechanism is wrong and we say so.")

    # --- 3. gas or not -----------------------------------------------------
    print("\n" + "=" * 72)
    print("3. Can the card noise be gas?")
    print("=" * 72)
    print(f"\n  Casing pressure posted            {CASING_PSI:6.0f} psi")
    print(f"  Baird (casing) valve set at       {BAIRD_VALVE_PSI:6.0f} psi")
    print(f"  Produced water                    {'none stated':>11}")
    print(f"\n  The casing sits {BAIRD_VALVE_PSI - CASING_PSI:.0f} psi BELOW the valve setting, so the")
    print("  annulus is not building gas — the valve is not passing because there")
    print("  is little to pass. A well with enough free gas at the pump to put a")
    print("  decompression signature on the card would normally be building")
    print("  casing pressure against that valve. This one is not.")
    print("\n  Reading: gas interference is a weak explanation for the noise on")
    print("  this well. Fluid pound and instrument artefact are the strong ones.")
    print("  That agrees with the specialist in the thread, who placed gas")
    print("  expansion at the TOP left of the card rather than the bottom left.")

    # --- 4. cycles ---------------------------------------------------------
    print("\n" + "=" * 72)
    print("4. What the slow-down is worth in wear")
    print("=" * 72)
    for spm, tag in ((SPM, "today"), (SLOW_SPM, "slowed")):
        per_day = spm * 60 * 24
        print(f"  {spm:.1f} SPM ({tag:>6}): {per_day:>9,.0f} cycles/day "
              f"{per_day*365/1e6:>6.1f} million/yr")
    saved = (1 - SLOW_SPM / SPM)
    print(f"\n  {saved*100:.0f}% fewer load reversals per barrel lifted, and each")
    print("  surviving reversal is gentler because the barrel is full.")
    print("  Fatigue damage accumulates per cycle, so this is the cheapest")
    print("  life extension available on the well — it costs a setpoint.")


if __name__ == "__main__":
    main()
