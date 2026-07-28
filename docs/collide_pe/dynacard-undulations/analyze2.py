"""Revision 2 — incorporates data added to the Collide thread after the first pass.

New inputs
  Reed Goodman:  "4200' of 3/4" steel rods"          -> untapered string, Fc = 1.0
  Walter Phillips: peaks must be EQUALLY spaced in time; digitizing a card
                   cannot resolve time reliably. Slide (O. Lynn Rowlan, 2016 SRPW,
                   "Over Travel Occurs on Up and Down Stroke", #13):
                   Natural frequency adjusted for taper = 45.88 SPM
                   Elapsed time between repeating load peaks = 1.31 s
  Research:      C-66 = Arrow Engine Co. C-66, single-cylinder natural-gas engine,
                 13 hp continuous @ 700 rpm. A PRIME MOVER, not a gearbox rating.
"""
import numpy as np

# ------------------------------------------------------------------ inputs
S, N, D_PUMP = 41.0, 6.4, 4300.0          # in, SPM, ft
L_ROD, D_PL = 4200.0, 1.25                # ft of 3/4" steel rod, in plunger
Q_OIL, P_TBG, P_CSG = 23.0, 150.0, 25.0   # bopd, psi, psi

# 3/4 in API sucker rod
W_ROD_FT = 1.634                          # lb/ft incl. couplings (API Spec 11B)
A_ROD = np.pi / 4 * 0.75**2               # in^2
E_STEEL = 3.1e7                           # psi (value used by API RP 11L)
Er = 12.0 / (A_ROD * E_STEEL)             # in per lb per ft

print("=" * 72)
print("1. ROD STRING (new data: 4200 ft of 3/4 in steel, single size)")
print("=" * 72)
Wr = L_ROD * W_ROD_FT
Kr = 1.0 / (Er * L_ROD)                   # lb/in, string spring rate
Skr = S * Kr
print(f"rod area A_r          = {A_ROD:.4f} in^2")
print(f"elastic constant Er   = {Er:.3e} in/lb/ft   (12/(A_r E), E=3.1e7 psi)")
print(f"rod weight in air Wr  = {Wr:,.0f} lb        ({W_ROD_FT} lb/ft x {L_ROD:,.0f} ft)")
print(f"string spring rate Kr = {Kr:.1f} lb/in")
print(f"Skr = S*Kr            = {Skr:,.0f} lb")

print()
print("=" * 72)
print("2. NATURAL FREQUENCY — the whole point of the thread")
print("=" * 72)
for label, L in (("rod length 4200 ft", L_ROD), ("pump depth 4300 ft", D_PUMP)):
    No = 245_000 / L
    print(f"{label:22s}: No = 245,000/L = {No:6.2f} SPM  ->  60/No = {60/No:.3f} s")
print("string is a SINGLE size -> taper factor Fc = 1.000 -> No' = No (no correction)")
No = 245_000 / L_ROD
T_peak = 60 / No
print(f"\npredicted peak-to-peak time = {T_peak:.3f} s   (rod length basis)")
print(f"N/No' = {N/No:.4f}   -> undulations per half-stroke = 0.5/(N/No') = {0.5/(N/No):.2f}")

# cross-check the method against Rowlan's slide
No_slide, T_slide = 45.88, 1.31
print(f"\ncheck vs Rowlan 2016 SRPW slide #13:")
print(f"  slide states No' (taper-adjusted) = {No_slide} SPM, measured peak spacing = {T_slide} s")
print(f"  60/{No_slide} = {60/No_slide:.4f} s  vs measured {T_slide} s  -> "
      f"{abs(60/No_slide - T_slide)/T_slide*100:.1f}% — the 60/No' rule is confirmed")

print()
print("=" * 72)
print("3. WHY DIGITIZING CANNOT SETTLE THE TIMING (Walter's correction)")
print("=" * 72)
w = 2 * np.pi * N / 60
print(f"peaks read off the card at 7.5 / 23.5 / 35.0 in -> spacings 1.24 s and 0.95 s")
print("Walter is right that these must be EQUAL. Error budget on a broad hump:")
for x_pk in (7.5, 23.5, 35.0):
    v = S / 2 * w * np.sin(np.arccos(np.clip(1 - 2 * x_pk / S, -1, 1)))
    print(f"  at {x_pk:5.1f} in: rod velocity {v:5.2f} in/s -> "
          f"+/-1.5 in peak-location error = +/-{1.5/v:.2f} s of time error")
print("  => +/-0.15 s per peak, i.e. the same size as the 1.24 vs 0.95 spread.")
print(f"  mean of the two intervals = 1.10 s vs predicted {T_peak:.2f} s "
      f"({abs(1.10-T_peak)/T_peak*100:.0f}% high) — that is the only defensible comparison.")
print("  Simple-harmonic crank motion was also assumed; a real Class I")
print("  crank-pitman-beam geometry is asymmetric and adds several % more.")

print()
print("=" * 72)
print("4. LOAD DATUM — the card does not sit where this rod string says it should")
print("=" * 72)
SG_OIL = 0.85                             # ~35 deg API
A_pl = np.pi / 4 * D_PL**2
grad = 0.433 * SG_OIL
Fo = grad * D_PUMP * A_pl + (P_TBG - P_CSG) * A_pl
Wrf = Wr * (1 - 0.128 * SG_OIL)           # buoyant rod weight
print(f"plunger area          = {A_pl:.4f} in^2")
print(f"fluid load Fo         = {Fo:,.0f} lb   (0.433*{SG_OIL} psi/ft x {D_PUMP:,.0f} ft"
      f" + {P_TBG-P_CSG:.0f} psi wellhead dP, on the plunger)")
print(f"buoyant rod weight    = {Wrf:,.0f} lb")
print(f"Fo/Skr                = {Fo/Skr:.3f}")
print(f"\nstatic bracket:  MPRL ~ Wrf - dynamic = below {Wrf:,.0f} lb")
print(f"                 PPRL ~ Wrf + Fo + dynamic = around {Wrf+Fo:,.0f} lb")
PPRL_OBS, MPRL_OBS = 12_438.0, 9_274.0
print(f"\nMEASURED (digitized): PPRL {PPRL_OBS:,.0f} lb, MPRL {MPRL_OBS:,.0f} lb, "
      f"range {PPRL_OBS-MPRL_OBS:,.0f} lb")
print(f"PREDICTED range Fo + dynamic ~ {Fo:,.0f}-{Fo*1.6:,.0f} lb  -> range AGREES")
print(f"\n*** MPRL {MPRL_OBS:,.0f} lb EXCEEDS the rod weight in AIR ({Wr:,.0f} lb) "
      f"by {MPRL_OBS-Wr:,.0f} lb ***")
print("Friction cannot explain this: on the downstroke friction acts upward and")
print("LOWERS the polished-rod load, so it makes the gap worse, not better.")
print("The card's shape and range are right; its zero is not. Two candidates:")
print("  (a) load-cell zero/scale offset of roughly +4,000 lb")
print("  (b) the string is heavier than reported. Required air weight to put")
print(f"      MPRL just under it: ~{MPRL_OBS/L_ROD:.2f} lb/ft over {L_ROD:,.0f} ft, i.e.")
for size, wft in (("3/4 in", 1.634), ("7/8 in", 2.224), ("1 in", 2.904)):
    print(f"        {size}: {wft} lb/ft -> {wft*L_ROD:,.0f} lb air weight")

print()
print("=" * 72)
print("5. PLUNGER STROKE AND FILLAGE — first pass was too crude")
print("=" * 72)
stretch = Fo / Kr
print(f"rod stretch under fluid load = Fo/Kr = {stretch:.1f} in")
Sp_lo, Sp_hi = S - stretch, S - stretch + 2.0     # + modest overtravel at low N/No'
print(f"plunger stroke Sp ~ S - stretch (+ overtravel) = {Sp_lo:.1f} to {Sp_hi:.1f} in")
PD_surface = 0.1484 * A_pl * S * N
for Sp in (Sp_lo, Sp_hi):
    print(f"  PD at Sp={Sp:4.1f} in : {0.1484*A_pl*Sp*N:5.1f} bfpd")
print(f"  (first pass used the SURFACE stroke -> {PD_surface:.1f} bfpd, an over-estimate)")
PD_mid = 0.1484 * A_pl * (Sp_lo + Sp_hi) / 2 * N
print(f"\nagainst 23 bopd STOCK-TANK oil:")
print(f"  raw ratio          = {Q_OIL/PD_mid*100:.0f} %")
for Bo in (1.10, 1.25):
    print(f"  x Bo={Bo:.2f} (downhole) = {Q_OIL*Bo/PD_mid*100:.0f} %")
print("  ...and this assumes 24 h runtime. On a pump-off controller the unit")
print("  cycles; at 70% runtime the same 23 bopd implies ~100% fillage.")
print("  => cannot claim partial fillage without runtime + Bo. Walter said he")
print("     does NOT expect partial fillage; nothing here contradicts that.")

print()
print("=" * 72)
print("6. PRIME MOVER — correction to the first pass")
print("=" * 72)
area = 69_950.0                            # lb-in, digitized card area
hp_pr = area / 12 * N / 33_000
print(f"polished-rod power = {hp_pr:.2f} hp (from digitized card area)")
print("C-66 = Arrow Engine Co. C-66: single-cylinder natural-gas engine,")
print("13 hp continuous @ 700 rpm max. It is the PRIME MOVER.")
print(f"  -> {hp_pr:.2f} hp at the polished rod against 13 hp installed:")
print(f"     load factor ~{hp_pr/13*100:.0f}% before gearbox/belt losses and counterbalance.")
print("  The first pass wrongly read C-66 as a 57,000 in-lb GEARBOX rating. Withdrawn.")
