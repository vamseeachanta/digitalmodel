# DYNMOOR — legacy dynamic mooring analysis (static + passing-ship port)

## What this is

`digitalmodel.subsea.mooring_analysis.dynmoor` and the routed workflow
basename `dynmoor_mooring` are a port of a legacy in-house Fortran mooring
program ("DYNMOOR", v4.10 1986 → v6.10 March 2007, single-file Fortran 77).
The full legacy program is a 3-DOF (surge/sway/yaw) nonlinear time-domain
simulation of a moored vessel under wind, current, waves, wave drift and
passing-ship excitation: up to 30 mooring lines of up to 3 mixed-material
segments (chain/wire/nylon), fenders, broken-line simulation, and
average / significant / 1/10- / 1/100-highest excursion and tension
statistics.

**This port covers the complete static chain and both passing-ship force
models.** The time-domain integrator is deliberately not yet ported (see
"Gaps" below).

## Method, as the legacy code actually computes it

1. **Load-excursion tables** (`CATINB`/`FULCAT`/`CATNRY` + `STRETC`/`SUB`):
   each line type collapses to a 50-row table of horizontal tension, total
   tension, fairlead–anchor distance, anchor pulls and grounded length,
   sampled at `H = BS_min·((j−1)/49)²` (the pile/bollard `FULCAT` table
   tops out at ⅔·BS via `49/0.8165`). Three models:
   - single segment → drag anchor on a (possibly sloped) bottom, with
     bottom friction and elastic stretch (`CATINB`);
   - single segment → elevated pile/bollard, full catenary (`FULCAT`);
   - three segments with buoy/clump (`CATNRY`, Newton iteration on the
     non-dimensional fairlead slope, tolerance `|ΔXI/XI| < 5e-4`).
   Legacy derived constants preserved: weight-in-water factors 0.87
   (chain) / 0.795 (wire) / 1.0 (nylon), `EA = A·E·1000`, buoy `WB·1000`,
   clump `WC·870`, nonlinear-spring quadratic load–stretch (including the
   `RSP = SK2/2SK1` vs `SK1/2SK2` inconsistency between subroutines).
2. **Pretension setup**: `PT = TI%·BS_min·10` lb looked up on the total-
   tension column (3-point Lagrange quadratic), giving anchor coordinates
   and the fairlead line angle.
3. **Static restoring force** (`RFORCE`): per-line horizontal/total
   tensions from table interpolation (`INTERP`, quadratic with linear
   foot), body-frame surge/sway forces and yaw moment at any (x, y, ψ)
   offset; broken-line zeroing supported.
4. **Fenders** (`FENDER`): piecewise-linear 4-point force-deflection per
   fender, engagement side convention (ISIDE).
5. **Static wind** (`WINDAF`/`WCOEF`): per-area 100-knot force
   `0.0338·Ch·Cs·A` kips, height-coefficient step table, resolved as
   `−(F_front·cosθ, F_lat·sinθ, M_yaw·sinθ)·V²/10` lb.
6. **Passing ship**:
   - *parallel* (`PARSHIP`, Wang 1975 slender-body): double Simpson
     integration (fixed 50 panels/hull) over parabolic sectional-area
     distributions, finite-depth image sources `n = −10..10`. Legacy
     quirks preserved bit-for-bit: `PI = 3.1415279` (source typo) and the
     implicit-INTEGER passing length (truncated to whole feet; Simpson
     step from integer division).
   - *perpendicular* (`PERSHIP`, v6.00 Nov 2006): regression force
     coefficients `C_X/C_Y/C_M(ΔR, T/d, S/L)` and piecewise
     quartic/quintic stagger shape functions fitted to U.S. Naval Academy
     model data (D. Kriebel 2005/2006).
   - `passing_ship_sweep` reproduces the legacy time-loop wrapper
     (stagger `x/L = DIR·((i−ISTAR)·SOG − 2L)/L`, force cutoff at
     `|x/L| > 2`, knots→ft/s 1.6888/1.6889, separation guards).

Units: legacy English set (ft, lb/kips, knots, degrees clockwise from
bow); `g = 32.174 ft/s²`, `ρ = 1.99 slug/ft³` (1.9905 in `PARSHIP`,
62.4 lb/ft³ in `PERSHIP`).

## Validation

Two independent oracles (see
`tests/subsea/mooring_analysis/test_dynmoor.py`):

1. **The original compiled binary.** `DynmoorMult.exe` (v6.10, Compaq
   Visual Fortran Win32) still runs; it was executed on four fully
   synthetic decks (round-number barge/ship and line parameters invented
   for validation) and the printed `.OUT`/`.MOR`/`.PAS` values were frozen
   as goldens:
   - single-segment `CATINB` catenary table + pretension/anchor summary,
   - 3-segment `CATNRY` table (chain–wire–wire + buoy),
   - parallel and perpendicular passing-ship force histories
     (237 samples each).
   Tolerances: `0.011 + 6e-5·|v|` (print rounding + the binary's REAL*4
   single precision); the `CATNRY` distance columns use `0.011 + 6e-4·|v|`
   because the Newton stopping test `|ΔXI/XI| < 5e-4` makes the accepted
   root precision-dependent — a float32 re-run of the port's loop
   reproduces the binary's numbers, isolating the difference to precision.
   Passing-ship histories: `0.06 + 1e-4·|v|` (F10.1 rounding).
2. **Line-by-line transliteration oracles** for `CATINB`, `PARSHIP` and
   `PERSHIP` (original variable names and operation order) — library vs
   oracle at rel 1e-12.

Plus property tests: table monotonicity, pretension round-trip, symmetric-
pattern equilibrium, broken-line zeroing, Wang surge/yaw antisymmetry and
V² scaling, the integer-length quirk, fender breakpoint continuity and
side convention, WCOEF steps, separation-guard errors.

## Intentional deviations from the legacy code

- float64 throughout (legacy binary is REAL*4).
- Off-table lookups clamp to the last table row; the legacy code indexes
  one row past the populated table (zero-initialised memory).
- Pretensions below the second table row raise instead of dereferencing
  row 0; `WCOEF` at exactly 50 ft returns 1.0 (legacy leaves it
  undefined).
- The fixed-format `.IN` deck reader is not reproduced; the workflow takes
  YAML (schema in `digitalmodel/dynmoor_mooring/workflow.py`, mirroring
  the legacy "Feeder Sheet" input generator's fields).

## Gaps (follow-up: TODO(dynmoor-time-domain))

Not ported yet: the `OSCILL` semi-implicit time integrator, random-sea
Fourier synthesis (P-M / 3×JONSWAP / user spectrum / regular wave),
Morison hull panel loads (`AGEN`/`WFCE`/`AFORCE`/`WAVEL`), the four
wave-drift options (API / L.M. Harris / read-in / reflection-coefficient
+ semi option), wind gusting, pitch option, and the peak statistics
(`SIG`). A skipped test skeleton for these lives at the bottom of
`test_dynmoor.py`. A `time_domain` key in the workflow config raises
`NotImplementedError` pointing here.

## Usage

```yaml
basename: dynmoor_mooring
dynmoor_mooring:
  vessel: {lbp_ft: 600.0, beam_ft: 100.0, draft_ft: 30.0,
           displacement_kips: 30000.0, water_depth_ft: 50.0}
  line_types:
    - segments:
        - {material: chain, length_ft: 1000.0, weight_air_lb_ft: 60.0,
           area_in2: 10.0, modulus_ksi: 3000.0,
           breaking_strength_kips: 500.0}
      anchor: {weight_kips: 10.0, mud_efficiency: 1.0, sand_efficiency: 1.5}
      vertical_distance_anchor_to_fairlead_ft: 50.0
      bottom_friction_percent: 2.0
  lines:
    - {type: 1, fairlead_x_ft: 300.0, fairlead_y_ft: -50.0,
       angle_deg: 300.0, pretension_percent: 15.0}
  offsets:
    - {surge_ft: 10.0, sway_ft: 5.0, yaw_deg: 1.0}
  passing_ship:
    mode: parallel
    nsteps: 300
    start_step: 10
    direction: -1.0
    speed_knots: 6.0
    separation_ft: 300.0
    passing_length_ft: 650.0
    passing_beam_ft: 100.0
    moored_midship_area_ft2: 3000.0
    passing_midship_area_ft2: 3200.0
  output_dir: results
```

Outputs: per-type 50-row catenary-table CSV, line pretension/anchor
summary CSV, restoring forces (+ optional fender/wind terms) per offset,
and the passing-ship force history CSV (the legacy `.PAS` file).
