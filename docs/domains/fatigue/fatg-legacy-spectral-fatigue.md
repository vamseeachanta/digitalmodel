# FATG — legacy closed-form spectral fatigue (validation lineage)

## What this is

`digitalmodel.fatigue.fatg` and the routed workflow basename
`fatg_spectral_fatigue` are a port of a legacy in-house Fortran
spectral-fatigue program ("FATG", 1989–1991 era, several successive
variants) used for plate structures and tubular jack-up legs. The method
is the classic closed-form narrow-band chain:

```
stress RAO  ×  Bretschneider wave spectrum  ×  wave scatter table
            ×  directional spreading        ×  one-slope DNV S-N
            →  Miner damage per year
```

## Method, as the legacy code actually computes it

1. **Significant stress response** per sea state: trapezoidal integration
   of `RAO(ω)² · S_B(ω)` on the RAO's own frequency grid (the legacy deck
   was a fixed 18 points). The legacy integration constants (1400, 700,
   with a factor 2 inside the square root) are algebraically identical to
   a standard Bretschneider spectrum
   `S(ω) = (5/16) Hs² ωp⁴ ω⁻⁵ exp(−1.25 (ωp/ω)⁴)` with
   `ωp = 560^0.25 / Tz` (i.e. `Tp ≈ 1.2916·Tz`), the result being the
   significant stress **range** `4·√m0`. (The source comments quote
   `Tp = Tz/0.71` and echo `Tp = 1.3·Tz`; the integration constants above
   are what the code computes. This equivalence is asserted to 1e-12 in
   the tests.)
2. **Three-block Rayleigh discretisation**: the stress-range distribution
   in each sea state collapses to three constant-amplitude blocks at
   `(1.271, 0.59, 0.28) ×` significant range, each carrying 1/3 of the
   cycles of its heading group. Cycle counts use the zero-upcrossing
   period: `n = T·occurrence/Tz` with `T = 31 536 000 s/yr`.
3. **Directional spreading**: cycles are split over 0°/±45°/90° heading
   groups by a fixed spreading matrix applied to input percentages
   `(p0, p45, p90)`, with per-heading stress reduction factors. Presets:
   - `general` (original program): factors `(1, cos 45°, cos 78.75°)` —
     the 90° factor is "theoretically 0 but that would not be
     conservative" per the source comments;
   - `chord` (later 3-chord-leg variant): `(1, 1, cos 78.75°)`;
   - `diagonal` (diagonals/horizontals): `(1, 1, 1)`.
   With no spreading input, the original program's uniform mode is
   reproduced: every heading group carries the full `T·occ/(3·Tz)` count
   per block (deliberately conservative — each wave is counted in all
   three heading groups).
4. **S-N + Miner**: one-slope curve `log10 N = log10_a − m·log10 S`,
   `m = 3`, with the legacy DNV constants in English units (stress in
   psi): E = 18.499, F = 18.2845, T = 18.64786. The curve — hard-coded in
   the first program version, selectable in the last — is fully
   selectable here (named legacy constants or any `log10_a`/`slope`,
   which also makes the chain metric if a metric intercept is supplied).

## Relationship to the modern spectral-fatigue modules

`spectral_fatigue` / `rao_spectral_fatigue` build a stress PSD on a
generated spectral grid and apply closed-form damage estimators (Dirlik,
narrow-band with the Γ-function Rayleigh closed form, etc.). FATG differs
in every stage that matters for reproducing legacy results: Tz-parameterised
Bretschneider evaluated on the RAO's own ω-grid, significant-response
trapezoid, discrete 3-block Rayleigh Miner roll-up, and the fixed
0°/45°/90° spreading matrix. The port therefore reproduces the legacy
arithmetic exactly rather than re-expressing it through the modern chain.

## Validation

No executable or original input/output decks survived with the source, so
the source of truth is a **line-by-line Python transliteration of the
Fortran arithmetic** (Fortran `REAL*8` ≡ IEEE float64), kept inside
`tests/fatigue/test_fatg.py` together with frozen golden values for a
fully synthetic 18-point RAO + 3-row scatter deck:

- library vs transliteration and vs frozen goldens: **rel 1e-9**
  (the only systematic difference is the legacy truncated
  `PI = 3.141592654` inside the direction cosines, ~3e-10 relative);
- FATG spectrum kernel vs standard Bretschneider form: **rel 1e-12**
  (algebraic identity);
- sweeps over all member presets × S-N curves, both spreading modes,
  plus slope-3 scaling and Hs-linearity property checks.

Gap: validation against an output deck produced by the compiled legacy
binary is still outstanding (no Fortran compiler on the porting machine);
the transliteration oracle is the interim source of truth. Re-running
`gfortran -std=legacy` on the original source against the synthetic deck
in `tests/fatigue/test_fatg.py` would close this.

## Usage

```yaml
basename: fatg_spectral_fatigue
fatg_spectral_fatigue:
  design_life_years: 20.0
  dff: 1.0
  sn_curve: F                # E | F | T, or {log10_a: ..., slope: 3.0}
  member_type: chord         # general | chord | diagonal
  rao:
    omega_rad_per_s: [0.2, 0.3, ...]          # strictly increasing
    stress_per_wave_height: [120.0, 260.0, ...]  # psi per unit wave height
  sea_states:
    - hs: 3.0
      tz: 5.0
      occurrence_fraction: 0.55
      spreading: {p0: 0.6, p45: 0.3, p90: 0.1}   # omit for uniform mode
  output_dir: results
```

Outputs: per-sea-state CSV (Hs, Tz, Tp echo, significant stress range,
damage/year), summary CSV, and the legacy report echoes
(`life_used_percent_1yr`, `life_used_percent_10yr`) alongside the modern
`fatigue_life_years` / `margin` / `screening_status` fields.
