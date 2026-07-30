# Rod-pump surface-card analysis (API RP 11L)

`digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump`

Kinematics and rod-string mechanics for a rod-pumped well: natural frequency,
card undulations, crank motion, plunger stroke, pump displacement, and the
dimensionless groups the API RP 11L correlations run on.

This sits **upstream** of the surface-to-downhole card transform. For that, use
the sibling [`everitt_jennings`](../../../src/digitalmodel/marine_ops/artificial_lift/dynacard/everitt_jennings/)
package, which implements the SPE 18189 space-marching finite-difference
solver.

## Quick start

```python
from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import analyse

result = analyse(
    rod_diameter_in=0.75,
    rod_length_ft=4200.0,
    surface_stroke_in=41.0,
    strokes_per_minute=6.4,
    plunger_diameter_in=1.25,
    fluid_level_ft=4300.0,
    specific_gravity=0.85,
    tubing_pressure_psi=150.0,
    casing_pressure_psi=25.0,
)

result.natural_frequency_spm          # 58.33  (No = 245,000 / L)
result.peak_interval_s                # 1.03   (dt = 60 / No')
result.undulations_per_half_stroke    # 4.56
result.plunger_stroke_in              # 33.29
result.pump_displacement_bpd          # 38.80
result.volumetric_efficiency          # None -- runtime and Bo unknown
```

## Three things this module is opinionated about

### 1. Pump displacement uses **plunger** stroke, not surface stroke

Rod stretch under fluid load shortens the plunger's travel. On a 4,200 ft
3/4 in string carrying 2,096 lb of fluid load, the stretch is 7.7 in — so a
41 in surface stroke drives only a 33.3 in plunger stroke.

Using the surface stroke gives PD = 47.8 bfpd; using the plunger stroke gives
38.8 bfpd. Against 23 bbl/day measured, that is the difference between a
reported 48% and 59% efficiency — before runtime is even considered.

### 2. Efficiency is `None` until runtime and `Bo` are known

A unit cycling on a pump-off controller has a duty cycle that looks exactly
like low fillage, and surface barrels are not reservoir barrels. Rather than
assume 24 h and `Bo = 1.0`, `volumetric_efficiency` returns `None` and the
analysis lists what it needs under `result.undetermined`.

### 3. It fails closed outside the RP 11L envelope

| `N/No'` | Behaviour |
|---|---|
| < 0.15 | Wave-dominated card, many undulations. Valid, flagged as expected. |
| 0.15 – 0.35 | Typical RP 11L design range. Valid. |
| > 0.35 | Resonance. **Raises `ValidityError`** — use a wave-equation solver. |

Also refused: tapered strings without an explicit `Fc`; Mark II, RotaFlex and
hydraulic long-stroke geometries passed to the Class I correlations; non-steel
sonic velocity used with the steel-derived 245,000 constant (caught by the
cross-check that `245,000/L` and `15c/L` agree within 0.5%).

## Peak spacing is not peak phase

The interval between load peaks is `60/No'` — a property of the rod string
alone. It is identical on upstroke and downstroke and identical at every
pumping speed.

What changes with speed is the **phase**. Upstroke ringing is triggered at
bottom of stroke (`t = 0`); downstroke ringing at top of stroke (`t = 30/N`).
Overlay several speeds on one time axis and the upstroke peaks align while the
downstroke peaks separate. `peak_times()` returns both trains with the correct
phase reference, and `divergence_onset()` gives the time at which overlaid
traces begin to disagree — the earliest top of stroke, set by the fastest unit.

## Timing read off a position-axis card is uncertain

`dt = dx / v`, and polished-rod velocity vanishes at both stroke ends. The same
digitizing error therefore buys far more time error near the top of the stroke:

| Peak position | Rod velocity | Time error from ±1.5 in |
|---:|---:|---:|
| 7.5 in | 10.62 in/s | ±0.14 s |
| 23.5 in | 13.59 in/s | ±0.11 s |
| 35.0 in | 9.71 in/s | ±0.15 s |

So `time_from_card_position()` returns a `Measurement(value, uncertainty)`
pair, not a bare float, and `intervals_are_distinguishable()` gates the
reporting path. On a card digitized to ±1.5 in, consecutive peak-to-peak
intervals of 1.24 s and 0.95 s are **not** resolvable — the honest statement is
the mean interval against the predicted 1.03 s. Prefer raw time-series input
(`.dyn`, timestamped CSV) whenever it exists.

## The load datum check

`load_datum_check()` flags a minimum polished-rod load above the rod string's
weight in air. This has no mechanical explanation: friction acts *upward* on
the downstroke, so it lowers polished-rod load and widens a card — it cannot
lift the whole card.

On the Collide fixture the measured MPRL of 9,274 lb exceeds the 6,863 lb air
weight by 2,411 lb. The two candidate causes are a load-cell zero or scale
offset, and a rod string heavier than recorded. Load *differences* remain
usable while absolute loads do not, so this surfaces as a warning in
`result.warnings` rather than failing the analysis.

## Validation fixtures

Both live in `tests/marine_ops/artificial_lift/dynacard/test_rod_pump.py`.

**Reed Goodman's well** — Collide "Dynamometer Discussions", July 2026. 4,200 ft
of 3/4 in steel, 41 in stroke, 6.4 SPM, 1.25 in plunger at 4,300 ft. Digitized
card at `docs/collide_pe/dynacard-undulations/`. Four clear load undulations
observed on the upstroke against 4.56 predicted.

**Rowlan validation case** — O. Lynn Rowlan (Echometer), *"Over Travel Occurs on
Both the Upstroke and Down Stroke"*, Sucker Rod Pumping Workshop / SWPSC,
slide 13. One well and one rod string at three speeds, which isolates what is
invariant with speed from what is not. The slide is internally inconsistent on
the middle speed — 5.22 SPM on the card panel, 5.44 SPM in both time-plot
legends — so the card fixtures use 5.22 and the time-domain fixtures 5.44.

## Two open items

**The load-datum discrepancy is unresolved.** On Reed's well, MPRL of 9,274 lb
exceeds the reported string's air weight of 6,863 lb. Whether that is a
load-cell offset or a string heavier than recorded has not been answered by the
operator. Until it is, absolute loads from that card should not be trusted;
load differences remain usable. The check surfaces this as a warning rather
than failing the analysis, precisely because the analysis is still useful.

**The Rowlan card-width reading is an inference.** The slide labels its arrows
57.9 / 71.1 / 77.9 in but does not state what they measure. Reading them as
plunger travel along the `Wrf + Fo Max` reference line follows Rowlan's own
reference-load-line method, but that reading is ours, not the source's. The
regression test therefore asserts only monotonicity with pumping speed — an
over-travel proxy that holds under any consistent interpretation — rather than
treating the numbers as absolute measurements.

## Note on the prime mover

The `C-66` on Reed's well is an **Arrow Engine Co. natural-gas engine** (single
cylinder, 13 hp continuous at 700 rpm) — a prime mover, not a gearbox rating.
Gearbox torque checks take the pumping *unit's* rating as an explicit input,
defaulting to `None` and skipping the check rather than inferring one from the
engine designation.

## Knowledge-side references

- `vamseeachanta/llm-wiki` — `wikis/drilling-engineering/wiki/concepts/rod-string-natural-frequency-and-card-undulations.md`
- `vamseeachanta/llm-wiki` — `wikis/drilling-engineering/wiki/sources/rowlan-2016-srpw-overtravel-load-peak-timing.md`

## Standards

- API RP 11L — Design Calculations for Sucker Rod Pumping Systems
- API 11E — Specification for Pumping Units
- Everitt, T.A. and Jennings, J.W., SPE 18189 — the downhole-card transform
  (implemented in the `everitt_jennings` package)
