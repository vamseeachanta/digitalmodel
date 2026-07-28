# Data request — Collide "Dynamometer Discussions" well

What we would need to take this from *"why the undulations?"* (answered) to a
full pump diagnosis. Ordered by how much each item actually changes the answer,
not by how easy it is to ask for.

Everything already posted in the thread is listed at the bottom under
[Already have](#already-have) — no need to resend those.

---

## Already answered without any new data

These follow from stroke, depth, speed and the card alone, and none of them
move if the items below change:

| Result | Value |
|---|---|
| Rod-string natural frequency `No` | 58.3 SPM |
| Load-peak interval `dt = 60/No'` | 1.03 s |
| `N/No'` | 0.110 |
| Predicted undulations per half-stroke | 4.56 (4 clear humps observed) |
| String spring rate `Kr` | 271.7 lb/in |
| `Skr` | 11,141 lb |
| Rod weight in air `Wr` | 6,863 lb |
| PPRL / MPRL | 12,438 / 9,274 lb |
| Card area | 5,829 ft-lb/stroke |
| Polished-rod power | 1.13 hp |

**The undulations are normal.** At `N/No' = 0.11` a 4,200 ft string will always
ring roughly four times per half-stroke; the absence of undulations is what
would be surprising.

---

## Tier 1 — blocking. Without these, no diagnosis is defensible.

### 1. Rod string, as actually run

Confirm the string is 4,200 ft of 3/4 in and **nothing else** — specifically
whether there are sinker bars, weight bars, or a section of larger rod at the
top.

*Why it's first:* the measured minimum polished-rod load (9,274 lb) is
**2,412 lb heavier than the entire rod string weighs in air** (6,863 lb). That
is not possible mechanically. Friction acts upward on the downstroke, so it
lowers polished-rod load and widens a card — it cannot lift the whole card.
Only two explanations survive:

1. The load cell has a zero or scale offset.
2. The string is heavier than recorded.

Until this is settled, every absolute load number from this card is suspect.
Load *differences* (and therefore the undulation analysis) remain sound.

### 2. Load-cell make, model, and last calibration date

Directly tests explanation (1) above. If the cell reads a fixed offset, the
whole card shifts and the fix is arithmetic.

### 3. Runtime — hours per day the unit actually pumped

Was the unit running 24 h, or cycling on a pump-off controller / timer?

*Why it matters more than it looks:* a unit on a 50% duty cycle produces half
the fluid with a perfectly healthy pump. That is indistinguishable from 50%
fillage if you only look at daily volume. Any efficiency number we quote
without runtime is not a measurement, it's an assumption — so we report it as
undetermined rather than guess.

---

## Tier 2 — unlocks the calculated downhole card

This is the piece both commenters in the thread asked for, and the only way to
separate pump-off from gas interference from a worn pump.

### 4. Tubing ID and anchor status

Tubing size (2-3/8 in, 2-7/8 in …) and whether the tubing is **anchored**.
Unanchored tubing breathes with the rod string and changes both the card shape
and the effective plunger stroke.

### 5. Fluid level above the pump

From an acoustic shot if one exists. We currently assume the pump is submerged
to its setting depth, which is the pessimistic bound.

### 6. Oil gravity (°API) and formation volume factor `Bo`

Needed to convert surface barrels to reservoir barrels before any efficiency
claim.

### 7. Produced-fluid viscosity, or bottomhole temperature

Sets rod-string damping, which controls how much of the ringing survives down
to the pump.

> **Note on precision:** items 5 and 6 matter less than they appear. Sweeping
> specific gravity 0.80–0.90 and fluid level 3,000–4,300 ft moves theoretical
> displacement only from **38.3 to 41.7 bfpd** — about ±4%. Reasonable
> estimates are fine; we do not need exact numbers. Runtime (item 3) swings the
> answer far harder than either.

---

## Tier 3 — unlocks the gearbox / torque / power checks

### 8. Pumping unit API designation

The full nameplate, e.g. `C-228D-200-74`. The `C-66` mentioned in the thread is
an **Arrow Engine natural-gas engine** — the prime mover — not a pumping-unit
or gearbox rating, so it doesn't give us a torque limit.

### 9. Unit geometry dimensions and counterbalance

The API 11E dimension sheet for that unit (`A`, `C`, `I`, `K`, `P`, crank
radius, phase angle), plus counterbalance moment and structural imbalance.
Without these the torque calculation cannot run at all.

### 10. Motor nameplate

Horsepower, voltage, NEMA class — for the electrical/power side.

---

## Tier 4 — better data beats more data

### 11. The raw card file, not a screenshot

A `.dyn` export or a timestamped CSV of load and position, straight from the
controller.

*Why this is worth more than several items above:* time read off a
position-axis card is inherently uncertain, because polished-rod velocity goes
to zero at both stroke ends. The same ±1.5 in digitizing error costs:

| Peak position | Rod velocity | Time uncertainty |
|---:|---:|---:|
| 7.5 in | 10.62 in/s | ±0.14 s |
| 23.5 in | 13.59 in/s | ±0.11 s |
| 35.0 in | 9.71 in/s | ±0.15 s |

With uncertainties that size, the apparent peak-to-peak spacings of 1.24 s and
0.95 s are **not distinguishable from each other**, and both are consistent
with the predicted 1.03 s. A raw time-series file removes this entirely.

### 12. A downhole card from the controller, if it can produce one

Lets us check our calculated card against the controller's, which is the
fastest possible validation of both.

### 13. Several consecutive strokes, and a card from a different day

The posted card shows overlaid strokes ("Display all"). Stroke-to-stroke
variability distinguishes a steady condition from an intermittent one, and a
second date shows whether anything is trending.

---

## Already have

From the thread, no need to resend:

- Surface stroke 41 in · pump setting depth 4,300 ft · 6.4 SPM
- 1-1/4 in top-hold-down insert pump
- Tubing pressure building to 150 psi (Baird valve setpoint) · casing 25 psi
- Production 23 bbl oil, 0 water, previous day
- The surface card itself (digitized to ±40 lb / ±0.3 in)

---

## What we'd return

With Tier 1 alone: a defensible statement on whether the card is
self-consistent, and an honest production check.

With Tier 1 + 2: the calculated downhole pump card, pump fillage, and a
diagnosis separating pump-off, gas interference and mechanical wear — the
question the thread actually ended on.

With Tier 3: gearbox torque loading against the unit's rating, counterbalance
condition, and power consumption.
