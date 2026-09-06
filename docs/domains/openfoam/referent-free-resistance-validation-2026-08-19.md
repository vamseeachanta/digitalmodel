# What "validated" means for a hull with no published Ct

Design note for issue #2023,
answering the open design question it flags as genuinely unresolved. Written
against the code as it stands at `467d76d5` plus the additions described in
§7. Nothing here is a solver change and no solver was run to produce it.

---

## 0. The short answer

**A client resistance number cannot be validated. It can be verified, bounded
by a numerical uncertainty, tested for implausibility, and reported. That is
the whole of what is available, and it is worth having — but it is a different
claim, not a weaker version of the same claim.**

Verification and validation are not two tiers of the same measurement.
Verification asks *am I solving the equations right* and is answerable from the
simulation alone. Validation asks *am I solving the right equations* and is
answerable only against a measurement of the thing being modelled. Removing the
referent does not shrink the validation claim. It deletes it.

The corollary that matters commercially: the honest deliverable is
`Ct = X ± U_SN`, where `U_SN` is numerical uncertainty only, accompanied by an
explicit statement that modelling error is **not** in the band and is not
bounded by anything in the report. Presenting that number with a pass stamp
would be manufacturing a grade.

---

## 1. What survives the loss of the referent, item by item

| Element | Needs a referent? | Survives? |
|---|---|---|
| Iterative convergence (ITTC 7.5-03-01-01 oscillatory criterion) | no | **yes** |
| Grid convergence / Richardson / GCI | no | **yes**, and it becomes the load-bearing evidence |
| Round-off | no | yes in principle; **not measured today** — see §2.3 |
| `Ct = Cp + Cv` identity | no | yes, but it is arithmetic, not evidence |
| V3 two-level self-consistency | no | yes, as machinery; its *constants* are KCS-sized |
| V2b (Cv vs ITTC-57) | no — ITTC-57 is a correlation line | **transfers, but mis-centred** — see §3.2 |
| V1 (Ct vs measured Ct) | **yes** | no. Becomes a prediction. |
| V2a (Cp vs measured Cr) | **yes** | no. Becomes a prediction. |
| Validation uncertainty `U_V` | **yes** (needs `U_D`) | no. There is no `U_D` without an experiment. |

Two things follow immediately.

First, **the strongest referent-free statement is a grid study**, and the
committed KCS study cannot make it. Two levels give a Richardson estimate only
under an *assumed* order of accuracy. With two levels the convergence ratio
`R = ε21/ε32` cannot be formed at all, so a 5.58% level-to-level difference is
equally consistent with slow monotonic convergence and with an oscillation
whose amplitude happens to be 5.58%. Roache's factor of safety rises from 1.25
to 3.0 to pay for that, which is the price of the assumption rather than a
refinement of it. Run through the referent-free scorer, the committed pair
yields `U_G = 5.66e-4`, **16.8% of Ct**, with `order_is_assumed: true`.

Second, **the loss is asymmetric with what a client wants**. V1 and V2a are the
criteria a client would recognise as "is the number right", and they are
exactly the two that vanish.

---

## 2. Verification without a referent

### 2.1 Iterative convergence

ITTC 7.5-03-01-01 estimates iterative uncertainty from the amplitude of the
residual oscillation in the quantity of interest, taken over a whole number of
periods: `U_I = ½(S_U − S_L)`.

This is available on any hull. But it has a precondition the committed KCS
artifact does not meet. `kcs-calm-water-resistance-verification.json` reports
`"averaging_window": 2` — the production mean is over **two force samples**, at
iterations 22500 and 25000, and the reported `iterative_scatter_ct` of 9.38e-7
is half the gap between two numbers, not a standard deviation. Two points define
a difference, not an oscillation.

**The averaging window is therefore a stated parameter with a derived minimum**,
and the derivation is pre-committed rather than fitted:

- Sampling a periodic signal `n` times per period recovers at worst `cos(π/n)`
  of its true amplitude, because the sampled extremum sits up to half a sample
  interval from the true one. The sampled half-range therefore under-estimates
  `U_I` by up to `1 − cos(π/n)`.
- Declare a budget for that deficit: **5%**. That fixes `n = 10`
  (`1 − cos(π/10) = 4.89%`; at `n = 9` it is 6.03% and misses).
- ITTC takes the extrema over a whole number of periods, and one period cannot
  show the extrema have stopped drifting. Declare **2 periods**.
- Minimum window = `10 × 2 = 20 samples`.

Nothing in that chain was tuned against a result. It does fail the committed KCS
run, at n=2, which is evidence of its independence rather than of its
convenience. Below 3 samples there is no statistic at all — the mean has no
residual degrees of freedom — and the implementation refuses to offer one.

The three-state verdict (`INADEQUATE` / `MARGINAL` / `ADEQUATE`) is emitted as a
first-class field, and an inadequate window makes the run **unreportable**: not
wrong, but not yet quotable with a band.

### 2.2 Grid convergence

Standard ITTC / Roache procedure, with the classification honest about what each
number of levels can support:

- **Three levels**: `R = ε21/ε32` classifies the sequence. `0 < R < 1` monotonic
  → the observed order `p` is *measured*, `δ_RE = ε21/(r^p − 1)`,
  `U_G = 1.25·|δ_RE|`. `−1 < R < 0` oscillatory → Richardson does not apply and
  the uncertainty is bounded by the half-range of the levels. `R > 1` divergent
  → **no estimate exists**, and that is a result, not a missing measurement.
- **Two levels**: no classification, assumed order, `Fs = 3.0`, flagged
  `order_is_assumed: true`.
- **One level**: not a grid study. No band may be quoted.

A refinement ratio below Roache's recommended 1.3 is flagged, because below it
the level-to-level difference is not cleanly separated from the iterative noise
it is meant to be measured against.

**Recommendation: a client engagement should be priced for three grid levels,
not two.** This is the single largest change to the honest cost of the work, and
it is the one item on this page that buys a materially stronger claim.

### 2.3 Round-off

Round-off survives the loss of the referent in principle: it is measured by
re-running at a different precision. In practice this repository has not
measured it, and the correct treatment is to say so. The implementation lists
round-off explicitly under `uncertainty.excludes` with the reason ("assumed
negligible in double precision and has not been measured here") rather than
silently folding it into a zero. An unstated assumption reads as an
absent error source.

---

## 3. Physical plausibility bands

Bands are the weakest tier on this page and the easiest to abuse. The
implementation tags every check with the *kind* of statement it is, because they
are not comparable:

| Tier | What it is | Verdicts |
|---|---|---|
| `identity` | arithmetic (`Ct = Cp + Cv`) | `holds` / `violated` |
| `sign` | follows from the definitions plus "displacement hull, towed, steady, calm water" | `holds` / `violated` |
| `band` | engineering judgement over a hull population | `implausible` / `not_implausible` / `not_applicable` |
| `reported` | a number with no band, because no defensible band exists | `reported` |

**The word "validated" never appears in this block, and neither does "passed".**
A number inside a band has not been confirmed by anything; it has failed to
contradict a weak expectation. That asymmetry is the entire content of the tier
and collapsing it into a pass/fail would be the manufactured criterion this
design exists to avoid. The precedent is in this repo already:
`scripts/cfd/yplus_after.sh` made y+ a reported diagnostic rather than a gate,
and says why — *a criterion invented after looking at the answer is not a
criterion*. Plausibility bands gate nothing here for the same reason.

### 3.1 The bands, and their provenance

- **Sign checks.** `Cp > 0`, `Cv > 0` (either would be thrust), and
  `Ct − Cf(ITTC-57) > 0` — a total below the flat-plate line at the same
  Reynolds number leaves no residuary resistance, which no wave-making hull can
  do. These are cheap and hard to argue with.
- **Implied form factor**, `(1 + k) = Cv / Cf(ITTC-57)`, banded to
  **[1.05, 1.45]**. Provenance: **engineering judgement, deliberately widened**
  around the neighbourhood conventional displacement hulls are commonly
  reported in by the Prohaska low-speed method. This repository holds no primary
  source for the interval, so it is quoted wider than the literature range on
  both sides. A band quoted tighter than its evidence is a fabricated tolerance.
- **Residuary fraction**, `(Ct − Cf)/Ct`, **reported with no band at all**. It is
  strongly Froude-dependent and there is no Froude-conditioned source in the
  repo. Inventing a wide band here would have looked like rigour and delivered
  none.

The band declines outside its declared hull class rather than guessing — a
planing hull or a multihull gets `not_applicable`, not a stretched interval.
That is the same discipline §4 demands of Holtrop & Mennen, applied to our own
band so the standard is not one we only impose on other people's methods.

### 3.2 What the form factor says about V2b — a real finding

V2b scores `Cv` against `Cf(ITTC-57)` at ±5%. Written out, that is the assertion
`(1 + k) ∈ [0.95, 1.05]`, i.e. **a form factor of zero**.

For KCS that is the right referent, and only for a specific reason: the
publishing workshop reduced its own experimental data with the ITTC-57 line, so
matching that line is what makes the comparison like-for-like. As a *referent-free*
criterion it is centred on a physically wrong value, because a conventional
displacement hull has `k ≈ 0.1–0.35`.

Two consequences:

1. Porting V2b to a client hull unchanged would carry a mis-centred criterion.
   The referent-free check must be a band on `(1 + k)`, not on `Cf`.
2. Re-centring makes the committed KCS result **worse, not better**. The run
   gives `Cv/Cf = 0.913` — an implied form factor of **−8.7%**: the hull is
   computed to generate *less* viscous resistance than the equivalent flat
   plate. That is implausible on any conventional displacement hull, with no
   benchmark needed to say so. It is the identical finding V2b makes at −8.70%,
   recovered from first principles, which is a useful cross-check that the
   referent-free path has not lost the signal — and confirmation that #2023's
   warning holds: **this defect follows the code onto a client hull unchanged.**

---

## 4. Empirical cross-check — corroboration, not validation

Holtrop & Mennen predicts resistance from principal dimensions and is the
obvious independent comparison for a hull with no benchmark. Three constraints
apply, and they are not negotiable:

1. **Agreement is corroboration, not validation.** H&M is a regression over a
   ship population and carries its own several-percent scatter. Two methods
   agreeing tells you they are not grossly wrong in *different* ways; it does not
   locate either against reality. Reported as a screening band, it catches gross
   errors cheaply, which is genuinely worth having. Reported as validation, it is
   a category error.
2. **The applicability envelope is load-bearing.** H&M has a stated envelope in
   `Cb`, `L/B`, `B/T` and Froude number. A hull outside it makes the comparison
   meaningless in *either* direction — agreement and disagreement are equally
   uninformative — so the envelope check must be arithmetic and must run before
   the comparison, not after.
3. **It is unavailable today, and the manifest says so.**
   Issue #2020 records
   that this repository's H&M returns a `Ct` that is near-identical for a
   Series 60 and a tanker — two hulls differing by 68% in waterline length and
   33% in block coefficient agree to 4 parts in 10⁵ — and that issue is blocked
   on obtaining the primary papers.

**The implementation does not import `holtrop_mennen` and does not reimplement
it.** Wiring in a half-remembered formula would produce a number that agrees or
disagrees for no traceable reason, and a fabricated corroboration is worse than
an absent one. The manifest emits
`cross_check: {status: "unavailable", blocked_on: "#2020"}`. Silence would read
as *not applicable*; the field says *blocked*, which is a different and true
statement.

---

## 5. What is reported rather than gated

The manifest deliberately has **no `criteria` key and no `all_passed` key**.
Those belong to the KCS path, where there is something to have passed. In their
place:

- **`validation`** — `available: false`, with the reason stated in the artifact
  itself rather than in a covering email.
- **`admissibility`** — three conditions, none of which is a statement about
  accuracy: the decomposition must sum, the averaging window must be an average,
  and the numerical uncertainty must be estimable. This gates the *reporting*,
  not the *correctness*. Failing it means the run is not quotable yet; passing
  it says **nothing at all** about whether the answer is right.
- **`plausibility`** — reported, never gated (§3).
- **`uncertainty`** — `U_I`, `U_G`, `U_SN = √(U_I² + U_G²)`, plus an explicit
  `excludes` list: turbulence-model form error, wall-function error, free-surface
  modelling error, geometric idealisation, fixed attitude, unmeasured round-off.
- **`cannot_establish`** — an explicit list, in the artifact, of the claims this
  scheme does not support. Repeated in §8 below.

The distinction between *admissibility* and *validation* is the one piece of
vocabulary this design adds, and it is worth stating plainly: **admissibility is
about whether a number may be quoted with a band; validation is about whether
the number is right. Passing the first has no bearing on the second.**

---

## 6. The normalisation-area inversion

`ship_resistance._assert_normalisation_area` refuses any area within 5e-3 of the
**generated** 9.5609 m², because for KCS the published 9.4379 m² is the only
admissible normalisation — a coefficient is defined by what it is divided by,
and scoring against the workshop's Ct requires the workshop's S.

For a hull with no publication that guard is exactly inverted: **the
mesh-derived area is the only area there is.** The discipline #1173 built has to
be re-derived rather than reused.

The referent-free path therefore takes a `NormalisationArea(value, provenance,
source)` rather than a bare float, with provenance constrained to
`published | mesh_derived | declared` and a non-empty citation. An area cannot
enter the calculation without the statement of where it came from, because the
coefficient it produces is uninterpretable without it — and because any
comparison of that coefficient with any other number is meaningless unless the
other number used the same area.

The KCS guard stays live on the KCS path. A regression test asserts it still
refuses the generated area there, so dropping it in one place does not quietly
drop it in both.

---

## 7. Implementation map

| Concern | Location |
|---|---|
| Referent-free scorer | `src/digitalmodel/solvers/openfoam/validation/referent_free_resistance.py` |
| Tests | `tests/solvers/openfoam/validation/test_referent_free_resistance.py` |
| Shared force-row reader (one implementation of the iteration-span window) | `ship_resistance.read_force_rows` / `ForceRow` |
| KCS path — unchanged behaviour | `ship_resistance.evaluate_ship_resistance_run` |

Entry point:

```python
evaluate_referent_free_run(force_dat, config, companions=[...], mesh_cells=N)
```

It is **additive**. It never calls `load_referent`, never reads the KCS fixture,
and never runs `_assert_normalisation_area` — all three asserted by test, one of
them twice, because monkeypatching an attribute does not catch a top-level
`from … import` and the cheaper check has a known hole.

### The committed KCS run, scored with no referent

| Output | Value |
|---|---|
| averaging window | `INADEQUATE`, 2 samples against a stated minimum of 20 |
| grid convergence | `indeterminate`, r = 1.412, `order_is_assumed: true`, `U_G` = 5.66e-4 (**16.8% of Ct**) |
| implied form factor | 0.913 → **implausible** |
| sign checks | all hold |
| admissible | **false** — the mean is not a converged average |
| reported result | no band may be quoted |

Every failure the benchmark path finds is recovered without the benchmark,
except the two that are definitionally unavailable (V1, V2a).

---

## 8. What this scheme cannot establish

Stated here and emitted in every manifest, so it travels with the number rather
than with the covering note:

1. **The accuracy of Ct.** With no measurement of this hull, the difference
   between the computed and the true value is not observable at any grid density.
2. **Modelling (model-form) error.** Turbulence closure, wall treatment and
   free-surface method are not verifiable against themselves. Refining the mesh
   converges the answer toward the *model's* answer, not toward the truth.
3. **A validation uncertainty `U_V`.** It requires an experimental uncertainty
   `U_D`, and there is no experiment.
4. **That agreement with an empirical method constitutes validation.**
5. **That a coefficient inside a plausibility band is correct.** A band can only
   fail to contradict.
6. **Transfer of the KCS validation to a client hull**, unless the regime — block
   coefficient, Froude number, Reynolds number, appendage state, attitude — is
   stated and checked arithmetically. Regime transfer is a claim about
   similarity, and an unstated one is an assumption. #2023 identifies this as the
   part most likely to be skipped and most likely to cause harm; nothing in this
   design makes it safe to skip.

And one prerequisite that sits outside this design entirely: per #2023, the KCS
gate currently fails on all four criteria. **A referent-free scoring mode does
not license a client number.** It formats an honest one. What licenses the
method is a KCS result that passes — and closing V2b matters twice over, because
§3.2 shows the same defect reappears on a client hull as a negative form factor.

---

*Design note for #2023. Related: #1173 (the KCS capability), #2020 (Holtrop &
Mennen, blocked on primary sources).*
