# Grounding — what this program can and cannot defend

Written 2026-08-09, after the decision to **stop outreach and post nothing** until
the work is grounded in fundamentals.

The first grounding check found a problem in our own output, so this document
starts there rather than with a reading list.

---

## Finding 1 — we reimplemented, worse, a module this repo already had

`src/digitalmodel/marine_ops/artificial_lift/dynacard/rod_pump` implements
**API RP 11L** and is documented at `docs/domains/artificial_lift/rod-pump-rp11l.md`.
Its quick-start example uses the *same well* the analysis scripts here were
written for.

The scripts in `examples/` hand-rolled Mills-era closed-form approximations
beside it. Comparison on the identical inputs:

| | hand-rolled script | RP 11L module | why they differ |
|---|---|---|---|
| Fluid load | 1,942 lb | 2,096 lb | **the script omitted the tubing-pressure term** (150 psi × 1.227 in² = 184 lb) |
| Rod stretch | 7.39 in | ~7.7 in | consequence of the above |
| Plunger stroke | 33.61 in | 33.29 in | consequence of the above |
| Pump displacement | 39.19 bpd | 38.80 bpd | consequence of the above |
| **Volumetric efficiency** | **0.587 — asserted** | **`None` — refused** | see finding 2 |

The displacement error is small. The habit is not: **check whether the domain
module exists before writing the physics again.** A hand-rolled version does not
inherit the validation, the envelope checks, or the refusals that the real module
carries.

---

## Finding 2 — we asserted a number the repo deliberately refuses to assert

`rod_pump.analyse()` returns `volumetric_efficiency = None` on purpose. The
module documentation states exactly why:

> *A unit cycling on a pump-off controller has a duty cycle that looks exactly
> like low fillage, and surface barrels are not reservoir barrels. Rather than
> assume 24 h and Bo = 1.0, `volumetric_efficiency` returns `None`.*

We published **"~59% fillage"** as a headline figure — in the analysis scripts,
in `HANDOFF.md`, and in a drafted public reply. To get 59% you must assume:

1. **24-hour runtime.** Plausible here (the operator stated no controller), but
   an assumption, and the module refuses to make it.
2. **Bo = 1.0.** Almost certainly wrong. 23 *stock-tank* barrels are not 23
   *reservoir* barrels at the pump. Even a modest Bo of 1.05–1.15 moves the
   efficiency by 5–15 percentage points on its own.

The same documentation spells out the neighbouring trap we had already walked
into once — surface stroke gives 48%, plunger stroke gives 59%, and **neither is
an efficiency until runtime and Bo are known.**

**Consequence:** the drafted public reply led with a number our own fundamentals
module declines to state. Had it gone out, that would have been the third public
correction on this program, and the first one that was avoidable by reading our
own repo.

---

## What is actually defensible today

| Claim | Status |
|---|---|
| Pump displacement ≈ 38.8 bpd at 6.4 SPM | **Defensible** — RP 11L, module-computed |
| Rod stretch shortens a 41 in stroke to ~33.3 in | **Defensible** — and it is the correction we were publicly given in July |
| Rod Goodman utilisation ~51% | **Screening-grade** — Mills α and API Modified Goodman, not a wave-equation result |
| Card fillage from a *card* | **Defensible** — the solver measures it; the corner-detection defect is fixed and regression-tested |
| Volumetric efficiency from *production* | **NOT defensible** without runtime and Bo |
| "~4 SPM would lift the same barrels" | **NOT defensible** — it rests on the efficiency figure above and on 23 bbl/d being inflow-limited, which needs a fluid-level shot |
| Compression-zone depth prediction | **Unvalidated** — first-principles impedance, never checked against a real failure record |

---

## The real gap: we have no data in the regime we claim to serve

The corner-detection defect survived because the only real validation cards are
**four vendor-analysed wells spanning 88–98% fillage**. The solver-parity test
says so itself:

> *these fixtures span 88.37–97.92% vendor fillage. No trustworthy severe
> fluid-pound card is available here.*

Every claim about severe pump-off — the regime this whole program is aimed at —
rests on synthetic generators. That is why the bug reported a pounded-off pump as
full and no test caught it, and it is the single largest thing to fix before any
client work.

---

## Grounding programme, in order

1. **Route the analysis scripts through `rod_pump`** instead of hand-rolled
   closed forms, and stop reporting efficiency where the module refuses to.
2. **Read the primary sources properly** and record what each does and does not
   license us to say: Gibbs (1963) wave equation; API RP 11L and its envelope;
   Everitt–Jennings SPE 18189, already implemented here; Lea & Rowlan on fluid
   level and pump-off control.
3. **Acquire real severe-pump-off cards.** Public state datasets, published
   papers with digitised cards, or a vendor with permission. Until this exists,
   the pump-off claims are physics without evidence.
4. **Validate the compression-zone model** against a real tubing-failure depth
   record before it is offered to anyone.
5. **Close the screening-vs-solver gap:** where a closed form and the solver both
   apply, run both and record where they diverge and why.

Only after 1–4 does outreach resume, and then with a result rather than an offer.

---

## The rule this program keeps relearning

Both public corrections and both self-inflicted errors have the same shape:
**an assumption stated as a finding.** The gearbox rating inferred from an engine
designation. Fillage computed on surface stroke. Efficiency computed without
runtime or Bo. A compression depth computed but never validated.

The repo's own module already models the correct behaviour — it returns `None`
and lists what it would need. That is the standard to hold everything else to.
