# Outreach — DRAFTS ONLY

**Nothing here has been sent.** Each needs Vamsee's explicit approval.

**Rule for every draft in this file: lead with what we can do for them.** No origin story,
no methodology, no history of our own mistakes beyond the one line that buys credibility.
Operators read these between well visits. If it doesn't fit on a phone screen, cut it.
Target 120 words. The attached PDF carries the detail — that is what the PDF is for.

---

## Routing

| | Reed Goodman | Neal Turluck |
|---|---|---|
| Company | Jaybird Resources, Cuero TX | S & S Oil and Gas, Illinois Basin |
| **Email** | **Address needed** | `nealturluck@gmail.com` |
| Basis | Public posts only; no address published | He published it himself |
| Attach | `AceEngineer-dynacard-review-matrix.pdf` | `AceEngineer-small-operator-field-note.pdf` |
| Never send | — | The matrix — it is Reed's data |

Two constraints. **Reed has published no email address**, so his draft below needs one
before it can be sent — ask in a one-line Collide DM, or send the same body as a DM.
Guessing an address at Jaybird is not on. And **the matrix is Reed-only**: it quotes his
posts and names his equipment.

Artifact links are **private by default**; a recipient clicking one gets nothing.
**Attach the PDF.**

---

## 1 — Reed Goodman · Email

**To:** *address needed — none published.* Ask for it in a one-line Collide DM, or send
this as a DM as-is (the body works either way).
**Attach:** `AceEngineer-dynacard-review-matrix.pdf`

> **Subject:** Three things I can do with your cards
>
> Reed,
>
> Went back through all your card posts. One page attached: what each card shows and what
> I'd need to take it further.
>
> Three things I can do, no charge:
>
> - **The 5-minute pump-off.** Send the raw card file and you get fillage stroke by
>   stroke plus a shutdown setpoint, with the barrels it costs stated separately. Send a
>   full-pump card too — a full pump reads 88% on this scale, not 100%, so "shut down at
>   75%" means something different than it sounds. That calibration comes first.
> - **The bottom-left noise.** Tell me whether the dyno is accelerometer-based and I'll
>   tell you whether you're looking at a real pump tag or at integration drift.
> - **The tubing splits.** Send the depths of the last few. If they track fluid pound,
>   then fixing the pump-off fixes the tubing, and rotators are treating a symptom.
>
> Two on our side. Your pump-off cards found a real bug in our fillage calculation — past
> a certain severity it reported a pounded-off pump as full, which is exactly the regime
> your wells live in. Fixed, with a test. And we got the gearbox wrong in July: you'd
> written that the C-66 was the prime mover and we should have read that.
>
> Vamsee Achanta
> Achanta AceEngineer Inc.

*179 words. Every bullet is an offer; the closing paragraph is the only thing that isn't,
and it is what makes the offers credible.*

### 1b — Optional, separate email
Send **only after** checking the comments on "Tally app needed" — confirm the bounty is unclaimed.

> **Subject:** Tally app — still open?
>
> Is the $500 tally app still open? Offline scan to Excel with manual correction for bad
> digits. I'd rather build it than bid on it — you decide afterwards if it was worth anything.

---

## 2 — Neal Turluck · Email

**To:** `nealturluck@gmail.com` · **Attach:** `AceEngineer-small-operator-field-note.pdf`

> **Subject:** Which five of your 152 wells to drive to this week
>
> Neal,
>
> Nobody holds 152 wellbores in their head. That's the part I can help with.
>
> Send whatever you already collect — cards, casing pressures, run times, a spreadsheet.
> You get back one ordered list: which five wells to drive to this week, and why. First
> pass free, nothing to install, no telemetry.
>
> Second thing if it's useful: whether the workover programme really carries +30 BOPD,
> run well by well with the plugging cost carried in. That's the number a lender asks for
> and rarely gets.
>
> To be clear — I'm not a capital source and I'm not brokering one.
>
> Vamsee Achanta
> Achanta AceEngineer Inc.

*Word count: 116. The pooled-spares proposal was cut — it is our idea, not his problem,
and it belongs in a reply if he engages, not in a first contact.*

---

## 3 — General template

Personalise line one with something they actually posted. **No quote, no send.**
**Attach:** `AceEngineer-small-operator-field-note.pdf`

> **Subject:** One card, one answer
>
> [Their own words, quoted.]
>
> Send one well's dynamometer card file and you get back the downhole card, fillage
> stroke by stroke, the diagnosis with a confidence number, and which parts of the card
> aren't trustworthy enough to diagnose on.
>
> No account, no telemetry, no charge for the first well.
>
> Vamsee Achanta
> Achanta AceEngineer Inc.

---

## Approval checklist

- [ ] Reed — get an email address, or send the body as a DM
- [ ] Reed — approve the email
- [ ] Reed — check "Tally app needed" comments before sending 1b
- [ ] Neal — approve the email
- [ ] PDFs attached, not private artifact links
- [ ] We can turn a card file round quickly if two people say yes at once

## Resolved

The corner-detection defect is **fixed** (2026-08-05), so Reed's email now says "found and
fixed" rather than "found". Sweep is monotonic 88.1% to 11.9%; the four vendor-analysed
reference wells still agree to within 0.85 fillage points; 1132 tests pass. Nothing is
being held for it.
