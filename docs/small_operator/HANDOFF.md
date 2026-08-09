# HANDOFF — small-operator program (2026-08-05 → 08)

Exit snapshot. Read the **Do not repeat** section before touching outreach.

**Split completed 2026-08-09.** This directory is now the **generic, public
half**: physics, reusable analysis, anonymised brochure. Operators appear as
**OP-A** / **OP-B**; community members are described by role. Nothing here names
a person, company, address or phone number.

**Client identities and client-specific deliverables are in the private
`aceengineer-strategy` repo**, `pipeline/small-operator-outreach/` — the
pseudonym key, the per-client card review, and `DO-NOT-CONTACT.md`. Read that
before contacting anyone.

Merged to `main` by **squash**, deliberately: the branch's intermediate commits
carried contact details, and squashing means they never reach `main` at all.

---

## What this program is

Solve problems that individual small / marginal operators actually voiced, one
at a time, and route each solution into the ecosystem rather than answering it
once. Sibling of `docs/collide_pe/`, which solves the PE-Problem-of-the-Day quiz
series; this one solves operators' real field problems.

Two named operators drove it, both from Collide: **OP-A** (a Gulf Coast operator
Resources, Gulf Coast) and **OP-B** (an Illinois Basin operator, Illinois Basin).

---

## Do not repeat — the expensive lesson

**OP-B replied "Please stop emailing me" on 2026-08-05.** Two unsolicited
emails in seven days. He is on `aceengineer-strategy/pipeline/DO-NOT-CONTACT.md`
permanently and channel-wide — no email, no Collide DM, no apology note.

What actually went wrong was not the copy:

1. We publicly committed to *a month of value creation on our own time*, then
   followed up **without any value in the follow-up**. The Indiana wellbore list
   — the thing worth receiving — was never built.
2. The draft was a no-ask value delivery opening "No reply needed". The version
   sent replaced that with "coming up with ideas to see how I may be able to
   help" and closed with "Please let me know your thoughts". That converts a
   gift into a second unprompted ask, a week after an unanswered first one.

**Rule that came out of it:** if a follow-up is justified only by "we promised
value", it must *contain* the value and ask for nothing. If the value is not
built, the follow-up is not due.

**Channel finding, and it is the important one.** Email is not working for this
segment. OP-A has now had **five emails and replied to none**, while posting on
Collide most days and answering other people in his own threads within hours —
and he gave us his address *by Collide DM* rather than by writing to us. The
motion that has actually produced engagement every single time is **answering
technical questions in public**. Scale that, not the inbox.

---

## Shipped

### digitalmodel — branch `feat/small-operator-program`, 6 commits, NOT pushed

| Commit | What |
|---|---|
| `5090e5ab` | **fix(dynacard)** — reject a bottom-right corner with no load transferred |
| `7032fb64` | **feat(production-engineering)** — marginal well economics carrying P&A liability |
| `8c82cb41` | docs — problem catalogue, client deliverables, outreach drafts |
| `16facb4b` | docs — AceEngineer wordmark, tally-app offer dropped |
| `8911f9cb` | docs — pump-off severity and slow-down SPM from stated data |
| `653f0598` | docs — rod stress, pound compression zone, falsifiable wear depth |

**1132 tests pass.** Branch is 6 ahead / 7 behind `origin/main` — rebase before
pushing.

**The dynacard defect** is the most important thing here. `calculate_corners`
scored the BR corner with load normalised against the whole card's span, giving
the top of the downstroke a ~0.18 head start before any load had transferred. On
severe pump-off the genuine knee lost, net stroke collapsed onto gross, and a
**fully pounded-off pump reported ~100% fillage** — silently, and toward
"healthy". A controller setpoint driven off it would never trip. Fixed by
rejecting a BR corner unless ≥35% of the downstroke's load drop has occurred
there; threshold measured (vendor wells realise 0.675–0.866, the failure 0.000).
Vendor agreement preserved within 0.85 fillage points.

**A rejected fix is recorded in `problems.yml`** — normalising over the
downstroke's own span. Cleaner-looking, fixed the pounded case, and broke vendor
well 4 by 17.5 fillage points because rod undershoot puts the downstroke load
minimum well past the transfer point. Someone will try it again; the record is
there so they stop sooner.

### aceengineer-website — pushed and live

`https://www.aceengineer.com/outreach/small-operator-brief.html` — verified 200,
in the sitemap, linked from `/outreach/`. Built on the existing
`vessel-contractor-brochure` pattern (same hero/pillar/niches/cta classes, same
partials), so **further briefs are content-only, not a new layout each time**.

Deploy is **Vercel**, not GitHub Pages — push to `main` triggers Production.

### aceengineer-strategy — 2 commits, NOT pushed, 4 behind

Opt-out record and `pipeline/DO-NOT-CONTACT.md`.

---

## Analysis available to use

All from figures the operator posted publicly. No card file needed, which is
what makes it postable — he can check it with a calculator.

| Finding | Value |
|---|---|
| Elastic rod stretch | ~7.7 in of a 41 in stroke never reaches the plunger |
| Plunger stroke | 33.3 in, not 41 in |
| Pump displacement at 6.4 SPM | **38.8 bbl/d** (`rod_pump`, RP 11L). The 39.2 in the scripts omitted the tubing-pressure term. |
| ~~Implied fillage ~59%~~ | **WITHDRAWN 2026-08-09** — undetermined without runtime and Bo; `rod_pump.analyse()` returns `None` for exactly this reason |
| ~~SPM to lift the same barrels full ~4.0~~ | **WITHDRAWN** — rests on the efficiency figure above and on 23 bbl/d being inflow-limited, which needs a fluid-level shot |
| Rod Goodman utilisation | 51% — string has margin, so tubing damage is side-load |
| Compression zone at 59% fillage | ~500 ft → wear should cluster **below ~3,800 ft** |
| Worst pound | **~50% fillage, not the emptiest barrel** — impact velocity peaks mid-stroke |
| Gas interference | Weak: casing 25 psi against a Baird valve at 150 psi |

Cross-checked two independent ways, agreeing to 0.02% (displacement) and to the
digit (impact physics). Scripts: `examples/pumpoff_spm_from_stated_data.py`,
`examples/rod_loading_and_neutral_point.py`.

**A correction made mid-analysis:** the compression zone was first modelled as a
static load reversal scaling with fillage shortfall. Wrong — once the travelling
valve opens, static equilibrium leaves the rod at the pump in zero tension. It is
now one-dimensional elastic impact, σ = ρcv, no fitted constant. The two agreed
within 10% on this well, which is exactly why the wrong one would have survived
review, and only the correct one surfaced the 50%-fillage result.

---

## Direction changed 2026-08-09 — no posts, ground first

Outreach is **stopped**. Nothing is posted and nothing is emailed until the work
is grounded in fundamentals, built out, and can lead with a result rather than an
offer. The drafted Collide reply is **withdrawn, not queued** — it led with a
number our own RP 11L module refuses to state.

Read `GROUNDING.md` before resuming any of it. Short version: we hand-rolled
physics next to a proper API RP 11L implementation in this same repo, omitted the
tubing-pressure term, and published a volumetric efficiency that is undetermined
without runtime and Bo. And every severe-pump-off claim rests on synthetic cards,
because the only real validation data spans 88–98% fillage.

## Pending — needs a decision

1. ~~Collide reply to OP-A's pump-off thread~~ — **withdrawn 2026-08-09.** Not
   queued, not pending: the numbers it led with are not defensible yet. Rework
   only after `GROUNDING.md` steps 1–4.
2. **The public/private split is done** — see the top of this file. If a new
   artefact names anyone, it belongs in the private repo, not here.
3. **The per-client card review names OP-A** and now lives only in the private repo. It goes to him or to nobody.

---

## Second expensive lesson — read the repo's own rules first

`aceengineer-strategy/.claude/CLAUDE.md` carries **hard gate 2: "No PII in
commits — contact details, deal terms, pricing specifics stay out of git."**

The do-not-contact commit was written with **two email addresses and a phone
number in it**. Caught only because the repo's CLAUDE.md surfaced when the
directory was touched again — after the file had already been written. Nothing
had been pushed; it was soft-reset, redacted, and rebuilt.

The convention was visible the whole time: the existing prospect record says
*"Contact details are in the post itself (public on Collide)"* — deliberately not
recording them. The repo also has a `pii-remediation/` directory, so this has
happened before.

**Rule:** a suppression list is not an exception to a PII gate. Identify people
by **name and company**, then look the address up in Gmail at send time. And read
`.claude/CLAUDE.md` in a repo *before* writing files into it, not after.

## Corrections to earlier claims in this program

Recorded because they were asserted confidently and were wrong:

- **"OP-A has published no email address."** False. He gave an address by Collide DM on 2026-07-03, with two existing email threads. Search Gmail before
  asserting contact history.
- **`so-lift-004` "do not answer unless we can cite specific fields."** False.
  Already answered and sent — the Kansas low-BHP one-pager in
  `aceengineer-strategy/pipeline/op-a-collide/`.
- **OP-B treated as a cold first contact.** He had been emailed 2026-07-29.
- **`problems.yml` did not parse** when first written. Validate YAML, do not
  assume.

The pattern in all four: **the strategy repo already held the answer and was not
checked.** Read `aceengineer-strategy/pipeline/` before starting outreach work.

---

## Environment

- `uv run` cannot build digitalmodel on this Mac — lxml 4.9.3 has no cp314 wheel.
  Use a 3.11 scratch venv (numpy scipy pydantic click scikit-learn pyyaml pandas
  matplotlib pytest) or `workspace-hub/.venv/bin/pytest` with `PYTHONPATH=src`.
  A failed `uv run` dirties `uv.lock` — revert it.
- No node on the Mac. The website builds on **ace-linux-1** (node 24), where the
  aceengineer-website checkout lives under the shared analysis mount.
- PDF build: `python docs/small_operator/build_pdfs.py`. Headless Chrome writes
  the PDF then fails to exit; the script treats the timeout as success and judges
  by the file on disk. Do not "fix" it by raising the timeout.
