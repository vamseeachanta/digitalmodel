# HANDOFF — small-operator program (2026-08-05 → 08)

Exit snapshot. Read the **Do not repeat** section before touching outreach.

**Everything is pushed.** digitalmodel `feat/small-operator-program` (8 commits,
rebased onto main), aceengineer-strategy `main`, aceengineer-website `main` and
live. No dirty trees, nothing unpushed. Open the digitalmodel PR at
`github.com/vamseeachanta/digitalmodel/pull/new/feat/small-operator-program`.

---

## What this program is

Solve problems that individual small / marginal operators actually voiced, one
at a time, and route each solution into the ecosystem rather than answering it
once. Sibling of `docs/collide_pe/`, which solves the PE-Problem-of-the-Day quiz
series; this one solves operators' real field problems.

Two named operators drove it, both from Collide: **Reed Goodman** (Jaybird
Resources, Cuero TX) and **Neal Turluck** (S & S Oil and Gas, Illinois Basin).

---

## Do not repeat — the expensive lesson

**Neal Turluck replied "Please stop emailing me" on 2026-08-05.** Two unsolicited
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
segment. Reed has now had **five emails and replied to none**, while posting on
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
| Elastic rod stretch | 7.4 in of a 41 in stroke never reaches the plunger |
| Plunger stroke | 33.6 in, not 41 in |
| Pump capacity at 6.4 SPM | 39.2 bbl/d full |
| **Implied fillage** | **~59%** against 23 bbl/d reported |
| **SPM to lift the same barrels full** | **~4.0** vs 6.4 today |
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

## Pending — needs a decision

1. **Collide reply to Reed's pump-off thread — drafted, NOT posted.** Full text
   in the session; the analysis behind it is committed. This is the highest-value
   pending item and the right channel. Open question: post the SPM number and the
   wear-depth prediction only, or include the Goodman and casing analysis too
   (roughly doubles the length).
2. **Open the digitalmodel PR.** Branch is pushed and rebased; no PR yet.
   `.pre-commit-config.yaml` exists but no hooks are installed locally, so
   nothing ran on these commits — CI will be the first to lint them.
3. **Move `docs/small_operator/` into aceengineer-strategy?** Raised and not
   resolved. `pipeline/reed-goodman-collide/HANDOFF.md` carries a standing policy:
   *"Client identities are kept in this private strategy repo … never in public
   repos"*. digitalmodel is PUBLIC and this directory names both operators. The
   user said public is fine; the policy still says otherwise. **Reconcile the two
   rather than leaving them contradicting each other.**
4. **The dynacard matrix PDF names Reed** and must not go to anyone else.

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

- **"Reed has published no email address."** False. He gave an address by Collide DM on 2026-07-03, with two existing email threads. Search Gmail before
  asserting contact history.
- **`so-lift-004` "do not answer unless we can cite specific fields."** False.
  Already answered and sent — the Kansas low-BHP one-pager in
  `aceengineer-strategy/pipeline/reed-goodman-collide/`.
- **Neal treated as a cold first contact.** He had been emailed 2026-07-29.
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
- No node on the Mac. The website builds on **ace-linux-1** (node 24), repo at
  `/mnt/ace/ws/aceengineer-website`.
- PDF build: `python docs/small_operator/build_pdfs.py`. Headless Chrome writes
  the PDF then fails to exit; the script treats the timeout as success and judges
  by the file on disk. Do not "fix" it by raising the timeout.
