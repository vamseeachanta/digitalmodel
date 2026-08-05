# Small-operator problem program

Solve the problems individual small / marginal operators actually voice — one by
one — and route each solution back into the ecosystem instead of leaving it as a
one-off answer.

Sibling of [`docs/collide_pe/`](../collide_pe/README.md). That program solves the
"PE Problem of the Day" quiz series; **this one solves real operators' real field
problems**, sourced from the same community.

## Layout

| Path | What |
|---|---|
| `problems.yml` | The catalogue: operators, problems, evidence, our capability, status. The "what to do next" list. |
| `brochure.html` | The general pamphlet. Anonymised — safe to send to any operator. |
| `dynacard-matrix.html` | Per-card review for ONE named operator. Quotes his posts, names his equipment. |
| `examples/pumpoff_diagnosis.py` | Runnable worked example. Prints its own analysis limit. |
| `build_pdfs.py` | Renders the HTML to client-ready PDFs. |
| `pdf/` | Build output. Regenerate rather than hand-edit. |
| `_wordmark.svg` | The AceEngineer wordmark, inlined into both documents. |
| `outreach/` | Draft nudge emails. **Nothing here has been sent.** |

## Building the client PDFs

```bash
python docs/small_operator/build_pdfs.py           # both
python docs/small_operator/build_pdfs.py matrix    # one
```

| PDF | Orientation | Send to |
|---|---|---|
| `AceEngineer-small-operator-field-note.pdf` | Portrait, 6 pp | Any small operator. Nothing company-specific. |
| `AceEngineer-dynacard-review-matrix.pdf` | Landscape, 4 pp | **One named operator only.** |

Three things the build script exists to handle, none of which are optional:

1. **The HTML files are artifact fragments** — no `<html>`/`<head>`/`<body>`, because
   the Artifact publisher supplies those. Opening one directly gives quirks mode.
2. **The theme must be pinned to light.** These pages follow the viewer's OS theme,
   so a dark-mode machine would otherwise produce a dark PDF for a client.
3. **The matrix must be forced to stay a matrix.** Printed landscape Letter gives
   ~700px of content width, which is *below* the 900px breakpoint where the page
   collapses to stacked cards for phones. Correct on screen, fatal on paper. The
   print CSS re-asserts `display: table` and `table-layout: fixed` — the latter
   because under automatic layout the Case column's pull-quote claimed over half
   the sheet and squeezed the four answer columns into ribbons.

Headless Chrome on macOS writes the PDF and then **fails to exit**, so the script
treats the timeout as expected and judges success by the file on disk, not the
exit code. Don't "fix" that by raising the timeout.

## Branding

Both documents carry the canonical AceEngineer wordmark, taken from
`aceengineer-website/assets/img/logo.svg` and inlined as `_wordmark.svg` with its
two fills rewired to CSS custom properties, so the mark inverts with the theme the
way `logo-inverse.svg` does on the site. The document palette follows it: navy
`#0B3D91` for headings, teal `#2BB2A6` for rules and markers, cool neutrals under
both. Amber is now reserved for semantic warnings only.

**A discrepancy to be aware of:** `aceengineer-website/brand/BRAND.md` states the
logo uses a "plum/copper family" and explicitly *not* navy/teal. The committed
`logo.svg` is navy/teal, `tests/js/brand-assets.test.js` asserts navy/teal, and the
wordmark commit (`510aa2e`) is newer than BRAND.md (`47694fc`). We followed the
asset and the test. **BRAND.md appears stale and should be corrected upstream** —
if it turns out BRAND.md is right and the asset is wrong, these two documents
inherit the error and need re-paletting.

## The routing pattern (inherited from `collide_pe`)

Each problem is solved and then **connected as a dot**:

- **Quantitative** → a reusable function in `src/digitalmodel/<domain>/` + a
  regression test pinning the numbers. Record it under `our_capability.functions`.
- **Conceptual** → an llm-wiki concept page.
- **Neither** (procurement, workload, capital) → a data file + a decision model,
  and say plainly that it is not a physics problem.

## Evidence discipline — read this before writing anything

This program exists downstream of a public correction. On 2026-07-27 an
AceEngineer analysis was linked into a Collide thread and was corrected within
hours by a rod-lift specialist: a gearbox rating had been inferred from an
equipment designation that turned out to be a gas *engine*, and a fillage number
had been computed off surface stroke instead of plunger stroke.

So, non-negotiably:

1. **Quote, don't infer.** Every equipment name in `problems.yml` is a verbatim
   quote with a pointer to the post.
2. **Tag inferences.** Anything we deduced carries `inference: true`.
3. **Error budget** on anything digitized off a plotted image.
4. **Verify secondary numbers** (bonding thresholds, plugging costs) against
   primary sources before they appear in outreach. The `research_refs` numbers
   in `problems.yml` are explicitly marked unverified.
5. **Fix the body, not just a comment,** when corrected.

## Status at capture (2026-08-05)

| Category | Problems | Notes |
|---|---|---|
| `lift` | 5 | 1 analysed (the 2026-07-27 thread), 4 open |
| `maintenance` | 4 | all open; `so-maint-002` (well triage) is the highest-leverage |
| `supply_chain` | 2 | the pooling thesis — **our proposal, not an operator ask** |
| `data` | 2 | `so-data-002` "Tally app needed" is the cheapest concrete win |
| `capital` | 1 | we underwrite technically; we are not a capital provider |
| `regulatory` | 3 | `so-reg-003` **built and tested**; the other two are context, not advice |

Two named operators: **Reed Goodman** (Jaybird Resources, Cuero TX) and
**Neal Turluck** (S & S Oil and Gas, Illinois Basin).

## Shipped

**`so-reg-003` — marginal well economics** (2026-08-05).
`src/digitalmodel/production_engineering/marginal_well_economics.py`, 34 tests green.

Scores KEEP / WORKOVER / SHUT_IN / PLUG_NOW for one wellbore on a single
comparable basis — **every branch ends with the well plugged**, because the P&A
liability is unavoidable and omitting it from the "keep" branch rigs the
comparison in favour of producing.

The load-bearing idea: plugging cost is *deferred*, not avoided, so a marginal
well competes against paying the P&A bill today rather than against zero. On the
illustrative deck in `problems.yml`, a 1.5 BOPD well needs $92/bbl to break even
on operating cash — yet shutting it in still beats plugging by ~$16k, purely from
deferral. That is the arithmetic keeping the US stripper fleet standing.

One defect was found and fixed during the build: the first version reported
`KEEP` for wells with *zero* cash-positive months, because deferring P&A by a
single month scored marginally better than plugging today. `KEEP` is now
withdrawn when nothing is profitable to produce; regression test pins it.

## Next actions

1. Read the three posts captured by title only: `Tally app needed`,
   `Tubing Wear - Rod pump`, `Under pressured/ Low BHP Gas Fields`.
2. Verify the bonding / plugging-cost figures against primary sources.
3. Confirm whether Reed's dyno is accelerometer-based (Walter Phillips asked
   in-thread; it went unanswered) before any card-trust-band work.
4. Ask Reed what the gearbox is on the C66 unit before any torque work.
5. Nothing gets posted to Collide or emailed without explicit approval.
