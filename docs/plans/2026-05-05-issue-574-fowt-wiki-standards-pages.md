# Plan: digitalmodel #574 — FOWT wiki standards-page family

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/574
**Status:** plan-review
**Tier:** T2 (documentation/wiki, but blocks every downstream FOWT calc PR via citation contract)

## Context
- **Standards / publishers / revisions to add as wiki resolver targets:**
  - `iec-61400-3-2.md` — IEC, Design requirements for floating offshore wind turbines (Ed.1, 2019)
  - `dnv-st-0119.md` — DNV, Floating wind turbine structures (2021-06)
  - `dnv-rp-0286.md` — DNV, Coupled analysis of floating wind turbines (2019-05)
  - `dnv-st-0126.md` — DNV, Support structures for wind turbines (2021-07)
  - `dnv-st-0358.md` — DNV, Certification of offshore gangways (2024-02)
  - `dnv-rp-0360.md` — DNV, Subsea power cables in shallow water (2024-03)
  - `api-rp-2sim.md` — API, Structural Integrity Management of Fixed Offshore Structures (2014, R2020)
- **Why it matters.** `.claude/rules/calc-citation-contract.md` D2 forces fail-closed at calc time. Two of these (`DNV-ST-0126`, `API RP 2SIM`) are **already named in `src/digitalmodel/structural/offshore_resilience/structural_health.py` docstring** without a resolvable wiki page; the moment that module emits a `Citation` instance it raises `CitationResolutionError`. The other five unblock issues #575/#576/#577/#578.
- **Dependencies on other issues.** This plan is the prerequisite for #575, #576, #577, #578. Sibling consumer is the planned cross-link entry in `knowledge/wikis/cross-links.md`.

## Scope
- Seven new metadata-only wiki resolver pages, each carrying #2471 frontmatter (`code_id`, `publisher`, `revision`, `revision_source`, `verified_on`, `public_url`, `sources`, `extraction_policy: metadata-only`, `raw_copy_allowed: false`).
- Each page should mirror the structure already proven in `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-c203.md` and `api-rp-2sk.md`: title heading, **Scope** (one paragraph, no clause text), **Why this page exists**, **Where to find the full text** (raw PDF path on `/mnt/ace/` if known, publisher catalog URL, free-text portal), **Cross-references** (`[[wikilinks]]` to adjacent standards), and a closing **Cross-References** block.
- Update `knowledge/wikis/cross-links.md` (workspace-hub repo) with one row per new code_id.
- **Non-goals:** no clause text, no formula reproductions, no S-N curve tables, no figure copies — fully metadata-only per `extraction_policy`. No code changes to `structural_health.py` here; that module's citation emission is wired in #575/#577/#578 consumers.

## Deliverables (paths in `vamseeachanta/workspace-hub`, not digitalmodel — wikis live in workspace-hub)
- `knowledge/wikis/engineering-standards/wiki/standards/iec-61400-3-2.md` — IEC FOWT design page
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-st-0119.md` — DNV floating WT structures
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-0286.md` — DNV coupled-analysis RP
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-st-0126.md` — DNV support structures
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-st-0358.md` — DNV gangway certification
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-0360.md` — DNV dynamic subsea power cables
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2sim.md` — API SIM
- `knowledge/wikis/cross-links.md` — append seven cross-link rows
- `digitalmodel/tests/citations/test_fowt_resolvers.py` (new) — fixture-backed resolver test that confirms each new `code_id` resolves via `digitalmodel.citations.schema` to a real wiki path.

## Approach
1. **Survey raw sources.** `find /mnt/ace/O&G-Standards/IEC /mnt/ace/O&G-Standards/DNV /mnt/ace/O&G-Standards/API -iname "*61400-3-2*" -o -iname "*0119*" -o -iname "*0286*" -o -iname "*0126*" -o -iname "*0358*" -o -iname "*0360*" -o -iname "*2SIM*"` to populate the `sources:` frontmatter blocks accurately. If a PDF isn't present, leave `sources:` empty and rely on `revision_source:` URL only.
2. **Write the seven pages** following the `dnv-rp-c203.md` template exactly — same heading order, same wikilink syntax. Smoke check: `python -c "import yaml,glob; [yaml.safe_load(open(f).read().split('---')[1]) for f in glob.glob('knowledge/wikis/engineering-standards/wiki/standards/{iec-61400-3-2,dnv-st-0119,dnv-rp-0286,dnv-st-0126,dnv-st-0358,dnv-rp-0360,api-rp-2sim}.md')]`.
3. **Update `cross-links.md`** with seven entries pointing into the new files. Smoke check: `grep -c '^| iec-61400-3-2 ' knowledge/wikis/cross-links.md`.
4. **Add `tests/citations/test_fowt_resolvers.py`** in digitalmodel with seven parametrised cases asserting `Citation(code_id=..., wiki_path=...).resolve()` succeeds. Smoke check: `cd digitalmodel && uv run pytest tests/citations/test_fowt_resolvers.py -q`.
5. **Re-run existing citation tests** to confirm no regression: `cd digitalmodel && uv run pytest tests/citations/ -q`.

## Open questions
- Which DNV revision year to lock for `DNV-ST-0119` (the public catalog has 2021-06 and a 2024 amendment)? Default: pick the latest non-amended issue and let downstream calc PRs upgrade if they need amendment text.
- Are the IEC 61400-3-2 PDFs licensed for `/mnt/ace/` storage, or does `sources:` have to omit the local path? Default: omit local path, keep `revision_source:` URL only.
- Should `cross-links.md` also gain a "FOWT family" cluster heading for navigability? Recommend yes; defer to executor.

## Acceptance Criteria
- [ ] All seven `*.md` files exist under `knowledge/wikis/engineering-standards/wiki/standards/` with valid YAML frontmatter (`code_id`, `publisher`, `revision`, `extraction_policy: metadata-only`).
- [ ] `digitalmodel/tests/citations/test_fowt_resolvers.py` passes against every new `code_id`.
- [ ] No clause text, no formula reproduction, no S-N table copy is present in any of the seven pages (manual diff scan acceptable).
- [ ] `knowledge/wikis/cross-links.md` lists every new `code_id` exactly once.
- [ ] `structural_health.py` docstring references for `DNV-ST-0126` and `API RP 2SIM` resolve when run through a Citation construction in a one-line smoke script.
