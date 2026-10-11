# Focused Plan Re-review Artifact — digitalmodel #594 — Codex

Prompt: `.planning/quick/review-594-rerun-prompt.md`
Raw log: `.planning/quick/review-594-rerun-codex.out`

---

OpenAI Codex v0.130.0
--------
workdir: /mnt/local-analysis/workspace-hub/digitalmodel
model: gpt-5.5
provider: openai
approval: never
sandbox: workspace-write [workdir, /tmp, /home/vamsee/.codex/memories]
reasoning effort: medium
reasoning summaries: none
session id: 019e212b-9437-7d62-a21a-b64fe8b48ae9
--------
user
# Focused Adversarial Re-review: digitalmodel #594 revised plan

You are reviewing the REVISED plan after initial MAJOR findings.
Return verdict APPROVE, MINOR, or MAJOR.
Do not repeat resolved findings unless still unresolved.

Check specifically whether these initial blockers are now resolved:
1. Dataset path contradiction: JSON under examples/demos/gtm vs internal_reference_only.
2. Digitized SeaOps-derived numerical values lack rights/confidence boundary.
3. Conditional gtm_storyline rename was under-specified.
4. examples/demos/gtm/README.md remained GTM-framed.
5. Test plan misquoted current assertions and missed LIVE MODE / gtm_storyline.
6. Verification too narrow / no consumer surface search.
7. Issue title/label/comment hygiene missing.
8. Session handoff treated as canonical instead of references README.

Required output:
- Verdict: APPROVE | MINOR | MAJOR
- Remaining blockers only, grouped by severity.
- What you checked.

Revised plan:

# Plan: digitalmodel #594 — Vessel operability reference synthesis for CTV/SOV/W2W suitability

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/594  
**Status:** revised after T3 adversarial review; not approval-ready until user approval  
**Scope:** docs/metadata/reference-boundary correction only; no engineering implementation  
**Review depth:** T3 plan review complete — Claude, Codex, Gemini artifacts saved  
**Created:** 2026-05-13T11:28:05Z  
**Revised:** 2026-05-13 after MAJOR review findings

## Scope correction

Issue #594 was opened with GTM wording, but the current user-approved boundary is narrower:

- CTV/Kincardine/SeaOps assets are **reference material** for future vessel-operability ecosystem work.
- SeaOps is a competitor; competitor-observed patterns may be studied but not copied into client/project/GTM deliverables.
- This plan produces an internal reference-synthesis package, moves/reclassifies misleading GTM-shaped fixture data, and adds guardrails against accidental promotion.
- It does **not** publish, regenerate, or promote the material as GTM/project collateral.

A later explicit user decision can promote selected, original, evidence-backed outputs into GTM or project collateral only after source-rights review and a separate approved issue.

## Resource Intelligence Summary

### Live issue state

Verified 2026-05-13T11:28:05Z:

- `#594` — OPEN — `Assess access and operability GTM extension for CTV/SOV/W2W vessel suitability`; labels: `enhancement`, `cat:engineering`, `priority:medium`, `cat:website`, `status:pending`.
- Related issue `#578` — OPEN, `status:plan-approved`; W2W gangway operability module plan exists at `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md`.
- Related issue `#467` — OPEN, `status:plan-approved`; subsea installation vessel-performance and real-time motion feedback scope.

### Authoritative boundary docs

- `references/vessel-suitability/README.md` is the authoritative current reference-boundary document. It defines `ctv-operability-kincardine-print-redline.*` as reference material only and says not to treat it as GTM or project-delivery collateral by default.
- `docs/session-handoffs/2026-05-13-ctv-operability-reference-boundary.md` is supporting session history only, not the canonical policy surface.

### Existing assets and tests

- `references/vessel-suitability/ctv-operability-kincardine-print-redline.{html,svg,png,pdf}` exists as the preserved SeaOps-adjacent reference asset family.
- `examples/demos/gtm/data/ctv_operability_kincardine.json` currently contains digitized/structured operability values plus GTM/prospect-facing wording.
- `examples/demos/gtm/tests/test_ctv_operability_reference.py` currently enforces old GTM framing:
  - `assert "GTM" in data["_description"]`
  - `use_cases = set(data["gtm_storyline"]["demo_use_cases"])`
  - assertions for `"vessel suitability analysis"`, `"CTV / SOV access operability screening"`, and `LIVE MODE`
  - `assert data["gtm_storyline"]["engineering_caveats"]`
- `examples/demos/gtm/README.md` currently describes the fixture as supporting "vessel/access suitability GTM storylines".

### Consumer surface check

Search terms checked in repo:

```text
ctv_operability_kincardine|gtm_storyline|demo_use_cases
```

Observed matches are limited to:

- this plan;
- `examples/demos/gtm/tests/test_ctv_operability_reference.py`;
- `examples/demos/gtm/README.md`;
- `examples/demos/gtm/data/ctv_operability_kincardine.json`.

No Python generator/consumer was found outside the fixture/test/index surface. Therefore the plan commits to moving the JSON out of the GTM demo tree instead of retaining a GTM-shaped compatibility key.

### Gaps identified

1. No standalone internal synthesis note exists that separates:
   - observed reference/competitor patterns;
   - reusable engineering concepts;
   - original AceEngineer/digitalmodel methodology requirements;
   - explicit non-promotion boundary.
2. Existing JSON/test/index metadata conflicts with the clarified boundary by treating the dataset as GTM/prospect-facing.
3. The digitized numerical values from the competitor-adjacent infographic need their own retention boundary; wording/layout constraints are not enough.
4. No data-contract sketch exists for a future source-native vessel-operability workflow using metocean, vessel/access limits, landing geometry, daylight windows, RAOs/motions, and operability criteria.
5. No issue-local restart checklist exists to convert #594 from `status:pending` into a reviewed, approval-ready plan.
6. Digitalmodel adapter requires WRK mapping; no WRK item was found for #594. This must be created or explicitly waived before implementation.

### Reproduction proofs

N/A — documentation/reference-boundary plan. There is no alleged runtime failure. Runtime verification is included because the existing tests and README encode the outdated GTM framing.

## Deliverable

A committed internal reference-synthesis package for vessel-operability work that:

1. preserves competitor/reference boundaries;
2. moves the SeaOps-derived structured data out of the GTM demo tree;
3. marks digitized numerical values as rights-unresolved and internal-only;
4. defines a future source-native data/report contract;
5. adds tests/guardrails so the material is not accidentally promoted to GTM/client/project collateral.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-13-issue-594-vessel-operability-reference-synthesis.md` |
| Reference synthesis note | `references/vessel-suitability/ctv-sov-w2w-operability-reference-synthesis.md` |
| Reference README update | `references/vessel-suitability/README.md` |
| New reference data directory | `references/vessel-suitability/data/` |
| Reclassified metadata fixture | `references/vessel-suitability/data/ctv_operability_kincardine.json` |
| Removed old GTM data fixture | `examples/demos/gtm/data/ctv_operability_kincardine.json` |
| GTM index cleanup | `examples/demos/gtm/README.md` |
| Boundary tests update | `examples/demos/gtm/tests/test_ctv_operability_reference.py` |
| Plan review artifacts | `scripts/review/results/2026-05-13-plan-594-{claude,codex,gemini}.md` |

## Proposed tasks

### Task 0 — WRK and issue hygiene before implementation

**Files / commands:**

- Create or update WRK mapping in workspace-hub work queue for digitalmodel #594, or document the explicit Route-A exception if the adapter rule is superseded.
- Retitle #594 to remove default GTM/website implication, for example:

```bash
gh issue edit 594 \
  --title "Vessel operability reference synthesis for CTV/SOV/W2W suitability" \
  --remove-label "cat:website"
```

- Post a scope-correction comment linking this plan and the review artifacts.
- Do **not** add `status:plan-approved` and do **not** implement until the user explicitly approves.

### Task 1 — Write boundary-first failing tests

**Files:**

- Modify: `examples/demos/gtm/tests/test_ctv_operability_reference.py`

**Test changes:**

- Rename misleading test functions, e.g. replace `test_ctv_operability_supports_gtm_vessel_suitability_storyline` with `test_ctv_operability_enforces_internal_reference_boundary`.
- Update fixture path to `references/vessel-suitability/data/ctv_operability_kincardine.json`.
- Replace these old assertions:
  - `assert "GTM" in data["_description"]`
  - `use_cases = set(data["gtm_storyline"]["demo_use_cases"])`
  - use-case assertions for vessel suitability, CTV/SOV screening, and `LIVE MODE`
  - `data["gtm_storyline"]["engineering_caveats"]`
- Add assertions requiring:
  - `data["usage_boundary"]["classification"] == "internal_reference_only"`
  - `data["usage_boundary"]["promotion_rule"]` states explicit user approval and source-rights review are required before external use
  - `data["usage_boundary"]["competitor_context"]` identifies SeaOps as competitor-adjacent reference material
  - `data["data_confidence"]["digitized_values"] == "rights_unresolved_no_external_use"`
  - `data["methodology_boundary"]` separates reference observation from design-basis engineering
  - no `gtm_storyline`, `demo_use_cases`, `prospect-facing`, or `LIVE MODE` strings remain in the reference fixture
  - the old GTM data path no longer exists or is not used by the test

**Verification command:**

```bash
cd /mnt/local-analysis/workspace-hub/digitalmodel
uv run pytest examples/demos/gtm/tests/test_ctv_operability_reference.py -q
```

**Expected first result:** FAIL until metadata and file placement are corrected.

### Task 2 — Move and correct structured metadata without losing traceability

**Files:**

- Move: `examples/demos/gtm/data/ctv_operability_kincardine.json` → `references/vessel-suitability/data/ctv_operability_kincardine.json`
- Modify: `references/vessel-suitability/data/ctv_operability_kincardine.json`
- Modify: `examples/demos/gtm/README.md`

**Required metadata changes:**

- Change `_description` from GTM/prospect-facing framing to internal reference framing.
- Rewrite `_source.license_note` from "external GTM material" to: no external use until source-rights resolution plus explicit user approval.
- Preserve source attribution: SeaOps Solutions, Kincardine, NORA3 stated basis, captured source type.
- Add explicit usage boundary:

```json
"usage_boundary": {
  "classification": "internal_reference_only",
  "competitor_context": "SeaOps Solutions is competitor-adjacent reference material; do not copy wording, layout, claims, or digitized values into deliverables.",
  "promotion_rule": "Project, client, or GTM reuse requires explicit user approval, source-rights review, and a separate approved issue."
}
```

- Add digitized-data confidence boundary:

```json
"data_confidence": {
  "digitized_values": "rights_unresolved_no_external_use",
  "engineering_use": "not_design_basis",
  "source_native_requirement": "Regenerate from licensed/public metocean and project data before engineering or external use."
}
```

- Rename `gtm_storyline` to `reference_insights` and remove `demo_use_cases`, `prospect-facing`, and `LIVE MODE` language.
- Replace demo-use-case framing with internal reference insights only.
- Update `examples/demos/gtm/README.md` to remove the old fixture row or mark it as moved to `references/vessel-suitability/data/` and not GTM-consumable.

### Task 3 — Add reference synthesis note

**Files:**

- Create: `references/vessel-suitability/ctv-sov-w2w-operability-reference-synthesis.md`

**Required sections:**

1. `Purpose and boundary` — internal reference only; no client/GTM/project reuse by default.
2. `Observed reference patterns` — high-level observations only, not copied wording/layout/claim language:
   - monthly operability by access configuration;
   - annual/headline operability uplift concept;
   - daylight and environmental thresholds;
   - directional/landing arrangement sensitivity;
   - bottleneck-month identification;
   - caveat block separating infographic-derived values from source-native metocean analysis.
3. `Reusable engineering concepts` — access mode, landing point, vessel/access limits, metocean climatology, persistence/weather windows, relative motion/RAO path.
4. `Original digitalmodel methodology requirements` — data schema, provenance, unit handling, confidence class, chart/report outputs, tests.
5. `Integration points` — #578 W2W gangway, #467 real-time vessel motion feedback, reference fixture.
6. `Promotion gate` — explicit approval + source-rights/attribution check + separate approved issue required before GTM/project use.
7. `Anti-laundering checklist` — no copied SeaOps wording, no replicated visual hierarchy, no client-facing claims, no export of digitized values.

### Task 4 — Update vessel-suitability README index

**Files:**

- Modify: `references/vessel-suitability/README.md`

Required updates:

- Add synthesis note and `data/ctv_operability_kincardine.json` to the file list.
- Declare the JSON as internal reference data only with digitized values rights-unresolved/no-external-use.
- Make `references/vessel-suitability/README.md` the canonical usage-boundary location.
- State that source-native engineering implementation belongs under a later approved issue.

### Task 5 — Add promotion guardrail test

**Files:**

- Modify or create a focused test near the existing reference test.

Guardrail requirements:

- Fail if `examples/demos/gtm/data/ctv_operability_kincardine.json` exists.
- Fail if `examples/demos/gtm/README.md` describes the fixture as a GTM storyline input.
- Fail if `gtm_storyline`, `prospect-facing`, or `LIVE MODE` remain in the reference fixture.
- Fail if the fixture lacks `usage_boundary.classification == internal_reference_only`.

### Task 6 — Run focused and surface-area verification

**Commands:**

```bash
cd /mnt/local-analysis/workspace-hub/digitalmodel
uv run pytest examples/demos/gtm/tests/test_ctv_operability_reference.py -q
rg "ctv_operability_kincardine|gtm_storyline|demo_use_cases|prospect-facing|LIVE MODE" examples src tests docs references
git diff -- references/vessel-suitability examples/demos/gtm/data/ctv_operability_kincardine.json examples/demos/gtm/README.md examples/demos/gtm/tests/test_ctv_operability_reference.py
```

If `rg` shows additional consumers, update them or explicitly document why they are safe.

## Acceptance Criteria

- [ ] Old GTM data path `examples/demos/gtm/data/ctv_operability_kincardine.json` is removed or proven non-consumable.
- [ ] Reclassified JSON lives under `references/vessel-suitability/data/`.
- [ ] Existing SeaOps-adjacent visual/reference assets remain in `references/vessel-suitability/` and are not copied into a new GTM/client output.
- [ ] New synthesis note separates competitor-observed patterns from original AceEngineer/digitalmodel methodology.
- [ ] JSON metadata no longer states that the dataset is prospect-facing/GTM by default.
- [ ] Digitized numerical values are marked `rights_unresolved_no_external_use` and `not_design_basis`.
- [ ] Tests enforce internal-reference-only boundary, rights/promotion guardrails, and old-GTM-path removal.
- [ ] `examples/demos/gtm/README.md` no longer advertises the fixture as a GTM storyline input.
- [ ] Integration points to #578 and #467 are documented.
- [ ] Targeted verification passes with `uv run pytest examples/demos/gtm/tests/test_ctv_operability_reference.py -q`.
- [ ] Surface-area search is clean or documented.
- [ ] #594 is retitled/de-labeled/commented before it is moved from `status:pending` to `status:plan-review`.

## Risks and controls

| Risk | Control |
|---|---|
| Accidentally laundering competitor material into AceEngineer collateral | Move JSON out of GTM tree; add usage boundary and guardrail tests |
| Exporting competitor-derived digitized values | Mark values `rights_unresolved_no_external_use`; source-native regeneration required before external use |
| Breaking existing GTM demo scripts | Consumer surface search shows no generator dependency; README/test updated explicitly |
| Treating digitized infographic values as design-basis data | Add confidence/provenance caveats and source-native data requirements |
| Over-expanding into engineering implementation | Keep this issue docs/metadata only; #578/#467 cover implementation paths |

## Plan Review Results

T3 adversarial review was run on 2026-05-13 and all three reviewers initially returned MAJOR.

Artifacts saved:

- `scripts/review/results/2026-05-13-plan-594-claude.md`
- `scripts/review/results/2026-05-13-plan-594-codex.md`
- `scripts/review/results/2026-05-13-plan-594-gemini.md`

Resolved in this revision:

- dataset path contradiction by moving JSON out of `examples/demos/gtm/data/`;
- conditional `gtm_storyline` rename by making rename mandatory;
- digitized values rights boundary via `rights_unresolved_no_external_use`;
- missing `examples/demos/gtm/README.md` update;
- exact test assertion replacement list;
- broader consumer search and guardrail test;
- nonexistent fanout script removed from plan path;
- issue title/label/comment hygiene added;
- session handoff demoted to supporting history.

## Current stop point

This is a revised plan after adversarial review. Do not implement Tasks 0–6 until the user approves #594 for implementation and the approval marker/label workflow is completed.


warning: Codex's Linux sandbox uses bubblewrap and needs access to create user namespaces.
codex
I’ll treat this as a plan review, not implementation. I’m going to verify the plan’s claims against the current repo surfaces that matter: the old fixture path, test assertions, README framing, reference README, and consumer-search terms.
codex
The first read hit the sandbox’s bubblewrap setup before it could open the skill files. I’ll continue with the review using the supplied plan text and, where possible, lighter repo checks.
codex
Verdict: APPROVE

**Remaining Blockers**
None.

**What I Checked**
I reviewed the revised plan against each prior blocker:

- Path contradiction is resolved: JSON is moved from `examples/demos/gtm/data/` to `references/vessel-suitability/data/`.
- Digitized SeaOps-derived values now have explicit `rights_unresolved_no_external_use` and `not_design_basis` boundaries.
- `gtm_storyline` rename is now mandatory, not conditional.
- `examples/demos/gtm/README.md` cleanup is included.
- Current test assertions are quoted specifically, including `LIVE MODE`, `gtm_storyline`, and old use-case assertions.
- Verification now includes focused tests, old-path guardrail, and repo-wide search over `examples src tests docs references`.
- Issue hygiene is included: retitle, remove `cat:website`, comment, and avoid premature `status:plan-approved`.
- Session handoff is correctly demoted; `references/vessel-suitability/README.md` is made canonical.

I attempted local repo checks, but shell execution is currently blocked by the sandbox with `bwrap: loopback: Failed RTM_NEWADDR: Operation not permitted`, so this verdict is based on the revised plan text rather than live filesystem verification.
tokens used
30,093
Verdict: APPROVE

**Remaining Blockers**
None.

**What I Checked**
I reviewed the revised plan against each prior blocker:

- Path contradiction is resolved: JSON is moved from `examples/demos/gtm/data/` to `references/vessel-suitability/data/`.
- Digitized SeaOps-derived values now have explicit `rights_unresolved_no_external_use` and `not_design_basis` boundaries.
- `gtm_storyline` rename is now mandatory, not conditional.
- `examples/demos/gtm/README.md` cleanup is included.
- Current test assertions are quoted specifically, including `LIVE MODE`, `gtm_storyline`, and old use-case assertions.
- Verification now includes focused tests, old-path guardrail, and repo-wide search over `examples src tests docs references`.
- Issue hygiene is included: retitle, remove `cat:website`, comment, and avoid premature `status:plan-approved`.
- Session handoff is correctly demoted; `references/vessel-suitability/README.md` is made canonical.

I attempted local repo checks, but shell execution is currently blocked by the sandbox with `bwrap: loopback: Failed RTM_NEWADDR: Operation not permitted`, so this verdict is based on the revised plan text rather than live filesystem verification.
