# Plan: digitalmodel #461 — Recover ship-dimensions template artifact from WRK-1339 Child E

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/461
**Status:** plan-review
**Tier:** T2 (recovery/replacement decision)
**Downstream:** #463 (manual curation), #457 (loader code, DONE)

## Context

`WRK-1339 Child E` was supposed to deliver either `generate-ship-dimension-template.py` or a generated `ship-dimensions.yaml` template. Local recovery checked workspace-hub files, WRK-1339 asset folder, repo history, sibling/child WRKs, and existing GitHub issue discussion — no generator script found.

What **was** found during this plan's prep:
- `data/ship_dimensions_template.yaml` exists in digitalmodel (111 lines, 10 hull entries with units in feet/long-tons)
- `src/digitalmodel/naval_architecture/ship_dimensions.py` exists (204 lines) with `default_dimension_template_path()` resolving to `data/ship_dimensions_template.yaml`
- No `generate-ship-dimension-template.py` anywhere in the digitalmodel repo (confirmed by `find . -name "generate*ship*dimension*"` → empty)
- No reference to such a script in workspace-hub either (executor must verify with `grep -r "generate-ship-dimension-template" /mnt/local-analysis/workspace-hub --include="*.py" --include="*.md"`)

This points strongly toward the **"deliberately replace"** path of the issue's required outcome (#3): the template was never generated programmatically — it was hand-authored as `data/ship_dimensions_template.yaml`. The generator concept can either be (a) declared a non-deliverable (template is the artifact, period), or (b) authored fresh as a post-hoc template generator that can rebuild the YAML from a vessel-class CSV plus PDF metadata.

## Plan

1. **Final search for the missing artifact.** Run a wider sweep:
   - `find /mnt/local-analysis/workspace-hub -name "generate*dimension*" -o -name "generate*ship*" 2>/dev/null`
   - `grep -rln "generate-ship-dimension-template" /mnt/local-analysis/workspace-hub --include="*.py" --include="*.md" --include="*.yaml"`
   - Check git history: `cd /mnt/local-analysis/workspace-hub/digitalmodel && git log --all --diff-filter=AD -- "*ship*dimension*"`
   - Check abandoned branches: `git branch -a | grep -i wrk-1339`
   Document outcome as a comment on this issue. If anything turns up, import to step 4's canonical path.

2. **Lock the canonical paths.** Decision (justified below): **declare `data/ship_dimensions_template.yaml` the canonical artifact**. No generator is required for the Phase-1 100-vessel data set — manual curation is the contract per #457's plan. If a programmatic generator is needed in the future (Phase 2+), it's a separate WRK.
   - Canonical template: `digitalmodel/data/ship_dimensions_template.yaml`
   - Loader: `digitalmodel.naval_architecture.ship_dimensions.load_dimension_template()`
   - Validator: `digitalmodel.naval_architecture.ship_dimensions.validate_vessel_entry()`

3. **Document the decision.** New section in `data/ship_dimensions_template.yaml` head comment:
   ```yaml
   # Canonical ship-dimensions template.
   #
   # Provenance: This file IS the canonical artifact. WRK-1339 Child E referenced
   # a generator script that was never produced; per digitalmodel#461, the YAML
   # was adopted directly as the artifact. New entries are added by hand
   # following the schema in src/digitalmodel/naval_architecture/ship_dimensions.py.
   #
   # Schema entry contract:
   #   hull_id (str, required)
   #   name (str)
   #   loa_ft, lwl_ft, beam_ft, draft_ft, displacement_lt (float, required for "complete")
   #   speed_kt (float)
   #   source (str)
   #   entry_status (str): "draft" | "verified"
   #   verification_source (str)
   ```

4. **Author optional generator (deferred).** If executor judges a programmatic regenerator is worth the effort: new file `scripts/ship-dimensions/generate-template.py` that takes a vessel-class CSV (USNA ship-plan inventory) and emits a fresh template skeleton. Keep this scope-bounded: v1 produces empty entries, no PDF parsing, no Jane's lookup. **Default: defer this to a follow-up issue**. The minimum acceptance for #461 is "canonical paths are locked" — generator is not required.

5. **Update downstream WRKs.** Edit any documentation referencing the missing artifact:
   - WRK-1380 plan in #457 body: replace "generator path TBD" with the canonical `data/ship_dimensions_template.yaml`
   - Anywhere else that says "blocked on Child E": update to "resolved via #461"
   These edits happen via `gh issue edit` or new comments — no source code changes outside `data/ship_dimensions_template.yaml`'s docstring.

6. **Smoke check.** `uv run python -c "from digitalmodel.naval_architecture.ship_dimensions import default_dimension_template_path, load_dimension_template; print(default_dimension_template_path()); print(len(load_dimension_template()), 'vessels')"` — confirms loader resolves the canonical path and parses the YAML.

7. **Communicate to #463.** Post a comment on #463 unblocking it: "#461 resolved — `data/ship_dimensions_template.yaml` is canonical. Proceed with curation."

## Acceptance Criteria

- [ ] Wider artifact search documented in issue thread (negative result acceptable)
- [ ] `data/ship_dimensions_template.yaml` head comment updates with provenance and schema documentation
- [ ] All references to "missing Child E artifact" in #457 / #463 / WRK trackers replaced with explicit canonical paths
- [ ] Loader smoke command passes
- [ ] #463 unblocked via cross-reference comment

## Open Questions

- Should a programmatic regenerator (step 4) ship in this PR or as a follow-up? Recommend follow-up — keep #461 small and unambiguously a recovery/decision issue. The generator is convenience, not contract.
- Does any non-digitalmodel repo (assethold, marine-orcaflex) reference the missing artifact? Step 1's grep across workspace-hub will surface this.
