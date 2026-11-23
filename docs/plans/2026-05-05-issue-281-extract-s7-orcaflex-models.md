# Plan: digitalmodel #281 — WRK-121 Extract & catalog OrcaFlex models from rock-oil-field/s7

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/281
**Status:** plan-review
**Tier:** T3 (large data-engineering pipeline + sanitization + skill scaffolding)

## Context

Issue #281 (WRK-121) extracts ~235 OrcaFlex `.dat` model files from the `rock-oil-field/s7/` working tree, sanitizes client-identifying material, converts each to YAML, organizes into four library categories (jumper, installation, mooring, training), and validates round-trip fidelity through a 3-way benchmark against the format converter (#280). The format converter foundation is already in place: `src/digitalmodel/solvers/orcaflex/format_converter/{cli.py, single_to_modular.py, modular_to_single.py, ...}` exists and is unit-tested per the #280 issue body (102 tests passing).

The deliverables in the issue's acceptance list (all unchecked) require: `scripts/sanitize_s7_models.py`, `scripts/extract_s7_specs.py`, `digitalmodel/.legal-deny-list.yaml`, sanitized model YAMLs in `docs/modules/orcaflex/{jumper,installation,mooring,training}/`, per-group `spec.yml` files, two SKILL.md files (`orcaflex-jumper-analysis`, `legal-sanity-scan`), passing legal-scan exit code, and round-trip validation on jumper + mooring subsets.

**Stale-flag:** Not stale, but resource-blocked. The s7 tree is a separate repo on a licensed machine; plan must run on `orcaflex-license-machine`. The 2026-02-11 progress note ("Plan approved") suggests this work paused; verify before execution that no partial output already exists in `docs/modules/orcaflex/`.

## Plan

### Task 1 — Pre-flight check for existing partial output
On `orcaflex-license-machine`, run `git log --diff-filter=A -- docs/modules/orcaflex/` and `find docs/modules/orcaflex -name '*.yml' | head` to determine whether an earlier sanitization pass left artifacts. If yes, scope the residue (date, count, category) into the plan's first commit message and decide whether to delete-and-redo or resume.

### Task 2 — Build the legal deny-list
Create `digitalmodel/.legal-deny-list.yaml` enumerating client names, project codes, vessel names, and field-development codenames known to appear in the s7 corpus. Include a fallback regex set (e.g., `\b[A-Z]{2,}-\d{3,}\b` for project codes). Add a `legal-sanity-scan` skill at `.claude/skills/legal-sanity-scan/SKILL.md` that runs the deny-list against any path argument and exits non-zero on hit.

### Task 3 — Sanitization script
Implement `scripts/sanitize_s7_models.py` that walks the s7 tree, reads each `.dat` via the format converter (`single_to_modular`), strips/replaces every deny-list match in both keys and string values, and writes the sanitized YAML to the appropriate `docs/modules/orcaflex/<category>/` path. Categorization key: filename heuristics (`*jumper*`, `*install*`, `*moor*`, `*training*`) with a fallback `unclassified/` bucket for human review. Emit a per-file CSV log of substitutions (input-path, hit-count, hit-categories) for audit.

### Task 4 — Spec extraction
Implement `scripts/extract_s7_specs.py` that reads each sanitized YAML and produces a per-group `spec.yml` capturing the union of vessel, line, and analysis parameters (driven by `ModularToSpecConverter` from #280). One `spec.yml` per category, parameterized so a downstream model in that group can be regenerated.

### Task 5 — Round-trip validation
Run a 3-way benchmark on jumper and mooring subsets: original `.dat` → format converter → YAML → format converter → `.dat'`. Compare original-vs-roundtripped `.dat` byte-modulo-comments. Threshold: ≥95% of files pass byte-equality (ignoring CRLF and comment-only differences). Capture failures into `scripts/conversion/s7_roundtrip_failures.csv` for triage.

### Task 6 — Jumper-analysis skill
Author `.claude/skills/orcaflex-jumper-analysis/SKILL.md` describing the sanitized jumper library: which models cover which configurations (M-shape, lazy-S, steep-S), which environmental conditions, and how to drive a parametric jumper analysis from the new `spec.yml`. This skill becomes the entry point for downstream WRK-032/036/045.

## Acceptance Criteria

- [ ] `scripts/sanitize_s7_models.py` and `scripts/extract_s7_specs.py` are present, lint-clean, and pass a smoke test on a 3-file fixture.
- [ ] `digitalmodel/.legal-deny-list.yaml` is committed and covers all client/project/vessel terms identified in s7.
- [ ] `docs/modules/orcaflex/{jumper,installation,mooring,training}/` populated with sanitized YAMLs (≥235 total across categories, allowing for unclassified residue).
- [ ] `legal-sanity-scan` skill exits 0 against every committed YAML; exits non-zero against an injected deny-list term (test fixture).
- [ ] Round-trip validation: ≥95% of jumper + mooring YAMLs survive 3-way conversion byte-modulo-comments.
- [ ] `orcaflex-jumper-analysis` SKILL.md present and references at least one sanitized model from the new library.

## Open questions

- The issue references the s7 tree by name but does not point at a path on the licensed machine. Confirm the absolute path and whether s7 is a git submodule, sibling repo, or shared drive before Task 3 begins.
- Categorization heuristics (filename glob) will mis-bucket some files; is a manual triage pass acceptable, or should the script defer ambiguous files to `unclassified/` and require human routing?
- Round-trip tolerance: ≥95% byte-equality is loose. If we want stricter (≥99%), we likely need to extend the format converter's CRLF/comment normalization first — flag if that becomes the long pole.
