# Plan: digitalmodel #509 — Pre-commit hook for YAML-strict validation on spec changes

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/509
**Status:** plan-review
**Tier:** T3

## Context

`yaml_validator.py` and `post_validator.py` are landed (`src/digitalmodel/solvers/orcaflex/`). To prevent silent regression, every commit that touches a `spec.yml` should validate it against `ProjectInputSpec` before landing. Currently this is left to ad hoc `scripts/audit_all_specs.py` invocations. Workspace policy (`.claude/rules/patterns.md`) puts pre-commit hooks at enforcement Level 3 — strongest available, fires automatically.

`scripts/audit_all_specs.py` already exists and walks the full spec tree. The new hook script reuses its validator imports but operates only on git-staged files for speed.

## Plan

1. **Confirm validator API.** Read `src/digitalmodel/solvers/orcaflex/yaml_validator.py` and `scripts/audit_all_specs.py` to identify the exact class/function used for single-file validation. Capture the import path and any required setup (config dirs, schema discovery). The hook script must use the same validator to guarantee parity with `audit_all_specs`.

2. **Author hook script.** New file `scripts/check_spec_validity.sh`:
   - reads staged files via `git diff --cached --name-only --diff-filter=ACM`
   - filters to paths matching `spec.yml` or `*.spec.yml` (confirm pattern by grepping existing specs in repo)
   - invokes `uv run python -c "from digitalmodel.solvers.orcaflex.yaml_validator import validate_spec_file; ..."` for each
   - exits non-zero with file:line:reason on first failure (don't continue silently)
   - prints "OK: N specs validated" on success
   Keep under 50 lines, no Python wrapper file unless the inline `-c` becomes unreadable.

3. **Wire into `.pre-commit-config.yaml`.** Read existing `.pre-commit-config.yaml` to identify the hook style in use (local vs. external). Add a new `local` hook entry:
   ```yaml
   - id: orcaflex-spec-validate
     name: OrcaFlex spec.yml strict validation
     entry: scripts/check_spec_validity.sh
     language: script
     files: '\\.spec\\.yml$|spec\\.yml$'
     pass_filenames: false
   ```
   Use `pass_filenames: false` because the script reads from `git diff --cached` directly — avoids re-invocation per file.

4. **Test against real specs.** Stage one valid spec change → confirm hook passes. Stage a deliberately invalid edit (e.g., `water_depth: -10000` violating schema) → confirm hook exits non-zero with a useful message. Stage no specs → confirm hook exits 0 fast.

5. **Smoke check.** Run `pre-commit run orcaflex-spec-validate --all-files` once to validate every existing spec via the hook (sanity that current main is clean). Document the result count in the issue.

## Acceptance Criteria

- [ ] `scripts/check_spec_validity.sh` exists, executable, validates only staged spec files
- [ ] `.pre-commit-config.yaml` has new `orcaflex-spec-validate` hook entry
- [ ] Hook exits non-zero with file path + reason on invalid spec
- [ ] Hook exits 0 fast when no spec files staged
- [ ] `pre-commit run orcaflex-spec-validate --all-files` passes on current main
- [ ] Hook script reuses the same validator function as `scripts/audit_all_specs.py` (no divergent validation logic)
