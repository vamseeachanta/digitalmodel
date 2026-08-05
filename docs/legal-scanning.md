# Legal Scanning — CP Stream Repos

> WRK-278 | Updated: 2026-02-20

## Deny List Files

| Repo | File | Patterns |
|------|------|----------|
| `digitalmodel` | `digitalmodel/.legal-deny-list.yaml` | s7 OrcaFlex model sources + Yellowtail (GYYT) + Woodfiber LNG (WLNG/350106/B1522) |
| `saipem` | `saipem/.legal-deny-list.yaml` | Yellowtail/GYYT/GYFI, ExxonMobil, SBM Offshore, EEPGL, Prosperity |
| `acma-projects` | `acma-projects/.legal-deny-list.yaml` | Woodfiber/WLNG/B1522, PSVM/BP Angola, MC252, FST-1/FST-2, CB&I |

All three lists extend the global `workspace-hub/.legal-deny-list.yaml`.

## Running the Scan Manually

```bash
# From workspace-hub root
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel
bash scripts/legal/legal-sanity-scan.sh --repo=saipem
bash scripts/legal/legal-sanity-scan.sh --repo=acma-projects
bash scripts/legal/legal-sanity-scan.sh --all
```

Exit codes: `0` = pass, `1` = block violations found.

## Pre-Commit Hook (digitalmodel)

`digitalmodel/.pre-commit-config.yaml` contains a `legal-sanity-scan` hook that runs
`scripts/legal/legal-sanity-scan.sh --repo=digitalmodel` at the `pre-commit` stage.
It does not receive filenames — it always scans the whole tree.

Install: `pre-commit install` inside the `digitalmodel` submodule.

## Manual Gate — saipem and acma-projects

Neither `saipem` nor `acma-projects` has a `.pre-commit-config.yaml`.
Run the scan manually before raising any PR from these repos:

```bash
bash scripts/legal/legal-sanity-scan.sh --repo=saipem
bash scripts/legal/legal-sanity-scan.sh --repo=acma-projects
```

The scan must exit 0 before the PR can proceed (legal-compliance rule: block severity).

## Pre-Existing Violations

The following files contain client identifiers intentionally as part of the
sanitization mapping logic. They are excluded via `exclusions:` in the deny list:

- `digitalmodel/scripts/sanitize_s7_models.py` — maps raw client names to generic
  equivalents; the patterns must appear here to perform the substitution.
- `digitalmodel/scripts/extract_s7_specs.py` — source extraction helper.
- `digitalmodel/docs/domains/cathodic_protection/saipem_cp_comparison_analysis.md` —
  pre-WRK-278 analysis document referencing source document numbers; not yet excluded.
- `digitalmodel/docs/domains/cathodic_protection/standards-inventory.md` — lists
  file paths that include repo names; paths, not code.

Do NOT modify `sanitize_s7_models.py` or `extract_s7_specs.py` to remove these
references — they are the sanitization source of truth.

---

## Correction — what this document describes does not exist here (#1961, 2026-08-05)

Everything above was written against a cross-repository arrangement. Measured
against this repository's tracked tree, three of its claims do not hold:

| Claim above | Measured state |
|---|---|
| `digitalmodel/.legal-deny-list.yaml` exists | **Absent.** `git ls-files` returns no deny-list file at any tracked path. |
| `scripts/legal/legal-sanity-scan.sh` can be run for this repo | **Absent.** No `scripts/legal/legal-sanity-scan.sh` is tracked here. The pre-commit hook pointed at `../scripts/legal/legal-sanity-scan.sh`, i.e. outside the repository, which does not resolve from a worktree. |
| The scan is a gate | **It was fail-open.** The `--repo=` form is under an open upstream defect: it was observed printing a passing result with exit code 0 over a worktree that contained a live leak, and the same form returned a pass both before and after a change that removed real identifiers. |

That is the same defect class the scanner below replaces: a check that reports
green while verifying nothing. The last two identifier leaks found in this
repository were both found by manual sweep, not by any check.

### What replaced it

| Path | Role |
|---|---|
| `scripts/legal/check_protected_identifiers.py` | Fail-closed, byte-oriented scanner over the complete tracked tree, the staged index, a pinned historical tree, or a commit-message file. |
| `scripts/legal/protected_surface_ownership.py` | Schema authority. A boundary rule and a whole-file exemption are both unrepresentable; every classification must carry a non-empty reason. |
| `scripts/legal/protected-surface-v1.json` | Versioned scope manifest: rule identifiers, matcher contract, content-addressed field declarations, limits, pinned oracle. |
| `scripts/legal/public_surface_snapshot.py` | Structural snapshot of the public Python surface, built from Git blobs. |
| `scripts/legal/verify_public_surface.sh` | The three verifications that do not route through the test suite. |

### Rule values are not stored in this repository

The manifest declares rule **identifiers** and their matcher contract. It
carries no rule **values**. Values are supplied at run time through `--rules`,
from a file kept outside the repository:

```bash
export PROTECTED_RULES_FILE=/path/outside/this/repo/rules.json
scripts/legal/verify_public_surface.sh --all
```

This is not decoration. The scanner enumerates its own tree, so a value
committed anywhere here would make the tool fail on itself — which is exactly
what the self-coverage case pins.

Without that file the scanner reports its authority as `UNAUTHENTICATED`,
returns a distinct exit code rather than a clean one, and asserts nothing about
production-clean state.

### Exit codes

`0` clean · `1` findings · `2` manifest schema error · `3` rule authority
unavailable · `4` fail-closed (unclassified path, oversize artifact, unreadable
artifact).

### Stage 2 is not built

Authenticated current-snapshot validation, anti-rollback, real rule values, the
protected maintainer workflow, fork-CI separation, and any production-clean
assertion are all blocked on external provisioning that has not started. The CI
job prints an `UNAUTHENTICATED` banner and is not a required status check —
this repository defines none.

### Known remaining gap

The gitleaks hook in `.pre-commit-config.yaml` still passes
`--config ../.gitleaks.toml`, which has the same unresolvable relative path as
the hook removed in this pass. It is recorded here rather than changed silently.
