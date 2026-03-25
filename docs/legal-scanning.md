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
