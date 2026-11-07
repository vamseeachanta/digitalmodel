# Report evals

Each public AceEngineer report whose code or data lives in a public repository has one folder of report evals in that repository:
- digitalmodel and worldenergydata: `examples/report-evals/<report-id>/`
- aceengineer-website: `assets/data/report-evals/<report-id>/`, also served under https://www.aceengineer.com/assets/data/report-evals/

This README, `evalkit.py` and `results.schema.json` are the same, byte for byte, in all three repositories.

## Files of a report

| File | What it is |
|---|---|
| `results.json` | The key results of the report: value, unit, period, source, method, repository and pinned commit, tolerance, basis (fixed or live), scope and limits. Also the inputs, the missing data and what the work cannot do. It follows `results.schema.json`, served at https://www.aceengineer.com/assets/data/report-evals/results.schema.json. |
| `eval.py` | Rebuilds the key results from public code and data and compares them with `results.json`. With `--compare my-results.json` it grades a reader's own results instead. |
| `PROMPT.md` | The run-it-yourself prompt for an AI assistant. It names its inputs, asks the reader for missing data, and says what it cannot do. |
| `last-run.json` | The record of the last rebuild run with the default revision; `last-run-pinned.json`, where present, is the record of a `--revision pinned` run. Each is bound to the bytes of `results.json`, `eval.py` and `evalkit.py` that it graded. |

`evalkit.py` holds the shared code. It uses only the Python standard library.

## Reports

| Report | Repository | Folder |
|---|---|---|
| Sacrificial anodes for six offshore structures on each DNV-RP-B401 and DNV-RP-F103 edition (newsletter issue 2) | digitalmodel | `issue-002-cp-anode-editions` |
| Generic hull roll reduction with a U-tube anti-roll tank (newsletter issue 1) | digitalmodel | `issue-001-hull-roll-screening` |
| 2D tank sloshing first-mode frequency, OpenFOAM against linear theory (newsletter issue 1) | digitalmodel | `issue-001-sloshing-frequency` |
| Gulf of Mexico well permits by rig type (newsletter issue 2) | worldenergydata | `issue-002-gom-well-permits` |
| Jack-up site-specific analysis (newsletter issue 2) | aceengineer-website | `issue-002-jackup-site-assessment` |

## Run

```text
python <folder>/eval.py                          # rebuild from this checkout and compare
python <folder>/eval.py --revision pinned        # rebuild from code.commit (historical reproduction)
python <folder>/eval.py --out run.json           # choose where the run record goes
python <folder>/eval.py --compare my-results.json
python <folder>/eval.py --validate-only          # schema and grading rules; no network, no report code
python <folder>/eval.py --check-record <folder>/last-run.json
```

Exit code 0 is PASS, 2 is PARTIAL and 1 is FAIL. Every run writes a record: to `--out`, or else to a new, uniquely named file under `<temp>/report-evals-runs/` that is never replaced. A record names the head of the repository holding the eval files (`artifacts_repo_head`) and whether those files had uncommitted changes (`artifacts_dirty`). `--check-record` fails when a file the record is bound to has changed or is missing.

`--revision candidate` (the default, except where a folder states otherwise) uses the code and repository inputs of the checkout. `--revision pinned` reads them from `code.commit` through git. Code that is an installed package (digitalmodel) must be installed from a checkout of `code.commit` for a pinned run. A folder may also commit `last-run-pinned.json`, the record of a pinned run.

## Grades

Each result gets one status:
- `MATCH`: within the tolerance of `results.json`.
- `DRIFT`: outside the tolerance, on a live source that changes (for example a daily BSEE file).
- `MISMATCH`: outside the tolerance, on pinned inputs, or a value of the wrong type.
- `NOT_EVALUATED`: no value (missing reader data, a source that cannot be reached, an optional package not installed).
- `ERROR`: the check raised an error. Other checks of the same run still grade, and the run record is still written.

Tolerance: `abs` t passes when |got - expected| <= t; `rel_pct` r passes when |got - expected| <= r/100 x |expected|. Both bounds are inclusive. Two integers compare exactly; otherwise an allowance of 1e-9 x |expected| covers binary representation, so a zero reference is exact.

The report grade is FAIL when any result is MISMATCH or ERROR, or when the rebuild raised an unexpected error, PARTIAL when any result is DRIFT or NOT_EVALUATED, and PASS otherwise. Scope `both` (default) is graded in every mode, `rebuild` only when the eval rebuilds, and `pinned` only in a `--revision pinned` rebuild. A reader file that cannot be read, or that has an unknown or repeated id, grades FAIL; so does a number too large to compare.

## Schema changes

- 7 November 2026: scope `pinned` added. The change is additive: every document that was valid before is still valid, so `schema_version` stays 1.0. Until the website serves this version at the URL above, validate a folder that uses scope `pinned` against the copy in this folder.
