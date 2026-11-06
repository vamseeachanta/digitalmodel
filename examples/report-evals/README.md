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
| `last-run.json` | The record of the last rebuild run. It is bound to the bytes of `results.json`, `eval.py` and `evalkit.py` that it graded. |

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
python <folder>/eval.py                          # rebuild and compare
python <folder>/eval.py --out run.json           # also write the run record
python <folder>/eval.py --compare my-results.json
python <folder>/eval.py --validate-only          # schema and grading rules; no network, no report code
python <folder>/eval.py --check-record <folder>/last-run.json
```

Exit code 0 is PASS, 2 is PARTIAL and 1 is FAIL. `--check-record` fails when the record is not bound to the current files.

## Grades

Each result gets one status:
- `MATCH`: within the tolerance of `results.json`.
- `DRIFT`: outside the tolerance, on a live source that changes (for example a daily BSEE file).
- `MISMATCH`: outside the tolerance, on pinned inputs, or a value of the wrong type.
- `NOT_EVALUATED`: no value (missing reader data, a source that cannot be reached, an optional package not installed).
- `ERROR`: the check raised an error. Other checks of the same run still grade, and the run record is still written.

Tolerance: `abs` t passes when |got - expected| <= t; `rel_pct` r passes when |got - expected| <= r/100 x |expected|, and is exact when the expected value is 0. Both bounds are inclusive.

The report grade is FAIL when any result is MISMATCH or ERROR, PARTIAL when any result is DRIFT or NOT_EVALUATED, and PASS otherwise. In `--compare` mode only results with scope `both` are graded; a reader file with an unknown or repeated id grades FAIL.
