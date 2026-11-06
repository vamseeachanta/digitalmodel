# Report evals

Each folder here belongs to one public AceEngineer report whose code lives in this repository. A folder holds four files:

| File | What it is |
|---|---|
| `results.json` | The key results of the report: value, unit, period, source, method, repository and pinned commit, tolerance and limits. It follows `results.schema.json` (also served at https://www.aceengineer.com/assets/data/report-evals/results.schema.json). |
| `eval.py` | Rebuilds the key results from this repository and public data, and compares them with `results.json`. With `--compare my-results.json` it grades a reader's own results instead. |
| `PROMPT.md` | The run-it-yourself prompt for an AI assistant. It names its inputs, asks the reader for any missing data, and says what it cannot do. |
| `last-run.json` | The record of the last eval run committed with the folder. |

`evalkit.py` holds the shared comparison code. It uses only the Python standard library.

## Reports in this repository

| Folder | Report | Needs |
|---|---|---|
| `issue-002-cp-anode-editions` | Sacrificial anodes for six offshore structures on each DNV-RP-B401 and DNV-RP-F103 edition (newsletter issue 2) | digitalmodel installed on Python 3.11; network for the BSEE file |
| `issue-001-hull-roll-screening` | Generic hull roll reduction with a U-tube anti-roll tank (newsletter issue 1) | standard library only |
| `issue-001-sloshing-frequency` | 2D tank sloshing first-mode frequency, OpenFOAM against linear theory (newsletter issue 1) | standard library only |

## Run

```text
python examples/report-evals/<folder>/eval.py                 # rebuild and compare
python examples/report-evals/<folder>/eval.py --out run.json  # also write the run record
python examples/report-evals/<folder>/eval.py --compare my-results.json
python examples/report-evals/<folder>/eval.py --validate-only
```

Exit code 0 is PASS, 2 is PARTIAL and 1 is FAIL.

## Grades

Each result gets one status:
- `MATCH`: within the tolerance of `results.json`.
- `DRIFT`: outside the tolerance, on a live source that changes (for example a daily BSEE file).
- `MISMATCH`: outside the tolerance, on pinned inputs.
- `NOT_EVALUATED`: the run could not produce it (missing reader data, no network, `--skip-live`).
- `ERROR`: the code raised an error.

The report grade is PASS when every result matches, PARTIAL when any result drifted or was not evaluated, and FAIL when any result mismatched or raised an error.
