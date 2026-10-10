# Compute program: first results (2026-10-09)

Three short case-study pages from the first evening of the compute program
(workspace-hub#4034). All runs used digitalmodel at `4f7bfc0c` on the primary
Linux CFD host. Machines are named by role only.

| Page | What it is | State |
|---|---|---|
| `openfoam-known-answer-rerun.html` | Six textbook OpenFOAM cases re-run from the committed case files on a second machine, set beside the published figures | Five complete, wave tank still running when this was built |
| `barge-diffraction-capytaine.html` | The baseline pack's box barge solved with Capytaine 3.0.0 | Open-source leg only; OrcaWave and AQWA runs not yet recorded |
| `openfoam-baseline-dev-primary.html` | First receipt of the solver baseline pack (#2300): OpenFOAM timing and repeatability | One machine; licensed solvers and the second Linux host not yet baselined |

`barge-capytaine.json` holds the Capytaine results at all 20 frequencies.
`capytaine_barge.py` produced it; `build_reports.py` built the pages from the
run outputs. Baseline receipts are kept outside this repository, as #2300
requires; the baseline page quotes their timings and drag coefficients.

The pages were generated on a headless host and have not been checked in a
browser.
