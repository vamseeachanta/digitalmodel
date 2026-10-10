# Compute program: first results (2026-10-09)

Three short case-study pages from the first evening of the compute program
(workspace-hub#4034). All runs used digitalmodel at `4f7bfc0c` on the primary
Linux CFD host. Machines are named by role only.

| Page | What it is | State |
|---|---|---|
| `openfoam-known-answer-rerun.html` | Seven textbook OpenFOAM cases re-run from the committed case files on a second machine, set beside the published figures | Seven complete. Six reproduce the published figures; the wave tank differs on one of four measures (height decay 5.0 % against a published 4.6 %) |
| `barge-diffraction-capytaine.html` | The baseline pack's box barge solved with Capytaine 3.0.0 | Open-source leg only; OrcaWave and AQWA runs not yet recorded |
| `openfoam-baseline-dev-primary.html` | First receipt of the solver baseline pack (#2300): OpenFOAM timing and repeatability | One machine; licensed solvers and the second Linux host not yet baselined |

`barge-capytaine.json` holds the Capytaine results at all 20 frequencies.
`capytaine_barge.py` produced it; `build_reports.py` built the pages from the
run outputs. Baseline receipts are kept outside this repository, as #2300
requires; the baseline page quotes their timings and drag coefficients.

The pages were generated on a headless host and have not been checked in a
browser.

The wave tank analysis script uses `numpy.trapz`, which NumPy 2 removed; it was run here with
`numpy.trapz = numpy.trapezoid` patched in.
