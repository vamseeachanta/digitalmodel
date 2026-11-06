# Run it yourself: generic hull roll reduction with a U-tube anti-roll tank

Article: https://www.aceengineer.com/newsletter/issue-001-energy-street/ (section "Tank sloshing")
Study page: https://vamseeachanta.github.io/digitalmodel/cfd/generic-hull-roll-screening.html

For decision makers: this prompt lets your team, or your AI assistant, repeat the screening estimate with public code. If you would rather have it done for you, or done for your own vessel, use the contact link on the article page.

For practitioners: paste the block below into an AI assistant that can run Python and use the internet. It needs only the Python standard library and a few minutes. The key results it must reach are in `results.json` next to this file. `eval.py` grades them.

## Prompt (copy all of it)

```text
Task: reproduce the peak roll reduction of a generic hull with a U-tube anti-roll
tank, as stated in AceEngineer newsletter issue 1
(https://www.aceengineer.com/newsletter/issue-001-energy-street/) and its study page
(https://vamseeachanta.github.io/digitalmodel/cfd/generic-hull-roll-screening.html).

Inputs (all public, no account needed):
- Code: https://github.com/vamseeachanta/digitalmodel at commit
  8d08ea24198dcb99217df82325688a331f8aefce, MIT licence:
  scripts/capabilities/build_generic_hull_screening.py (the calculation) and
  src/digitalmodel/hydrodynamics/utube_tank.py (the public U-tube model).
- The inputs are set in the builder script and listed on the study page: legs 20 m
  long and 6 m wide, fill depth 5 m, centroid offset 5 m, conduit area 6.8 m2,
  sea water 1025 kg/m3; hull roll periods 18 s and 21 s; hull damping 2 % and 5 %
  of critical; alpha 0.02, 0.05, 0.10, 0.15 and 0.20. Alpha is the tank's
  quasi-static free-surface moment stiffness divided by the hull's roll restoring.

Steps:
1. git clone --filter=blob:none https://github.com/vamseeachanta/digitalmodel
   cd digitalmodel && git checkout 8d08ea24198dcb99217df82325688a331f8aefce
   Use Python 3.10 or later. No package install is needed.
2. Load scripts/capabilities/build_generic_hull_screening.py as a module (for
   example with importlib) and call compute(). Do not run its main(): that
   rewrites the study page.
3. From record["cases"], read reduction_percent, tank_peak_period_s and
   long_period_penalty_percent for each roll period, damping and alpha.
4. Compare the 20 cases with docs/api/cfd/data/generic_hull_roll_screening.json.

Expected key results (from the article and the study page):
- 18 s hull roll period, alpha 0.05: peak roll reduction 46.1 % at 2 % hull damping
  and 23.3 % at 5 %. The 2 % peak lies at an 18.5 s forcing period.
- 18 s, 2 % damping, alpha 0.02 / 0.10 / 0.15 / 0.20: 24.3 / 61.4 / 66.0 / 67.2 %.
- 18 s, 5 % damping, same alphas: 10.5 / 37.9 / 45.6 / 48.7 %.
- At 60 s forcing, 18 s hull, 2 % damping: 6.0 % more roll at alpha 0.05 and 29.5 %
  at alpha 0.20, against the 25.0 % slow-forcing limit 1/(1 - alpha) - 1.

Ask me before you go on if:
- you want to apply it to my vessel: ask me for the hull roll period, the roll
  damping (% of critical), the roll restoring and the tank dimensions, and say
  which of them changes the reduction most.

You cannot:
- rebuild the tank moment fit: it comes from 35 CFD cases on a different
  connected-tank geometry, published as aggregate results only;
- validate the result against a real vessel: no model or full-scale test is cited;
- treat the result as a design value: it is a screening estimate on invented tank
  geometry, and the 18 s results lie below the 20 s band in which the tank moment
  fit is validated.

Output: a table of your values against the expected values, and a file
my-results.json in this form, with the result ids of
https://github.com/vamseeachanta/digitalmodel/blob/main/examples/report-evals/issue-001-hull-roll-screening/results.json
{"results": [{"id": "reduction_18s_2pct_alpha005", "value": 46.1}, ...]}
Then grade it. The eval files are on the main branch, so bring them into the clone:
git checkout origin/main -- examples/report-evals
python examples/report-evals/issue-001-hull-roll-screening/eval.py --compare my-results.json
```

## Check your answer

From the clone, after `git checkout origin/main -- examples/report-evals`:

```text
python examples/report-evals/issue-001-hull-roll-screening/eval.py --compare my-results.json
python examples/report-evals/issue-001-hull-roll-screening/eval.py
```

The first line grades your own results. The second rebuilds every key result itself. `PASS` means every key result is within its tolerance (0.05 percentage points for the reductions).

## Adapt it to your project

Change the roll periods, damping ratios and alphas in the builder's `compute()` call, or call `screen_case(roll_period, damping, alpha)` directly. Keep the forcing band inside 0.7 to 1.3 times the roll period. Results below a 20 s forcing period lie outside the band in which the tank moment fit is validated.
