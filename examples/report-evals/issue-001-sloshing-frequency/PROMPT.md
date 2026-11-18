# Run it yourself: 2D tank sloshing frequency, OpenFOAM against linear theory

Article: https://www.aceengineer.com/newsletter/issue-001-energy-street/ (section "Tank sloshing")
Study page: https://vamseeachanta.github.io/digitalmodel/cfd/sloshing-cfd-study.html

For decision makers: this prompt lets your team, or your AI assistant, check the sloshing result with public code and the archived CFD outputs. If you would rather have it done for you, or for your own tank, use the contact link on the article page.

For practitioners: paste the block below into an AI assistant that can run Python and use the internet. The check needs only the Python standard library and a few minutes. Rerunning the CFD itself is optional and needs OpenFOAM on Linux. The key results are in `results.json` next to this file. `eval.py` grades them.

## Prompt (copy all of it)

```text
Task: check the statement of AceEngineer newsletter issue 1
(https://www.aceengineer.com/newsletter/issue-001-energy-street/) that a 2D
OpenFOAM sloshing model reproduced the tank's small-amplitude first-mode frequency
within about 0.30 % of linear theory across the tested fill levels. The study page
is https://vamseeachanta.github.io/digitalmodel/cfd/sloshing-cfd-study.html.

Inputs (all public, no account needed):
- Archived CFD outputs in https://github.com/vamseeachanta/digitalmodel at commit
  705857af62385a4567c96be99a129cdff3051a52, MIT licence:
  docs/api/structural/sloshing-cfd-benchmark.json (free-decay runs at h/L 0.15,
  0.30, 0.50 and 0.70; tank breadth L = 1 m; g = 9.80665 m/s2) and
  docs/api/cfd/sloshing-convergence.json (five meshes and three timesteps at
  h/L 0.30).
- Linear theory for a rectangular tank, first mode:
  omega1 * sqrt(L/g) = sqrt(pi * tanh(pi * h / L)).

Steps:
1. git clone --filter=blob:none https://github.com/vamseeachanta/digitalmodel
   cd digitalmodel && git checkout 705857af62385a4567c96be99a129cdff3051a52
   Use Python 3.10 or later. No package install is needed.
2. For each free-decay point, compute the linear-theory value and the measured
   value 2 * pi * freq_meas_hz * sqrt(L / g), then the error in percent.
3. For the mesh study, compute the linear frequency in Hz for L = 1 m, h/L = 0.30,
   the error of each mesh, and the Richardson extrapolation on the ratio-2 meshes
   40, 80 and 160 cells per breadth: observed order p, extrapolated frequency and
   the fine-grid GCI with a safety factor of 1.25.

Expected key results (from the study page):
- omega1 linear / CFD / error: h/L 0.15 1.1746 / 1.1779 / 0.28 %; h/L 0.30
  1.5210 / 1.5164 / 0.30 %; h/L 0.50 1.6974 / 1.6974 / 0.00 %; h/L 0.70
  1.7508 / 1.7508 / 0.00 %. Largest error 0.30 %.
- Mesh study: linear frequency 0.758054 Hz; finest-mesh error 0.296 %; p = 0.191;
  extrapolated frequency 0.756556 Hz; GCI 0.1239 %.

Ask me before you go on if:
- you are to rerun the CFD: ask whether I have a Linux machine with OpenFOAM ESI
  v2312. Each free-decay case took 520 to 660 s on our workstation. The case
  scripts are scripts/cfd/run_sloshing_benchmark.py and
  scripts/cfd/run_sloshing_convergence.py. Without OpenFOAM, use the archived
  outputs and say so;
- you want to apply it to my tank: ask for the tank breadth, fill depth and
  shape. The closed form holds for a rectangular tank only.

You cannot:
- rerun the CFD inside a chat session without OpenFOAM;
- say anything about 3D or violent sloshing or impact pressures: the study covers
  the planar first mode of a 2D tank at small amplitude (0.02 m perturbation).

Output: a table of your values against the expected values, and a file
my-results.json in this form, with the result ids of
https://github.com/vamseeachanta/digitalmodel/blob/main/examples/report-evals/issue-001-sloshing-frequency/results.json
{"results": [{"id": "error_pct_hl30", "value": 0.30}, ...]}
Then grade it. The eval files are on the main branch, so bring them into the clone:
git checkout origin/main -- examples/report-evals
python examples/report-evals/issue-001-sloshing-frequency/eval.py --compare my-results.json
```

## Check your answer

From the clone, after `git checkout origin/main -- examples/report-evals`:

```text
python examples/report-evals/issue-001-sloshing-frequency/eval.py --compare my-results.json
python examples/report-evals/issue-001-sloshing-frequency/eval.py
```

The first line grades your own results. The second rebuilds every key result itself from the archived CFD outputs.

## Adapt it to your project

For a rectangular tank of your own, change L and h in the closed form to get the linear first-mode frequency. Compare it with the roll period of the vessel. To check it with CFD, run `scripts/cfd/run_sloshing_benchmark.py` with OpenFOAM v2312 and your tank breadth and fill.
