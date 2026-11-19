# Reusable prompts

Article: "Fitness for service of corroded pipelines: strength and remaining useful life (RUL)", The Deckhand Chronicles, revision 5 of 19 Nov 2026. This page holds in full what Appendix B of the article summarises.

This prompt reruns the published assessment from its pinned data set, and checks the Section A pressures and life projections against the published values.

## What you need to run this

- Data required: none; the worked-example inputs are in our public code repository. Git, uv and Python 3.11. Allow about 3.5 GB of disk and about 10 minutes.
- Data required to adapt: wall readings with their positions, pipe dimensions and material, design pressure, loads, and a corrosion rate, ideally from repeat inspection.
- Paid or behind an account: nothing to run it. The cited standards are paid documents; the code needs no copy of them.
- Changes over time: nothing for the result; the code and inputs are fixed at one commit. Python package versions can change: only plotly is pinned, so record the versions you get (uv pip freeze).

## Reproduce the published result

Give this prompt to your assistant:

```text
Use https://vamseeachanta.github.io/digitalmodel/ffs/pipeline-ffs-assessment.html
and https://github.com/vamseeachanta/digitalmodel at the full pinned commit
09089a121a16069c4b657d1a320a630a23ab1dd5. Use Python 3.11 and uv.
1. In an empty folder with a short path (on Windows, also run
   git config --global core.longpaths true), install with Python 3.11:
     git clone --filter=blob:none https://github.com/vamseeachanta/digitalmodel
     cd digitalmodel
     git checkout 09089a121a16069c4b657d1a320a630a23ab1dd5
     uv venv --python 3.11
     uv pip install --no-sources --no-deps -e .
     uv pip install "numpy<2" "pandas>=2,<3" pyyaml pint jinja2 pydantic plotly==5.17.0 scipy
   The second install gives only the packages this assessment needs; the
   full dependency set of the library is not needed.
2. The exact entry point is scripts/reporting/build_pipeline_ffs_report.py.
   Its configuration is examples/workflows/pipeline-ffs-assessment/input.yml;
   assumptions.yml is beside it. It reads the pinned Section A input from
   examples/workflows/pipeline-corroded-defect-screen/synthetic.yml and the
   Section B input from examples/workflows/ffs-metal-loss/input.yml, which also sets the allowable strength factor.
   Preserve these files. Section A has outside diameter 24 in, nominal wall
   0.500 in, SMYS 52,000 psi, tensile strength 66,700 psi, design pressure
   800 psi, axial stress 10,000 psi and circumferential width 4 in.
   Axial positions are 0, 2, 4, 6, 8 in. The three-column grid, in inches, is:
   [0.500,0.500,0.500]; [0.400,0.300,0.450]; [0.350,0.200,0.400];
   [0.450,0.250,0.500]; [0.500,0.500,0.500].
   The assessment rate is 0.005 in/year, with half-rate and double-rate cases.
   The shared allowable remaining strength factor is 0.90.
3. In PowerShell, set $env:PYTHONPATH='src', then run:
   .venv/Scripts/python.exe scripts/reporting/build_pipeline_ffs_report.py
   On POSIX use PYTHONPATH=src .venv/bin/python scripts/reporting/build_pipeline_ffs_report.py.
4. The run rewrites docs/api/ffs/pipeline-ffs-assessment.manifest.json.
   Compare it with the committed version (git diff on that path shows no
   numerical change) and read assessment.sections[0] for the values below.
   Save the manifest and the console log; then delete the clone and venv.
Check: minimum wall 0.200 in from 15 readings; loss 60.0 %; reduced rating
1,022 psi [7.046 MPa], 127.7 % of 800 psi; strength factor 0.5896 against
0.90; allowable pressures in order B31G, Modified B31G, RSTRENG,
RSTRENG-2D, DNV-RP-F101: 1,246, 1,210, 1,417, 1,417, 1,165 psi;
failure pressures: 1,732, 1,682, 1,969, 1,969, 1,797 psi; allowable spread
21.6 % above the lowest; reduced rating 12.3 % below the lowest allowable.
At rates 0.0025, 0.005, 0.01 in/year, lives are 23.09, 11.54 and
5.772 years (shown as 23.1, 11.5 and 5.8); the life-limit wall is
0.1423 in. Values agree to 4 significant figures.
```

## Adapt it to your project

```text
Before you start, ask me for: my wall readings with their axial and
circumferential positions; inspection dates and measurement uncertainty;
pipe outside diameter, nominal wall, material strengths, design code and
edition, location class and design factor; design pressure and loads; and
a corrosion rate, ideally from repeat inspections.
Reproduce the published worked example first. Then copy
examples/workflows/pipeline-corroded-defect-screen/synthetic.yml, replace
its pipe data, grid (rows of readings at the axial positions) and rates
with mine, keeping the field names and units, and point the assessment
input at the copy. Report allowable pressure separately from failure
pressure, and state the factor applied to each method. Check the reduced
local-loss rating against design pressure and the strength factor against
its allowable value. Project life at half, one and two times my rate.
Check the applicability limits of API 579-1 Part 5 for my case, and any
other failure mode that may govern.
Where a step needs a paid standard I do not hold, cite the standard and
Part from the article and mark it "to be checked".
```

Public data set: our corroded-pipeline worked example in digitalmodel, published by AceEngineer, MIT licence, worked-example readings, retrieved 12 Nov 2026 at commit 09089a121a16069c4b657d1a320a630a23ab1dd5. <https://github.com/vamseeachanta/digitalmodel/tree/09089a121a16069c4b657d1a320a630a23ab1dd5/examples/workflows/pipeline-ffs-assessment>
