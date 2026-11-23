# Reusable prompts

Article: "Fitness for service of corroded pipelines: strength and remaining useful life (RUL)", The Deckhand Chronicles, revision 14 of 23 Nov 2026. This page holds in full what Appendix B of the article summarises.

The first prompt reruns the measured-scan assessment from its pinned data set, and checks its pressures and life projections against the published values. The second does the same for the synthetic grid, Section A, the comparison case. The third adapts the assessment to your own readings.

## What you need to run this

- Data required: none; the measured scan and the worked-example inputs are in the public digitalmodel repository. Git, uv and Python 3.11. Allow about 3.5 GB of disk and about 10 minutes.
- Data required to adapt: wall readings with their positions, pipe dimensions and material, design pressure, loads, and a corrosion rate, ideally from repeat inspection.
- Paid or behind an account: nothing to run it. The cited standards are paid documents; the code needs no copy of them.
- Changes over time: nothing for the result; the code and inputs are fixed at one commit. Python package versions can change: only plotly is pinned, so record the versions you get (uv pip freeze).

## Reproduce the measured-scan result

Give this prompt to your assistant:

```text
Use https://github.com/vamseeachanta/digitalmodel at the full pinned commit
193bfe3086f67a49ce163fa05324308a35f0cff0. Use Python 3.11 and uv.
1. Install as in step 1 of the synthetic-grid prompt below, but check out
   193bfe3086f67a49ce163fa05324308a35f0cff0.
2. The exact entry point is scripts/ffs/c3_measured_grid_assessment.py. It
   reads the anonymized scan
   tests/asset_integrity/test_data/real_inspection/ut_grid_scan-a_window_fullres.csv
   (no header; rows at a 4 mm axial pitch by 64 circumferential
   positions; wall thickness in mm; blank cells are unmeasured). Read
   README.md in that folder for its provenance. The script calls
   evaluate_section from scripts/reporting/build_pipeline_ffs_report.py,
   the function that rates Section A. Preserve these files.
   Pipe: the outside diameter and nominal wall set in the script (about
   21 in and 0.9 in, rounded), X80 (SMYS 80,000 psi, tensile strength
   90,600 psi). Assumed: design pressure
   2,400 psi, axial stress 10,000 psi, ASME B31.8 design factor 0.72,
   allowable remaining strength factor 0.90, corrosion rate 0.253 mm/year
   (from the fixture's results register) with half-rate and double-rate cases.
3. In PowerShell, set $env:PYTHONPATH='src', then run:
   .venv/Scripts/python.exe scripts/ffs/c3_measured_grid_assessment.py --out check
   On POSIX use PYTHONPATH=src .venv/bin/python with the same arguments.
4. Compare check/measured-grid-results.json and check/measured-grid-window.csv
   with the committed files in
   docs/articles/fitness-for-service-corroded-pipelines/data (no numerical
   change). Save them and the console log; then delete the clone and venv.
Check: the reading and blank counts, the thinnest reading, the patch rows
and columns and the flaw length equal those in the committed
measured-grid-results.json; the thinnest reading is a 32.4 % loss; the
patch is 20 by 7 cells; 21 blank cells filled and 8 above-nominal cells
set to nominal, and blank_fill_changes_no_result true; strength factor
0.9343 against 0.90; rating before loss and local-loss rating both 4,744 psi
[32.71 MPa], 197.6 % of 2,400 psi; allowable pressures in order B31G,
Modified B31G, RSTRENG, RSTRENG-2D, DNV-RP-F101: 5,006, 5,093, 5,095,
5,095, 4,885 psi; failure pressures: 6,959, 7,080, 7,083, 7,083,
7,539 psi; allowable spread 4.3 % above the lowest; rating 2.9 % below
the lowest allowable. Life-limit wall 0.4427 in [11.245 mm]; at rates
0.1265, 0.253 and 0.506 mm/year, lives are 29.88, 14.94 and 7.469 years
(shown as 29.9, 14.9 and 7.5). The same run reproduces the Section A values
below. Values agree to 4 significant figures.
```

## Reproduce the synthetic-grid result

Give this prompt to your assistant:

```text
Use https://vamseeachanta.github.io/digitalmodel/ffs/pipeline-ffs-assessment.html
and https://github.com/vamseeachanta/digitalmodel at the full pinned commit
193bfe3086f67a49ce163fa05324308a35f0cff0. Use Python 3.11 and uv.
1. In an empty folder with a short path (on Windows, also run
   git config --global core.longpaths true), install with Python 3.11:
     git clone --filter=blob:none https://github.com/vamseeachanta/digitalmodel
     cd digitalmodel
     git checkout 193bfe3086f67a49ce163fa05324308a35f0cff0
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
Reproduce the published measured-scan result first. If my readings are a
scanned grid, copy scripts/ffs/c3_measured_grid_assessment.py, point it at
my scan and replace its pipe data, assumptions and rate with mine; keep its
patch selection, its above-nominal and blank-cell treatment, and the check
that the blank-cell fill changes no result, and report each. Otherwise copy
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

Public data set, measured scan: anonymized riser ultrasonic scan in digitalmodel, published by AceEngineer, MIT licence, measured readings, retrieved 20 Nov 2026, repinned 23 Nov 2026 to commit 193bfe3086f67a49ce163fa05324308a35f0cff0. <https://github.com/vamseeachanta/digitalmodel/tree/193bfe3086f67a49ce163fa05324308a35f0cff0/tests/asset_integrity/test_data/real_inspection>

Public data set, synthetic grid: the AceEngineer corroded-pipeline worked example in digitalmodel, published by AceEngineer, MIT licence, worked-example readings, retrieved 12 Nov 2026, repinned 23 Nov 2026 to commit 193bfe3086f67a49ce163fa05324308a35f0cff0. <https://github.com/vamseeachanta/digitalmodel/tree/193bfe3086f67a49ce163fa05324308a35f0cff0/examples/workflows/pipeline-ffs-assessment>
