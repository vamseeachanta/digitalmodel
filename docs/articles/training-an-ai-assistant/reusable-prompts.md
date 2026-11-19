# Reusable prompts

Article: "Training (and attitude towards) an AI assistant", The Deckhand Chronicles, revision 5 of 19 Nov 2026. This page holds in full what Appendix B of the article summarises.

The reproduce prompt fits the public Norris observations and compares the result with NIST's certified values. The adapt prompt helps you write an instruction file and a small problem set for your own work.

## What you need to run this

- Data required: NIST Norris ASCII file, linked below; Python with its standard library and network access. No additional packages or installation commands are needed. The optional chart-count clone needs disk space for Git metadata and time for network transfer; both vary with repository growth.
- Data required to adapt: a repeated problem, inputs with units, code and edition, a trusted answer and your preferred result layout.
- Paid or behind an account: NIST needs no sign-in; your assistant or your own design standards may require an account or licence.
- Changes over time: keep the downloaded data and retrieval date; repeat the same checks after a model or instruction-file change.

## Reproduce the published result

Give this prompt to your assistant:

```text
Use Python to reproduce the Norris known-answer check.
1. Read the dataset description:
   https://www.itl.nist.gov/div898/strd/lls/data/Norris.shtml
   Download the original file:
   https://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Norris.dat
   Retain the original, retrieval date and SHA-256 digest.
2. Read the observations after the final Data: heading. The first column
   is y, the second x. There are 36 rows, originally lines 61 to 96.
   Keep the certified results separate from the fit.
3. Fit y = B0 + B1*x by ordinary least squares with an intercept.
   Use the code below, which needs only Python's standard library.
   Save it as norris_check.py and run: python norris_check.py

from decimal import Decimal, getcontext
from pathlib import Path
import hashlib
import urllib.request

getcontext().prec = 50
url = 'https://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Norris.dat'
request = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
with urllib.request.urlopen(request, timeout=60) as response:
    raw = response.read()
Path('Norris.dat').write_bytes(raw)
print('SHA-256:', hashlib.sha256(raw).hexdigest())
lines = raw.decode('ascii').rsplit('Data:', 1)[1].splitlines()[1:]
pairs = [tuple(map(Decimal, line.split())) for line in lines if line.strip()]
assert len(pairs) == 36
y, x = zip(*pairs)
n = Decimal(len(x))
xbar, ybar = sum(x)/n, sum(y)/n
sxx = sum((v-xbar)**2 for v in x)
sxy = sum((a-xbar)*(b-ybar) for a, b in zip(x, y))
b1 = sxy/sxx
b0 = ybar-b1*xbar
sse = sum((b-b0-b1*a)**2 for a, b in zip(x, y))
s = (sse/(n-2)).sqrt()
sd_b0 = s*(1/n+xbar*xbar/sxx).sqrt()
sd_b1 = s/sxx.sqrt()
values = [b0, b1, sd_b0, sd_b1, s]
targets = list(map(Decimal, ['-0.262323073774029', '1.00211681802045',
    '0.232818234301152', '0.000429796848199937', '0.884796396144373']))
names = ['intercept', 'slope', 'SD intercept', 'SD slope', 'residual SD']
for name, got, certified in zip(names, values, targets):
    passed = format(got, '.6g') == format(certified, '.6g')
    print(name, format(got, '.15g'), format(certified, '.15g'), passed)
    assert passed

4. Standard deviations of the fitted parameters are their standard
   errors, based on residual variance SSE/(n-2). Report all five fitted
   statistics to 15 significant digits and compare with the certified
   values to at least 6 significant digits. This is our pass criterion.
   Record a pass or failure for each statistic on your dashboard.
5. To check the chart counts separately, use the public source tree:
   https://github.com/vamseeachanta/digitalmodel/tree/63584454b62aa05fd5680c9148e950609389c1c2/src
   With Git installed, obtain that revision in a disposable folder:
   git clone --filter=blob:none --no-checkout https://github.com/vamseeachanta/digitalmodel.git digitalmodel-check
   git -C digitalmodel-check ls-tree -r --name-only 63584454b62aa05fd5680c9148e950609389c1c2 src/
   Count paths ending in .py containing, case-insensitively, each literal
   substring: catenary, hydrostatic, wall_thickness, sn_curve, viv.
   Each path counts once per topic. These count names, not model ability.
   Allow disk and time for Git metadata; network transfer varies.
Check: 36 observations; intercept -0.262323073774029; slope
1.00211681802045; SD intercept 0.232818234301152; SD slope
0.000429796848199937; residual SD 0.884796396144373. All five agree
to at least 6 significant digits. Chart file counts: 31, 7, 20, 3, 27,
in the substring order above; chart placements remain our estimate.
```

## Adapt it to your project

```text
Before you start, ask me for: one repeated engineering problem; inputs
and units; the governing design code, edition and clauses; a trusted
worked answer; acceptance criteria; and the layout I review.
Ask what documents my firm's data policy permits you to use.
Explain your method, missing inputs and checks before calculating.
Build a small problem set with known answers. Include the trusted case
and a limiting case for the method. Agree tolerances before running.
Calculate with code and show the inputs, units, result and comparison.
Point out gaps and resolve each failed comparison before proceeding.
After the workflow checks out, write a short instruction file covering
the problem, units, code and edition, method and clauses, input and
output locations, acceptance criteria and checks. Keep team rules and
personal layout preferences in separate files. Read them each session.
Use the same problem set after each model or instruction-file change.
Keep the Norris check as a standing check that your numerical code
reproduces certified values, with the same 6-significant-digit criterion. Track corrections only for like problems.
Log the problem, corrections and check result in one dashboard alongside
open work, working preferences and the next step in my development.
Check context when simple rules are missed. If overloaded, give a fresh
helper assistant the instructions and evidence. Review your own work;
have a fresh reviewer look for faults in consequential calculations.
Where a step needs a paid standard I do not hold, cite the clause from
the standard or reference I provide and mark it "to be checked". If
none is provided, record the missing clause and ask for it.
```

Public data set: NIST Statistical Reference Datasets, Norris linear regression, NIST SRD 140, free to download without sign-in; retrieved 12 Nov 2026. Cite NIST as the source.
