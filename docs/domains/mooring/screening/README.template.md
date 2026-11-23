# Mooring static-line analysis — {revision}

Five homogeneous static line solutions are computed at the source page's depths.
Sixty catenary matrix entries reuse those solutions; sixty taut or hybrid entries
are not reproduced. No environmental case is qualified. The resultant is
constrained by prescribed, assumed pretension and is not peak sea-state tension.

{revision_history}

The human-readable result is [report.html](report.html). Native line components,
geometry and every matrix entry are retained in [results.json](results.json).
The [manifest](manifest.json) binds output bytes, computation-module source
bytes and record inputs. [basis.json](basis.json) records provenance, units,
missing inputs and assumptions. [criteria.json](criteria.json) contains the
mathematical checks; required code factors remain unknown. [execution.json](execution.json)
records the criterion digest and UTC time before the first engineering run.
The time order is retained because the implementation is published as one commit.

The source matrix contains generic platform classes, catenary line counts,
water depths and operating/survival labels. Numerical environmental loads,
platform properties, fairlead geometry and certified line make-up are absent.
A homogeneous generic wire from the held MoorPy examples is assumed for the
catenary lines. Diameter selection, gravity, line-length ratio, surface fairlead,
frictionless seabed and the normalized pretension rule are explicit assumptions,
with repository locators in the basis. Platform and sea-state labels do not
change the single-line solution; they are not independent computations.

Provided empirical MBL/static-tension ratios are recorded. Required safety
factors and standards acceptance are not established. DNV-OS-E301, Position
Mooring, July 2021, Section 2.2.3 is a repository registry locator for intact
quasi-static mooring. API RP 2SK, Design and Analysis of Stationkeeping Systems
for Floating Structures, Section 5 is a repository module locator; the governing
edition and clause title are not established. No licensed table values or
standards-derived numerical factors are applied or reproduced.

Tension checks require environmental demand and certified component strength;
offset checks require vessel equilibrium and allowable excursion; fatigue
requires qualified dynamic tension and component fatigue data. These remain
not established, together with stiffness, natural periods, damaged response,
taut/hybrid systems and anchor capacity. The mathematical checks establish
positive horizontal force, sufficient assumed line length and force closure
only. The assumptions do not establish station-keeping acceptance.

## Reproduction

From the repository root, with the repository environment available:

```sh
PYTHONPATH=src uv run --no-sources python docs/domains/mooring/screening/run_screening.py --receipt docs/domains/mooring/screening/execution.json --output-dir <output>
PYTHONPATH=src uv run --no-sources python docs/domains/mooring/screening/build_report.py --output-dir <output>
PYTHONPATH=src uv run --no-sources python -m pytest tests/docs/test_mooring_screening_run_record.py -q -p no:cacheprovider --basetemp <scratch>
```

The source/module digests and the loaded solver origin must match the pinned basis. The run rejects a changed
criterion digest or a criterion time at or after the first run. The native JSON
is read back and rerun for exact equality; report and manifest builds are compared
byte for byte. Numerical result text is bound to the record; additional numerical
claims are rejected, with only retained citation and revision metadata permitted.
Absolute output locations are execution inputs and are not saved
in the payload. Scratch output is retained separately from the source tree.

The prepared website source is built separately with `--website-source <source>`
and `--website-out <draft>`. The supplied source bytes must match the website
commit and digest in the basis. The draft removes unsupported values and links
this record. [comparison.md](comparison.md) lists the original values and their
replacement or removal, including each illustrative concept row. Website tests,
the website branch, second-provider review, merge and approved republication
remain under [the open acceptance issue](https://github.com/vamseeachanta/workspace-hub/issues/2115).
No website content is published by this run record.
