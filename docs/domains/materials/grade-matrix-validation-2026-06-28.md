# Material input analysis

The embedded material matrix and legacy strength snapshots have been removed.
Numerical properties now come from a validated caller catalogue and its adjacent
`sources.yml`. No engineering material row is shipped or re-sourced.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-18 | Added the #2726 supersession note to distinguish historical conversions from the current product-level basis. |
| 2 | 2026-10-18 | Reworded the grade designation sentence to pass the engineering register check. |
| 3 | 2026-10-21 | Replaced the superseded PSL interpretation with the current lookup basis and added an executable example to prevent stale answers. |

| 4 | 2026-10-26 | Removed standards-derived entries and replaced the worked example with explicitly synthetic test inputs. |

## Input basis

See [caller-supplied data](caller-supplied-data.md) for the schema, removed-row
inventory, geometry formulas and caller migration. Specification designations
remain identifiers; they do not supply properties or imply qualification.

## Worked example with synthetic inputs

This example runs inside the synthetic fixture used by the documentation test.
It is not an engineering design basis. In application code, first load the
caller-owned data and activate it with `use_catalog` as described in the input
contract. No default catalogue is installed.

>>> from digitalmodel.materials import smts_mpa
>>> from digitalmodel.materials.database import load_database
>>> record = load_database().lookup("X52", psl="PSL1")
>>> (record.designation, record.product_level, record.strengths.units)
('X52', 'PSL1', 'MPa')
>>> record.strengths.smts_mpa
591
>>> smts_mpa("X52", psl="PSL2") > record.strengths.smts_mpa
True
>>> record.provenance.citation.startswith("Synthetic material and geometry test basis;")
True
>>> smts_mpa("X80", psl="PSL1")
Traceback (most recent call last):
    ...
ValueError: X80: PSL1 strength was not supplied
>>> load_database().lookup("X80", psl="PSL1")
Traceback (most recent call last):
    ...
ValueError: X80: PSL1 strength was not supplied

## Limits

The tests establish input validation, product-level selection, aliases,
conversions and source propagation. They do not qualify a material, certify a
product or verify a standards table. A missing property requires a sourced
caller input; a different product level is never substituted.
