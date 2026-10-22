# BHA database — synthetic slice

The catalogue is an illustrative drilling bottom hole assembly database. All
components and assemblies in this PR are `SYNTHETIC-EXAMPLE`, with assumed
quantities; none establishes an equipment rating or qualified drilling design.
The three examples are rotary pendulum, rotary building and motor assemblies.

The [module analysis](reports/modules/bhadatabase/report.html) presents the
documented building example, calculation basis, existing test evidence and
limits of use in a single HTML report with section comments.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-21 | Document the separate nominal table and comparison audit so missing source evidence is visible. |

```python
from digitalmodel.bha_database import load_database

db = load_database()
result = db.summary('SYNTHETIC-BUILDING')
sections = db.sections('SYNTHETIC-BUILDING')
```

Run `PYTHONPATH=src python -m digitalmodel.bha_database` for SI summaries.
The default database root follows the existing source-checkout riser loader;
installed callers can supply an explicit data directory to `load_database(root)`.
Regenerate the consistency manifest with
`python scripts/bha_database/build_manifest.py` after intentional data changes.
Hashes detect accidental edits, not authenticity. Rehashed data still undergoes
all semantic checks. Sources and their checked dates are in `examples.yml`.

Each physical quantity carries `value`, `unit`, `source_key`, `basis`,
`as_published`, and `as_published_on`. For assumptions, the latter two describe
the example value and creation date, not external publication. Computed stored
values also require `formula`. Positions and joint counts are strict integers,
not dimensional measurements. Record identity and descriptive fields inherit
the record's `source_key`. All quantities convert with Pint; numeric outputs
include units, formula, computed basis and resolving source keys.

WITSML alignment: `component_id` → `uid`, `type` → `typeTubularComp`,
`pos` → `sequence` (this database explicitly counts from the bit up),
`od` → `od`, `id` → `id`, `od_max` → `odMx`, `length` → `len`,
`weight_per_length` → `wtPerLen`; connection `label` → `typeThread` and
`gender` → `configCon`. Stabilizer `blade_od` → `odBladeMx`,
`blade_length` → `lenBlade`, `blade_bottom_to_component_bottom` → `distBladeBot`.
These are field correspondences, not a WITSML interchange implementation.
OSDC type names match `Bit`, `DrillCollar`, `HeavyWeightDrillPipe`, `DrillPipe`,
`Stabilizer`, `Mwd`, `Jar`, `SteerableRotaryTool`, and `Other`. A motor maps to
`Other`; RSS maps to `SteerableRotaryTool`. Sources reference
[WITSML](https://docs.energistics.org/WITSML/WITSML_TOPICS/WITSML-500-276-0-R-sv2000.html)
and the [Open Source Drilling Community model](https://github.com/Open-Source-Drilling-Community/DrillString).

| Rule | Disposition |
| --- | --- |
| R1 | Error: exactly one bit, at position 1; contiguous ascending positions. |
| R2 | Error: near-bit stabilizer directly on the bit, count 1, bottom box. Either catalogue or assembly role triggers the rule. |
| R3 | Error: nominal bit OD shall match hole size; body, maximum and blade OD shall not exceed it (relative tolerance 1e-9 in metres). |
| R4 | Error: collar, HWDP, drill-pipe types shall retain bit-up order; intervening tools are permitted. Incomplete strings are permitted. |
| R5 | Error: each adjacent interface, including both sides of a crossover and repeated joints, requires equal thread labels and opposite genders. |
| R6 | Error: every item shall reference a known component. |
| R7 | Advisory: planned WOB compared with buoyed weight below first HWDP, reduced by an explicitly assumed margin. Missing inputs report NOT_EVALUATED. |
| R8 | OPEN: non-magnetic MWD spacing and BSR design-band checks require later qualified criteria. |

Table 1. Validation semantics for this slice.

The 15% margin and 65.5 lb/U.S. liquid gallon material density in the examples are
assumptions, not API-prescribed values. Buoyancy uses the uniform-material
identity `1 - mud_density/material_density`, and includes all components for
assembly totals. R7 excludes the HWDP and everything above it. With no HWDP,
the available-weight calculation includes the whole modelled assembly. This
teaching model does not calculate contact loads, drag or the neutral point.
Sections aggregate repeated identical joints; lengths include counts and
per-joint overrides. Blade-centre position uses the first joint's blade only.

The public loader refuses `CLIENT-PRIVATE` components and assemblies, including
when their manifest is valid. Unknown classifications and fields also fail.
Only fixed files are read, at most 1 MiB each; aliases, duplicate YAML keys,
symlinks and unexpected files are refused. Errors do not echo row payloads.
This synthetic slice supports exactly `NC46` and `NC50` connection labels;
other labels and aliases are refused. This subset is not a connection standard
or a catalogue of all available connections. The examples use equal nominal
12-inch bit and hole diameters; wear and undergauge operation are not modelled.

## Table 14 source comparison

The separate `data/bha_standards/drill_collars_std.yml` transcription cites
ISO 10424-1:2004, Table 14. `load_table14()` validates its structure, units and
source identities; it does not authenticate the source or qualify equipment.
The source digest, checked date and page locator are inherited metadata.
No licensed PDF is retained in Git. The synthetic assembly catalogue remains
independent of these nominal reference rows.

Issue #2629 requires comparison of every row with an authorized licensed copy,
recording the printed page before correcting any value. The disputed
`6 5/8 REG` bevel cell remains pending. An inch conversion cannot determine what
the standard printed. No new licensed-source comparison is established here,
and every stored table quantity remains unchanged.

`audit_table14(path)` enumerates every published field in every row. It excludes
computed weight, which is an assumption outside Table 14. A pending or unmarked
cell reports `NOT_EVALUATED`. A cell explicitly marked `verified` with its own
`verification_locator` reports `COMPARISON_RECORDED`. This status records the
presence of comparison metadata; the source evidence still needs review.
An inherited source locator cannot fill missing cell records. The overall
status remains `NOT_EVALUATED` until all cells have individual records.
Coverage is recomputed from validated cell metadata on every call, without
writing files or changing values.

### Worked example: missing source evidence

Input: the repository transcription, with its existing pending-cell marker.
Method: enumerate the six published fields in each of the 23 supplied rows
(ISO 10424-1:2004, Table 14 transcription); count by comparison status.
The independent count is 23 × 6 = 138 cells, of which one is pending,
138 − 1 = 137 are unspecified, and none has a comparison record.
These counts describe the repository file, not verified standard values.

```pycon
>>> from digitalmodel.bha_database.iso_table14 import audit_table14
>>> report = audit_table14()
>>> report['status']
'NOT_EVALUATED'
>>> report['counts']
{'total': 138, 'recorded': 0, 'pending': 1, 'unspecified': 137}
>>> pending = [cell for cell in report['cells'] if cell['verification'] == 'pending']
>>> [(cell['field'], cell['issue'], cell['locator']) for cell in pending]
[('bevel_diameter', '#2629', None)]

```

Answer: source comparison is `NOT_EVALUATED`; keep all values unchanged.
The way forward is to obtain authorized source access, compare every row,
and attach each field's actual printed-page locator before reviewing corrections.
Record neither a conversion nor the inherited locator as fresh source evidence.

Run `PYTHONPATH=src python -m digitalmodel.bha_database.iso_table14` to print
the JSON report, or append an explicit YAML path for an installed package.
Command success means that a report was generated; callers inspect `status`
for coverage. Invalid data or a missing file produces an error.
Run `PYTHONPATH=src python -m doctest docs/bha_database.md` to check the example.

Changes anywhere under `data/bha_standards/` select the subsea CI domain;
its gate runs `tests/bha_database/`. Routing and execution configuration are
covered by `tests/scripts/test_detect_touched_domains.py`.
API RP 7G weight-table comparison and a verified R7 margin clause remain OPEN.
