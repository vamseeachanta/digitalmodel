# BHA database — synthetic slice

The catalogue is an illustrative drilling bottom hole assembly database. All
components and assemblies in this PR are `SYNTHETIC-EXAMPLE`, with assumed
quantities; none establishes an equipment rating or qualified drilling design.
The three examples are rotary pendulum, rotary building and motor assemblies.

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
| R3 | Error: body, maximum and blade OD shall not exceed hole size. |
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
Exact connection spelling is required; aliases are not normalized.

Held outside this PR: ISO 10424-1:2004 Table 14 rows and their PDF comparison
record on `bha-iso-table14`, pending maintainer publication confirmation
(intake row I-0204). No licensed PDF is retained in Git. API RP 7G weight-table
comparison and a verified R7 margin clause remain OPEN. The public tests replace
the held table-specific checks with synthetic schema and assembly checks.
